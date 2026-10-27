library(DBI)
library(RPostgres)
library(pool)
library(jsonlite)

make_pool <- function() {
  keys <- c('PGHOST','PGDATABASE','PGUSER','PGPASSWORD')
  missing <- keys[!nzchar(Sys.getenv(keys))]
  if (length(missing)) stop('Missing environment variables: ',paste(missing,collapse=', '))
  pool::dbPool(drv=RPostgres::Postgres(),host=Sys.getenv('PGHOST'),port=as.integer(Sys.getenv('PGPORT','5432')),dbname=Sys.getenv('PGDATABASE'),user=Sys.getenv('PGUSER'),password=Sys.getenv('PGPASSWORD'),sslmode='require',minSize=0,maxSize=5,idleTimeout=60000)
}

init_db <- function(p) {
  sql <- c(
    "CREATE TABLE IF NOT EXISTS studies (study_id TEXT PRIMARY KEY, doi TEXT, url TEXT, title TEXT NOT NULL, year INTEGER, first_author TEXT NOT NULL, country TEXT, study_type TEXT, total_n INTEGER, male_n INTEGER, female_n INTEGER, other_gender_n INTEGER, updated_at TIMESTAMPTZ NOT NULL DEFAULT now())",
    "CREATE TABLE IF NOT EXISTS study_groups (group_id TEXT PRIMARY KEY, study_id TEXT NOT NULL REFERENCES studies(study_id) ON DELETE CASCADE, group_order INTEGER NOT NULL, group_label TEXT NOT NULL, UNIQUE(study_id,group_order), UNIQUE(study_id,group_id))",
    "CREATE TABLE IF NOT EXISTS measurements (measurement_id TEXT PRIMARY KEY, study_id TEXT NOT NULL REFERENCES studies(study_id), outcome TEXT NOT NULL, tool TEXT, domain TEXT, self_reported BOOLEAN, validated BOOLEAN, validation_url TEXT, used_in_studies_json JSONB NOT NULL DEFAULT '[]'::jsonb, updated_at TIMESTAMPTZ NOT NULL DEFAULT now(), UNIQUE(study_id,measurement_id))",
    "CREATE TABLE IF NOT EXISTS results (result_id TEXT PRIMARY KEY, study_id TEXT NOT NULL REFERENCES studies(study_id), predictor_id TEXT NOT NULL, outcome_id TEXT NOT NULL, df DOUBLE PRECISION, beta DOUBLE PRECISION, r DOUBLE PRECISION CHECK (r BETWEEN -1 AND 1), unstandardized_coefficient DOUBLE PRECISION, p_value DOUBLE PRECISION CHECK (p_value BETWEEN 0 AND 1), effect_size DOUBLE PRECISION, effect_size_type TEXT, updated_at TIMESTAMPTZ NOT NULL DEFAULT now(), FOREIGN KEY (study_id,predictor_id) REFERENCES measurements(study_id,measurement_id), FOREIGN KEY (study_id,outcome_id) REFERENCES measurements(study_id,measurement_id))",
    "CREATE TABLE IF NOT EXISTS result_groups (result_id TEXT NOT NULL REFERENCES results(result_id) ON DELETE CASCADE, group_id TEXT NOT NULL REFERENCES study_groups(group_id), mean DOUBLE PRECISION, sd DOUBLE PRECISION CHECK (sd >= 0), n INTEGER CHECK (n >= 0), PRIMARY KEY(result_id,group_id))",
    "CREATE TABLE IF NOT EXISTS correlations (correlation_id TEXT PRIMARY KEY, matrix_id TEXT NOT NULL, study_id TEXT NOT NULL REFERENCES studies(study_id), variable_1 TEXT NOT NULL, variable_2 TEXT NOT NULL, r DOUBLE PRECISION NOT NULL CHECK (r BETWEEN -1 AND 1), matrix_json JSONB, updated_at TIMESTAMPTZ NOT NULL DEFAULT now())",
    "CREATE TABLE IF NOT EXISTS qualitative (qualitative_id TEXT PRIMARY KEY, study_id TEXT NOT NULL REFERENCES studies(study_id), theme TEXT NOT NULL, description TEXT, updated_at TIMESTAMPTZ NOT NULL DEFAULT now())"
  )
  for (s in sql) DBI::dbExecute(p,s)
}
query <- function(p,sql,params=list()) DBI::dbGetQuery(p,sql,params=params)
exec <- function(p,sql,params=list()) DBI::dbExecute(p,sql,params=params)
new_id <- function(prefix) paste0(prefix,'_',paste(sample(c(letters,0:9),24,TRUE),collapse=''))
num_or_na <- function(x) if(length(x)==0 || is.na(x) || !is.finite(x)) NA_real_ else as.numeric(x)
int_or_na <- function(x) if(length(x)==0 || is.na(x) || !is.finite(x)) NA_integer_ else as.integer(x)
fetch_all <- function(p) {
  list(studies=query(p,'SELECT * FROM studies ORDER BY updated_at DESC'),study_groups=query(p,'SELECT * FROM study_groups ORDER BY study_id,group_order'),measurements=query(p,'SELECT * FROM measurements ORDER BY updated_at DESC'),results=query(p,'SELECT * FROM results ORDER BY updated_at DESC'),result_groups=query(p,'SELECT * FROM result_groups'),correlations=query(p,'SELECT * FROM correlations ORDER BY updated_at DESC'),qualitative=query(p,'SELECT * FROM qualitative ORDER BY updated_at DESC'))
}
lookup_doi <- function(value) {
  doi <- regmatches(value,regexpr('10\\.[0-9]{4,9}/[^[:space:]?#]+',value,perl=TRUE))
  if(!length(doi) || !nzchar(doi)) stop('No DOI found. Enter metadata manually.')
  url <- paste0('https://api.crossref.org/works/',utils::URLencode(doi,reserved=TRUE))
  r <- httr::GET(url,httr::timeout(12));httr::stop_for_status(r)
  x <- httr::content(r,as='parsed',type='application/json')$message
  list(doi=doi,title=x$title[[1]] %||% '',year=x$published[['date-parts']][[1]][[1]] %||% NA_integer_,first_author=x$author[[1]]$family %||% '')
}
`%||%` <- function(x,y) if(is.null(x) || length(x)==0) y else x
parse_matrix <- function(txt) {
  sep <- if(grepl('\t',txt)) '\t' else ','
  d <- utils::read.table(text=txt,sep=sep,header=TRUE,row.names=1,check.names=FALSE,fill=TRUE,na.strings=c('','NA'),quote='"',comment.char='',strip.white=TRUE)
  m <- as.matrix(d);storage.mode(m) <- 'numeric'
  if(nrow(m)!=ncol(m) || !setequal(rownames(m),colnames(m))) stop('Matrix must be square with identical row and column labels.')
  m <- m[,rownames(m),drop=FALSE]
  if(any(!is.na(m) & (m < -1 | m > 1))) stop('Correlation values must be between -1 and 1.')
  for(i in seq_len(nrow(m))) for(j in seq_len(ncol(m))) {
    if(is.na(m[i,j]) && !is.na(m[j,i])) m[i,j] <- m[j,i]
    if(!is.na(m[i,j]) && !is.na(m[j,i]) && abs(m[i,j]-m[j,i])>1e-6) stop('Matrix is not symmetric.')
  }
  m
}
