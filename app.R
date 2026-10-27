library(shiny)
library(DT)
library(DBI)
library(dplyr)
library(httr)
library(jsonlite)
source('db.R')
p <- make_pool()
init_db(p)
onStop(function() pool::poolClose(p))

ui <- fluidPage(
 titlePanel('Systematic review | PostgreSQL extraction'),
 p('Data stored in Neon PostgreSQL. Study groups define the result-specific group mean, SD and n fields.'),
 tabsetPanel(
  tabPanel('1. Studies',textInput('doi_input','DOI or DOI URL'),actionButton('lookup','Fetch DOI metadata'),textInput('study_id','Study ID (blank to create; enter existing ID to update)'),textInput('title','Title'),numericInput('year','Publication year',NA),textInput('author','First author surname'),textInput('country','Country of data collection'),selectInput('design','Study design',c('Experimental','Quasi-experimental','Cross-sectional','Cohort','Case-control','Longitudinal observational','Other')),numericInput('group_count','Number of comparison groups',2,min=1,max=12),uiOutput('groups_ui'),numericInput('total','Total sample',NA,min=0),numericInput('male','Male',NA,min=0),numericInput('female','Female',NA,min=0),numericInput('other','Other gender',NA,min=0),actionButton('save_study','Save study and groups'),DTOutput('studies_view'),h4('Study groups'),DTOutput('study_groups_view')),
  tabPanel('2. Measurements',textInput('measurement_id','Measurement ID (blank for new)'),selectInput('measurement_study','Study',choices=NULL),textInput('outcome','Outcome/variable'),textInput('tool','Measurement instrument'),textInput('domain','Domain'),checkboxInput('self_report','Self-reported'),checkboxInput('validated','Validated'),textInput('validation_url','Validation study URL'),selectizeInput('used_studies','Used in studies',choices=NULL,multiple=TRUE),actionButton('save_measurement','Save measurement'),DTOutput('measurements_view')),
  tabPanel('3. Results & statistics',selectInput('result_study','Study',choices=NULL),selectInput('predictor','Predictor',choices=NULL),selectInput('result_outcome','Outcome',choices=NULL),p('Each group below comes from the selected study. Leave mean/SD/n empty if not reported.'),uiOutput('result_groups_ui'),numericInput('df','Degrees of freedom',NA),numericInput('beta','Standardized beta',NA),numericInput('r','Correlation r',NA,min=-1,max=1),numericInput('b','Unstandardized coefficient',NA),numericInput('pvalue','p value',NA,min=0,max=1),numericInput('effect','Effect size',NA),textInput('effect_type','Effect size type'),actionButton('save_result','Save result and group statistics'),DTOutput('results_view'),h4('Group-level statistics (long format)'),DTOutput('result_groups_view')),
  tabPanel('4. Correlations',selectInput('corr_study','Study',choices=NULL),textAreaInput('matrix','Paste tab-delimited or CSV square correlation matrix, including row and column labels',rows=10),actionButton('save_matrix','Save correlation matrix'),DTOutput('correlations_view')),
  tabPanel('5. Qualitative',selectInput('qual_study','Study',choices=NULL),textInput('theme','Theme'),textAreaInput('description','Description',rows=5),actionButton('save_qual','Save theme'),DTOutput('qualitative_view')),
  tabPanel('Export',actionButton('refresh','Refresh database'),selectInput('export_table','Export table',choices=c('studies','study_groups','measurements','results','result_groups','correlations','qualitative')),downloadButton('download','Download CSV'))
 )
)
server <- function(input,output,session) {
 tables <- reactiveVal(fetch_all(p))
 refresh <- function() tables(fetch_all(p))
 safe <- function(fun) tryCatch({fun();refresh();showNotification('Saved to Neon PostgreSQL',type='message')},error=function(e) showNotification(conditionMessage(e),type='error',duration=12))
 observeEvent(input$refresh,refresh())
 observe({
  t <- tables();s <- t$studies
  choices <- setNames(s$study_id,paste(s$first_author,s$year,s$title,sep=' | '))
  for(id in c('measurement_study','result_study','corr_study','qual_study')) updateSelectInput(session,id,choices=choices,selected=isolate(input[[id]]))
  updateSelectizeInput(session,'used_studies',choices=choices,selected=isolate(input$used_studies),server=TRUE)
 })
 output$groups_ui <- renderUI({lapply(seq_len(input$group_count),function(i) textInput(paste0('group_',i),paste('Group',i,'name'),value=if(i==1)'Intervention 1' else if(i==2)'Control 1' else paste('Group',i)))})
 observeEvent(input$lookup,safe_lookup <- {
  tryCatch({x <- lookup_doi(input$doi_input);updateTextInput(session,'title',value=x$title);updateNumericInput(session,'year',value=x$year);updateTextInput(session,'author',value=x$first_author);showNotification('Metadata found. Check before saving.')},error=function(e)showNotification(conditionMessage(e),type='warning'))
 })
 observeEvent(input$save_study,safe(function(){
  req(nzchar(trimws(input$title)),nzchar(trimws(input$author)))
  id <- if(nzchar(trimws(input$study_id))) trimws(input$study_id) else new_id('study')
  labels <- vapply(seq_len(input$group_count),function(i) trimws(input[[paste0('group_',i)]] %||% ''),character(1))
  if(any(!nzchar(labels)) || anyDuplicated(labels)) stop('All group names must be non-empty and unique.')
  old <- query(p,'SELECT group_id,group_order FROM study_groups WHERE study_id=$1 ORDER BY group_order',list(id))
  if(nrow(old)>input$group_count && nrow(query(p,'SELECT result_id FROM results WHERE study_id=$1 LIMIT 1',list(id)))) stop('Cannot remove groups after results exist. Keep the original group count.')
  DBI::dbWithTransaction(p,{
   exec(p,'INSERT INTO studies (study_id,doi,url,title,year,first_author,country,study_type,total_n,male_n,female_n,other_gender_n) VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12) ON CONFLICT (study_id) DO UPDATE SET doi=EXCLUDED.doi,url=EXCLUDED.url,title=EXCLUDED.title,year=EXCLUDED.year,first_author=EXCLUDED.first_author,country=EXCLUDED.country,study_type=EXCLUDED.study_type,total_n=EXCLUDED.total_n,male_n=EXCLUDED.male_n,female_n=EXCLUDED.female_n,other_gender_n=EXCLUDED.other_gender_n,updated_at=now()',list(id,if(grepl('10\\.[0-9]{4,9}/',input$doi_input)) sub('^.*?(10\\.[0-9]{4,9}/[^?# ]+).*$','\\1',input$doi_input) else NA_character_,input$doi_input,input$title,int_or_na(input$year),input$author,input$country,input$design,int_or_na(input$total),int_or_na(input$male),int_or_na(input$female),int_or_na(input$other)))
   for(i in seq_along(labels)) {
    gid <- if(i<=nrow(old)) old$group_id[i] else new_id('grp')
    exec(p,'INSERT INTO study_groups(group_id,study_id,group_order,group_label) VALUES($1,$2,$3,$4) ON CONFLICT (group_id) DO UPDATE SET group_label=EXCLUDED.group_label',list(gid,id,as.integer(i),labels[i]))
   }
   if(nrow(old)>length(labels)) exec(p,'DELETE FROM study_groups WHERE study_id=$1 AND group_order>$2',list(id,as.integer(length(labels))))
  })
  updateTextInput(session,'study_id',value=id)
 }))
 observeEvent(input$save_measurement,safe(function(){
  req(input$measurement_study,nzchar(trimws(input$outcome)))
  id <- if(nzchar(trimws(input$measurement_id))) trimws(input$measurement_id) else new_id('measure')
  used <- unique(c(input$measurement_study,input$used_studies))
  if(!all(used %in% tables()$studies$study_id)) stop('One or more linked studies no longer exist.')
  existing <- query(p,'SELECT study_id FROM measurements WHERE measurement_id=$1',list(id))
  if(nrow(existing) && existing$study_id[1] != input$measurement_study) stop('Cannot move a measurement to a different study.')
  exec(p,"INSERT INTO measurements(measurement_id,study_id,outcome,tool,domain,self_reported,validated,validation_url,used_in_studies_json) VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9::jsonb) ON CONFLICT (measurement_id) DO UPDATE SET outcome=EXCLUDED.outcome,tool=EXCLUDED.tool,domain=EXCLUDED.domain,self_reported=EXCLUDED.self_reported,validated=EXCLUDED.validated,validation_url=EXCLUDED.validation_url,used_in_studies_json=EXCLUDED.used_in_studies_json,updated_at=now()",list(id,input$measurement_study,input$outcome,input$tool,input$domain,input$self_report,input$validated,input$validation_url,as.character(toJSON(used,auto_unbox=FALSE))))
  updateTextInput(session,'measurement_id',value=id)
 }))
 observe({
  req(input$result_study);m <- tables()$measurements
  m <- m[m$study_id==input$result_study,,drop=FALSE]
  choices <- setNames(m$measurement_id,paste(m$outcome,m$tool,sep=' | '))
  updateSelectInput(session,'predictor',choices=choices)
  updateSelectInput(session,'result_outcome',choices=choices)
 })
 selected_groups <- reactive({req(input$result_study);g <- tables()$study_groups;g[g$study_id==input$result_study,,drop=FALSE]})
 output$result_groups_ui <- renderUI({
  g <- selected_groups();if(!nrow(g)) return(p('No groups defined. Create study groups first.'))
  tagList(lapply(seq_len(nrow(g)),function(i){
   gid <- g$group_id[i];wellPanel(h4(paste0('Group ',g$group_order[i],': ',g$group_label[i])),fluidRow(column(4,numericInput(paste0('mean_',gid),'Mean',NA)),column(4,numericInput(paste0('sd_',gid),'SD',NA,min=0)),column(4,numericInput(paste0('n_',gid),'n',NA,min=0))))
  }))
 })
 observeEvent(input$save_result,safe(function(){
  req(input$result_study,input$predictor,input$result_outcome)
  g <- selected_groups();if(!nrow(g)) stop('Define study groups first.')
  vals <- lapply(g$group_id,function(gid) list(mean=num_or_na(input[[paste0('mean_',gid)]]),sd=num_or_na(input[[paste0('sd_',gid)]]),n=int_or_na(input[[paste0('n_',gid)]])))
  if(any(vapply(vals,function(x) !is.na(x$sd) && x$sd<0 || !is.na(x$n) && x$n<0,logical(1)))) stop('SD and n cannot be negative.')
  id <- new_id('result')
  DBI::dbWithTransaction(p,{
   exec(p,'INSERT INTO results(result_id,study_id,predictor_id,outcome_id,df,beta,r,unstandardized_coefficient,p_value,effect_size,effect_size_type) VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11)',list(id,input$result_study,input$predictor,input$result_outcome,num_or_na(input$df),num_or_na(input$beta),num_or_na(input$r),num_or_na(input$b),num_or_na(input$pvalue),num_or_na(input$effect),input$effect_type))
   for(i in seq_len(nrow(g))) exec(p,'INSERT INTO result_groups(result_id,group_id,mean,sd,n) VALUES($1,$2,$3,$4,$5)',list(id,g$group_id[i],vals[[i]]$mean,vals[[i]]$sd,vals[[i]]$n))
  })
 }))
 observeEvent(input$save_matrix,safe(function(){
  req(input$corr_study,nzchar(input$matrix));m <- parse_matrix(input$matrix)
  mid <- new_id('matrix');j <- as.character(toJSON(list(variables=rownames(m),matrix=unname(m)),auto_unbox=FALSE,na='null'))
  count <- 0L
  DBI::dbWithTransaction(p,{
   for(i in seq_len(nrow(m))) for(k in seq_len(ncol(m))) if(k>i && !is.na(m[i,k])) {
    exec(p,'INSERT INTO correlations(correlation_id,matrix_id,study_id,variable_1,variable_2,r,matrix_json) VALUES($1,$2,$3,$4,$5,$6,$7::jsonb)',list(new_id('corr'),mid,input$corr_study,rownames(m)[i],colnames(m)[k],as.numeric(m[i,k]),j));count <- count+1L
   }
   if(count==0L) stop('No off-diagonal correlations found.')
  })
 }))
 observeEvent(input$save_qual,safe(function(){
  req(input$qual_study,nzchar(trimws(input$theme)))
  exec(p,'INSERT INTO qualitative(qualitative_id,study_id,theme,description) VALUES($1,$2,$3,$4)',list(new_id('theme'),input$qual_study,input$theme,input$description))
 }))
 for(nm in c('studies','study_groups','measurements','results','result_groups','correlations','qualitative')) local({name <- nm;output[[paste0(name,'_view')]] <- renderDT(datatable(tables()[[name]],options=list(scrollX=TRUE,pageLength=5)))})
 output$download <- downloadHandler(filename=function() paste0(input$export_table,'_',Sys.Date(),'.csv'),content=function(file) utils::write.csv(query(p,paste0('SELECT * FROM ',input$export_table)),file,row.names=FALSE,na=''))
}
shinyApp(ui,server)
