library(shiny)
library(DT)
library(DBI)
library(jsonlite)
library(pool)
source('db.R')
source('auth.R')

p <- make_pool()
tryCatch(init_db(p), error = function(e) { pool::poolClose(p); stop(e) })
onStop(function() pool::poolClose(p))

TABLES <- c('studies','study_groups','measurements','results','result_measurement_groups','correlations','qualitative')
ui <- fluidPage(
  uiOutput("authenticated_ui")
)

extraction_ui <- fluidPage(
  titlePanel('Systematic review | Neon PostgreSQL extraction'),
  fluidRow(
    column(9, textOutput('current_reviewer')),
    column(3, actionButton('logout', 'Log out'))
  ),
  p('Database-generated numeric IDs; dynamic group statistics; correlation pairs.'),
  tabsetPanel(
    tabPanel('1. Studies',
             textInput('doi_input','DOI or DOI URL'), actionButton('lookup','Fetch DOI metadata'),
             selectInput('edit_study','Edit existing study (optional)',choices=c('Create new'='')),
             textInput('title','Title'),numericInput('year','Publication year',NA),
             textInput('author','First author surname'),textInput('country','Country of data collection'),
             selectInput('design','Study design',c('Experimental','Quasi-experimental','Cross-sectional','Cohort','Case-control','Longitudinal observational','Systematic review','Meta-analysis','Other')),
             numericInput('group_count','Number of comparison groups',2,min=1,max=12),
             uiOutput('groups_ui'),numericInput('total','Total sample',NA,min=0),
             numericInput('male','Male',NA,min=0),numericInput('female','Female',NA,min=0),
             numericInput('other','Other gender',NA,min=0),actionButton('save_study','Save study and groups'), div(style='height:50px;'), DTOutput('studies_view'),h4('Study groups'),DTOutput('study_groups_view')
    ),
    tabPanel('2. Measurements',
             fluidRow(
               column(
                 9,
                 selectInput(
                   'edit_measurement',
                   'Edit existing measurement (optional)',
                   choices = c('Create new' = '')
                 )
               ),
               column(
                 3,
                 br(),
                 actionButton(
                   'new_measurement',
                   'New measurement',
                   icon = icon('plus')
                 )
               )
             ),
             textInput('outcome','Outcome / variable'),textInput('tool','Measurement instrument'),
             textInput('domain','Domain'),checkboxInput('self_report','Self-reported'),
             checkboxInput('validated','Validated'),textInput('validation_url','Validation study URL'),
             selectizeInput('used_studies','Used in studies',choices=NULL,multiple=TRUE),
             actionButton('save_measurement','Save measurement'), div(style='height:50px;'), DTOutput('measurements_view')
    ),
    tabPanel('3. Results & statistics',
             selectInput('result_study','Study',choices=NULL),selectInput('predictor','Predictor',choices=NULL),
             selectInput('result_outcome','Outcome',choices=NULL),
             p('Enter mean, SD and n separately for the predictor and outcome within each study group. Leave unreported statistics blank.'),
             uiOutput('result_groups_ui'),numericInput('df','Degrees of freedom',NA),
             numericInput('beta','Standardized beta',NA),numericInput('r','Correlation r',NA,min=-1,max=1),
             numericInput('b','Unstandardized coefficient',NA),numericInput('pvalue','p value',NA,min=0,max=1),
             numericInput('effect','Effect size',NA),textInput('effect_type','Effect size type'),
             actionButton('save_result','Save result and group statistics'),
             div(style='height:50px;'), DTOutput('results_view'),
             h4('Predictor and outcome group statistics'),DTOutput('result_measurement_groups_view')
    ),
    tabPanel('4. Correlations',selectInput('corr_study','Study',choices=NULL),
             textAreaInput('matrix','Paste tab-delimited or CSV square correlation matrix with labels',rows=10),
             actionButton('save_matrix','Save correlation matrix'), div(style='height:50px;'), DTOutput('correlations_view')
    ),
    tabPanel('5. Qualitative',selectInput('qual_study','Study',choices=NULL),
             textInput('theme','Theme'),textAreaInput('description','Theme description',rows=5),
             actionButton('save_qual','Save theme'), div(style='height:50px;'), DTOutput('qualitative_view')
    ),
    tabPanel('Export',actionButton('refresh','Refresh database'),
             selectInput('export_table','Export table',choices=TABLES),downloadButton('download','Download CSV'))
  )
)

login_ui <- fluidPage(
  titlePanel("URA-BE-FIT Data Extraction"),
  tabsetPanel(
    tabPanel("Login",
      textInput("login_username", "Username"),
      passwordInput("login_password", "Password"),
      actionButton("login", "Log in")
    ),
    tabPanel("Register",
      textInput("register_username", "Choose username"),
      passwordInput("register_password", "Choose password"),
      passwordInput("register_confirm", "Confirm password"),
      actionButton("register", "Create account"),
      p("Accounts require administrator approval before access.")
    )
  )
)

server <- function(input,output,session) {
  auth <- reactiveValues(logged_in = FALSE,user_id = NULL,username = NULL,role = NULL)
  fetch_tables <- function() setNames(lapply(TABLES,function(x) fetch_all(p,x)),TABLES)
  tables <- reactiveVal(NULL)
  
  require_reviewer <- function() {
    if (!isTRUE(auth$logged_in) || is.null(auth$user_id)) {
      stop('Login is required.')
    }
    reviewer <- db_query(
      p,
      "SELECT user_id, username, role FROM app_users WHERE user_id=$1 AND status='active'",
      list(as.character(auth$user_id))
    )
    if (nrow(reviewer) != 1L) {
      auth$logged_in <- FALSE
      auth$user_id <- NULL
      auth$username <- NULL
      auth$role <- NULL
      tables(NULL)
      stop('Reviewer account is no longer authorized.')
    }
    invisible(reviewer)
  }

  refresh <- function() {
    require_reviewer()
    tables(fetch_tables())
  }

  observeEvent(auth$logged_in, {
    if (isTRUE(auth$logged_in)) {
      tryCatch(refresh(), error = function(e) {
        message('Unable to load extraction data: ', conditionMessage(e))
        showNotification('Unable to load extraction data.', type = 'error')
      })
    } else {
      tables(NULL)
    }
  })

  safe <- function(fun) tryCatch({
    require_reviewer()
    fun()
    refresh()
    showNotification('Saved to Neon PostgreSQL', type='message')
  }, error=function(e) {
    message('Save failed: ', conditionMessage(e))
    showNotification('Operation failed. Please check with the administrator.',
                     type='error', duration=12)
  })
  
  observeEvent(input$register, {
    if (!identical(input$register_password, input$register_confirm)) {
      showNotification("Passwords do not match.", type = "error")
      return()
    }
    tryCatch({
      register_user(p, input$register_username, input$register_password)
      showNotification("Account created. Await administrator approval.", duration = 10)
      updateTextInput(session, "register_username", value = "")
      updateTextInput(session, "register_password", value = "")
      updateTextInput(session, "register_confirm", value = "")
    }, error = function(e) {
      message("Registration error: ", conditionMessage(e))
      showNotification("Registration failed. Check username and password requirements.",
                       type = "error", duration = 10)
    })
  })

  login_failures <- 0L
  login_blocked_until <- as.POSIXct(NA)

  observeEvent(input$login, {
    if (!is.na(login_blocked_until) && Sys.time() < login_blocked_until) {
      showNotification('Too many attempts. Try again later.', type='error')
      return()
    }
    user <- tryCatch(
      authenticate_user(p, input$login_username, input$login_password),
      error = function(e) { message('Login error: ', conditionMessage(e)); NULL }
    )
    if (is.null(user)) {
      login_failures <<- login_failures + 1L
      if (login_failures >= 5L) {
        login_blocked_until <<- Sys.time() + 300
        login_failures <<- 0L
      }
      showNotification('Invalid credentials or account not approved.', type='error')
      return()
    }
    login_failures <<- 0L
    login_blocked_until <<- as.POSIXct(NA)
    auth$user_id <- user$user_id[1]
    auth$username <- user$username[1]
    auth$role <- user$role[1]
    auth$logged_in <- TRUE
    updateTextInput(session, 'login_username', value='')
    updateTextInput(session, 'login_password', value='')
  })

  output$current_reviewer <- renderText({
    req(isTRUE(auth$logged_in))
    paste('Logged in as:', auth$username)
  })

  observeEvent(input$logout, {
    auth$logged_in <- FALSE
    auth$user_id <- NULL
    auth$username <- NULL
    auth$role <- NULL
    tables(NULL)
    showNotification('Logged out.', type='message')
  })
  output$authenticated_ui <- renderUI({
    if (!isTRUE(auth$logged_in)) {
      return(login_ui)
    }
    extraction_ui
  })
  
  observeEvent(input$refresh, refresh())
  studies_choices <- reactive({req(isTRUE(auth$logged_in), tables());s <- tables()$studies;setNames(as.character(s$study_id),paste(s$first_author,s$publication_year,s$title,sep=' | '))})
  observe({
    choices <- studies_choices()
    updateSelectInput(session,'edit_study',choices=c('Create new'='',choices),selected=isolate(input$edit_study))
    for (id in c('result_study','corr_study','qual_study'))
      updateSelectInput(session,id,choices=choices,selected=isolate(input[[id]]))
  })
  # Selecting an existing study populates the edit form and preserves group names.
  group_edit <- reactiveVal(NULL)
  observeEvent(input$edit_study,{
    id <- parse_id(input$edit_study)
    if (is.null(id)) {
      group_edit(NULL)
      for (key in c('title','author','country','doi_input')) updateTextInput(session,key,value='')
      for (key in c('year','total','male','female','other')) updateNumericInput(session,key,value=NA)
      updateNumericInput(session,'group_count',value=2)
      return()
    }
    s <- tables()$studies; s <- s[as.character(s$study_id)==id,,drop=FALSE];req(nrow(s)==1)
    updateTextInput(session,'title',value=s$title[1]);updateTextInput(session,'author',value=s$first_author[1] %||% '')
    updateTextInput(session,'country',value=s$country[1] %||% '')
    updateTextInput(session,'doi_input',value=if(!is.na(s$doi[1])) s$doi[1] else (s$url[1] %||% ''))
    updateNumericInput(session,'year',value=s$publication_year[1]);updateNumericInput(session,'total',value=s$total_sample[1])
    updateNumericInput(session,'male',value=s$male_n[1]);updateNumericInput(session,'female',value=s$female_n[1]);updateNumericInput(session,'other',value=s$other_gender_n[1])
    updateSelectInput(session,'design',selected=s$study_design[1])
    g <- tables()$study_groups;g <- g[as.character(g$study_id)==id,,drop=FALSE];g <- g[order(g$group_order),,drop=FALSE]
    group_edit(g);updateNumericInput(session,'group_count',value=max(1,nrow(g)))
  },ignoreInit=TRUE)
  output$groups_ui <- renderUI({
    n <- input$group_count;req(!is.null(n),n>=1)
    g <- group_edit()
    tagList(lapply(seq_len(n),function(i) {
      label <- if(!is.null(g) && nrow(g)>=i) g$group_name[i] else if(i==1) 'Intervention 1' else if(i==2) 'Control 1' else paste('Group',i)
      kind <- if(!is.null(g) && nrow(g)>=i) g$group_type[i] else if(i==2) 'Control' else 'Intervention'
      fluidRow(column(7,textInput(paste0('group_',i),paste('Group',i,'name'),value=label)),
               column(5,selectInput(paste0('group_type_',i),'Type',choices=c('Intervention','Control','Other'),selected=if(is.na(kind)) 'Other' else kind)))
    }))
  })
  observeEvent(input$lookup,{
    require_reviewer()
    tryCatch({x <- lookup_doi(input$doi_input);updateTextInput(session,'title',value=x$title)
    updateNumericInput(session,'year',value=x$year);updateTextInput(session,'author',value=x$first_author)
    showNotification('Metadata found. Verify before saving.')},
    error=function(e)showNotification(conditionMessage(e),type='warning',duration=10))
  })
  observeEvent(input$save_study,safe(function(){
    if(!nzchar(trimws(input$title))) stop('Study title is required.')
    n <- input$group_count;req(n,n>=1)
    labels <- vapply(seq_len(n),function(i)trimws(input[[paste0('group_',i)]] %||% ''),character(1))
    kinds <- vapply(seq_len(n),function(i)input[[paste0('group_type_',i)]] %||% 'Other',character(1))
    if(any(!nzchar(labels)) || anyDuplicated(labels)) stop('Group names must be nonempty and unique.')
    id <- parse_id(input$edit_study)
    doi <- regmatches(input$doi_input,regexpr('10\\.[0-9]{4,9}/[^[:space:]?#]+',input$doi_input,perl=TRUE))
    doi <- if(length(doi)) doi else NA_character_
    with_reviewer_transaction(p,auth$user_id,function(con) {
      values <- list(input$title,int_or_na(input$year),input$author,doi,input$doi_input,input$country,input$design,
                     int_or_na(input$total),int_or_na(input$male),int_or_na(input$female),int_or_na(input$other))
      if(is.null(id)) {
        id <- as.character(query_con(con,paste0('INSERT INTO studies(title,publication_year,first_author,doi,url,country,study_design,total_sample,male_n,female_n,other_gender_n) ',
                                                'VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11) RETURNING study_id'),values)$study_id[1])
      } else {
        exec_con(con,paste0('UPDATE studies SET title=$1,publication_year=$2,first_author=$3,doi=$4,url=$5,country=$6,study_design=$7,',
                            'total_sample=$8,male_n=$9,female_n=$10,other_gender_n=$11,updated_at=NOW() WHERE study_id=$12'),c(values,list(id)))
      }
      old <- query_con(con,'SELECT group_id,group_order FROM study_groups WHERE study_id=$1 ORDER BY group_order',list(id))
      if(nrow(old)>n) {
        removed <- old$group_id[seq.int(n+1,nrow(old))]
        for(gid in removed) {
          linked <- query_con(con,'SELECT 1 FROM result_measurement_groups WHERE group_id=$1 LIMIT 1',list(as.character(gid)))
          if(nrow(linked)) stop('Cannot remove a group with saved result statistics.')
        }
        exec_con(con,'DELETE FROM study_groups WHERE study_id=$1 AND group_order>$2',list(id,as.integer(n)))
      }
      for(i in seq_len(n)) {
        if(i<=nrow(old)) exec_con(con,'UPDATE study_groups SET group_name=$1,group_type=$2,group_order=$3 WHERE group_id=$4',
                                  list(labels[i],kinds[i],as.integer(i),as.character(old$group_id[i])))
        else exec_con(con,'INSERT INTO study_groups(study_id,group_name,group_type,group_order) VALUES($1,$2,$3,$4)',
                      list(id,labels[i],kinds[i],as.integer(i)))
      }
      id
    }) -> saved_id
    updateSelectInput(session,'edit_study',selected=saved_id)
  }))
  # Measurement selection and study associations
  measurement_study_ids <- function(x) {
    if (is.list(x) && length(x) == 1L) x <- x[[1L]]
    if (is.null(x) || !length(x) || all(is.na(x))) return(character())
    if (is.character(x) && length(x) == 1L) {
      value <- trimws(x)
      if (!nzchar(value) || value == 'null') return(character())
      if (startsWith(value, '[') || startsWith(value, '"')) {
        x <- tryCatch(jsonlite::fromJSON(value), error = function(e) character())
      }
    }
    unique(as.character(unlist(x, use.names = FALSE)))
  }

  observeEvent(tables()$measurements, {
    m <- tables()$measurements
    opts <- setNames(as.character(m$measurement_id),
                     paste(m$outcome, m$measurement_tool, sep = ' | '))
    current <- isolate(input$edit_measurement) %||% ''
    if (!current %in% c('', as.character(m$measurement_id))) current <- ''
    updateSelectInput(session, 'edit_measurement',
                      choices = c('Create new' = '', opts), selected = current)
  })

  reset_measurement <- function() {
    for (key in c('outcome', 'tool', 'domain', 'validation_url')) {
      updateTextInput(session, key, value = '')
    }
    for (key in c('self_report', 'validated')) {
      updateCheckboxInput(session, key, value = FALSE)
    }
    updateSelectizeInput(session, 'used_studies', choices = studies_choices(),
                         selected = character(), server = FALSE)
  }

  observeEvent(input$new_measurement, {
    updateSelectInput(session, 'edit_measurement', selected = '')
    reset_measurement()
  })

  observeEvent(input$edit_measurement, {
    id <- parse_id(input$edit_measurement)
    if (is.null(id)) {
      reset_measurement()
      return()
    }

    m <- tables()$measurements
    m <- m[as.character(m$measurement_id) == id, , drop = FALSE]
    if (nrow(m) != 1L) return()

    text_fields <- c(outcome = 'outcome', tool = 'measurement_tool',
                     domain = 'domain', validation_url = 'validation_url')
    for (key in names(text_fields)) {
      value <- m[[text_fields[[key]]]][1]
      updateTextInput(session, key, value = if (is.na(value)) '' else as.character(value))
    }
    updateCheckboxInput(session, 'self_report', value = isTRUE(m$self_reported[1]))
    updateCheckboxInput(session, 'validated', value = isTRUE(m$validated[1]))
    ids <- intersect(measurement_study_ids(m$study_ids[1]),
                     as.character(tables()$studies$study_id))
    updateSelectizeInput(session, 'used_studies', choices = studies_choices(),
                         selected = ids, server = FALSE)
  })

  # Refresh study options without clearing current selections.
  observeEvent(tables()$studies, {
    choices <- studies_choices()
    selected <- isolate(input$used_studies) %||% character()
    if (!is.null(isolate(parse_id(input$edit_measurement))) && !length(selected)) {
      m <- tables()$measurements
      id <- isolate(parse_id(input$edit_measurement))
      row <- m[as.character(m$measurement_id) == id, , drop = FALSE]
      if (nrow(row) == 1L) selected <- measurement_study_ids(row$study_ids[1])
    }
    updateSelectizeInput(session, 'used_studies', choices = choices,
                         selected = intersect(as.character(selected), unname(choices)),
                         server = FALSE)
  })
  observeEvent(input$save_measurement,safe(function(){
    if(!nzchar(trimws(input$outcome))) stop('Outcome is required.')
    used <- unique(as.character(input$used_studies %||% character()))
    if(!length(used)) stop('Select at least one study.')
    if(!all(used %in% as.character(tables()$studies$study_id))) stop('Invalid study selection.')
    json <- as.character(jsonlite::toJSON(used,auto_unbox=FALSE))
    id <- parse_id(input$edit_measurement)
    values <- list(input$outcome,input$tool,input$domain,input$self_report,input$validated,input$validation_url,json)
    saved_id <- with_reviewer_transaction(p, auth$user_id, function(con) {
      if (is.null(id)) {
        as.character(query_con(con, paste0(
          'INSERT INTO measurements(outcome,measurement_tool,domain,self_reported,validated,validation_url,study_ids) ',
          'VALUES($1,$2,$3,$4,$5,$6,$7::jsonb) RETURNING measurement_id'
        ), values)$measurement_id[1])
      } else {
        exec_con(con, paste0(
          'UPDATE measurements SET outcome=$1,measurement_tool=$2,domain=$3,self_reported=$4,validated=$5,',
          'validation_url=$6,study_ids=$7::jsonb WHERE measurement_id=$8'
        ), c(values, list(id)))
        id
      }
    })
    updateSelectInput(session,'edit_measurement',selected=saved_id)
  }))
  study_measurements <- reactive({
    req(isTRUE(auth$logged_in), tables(), input$result_study)
    m <- tables()$measurements
    keep <- vapply(seq_len(nrow(m)),function(i){
      ids <- measurement_study_ids(m$study_ids[i])
      as.character(input$result_study) %in% as.character(ids)
    },logical(1))
    m[keep,,drop=FALSE]
  })
  observe({
    m <- study_measurements();opts <- setNames(as.character(m$measurement_id),paste(m$outcome,m$measurement_tool,sep=' | '))
    updateSelectInput(session,'predictor',choices=opts,selected=isolate(input$predictor))
    updateSelectInput(session,'result_outcome',choices=opts,selected=isolate(input$result_outcome))
  })
  selected_groups <- reactive({
    req(isTRUE(auth$logged_in), tables(), input$result_study);g <- tables()$study_groups
    g <- g[as.character(g$study_id)==as.character(input$result_study),,drop=FALSE]
    g[order(g$group_order),,drop=FALSE]
  })
  # Each selected measurement has its own group-specific descriptive statistics.
  # Inputs are keyed by ROLE and GROUP ID, not measurement ID, so changing
  # predictor/outcome selections does not accidentally reuse values.
  output$result_groups_ui <- renderUI({
    g <- selected_groups()
    req(input$predictor, input$result_outcome)
    if (!nrow(g)) return(tags$p('No groups defined. Create study groups first.'))
    m <- study_measurements()
    label_for <- function(id) {
      x <- m[m$measurement_id == id, , drop = FALSE]
      if (nrow(x)) paste(x$outcome[1], x$measurement_tool[1], sep = ' | ') else as.character(id)
    }
    tagList(lapply(seq_len(nrow(g)), function(i) {
      gid <- as.character(g$group_id[i])
      wellPanel(
        h4(paste('Group', g$group_order[i], ':', g$group_name[i])),
        lapply(c('predictor', 'outcome'), function(role) {
          measurement_id <- if (role == 'predictor') input$predictor else input$result_outcome
          tagList(
            h5(strong(paste0(tools::toTitleCase(role), ': ', label_for(measurement_id)))),
            fluidRow(
              column(4, numericInput(paste0(role, '_mean_', gid), 'Mean', NA)),
              column(4, numericInput(paste0(role, '_sd_', gid), 'SD', NA, min = 0)),
              column(4, numericInput(paste0(role, '_n_', gid), 'n', NA, min = 0))
            )
          )
        })
      )
    }))
  })
  
  observeEvent(input$save_result, safe(function() {
    req(input$result_study, input$predictor, input$result_outcome)
    if (as.character(input$predictor) == as.character(input$result_outcome)) {
      stop('Predictor and outcome must be different measurements for this form.')
    }
    allowed <- as.character(study_measurements()$measurement_id)
    if (!all(c(input$predictor, input$result_outcome) %in% allowed)) {
      stop('Measurements must be linked to the selected study.')
    }
    g <- selected_groups()
    if (!nrow(g)) stop('Define study groups first.')
    stats <- list()
    for (i in seq_len(nrow(g))) {
      gid <- as.character(g$group_id[i])
      for (role in c('predictor', 'outcome')) {
        stats[[length(stats) + 1L]] <- list(
          group_id = gid,
          measurement_id = if (role == 'predictor') input$predictor else input$result_outcome,
          mean = num_or_na(input[[paste0(role, '_mean_', gid)]]),
          sd = num_or_na(input[[paste0(role, '_sd_', gid)]]),
          n = int_or_na(input[[paste0(role, '_n_', gid)]])
        )
      }
    }
    for (s in stats) {
      if (!is.na(s$sd) && s$sd < 0) stop('SD cannot be negative.')
      if (!is.na(s$n) && s$n < 0) stop('n cannot be negative.')
    }
    with_reviewer_transaction(p,auth$user_id,function(con) {
      id <- as.character(query_con(con, paste0(
        'INSERT INTO results(study_id,predictor_measurement_id,outcome_measurement_id,df,beta,r,',
        'unstandardized_coefficient,p_value,effect_size,effect_size_type) ',
        'VALUES($1,$2,$3,$4,$5,$6,$7,$8,$9,$10) RETURNING result_id'
      ), list(input$result_study, input$predictor, input$result_outcome,
              num_or_na(input$df), num_or_na(input$beta), num_or_na(input$r),
              num_or_na(input$b), num_or_na(input$pvalue), num_or_na(input$effect),
              input$effect_type))$result_id[1])
      for (s in stats) {
        # Omit wholly unreported measurement/group combinations.
        if (all(is.na(c(s$mean, s$sd, s$n)))) next
        exec_con(con, paste0(
          'INSERT INTO result_measurement_groups',
          '(result_id,measurement_id,group_id,mean,sd,n) ',
          'VALUES($1,$2,$3,$4,$5,$6)'
        ), list(id, s$measurement_id, s$group_id, s$mean, s$sd, s$n))
      }
    })
  }))
  observeEvent(input$save_matrix,safe(function(){
    req(input$corr_study,nzchar(input$matrix));m <- parse_matrix(input$matrix)
    count <- 0L
    with_reviewer_transaction(p,auth$user_id,function(con) {
      for(i in seq_len(nrow(m))) for(k in seq_len(ncol(m))) if(k>i && !is.na(m[i,k])) {
        exec_con(con,'INSERT INTO correlations(study_id,variable_1,variable_2,correlation) VALUES($1,$2,$3,$4)',
                 list(input$corr_study,rownames(m)[i],colnames(m)[k],as.numeric(m[i,k])))
        count <- count+1L
      }
      if(count==0L) stop('No off-diagonal correlations found.')
    })
  }))
  observeEvent(input$save_qual,safe(function(){
    req(input$qual_study)
    if(!nzchar(trimws(input$theme))) stop('Theme is required.')
    with_reviewer_transaction(p, auth$user_id, function(con) {
      exec_con(con,
        'INSERT INTO qualitative(study_id,theme,theme_description) VALUES($1,$2,$3)',
        list(input$qual_study,input$theme,input$description))
    })
  }))
  for(nm in TABLES) local({name <- nm
  output[[paste0(name,'_view')]] <- renderDT({
    req(isTRUE(auth$logged_in), tables())
    DT::datatable(tables()[[name]], options=list(scrollX=TRUE,pageLength=5))
  })
  })
  output$download <- downloadHandler(
    filename = function() {
      require_reviewer()
      paste0(input$export_table, '_', Sys.Date(), '.csv')
    },
    content = function(file) {
      require_reviewer()
      d <- fetch_all(p, input$export_table)
      d[] <- lapply(d, function(x) {
        if (is.list(x)) {
          vapply(x, function(z) as.character(jsonlite::toJSON(z, auto_unbox=TRUE)), character(1))
        } else x
      })
      utils::write.csv(d, file, row.names=FALSE, na='')
    }
  )
}
shinyApp(ui,server)
