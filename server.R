library(rlist)
library(dplyr)
library(readr)
library(tidyr)
router <- require.r('./router.R')$router
patientRouter <- require.r('./router.R')$patientRouter
filesToList <- require.r('./transpose.R')$filesToList
components <- require.r('./components.R')


server <- function(input, output, session) {
  router$server(input, output, session)
  # patientRouter$server(input, output, session)
  recordingProperty <- reactive({
    if(is.null(input$variableName)){ "my property" }
    else {input$variableName}
  })
  recordingType <- reactive({
    # boolean | number | txt
    
    if (is.null(input$variableType)){ "boolean" }
    else { input$variableType }
  })
  
  patients <- eventReactive({input$dataLoadTrigger; input$file}, {
    file <- input$file
    if (is.null(file) | is.null(input$dataLoadTrigger)) { 
      NULL 
    } else {
      
      raw_data <- purrr::map2(file$datapath, file$name, 
                              ~read.csv(.x) %>% 
                                mutate(Category = .y)) %>% 
        setNames(file$name)
      
      if (!is.null(input$filterFileName) & !is.null(input$filterCode)) {
        raw_data <- raw_data[[input$filterFileName]] %>% 
          filter(eval(parse(text = input$filterCode)))
      }
      
      raw_data %>% 
        bind_rows() %>% 
        group_by(Patient.Id) %>% 
        nest() %>% 
        mutate(new_data = map(data, ~mutate(.x, Patient.Id = Patient.Id) %>% 
                                as.data.frame())) %>% 
        pull(new_data, name = Patient.Id) %>% 
        imap(~list(id = .y, 
                   chartGroups = purrr::map(split(.x, .x$Category), function (chart.group){
                     purrr::map(seq_len(nrow(chart.group)),
                                function(row){as.list(unlist(chart.group[row,]))}
                     )
                   })
        ))
      
    }
  }, ignoreNULL = F)
  
  all_charts_df <- eventReactive({input$dataLoadTrigger; input$file}, {
    file <- input$file
    if (is.null(file) | is.null(input$dataLoadTrigger)) { NULL }
    else {
      all_dfs <- purrr::map(file$datapath, read.csv) %>% 
        setNames(file$name)
    }
  }, ignoreNULL = F)
  
  
  selectedPatient <- reactive({
    pIndex <- get_query_param('patient_id')
    if (is.null(pIndex)){
      NULL
    }
    else if (pIndex > 0){
      patients()[[pIndex]]
    }
    else {
      NULL
    }
    
  })
  selectedChartGroup <- reactive({get_query_param('chart_group')});
  selectedChart <- reactive({get_query_param('chart_id')});
  
  output$patientList <- renderUI({
    patient.list <- patients()
    if (is.null(patient.list)){
      div(class="rounded p-4 text-muted bg-grey-100", "Please upload charts.")
    }
    else {
      div(class="flex flex-col",
          components$patientsList(patient.list, selectedId = selectedPatient()$id)
      )
    }
  })
  
  output$recordConfiguration <- renderUI({
    
    
    # a little bit of a delay to make it user friendly
    tags$form(class="flex flex-col pr-8 gap-2",
              span(class="font-bold text-sidebarHeader mb-2", "Variable"),
              tags$input(onblur="Shiny.setInputValue('variableName', this.value);", class="rounded bg-grey-200 px-3 py-2 focus:outline-none ring-0 border-0 outline-none", type="text", name="variableName", placeholder="Variable Name", value=input$variableName,
                         tags$fieldset(class="flex flex-col border border-solid border-grey-200 p-3", name="variableType", onchange=" Shiny.setInputValue('variableType', event.target.value);",
                                       tags$legend(class="px-1", "Variable Type"),
                                       div(class="flex flex-row gap-2",
                                           tags$input(class="my-auto",
                                                      name="variableType",
                                                      checked={if(recordingType() == "boolean"){ "yes"} else NULL},
                                                      type="radio", id="boolean", value="boolean",
                                                      tags$label("Boolean",
                                                                 `for`="boolean", class="grow my-auto"))),
                                       div(class="flex flex-row gap-2", 
                                           tags$input(class="my-auto", 
                                                      name="variableType", 
                                                      checked={if(recordingType() == "number"){"yes"} else NULL}, 
                                                      type="radio", id="number", value="number", 
                                                      tags$label("Number", `for`="number", class="grow my-auto"))),
                                       div(class="flex flex-row gap-2", 
                                           tags$input(class="my-auto", 
                                                      name="variableType", 
                                                      checked={if(recordingType() == "txt"){"yes"} else NULL}, 
                                                      type="radio", id="txt", value="txt", 
                                                      tags$label("Text", `for`="txt", class="grow my-auto")))
                                       
                         )
              ))
    
  })
  
  # output$recordConfiguration <- renderUI({
  #   
  #   # a little bit of a delay to make it user friendly
  #   tags$form(class = "flex flex-col pr-8 gap-2",
  #             span(class = "font-bold text-sidebarHeader mb-2", "Variable"),
  #             tags$input(
  #               onblur = "Shiny.setInputValue('variableName', this.value);",
  #               class = "rounded bg-grey-200 px-3 py-2 focus:outline-none ring-0 border-0 outline-none",
  #               type = "text",
  #               name = "variableName",
  #               placeholder = "Variable Name",
  #               value = input$variableName
  #             ),
  #             tags$fieldset(
  #               class = "flex flex-col border border-solid border-grey-200 p-3",
  #               name = "variableType",
  #               onchange = "Shiny.setInputValue('variableType', event.target.value);",
  #               tags$legend(class = "px-1", "Variable Type"),
  #               div(
  #                 class = "flex flex-row gap-2",
  #                 selectInput(
  #                   inputId = "variableType",
  #                   label = "Boolean",
  #                   choices = c("NA", "TRUE", "FALSE"),
  #                   selected = if (recordingType() == "boolean") "yes" else NULL
  #                 )
  #               ),
  #               div(
  #                 class = "flex flex-row gap-2",
  #                 tags$input(
  #                   class = "my-auto",
  #                   name = "variableType",
  #                   checked = if (recordingType() == "number") "TRUE" else NULL,
  #                   type = "radio",
  #                   id = "number",
  #                   value = "number",
  #                   tags$label("Number", `for` = "number", class = "grow my-auto")
  #                 )
  #               ),
  #               div(
  #                 class = "flex flex-row gap-2",
  #                 tags$input(
  #                   class = "my-auto",
  #                   name = "variableType",
  #                   checked = if (recordingType() == "txt") "yes" else NULL,
  #                   type = "radio",
  #                   id = "txt",
  #                   value = "txt",
  #                   tags$label("Text", `for` = "txt", class = "grow my-auto")
  #                 )
  #               )
  #             )
  #   )
  #   
  # })
  
  
  variableSelector <- reactive({
    if (is.null(input$filterFileName)) {
      NULL
    } else {
      components$dropdownButton("Variables to filter", colnames(all_charts_df()[[input$filterFileName]]), "filterVariableName")
    }
  })
  
  filterCodeEntry <- reactive({
    if (is.null(input$filterFileName)) {
      tags$span("Select a file")
    } else {
      textInput("filterCode", NULL, "",
                width = '1000px')    }
  })
  
  stringHighlightEntry <- reactive({
    textInput("chartSearchString", NULL, "",
              width = '800px')    
  })
  
  output$settingsBody <- renderUI({
    div(class="flex flex-col w-full grow gap-4 px-24 pt-24",
        fileInput(
          'file',
          '',
          multiple = TRUE,
          accept = '.csv',
          width = NULL,
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),
        
        div(class="flex flex-row px-28",
            tags$button(class="ml-12 px-4 py-2 bg-primary rounded text-white", 
                        onclick="Shiny.setInputValue('dataLoadTrigger', (new Date).toUTCString()); console.log(`yo`)", "Update Data")),
        
        # Filtering area
        tags$form(class="flex flex-col pr-8 gap-2",
                  span(class="font-bold text-sidebarHeader mb-2", "Filter data"),
                  filterCodeEntry(),
                  variableSelector(),
                  components$dropdownButton("File to filter", names(all_charts_df()), "filterFileName")
        ),
        
        # String highlight area
        tags$form(class="flex flex-col pr-8 gap-2",
                  span(class="font-bold text-sidebarHeader mb-2", "String to highlight"),
                  stringHighlightEntry()
        ),
        
        div(class="flex flex-row w-full divide-x-2 divide-grey-200 p-5 rounded bg-white focus-within:shadow-lg transition transition-all",
            #div(class="px-28","omg file names"), # for renaming the files
            uiOutput("recordConfiguration"), # for inputting the input types
            div(class="pl-8 flex flex-col",
                span(class="font-bold text-sidebarHeader mb-4", "Preview"),
                div(class="flex flex-row justify-center gap-2",
                    span(class="font-medium mr-auto my-auto", recordingProperty()),
                    switch(recordingType(),
                           boolean=components$switchInput(),
                           number=components$numberInput(),
                           txt=components$txtInput())
                )
            )
        )
    )
  })
  
  recordedData <- reactiveVal(list());
  observeEvent(input$recordingTime, {
    the.data <- recordedData()
    the.data[[get_query_param("chart_group")]][[get_query_param('chart_id')]] <- list(
      data=input$recordingData,
      date=input$recordingTime
    )
    recordedData(the.data)
  })
  output$recordDataSidebar <- renderUI({
    # print(recordedData())
    value <- isolate({ recordedData()})[[selectedChartGroup()]][[selectedChart()]][['data']]
    # if (length(value) == 0) {
    #   value <- NA
    # }
    print(paste0("value: ", value))
    div(class="flex flex-row justify-center gap-2 mt-4",
        span(class="font-medium mr-auto my-auto", recordingProperty()),
        switch(recordingType(),
               # boolean = components$switchInput(checked= { if (isTruthy(value)){TRUE} else NULL }, 
               #                                oninput="Shiny.setInputValue('recordingData', this.value == 'on'); Shiny.setInputValue('recordingTime', (new Date).toUTCString())"),
               boolean = components$switchInput(checked = value), 
                                                # oninput="if (this.checked) {Shiny.setInputValue('recordingData', this.value);} else if (this.value === 'NA') {Shiny.setInputValue('recordingData', NA);} else {Shiny.setInputValue('recordingData', NULL);} Shiny.setInputValue('recordingTime', (new Date).toUTCString())"),
                                                # oninput="if (this.checked) {Shiny.setInputValue('recordingData', TRUE);} else if (this.value === 'NA') {Shiny.setInputValue('recordingData', NA);} else {Shiny.setInputValue('recordingData', NULL);} Shiny.setInputValue('recordingTime', (new Date).toUTCString())"),
                                                #oninput="if (this.checked) {Shiny.setInputValue('recordingData', TRUE);} else if (this.value === 'NA') {Shiny.setInputValue('recordingData', NA);} Shiny.setInputValue('recordingTime', (new Date).toUTCString())"),
        
        # oninput="Shiny.setInputValue('recordingData', this.value); Shiny.setInputValue('recordingTime', (new Date).toUTCString())"),
        number = components$numberInput(value = value, 
                                        oninput="Shiny.setInputValue('recordingData', this.value); Shiny.setInputValue('recordingTime', (new Date).toUTCString())"),
        txt = components$txtInput(value = value, 
                                  oninput="Shiny.setInputValue('recordingData', this.value); Shiny.setInputValue('recordingTime', (new Date).toUTCString())")
    )) })

flaggedCharts <- reactiveVal(list());
observeEvent(input$flaggingTime, {
  the.data <- flaggedCharts();
  the.data[[get_query_param("chart_group")]][[get_query_param('chart_id')]] <- list(
    data=input$flagReason,
    date=input$flaggingTime
  )
  flaggedCharts(the.data)
})

output$flagButton <- renderUI({
  value <- flaggedCharts()[[selectedChartGroup()]][[selectedChart()]][['data']]
  needUpdate <- { is.null(value) || input$flagReason != value }
  tags$button(class=paste({ if (needUpdate) "bg-[#FEA725]" else "bg-[#67D292]"}, "text-white flex flex-row px-3 py-2 ml-auto rounded mt-2 gap-2"),
              onclick="Shiny.setInputValue('flaggingTime', (new Date).toUTCString());",
              rheroicon(ifelse(needUpdate, "flag", "check_circle"), "solid", class="h-5 w-5 my-auto"),
              span(class="my-auto", ifelse(needUpdate, "Flag", "Flagged"))
  )
})

output$flagChartSidebar <- renderUI({
  value <- flaggedCharts()[[selectedChartGroup()]][[selectedChart()]][['data']]
  div(class="flex flex-col",
      tags$textarea(class="rounded bg-grey-200 outline-none ring-0 focus:outline-none border-0 px-3 py-2", placeholder="Reason", oninput="Shiny.setInputValue('flagReason', this.value)", value),
      uiOutput('flagButton')
  )
})

output$generatedData <- renderUI({
  # 1. obtain file data
  file <- input$file #isolate({ input$file })
  if (is.null(file)){
    return(NULL)
  }
  record <- recordedData()
  recordName <- recordingProperty()
  recordType <- recordingType()
  flag <- flaggedCharts()
  files <- purrr::map(seq_len(nrow(as.matrix(file))), function(n) {
    # a string and a data.frame
    f <- list(name = file[n, 'name'], data = read.csv(file[n, 'datapath']))
    defaultValue <- switch(recordType,
                           boolean=NA,
                           number=0,
                           txt="",
                           FALSE
    )
    f$data <- mutate(f$data, thing=defaultValue, thing_date="", flag=FALSE, flag_reason="", flag_date="")
    if (!is.null(record[[f$name]])){
      for (chart.id in names(record[[f$name]])){
        chart.data <- record[[f$name]][[chart.id]]
        f$data[f$data$Chart.ID==chart.id,][['thing']] <- chart.data$data
        f$data[f$data$Chart.ID==chart.id,][['thing_date']] <- chart.data$date
      }
    }
    if (!is.null(flag[[f$name]])){
      for (chart.id in names(flag[[f$name]])){
        chart.data <- flag[[f$name]][[chart.id]]
        f$data[f$data$Chart.ID==chart.id,][['flag']] <- TRUE
        f$data[f$data$Chart.ID==chart.id,][['flag_reason']] <- chart.data$data
        f$data[f$data$Chart.ID==chart.id,][['flag_date']] <- chart.data$date
      }
    }
    names(f$data)[names(f$data) == 'thing'] <- recordName
    names(f$data)[names(f$data) == 'thing_date'] <- paste0(recordName, '_date')
    # convert to csv string
    f$data <- format_csv(f$data)
    f
  })
  downloadHandler(
    filename = function() {
      paste("output", "zip", sep=".")
    },
    content = function(fname) {
      fs <- c()
      tmpdir <- tempdir()
      setwd(tmpdir)
      for (f in files) {
        path <- f$name
        fs <- c(fs, path)
        write(f$data, path)
      }
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip",
    outputArgs = list(class="w-full bg-grey-100 flex flex-row gap-3 px-4 py-3 rounded font-medium text-sidebar items-center")
  )
})

# counts the number of matches of a string
output$chartMatches <- renderUI({
  chart <- Filter(function(x){x$Chart.ID == selectedChart()}, selectedPatient()$chartGroups[[selectedChartGroup()]])[[1]]
  if (is.null(input$chartSearchString)) { "No "} else matches <- str_count(chart$Text, input$chartSearchString)
  span(class="text-muted", paste(matches, "matches"))
})

output$body <- renderUI({
  components$patientView(selectedPatient(), selectedChartGroup(), selectedChart(), input$chartSearchString,
                         all_patients = patients())
})

chartSearchResults <- reactive({
  
  filterFunc <- function(charts, query){
    list.filter(
      charts,
      tryCatch({grepl(query, Text, ignore.case = T)}, #grepl(regex(query, ignore_case = T), Text, fixed = !input$useRegex) },
               error = function(e) {grepl(query, Text, ignore.case = T)},
               warning = function(e) {grepl(query, Text, ignore.case = T)}
               # error= function(cond){ grepl(regex(query, ignore_case = T), Text, fixed = TRUE) },
               # warning= function(cond){ grepl(regex(query, ignore_case = T), Text, fixed = TRUE) }
      )
    )
  }
  
  searchString <- if (is.null(input$patientSearchString)) "" else input$patientSearchString
  chart.group <- selectedChartGroup()
  patient <- selectedPatient()
  
  if (is.null(chart.group)){
    # ALL tab
    # note that the output format is different for the ALL tab
    purrr::map(patient$chartGroups, function(curr.chart.group){
      filterFunc(curr.chart.group, searchString)
    })
  }
  else {
    # normal tab
    charts <- patient$chartGroups[[chart.group]]
    if (is.null(charts)){
      NULL
    }
    else {
      filterFunc(charts, searchString)
    }
  }
  
  
  
})

output$searchSpaceRender <- renderUI({
  chart.group <- selectedChartGroup();
  res <- chartSearchResults()
  if (is.null(res)){
    # the search results are null... because there's no charts?
    # todo: investigate whether this needs a special case for is.null(chart.group)
    components$searchSpace(selectedPatient(), selectedPatient()$chartGroups[[selectedChartGroup()]])
  }
  else {
    components$searchSpace(selectedPatient(), res, all=is.null(chart.group))
  }
})


# a list of dataframes ready for export


NULL


}

exports$server <- server