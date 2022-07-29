library(rlist)
library(dplyr)
library(readr)
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
    if (is.null(file) | is.null(input$dataLoadTrigger)) { NULL }
    else {
      do.call(filesToList, purrr::map(seq_len(nrow(as.matrix(file))), function(n) {
        f <- list(name = file[n, 'name'], data = read.csv(file[n, 'datapath']), record = list(name = recordingProperty(), type = recordingType()))
        f
      }))
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
          div(class="flex flex-row gap-2", tags$input(class="my-auto", name="variableType", checked={if(recordingType() == "boolean"){ "yes"} else NULL}, type="radio", id="boolean", value="boolean", tags$label("Boolean", `for`="boolean", class="grow my-auto"))),
          div(class="flex flex-row gap-2", tags$input(class="my-auto", name="variableType", checked={if(recordingType() == "number"){"yes"} else NULL}, type="radio", id="number", value="number", tags$label("Number", `for`="number", class="grow my-auto"))),
          div(class="flex flex-row gap-2", tags$input(class="my-auto", name="variableType", checked={if(recordingType() == "txt"){"yes"} else NULL}, type="radio", id="txt", value="txt", tags$label("Text", `for`="txt", class="grow my-auto")))
        )
    ))
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
    div(class="flex flex-row justify-center gap-2 mt-4",
        span(class="font-medium mr-auto my-auto", recordingProperty()),
        switch(recordingType(),
               boolean=components$switchInput(oninput="Shiny.setInputValue('recordingData', this.value === 'on'); Shiny.setInputValue('recordingTime', (new Date).toUTCString())"),
               number=components$numberInput(oninput="Shiny.setInputValue('recordingData', this.value); Shiny.setInputValue('recordingTime', (new Date).toUTCString())"),
               txt=components$txtInput(oninput="Shiny.setInputValue('recordingData', this.value); Shiny.setInputValue('recordingTime', (new Date).toUTCString())"))) })

  flaggedCharts <- reactiveVal(list());
  observeEvent(input$flaggingTime, {
    the.data <- flaggedCharts();
    the.data[[get_query_param("chart_group")]][[get_query_param('chart_id')]] <- list(
      data=input$flagReason,
      date=input$flaggingTime
    )
    flaggedCharts(the.data)
  })
  output$flagChartSidebar <- renderUI({
    div(class="flex flex-col",
        tags$textarea(class="rounded bg-grey-200 outline-none ring-0 focus:outline-none border-0 px-3 py-2", placeholder="Reason", oninput="Shiny.setInputValue('flagReason', this.value)"),
        tags$button(class="bg-[#FEA725] text-white flex flex-row px-3 py-2 ml-auto rounded mt-2",
            onclick="Shiny.setInputValue('flaggingTime', (new Date).toUTCString());",
            rheroicon("flag", "solid", class="h-5 w-5 my-auto"),
            span(class="my-auto", "Flag")
        )
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
          boolean=FALSE,
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
    div(class="flex flex-row w-full divide-x-2 divide-grey-200",
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
      ),
    div(class="flex flex-row px-28",
      tags$button(class="ml-12 px-4 py-2 bg-primary rounded text-white", onclick="Shiny.setInputValue('dataLoadTrigger', (new Date).toUTCString()); console.log(`yo`)", "Load Data"))

    )
  })

  output$body <- renderUI({
    components$patientView(selectedPatient(), selectedChartGroup(), selectedChart())
  })

  chartSearchResults <- reactive({
    searchString <- if (is.null(input$patientSearchString)) "" else input$patientSearchString;
    filterFunc <- function(charts, query){
      list.filter(
        charts,
        tryCatch({ grepl(regex(query, ignore_case = T), Text, fixed = !input$useRegex) },
                 error= function(cond){ grepl(regex(query, ignore_case = T), Text, fixed = TRUE) },
                 warning= function(cond){ grepl(regex(query, ignore_case = T), Text, fixed = TRUE) }
        )
      )
    }

    chart.group <- selectedChartGroup();
    patient <- selectedPatient();

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