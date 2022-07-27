library(rlist)
library(dplyr)
router <- require.r('./router.R')$router
patientRouter <- require.r('./router.R')$patientRouter
filesToList <- require.r('./transpose.R')$filesToList
components <- require.r('./components.R')


server <- function(input, output, session) {
  router$server(input, output, session)
  # patientRouter$server(input, output, session)
  recordingProperty <- reactive({ input$variableName })
  recordingType <- reactive({
    if (is.null(input$variableType)){ "boolean" }
    else { input$variableType }
  })
  patients <- eventReactive(input$dataLoadTrigger, {
    file <- input$file
    if (is.null(file)) { NULL }
    else {
      do.call(filesToList, purrr::map(seq_len(nrow(as.matrix(file))), function(n) {
        f <- list(name = file[n, 'name'], data = read.csv(file[n, 'datapath']), record = list(name = recordingProperty(), type = recordingType()))
        f
      }))
    }
  }, ignoreNULL = T)
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
  recordedData <- reactiveVal(list());
  flaggedCharts <- reactiveVal(list());

  observeEvent(input$recordingTime, {
    recordedData(append(recordedData(), list(list(
      patient=get_query_param('patient_id'),
      chart=get_query_param('chart_id'),
      group=get_query_param("chart_group"),
      data=input$recordingData
    ))))
  })

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
    tags$form(class="flex flex-col px-8 gap-2", onchange="Shiny.setInputValue('variableName', this.variableName.value); Shiny.setInputValue('variableType', this.variableType.value)",
        span(class="font-bold text-sidebarHeader mb-2", "Variable"),
        tags$input(class="rounded bg-grey-200 px-3 py-2 focus:outline-none ring-0 border-0 outline-none", type="text", name="variableName", placeholder="Variable Name", value=recordingProperty()),
        tags$fieldset(class="flex flex-col border border-solid border-grey-200 p-3", name="variableType",
          tags$legend(class="px-1", "Variable Type"),
          div(class="flex flex-row gap-2", tags$input(class="my-auto", name="variableType", checked={if(recordingType() == "boolean"){ "yes"} else NULL}, type="radio", id="boolean", value="boolean", tags$label("Boolean", `for`="boolean", class="grow my-auto"))),
          div(class="flex flex-row gap-2", tags$input(class="my-auto", name="variableType", checked={if(recordingType() == "number"){"yes"} else NULL}, type="radio", id="number", value="number", tags$label("Number", `for`="number", class="grow my-auto"))),
          div(class="flex flex-row gap-2", tags$input(class="my-auto", name="variableType", checked={if(recordingType() == "txt"){"yes"} else NULL}, type="radio", id="txt", value="txt", tags$label("Text", `for`="txt", class="grow my-auto")))
        ),
        tags$button(type="submit", class="rounded bg-white px-3 py-2", "Save")
    )
  })

  output$recordDataSidebar <- renderUI({
    div(class="flex flex-row justify-center gap-2 mt-4",
        span(class="font-medium mr-auto my-auto", recordingProperty()),
        switch(recordingType(),
               boolean=components$switchInput(oninput="Shiny.setInputValue('recordingData', this.value === 'on'); Shiny.setInputValue('recordingTime', Date.now())"), number=components$numberInput(),
               txt=tags$input(class="rounded px-2 py-1 bg-grey-200")
        )
    )
  })

  output$flagChartSidebar <- renderUI({
    div(class="flex flex-col",
      span(class="font-bold text-sidebarHeader my-3", "Flag Chart"),
        tags$textarea(class="rounded bg-grey-200 outline-none ring-0 focus:outline-none border-0 px-3 py-2", placeholder="Reason"),
        tags$button(class="bg-[#FEA725] text-white flex flex-row px-3 py-2 ml-auto rounded mt-2",
            rheroicon("flag", "solid", class="h-5 w-5"),
            "Flag"
        )
    )
  })

  output$settingsBody <- renderUI({
    div(class="flex flex-col w-full grow gap-4",
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
          div(class="px-28","omg file names"), # for renaming the files
          uiOutput("recordConfiguration"), # for inputting the input types
          div(class="px-8 flex flex-col",
            span(class="font-bold text-sidebarHeader mb-4", "Preview"),
              div(class="flex flex-row justify-center gap-2",
             span(class="font-medium mr-auto my-auto", recordingProperty()),
             switch(recordingType(),
              boolean=components$switchInput(),
              number=components$numberInput(),
              txt=tags$input(class="rounded px-2 py-1 bg-grey-200"))
             )
          )
      ),
    div(class="flex flex-row px-28",
      tags$button(class="ml-auto px-4 py-2 bg-primary rounded text-white", onclick="Shiny.setInputValue('dataLoadTrigger', Date.now()); console.log(`yo`)", "Load Data")
    )
    )
  })

  output$body <- renderUI({
    components$patientView(selectedPatient(), selectedChartGroup(), selectedChart())
  })

  chartSearchResults <- reactive({
    charts <- selectedPatient()$chartGroups[[selectedChartGroup()]]
    searchString <- if (is.null(input$patientSearchString)) "" else input$patientSearchString;
    if (is.null(charts)){
      NULL
    }
    else {
      list.filter(
      charts,
      str_detect(Text, searchString)
    )
    }

  })

  output$searchSpaceRender <- renderUI({
    res <- chartSearchResults()
    if (is.null(res)){
      components$searchSpace(selectedPatient(), selectedPatient()$chartGroups[[selectedChartGroup()]])
    }
    else {
      components$searchSpace(selectedPatient(), res)
    }
  })


}

exports$server <- server