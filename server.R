library(rlist)
library(dplyr)
router <- require.r('./router.R')$router
patientRouter <- require.r('./router.R')$patientRouter
filesToList <- require.r('./transpose.R')$filesToList
components <- require.r('./components.R')


server <- function(input, output, session) {
  router$server(input, output, session)
  #patientRouter$server(input, output, session)

  patients <- reactive({
    file <- input$file
    if (is.null(file)){
      NULL
    }
    else {
      do.call(filesToList, purrr::map(seq_len(nrow(as.matrix(file))), function(n){
        f <- list(name=file[n,'name'], data=read.csv(file[n, 'datapath']))
        f
      }))
    }
  })
  #patients <- list(list(id="1", chartGroups=list(list(name="Hello", charts=list("a", "b")), "World")),
  #                 list(id="2", chartGroups=list("Goodnight", "Moon")))

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
  selectedChartGroup <- reactive({get_query_param('chart_group')})

  selectedChart <- reactive({get_query_param('chart_id')})

  output$patientList <- renderUI({
    patient.list <- patients()
    if (is.null(patient.list)){
      div(class="rounded p-4 text-muted bg-grey-100", "Please upload charts.")
    }
    else {
      components$patientsList(patient.list, selectedId = selectedPatient()$id)
    }
  })

  recordDataList <- list(
    list(name="Value One", type="boolean"),
    list(name="Value Two", type="number")
  )

  output$recordDataSidebar <- renderUI({
    do.call(div,
            c(class="flex flex-col gap-4 mt-4",
              purrr::map(recordDataList,
                         function(value.definition){
                           div(class="flex flex-row justify-center gap-2",
                               span(class="font-medium mr-auto my-auto", value.definition$name),
                               switch(value.definition$type,
                                      boolean=components$switchInput(), number=components$numberInput(),
                                      text=tags$input(class="rounded bg-grey-200"))
                           )
                         })
            ))
  })

  output$settingsBody <- renderUI({
    div(class="flex flex-col",
    fileInput(
      'file',
      '',
      multiple = TRUE,
      accept = '.csv',
      width = NULL,
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    ),
      div(class="flex flex-row divide-x-2 divide-grey-200",
          div(), # for renaming the files
          div(), # for inputting the input types
      ),
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
      str(res)
      components$searchSpace(selectedPatient(), res)
    }
  })


}

exports$server <- server