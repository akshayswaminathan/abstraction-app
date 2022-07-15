library(rlist)
library(dplyr)
router <- require.r('./router.R')$router
filesToList <- require.r('./transpose.R')$filesToList
components <- require.r('./components.R')


server <- function(input, output, session) {
  router$server(input, output, session)

  patients <- reactive({
    file <- input$file
    do.call(filesToList, purrr::map(seq_len(nrow(as.matrix(file))), function(n){
      f <- list(name=file[n,'name'], data=read.csv(file[n, 'datapath']))
      # if (n==1) { str(f$data) }
      f
    }))
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
  selectedChart <- reactive({get_query_param('chart_group')})

  output$patientList <- renderUI({
    components$patientsList(patients(), selectedId = selectedPatient()$id)
  })

  output$body <- renderUI({
    components$patientView(selectedPatient(), selectedChart())
  })
}

exports$server <- server