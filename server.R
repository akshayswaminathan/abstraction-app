router <- require.r('./router.R')$router
library(rlist)
components <- require.r('./components.R')
server <- function(input, output, session) {
  router$server(input, output, session)

  patients <- list(list(id="1", charts=list("Hello", "World")),
                   list(id="2", charts=list("Goodnight", "Moon")))
  selectedPatient <- reactive({
    a <- list.find(patients, id == get_query_param('patient_id'), 1)
    if (length(a) == 0){
      NULL
    }
    else {
      a[[1]]
    }
  })
  output$patientList <- renderUI({
    components$patientsList(patients, selectedId = selectedPatient()$id)
  })

  output$body <- renderUI({
    components$patientView(selectedPatient())
  })
}

exports$server <- server