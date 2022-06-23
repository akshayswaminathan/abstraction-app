
server <- function(input, output) {

  output$selected_var <- renderTable({
    if (!isTruthy(input$fileContent)){
      list()
    }
    else {
      datums = str_split(input$fileContent, ",")[[1]]
    if (isTruthy(input$searchString)){
          str_subset(datums, regex( input$searchString, ignore_case = TRUE))
    }
    else {
      datums
    }}

  })

}

exports$server = server