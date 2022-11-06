library(shiny)
library(shiny.router)
library(shiny.tailwind)
library(stringr)
library(purrr)
library(commonr)
library(rheroicons)
library(marker)
options(shiny.autoreload = TRUE, 
        shiny.port=3300,
        shiny.maxRequestSize = 50 * 1024^2)
print(getOption("shiny.autoreload"))
server <- require.r('./server.R')$server
ui <- require.r('./ui.R')$ui


# Create Shiny app ----
app <- shinyApp(ui = ui, server = server)
runApp(app)
