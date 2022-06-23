library(shiny)
library(shiny.tailwind)
library(stringr)
library(commonr)
options(shiny.autoreload.pattern = glob2rx("ui.R"), shiny.port=3300)
print(getOption("shiny.autoreload.pattern"))
server = require.r('./server.R')$server
ui <- require.r('./ui.R')$ui


# Create Shiny app ----
app = shinyApp(ui = ui, server = server)
runApp(app)