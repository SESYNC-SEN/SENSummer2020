# Packages
library(...)
library(...)

# Data
popdata <- read.csv('data/citypopdata.csv')

# User Interface
in1 <- selectInput(
  inputId = 'selected_city',
  label = 'Select a city',
  choices = unique(popdata[['NAME']])
)

out1 <- textOutput('city_label')
...
tab1 <- tabPanel(
  title = 'City Population',
  in1, out1, ...)
ui <- navbarPage(
  title = 'Census Population Explorer',
  tab1)

# Server
server <- function(input, output) {
  output[['city_label']] <- renderText({
    input[['selected_city']]
  })
  output[['city_plot']] <- ...({
    df <- popdata %>%
      ...(NAME == ...)
    ...(..., aes(x = year, y = population)) +
      ...()
  })
}

# Create the Shiny App
shinyApp(ui = ui, server = server)
