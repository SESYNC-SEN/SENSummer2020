# Data
popdata <- ...('data/citypopdata.csv')

# User Interface
in1 <- selectInput(
  ...,
  label = 'Select a city',
  choices = ...)
...
tab1 <- ...(
  ...
  ...)
ui <- navbarPage(
  title = 'Census Population Explorer',
  ...)

# Server
server <- function(...) {
  ... <- ...({
    input[[...]]
  })
}

# Create the Shiny App
shinyApp(ui = ui, server = server)
