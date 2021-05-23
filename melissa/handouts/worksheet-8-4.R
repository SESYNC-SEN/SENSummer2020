# Packages
library(dplyr)
library(ggplot2)

# Data
popdata <- read.csv('data/citypopdata.csv')

# User Interface
in1 <- selectInput(
  inputId = 'selected_city',
  label = 'Select a city',
  choices = unique(popdata[['NAME']])
)
in2 <- ...(
  ... = 'my_xlims',
  ... = "Set X axis limits",
  ...,
  ...,
  ...)
side <- sidebarPanel('Options', in1, ...)
out1 <- textOutput('city_label')
out2 <- plotOutput('city_plot')
...
main <- mainPanel(out1, out2, ...)
tab <- tabPanel(
  title = 'City Population',
  sidebarLayout(side, main))
ui <- navbarPage(
  title = 'Census Population Explorer',
  tab)

# Server
server <- function(input, output) {

  slider_years <- reactive({
    ...
    ...
  })
  
  output[['city_label']] <- renderText({
    input[['selected_city']]
  })
  
  output[['city_plot']] <- renderPlot({
    df <- popdata %>% 
      filter(NAME == input[['selected_city']]) %>%
      filter(year %in% ...)
    ggplot(df, aes(x = year, y = population)) + 
      geom_line()
  })
  
  output[['city_table']] <- renderDataTable({
    df <- popdata %>% 
      dplyr::filter(NAME == input[['selected_city']]) %>%
      filter(year %in% ...)
    df
  })
}

# Create the Shiny App
shinyApp(ui = ui, server = server)
