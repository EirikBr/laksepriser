#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)

# load data
eksport <- read_delim("http://data.ssb.no/api/v0/dataset/1122.csv?lang=no",
";", escape_double = FALSE, trim_ws = TRUE)



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Eksport av fersk og frosset oppdrettslaks"),
   
   fluidRow(
     column(3,
            checkboxGroupInput("varegruppe",
                              h3("Velg varegruppe"),
                              choices = list("Frosset laks" = 1,
                                             "Fersk laks" = 2),
                              selected = 1)),
     column(3,
            dateRangeInput("uke",
                      h3("Velg tidsperiode"))
            ),
     mainPanel(plotOutput(outputId = "scatterplot"))
   )
)

# Define server logic required to draw a plot
server <- function(input, output) {
  output$scatterplot <- renderPlot({
    ggplot(data = eksport, 
           aes_string(x = eksport$uke, y = eksport$`Eksport av oppalen laks, etter varegruppe, uke og statistikkvariabel`)) + 
      geom_point()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

