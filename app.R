#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

list.of.packages <- c("shiny", "tidyverse", "readr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(tidyverse)
library(readr)

# load data
eksport <- read_delim("http://data.ssb.no/api/v0/dataset/1120.csv?lang=no",
";", escape_double = FALSE, trim_ws = TRUE)



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Eksport av fersk og frosset oppdrettslaks"),
   
   fluidRow(
     column(3,
            checkboxGroupInput("varegruppe",
                              h3("Velg varegruppe"),
                              choices = list("Frosset laks" = "",
                                             "Fersk laks" = eksport$varegruppe),
                              selected = 1)),
     column(3,
            dateRangeInput("uke",
                      h3("Velg tidsperiode"))
            ),
     mainPanel(plotOutput(outputId = "barplot"))
   )
)

# Define server logic required to draw a plot
server <- function(input, output) {
  output$barplot <- renderPlot({
    ggplot(data = eksport, aes(
      x = eksport$uke, 
      y = eksport$`Eksport av oppalen laks, etter varegruppe, uke og statistikkvariabel`)
    ) + 
      geom_col() +
      facet_grid(statistikkvariabel ~ varegruppe)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

