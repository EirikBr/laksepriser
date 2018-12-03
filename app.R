#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

list.of.packages <- c("shiny", "tidyverse", "readr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)



library(shiny)

library(tidyverse)

library(readr)



# load data

eksport <- read_delim("http://data.ssb.no/api/v0/dataset/1120.csv?lang=no",
                      
                      ";", escape_double = FALSE, trim_ws = TRUE)


library(httr)
library(tidyverse)
library(ggplot2)
# use rjstat JSON-stat library
library(rjstat)
# bruk dplyr for Ã¥ rydde tabeller og data
library(dplyr)
# bruk writexl for Ã¥ eksportere til excel
#library(writexl)
# Adress to laks  JSON-Stat dataset for laksestat - Index 
url <- "https://data.ssb.no/api/v0/no/table/03024/"
data <- '{
"query": [],
"response": {
"format": "json-stat"
}
}'
d.tmp <- POST(url , body = data, encode = "json", verbose())

# get content from d.tmp as text, using fromJSONstat
laks <- fromJSONstat(content(d.tmp, "text"))

# definere tabell som frame
laks2 <- data.frame(laks)

# sette enklere navn pÃ¥ kolonner
colnames(laks2) <- c("gruppe", "enhet", "uke", "tall")

# definere og endre  kolonner og radnavn for Ã¥ lette hÃ¥ndtering
  laks2$enhet <- as.character(laks2$enhet)
  laks2$tall <- as.numeric(laks2$tall)
  laks2$enhet[laks2$enhet == "Vekt (tonn)"] <- "tonn"
  laks2$enhet[laks2$enhet == "Kilopris (kr)"] <- "pris"
  laks2$gruppe[laks2$gruppe == "Fersk oppalen laks"] <- "fersk"
  laks2$gruppe[laks2$gruppe == "Frosen oppalen laks"] <- "frosen"
  
# skrive tabell som inneholder data om fersk fisk
  fersk <- laks2 %>% filter(gruppe %in% "fersk") %>%
  select(enhet, uke, tall)
  
# skrive tabell som inneholder data om frossen fisk
  frosen <- laks2 %>% filter(gruppe %in% "frosen") %>%
  select(enhet, uke, tall)
  
# rydde i tabellene
  fersk2 <- reshape(fersk, idvar = "uke", timevar = "enhet", direction = "wide")
  frosen2 <- reshape(frosen, idvar = "uke", timevar = "enhet", direction = "wide")
  colnames(fersk2) <- c("uke", "fersk_tonn", "fersk_pris")
  colnames(frosen2) <- c("uke", "frosen_tonn", "frosen_pris")
# slÃ¥ sammen fersk og frossen fisk
  
#samlet_laks <- data.frame()
  samletlaks <- cbind(fersk2, frosen2)
  colnames(samletlaks) <- c("uke", "fersk_tonn", "fersk_pris", "uke_s", "frosen_tonn", "frosen_pris")
  
# sletter en unÃ¸dvendig kolonne
  samletlaks$uke_s <- NULL


aar <- data.frame()
as.integer(aar <- substr(samletlaks$uke, 1, 4))
samletlaks2 <- cbind(aar, samletlaks)
head (samletlaks2, 10)

snitt_data <- data.frame()


sort_aar <- group_by(samletlaks2, aar)

#SUMMERER PRIS FERSK LAKS
snitt_fersk <- as.integer()

summarize(samletlaks2, snitt_fersk = mean(fersk_pris))

ferskpris <- summarize(sort_aar, snitt_fersk = mean(fersk_pris))

#SUMMERER VOLUM fersk LAKS

summarize(samletlaks2, fersk_tonn = mean(fersk_tonn))


fersktonn <- summarize(sort_aar, fersk_tonn = mean(fersk_tonn))

gjennomsnitt <- merge(fersktonn, ferskpris)


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
    
    ggplot(data=gjennomsnitt,aes(x=aar,y=snitt_fersk))+ 
      geom_point()
  
    
    
  })
  
}



# Run the application 

shinyApp(ui = ui, server = server)
