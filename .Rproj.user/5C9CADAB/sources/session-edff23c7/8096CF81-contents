#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
library(tidyverse)
library(ggplot2)
library(plotly)

#Load Local Data 2018
whr2018 <- read.csv("2018.csv")
mapdata<- read.csv("mapdata.csv")
mapdata <- left_join(mapdata, whr2018, by=c("region"="Country.name"))
mapdata_2018 <- mapdata %>% filter(!is.na(mapdata$Ladder.score))

#Load Local Data 2019
whr2019 <- read.csv("2019.csv")
mapdata<- read.csv("mapdata.csv")
mapdata <- left_join(mapdata, whr2019, by=c("region"="Country.name"))
mapdata_2019 <- mapdata %>% filter(!is.na(mapdata$Ladder.score))

#Load Local Data 2020
whr2020 <- read.csv("2020.csv")
mapdata<- read.csv("mapdata.csv")
mapdata <- left_join(mapdata, whr2020, by=c("region"="Country.name"))
mapdata_2020 <- mapdata %>% filter(!is.na(mapdata$Ladder.score))

#Load Local Data 2021
whr2021 <- read.csv("2021.csv")
mapdata <- read.csv("mapdata.csv")
mapdata <- left_join(mapdata, whr2021, by=c("region"="Country.name"))
mapdata_2021 <- mapdata %>% filter(!is.na(mapdata$Ladder.score))

#Load Local Data 2022
whr2022 <- read.csv("2022.csv")
mapdata<- read.csv("mapdata.csv")
mapdata <- left_join(mapdata, whr2022, by=c("region"="Country"))
mapdata_2022 <- mapdata %>% filter(!is.na(mapdata$Ladder.score)) 

#Load Local Data 2023
whr2023 <- read.csv("2023.csv")
mapdata<- read.csv("mapdata.csv")
mapdata <- left_join(mapdata, whr2023, by=c("region"="Country.name"))
mapdata_2023 <- mapdata %>% filter(!is.na(mapdata$Ladder.score))



library(shiny)

ui <- fluidPage(
  titlePanel("Global Happiness Level Over Time"),
  sidebarLayout(
    position = "right",
    sidebarPanel (
      h3("2018-2023"),
      sliderInput(inputId = "input_years",
                  label = "years",
                  min=2018,
                  max=2023,
                  value=2020)),
    
    mainPanel(
      plotOutput (outputId = "output_ghraph")
    )
  )
)
server <- function(input, output){
  output$output_ghraph<- renderPlot({
    mapdata_shiny <- if (input$input_years == "2018") {
      mapdata_2018
    } else if (input$input_years == "2019") {
      mapdata_2019
    } else if (input$input_years == "2020") {
      mapdata_2020
    } else if (input$input_years == "2021") {
      mapdata_2021
    } else if (input$input_years == "2022") {
      mapdata_2022
    } else if (input$input_years == "2023") {
      mapdata_2023
    } else {
      NULL
    }
    
    ggplot(mapdata_shiny, 
           aes(x=long, y=lat,text = region,
               group=group)) +
      scale_fill_viridis_b(name="Happiness Score",breaks = seq(0, 8, by = 0.5))+
      geom_polygon(aes(fill=Ladder.score), color="black")+
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            rect = element_blank())+
      labs(title = "World Happiness Level")
    
  })
}


shinyApp(ui, server)
