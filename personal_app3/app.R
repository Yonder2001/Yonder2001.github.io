library(shiny)
library(tidyverse)

#Load data
Full_Report_of_Factors <- read.csv("Final_Happiness_Score.csv")
GDP_long <- Full_Report_of_Factors %>%
  pivot_longer(cols = starts_with("GDP"), names_to = "Years", values_to = "GDP per capita")
GDP_long <-GDP_long[,c("Country","Years","GDP per capita")]

SS_long <-Full_Report_of_Factors %>%
  pivot_longer(cols = starts_with("Social"), names_to = "Years", values_to = "Social Support")
SS_long <-SS_long[,c("Country","Years","Social Support")]

Health_long <- Full_Report_of_Factors %>%
  pivot_longer(cols = starts_with("Health"), names_to = "Years", values_to = "Health Life Expectancy")
Health_long <-Health_long[,c("Country","Years","Health Life Expectancy")]

Freedom_long <- Full_Report_of_Factors %>%
  pivot_longer(cols = starts_with("Freedom"), names_to = "Years", values_to = "Freedom in Life")
Freedom_long <-Freedom_long[,c("Country","Years","Freedom in Life")]

Generosity_long <- Full_Report_of_Factors %>%
  pivot_longer(cols = starts_with("Generosity"), names_to = "Years", values_to = "Generosity")
Generosity_long <- Generosity_long[,c("Country","Years","Generosity")]

Corroption_long <- Full_Report_of_Factors %>%
  pivot_longer(cols = starts_with("Perceptions"), names_to = "Years", values_to = "Perceptions of corruption")
Corroption_long <- Corroption_long[,c("Country","Years","Perceptions of corruption")]

#Combine
Summery_Factors <- bind_rows(
  GDP_long,
  SS_long,
  Health_long,
  Freedom_long,
  Generosity_long,
  Corroption_long
) 
Summery_Factors <- Summery_Factors %>%
  mutate(Years = str_replace(Years, ".*_(\\d+)$", "\\1"))

#NA omit removes NA, "." means check every row every column 
Summery_Factors <- Summery_Factors %>% group_by(Country, Years) %>% summarise_all(~ na.omit(.))


#Shiny App
ui <- fluidPage(
  
  titlePanel("6 Factor Analysis"),
  
  # Select Country 
  sidebarLayout(
    sidebarPanel(
      selectInput("Country",
                  "Choice of Country",
                  choices = c("Finland", "Belgium", "Denmark",
                              "France","Iceland","Kenya",
                              "Myanmar","Spain","Sweden","Togo"), 
                  selected = "Finland")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(4, plotOutput("GDP")),
        column(4, plotOutput("Social")),
        column(4, plotOutput("Health"))
      ),
      fluidRow(
        column(4, plotOutput("Freedom")),
        column(4, plotOutput("Generosity")),
        column(4, plotOutput("Corruption"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$GDP <- renderPlot({
    ggplot(filter(Summery_Factors, Country == input$Country), 
           aes(x = Years, y = `GDP per capita`)) +
      geom_col()+
      labs(title = paste("Explained by GDP"))+
      ylim(0, 1.6)
  })
  output$Social <- renderPlot({
    ggplot(filter(Summery_Factors, Country == input$Country), 
           aes(x = Years, y = `Social Support`)) +
      geom_col()+
      labs(title = paste("Explained by Social Support"))+
      ylim(0, 1.6)
  })
  output$Health <- renderPlot({
    ggplot(filter(Summery_Factors, Country == input$Country), 
           aes(x = Years, y = `Health Life Expectancy`)) +
      geom_col()+
      labs(title = paste("Explained by Health"))+
      ylim(0, 1.6)
  })
  
  output$Freedom <- renderPlot({
    ggplot(filter(Summery_Factors, Country == input$Country), 
           aes(x = Years, y = `Freedom in Life`)) +
      geom_col()+
      labs(title = paste("Explained by Freedom"))+
      ylim(0, 1.6)
  })
  output$Generosity <- renderPlot({
    ggplot(filter(Summery_Factors, Country == input$Country), 
           aes(x = Years, y = `Generosity`)) +
      geom_col()+
      labs(title = paste("Explained by Generosity"))+
      ylim(0, 1.6)
  })
  output$Corruption <- renderPlot({
    ggplot(filter(Summery_Factors, Country == input$Country), 
           aes(x = Years, y = `Perceptions of corruption`)) +
      geom_col()+
      labs(title = paste("Explained by Trust in Organization"))+
      ylim(0, 1.6)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
