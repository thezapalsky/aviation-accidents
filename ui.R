#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Analiza katastrof cywilnych samolotów pasażerskich w latach 1930-2018"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("Wskazniki", "Wybierz wskaznik:",
                  choices = c("Ilosc katastrof","Procent ocalalych", "Najniebezpieczniejsze linie", "Najniebezpieczniejsze kraje",
                              "Srednia liczba zgonow", "Srednia liczba ocalalych", "Zgony/ocaleni",
                              "Odchylenie standardowe zgonow", "Odchylenie standardowe ocalalych")),
      
       
      conditionalPanel("input.Wskazniki == 'Ilosc katastrof'",
                       textOutput("IK")),
      conditionalPanel("input.Wskazniki == 'Procent ocalalych'",
                       textOutput("PO")),
      conditionalPanel("input.Wskazniki == 'Srednia liczba zgonow'",
                       textOutput("SZ1"),
                       textOutput("SZ2")),
      conditionalPanel("input.Wskazniki == 'Srednia liczba ocalalych'",
                       textOutput("SO1"),
                       textOutput("SO2")),
      conditionalPanel("input.Wskazniki == 'Zgony/ocaleni'",
                       textOutput("ZO")),
      conditionalPanel("input.Wskazniki == 'Odchylenie standardowe zgonow'",
                       textOutput("OZ1"),
                       textOutput("OZ2")),
      conditionalPanel("input.Wskazniki == 'Odchylenie standardowe ocalalych'",
                       textOutput("OO1"),
                       textOutput("OO2"))
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("Wykres")
    )
  )
))
