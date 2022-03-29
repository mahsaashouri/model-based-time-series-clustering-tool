library(shiny)
library(shinyscreenshot)
library(shinydashboard)
library(shinythemes)
## themes available
## cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper, readable, sandstone, 
##simplex, slate, spacelab, superhero, united, yeti
# Define UI 
shinyUI(fluidPage(#theme = shinytheme("cosmo"),
    # title
    titlePanel("Model-Based Time Series Clustering Tool"),
       sidebarPanel(
         fileInput('target_upload', 'Choose file to upload',
                   accept = c(
                     'text/csv',
                     'text/comma-separated-values',
                     '.csv'
                   )),
         radioButtons(inputId="Frequency", label="Frequency", 
                      choices=c("Weekly" = 7, "Monthly" = 12), selected=list("12")),
                    fluidRow( 
                     h4("First Set"),
           selectInput("Depth1", label="MOB depth", choices=list("1-no"= "1","2" = "2", "3" = "3", "4" = "4", 
                                                                          "5-full" = "5"), selected=list("3")),
                     selectInput("Prune1", label = "Prune options", 
                                 choices = list("AIC" = "AIC", "BIC" = "BIC"), selected = list("AIC")),
                     checkboxGroupInput("SplitVariables1", label = "Split variables", 
                                        choices = list("label 1" = "label 1", "label 2" = "label 2")),
           selectInput("colorhigh1", label = "Heatmap Color - High", 
                       choices = list("red" = "darkred", "orange" = "darkorange"), selected = list("red")),
           selectInput("colorlow1", label = "Heatmap Color - Low", 
                       choices = list("green" = "darkgreen", "blue" = "darkblue"), selected = list("green"))
                                                             ),
          
       fluidRow( 
         h4("Second Set"),
          selectInput("Depth2", label="MOB depth", choices=list("1-no"= "1","2" = "2", "3" = "3", "4" = "4", 
                                                               "5-full" = "5"), selected=list("2")),
          selectInput("Prune2", label = "Prune options", 
                      choices = list("AIC" = "AIC", "BIC" = "BIC"), selected = list("AIC")),
         checkboxGroupInput("SplitVariables2", label = "Split variables", 
                            choices = list("label 1" = "label 1", "label 2" = "label 2")),
         selectInput("colorhigh2", label = "Heatmap Color - High", 
                     choices = list("red" = "darkred", "orange" = "darkorange"), selected = list("red")),
         selectInput("colorlow2", label = "Heatmap Color - Low", 
                     choices = list("green" = "darkgreen", "blue" = "darkblue"), selected = list("green")),
          actionButton("go", "Take a screenshot"),
        )),
        mainPanel(
          
        tabsetPanel(
            tabPanel(h4("First Set"), 
                 tabPanel(h3("Performance Statistics"), title = uiOutput("titleAICMSE1")),
                 tabPanel("MSEAIC1", div(tableOutput("MSEAIC1"), value = "title"), style = "font-size:150%"),
                 tabPanel(h3("MOB - Heatmaps"), title = uiOutput("titleHeatmap11")),
                 tabPanel("MOB - Heatmaps1", plotOutput("MOBTree11" , width = 2000, height = 800
                                                       )),
                 tabPanel(h3("MOB - Heatmaps (month.of.year)"), title = uiOutput("titleHeatmap21")),
                 tabPanel("MOB - Heatmaps (month.of.year)1", plotOutput("MOBTree21", width = 2000, height = 800
                                                                       )),
                 tabPanel(h3("Series with average line"), title = uiOutput("titleline1")),
                 tabPanel("Series with average line1", plotOutput("ClusterSeries1")),
                 tabPanel(h3("Coefficient plot"), title = uiOutput("titlecoefficient1")),
                 tabPanel("Coefficient plot1", plotOutput("Coefficientplot1", click = "plot_click")),
                 verbatimTextOutput("info1")),
            tabPanel(h4("Second Set"),
                 tabPanel(h3("Performance Statistics"), title = uiOutput("titleMSEAIC2")),
                 tabPanel("MSEAIC2", div(tableOutput("MSEAIC2"), value = "title"), style = "font-size:150%"),
                 tabPanel(h3("MOB - Heatmaps"), title = uiOutput("titleHeatmap12")),
                 tabPanel("MOB - Heatmaps2", plotOutput("MOBTree12", width = 2000, height = 800
                                                       )),
                 tabPanel(h3("MOB - Heatmaps (month.of.year)"), title = uiOutput("titleHeatmap22")),
                 tabPanel("MOB - Heatmaps (month.of.year)2", plotOutput("MOBTree22", width = 2000, height = 800
                                                                       )),
                 tabPanel(h3("Series with average line"), title = uiOutput("titleline2")),
                 tabPanel("Series with average line2", plotOutput("ClusterSeries2")),
                 tabPanel(h3("Coefficient plot"), title = uiOutput("titlecoefficient2")),
                 tabPanel("Coefficient plot2", plotOutput("Coefficientplot2", click = "plot_click")),
                 verbatimTextOutput("info2"))
      )
    )
  )
)
