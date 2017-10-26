#setwd('C:\\RNAidbtest\\RNAidb2')

# Open libraries
#library(dplyr)
#library(ggplot2)
#library(plotly)
library(rsconnect)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(DT)

# Open datasets
About <- "Text text text text"

RNAidb<- read.csv("RNAidb_out.csv") 
load('RNAidb_out.Rda')
col_list <- c("Author","Year", "Genus", "Species", "Gene of interest", "KD mean vs ctrl")

RNAidbgene<- read.csv("RNAidbgenev3_out.csv")
load('RNAidbgenev3_out.Rda')
col_list_2 <- c("ID","Species")

RNAidbgenome<- read.csv("RNAidbgenomev3_out.csv")
load('RNAidbgenomev3_out.Rda')
col_list_3 <- c("ID","Species")


# USER INTERFACE
ui <- fluidPage(theme = shinytheme("flatly"),
        navbarPage("MosquitoRNAidb",
                   
# About Page
        tabPanel("About",
          mainPanel(title = "About", 
                   p(h2("Welcome to MosquitoRNAidb")),
                   div(img(src = "About1.jpg", height = 266, width = 200), style="text-align: center;"),
                   p("MosquitoRNAidb is a repository of published mosquito RNAi knockdown studies curated by Paul Airs. Here you can browse, search, and compare RNAi study designs and outcomes, create summary data, graphs, and protocol recommendations based previous data."))),
      
# RNAi dataset 1
        tabPanel("RNAi dataset",
          sidebarLayout(
            sidebarPanel(
                checkboxGroupInput("show_vars", "Columns to show:", names(RNAidb), selected = col_list)),
          mainPanel(title = "RNAidb Table", value = "RNAidb", DT::dataTableOutput("mytable1")))),

# More Datasets                
        navbarMenu("More Datasets",

# Gene vectorbase hits
          tabPanel("Gene",
            sidebarLayout(
             sidebarPanel(
                 checkboxGroupInput("show_vars2", "Columns to show:", names(RNAidbgene), selected = col_list_2)),
            mainPanel(title = "RNAi Vectorbase Gene Hits Table", value = "RNAidbgene", DT::dataTableOutput("mytable2")))),
        
# Genome vectorbase hits
          tabPanel("Genome",
#            sidebarLayout(
#             sidebarPanel(""
#                checkboxGroupInput("show_vars3", "Columns to show:", names(RNAidbgenome), selected = col_list_3)),
            mainPanel(title = "RNAi Vectorbase Genome Hits Table", value = "RNAidbgenome", DT::dataTableOutput("mytable3")))),

# Graphs
        navbarMenu("Graphs",
          tabPanel("Knockdown vs Control",
            sidebarLayout(
              sidebarPanel(
                  sliderInput(inputId = "bins",
                            label = "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30)),
            mainPanel(title = "Data Vizualisation", value = "distPlot", plotOutput(outputId = "distPlot")))),
          tabPanel("Graph 2",
            mainPanel("Under construction"))),

# Protocol Builder
        navbarMenu("Protocol Builder",
          tabPanel("Builder 1",
            mainPanel("Under construction")),
          tabPanel("Builder 2",
            mainPanel("Under construction")))

))
  

# SERVER      
server <- function(input, output) {
  
# RNAidb dataset
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(RNAidb[, input$show_vars, drop = FALSE], options = list(orderClasses = TRUE))
  })
  
# Gene vectorbase dataset
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(RNAidbgene[, input$show_vars2, drop = FALSE], options = list(orderClasses = TRUE))
  })
  
# Genome vectorbase dataset
  output$mytable3 <- DT::renderDataTable({
    DT::datatable(RNAidbgenome, options = list(orderClasses = TRUE))
  })
  
# Histogram of KD values
  output$distPlot <- renderPlot({
    kd <- c(as.numeric(levels(RNAidb$`KD mean vs ctrl`))[RNAidb$`KD mean vs ctrl`])
    x <- c(na.omit(kd))
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, 
         breaks = bins, 
         col = "grey", border = "white",
         xlab = "Knockdown average (%)", main = "Average Knockdown vs Control")
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
