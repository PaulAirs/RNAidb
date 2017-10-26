#install.packages('rsconnect')
#install.packages("shinydashboard")
#install.packages('shiny')
#rsconnect::setAccountInfo(name='mosquitornai',
#                          token='223D307F7A3A98611034F0F74E5B2159',
#                         secret='QdSf1RKMcAp+XSGzRS4QxBMrs4gZLcYYDjrqbxQT')
#rsconnect::deployApp('')
#setwd('C:\\RNAidbtest\\RNAidb2')

# Open libraries
library(rsconnect)
library(shinydashboard)
#library(dplyr)
#library(ggplot2)
#library(plotly)
library(shiny)
library(DT)

# Open datasets
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
ui <- fluidPage(
    sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "distPlot"',
        sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
        ),
      conditionalPanel(
        'input.dataset === "RNAidb"',
        checkboxGroupInput("show_vars", "Columns to show:",
                           names(RNAidb), selected = col_list)
        ),
      conditionalPanel(
         'input.dataset === "RNAidbgene"',
         checkboxGroupInput("show_vars2", "Columns to show:",
                            names(RNAidbgene), selected = col_list_2)
        )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel(title = "Histogram", value = "distPlot", plotOutput(outputId = "distPlot")),
        tabPanel(title = "RNAidb Table", value = "RNAidb", DT::dataTableOutput("mytable1")),
        tabPanel(title = "RNAi Vectorbase Gene Hits Table", value = "RNAidbgene", DT::dataTableOutput("mytable2")),
        tabPanel(title = "RNAi Vectorbase Genome Hits Table", value = "RNAidbgenome", DT::dataTableOutput("mytable3"))
      )
    )
    
))



server <- function(input, output) {
  
  # choose columns to display
  #RNAidb2 = RNAidb[sample(nrow(RNAidb), 100), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(RNAidb[, input$show_vars, drop = FALSE], options = list(orderClasses = TRUE))
  })
  
  # # sorted columns are colored now because CSS are attached to them
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(RNAidbgene[, input$show_vars2, drop = FALSE], options = list(orderClasses = TRUE))
  })
  
  # customize the length drop-down menu; display 5 rows per page by default
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
  
#x <- c(as.numeric(levels(RNAidb$`KD mean vs ctrl`))[RNAidb$`KD mean vs ctrl`])
#list(x)
#as.numeric(levels(x))[x]
#mean(as.numeric(levels(x))[x])
#hist(x, xlab = "% Knockdown", ylab = "Frequency", main = "Average Knockdown vs Control", col = "Grey")
