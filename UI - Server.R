#####################
# Open Source Files #
#####################

# Set Working Directory (local)
#setwd('C:\\RNAidbtest\\RNAidb')
source("Load.R")  # Load libraries and data
source("UI-Tabs.R") # UI - Tab functions
#source()#R functions

##################
# User Interface #
##################

ui <- (fluidPage(theme = shinytheme("cosmo"),

  navbarPage("MosquitoRNAidb",
   tabPanel("About",
      Aboutp()),
   tabPanel("RNAi Database Table",
             RNAidbp()),
   tabPanel("Data Explorer",
             Explorerp()))))

##########
# Server #
##########

source("Server-Tabs.R")

server <- (function(input, output, session){
  env_serv = environment()
  
# About Tab 
  
  Aboutp(env_serv)

# The RNAi dataset
  
  RNAidbp(env_serv)
  
# Explorer
  
  Explorerp(env_serv)
  
})

############
# Load App #
############

shinyApp(ui = ui, server = server)