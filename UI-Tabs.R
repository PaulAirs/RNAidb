# UI Tabs

# About Tab PAGE

Aboutp = function()

  #imageOutput("About2.png")
  fluidPage(
  fluidRow(
    div(img(src = "About2.png", height = "145px", width = "527px"),  style="text-align: center;")),
  fluidRow(
    column(12,
    div(
      p("MosquitoRNAidb is a repository of published mosquito RNAi experiments. RNAi is an essential tool for indicuble knockdown of target genes but
              suffers from variability between studies. This site provides a resource for exploring parameters of successful and unsuccessful RNAi knockdown studies in 
              mosquito systems with the aim of generating standardized protocols for more successfull knockdown in any given experiment. Users are free to browse, search, 
              and contrast RNAi study designs and outcomes. To see the code used for this site please 
              visit ", a("Github.com/PaulAirs/RNAidb", href="https://github.com/PaulAirs/RNAidb"), "."),
      p("If you wish to add/remove/alter data on the site please see the 'Downloads and Alterations' 
        tab or contact Paul Airs at P.airs@qub.ac.uk.")))))

# The RNAi dataset PAGE

RNAidbp = function()
      sidebarLayout(
      sidebarPanel(
        width = 2,  
        h4("Select Columns to show"),
        actionLink("selectall", "Select All / Reset"),
        checkboxGroupInput("show_vars", "", names(RNAidb), selected = col_list)),
        
      mainPanel("RNAi dataset",
         width = 10, 
         span(("The following data table was derived directly from published literature and contains all information pertaining to
               knockdown experiments performed on mosquito specific genes."), 
              verbatimTextOutput("LastUpdate")),
         br(),
         value = "RNAidb", DT::dataTableOutput("mytable1")))

# Explorer page

Explorerp = function()
  sidebarLayout(
    sidebarPanel(
    sliderInput('sampleSize', 'Sample Size', min = 1, max = nrow(Explorer),
            value = 500, step = 50, round = 0),
    selectInput('x', 'X', choices = nms, selected = "RNAi.trigger.length"),
    selectInput('y', 'Y', choices = nms, selected = "Knockdown.KD"),
    selectInput('color', 'Color', choices = nms, selected = "Species"),
    selectInput('facet_row', 'Facet Row', c(None = '.', nms)),
    selectInput('facet_col', 'Facet Column', c(None = '.', nms)),
    sliderInput('plotHeight', 'Height of plot (in pixels)', 
            min = 100, max = 2000, value = 1000),
    sliderInput('plotWidth', 'Width of plot (in pixels)', 
            min = 100, max = 4000, value = 1000)),
  
  mainPanel(
    plotlyOutput('trendPlot', height = "900px")))
