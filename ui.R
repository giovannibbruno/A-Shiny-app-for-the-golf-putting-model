# UI for golf model app -- QHELP 2021

# Layout includes sidebar and main panel. Main panel is divided into tabs

ui <- fluidPage(



  # title
  titlePanel("Tabsets"),

  # sidebar layout
  sidebarLayout(

    # sidebar
    sidebarPanel(

      # Choose Data

      # Model Estimation (ML, Bayes, OLS)

      # Input: Slider for the number of observations to generate ----

      ),

    # main panel
    mainPanel(

      # tabs
      tabsetPanel(type = "tabs",
        tabPanel("Plot", plotOutput("model_plot")),
        tabPanel("Parameter Estimation", verbatimTextOutput("parameter_estimation")),
        tabPanel("Model comparison", verbatimTextOutput("model_comparison")),
        
        tabPanel("Collect data", 
        
        # include p5js library and collect_data_golf.js
        # run in div 'divCollectData'
          tags$html(
            tags$body(
              tags$head(tags$script(src = "p5.js")),
              tags$head(tags$script(src = "sketch.js")),
              tags$div(id = 'divCollectData', style = 'width:auto; height:auto')
              ))
        
        
        
        )
      )
    )
  )
)
