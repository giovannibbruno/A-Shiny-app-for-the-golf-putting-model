# UI for golf model app -- QHELP 2021

# Layout includes sidebar and main panel. Main panel is divided into tabs

ui <- fluidPage(



  # title
  titlePanel("Golf putting model"),

  # sidebar layout
  sidebarLayout(

    # sidebar
    sidebarPanel(

      # Choose Data
      ## only show on plot and estimate tab
      conditionalPanel(
        condition = "input.current_tab == 'Plot' || 
                     input.current_tab == 'Parameter Estimation' ||
                     input.current_tab == 'Model comparison'",
        uiOutput("load_data")
      ),
      ## show combine checkbox only if more than 1 dataset is selected 
      conditionalPanel(
         condition = "input.input_files && input.input_files.length > 1",
         checkboxInput("combine_data", "Combine datasets")
      ),
      uiOutput("tab_dependent_UI")
      
      # Model Estimation (ML, Bayes, OLS)


      # Input: Slider for the number of observations to generate ----

      ),

    # main panel
    mainPanel(

      # tabs
      tabsetPanel(type = "tabs", id = "current_tab", 
        tabPanel("Plot", plotOutput("model_plot")),
        tabPanel("Parameter Estimation", plotOutput("loglikelihood_plot")),
        tabPanel("Model comparison", verbatimTextOutput("model_comparison")),
        
        tabPanel("Collect data", 

        fluidRow(
            column(5,
        
        # include p5js library and collect_data_golf.js
        # run in div 'divCollectData'
          tags$html(
            tags$body(
              tags$head(tags$script(src = "p5.js")),
              tags$head(tags$script(src = "sketch.js")),
              tags$div(id = 'divCollectData', style = 'width:auto; height:auto')
              )
          )),
       column(7,
        plotOutput("live_angle_distribution")      
        )
       )
        
        )
      )
    )
  )
)
