# server function for golf model app -- QHELP 2021
library(ggplot2)

# some useful functions 
## Gelman golf model -> probability of hit depending of ft from hole (distance) and
## sigma (standard deviation of angle distribution [N(0, sigma^2)])
ggm <- function(ft, sigma){
  r <- 1.68/2 * 1/12                     # radius of ball (inch -> feet)
  R <- 4.25/2 * 1/12                     # radius of hole
  2*pnorm(asin((R - r)/ft) / sigma) - 1  # prob of hit
}

## negative log-likelihood --> minimum = MLE
nll <- function(sigma, ft, tries, hits)
  -sum(dbinom(x = hits, size = tries, prob = ggm(ft = ft, sigma = sigma),
              log = TRUE))


server2 <- function(input, output) {
  
  # tab dependent UI
  ## renders a different user interface in each tab
  output$tab_dependent_UI <- renderUI({
    if(input$current_tab == "Plot"){
      return(NULL)
      
    }else if(input$current_tab == "Parameter Estimation"){
      return(NULL)
      
    }else if(input$current_tab == "Model comparison"){
      return(NULL)
      
    }else if(input$current_tab == "Collect data"){
      tagList(
        textOutput("collected_data_summary"), 
        verbatimTextOutput("show_collected_data"),
        actionButton("save_collected_data", "Save data"),
        actionButton("reset_collected_data", "Reset")
      )
    }
    
    
    
  })
  
  # Choose data
  ## user interface to choose datasets: all datasets in /data with .txt
  ## ending are displayed
  output$load_data <- renderUI({
    files <- list.files("./data", pattern = ".txt")
    selectInput("input_files", "Choose data", files, multiple = TRUE, selected = files[1])
  })
  
  # Load data
  ## reads data and generates reactive dataframe
  ## structure:
  ### ft tries hits ident
  ### 2  1443  1346 data1996
  ### 3   694   577 data1996
  ### 4   455   337 data1996
  ### ...
  ### ft:    distance from hole in feet
  ### tries: total number of tries
  ### hits:  number of hits 
  ### ident: corresponding dataset (data1996, data2018 and maybe sim1, sim2
  ### ... for simulated data and collected1, collected2 ... for collected data) 
  ###  if checkbox "Combine datasets" is checked (--> true), ident becomes the
  ###  same value ("one_set") regardless of which dataset the observation
  ###  originates from
  
  get_data <- reactive({
    req(input$input_files)
    if(length(input$input_files) > 0){
      temp <- do.call(rbind.data.frame, lapply(paste0("./data/", input$input_files), 
                                               read.table, header = T))
      if(input$combine_data) temp$ident <- "one_set"
      temp
    }else {
      showNotification("Please choose at least one dataset.", type = "warning")
    }
    
  })
  
  # plot [tab: Plot]
  output$model_plot <- renderPlot({
    dat <- get_data()
    
    sets <- unique(dat$ident) # ident names of datasets
    n_sets <- length(sets)    # number of datasets
    # could be useful for the plot
    
    # relative frequency (hits/tries) depending on distance from hole in ft
    if (n_sets <= 1) {
      ggplot(dat[dat$ident == sets[1], ], aes(x = ft, y = hits/tries, col = ident)) + 
        geom_point() +
        geom_line(size=0.7) +
        ylim(0,1) +
        xlim(0, max(dat$ft)) +
        ylab("hit probability (hits/tries)") +
        xlab("distance from the hole (ft)") +
        theme_light() +
        theme(legend.position = "none")
    } else {
      ggplot(dat[dat$ident == sets[1:n_sets], ], aes(x = ft, y = hits/tries, col=ident)) + 
        geom_point() +
        geom_line(size=0.7) +
        ylim(0,1) +
        xlim(0, max(dat$ft)) +
        ylab("hit probability (hits/tries)") +
        xlab("distance from the hole (ft)") +
        theme_light() +
        labs(col = "Dataset")
    }
    
    
    
    
    
    ###### ggplot based on dat [see above: dat <- get_data()]
    
    
    
    
  })
  
  
  
  # Collect data [tab: Plot]
  collected <- reactiveValues(data = NULL)
  
  ## add data from p5js application [work in progress... just a proof of
  ## concept 'till now]
  # observeEvent(input$xMouse, {
  #print(input$xMouse)
  #print(str(input$xMouse))
  
  #collected$data <- rbind(collected$data, data.frame(ft = 1, angle = 1, hit = 1))
  #})
  
  output$collected_data_summary <- renderPrint({
    paste0("n = ", nrow(collected$data))
  })
  
  output$show_collected_data <- renderPrint({
    head(collected$data, 50)
  })
  
  
  
  
  
  
}

