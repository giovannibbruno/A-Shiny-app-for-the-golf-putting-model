# server function for golf model app -- QHELP 2021
library(ggplot2)

# some useful functions 
## Gelman golf model -> probability of hit depending on ft from hole (distance) and
## sigma (standard deviation of angle distribution [N(0, sigma^2)])
ggm <- function(ft, sigma){
  r <- 1.68/2 * 1/12                     # radius of ball (inch -> feet)
  R <- 4.25/2 * 1/12                     # radius of hole
  #2*pnorm(asin((R - r)/ft) / sigma) - 1  # prob of hit
  hit <- 2*pnorm(asin((R - r)/ft) / sigma) - 1 # prob of hit
  hit - .Machine$double.eps                    # subtract a tiny bit

}

## negative log-likelihood --> minimum = MLE
nll <- function(sigma, ft, tries, hits)
  -sum(dbinom(x = hits, size = tries, prob = ggm(ft = ft, sigma = sigma),
              log = TRUE))


## functions to simualate data
# Simulation of Binomial Data based sigma

# a data frame wich shows all the tries, successes and the resulting probability 
# per distance
distance <- c(seq(2,20,1))

simulated_data <- data.frame(distance, tries = 1000, successes = NA, 
                             probability = NA, row.names = NULL)

# function for data simulation with probability and tries as variables
data_sampler <- function(x){
  generated <- rbinom(n = 1000, size = 1, prob = x)
  success <- sum(generated)
  return (success)
}



server <- function(input, output) {
  
  # tab dependent UI
  ## renders a different user interface in each tab
  output$tab_dependent_UI <- renderUI({
    if(input$current_tab == "Plot"){

      return(NULL)

    }else if(input$current_tab == "Parameter Estimation"){
      tagList(
        sliderInput("range_sigma", "Range Sigma:",
          min = 0, max = 1, step = .01,
          value = c(0, 1)),
        actionButton("optim_nll", "Find minimum"),
        textOutput("estimated_sigma") 
      )


    }else if(input$current_tab == "Model comparison"){
      tagList(
        checkboxInput("information", "Show more information?", value = FALSE),
        textOutput("infos") 
      )

    }else if(input$current_tab == "Data simulation"){
      tagList(
        # *Input() functions
        sliderInput(inputId = "sigma1", label = "Choose a sigma", value = 0.026,
          min = 0.005, max = 0.35),
        # *Output() functions
        tableOutput(outputId = "table_simulated")
      )

    }else if(input$current_tab == "Let's play a game!"){
      tagList(
        h4("Want to play a game?"),
        p("Just press the 'Set direction' button and see what happens. 
          If you want to save your data, click 'Save data' otherwise use 
          'Reset' to reset the board."),
        p("Under 'Choose data' you can retrieve already saved data sets. 
          You can find your own data set by date and time. Here it is worth 
          to have a look at the distribution of the angles and the hit rate
          depending on the distance: Do the assumptions of the golf putting
          model fit to this data and how well does the model describe your 
          collected data?"),
        strong("Scoring Board:"),
        verbatimTextOutput("show_collected_data"),
        actionButton("save_collected_data", "Save data"),
        actionButton("reset_collected_data", "Reset"),
        br(),
        br(),
        uiOutput("load_collected_data"),
        checkboxInput("show_angle_distro", "Show distribution of the angle")
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
      temp <- do.call(rbind.data.frame, lapply(paste0("./data/", input$input_files), 
                                               read.table, header = T))
      if(input$combine_data) temp$ident <- "one_set"
      temp
  })
  
  #################################
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
        ylab("Probability of success") +
        xlab("Distance from hole (feet)") +
        theme_light() +
        theme(legend.position = "none")
    } else {
      ggplot(dat[dat$ident == sets[1:n_sets], ], aes(x = ft, y = hits/tries, col=ident)) + 
        geom_point() +
        geom_line(size=0.7) +
        ylim(0,1) +
        xlim(0, max(dat$ft)) +
        ylab("Probability of success") +
        xlab("Distance from hole (feet)") +
        theme_light() +
        labs(col = "Dataset")
    }
      
    
    
  })
 
  #################################
  # Parameter Estimation [tab: Parameter Estimation]
  
  estim <- reactiveValues(par = NA) # to store estimated sigma

  ## estimate sigma if button optim_nll is pressed
  observeEvent(input$optim_nll, {
        dat <- get_data()
        #defaultW <- getOption("warn")  # turn warnings of for optimization to
        # make debugging easier
        #options(warn=-1)
        mle <- optim(par = 0.1, fn = nll,
             ft = dat$ft, tries = dat$tries, hits = dat$hits,
             method = "BFGS")
        #options(warn = defaultW)
        estim$par <- mle$par
  })

  ## show estimated sigma if available
  output$estimated_sigma <- renderText({
    if(!is.na(estim$par)) 
      paste0("Minimum of the negative log-likelihood function is at sigma = ", 
              round(estim$par, 4))
  })

  ## plot the negaitve log-likelihood
  output$loglikelihood_plot <- renderPlot({
    req(input$range_sigma)
    dat <- get_data()
    sets <- unique(dat$ident) # ident names of datasets
    n_sets <- length(sets)    # number of datasets
    
    sigmas <- seq(input$range_sigma[1], input$range_sigma[2], length.out = 500)
    lls <- sapply(sigmas, nll, ft = dat$ft, tries = dat$tries, hits = dat$hits)
    
    plot(lls ~ sigmas, type = "l", xlab = "Sigma", 
         ylab = "negative log-likelihood", axes = F, col="cornflowerblue", lwd= 2.5)
    axis(1)
    
    if(!is.na(estim$par)) abline(v = estim$par, col="red", lty=2) # plot estimated sigma if available
    
    box()

  })
  
  #################################
  # Model comparison [tab: Model Comparison]
  
  # generic for multiple datasets / combined
  
  
  # Plot with logistic regression and probability model
  output$comparison_plot <- renderPlot({
    
    dat <- get_data()
    sets <- unique(dat$ident) # ident names of datasets
    n_sets <- length(sets)    # number of datasets
    
    # flexible for different distances
    plot(hits/tries ~ ft, dat, ylim=0:1, xlim=c(0, max(dat$ft + 1)), pch=16,
         panel.first=quote(grid(lty=1)), ylab="Probability of success",
         xlab="Distance from hole (feet)")
    
    # with CIs
    ci <- mapply(binom.test, dat$hits, dat$tries)["conf.int", ]
    arrows(dat$ft, sapply(ci, "[[", 1), dat$ft, sapply(ci, "[[", 2), .05, 90, 3)
    
    
    # model estimations and predictions
    for(i in 1:n_sets){
      dat.loc <- dat[dat$ident == sets[i],]
      
      mle <- optim(par = 0.1, fn = nll,
                   ft = dat.loc$ft, tries = dat.loc$tries, hits = dat.loc$hits,
                   method = "BFGS")
      
      xval <- seq(0.2, max(dat$ft) + 1, length.out=1001)
      lines(ggm(xval, sigma = mle$par) ~ xval, dat.loc, col = "blue")
      lines(predict(glm(cbind(hits, tries - hits) ~ ft, binomial, dat.loc),
                    data.frame(ft = xval), type = "response") ~ xval, lty = 2, col = "firebrick")
    }
    
    legend("topright", col = c("blue", "firebrick"), 
           legend = c("Golf Model", "Logistic Regression"), lty = 1:2)
    
    box()
    
  })
  
  
  ## some more information like G2, AIC and BIC per model and dataset
  output$stats <- renderPrint({
    
    # default is FALSE
    if(input$information == TRUE){
      
      dat <- get_data()
      sets <- unique(dat$ident) # ident names of datasets
      n_sets <- length(sets)    # number of datasets
      
      # set up dataframe/table to fill
      out.table <- data.frame(Set = factor(rep(sets, each = 2)), 
                              Model = factor(rep(c("Golf model", "Logistic"), times = n_sets)),
                              df = rep(0, 2*n_sets), G2 = rep(0, 2*n_sets), 
                              AIC = rep(0, 2*n_sets), BIC = rep(0, 2*n_sets)
      )
      
      # goodness of fit for both models in all choosen datasets
      for(i in 1:n_sets){
        dat.loc <- dat[dat$ident == sets[i],]
        
        mle <- optim(par = 0.1, fn = nll,
                     ft = dat.loc$ft, tries = dat.loc$tries, hits = dat.loc$hits,
                     method = "BFGS")
        
        n <- length(dat.loc$ft)
        
        G2.Geom <- 2*(with(dat.loc, sum(dbinom(hits, tries, hits/tries, log = TRUE))) + mle$value)
        df.Geom <- length(unique(dat.loc$ft)) - 1
        k.Geom <- 1
        AIC.Geom <- -2*-mle$value + 2*k.Geom
        BIC.Geom <- -2*-mle$value + k.Geom*log(n)
        
        glm1 <- glm(cbind(hits, tries - hits) ~ ft, binomial, dat.loc)
        G2.Log <- sum(resid(glm1, type = "deviance")^2)
        df.Log <- length(unique(dat.loc$ft)) - 2
        AIC.Log <- AIC(glm1)
        BIC.Log <- BIC(glm1)
        
        # A check for the likelihoods:
        #
        # nll2 <- function(beta, ft, tries, hits)
        #   -sum(dbinom(x = hits, size = tries,
        #               prob = plogis(beta[1] + beta[2]*ft),
        #               log = TRUE))
        # 
        # mle2 <- optim(par = c(2, -0.2), fn = nll2,
        #               ft = dat.loc$ft, tries = dat.loc$tries, hits = dat.loc$hits,
        #               method = "BFGS")
        # 
        # ## Compare with glm()
        # logLik(glm(cbind(hits, tries - hits) ~ ft, binomial, dat.loc))
        # 
        # -> Looks as if they are on the same scale.
        
        out.table[2*i-1,3:6] <- c(df.Geom, round(G2.Geom,2), round(AIC.Geom,2), round(BIC.Geom,2))
        out.table[2*i, 3:6] <- c(df.Log, round(G2.Log,2), round(AIC.Log,2), round(BIC.Log,2))
      }
      print(out.table)
      
      ### Are these outputs correct?
      
    }
  })
  
  # some general information based on the goodness of fit
  output$infos <- renderText({
    
    # default = FALSE
    if(input$information == TRUE){
      paste("The Golf model fits much better, while it is also more parsimonious.
            Nevertheless, it is not perfect and ignores, for example, 
            whether a shot might be too short or too long. 
            Moreover, it does not take into account the variations between golf greens, 
            playing conditions and personal abilities.")
    }
  })



  #################################
  # simulate data [tab: "Data simulation"]
  # output table based on sigma slider
  output$table_simulated <- renderTable({
    simulated_data$probability <- sapply(simulated_data$distance, 
                                         FUN = ggm, sigma = input$sigma1)
    simulated_data$successes <- sapply(simulated_data$probability, FUN = data_sampler)
    return(simulated_data)
  })
  
  # output plot based on sigma slider
  output$plot_simulated <- renderPlot({
                simulated_data$probability <- sapply(simulated_data$distance, 
                                                      FUN = ggm, sigma = input$sigma1)
                simulated_data$successes <- sapply(simulated_data$probability, 
                                                   FUN = data_sampler)
                
                plot(successes/tries ~ distance, simulated_data, ylim=0:1, xlim=c(0, 21), pch=16,
                     panel.first=quote(grid(lty=1)), ylab="Probability of success",
                     xlab="Distance from hole (feet)")
                
                
                # also show the estimated golf_model
                mle <- optim(par = 0.1, fn = nll,
                             ft = simulated_data$distance, 
                             tries = simulated_data$tries, 
                             hits = simulated_data$successes,
                             method = "BFGS")
                
                xval <- seq(0.2, 21, length.out=101)
                lines(ggm(xval, sigma = mle$par) ~ xval, simulated_data, col = "blue")
                
                txt <- paste("Sigma reestimation:", round(mle$par,4))
                text(12.5, 0.7, txt, cex = 1.3)
   })
  
  
  #################################  
  # Collect data [tab: "Let's play a game!"]

  # load collected data
  output$load_collected_data <- renderUI({
    files <- list.files("./collected_data", pattern = ".txt")
    tagList(
    selectInput("input_collected_files", "Choose data", files, multiple = TRUE),

   conditionalPanel(
         condition = "input.input_collected_files && input.input_collected_files.length > 1",
          p("If you select more than one dataset, they will be combined into one."),
          br()
      )
   )
  })
    
  get_saved_data <- reactive({
    if(isTruthy(input$input_collected_files)) {
      temp <- do.call(rbind.data.frame, lapply(paste0("./collected_data/", 
            input$input_collected_files), read.table, header = T))
      temp
    } else {
      data.frame()
    }
  })


  collected <- reactiveValues(data = data.frame(ft = c(), angle = c(), hit = c()))


  ## save collected data to .txt file
  observeEvent(input$save_collected_data, {
    write.table(collected$data, 
                paste0("./collected_data/collected_",
                        format(Sys.time(), "%d%m%Y_%H%M%S"), ".txt"), 
                row.names = FALSE, quote = FALSE)
    collected$data = data.frame(ft = c(), angle = c(), hit = c())
  })

  ## reset data
  observeEvent(input$reset_collected_data, {
    collected$data = data.frame(ft = c(), angle = c(), hit = c())
  })

  
  ## add data from p5js application [work in progress... just a proof of
  ## concept 'till now]
  observeEvent(input$jsGolfData, { 
    jsData <- input$jsGolfData
    collected$data <- rbind(collected$data, data.frame(dist  = round(jsData[1], 0), 
                                                       angle = jsData[2], 
                                                       hit   = jsData[3]))
  })
  
  ## show scoring board
  output$show_collected_data <- renderPrint({
    if(nrow(collected$data) > 0) tail(collected$data, 10)
  })

  ## show angle distribution
  output$live_angle_distribution <- renderPlot({
    req(input$show_angle_distro)
    if(input$show_angle_distro & nrow(collected$data) > 0){ 
      saved_data <- get_saved_data()
      all_data <- rbind(collected$data, saved_data)
      hist(all_data$angle, xlab = "angle [radians]", 
        main = "Distribution of the angle") 
  
    }
  })
  
  ## hitrate depending on distance
  output$live_hitrate_distance <- renderPlot({
    req(input$show_angle_distro)
    if(input$show_angle_distro & nrow(collected$data) > 0){ 
      saved_data <- get_saved_data()
      all_data <- rbind(collected$data, saved_data)

      all_data$dist_cut <- cut(all_data$dist, 15)
      ag <- aggregate(hit ~ dist_cut, all_data, mean)

      plot(hit ~ as.numeric(dist_cut), ag, ylim=0:1, pch=16,
         panel.first=quote(grid(lty=1)), ylab="Probability of success",
         xlab="Distance from hole (pixel)", axes = F)
      axis(2)
      axis(1, at = 1:15, labels = levels(ag$dist_cut))

  
    }
  })

  
  
  
  
}

