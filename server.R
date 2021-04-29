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

    }else if(input$current_tab == "Collect data"){
      tagList(
        checkboxInput("show_angle_distro", "Show angle distribution"),
        strong("Scoring Board:"),
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
      temp <- do.call(rbind.data.frame, lapply(paste0("./data/", input$input_files), 
                                               read.table, header = T))
      if(input$combine_data) temp$ident <- "one_set"
      temp
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
      
    
    
  })
 

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
         ylab = "negativ log-likelihood", axes = F)
    axis(1)
  
    if(!is.na(estim$par)) abline(v = estim$par) # plot estimated sigma if available

    box()


  })
  
  
  
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
      
      xval <- seq(0.2, max(dat$ft) + 1, length.out=101)
      lines(ggm(xval, sigma = mle$par) ~ xval, dat, col = "blue")
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
            Therefore, it seems pretty reasonable. 
            Nevertheless, it is not perfect and ignores, for example, 
            whether a shot might be too short or too long. 
            Moreover, it does not take into account the variations between golf greens, 
            playing conditions and personal abilities.")
    }
  })

  
  
  # Collect data [tab: Plot]
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
    if(input$show_angle_distro & nrow(collected$data) > 0) 
      hist(collected$data$angle, xlab = "angle [radians]", 
        main = "Distribution of the angle") 
  
  
  })
  
  
  
  
  
}

