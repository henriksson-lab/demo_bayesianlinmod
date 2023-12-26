library(plotly)
library(Cairo)
options(shiny.usecairo=T)


if(FALSE){
  install.packages("matlib")
}



if(FALSE){
  #To run this app
  library(shiny)
  runApp(".")
}

############ TODO: how many % of data points to include? or number?

allowed_datafiles <- c("trivial.csv","diabetes.csv")


###
# X is the data
# t is the expected response
# t=wX   (I am more used to b=AX, so A would be w here, and X is PSI)
#
# PSI: psi(x), so base functions evaluated at x. or simply x in the simplest case
# prior_mu
# prior_S - covariance matrix, sigma2
# beta - the known error vs the predicted points
calc_posterior_from_prior <- function(prior_mu, prior_S, PSI, input_t, beta){
  
  # https://www.microsoft.com/en-us/research/uploads/prod/2006/01/Bishop-Pattern-Recognition-and-Machine-Learning-2006.pdf
  #
  #3.49 p(w|t) = N (w|mN , SN )
  #3.50 mN = SN ( S0^−1 m0 + β Φ^T t ) 
  #3.51 SN−1 = S0^−1 + β Φ^T Φ
  
  inv1 <- function(m){
    if(ncol(m)==1){
      1/m
    } else {
      matlib::inv(m)
    }
  }
  
  inv_posterior_S <- inv1(prior_S) + beta * ( t(PSI) %*% PSI )
  posterior_S <- inv1(inv_posterior_S) 
  posterior_mu <- posterior_S %*% ( inv1(prior_S) %*% prior_mu + beta * (t(PSI) %*% input_t))
  
  posterior <- list(
    mu=posterior_mu,
    S=posterior_S
  )
  return(posterior)
}


#Construct covariance matrix assuming to cross-variable covariates
sigma2_to_diagmatrix <- function(S){
  m <- matrix(0,nrow=length(S),ncol=length(S))
  diag(m) <- S
  m
}




server <- function(input, output, session) {

  getDataTable <- reactive({    
    current_ds <- input$input_ds
    available_datasets[[current_ds]]
#    thedat <- read.csv(sprintf("/corgi/websites/demo_linmod/data/%s", current_ds))
#    thedat
  })

  getSubsetPoints <- function(thedat){
    set.seed(input$random_seed)
    thedat[sample(1:nrow(thedat), input$numpoint),,drop=FALSE]
  }
    
  ##############################################################################
  ########### General functions ################################################
  ##############################################################################
  
  
  getPriorPosterior <- reactive({

    ##### Load the data
    thedat <- getDataTable()
    thedat <- getSubsetPoints(thedat)
    thedat_PSI <- thedat[,colnames(thedat)!=input$input_predict,drop=FALSE]
    thedat_t <- thedat[,colnames(thedat)==input$input_predict,drop=FALSE]
    
    param_names <- colnames(thedat_PSI)
    
    ##### Gather prior values from sliders
    prior_mu <- rep(0, ncol(thedat_PSI))
    prior_S_diag <- rep(1, ncol(thedat_PSI))
    for(i in 1:length(param_names)){
     # print(input)
      
      val1 <- paste("mean",colnames(thedat_PSI)[i])
      val2 <- paste("sd",colnames(thedat_PSI)[i])
      #  print(val)
      
      if(val1 %in% names(input)){
        prior_mu[i] <- input[[val1]]
        prior_S_diag[i] <- input[[val2]]**2  #Should be the variance
      } else {
        print("Parameters not yet in UI")
        break
      }
    }
    
    #print("######## priors")
   # print(prior_mu)
  #  print(prior_S_diag)
        
    
    ##### First guess at values
    thedat_PSI <- as.matrix(thedat_PSI)
    thedat_t <- as.matrix(thedat_t)
    
   # print(thedat_PSI)
  #  print(thedat_t)
    
    input_beta <- 1 ############################# Should be a slider separately
    
    ##### Compute posterior
    prior_S <- sigma2_to_diagmatrix(prior_S_diag)
#    print(prior_S)
    posterior <- calc_posterior_from_prior(prior_mu, prior_S, thedat_PSI, thedat_t, input_beta)
    posterior$mu <- posterior$mu[,1]
    
    
    ##### Compute regular linear model
    themod <- lm(data=thedat,formula=paste(input$input_predict, "~.-1"))
    classic_mu <- themod$coefficients
    classic_mu <- classic_mu[param_names] #ensure the same order
    
    ##### Set nice names, possibly helping later
    names(prior_mu) <- param_names
    colnames(prior_S) <- param_names
    rownames(prior_S) <- param_names
    names(posterior$mu) <- param_names
    colnames(posterior$S) <- param_names
    rownames(posterior$S) <- param_names
    
    ##### Return value
    thefit <- list(
      classic_mu=classic_mu,
      
      prior_mu=prior_mu,
      prior_S=prior_S,

      post_mu=posterior$mu,
      post_S=posterior$S,
      
      param_names=param_names,
      numparam=length(prior_S_diag)
    )
    
#     print("===================== fitted equation ==============")
 #   print(thefit)
    
    thefit
  })
  
  #getPriorPosterior()
  
  
  ##############################################################################
  ########### Callbacks - dataset ##############################################
  ##############################################################################
  
  observeEvent(c(input$input_ds),{
    thedat <- getDataTable()
    numparam <- ncol(thedat)-1
    
    ######### Side bar
    updateSelectizeInput(session, 'input_predict', choices = colnames(thedat), server = TRUE, selected="Outcome")
    updateSliderInput(session, "numpoint", min=0, max=nrow(thedat), value = nrow(thedat), step = 1)
    
  })
  
  
  
  ##############################################################################
  ########### Callbacks - dataset & outcome ####################################
  ##############################################################################

  observeEvent(c(input$input_ds, input$input_predict),{
    thedat <- getDataTable()
    numparam <- ncol(thedat)-1
    all_param_name <- colnames(thedat)[colnames(thedat) != input$input_predict]
    
    
    ######### Side bar --- all prior sliders. these must be made reactive
    #https://mastering-shiny.org/action-dynamic.html#multiple-controls
    
    all_param_input <- list()
    names_param_mean_called <- paste("mean", all_param_name)
    names_param_sd_called <- paste("sd", all_param_name)

    ### updates, but only if using "outcome" slider, which is not connected
    
    #    print(names_param_mean_called)
    
    for(i in 1:length(all_param_name)){
      pname <- all_param_name[i]
      param_vals <- thedat[,pname]
      
      ########## Construct mean slider   #################### TODO use exact fit equation to inform
      
      #Ensure 0 can be used as a value
      param_range <- range(param_vals)
      if(param_range[1]>0){
        param_range[1] <- 0
      }
      if(param_range[2]<0){
        param_range[2] <- 0
      }
      
      param_range <- c(-10,10)
      
      all_param_input[[paste("param",pname,"mean")]] <- sliderInput(
        names_param_mean_called[i], label = paste(pname, ": Mean",sep=""),
        min = param_range[1], value = 0, max = param_range[2]
      )
      
      ########## Construct SD slider
      min_sd <- 0.001
      #    max_sd <- round(sd(param_vals)*5, digits=2)
      
      max_sd <- 5
#      max_sd <- ceiling(sd(param_vals)*5)
      cur_sd <- 1
      if(cur_sd>max_sd){
        cur_sd <- max_sd/2
      }
      
      all_param_input[[paste("param",pname,"sd")]] <- sliderInput(
        names_param_sd_called[i], label = paste(pname,": SD",sep=""),
        min = min_sd, value = cur_sd, max = max_sd
      )
    }
    output$priors <- renderUI(all_param_input)
    

    ######### Scatter plot
    updateSelectizeInput(session, 'input_scatter_x', choices = colnames(thedat), server = TRUE)
    updateSelectizeInput(session, 'input_scatter_y', choices = colnames(thedat), server = TRUE)

    ######### Posterior 2D
    updateSelectizeInput(session, 'input_posterior_x', choices = all_param_name, server = TRUE)
    updateSelectizeInput(session, 'input_posterior_y', choices = all_param_name, server = TRUE)

  })

  
  ##############################################################################
  ########### Fitted equation table ############################################
  ##############################################################################
  
  output$plotFittedTable <- renderTable({
    params <- getPriorPosterior()

    data.frame(
      Parameter=params$param_names,
      
      "Prior mean"=params$prior_mu,
      "Prior sd"=sqrt(diag(params$prior_S)),
      
      "Posterior mean"=params$post_mu,
      "Posterior sd"=sqrt(diag(params$post_S)),
      
      "Classic mean"=params$classic_mu
    )
  })

  ##############################################################################
  ########### Data table tab ###################################################
  ##############################################################################
  
  output$plotDataTable <- renderTable(getSubsetPoints(getDataTable()))
  
  
  ##############################################################################
  ########### Scatter plot tab #################################################
  ##############################################################################

  output$plotScatter <- renderPlotly({

    thedat <- getDataTable()
    thedat <- getSubsetPoints(thedat)

    if(input$input_scatter_x %in% colnames(thedat) & 
       input$input_scatter_y %in% colnames(thedat) &
       input$input_scatter_c %in% colnames(thedat)){

      ds <- data.frame(
        x=thedat[,input$input_scatter_x],
        y=thedat[,input$input_scatter_y],
        c=thedat[,input$input_scatter_c]
      )

      ggplotly(ggplot(ds,aes(x,y, color=c))+geom_point()) 
      
    } else {
      ds <- data.frame(x=c(),y=c())
      ggplotly(ggplot()) 
    }
    
  })

    
  ##############################################################################
  ########### Posteriors 1D plot tab ###########################################
  ##############################################################################
  
  #############
  ## Figure out range to show for one of the distribution axes
  getSensibleDistRangeToShow <- function(params, cur_axis){
    numsigma <- 3
    sensible_points_range <- c(
      params$prior_mu[cur_axis] - numsigma*sqrt(params$prior_S[cur_axis,cur_axis]),
      params$prior_mu[cur_axis] + numsigma*sqrt(params$prior_S[cur_axis,cur_axis]),
      params$post_mu[cur_axis] - numsigma*sqrt(params$post_S[cur_axis,cur_axis]),
      params$post_mu[cur_axis] + numsigma*sqrt(params$post_S[cur_axis,cur_axis])
    )
    sensible_points_range
    
    ##### new method 
    use_sd <- max(
      sqrt(params$prior_S[cur_axis,cur_axis]),
      sqrt(params$post_S[cur_axis,cur_axis])
    )
    
    sensible_points_range <- c(
      params$prior_mu[cur_axis] - numsigma*use_sd,
      params$prior_mu[cur_axis] + numsigma*use_sd,
      params$post_mu[cur_axis] - numsigma*use_sd,
      params$post_mu[cur_axis] + numsigma*use_sd
    )
    sensible_points_range
    
  }
  
  
  
  output$plotPosterior1D <- renderPlot({ #renderPlotly({

    params <- getPriorPosterior()

    allplots <- list()
    for(cur_axis in 1:params$numparam){

      #### Figure out range to show, sample x
      sensible_points_range <- getSensibleDistRangeToShow(params, cur_axis)
      posx <- seq(
        length.out=1000,
        from=min(sensible_points_range), 
        to=max(sensible_points_range))

      #### Get prior and posterior densities
      densx_prior <- dnorm(posx, 
                           mean=params$prior_mu[cur_axis], 
                           sd=sqrt(params$prior_S[cur_axis,cur_axis]))
      densx_prior <- densx_prior/max(densx_prior)
      
      densx_post <- dnorm(posx, 
                          mean=params$post_mu[cur_axis], 
                          sd=sqrt(params$post_S[cur_axis,cur_axis]))
      densx_post <- densx_post/max(densx_post)
      
      ### Plotting
      data.frame(x=posx, y=densx_prior, group="Prior")
      alldat <- rbind(
        data.frame(x=posx, y=  densx_prior, group="Prior"),
        data.frame(x=posx, y= -densx_post, group="Posterior")
      )
      
#      print(params)
 #     print(colnames(params$post_s))
      
#      print(params)
#      print(params$param_names)
      
      df_points <- data.frame(
        x=params$classic_mu[cur_axis],#,thedat[,params$param_names[cur_axis]],
        y=0,
        group="Classic mu"
      )

      allplots[[paste(cur_axis)]] <- 
        ggplot(alldat, aes(x,y,fill=group)) + geom_area() + 
        xlab(params$param_names[cur_axis]) + geom_point(data=df_points, mapping = aes(x,y), color="black")
      
      #TODO could also add all the points on top
      
    }
    totplot <- do.call(egg::ggarrange, c(allplots, nrow=length(allplots)))

    totplot
#    ggplotly(totplot) 
  })
  
  
  
  
  
  
  
  
  ##############################################################################
  ########### Posteriors 1D plot tab ###########################################
  ##############################################################################
  
  output$plotPosterior2D <- renderPlot({
      
    params <- getPriorPosterior()

    cur_axis_x <- which(params$param_names==input$input_posterior_x)
    cur_axis_y <- which(params$param_names==input$input_posterior_y)

    #### Figure out range to show, and sample
    sensible_points_range <- getSensibleDistRangeToShow(params, cur_axis_x)
    posx <- seq(
      length.out=500,
      from=min(sensible_points_range), 
      to=max(sensible_points_range))
    
    sensible_points_range <- getSensibleDistRangeToShow(params, cur_axis_y)
    posy <- seq(
      length.out=200,
      from=min(sensible_points_range), 
      to=max(sensible_points_range))
    
    #### Get prior and posterior densities
    posxy <- merge(data.frame(x=posx),data.frame(y=posy))
    densxy <- posxy
    
    densxy$prior_p <- mvtnorm::dmvnorm(posxy, 
                      mean=params$prior_mu[c(cur_axis_x, cur_axis_y)],
                      sigma=params$prior_S[c(cur_axis_x, cur_axis_y),c(cur_axis_x, cur_axis_y)])

    densxy$post_p <- mvtnorm::dmvnorm(posxy, 
                                       mean=params$post_mu[c(cur_axis_x, cur_axis_y)],
                                       sigma=params$post_S[c(cur_axis_x, cur_axis_y),c(cur_axis_x, cur_axis_y)])


    tile_w <- posx[2]-posx[1]
    tile_h <- posy[2]-posy[1]
    
    ### Plotting
    p1 <- ggplot(densxy, aes(x,y,fill=prior_p)) + 
      geom_tile()+#width=tile_w*1.1, height=tile_h*1.1)+#width=1) + 
      theme_classic()+
      ggtitle("Prior") + 
      xlab(input$input_prior_x) +
      ylab(input$input_prior_y) + 
      theme(legend.position = "none")
    
    
    p2 <- ggplot(densxy, aes(x,y,fill=post_p)) + 
      geom_tile()+#width=1) + 
      theme_classic()+
      ggtitle("Posterior") + 
      xlab(input$input_posterior_x) +
      ylab(input$input_posterior_y) + 
      theme(legend.position = "none")
    
    egg::ggarrange(p1,p2, ncol=2)
    
    ### Should really send proper images instead!!!! 
    
  })

}



