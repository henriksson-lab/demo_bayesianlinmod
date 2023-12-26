library(plotly)
library(shiny)
library(ggplot2)

################################################################################
########### Samplemeta #########################################################
################################################################################


tab_about <- fluidPage(
  p("This demo was originally developed by ", a("Johan Henriksson", href="http://www.henlab.org")),
  p("The posterior solution is taken from",
     a("Bishop",href="https://www.microsoft.com/en-us/research/uploads/prod/2006/01/Bishop-Pattern-Recognition-and-Machine-Learning-2006.pdf"),
    "equations 3.49-3.51"
  ),
  p("Licensed under 2-clause BSD license, https://opensource.org/license/bsd-2-clause/")
)

tab_datatable <- fluidPage(
  tableOutput(outputId = "plotDataTable")
)

tab_fittedtable <- fluidPage(
  tableOutput(outputId = "plotFittedTable")
)


tab_scatter <- fluidPage(
  
  selectInput(
    inputId = "input_scatter_x",
    label = "X:",
    selectize = TRUE,
    multiple = FALSE,
    choices = c(""), 
    selected = ""
  ),
  selectInput(
    inputId = "input_scatter_y",
    label = "Y:",
    selectize = TRUE,
    multiple = FALSE,
    choices = c(""), 
    selected = ""
  ),
  selectInput(
    inputId = "input_scatter_c",
    label = "Color:",
    selectize = TRUE,
    multiple = FALSE,
    choices = c("Outcome"), 
    selected = "Outcome"
  ),
  
  #plotOutput
  plotlyOutput(outputId = "plotScatter", height = "400px")
)




tab_posterior1d <- fluidPage(
  plotOutput(outputId = "plotPosterior1D", height = "1000px")
)



tab_posterior2d <- fluidPage(
  
  selectInput(
    inputId = "input_posterior_x",
    label = "X:",
    selectize = TRUE,
    multiple = FALSE,
    choices = c(""), 
    selected = ""
  ),
  
  selectInput(
    inputId = "input_posterior_y",
    label = "Y:",
    selectize = TRUE,
    multiple = FALSE,
    choices = c(""), 
    selected = ""
  ),

  plotOutput(outputId = "plotPosterior2D", height = "400px")
)



################################################################################
########### Total page #########################################################
################################################################################

#https://stackoverflow.com/questions/72040479/how-to-position-label-beside-slider-in-r-shiny

ui <- fluidPage(
  tags$style(HTML(
    "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .irs {
      flex-basis: 300px;          /* Target width for slider */
    }
    "
  )),
  
  titlePanel("Demo of Bayesian linear models"),

  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        inputId = "input_ds",
        label = "Dataset:",
        selectize = FALSE,
        multiple = FALSE,
        choices = names(available_datasets), #c("trivial.csv","diabetes.csv"), 
        selected = "trivial.csv"
      ),

      selectInput(
        inputId = "input_predict",
        label = "Predict:",
        selectize = TRUE,
        multiple = FALSE,
        choices = c("Outcome"), 
        selected = "Outcome"
      ),

      div(class = "label-left",
          
          sliderInput(
            inputId = "numpoint",
            label = "Use # points:",
            min=0,
            max=1,
            value=1
          ),
          
          sliderInput(
            inputId = "random_seed",
            label = "Random seed:",
            min=0,
            max=10,
            value=1
          ),
          
          uiOutput("priors")
      )

    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Fitted equation", tab_fittedtable),
                  tabPanel("Distribution 1D", tab_posterior1d),
                  tabPanel("Distribution 2D", tab_posterior2d),
                  tabPanel("Scatter plot", tab_scatter),
                  tabPanel("Data table", tab_datatable),
                  tabPanel("About", tab_about)
                  
      )
    )
  )
  
)



