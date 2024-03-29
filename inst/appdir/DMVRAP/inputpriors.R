# Define UI for random distribution application 
fluidPage(

  # controls to select the parameters
  fluidRow(
    column(3,
      h4("Production"),
      sliderInput("pMode", 
                  "mode:", 
                  value = 20,
                  min = 1, 
                  max = 40, step=0.1),
      sliderInput("pSig", 
                  "lognormal sigma:", 
                  value = 7,
                  min = 0.01, 
                  max = 10, step=0.001),
      sliderInput("pRange", 
                  "min & max:", 
                  value = c(0,40),
                  min = 0, 
                  max = 60)
    ),
    column(3,
      h4("Ln Capacity"),
      sliderInput("cMu", 
                  "mean:", 
                  value = 9,
                  min = 1, 
                  max = 20,step=.001),
      sliderInput("cSig", 
                  "std deviation:", 
                  value = 10,
                  min = 1, 
                  max = 200, step=.001),
      sliderInput("cRange", 
                  "min & max:", 
                  value = c(3,12),
                  min = .1, 
                  max = 20)
    ),
    column(3,
           h4("Marine Surv Coef"),
           sliderInput("msMu", 
                       "mean:", 
                       value = 0,
                       min = 0, 
                       max = 5, step=1),
           sliderInput("msSig", 
                       "std deviation:", 
                       value = 10,
                       min = 0, 
                       max = 20, step=.1),
           sliderInput("msRange", 
                       "min & max:", 
                       value = c(0,100),
                       min = 0, 
                       max = 100)
           
    ),
    column(3,
           h4("Freshwater Surv Coef"),
           sliderInput("flowMu", 
                       "mean:", 
                       value = 0,
                       min = -5, 
                       max = 0, step=1),
           sliderInput("flowSig", 
                       "std deviation:", 
                       value = 10,
                       min = 0, 
                       max = 20, step=.1),
           sliderInput("flowRange", 
                       "min & max:", 
                       value = c(-100,0),
                       min = -100, 
                       max = 0)
    )
    
    ),

    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    fluidRow(
      plotOutput("priorsplot")
    )
  )
