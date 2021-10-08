modelling_resultsUI <- function(id){
  ns <- NS(id)
  tagList(
    h2("Modelling Results"),
    br(),
    
    actionBttn(
      inputId = ns('refresh'),
      label = "Refresh",
      color = "success",
      style = "gradient",
      icon = icon("sync"),
      size= 'md',
      block = FALSE
    ),
    
    fluidRow(column(width = 12,h3('Model Performance:'))),
    
    fluidRow(column(width = 12,dataTableOutput(ns('scoresTable')))),
    
    fluidRow(column(width = 6,h3('MLR Feature Importances:'),dataTableOutput(ns('mlrFI'))),
             column(width = 6,h3('LASSO Feature Importances:'),dataTableOutput(ns('lassoFI')))),
    
    # fluidRow(column(width = 6,h3('MLR Feature Importances (Viz):'),plotOutput(ns('mlrFI_Plot'))),
    #          column(width = 6,h3('LASSO Feature Importances (Viz):'),plotOutput(ns('lassoFI_Plot')))),
    
    fluidRow(column(width = 6,h3('MLR Feature Importances:'),plotlyOutput(ns('mlrFI_Plot1'))),
             column(width = 6,h3('LASSO Feature Importances:'),plotlyOutput(ns('lassoFI_Plot1'))))
    
 
  #   useWaiter(),
  #   br(),
  #   br(),
  #   uiOutput(ns('results')),
  #   imageOutput(ns('1coef')),
  #   imageOutput(ns('1coef')),
  #   fluidRow(column(width = 6,img(src= "Plot_LASSO_coefs.jpg", height="100%", width="100%", align="left")),
  #            column(width = 6,img(src ="Plot_LASSO_Predicted.jpg", align = "left"))),
  #   #(dataTableOutput(ns("results")))
  )
}