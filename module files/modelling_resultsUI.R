modelling_resultsUI <- function(id){
  ns <- NS(id)
  tagList(
    

    
    #uiOutput(ns('results'))
    
    # fluidRow(column(width = 6,h3('MLR Feature Importances (Viz):'),plotOutput(ns('mlrFI_Plot'))),
    #          column(width = 6,h3('LASSO Feature Importances (Viz):'),plotOutput(ns('lassoFI_Plot')))),
    fluidRow(column(width = 12,span(textOutput(ns('model_title_Text')),style="font-size: 25px;font-variant: small-caps;font-weight: bold"))),
    br(),
     actionBttn(
      inputId = ns('refresh'),
      label = "Refresh",
      color = "success",
      style = "simple",
      icon = icon("sync"),
      size= 'md',
      block = FALSE
    ),
    
    br(),
    br(),
    # fluidRow(column(width = 12,textOutput(ns('material')))),
    # br(),
    # fluidRow(column(width = 12,dataTableOutput(ns('predictors')))),
    # br(),
    fluidRow(column(width = 12,span(textOutput(ns('model_perf_Text')),style="font-size: 20px;font-weight: bold"))),
    br(),

    fluidRow(column(width = 12,dataTableOutput(ns('scoresTable')))),
    br(),

    fluidRow(column(width = 6,span(textOutput(ns('mlrFI_Text')), style="font-size: 20px;font-weight: bold"),br(),dataTableOutput(ns('mlrFI'))),
             column(width = 6,span(textOutput(ns('lassoFI_Text')),style="font-size: 20px;font-weight: bold"),br(),dataTableOutput(ns('lassoFI')))),

    fluidRow(column(width = 6,span(textOutput(ns('mlrFI_Plot1_Text')),style="font-size: 20px;font-weight: bold"),br(),plotlyOutput(ns('mlrFI_Plot1'))),
             column(width = 6,span(textOutput(ns('lassoFI_Plot1_Text')),style="font-size: 20px;font-weight: bold"),br(),plotlyOutput(ns('lassoFI_Plot1'))))
    
 
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