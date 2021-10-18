modellerUI <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    
    fluidRow(column(width = 6,HTML(paste0("<b><i>","Select Laminate Family","</i></b>")))), 
    fluidRow(column(width = 6,
           
           switchInput(
             inputId = ns("laminateFamily"),
             onLabel = "Polymer",
             offLabel = "Paper",
             onStatus = "primary", 
             offStatus = "success",
             label='Switch',
             size = "normal"
          )
    )
    ),
    br(),
    
    fluidRow(column(width = 6,fileInput(ns('dataset'),em('Upload Dataset')))), 
    
    fluidRow(column(width = 6,selectInput(ns('material'),em("Select Material"), choices = c('',''))),
             column(width = 6,uiOutput(ns('materialNote')))), 
             
    
    selectInput(ns('response_var'),em("Select Response Variable"), choices = c('Seal Strength',''), selected = 'Seal Strength') , 
   
    
    
    fluidRow(column(width = 6,selectInput(ns('predictor_vars'),em("Select Predictors"), choices = c('',''), multiple = TRUE)),
             column(width = 6,uiOutput(ns('predictorNote')))),
    # actionButton(ns('build'),"BUILD",icon("paper-plane"), 
    #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    br(),
    materialSwitch(inputId = "outlierflag", label = em("Handle Outliers (Tool in development)"), status = "danger"),
    br(),
    
    actionBttn(
      inputId = ns('build'),
      label = "BUILD",
      color = "success",
      style = "simple",
      icon = icon("tools"),
      size= 'md',
      block = FALSE
    ),
    useWaiter(),
    br(),
    br(),
    fluidRow(column(width = 12,span(textOutput(ns('model_title1_Text')),style="font-size: 20px;font-weight: bold;font-variant: small-caps"))),
    br(),
    fluidRow(column(width = 12,span(textOutput(ns('model_title2_Text')),style="font-size: 16px;font-weight: bold"))),
    
    br(),
    uiOutput(ns('results')),
    br(),
    actionBttn(
      inputId = ns('goToResults'),
      label = "Go to Results",
      color = "success",
      style = "simple",
      icon = icon("paper-plane"),
      size= 'md',
      block = FALSE
    ),
    #uiOutput(ns('images'))
    #(dataTableOutput(ns("results")))
    bsPopover(id = ns("material"), title = 'NOTE:', content='Upload dataset to fetch all materials within it.',
              placement = "bottom", trigger = "hover"),
    bsPopover(id = ns("predictor_vars"), title = 'NOTE: ', content='Upload dataset to fetch all statistically significant predictors within it.', 
              placement = "bottom", trigger = "hover")

  )
}