modellerUI <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(ns('dataset'),'Upload Dataset'),
    selectInput(ns('material'),"Select Material ", choices = c('','')) , 
    selectInput(ns('response_var'),"Select Response Variable", choices = c('Mean(Seal Strength)',''), selected = 'Mean(Seal Strength)') , 
    selectInput(ns('predictor_vars'),"Select Predictor Variables", choices = c('',''), multiple = TRUE),
    # actionButton(ns('build'),"BUILD",icon("paper-plane"), 
    #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    br(),
    materialSwitch(inputId = "outlierflag", label = "Handle Outliers:", status = "danger"),
    br(),
    
    actionBttn(
      inputId = ns('build'),
      label = "BUILD",
      color = "success",
      style = "simple",
      icon = icon("paper-plane"),
      size= 'md',
      block = FALSE
    ),
    useWaiter(),
    br(),
    br(),
    uiOutput(ns('results')),
    #uiOutput(ns('images'))
    #(dataTableOutput(ns("results")))
  )
}