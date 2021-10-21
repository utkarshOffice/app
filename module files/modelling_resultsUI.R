modelling_resultsUI <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(column(width = 12,span(textOutput(ns('model_title_Text')),style="font-size: 25px;font-weight: bold"))),
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

    fluidRow(column(width = 12,span(textOutput(ns('model_perf_Text')),style="font-size: 20px;font-weight: bold"))),
    br(),

    fluidRow(column(width = 12,dataTableOutput(ns('scoresTable')))),
    br(),

    fluidRow(column(width = 6,span(textOutput(ns('model1FI_Text')), style="font-size: 16px;font-weight: bold"),
                    br(),
                    dataTableOutput(ns('model1FI'))),
             column(width = 6,span(textOutput(ns('model2FI_Text')),style="font-size: 16px;font-weight: bold"),
                    br(),
                    dataTableOutput(ns('model2FI')))),

    fluidRow(column(width = 6,span(textOutput(ns('model1FI_Plot1_Text')),style="font-size: 16px;font-weight: bold"),
                    br(),
                    plotlyOutput(ns('model1FI_Plot1'))),
             column(width = 6,span(textOutput(ns('model2FI_Plot1_Text')),style="font-size: 16px;font-weight: bold"),
                    br(),
                    plotlyOutput(ns('model2FI_Plot1')))),
    
    fluidRow(column(width = 6,span(textOutput(ns('model1_Eqn_Text')), style="font-size: 16px;font-weight: bold"),
                    br(),
                    textOutput(ns('model1_Eqn'))),
             column(width = 6,span(textOutput(ns('model2_Eqn_Text')),style="font-size: 16px;font-weight: bold"),
                    br(),
                    textOutput(ns('model2_Eqn'))))

  )
}