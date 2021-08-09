conditionerUI <- function(id){
  ns <- NS(id)
  
  tagList(
    {
      tabsetPanel(id = "tab_id",tabPanel("Model Introduction",
        h1("Hair Conditioner Quality - HW Chassis Model"),
        em("Model Owners: Amanda lane, Erin Bardsley"),
        
      ),
                  tabPanel("Model & Data Import",
                                                   wellPanel(
                                                    # h1("Hair Conditioner Quality - HW Chassis Model"),
                                                     #em("Model Owners: Amanda lane, Erin Bardsley"),
                                                     h2("Available Model Outputs"),
                                                     tags$ul(
                                                       tags$li(" This table presents the available model for this segment.")
                                                       ,tags$li(" Simulation will take place for all equations simultaneously.")
                                                     ),
                                                     dataTableOutput(ns("models"))
                                                   ) ,
                                         wellPanel(
                                           h3("Acceptable Range for User Input"),
                                           dataTableOutput(ns("advice"))
                                         ),
                                                   wellPanel(
                                                     h2("Simulation Data Inputs"),
                                                     p("Only one of manual data entry and external import is allowed. Select one and proceed.In manual entry user needs to input variable values 
                                          within the prescribed limits. And external import will allow user to bring in outside files for simulation."),
                                                     tabsetPanel(id = "simulation_entry",
                                                                 tabPanel("Manual Entry",
                                                                          dataTableOutput(ns("simulation_input")),
                                                                          actionButton(ns("commit"),"Go to simulation",icon("paper-plane"), 
                                                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                                 ),
                                                                 tabPanel("External Data Import",
                                                                          br(),
                                                                          fileInput(ns("input"),"Import Simulation Data"),
                                                                          dataTableOutput(ns("simulationdata")),
                                                                          actionButton(ns("commit2"), "Go to Visualization",icon("paper-plane"), 
                                                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                                     ),
                                                   ),
                                         ),
                  tabPanel("Visualization",
                           fluidRow(
                             wellPanel(
                               h4("Visual guide for imported data."),
                               tags$ul(
                                 tags$li(" This tab presents visualizations of the externally imported data."),
                                 tags$li(" First two graphs allow overlapping of two variables on Y axis."),
                                 tags$li(" While the first graph presents linear trend through line graph, the second graph presents linear regression based trend in data."),
                                 tags$li(" Finally a histogram is presented.")
                               ),
                             ),
                             wellPanel(
                               h2("Scatter Plot"),
                               selectInput(ns("x_axis"), 'X axis', choices = c(" "," ") ),
                               selectInput(ns("y_axis"), 'Y axis', choices = c(" "," ")
                                           ,selected = names(mtcars)[2], multiple = TRUE),
                               br(),
                               tags$ul(
                                 tags$li(" This scatter graph allows for double Y axis selection on single x axis."),
                                 tags$li(" It takes the first two selections of y-axis for plotting . Linear regression method is used for creating trendlines."),
                                 tags$li("Trend Line checkbox can be adding or removing the trend line in graph.")
                               ),
                               checkboxInput(ns("smooth"),"Trend Line", TRUE),
                               plotOutput(ns("ggscatter")),
                               checkboxInput(ns("smooth2"),"Multitrend", TRUE),
                               plotOutput(ns("ggsmooth")),
                               #plotOutput(ns("scatterplot"))
                             ),
                             
                             wellPanel(
                               h2("Histogram"),
                               selectInput(ns("hist_choice"), "Histogram Variable", c(" "," ")),
                               plotOutput(ns("hist"))
                             )
                           ),
                  ),
                  tabPanel("Simulation",
                           h3("Simulation Input Response"),
                           tabsetPanel(id = "simuPage",
                                       tabPanel("Manual Entry Simulation",
                                                h3("Input values for manual simulation"),
                                                dataTableOutput(ns("modeltable")),
                                                uiOutput(ns("Simulation_Result")),
                                                dataTableOutput(ns("result1")),
                                                actionButton(ns("simulate"),"Simulate on entered Data",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                # br(),
                                                # downloadButton(ns("download"),"Download above result"),
                                                # br(),
                                                # fileInput(ns("simres_upload"), "Upload previous results", multiple = TRUE),
                                                # br(),
                                                # dataTableOutput(ns("download_data"))
                                                downloadButton(ns("download1"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                
                                                ),
                                       tabPanel("Profiler",
                                                wellPanel(
                                                  h1("Profiler"),
                                                  br(),
                                                  fluidRow(column(width = 6,
                                                                  plotOutput(ns("plot1")),
                                                                  plotOutput(ns("plot2"))
                                                  ),
                                                  column(width = 6,
                                                         plotOutput(ns("plot3")),
                                                         plotOutput(ns("plot4"))
                                                  )),
                                                  fluidRow(column(width = 6,
                                                                  sliderInput(ns("solidscontent"),"Solids Content:", min = 0.85, max = 1, value = 0.9),
                                                                  sliderInput(ns("silversontipspeedduringemulsion"),"Silverson Tip Speed during Emulsion [m/s]:", min = 7.76, max = 23.26, value = 8),
                                                                  sliderInput(ns("silversontipspeedduringquench"),"Silverson Tip Speed during Quench  [m/s]:", min = 0, max = 23.26, value = 1),
                                                                  sliderInput(ns("silversontipspeedondischarge"),"Silverson Tip Speed on Discharge [m/s]:", min = 0, max = 23.26, value = 1),
                                                                  sliderInput(ns("silversontipspeedduringendmixing"),"Silverson Tip Speed during End Mixing [m/s]:", min = 0, max = 23.26, value = 1),
                                                                  sliderInput(ns("tempofsamplesondischarge"),"Temperature of samples on discharge [C]:", min = 38.34, max = 42.65, value = 39)
                                                  ),
                                                  column(width = 6,
                                                         sliderInput(ns("finalmixingtime"),"Final Mixing Time [mins]:", min = 8.08, max = 46.05, value = 9),
                                                         sliderInput(ns("updatedfatstemp"),"Updated Fats Temperature [C]:", min = 59.7, max = 77.7, value = 60),
                                                         sliderInput(ns("tempatpointofquenchinjection"),"Temp at point of Quench [C]:", min = 53.94, max = 59.77, value = 54),
                                                         sliderInput(ns("quenchinjectionrate"),"Quench Injection Rate [%kg/hr]:", min = 164.06, max = 837.99, value = 165),
                                                         sliderInput(ns("fatsinjectionrate"),"Fats Injection Rate [%kg/hr]:", min = 33.54, max = 117.51, value = 34)
                                                  ))
                                                )
                                       ),
                                       tabPanel("Imported Data Simulation",
                                                uiOutput(ns("Simulation_Result2")),
                                                dataTableOutput(ns("modeltable2")),
                                                br(),
                                                actionButton(ns("simulate2"),"Simulate on Imported Data",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                br(),
                                                plotOutput(ns("numvals")),
                                                downloadButton(ns("download2"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                
                                                ))
                           
                  ),
                  tabPanel("Optimization", 
                           h2("Since models are non-linear, optimization is pending."),
                           wellPanel(
                             h2("Global Download"),
                             h4("Download all the results that have been generated throughout the app"),
                             actionButton(ns("downloadresults"),"Proceed to download all Results", 
                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                             uiOutput(ns("Download_Values"))
                             
                             
                             #downloadButton(ns("download_all"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                             
                           ))
      )
    }
    
  )
}












