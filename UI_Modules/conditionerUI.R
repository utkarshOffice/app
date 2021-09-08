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
                  tabPanel("Optimisation",
                           wellPanel(
                             h2("Process Optimiser (Non Linear Optimisation)"),
                             wellPanel(
                               tags$ul(
                                 br(),
                                 h3("Target Variables : Fresh Viscosity [mPas], 24 Hour Viscosity [mPas],
                                     24 Hour Yield Stress [mPas], 1 Week Viscosity [mPas]"),
                                 br(),
                                 tags$li("The optimisation page can be used to derive the values of the model predictors that are predicted based on specific values of the Target variables."),
                                 tags$li("The solution can be further constrained by minimizing or maximizing an objective function."),
                                 tags$li("Objective function is defined as a linear combination of model predictors."),
                                 tags$li("Select minimization or maximization as per need by clicking on the checkbox. "),
                                 tags$li("Select the desired inequality from \"less than or equal to\", \"equal to\" and \"greater than or equal to\". "),
                                 tags$li("Input the desired values of the Target variables. "),
                                 tags$li("Enter the weights (relative importance) of the target variables; by default it is 1.")

                               ),br()),

                             wellPanel(
                               radioButtons(ns("radio_button_erin"),"Objective Type",choices=c("Minimization"="min","Maximization"="max"),inline = TRUE),
                               br(),
                                 fluidRow(h3("Fresh Viscosity [mPas]"),br(),
                                 column(2,selectInput(ns("inequality_selection_erin_one"),"Select the inequality",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                                 column(2,offset= 2,numericInput(ns("numeric_input_erin_one"),"Enter the target value",250)),
                                 column(2, offset = 3, numericInput(ns("weight_erin_one"), "Enter the weight",1)),
                                 br()),

                               fluidRow( h3("24 Hour Viscosity [mPas]"),br(),
                                 column(2,selectInput(ns("inequality_selection_erin_two"),"Select the inequality",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                                 column(2,offset= 2,numericInput(ns("numeric_input_erin_two"),"Enter the target value",400)),
                                 column(2, offset = 3, numericInput(ns("weight_erin_two"), "Enter the weight",1)),
                                 br()),

                               fluidRow( h3(" 24 Hour Yield Stress [mPas]"),br(),
                                 column(2,selectInput(ns("inequality_selection_erin_three"),"Select the inequality",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                                 column(2,offset= 2,numericInput(ns("numeric_input_erin_three"),"Enter the target value",150)),
                                 column(2, offset = 3, numericInput(ns("weight_erin_three"), "Enter the weight",1)),
                                 br()),

                               fluidRow( h3(" 1 Week Viscosity [mPas]"),br(),
                                 column(2,selectInput(ns("inequality_selection_erin_four"),"Select the inequality",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                                 column(2,offset= 2,numericInput(ns("numeric_input_erin_four"),"Enter the target value",550)),
                                 column(2, offset = 3, numericInput(ns("weight_erin_four"), "Enter the weight",1)),
                                 br()),

                               # wellPanel(
                               h2("Objective Function Table"), br(),
                               tags$li("Enter the allowed range for each model predictor by editing the 'Lower Bounds' and 'Upper Bounds' columns in the below table."),
                               tags$li("The objective function is defined as a linear combination of the predictors whose coefficients are given in the 'obj coeff' column. "),
                               tags$li("The values in this column are defaulted to one and they can be edited as per the requirements. "),
                               tags$li("Press the 'Run optimiser' button to generate the optimal solution."),
                               br(),
                               dataTableOutput(ns("optimiser_table1_erin"))),
                             fluidRow(column(2,actionButton(ns("run_optimiser_erin"),"Run Optimiser",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                      column(2,actionButton(ns("reset_erin"),"Reset to defaults",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")) ),
                             wellPanel(
                               h3("Results"),br(),
                               tags$li("The target variable values associated with the predictor values, is shown in this table") ,br(),
                               dataTableOutput(ns("optimiser_table22_erin")),
                               h3("Predictors"),br(),
                               tags$li("The \"Predictors\" table show the optimised value taken by each predictor to obtain the above target variable values. "),br(),
                               dataTableOutput(ns("optimiser_table32_erin")),
                               h4("Objective Function Value"),
                               uiOutput(ns("value_results_erin")),
                               downloadButton(ns("download5_erin"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")

                             ),
                             wellPanel(
                               h2("Global Download"),
                               h4("Download all the results that have been generated throughout the app"),
                               actionButton(ns("downloadresults_erin"),"Proceed to download all Results",
                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                               uiOutput(ns("Download_Values_erin"))
                                     )

                           ))
                  
      )}
    
  )}












