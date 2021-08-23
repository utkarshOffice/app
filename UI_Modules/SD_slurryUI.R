SD_slurryUI <- function(id){
  ns <- NS(id)
  
  tagList(
    {
      tabsetPanel(id = "tabs_uday",tabPanel("Model Introduction",
                                            h1("Spray Dried Slurry Properties Model"), 
                                            em("Model Owners: Uday Baviskar"), 
                                            ),
                  
                  tabPanel("Model & Data Import",
                                            wellPanel(
                                             # h1("Spray Dried Slurry Properties Model"),
                                              #em("Model Owners: Uday Baviskar"), 
                                              h2("Available Model Outputs"),
                                              tags$ul(
                                                tags$li(" This table presents the available model for this segment.")
                                                ,tags$li(" Simulation will take place for all equations simultaneously.")
                                              ),
                                              dataTableOutput(ns("models_uday")),
                                              h3("Accepted range for user inputs"),
                                              dataTableOutput(ns("advice_uday"))
                                            )
                                            ,
                                            wellPanel(
                                              h2("Simulation Data Inputs"),
                                              p("Only one of manual data entry and external import is allowed. Select one and proceed.In manual entry user needs to input variable values 
                                          within the prescribed limits. And external import will allow user to bring in outside files for simulation."),
                                              tabsetPanel(tabPanel("Manual Entry",
                                                                   tags$ul(
                                                                     tags$li(" Following table is editable. Enter the simulation values."),
                                                                     tags$li(" Entered values will automatically be saved for simulation stage.")
                                                                   ),
                                                                   h3("The table shows variables for the Pred Formula Low Sheer Viscosity, Torque 300 Equation and Drying Prediction."),
                                                                   dataTableOutput(ns("simulation_input_uday")),
                                                                   actionButton(ns("commit_uday"),"Go to simulation",icon("paper-plane"), 
                                                                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                              ),
                                              tabPanel("External Data Import",
                                                       tags$ul(
                                                         tags$li(" This segment provides simulation data import functionality."),
                                                         tags$li(" A time series excel file dataset is expected with the naming format of variables in equation.")
                                                       ),
                                                       fileInput(ns("datacall_uday"),"Import Simulation Data"),
                                                       dataTableOutput(ns("simulationdata_uday")),
                                                       actionButton(ns("commit2_uday"), "Go to Visualization",icon("paper-plane"), 
                                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                              ),
                                              )),
                  tabPanel("Visualization",
                           fluidRow(
                             wellPanel(
                               h4("Visual guide for imported data."),
                               tags$ul(
                                 tags$li(" This tab presents visualizations of the externally imported data."),
                                 tags$li(" First two graphs allow overlapping of two variables on Y axis."),
                                 tags$li(" While the first graph presents linear trend through line graph, the second graph presents linear regression based trend in data."),
                                 tags$li(" Third graph shows scatter plot as well as trend lines. Upto 4 variables are allowed in this graph on Y axis."),
                                 tags$li(" Finally a histogram is presented.")
                               )
                             ),
                             wellPanel(
                               h2("Scatter Plot"),
                               tags$ul(
                                 tags$li(" This scatter graph allows for multiple Y axis selection on single x axis."),
                                 tags$li(" Maximum 4 selections are allowed for Y axis. Linear regression method is used for creating trendlines.")
                               ),
                               selectInput(ns("x_axis_bd"), 'X axis', choices = c(" "," ") ),
                               selectInput(ns("y_axis_bd"), 'Y axis', choices = c(" "," ")
                                           ,selected = names(mtcars)[2], multiple = TRUE),
                               plotOutput(ns("scatterplot_uday")),
                               br(),
                               br(),
                               tags$ul(
                                 tags$li(" This scatter graph allows for double Y axis selection on single x axis."),
                                 tags$li(" It takes the first two selections of y-axis for plotting . Linear regression method is used for creating trendlines."),
                                 tags$li("Trend Line checkbox can be adding or removing the trend line in graph.")
                               ),
                               checkboxInput(ns("smooth_uday"),"Trend Line", TRUE),
                               plotOutput(ns("ggscatter_uday")),
                               checkboxInput(ns("smooth2_uday"),"Multitrend", TRUE),
                               plotOutput(ns("ggsmooth_uday"))
                               #plotlyOutput(ns("multi_lines_graph_uday"))
                               
                             ),
                             wellPanel(
                               h2("Histogram"),
                               selectInput(ns("hist_choice_uday"), "Histogram Variable", c(" "," ")),
                               plotOutput(ns("hist_uday"))
                             )
                           ),
                  ),
                  tabPanel("Simulation",
                           h3("Simulation Input Response"),
                           tabsetPanel(id = "simuPage_uday",
                                       tabPanel("Manual Entry Simulation",
                                                h3("Informative Plots"),
                                                #img(src= "SDslurry1.png"),
                                                img(src= "SDslurry2.png"),
                                                h3("Input values taken for manual simulation."),
                                                dataTableOutput(ns("modeltable_uday")),
                                                actionButton(ns("simulate_uday"),"Simulate on entered Data",
                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                uiOutput(ns("simulation_heading_uday")),
                                                dataTableOutput(ns("newvals_uday")),
                                                uiOutput(ns("heading1_uday")),
                                                dataTableOutput(ns("result1_uday")),
                                                downloadButton(ns("download1_uday"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                ),
                                       tabPanel("Profiler",
                                                wellPanel(
                                                  h1("Profiler"),
                                                  fluidRow(column(width = 4,plotOutput(ns("plot2"))),
                                                           column(width = 4,plotOutput(ns("plot1"))),
                                                           column(width = 4,plotOutput(ns("plot3")))),
                                                  fluidRow(column(width = 6,
                                                                  sliderInput(ns("profiler_targetsmc"),"TargetSMC:", min = 0.287, max = 0.37, value = 0.31),
                                                                  sliderInput(ns("profiler_sulphate"),"Sulphate (dry basis):", min = 0.17, max = 0.52, value = 0.28),
                                                                  sliderInput(ns("profiler_lsa"),"LSA (dry basis):", min = 0.17, max = 0.45, value = 0.19),
                                                                  sliderInput(ns("profiler_nalas"),"NaLAS (dry basis):", min = 0.13, max = 0.41, value = 0.21)
                                                  ),
                                                  column(width = 6,
                                                         sliderInput(ns("profiler_scmc"),"SCMC (dry basis):", min = 0.00, max = 0.01, value = 0.00),
                                                         sliderInput(ns("profiler_alksilicate"),"Alkaline Silicate (dry basis):", min = 0.07, max = 0.16, value = 0.11),
                                                         sliderInput(ns("profiler_cp5"),"CP5 (dry basis):", min = 0.00, max = 0.03, value = 0.02)
                                                  ))
                                                )),

                                       
                                       tabPanel("Imported Data Simulation",
                                                uiOutput(ns("heading_uday")),
                                                dataTableOutput(ns("modeltable2_uday")),
                                                br(),
                                                actionButton(ns("simulate2_uday"),"Simulate on Imported Data",
                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                downloadButton(ns("download2_uday"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                ,

                                                
                                       ))
                           
                  ),
                  tabPanel("Optimisation",
                           tabsetPanel(tabPanel("Linear Optimisation",
                             wellPanel(
                               tags$ul(
                                 h2("Process Optimiser for Drying Prediction "),
                                 br(),
                                 h5("-Non Linear optimisation ought to be used for the equation based on 'Torque' and 'Pred Formula Low Sheer Viscosity' "),
                                 tags$li("The optimisation page can be used to derive the values of the model predictors that are predicted based on a specified Drying Prediction value."),
                                 tags$li("The solution can be further constrained by minimizing or maximizing an objective function."),
                                 tags$li("Objective function is defined as a linear combination of model predictors."),
                                 tags$li("Select minimization or maximization as per need by clicking on the checkbox. "),
                                 tags$li("Select the desired inequality from \"less than or equal to\", \"equal to\" and \"greater than or equal to\". "),
                                 tags$li("Input the desired value of Drying Prediction, by default it is set to 0.32. ")
                               ),
                               br() ),
                             wellPanel(
                               fluidRow(radioButtons(ns("radio_button_uday"),"Objective Type",choices=c("Minimization"="min","Maximization"="max"),inline = TRUE),
                                        br(),
                                        column(2,selectInput(ns("inequality_selection_uday"),"Select the inequality type",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                                        column(2,offset=3,numericInput(ns("numeric_input_uday"),"Enter the target Drying Prediction Value",0.32))),
                               
                               wellPanel(
                                 h2("Objective Function Table"), br(),
                                 
                                 tags$li("Enter the allowed range for each model predictor by editing the 'Lower Bounds' and 'Upper Bounds' columns in the below table."),
                                 tags$li("The objective function is defined as a linear combination of the predictors whose coefficients are given in the 'obj coeff' column. "),
                                 tags$li("The values in this column are defaulted to one and they can be edited as per the requirements. "),
                                 tags$li("Press the 'Run optimiser' button to generate the optimal solution.")),
                               br(),
                               dataTableOutput(ns("optimiser_table1_uday"))),
                             
                             fluidRow(column(2,actionButton(ns("run_optimiser_uday"),"Run Optimiser",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                      column(2,actionButton(ns("reset_uday"),"Reset to defaults",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")) ),
                             wellPanel(
                               h3("Results"),
                               tags$li("The \"Results\" table shows the value of Drying Prediction obtained.") ,br(),
                               dataTableOutput(ns("optimiser_table2_uday")),
                               h3("Predictors"),br(),
                               tags$li("The \"Predictors table\" shows the optimised value taken by each predictor to obtain the above Drying Prediction value. "),br(),
                               dataTableOutput(ns("optimiser_table3_uday")),
                               h4("Objective Function Value"),
                               uiOutput(ns("value_results_uday")),
                               downloadButton(ns("download3_uday"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), br()
                               
                             ),
                             wellPanel(
                               h2("Global Download"),
                               h4("Download all the results that have been generated throughout the app"),
                               actionButton(ns("downloadresults_uday"),"Proceed to download all Results",
                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                               uiOutput(ns("Download_Values_uday"))
                             )
                           ), #tabpanel linear opt end
                           
                           tabPanel("Non Linear Optimisation" ,
                                    fluidRow(
                                      wellPanel(
                                        tags$ul(
                                          h2("Process Optimiser for Torque and Pred Formula Low Sheer Viscosity")
                                          ,br(),
                                          tags$li("The optimisation page can be used to derive the values of the model predictors that are predicted based on a specified Torque & Pred Formula Low Sheer Viscosity values."),
                                          tags$li("The solution can be further constrained by minimizing or maximizing an objective function."),
                                          tags$li("Objective function is defined as a linear combination of model predictors."),
                                          tags$li("Select minimization or maximization as per need by clicking on the checkbox. "),
                                          tags$li("Select the desired inequality from \"less than or equal to\", \"equal to\" and \"greater than or equal to\". "),
                                          tags$li("Input the desired value of Torque and Pred Formula Low Sheer Viscosity. ")
                                          
                                        ),br()),
                                      
                                      wellPanel(fluidRow(
                                        radioButtons(ns("radio_button_uday_torque"),"Objective Type(Torque)",choices=c("Minimization"="min","Maximization"="max"),inline = TRUE),
                                        column(2,selectInput(ns("inequality_selection_uday_torque"),"Select the inequality type (Torque)",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                                        column(2,offset= 2,numericInput(ns("numeric_input_uday_torque"),"Enter the target Torque value",28)),
                                        column(2, offset = 3, numericInput(ns("weight_torque"), "Enter the weight for the Torque equation",1)),
                                        br()),
                                        
                                        fluidRow(
                                        radioButtons(ns("radio_button_uday_pred"),"Objective Type(Pred Formula Low Sheer Viscosity)",choices=c("Minimization"="min","Maximization"="max"),inline = TRUE),
                                        column(2,selectInput(ns("inequality_selection_uday_pred"),"Select the inequality type (Pred Formula Low Sheer Viscosity)",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                                        column(2,offset= 2,numericInput(ns("numeric_input_uday_pred"),"Enter the target Pred Formula Low Sheer Viscosity value",33)),
                                        column(2, offset = 3, numericInput(ns("weight_pred"), "Enter the weight for the Pred Formula Low Sheer Viscosity equation",1)),
                                        br()),
                                        
                                        wellPanel(
                                          h2("Objective Function Table"), br(),
                                          tags$li("Enter the allowed range for each model predictor by editing the 'Lower Bounds' and 'Upper Bounds' columns in the below table."),
                                          tags$li("The objective function is defined as a linear combination of the predictors whose coefficients are given in the 'obj coeff' column. "),
                                          tags$li("The values in this column are defaulted to one and they can be edited as per the requirements. "),
                                          tags$li("Press the 'Run optimiser' button to generate the optimal solution.")),
                                        br(),
                                        dataTableOutput(ns("optimiser_table1_uday_non_linear"))),
                                      fluidRow(column(2,actionButton(ns("run_optimiser_non_linear"),"Run Optimiser",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                               column(2,actionButton(ns("reset_non_linear"),"Reset to defaults",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")) ),
                                      wellPanel(
                                        h3("Results"),br(),
                                        tags$li("The Torque and Pred Formula Low Sheer Viscosity values associated with the predictor values, is shown in this table") ,br(),
                                        dataTableOutput(ns("optimiser_table22_uday_torque")),
                                        h3("Predictors"),br(),
                                        tags$li("The \"Predictors\" table show the optimised value taken by each predictor to obtain the optimised Torque value & 
                                                Pred Formula Low Sheer Viscosity values. "),br(),
                                        dataTableOutput(ns("optimiser_table32_uday_torque")),
                                        h4("Objective Function Value"),
                                        uiOutput(ns("value_results_uday_torque")),
                                        downloadButton(ns("download5"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                        
                                      ),
                                      wellPanel(
                                        h2("Global Download"),
                                        h4("Download all the results that have been generated throughout the app"),
                                        actionButton(ns("downloadresults"),"Proceed to download all Results",
                                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        uiOutput(ns("Download_Values"))
                                      )
                                      
                                    )#fluidrow end
                                    
                           )#tabpanel non linear opt end
                           
                           
                           )
                           ) #tabpanel optimisation end
                  
      )}
  )}







