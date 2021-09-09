chillrollUI <- function(id){
  ns <- NS(id)
  
  tagList(
    {
      tabsetPanel(id = "tabs_santosh",tabPanel("Model Introduction",
                                            h1("Chill Roll Mill Model"), 
                                            em("Model Owners: Aditi/Santosh"), 
      ),
      
      tabPanel("Model & Data Import",
               wellPanel(
                 # h1("Chill Roll Mill Model"),
                 #em("Model Owners:  Aditi/Santosh"), 
                 h2("Available Model Outputs"),
                 tags$ul(
                   tags$li(" This table presents the available model for this segment.")
                   ,tags$li(" Simulation will take place for all equations simultaneously.")
                 ),
                 dataTableOutput(ns("models_santosh")),
                 h3("Accepted range for user inputs"),
                 dataTableOutput(ns("advice_santosh"))
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
                                      h3("The table shows variables for Final Flake Temp Model 1 and Model 2 Predictions."),
                                      dataTableOutput(ns("simulation_input_santosh")),
                                      actionButton(ns("commit_santosh"),"Go to simulation",icon("paper-plane"), 
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6d7a4"),
                 ),
                 tabPanel("External Data Import",
                          tags$ul(
                            tags$li(" This segment provides simulation data import functionality."),
                            tags$li(" A time series excel file dataset is expected with the naming format of variables in equation.")
                          ),
                          fileInput(ns("datacall_santosh"),"Import Simulation Data"),
                          dataTableOutput(ns("simulationdata_santosh")),
                          actionButton(ns("commit2_santosh"), "Go to Visualization",icon("paper-plane"), 
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
                   plotOutput(ns("scatterplot_santosh")),
                   br(),
                   br(),
                   tags$ul(
                     tags$li(" This scatter graph allows for double Y axis selection on single x axis."),
                     tags$li(" It takes the first two selections of y-axis for plotting . Linear regression method is used for creating trendlines."),
                     tags$li("Trend Line checkbox can be adding or removing the trend line in graph.")
                   ),
                   checkboxInput(ns("smooth_santosh"),"Trend Line", TRUE),
                   plotOutput(ns("ggscatter_santosh")),
                   checkboxInput(ns("smooth2_santosh"),"Multitrend", TRUE),
                   plotOutput(ns("ggsmooth_santosh"))
                   #plotlyOutput(ns("multi_lines_graph_santosh"))
                   
                 ),
                 wellPanel(
                   h2("Histogram"),
                   selectInput(ns("hist_choice_santosh"), "Histogram Variable", c(" "," ")),
                   plotOutput(ns("hist_santosh"))
                 )
               ),
      ),
      tabPanel("Simulation",
               h3("Simulation Input Response"),
               tabsetPanel(id = "simuPage_santosh",
                           tabPanel("Manual Entry Simulation",
                                    #h3("Informative Plots"),
                                    #img(src= "SDslurry1.png"),
                                    #img(src= "SDslurry2.png"),
                                    h3("Input values taken for manual simulation."),
                                    dataTableOutput(ns("modeltable_santosh")),
                                    actionButton(ns("simulate_santosh"),"Simulate on entered Data",
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                    uiOutput(ns("simulation_heading_santosh")),
                                    dataTableOutput(ns("newvals_santosh")),
                                    uiOutput(ns("heading1_santosh")),
                                    dataTableOutput(ns("result1_santosh")),
                                    downloadButton(ns("download1_santosh"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                           ),
                           tabPanel("Profiler",
                                    wellPanel(
                                      h1("Profiler"),
                                      fluidRow(column(width = 4,plotOutput(ns("plot2"))),
                                               column(width = 4,plotOutput(ns("plot1"))),
                                               column(width = 4,plotOutput(ns("plot3")))),
                                      fluidRow(column(width = 6,
                                                      sliderInput(ns("profiler_Film_thickness"),"Film_thickness:", min = 0.5, max = 2.0, value = 0.72),
                                                      sliderInput(ns("profiler_Cooling_seg_fraction"),"Cooling_seg_fraction:", min = 0.5, max = 0.875, value = 0.78),
                                                      sliderInput(ns("profiler_T_flake_feed"),"T_flake_feed:", min = 100, max = 130, value = 110),
                                                      sliderInput(ns("profiler_T_ambient"),"T_ambient:", min = 15, max = 40, value = 21)
                                      ),
                                      column(width = 6,
                                             sliderInput(ns("profiler_T_chilled_water"),"T_chilled_water:", min = -5, max = 15, value = 7),
                                             sliderInput(ns("profiler_DEFI_Free_roll_length"),"DEFI_Free_roll_length:", min = 0.001, max = 0.1, value = 0.01),
                                             sliderInput(ns("profiler_Roll_Speed"),"Roll_Speed:", min = 1.0, max = 4.0, value = 2.0)
                                      ))
                                    )),
                           
                           
                           tabPanel("Imported Data Simulation",
                                    uiOutput(ns("heading_santosh")),
                                    dataTableOutput(ns("modeltable2_santosh")),
                                    br(),
                                    actionButton(ns("simulate2_santosh"),"Simulate on Imported Data",
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                    downloadButton(ns("download2_santosh"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                    ,
                                    
                                    
                           ))
               
      ),
      tabPanel("Optimisation",
               wellPanel( 
                 h2("Process optimiser (Non Linear Optimisation)"),
                 tags$ul(
                   h4("Target Variables :	Flake Final Temp (Model 1), Flake Final Temp (Model 2)  ")
                   ,br(),
                   tags$li("The optimisation page can be used to derive the values of the model predictors that are predicted based on specified Target variables."),
                   tags$li("The solution can be further constrained by minimizing or maximizing an objective function."),
                   tags$li("Objective function is defined as a linear combination of model predictors."),
                   tags$li("Select minimization or maximization as per need by clicking on the checkbox. "),
                   tags$li("Select the desired inequality from \"less than or equal to\", \"equal to\" and \"greater than or equal to\". "),
                   tags$li("Input the desired value of the Target variables. ")
                   
                 ),br()),
               
               wellPanel(
                 radioButtons(ns("radio_button_chill"),"Objective Type",choices=c("Minimization"="min","Maximization"="max"),inline = TRUE),
                 br(),
                 fluidRow(
                   h4("Flake Final Temp (Model 1)"),br(),
                   column(2,selectInput(ns("inequality_selection_one"),"Select the inequality type ",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                   column(2,offset= 2,numericInput(ns("numeric_input_one"),"Enter the Target variable value",45)),
                   column(2, offset = 3, numericInput(ns("weight_one"), "Enter the weight for the equation",1)),
                   br()),
                 
                 fluidRow(
                 h4("Flake Final Temp (Model 2)"),br(),
                 column(2,selectInput(ns("inequality_selection_two"),"Select the inequality type ",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                 column(2,offset= 2,numericInput(ns("numeric_input_two"),"Enter the Target variable value",45)),
                 column(2, offset = 3, numericInput(ns("weight_two"), "Enter the weight for the equation",1)),
                 br()),
                 
                 h2("Objective Function Table"), br(),
                 tags$li("Enter the allowed range for each model predictor by editing the 'Lower Bounds' and 'Upper Bounds' columns in the below table."),
                 tags$li("The objective function is defined as a linear combination of the predictors whose coefficients are given in the 'obj coeff' column. "),
                 tags$li("The values in this column are defaulted to one and they can be edited as per the requirements. "),
                 tags$li("Press the 'Run optimiser' button to generate the optimal solution."),
                 br(),
                 dataTableOutput(ns("optimiser_table1_chill"))),
               
               fluidRow(column(2,actionButton(ns("run_optimiser_chill"),"Run Optimiser",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                        column(2,actionButton(ns("reset_chill"),"Reset to defaults",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")) ),
               
               wellPanel(
                 h3("Results"),br(),
                 tags$li("The Target variable value associated with the predictor values, is shown in this table") ,br(),
                 dataTableOutput(ns("optimiser_table22_chill")),
                 h3("Predictors"),br(),
                 tags$li("The \"Predictors\" table show the optimised value taken by each predictor to obtain the optimised Target variable value. "),br(),
                 dataTableOutput(ns("optimiser_table32_chill")),
                 h4("Objective Function Value"),
                 uiOutput(ns("value_results_chill")),
                 downloadButton(ns("download5_santosh"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                 
               ),
               wellPanel(
                 h2("Global Download"),
                 h4("Download all the results that have been generated throughout the app"),
                 actionButton(ns("downloadresults_santosh"),"Proceed to download all Results",
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 uiOutput(ns("Download_Values_santosh"))
               )
               
               
      )#optimisation end
      
      )}
    
  )}







