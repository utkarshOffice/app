lather_volumeUI <- function(id){
  ns <- NS(id)
  
  tagList(
    {
      tabsetPanel(id = "tabs_uday_sd",tabPanel("Model Introduction",
                                               h1("Soap Bar Performance Models - Lather Volume"),
                                               em("Model Owner: Pavitra")),
                  tabPanel("Model & Data Import",
                           wellPanel(
                             # h1("Finished Goods Bulk Density(SD Powder) Model"),
                             # em("Model Owner: Uday Baviskar"),
                             h2("Available Model Outputs"),
                             tags$ul(
                               tags$li(" This table presents the available models for this segment.")
                               ,tags$li(" Simulation will take place for all equations simultaneously.")
                             ),
                             dataTableOutput(ns("models_uday_sd"))
                           )
                           ,
                           wellPanel(
                             h2("Simulation Data Inputs"),
                             p("Only one of manual data entry and external import is allowed. Select one and proceed."),
                             tabsetPanel(tabPanel("Manual Entry",
                                                  tags$ul(
                                                    tags$li(" Following table is editable. Enter the simulation values."),
                                                    tags$li(" Entered values will automatically be saved for simulation stage.")
                                                  ),
                                                  dataTableOutput(ns("simulation_input_uday_sd")),
                                                  actionButton(ns("commit_uday_sd"),"Go to simulation",icon("paper-plane"), 
                                                               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                  br(),
                             ),
                             tabPanel("External Data Import",
                                      tags$ul(
                                        tags$li(" This segment provides simulation data import functionality."),
                                        tags$li(" A time series excel file dataset is expected with the naming format of TurningPoint equation.")
                                      ),
                                      fileInput(ns("datacall_uday_sd"),"Import Simulation Data"),
                                      dataTableOutput(ns("simulationdata_uday_sd")),
                                      actionButton(ns("commit2_uday_sd"), "Go to Visualization",icon("paper-plane"), 
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
                               ),
                             ),
                             wellPanel(
                               h2("Scatter Plot"),
                               tags$ul(
                                 tags$li(" This scatter graph allows for multiple Y axis selection on single x axis."),
                                 tags$li(" Maximum 4 selections are allowed for Y axis. Linear regression method is used for creating trendlines.")
                               ),
                               selectInput(ns("x_axis_sd"), 'X axis', choices = c(" "," ") ),
                               selectInput(ns("y_axis_sd"), 'Y axis', choices = c(" "," ")
                                           ,selected = names(mtcars)[2], multiple = TRUE),
                               plotOutput(ns("scatterplot_uday_sd")),
                               br(),
                               br(),
                               tags$ul(
                                 tags$li(" This scatter graph allows for double Y axis selection on single x axis."),
                                 tags$li(" It takes the first two selections of y-axis for plotting . Linear regression method is used for creating trendlines."),
                                 tags$li("Trend Line checkbox can be adding or removing the trend line in graph.")
                               ),
                               checkboxInput(ns("smooth_uday_sd"),"Trend Line", TRUE),
                               plotOutput(ns("ggscatter_uday_sd")),
                               checkboxInput(ns("smooth2_uday_sd"),"Multitrend", TRUE),
                               plotOutput(ns("ggsmooth_uday_sd"))
                             ),
                             wellPanel(
                               h2("Histogram"),
                               selectInput(ns("hist_choice_uday_sd"), "Histogram Variable", c(" "," ")),
                               plotOutput(ns("hist_uday_sd"))
                             )
                           ),
                  ),
                  tabPanel("Simulation",
                           h3("Simulation Input Response"),
                           tabsetPanel(id = "simuPage_uday_sd",
                                       tabPanel("Manual Entry Simulation",
                                                h3("Input values for manual Simulation"),
                                                dataTableOutput(ns("modeltable_uday_sd")),
                                                actionButton(ns("simulate_uday_sd"),"Simulate on entered Data",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                uiOutput(ns("simulation_result_uday_sd")),
                                                dataTableOutput(ns("result1_uday_sd")),
                                                downloadButton(ns("download1_uday_sd"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                       ),
                                       
                                       tabPanel("Profiler",
                                                wellPanel(
                                                  h1("Profiler"),
                                                  fluidRow(column(width = 10,plotOutput(ns("plot"))),
                                                  ),
                                                  
                                                  fluidRow(column(width = 6,
                                                                  sliderInput(ns("sodiumchloride"),"Sodium Chloride:", min = 0, max = 100, value = 1),
                                                                  sliderInput(ns("sodiumsulfate"),"Sodium Sulfate:", min = 0, max = 100, value = 1),
                                                                  sliderInput(ns("sodiumsilicate"),"Sodium Silicate:", min = 0, max = 100, value = 1),
                                                                  sliderInput(ns("ffacombined"),"FFA_Combined:", min = 0, max = 100, value = 1),
                                                                  sliderInput(ns("oxiflowoxiteno"),"Oxiflow-Oxiteno:", min = 0, max = 100, value = 1),
                                                                  sliderInput(ns("sodiumcarbonate"),"Sodium Carbonate:", min = 0, max = 100, value = 1)
                                                  ),
                                                  column(width = 6,
                                                         sliderInput(ns("sodiumcitrate"),"Sodium Citrate:", min = 0, max = 100, value = 1),
                                                         sliderInput(ns("pkocontent"),"PKO Content:", min = 0, max = 100, value = 1),
                                                         sliderInput(ns("titaniumdioxide"),"Titanium Dioxide:", min = 0, max = 100, value = 1),
                                                         sliderInput(ns("tinopalcbs"),"Tinopal CBS:", min = 0, max = 100, value = 1),
                                                         sliderInput(ns("perfume"),"Perfume:", min = 0, max = 100, value = 1),
                                                         sliderInput(ns("iv"),"IV (Iodine Value):", min = 0, max = 100, value = 1)
                                                  ))
                                                )),
                                       tabPanel("Imported Data Simulation",
                                                uiOutput(ns("simulation_result_uday_sd2")),
                                                dataTableOutput(ns("modeltable2_uday_sd")),
                                                br(),
                                                actionButton(ns("simulate2_uday_sd"),"Simulate on Imported Data",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                downloadButton(ns("download2_uday_sd"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                
                                                
                                       ))
                           
                  ),
                  tabPanel("Optimisation",
                           wellPanel( 
                             h2("Process optimiser (Non Linear Optimisation)"),
                             tags$ul(
                               h4("Target Variable :	Lather volume  ")
                               ,br(),
                               tags$li("The optimisation page can be used to derive the values of the model predictors that are predicted based on a specified Target variable."),
                               tags$li("The solution can be further constrained by minimizing or maximizing an objective function."),
                               tags$li("Objective function is defined as a linear combination of model predictors."),
                               tags$li("Select minimization or maximization as per need by clicking on the checkbox. "),
                               tags$li("Select the desired inequality from \"less than or equal to\", \"equal to\" and \"greater than or equal to\". "),
                               tags$li("Input the desired value of the Target variable. ")
                               
                             ),br()),
                           
                           wellPanel(
                             radioButtons(ns("radio_button_lather"),"Objective Type",choices=c("Minimization"="min","Maximization"="max"),inline = TRUE),
                             br(),
                             fluidRow(
                               column(2,selectInput(ns("inequality_selection_lather"),"Select the inequality type ",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                               column(2,offset= 2,numericInput(ns("numeric_input_lather"),"Enter the Target variable value",280)),
                               # column(2, offset = 3, numericInput(ns("weight_torque"), "Enter the weight for the Torque equation",1)),
                               br()),
                             
                             h2("Objective Function Table"), br(),
                             tags$li("Enter the allowed range for each model predictor by editing the 'Lower Bounds' and 'Upper Bounds' columns in the below table."),
                             tags$li("The objective function is defined as a linear combination of the predictors whose coefficients are given in the 'obj coeff' column. "),
                             tags$li("The values in this column are defaulted to one and they can be edited as per the requirements. "),
                             tags$li("Press the 'Run optimiser' button to generate the optimal solution."),
                             br(),
                             dataTableOutput(ns("optimiser_table1_lather"))),
                           
                           fluidRow(column(2,actionButton(ns("run_optimiser_lather"),"Run Optimiser",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                    column(2,actionButton(ns("reset_lather"),"Reset to defaults",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")) ),
                           
                           wellPanel(
                             h3("Results"),br(),
                             tags$li("The Target variable value associated with the predictor values, is shown in this table") ,br(),
                             dataTableOutput(ns("optimiser_table22_lather")),
                             h3("Predictors"),br(),
                             tags$li("The \"Predictors\" table show the optimised value taken by each predictor to obtain the optimised Target variable value. "),br(),
                             dataTableOutput(ns("optimiser_table32_lather")),
                             h4("Objective Function Value"),
                             uiOutput(ns("value_results_lather")),
                             downloadButton(ns("download5_lather"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                             
                           ),
                           wellPanel(
                             h2("Global Download"),
                             h4("Download all the results that have been generated throughout the app"),
                             actionButton(ns("downloadresults_lather"),"Proceed to download all Results",
                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                             uiOutput(ns("Download_Values_lather"))
                           )
                           
                           
                  )#optimisation end
      )}
    )}