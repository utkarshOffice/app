NTR_powderUI <- function(id){
  ns <- NS(id)
  
  tagList(
    
    {
      tabsetPanel(id = "tabs_uday_ntr",tabPanel("Model Introduction",
        h1("Finished Goods Bulk Density( NTR Powder) Model"),
        em("Model Owner: Uday Baviskar"),
                                                
      ),
                  
                  tabPanel("Model & Data Import",
                                                wellPanel(
                                                 # h1("Finished Goods Bulk Density( NTR Powder) Model"),
                                                 # em("Model Owner: Uday Baviskar"),
                                                  h2("Available Model Outputs"),
                                                  tags$ul(
                                                    tags$li(" This table presents the available model for this segment.")
                                                    ,tags$li(" Simulation will take place for both equations simultaneously.")
                                                  ),
                                                  dataTableOutput(ns("models_uday_ntr")),
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
                                                                       dataTableOutput(ns("simulation_input_uday_ntr")),
                                                                       actionButton(ns("commit_uday_ntr"),"Go to simulation",icon("paper-plane"), 
                                                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                       br(),
                                                                       
                                                                       
                                                  ),
                                                  tabPanel("External Data Import",
                                                           tags$ul(
                                                             tags$li(" This segment provides simulation data import functionality."),
                                                             tags$li(" A time series excel file dataset is expected with the naming format of TurningPoint equation.")
                                                           ),
                                                           fileInput(ns("datacall_uday_ntr"),"Import Simulation Data"),
                                                           dataTableOutput(ns("simulationdata_uday_ntr")),
                                                           actionButton(ns("commit2_uday_ntr"), "Go to Visualization",icon("paper-plane"), 
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
                               selectInput(ns("x_axis_ntr"), 'X axis', choices = c(" "," ") ),
                               selectInput(ns("y_axis_ntr"), 'Y axis', choices = c(" "," ")
                                           ,selected = names(mtcars)[2], multiple = TRUE),
                               plotOutput(ns("scatterplot_uday_ntr")),
                               br(),
                               br(),
                               tags$ul(
                                 tags$li(" This scatter graph allows for double Y axis selection on single x axis."),
                                 tags$li(" It takes the first two selections of y-axis for plotting . Linear regression method is used for creating trendlines."),
                                 tags$li("Trend Line checkbox can be adding or removing the trend line in graph.")
                               ),
                               checkboxInput(ns("smooth_uday_ntr"),"Trend Line", TRUE),
                               plotOutput(ns("ggscatter_uday_ntr")),
                               checkboxInput(ns("smooth2_uday_ntr"),"Multitrend", TRUE),
                               plotOutput(ns("ggsmooth_uday_ntr"))
                             ),
                             wellPanel(
                               h2("Histogram"),
                               selectInput(ns("hist_choice_uday_ntr"), "Histogram Variable", c(" "," ")),
                               plotOutput(ns("hist_uday_ntr"))
                             )
                           ),
                  ),
                  tabPanel("Simulation",
                           h3("Simulation Input Response"),
                           tabsetPanel(id = "simuPage_uday_ntr",
                                       tabPanel("Manual Entry Simulation",
                                                h3("Input values for manual Simulation"),
                                                dataTableOutput(ns("modeltable_uday_ntr")),
                                                actionButton(ns("simulate_uday_ntr"),"Simulate on entered Data", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                uiOutput(ns("simulation_result_uday_ntr")),
                                                dataTableOutput(ns("result1_uday_ntr")),
                                                downloadButton(ns("download1_uday_ntr"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                
                                                ),
                                       tabPanel("Profiler",
                                                wellPanel(
                                                  h1("Profiler"),
                                                  fluidRow(column(width = 10,plotOutput(ns("plot2"))),
                                                  ),
                                                  fluidRow(column(width = 6,
                                                                  sliderInput(ns("Base_Factor"),"Base Factor:", min = 1, max = 100, value = 5),
                                                                  sliderInput(ns("Filler_Sulphate_Salt_as_Balancing_ingredient"),"Filler Sulphate Salt as Balancing ingredient:", min = 1, max = 100, value = 5),
                                                                  sliderInput(ns("Base_Powder_Bulk_Density"),"Base Powder Bulk Density:", min = 1, max = 100, value = 5)
                                                  ),
                                                  column(width = 6,
                                                         sliderInput(ns("Post_Dosing_Ingredients_Majors"),"Post Dosing Ingredients Majors:", min = 1, max = 100, value = 5),
                                                         sliderInput(ns("Post_Dosing_Ingredients_Minors"),"Post Dosing Ingredients Minors:", min = 1, max = 100, value = 5)
                                                  ))
                                                )),
                                       tabPanel("Imported Data Simulation",
                                                uiOutput(ns("simulation_result_uday_ntr2")),
                                                column(4, dataTableOutput(ns("modeltable2_uday_ntr"))),
                                                br(),
                                                br(),
                                                fluidRow(column(2, actionButton(ns("simulate2_uday_ntr"),"Simulate on Imported Data",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                                downloadButton(ns("download2_uday_ntr"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                        
                                       ))
                           
                  ),
      tabPanel("Optimisation",
               wellPanel(  h2("Process optimiser (Non Linear Optimisation)"),
                 tags$ul(
                   h4("Target Variable :	BD_Prediction_by_Model  ")
                   ,br(),
                   tags$li("The optimisation page can be used to derive the values of the model predictors that are predicted based on a specified Target variable."),
                   tags$li("The solution can be further constrained by minimizing or maximizing an objective function."),
                   tags$li("Objective function is defined as a linear combination of model predictors."),
                   tags$li("Select minimization or maximization as per need by clicking on the checkbox. "),
                   tags$li("Select the desired inequality from \"less than or equal to\", \"equal to\" and \"greater than or equal to\". "),
                   tags$li("Input the desired value of the Target variable. ")
                   
                 ),br()),
               
               wellPanel(
                 radioButtons(ns("radio_button_uday_ntr"),"Objective Type",choices=c("Minimization"="min","Maximization"="max"),inline = TRUE),
                 br(),
                 fluidRow(
                   column(2,selectInput(ns("inequality_selection_uday_ntr"),"Select the inequality type ",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                   column(2,offset= 2,numericInput(ns("numeric_input_uday_ntr"),"Enter the Target variable value",900)),
                   br()),
                 
                 h2("Objective Function Table"), br(),
                 tags$li("Enter the allowed range for each model predictor by editing the 'Lower Bounds' and 'Upper Bounds' columns in the below table."),
                 tags$li("The objective function is defined as a linear combination of the predictors whose coefficients are given in the 'obj coeff' column. "),
                 tags$li("The values in this column are defaulted to one and they can be edited as per the requirements. "),
                 tags$li("Press the 'Run optimiser' button to generate the optimal solution."),
                 br(),
                 dataTableOutput(ns("optimiser_table1_uday_ntr"))),
               
               fluidRow(column(2,actionButton(ns("run_optimiser_uday_ntr"),"Run Optimiser",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                        column(2,actionButton(ns("reset_uday_ntr"),"Reset to defaults",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")) ),
               
               wellPanel(
                 h3("Results"),br(),
                 tags$li("The Target variable value associated with the predictor values, is shown in this table") ,br(),
                 dataTableOutput(ns("optimiser_table22_uday_ntr")),
                 h3("Predictors"),br(),
                 tags$li("The \"Predictors\" table show the optimised value taken by each predictor to obtain the optimised Target variable value. "),br(),
                 dataTableOutput(ns("optimiser_table32_uday_ntr")),
                 h4("Objective Function Value"),
                 uiOutput(ns("value_results_uday_ntr")),
                 downloadButton(ns("download5_uday_ntr"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                 
               ),
               wellPanel(
                 h2("Global Download"),
                 h4("Download all the results that have been generated throughout the app"),
                 actionButton(ns("downloadresults_uday_ntr"),"Proceed to download all Results",
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 uiOutput(ns("Download_Values_uday_ntr"))
               )
               
               
      )#optimisation end
      )
    }
    
    
  )
}