ssmUI <- function(id){
  ns <- NS(id)
  
  tagList(
    {
      tabsetPanel(id = "tabs_ashutosh",tabPanel("Model Introduction",
                                               h1("Seal-Strength Models"), 
                                               em("Model Owners: Ashutosh/ Wagner"), 
                                               h4("Data-based sealing strength models"),
                                               h4("Model Introduction"),
                                               p("The data-based model predicts sealing strength of a flexible laminate material based on the sealing parameters – temperature, dwell time and pressure.
It can be used to identify optimal sealing parameters that would deliver the desired seal strength under the applicable line constraints, for e.g. line speed setting etc.
 "),
                                               img(src= "seal_strength_img1.png"),
                                               
                                               h4("Scope of the Model"), 
                                               p("The model can be used for the sealing of 4-side-seal and 3-side-seal pouches. The laminates that are already a part of the model are listed below:  "), 
                                               
                                               img(src= "seal_strength_img2.png"),
                                               
                                               h4("Note to all users:"), 
                                               p("We are constantly interested to increase the model, laminate material and pack format offering of this model. Please contact us with your interests, if they are not covered by the existing offering."),
                                               
                                               h4("Developed By:"), 
                                               img(src= "seal_strength_img3.png"),
                                               
                                               h4("Contact:"),
                                               p("Omer.Bin-Younos@unilever.com"),
      ),
      
      tabPanel("Model & Data Import",
               wellPanel(
                 # h1("Chill Roll Mill Model"),
                 #em("Model Owners:  Aditi/ashutosh"), 
                 h2("Available Model Outputs"),
                 tags$ul(
                   tags$li(" This table presents the available model for this segment.")
                   ,tags$li(" Simulation will take place for all equations simultaneously.")
                 ),
                 dataTableOutput(ns("models_ashutosh")),
                 h3("Accepted range for user inputs"),
                 dataTableOutput(ns("advice_ashutosh"))
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
                                      h3("The table shows variables for Mean Seal Strength Prediction"),
                                      dataTableOutput(ns("simulation_input_ashutosh")),
                                      actionButton(ns("commit_ashutosh"),"Go to simulation",icon("paper-plane"), 
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6d7a4"),
                 ),
                 tabPanel("External Data Import",
                          tags$ul(
                            tags$li(" This segment provides simulation data import functionality."),
                            tags$li(" A time series excel file dataset is expected with the naming format of variables in equation.")
                          ),
                          fileInput(ns("datacall_ashutosh"),"Import Simulation Data"),
                          dataTableOutput(ns("simulationdata_ashutosh")),
                          actionButton(ns("commit2_ashutosh"), "Go to Visualization",icon("paper-plane"), 
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
                   plotOutput(ns("scatterplot_ashutosh")),
                   br(),
                   br(),
                   tags$ul(
                     tags$li(" This scatter graph allows for double Y axis selection on single x axis."),
                     tags$li(" It takes the first two selections of y-axis for plotting . Linear regression method is used for creating trendlines."),
                     tags$li("Trend Line checkbox can be adding or removing the trend line in graph.")
                   ),
                   checkboxInput(ns("smooth_ashutosh"),"Trend Line", TRUE),
                   plotOutput(ns("ggscatter_ashutosh")),
                   checkboxInput(ns("smooth2_ashutosh"),"Multitrend", TRUE),
                   plotOutput(ns("ggsmooth_ashutosh"))
                   #plotlyOutput(ns("multi_lines_graph_ashutosh"))
                   
                 ),
                 wellPanel(
                   h2("Histogram"),
                   selectInput(ns("hist_choice_ashutosh"), "Histogram Variable", c(" "," ")),
                   plotOutput(ns("hist_ashutosh"))
                 )
               ),
      ),
      tabPanel("Simulation",
               h3("Simulation Input Response"),
               tabsetPanel(id = "simuPage_ashutosh",
                           tabPanel("Manual Entry Simulation",
                                    #h3("Informative Plots"),
                                    #img(src= "SDslurry1.png"),
                                    #img(src= "SDslurry2.png"),
                                    h3("Input values taken for manual simulation."),
                                    dataTableOutput(ns("modeltable_ashutosh")),
                                    actionButton(ns("simulate_ashutosh"),"Simulate on entered Data",
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                    uiOutput(ns("simulation_heading_ashutosh")),
                                    dataTableOutput(ns("newvals_ashutosh")),
                                    uiOutput(ns("heading1_ashutosh")),
                                    dataTableOutput(ns("result1_ashutosh")),
                                    downloadButton(ns("download1_ashutosh"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                           ),
                           tabPanel("Profiler",
                                    wellPanel(
                                      h1("Profiler"),
                                      
                                      fluidRow(column(width = 3,plotOutput(ns("plot1"))),
                                               column(width = 3,plotOutput(ns("plot2"))),
                                               column(width = 3,plotOutput(ns("plot3"))),
                                               column(width = 3,plotOutput(ns("plot4"))),
                                      ),
                                      fluidRow(column(width = 3,sliderInput(ns("profiler_Sealing_Pressure"),"Sealing_Pressure:", min = 30, max = 70, value = 50)),
                                               column(width = 3,sliderInput(ns("profiler_Sealing_Time"),"Sealing_Time:", min = 200, max = 700, value = 475)),
                                               column(width = 3,sliderInput(ns("profiler_Sealing_Temperature"),"Sealing_Temperature:", min = 100, max = 140, value = 120)),
                                               column(width = 3,sliderInput(ns("profiler_Layer_Thickness"),"Layer_Thickness:", min = 30, max = 40, value = 35))
                                      ),
                                      selectInput(ns("Profiler_model_select"), 'Select Packaging Model', choices = c(" "," "), width= "30%" ))
                                    ),
                           
                           tabPanel("Imported Data Simulation",
                                    uiOutput(ns("heading_ashutosh")),
                                    dataTableOutput(ns("modeltable2_ashutosh")),
                                    br(),
                                    actionButton(ns("simulate2_ashutosh"),"Simulate on Imported Data",
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                    downloadButton(ns("download2_ashutosh"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                    ,
                                    
                                    
                           ))
               
      ),
      tabPanel("Optimisation",
               fluidRow(
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
                   fluidRow(radioButtons(ns("radio_button_ashutosh"),"Objective Type",choices=c("Minimization"="min","Maximization"="max"),inline = TRUE),
                            br(),
                            column(2,selectInput(ns("inequality_selection_ashutosh"),"Select the inequality type",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                            column(2,offset=3,numericInput(ns("numeric_input_ashutosh"),"Enter the target Drying Prediction Value",0.32))),
                   
                   wellPanel(
                     h2("Objective Function Table"), br(),
                     
                     tags$li("Enter the allowed range for each model predictor by editing the 'Lower Bounds' and 'Upper Bounds' columns in the below table."),
                     tags$li("The objective function is defined as a linear combination of the predictors whose coefficients are given in the 'obj coeff' column. "),
                     tags$li("The values in this column are defaulted to one and they can be edited as per the requirements. "),
                     tags$li("Press the 'Run optimiser' button to generate the optimal solution.")),
                   br(),
                   dataTableOutput(ns("optimiser_table1_ashutosh"))),
                 
                 fluidRow(column(2,actionButton(ns("run_optimiser_ashutosh"),"Run Optimiser",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                          column(2,actionButton(ns("reset_ashutosh"),"Reset to defaults",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")) ),
                 wellPanel(
                   h3("Results"),
                   tags$li("The \"Results\" table shows the value of Drying Prediction obtained.") ,br(),
                   dataTableOutput(ns("optimiser_table2_ashutosh")),
                   h3("Predictors"),br(),
                   tags$li("The \"Predictors table\" shows the optimised value taken by each predictor to obtain the above Drying Prediction value. "),br(),
                   dataTableOutput(ns("optimiser_table3_ashutosh")),
                   h4("Objective Function Value"),
                   uiOutput(ns("value_results_ashutosh")),
                   downloadButton(ns("download3_ashutosh"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), br()
                   
                 ),
                 wellPanel(
                   h2("Global Download"),
                   h4("Download all the results that have been generated throughout the app"),
                   actionButton(ns("downloadresults_ashutosh"),"Proceed to download all Results",
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                   uiOutput(ns("Download_Values_ashutosh"))
                 )
               ) #fluidrow end
      ) #tabpanel optimisation end
      
      )}
    
  )}







