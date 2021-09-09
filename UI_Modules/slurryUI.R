slurryUI <- function(id){
  ns <- NS(id)
  
  tagList(
    {tabsetPanel(id = "tabs",tabPanel("Model Introduction",
                                      h1("Home Care Slurry Viscosity Drying Model"),
                                      em("Model Owners: Amanda lane, Aditi Mahajan"),
                                      h4("This model uses the slurry composition to predict slurry viscocity (Torque) and the ease of drying and provide recommendations for optimal slurry composition."),
                                      br(),
                                      p("The viscosity and dry-ability of slurries for spray dried laundry powders have been found to be closely related to the formulation composition.
                                      As a result, the Home Care Slurry Viscosity Drying Model has been developed, through a design of experiments, to predict slurry viscosity and slurry dry-ability as a function of slurry composition.  
                                      "),
                                      strong("Note - All ingredients (except target slurry moisture content i.e. Target SMC) should be reported as fractions on a dry basis (w/w). Sum of all dry ingredient should be = 1 
                                        Target SMC should be reported as a fraction on the wet slurry basis (w/w) "),
                                      h3("Modelling approach and accuracy"),
                                      p("The dry-ability of a slurry was found to directly relate to the transition point to
                                        falling rate period (point 2) on the drying curve for slurry and a model was constructed 
                                        to classify slurries on a scale of easy (green) to hard/impossible to dry (red) using the turning point prediction."),
                                      fluidRow(column(width = 6,img(src= "DryingRatePic.png", height="100%", width="100%", align="left")),
                                               column(width = 6,img(src =  "torquePic2.png", align = "left"))),
                                      strong("The dry-ability (turning point) model has an R2 of 0.96."),
                                      
                                      p("The model uses the turning point prediction to then indicate the closest friend
                                        formulation (from the pool of known formulations), in terms of dry-ability.
                                        The dry-ability of known formulations can be arranged on a scale of easy to hard
                                        and are shown below."),
                                      img(src= "torquePic1.png"),
                                      p("A second model, built to predict torque, gives an indication of the slurry viscosity."),
                                      strong("A slurry can be sprayed through nozzles only if the Torque is between 19 and 36."),
                                      strong("The Torque model has an R2 of 0.94"),
                                      column(width = 12, img(src= "torquePic3.png",  height="50%", width="50%", align="left")),
                                      p("The two models allow the user to find the optimal composition for a given slurry in terms of  viscosity and dry-ability
                                        and view the impact of varying ingredient levels using the profiler feature.")
                                      ),
                 tabPanel("Model & Data Import",
                                      wellPanel(
                                        h2("Available Model Outputs"),
                                        tags$ul(
                                          tags$li(" This table presents the available model for this segment.")
                                          ,tags$li(" Simulation will take place for both equations simultaneously.")
                                          ,tags$li(" Variables used in Torque, except TargetSMC, are derived variables e.g. NaLASnew = (NaLAS (dry basis)/Sum)*(1-TargetSMC)", style = "color:green")
                                          ,tags$li(" Sum = NaLAS (dry basis) + AlkSilicate (dry basis) + LSA (dry basis) + CP5 (dry basis) + SCMC (dry basis) + Sulphate (dry basis)", style = "color:green")
                                        ),
                                        dataTableOutput(ns("models")),
                                        h3("Advisory Table"),
                                        tags$ul(
                                          tags$li(" This table presents the advisable Lower and Upper Limits for all Predictors"),
                                        ),
                                        dataTableOutput(ns("advice"))
                                      ),
                                      wellPanel(
                                        h2("Simulation Data Inputs"),
                                        p("Only one of manual data entry and external import is allowed. Select one and proceed. In manual entry user needs to input variable values 
                                          within the prescribed limits. And external import will allow user to bring in outside files for simulation."),
                                        tabsetPanel(tabPanel("Manual Entry",
                                                             tags$ul(
                                                               tags$li(" Following table is editable. Enter the simulation values."),
                                                               tags$li(" Entered values will automatically be saved for simulation stage."),
                                                               tags$li(" Target Slurry Moisture Content denotes the fraction of water in the slurry."),
                                                               tags$li(" All other ingredient content must be reported on dry basis."),
                                                               tags$li(" Measurement Units of all variables are in fractions i.e. 0-1. User must follow these limits.", style = "color:blue")
                                                             ),
                                                             dataTableOutput(ns("simulation_input")),
                                                             uiOutput(ns("valWarning")),
                                                             br(),
                                                             actionButton(ns("commit"),"Go to simulation",icon("paper-plane"), 
                                                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), 
                                                            ),
                                                    tabPanel("External Data Import",
                                                             tags$ul(
                                                               tags$li(" This segment provides simulation data import functionality."),
                                                               tags$li(" A time series excel file dataset is expected with the naming format of equations.")
                                                             ),
                                                             fileInput(ns("datacall"),"Import Simulation Data"),
                                                             dataTableOutput(ns("simulationdata")),
                                                             actionButton(ns("commit2"), "Go to Visualization",icon("paper-plane"), 
                                                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                        ),
                                      ),
                                      wellPanel(
                                        br(),
                                        h3("Closest Friend classfication logic for turning point"),
                                        dataTableOutput(ns("knntable")),
                                        h3("Dryability classification logic"),
                                        dataTableOutput(ns("classificationtable"))
                                      )),
                 tabPanel("Visualization",
                          fluidRow(
                            wellPanel(
                              h4("Visual guide for imported data."),
                              tags$ul(
                                tags$li(" This tab presents visualizations of the externally imported data."),
                                tags$li(" First two graphs allow overlapping of two variables on Y axis."),
                                tags$li(" While the first graph presents linear trend through line graph, the second graph presents linear regression based trend in data."),
                                #tags$li(" Third graph shows scatter plot as well as trend lines. Upto 4 variables are allowed in this graph on Y axis."),
                                tags$li(" Finally a histogram is presented.")
                              ),
                            ),
                            wellPanel(
                              h2("Scatter Plot"),
                              selectInput(ns("x_axis"), 'X axis', choices = c(" "," ") ),
                              selectInput(ns("y_axis"), 'Y axis', choices = c(" "," ")
                                          ,selected = c(" "," ")[2], multiple = TRUE),
                              br(),
                              br(),
                              tags$ul(
                                tags$li(" First two graphs allow for double Y axis selection with single x axis."),
                                tags$li(" It takes the first two selections of y-axis for plotting. Linear regression method is used for creating trendlines in second graph."),
                                tags$li(" Checkboxes can be used for adding or removing the trends in graph."),
                                #tags$li(" Third graph allows for 4 variable selection. Backspace button can be used for remvoing selected choices.")
                              ),
                              checkboxInput(ns("smooth"),"Trend Line", TRUE),
                              plotOutput(ns("ggscatter")),
                              checkboxInput(ns("smooth2"),"Multitrend", TRUE),
                              plotOutput(ns("ggsmooth")),
                              plotlyOutput(ns("multi_lines_graph"))
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
                                               h3("Input values taken for manual simulation"),
                                               dataTableOutput(ns("modeltable")),
                                               actionButton(ns("simulate"),"Simulate on entered Data", 
                                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                               br(),
                                               uiOutput(ns("simulation_heading")),
                                               dataTableOutput(ns("newvals")),
                                               uiOutput(ns("Predicted_Values")),
                                               dataTableOutput(ns("result1")),
                                               uiOutput(ns("torque_message")),
                                               fluidRow(column(width = 10,dataTableOutput(ns("resultcolor")))
                                                        #,column(width = 4,img(src= "torquePic1.png"))
                                                        ),
                                               br(),
                                               br(),
                                               downloadButton(ns("download1"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                               
                                      ),
                                      tabPanel("Profiler",
                                               wellPanel(
                                                 h1("Profiler"),
                                                 br(),
                                                 fluidRow(column(width = 6,
                                                                 h2("Dryability Classification of Slurry"),
                                                                 dataTableOutput(ns("profilertable")),
                                                 ),
                                                 column(width = 6, 
                                                        h2("Torque"),
                                                        plotOutput(ns("slider_torque")),
                                                        uiOutput(ns("profiler_turningpoint")),
                                                        uiOutput(ns("profiler_torque"))
                                                 )),
                                                 fluidRow(column(width = 6,
                                                                 sliderInput(ns("profiler_targetsmc"),"TargetSMC:", min = 0.287, max = 0.37, value = 0.31),
                                                                 sliderInput(ns("profiler_lsa"),"LSA (dry basis):", min = 0.17, max = 0.45, value = 0.19),
                                                                 sliderInput(ns("profiler_cp5"),"CP5 (dry basis):", min = 0.00, max = 0.03, value = 0.02),
                                                                 sliderInput(ns("profiler_sulphate"),"Sulphate (dry basis):", min = 0.17, max = 0.52, value = 0.28)
                                                                 
                                                 ),
                                                 column(width = 6,
                                                        sliderInput(ns("profiler_nalas"),"NaLAS (dry basis):", min = 0.13, max = 0.41, value = 0.21),
                                                        sliderInput(ns("profiler_alksilicate"),"Alkaline Silicate (dry basis):", min = 0.07, max = 0.16, value = 0.11),
                                                        sliderInput(ns("profiler_scmc"),"SCMC (dry basis):", min = 0.00, max = 0.01, value = 0.00)
                                                        
                                                 ))
                                               )
                                      ),
                                      tabPanel("Imported Data Simulation",
                                               p("Table only presents the results with condition of predicted Torque being in the range of 19 to 36."),
                                               br(),
                                               actionButton(ns("simulate2"),"Simulate on Imported Data", 
                                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                               uiOutput(ns("Predicted_Values2")),
                                               dataTableOutput(ns("modeltable2")),
                                               plotOutput(ns("numvals")),
                                               downloadButton(ns("download2"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                      )
                                     
                                   )
                 ),
                 tabPanel("Optimisation", 
                          tabsetPanel(tabPanel("Linear Optimisation",
                                               # fluidRow(
                                                 wellPanel(
                                                   tags$ul(
                                                     h2("Process Optimiser (Linear Optimisation) "),br(),
                                                     h4("Target Variable : Turning Point"),
                                                     br(),
                                                     h5("-Non Linear optimisation ought to be used for the equation based on Torque"),
                                                     tags$li("The optimisation page can be used to derive the values of the model predictors that are predicted based on a specified Turning Point value."),
                                                     tags$li("The solution can be further constrained by minimizing or maximizing an objective function."),
                                                     tags$li("Objective function is defined as a linear combination of model predictors."),
                                                     tags$li("Select minimization or maximization as per need by clicking on the checkbox. "),
                                                     tags$li("Select the desired inequality from \"less than or equal to\", \"equal to\" and \"greater than or equal to\". "),
                                                     tags$li("Input the desired value of Turning Point, by default it is set to 0.32. ")
                                                   ),
                                                   
                                                   br()),
                                                 wellPanel(
                                                   radioButtons(ns("radio_button"),"Objective Type",choices=c("Minimization"="min","Maximization"="max"),inline = TRUE),
                                                   br(),
                                                   fluidRow(
                                                   column(2,selectInput(ns("inequality_selection"),"Select the inequality type",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                                                   # h3("Target Turning Point"),
                                                   column(2,offset= 3,numericInput(ns("numeric_input"),"Enter the target Turning Point value",0.32))),br(),
                                                   
                                                   # wellPanel(
                                                     h2("Objective Function Table"), br(),
                                                     tags$li("Enter the allowed range for each model predictor by editing the 'Lower Bounds' and 'Upper Bounds' columns in the below table."),
                                                     tags$li("The objective function is defined as a linear combination of the predictors whose coefficients are given in the 'obj coeff' column. "),
                                                     tags$li("The values in this column are defaulted to one and they can be edited as per the requirements. "),
                                                     tags$li("Press the 'Run optimiser' button to generate the optimal solution."),
                                                   br(),
                                                   dataTableOutput(ns("optimiser_table1"))),
                                                 fluidRow(column(2,actionButton(ns("run_optimiser"),"Run Optimiser",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                                          column(2,actionButton(ns("reset"),"Reset to defaults",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")) ),
                                                 wellPanel( 
                                                   h3("Results"),br(),
                                                   # h3("Quality Measure(s) "),br(),
                                                   tags$li("The \"Results\" table shows the value of Turning Point obtained.") ,br(),
                                                   dataTableOutput(ns("optimiser_table2")),
                                                   h3("Predictors"),br(),
                                                   tags$li("The \"Predictors table\" shows the optimised value taken by each predictor to obtain the above Turning Point value. "),br(),
                                                   dataTableOutput(ns("optimiser_table3")),
                                                   h4("Objective Function Value"),
                                                   uiOutput(ns("value_results")),
                                                   downloadButton(ns("download3"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                   
                                                   
                                                 ), #optimisation part 1 end
                                                 
                                                 # fluidRow(
                                                   wellPanel(
                                                     tags$ul(
                                                       h2("Linear Optimisation of Turning Point based on Categories"),
                                                       br(),
                                                       # tags$li("The optimization page can be used to derive the values of the model predictors that are predicted based on a specified Turning Point value."),
                                                       tags$li("Here, linear optimisation of Turning Point based on a desired Dryability category is done."),
                                                       # tags$li("Select the desired Dryability category from \"Green \",\" Amber\",\" Red\",\" Impossible\" "),
                                                       # tags$li("")
                                                       
                                                     ),br(),
                                                     wellPanel(fluidRow( 
                                                       radioButtons(ns("radio_button2"),"Objective Type",choices=c("Minimization"="min","Maximization"="max"),inline = TRUE),
                                                       
                                                       fluidRow(column(2,selectInput(ns("category_selection"),"Select the Target Category of Dryability",choices = c("Green","Amber","Red","Impossible"),selected = "Green"))),
                                                     )),
                                                     br(),
                                                     wellPanel(
                                                       h2("Objective Function Table"), br(),
                                                       tags$li("Enter the allowed range for each model predictor by editing the 'Lower Bounds' and 'Upper Bounds' columns in the below table."),
                                                       tags$li("The objective function is defined as a linear combination of the predictors whose coefficients are given in the 'obj coeff' column. "),
                                                       tags$li("The values in this column are defaulted to one and they can be edited as per the requirements. "),
                                                       tags$li("Once the changes are made click on \"Run Optimiser\" to generate the optimal solution."),br(),
                                                       dataTableOutput(ns("optimiser_table12"))),
                                                     fluidRow(column(2,actionButton(ns("run_optimiser2"),"Run Optimiser",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                                              column(2,actionButton(ns("reset2"),"Reset to defaults",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")) ) 
                                                   ),
                                                   wellPanel(
                                                     h3("Results"),br(),
                                                     # h3("Quality Measure(s) "),br(),
                                                     tags$li("The Turning Point value associated with the predictor values, is shown in this table") ,br(),
                                                     dataTableOutput(ns("optimiser_table22")),
                                                     h3("Predictors"),br(),
                                                     tags$li("The \"Predictors\" table show the optimised value taken by each predictor to obtain the above Turning Point value. "),br(),
                                                     dataTableOutput(ns("optimiser_table32")),
                                                     h4("Objective Function Value"),
                                                     uiOutput(ns("value_results2")),
                                                     downloadButton(ns("download4"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                     
                                                   ) #wellpanel end
                                                 # )#fluidrow end
                                               # ) #wellpanel end
                          ),
                          tabPanel("Non Linear Optimisation" ,
                                   # fluidRow(
                                     wellPanel(
                                       tags$ul(
                                         h2("Process Optimiser (Non Linear Optimisation)"),br(),
                                         h4("Target Variable : Torque")
                                         ,br(),
                                         tags$li("The optimisation page can be used to derive the values of the model predictors that are predicted based on a specified Torque value."),
                                         tags$li("The solution can be further constrained by minimizing or maximizing an objective function."),
                                         tags$li("Objective function is defined as a linear combination of model predictors."),
                                         tags$li("Select minimization or maximization as per need by clicking on the checkbox. "),
                                         tags$li("Select the desired inequality from \"less than or equal to\", \"equal to\" and \"greater than or equal to\". "),
                                         tags$li("Input the desired value of Torque, by default it is set to 28. ")
                                         
                                       ),br()),
                                     
                                     wellPanel(fluidRow(
                                       radioButtons(ns("radio_button_adititorque"),"Objective Type",choices=c("Minimization"="min","Maximization"="max"),inline = TRUE),
                                       br(),
                                       column(2,selectInput(ns("inequality_selection_adititorque"),"Select the inequality type",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                                       # h3("Target Turning Point"),
                                       column(2,offset= 3,numericInput(ns("numeric_input_adititorque"),"Enter the target Torque value",28))),
                                       
                                       # wellPanel(
                                         h2("Objective Function Table"), br(),
                                         tags$li("Enter the allowed range for each model predictor by editing the 'Lower Bounds' and 'Upper Bounds' columns in the below table."),
                                         tags$li("The objective function is defined as a linear combination of the predictors whose coefficients are given in the 'obj coeff' column. "),
                                         tags$li("The values in this column are defaulted to one and they can be edited as per the requirements. "),
                                         tags$li("Press the 'Run optimiser' button to generate the optimal solution."),
                                       br(),
                                       dataTableOutput(ns("optimiser_table1_adititorque"))),
                                     fluidRow(column(2,actionButton(ns("run_optimiser12"),"Run Optimiser",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                              column(2,actionButton(ns("reset12"),"Reset to defaults",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")) ),
                                     wellPanel(
                                       h3("Results"),br(),
                                       tags$li("The Torque value associated with the predictor values, is shown in this table") ,br(),
                                       dataTableOutput(ns("optimiser_table22_adititorque")),
                                       h3("Predictors"),br(),
                                       tags$li("The \"Predictors\" table show the optimised value taken by each predictor to obtain the above Torque value. "),br(),
                                       dataTableOutput(ns("optimiser_table32_adititorque")),
                                       h4("Objective Function Value"),
                                       uiOutput(ns("value_results_adititorque")),
                                       downloadButton(ns("download5"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                       
                                     ),
                                     wellPanel(
                                       h2("Global Download"),
                                       h4("Download all the results that have been generated throughout the app"),
                                       actionButton(ns("downloadresults"),"Proceed to download all Results",
                                                    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                       uiOutput(ns("Download_Values"))
                                     )
                                     
                                   # )#fluidrow end
                                   
                          )#tabpanel non linear opt end
                          )#tabsetpanel end
                          
                 )#tabpanel end          
    )}
  )}

