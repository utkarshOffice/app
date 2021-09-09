Facial_MoisturizerUI <- function(id){
  ns <- NS(id)
  
  tagList(
    
    {
      tabsetPanel(id = "tabs_skincare_kayla",tabPanel("Model Introduction",
        h1("Skin Care Facial Moisturizer L63 Model"),
        em("Model Owner: Kayla Liu"),
        
        h4("L63 Chassis"),
        p("L63 chassis is one of the core chassis in Skin Care, and the largest volume facial moisturizer. The process of making L63 is challenging as it is not a traditional oil in water emulsification reaction. There are 4 main phases in making L63, heating, first stage emulsification, second stage emulsification and cooling. During the emulsification phase, homogenization speed and time length are controlled to ensure a good emulsification and well mixing of powders. During the cooling phase, cooling rate and shear rate are controlled to ensure the formation of lamellar structure and a smooth appearance. "),
        
        h4("Model Design"),
        p("A DoE model was designed to study the effect of key processing parameters (Table 1) on L63 chassis viscosity, specifically, viscosity 2hrs and 24hrs results measured by Brookfield. 6 key processing parameters, initial emulsification temperature, homogenization speed, first homogenization time length, second homogenization time length, cooling rate and discharge temperature were chosen based on a previous screening study. 

JMP was used to generate the DoE plan. DMP data, including material, sensor and quality data were captured during the trial running. PAE was used for DoE model development and exploration. PAE multi-sampling feature was used to split a single trial into multiple sub trials based on the sample taken. There was a total of 12 pilot plant trials in this study (Table 2). "),
        
        h4("Model Development"), 
        p("The DMP sensor data was first onboarded on DataLab platform, then segmented into 7 phases: heating, homogenization I, homogenization II, homogenization III, cooling I, cooling II and discharge. Segmentation of the sensor data was mainly based on the signals of main mixer temperature, main recycle flow meter, in-line homogenization speed and main recycle temperature. 

After segmentation, viscosity 2hrs and viscosity 24hrs were selected as responses, key processing parameters at corresponding phases: 2 Homo I PT100 (oC) Mean, 3 Homo II Silverson (Hz) Mean, 2 Homo I Length, 3 Homo II Length, 5 Cooling I Length, 6 Cooling II Length, 7 Discharge IFM temp (oC) Mean and 7 Discharge Length were selected as factors to build the model. "), 
        
        h4("Model Exploration"), 
        p("Viscosity 2hrs and viscosity 24hrs results measured by Brookfield were modelled independently (Figure1, Figure 3). Linear regression was used and no transformation of data was made.  As we observed in the sensitivity analysis (Figure 2, Figure 4), homogenization speed is a critical variable in the determination of both viscosity 2hrs and viscosity 24hrs. An increase in the homogenization speed is predicted to yield an increase in product viscosity. Specific to viscosity 2hrs, discharge temperature, initial emulsification temperature and second homogenization time length are also significant factors, while the viscosity 24hrs are affected by the changes in discharge temperature, first homogenization time length and first cooling time length. It’s worth noting that an increase in discharge temperature is predicted to yield an increase in viscosity 2hrs, while a decrease in viscosity 24hrs. "),
        
        h4("A final cautionary word"),
        p("This study was designed to understand the effect of key processing parameters on L63 chassis viscosity, the data collected were from pilot plant trials. Further experimentation and validation are strongly recommended to have a sufficient level of confidence when using this model for prediction. "),
        
        img(src= "kayla_new_tables.png"),
        
        h4("Figure 1. Viscosity 2hrs Model Fit"), 
        column(width = 12, img(src= "kayla_new_image1.png",  height="100%", width="80%", align="left")),

        h4("Figure 2. Viscosity 2hrs Sensitivity"),
        column(width = 12, img(src= "kayla_new_image2.png",  height="100%", width="80%", align="left")),

        h4("Figure 3. Viscosity 24hrs Model Fit"),
        column(width = 12, img(src= "kayla_new_image3.png",  height="100%", width="80%", align="left")),

        h4("Figure 4. Viscosity 24hrs Sensitivity"),
        column(width = 12, img(src= "kayla_new_image4.png",  height="100%", width="80%", align="left")),

      ),
                  tabPanel("Model & Data Import",
                                                      wellPanel(
                                                        #h1("Skin Care Facial Moisturizer L63 Model"),
                                                        #em("Model Owner: Kayla Liu"),
                                                        br(),
                                                        h2("Available Model Outputs"),
                                                        tags$ul(
                                                          tags$li(" This table presents the available model for this segment.")
                                                          ,tags$li(" Simulation will take place for both equations simultaneously.")
                                                        ),
                                                        dataTableOutput(ns("models_skincare_kayla")),
                                                        h3("Advisory Table"),
                                                        tags$ul(
                                                          tags$li(" This table presents the advisable Lower and Upper Limits for all Predictors"),
                                                        ),
                                                        dataTableOutput(ns("advice_skincare_kayla"))
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
                                                                             dataTableOutput(ns("simulation_input_skincare_kayla")),
                                                                             actionButton(ns("commit_skincare_kayla"),"Go to simulation",icon("paper-plane"), 
                                                                                          style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                             br(),
                                                                             
                                                                             
                                                        ),
                                                        tabPanel("External Data Import",
                                                                 tags$ul(
                                                                   tags$li(" This segment provides simulation data import functionality."),
                                                                   tags$li(" A time series excel file dataset is expected with the naming format of TurningPoint equation.")
                                                                 ),
                                                                 fileInput(ns("datacall_skincare_kayla"),"Import Simulation Data"),
                                                                 dataTableOutput(ns("simulationdata_skincare_kayla")),
                                                                 actionButton(ns("commit2_skincare_kayla"), "Go to Visualization",icon("paper-plane"), 
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
                               selectInput(ns("x_axis_skincare_kayla"), 'X axis', choices = c(" "," ") ),
                               selectInput(ns("y_axis_skincare_kayla"), 'Y axis', choices = c(" "," ")
                                           ,selected = names(mtcars)[2], multiple = TRUE),
                               plotOutput(ns("scatterplot_skincare_kayla")),
                               br(),
                               br(),
                               tags$ul(
                                 tags$li(" This scatter graph allows for double Y axis selection on single x axis."),
                                 tags$li(" It takes the first two selections of y-axis for plotting . Linear regression method is used for creating trendlines."),
                                 tags$li("Trend Line checkbox can be adding or removing the trend line in graph.")
                               ),
                               checkboxInput(ns("smooth_skincare_kayla"),"Trend Line", TRUE),
                               plotOutput(ns("ggscatter_skincare_kayla")),
                               checkboxInput(ns("smooth2_skincare_kayla"),"Multitrend", TRUE),
                               plotOutput(ns("ggsmooth_skincare_kayla"))
                             ),
                             wellPanel(
                               h2("Histogram"),
                               selectInput(ns("hist_choice_skincare_kayla"), "Histogram Variable", c(" "," ")),
                               plotOutput(ns("hist_skincare_kayla"))
                             )
                           ),
                  ),
                  tabPanel("Simulation",
                           h3("Simulation Input Response"),
                           tabsetPanel(id = "simuPage_skincare_kayla",
                                       tabPanel("Manual Entry Simulation",
                                                h3("Input values for manual Simulation"),
                                                dataTableOutput(ns("modeltable_skincare_kayla")),
                                                actionButton(ns("simulate_skincare_kayla"),"Simulate on entered Data",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                uiOutput(ns("kayla_simulation")),
                                                dataTableOutput(ns("result1_skincare_kayla")),
                                                downloadButton(ns("download1_skincare_kayla"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                       ),
                                       tabPanel("Profiler",
                                                wellPanel(
                                                  h1("Profiler"),
                                                  br(),
                                                  fluidRow(column(width = 6,
                                                                  plotOutput(ns("plot1"))
                                                  ),column(width = 6,
                                                           plotOutput(ns("plot2"))
                                                  )
                                                  ),

                                                  fluidRow(column(width = 6,
                                                                  sliderInput(ns("Initial_Emulsification_Temperature"),"Initial Emulsification Temperature:", min = 68, max = 72, value = 68),
                                                                  sliderInput(ns("Homogenization_Speed"),"Homogenization Speed:", min = 33, max = 50, value = 33),
                                                                  sliderInput(ns("First_Homogenization_Time_Length"),"First Homogenization Time Length:", min = 10, max = 20, value = 11),
                                                                  sliderInput(ns("Second_Homogenization_Time_Length"),"Second Homogenization Time Length:", min = 3, max = 13, value = 4)
                                                  ),
                                                  column(width = 6,
                                                         sliderInput(ns("First_Cooling_Time_Length"),"First Cooling Time Length:", min = 14, max = 43, value = 14),
                                                         sliderInput(ns("Second_Cooling_Time_Length"),"Second Cooling Time Length:", min = 10, max = 52, value = 10),
                                                         sliderInput(ns("Discharge_Temperature"),"Discharge Temperature:", min = 36, max = 42, value = 36),
                                                         sliderInput(ns("Discharge_Time_Length"),"Discharge Time Length:", min = 3, max = 26, value = 3)
                                                  ))
                                                )
                                       ),
                                       tabPanel("Imported Data Simulation",
                                                uiOutput(ns("kayla_simulation2")),
                                                column(4, dataTableOutput(ns("modeltable2_skincare_kayla"))),
                                                br(),
                                                br(),
                                                fluidRow(column(2, actionButton(ns("simulate2_skincare_kayla"),"Simulate on Imported Data", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                                downloadButton(ns("download2_skincare_kayla"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                                
                                                   ))
                           
                  ),
      tabPanel("Optimisation",
               # fluidRow(
                 wellPanel(
                   tags$ul(
                     h2("Process Optimiser (Linear Optimisation) "),
                     br(), h4("Target Variables : Viscosity_24hrs and Viscosity_2hrs "),
                     br(),
                     tags$li("The optimisation page can be used to derive the values of the model predictors that are predicted based on a specific value of the Target variables."),
                     tags$li("The solution can be further constrained by minimizing or maximizing an objective function."),
                     tags$li("Objective function is defined as a linear combination of model predictors."),
                     tags$li("Select minimization or maximization as per need by clicking on the checkbox. "),
                     tags$li("Select the desired inequality from \"less than or equal to\", \"equal to\" and \"greater than or equal to\". "),
                     tags$li("Input the desired values of the Target variables. ")
                   ),
                   
                   br()),
                 wellPanel(
                   radioButtons(ns("radio_button_skincare_kayla"),"Objective Type",choices=c("Minimization"="min","Maximization"="max"),inline = TRUE),
                   br(),
                   fluidRow(
                   column(2,selectInput(ns("inequality_selection_skincare_kayla_24"),"Select the inequality type for Viscosity_24hrs",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                   br(),
                   column(2,numericInput(ns("numeric_input_skincare_kayla_24"),"Enter the target Viscosity_24hrs value",56000)),
                   # fluidRow(
                   column(2,selectInput(ns("inequality_selection_skincare_kayla_2"),"Select the inequality type for Viscosity_2hrs",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                   br(),
                    column(2,numericInput(ns("numeric_input_skincare_kayla_2"),"Enter the target Viscosity_2hrs value",56000))),
                     h2("Objective Function Table"), br(),
                     tags$li("Enter the allowed range for each model predictor by editing the 'Lower Bounds' and 'Upper Bounds' columns in the below table."),
                     tags$li("The objective function is defined as a linear combination of the predictors whose coefficients are given in the 'obj coeff' column. "),
                     tags$li("The values in this column are defaulted to one and they can be edited as per the requirements. "),
                     tags$li("Press the 'Run optimiser' button to generate the optimal solution."),
                   br(),
                   dataTableOutput(ns("optimiser_table1_skincare_kayla"))),
                 
                 fluidRow(column(2,actionButton(ns("run_optimiser_skincare_kayla"),"Run Optimiser",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                          column(2,actionButton(ns("reset_skincare_kayla"),"Reset to defaults", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")) ),
                 wellPanel(
                   h3("Results"), br(),
                   tags$li("The \"Results\" table shows the value of Viscosity fresh Sample 4 Cps (mPas) obtained.") ,br(),
                   dataTableOutput(ns("optimiser_table2_skincare_kayla")),
                   h3("Predictors"),br(),
                   tags$li("The \"Predictors table\" shows the optimised value taken by each predictor to obtain the above Viscosity fresh Sample 4 Cps (mPas) value. "),br(),
                   dataTableOutput(ns("optimiser_table3_skincare_kayla")),
                   h4("Objective Function Value"),
                   uiOutput(ns("value_results_skincare_kayla")),
                   downloadButton(ns("download3_skincare_kayla"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                   
                 ),#taglist end,
                 wellPanel(
                   h2("Global Download"),
                   h4("Download all the results that have been generated throughout the app"),
                   actionButton(ns("downloadresults_skincare_kayla"),"Proceed to download all Results",
                                style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                   uiOutput(ns("Download_Values_skincare_kayla"))
                 )
                 
               # ) #fluidrow end
      )#tabpanel optimisation end
      
      )}
    
  )}