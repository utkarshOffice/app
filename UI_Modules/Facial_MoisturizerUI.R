Facial_MoisturizerUI <- function(id){
  ns <- NS(id)
  
  tagList(
    
    {
      tabsetPanel(id = "tabs_skincare_kayla",tabPanel("Model Introduction",
        h1("Skin Care Facial Moisturizer L63 Model"),
        em("Model Owner: Kayla Liu"),
        h4("L63 Chassis"),
        p("L63 chassis is one of the core chassis in Skin Care, and the largest volume facial moisturizer. The process of making L63 is challenging as it is not a traditional oil in water emulsification reaction. There are 4 main phases in making L63, heating, first stage emulsification, second stage emulsification and cooling. During the emulsification phase, homogenization speed and time length are controlled to ensure a good emulsification and well mixing of powders. During the cooling phase, cooling rate and shear rate are controlled to ensure the formation of lamellar structure and a smooth appearance."),
        h4("Model Design"),
        p("A DoE model was designed to study the effect of key processing parameters (Table 1) on L63 chassis viscosity, specifically, viscosity 2hr result measured by Brookfield. 6 key processing parameters, initial emulsification temperature, homogenization speed, homogenization time length at second stage emulsification, main recycle flow rate at second stage emulsification (note: main recycle flow rate is observation data, we cannot set target or control the levels directly, but it can be considered as a factor when developing the model in PAE), cooling rate and discharge temperature were chosen based on a previous screening study.
JMP was used to generate the DoE plan. DMP data, including material, sensor and quality data were collected during the trial running. PAE was used for DoE model development and post analysis. There was a total of 12 pilot plant trials in this study."),
        h4("Model Development"), 
        p("The DMP sensor data was first onboarded on DataLab platform, then segmented into 7 phases: heating, homogenization I, homogenization II, homogenization III, cooling I, cooling II and discharge. Segmentation of the sensor data was mainly based on the signals of main mixer temperature, main recycle flow meter, in-line homogenizer speed and main recycle temperature.
After segmentation, Viscosity fresh Sample 4 Cps (mPas) (Table 3) was selected as response, key processing parameters at corresponding phases: 2 Homo I PT100 (oC) Mean, 3 Homo II Main recycle pipe flowmeter (kg/h) Mean, 3 Homo II Silverson (Hz) Mean, 3 Homo II Length, 6 Cooling II Length, 7 Discharge PT100 (oC) Mean (Table 4) were selected as factors to build the model.
"), 
        h4("Model Exploration"), 
        p("A linear regression model was developed (Figure 1). The R square is 0.81, which is good considering the variability we might get in the viscosity measurement and the limited batch numbers we have for training dataset. The RMSE of 4,550 describes the prediction error of the model and that is quite small considering we are operating in the 40,000 to 80,000 range.
As we observed in the sensitivity analysis (Figure 2), homogenization speed, homogenization time length at second stage emulsification and discharge temperature have significant impact on viscosity 2hr result measured by Brookfield. An increase in the homogenization speed or discharge temperature is predicted to yield an increase in the viscosity 2hr result, while a decrease in the homogenization time length at second stage emulsification is predicted to yield an increase in viscosity 2hr result."),
        h4("A final cautionary word"),
        p("This study was designed to understand the effect of key processing parameters on L63 chassis viscosity, the data collected were from pilot plant trials. Further experimentation and validation is strongly recommended to have a sufficient level of confidence when using this model for prediction."),
        h4("Table 1. Key Processing Parameters"),
        img(src= "table1kayla.png"),
        h4("Table 2. DoE Design Points"),
        img(src= "table2kayla.png"),
        h4("Table 3. Response in PAE Model"), 
        img(src= "table3kayla.png"),
        h4("Table 4. Factors in PAE Model"),
        img(src= "table4kayla.png"),
        h4("Figure 1. Viscosity 2hr Model Fit Plot"),
        img(src= "fig1kayla.png"),
        h4("Figure 2. Viscosity 2hr Sensitivity"),
        img(src= "fig2kayla.png"),
        
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
                                                        h2("Advisory Table"), 
                                                        dataTableOutput(ns("advice_skincare_kayla")),
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
                                                  fluidRow(column(width = 10,
                                                                  plotOutput(ns("plot1"))
                                                  )),
                                                  fluidRow(column(width = 6,
                                                                  sliderInput(ns("homoiptmean"),"2 Homo I PT100 (oC) Mean :", min = 67, max = 72, value = 68),
                                                                  sliderInput(ns("homoiimainrecyclepipeflowmetermean"),
                                                                              "3 Homo II Main recycle pipe flowmeter (kg/h) Mean:", min = 486, max = 955, value = 500),
                                                                  sliderInput(ns("homoiisilversonmean"),"3 Homo II Silverson (Hz) Mean:", min = 33, max = 50, value = 34)
                                                  ),
                                                  column(width = 6,
                                                         sliderInput(ns("homoiilength"),"3 Homo II Length:", min = 3, max = 13, value = 4),
                                                         sliderInput(ns("coolingiilength"),"6 Cooling II Length:", min = 10, max = 50, value = 11),
                                                         sliderInput(ns("dischargeptmean"),"7 Discharge PT100 (oC) Mean:", min = 40, max = 41, value = 40)
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
               fluidRow(
                 wellPanel(
                   tags$ul(
                     h2("Process Optimiser for Viscosity fresh Sample 4 Cps (mPas) "),
                     br(),
                     tags$li("The optimisation page can be used to derive the values of the model predictors that are predicted based on a specified Viscosity fresh Sample 4 Cps (mPas) value."),
                     tags$li("The solution can be further constrained by minimizing or maximizing an objective function."),
                     tags$li("Objective function is defined as a linear combination of model predictors."),
                     tags$li("Select minimization or maximization as per need by clicking on the checkbox. "),
                     tags$li("Select the desired inequality from \"less than or equal to\", \"equal to\" and \"greater than or equal to\". "),
                     tags$li("Input the desired value of Viscosity fresh Sample 4 Cps (mPas), by default it is set to 56000. ")
                   ),
                   
                   br()),
                 wellPanel(fluidRow(
                   radioButtons(ns("radio_button_skincare_kayla"),"Objective Type",choices=c("Minimization"="min","Maximization"="max"),inline = TRUE),
                   br(),
                   column(2,selectInput(ns("inequality_selection_skincare_kayla"),"Select the inequality type",choices=c("less than or equal to", "equal to", "greater than or equal to"))),
                   # h3("Target Quality Measure "),
                   column(2,offset= 3,numericInput(ns("numeric_input_skincare_kayla"),"Enter the Quality Measure Value",56000))),
                   wellPanel(
                     h2("Objective Function Table"), br(),
                     tags$li("Enter the allowed range for each model predictor by editing the 'Lower Bounds' and 'Upper Bounds' columns in the below table."),
                     tags$li("The objective function is defined as a linear combination of the predictors whose coefficients are given in the 'obj coeff' column. "),
                     tags$li("The values in this column are defaulted to one and they can be edited as per the requirements. "),
                     tags$li("Press the 'Run optimiser' button to generate the optimal solution.")),
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
                 
               ) #fluidrow end
      )#tabpanel optimisation end
      
      )}
    
  )}