introUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      h2("Introduction to the Modeller"), 
      br(),
      em("Contact: satyajit.wattamwar@unilever.com, ashutosh.gandhi@unilever.com"), 
      br(),
      br(),
      p("This Data-based ML modelling application predicts 
     sealing strength of a flexible laminate material based on the sealing parameters
     : temperature, dwell time, pressure & sealent layer thickness.
        The goal of the app is to model the dependence of seal strength on these factors.
        It can be used to build consumable models (in form of equations) on DOE data 
        to predict seal strength of different materials :
        by employing various ML algorithms and comparing them.
                             "),
      column(width = 12, img(src= "seal_strength_img1.JPG", width="50%", align="center")),
      br(),
      h3("Key Features:"), 

      tags$ol(tags$li("User can upload data of 2 segments of materials : polymer or paper."),
              tags$li("Data uploaded is cleaned -> wrangling is done in backend automatically using python scripts.:  outliers, missing imputations."),
              tags$li("Material on which the modelling should be done can be selected."),
              tags$li("User can also provide his own choice of predictors to be used in modelling. : continuous data -> regression."),
              tags$li("Once Build is hit -> Wrangled data is picked up -> Feature Engineering is done (new transformed features are created) -> and modelling is done using different algorithms on this data."),
              tags$li("Different model results are displayed along with their metrics."),
              tags$li("Top 2 models -> Model Interpretation is provided -> Their metrics, Top features, Correlation Plots (EDA vs response) , Predicted vs Real Plots and Residuals. (All downloadable)"),
              tags$li("Results exploration -> how much does our predictors have a effect on the response."),
              tags$li("Consumable (Downloadable) Equations are provided."),
              tags$li("These can be further consumed into the Model Bank for Simulation and Optimization. ")),
      
      )
  )
}