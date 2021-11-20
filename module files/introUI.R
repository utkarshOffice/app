introUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      h2("Introduction to the Modeller"), 
      br(),
      em("Contact: satyajit.wattamwar@unilever.com, ashutosh.gandhi@unilever.com"), 
      br(),
      br(),
      p("This is a machine learning modeling application for the development of data-based models 
      that predict sealing strength of flexible laminate materials. The application models the
      influence of sealing parameters (dwell time, temperature, pressure) on seal strength 
      in the form of consumable equations.
      The model development is performed based on a Design of Experiment (DOE) dataset and using 
      various machine learning algorithms for regression analysis.
      This is an automated model development workflow where the user simply needs to upload the DOE dataset.
      Once the regression analysis is complete, the application compares the performance of the different 
      algorithms and allows the user to select the best one.
      Underneath is a schematic representation of the steps involved in this automated workflow:
                             "),
      fluidRow(column(width = 12, img(src= "seal_strength_img1.JPG", width="50%", align="center"))),
      br(),
      h4(HTML(paste0("<b>","Key Features & Usage Instructions:","</b>"))),

      tags$ul(tags$li("Data uploaded is cleaned. Wrangling is done in backend automatically using python scripts."),
              tags$li("Material on which the modelling should be done can be selected from the drop-down menu. (This is based on the materials available 
                      inside the uploaded Excel DOE dataset.)"),
              tags$li("The user can also select the predictors to be used for the model."),
              tags$li("The model is developed once the user clicks on \"Build\"."),
              tags$li("Different modeling algorithms are used in the back end."),
              tags$li("The results of the top two performing models are then displayed along with their complete set of metrics as the results summary"),
              tags$li("Consumable (Downloadable) Model Equations are delivered in the output."),
              tags$li("These equations can then be separately on-boarded into the Model bank for democratized simulation and optimization runs."),
              tags$li("The user can upload two types of sealing DOE data - 2-ply and 3-ply material structures. Other structures can also be modeled but it requires manipulating the contents of the set upload file format. In order to do this please contact the F&R Digital Packaging team.")),
      
      )
  )
}