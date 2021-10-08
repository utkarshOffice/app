introUI <- function(id){
  ns <- NS(id)
  tagList(
   fluidPage(
     h1("Data-based Sealing Strength Models for Flexibles Packaging"), 
     br(),
     em("Contact: Omer.Bin-Younos@unilever.com"), 
     br(),
     br(),
     p("The data-based model predicts sealing strength of a flexible laminate material based on the sealing parameters - temperature, dwell time and pressure.
                             It can be used to identify optimal sealing parameters that would deliver the desired seal strength under the applicable line constraints, for e.g. line speed setting, known temperature fluctuation across the sealing jaw on the machine, etc.
                             "),
     column(width = 12, img(src= "seal_strength_img1.JPG", width="50%", align="center")),
     
     p("Design of Experiment (DoE) studies have been performed to create extensive data on the sealing behavior of the flexible laminate. A sophisticated mathematical model is then built on this data. 
                                                 This model is now made available to you through this functionality on DataLab."),
     h3("Benefits of the Model"), 
     column(width = 12, img(src= "seal_strength_img2.jpg", width="50%", align="center")),
     p("The simulation toolsets of Profiler and Optimizer allow quick trial and error on heat sealing parameters - dwell time, 
                                               temperature and pressure, to identify the ones that deliver the desired sealing strength performance. 
                                               These investigations now take only a few seconds. 
                                               The aim with this is to accelerate the flexible packaging development through digital experimentation.
                                              "), 
     h3("Materials available in the model"),
     p("The laminates and the packaging formats that are already a part of the current model offering are listed below: "),
     #dataTableOutput(ns("sampletable_ashutosh")),
     
     h4("Note to all users:"), 
     p("We are constantly interested to increase the model, laminate material and pack format offering of this model. 
                                                 Please contact us with your interests, if they are not covered by the existing offering."),
     
     h4("Developed By:"), 
     #column(width = 12, img(src= "seal_strength_img3.png",  height="50%", width="50%", align="left")),
     img(src= "seal_strength_img3.JPG",width="15%", align="center")
   )
  )
}