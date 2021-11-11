#install.packages('pkgconfig','munsell','ggplot2','tidyselect','generics','yaml')

library(shinydashboard)
library(shiny)
library(DT)
library(readxl)
library(plotly)
library(stringr)
library(xlsx)
library(writexl)
library(ggplot2)
library(gghighlight)
library(shinyjs)
library(Rcpp)
library(shinyWidgets)
library(shinythemes)
library(reticulate)
library(waiter)
library(shinycssloaders)
library(shinyBS)

# reticulate::virtualenv_create(envname = "myreticulate")
# reticulate::virtualenv_install("myreticulate",  packages = c("pandas","numpy","scikit-learn","seaborn","matplotlib","openpyxl","statsmodels"))
# reticulate::use_virtualenv("myreticulate", required = TRUE)

#source all files in back_end
for(f in list.files(path="./module files",
                    recursive=TRUE, full.names=TRUE)){
  print(as.character(f))
  source(f)
}
#source_python("python_modeller.py")

ui <- function(request){
  dashboardPage(skin = c("green"),
                dashboardHeader(title = span("PAE Non-Linear Regression Modeller V1.0",
                                             style = "font-family: Georgia, sans-serif; font-size: 20px"), titleWidth=420),
                dashboardSidebar(disable = TRUE,tags$head(
                  tags$style(HTML("   .sidebar { height: 90vh; overflow-y: auto; }
                      .dataTables_wrapper { overflow-x: scroll; }
                      " )
                  )
                ),width = 100
              ),
                dashboardBody(
                  
                  tags$style("* { font-family: Helvetica, sans-serif; }"),
                    tabsetPanel(id = 'tabs',
                      tabPanel('INTRO', introUI("introUI")),
                      tabPanel('BUILDER', modellerUI("modellerUI")),
                      tabPanel('RESULTS', modelling_resultsUI("modelling_resultsUI"))
                     )
                    )
                )
       
    
}

server <-  function(input, output, session) {
  
  introServer("introUI",top_session = session)
  modellerServer("modellerUI",top_session = session)
  modelling_resultsServer("modelling_resultsUI",top_session = session)
  
  
  # observe({
  #   reactiveValuesToList(input)
  #   session$doBookmark()
  # })
  # onBookmarked(updateQueryString)

}
# Run the application
shinyApp(ui, server, enableBookmarking = "url")