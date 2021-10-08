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
library(shinycssloaders)
library(waiter)

#source all files in back_end
for(f in list.files(path="./module files",
                    recursive=TRUE, full.names=TRUE)){
  print(as.character(f))
  source(f)
}
source_python("python_modeller.py")

ui <- function(request){
  dashboardPage(skin = c("green"),
                dashboardHeader(title = "PAE Non-Linear Regression Modeller V1.0", titleWidth=380),
                dashboardSidebar(disable = TRUE,tags$head(
                  tags$style(HTML("   .sidebar { height: 90vh; overflow-y: auto; }
                      .dataTables_wrapper { overflow-x: scroll; }
                      " )
                  )
                ),width = 100
              ),
                dashboardBody(
                    tabsetPanel(
                      tabPanel(title='Introduction', introUI("introUI")),
                      tabPanel(title='Model Builder', modellerUI("modellerUI")),
                      tabPanel(title='Modelling Results', modelling_resultsUI("modelling_resultsUI"))
                     )
                    )
                )
       
    
}

server <-  function(input, output, session) {
  
  introServer("introUI",top_session = session)
  modellerServer("modellerUI",top_session = session)
  modelling_resultsServer("modelling_resultsUI",top_session = session)
  
  
  observe({
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(updateQueryString)
  
}
# Run the application
shinyApp(ui, server, enableBookmarking = "url")