# I'm adding this line to check the git workings
# adding second line for further check on commits
# I still can't see the comments in remote repo master branch

library(shinydashboard)
library(shiny)
library(DT)
library(readxl)
library(plotly)
library(stringr)
library(lpSolveAPI)
library(xlsx)
library(writexl)
library(nloptr)
library(ggplot2)
library(gghighlight)

#source all files in back_end
for(f in list.files(path=c("./UI_Modules/","./Server_Modules/"),
                    recursive=TRUE, full.names=TRUE)){
    source(f)
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "PAE Model Bank V1.0"),
    dashboardSidebar(tags$head(
        tags$style(HTML("
                      .sidebar { height: 90vh; overflow-y: auto; }
                      .dataTables_wrapper { overflow-x: scroll; }
                      " )
        )
    ),width = 320,
                     sidebarMenu(
                         menuItem('Home Care', tabName = 'homecare',
                                  menuSubItem("Slurry Viscosity Drying Model", tabName = "slurry"),
                                  menuSubItem('SD Slurry Properties', tabName = 'bulkdensity'),
                                  menuSubItem('Finished Good Bulk Density (SD Powder)', tabName = 'bulkdensity1'),
                                  menuSubItem('Finished Good Bulk Density (NTR Powder)', tabName = 'bulkdensity2')
                         ),
                         menuItem('Beauty & Personal Care', tabName = 'BCP'
                                  ,menuSubItem('Hair Conditioner Quality - HW Chassis', tabName = 'conditioner')
                                  ,menuSubItem("Skin Care Facial Moisturizer L63", tabName = "skin")
                         ),
                         menuItem('Food & Refreshment', tabName = 'fr')
                     ),
    br(),
    column(12,align = "left",offset = 0,
           a("Download PAE Model Bank V1.0 Guide",
             href="PAE Model Bank User Guide.pdf", 
             download="PAE Model Bank User Guide.pdf",
             class="dl2"))
    ),
    dashboardBody(
        tagList(
            wellPanel(
                h1("Model Bank Functionalities"),
                tags$ul(
                    tags$li(" The application has models built in itself already and allows user to select one for further exploration. "),
                    tags$li(" Import of new simulation data is possible. User can input simulation values as well."),
                    tags$li(" Visualization tab provides visual guide of the imported simualtion data."),
                    tags$li(" Simulation tab allows for simulation of selected models on the imported data or input values."),
                    tags$li(" Optimisation tab helps to optimise the models and draw meaningful insights."),
                )
            )
        ),
        tabItems(
            tabItem(tabName = "slurry",uiOutput("slurry"),                  ),
            tabItem(tabName = "bulkdensity", uiOutput("laundrybulkdensity_uday")),
            tabItem(tabName = "bulkdensity1", uiOutput("sdbulkdensity_uday")),
            tabItem(tabName = "bulkdensity2", uiOutput("ntrbulkdensity_uday")),
            tabItem(tabName = "skin", uiOutput("skincare_kayla")),
            tabItem(tabName = "conditioner", uiOutput("conditioner") ),
            tabItem(tabName = "hair", uiOutput("hair"))
        ),

    )
)


# Define server logic required to draw a histogram
server <-  function(input, output, session) {

    output$slurry <- renderUI(
        slurryUI("slurrymodelUI")
    )

    output$conditioner <- renderUI(
        conditionerUI("conditionermodelUI")
    )

    output$laundrybulkdensity_uday <- renderUI(
        SD_slurryUI("SD_slurrymodel")
    )

    output$sdbulkdensity_uday <- renderUI(
        SD_powderUI("SD_powdermodel")
    )

    output$ntrbulkdensity_uday <- renderUI(
        NTR_powderUI("NTR_powdermodel")
    )

    output$skincare_kayla <- renderUI(
        Facial_MoisturizerUI("Facial_Moisturizermodel")
    )

    output$hair <- renderUI(
        hair_conditionerUI("hairmodel")
    )

    slurryServer("slurrymodelUI", top_session=session)
    conditionerServer("conditionermodelUI", top_session = session)
    SD_slurryServer("SD_slurrymodel", top_session=session)
    SD_powderServer("SD_powdermodel", top_session=session)
    NTR_powderServer("NTR_powdermodel", top_session=session)
    Facial_MoisturizerServer("Facial_Moisturizermodel", top_session=session)
    #hair_conditionerServer("hairmodel", top_session = session)
}
# Run the application
shinyApp(ui = ui, server = server)

