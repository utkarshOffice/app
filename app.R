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
ui <- function(request){
    dashboardPage(
        dashboardHeader(title = "PAE Model Bank V1.0"),
        dashboardSidebar(tags$head(
            tags$style(HTML("
                      .sidebar { height: 90vh; overflow-y: auto; }
                      .dataTables_wrapper { overflow-x: scroll; }
                      " )
            )
        ),width = 320,
        sidebarMenu(
            menuItem('Processing', tabName = 'proc',
                    menuItem('Home Care', tabName = 'homecare',
                             menuSubItem("Slurry Viscosity Drying Model", tabName = "slurry"),
                             menuSubItem('SD Slurry Properties', tabName = 'bulkdensity'),
                             menuSubItem('Finished Good Bulk Density (SD Powder)', tabName = 'bulkdensity1'),
                             menuSubItem('Finished Good Bulk Density (NTR Powder)', tabName = 'bulkdensity2'),
                             menuSubItem('Chill Roll Mill Model', tabName = 'crm')
                             
                    ),
            menuItem('Beauty & Personal Care', tabName = 'BCP',
                     menuSubItem('Hair Conditioner Quality - HW Chassis', tabName = 'conditioner'),
                     menuSubItem("Skin Care Facial Moisturizer L63", tabName = "skin")
                     ,menuSubItem("Soap Lather Volume", tabName = "soap_lather")
                     ,menuSubItem("Soap Bar Hardness", tabName = "soap_hardness")
                     
            )
            ),
            menuItem('Packaging', tabName = 'pack',
                     menuSubItem('MonoPP Haiti Type Model', tabName = 'monoPP_haiti')
                     
            )
            # menuItem('Food & Refreshment', tabName = 'fr')
        ),
        br(),
        column(12,align = "left",offset = 0,
               a("Download PAE Model Bank V1.0 Guide",
                 href="PAE Model Bank User Guide.pdf", 
                 download="PAE Model Bank User Guide.pdf",
                 class="dl2"))
        #br(),
        #bookmarkButton()
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
                tabItem(tabName = "slurry", slurryUI("slurrymodelUI")),
                tabItem(tabName = "bulkdensity", SD_slurryUI("SD_slurrymodel")),
                tabItem(tabName = "bulkdensity1", SD_powderUI("SD_powdermodel")),
                tabItem(tabName = "bulkdensity2", NTR_powderUI("NTR_powdermodel")),
                tabItem(tabName = "skin", Facial_MoisturizerUI("Facial_Moisturizermodel")),
                tabItem(tabName = "conditioner", conditionerUI("conditionermodelUI")),
                tabItem(tabName = "crm", chillrollUI("chillrollmodel")),
                tabItem(tabName = "monoPP_haiti", monoPP_HaitiUI("monoPP_Haitimodel")),
                tabItem(tabName = "soap_lather", lather_volumeUI("latherVolumeUI")),
                tabItem(tabName = "soap_hardness", Soap_HardnessUI("soapHardnessUI"))
            ),
            
        )
    )
}


# Define server logic required to draw a histogram
server <-  function(input, output, session) {

    slurryServer("slurrymodelUI", top_session=session)
    conditionerServer("conditionermodelUI", top_session = session)
    SD_slurryServer("SD_slurrymodel", top_session=session)
    SD_powderServer("SD_powdermodel", top_session=session)
    NTR_powderServer("NTR_powdermodel", top_session=session)
    Facial_MoisturizerServer("Facial_Moisturizermodel", top_session=session)
    chillrollServer("chillrollmodel", top_session=session)
    monoPP_HaitiServer("monoPP_Haitimodel", top_session=session)
    lather_volumeServer("latherVolumeUI", top_session=session)
    Soap_hardnessServer("soapHardnessUI", top_session=session)
    # i chenged it man
    observe({
        reactiveValuesToList(input)
        session$doBookmark()
    })
    onBookmarked(updateQueryString)

    }
# Run the application
shinyApp(ui, server, enableBookmarking = "url")


# changed by abhishek
