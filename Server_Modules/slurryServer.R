slurryServer <- function(id, top_session){
  moduleServer(
    id,
    function(input, output, session) {
      #ns <- session$ns
      gicon <- function(x) as.character(icon(x, lib = "glyphicon"))
      
      proxy_opt <- DT::dataTableProxy('optimiser_table1')
      proxy_opt2 <- DT::dataTableProxy('optimiser_table12')
      proxy_nlopt <- DT::dataTableProxy('optimiser_table1_adititorque')
    
      
      manualinput <- reactiveVal(NULL)
      manual <- reactiveVal(NULL)
      importresults <- reactiveVal(NULL)
      optimise <- reactiveVal(NULL)
      optimise1 <- reactiveVal(NULL)
      optimise2 <- reactiveVal(NULL)
      
      opt<-reactiveValues(tab_1=NULL,tab_1_uday=NULL,tab_12=NULL,tab_2=NULL)
      
      
      x <- data.frame(Models <- c("Torque := 550.942517169757 *TargetSMC + 657.293920443309 *NaLASnew + -178.742137567497 *AlkSilicatenew + -145.925867640988 *CP5new +484.822800006602 *LSAnew + 205.55728325435 *SCMCnew + 14.7480644403947 *Sulphatenew + -435.366522312941 *0 + TargetSMC * (NaLASnew *-2509.30663159213) +TargetSMC * (Sulphatenew * -909.820229343898) +TargetSMC * (LSAnew * -2140.43715033235)"
                                  ,"TurningPoint := 1.64652727504537*TargetSMC + -0.340054974118285*NaLAS + 0.0349876142645199*AlkSilicate + -0.26064073764549*CP5 + -0.0575389664392278*LSA + -1.17237663840093*SCMC + -0.298363251134605*Sulphate"))
      
      b <- c("TargetSMC","NaLAS (dry basis)","AlkSilicate (dry basis)","LSA (dry basis)","CP5 (dry basis)","SCMC (dry basis)","Sulphate (dry basis)")
      z <- c("NaLASnew",	"AlkSilicatenew",	"CP5new",	"LSAnew",	"SCMCnew","Sulphatenew")
      all_vars <- c("NaLASnew",	"AlkSilicatenew",	"CP5new",	"LSAnew",	"SCMCnew","Sulphatenew","TargetSMC","NaLAS","AlkSilicate","LSA","CP5","SCMC","Sulphate")
      
      # model bank table
      output$models <- renderDataTable({
        datatable(x, colnames = c("Models"))
      } )
      
      # go to simulation
      observeEvent(input$commit,{
        updateTabsetPanel(top_session, "tabs", selected = "Simulation")
      })
      # go to visualization
      observeEvent(input$commit2,{
        updateTabsetPanel(top_session, "tabs", selected = "Visualization")
      })
      #rendering advisory tables
      output$advice <- renderDataTable({
        Advisory_table <- data.frame(Ingredients = c("TargetSMC","NaLAS (dry basis)",
                                                     "Alkaline Silicate (dry basis)",
                                                     "Sodium Carbonate (dry basis)","CP5 (dry basis)",
                                                     "SCMC (dry basis)", "Sodium Sulphate (dry basis)"),
                                     Lower_Level = c(0.287, 0.13, 0.07, 0.17, 0.00, 0.00, 0.17),
                                     Upper_Level = c( 0.37, 0.41, 0.16, 0.45, 0.03, 0.01, 0.52))
        datatable(Advisory_table)
      })
      output$knntable <- renderDataTable({
        frame <- data.frame(c("(TurningPoint < 0.31) then ClosestFriend",
                              "(TurningPoint between 0.31 and 0.33) then ClosestFriend",
                              "(TurningPoint between 0.33 and 0.34) then ClosestFriend",
                              "(TurningPoint between 0.34 and 0.35) then ClosestFriend",
                              "(TurningPoint between 0.35 and 0.375) then ClosestFriend",
                              "(TurningPoint between 0.375 and 0.425) then ClosestFriend",
                              "(TurningPoint between 0.425 and 0.45) then ClosestFriend",
                              "(TurningPoint between 0.45 and 0.48) then ClosestFriend",
                              "(TurningPoint between 0.48 and 0.53) then ClosestFriend",
                              "(TurningPoint between 0.53 and 0.55) then ClosestFriend",
                              "(TurningPoint > 0.55) then ClosestFriend"),
                            c("SRC12.5","SRC13.5","C53","SRG20","M20","M25","M30","H35","F40","P42","P45"))
        colnames(frame) <- c("Interval of Turning Point values","Closest Friend")
        datatable(frame)
      })
      output$classificationtable <- renderDataTable({
        frame2 <- data.frame(c("(TurningPoint < 0.35) then Dryability",
                               "(TurningPoint between 0.35 and 0.45) then Dryability",
                               "(TurningPoint between 0.45 and 0.55) then Dryability",
                               "(TurningPoint above 0.55) then Dryability"),
                             c("Green(Easiest)","Amber(Intermediate)","Red(Hardest)","Impossible"))
        colnames(frame2) <- c("Turning Point Value Intervals","Classification")
        datatable(frame2)
      })
      
      colors <- c("red","black", "green","yellow","violet")

      # graph based profiler
      observeEvent(req(input$profiler_nalas),{
        NaLAS <- reactive(input$profiler_nalas)
        TargetSMC <- reactive(input$profiler_targetsmc)
        CP5 <- reactive(input$profiler_cp5)
        SCMC <- reactive(input$profiler_scmc)
        Sulphate <- reactive(input$profiler_sulphate)
        AlkSilicate <- reactive(input$profiler_alksilicate)
        LSA <- reactive(input$profiler_lsa)
        the_sum <- reactive(as.numeric(NaLAS()+CP5()+SCMC()+Sulphate()+AlkSilicate()+LSA()))
        
        observeEvent(input$profiler_nalas | input$profiler_scmc | input$profiler_alksilicate | input$profiler_cp5 |input$profiler_lsa | input$profiler_sulphate | input$profiler_targetsmc,
                     {
                       if(the_sum() == 1){
                         eqn2 <- "1.64652727504537*TargetSMC() + -0.340054974118285*NaLAS() + 0.0349876142645199*AlkSilicate() + -0.26064073764549*CP5() + -0.0575389664392278*LSA() + -1.17237663840093*SCMC() + -0.298363251134605*Sulphate()"
                         
                         TurningPoint <- reactive(eval(parse(text = eqn2)))
                         ClosestFriend <- reactiveVal(NULL)
                         Dryability <- reactiveVal(NULL)
                         
                         if(TurningPoint() <= 0.31) {ClosestFriend("SRC12.5")}
                         else if(TurningPoint()>0.31 & TurningPoint() <= 0.33){ClosestFriend("SRC13.5")}
                         else if(TurningPoint()>0.33 & TurningPoint() <= 0.34){ClosestFriend("C53")}
                         else if(TurningPoint()>0.34 & TurningPoint() <= 0.35){ClosestFriend("SRG20")}
                         else if(TurningPoint()>0.35 & TurningPoint() <= 0.375){ClosestFriend("M20")}
                         else if(TurningPoint()>0.375 & TurningPoint() <= 0.425){ClosestFriend("M25")}
                         else if(TurningPoint()>0.425 & TurningPoint() <= 0.45){ClosestFriend("M30")}
                         else if(TurningPoint()>0.45 & TurningPoint() <= 0.48){ClosestFriend("H35")}
                         else if(TurningPoint()>0.48 & TurningPoint() <= 0.53){ClosestFriend("F40")}
                         else if(TurningPoint()>0.53 & TurningPoint() <= 0.55){ClosestFriend("P42")}
                         else {ClosestFriend("P45")}
                         
                         
                         if (TurningPoint() <= 0.35){Dryability("Green")}
                         else if (TurningPoint() > 0.35 & TurningPoint() <= 0.45){Dryability("Amber")}
                         else if (TurningPoint() > 0.45 & TurningPoint() <= 0.55){Dryability("Red")}
                         else {Dryability("Impossible")}
                         
                         output$profilertable <- renderDataTable({
                           df <- data.frame(a =rep("",11),b =rep("",11),c =rep("",11),d =rep("",11),e =rep("",11),f =rep("",11),
                                            g =rep("",11), h =rep("",11),i =rep("",11), j =rep("",11))
                           rownames(df) <- c("P45","P42","F40","H35","M30","M25","M20","SRG20","C53","SRC13.5","SRC12.5")
                           colnames(df) <- c("Easy(1)","Easy(2)","Easy(3)","Easy(4)","Hard(5)","Hard(6)","Hard(7)","Very_Hard(8)","Very_Hard(9)","Impossible")
                           
                           df["SRC12.5", "Easy(1)"] <- "X"
                           df["SRC13.5", "Easy(2)"] <- "X"
                           df["C53", "Easy(3)"] <- "X"
                           df["SRG20", "Easy(4)"] <- "X"
                           df['M20', "Hard(5)"] <- "X"
                           df["M25", "Hard(6)"] <- "X"
                           df["M30", "Hard(7)"] <- "X"
                           df["H35", "Very_Hard(8)"] <- "X"
                           df["F40", "Very_Hard(9)"] <- "X"
                           df["P42", "Very_Hard(9)"] <- "X"
                           df["P45", "Impossible"] <- "X"
                           
                           if(ClosestFriend()=="SRC12.5"){df["SRC12.5", "Easy(1)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="SRC13.5"){df["SRC13.5", "Easy(2)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="C53"){df["C53", "Easy(3)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="SRG20"){df["SRG20", "Easy(4)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="M20"){df['M20', "Hard(5)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="M25"){df["M25", "Hard(6)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="M30"){df["M30", "Hard(7)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="H35"){df["H35", "Very_Hard(8)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="F40"){df["F40", "Very_Hard(9)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="P42"){df["P42", "Very_Hard(9)"] <- as.character(gicon("ok"))}
                           else {df["P45", "Impossible"] <- as.character(gicon("ok"))}
                           
                           datatable(df, options = list(pageLength = 11), escape = FALSE) %>% formatStyle(
                             c( "Easy(1)","Easy(2)","Easy(3)","Easy(4)"),
                             backgroundColor = styleEqual(c("","X"),c("#C3F3C0","#C3F3C0"))) %>% formatStyle(
                               c("Hard(5)","Hard(6)","Hard(7)"),
                               backgroundColor = styleEqual(c("","X"),c("#FFF157","#FFF157"))) %>% formatStyle(
                                 c("Very_Hard(8)","Very_Hard(9)","Impossible"),
                                 backgroundColor = styleEqual(c("","X"),c("#FF6863","#FF6863")))
                           
                         })
                         
                         
                       }else{
                         Sulphate2 <- reactive(1- as.numeric(NaLAS()+CP5()+SCMC()+AlkSilicate()+LSA()))
                         the_sum2 <- reactive(as.numeric(NaLAS()+CP5()+SCMC()+Sulphate2()+AlkSilicate()+LSA()))
                         
                         eqn2 <- "1.64652727504537*TargetSMC() + -0.340054974118285*NaLAS() + 0.0349876142645199*AlkSilicate() + -0.26064073764549*CP5() + -0.0575389664392278*LSA() + -1.17237663840093*SCMC() + -0.298363251134605*Sulphate2()"
                         TurningPoint <- reactive(eval(parse(text = eqn2)))
                         ClosestFriend <- reactiveVal(NULL)
                         Dryability <- reactiveVal(NULL)
                         
                         if(TurningPoint() <= 0.31) {ClosestFriend("SRC12.5")}
                         else if(TurningPoint()>0.31 & TurningPoint() <= 0.33){ClosestFriend("SRC13.5")}
                         else if(TurningPoint()>0.33 & TurningPoint() <= 0.34){ClosestFriend("C53")}
                         else if(TurningPoint()>0.34 & TurningPoint() <= 0.35){ClosestFriend("SRG20")}
                         else if(TurningPoint()>0.35 & TurningPoint() <= 0.375){ClosestFriend("M20")}
                         else if(TurningPoint()>0.375 & TurningPoint() <= 0.425){ClosestFriend("M25")}
                         else if(TurningPoint()>0.425 & TurningPoint() <= 0.45){ClosestFriend("M30")}
                         else if(TurningPoint()>0.45 & TurningPoint() <= 0.48){ClosestFriend("H35")}
                         else if(TurningPoint()>0.48 & TurningPoint() <= 0.53){ClosestFriend("F40")}
                         else if(TurningPoint()>0.53 & TurningPoint() <= 0.55){ClosestFriend("P42")}
                         else {ClosestFriend("P45")}
                         
                         
                         if (TurningPoint() <= 0.35){Dryability("Green")}
                         else if (TurningPoint() > 0.35 & TurningPoint() <= 0.45){Dryability("Amber")}
                         else if (TurningPoint() > 0.45 & TurningPoint() <= 0.55){Dryability("Red")}
                         else {Dryability("Impossible")}
                         
                         output$profilertable <- renderDataTable({
                           df <- data.frame(a =rep("",11),b =rep("",11),c =rep("",11),d =rep("",11),e =rep("",11),f =rep("",11),
                                            g =rep("",11), h =rep("",11),i =rep("",11), j =rep("",11))
                           rownames(df) <- c("P45","P42","F40","H35","M30","M25","M20","SRG20","C53","SRC13.5","SRC12.5")
                           colnames(df) <- c("Easy(1)","Easy(2)","Easy(3)","Easy(4)","Hard(5)","Hard(6)","Hard(7)","Very_Hard(8)","Very_Hard(9)","Impossible")
                           
                           df["SRC12.5", "Easy(1)"] <- "X"
                           df["SRC13.5", "Easy(2)"] <- "X"
                           df["C53", "Easy(3)"] <- "X"
                           df["SRG20", "Easy(4)"] <- "X"
                           df['M20', "Hard(5)"] <- "X"
                           df["M25", "Hard(6)"] <- "X"
                           df["M30", "Hard(7)"] <- "X"
                           df["H35", "Very_Hard(8)"] <- "X"
                           df["F40", "Very_Hard(9)"] <- "X"
                           df["P42", "Very_Hard(9)"] <- "X"
                           df["P45", "Impossible"] <- "X"
                           
                           if(ClosestFriend()=="SRC12.5"){df["SRC12.5", "Easy(1)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="SRC13.5"){df["SRC13.5", "Easy(2)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="C53"){df["C53", "Easy(3)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="SRG20"){df["SRG20", "Easy(4)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="M20"){df['M20', "Hard(5)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="M25"){df["M25", "Hard(6)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="M30"){df["M30", "Hard(7)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="H35"){df["H35", "Very_Hard(8)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="F40"){df["F40", "Very_Hard(9)"] <- as.character(gicon("ok"))}
                           else if(ClosestFriend()=="P42"){df["P42", "Very_Hard(9)"] <- as.character(gicon("ok"))}
                           else {df["P45", "Impossible"] <- as.character(gicon("ok"))}
                           
                           datatable(df, options = list(pageLength = 11), escape = FALSE) %>% formatStyle(
                             c( "Easy(1)","Easy(2)","Easy(3)","Easy(4)"),
                             backgroundColor = styleEqual(c("","X"),c("#C3F3C0","#C3F3C0"))) %>% formatStyle(
                               c("Hard(5)","Hard(6)","Hard(7)"),
                               backgroundColor = styleEqual(c("","X"),c("#FFF157","#FFF157"))) %>% formatStyle(
                                 c("Very_Hard(8)","Very_Hard(9)","Impossible"),
                                 backgroundColor = styleEqual(c("","X"),c("#FF6863","#FF6863")))
                         })
                         
                         
                       }
                       
                       
                     })
      })
      
      #  Profiler for Torque
      observeEvent(req(input$profiler_nalas), {

        eqn1 <- "550.942517169757 *TargetSMC + 657.293920443309 *NaLASnew() + -178.742137567497 *AlkSilicatenew() + -145.925867640988 *CP5new() +484.822800006602 *LSAnew() + 205.55728325435 *SCMCnew() + 14.7480644403947 *Sulphatenew() + -435.366522312941 *0 + TargetSMC * (NaLASnew() *-2509.30663159213) +TargetSMC * (Sulphatenew() * -909.820229343898) +TargetSMC * (LSAnew() * -2140.43715033235)"

        NaLAS <- reactive(input$profiler_nalas)
        TargetSMC <- round(seq(from  = 0.287, to = 0.37, length.out = 100),3)
        CP5 <- reactive(input$profiler_cp5)
        SCMC <- reactive(input$profiler_scmc)
        Sulphate <- reactive(input$profiler_sulphate)
        AlkSilicate <- reactive(input$profiler_alksilicate)
        LSA <- reactive(input$profiler_lsa)
        the_sum <- reactive(as.numeric(NaLAS()+CP5()+SCMC()+Sulphate()+AlkSilicate()+LSA()))
        

        observeEvent(input$profiler_nalas | input$profiler_scmc | input$profiler_alksilicate | input$profiler_cp5 |input$profiler_lsa | input$profiler_sulphate | input$profiler_targetsmc,{
          if(the_sum() == 1){
            NaLASnew <- reactive(NaLAS()/the_sum() * (1- TargetSMC))
            AlkSilicatenew <- reactive(AlkSilicate()/the_sum() * (1- TargetSMC))
            CP5new <- reactive(CP5()/the_sum() * (1- TargetSMC))
            LSAnew <- reactive(LSA()/the_sum() * (1- TargetSMC))
            SCMCnew <- reactive(SCMC()/the_sum() * (1- TargetSMC))
            Sulphatenew <- reactive(Sulphate()/the_sum() * (1- TargetSMC))
            
            Torque <- reactive(eval(parse(text = eqn1)))
            
            output$slider_torque <- renderPlot({
              Torque <- Torque()
              data <- data.frame(TargetSMC, Torque)
              ggplot(data, aes(x=TargetSMC, y= Torque)) +
                geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                gghighlight(TargetSMC == input$profiler_targetsmc)
            })
            
            output$profiler_torque <- renderUI({
              em("Torque should fall between 19 and 36.
             A slurry can be sprayed through nozzles only if the Torque is between 19 and 36.")
            })
            
            output$profiler_turningpoint <- renderUI(NULL)

          }
          else{
            Sulphate2 <- reactive(1 - as.numeric(NaLAS()+CP5()+SCMC()+AlkSilicate()+LSA()))
            the_sum2 <- reactive(as.numeric(NaLAS()+CP5()+SCMC()+Sulphate2()+AlkSilicate()+LSA()))
            
            NaLASnew <- reactive(NaLAS()/the_sum2() * (1- TargetSMC))
            AlkSilicatenew <- reactive(AlkSilicate()/the_sum2() * (1- TargetSMC))
            CP5new <- reactive(CP5()/the_sum2() * (1- TargetSMC))
            LSAnew <- reactive(LSA()/the_sum2() * (1- TargetSMC))
            SCMCnew <- reactive(SCMC()/the_sum2() * (1- TargetSMC))
            Sulphatenew <- reactive(Sulphate2()/the_sum2() * (1- TargetSMC))
            
            Torque <- reactive(eval(parse(text = eqn1)))
            
            output$slider_torque <- renderPlot({
              Torque <- Torque()
              ggplot(data=data.frame(TargetSMC, Torque), aes(x=TargetSMC, y= Torque)) +
                geom_line() + geom_point(size = 4)+ theme(text = element_text(size = 20))+
                gghighlight(TargetSMC == input$profiler_targetsmc)
            })
            output$profiler_torque <- renderUI({
              em("Torque should fall between 19 and 36.
             A slurry can be sprayed through nozzles only if the Torque is between 19 and 36.")
            })
            output$profiler_turningpoint <- renderUI({
              em("Since the sum of all variables except TargetSMC is not equal to 1, sulphate is automatically adjusted to make the sum equal to 1.")
            })
          }
          

        })
      }  )
      
      #visualization page renderings
      observeEvent(req(input$datacall),
                   {data <- reactive(read_excel(input$datacall$datapath))
                   #data <- reactive(data()[,-(which(colSums(data())==0))])
                   
                   updateSelectInput(session, "y_axis", choices = colnames(data()), selected = colnames(data())[2])
                   updateSelectInput(session, "x_axis", choices = colnames(data()))
                   updateSelectInput(session, "x_line", choices = colnames(data()))
                   updateSelectInput(session, "y_line", choices = colnames(data()))
                   updateSelectInput(session, "hist_choice", choices = colnames(data()))
                   
                   output$simulationdata <- renderDataTable(round(data(),3))
                   
                   output$multi_lines_graph <- renderPlotly({
                     if(length(input$y_axis) == 1){
                       fig <- plot_ly( x = ~data()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis]],mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = as.character(input$y_axis)) )
                       fig
                     }else if(length(input$y_axis) == 2){
                       fig <- plot_ly(data(), x = ~data()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[1]]], name = input$y_axis[1],mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[2]]], name = input$y_axis[2], mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = "") )
                       fig
                     }
                     else if(length(input$y_axis) == 3){
                       fig <- plot_ly(data(), x = ~data()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[1]]], name = input$y_axis[1],mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[2]]], name = input$y_axis[2], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[3]]], name = input$y_axis[3], mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = "") )
                       fig
                     } else if(length(input$y_axis) == 4){
                       fig <- plot_ly(data(), x = ~data()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[1]]], name = input$y_axis[1],mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[2]]], name = input$y_axis[2], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[3]]], name = input$y_axis[3], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[4]]], name = input$y_axis[4], mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = "") )
                       fig
                     } else if(length(input$y_axis) == 5){
                       fig <- plot_ly(data(), x = ~data()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[1]]], name = input$y_axis[1],mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[2]]], name = input$y_axis[2], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[3]]], name = input$y_axis[3], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[4]]], name = input$y_axis[4], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[5]]], name = input$y_axis[5], mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = "") )
                       fig
                     } else if(length(input$y_axis) == 6){
                       fig <- plot_ly(data(), x = ~data()[[input$x_axis]], marker=list(size=10))
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[1]]], name = input$y_axis[1],mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[2]]], name = input$y_axis[2], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[3]]], name = input$y_axis[3], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[4]]], name = input$y_axis[4], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[5]]], name = input$y_axis[5], mode = 'lines')
                       fig <- fig %>% add_lines(y = ~data()[[input$y_axis[6]]], name = input$y_axis[6], mode = 'lines')
                       fig <- fig %>% layout(xaxis = list(title = as.character(input$x_axis)),yaxis = list (title = "") )
                       fig
                     }
                   })
                   
                   output$ggsmooth <- renderPlot({
                     if(input$smooth2){
                       if(length(input$y_axis) == 1){
                         ggplot(data(), aes(x= data()[[input$x_axis]],y= data()[[input$y_axis]])) +
                           geom_point(col="blue") + geom_smooth(method="lm", col="blue") +xlab(as.character(input$x_axis))+
                           ylab(as.character(input$y_axis))
                         #labs(x = as.character(input$x_axis),y = as.character(input$y_axis))
                       }else{
                         #scaleFactor <- max(data()[[input$y_axis[1]]]) / max(data()[[input$y_axis[2]]])
                         first <- input$y_axis[1]
                         second <- input$y_axis[2]
                         
                         ggplot(data(), aes(x=data()[[input$x_axis]])) +
                           geom_point(aes(y=data()[[first]]), col="blue")+
                           geom_point(aes(y=data()[[second]] * 1), col="red", shape = 18)+
                           geom_smooth(aes(y=data()[[first]]), method="lm", col="blue") +
                           geom_smooth(aes(y=data()[[second]] * 1), method="lm", col="red") +
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis))
                       }
                     }else{
                       if(length(input$y_axis) == 1){
                         ggplot(data(), aes(x= data()[[input$x_axis]],y= data()[[input$y_axis]])) +
                           geom_point() +xlab(as.character(input$x_axis))+
                           ylab(as.character(input$y_axis))
                         # labs(x = as.character(input$x_axis),y = as.character(input$y_axis))
                       }else
                       {
                         first <- input$y_axis[1]
                         second <- input$y_axis[2]
                         #scaleFactor <- max(data()[[first]]) / max(data()[[second]])
                         
                         ggplot(data(), aes(x=data()[[input$x_axis]])) +
                           geom_point(aes(y=data()[[first]]), col="blue")+
                           geom_point(aes(y=data()[[second]] * 1), col="red", shape = 18)+
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis))
                       }
                     }
                   })
                   
                   output$ggscatter <- renderPlot({
                     if(input$smooth){
                       if(length(input$y_axis) == 1){
                         ggplot(data(), aes(x= data()[[input$x_axis]],y= data()[[input$y_axis]])) +
                           geom_point(col="blue") + geom_line(col="blue") +labs(x = as.character(input$x_axis))+
                           ylab(as.character(input$y_axis))
                       }else{
                         #scaleFactor <- max(data()[[input$y_axis[1]]]) / max(data()[[input$y_axis[2]]])
                         first <- input$y_axis[1]
                         second <- input$y_axis[2]
                         
                         ggplot(data(), aes(x=data()[[input$x_axis]])) +
                           geom_point(aes(y=data()[[first]]), col="blue")+
                           geom_point(aes(y=data()[[second]] * 1), col="red", shape = 18)+
                           geom_line(aes(y=data()[[first]]), col="blue")+
                           geom_line(aes(y=data()[[second]] * 1), col="red")+
                           # geom_smooth(aes(y=data()[[first]]), method="loess", col="blue") +
                           # geom_smooth(aes(y=data()[[second]] * 20), method="loess", col="red") +
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis))
                       }
                     }else{
                       if(length(input$y_axis) == 1){
                         ggplot(data(), aes(x= data()[[input$x_axis]],y= data()[[input$y_axis]])) +
                           geom_point() +labs(x = as.character(input$x_axis))
                       }else
                       {
                         first <- input$y_axis[1]
                         second <- input$y_axis[2]
                         #scaleFactor <- max(data()[[first]]) / max(data()[[second]])
                         
                         ggplot(data(), aes(x=data()[[input$x_axis]])) +
                           geom_point(aes(y=data()[[first]]), col="blue")+
                           geom_point(aes(y=data()[[second]] * 1), col="red", shape = 18)+
                           scale_y_continuous(name=as.character(first), sec.axis=sec_axis(~./1, name=as.character(second))) +
                           theme(
                             axis.title.y.left=element_text(color="blue"),
                             axis.text.y.left=element_text(color="blue"),
                             axis.title.y.right=element_text(color="red"),
                             axis.text.y.right=element_text(color="red")
                           )+labs(x = as.character(input$x_axis))
                       }
                     }
                   })
                   
                   output$hist <- renderPlot({
                     ggplot(data(), aes(x= data()[[input$hist_choice]])) +
                       geom_histogram(color="black", fill="lightblue")+
                       labs(x = as.character(input$hist_choice))
                   })
                   })
      
      #simulation page renderings
      observeEvent(req(x),{
        x1 <- reactiveValues()
        
        observe({
          y <- reactive({
            #munits <- rep("0-1",length(b))
            values <- c(0.287, 0.13, 0.07, 0.17, 0.00, 0.00, 0.17)
            sqr <- data.frame(t(values))
            colnames(sqr) <- b
            rownames(sqr) <- c( "Enter Simulation Values")
            sqr
          })
          x1$df <- y()
        })
        
        output$simulation_input <- renderDataTable({ 
          datatable(x1$df, editable = T) %>%
            formatStyle(
              "NaLAS (dry basis)",
              color = styleInterval(c(0.129, 0.411), c('red', 'black', 'red')))%>%
            formatStyle(
              "TargetSMC",
              color = styleInterval(c(0.286, 0.371), c('red', 'black', 'red')))%>%
            formatStyle(
              "AlkSilicate (dry basis)",
              color = styleInterval(c(0.069, 0.161), c('red', 'black', 'red')))%>%
            formatStyle(
              "CP5 (dry basis)",
              color = styleInterval(c(0.00, 0.031), c('red', 'black', 'red')))%>%
            formatStyle(
              "LSA (dry basis)",
              color = styleInterval(c(0.169, 0.451), c('red', 'black', 'red')))%>%
            formatStyle(
              "Sulphate (dry basis)",
              color = styleInterval(c(0.169, 0.521), c('red', 'black', 'red')))%>%
            formatStyle(
              "SCMC (dry basis)",
              color = styleInterval(c(0.00, 0.011), c('red', 'black', 'red')))
        })
        
        proxy <- dataTableProxy("simulation_input")
        
        observeEvent(input[["simulation_input_cell_edit"]], {
          info <- input[["simulation_input_cell_edit"]]
          
          i <- info$row
          j <- info$col
          v <- info$value
          
          if(!is.na(v) && !is.na(as.numeric(v))){
            v <- as.numeric(v)
            x1$df[i, j] <- DT::coerceValue(v,x1$df[i, j])}
          
          rep <- x1$df
          DT::replaceData(proxy, rep, resetPaging = FALSE)
          
          #var_sum <- reactive(sum(as.numeric(x1$df[1,c(2,3,4,5,6,7)])))
          
        })
        

        var_sum <- reactive(sum(as.numeric(x1$df[1,c(2,3,4,5,6,7)])))
        
        observeEvent(input[["simulation_input_cell_edit"]],{
          if(var_sum() == 1 ){
            output$valWarning <- renderUI(NULL)
          }else{
            output$valWarning <- renderUI({
              em("The sum of all ingredients except TargetSMC should equal to 1.
               Simulation in any other scenario is not permitted. Currently It is:", var_sum(), style = "color:red")
            })
          }  
          })
        
        
        
        
        output$modeltable <- renderDataTable(
          datatable(x1$df)
        )
        
        observeEvent(input$simulate,{
          eqn1 <- "550.942517169757 *TargetSMC + 657.293920443309 *NaLASnew + -178.742137567497 *AlkSilicatenew + -145.925867640988 *CP5new +484.822800006602 *LSAnew + 205.55728325435 *SCMCnew + 14.7480644403947 *Sulphatenew + -435.366522312941 *0 + TargetSMC * (NaLASnew *-2509.30663159213) +TargetSMC * (Sulphatenew * -909.820229343898) +TargetSMC * (LSAnew * -2140.43715033235)"
          eqn2 <- "1.64652727504537*TargetSMC + -0.340054974118285*NaLAS + 0.0349876142645199*AlkSilicate + -0.26064073764549*CP5 + -0.0575389664392278*LSA + -1.17237663840093*SCMC + -0.298363251134605*Sulphate"
          
          slurry_sum <- reactive( as.numeric(x1$df[1,c("NaLAS (dry basis)")])+ as.numeric(x1$df[1,c("AlkSilicate (dry basis)")])+ as.numeric(x1$df[1,c("CP5 (dry basis)")])+
                                    as.numeric(x1$df[1,c("LSA (dry basis)")])+ as.numeric(x1$df[1,c("SCMC (dry basis)")])+ as.numeric(x1$df[1,c("Sulphate (dry basis)")]))
          
          if(slurry_sum() == 1){
            TargetSMC <- as.numeric(x1$df[1,c("TargetSMC")])
            NaLASnew <- as.numeric(x1$df[1,c("NaLAS (dry basis)")])/slurry_sum() * (1- TargetSMC)
            AlkSilicatenew <- as.numeric(x1$df[1,c("AlkSilicate (dry basis)")])/slurry_sum() * (1- TargetSMC)
            CP5new <- as.numeric(x1$df[1,c("CP5 (dry basis)")])/slurry_sum() * (1- TargetSMC)
            LSAnew <- as.numeric(x1$df[1,c("LSA (dry basis)")])/slurry_sum() * (1- TargetSMC)
            SCMCnew <- as.numeric(x1$df[1,c("SCMC (dry basis)")])/slurry_sum() * (1- TargetSMC)
            Sulphatenew <- as.numeric(x1$df[1,c("Sulphate (dry basis)")])/slurry_sum() * (1- TargetSMC)
            
            output$newvals <- renderDataTable({
              round(data.frame(cbind(NaLASnew,AlkSilicatenew,CP5new,LSAnew,SCMCnew,Sulphatenew)),3)
            })
            
            output$simulation_heading <- renderUI({
              h3("Calculated Values for New Variables")
            })
            
            output$Predicted_Values <- renderUI({
              h3("Predicted Values and their Classification")
            })
            
            for(i in b){
              j <- str_replace(i, " \\s*\\([^\\)]+\\)", "")
              eqn2 <- gsub(j, x1$df[1,i], eqn2)
            }
            
            Torque <- eval(parse(text = eqn1))
            TurningPoint <- eval(parse(text = eqn2))
            ClosestFriend <- NULL
            Dryability <- NULL
            
            if(Torque <19 | Torque >36){
              output$torque_message <- renderUI({
                em("Torque value should be between 19 & 36. Above value is out of bounds.", style = "color:red")
              })
            }else{
              output$torque_message <- renderUI({NULL})
            }
            
            if(TurningPoint <= 0.31) {ClosestFriend <-  "SRC12.5"}
            else if(TurningPoint > 0.31 & TurningPoint <= 0.33){ClosestFriend <-  "SRC13.5"}
            else if(TurningPoint > 0.33 & TurningPoint <= 0.34){ClosestFriend <-  "C53"}
            else if(TurningPoint > 0.34 & TurningPoint <= 0.35){ClosestFriend <-  "SRG20"}
            else if(TurningPoint > 0.35 & TurningPoint <= 0.375){ClosestFriend <-  "M20"}
            else if(TurningPoint > 0.375 & TurningPoint <= 0.425){ClosestFriend <-  "M25"}
            else if(TurningPoint > 0.425 & TurningPoint <= 0.45){ClosestFriend <-  "M30"}
            else if(TurningPoint > 0.45 & TurningPoint <= 0.48){ClosestFriend <-  "H35"}
            else if(TurningPoint > 0.48 & TurningPoint <= 0.53){ClosestFriend <-  "F40"}
            else if(TurningPoint > 0.53 & TurningPoint <= 0.55){ClosestFriend <-  "P42"}
            else {ClosestFriend <-  "P45"}
            
            
            if (TurningPoint <= 0.35){Dryability <- "Green"}
            else if (TurningPoint > 0.35 & TurningPoint <= 0.45){Dryability <- "Amber"}
            else if (TurningPoint > 0.45 & TurningPoint <= 0.55){Dryability <- "Red"}
            else {Dryability <- "Impossible"}
            
            tbl <- cbind(Torque,TurningPoint,ClosestFriend,Dryability)
            df <- x1$df
            manual(tbl) 
            manualinput(df)
            
            output$resultcolor <- renderDataTable({
              df <- data.frame(a =rep("",11),b =rep("",11),c =rep("",11),d =rep("",11),e =rep("",11),f =rep("",11),
                               g =rep("",11), h =rep("",11),i =rep("",11), j =rep("",11))
              rownames(df) <- c("P45","P42","H35","F40","M30","M25","M20","SRG20","C53","SRC13.5","SRC12.5")
              colnames(df) <- c("Easy(1)","Easy(2)","Easy(3)","Easy(4)","Hard(5)","Hard(6)","Hard(7)","Very_Hard(8)","Very_Hard(9)","Impossible")
              
              df["SRC12.5", "Easy(1)"] <- "X"
              df["SRC13.5", "Easy(2)"] <- "X"
              df["C53", "Easy(3)"] <- "X"
              df["SRG20", "Easy(4)"] <- "X"
              df['M20', "Hard(5)"] <- "X"
              df["M25", "Hard(6)"] <- "X"
              df["M30", "Hard(7)"] <- "X"
              df["H35", "Very_Hard(9)"] <- "X"
              df["F40", "Very_Hard(8)"] <- "X"
              df["P42", "Very_Hard(9)"] <- "X"
              df["P45", "Impossible"] <- "X"
              
              if(ClosestFriend=="SRC12.5"){df["SRC12.5", "Easy(1)"] <- as.character(gicon("ok"))}
              else if(ClosestFriend=="SRC13.5"){df["SRC13.5", "Easy(2)"] <- as.character(gicon("ok"))}
              else if(ClosestFriend=="C53"){df["C53", "Easy(3)"] <- as.character(gicon("ok"))}
              else if(ClosestFriend=="SRG20"){df["SRG20", "Easy(4)"] <- as.character(gicon("ok"))}
              else if(ClosestFriend=="M20"){df['M20', "Hard(5)"] <- as.character(gicon("ok"))}
              else if(ClosestFriend=="M25"){df["M25", "Hard(6)"] <- as.character(gicon("ok"))}
              else if(ClosestFriend=="M30"){df["M30", "Hard(7)"] <- as.character(gicon("ok"))}
              else if(ClosestFriend=="H35"){df["H35", "Very_Hard(9)"] <- as.character(gicon("ok"))}
              else if(ClosestFriend=="F40"){df["F40", "Very_Hard(8)"] <- as.character(gicon("ok"))}
              else if(ClosestFriend=="P42"){df["P42", "Very_Hard(9)"] <- as.character(gicon("ok"))}
              else {df["P45", "Impossible"] <- as.character(gicon("ok"))}
              
              datatable(df, options = list(pageLength = 11), escape = FALSE) %>% formatStyle(
               c( "Easy(1)","Easy(2)","Easy(3)","Easy(4)"),
                backgroundColor = styleEqual(c("","X"),c("#C3F3C0","#C3F3C0"))) %>% formatStyle(
                  c("Hard(5)","Hard(6)","Hard(7)"),
                  backgroundColor = styleEqual(c("","X"),c("#FFF157","#FFF157"))) %>% formatStyle(
                    c("Very_Hard(8)","Very_Hard(9)","Impossible"),
                    backgroundColor = styleEqual(c("","X"),c("#FF6863","#FF6863")))
              
            })
            
            output$result1 <- renderDataTable(
              { tbl })
            
            nrdata <- as.data.frame(tbl)
            nrdata1 <- as.data.frame(df)
            output$download1 <- downloadHandler(
              filename = function() { "Manual Entry Simulation.xlsx"},
              content = function(file) {
                write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata), file)
              }
            )
          }else
            {
            output$torque_message <- renderUI({
                   h3("Sum of all variables except TargetSMC is not 1. Recalibrate the values to make it 1 in manual entry table and simulate again.", style = "color:red")
               })
            
            output$newvals <- renderDataTable({NULL            })
            
            output$simulation_heading <- renderUI({NULL            })
            
            output$Predicted_Values <- renderUI({NULL            })
            output$resultcolor <- renderDataTable(NULL)
            output$result1 <- renderDataTable(NULL)
            output$download1 <- NULL
          }
          
        })
        
        data <- reactive({
          req(input$datacall)
          inFile <- input$datacall
          if(is.null(inFile)) return(NULL)
          
          xlfile <- read_excel(input$datacall$datapath)
          colnames(xlfile) <- gsub("\\[.*?\\]","",colnames(xlfile))
          colnames(xlfile) <- gsub(" ","",colnames(xlfile))
          colnames(xlfile) <- tolower(colnames(xlfile))
          xlfile
        })
        
        #this part not working correctly, need to look into it
        observeEvent(data(),
                     {b <- str_replace(tolower(b), " \\s*\\([^\\)]+\\)", "")
                       i <- b[!is.element(b,colnames(data()))]
                     if(length(i)>=1){
                       showModal(modalDialog(paste0(as.character(str_c(i,collapse = ",")), " is not present in imported data but required in equation. 0 is placed in the place of missing variables to render the respective coefficients ineffective.")))
                     }
                     })
        
        observeEvent(req(input$simulate2,data()),{
          eqn1 <- "550.942517169757 *TargetSMC + 657.293920443309 *NaLASnew + -178.742137567497 *AlkSilicatenew + -145.925867640988 *CP5new +484.822800006602 *LSAnew + 205.55728325435 *SCMCnew + 14.7480644403947 *Sulphatenew + -435.366522312941 *0 + TargetSMC * (NaLASnew *-2509.30663159213) +TargetSMC * (Sulphatenew * -909.820229343898) +TargetSMC * (LSAnew * -2140.43715033235)"
          eqn2 <- "1.64652727504537*targetsmc + -0.340054974118285*nalas + 0.0349876142645199*alksilicate + -0.26064073764549*cp5 + -0.0575389664392278*lsa + -1.17237663840093*scmc + -0.298363251134605*sulphate"
          
          slurry_sum_import <- reactive(data()[,c("nalas")]+data()[,c("alksilicate")]+data()[,c("cp5")]+
                                          data()[,c("lsa")]+data()[,c("scmc")]+data()[,c("sulphate")])
          
          TargetSMC <- data()[,c("targetsmc")] 
          NaLASnew <- data()[,c("nalas")]/slurry_sum_import() * (1- TargetSMC)
          AlkSilicatenew <- data()[,c("alksilicate")]/slurry_sum_import() * (1- TargetSMC)
          CP5new <- data()[,c("cp5")]/slurry_sum_import() * (1- TargetSMC)
          LSAnew <- data()[,c("lsa")]/slurry_sum_import() * (1- TargetSMC)
          SCMCnew <- data()[,c("scmc")]/slurry_sum_import() * (1- TargetSMC)
          Sulphatenew <- data()[,c("sulphate")]/slurry_sum_import() * (1- TargetSMC)
          
          b <- str_replace(tolower(b), " \\s*\\([^\\)]+\\)", "")
          j <- b[!is.element(b,colnames(data()))]
          
          for(i in j){
            eqn2 <- gsub(i,0,eqn2)
          }
          
          output$Predicted_Values2 <- renderUI({
            h3("Predicted Values and their Classification")
          })
          
          
          for(i in b){
            eqn2 <- gsub(i, paste0("data()$",i), eqn2)
          }
          
          Torque <- eval(parse(text = eqn1))
          TurningPoint <- eval(parse(text = eqn2))
          ClosestFriend <- rep("P45", length(TurningPoint))
          Dryability <- rep("Green", length(TurningPoint))
          
          if(sum(Torque < 0) > 0){
            showModal(modalDialog("Torque values cannot be zero or lower. Such result are impractical."))
          }
          if(sum(TurningPoint < 0) > 0){
            showModal(modalDialog("TurningPoint values cannot be zero or lower. Such result are impractical."))
          }
          
          
          for(i in 1:length(TurningPoint)){
            if(TurningPoint[i] <= 0.31) {ClosestFriend[i] <-  "SRC12.5"}
            else if(TurningPoint[i] > 0.31 & TurningPoint[i] <= 0.33){ClosestFriend[i] <-  "SRC13.5"}
            else if(TurningPoint[i] > 0.33 & TurningPoint[i] <= 0.34){ClosestFriend[i] <-  "C53"}
            else if(TurningPoint[i] > 0.34 & TurningPoint[i] <= 0.35){ClosestFriend[i] <-  "SRG20"}
            else if(TurningPoint[i] > 0.35 & TurningPoint[i] <= 0.375){ClosestFriend[i] <-  "M20"}
            else if(TurningPoint[i] > 0.375 & TurningPoint[i] <= 0.425){ClosestFriend[i] <-  "M25"}
            else if(TurningPoint[i] > 0.425 & TurningPoint[i] <= 0.45){ClosestFriend[i] <-  "M30"}
            else if(TurningPoint[i] > 0.45 & TurningPoint[i] <= 0.48){ClosestFriend[i] <-  "H35"}
            else if(TurningPoint[i] > 0.48 & TurningPoint[i] <= 0.53){ClosestFriend[i] <-  "F40"}
            else if(TurningPoint[i] > 0.53 & TurningPoint[i] <= 0.55){ClosestFriend[i] <-  "P42"}
            else {ClosestFriend[i] <-  "P45"}
            
            
            if (TurningPoint[i] <= 0.35){Dryability[i] <- "Green (Easy)"}
            else if (TurningPoint[i] > 0.35 & TurningPoint[i] <= 0.45){Dryability[i] <- "Amber (Hard)"}
            else if (TurningPoint[i] > 0.45 & TurningPoint[i] <= 0.55){Dryability[i] <- "Red (Very Hard)"}
            else {Dryability[i] <- "Impossible"}
          }
          
          importresults(cbind(Torque,TurningPoint,ClosestFriend,Dryability))

          output$modeltable2 <- renderDataTable({
            result_table <- data.frame(Torque,TurningPoint,ClosestFriend,Dryability)
            colnames(result_table) <- c("Torque","TurningPoint","ClosestFriend","Dryability")
            nrdata <- result_table
            output$download2 <- downloadHandler(
              filename = function() { "Import Data Simulation.xlsx"},
              content = function(file) {
                write_xlsx(list("Import Results" = nrdata), file)
              }
            )
            result_table[(result_table$Torque >19)& (result_table$Torque <36),]
          })
         
        })
      } )
      
      #optimisation for aditi slurry
      
      observeEvent(req(x),
                   {
                     predictors_in_model2 <- c("TargetSMC_[0.287,0.37]","NaLAS(Dry Basis)_[0.13,0.41]","AlkSilicate(Dry Basis)_[0.07,0.16]",
                                               "CP5(Dry Basis)_[0,0.03]", "LSA(Dry Basis)_[0.17,0.45]",
                                               "SCMC(Dry Basis)_[0,0.01]","Sulphate(Dry Basis)_[0.17,0.52]")
                     zero_vector<-rep(1,length(predictors_in_model2))
                     min_vector <- c(0.287,0.13,0.07,0,0.17,0,0.17)
                     max_vector <- c(0.37,0.41,0.16,0.03,0.45,0.01,0.52)
                     coef_data <- data.frame(cbind(predictors_in_model2,zero_vector,min_vector,max_vector),stringsAsFactors = FALSE)
                     opt$tab_1 <- coef_data
                     # opt$tab_1[[2]]<- as.numeric(opt$tab_1[[2]])
                     opt$tab_1[[3]]<- as.numeric(opt$tab_1[[3]])
                     opt$tab_1[[4]]<- as.numeric(opt$tab_1[[4]])
                     # View(opt$tab_1[[2]])
                     
                     #table 1
                     output$optimiser_table1<-renderDataTable({
                       DT::datatable(opt$tab_1,selection="none",editable=TRUE,colnames = c("Predictors_[Expected lower bound, Expected upper bound]","obj_coeff","Lower Bounds(editable)","Upper Bounds(editable)") )
                     })
                     
                     observeEvent(input$optimiser_table1_cell_edit,{
                       info <- input$optimiser_table1_cell_edit
                       
                       i <- info$row
                       j <- info$col
                       v <- info$value
                       # j<- j+1
                       if(j >= 2 && !is.na(v) && !is.na(as.numeric(v))){
                         v <- as.numeric(v)
                         if(j==2 || ( j==3 && opt$tab_1[i, j+1] > v) || (j==4 && opt$tab_1[i, j-1] < v )){
                           opt$tab_1[i,j] <<- DT::coerceValue(v,opt$tab_1[i, j])
                         }
                       }
                       rep <- opt$tab_1
                       DT::replaceData(proxy_opt, rep, resetPaging = FALSE)
                     })
                     
                     observeEvent(input$run_optimiser,{
                       
                       predictors_in_model2<-c("TargetSMC","NaLAS(Dry Basis)","AlkSilicate(Dry Basis)","CP5(Dry Basis)","LSA(Dry Basis)","SCMC(Dry Basis)","Sulphate(Dry Basis)")
                       reg_coeff_in_model2<-c(1.64652727504537,-0.340054974118285,0.0349876142645199,-0.26064073764549,-0.0575389664392278,-1.17237663840093,-0.298363251134605)
                       if(input$inequality_selection=="less than or equal to"){
                         constr<-'<='
                       }
                       else if(input$inequality_selection=="greater than or equal to"){
                         constr<-'>='
                       }
                       else{
                         constr<-'='
                       }
                       # View(constr)#works
                       
                       target<-input$numeric_input
                       number.predictors<-length(predictors_in_model2)
                       # View(opt$tab_1)
                       low.lims<-opt$tab_1[[3]]
                       upp.lims<-opt$tab_1[[4]]
                       low.lims<-as.numeric(low.lims)
                       upp.lims<-as.numeric(upp.lims)
                       # View(upp.lims)
                       objective.in<-opt$tab_1[[2]]
                       objective.in<-as.numeric(objective.in)
                       obj.type<-input$radio_button
                       # View(objective.in)#works
                       
                       lps.model <- make.lp(0,number.predictors)
                       add.constraint(lps.model,reg_coeff_in_model2,constr,target)
                       add.constraint(lps.model,c(0,1,1,1,1,1,1),"=",1)
                       
                       # Bounds for variables
                       set.bounds(lps.model,lower=low.lims)
                       set.bounds(lps.model,upper=upp.lims)
                       # View(low.lims)
                       # View(nrow(low.lims))
                       # View(obj.type)
                       # View(objective.in)
                       # View(opt$tab_1)
                       
                       # Objective function
                       lp.control(lps.model,sense=obj.type) # min or max
                       # View(objective.in) #getting output
                       
                       set.objfn(lps.model,objective.in) # coefficients
                       # View(upp.lims) 
                       
                       # Apply solver
                       solution.status <- solve(lps.model)
                       # View(solution.status)
                       if(solution.status!=0){
                         showModal(modalDialog("Linear Optimisation could not find a solution for the given inputs. Please change the inputs and re-run."))
                       }
                       #unpacking
                       solution.values <- get.primal.solution(lps.model)
                       # View(solution.values)
                       objective.function.value <- solution.values[1]
                       fitted.response <- solution.values[2]
                       solution.values <- solution.values[4:length(solution.values)]
                       
                       results<-data.frame(Value = fitted.response)
                       # colnames(results)<-""
                       row.names(results)<-"TurningPoint"
                       
                       downresults <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Turning Point"), Predicted_or_Optimal_Value= fitted.response)
                       downdf<-data.frame(Response_or_Predictors_or_Objective_Function_Value=c("TargetSMC","NaLAS(Dry Basis)","AlkSilicate(Dry Basis)","CP5(Dry Basis)","LSA(Dry Basis)","SCMC(Dry Basis)","Sulphate(Dry Basis)"),
                                          Predicted_or_Optimal_Value=solution.values)
                       downopt <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Objective func. Value"), Predicted_or_Optimal_Value = objective.function.value)
                       
                       
                       final1 <- rbind(downresults,downdf,downopt)
                       #View(final1)
                       optimise(final1)
                       output$download3 <- downloadHandler(
                         filename = function() { "Linear Optimisation Turning Point (non categorical) .xlsx"},
                         content = function(file) {
                           write_xlsx(list("Optimisation Result" = final1), file)
                         }
                       )
                       
                       # optmiser table2
                       output$optimiser_table2<-renderDataTable({
                         DT::datatable(results) })
                       
                       # optmiser table3
                       output$optimiser_table3<- renderDataTable({
                         df<-data.frame(Predictors=c("TargetSMC","NaLAS(Dry Basis)","AlkSilicate(Dry Basis)","CP5(Dry Basis)","LSA(Dry Basis)","SCMC(Dry Basis)","Sulphate(Dry Basis)"),
                                        Value=solution.values)
                         DT::datatable(df, rownames = FALSE)
                         # DT::datatable(df)
                       })
                       
                       #optimiser textoutput
                       output$value_results<- renderUI({
                         ns <- session$ns
                         p(paste0("The objective value resulting from the optimisation is : "),objective.function.value)
                       })
                       
                     })#observeEvent run optimiser ends
                     
                     # reset button
                     observeEvent(input$reset,{
                       updateSelectInput(session,"inequality_selection",selected = "less than or equal to")
                       updateNumericInput(session,"numeric_input",value = .32)
                       updateRadioButtons(session,"radio_button",selected = "min")
                       predictors_in_model2<-c("TargetSMC_[0.287,0.37]","NaLAS(Dry Basis)_[0.13,0.41]","AlkSilicate(Dry Basis)_[0.07,0.16]",
                                               "CP5(Dry Basis)_[0,0.03]", "LSA(Dry Basis)_[0.17,0.45]",
                                               "SCMC(Dry Basis)_[0,0.01]","Sulphate(Dry Basis)_[0.17,0.52]")
                       zero_vector<-rep(1,length(predictors_in_model2))
                       min_vector <- c(0.287,0.13,0.07,0,0.17,0,0.17)
                       max_vector <- c(0.37,0.41,0.16,0.03,0.45,0.01,0.52)
                       coef_data <- data.frame(cbind(predictors_in_model2,zero_vector,min_vector,max_vector),stringsAsFactors = FALSE)
                       opt$tab_1 <- coef_data
                       opt$tab_1[[3]]<- as.numeric(opt$tab_1[[3]])
                       opt$tab_1[[4]]<- as.numeric(opt$tab_1[[4]])
                     })
                     
                     #optimisation based on category - class
                     
                     predictors_in_model22<-c("TargetSMC_[0.287,0.37]","NaLAS(Dry Basis)_[0.13,0.41]","AlkSilicate(Dry Basis)_[0.07,0.16]",
                                              "CP5(Dry Basis)_[0,0.03]", "LSA(Dry Basis)_[0.17,0.45]",
                                              "SCMC(Dry Basis)_[0,0.01]","Sulphate(Dry Basis)_[0.17,0.52]")
                     
                     zero_vector2<-rep(1,length(predictors_in_model22))
                     min_vector2 <- c(0.287,0.13,0.07,0,0.17,0,0.17)
                     max_vector2 <- c(0.37,0.41,0.16,0.03,0.45,0.01,0.52)
                     coef_data2 <- data.frame(cbind(predictors_in_model22,zero_vector2,min_vector2,max_vector2),stringsAsFactors = FALSE)
                     opt$tab_12 <- coef_data2
                     opt$tab_12[[3]]<- as.numeric(opt$tab_12[[3]])
                     opt$tab_12[[4]]<- as.numeric(opt$tab_12[[4]])
                     # View(opt$tab_1[[2]])
                     
                     #table 1
                     output$optimiser_table12<-renderDataTable({
                       DT::datatable(opt$tab_12,selection="none",editable=TRUE,colnames = c("Predictors_[Expected lower bound, Expected upper bound]","obj_coeff","Lower Bounds(editable)","Upper Bounds(editable)") )
                     })
                     
                     observeEvent(input$optimiser_table12_cell_edit,{
                       info <- input$optimiser_table12_cell_edit
                       
                       i <- info$row
                       j <- info$col
                       v <- info$value
                       if(j >= 2 && !is.na(v) && !is.na(as.numeric(v))){
                         v <- as.numeric(v)
                         if(j==2 || ( j==3 && opt$tab_12[i, j+1] > v) || (j==4 && opt$tab_12[i, j-1] < v )){
                           opt$tab_12[i,j] <<- DT::coerceValue(v,opt$tab_12[i, j])
                         }
                       }
                       rep <- opt$tab_12
                       DT::replaceData(proxy_opt2, rep, resetPaging = FALSE)
                     })
                     
                     observeEvent(input$run_optimiser2,{
                       
                       predictors_in_model2<-c("TargetSMC","NaLAS(Dry Basis)","AlkSilicate(Dry Basis)","CP5(Dry Basis)",
                                               "LSA(Dry Basis)","SCMC(Dry Basis)","Sulphate(Dry Basis)")
                       reg_coeff_in_model2<-c(1.64652727504537,-0.340054974118285,0.0349876142645199,-0.26064073764549,-0.0575389664392278,-1.17237663840093,-0.298363251134605)
                       
                       
                       number.predictors<-length(predictors_in_model2)
                       # View(opt$tab_12)#correct output
                       low.lims<-opt$tab_12[[3]]
                       upp.lims<-opt$tab_12[[4]]
                       low.lims<-as.numeric(low.lims)
                       upp.lims<-as.numeric(upp.lims)
                       # View(upp.lims)#correct output
                       objective.in<-opt$tab_12[[2]]
                       objective.in<-as.numeric(objective.in)
                       obj.type<-input$radio_button2
                       # View(objective.in)#correct output
                       
                       if(input$category_selection=="Green"){
                         lps.model <- make.lp(0,number.predictors)
                         add.constraint(lps.model,reg_coeff_in_model2,"<=",0.34)
                         add.constraint(lps.model,reg_coeff_in_model2,">=",0)
                         add.constraint(lps.model,c(0,1,1,1,1,1,1),"=",1)
                         
                         s<- as.numeric(2)
                         
                       }
                       else if(input$category_selection=="Amber"){
                         lps.model <- make.lp(0,number.predictors)
                         add.constraint(lps.model,reg_coeff_in_model2,"<=",0.44)
                         add.constraint(lps.model,reg_coeff_in_model2,">=",0.35)
                         add.constraint(lps.model,c(0,1,1,1,1,1,1),"=",1)
                         
                         s<- as.numeric(2)
                       }
                       else if(input$category_selection=="Red"){ 
                         lps.model <- make.lp(0,number.predictors)
                         add.constraint(lps.model,reg_coeff_in_model2,"<=",0.54)
                         add.constraint(lps.model,reg_coeff_in_model2,">=",0.45)
                         add.constraint(lps.model,c(0,1,1,1,1,1,1),"=",1)
                         
                         s<- as.numeric(2)
                       }
                       else {
                         lps.model <- make.lp(0,number.predictors)
                         add.constraint(lps.model,reg_coeff_in_model2,">=",0.55)
                         add.constraint(lps.model,c(0,1,1,1,1,1,1),"=",1)
                         s<- as.numeric(1)
                         
                       }
                       
                       # Bounds for variables
                       set.bounds(lps.model,lower=low.lims)
                       set.bounds(lps.model,upper=upp.lims)
                       # View(low.lims)
                       # View(nrow(low.lims))
                       # View(obj.type)
                       # View(objective.in)
                       # View(opt$tab_1)
                       
                       # Objective function
                       lp.control(lps.model,sense=obj.type) # min or max
                       # View(objective.in) #getting output
                       
                       set.objfn(lps.model,objective.in) # coefficients
                       # View(upp.lims) 
                       
                       # Apply solver
                       solution.status <- solve(lps.model)
                       # View(solution.status)
                       if(solution.status!=0){
                         showModal(modalDialog("Linear Optimisation could not find a solution for the given inputs. Please change the inputs and re-run it."))
                       }
                       
                       #unpacking
                       solution.values <- get.primal.solution(lps.model)
                       # View(solution.values)
                       objective.function.value <- solution.values[1]
                       fitted.response <- solution.values[2]
                       if(s==2){
                         solution.values <- solution.values[5:length(solution.values)]}
                       else{
                         solution.values <- solution.values[4:length(solution.values)]
                       }
                       
                       # View(solution.values) #correct output
                       results<-data.frame(Value = fitted.response)
                       row.names(results)<-"Turning Point"
                       
                       #downloading
                       downresults1 <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Turning Point"), Predicted_or_Optimal_Value= fitted.response)
                       downdf1<-data.frame(Response_or_Predictors_or_Objective_Function_Value=c("TargetSMC","NaLAS(Dry Basis)","AlkSilicate(Dry Basis)","CP5(Dry Basis)","LSA(Dry Basis)","SCMC(Dry Basis)","Sulphate(Dry Basis)"),
                                           Predicted_or_Optimal_Value=solution.values)
                       downopt1 <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Objective func. Value"), Predicted_or_Optimal_Value = objective.function.value)
                       
                       
                       final12 <- rbind(downresults1,downdf1,downopt1)
                       # View(final12)
                       optimise1(final12)
                       output$download4 <- downloadHandler(
                         filename = function() { "Linear Optimisation Turning Point (categorical).xlsx"},
                         content = function(file) {
                           write_xlsx(list("Optimisation Result" = final12), file)
                         }
                       )
                       
                       # optmiser table2
                       output$optimiser_table22<-renderDataTable({
                         DT::datatable(results) })
                       
                       # optmiser table3
                       output$optimiser_table32<- renderDataTable({
                         df<-data.frame(
                           Predictors=c("TargetSMC","NaLAS(Dry Basis)","AlkSilicate(Dry Basis)","CP5(Dry Basis)", "LSA(Dry Basis)","SCMC(Dry Basis)","Sulphate(Dry Basis)"),
                           Value=solution.values)
                         DT::datatable(df, rownames = FALSE)
                         # DT::datatable(df)
                       })
                       
                       #optimiser textoutput
                       output$value_results2<- renderUI({
                         ns <- session$ns
                         p(paste0("The objective value resulting from the optimisation is : "),objective.function.value)
                       })
                       
                     })#observeEvent run optimiser ends
                     
                     # reset button 2 
                     observeEvent(input$reset2,{
                       updateSelectInput(session,"category_selection",selected = "Green")
                       updateRadioButtons(session,"radio_button2",selected = "min")
                       predictors_in_model2<-c("TargetSMC_[0.287,0.37]","NaLAS(Dry Basis)_[0.13,0.41]","AlkSilicate(Dry Basis)_[0.07,0.16]",
                                               "CP5(Dry Basis)_[0,0.03]", "LSA(Dry Basis)_[0.17,0.45]",
                                               "SCMC(Dry Basis)_[0,0.01]","Sulphate(Dry Basis)_[0.17,0.52]")
                       zero_vector<-rep(1,length(predictors_in_model2))
                       min_vector2 <- c(0.287,0.13,0.07,0,0.17,0,0.17)
                       max_vector2 <- c(0.37,0.41,0.16,0.03,0.45,0.01,0.52)
                       coef_data <- data.frame(cbind(predictors_in_model2,zero_vector,min_vector2,max_vector2),stringsAsFactors = FALSE)
                       opt$tab_12 <- coef_data
                       opt$tab_12[[3]]<- as.numeric(opt$tab_12[[3]])
                       opt$tab_12[[4]]<- as.numeric(opt$tab_12[[4]])
                     })
                     
                   })#observeevent datacall close
      
      observeEvent(input$downloadresults,{
        
        output$Download_Values <- renderUI({
          ns <- session$ns
          downloadButton(ns("download_all"),"Download above result",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        })
        
        nrdata <- as.data.frame(manual())
        nrdata1 <- as.data.frame(manualinput())
        nrdata2 <- as.data.frame(importresults())
        nrdata3 <- as.data.frame(optimise())
        nrdata4 <- as.data.frame(optimise1())
        nrdata5 <- as.data.frame(optimise2())
        
        output$download_all <- downloadHandler(
          filename = function() { "All Results.xlsx"},
          content = function(file) {
            write_xlsx(list("Manual Input" = nrdata1,"Manual Results" = nrdata, "Import Results" = nrdata2, "Optimisation Turning Point(non categorical)" = nrdata3,
                            "Optimisation Turning Point Categorical" = nrdata4, "Optimisation Torque" = nrdata5 ), file)
          }
        )
      })
      
      #non linear optimisation for aditi - torque
      observeEvent(req(x),{
        predictor_names_torque<- c("TargetSMC_[0.287,0.37]","NaLAS(Dry Basis)_[0.13,0.41]","AlkSilicate(Dry Basis)_[0.07,0.16]",
                                   "CP5(Dry Basis)_[0,0.03]", "LSA(Dry Basis)_[0.17,0.45]",
                                   "SCMC(Dry Basis)_[0,0.01]","Sulphate(Dry Basis)_[0.17,0.52]")
        zero_vector <- rep(1,length(predictor_names_torque))
        min_vector<- c(0.287,0.13,0.07,0,0.17,0,0.17)
        max_vector <- c(0.37,0.41,0.16,0.03,0.45,0.01,0.52)
        opt$tab_2 <- data.frame(cbind(predictor_names_torque,zero_vector,min_vector,max_vector))
        opt$tab_2[[2]]<- as.numeric(opt$tab_2[[2]])
        opt$tab_2[[3]]<- as.numeric(opt$tab_2[[3]])
        opt$tab_2[[4]]<- as.numeric(opt$tab_2[[4]])
        
        observeEvent(req(input$numeric_input_adititorque),{
          if(input$numeric_input_adititorque<19 ||input$numeric_input_adititorque>36 ){
            showModal(modalDialog("Please enter Torque values between 19 and 36 for optimal results"))
          }
        })
        
        #table 1 - obj func table
        output$optimiser_table1_adititorque <- renderDataTable({
          DT::datatable(opt$tab_2,selection="none",editable=TRUE,
                        colnames = c("Predictors_[Expected lower bound, Expected upper bound]","obj_coeff","Lower Bounds(editable)","Upper Bounds(editable)"))
        })
        
        observeEvent(input$optimiser_table1_adititorque_cell_edit,{
          info <- input$optimiser_table1_adititorque_cell_edit
          
          i <- info$row
          j <- info$col
          v <- info$value
          if(j >= 2 && !is.na(v) && !is.na(as.numeric(v))){
            v <- as.numeric(v)
            if(j==2 || ( j==3 && opt$tab_2[i, j+1] > v) || (j==4 && opt$tab_2[i, j-1] < v )){
              opt$tab_2[i,j] <<- DT::coerceValue(v,opt$tab_2[i, j])
            }
          }
          rep <- opt$tab_2
          
          DT::replaceData(proxy_nlopt, rep, resetPaging = FALSE)
        })
        
        observeEvent(input$run_optimiser12,{
          
          target_adititorque <- input$numeric_input_adititorque
          inequality_selection_adititorque <- input$inequality_selection_adititorque
          
          opt$tab_2[[2]] <- as.numeric(opt$tab_2[[2]])
          
          if(inequality_selection_adititorque=="less than or equal to"){
            constr <- function(x){
              return(c(550.942517169757*x[1]+ 657.293920443309*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-178.742137567497*(x[3]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ -145.925867640988 *(x[4]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +484.822800006602*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ 205.55728325435*(x[6]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +14.7480644403947*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-2509.30663159213*x[1]*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-909.820229343898*x[1]*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-2140.43715033235*x[1]*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))-target_adititorque,
                       
                       550.942517169757*x[1]+ 657.293920443309*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-178.742137567497*(x[3]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ -145.925867640988 *(x[4]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +484.822800006602*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ 205.55728325435*(x[6]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +14.7480644403947*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-2509.30663159213*x[1]*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-909.820229343898*x[1]*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-2140.43715033235*x[1]*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))-36,
                       
                       -1*(
                         550.942517169757*x[1]+ 657.293920443309*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                         +-178.742137567497*(x[3]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ -145.925867640988 *(x[4]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                         +484.822800006602*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ 205.55728325435*(x[6]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                         +14.7480644403947*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                         +-2509.30663159213*x[1]*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                         +-909.820229343898*x[1]*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                         +-2140.43715033235*x[1]*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1])) )+19,
                       
                       x[2]+x[3]+x[4]+x[5]+x[6]+x[7] -1.000001,
                       -x[2]-x[3]-x[4]-x[5]-x[6]-x[7] + 0.999999 ))
            }
          }
          
          else if(inequality_selection_adititorque=="greater than or equal to"){
            constr <- function(x){
              return(c(-1*(550.942517169757*x[1]+ 657.293920443309*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                           +-178.742137567497*(x[3]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ -145.925867640988 *(x[4]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                           +484.822800006602*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ 205.55728325435*(x[6]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                           +14.7480644403947*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                           +-2509.30663159213*x[1]*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                           +-909.820229343898*x[1]*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                           +-2140.43715033235*x[1]*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1])))+target_adititorque,
                       
                       550.942517169757*x[1]+ 657.293920443309*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-178.742137567497*(x[3]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ -145.925867640988 *(x[4]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +484.822800006602*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ 205.55728325435*(x[6]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +14.7480644403947*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-2509.30663159213*x[1]*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-909.820229343898*x[1]*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-2140.43715033235*x[1]*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))-36,
                       
                       -1*(
                         550.942517169757*x[1]+ 657.293920443309*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                         +-178.742137567497*(x[3]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ -145.925867640988 *(x[4]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                         +484.822800006602*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ 205.55728325435*(x[6]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                         +14.7480644403947*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                         +-2509.30663159213*x[1]*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                         +-909.820229343898*x[1]*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                         +-2140.43715033235*x[1]*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1])) )+19,
                       
                       x[2]+x[3]+x[4]+x[5]+x[6]+x[7] -1.000001,
                       -x[2]-x[3]-x[4]-x[5]-x[6]-x[7] + 0.999999
                       
              ))
            }
          }
          
          else{
            constr <- function(x){
              return(c(-1*(550.942517169757*x[1]+ 657.293920443309*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                           +-178.742137567497*(x[3]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ -145.925867640988 *(x[4]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                           +484.822800006602*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ 205.55728325435*(x[6]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                           +14.7480644403947*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                           +-2509.30663159213*x[1]*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                           +-909.820229343898*x[1]*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                           +-2140.43715033235*x[1]*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1])))+(target_adititorque-0.00001),
                       
                       550.942517169757*x[1]+ 657.293920443309*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-178.742137567497*(x[3]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ -145.925867640988 *(x[4]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +484.822800006602*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ 205.55728325435*(x[6]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +14.7480644403947*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-2509.30663159213*x[1]*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-909.820229343898*x[1]*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                       +-2140.43715033235*x[1]*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))-(target_adititorque+0.00001) ,
                       
                       x[2]+x[3]+x[4]+x[5]+x[6]+x[7] -1.000001,
                       -x[2]-x[3]-x[4]-x[5]-x[6]-x[7] + 0.999999
              ))
            }
          }
          
          if(input$radio_button_adititorque=='min'){
            
            obj<-function(x){
              return(opt$tab_2[1,2]*x[1]+opt$tab_2[2,2]*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+
                       opt$tab_2[3,2]*(x[3]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                     +opt$tab_2[4,2]*(x[4]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+opt$tab_2[5,2]*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+
                       opt$tab_2[6,2]*(x[6]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+opt$tab_2[7,2]*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+
                       (opt$tab_2[1,2]*opt$tab_2[2,2])*x[1]*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                     +(opt$tab_2[1,2]*opt$tab_2[7,2])*x[1]*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+
                       (opt$tab_2[1,2]*opt$tab_2[5,2])*x[1]*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1])) )
            }
          } #if end
          else {
            obj<-function(x){
              return(-1*(opt$tab_2[1,2]*x[1]+opt$tab_2[2,2]*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+opt$tab_2[3,2]*(x[3]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                         +opt$tab_2[4,2]*(x[4]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+opt$tab_2[5,2]*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+
                           opt$tab_2[6,2]*(x[6]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+opt$tab_2[7,2]*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+
                           (opt$tab_2[1,2]*opt$tab_2[2,2])*x[1]*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                         +(opt$tab_2[1,2]*opt$tab_2[7,2])*x[1]*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+
                           (opt$tab_2[1,2]*opt$tab_2[5,2])*x[1]*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1])) ))
            }
          }
          
          x0 <- opt$tab_2[[3]]
          lb <- opt$tab_2[[3]]
          ub <- opt$tab_2[[4]]
          
          opts <- list("algorithm"="NLOPT_LN_COBYLA",
                       "xtol_rel"=1.0e-8)
          res<- nloptr(x0=x0,eval_f =  obj,
                       eval_g_ineq = constr,
                       opts = opts,
                       lb=lb, ub=ub)
          # View(sum(res$solution[-1]))
          # View(sum(res$solution))
          
          
          # View(res$objective)
          
          #this function is needed to calculate the preditcted y value
          constraint <- function(x){
            return(550.942517169757*x[1]+ 657.293920443309*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                   +-178.742137567497*(x[3]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ -145.925867640988 *(x[4]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                   +484.822800006602*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))+ 205.55728325435*(x[6]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                   +14.7480644403947*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                   +-2509.30663159213*x[1]*(x[2]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                   +-909.820229343898*x[1]*(x[7]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1]))
                   +-2140.43715033235*x[1]*(x[5]/(x[2]+x[3]+x[4]+x[5]+x[6]+x[7])*(1-x[1])))
          }
          
          if(abs(sum(res$solution[-1])-1) > .1 || constraint(res$solution)< 18 || constraint(res$solution) > 37  ){
            showModal(modalDialog("Non Linear optimisation will give unexpected results for the given inputs, 
                                  since the sum of the variables is coerced to one. Please alter the inputs and re-run. "))
          }
          
          if(input$inequality_selection_adititorque=='equal to' && abs(constraint(res$solution)-target_adititorque)>.2 ){
            showModal(modalDialog("Non Linear optimisation will give unexpected results for the given inputs, 
                                  since the sum of the variables is coerced to one. Please alter the inputs and re-run."))
            
          }
          
          # optimiser output table 1
          output$optimiser_table32_adititorque <- renderDataTable({
            df<-data.frame(Predictors = c("TargetSMC","NaLAS(Dry Basis)",	"AlkSilicate(Dry Basis)",	"CP5(Dry Basis)",
                                          "LSA(Dry Basis)",	"SCMC(Dry Basis)","Sulphate(Dry Basis)"),
                           Value = res$solution
            )
            DT::datatable(df,selection ="none",rownames = FALSE )
          })
          # View(constraint(res$solution))
          
          # optimiser output table 2
          output$optimiser_table22_adititorque <- renderDataTable({
            Value <- data.frame(Torque=as.data.frame(constraint(res$solution)) )
            DT::datatable(Value,rownames = "Torque", colnames = "Value")
          })
          
          
          # optimiser output table 3
          if(input$radio_button_adititorque=='min'){
            output$value_results_adititorque<- renderUI({
              ns <- session$ns
              p(paste0("The objective value resulting from the optimisation is : "),res$objective)
            })
          }
          else{
            output$value_results_adititorque<- renderUI({
              ns <- session$ns
              p(paste0("The objective value resulting from the optimisation is : "),-1*res$objective)
            })
            
          }
          
          downresults12 <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Torque"), Predicted_or_Optimal_Value= constraint(res$solution))
          downdf12<-data.frame(Response_or_Predictors_or_Objective_Function_Value=c("TargetSMC","NaLAS(Dry Basis)","AlkSilicate(Dry Basis)","CP5(Dry Basis)","LSA(Dry Basis)","SCMC(Dry Basis)","Sulphate(Dry Basis)"),
                               Predicted_or_Optimal_Value=res$solution)
          
          if(input$radio_button_adititorque=='min'){
            downopt12 <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Objective Function Value"), Predicted_or_Optimal_Value = res$objective)
          }
          else{
            downopt12 <- data.frame(Response_or_Predictors_or_Objective_Function_Value = c("Objective Function Value"), Predicted_or_Optimal_Value = -1*res$objective)
          }
          
          final123 <- rbind(downresults12,downdf12,downopt12)
          #View(final123)
          optimise2(final123)
          output$download5 <- downloadHandler(
            filename = function() { "Optimisation Torque .xlsx"},
            content = function(file) {
              write_xlsx(list("Optimisation Result" = final123), file)
            }
          )
          # })
          
        }) #run opt button end
        
        observeEvent(input$reset12,{
          updateSelectInput(session,"inequality_selection_adititorque",selected = "less than or equal to")
          updateNumericInput(session,"numeric_input_adititorque",value = 28)
          updateRadioButtons(session,"radio_button_adititorque",selected = "min")
          predictors_in_model2<-c("TargetSMC_[0.287,0.37]","NaLAS(Dry Basis)_[0.13,0.41]","AlkSilicate(Dry Basis)_[0.07,0.16]",
                                  "CP5(Dry Basis)_[0,0.03]", "LSA(Dry Basis)_[0.17,0.45]",
                                  "SCMC(Dry Basis)_[0,0.01]","Sulphate(Dry Basis)_[0.17,0.52]")
          zero_vector<-rep(1,length(predictors_in_model2))
          min_vector<- c(0.287,0.13,0.07,0,0.17,0,0.17)
          max_vector <- c(0.37,0.41,0.16,0.03,0.45,0.01,0.52)
          coef_data <- data.frame(cbind(predictors_in_model2,zero_vector,min_vector,max_vector),stringsAsFactors = FALSE)
          opt$tab_2 <- coef_data
          opt$tab_2[[3]]<- as.numeric(opt$tab_2[[3]])
          opt$tab_2[[4]]<- as.numeric(opt$tab_2[[4]])
        })
        
        
      })#non linear opt end
      
      
    }
    
  )}


