#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(RColorBrewer)
source('flamegen.R')
source('spelltrace.R')
require(echarts4r)

# Define UI for application that draws a histogram
ui <- navbarPage("Maplestory Statistics",
                 tabPanel("Starforce",
                          titlePanel("Expected Starforce Costs"),
                          sidebarPanel(
                            selectInput(inputId = "type",label = strong("Starforce System"),
                                        choices = c("KMS 25* (old)", "GMS/MSEA 25* (old)", "MSEA 20* (old)","KMS/MSEA Adventure 25*","GMS Adventure 25*"),
                                        selected = "KMS/MSEA Adventure 25*"),
                            numericInput(inputId = "lv", label = strong("Equipment Level"), value = 150, min = 0, max = 250, step=1,width = 100),
                            htmlOutput('lvwarning'),
                            numericInput(inputId = "r", label = HTML("Replacement Cost: <a title='Full cost in mesos, not millions or billions'>(?)</a>"), value = 0, min = 0, max = 999999999999, step=1),
                            checkboxGroupInput("event", inline = TRUE, label= strong("Events:"),choiceNames = c("30% off","5/10/12 100%","5/10/15 100%","10-15 no boom"),
                                               choiceValues = c("30off", "12g","15g","1015nb")),
                            radioButtons(inputId = "mvp",label = strong("MVP"),
                                         choiceNames = c("None", "Silver", "Gold","Diamond"),
                                         choiceValues = c('0','0.03','0.05','0.1')),
                            tags$br(),
                            tags$label(HTML("Star Catcher: <a title='Modelled as a 1.04x success rate, with failure/boom chances decreasing proportionally'>(?)</a>"),class = 'control-label'),
                            checkboxGroupInput("starcatch", inline = TRUE, choices = 10:24, label = NULL),
                            actionButton('allcatch', label = HTML("Always starcatch")),
                            actionButton('nocatch', label = HTML("Never starcatch")),
                            tags$br(),
                            tags$br(),
                            tags$label(strong("Safeguard:"),class = 'control-label'),
                            checkboxGroupInput("protect", inline = TRUE, choices = 12:16, label = NULL),
                            tags$label(HTML("Post-failure safeguard: <a title='If you fail an enhancement at 14*, safeguarding the 13* replaces boom chance with a Chance Time for another shot at 13*. Not recommended at 13, but recommended at 14 and 17'>(?)</a>"),class = 'control-label'),
                            checkboxGroupInput("protect2", inline = TRUE,
                                               choices = c(13,14,17), label = NULL),
                            actionButton('allsafe', label = HTML("Always safeguard")),
                            actionButton('nosafe', label = HTML("Never safeguard")),
                            tags$br(),
                            selectInput(inputId = "gap", label = strong("Guardian Angel: Polaris"),
                                        choices = c('Inactive','1.25','1.5','1.75','2'), selected = 'None'),
                            selectInput(inputId = 'gal', label = strong('Guardian Angel: Lil White Star'),
                                        choices = c('Inactive','10%','20%','40%','50%'), selected = 'Inactive'),
                            tags$hr(style = 'border: 0; height: 1px; background: #333; background-image: -webkit-linear-gradient(left, #ccc, #333, #ccc);
                                    background-image: -moz-linear-gradient(left, #ccc, #333, #ccc); background-image: -ms-linear-gradient(left, #ccc, #333, #ccc); background-image: -o-linear-gradient(left, #ccc, #333, #ccc);'),
                            tags$b("Below the Table:"),
                            numericInput(inputId = "from", label = "Calculate overall cost to get from", value = 10, min = 10, max = 24, step = 1),
                            numericInput(inputId = "to", label = "to", value = 17, min = 11, max = 25, step = 1),
                            tags$hr(style = 'border: 0; height: 1px; background: #333; background-image: -webkit-linear-gradient(left, #ccc, #333, #ccc);
                                    background-image: -moz-linear-gradient(left, #ccc, #333, #ccc); background-image: -ms-linear-gradient(left, #ccc, #333, #ccc); background-image: -o-linear-gradient(left, #ccc, #333, #ccc);'),
                            tags$p("Press this button to make a Monte Carlo simulation of starring from/to the stars above, and graph the results. Feature still in development; taking feedback on reddit; contact /u/hailcrest"),
                            actionButton("go","Generate Graphs"),
                            tags$hr(style = 'border: 0; height: 1px; background: #333; background-image: -webkit-linear-gradient(left, #ccc, #333, #ccc);
                                    background-image: -moz-linear-gradient(left, #ccc, #333, #ccc); background-image: -ms-linear-gradient(left, #ccc, #333, #ccc); background-image: -o-linear-gradient(left, #ccc, #333, #ccc);'),
                            tags$p(style='font-size:9px;color:#BBBBBB;font-family:Segoe UI','I play on MapleSEA Draco, IGN vireys. If you think this app is useful, drop me a thanks in-game!'),
                            tags$a(href='starforce.pdf',style='font-size:6.5px;font-weight:bold;color:#CCCCCC',"WARNING: NERDS ONLY (formula derivation)")
                                    ),
                          
                          mainPanel(
                            DT::DTOutput('table'),
                            tags$hr(style = 'border: 0; height: 1px; background: #333; background-image: -webkit-linear-gradient(left, #ccc, #333, #ccc);
                                    background-image: -moz-linear-gradient(left, #ccc, #333, #ccc); background-image: -ms-linear-gradient(left, #ccc, #333, #ccc); background-image: -o-linear-gradient(left, #ccc, #333, #ccc);'),
                            htmlOutput('cost1'),
                            tags$hr(style = 'border: 0; height: 1px; background: #333; background-image: -webkit-linear-gradient(left, #ccc, #333, #ccc);
                                    background-image: -moz-linear-gradient(left, #ccc, #333, #ccc); background-image: -ms-linear-gradient(left, #ccc, #333, #ccc); background-image: -o-linear-gradient(left, #ccc, #333, #ccc);'),
                            htmlOutput('cost2'),
                            plotOutput("meso"),
                            plotOutput("booms"),
                            tags$hr(style = 'border: 0; height: 1px; background: #333; background-image: -webkit-linear-gradient(left, #ccc, #333, #ccc);
                                    background-image: -moz-linear-gradient(left, #ccc, #333, #ccc); background-image: -ms-linear-gradient(left, #ccc, #333, #ccc); background-image: -o-linear-gradient(left, #ccc, #333, #ccc);')
                            
                            
                                    )
                          ),
                 tabPanel("Scrolling",
                          titlePanel("Scrolling strategies/costs"),
                          sidebarPanel(
                            HTML("<h2>Prices <a title='Enter prices on auction house'>(?)</a></h2>"),
                            numericInput(inputId = "pTrace", label = "Spell Traces (each)", value = 2000, min = 1, max = 29999999999, step=1),
                            numericInput(inputId = "pInno", label = "Innocence Scroll 50%", value = 0, min = 0, max = 29999999999, step=1),
                            numericInput(inputId = "pCSS", label = "Clean Slate Scroll 10%", value = 0, min = 0, max = 29999999999, step=1),
                            tags$hr(style = 'border: 0; height: 1px; background: #333; background-image: -webkit-linear-gradient(left, #ccc, #333, #ccc);
                                    background-image: -moz-linear-gradient(left, #ccc, #333, #ccc); background-image: -ms-linear-gradient(left, #ccc, #333, #ccc); background-image: -o-linear-gradient(left, #ccc, #333, #ccc);'),
                            selectInput(inputId = "preset",label = "Presets (auto-fill below fields)",choices = names(presets),selected = character(0)),
                            numericInput(inputId = "n", label = "# slots on equipment (base)", value = 0, min = 0, max = 25, step=1),
                            radioButtons(inputId = "scrollType",label = "Are you using scrolls or traces?",
                                         choiceNames = c("Scroll","Trace"),
                                         choiceValues = c(0,1),
                                         selected = 1),
                            numericInput(inputId = "scrollCost", label = "Spell Traces per attempt",value = 1, min = 1, max = 10000, step = 1),
                            checkboxInput(inputId = "fever", label = "Fever Time?", value = T),
                            numericInput(inputId = "p", label = "Base success chance", value= 15,min = 0,max = 100, step = 1),
                            sliderInput(inputId = 'dili', label = HTML("Diligence Level (round down)"), value = 100,min = 0, max = 100,step = 5),
                            sliderInput(inputId = 'guild', label = "Guild Skill: Master of Enhancing", value = 4, min = 0, max = 4, step = 1),
                            selectInput(inputId = 'hammer', label = 'Hammer Used',choices = list("50%","100%"),selected = "50%"),
                            checkboxInput(inputId = 'ark', label = "Ark Inno?",value = F),
                            actionButton('stcalc',label = 'Go!'),
                            tags$hr(style = 'border: 0; height: 1px; background: #333; background-image: -webkit-linear-gradient(left, #ccc, #333, #ccc);
                                    background-image: -moz-linear-gradient(left, #ccc, #333, #ccc); background-image: -ms-linear-gradient(left, #ccc, #333, #ccc); background-image: -o-linear-gradient(left, #ccc, #333, #ccc);'),
                            tags$p(style='font-size:9px;color:#BBBBBB;font-family:Segoe UI','I play on MapleSEA Draco, IGN vireys. If you think this app is useful, drop me a thanks in-game!'),
                            tags$a(href='spelltrace.pdf',style='font-size:6.5px;font-weight:bold;color:#CCCCCC',"WARNING: NERDS ONLY (formula derivation)")
                            
                          ),
                          mainPanel(
                            htmlOutput('fever'),
                            htmlOutput('cssTrace'),
                            htmlOutput('innoTrace'),
                            htmlOutput('pText'),
                            DT::DTOutput('scrolltable'),
                            htmlOutput('analysis')
                            
                          
                          )
                          ),
                 tabPanel("Flame Statistics",
                          titlePanel("Flame Distribution Plots"),
                          sidebarPanel(
                             selectInput(inputId = 'flameSource',label = 'Equip generation method', choices = list(`Monster Drop`='drop',
                                                                                                                   `Crimson Resurrection Flame` = 'crimson',
                                                                                                                   `Rainbow Resurrection Flame` = 'rainbow',
                                                                                                                   `Lv1 - 10 Crafting/Fusing` = 'smallcraft',
                                                                                                                   `Master Craftsman Crafting` = 'mccraft',
                                                                                                                   `Meister Crafting` = 'meistercraft',
                                                                                                                   `Master Craftsman Fusing` = 'mcfuse',
                                                                                                                   `Meister Fusing` = 'meisterfuse',
                                                                                                                   `Boss Fragment Chance Time` = 'chancetime'),selected='rainbow'),
                             checkboxInput(inputId = 'flameBoss', label = 'Does the equip have strong flames?', value=T),
                             numericInput(inputId = 'flameLevel', label = 'Equip level', value = 160, min=100,max=200,step=1),
                             selectInput(inputId = 'flameMainstat', label = 'Main Stat', choices = list(STR=1,DEX=2,INT=3,LUK=4,HP = 9,`STR+DEX+LUK` = -1)),
                             numericInput(inputId = 'flameAsweight', label = 'All Stats % weight', value = 10, min=0),
                             numericInput(inputId = 'flameAttweight', label = 'ATT/MATT weight', value = 4, min=0),
                             numericInput(inputId = 'flameThreshold', label = 'Desired Flame Score threshold', value = 100, min=0),
                             actionButton('flameGo',label = 'Go!')
                             
                             
                          ),
                          mainPanel(
                             echarts4rOutput('flamePlot')
                          )
                     )
                 )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #######################################################
  ## START OF FLAME SECTION
  #######################################################
   observeEvent(input$flameGo, {
      level <- input$flameLevel
      tierweights <- tier.weights[[input$flameSource]]
      boss <- input$flameBoss
      n <- 20000
      df <- data.frame(simulateflame(level,n,boss,tierweights=tierweights))
      colnames(df) <- c('STR','DEX','INT','LUK', '%AS', 'ATT','MA','DEF','HP','MP','SPD','JUMP','Lv Reduction')
      df$Score <- scoreoutput(df,as.numeric(input$flameMainstat),input$flameAsweight,input$flameAttweight)
      ordered <- df$Score[order(df$Score)]
      print(ordered)
      index <- (1:nrow(df)) / nrow(df)
      q99 <- ordered[n*0.99 + 1]
      q995 <- ordered[n*0.995 + 1]
      q999 <- ordered[n*0.999 + 1]
      q90 <- ordered[n*0.9+1]
      cdfdf <- data.frame(Score=ordered,Quantile=index,q99=0,q995=0,q999=0,q90=0)
      flameThresholdProb <- sum(ordered > input$flameThreshold)/length(ordered)
      meanFlames <- 1/flameThresholdProb
      
      
      output$flamePlot <- renderEcharts4r({
         cdfdf %>%
            e_charts(Quantile) %>%
            e_line(Score,symbol='circle') %>%
            e_title('Flame Score Quantiles','Like cumulative density but with swapped axes')%>%
            e_visual_map(Score,show=F,type='piecewise',inRange=list(color=c('rgb(0,123,198)','rgb(194,53,49)')))%>%
            e_mark_line('Score',data=list(name='',xAxis=1-flameThresholdProb),label=list(overflow='break',width=100),title=paste0("Avg ",format(meanFlames,digits=2),' attempts\nto get threshold'),lineStyle=list(color='#aaa'))%>%
            e_mark_line('Score',data=list(name='90th Percentile',yAxis=q90),lineStyle=list(color='rgb(194,53,49)')) %>%
            e_mark_line('Score',data=list(name='99th Percentile',yAxis=q99),lineStyle=list(color='rgb(183,50,47)')) %>%
            e_mark_line('Score',data=list(name='99.5th Percentile',yAxis=q995),lineStyle=list(color='rgb(149,41,38)')) %>%
            e_mark_line('Score',data=list(name='99.9th Percentile',yAxis=q999),lineStyle=list(color='rgb(128,34,32)')) %>%
            e_legend(F) %>%
            e_tooltip(trigger='item')
      })
      
   })

  
  #######################################################
  ## START OF STARFORCE SECTION
  #######################################################
  
    observeEvent(input$allcatch, {
    updateCheckboxGroupInput(session,"starcatch", inline = TRUE, label = NULL, choices = 10:24, selected = 10:24)
  })
  
  observeEvent(input$nocatch, {
    updateCheckboxGroupInput(session,"starcatch", inline = TRUE, label = NULL, choices = 10:24)
  })
  
  observeEvent(input$nosafe, {
    updateCheckboxGroupInput(session,"protect", inline = TRUE, label = NULL, choices = 12:16)
    updateCheckboxGroupInput(session,"protect2", inline = TRUE, label = NULL,
                             choices = c(13,14,17))
  })
  
  observeEvent(input$allsafe, {
    updateCheckboxGroupInput(session,"protect", inline = TRUE, label = NULL, choices = 12:16, selected = 12:16)
    updateCheckboxGroupInput(session,"protect2", inline = TRUE, label = NULL,
                             choices = c(13,14,17), selected = c(13,14,17))
  })
  
   costs <- reactive({
     lv <- floor(input$lv / 10) * 10
     if(input$type %in% c("KMS 25* (old)","KMS/MSEA Adventure 25*")){
       c1 <- ( 11:15 )^2.7 / 400 
       c2 <- (16:25)^2.7 / 200
       basecost <- c(c1,c2)
       basecost <- round((lv^3) * basecost,-2) + 1000
     }
     if(input$type %in% c("GMS/MSEA 25* (old)","GMS Adventure 25*")){
       c1 <- (11:15) ^2.7 / 400
       c2 <- (16:18) ^2.7 / 120
       c3 <- (19:20) ^2.7 / 110
       c4 <- (21:25) ^2.7 / 100
       basecost <- c(c1,c2,c3,c4)
       basecost <- round((lv^3)*basecost,-2) + 1000
       if("test" %in% input$event){
         basecost <- rep(1000000,15)
       }
     }
     if(input$type == "MSEA 20* (old)"){
       c1 <- round(lv^3 * 11:20 / 25,-2) + 1000
       basecost <- c(c1,0,0,0,0,0)
     }
     basecost
   })
   successRates <- reactive({
     starCatch <- catchChoiceUpdate()
     if(input$type %in% c('KMS 25* (old)','GMS/MSEA 25* (old)')) {
       basePass <- 1.05 ^ starCatch * c(0.45,0.35,rep(0.3,10),0.03,0.02,0.01)
       boom <- c(0,0,0.01,0.02,0.02,rep(0.03,3),0.04,0.04,rep(0.1,2),0.2,0.3,0.4)
     }
     if(input$type == 'MSEA 20* (old)') {
       basePass <- 1.05 ^ starCatch * c(0.45,0.35,0.3,0.3,0.25,0.25,0.20,0.15,0.1,0.05,0,0,0,0,0)
       boom <- c(0,0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,rep(0,5))
     }
     if(input$type %in% c("KMS/MSEA Adventure 25*","GMS Adventure 25*")) {
       basePass <- 1.05 ^ starCatch * c(0.5, 0.45, 0.4, 0.35, 0.3, rep(0.3,7),0.03,0.02,0.01) 
       boom <- c(0,0,0.01,0.02,0.02,rep(0.03,3),0.04,0.04,rep(0.1,2),0.2,0.3,0.4)
     }
     if('12g' %in% input$event) {
       basePass[1] <- 1
       basePass[3] <- 1
     }
     if('15g' %in% input$event){
       basePass[1] <- 1
       basePass[6] <- 1
     }
     if('1015nb' %in% input$event) {
       boom[1:5] <- 0
     }
     if(input$gal != 'Inactive'){
        factor = 1 - (as.numeric(substr(input$gal,1,2))/100)
        boom <- factor * boom
     }
     if(input$gap != 'Inactive'){
        factor = as.numeric(input$gap)
        basePass = pmin(1,basePass*factor)
     }
     result <- data.frame(pass = basePass, fail = (1 - boom)*(1 - basePass), boom = boom*(1-basePass))
    result
   })
   catchChoiceUpdate <- reactive({
     10:24 %in% input$starcatch
   })
   protectChoiceUpdate <- reactive({
     10:24 %in% input$protect
   })
   protect2ChoiceUpdate <- reactive({
     c(13,14,17) %in% input$protect2
   })
   thirtyoffUpdate <- reactive({
     "30off" %in% input$event
   })
   mvpUpdate <- reactive({
     as.numeric(input$mvp) * c(1,1,1,1,1,0,0,0,0,0,0,0,0,0,0)
   })
                         
   costsToNextStar <- reactive({
     baserCost <- costs()
     off30 <- thirtyoffUpdate()
     MVP <- mvpUpdate()
     baseCost <- baserCost - off30 * 0.3 * baserCost - MVP * baserCost
     protCost <- baserCost + baseCost
     outcomes <- successRates()
     catch <- catchChoiceUpdate()
     protect <- protectChoiceUpdate()
     protect2 <- protect2ChoiceUpdate()
     sfTable(baserCost,off30,MVP,baseCost,protCost,outcomes,catch,protect,protect2)
   })
   

   output$table <- DT::renderDT({
     baseCost <- costs()
     succRate <- successRates()$pass
     failRate <- successRates()$fail
     destroyRate <- successRates()$boom
     nt <- costsToNextStar()
     mat <- data.frame(AttemptCost = baseCost,
                                  SuccessRate = 100 * succRate,
                                  FailRate = 100 * failRate,
                                  DestroyRate = 100* destroyRate,
                       nextCost = round(nt[,1] / 1000000,2),
                       nextBoom = round(nt[,2],2))
     
     
     dimnames(mat) <- list(10:24,c("Base Attempt Cost",
                                   "Success %",
                                   "Fail %",
                                   "Destroy %",
                                   "Expected Meso to Next Star (mil)",
                                   "Expected Booms to Next Star"))
     
     if(input$type == 'MSEA 20* (old)') {
       mat <- mat[1:10,]
     }
     
     DT::datatable(mat, escape = FALSE, selection = 'none',  options = list(
       dom = 't',
                                      server = FALSE,
                                      searching = FALSE,
                                       paging = FALSE,
                                       ordering = FALSE,
                                      fixedHeader.footer = FALSE
                                     )
                                      
     )
     })
   output$lvwarning <- renderUI({
     if(input$lv < 140 & input$lv > 129) { HTML("<font color=#FF0000>Warning: Level 130-139 equipment can be enhanced only to a max of 20*</color>")} 
     else if(input$lv < 130 & input$lv > 119) { HTML("<font color=#FF0000>Warning: Level 120-129 equipment can be enhanced only to a max of 15*</color>")} 
     else if(input$lv < 120) { HTML("<font color=#FF0000>Warning: Equipment of level less than 120 cannot be enhanced past 10*. This tool is inapplicable.</color>")} 
     else {NULL}
   })
   
   output$cost1 <- renderUI({
     nt <- costsToNextStar()
     k <- input$from - 9
     l <- input$to - 10
     if(input$type == 'MSEA 20* (old)' & (input$from > 20 | input$to > 20)) {
       HTML("<b style='color:#FF0000'>Error: invalid choices for from/to</b>")
     }
     else if(l >= k & k > 0){
       if(l == k){
         v <- nt[k,]
       }
       else {
         v <- apply(nt[k:l,],2,sum)
       }
       meso <- round(v[1] / 1000000000,3)
       str1 <- paste0('From ',input$from,'* to ',input$to,'*:')
       str2 <- 'Total meso cost (billions, w/o replacement cost)'
       str3 <- 'Total booms'
       totcost <- round(((input$r * v[2]) + v[1])/1000000000,3)
       str4 <- 'Overall cost (billions)'
       HTML(sprintf("<table style='border:none'>
                    <tr>
                    <td>%s</td>
                    <td></td>
                    </tr>
                    <tr>
                    <td style='padding-right:4px'>%s</td>
                    <td>%s</td>
                    </tr>
                    <tr>
                    <td>%s</td>
                    <td>%s</td>
                    </tr>
                    <tr>
                    <td>%s</td>
                    <td>%s</td>
                    </tr>
                    </table>",str1,str2,meso,str3,round(v[2],3),str4,totcost))
     }
     else {
       HTML("<b style='color:#FF0000'>Error: invalid choices for from/to</b>")
     }

   })
   
   output$cost2 <- NULL
   output$booms <- NULL
   output$meso <- NULL
   
   genMatrix <- reactive({
     #produces a cost-probability matrix for use in simulation
     #outputs a 30-row data table with columns:
        #star     - current star
        #state    - 1 if we have just decreased 1 star in the previous enhancement attempt
        #cost     - cost of enhancing (doubled for anti-destruction)
        #success  - chance of success, x1.04 if star catcher was played
        #maintain - chance of maintaining (nonzero only at 10, 15, 20)
        #fail     - chance of dropping a star
        #boom     - chance of destroying equipment
     baseCost <- costs()
     outcomes <- successRates()
     catch <- catchChoiceUpdate()
     protect <- protectChoiceUpdate()
     protect2 <- protect2ChoiceUpdate()
     protectvector <- rep(protect,each=2)
     protectvector[5] <- protect2[1]
     protectvector[7] <- protect2[2]
     protectvector[13] <- protect2[3]
     finalCost <- rep(baseCost,each=2) * (2^protectvector)
     boom <- round(outcomes$boom / (outcomes$boom + outcomes$fail),2)
     boom <- rep(boom,each=2)
     boom[c(6,8,10,12,14)] <- boom[c(6,8,10,12,14)] * (1 - protect[3:7])
     boom[5] <- boom[5]*(1 - protect2[1])
     boom[7] <- boom[7]*(1 - protect2[2])
     boom[9] <- boom[10]
     boom[11] <- boom[12]
     boom[13] <- boom[13]*(1 - protect2[3])
     success <- rep(outcomes$pass,each=2)
     fail <- (1-boom)*(1-success)
     maintain <- c(fail[1:2],numeric(8),fail[11:12],numeric(8),fail[21:22],numeric(8))
     fail[c(1,2,11,12,21,22)] <- 0
     destroy <- boom*(1-success)
     outputMatrix <- data.frame(star = rep(10:24,each=2),state = rep(1:0,15), cost = finalCost, success = success, maintain = maintain, fail = fail, boom = destroy)
     outputMatrix
   })
   
   
   observeEvent(input$go, {
     n <- 250
     loadingmessage <- paste("Simulating",n,"trials...")
     if(input$to %in% c(20,21,22)) {
       n <- 50
       loadingmessage <- paste("Simulating small sample due to high star count...")
     }
     if(input$to %in% c(23,24,25)) {
       n <- 5
       loadingmessage <- paste("Warning: 23* and above takes very long. Simulating very small sample size.")
     }
     costProbMatrix <- genMatrix()
     baseCost <- costs()
     simulation <- data.frame(cost = numeric(n),booms = numeric(n))
     rng <- sample(1:100000,10000,replace=TRUE)
     j <- 1
     withProgress(message = loadingmessage, value = 0, {
       for(i in 1:n) {
         state <- c(input$from,0,0,0)
         while(state[1] != input$to) {
           state <- Star(state,costProbMatrix,baseCost,rng[j])
           j <- j+1
           if(j > 10000) {
             j <- 1
             rng <- sample(1:100000, 10000, replace=TRUE)
           }
         }
         simulation[i,] <- state[3:4]
         incProgress(1/n,detail = paste0(i,"/",n))
       }
     })
     
     pcts <- c(0,25, 50, 90, 100)
     pcttext <- character(length(pcts)-1)
     for(i in 1:length(pcttext)) {pcttext[i] <- paste0(pcts[i]," - ",pcts[i+1])}
     pctdec <- pcts / 100
     
     simulation$overall <- simulation$cost + simulation$booms * input$r
     mesocost <- simulation$cost[order(simulation$cost)] / 1000000
     overallcost <- simulation$overall[order(simulation$overall)] / 1000000
     mesoquants <- quantile(mesocost, prob = pctdec)
     quantlabels <- paste0(rep(c('\n',''),length(pcts))[1:length(mesoquants)],as.numeric(mesoquants))
     mesodens <- density(mesocost)
     mesodf <- data.frame(x = mesodens$x, y = mesodens$y, quant = factor(findInterval(mesodens$x,mesoquants)))
     mesodf <- mesodf[as.character(mesodf$quant) != as.character(0),]
     mesodf <- mesodf[as.character(mesodf$quant) != as.character(length(pcts)),]
     for(i in 1:length(mesocost)) {
       if(sum(mesocost[1:i]) > 0.5 * sum(mesocost)) {
         mesohalf <- length(mesocost) - i + 1
         mesohalf <- round(mesohalf / length(mesocost) * 100,1)
         break
       }
     }
     
     for(i in 1:length(overallcost)) {
       if(sum(overallcost[1:i]) > 0.5 * sum(overallcost)) {
         overallhalf <- length(overallcost) - i + 1
         overallhalf <- round(overallhalf / length(overallcost) * 100, 1)
         break
       }
     }
     output$meso <- renderPlot({
       ggplot(mesodf, aes(x,y)) + 
         geom_line() + 
         geom_ribbon(aes(ymin=0,ymax=y,fill=quant)) + 
         scale_x_continuous(breaks = as.numeric(mesoquants)) +
         theme(axis.text.y = element_blank(),
               axis.title.y = element_blank(),
               axis.text.x = element_text(angle=90,hjust=1)) +
         scale_fill_brewer(name = '%ile brackets', guide = 'legend', labels = pcttext) +
         labs(x = "Mesos spent (millions)", title = "Density plot of costs w/o replacement")
     })
     
     output$booms <- renderPlot({
       ggplot(simulation,aes(x = booms)) + geom_histogram(binwidth = 0.8, aes(y = ..prop..), stat = "count") +
         scale_y_continuous(labels = scales::percent) + 
         labs(x = "Number of booms", y = "", title = "Frequency of number of booms in simulation")
     })
     
     output$cost2 <- renderUI({
       HTML(sprintf('<font color=#cc4b4b>Warning:</font> The following are results of a Monte Carlo (i.e. trial and error) simulation of %d trials. Results are subject to random variance, but hopefully not too much. <br><br>
                    In this simulation, 50%% of meso was used to star the most expensive %.1f%% of equipment. Taking into account replacement costs, this figure becomes %.1f%%.',n,mesohalf,overallhalf))
     })

     
   })
   
   #######################################################
   ## START OF SPELLTRACE SECTION
   #######################################################
   
   observeEvent(input$scrollType,{
     if(input$scrollType == 1) {
       updateNumericInput(session,inputId = 'scrollCost',label = 'Spell Traces per attempt',value = 1,min = 1,max = 10000)

     }
     else {
       updateNumericInput(session,inputId = 'scrollCost',label = 'Meso cost per Scroll', value = 1, min = 1, max = 29999999999)

     }
   })
   
   observeEvent(input$preset,{
     list <- presets[[as.character(input$preset)]]
     updateNumericInput(session,inputId='n',label = "# slots on equipment (base)",value = list$n,max = 25,min = 0,step = 1)
     updateRadioButtons(session, inputId = 'scrollType',label = "Are you using scrolls or traces?", choiceNames = c("Scroll","Trace"), choiceValues = c(0,1), selected = 1)
     updateNumericInput(session, inputId = 'scrollCost', label = "Spell Traces per attempt", value= list$cost, max = 10000, min = 1, step = 1)
     updateNumericInput(session, inputId = 'p', label = 'Base Success Chance (%)', value = 100*list$p, min = 1, max = 100)
     updateSliderInput(session, inputId = 'dili', label = HTML("Diligence Level (round down)"), value = 100,min = 0, max = 100,step = 5)
     updateSliderInput(session,inputId = 'guild', label = "Guild Skill: Master of Enhancing", value = 4, min = 0, max = 4, step = 1)
     updateCheckboxInput(session,inputId = "fever", label = "Fever Time?", value = T)
     updateCheckboxInput(session,inputId = 'ark', label = "Ark Inno?",value = F)
   })
   
   observeEvent(input$stcalc,{
     ## Gather parameters
     if(input$fever == T){
       succInno <- 0.45
       succCSS <- 0.1
       if(input$scrollType == 1) {
         p <- feverSub(input$p) / 100
       } else{
         p <- input$p / 100
       }
       output$fever <- renderUI({
         HTML('Fever time active.')
       })
     } else {
       p <- input$p / 100
       succInno <- 0.3
       succCSS <- 0.05
       output$fever <- renderUI({
       HTML('Fever time not selected.')
      })
     }
     
     if(input$pCSS == 0){
       cssTrace <- T
       pCSS <- input$pTrace * 2000
       output$cssTrace <- renderUI({
         HTML("Market price of Clean Slate Scroll 10%s not given, will assume they are unused.")
       })
     }
     else {
       if(input$pCSS / 0.1 < input$pTrace * 2000 / succCSS){
         cssTrace <- F
         pCSS <- input$pCSS
         output$cssTrace <- renderUI({
           HTML("It's cheaper in the long run to use Clean Slate Scroll 10%s than traces for recovering slots, so using those.")
         })
       }
       else {
         cssTrace <- T
         pCSS <- input$pTrace * 2000
         output$cssTrace <- renderUI({
           HTML("It's cheaper in the long run to use traces than Clean Slate Scroll 10%s for recovering slots, so using those.")
         })
       }
     }
     
     if(input$ark){
       innoTrace <- T
       pInno <- input$pTrace * 10000
       output$innoTrace <- renderUI({
         HTML("Using Ark Innocence Scrolls, so using traces.")
       })
     } else {
       if(input$pInno == 0) {
         innoTrace <- T
         pInno <- input$pTrace * 5000
         output$innoTrace <- renderUI({
           HTML("Market price of Innocence Scroll 50%s not given, assume they are unused.")
         })
       } else {
         if(input$pInno / 0.5 < input$pTrace * 5000 / succInno) {
           innoTrace <- F
           succInno <- 0.5
           pInno <- input$pInno
           output$innoTrace <- renderUI({
               HTML("It's cheaper in the long run to use Innocence Scroll 50%s rather than traces, so using those.")
           })
         } else {
           innoTrace <- T
           pInno <- input$pTrace * 5000
           output$innoTrace <- renderUI({
               HTML("It's cheaper in the long run to use traces than Innocence Scroll 50%s, so using those.")
           })
         }
       }
     }
     
     if(input$hammer == "50%") {
       succHam <- 0.5
     } else {
       succHam <- 1
     }
     
     finalp <- p + input$dili * 0.001 + input$guild * 0.01
     finalp <- min(finalp,1)
     output$pText <- renderUI({
       HTML(sprintf('Calculated success chance per scroll is <b>%.1f%%</b> after taking Fever, Diligence and Guild Skills into account.',finalp*100))
     })
     finaln <- input$n
     
     ## Create table of costs
     varTable <- sdTable <- meanTable <- data.frame(f = 1:finaln,Scrolls = numeric(finaln),Hammers= numeric(finaln),Innos = numeric(finaln),CSS = numeric(finaln))
     for(f in 1:finaln){
       meanTable[f,2:5] <- scrollStats(finaln,f,finalp,succHam,succCSS,succInno)$tMean
       sdTable[f,2:5] <- scrollStats(finaln,f,finalp,succHam,succCSS,succInno)$tSd
       varTable[f,2:5] <- scrollStats(finaln,f,finalp,succHam,succCSS,succInno)$tVar
     }
     meanTable$sts <- (meanTable$Scrolls * input$scrollCost * as.numeric(input$scrollType) + meanTable$Innos * as.numeric(innoTrace) * (5000 + as.numeric(input$ark) * 5000) + meanTable$CSS * as.numeric(cssTrace) * 2000)/1000
     meanTable$total <- (meanTable$Scrolls * input$scrollCost * (input$pTrace^as.numeric(input$scrollType)) + meanTable$Innos * pInno + meanTable$CSS * pCSS) / 1000000
     mincost <- match(min(meanTable$total),meanTable$total)
     
     colnames(meanTable) <- c("# Failures before Inno", "Avg # Scroll Attempts", "Avg Hammers Used", "Avg Innos Used","Avg CSS Used","Mean traces used (thousands)","Mean total cost (millions)")
     
     output$scrolltable <- DT::renderDT({

       
       scrolltable <- DT::datatable(meanTable, escape = FALSE, selection = 'none', rownames = F, options = list(
         dom = 't',
         server = FALSE,
         searching = FALSE,
         paging = FALSE,
         ordering = FALSE,
         fixedHeader.footer = FALSE
       )) %>%
         formatStyle('# Failures before Inno',target = 'row',backgroundColor = styleEqual(mincost,'#b4ecb4'), fontWeight = styleEqual(mincost,'bold')) %>%
       formatRound(columns =  c("Avg # Scroll Attempts", "Avg Hammers Used", "Avg Innos Used","Avg CSS Used","Mean traces used (thousands)","Mean total cost (millions)"), digits = 2)
       
     })
     
     output$analysis <- renderUI({
       HTML(sprintf('<br><br>The optimal strategy determined is to stop and Innocence the equip after <b>%i slots fail</b>. This is the same as going for <b>%i passes</b> including the +1 slot given by the hammer before clean-slating.<br><br>
                    Note that because scrolling depends heavily on the geometric distribution, the distribution will be top-heavy; most people, about two-thirds will turn out to use less than the expected cost, but the people who get unlucky get REALLY unlucky, taking as much as 1.5x or even more.',mincost,finaln + 2 - mincost))
     })
   })
   

}



# Run the application 
shinyApp(ui = ui, server = server)