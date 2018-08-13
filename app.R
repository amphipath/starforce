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

# Define UI for application that draws a histogram
ui <- fluidPage(
     titlePanel("Expected Starforce Costs"),
     sidebarPanel(
     selectInput(inputId = "type",label = strong("Starforce System"),
                choices = c("KMS 25*", "GMS/MSEA 25*", "MSEA 20* (old)"),
                selected = "GMS/MSEA 25*"),
     numericInput(inputId = "lv", label = strong("Equipment Level"), value = 150, min = 0, max = 250, step=1,width = 100),
     htmlOutput('lvwarning'),
     numericInput(inputId = "r", label = HTML("Replacement Cost: <a title='Full cost in mesos, not millions or billions'>(?)</a>"), value = 0, min = 0, max = 999999999999, step=1),
     checkboxGroupInput("event", inline = TRUE, label= strong("Events:"),choiceNames = c("30% off","5/10/12 100%","5/10/15 100%"),
                        choiceValues = c("30off", "12g","15g")),
     checkboxGroupInput("starcatch", inline = TRUE, label = HTML("Star Catcher: <a title='Modelled as a 1.04x success rate, with failure/boom chances decreasing proportionally'>(?)</a>"), choiceNames = as.character(10:24), choiceValues = 10:24),
     checkboxGroupInput("protect", inline = TRUE, label = strong("Safeguard:"), choiceNames = as.character(12:16), choiceValues = 12:16),
     checkboxGroupInput("protect2", inline = TRUE, label = HTML("Post-failure safeguard: <a title='If you fail an enhancement at 14*, safeguarding the 13* replaces boom chance with a Chance Time for another shot at 13*. Not recommended at 13, but recommended at 14 and 17'>(?)</a>"),
                        choiceNames = c("13",'14','17'), choiceValues = c(13,14,17)),
     tags$hr(style = 'border: 0; height: 1px; background: #333; background-image: -webkit-linear-gradient(left, #ccc, #333, #ccc);
background-image: -moz-linear-gradient(left, #ccc, #333, #ccc); background-image: -ms-linear-gradient(left, #ccc, #333, #ccc); background-image: -o-linear-gradient(left, #ccc, #333, #ccc);'),
     tags$b("Below the Table:"),
     numericInput(inputId = "from", label = "Calculate overall cost to get from", value = 10, min = 10, max = 24, step = 1),
     numericInput(inputId = "to", label = "to", value = 17, min = 11, max = 25, step = 1),
     tags$hr(style = 'border: 0; height: 1px; background: #333; background-image: -webkit-linear-gradient(left, #ccc, #333, #ccc);
background-image: -moz-linear-gradient(left, #ccc, #333, #ccc); background-image: -ms-linear-gradient(left, #ccc, #333, #ccc); background-image: -o-linear-gradient(left, #ccc, #333, #ccc);'),
     tags$p(style='font-size:9px;color:#BBBBBB;font-family:Segoe UI','I play on MapleSEA Izar, IGN vireys. If you think this app is useful, drop me a thanks in-game!'),
     tags$a(href='starforce.pdf',style='font-size:6.5px;font-weight:bold;color:#CCCCCC',"WARNING: NERDS ONLY (formula derivation)")
     ),
     
     mainPanel(
     DT::DTOutput('table'),
     tags$hr(style = 'border: 0; height: 1px; background: #333; background-image: -webkit-linear-gradient(left, #ccc, #333, #ccc);
background-image: -moz-linear-gradient(left, #ccc, #333, #ccc); background-image: -ms-linear-gradient(left, #ccc, #333, #ccc); background-image: -o-linear-gradient(left, #ccc, #333, #ccc);'),
     htmlOutput('cost1')
     
     )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   costs <- reactive({
     lv <- floor(input$lv / 10) * 10
     if(input$type == "KMS 25*"){
       c1 <- ( 11:15 )^2.7 / 400 
       c2 <- (16:25)^2.7 / 200
       basecost <- c(c1,c2)
       basecost <- round((lv^3) * basecost,-2) + 1000
     }
     if(input$type == "GMS/MSEA 25*"){
       c1 <- (11:15) ^2.7 / 400
       c2 <- (16:18) ^2.7 / 120
       c3 <- (19:20) ^2.7 / 110
       c4 <- (21:25) ^2.7 / 100
       basecost <- c(c1,c2,c3,c4)
       basecost <- round((lv^3)*basecost,-2) + 1000
     }
     if(input$type == "MSEA 20* (old)"){
       c1 <- round(lv^3 * 11:20 / 25,-2) + 1000
       basecost <- c(c1,0,0,0,0,0)
     }
     if("30off" %in% input$event) {
       basecost <- 0.7 * basecost
     }
     basecost
   })
   successRates <- reactive({
     starCatch <- catchChoiceUpdate()
     if(input$type %in% c('KMS 25*','GMS/MSEA 25*')) {
       basePass <- 1.04 ^ starCatch * c(0.45,0.35,rep(0.3,10),0.03,0.02,0.01)
       boom <- c(0,0,0.01,0.02,0.02,rep(0.03,3),0.04,0.04,rep(0.1,2),0.2,0.3,0.4)
     }
     if(input$type == 'MSEA 20* (old)') {
       basePass <- 1.04 ^ starCatch * c(0.45,0.35,0.3,0.3,0.25,0.25,0.20,0.15,0.1,0.05,0,0,0,0,0)
       boom <- c(0,0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,rep(0,5))
     }
     if('12g' %in% input$event) {
       basePass[1] <- 1
       basePass[3] <- 1
     }
     if('15g' %in% input$event){
       basePass[1] <- 1
       basePass[6] <- 1
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
                         
   costsToNextStar <- reactive({
     baseCost <- costs()
     outcomes <- successRates()
     catch <- catchChoiceUpdate()
     protect <- protectChoiceUpdate()
     protect2 <- protect2ChoiceUpdate()
     costTable <- matrix(0,nrow=15,ncol=2,dimnames=list(as.character(10:24),c("Cost","Booms")))
     L <- function(i) {
       apply(costTable[3:(i-1),],2,sum)
     }
     yolo <- function(i) {
       outcomes$pass[i] ^ (-1) * (c(baseCost[i] + 
                                      outcomes$fail[i] * baseCost[i-1] +
                                      outcomes$fail[i] * outcomes$fail[i-1] * baseCost[i-2],0) +
                                    outcomes$fail[i] * outcomes$fail[i-1] * costTable[i-1,] + 
                                    (outcomes$fail[i] * outcomes$boom[i-1] + outcomes$boom[i]) * L(i) + 
                                    c(0,outcomes$fail[i] * outcomes$boom[i-1] + outcomes$boom[i]))
     }
     
     postProtect <- function(i) {
       outcomes$pass[i] ^ (-1) * (c(baseCost[i] + 
                                      2 * outcomes$fail[i] * baseCost[i-1] + 
                                      outcomes$fail[i] * (1 - outcomes$pass[i-1]) * baseCost[i-2],0) +
                                    outcomes$fail[i] * (1 - outcomes$pass[i-1]) * costTable[i-1,] +
                                    outcomes$boom[i] * L(i) + 
                                    c(0,outcomes$boom[i]))
     }
     
     costTable[1,1] <- baseCost[1] / outcomes$pass[1]
     costTable[2,1] <- baseCost[2] / outcomes$pass[2] + (outcomes$fail[2] / outcomes$pass[2]) * costTable[1,1]
     if(protect[3]) {
       costTable[3,1] <- outcomes$pass[3]^(-1) * (2 * baseCost[3] + 
                                                    (1 - outcomes$pass[3]) * baseCost[2] + 
                                                    (1 - outcomes$pass[3]) * outcomes$fail[2] * (baseCost[1] + costTable[2,1]))
     }
     else costTable[3,] <- outcomes$pass[3]^(-1) * c(baseCost[3] + outcomes$fail[3] * baseCost[2] + outcomes$fail[3] * outcomes$fail[2] * (baseCost[1] + costTable[2,1]),outcomes$boom[3])
     if(protect[4]) {
       if(protect2[1]){
         costTable[4,] <- outcomes$pass[4]^(-1) * (c(2 * baseCost[4] + 2 * (1 - outcomes$pass[4]) * baseCost[3] +
                                                      (1 - outcomes$pass[4]) * (1 - outcomes$pass[3]) * baseCost[2],0) + 
                                                       (1 - outcomes$pass[4]) * (1 - outcomes$pass[3]) * costTable[3,])
       }
       else {
         costTable[4,] <- outcomes$pass[4]^(-1) * (c(2 * baseCost[4] + 
                                                       (1 - outcomes$pass[4])*baseCost[3] +
                                                       (1 - outcomes$pass[4])*outcomes$fail[3] * baseCost[2],0) +
                                                     (1 - outcomes$pass[4]) * (1 - outcomes$pass[3]) * costTable[3,] + 
                                                     (1 - outcomes$pass[4]) * outcomes$boom[3] * c(0,1))
       }
     }
     else{
       if(protect2[1]) {
         costTable[4,] <- outcomes$pass[4]^(-1) * (c(baseCost[4] + 
                                                       2 * outcomes$fail[4] * baseCost[3] +
                                                       outcomes$fail[4] * (1 - outcomes$pass[3]) * baseCost[2],0) +
                                                     (outcomes$boom[4] + outcomes$fail[4] * (1 - outcomes$pass[3])) * costTable[3,] + 
                                                     c(0,(1 - outcomes$pass[4]) * outcomes$boom[3]))
       }
       else {
         costTable[4,] <- outcomes$pass[4]^(-1) * (c(baseCost[4] + 
                                                       outcomes$fail[4] * baseCost[3] + 
                                                       outcomes$fail[4] * outcomes$fail[3] * baseCost[2],0) +
                                                     (outcomes$fail[4] * (1 - outcomes$pass[3]) + outcomes$boom[4]) * costTable[3,] + 
                                                     c(0,outcomes$boom[4] + outcomes$fail[4] * outcomes$boom[3]))
       }
     }
     if(protect[5]) {
       if(protect2[2]){
         costTable[5,] <- outcomes$pass[5]^(-1) * (c(2 * baseCost[5] +
                                                       2 * (1 - outcomes$pass[5]) * baseCost[4] +
                                                   (1 - outcomes$pass[5]) * (1 - outcomes$pass[4]) * baseCost[3],0) +
                                                     (1 - outcomes$pass[5]) * (1 - outcomes$pass[4]) * costTable[4,])
       }
       else{
         costTable[5,] <- outcomes$pass[5]^(-1) * (c(2* baseCost[5] +
                                                       2 * (1 - outcomes$pass[5]) * baseCost[4] + 
                                                       (1 - outcomes$pass[5]) * outcomes$fail[4] * baseCost[3],0) + 
                                                     (1 - outcomes$pass[5]) * outcomes$boom[4] * L(5) +
                                                     (1 - outcomes$pass[5]) * outcomes$fail[4] * costTable[4,] +
                                                     c(0,(1 - outcomes$pass[5]) * outcomes$boom[4]))
       }
     }
     else{
       if(protect2[2]) {
         costTable[5,] <- postProtect(5)
       }
       else{
         costTable[5,] <- yolo(5)
       }
     }
     if(protect[6]) {
       costTable[6,1] <- 2 * baseCost[6] / outcomes$pass[6]
     }
     else{
       costTable[6,] <- outcomes$pass[6]^(-1) * (c(baseCost[6],0) + 
                                                  outcomes$boom[6] * L(6) +
                                                  c(0,outcomes$boom[6]))
     }
     if(protect[7]) {
       costTable[7,] <- outcomes$pass[7]^(-1) * (c(2 * baseCost[7],0) +
                                                   (1 - outcomes$pass[7]) * costTable[6,])
     }
     else{
       costTable[7,] <- outcomes$pass[7]^(-1) * (c(baseCost[7],0) + 
                                                   outcomes$fail[7] * costTable[6,] + 
                                                   outcomes$boom[7] * L(7) +
                                                   c(0,outcomes$boom[7]))
     }
     if(protect2[3]) {
       costTable[8,] <- postProtect(8)
     }
     else {
       costTable[8,] <- yolo(8)
     }
     for(i in 9:10) {
       costTable[i,] <- yolo(i)
     }
     costTable[11,] <- outcomes$pass[11]^(-1) * (c(baseCost[11],0) + 
                                                  outcomes$boom[11] * L(11) +
                                                  c(0,outcomes$boom[11]))
     costTable[12,] <- outcomes$pass[12]^(-1) * (c(baseCost[12],0) + 
                                                  outcomes$fail[12] * costTable[11,] + 
                                                  outcomes$boom[12] * L(12) +
                                                  c(0,outcomes$boom[12]))
     for(i in 13:15) {
       costTable[i,] <- yolo(i)
     }
     costTable
     
     
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

}



# Run the application 
shinyApp(ui = ui, server = server)