## SF Calculation backend

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
    basePass <- 1.04 ^ starCatch * c(0.45,0.35,rep(0.3,10),0.03,0.02,0.01)
    boom <- c(0,0,0.01,0.02,0.02,rep(0.03,3),0.04,0.04,rep(0.1,2),0.2,0.3,0.4)
  }
  if(input$type == 'MSEA 20* (old)') {
    basePass <- 1.04 ^ starCatch * c(0.45,0.35,0.3,0.3,0.25,0.25,0.20,0.15,0.1,0.05,0,0,0,0,0)
    boom <- c(0,0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,rep(0,5))
  }
  if(input$type %in% c("KMS/MSEA Adventure 25*","GMS Adventure 25*")) {
    basePass <- 1.04 ^ starCatch * c(0.5, 0.45, 0.4, 0.35, 0.3, rep(0.3,7),0.03,0.02,0.01) 
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
                                   outcomes$fail[i] * protCost[i-1] + 
                                   outcomes$fail[i] * (1 - outcomes$pass[i-1]) * baseCost[i-2],0) +
                                 outcomes$fail[i] * (1 - outcomes$pass[i-1]) * costTable[i-1,] +
                                 outcomes$boom[i] * L(i) + 
                                 c(0,outcomes$boom[i]))
  }
  
  costTable[1,1] <- baseCost[1] / outcomes$pass[1]
  costTable[2,1] <- baseCost[2] / outcomes$pass[2] + (outcomes$fail[2] / outcomes$pass[2]) * costTable[1,1]
  if(protect[3]) {
    costTable[3,1] <- outcomes$pass[3]^(-1) * (protCost[3] + 
                                                 (1 - outcomes$pass[3]) * baseCost[2] + 
                                                 (1 - outcomes$pass[3]) * outcomes$fail[2] * (baseCost[1] + costTable[2,1]))
  }
  else costTable[3,] <- outcomes$pass[3]^(-1) * c(baseCost[3] + outcomes$fail[3] * baseCost[2] + outcomes$fail[3] * outcomes$fail[2] * (baseCost[1] + costTable[2,1]),outcomes$boom[3])
  if(protect[4]) {
    if(protect2[1]){
      costTable[4,] <- outcomes$pass[4]^(-1) * (c(protCost[4] + (1 - outcomes$pass[4]) * protCost[3] +
                                                    (1 - outcomes$pass[4]) * (1 - outcomes$pass[3]) * baseCost[2],0) + 
                                                  (1 - outcomes$pass[4]) * (1 - outcomes$pass[3]) * costTable[3,])
    }
    else {
      costTable[4,] <- outcomes$pass[4]^(-1) * (c(protCost[4] + 
                                                    (1 - outcomes$pass[4])*baseCost[3] +
                                                    (1 - outcomes$pass[4])*outcomes$fail[3] * baseCost[2],0) +
                                                  (1 - outcomes$pass[4]) * (1 - outcomes$pass[3]) * costTable[3,] + 
                                                  (1 - outcomes$pass[4]) * outcomes$boom[3] * c(0,1))
    }
  }
  else{
    if(protect2[1]) {
      costTable[4,] <- outcomes$pass[4]^(-1) * (c(baseCost[4] + 
                                                    outcomes$fail[4] * protCost[3] +
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
      costTable[5,] <- outcomes$pass[5]^(-1) * (c(protCost[5] +
                                                    (1 - outcomes$pass[5]) * protCost[4] +
                                                    (1 - outcomes$pass[5]) * (1 - outcomes$pass[4]) * baseCost[3],0) +
                                                  (1 - outcomes$pass[5]) * (1 - outcomes$pass[4]) * costTable[4,])
    }
    else{
      costTable[5,] <- outcomes$pass[5]^(-1) * (c(protCost[5] +
                                                    (1 - outcomes$pass[5]) * baseCost[4] + 
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
    costTable[6,1] <- protCost[6] / outcomes$pass[6]
  }
  else{
    costTable[6,] <- outcomes$pass[6]^(-1) * (c(baseCost[6],0) + 
                                                outcomes$boom[6] * L(6) +
                                                c(0,outcomes$boom[6]))
  }
  if(protect[7]) {
    costTable[7,] <- outcomes$pass[7]^(-1) * (c(protCost[7],0) +
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
  pboom <- numeric(15)
  pboom[1:2] <- 1
  pboom[3] <- outcomes$pass[3] / (1 - (outcomes$fail[3] + protect[3] * outcomes$boom[3]) * (outcomes$pass[2] + (outcomes$fail[2])*pboom[2]))
  for(i in 4:5) {
    pboom[i] <- outcomes$pass[i] / (1 - (outcomes$fail[i] + protect[i] * outcomes$boom[i]) * (outcomes$pass[i-1] + (outcomes$fail[i-1] + protect2[i-3] * outcomes$boom[i-1]) * pboom[i-1]))
  }
  pboom[6] <- outcomes$pass[6] / (1 - outcomes$fail[6] + protect[6] * outcomes$boom[6])
  pboom[7] <- outcomes$pass[7] / (1 - (outcomes$fail[7] + protect[7] * outcomes$boom[7]) * pboom[6])
  pboom[8] <- outcomes$pass[8] / (1 - (outcomes$fail[8]) * (outcomes$pass[7] + (outcomes$fail[7] + protect2[3] * outcomes$boom[7]) * pboom[7]))
  for(i in 9:10) {
    pboom[i] <- outcomes$pass[i] / (1 - (outcomes$fail[i]) * (outcomes$pass[i-1] + (outcomes$fail[i-1]) * pboom[i-1]))
  }
  pboom[11] <- outcomes$pass[11] / (1 - outcomes$fail[11])
  pboom[12] <- outcomes$pass[12] / (1 - outcomes$fail[12] * pboom[11])
  for(i in 13:15) {
    pboom[i] <- outcomes$pass[i] / (1 - (outcomes$fail[i]) * (outcomes$pass[i-1] + (outcomes$fail[i-1]) * pboom[i-1]))
  }
  costTable$pboom <- pboom
  costTable
  
  
})

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











## Simulation things

testStar <- function(state,costProbMatrix,baseCost,i) {
  #function that takes a state (a 4-length vector: 1st entry is current star, 2nd entry is 1 if previous attempt dropped a star and 0 otherwise, 3rd entry is current mesos spent, 4th entry is current number of booms)
  #and a costProbmatrix which is generated by genMatrix()
  #and outputs another state after attempting a star.
  #baseCost is there to provide a base cost of starring that is unchanged by safeguarding (for chance time purposes)
  index <- match(paste0(state[1],state[2]),paste0(costProbMatrix$star,costProbMatrix$state))
  output <- state
  probs <- round(costProbMatrix[index,4:7],5) * 100000
  probs[4] <- sum(probs)
  probs[3] <- sum(probs[1:3])
  probs[2] <- sum(probs[1:2])
  outcome <- i
  if(outcome <= probs[1]) {
    output[2] <- 0
    output <- output + c(1,0,costProbMatrix[index,3],0,1)
  }
  else if(outcome <= probs[2]) {
    output[2] <- 0
    output[3] <- output[3] + costProbMatrix[index,3]
    output[5] <- output[5] + 1
  }
  else if(outcome <= probs[3] & state[2] == 1) {
    output[2] <- 0
    output[3] <- output[3] + costProbMatrix[index,3] + baseCost[state[1] - 10]
    output[5] <- output[5] + 1
  }
  else if(outcome <= probs[3] & state[2] == 0) {
    output <- output + c(-1,1,costProbMatrix[index,3],0,1)
  }
  else {
    output <- c(12,0,output[3] + costProbMatrix[index,3],output[4] + 1,output[5]+1)
  }
  output
}

Star <- function(state,costProbMatrix,baseCost,i) {
  #function that takes a state (a 4-length vector: 1st entry is current star, 2nd entry is 1 if previous attempt dropped a star and 0 otherwise, 3rd entry is current mesos spent, 4th entry is current number of booms)
  #and a costProbmatrix which is generated by genMatrix()
  #and outputs another state after attempting a star.
  #baseCost is there to provide a base cost of starring that is unchanged by safeguarding (for chance time purposes)
  index <- match(paste0(state[1],state[2]),paste0(costProbMatrix$star,costProbMatrix$state))
  output <- state
  probs <- round(costProbMatrix[index,4:7],5) * 100000
  probs[4] <- sum(probs)
  probs[3] <- sum(probs[1:3])
  probs[2] <- sum(probs[1:2])
  outcome <- i
  if(outcome <= probs[1]) {
    output[2] <- 0
    output <- output + c(1,0,costProbMatrix[index,3],0)
  }
  else if(outcome <= probs[2]) {
    output[2] <- 0
    output[3] <- output[3] + costProbMatrix[index,3]
  }
  else if(outcome <= probs[3] & state[2] == 1) {
    output[2] <- 0
    output[3] <- output[3] + costProbMatrix[index,3] + baseCost[state[1] - 10]
  }
  else if(outcome <= probs[3] & state[2] == 0) {
    output <- output + c(-1,1,costProbMatrix[index,3],0)
  }
  else {
    output <- c(12,0,output[3] + costProbMatrix[index,3],output[4] + 1)
  }
  output
}

simulate <- function(from,to,costProbMatrix,baseCost,n) {
  output <- data.frame(cost = numeric(n),booms = numeric(n))
  rng <- sample(1:100000,10000,replace=TRUE)
  j <- 1
  for(i in 1:n) {
    state <- c(from,0,0,0,0)
    while(state[1] != to) {
      state <- testStar(state,costProbMatrix,baseCost,rng[j])
      j <- j+1
      if(j > 10000) {
        j <- 1
        rng <- sample(1:100000, 10000, replace=TRUE)
      }
    }
    output[i,] <- state[3:4]
  }
  output
}