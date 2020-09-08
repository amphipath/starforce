## Starforce Calculation Backend

sfTable <- function(baserCost,off30,MVP,baseCost,protCost,outcomes,catch,protect,protect2) {
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
  return(costTable)
}

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

## Spelltrace Calculation Backend

probabilityTable <- function(n,f,p,ham) {
  prob <- scroll <- hammer <- failed <- inno <- numeric(n + 2*f + 1)
  output <- data.frame(prob,scroll,hammer,failed,inno)
  # First calculate the situations where F fails are hit, prompting an inno
  output$prob[1:(n-f+1)] <- choose((f-1):(n-1),(f-1)) * p^(0:(n-f)) * (1-p)^f
  output$scroll[1:(n-f+1)] <- f:n
  output$failed[1:(n-f+1)] <- f
  #inno and hammer columns are easily specified without going into case-by-case
  output$inno[1:(n-f+3)] <- 1
  output$hammer[(n-f+2):nrow(output)] <- 1
  #generate probabilities when the hammer fails
  hammerfailindex <- seq(n-f+2,nrow(output),3)
  output$prob[hammerfailindex] <- (1 - ham) * choose(n,(f-1):0) * p^((n-f+1):n) * (1-p)^((f-1):0)
  output$scroll[hammerfailindex] <- n
  output$scroll[output$scroll == 0] <- n+1
  output$failed[hammerfailindex] <- f:1
  #generate probabilties when the hammer passes, and the next scroll fails
  hammerthenfailindex <- seq(n-f+3,nrow(output),3)
  output$prob[hammerthenfailindex] <- ham * choose(n,(f-1):0) * p^((n-f+1):n) * (1-p) ^ ((f):1)
  output$failed[hammerthenfailindex] <- f:1
  #generate probabilties when the hammer passes, and the next scroll passes
  hammerthenpassindex <- seq(n-f+4,nrow(output),3)
  output$prob[hammerthenpassindex] <- ham * choose(n,(f-1):0) * p^((n-f+2):(n+1)) * (1-p)^((f-1):0)
  output$failed[hammerthenpassindex] <- (f-1):0
  return(output)
}

## given a probability table, create an object that has all the properties we want

analyseProbs <- function(table,p,css,inno){
  output <- list()
  P <- sum(table$prob[table$inno == 0])
  ## Q is a random variable sampled from the inno'd outcmes.
  Q <- table[table$inno == 1,]
  Q$prob <- Q$prob / sum(Q$prob)
  Qmeans <- c(sum(Q$scroll * Q$prob),sum(Q$prob * Q$hammer),0,0)
  Qvars <- c(sum(Q$prob * (Q$scroll - Qmeans[1])^2),sum(Q$prob * (Q$hammer - Qmeans[2])^2),0,0)
  
  ## N is a random variable dictating how many times we inno before hitting on an "acceptable" outcome.
  Nmean <- 1/P - 1
  Nvar <- (1-P)/(P^2)
  
  ## CS is a random variable indicating the cost of filling a failed slot (in terms of scrolls and css's).
  CSmean <- c(1/p,0,0,1 / css / p)
  CSvar <- c((1-p)/p^2,0,0,(1 - css) / (css * css * p))
  
  ## A is a random variable indicating the state of an "acceptable" outcome
  A <- table[table$inno == 0,]
  A$prob <- A$prob / sum(A$prob)
  CmeanMatrix <- (A$fail * A$prob) %*% t(CSmean)
  Cmeans <- apply(CmeanMatrix,2,sum)
  CvarMatrix <- (A$fail * A$prob) %*% t(CSvar)
  Cvars <- apply(CvarMatrix,2,sum)
  Ameans <- c(sum(A$scroll * A$prob),sum(A$prob * A$hammer),0,0)
  Avars <- c(sum(A$prob * (A$scroll - Ameans[1])^2),sum(A$prob * (A$hammer - Ameans[2])^2),0,0)
  
  ## taking care of innos
  innoMean <- c(0,0,1/inno,0)
  innoVar <- c(0,0,(1 - inno)/inno^2,0)
  
  totalMean <- Nmean*Qmeans + Nmean*innoMean  + Ameans + Cmeans
  totalVar <- Nmean*Qvars + Nmean*innoVar + Avars + Cvars
  names(totalMean) <- names(totalVar) <- c('Scrolls','Hammers','Innos','Clean Slates')
  output$tMean <- totalMean
  output$tVar <- totalVar
  output$tSd <- sqrt(totalVar)
  
  return(output)
}

## Combining the above two functions

scrollStats <- function(n,f,p,ham,css,inno){
  table <- probabilityTable(n,f,p,ham)
  return(analyseProbs(table,p,css,inno))
}

## list of presets

presets <- list()

presetTable <- read.csv('presets.csv',header=T)

addPreset <- function(list, name, n, cost, p) {
  list[[name]] <- data.frame(n = n, cost= cost, p = p)
  return(list)
}

for(i in 1:nrow(presetTable)) {
  line <- presetTable[i,]
  presets <- addPreset(presets, as.character(line$name), line$slots, line$cost, line$p)
}


## Miscellaneous declarations

innoTrace <- T
cssTrace <- T

## Fever substitution of passing chances
feverDict <- list()
feverDict[[70]] <- 95
feverDict[[15]] <- 25
feverDict[[5]] <- 10
feverDict[[30]] <- 45


feverSub <- function(p) {
  return(feverDict[[p]])
}
