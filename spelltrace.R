
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
