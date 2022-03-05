stateq.intro <- "You can calculate your stat equivalences by looking at how your stats, etc change when adding link skills. This calculator will guide you along the steps to record the relevant info, then automatically churn out the results."

stateq.jobstats <- list(list(STR=4,DEX=1),
                        list(INT=4,LUK=1),
                        list(DEX=4,STR=1),
                        list(LUK=4,DEX=1),
                        list(LUK=4,DEX=1,STR=1),
                        list(LUK=1,DEX=1,STR=1),
                        list(HP=1,STR=4.375))

stateq.inputLink <- function(inputId, label, value, max, ...) {
  shiny::div(style="display:inline-block",
             selectInput(inputId,label,1:max,selected=value,width='150px',...))
}
stateq.linkExp <- "Choose the link skill levels that will be used. For the stacking links, higher levels produce more accurate results but are more troublesome to link."
stateq.xenonLabel <- "Hybrid Logic"
stateq.pirateLabel <- "Pirate's Boon"
stateq.cygnusLabel <- "Cygnus' Blessing"
stateq.kaiserLabel <- "Iron Will Level"

stateq.currentStatExp <- "Input these as you see on your stat screen. You don't have to do it clean/unbuffed, but if using any non-100% up time buffs make sure those buffs are consistent throughout measurements, e.g. if you want to measure with full-stacked Ark Link then make sure all subsequent measurements have Ark Link stacked too. For stats like STR, we are only interested in the total stat (outside of the parentheses)."

stateq.inputDmg <- function(inputId, label, value, ...) {
  shiny::div(style="display:inline-block",
             numericInput(inputId,label,value=0,min=0,width='150px',...))
}

stateq.currStatInput <- function(stat,...){
  return(shiny::tags$div(style="display:inline-block",
                         numericInput(paste0('stateq.curr',stat),label=stat,value=0,min=0,width='150px',...)))
}

stateq.refreshCurrent <- function(stats){
  removeUI(selector="#stateq-statinputinner",immediate=T)
  insertUI(selector="#stateq-statinput",
           ui=div(id='stateq-statinputinner',
                  div(lapply(names(stats),stateq.currStatInput))),immediate=T)
}

stateq.xenonExp <- "Link Hybrid Logic to your character then record the new total stats below."
stateq.pirateExp <- "Unlink Hybrid Logic, then link Pirate's Boon (make sure the number of stacks/levels you link corresponds to what you chose in step 2)."
stateq.cygnusExp <- "Now unlink Pirate's Boon and link Cygnus' Blessing (again, to the level declared in step 2). Record the upper damage range."
stateq.echoExp <- "Again, unlink, and this time cast Echo of Hero and record the upper range of Stat ATT."
stateq.kaiserExp <- "Unlink Cygnus' Blessing, and link Iron Will, then record the (real) Max HP."

stateq.xenonStatInput <- function(stat,...){
  return(shiny::tags$div(style="display:inline-block",
                         numericInput(paste0('stateq.xenon',stat),label=paste0(stat,' (Xenon Link)'),value=0,min=0,width='150px',...)))
}

stateq.refreshXenon <- function(stats){
  removeUI(selector="#stateq-xenonstatinputinner",immediate=T)
  subset <- names(stats)[names(stats) != 'HP']
  insertUI(selector="#stateq-xenonstatinput",
           ui=div(id='stateq-xenonstatinputinner',
                  div(lapply(subset,stateq.xenonStatInput))),immediate=T)
}

stateq.pirateStatInput <- function(stat,...){
  return(shiny::tags$div(style="display:inline-block",
                         numericInput(paste0('stateq.pirate',stat),label=paste0(stat,' (Pirate Link)'),value=0,min=0,width='150px',...)))
}

stateq.refreshPirate <- function(stats){
  removeUI(selector="#stateq-piratestatinputinner",immediate=T)
  insertUI(selector="#stateq-piratestatinput",
           ui=div(id='stateq-piratestatinputinner',
                  div(lapply(names(stats),stateq.pirateStatInput))),immediate=T)
}

stateq.statMult <- function(stats,coefficients){
  sum = 0
  for(i in names(coefficients)) {
    if(!is.null(stats[[i]])){
      sum <- sum + stats[[i]] * coefficients[[i]]
    }
  }
  return(sum)
}

stateq.statsum <- function(stat, increment) {
  for(i in names(increment)){
    stat[[i]] <- stat[[i]] + increment[[i]]
  }
  return(stat)
}

stateq.equivInput = function(ids, values, ...) {
  ## ids = vector of ids,
  ## values = vector of values to put into each input. must be same length as ids
  len <- length(ids)
  if (length(values) == 1) values <- rep(values, len)
  inputs = character(len)
  names(inputs) <- ids
  for (i in 1:len) {
    inputs[i] = as.character(numericInput(paste0("stateq.equiv",ids[i]), label = NULL, value = values[i], min=1, width='75px'))
  }
  inputs
}



stateq.codeMap <- list(ASr = 'All Stats %', FDr = 'Final Damage %', ATTr = 'ATT/MA %', ATT = 'ATT/MA', DMGr = 'DMG/BD %', CDr = 'Crit Damage %')

for(i in c("STR","DEX","INT","LUK","HP")) {
  stateq.codeMap[[i]] <- i
  stateq.codeMap[[paste0(i,'r')]] <- paste0(i,' %')
  stateq.codeMap[[paste0(i,'f')]] <-paste0('Final ',i)
}

stateq.desc <- function(names) {
  sapply(names,function(i){stateq.codeMap[[i]]})
}

stateq.zeronull <- function(x) {
  k <- as.numeric(x)
  if(sum(k) > 0) k else 0
}

stateq.generateExcel <- function(fdr) {
  n <- length(fdr)
  rownames <- stateq.desc(names(fdr))
  colnames <- c(' ',"Equivalences",stateq.desc(names(fdr)))
  cells <- paste0('=B',(1:n)+1,'*')
  matrix <- apply(fdr %*% t(1/fdr),2,function(x) {paste0(cells,x)})
  matrix <- cbind(rownames,'',matrix)
  matrix <- rbind(colnames,matrix)
  fusedrows <- apply(matrix,1,paste0,collapse='\t')
  return(paste0(fusedrows,collapse='\n'))
}
