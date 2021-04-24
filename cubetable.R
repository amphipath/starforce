require(data.table)

cubetable <- fread('https://raw.githubusercontent.com/amphipath/starforce/master/weights_en.csv')

get_lines <- function(cube,slot,grade) {
  result <- list()
  lines <- cubetable[Cube == cube & Equip == slot]
  grades <- unique(lines$Grade)
  nonprime <- grades[which(grades == grade)-1]
  result$prime <- lines[Grade == grade]
  result$nonprime <- lines[Grade == nonprime,]
  return(result)
}

equivalent_stats <- function(statper10lvs,att) { 
  if(is.null(statper10lvs)) {
    statper10lvs = 3
  }
  if(is.null(att)){
    att = 0.4
  }
  return(list(
  `STR %` = list(`All Stats %` = 1,`STR per 10 character levels` = statper10lvs,`Attack` = att),
  `DEX %` = list(`All Stats %` = 1,`DEX per 10 character levels` = statper10lvs, Attack = att),
  `INT %` = list(`All Stats %` = 1,`DEX per 10 character levels` = statper10lvs, `Magic Attack` = att),
  `LUK %` = list(`All Stats %` = 1,`DEX per 10 character levels` = statper10lvs, `Attack` = att),
  `Boss Damage %` = list(`Damage %` = 1)
  )
  )
}

relevant_stats <- function(desired, equivalences = equivalent_stats(NULL,NULL)) {
  stats <- desired[,1]
  for(i in stats) {
    if(i %in% names(equivalences)){
      stats <- c(stats,names(equivalences[[i]]))
    }
  }
  return(stats)
}

check_satisfy <- function(desired,potential,equivalences = equivalent_stats(NULL,NULL)) {
  for(desired.row in 1:nrow(desired)) {
    desired.stat <- desired[[desired.row,1]]
    req.score <- desired[[desired.row,2]]
    if(desired.stat %in% names(equivalences)) {
      for(i in names(equivalences[[desired.stat]])) {
        if(i %in% desired[,1]) {
          req.score <- req.score + desired[match(i,desired[,1]),2] * equivalences[[desired.stat]][[i]]
        }
      }
    }
    score <- 0
    for(i in list(list(potential$stat1,potential$value1),list(potential$stat2,potential$value2),list(potential$stat3, potential$value3))) {
      if(i[[1]] == desired.stat) {
        score <- score + i[[2]]
      }
      else {
        if(desired.stat %in% names(equivalences)) {
          if(i[[1]] %in% names(equivalences[[desired.stat]])) {
            score <- score + equivalences[[desired.stat]][[i[[1]]]] * i[[2]]
          }
        }
      }
    }
    if(score < req.score) {
      return(FALSE)
    }
  }
  return(TRUE)
}

one_line_allowed <- c('All Skill Levels','Invincibility duration after getting attacked')
two_lines_allowed <- c('Ignore Enemy DEF', "Boss Damage", "Item Drop Rate", "chance to ignore",'invincibility after getting attacked')
prime_chances <- list(Black = list(Rare = c(0.2,0.05),
                                   Epic = c(0.2,0.05),
                                   Unique = c(0.2,0.05),
                                   Legendary = c(0.2,0.05)),
                      Red = list(Rare = c(0.1,0.01),
                                 Epic = c(0.1,0.01),
                                 Unique = c(0.1,0.01),
                                 Legendary = c(0.1,0.01)),
                      Additional = list(Rare = c(1/51,1/51),
                                        Epic = c(1/21,1/21),
                                        Unique = c(1/51,1/51),
                                        Legendary = c(1/201,1/201)))

simplify_linepool <- function(pool,desiredstats) {
  desired <- pool$Stat %in% desiredstats
  restricted <- sapply(pool$Stat,check_any_regex,patternvector = c(one_line_allowed,two_lines_allowed))
  filter <- desired | restricted
  relevant_lines <- pool[filter,]
  other_lines <- pool[!filter,]
  relevant_lines <- rbind(relevant_lines,data.frame(Stat = 'Other',Value=0,Weight=sum(other_lines$Weight),Probability=sum(other_lines$Probability)))
  return(relevant_lines)
}

check_any_regex <- function(patternvector,string) {
  for(i in patternvector){
    if(grepl(i,string)) {
      return(T)
    }
  }
  return(F)
}

list_satisfy <- function(cube, slot, grade, desired, equivalences = equivalent_stats(NULL,NULL)) {
  potlines <- get_lines(cube,slot,grade)
  prime <- potlines$prime
  nonprime <- potlines$nonprime
  first_line_pool <- data.frame(prime[,c('Stat','Value','Weight')])
  first_line_pool$Probability <- first_line_pool$Weight / sum(first_line_pool$Weight) 
  second_line_prime_pool <- data.frame(prime[,c('Stat','Value','Weight')]) 
  second_line_prime_pool$Probability <- second_line_prime_pool$Weight / sum(second_line_prime_pool$Weight) * prime_chances[[cube]][[grade]][1]
  second_line_nonprime_pool <- data.frame(nonprime[,c('Stat','Value','Weight')])
  second_line_nonprime_pool$Probability <- second_line_nonprime_pool$Weight / sum(second_line_nonprime_pool$Weight) * (1 - prime_chances[[cube]][[grade]][1])
  second_line_pool <- rbind(second_line_prime_pool,second_line_nonprime_pool)
  third_line_prime_pool <- data.frame(prime[,c('Stat','Value','Weight')]) 
  third_line_prime_pool$Probability <- third_line_prime_pool$Weight / sum(third_line_prime_pool$Weight) * prime_chances[[cube]][[grade]][2]
  third_line_nonprime_pool <- data.frame(nonprime[,c('Stat','Value','Weight')])
  third_line_nonprime_pool$Probability <- third_line_nonprime_pool$Weight / sum(third_line_nonprime_pool$Weight) * (1 - prime_chances[[cube]][[grade]][2])
  third_line_pool <- rbind(third_line_prime_pool,third_line_nonprime_pool)
  desiredstats <- relevant_stats(desired,equivalences)
  first_line_pool <- simplify_linepool(first_line_pool,desiredstats)
  second_line_pool <- simplify_linepool(second_line_pool,desiredstats)
  third_line_pool <- simplify_linepool(third_line_pool,desiredstats)
  numrows <- 1000
  pottable <- data.table(stat1 = character(numrows), value1 = numeric(numrows), stat2 = character(numrows), value2 = numeric(numrows), stat3 = character(numrows), value3 = numeric(numrows), p = numeric(numrows))
  emptyrow <- 1
  
  for(i in 1:nrow(first_line_pool)) {
    potential = list()
    potential$stat1 <- first_line_pool$Stat[i]
    potential$value1 <- first_line_pool$Value[i]
    potential$p1 <- first_line_pool$Probability[i]
    second_line_pool_temp <- copy(second_line_pool)
    for(restrictedline in one_line_allowed){
      if(grepl(restrictedline,potential$stat1)){
        second_line_pool_temp <- second_line_pool_temp[!grepl(restrictedline,second_line_pool_temp$Stat),]
        second_line_pool_temp$Probability <- second_line_pool_temp$Probability / sum(second_line_pool_temp$Probability)
      } 
    }
    for(j in 1:nrow(second_line_pool_temp)){
      potential$p2 <- second_line_pool_temp$Probability[j] * potential$p1
      potential$stat2 <- second_line_pool_temp$Stat[j]
      potential$value2 <- second_line_pool_temp$Value[j]
      third_line_pool_temp <- copy(third_line_pool)
      for(restrictedline in one_line_allowed){
        if(sum(grepl(restrictedline,c(potential$stat1,potential$stat2))) > 0) {
          third_line_pool_temp <- third_line_pool_temp[!grepl(restrictedline,third_line_pool_temp$Stat),]
          third_line_pool_temp$Probability <- third_line_pool_temp$Probability / sum(third_line_pool_temp$Probability)
        }
      }
      for(restrictedline in two_lines_allowed) {
        if(sum(grepl(restrictedline,c(potential$stat1,potential$stat2))) >= 2) {
          third_line_pool_temp <- third_line_pool_temp[!grepl(restrictedline,third_line_pool_temp$Stat),]
          third_line_pool_temp$Probability <- third_line_pool_temp$Probability / sum(third_line_pool_temp$Probability)
        }
      }
      for(k in 1:nrow(third_line_pool_temp)) {
        potential$stat3 <- third_line_pool_temp$Stat[k]
        potential$value3 <- third_line_pool_temp$Value[k]
        potential$p <- potential$p2 * third_line_pool_temp$Probability[k]
        if(check_satisfy(desired,potential,equivalences)){
          if(emptyrow > numrows){
            pottable <- rbind(df,list(character(numrows),numeric(numrows),character(numrows),numeric(numrows),character(numrows),numeric(numrows),numeric(numrows)))
            numrows <- numrows + numrows
          }
          set(pottable,emptyrow,c(1L,2L,3L,4L,5L,6L,7L),potential[c('stat1','value1','stat2','value2','stat3','value3','p')])
          emptyrow <- emptyrow + 1
        }
      }
    }
  }
  pottable <- pottable[p>0]
  return(pottable[order(-p)])
}

