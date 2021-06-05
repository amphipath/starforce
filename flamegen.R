flamelevelmap <- function(level){
  thresholds = c(0,20,40,60,80,100,120,140,160,180,200)
  return(max(which(thresholds <= level)))
}

tier.weights <- data.frame(drop = c(0.25,0.3,0.3,0.14,0.01),crimson = c(0.2,0.3,0.36,0.14,0),
                           rainbow = c(0,0.29,0.45,0.25,0.01), smallcraft = c(0.5,0.4,0.1,0,0),
                           mccraft = c(0.15,0.3,0.4,0.14,0.01), meistercraft = c(0,0.19,0.5,0.3,0.01),
                           mcfuse = c(0.25,0.35,0.3,0.1,0), meisterfuse = c(0,0.4,0.45,0.14,0.01),
                           chancetime = c(0,0.3,0.5,0.19,0.01))

#categorise all the possible flame types:
#label 1-4 for str/dex/int/luk
#5-10 for strdex, strint, strluk, dexint, dexluk, intluk
#11 for %as
#12 for att
#13 for matt
#14 for def
#15 for hp
#16 for mp
#17 for speed
#18 for jump
#19 for equip level reduction

armourflametable = function(level){
  # categorise the total stats as a 13-length vector:
  # str, dex, int, luk, %as, att, matt, def, hp, mp, speed, jump, lv red
  flatstatlevel = floor(level / 20) + 1
  mixstatlevel = floor(level/40) + 1
  hpmplevel = ifelse(floor(level/10),floor(level/10)*30,3)
  f1 = c(flatstatlevel,numeric(12))
  f2 = c(0,flatstatlevel,numeric(11)) 
  f3 = c(0,0,flatstatlevel,numeric(10)) 
  f4 = c(0,0,0,flatstatlevel,numeric(9)) 
  f5 = c(1,1,0,0,numeric(9)) * mixstatlevel
  f6 = c(1,0,1,0,numeric(9)) * mixstatlevel
  f7 = c(1,0,0,1,numeric(9)) * mixstatlevel
  f8 = c(0,1,1,0,numeric(9)) * mixstatlevel
  f9 = c(0,1,0,1,numeric(9)) * mixstatlevel
  f10 = c(0,0,1,1,numeric(9)) * mixstatlevel
  f11 = c(numeric(4),1,numeric(8))
  f12 = c(numeric(5),1,numeric(7))
  f13 = c(numeric(6),1,numeric(6))
  f14 = c(numeric(7),flatstatlevel,numeric(5))
  f15 = c(numeric(8),hpmplevel,numeric(4)) 
  f16 = c(numeric(9),hpmplevel,numeric(3)) 
  f17 = c(numeric(10),1,numeric(2))
  f18 = c(numeric(11),1,0)
  f19 = c(numeric(12),5)
  flametable = matrix(c(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19),13,19)
  return(flametable)
}

simulateflame <- function(level,n,boss=T,tierweights=NULL,typeweights=NULL){
  table = armourflametable(level)
  output = matrix(0,n,13)
  for(i in 1:n) {
    flamechoices = sample(1:19,4,prob = typeweights)
    flametiers = sample(1:5,4,replace=T,prob=tierweights)
    if(boss) {
      flametiers <- flametiers + 2
    }
    flames = matrix(0,4,13)
    for(j in 1:4) {
      flames[j,] = table[,flamechoices[j]] * flametiers[j]
    }
    flame = apply(flames,2,sum)
    output[i,] <- flame
  }
  return(output)
}

simulateflame2 <- function(level,n,boss=T,tierweights=NULL,typeweights=NULL){
  table = armourflametable(level)
  output = matrix(0,n,13)
  for(i in 1:n) {
    flamechoices = sample(1:19,4,prob = typeweights)
    flametiers = sample(1:5,4*n,replace=T,prob=tierweights)
    if(boss) {
      flametiers <- flametiers + 2
    }
    flames = matrix(0,4,13)
    for(j in 1:4) {
      flames[j,] = table[,flamechoices[j]] * flametiers[(j-1)*n+i]
    }
    flame = apply(flames,2,sum)
    output[i,] <- flame
  }
  return(output)
}

# stat: 1 for str, 2 for dex, 3 for int, 4 for luk, 9 for hp, -1 for xenon
scoreflame = function(flame,stat,asweight,attweight){
  weights <- c(0,0,0,0,asweight,0,0,0,0,0,0,0,0)
  if(stat == -1) {
    weights[c(1,2,4)] <- 1
    weights[6] <- attweight
  }
  else{
    weights[stat] <- 1
    if(stat == 3) {
      weights[7] <- attweight
    }
    else {
      weights[6] <- attweight
    }
  }
  return(sum(flame * weights))
}

scoreoutput <- function(output,stat,asweight,attweight) {
  return(apply(output,1,scoreflame,stat = stat, asweight = asweight, attweight = attweight))
}

flamedistribution <- function(threshold,level,n,stat,asweight=10,attweight=5,tierweights = c(0.4,0.3,0.2,0.1), typeweights = NULL) {
  output <- simulateflame(level,n,tierweights,typeweights)
  scores = scoreoutput(output,stat,asweight,attweight)
  plot(hist(scores))
  return(sum(scores > threshold) / length(scores))
}
