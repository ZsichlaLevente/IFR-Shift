#Set working directory
setwd("C:/Users/ASUS/Documents/MyDocs/Work/COVID-19/Project/IFR Shift/Data")

# Libraries----
library(optimx)
library(tidyverse)
library(scales)
library(gridExtra)

# Lists----
functions_optim <- list(
  genlog = list(
    nop = 4,
    body = "parmsvector['par1']/(1+exp(-parmsvector['par2']*(age-(parmsvector['par3']))))+parmsvector['par4']",
    body.m = "pars.m[1]/(1+exp(-pars.m[2]*(age.d-(pars.m[3]))))+pars.m[4]",
    body.f = "pars.f[1]/(1+exp(-pars.f[2]*(age.d-(pars.f[3]))))+pars.f[4]",
    parints = list(c(0, 1), c(0.01, 1), c(10, 200), c(1e-6, 1e-4))
  ),
  exp = list(
    nop = 2,
    body = "parmsvector['par1']*exp(age*parmsvector['par2'])",
    body.m = "pars.m[1]*exp(age.d*pars.m[2])",
    body.f = "pars.f[1]*exp(age.d*pars.f[2])",
    parints = list(c(1e-7, 1e-5), c(0.01, 0.5))
  ),
  GM = list(
    nop = 3,
    body = "parmsvector['par1']*exp(parmsvector['par2']*age)+parmsvector['par3']",
    body.m = "pars.m[1]*exp(pars.m[2]*age.d)+pars.m[3]",
    body.f = "pars.f[1]*exp(pars.f[2]*age.d)+pars.f[3]",
    parints = list(c(1e-6, 1e-4), c(0.01, 0.05),c(1e-6,1e-4))
  ),
  G = list(
    nop = 2,
    body = "parmsvector['par1']*exp(parmsvector['par2']*age)",
    body.m = "pars.m[1]*exp(pars.m[2]*age.d)",
    body.f = "pars.f[1]*exp(pars.f[2]*age.d)",
    parints = list(c(1e-6, 1e-4), c(0.01, 0.05))
  )
)

functions_optim_shift <- list(
  genlog = list(
    nop = 1,
    body = "pars.f[1]/(1+exp(-pars.f[2]*(age.d-(pars.f[3])+parmsvector['par1'])))+pars.f[4]",
    parints = list(c(1, 5))
  ),
  exp = list(
    nop = 1,
    body = "pars.f[1]*exp((age.d+parmsvector['par1'])*pars.f[2])",
    parints = list(c(1, 5))
  ),
  GM = list(
    nop = 1,
    body = "pars.f[1]*exp(pars.f[2]*(age.d+parmsvector['par1']))+pars.f[3]",
    parints = list(c(1, 5))
  ),
  G = list(
    nop = 1,
    body = "pars.f[1]*exp(pars.f[2]*(age.d+parmsvector['par1']))",
    parints = list(c(1, 5))
  )
)




IFR_optim <- list(
  ODriscoll = list(
    male = data.frame(
      lower = c(0.002, 0.0005, 0.001, 0.002, 0.007, 0.014, 0.028, 0.048, 0.091, 0.144, 0.250, 0.384, 0.511, 1.244, 1.976, 3.651, 9.276),
      median = c(0.003, 0.001, 0.001, 0.003, 0.008, 0.017, 0.033, 0.056, 0.106, 0.168, 0.291, 0.448, 0.595, 1.452, 2.307, 4.260, 10.825),
      upper = c(0.004, 0.001, 0.002, 0.003, 0.009, 0.020, 0.038, 0.065, 0.123, 0.195, 0.336, 0.518, 0.688, 1.680, 2.668, 4.929, 12.523)
    ) / 100,
    female = data.frame(
      lower = c(0.002, 0.0005, 0.0005, 0.002, 0.004, 0.008, 0.013, 0.021, 0.038, 0.063, 0.105, 0.169, 0.273, 0.598, 0.893, 1.838, 4.935),
      median = c(0.003, 0.001, 0.001, 0.002, 0.005, 0.009, 0.015, 0.025, 0.044, 0.073, 0.123, 0.197, 0.318, 0.698, 1.042, 2.145, 5.759),
      upper = c(0.003, 0.001, 0.001, 0.003, 0.006, 0.011, 0.018, 0.029, 0.051, 0.085, 0.142, 0.228, 0.367, 0.807, 1.206, 2.482, 6.662)
    ) / 100,
    ints = c(seq(0, 80, by = 5), 100),
    gender = TRUE
  ),
  Salje = list(
    male = data.frame(
      lower = c(0.0005, 0.003, 0.02, 0.03, 0.1, 0.6, 1.5, 7.9),
      median = c(0.001, 0.007, 0.03, 0.06, 0.2, 1, 2.7, 14),
      upper = c(0.003, 0.01, 0.05, 0.1, 0.4, 1.6, 3.4, 22.7)
    ) / 100,
    female = data.frame(
      lower = c(0.0005, 0.002, 0.007, 0.02, 0.08, 0.3, 0.7, 2.7),
      median = c(0.001, 0.004, 0.01, 0.03, 0.1, 0.5, 1.3, 4.9),
      upper = c(0.002, 0.007, 0.02, 0.06, 0.2, 0.8, 2.1, 8)
    ) / 100,
    ints = c(0, 20, 30, 40, 50, 60, 70, 80, 100),
    gender = TRUE
  )
)

#Save or load workspace----
#save.image(file = "Results.RData")
#load("Results.RData", envir = .GlobalEnv)

#Functions----
dem.fraction <- function(pop) {
  total <- sapply(2:4, function(x) sum(pop[, x])) # total number of individuals, males and females
  fraction <- pop[, 2:4] / total[1] # fraction of the whole pop in each age/gender class
  names(fraction) <- c("fr.Total", "fr.Males", "fr.Females")
  pop <- cbind(pop, fraction) # adding fractional data to the data frame
  pop <- pop[(age[1] + 1 - 20):(age[length(age)] + 1 - 20), ]
  return(pop)
}

agebased <- function(iterations, 
                     population = population_def, 
                     optFunction,
                     optIFR = optIFR_def) {
  dem <- read.table(file = population, sep = " ", dec = ".", header = T, stringsAsFactors = F)
  dem <- dem.fraction(dem)
  
  ef <- function(par, func = optFunction, IFR = optIFR) {
    nop <- eval(parse(text = paste("functions_optim$", func, "$nop", sep = "")))
    
    for (j in 1:2) {
      parmsvector <- numeric(0)
      for (i in 1:nop) {
        if (j == 1) {
          parmsvector <- append(parmsvector, par[i] * parscale[i])
        } else if (j == 2) {
          parmsvector <- append(parmsvector, par[nop + i] * parscale[i])
        }
        
        names(parmsvector)[i] <- paste("par", i, sep = "")
      }
      
      eval(parse(text = paste("fx <- function(parmsvector) { return(",
                              eval(parse(text = paste("functions_optim$", func, "$body", sep = ""))),
                              ")}",
                              sep = ""
      )))
      
      if (j == 1) {
        assign("fx.m.age", fx(parmsvector), envir = .GlobalEnv)
        assign("pars.m", parmsvector, envir = .GlobalEnv)
      } else if (j == 2) {
        assign("fx.f.age", fx(parmsvector), envir = .GlobalEnv)
        assign("pars.f", parmsvector, envir = .GlobalEnv)
      }
    }
    assign("fx.fm.age", (fx.f.age * dem$fr.Females[(age + 1) - 20] + fx.m.age * dem$fr.Males[(age + 1) - 20]) / dem$fr.Total[(age + 1) - 20], envir = .GlobalEnv)
    
    factor <- numeric()
    for (i in 1:(length(ints) - 1)) {
      factor <- c(factor, rep(x = i, times = (ints[i + 1] - ints[i])))
    }
    intervals <- data.frame(age, factor)
    
    mort.pred.male.age <<- numeric(0)
    mort.pred.female.age <<- numeric(0)
    meanage.male <<- numeric(0)
    meanage.female <<- numeric(0)
    
    for (z in 1:(length(ints) - 1)) {
      mort.pred.male.age[z] <<- sum(fx.m.age[intervals$factor == z] * dem$Males[intervals$factor == z] / sum(dem$Males[intervals$factor == z]))
      mort.pred.female.age[z] <<- sum(fx.f.age[intervals$factor == z] * dem$Females[intervals$factor == z] / sum(dem$Females[intervals$factor == z]))
      meanage.male[z] <<- sum(age[intervals$factor == z] * dem$fr.Males[intervals$factor == z] / sum(dem$fr.Males[intervals$factor == z]))
      meanage.female[z] <<- sum(age[intervals$factor == z] * dem$fr.Females[intervals$factor == z] / sum(dem$fr.Females[intervals$factor == z]))
    }
    
    sse<-(sum((log(mort.pred.male.age) - log(mortality.male.age))^2) + sum((log(mort.pred.female.age) - log(mortality.female.age))^2))/(length(mortality.male.age)+length(mortality.female.age))
    return(sse)
  }
  IFR.Gen <- function(coords = F) {
    if (coords) {
      for (i in 1:3) {
        ints <<- eval(parse(text = paste("IFR_optim$", optIFR, "$ints", sep = "")))
        
          mortality.male.age <- eval(parse(text = paste("IFR_optim$", optIFR, "$male", sep = "")))[, i]
          mortality.male.age <- mortality.male.age[ints[-length(ints)] >= age[1] & ints[-length(ints)] <= (age[length(age)])]
          mortality.female.age <- eval(parse(text = paste("IFR_optim$", optIFR, "$female", sep = "")))[, i]
          mortality.female.age <- mortality.female.age[ints[-length(ints)] >= age[1] & ints[-length(ints)] <= (age[length(age)])]
        
        ints <<- ints[ints >= age[1] & ints <= (age[length(age)] + 1)]
        
        xcoord <<- rep(ints, rep(2, length(ints)))
        xcoord <<- xcoord[c(-1, -length(xcoord))]
        if (i == 1) {
            ycoord.male.lower <<- rep(mortality.male.age, rep(2, length(mortality.male.age)))
            ycoord.female.lower <<- rep(mortality.female.age, rep(2, length(mortality.female.age)))
        } else if (i == 2) {
            mortality.male.age <<- mortality.male.age
            mortality.female.age <<- mortality.female.age
            ycoord.male.median <<- rep(mortality.male.age, rep(2, length(mortality.male.age)))
            ycoord.female.median <<- rep(mortality.female.age, rep(2, length(mortality.female.age)))
        } else if (i == 3) {
            ycoord.male.upper <<- rep(mortality.male.age, rep(2, length(mortality.male.age)))
            ycoord.female.upper <<- rep(mortality.female.age, rep(2, length(mortality.female.age)))
        }
      }
    } else {
      ints <<- eval(parse(text = paste("IFR_optim$", optIFR, "$ints", sep = "")))
      
      maleIFR <- eval(parse(text = paste("IFR_optim$", optIFR, "$male", sep = "")))
      femaleIFR <- eval(parse(text = paste("IFR_optim$", optIFR, "$female", sep = "")))
      neutralIFR <- eval(parse(text = paste("IFR_optim$", optIFR, "$mort", sep = "")))
      
        mortality.male.age <<- numeric()
        mortality.female.age <<- numeric()
        for (j in 1:nrow(maleIFR)) {
          mortality.male.age <<- append(mortality.male.age, runif(min = maleIFR$lower[j], max = maleIFR$upper[j], 1))
          mortality.female.age <<- append(mortality.female.age, runif(min = femaleIFR$lower[j], max = femaleIFR$upper[j], 1))
        }
        mortality.male.age <<- mortality.male.age[ints[-length(ints)] >= age[1] & ints[-length(ints)] <= (age[length(age)])]
        mortality.female.age <<- mortality.female.age[ints[-length(ints)] >= age[1] & ints[-length(ints)] <= (age[length(age)])]
      
        ycoord.male <<- rep(mortality.male.age, rep(2, length(mortality.male.age)))
        ycoord.female <<- rep(mortality.female.age, rep(2, length(mortality.female.age)))
        
      ints <<- ints[ints >= age[1] & ints <= (age[length(age)] + 1)]
    }
  }
  
  parints <- eval(parse(text = paste("functions_optim$", optFunction, "$parints", sep = "")))
  parscale <- numeric(0)
  for (p in 1:length(parints)) {
    parscale <- append(parscale, runif(min = parints[[p]][1], max = parints[[p]][2], 1))
  }
  firstguess <- rep(1, 2 * eval(parse(text = paste("functions_optim$", optFunction, "$nop", sep = ""))))
  
  ycoords.age.results <<- data.frame(it = numeric(), agecat = numeric(), gender = numeric(), value = numeric())
  sse.age.results <<- data.frame(it = numeric(), sse=numeric(),shift=numeric(),sse2=numeric())
  fx.age.age.results <<- data.frame(it = numeric(), age = numeric(), gender = numeric(), value = numeric())
  fx.age.agegroup.results <<- data.frame(it = numeric(), agecat = numeric(), gender = numeric(), value = numeric())
  
  IFR.Gen(coords = T)
  estimate_all <<- optimx(firstguess, ef, control = list(all.methods = TRUE), lower = 1e-100)
  bestmethod <- rownames(estimate_all)[estimate_all$value == min(estimate_all$value)]
  estimate <- optimx(firstguess, ef, method = bestmethod, lower = 0)
  
  ef2 <- function(par, func = optFunction, IFR = optIFR) {
    age.d<-seq(20,99,by=0.01)
    nop <- eval(parse(text = paste("functions_optim_shift$", func, "$nop", sep = "")))
      parmsvector <- par
      names(parmsvector) <- "par1"
      
      eval(parse(text = paste("fx.m <- function() { return(",
                              eval(parse(text = paste("functions_optim$", func, "$body.m", sep = ""))),
                              ")}",
                              sep = ""
      )))
      fx.m.age.d<-fx.m()
      
      eval(parse(text = paste("fx <- function(parmsvector) { return(",
                              eval(parse(text = paste("functions_optim_shift$", func, "$body", sep = ""))),
                              ")}",
                              sep = ""
      )))
      assign("fx.m.age.d.shift", fx(parmsvector), envir = .GlobalEnv)

    #sse<-(sum((log(fx.m.age.d.shift) - log(fx.m.age.d))^2) )
    sse<-(sum(((fx.m.age.d.shift) - (fx.m.age.d))^2) )
    return(sse)
  }
  
  estimate_all2 <<- optimx(1, ef2, control = list(all.methods = TRUE), lower = 1e-100)
  bestmethod2 <- rownames(estimate_all2)[estimate_all2$value == min(estimate_all2$value)]
  estimate2 <- optimx(1, ef2, method = bestmethod2, lower = 0)

    ycoords.age.results <<- rbind(ycoords.age.results, data.frame(it = 0, agecat = xcoord, gender = "m", value = ycoord.male.median))
    ycoords.age.results <<- rbind(ycoords.age.results, data.frame(it = 0, agecat = xcoord, gender = "f", value = ycoord.female.median))
    
    fx.age.agegroup.results <<- rbind(fx.age.agegroup.results, data.frame(it = 0, agecat = meanage.male, gender = "m", value = mort.pred.male.age))
    fx.age.agegroup.results <<- rbind(fx.age.agegroup.results, data.frame(it = 0, agecat = meanage.female, gender = "f", value = mort.pred.female.age))

  fx.age.age.results <<- rbind(fx.age.age.results, data.frame(it = 0, age = 1:length(fx.m.age), gender = "m", value = fx.m.age))
  fx.age.age.results <<- rbind(fx.age.age.results, data.frame(it = 0, age = 1:length(fx.f.age), gender = "f", value = fx.f.age))
  fx.age.age.results <<- rbind(fx.age.age.results, data.frame(it = 0, age = 1:length(fx.f.age), gender = "ms", value = fx.m.age.d.shift[seq(20,99,by=0.01)%%1==0]))
  sse.age.results <<- rbind(sse.age.results, data.frame(it = 0, sse = estimate$value,shift=estimate2$p1, sse2 = estimate2$value))
  
  for (I in 1:iterations) {
    IFR.Gen(coords = F)
    estimate <- optimx(firstguess, ef, method = bestmethod, lower = 0)
    estimate2 <- optimx(1, ef2, method = bestmethod2, lower = 0)
    
      ycoords.age.results <<- rbind(ycoords.age.results, data.frame(it = I, agecat = xcoord, gender = "m", value = ycoord.male))
      ycoords.age.results <<- rbind(ycoords.age.results, data.frame(it = I, agecat = xcoord, gender = "f", value = ycoord.female))
      
      fx.age.agegroup.results <<- rbind(fx.age.agegroup.results, data.frame(it = I, agecat = meanage.male, gender = "m", value = mort.pred.male.age))
      fx.age.agegroup.results <<- rbind(fx.age.agegroup.results, data.frame(it = I, agecat = meanage.female, gender = "f", value = mort.pred.female.age))

    sse.age.results <<- rbind(sse.age.results, data.frame(it = I, sse = estimate$value,shift=estimate2$p1, sse2 = estimate2$value))
    fx.age.age.results <<- rbind(fx.age.age.results, data.frame(it = I, age = 1:length(fx.m.age), gender = "m", value = fx.m.age))
    fx.age.age.results <<- rbind(fx.age.age.results, data.frame(it = I, age = 1:length(fx.f.age), gender = "f", value = fx.f.age))
    fx.age.age.results <<- rbind(fx.age.age.results, data.frame(it = I, age = 1:length(fx.f.age), gender = "ms", value = fx.m.age.d.shift[seq(20,99,by=0.01)%%1==0]))
    
    print(paste("Estimated sse:", estimate$value,"Estimated sse2:", estimate2$value, " Progress:", (I / iterations) * 100, "%"))
  }
}


predict.compare <- function(population,
                            noi,
                            type,
                            permillion,
                            combIFR = optIFR_def) {
  # Demography----
  dem <- read.table(file = population, sep = " ", dec = ".", header = T, stringsAsFactors = F)
  dem<-dem.fraction(dem)
  if (permillion == T) {
    dem[, c(which(colnames(dem) == "Total"), which(colnames(dem) == "Males"), which(colnames(dem) == "Females"))] <- dem[, c(which(colnames(dem) == "Total"), which(colnames(dem) == "Males"), which(colnames(dem) == "Females"))] * 1e6 / sum(dem$Total)
  } else {}
  
  ints <- eval(parse(text = paste("IFR_optim$", combIFR, "$ints", sep = "")))
  ints <- ints[ints >= age[1] & ints <= (age[length(age)] + 1)]
  
  factor <- numeric()
  for (i in 1:(length(ints) - 1)) {
    factor <- c(factor, rep(x = i, times = (ints[i + 1] - ints[i])))
  }
  intervals <- data.frame(age, factor)
  
  # Age-based predictions----
  if (type == "number") {
    agebased.infected <- dem[, c(which(colnames(dem) == "fr.Total"), which(colnames(dem) == "fr.Males"), which(colnames(dem) == "fr.Females"))] * noi # it is assumed that the infections do not depend on age
  } else if (type == "fraction") {
    agebased.infected <- dem[, c(which(colnames(dem) == "Total"), which(colnames(dem) == "Males"), which(colnames(dem) == "Females"))] * noi # it is assumed that the infections do not depend on age
  }
  agebased.infected <- cbind(Age = dem[, which(colnames(dem) == "Age")], agebased.infected)
  
  agebased.dead <<- data.frame(it = numeric(), age = numeric(), gender = numeric(), value = numeric())
  
  for (I in 1:length(unique(sse.age.results$it))) {
      agebased.dead <<- rbind(agebased.dead, data.frame(it = unique(sse.age.results$it)[I], age = (1:length(age))+19, gender = "m", value = agebased.infected$Males * fx.age.age.results$value[fx.age.age.results$it == I-1 & fx.age.age.results$gender == "m"]))
      agebased.dead <<- rbind(agebased.dead, data.frame(it = unique(sse.age.results$it)[I], age = (1:length(age))+19, gender = "f", value = agebased.infected$Females * fx.age.age.results$value[fx.age.age.results$it == I-1 & fx.age.age.results$gender == "f"]))
  }
  
    agebased.meanIFR <- sum(fx.age.age.results$value[fx.age.age.results$it == 0 & fx.age.age.results$gender == "m"] * dem$fr.Males + fx.age.age.results$value[fx.age.age.results$it == 0 & fx.age.age.results$gender == "f"] * dem$fr.Females)
  
}


plots.models <- function(optIFR = optIFR_def) {
  myTheme<-function(){
    theme_bw() %+replace%
      theme(
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 24, face = "bold"),
        title = element_text(size = 24),
        legend.position = "top",
        legend.background = element_rect(colour = "black"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size=15)
      )
  }
  
  agebased1 <- ggplot() +
    geom_line(data = subset(fx.age.age.results,gender!="ms"), aes(x = age + 20 - 1, y = value, group = paste(it, gender), col = gender), alpha = 0.5)+
      geom_line(data = NULL, aes(x = xcoord, y = ycoord.male.lower), col = "lightskyblue", size = 1, linetype = "longdash") +
      geom_line(data = NULL, aes(x = xcoord, y = ycoord.male.median), col = "lightskyblue", size = 1) +
      geom_line(data = NULL, aes(x = xcoord, y = ycoord.male.upper), col = "lightskyblue", size = 1, linetype = "longdash") +
      geom_line(data = NULL, aes(x = xcoord, y = ycoord.female.lower), col = "lightpink", size = 1, linetype = "longdash") +
      geom_line(data = NULL, aes(x = xcoord, y = ycoord.female.median), col = "lightpink", size = 1) +
      geom_line(data = NULL, aes(x = xcoord, y = ycoord.female.upper), col = "lightpink", size = 1, linetype = "longdash") +
      stat_summary(data = subset(fx.age.agegroup.results, gender == "m"), aes(x = agecat, y = value), fun.max = max, fun.min = min, geom = "errorbar", width = 2, size = 1.3, col = "blue") +
      stat_summary(data = subset(fx.age.agegroup.results, gender == "f"), aes(x = agecat, y = value), fun.max = max, fun.min = min, geom = "errorbar", width = 2, size = 1.3, col = "red")+
    labs(x = "Age", y = "IFR", title = "Fit of the agebased model") +
    scale_color_manual(name = "Gender", labels = c("Female", "Male"), values = c("lightpink", "lightskyblue"))+
    myTheme()
  
  agebased2 <- agebased1 +
    scale_y_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x))
    ) +
    theme(legend.position = "None")
  
  agebased.shift <- ggplot() +
    geom_line(data =NULL, aes(x = subset(fx.age.age.results,gender=="m")$value, y = subset(fx.age.age.results,gender=="ms")$value, group = subset(fx.age.age.results,gender=="ms")$it), alpha = 0.5,color="red")+
    geom_abline(slope=1,lwd=1.2)+
    labs(x = "Male IFR", y = "Male Shifted IFR", title = "Fit of the shift model") +
    myTheme()
  
  sse.age <- ggplot(data = sse.age.results, aes(x = "Agebased", y = sse)) +
    geom_violin(fill = "darkorchid", alpha = 0.5) +
    geom_boxplot(width = 0.2) +
    labs(x = "", y = "sse", title = "Agebased model") +
    myTheme()
  
  sse.shift <- ggplot(data = sse.age.results, aes(x = "Shift", y = sse2)) +
    geom_violin(fill = "darkorchid", alpha = 0.5) +
    geom_boxplot(width = 0.2) +
    labs(x = "", y = "sse", title = "Shift model") +
    myTheme()
  
  shift <- ggplot(data = sse.age.results, aes(x = "Agebased", y = shift)) +
    geom_violin(fill = "forestgreen", alpha = 0.5) +
    geom_boxplot(width = 0.2) +
    labs(x = "", y = "Age shift", title = "Agebased model") +
    myTheme()

  agebased.dead.ribbon<-data.frame(gender=numeric(),x=numeric(),y=numeric(),lower=numeric(),upper=numeric())
  
  agebased.plotindex="0"
  
  for(A in age){
    agebased.dead.ribbon=rbind(agebased.dead.ribbon,data.frame(gender="m", x=A, y=agebased.dead$value[agebased.dead$it==agebased.plotindex&agebased.dead$gender=="m"&agebased.dead$age==A], lower=min(agebased.dead$value[agebased.dead$gender=="m"&agebased.dead$age==A]), upper=max(agebased.dead$value[agebased.dead$gender=="m"&agebased.dead$age==A])))
    agebased.dead.ribbon=rbind(agebased.dead.ribbon,data.frame(gender="f", x=A, y=agebased.dead$value[agebased.dead$it==agebased.plotindex&agebased.dead$gender=="f"&agebased.dead$age==A], lower=min(agebased.dead$value[agebased.dead$gender=="f"&agebased.dead$age==A]), upper=max(agebased.dead$value[agebased.dead$gender=="f"&agebased.dead$age==A])))
    
  }
  
  compare.deaths.male1 <- ggplot() +
    geom_col(data = subset(agebased.dead, gender == "m" & it == agebased.plotindex), aes(x = age, y = value, group = paste(it)), fill = "lightskyblue") +
    geom_ribbon(data=subset(agebased.dead.ribbon,gender=="m"),aes(ymin=lower, ymax=upper, x=x), fill = "blue4", alpha = 0.3)+
    labs(x = "Age", y = "Number of deaths", title = "Agebased") +
    myTheme()
  
  compare.deaths.female1 <- ggplot() +
    geom_col(data = subset(agebased.dead, gender == "f" & it == agebased.plotindex), aes(x = age, y = value, group = paste(it)), fill = "lightpink") +
    geom_ribbon(data=subset(agebased.dead.ribbon,gender=="f"),aes(ymin=lower, ymax=upper, x=x), fill = "red4", alpha = 0.3)+
    labs(x = "Age", y = "HNumber of deaths", title = "Agebased") +
    myTheme()
  
  
  pdf("Results.pdf", width = 22, height = 10)
  grid.arrange(agebased1,agebased2,sse.age,ncol=3)
  grid.arrange(sse.shift,shift,agebased.shift,ncol=3)
  grid.arrange(compare.deaths.male1,compare.deaths.female1,ncol=2)
  dev.off()
}


# Commands----
age <- 20:99
bestMethod=T
population_def <- "mort_BGR_17_mod.csv"
optIFR_def <- "ODriscoll"

agebased(
  iterations = 10,
  optFunction = "G"
)

predict.compare(
  population=population_def,
  noi = 0.2,
  type = "fraction",
  permillion = T
)


plots.models()

