
# Libraries----
library("microbenchmark")
#library("profvis")
library(optimx)
library(tidyverse)
library(scales)
library(gridExtra)
#####

#profvis(expr = {


  # Set working directory----
  setwd("C:/Users/leven/OneDrive/Dokumentumok/Dokumentumok/Work/COVID-19/Project/IFR Shift/Data")
  #load("Results2.RData", envir = .GlobalEnv)

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
      parints = list(c(1e-6, 1e-4), c(0.01, 0.05), c(1e-6, 1e-4))
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
      male = tibble(
        lower = c(0.002, 0.0005, 0.001, 0.002, 0.007, 0.014, 0.028, 0.048, 0.091, 0.144, 0.250, 0.384, 0.511, 1.244, 1.976, 3.651, 9.276),
        median = c(0.003, 0.001, 0.001, 0.003, 0.008, 0.017, 0.033, 0.056, 0.106, 0.168, 0.291, 0.448, 0.595, 1.452, 2.307, 4.260, 10.825),
        upper = c(0.004, 0.001, 0.002, 0.003, 0.009, 0.020, 0.038, 0.065, 0.123, 0.195, 0.336, 0.518, 0.688, 1.680, 2.668, 4.929, 12.523)
      ) / 100,
      female = tibble(
        lower = c(0.002, 0.0005, 0.0005, 0.002, 0.004, 0.008, 0.013, 0.021, 0.038, 0.063, 0.105, 0.169, 0.273, 0.598, 0.893, 1.838, 4.935),
        median = c(0.003, 0.001, 0.001, 0.002, 0.005, 0.009, 0.015, 0.025, 0.044, 0.073, 0.123, 0.197, 0.318, 0.698, 1.042, 2.145, 5.759),
        upper = c(0.003, 0.001, 0.001, 0.003, 0.006, 0.011, 0.018, 0.029, 0.051, 0.085, 0.142, 0.228, 0.367, 0.807, 1.206, 2.482, 6.662)
      ) / 100,
      ints = c(seq(0, 80, by = 5), 100),
      gender = TRUE
    ),
    Salje = list(
      male = tibble(
        lower = c(0.0005, 0.003, 0.02, 0.03, 0.1, 0.6, 1.5, 7.9),
        median = c(0.001, 0.007, 0.03, 0.06, 0.2, 1, 2.7, 14),
        upper = c(0.003, 0.01, 0.05, 0.1, 0.4, 1.6, 3.4, 22.7)
      ) / 100,
      female = tibble(
        lower = c(0.0005, 0.002, 0.007, 0.02, 0.08, 0.3, 0.7, 2.7),
        median = c(0.001, 0.004, 0.01, 0.03, 0.1, 0.5, 1.3, 4.9),
        upper = c(0.002, 0.007, 0.02, 0.06, 0.2, 0.8, 2.1, 8)
      ) / 100,
      ints = c(0, 20, 30, 40, 50, 60, 70, 80, 100),
      gender = TRUE
    )
  )



  # Functions----
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
      intervals <- tibble(age, factor)

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

      sse <- (sum((log(mort.pred.male.age) - log(mortality.male.age))^2) + sum((log(mort.pred.female.age) - log(mortality.female.age))^2)) / (length(mortality.male.age) + length(mortality.female.age))
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

    ycoords.age.results <<- tibble(it = numeric(), agecat = numeric(), gender = numeric(), value = numeric())
    sse.age.results <<- tibble(it = numeric(), sse = numeric(), shift = numeric(), sse2 = numeric())
    fx.age.age.results <<- tibble(it = numeric(), age = numeric(), gender = numeric(), value = numeric())
    fx.age.agegroup.results <<- tibble(it = numeric(), agecat = numeric(), gender = numeric(), value = numeric())

    IFR.Gen(coords = T)
    estimate_all <<- optimx(firstguess, ef, control = list(all.methods = TRUE), lower = 1e-100)
    bestmethod <- rownames(estimate_all)[estimate_all$value == min(estimate_all$value)]
    estimate <- optimx(firstguess, ef, method = bestmethod, lower = 0)

    ef2 <- function(par, func = optFunction, IFR = optIFR) {
      age.d <- seq(20, 99, by = 0.01)
      nop <- eval(parse(text = paste("functions_optim_shift$", func, "$nop", sep = "")))
      parmsvector <- par
      names(parmsvector) <- "par1"

      eval(parse(text = paste("fx.m <- function() { return(",
        eval(parse(text = paste("functions_optim$", func, "$body.m", sep = ""))),
        ")}",
        sep = ""
      )))
      fx.m.age.d <- fx.m()

      eval(parse(text = paste("fx <- function(parmsvector) { return(",
        eval(parse(text = paste("functions_optim_shift$", func, "$body", sep = ""))),
        ")}",
        sep = ""
      )))
      assign("fx.m.age.d.shift", fx(parmsvector), envir = .GlobalEnv)

      # sse<-(sum((log(fx.m.age.d.shift) - log(fx.m.age.d))^2) )
      sse <- (sum(((fx.m.age.d.shift) - (fx.m.age.d))^2))
      return(sse)
    }

    estimate_all2 <<- optimx(1, ef2, control = list(all.methods = TRUE), lower = 1e-100)
    bestmethod2 <- rownames(estimate_all2)[estimate_all2$value == min(estimate_all2$value)]
    estimate2 <- optimx(1, ef2, method = bestmethod2, lower = 0)

    ycoords.age.results <<- rbind(ycoords.age.results, tibble(it = 0, agecat = xcoord, gender = "m", value = ycoord.male.median))
    ycoords.age.results <<- rbind(ycoords.age.results, tibble(it = 0, agecat = xcoord, gender = "f", value = ycoord.female.median))

    fx.age.agegroup.results <<- rbind(fx.age.agegroup.results, tibble(it = 0, agecat = meanage.male, gender = "m", value = mort.pred.male.age))
    fx.age.agegroup.results <<- rbind(fx.age.agegroup.results, tibble(it = 0, agecat = meanage.female, gender = "f", value = mort.pred.female.age))

    fx.age.age.results <<- rbind(fx.age.age.results, tibble(it = 0, age = 1:length(fx.m.age), gender = "m", value = fx.m.age))
    fx.age.age.results <<- rbind(fx.age.age.results, tibble(it = 0, age = 1:length(fx.f.age), gender = "f", value = fx.f.age))
    fx.age.age.results <<- rbind(fx.age.age.results, tibble(it = 0, age = 1:length(fx.f.age), gender = "ms", value = fx.m.age.d.shift[seq(20, 99, by = 0.01) %% 1 == 0]))
    sse.age.results <<- rbind(sse.age.results, tibble(it = 0, sse = estimate$value, shift = estimate2$p1, sse2 = estimate2$value))

    for (I in 1:iterations) {
      IFR.Gen(coords = F)
      estimate <- optimx(firstguess, ef, method = bestmethod, lower = 0)
      estimate2 <- optimx(1, ef2, method = bestmethod2, lower = 0)

      ycoords.age.results <<- rbind(ycoords.age.results, tibble(it = I, agecat = xcoord, gender = "m", value = ycoord.male))
      ycoords.age.results <<- rbind(ycoords.age.results, tibble(it = I, agecat = xcoord, gender = "f", value = ycoord.female))

      fx.age.agegroup.results <<- rbind(fx.age.agegroup.results, tibble(it = I, agecat = meanage.male, gender = "m", value = mort.pred.male.age))
      fx.age.agegroup.results <<- rbind(fx.age.agegroup.results, tibble(it = I, agecat = meanage.female, gender = "f", value = mort.pred.female.age))

      sse.age.results <<- rbind(sse.age.results, tibble(it = I, sse = estimate$value, shift = estimate2$p1, sse2 = estimate2$value))
      fx.age.age.results <<- rbind(fx.age.age.results, tibble(it = I, age = 1:length(fx.m.age), gender = "m", value = fx.m.age))
      fx.age.age.results <<- rbind(fx.age.age.results, tibble(it = I, age = 1:length(fx.f.age), gender = "f", value = fx.f.age))
      fx.age.age.results <<- rbind(fx.age.age.results, tibble(it = I, age = 1:length(fx.f.age), gender = "ms", value = fx.m.age.d.shift[seq(20, 99, by = 0.01) %% 1 == 0]))

      print(paste("Estimated sse:", estimate$value, "Estimated sse2:", estimate2$value, " Progress:", (I / iterations) * 100, "%"))
    }
  }


  predict <- function(population,
                      type,
                      permillion,
                      optIFR = optIFR_def) {
    # Demography----
    dem <- read.table(file = population, sep = " ", dec = ".", header = T, stringsAsFactors = F)
    dem <- dem.fraction(dem)
    if (permillion == T) {
      dem[, c(which(colnames(dem) == "Total"), which(colnames(dem) == "Males"), which(colnames(dem) == "Females"))] <- dem[, c(which(colnames(dem) == "Total"), which(colnames(dem) == "Males"), which(colnames(dem) == "Females"))] * 1e6 / sum(dem$Total)
    } else {}

    ints <- eval(parse(text = paste("IFR_optim$", optIFR, "$ints", sep = "")))
    ints <- ints[ints >= age[1] & ints <= (age[length(age)] + 1)]

    factor <- numeric()
    for (i in 1:(length(ints) - 1)) {
      factor <- c(factor, rep(x = i, times = (ints[i + 1] - ints[i])))
    }
    intervals <- tibble(age, factor)

    # Age-based predictions----
    # 1%risk
    # maleCM<-length(fx.m.age[fx.m.age<0.01])+19+1
    # femaleCM<-length(fx.f.age[fx.f.age<0.01])+19+1
    # neutralCM<-length(fx.fm.age[fx.fm.age<0.01])+19+1

    
    predictionModel<-function(id=id,
                              iF=infectedFraction,
                              rF=reductionFactor,
                              pT=protectionType,
                              mMA=maleMinAge,
                              fMA=femaleMinAge
                              ){
      agebased.infected <- cbind(Age = dem$Age, select(dem, Total, Males, Females) * iF)
      
      if(mMA==20 && fMA==20){
        reductionVectorMale <- c(rep(rF, length(mMA:99)))
        reductionVectorFemale <- c(rep(rF, length(fMA:99)))
      }else if(mMA==20){
        reductionVectorMale <- c(rep(rF, length(mMA:99)))
        reductionVectorFemale <-  c(rep(1, length(20:(fMA - 1))), rep(rF, length(fMA:99)))
      }else if(mMA!=20){
        reductionVectorMale <- c(rep(1, length(20:(mMA - 1))), rep(rF, length(mMA:99)))
        reductionVectorFemale <- c(rep(1, length(20:(fMA - 1))), rep(rF, length(fMA:99)))
      }
    
        agebased.infected <- agebased.infected %>%
          transmute(
            Age = Age,
            Total = Total,
            Males = Males,
            Females = Females,
            Males.prot = Males * reductionVectorMale,
            Females.prot = Females * reductionVectorFemale,
            Total.prot = Males.prot + Females.prot,
          )

        res<-tibble(
          Males = sum(dem$Males),
          Females = sum(dem$Females),
          inf.Males = sum(agebased.infected$Males),
          inf.Females = sum(agebased.infected$Females),
          inf.prot.Males = sum(agebased.infected$Males.prot),
          inf.prot.Females = sum(agebased.infected$Females.prot),
          dead.Males = sum(agebased.infected$Males * filter(fx.age.age.results, it == id & gender == "m")$value),
          dead.Females = sum(agebased.infected$Females * filter(fx.age.age.results, it == id & gender == "f")$value),
          dead.prot.Males = sum(agebased.infected$Males.prot * filter(fx.age.age.results, it == id & gender == "m")$value),
          dead.prot.Females = sum(agebased.infected$Females.prot * filter(fx.age.age.results, it == id & gender == "f")$value),
          old.Males=sum(filter(dem,Age>=mMA)$Males),
          old.Females=sum(filter(dem,Age>=fMA)$Females),
          old.inf.Males=sum(filter(agebased.infected,Age>=mMA)$Males),
          old.inf.Females=sum(filter(agebased.infected,Age>=fMA)$Females),
          old.inf.prot.Males=sum(filter(agebased.infected,Age>=mMA)$Males.prot),
          old.inf.prot.Females=sum(filter(agebased.infected,Age>=fMA)$Females.prot),
          old.dead.Males=sum(filter(agebased.infected,Age>=mMA)$Males * filter(fx.age.age.results, it == id & gender == "m",(age+19)>=mMA)$value),
          old.dead.Females=sum(filter(agebased.infected,Age>=fMA)$Females * filter(fx.age.age.results, it == id & gender == "f",(age+19)>=fMA)$value),
          old.dead.prot.Males=sum(filter(agebased.infected,Age>=mMA)$Males.prot * filter(fx.age.age.results, it == id & gender == "m",(age+19)>=mMA)$value),
          old.dead.prot.Females=sum(filter(agebased.infected,Age>=fMA)$Females.prot * filter(fx.age.age.results, it == id & gender == "f",(age+19)>=fMA)$value)
        )
              
          return(res)
      }
    
    
    halfgrid.gender<-expand.grid(
      id=sse.age.results$it,
      maleMinAge=50:99 #20:99
    )%>%
      rowwise()%>%
      mutate(shift=round(filter(sse.age.results,it==id)$shift,0),
             femaleMinAge=maleMinAge+round(shift,0),
             protectionType="gender")%>%
      filter(femaleMinAge<=99)%>%
      select(id,protectionType,maleMinAge,femaleMinAge)
    
    halfgrid.neutral<-expand.grid(
      id=sse.age.results$it,
      maleMinAge=50:99 #20:99
    )%>%
      rowwise()%>%
      mutate(femaleMinAge=maleMinAge,
             protectionType="neutral")%>%
      select(id,protectionType,maleMinAge,femaleMinAge)
    
    agebased.predictions<<-expand.grid(
      infectedFraction=seq(0, 1, by = 0.1), #seq(0.01, 1, by = 0.01)
      reductionFactor=seq(1, 0, by = -0.1), #seq(1, 0.1, by = -0.1)
      protectionType=c("neutral","gender")
    )%>%
      left_join(x=.,y=bind_rows(halfgrid.gender,halfgrid.neutral),by=c("protectionType"="protectionType"))%>%
      rowwise()%>%
      mutate(model=predictionModel(id,infectedFraction,reductionFactor,protectionType,maleMinAge,femaleMinAge))%>%
      unnest(model)
    #####
  }


  plots.models <- function() {
    myTheme <- function() {
      theme_bw() %+replace%
        theme(
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 24, face = "bold"),
          title = element_text(size = 24),
          legend.position = "top",
          legend.background = element_rect(colour = "black"),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 15)
        )
    }

    agebased1 <- ggplot() +
      geom_line(data = subset(fx.age.age.results, gender != "ms"), aes(x = age + 20 - 1, y = value, group = paste(it, gender), col = gender), alpha = 0.5) +
      geom_line(data = NULL, aes(x = xcoord, y = ycoord.male.lower), col = "lightskyblue", size = 1, linetype = "longdash") +
      geom_line(data = NULL, aes(x = xcoord, y = ycoord.male.median), col = "lightskyblue", size = 1) +
      geom_line(data = NULL, aes(x = xcoord, y = ycoord.male.upper), col = "lightskyblue", size = 1, linetype = "longdash") +
      geom_line(data = NULL, aes(x = xcoord, y = ycoord.female.lower), col = "lightpink", size = 1, linetype = "longdash") +
      geom_line(data = NULL, aes(x = xcoord, y = ycoord.female.median), col = "lightpink", size = 1) +
      geom_line(data = NULL, aes(x = xcoord, y = ycoord.female.upper), col = "lightpink", size = 1, linetype = "longdash") +
      stat_summary(data = subset(fx.age.agegroup.results, gender == "m"), aes(x = agecat, y = value), fun.max = max, fun.min = min, geom = "errorbar", width = 2, size = 1.3, col = "blue") +
      stat_summary(data = subset(fx.age.agegroup.results, gender == "f"), aes(x = agecat, y = value), fun.max = max, fun.min = min, geom = "errorbar", width = 2, size = 1.3, col = "red") +
      labs(x = "Age", y = "IFR", title = "Fit of the agebased model") +
      scale_color_manual(name = "Gender", labels = c("Female", "Male"), values = c("lightpink", "lightskyblue")) +
      myTheme()

    agebased2 <- agebased1 +
      scale_y_log10(
        breaks = trans_breaks("log10", function(x) 10^x),
        labels = trans_format("log10", math_format(10^.x))
      ) +
      theme(legend.position = "None")

    agebased.shift <- ggplot() +
      geom_line(data = NULL, aes(x = subset(fx.age.age.results, gender == "m")$value, y = subset(fx.age.age.results, gender == "ms")$value, group = subset(fx.age.age.results, gender == "ms")$it), alpha = 0.5, color = "red") +
      geom_abline(slope = 1, lwd = 1.2) +
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
    
    #Agebased.predictions visualization
    
    agebased.predictions<-agebased.predictions%>%
      mutate(
        infectedFraction=as.numeric(infectedFraction),
        reductionFactor=as.numeric(reductionFactor),
        protectionType=as.factor(protectionType),
        id=as.factor(id),
        maleMinAge=as.integer(maleMinAge),
        femaleMinAge=as.integer(femaleMinAge),
      )
    
    mIFR_div_fIFR<-ggplot(data=filter(agebased.predictions,id=="0"),
                  aes(group=paste(id,infectedFraction,protectionType,reductionFactor,femaleMinAge-maleMinAge)))+
      geom_line(aes(maleMinAge,(dead.prot.Males/Males)/(dead.prot.Females/Females),col=reductionFactor,lty=protectionType))+
      scale_color_gradientn("reductionFactor",colours=topo.colors(7),na.value = "transparent",breaks=c(0,0.5,1),labels=c(0,0.5,1),limits=c(0,1))+
      labs(x="Nemspecifikus védelem kezdete férfiaknál",y="(Férfi IFR) / (Nõi IFR)")+
      geom_text(x=90, y=1, label="Futtatási azonosító = '0'",size=7)+
      myTheme()
    
    
    avoided.m<-ggplot(filter(agebased.predictions,reductionFactor=="0.2"),aes(group=paste(id,infectedFraction,protectionType,reductionFactor,femaleMinAge-maleMinAge)))+
      geom_line(aes(maleMinAge ,(dead.Males-dead.prot.Males)/dead.Males),col="royalblue")+
      geom_line(aes(maleMinAge ,(dead.Females-dead.prot.Females)/dead.Females,lty=protectionType),col="firebrick2")+
      labs(x="Nemspecifikus védelem kezdete férfiaknál",y="Elkerült halálozások aránya")+
      geom_text(x=80, y=1, label="Védelem hatásossági aránya = 0.8",size=7,family="sans")+
      lims(y=c(0,1))+
      myTheme()
    
    avoided.f<-ggplot(filter(agebased.predictions,reductionFactor=="0.2"),aes(group=paste(id,infectedFraction,protectionType,reductionFactor,femaleMinAge-maleMinAge)))+
      geom_line(aes(femaleMinAge ,(dead.Males-dead.prot.Males)/dead.Males,lty=protectionType),col="royalblue")+
      geom_line(aes(femaleMinAge ,(dead.Females-dead.prot.Females)/dead.Females),col="firebrick2")+
      labs(x="Nemspecifikus védelem kezdete nõknél",y="Elkerült halálozások aránya")+
      geom_text(x=80, y=1, label="Védelem hatásossági aránya = 0.8",size=7,family="sans")+
      lims(y=c(0,1))+
      myTheme()
    
    avoided.rel<-ggplot(filter(agebased.predictions,reductionFactor=="0.2",id=="0"),aes(group=paste(id,infectedFraction,protectionType,reductionFactor,femaleMinAge-maleMinAge)))+
      geom_line(aes(198-(maleMinAge+femaleMinAge) ,((dead.Males+dead.Females)-(dead.prot.Males+dead.prot.Females))/(dead.Males+dead.Females),lty=protectionType),col="forestgreen")+
      geom_line(aes(198-(maleMinAge+femaleMinAge) ,((dead.Males)-(dead.prot.Males))/(dead.Males),lty=protectionType),col="royalblue")+
      geom_line(aes(198-(maleMinAge+femaleMinAge) ,((dead.Females)-(dead.prot.Females))/(dead.Females),lty=protectionType),col="firebrick2")+
      labs(x="Védett életkorkategóriák száma",y="Elkerült halálozások aránya")+
      geom_text(x=70, y=1, label="Védelem hatásossági aránya = 0.8",size=7,family="sans")+
      geom_text(x=73, y=0.9, label="Futtatás azonosítója = '0'",size=7,family="sans")+
      lims(y=c(0,1))+
      myTheme()
    
    avoided.abs<-ggplot(filter(agebased.predictions,reductionFactor=="0.2",infectedFraction=="1",id=="0"),aes(group=paste(id,infectedFraction,protectionType,reductionFactor,femaleMinAge-maleMinAge)))+
      geom_line(aes(198-(maleMinAge+femaleMinAge) ,((dead.Males+dead.Females)-(dead.prot.Males+dead.prot.Females)),lty=protectionType),col="forestgreen")+
      geom_line(aes(198-(maleMinAge+femaleMinAge) ,((dead.Males)-(dead.prot.Males)),lty=protectionType),col="royalblue")+
      geom_line(aes(198-(maleMinAge+femaleMinAge) ,((dead.Females)-(dead.prot.Females)),lty=protectionType),col="firebrick2")+
      labs(x="Védett életkorkategóriák száma",y="Elkerült halálozások száma")+
      geom_text(x=70, y=1, label="Védelem hatásossági aránya = 0.8",size=7,family="sans")+
      geom_text(x=73, y=200, label="Futtatás azonosítója = '0'",size=7,family="sans")+
      geom_text(x=70, y=400, label="Megertõzõdés valószínûsége = 1",size=7,family="sans")+
      #lims(y=c(0,1))+
      myTheme()
    
    mIFR_div_fIFR.o<-ggplot(agebased.predictions,aes(group=paste(id,infectedFraction,protectionType,reductionFactor,femaleMinAge-maleMinAge)))+
      geom_line(aes(maleMinAge,(old.dead.prot.Males/old.inf.Males)/(old.dead.prot.Females/old.inf.Females),lty=protectionType),col="firebrick2")+
      labs(x="Nemspecifikus védelem kezdete férfiaknál",y="(Férfi IFR védetteknél) / (Nõi IFR védetteknél)")+
      myTheme()
    
    IFR.g.specs<-list(
      scale_color_viridis_c(),
      myTheme(),
      theme(legend.text = element_text(angle=45, vjust=.5, hjust=0.5),
            legend.margin=margin(10,20,10,10))
    )
    
    
    IFR.m<-ggplot(filter(agebased.predictions,infectedFraction=="0.6",id=="0",reductionFactor %in% c(0,"0.2",0.4,"0.6",0.8,1)),aes(group=paste(id,infectedFraction,protectionType,reductionFactor,femaleMinAge-maleMinAge)))+
      geom_line(aes(maleMinAge,(dead.prot.Males/inf.Males),col=reductionFactor))+
      geom_line(aes(maleMinAge,(dead.prot.Females/inf.Females),lty=protectionType),col="firebrick2")+
      labs(x="Nemspecifikus védelem kezdete férfiaknál",y="Férfi IFR")+
      geom_text(x=58, y=0.013, label="Futtatási azonosító = '0'",size=7,family="sans")+
      geom_text(x=57, y=0.012, label="Fertõzési arány = 0.6",size=7)+
      lims(y=c(0,0.013))+
      IFR.g.specs
    
    IFR.f<-ggplot(filter(agebased.predictions,infectedFraction=="0.6",id=="0"),aes(group=paste(id,infectedFraction,protectionType,reductionFactor,femaleMinAge-maleMinAge)))+
      geom_line(aes(femaleMinAge,(dead.prot.Females/inf.Females),col=reductionFactor))+
      labs(x="Nemspecifikus védelem kezdete nõknél",y="Nõi IFR")+
      lims(y=c(0,0.013))+
      IFR.g.specs
    
    IFR.m.o<-ggplot(filter(agebased.predictions,infectedFraction=="0.6",id=="0",reductionFactor %in% c(0,"0.2",0.4,"0.6",0.8,1)),aes(group=paste(id,infectedFraction,protectionType,reductionFactor,femaleMinAge-maleMinAge)))+
      geom_line(aes(maleMinAge,(old.dead.prot.Males/old.inf.Males),col=reductionFactor))+
      geom_line(aes(maleMinAge,(old.dead.prot.Females/old.inf.Females),lty=protectionType),col="firebrick2")+
      labs(x="Nemspecifikus védelem kezdete férfiaknál",y="Férfi IFR a védett korosztályban")+
      geom_text(x=58, y=0.3, label="Futtatási azonosító = '0'",size=7,family="sans")+
      geom_text(x=57, y=0.28, label="Fertõzési arány = 0.6",size=7)+
      lims(y=c(0,0.5))+
      IFR.g.specs
    
    IFR.f.o<-ggplot(filter(agebased.predictions,infectedFraction=="0.6",id=="0"),aes(group=paste(id,infectedFraction,protectionType,reductionFactor,femaleMinAge-maleMinAge)))+
      geom_line(aes(femaleMinAge,(old.dead.prot.Females/old.inf.Females),col=reductionFactor))+
      labs(x="Nemspecifikus védelem kezdete nõknél",y="Nõi IFR a védett korosztályban")+
      lims(y=c(0,0.5))+
      IFR.g.specs

    ########
    
    pdf("Results2.pdf", width = 22, height = 10)
    grid.arrange(agebased1, agebased2, sse.age, ncol = 3)
    grid.arrange(sse.shift, shift, agebased.shift, ncol = 3)
    grid.arrange(IFR.m,IFR.f,ncol=2)
    grid.arrange(IFR.m.o,IFR.f.o,ncol=2)
    grid.arrange(mIFR_div_fIFR,mIFR_div_fIFR.o,ncol=2)
    grid.arrange(avoided.f,avoided.m,ncol=2)
    grid.arrange(avoided.abs,avoided.rel,ncol=2)
    dev.off()
  }


  # Commands----
  age <- 20:99
  bestMethod <- T
  #population_def <- "mort_HUN_17_mod.csv"
  population_def <- "morthun.csv"
  optIFR_def <- "ODriscoll"

  agebased(
    iterations = 10,
    optFunction = "G"
  )

  predict(
    population = population_def,
    type = "fraction",
    permillion = T,
  )

  plots.models()
#}, interval = 0.01)

# Save workspace----
 save.image(file = "Results3.RData")