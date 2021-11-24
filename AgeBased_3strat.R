
# Libraries----
library("microbenchmark")
#library("profvis")
library(optimx)
library(tidyverse)
library(scales)
library(gridExtra)
library(egg)
#####

#profvis(expr = {


  # Set working directory----
  setwd("C:/Users/leven/OneDrive/Dokumentumok/Dokumentumok/Work/COVID-19/Project/IFR Shift/Data")
  load("Results.3strat.RData", envir = .GlobalEnv)

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
  dem.gen <- function(population,permillion) {
    
    pop<-read.table(file = population, sep = " ", dec = ".", header = T, stringsAsFactors = F)
    pop<-pop%>%
      mutate(
        fr.Total = Total / sum(Total),
        fr.Males = Males / sum(Total),
        fr.Females = Females / sum(Total)
      )%>%
      filter(Age>=age[1],Age<=last(age))
    
    if (permillion) {
      pop%>%
        mutate(
          Total = Total * 1e6 / sum(dem$Total),
          Males = Males * 1e6 / sum(dem$Total),
          Females = Females * 1e6 / sum(dem$Total)
        )
    }
    
    return(pop)
  }

  agebased <- function(population = population_def,
                       optFunction,
                       optIFR = optIFR_def) {
    #### Demographic data
    
    dem <- dem.gen(population,permillion=F)

    #### Agebased model setup
    
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

    #### Agebased model fit
    
    IFR.Gen(coords = T)
    estimate_all <<- optimx(firstguess, ef, control = list(all.methods = TRUE), lower = 1e-100)
    bestmethod <- rownames(estimate_all)[estimate_all$value == min(estimate_all$value)]
    estimate <- optimx(firstguess, ef, method = bestmethod, lower = 0)

    #### Shift model setup
    
    eqRisk.model <- function(par, func = optFunction, IFR = optIFR) {
      age.d <- seq(age[1], last(age), by = 0.01)
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
    
    #### Shift model fit

    estimate_all2 <<- optimx(1, eqRisk.model, control = list(all.methods = TRUE), lower = 1e-100)
    bestmethod2 <- rownames(estimate_all2)[estimate_all2$value == min(estimate_all2$value)]
    estimate2 <- optimx(1, eqRisk.model, method = bestmethod2, lower = 0)

    #### Setting up tibbles
    
    ycoords.age.results <<- tibble(it = numeric(), agecat = numeric(), gender = character(), value = numeric())
    sse.age.results <<- tibble(it = numeric(), sse = numeric(), shift = numeric(), sse2 = numeric())
    fx.age.age.results <<- tibble(it = numeric(), age = numeric(), gender = character(), value = numeric())
    fx.age.agegroup.results <<- tibble(it = numeric(), agecat = numeric(), gender = character(), value = numeric())
    
    ####Saving results
    
    ycoords.age.results <<- bind_rows(
      ycoords.age.results, 
      tibble(it = 0, agecat = xcoord, gender = "m", value = ycoord.male.median),
      tibble(it = 0, agecat = xcoord, gender = "f", value = ycoord.female.median)
      )

    fx.age.agegroup.results <<- bind_rows(
      fx.age.agegroup.results, 
      tibble(it = 0, agecat = meanage.male, gender = "m", value = mort.pred.male.age),
      tibble(it = 0, agecat = meanage.female, gender = "f", value = mort.pred.female.age)
      )

    fx.age.age.results <<- bind_rows(
      fx.age.age.results, 
      tibble(it = 0, age = 1:length(fx.m.age), gender = "m", value = fx.m.age),
      tibble(it = 0, age = 1:length(fx.f.age), gender = "f", value = fx.f.age),
      tibble(it = 0, age = 1:length(fx.f.age), gender = "ms", value = fx.m.age.d.shift[seq(20, 99, by = 0.01) %% 1 == 0]))
    
    sse.age.results <<- bind_rows(
      sse.age.results, 
      tibble(it = 0, sse = estimate$value, shift = estimate2$p1, sse2 = estimate2$value)
      )
    
    #### 3 strategies for calculating fMA
    
    threeModels<-function(mMA,rF){
      fMA.opts<-tibble(fMA=20:99)%>%
        mutate(mMA=mMA)%>%
        filter(fMA>=mMA)%>%
        rowwise()%>%
        mutate(infected=bind_cols(Age = dem$Age, select(dem, Total, Males, Females) * 1)%>% # 1 is the iF
                 rowwise()%>%
                 mutate(
                   Males.prot = Males * if (Age<mMA) 1 else rF,
                   Females.prot = Females * if (Age<fMA) 1 else rF,
                   Total.prot = Males.prot + Females.prot
                 )%>%
                 list(),
               mDR=sum(infected$Males.prot * filter(fx.age.age.results,  gender == "m")$value) / sum(infected$Males),
               fDR=sum(infected$Females.prot * filter(fx.age.age.results,  gender == "f")$value) / sum(infected$Females),
               mDR_fDR=mDR/fDR,
               eqDR.vis=if_else(mDR_fDR>=0.95&mDR_fDR<=1.05,T,F),
               mPD= (sum(infected$Males * filter(fx.age.age.results,  gender == "m")$value) - sum(infected$Males.prot * filter(fx.age.age.results,  gender == "m")$value)) / sum(infected$Males * filter(fx.age.age.results,  gender == "m")$value),
               fPD=(sum(infected$Females * filter(fx.age.age.results,  gender == "f")$value) - sum(infected$Females.prot * filter(fx.age.age.results,  gender == "f")$value)) / sum(infected$Females * filter(fx.age.age.results,  gender == "f")$value),
               mPD_fPD=mPD/fPD,
               eqPD.vis=if_else(mPD_fPD>=0.95&mPD_fPD<=1.05,T,F)
               )
      
      eqDR<-fMA.opts%>%
        rowwise()%>%
        filter(abs(1-mDR_fDR)==min(summarize(fMA.opts,abs(1-mDR_fDR))))%>%
        ungroup()%>%
        slice(1)%>%
        select(fMA,mDR,fDR,mDR_fDR,eqDR.vis)%>%
        rename(eqDR.fMA=fMA,
               eqDR.mDR=mDR,
               eqDR.fDR=fDR,
               eqDR.mDR_fDR=mDR_fDR)
      
      eqPD<-fMA.opts%>%
        rowwise()%>%
        filter(.,abs(1-mPD_fPD)==min(summarize(fMA.opts,abs(1-mPD_fPD))) )%>%
        ungroup()%>%
        slice(1)%>%
        select(fMA,mPD,fPD,mPD_fPD,eqPD.vis)%>%
        rename(eqPD.fMA=fMA,
               eqPD.mPD=mPD,
               eqPD.fPD=fPD,
               eqPD.mPD_fPD=mPD_fPD)
      
      if((mMA+round(estimate2$p1,0))<100){
        eqRisk<-fMA.opts%>%
          rowwise()%>%
          filter(fMA==mMA+round(estimate2$p1,0))%>%
          ungroup()%>%
          select(fMA)%>%
          rename(eqRisk.fMA=fMA)%>%
          mutate(eqRisk.mRisk=filter(fx.age.age.results,  gender == "m",age==mMA-20+1)$value,
                 eqRisk.fRisk=filter(fx.age.age.results,  gender == "f",age==eqRisk.fMA-20+1)$value,
                 eqRisk.mRisk_fRisk=eqRisk.mRisk/eqRisk.fRisk,
                 eqRisk.vis=if_else(eqRisk.mRisk_fRisk>=0.95&eqRisk.mRisk_fRisk<=1.05,T,F))
      }else{
        eqRisk<-tibble(eqRisk.fMA=NA,
                       eqRisk.mRisk=NA,
                       eqRisk.fRisk=NA,
                       eqRisk.mRisk_fRisk=NA,eqRisk.vis=F)
      }

      
      print(paste("Equal Death Rate Model: mMA = ",mMA,"  fMA = ",eqDR$eqDR.fMA,"  mDR/fDR = ",eqDR$eqDR.mDR_fDR))
      print(paste("Equal Prevented Deaths Model: mMA = ",mMA,"  fMA = ",eqPD$eqPD.fMA,"  mPD/fPD = ",eqPD$eqPD.mPD_fPD))
      print(paste("Equal Risk Model: mMA = ",mMA,"  fMA = ",eqRisk$eqRisk.fMA,"mRisk/fRisk = ",eqRisk$eqRisk.mRisk_fRisk))
      cat("\n")
      
      return(bind_cols(eqDR,eqPD,eqRisk))
    }
    
    # Calculating optimal female protection ages
    
    minAge.results<<-expand.grid(mMA=age,
                                 rF=seq(0.1,0.9,by=0.1))%>% 
      mutate(fMA.neutral = mMA)%>%
      rowwise()%>%
      mutate(r=threeModels(mMA,rF))
  }


  plots.models <- function(detailed=F,doc) {
    myTheme <- function() {
      theme_bw() %+replace%
        theme(
          axis.text = element_text(size = 20),
          axis.title = element_text(size = 24, face = "bold"),
          title = element_text(size = 24),
          legend.position = "right",
          legend.background = element_rect(colour = "black"),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 15),
          plot.margin = margin(1, 1, 1, 1, "cm")
        )
    }
    
    scale_s1<-6000
    rF.vis<-0.4
    #detailed=F
    
    if(detailed){
      s1<-
      ggplot(minAge.results,aes(x=mMA))+
        geom_line(aes(y=fMA.neutral),lty=2,lwd=1.2 )+
        geom_point(aes(y=r$eqDR.fMA,group=rF,fill=r$eqDR.mDR_fDR),size=3,pch=21,stroke = 0.01)+
        geom_line(aes(y=r$eqDR.mDR*scale_s1,group=rF),color="royalblue",alpha=0.5)+
        geom_line(aes(y=r$eqDR.fDR*scale_s1,group=rF),color="firebrick",alpha=0.5)+
        labs(x="Minimum age of protection for males",y="Minimum age of protection for females",title="Equal Death Rate Model")+
        scale_fill_viridis_c(name = "DRm/DRf",  na.value = "transparent",breaks=c(0.8,1,1.4,1.8),labels=c(0.8,1,1.4,1.8),limits=c(0.8,1.8),option="C")+
        scale_y_continuous(
          sec.axis = sec_axis( trans=~./scale_s1, name="Overall Death Rate"),
          breaks = c(20,40,60,80,100)
        ) +
        myTheme()      
    }else{
      s1<-
      ggplot(filter(minAge.results,r$eqDR.vis,rF==rF.vis),aes(x=mMA))+
        geom_abline(lty=2,lwd=1.2 )+
        geom_point(aes(y=r$eqDR.fMA,group=rF),fill="orchid4",size=3,pch=21,stroke = 0.01)+
        geom_line(aes(y=r$eqDR.mDR*scale_s1,group=rF),color="royalblue",alpha=0.5)+
        geom_line(aes(y=r$eqDR.fDR*scale_s1,group=rF),color="firebrick",alpha=0.5)+
        labs(x="Minimum age of protection for males",y="Minimum age of protection for females")+
        lims(x=c(20,99))+
        scale_y_continuous(
          limits = c(20,99),
          sec.axis = sec_axis( trans=~./scale_s1, name="Overall Death Rate"),
          breaks = c(20,40,60,80,100)
        ) +
        myTheme()+
        theme(plot.margin = margin(0, 1, 1, 1, "cm"))
      
      s11<-
        ggplot(filter(minAge.results,r$eqDR.vis,rF==rF.vis),aes(x=mMA))+
          geom_point(aes(y=r$eqDR.fMA-mMA),size=2)+
          labs(y="Age Shift",title=paste("Equal Death Rate Model (rF = ",rF.vis," )",sep=""))+
          theme_bw()+
          lims(x=c(20,99))+
          theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y = element_text(size = 20),
              axis.title = element_text(size = 24, face = "bold"),
              title = element_text(size = 24),
              plot.margin = margin(1, 1, 0, 1, "cm"))
        
        
    }
      
      scale_s2<-400
      
      if (detailed){
        s2<-
        ggplot(minAge.results,aes(x=mMA))+
          geom_line(aes(y=fMA.neutral),lty=2,lwd=1.2 )+
          geom_point(aes(y=r$eqRisk.fMA,fill=r$eqPD.mPD_fPD),size=3,pch=21,stroke = 0.01)+
          geom_line(aes(y=r$eqRisk.mRisk*scale_s2),color="royalblue",alpha=0.5)+
          geom_line(aes(y=r$eqRisk.fRisk*scale_s2),color="firebrick",alpha=0.5)+
          labs(x="Minimum age of protection for males",y="Minimum age of protection for females",title="Equal Risk Model")+
          scale_fill_viridis_c(name = "MDRm/MDRf",  na.value = "transparent",breaks=c(0.8,1,1.2),labels=c(0.8,1,1.2),limits=c(0.8,1.2),option="C")+
          scale_y_continuous(
            sec.axis = sec_axis( trans=~./scale_s2, name="Death Risk at the minimum age"),
            breaks = c(20,40,60,80,100)
          ) +
          myTheme()
      }else{
        s2<-
        ggplot(filter(minAge.results,r$eqRisk.vis,rF==rF.vis),aes(x=mMA))+
          geom_abline(lty=2,lwd=1.2 )+
          geom_point(aes(y=r$eqRisk.fMA),fill="orchid4",size=3,pch=21,stroke = 0.01)+
          geom_line(aes(y=r$eqRisk.mRisk*scale_s2),color="royalblue",alpha=0.5)+
          geom_line(aes(y=r$eqRisk.fRisk*scale_s2),color="firebrick",alpha=0.5)+
          labs(x="Minimum age of protection for males",y="Minimum age of protection for females")+
          lims(x=c(20,99))+
          scale_y_continuous(
            #limits = c(20,99),
            sec.axis = sec_axis( trans=~./scale_s2, name="Death Risk at the minimum age"),
            breaks = c(20,40,60,80,100)
          ) +
          myTheme()+
          theme(plot.margin = margin(0, 1, 1, 1, "cm"))
        
        s22<-
          ggplot(filter(minAge.results,r$eqRisk.vis,rF==rF.vis),aes(x=mMA))+
          geom_point(aes(y=r$eqRisk.fMA-mMA),size=2)+
          labs(y="Age Shift",title="Equal Risk Model")+
          theme_bw()+
          lims(x=c(20,99))+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.text.y = element_text(size = 20),
                axis.title = element_text(size = 24, face = "bold"),
                title = element_text(size = 24),
                plot.margin = margin(1, 1, 0, 1, "cm"))
      }

      
      scale_s3<-100
      if(detailed){
        s3<-
        ggplot(minAge.results,aes(x=mMA))+  
          geom_abline(lty=2,lwd=1.2 )+
          geom_point(aes(y=r$eqPD.fMA,fill=r$eqPD.mPD_fPD,group=rF ),size=3,pch=21,stroke = 0.01)+
          geom_line(aes(y=r$eqPD.mPD*scale_s3,group=rF,color=rF),alpha=0.7)+  
          labs(x="Minimum age of protection for males",y="Minimum age of protection for females",title="Equal Prevented Deaths Model")+
          scale_fill_viridis_c(name = "PDm/PDf",  na.value = "transparent",breaks=c(0.4,0.7,1,1.2),labels=c(0.4,0.7,1,1.2),limits=c(0.4,1.2),option="C")+
          scale_color_viridis_c(name = "reductionFactor",  na.value = "transparent",breaks=c(0,0.5,1),labels=c(0,0.5,1),limits=c(0,1),option="G")+
          scale_y_continuous(
            sec.axis = sec_axis( trans=~./scale_s3, name="Ratio of prevented deaths"),
            breaks = c(20,40,60,80,100)
          ) +
          myTheme()        
      }else{
        s3<-
        ggplot(filter(minAge.results,r$eqPD.vis,rF==rF.vis),aes(x=mMA))+  
          geom_abline(lty=2,lwd=1.2 )+
          geom_point(aes(y=r$eqPD.fMA),fill="orchid4",size=3,pch=21,stroke = 0.01)+
          geom_line(aes(y=r$eqPD.mPD*scale_s3,group=rF),color="forestgreen",alpha=0.7)+  
          labs(x="Minimum age of protection for males",y="Minimum age of protection for females")+
          lims(x=c(20,99))+
          scale_y_continuous(
            sec.axis = sec_axis( trans=~./scale_s3, name="Ratio of prevented deaths"),
            breaks = c(20,40,60,80,100)
          ) +
          myTheme()+
          theme(plot.margin = margin(0, 1, 1, 1, "cm"))
        
        s33<-
          ggplot(filter(minAge.results,r$eqPD.vis,rF==rF.vis),aes(x=mMA))+
          geom_point(aes(y=r$eqPD.fMA-mMA),size=2)+
          labs(y="Age Shift",title="Equal Prevented Deaths Model")+
          theme_bw()+
          lims(x=c(20,99))+
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.text.y = element_text(size = 20),
                axis.title = element_text(size = 24, face = "bold"),
                title = element_text(size = 24),
                plot.margin = margin(1, 1, 0, 1, "cm"))
      }
    
      
      if(detailed){
        pdf(doc, width = 14, height = 10)
        grid.arrange(s1,ncol=1)
        grid.arrange(s2,ncol=1)
        grid.arrange(s3,ncol=1)
        dev.off()
      }else{
        pdf(doc, width = 14, height = 12)
        ggarrange(s11,s1,ncol=1, heights=c(2,10))
        ggarrange(s22,s2,ncol=1, heights=c(2,10))
        ggarrange(s33,s3,ncol=1, heights=c(2,10))
        dev.off()
      }
  }


  # Commands----
  age <- 20:99
  bestMethod <- T
  population_def <- "mort_GBR_NP_18_mod.csv"
  #population_def <- "morthun.csv"
  optIFR_def <- "ODriscoll"

  agebased(optFunction = "G")

  plots.models(detailed=F,doc="3strategies.pdf")
  plots.models(detailed=T,doc="3strategies_detailed.pdf")
#}, interval = 0.01)

# Save workspace----
 #save.image(file = "Results.3strat.RData")
  