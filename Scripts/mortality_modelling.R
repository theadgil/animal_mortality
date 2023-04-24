################################################################
################################################################
#####               R-Script: Mortality modelling          #####
#####      Dorothea Gilbert & Matz A. Haugen (2023):       #####
#####                      03.04.2023				               #####
################################################################
################################################################

# install packages

#install.packages("tseries")
#install.packages("AICmodavg")
#install.packages("reshape2")


# load packages

library(tseries)      # Run's test
library(AICcmodavg)   # AIC
library(reshape2)     


#setwd("~/RawData/OutFiles")



###########################################################################
#### Define equations
###########################################################################

fit_Gompertz <- function(y) nls(y ~ b*k*exp(-exp(a-b*weeks))*exp(a-b*weeks),
                                data=data.frame(y=y,df),
                                start=c(k=3, a=5, b=0.5),
                                lower = c(-Inf, -Inf, 0.1),
                                algorithm = "port")


fit_Logistic <- function(y) nls(y ~ b / (1 + exp(a-b*weeks))*(k-(k / (1 + exp(a-b*weeks)))),
                                data=data.frame(y=y,df),
                                start=c(k=3, a=5, b=0.5),
                                lower = c(-Inf, -Inf, 0.1),
                                algorithm = "port")



###########################################################################
#### Fit the Gompertz function and the Logistic function to normalized data
###########################################################################

# load raw data (weekly deaths)

mort <- read.csv("~/Corona/Manuscript/RawData/deaths.csv", header=TRUE, sep = ";")

#str(mort)

# normalize all data

norm <- mort 
for (k in 2:(ncol(mort ))){
  for (i in 1:(nrow(mort ))){
    norm[i,k] <- (mort [i,k] - min(mort [,k], na.rm=T))/(max(mort [,k], na.rm=T) - min(mort [,k], na.rm=T))
  }}
norm$date <- seq.Date(from = as.Date('2020-03-01'), to = as.Date('2020-06-20'), by = 'weeks')


# number of weeks elapsed

norm$weeks <- seq(1:nrow(mort))-1



# combined bird data set 

birds <- data.frame(
  weeks = rep(norm$weeks,2),
  birds = c(norm$birds_GER, norm$birds_LUX),
  birds2 = c(norm$birds_GER, norm$birds_LUX2)
  )



# fit Gompertz and Logistic model 

df <- norm     # normalized animal and human deaths
mort.Gompertz <- lapply(df[,c(2:(ncol(df)-2))], fit_Gompertz) 
mort.Logistic <- lapply(df[,c(2:(ncol(df)-2))], fit_Logistic)

df <- birds    # combined bird data sets
birds.Gompertz <- lapply(df[,c(2:(ncol(df)))], fit_Gompertz) 
birds.Logistic <- lapply(df[,c(2:(ncol(df)))], fit_Logistic)



# add bird data to mort.Gompertz and mort.Logistic

mort.Gompertz$birds <- birds.Gompertz$birds
mort.Gompertz$birds2 <- birds.Gompertz$birds2

mort.Logistic$birds <- birds.Logistic$birds
mort.Logistic$birds2 <- birds.Logistic$birds2

# Collect Gompertz model curve fit data

fit.Gompertz <- data.frame()
for (i in 1:length(mort.Gompertz)){
  out <- data.frame(
    name=names(mort.Gompertz)[i],
    k=coefficients(mort.Gompertz[[i]])["k"],
    RSE_k=summary(mort.Gompertz[[i]])$parameters["k","Std. Error"] / coefficients(mort.Gompertz[[i]])["k"],
    
    a=coefficients(mort.Gompertz[[i]])["a"],
    RSE_a=summary(mort.Gompertz[[i]])$parameters["a","Std. Error"] / coefficients(mort.Gompertz[[i]])["a"],
    
    b=coefficients(mort.Gompertz[[i]])["b"],
    RSE_b=summary(mort.Gompertz[[i]])$parameters["b","Std. Error"] / coefficients(mort.Gompertz[[i]])["b"],
    
    RSE=c(summary(mort.Gompertz[[i]])$sigma),
    RSS=c(sum(resid(mort.Gompertz[[i]])^2)),
    df=summary(mort.Gompertz[[i]])$df[2],
    
    t_k=summary(mort.Gompertz[[i]])$parameters["k","t value"],
    t_a=summary(mort.Gompertz[[i]])$parameters["a","t value"],
    t_b=summary(mort.Gompertz[[i]])$parameters["b","t value"],
    P_k=summary(mort.Gompertz[[i]])$parameters["k","Pr(>|t|)"],
    P_a=summary(mort.Gompertz[[i]])$parameters["a","Pr(>|t|)"],
    P_b=summary(mort.Gompertz[[i]])$parameters["b","Pr(>|t|)"],
    Shapiro_p.value=shapiro.test(resid(mort.Gompertz[[i]]))$p.value,
    Runs_p.value=runs.test(factor(sign(resid(mort.Gompertz[[i]]))))$p.value
  )
  fit.Gompertz <- rbind(fit.Gompertz,out)
}

# sort alphabetically

fit.Gompertz <- fit.Gompertz[order(fit.Gompertz$name),]


# Collect Logistic model curve fit data

fit.Logistic <- data.frame()
for (i in 1:length(mort.Logistic)){
  out <- data.frame(
    name=names(mort.Logistic)[i],
    k=coefficients(mort.Logistic[[i]])["k"],
    RSE_k=summary(mort.Logistic[[i]])$parameters["k","Std. Error"] / coefficients(mort.Logistic[[i]])["k"],
    
    a=coefficients(mort.Logistic[[i]])["a"],
    RSE_a=summary(mort.Logistic[[i]])$parameters["a","Std. Error"] / coefficients(mort.Logistic[[i]])["a"],
    
    b=coefficients(mort.Logistic[[i]])["b"],
    RSE_b=summary(mort.Logistic[[i]])$parameters["b","Std. Error"] / coefficients(mort.Logistic[[i]])["b"],
    
    RSE=c(summary(mort.Logistic[[i]])$sigma),
    RSS=c(sum(resid(mort.Logistic[[i]])^2)),
    df=summary(mort.Logistic[[i]])$df[2],
    
    t_k=summary(mort.Logistic[[i]])$parameters["k","t value"],
    t_a=summary(mort.Logistic[[i]])$parameters["a","t value"],
    t_b=summary(mort.Logistic[[i]])$parameters["b","t value"],
    P_k=summary(mort.Logistic[[i]])$parameters["k","Pr(>|t|)"],
    P_a=summary(mort.Logistic[[i]])$parameters["a","Pr(>|t|)"],
    P_b=summary(mort.Logistic[[i]])$parameters["b","Pr(>|t|)"],
    Shapiro_p.value=shapiro.test(resid(mort.Logistic[[i]]))$p.value,
    Runs_p.value=runs.test(factor(sign(resid(mort.Logistic[[i]]))))$p.value
  )
  fit.Logistic <- rbind(fit.Logistic,out)
}

# sort alphabetically

fit.Logistic <- fit.Logistic[order(fit.Logistic$name),]


###########################################################################
#### Time to peak, max growth rate and maxima (Gompertz)
###########################################################################

ymax <- c(unlist(lapply(mort[,c(2:(ncol(mort)))], max, na.rm=T)),
          max(c(mort$birds_GER + mort$birds_LUX),na.rm=T),
          max(c(mort$birds_GER + mort$birds_LUX2),na.rm=T))
names(ymax)[31:32] <- c("birds", "birds2")

Gomp.est <- data.frame(
  name = names(mort.Gompertz),
  max_growthrate = fit.Gompertz$b*fit.Gompertz$k / exp(1),
  RSE_max.growthrate = sqrt(fit.Gompertz$RSE_b^2 + fit.Gompertz$RSE_k^2),
  
  peak = fit.Gompertz$a / fit.Gompertz$b,
  RSE_peak = sqrt(fit.Gompertz$RSE_a^2 + fit.Gompertz$RSE_b^2),
  
  ymax_observed = ymax,
  max_weekly_est = ymax*fit.Gompertz$k / exp(1),
  max_cumul_est = ymax*fit.Gompertz$k
)

# sort alphabetically 

Gomp.est <- Gomp.est[order(Gomp.est$name),]

###########################################################################
#### Model comparison (Gompertz vs Logistic)
###########################################################################

# compare models using AIC

# AIC OUTPUT INTERPRETATION
# AICc: The information score of the model 
# (the lower-case 'c' indicates that the value has been calculated 
# from the AIC test corrected for small sample sizes). 
# The smaller the AIC value, the better the model fit.



# collect AIC results in fit_AIC dataframe

fit_AIC <- data.frame()

for (i in 1:length(mort.Logistic)){
  
  out <- data.frame(
    dataset=c(names(mort.Gompertz)[i], names(mort.Logistic)[i]),
    name=aictab(cand.set = list(mort.Gompertz[[i]],mort.Logistic[[i]]), modnames = c("Gompertz","Logistic"))$Modnames,
    AICc=aictab(cand.set = list(mort.Gompertz[[i]],mort.Logistic[[i]]), modnames = c("Gompertz","Logistic"))$AICc
  )
  fit_AIC <- rbind(fit_AIC,out)
}


# collect curve fit RSE and RSS in fit_stat dataframe

fit_stat <- data.frame()

for (i in 1:length(mort.Logistic)){
  
  out <- data.frame(
    dataset=c(names(mort.Gompertz)[i], names(mort.Logistic)[i]),
    name=c("Gompertz","Logistic"),
    RSE=c(summary(mort.Gompertz[[i]])$sigma,summary(mort.Logistic[[i]])$sigma),
    RSS=c(sum(resid(mort.Gompertz[[i]])^2),sum(resid(mort.Logistic[[i]])^2)),
    LL=c(logLik(mort.Gompertz[[i]]),logLik(mort.Logistic[[i]]))
  )
  fit_stat <- rbind(fit_stat,out)
}



# transpose into wide format

fit.AICc <- dcast(fit_AIC, dataset  ~ name, value.var="AICc",sum)
fit.RSE <- dcast(fit_stat, dataset  ~ name, value.var="RSE",sum)
fit.RSS <- dcast(fit_stat, dataset  ~ name, value.var="RSS",sum)
fit.LL <- dcast(fit_stat, dataset  ~ name, value.var="LL",sum)

# check differences

fit.AICc$Delta <- fit.AICc$Gompertz - fit.AICc$Logistic
fit.RSE$Delta <- fit.RSE$Gompertz - fit.RSE$Logistic
fit.RSS$Delta <- fit.RSS$Gompertz - fit.RSS$Logistic
fit.LL$Delta <- fit.LL$Gompertz - fit.LL$Logistic

colnames(fit.AICc) <- c("name","Gompertz_AICc", "Logistic_AICc", "Delta_AICc")
colnames(fit.RSE) <- c("name","Gompertz_RSE", "Logistic_RSE", "Delta_RSE")
colnames(fit.RSS) <- c("name","Gompertz_RSS", "Logistic_RSS", "Delta_RSS")
colnames(fit.LL) <- c("name","Gompertz_LogLik", "Logistic_LogLik", "Delta_LogLik")

fit.wide <- cbind(fit.AICc, fit.RSE[,-1], fit.RSS[,-1], fit.LL[,-1])


# write to CSV-file

#write.table(Gomp.est, file="Gompertz_estimates.csv",sep=";",row.names=T)
#write.table(fit.Gompertz, file="Gompertz_model_pars.csv",sep=";",row.names=T)
#write.table(fit.Logistic, file="Logistic_model_pars.csv",sep=";",row.names=T)
#write.table(fit.wide, file="model_comparison.csv",sep=";",row.names=T)


