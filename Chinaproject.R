library(readxl)
library(tseries)
library(urca)
library(ggplot2)
library(dplyr)
library(zoo)
library(xts)
library(ggfortify)
library(dyn)
library(lmtest)
library(car)

install.packages("MASS")
install.packages("reshape2")
install.packages("data.table")
install.packages("stargazer")

library(MASS)
library(reshape2)
library(data.table)
library(stargazer)

Chinapanel <- read_excel("C:/Users/Charles Feng/Downloads/Rwork/China panel.xlsx", sheet = "Datasheet", col_names = TRUE)

library(tidyr)


Chinapanel <- Chinapanel %>%
  mutate(Province = as.factor(Province))

Summary(lm(Chinapanel$`WorkerOutput (RMB)` ~ Chinapanel$`PerCapPower (KWh)` + Chinapanel$Provine))

lm(Chinapanel$`PerCapPower (KWh)` ~ Province + Chinapanel$`PerCapDefense (RMB)` + Chinapanel$DepRatioOld + Chinapanel$`%GDPHousing`)

summary(Fittedlm)

ols_fe<- lm(Chinapanel$`WorkerOutput (RMB)`~ Chinapanel$+exp+State,data=state_wage)
summary(ols_fe)
wages_demeaned<-with(state_wage,data.frame(
  d_wage=wage- ave(wage,State)
  

  
#############
  

  #loading libraries
  
  install.packages("plm")
  library(plm)
  library(sandwich)
  
  
  data <- read_excel("C:/Users/Charles Feng/Downloads/Rwork/China panel.xlsx", sheet = "Datasheet", col_names = TRUE)
  
  names(data) <- trimws(names(data))  # remove any trailing spaces
  colnames(data)[colnames(data) == "%GDPHousing"] <- "gdp_housing_pct"
  colnames(data)[colnames(data) == "WorkerOutput (RMB)"] <- "output_per_worker"
  colnames(data)[colnames(data) == "PerCapDefense (RMB)"] <- "defense_pc"
  colnames(data)[colnames(data) == "PerCapPower (KWh)"] <- "power_pc"
  colnames(data)[colnames(data) == "DepRatioOld"] <- "dependency_ratio"
  
  # Convert identifiers to factors
  data$Year <- as.factor(data$Year)
  data$Province <- as.factor(data$Province)
  
  View(data)
  
  # Create panel structure
  pdata <- pdata.frame(data, index = c("Province", "Year"))
  
  # --- Step 1: TWFE model (no IV)
  pdata$squaredpower_pc <- pdata$power_pc^2
  
  twfe_model <- plm(output_per_worker ~ power_pc + (power_pc)^2 + gdp_housing_pct + dependency_ratio,
                    data = pdata,
                    model = "within",
                    effect = "twoways")
  
  summary(twfe_model)
  
  # Clustered SEs
  coeftest(twfe_model, vcov = vcovHC(twfe_model, type = "HC1", cluster = "group"))
  
  # --- Step 2: TWFE model with IV
  iv_model <- plm(output_per_worker ~ power_pc + gdp_housing_pct + dependency_ratio | . - power_pc + defense_pc,
                  data = pdata,
                  model = "within", 
                  effect = "twoways")
  
  summary(iv_model)
  
  # Clustered SEs
  coeftest(iv_model, vcov = vcovHC(iv_model, type = "HC1", cluster="group"))
  
