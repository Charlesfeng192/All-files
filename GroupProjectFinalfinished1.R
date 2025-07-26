
#Loading in libraries

install.packages("readxl")
install.packages("plm")
install.packages("ggplot2")

library(readxl)
library(dplyr)
library(plm)
library(ggplot2)

#Loading in the datasheet for our first OLS reg
# Load the data
file_path <- "C:/Users/charl/OneDrive/Desktop/RDirect/AllChinaData.xlsx"
df <- read_excel(file_path, sheet = 1)

#An issue is that with overly lengthy numbers, R thinks it's a character.

# Checking the type of every variable
View(df)

#creating a time trend to hopefully remove time effects while preserving DoF

df$year_trend <- df$Year - min(df$Year)

str(df)

# Convert to panel data
df_panel <- pdata.frame(df, index = c("Province", "Year"))

View(df_panel)

colnames(df_panel)

#it's important that each variable is correctly scaled to population or workers, respectively. 

#10,000's of broadband units, per population. 
#divide by population, multiply by 100 to get broadband units per 1 million people

df_panel$BBroadband <- 1000000*df_panel$Broadband/df_panel$Population

df_panel$lBBroadband <- log(df_panel$BBroadband)

#output per worker

df_panel$lwoutput <- log(df_panel$WorkerOutput)

#Next, tap water access per million

df_panel$wateraccess <- 1000000*df$Water/df$Population

df_panel$lwateraccess <- log(df_panel$wateraccess)

#Energyconsumption

df_panel$Energypm <- 1000000*df_panel$Energy.Consumption/df_panel$Population

df_panel$lenergy <- log(df_panel$Energypm)

#Highwayutilization

df_panel$Highwayphm <- 100000000*df_panel$Highway.length/df_panel$Population

df_panel$lHighwayphm <- log(df_panel$Highwayphm)


#Visualizing our Infrastructure Variables: 

ggplot(df_panel, aes(x = df_panel$lBBroadband, y = df_panel$lwoutput)) +
  geom_point(alpha = 0.4) +
  labs(title = "Pooled Plot: log broadband access vs log worker output",
       x = "Log Broadband Access", y = "Log Worker Output") +
  theme_minimal()

ggplot(df_panel, aes(x = df_panel$lHighwayphm, y = df_panel$lwoutput)) +
  geom_point(alpha = 0.4) +
  labs(title = "Pooled Plot: log highways per 100M vs log worker output",
       x = "Log Highways per 100 Million", y = "Log Worker Output") +
  theme_minimal()








#Checking for multicollinearity

cor(df_panel %>% select(lwoutput, Quake, lBBroadband, lwateraccess, lHighwayphm, lenergy), use = "complete.obs")

#highways and broadband are promising- they have real implied predictive power in regards to our dep. var 

model1 <- plm(lwoutput ~  
              + lHighwayphm + year_trend, 
    data=df_panel,
    model="within",
    effect="individual")

summary(model1)

#Now, to load in our independent controls:
#College:

df_panel$Collegepm <- 1000000*df_panel$CollegeRate
df_panel$lCollege <- log(df_panel$Collegepm)


#Patents:

df_panel$PatentsPH <- 100000*df_panel$Patents/df_panel$Population
df_panel$lPatent <- log(df_panel$PatentsPH)


#NONCURRENT ASSETS:

df_panel$workersassets <- 100000*df_panel$NCAssets/df_panel$Workers...4
df_panel$lassets <- log(df_panel$workersassets)


#Test the significance of the coefficient of broadband on earthquake

install.packages("fixest")
library(fixest)

#Probit model:

help(feglm)

Quake_probit <- feglm(
  Quake ~ lHighwayphm + lassets + lCollege + lPatent | Province + Year,
  data = df_panel, 
  family = binomial(link = "probit")
)

summary(Quake_probit)

library(sandwich)
library(lmtest)

#creating clustered robust standard errors for t-testing

Quake_robust <- sandwich(Quake_probit, vcov = vcovHC, type = "HC1")

#joint significance test- Highway coefficient IS significant.

coeftest(Quake_probit, vcov = Quake_robust)

#We observe that highway is significant at the 10% level
#This gives us confidence in our instrument approach. 


#Conducting a regression, we fit the highway construction due to instruments:

IVModel1 <- plm(
  lHighwayphm ~ Quake + lCollege + lPatent + lassets,
  data = df_panel,
  model = "within",         
  effect = "individual"
)

summary(IVModel1)

vcov_nw_1 <- vcovNW(IVModel1, lag = 4, prewhite = TRUE)

coeftest(IVModel1, vcov = vcov_nw_1)


#Variance-covariance matrix:

summary(IVModel1, cluster = c("individual", "time"))

#Correlation matrix of the coefficients of first IVModel1

cor_beta <- cov2cor(vcov_nw_1)

print(cor_beta)

#It's obvious that our instrument is independent of the rest of the model. 







#Fit Highways to instruments:

df_panel$HighwayIV <- 0.0245*df_panel$Quake

#2ndRegression, TWFE


IVModel2 <- plm(
  lwoutput ~ HighwayIV + lCollege + lPatent + lassets,
  data = df_panel,
  model = "within",
  effect = "twoways"
)

summary(IVModel2)

#Het-scedast. and autocorr. robust standard errors at 4th lag- NWHACSE

vcov_nw <- vcovNW(IVModel2, lag = 4, prewhite = TRUE)

#test for IV model:

coeftest(IVModel2, vcov = vcov_nw)

#highway is significant at the 5% level, using our robust t-test

#Hausman-wu test of IV significance

#Finally, we're curious about whether our model achieves a higher efficiency level than a normal OLS

#Estimate OLS:

OLSModel <- plm(lwoutput ~ lHighwayphm + lCollege + lPatent + lassets,
                data = df_panel,
                model = "within",
                effect = "twoways")


summary(OLSModel)

phtest(OLSModel,IVModel2)

#Chi squared p-value of 43, p-value significant at 1% significance level. Our OLS model IS inconsistent. 



#one last robustness check:

# Drop specific provinces
pdata_reduced <- subset(df_panel, !(Province %in% c("Sichuan", "Xinjiang")))

# Check
View(pdata_reduced)
table(pdata_reduced$province)

# Re-run plm model on the reduced dataset

RedIVModel1 <- plm(
  lHighwayphm ~ Quake + lCollege + lPatent + lassets,
  data = pdata_reduced,
  model = "within",         
  effect = "twoways"
)

summary(RedIVModel1)

pdata_reduced$fitHighway <- pdata_reduced$Quake*.0212

IVModel2 <- plm(
  lwoutput ~ fitHighway + lCollege + lPatent + lassets,
  data = pdata_reduced,
  model = "within",
  effect = "twoways"
)

summary(IVModel2)

#omitting the earthquake outliers of Xinjiang and Sichuan didn't change our results.



#creating tables for our document

install.packages("xtable")
library(xtable)

#Generating individual summary statistics

vars <- c("lwoutput", "lHighwayphm", "lCollege", "lPatent","lassets")

# Step 1: individual means
df_individual_means <- df_panel %>%
  group_by(Province) %>%
  summarize(across(all_of(vars), mean, na.rm = TRUE))

# Step 2: overall stats
summary_stats <- df_individual_means %>%
  summarize(across(
    all_of(vars),
    list(mean = ~mean(.x, na.rm = TRUE),
         sd   = ~sd(.x, na.rm = TRUE),
         min  = ~min(.x, na.rm = TRUE),
         max  = ~max(.x, na.rm = TRUE))
  ))

xtable(summary_stats)

save(df, file = "Chinadata.RData")

load("Chinadata.RData")

