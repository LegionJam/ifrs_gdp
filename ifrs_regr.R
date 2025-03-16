# clear start
  rm(list=ls())
  cat("\014")

# Adatok beolvasása: mydatafr nevű objektumba
  load("dataframe_ifrs.Rdata")

# további adatmanipuláció
library(dplyr)
  
# growth rates  
  mydatafr <- mydatafr %>%
    arrange(ccode, year) %>%    
    group_by(ccode) %>%
    mutate(GDP_growth = c(NA, diff(GDP))/lag(GDP,1)) %>%
    mutate(GDP_growth = GDP_growth*100) %>%
    mutate(IndVA_growth = c(NA,diff(industry_va))/lag(industry_va,1)) %>%
    mutate(IndVA_growth = IndVA_growth*100)
  

## ===============================================================
## ANALYSIS
## ===============================================================
library(stargazer)
library(broom)

# regression
  model_ols_1 <- lm(GDP_growth ~ IndVA_growth + infl_CPI + unempl + ifrs_treat, data = mydatafr)
  summary(model_ols_1)
  
# év dummyk beillesztése
  mydatafr$yeard <- factor(mydatafr$year)
  model_ols_2 <- lm(GDP_growth ~ IndVA_growth + infl_CPI + unempl + ifrs_treat + yeard, data = mydatafr)
  summary(model_ols_2)
  
# Regressziós eredmény képernyőre blook package-el
  ols_models <- list(model_ols_1, model_ols_2)
  lapply(ols_models, tidy)
  

# panel regresszió
library(plm)

# Adatok panel formátumba alakítása
  panel_data <- pdata.frame(mydatafr, index = c("ccode", "year"))

# Fixált hatások panel regresszió futtatása
  model_fixed_1 <- plm(GDP_growth ~ IndVA_growth + infl_CPI + unempl + ifrs_treat, data = panel_data, model = "within")

# Eredmények kiírása
  summary(model_fixed_1)

# Eredmények kiiratása    
# LaTeX kimenet
  stargazer(model_ols_1, model_ols_2, model_fixed_1, type = "latex", 
            digits = 3, 
            title = "GDP növekedési ütem becslése",
            report = "vc*s",
            out = "regression_table_1.tex") # Különböző statisztikák: koefficiens, p-érték, std. hiba stb.
  

