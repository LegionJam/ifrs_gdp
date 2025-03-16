## ===============================================================
## READ DATA
## ===============================================================


# package: readxl, WDI
library(readxl)
library(WDI)

# Penn World Table DataSet
#pwt1001 <- read_excel("C:/Users/klari/OneDrive - Corvinus University of Budapest/8_Research/Accounting and Macro/original data files/pwt1001.xlsx", 
#                      sheet = "Data")

#IRFS dátumok
ifrs <- read_excel("C:/Users/klari/OneDrive - Corvinus University of Budapest/8_Research/Accounting and Macro/original data files/ifrs_data.xlsx", 
                    sheet = "R_input", range = "B1:D169")

#kausar-park sample
kausar_park <- read_excel("C:/Users/klari/OneDrive - Corvinus University of Budapest/8_Research/Accounting and Macro/original data files/kausar_park_sample.xlsx",
                    sheet = "countries", range = "B1:C33")


# WorldBank dataset
wb_data <- WDI(
  country = "all",
  indicator = c('GDP' = 'NY.GDP.MKTP.CN',
                'GOV' = 'NE.CON.GOVT.CN',
                'infl_CPI' = 'FP.CPI.TOTL.ZG',
                'infl_GDPdefl' = 'NY.GDP.DEFL.KD.ZG',  
                'unempl' = 'SL.UEM.TOTL.ZS',
                'export' = 'NE.EXP.GNFS.KD',  
                'import' = 'NE.IMP.GNFS.KD',  
                'industry_va' = 'NV.IND.TOTL.CN',
                'investment' = 'NE.GDI.FTOT.CN'),
  start = 2000,
  end = 2023,
  extra = FALSE,
  cache = NULL,
  latest = NULL,
  language = "en"
)
# mentsük el
save(wb_data, file = "wb_data.RData")


## ===============================================================
## DATA MANIPULATION
## ===============================================================
library(tidyverse)

# átnevezzük az országkódot és eldobjuk a felesleges változókat
wb_data <- wb_data %>%
  rename(ccode = iso3c) %>%
  select(-iso2c)

# merge with irfs dataset
mydatafr <- merge(wb_data, ifrs, by = "ccode") 
mydatafr <- mydatafr %>%
  select(-Countryname)

# set kausar-park sample
mydatafr <- merge(mydatafr, kausar_park, by = "ccode", all.x = TRUE)
mydatafr$kausar_park_sample <- FALSE
mydatafr <- mydatafr %>%
  mutate(kausar_park_sample = if_else(ccode %in% kausar_park$ccode, TRUE, kausar_park_sample)) %>%
  select(-Country)

# treatment dummy
mydatafr <- mydatafr %>%
  mutate(ifrs_treat = if_else(year >= ifrs_year, 1, 0))

# elmentjük az adatfilet
save(mydatafr,file="dataframe_ifrs.Rdata")

