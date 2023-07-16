################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)
library(readxl)

################################################################################
# SIC 1987 (combined) to NAICS 1997 (combined)
################################################################################
# load concordance data:
# Pierce and Schott 2018 <https://faculty.som.yale.edu/peterschott/international-trade-data/>
# Concordance of 1989-2017 US HS codes to US SIC, SITC and NAICS codes over time
# https://spinup-000d1a-wp-offload-media.s3.amazonaws.com/faculty/wp-content/uploads/sites/47/2019/06/hssicnaics_20181015.zip
s87n97.data <- read_excel("1987_SIC_to_1997_NAICS.xls")
n97s87.data <- read_excel("1997_NAICS_to_1987_SIC.xls")

# align names of variables
s87n97.data$`NAICS Part Indicator` <- NA
s87n97.data$`SIC Part Indicator` <- s87n97.data$`Part Indicator`
s87n97.data$`Part Indicator` <- NULL

s87n97.data$`SIC Title and Part Description` <- s87n97.data$`SIC Titles and Part Descriptions`
s87n97.data$`SIC Titles and Part Descriptions` <- NULL

s87n97.data$`1997 NAICS Title` <- s87n97.data$`1997 NAICS Titles and Part Indicators`
s87n97.data$`1997 NAICS Titles and Part Indicators` <- NULL

# check
s87n97.data %>%
  pull(SIC) %>%
  unique() %>%
  sort()

n_distinct(s87n97.data$SIC)
#1087

n97s87.data %>%
  pull(SIC) %>%
  unique() %>%
  sort()

n_distinct(n97s87.data$SIC)
#1006

setdiff(s87n97.data$SIC, n97s87.data$SIC)
# missing SIC codes in n97s87.data: 
# [1] "01"   "02"   "07"   "08"   "09"   "10"   "12"  
# [8] "13"   "14"   "15"   "16"   "17"   "20"   "21"  
# [15] "22"   "23"   "24"   "25"   "26"   "27"   "28"  
# [22] "29"   "30"   "31"   "32"   "33"   "34"   "35"  
# [29] "36"   "37"   "38"   "39"   "41"   "42"   "43"  
# [36] "44"   "45"   "46"   "47"   "48"   "49"   "50"  
# [43] "51"   "52"   "53"   "54"   "55"   "56"   "57"  
# [50] "58"   "59"   "60"   "61"   "62"   "63"   "64"  
# [57] "65"   "67"   "70"   "72"   "73"   "75"   "76"  
# [64] "78"   "79"   "80"   "81"   "82"   "83"   "84"  
# [71] "86"   "87"   "88"   "89"   "91"   "92"   "93"  
# [78] "94"   "95"   "96"   "97"   "9999"

setdiff(n97s87.data$SIC, s87n97.data$SIC)
# missing SIC codes in s87n97.data: [1] "781"


# combine all
sic87_naics97 <- rbind(s87n97.data, n97s87.data)

#change the order of variables
#sic87_naics97 <- sic87_naics97[, c("C", "A", "B")]

# clean
sic87_naics97 <- sic87_naics97 %>%
  select(SIC, `1997 NAICS`) %>%
  filter(nchar(SIC) != ".") %>%
  filter(`1997 NAICS` != ".") %>%
  rename(SIC_10d = SIC,
         NAICS_6d = `1997 NAICS`) 

# check digits
## SIC:3, 4, NAICS: 2, 3, 5, 6
sic87_naics97$SIC_10d%>%
  nchar()%>%
  table()

sic87_naics97$NAICS_6d%>%
  nchar()%>%
  table()

# mutate
sic87_naics97 <- sic87_naics97 %>%
  mutate(#SIC_10d = str_pad(SIC_10d, 10, side = "left", pad = "0"),
         #SIC_6d = str_sub(SIC_10d, start = 1, end = 6),
         SIC_4d = str_sub(SIC_10d, start = 1, end = 4),
         SIC_2d = str_sub(SIC_10d, start = 1, end = 2),
         NAICS_5d = str_sub(NAICS_6d, start = 1, end = 5),
         NAICS_4d = str_sub(NAICS_6d, start = 1, end = 4),
         NAICS_3d = str_sub(NAICS_6d, start = 1, end = 3),
         NAICS_2d = str_sub(NAICS_6d, start = 1, end = 2)
         ) %>%
  arrange(SIC_10d) %>%
  select(SIC_4d, SIC_2d,
         NAICS_6d, NAICS_5d, NAICS_4d, NAICS_3d, NAICS_2d) %>%
  distinct()


#################################################
# combine
#hs_naics <- rbind(hs_naics, miss.rows) %>%
  #arrange(HS_6d)

# fix unusual 2-digit NAICS codes
sic87_naics97 <- sic87_naics97 %>%
  mutate(NAICS_2d = if_else(NAICS_2d == "31", "31-33", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "32", "31-33", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "33", "31-33", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "44", "44-45", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "45", "44-45", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "48", "48-49", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "49", "48-49", NAICS_2d))

# save
save(sic87_naics97,
     file = "./data/sic87_naics97.RData", compress = "xz")


################################################################################
# SIC 1987 to NAICS 2002 (not completed yet 07/15/2023)
################################################################################

# s87n02.data <- read_excel("1987_SIC_to_2002_NAICS.xls")
# n02s87.data <- read_excel("2002_NAICS_to_1987_SIC.xls")
# 
# s87n02.data %>%
#   pull(SIC) %>%
#   unique() %>%
#   sort()
# 
# n_distinct(s87n02.data$SIC)
# 
# n02s87.data %>%
#   pull(SIC) %>%
#   unique() %>%
#   sort()
# 
# n_distinct(n02s87.data$`2002 NAICS`)
# 
# 
# setdiff(exp.data$commodity, imp.data$commodity)
# setdiff(imp.data$commodity, exp.data$commodity)


