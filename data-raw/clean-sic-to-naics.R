################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)
library(readxl)
# library(concordance)

################################################################################
# SIC 1987 (combined) to NAICS 1997 (combined)
################################################################################
# load concordance data:
# Pierce and Schott 2018 <https://faculty.som.yale.edu/peterschott/international-trade-data/>
# Concordance of 1989-2017 US HS codes to US SIC, SITC and NAICS codes over time
# https://spinup-000d1a-wp-offload-media.s3.amazonaws.com/faculty/wp-content/uploads/sites/47/2019/06/hssicnaics_20181015.zip
s87n97.data.r <- read_excel("./data-raw/1987_SIC_to_1997_NAICS.xls")
n97s87.data.r <- read_excel("./data-raw/1997_NAICS_to_1987_SIC.xls")

# select variables
s87n97.data <- s87n97.data.r %>%
  select(SIC, `1997 NAICS`)

n97s87.data <- n97s87.data.r %>%
  select(SIC, `1997 NAICS`)

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

table(nchar(n97s87.data$SIC))

# test <- n97s87.data %>%
#   filter(nchar(SIC) == 3)

setdiff(n97s87.data$SIC, s87n97.data$SIC)
# missing SIC codes in s87n97.data: [1] "781"

# check digits before merging
table(nchar(s87n97.data$SIC))
table(nchar(n97s87.data$SIC))

# combine all
sic87_naics97.r <- rbind(s87n97.data, n97s87.data)


# change name
sic87_naics97 <- sic87_naics97.r %>%
  select(SIC, `1997 NAICS`) %>%
  rename(NAICS = `1997 NAICS`) 

# check digits
## SIC:3, 4, (2-4 three columns) NAICS: 2, 3, 5, 6 (2-6 five columns)
sic87_naics97$SIC%>%
  nchar()%>%
  table()

sic87_naics97$NAICS%>%
  nchar()%>%
  table()

# create new variables
sic87_naics97 <- sic87_naics97 %>%
  mutate(SIC_4d = str_sub(SIC, start = 1, end = 4),
         SIC_3d = str_sub(SIC, start = 1, end = 3),
         SIC_2d = str_sub(SIC, start = 1, end = 2),
         NAICS_6d = str_sub(NAICS, start = 1, end = 6),
         NAICS_5d = str_sub(NAICS, start = 1, end = 5),
         NAICS_4d = str_sub(NAICS, start = 1, end = 4),
         NAICS_3d = str_sub(NAICS, start = 1, end = 3),
         NAICS_2d = str_sub(NAICS, start = 1, end = 2)
  ) %>%
  arrange(SIC) %>%
  select(SIC_4d, SIC_3d, SIC_2d,
         NAICS_6d, NAICS_5d, NAICS_4d, NAICS_3d, NAICS_2d) %>%
  distinct()

sic87_naics97 %>%
  filter(is.na(SIC_2d) == TRUE)

# clean digits
sic87_naics97 <- sic87_naics97 %>%
  mutate(SIC_4d = ifelse(nchar(SIC_4d) == 4, SIC_4d, NA),
         SIC_3d = ifelse(nchar(SIC_3d) == 3, SIC_3d, NA),
         NAICS_6d = ifelse(nchar(NAICS_6d) == 6, NAICS_6d, NA),
         NAICS_5d = ifelse(nchar(NAICS_5d) == 5, NAICS_5d, NA),
         NAICS_4d = ifelse(nchar(NAICS_4d) == 4, NAICS_4d, NA),
         NAICS_3d = ifelse(nchar(NAICS_3d) == 3, NAICS_3d, NA)) %>%
  filter(!is.na(SIC_2d)) %>%
  filter(!is.na(NAICS_2d))

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
s87n02.data.r <- read_excel("./data-raw/1987_SIC_to_2002_NAICS.xls")
n02s87.data.r <- read_excel("./data-raw/2002_NAICS_to_1987_SIC.xls")

# select variables
s87n02.data <- s87n02.data.r %>%
  select(SIC, `2002 NAICS`)

n02s87.data <- n02s87.data.r %>%
  select(SIC, `2002 NAICS`)


# check
s87n02.data %>%
  pull(SIC) %>%
  unique() %>%
  sort()

n_distinct(s87n02.data$SIC)
#1005

n02s87.data %>%
  pull(SIC) %>%
  unique() %>%
  sort()

n_distinct(n02s87.data$`2002 NAICS`)
#1180

setdiff(s87n02.data$SIC, n02s87.data$SIC)
# missing SIC codes in n02s87.data: numeric(0)

setdiff(n02s87.data$SIC, s87n02.data$SIC)
# missing SIC codes in s87n02.data: numeric(0)

# check digits before merging
table(nchar(s87n02.data$SIC))
table(nchar(n02s87.data$SIC))

# combine all
sic87_naics02.r <- rbind(s87n02.data, n02s87.data)


# change name
sic87_naics02 <- sic87_naics02.r %>%
  select(SIC, `2002 NAICS`) %>%
  rename(NAICS = `2002 NAICS`) 

# check digits
## SIC:3, 4, NAICS: 6
sic87_naics02$SIC_10d%>%
  nchar()%>%
  table()

sic87_naics02$NAICS_10d%>%
  nchar()%>%
  table()

# create new variables
sic87_naics02 <- sic87_naics02 %>%
  mutate(SIC_4d = str_sub(SIC, start = 1, end = 4),
    SIC_3d = str_sub(SIC, start = 1, end = 3),
    SIC_2d = str_sub(SIC, start = 1, end = 2),
    NAICS_6d = str_sub(NAICS, start = 1, end = 6),
    NAICS_5d = str_sub(NAICS, start = 1, end = 5),
    NAICS_4d = str_sub(NAICS, start = 1, end = 4),
    NAICS_3d = str_sub(NAICS, start = 1, end = 3),
    NAICS_2d = str_sub(NAICS, start = 1, end = 2)
  ) %>%
  arrange(SIC) %>%
  select(SIC_4d, SIC_3d, SIC_2d,
         NAICS_6d, NAICS_5d, NAICS_4d, NAICS_3d, NAICS_2d) %>%
  distinct()

# clean digits
sic87_naics02 <- sic87_naics02 %>%
  mutate(SIC_4d = ifelse(nchar(SIC_4d) == 4, SIC_4d, NA),
         SIC_3d = ifelse(nchar(SIC_3d) == 3, SIC_3d, NA),
         NAICS_6d = ifelse(nchar(NAICS_6d) == 6, NAICS_6d, NA),
         NAICS_5d = ifelse(nchar(NAICS_5d) == 5, NAICS_5d, NA),
         NAICS_4d = ifelse(nchar(NAICS_4d) == 4, NAICS_4d, NA),
         NAICS_3d = ifelse(nchar(NAICS_3d) == 3, NAICS_3d, NA)) %>%
  filter(!is.na(SIC_2d))

# fix unusual 2-digit NAICS codes
sic87_naics02 <- sic87_naics02 %>%
  mutate(NAICS_2d = if_else(NAICS_2d == "31", "31-33", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "32", "31-33", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "33", "31-33", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "44", "44-45", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "45", "44-45", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "48", "48-49", NAICS_2d),
         NAICS_2d = if_else(NAICS_2d == "49", "48-49", NAICS_2d))

# save
save(sic87_naics02,
     file = "./data/sic87_naics02.RData", compress = "xz")

######
# check differences in naics 1997 and 2000 (need to load clean-sic-to-naics.R first)
naics97 <- sic87_naics97.r %>% 
  select('1997 NAICS') 

naics02 <- sic87_naics02.r %>% 
  select('2002 NAICS') 


dif9702 <- setdiff(naics02$'2002 NAICS', naics97$'1997 NAICS') %>%
  as_data_frame() %>%
  distinct()
View(dif9702)

# Compare with naics2002_desc, 115 different values
load("./data/naics2002_desc.RData")
dif97.desc02 <- setdiff(naics97$'1997 NAICS', naics2002_desc$code) %>%
  as_data_frame() %>%
  distinct()
View(dif97.desc02)

# Compare naics2002_desc with naics02, no difference
dif02.desc02 <- setdiff(naics02$'2002 NAICS', naics2002_desc$code) %>%
  as_data_frame() %>%
  distinct()
View(dif02.desc02)

# Need NAICS 1997 description



#########draft


