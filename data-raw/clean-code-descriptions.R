################################################################################
## setup
################################################################################
# clean slate
rm(list = ls())
date()

# load packages
library(tidyverse)
library(jsonlite)
library(readxl)
library(stringr)

# # load previously cleaned data
# load("./data/codedesc.rda")

################################################################################
## NAICS 1997
################################################################################
# load downloaded data from Census
# https://www.census.gov/naics/?68967
naics1997_desc



################################################################################
## NAICS 2002
################################################################################
# load downloaded data from Census
# https://www.census.gov/eos/www/naics/reference_files_tools/2002/naics_6_02.txt
naics2002_desc <- read_table("./data-raw/naics_2_6_02.txt",
                             skip = 8, col_types = "cc",  col_names = FALSE)

# rename
names(naics2002_desc)

naics2002_desc <- naics2002_desc %>%
  rename(code = X1,
         desc  = X2) %>%
  mutate(code = str_trim(code, "both"),
         desc = str_replace_all(desc, "[\"]", ""))

# save
save(naics2002_desc,
     file = "./data/naics2002_desc.RData", compress = "xz")


################################################################################
## NAICS 2007
################################################################################
# load downloaded data from Census
# https://www.census.gov/eos/www/naics/reference_files_tools/2007/naics07_6.txt
naics2007_desc <- read_table("./data-raw/naics07.txt",
                             skip = 2, col_types = "ccc", col_names = FALSE)

# rename
names(naics2007_desc)

naics2007_desc <- naics2007_desc %>%
  select(X2, X3) %>%
  rename(code = X2,
         desc  = X3) %>%
  mutate(code = str_trim(code, "both"),
         desc = str_replace_all(desc, "[\"]", ""))

# save
save(naics2007_desc,
     file = "./data/naics2007_desc.RData", compress = "xz")


################################################################################
## NAICS 2012
################################################################################
# load downloaded data from BLS
# https://data.bls.gov/cew/apps/bls_naics/v2/bls_naics_app.htm#tab=download&naics=2012
naics2012_desc <- read_csv("./data-raw/2012_titles_descriptions.csv", col_types = "cccc")

# rename
names(naics2012_desc)

naics2012_desc <- naics2012_desc %>%
  select(NAICS, `2012 NAICS Full Title`) %>%
  rename(code = NAICS,
         desc  = `2012 NAICS Full Title`) %>%
  mutate(desc = str_replace_all(desc, "[\"]", ""))

# save
save(naics2012_desc,
     file = "./data/naics2012_desc.RData", compress = "xz")


################################################################################
## NAICS 2017
################################################################################
# load downloaded data from BLS
# https://data.bls.gov/cew/apps/bls_naics/v2/bls_naics_app.htm#tab=download&naics=2017
naics2017_desc <- read_csv("./data-raw/2017_titles_descriptions.csv", col_types = "cccc")

# rename
names(naics2017_desc)

naics2017_desc <- naics2017_desc %>%
  select(NAICS, `2017 NAICS Full Title`) %>%
  rename(code = NAICS,
         desc  = `2017 NAICS Full Title`) %>%
  mutate(desc = str_replace_all(desc, "[\"]", ""))

# save
save(naics2017_desc,
     file = "./data/naics2017_desc.RData", compress = "xz")


################################################################################
## HS0
################################################################################
# https://comtrade.un.org/data/cache/classificationH0.json
# read json
hs0.desc.r <- fromJSON(file.path("./data-raw", "classificationH0.json"))

hs0_desc <- hs0.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# save
save(hs0_desc,
     file = "./data/hs0_desc.RData", compress = "xz")


################################################################################
## HS1
################################################################################
# https://comtrade.un.org/data/cache/classificationH1.json
# read json
hs1.desc.r <- fromJSON(file.path("./data-raw", "classificationH1.json"))

hs1_desc <- hs1.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# save
save(hs1_desc,
     file = "./data/hs1_desc.RData", compress = "xz")


################################################################################
## HS2
################################################################################
# https://comtrade.un.org/data/cache/classificationH2.json
# read json
hs2.desc.r <- fromJSON(file.path("./data-raw", "classificationH2.json"))

hs2_desc <- hs2.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# save
save(hs2_desc,
     file = "./data/hs2_desc.RData", compress = "xz")


################################################################################
## HS3
################################################################################
# https://comtrade.un.org/data/cache/classificationH3.json
# read json
hs3.desc.r <- fromJSON(file.path("./data-raw", "classificationH3.json"))

hs3_desc <- hs3.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# save
save(hs3_desc,
     file = "./data/hs3_desc.RData", compress = "xz")


################################################################################
## HS4
################################################################################
# https://comtrade.un.org/data/cache/classificationH4.json
# read json
hs4.desc.r <- fromJSON(file.path("./data-raw", "classificationH4.json"))

hs4_desc <- hs4.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# save
save(hs4_desc,
     file = "./data/hs4_desc.RData", compress = "xz")


################################################################################
## HS5
################################################################################
# https://comtrade.un.org/data/cache/classificationH5.json
# read json
hs5.desc.r <- fromJSON(file.path("./data-raw", "classificationH5.json"))

hs5_desc <- hs5.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# save
save(hs5_desc,
     file = "./data/hs5_desc.RData", compress = "xz")


################################################################################
## HS6
################################################################################
# https://comtrade.un.org/data/cache/classificationH6.json
# read json
hs6.desc.r <- fromJSON(file.path("./data-raw", "classificationH6.json"))

hs6_desc <- hs6.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# save
save(hs6_desc,
     file = "./data/hs6_desc.RData", compress = "xz")


################################################################################
## HS Combined
################################################################################
# combine all HS codes
hs_desc <- rbind(hs0_desc %>% mutate(classification = "HS0"),
                 hs1_desc %>% mutate(classification = "HS1"),
                 hs2_desc %>% mutate(classification = "HS2"),
                 hs3_desc %>% mutate(classification = "HS3"),
                 hs4_desc %>% mutate(classification = "HS4"),
                 hs5_desc %>% mutate(classification = "HS5"),
                 hs6_desc %>% mutate(classification = "HS6")
                 )

# drop duplicates
hs_desc <- hs_desc %>%
  distinct_at(vars(code, desc), .keep_all = TRUE) %>%
  arrange(code)

# paste desc
hs_desc <- hs_desc %>%
  mutate(desc = paste(desc, " (", classification, ")", sep = ""))

# check multiple entries
check.df <- hs_desc %>%
  group_by(code) %>%
  mutate(n = length(desc)) %>%
  filter(n > 1)

# append desc from different years
hs_desc <- hs_desc %>%
  group_by(code) %>%
  mutate(desc = paste0(desc, collapse = "; "),
         n = length(desc)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(desc = ifelse(n == 1, str_replace_all(desc, "\\(HS.*\\)$", ""), desc),
         desc = str_trim(desc, side = "right")) %>%
  select(code, desc)

# save
save(hs_desc,
     file = "./data/hs_desc.RData", compress = "xz")


################################################################################
## ISIC2
################################################################################
# load UN data
# https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/ISIC_Rev_2_english_structure.txt
isic2.desc.r <- read_table("./data-raw/ISIC_Rev_2_english_structure.txt")

isic2_desc <- isic2.desc.r %>%
  rename(code = Code,
         desc = Description)

# add description from World Bank
isic2.augment <- tibble(code = c("99", "999", "9999"),
                        desc = rep("Goods not elsewhere classified", 3))

isic2_desc <- rbind(isic2_desc,
                    isic2.augment)

# save
save(isic2_desc,
     file = "./data/isic2_desc.RData", compress = "xz")


################################################################################
## ISIC3
################################################################################
# load UN data
# https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/ISIC_Rev_3_english_structure.txt
isic3.desc.r <- read_table("./data-raw/ISIC_Rev_3_english_structure.txt")

isic3_desc <- isic3.desc.r %>%
  rename(code = Code,
         desc = Description)

# add description from World Bank
isic3.augment <- tibble(code = c("999", "9999"),
                        desc = rep("Goods not elsewhere classified", 2))

isic3_desc <- rbind(isic3_desc,
                    isic3.augment)

# save
save(isic3_desc,
     file = "./data/isic3_desc.RData", compress = "xz")


################################################################################
## ISIC3.1
################################################################################
# load UN data
# https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/ISIC_Rev_3_1_english_structure.txt
isic3.1.desc.r <- read_delim("./data-raw/ISIC_Rev_3_1_english_structure.txt", delim = ",")

isic3.1_desc <- isic3.1.desc.r %>%
  rename(code = Code,
         desc = Description)

# add description from World Bank
isic3.1.augment <- tibble(code = c("999", "9999"),
                          desc = rep("Goods not elsewhere classified", 2))

isic3.1_desc <- rbind(isic3.1_desc,
                      isic3.1.augment)

# save
save(isic3.1_desc,
     file = "./data/isic3.1_desc.RData", compress = "xz")


################################################################################
## ISIC4
################################################################################
# load UN data
# https://unstats.un.org/unsd/classifications/Econ/Download/In%20Text/ISIC_Rev_4_english_structure.txt
isic4.desc.r <- read_delim("./data-raw/ISIC_Rev_4_english_structure.txt", delim = ",")

isic4_desc <- isic4.desc.r %>%
  rename(code = Code,
         desc = Description)

# save
save(isic4_desc,
     file = "./data/isic4_desc.RData", compress = "xz")


################################################################################
## SITC1
################################################################################
# https://comtrade.un.org/data/cache/classificationS1.json
# read json
sitc1.desc.r <- fromJSON(file.path("./data-raw", "classificationS1.json"))

sitc1.desc <- sitc1.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# add 5 digit description
sitc1.desc.5d.1 <- sitc1.desc %>%
  filter(nchar(code) == 4) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc1.desc.5d.2 <- sitc1.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc1.desc.5d.3 <- sitc1.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc1.desc.5d.4 <- sitc1.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

# add 4-digit description
sitc1.desc.4d.1 <- sitc1.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc1.desc.4d.2 <- sitc1.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc1.desc.4d.3 <- sitc1.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

# add 3-digit description
sitc1.desc.3d.1 <- sitc1.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

sitc1.desc.3d.2 <- sitc1.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

# add 2-digit description
sitc1.desc.2d <- sitc1.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 2, side = "right", pad = "0"))

# combine
sitc1.desc <- rbind(sitc1.desc,
                    sitc1.desc.5d.1, sitc1.desc.5d.2, sitc1.desc.5d.3, sitc1.desc.5d.4,
                    sitc1.desc.4d.1, sitc1.desc.4d.2, sitc1.desc.4d.3,
                    sitc1.desc.3d.1, sitc1.desc.3d.2,
                    sitc1.desc.2d) %>%
  arrange(code) %>%
  distinct()

# check
sitc1.desc[duplicated(sitc1.desc$code),]

sitc1_desc <- sitc1.desc[!duplicated(sitc1.desc$code),]

# save
save(sitc1_desc,
     file = "./data/sitc1_desc.RData", compress = "xz")


################################################################################
## SITC2
################################################################################
# https://comtrade.un.org/data/cache/classificationS2.json
# read json
sitc2.desc.r <- fromJSON(file.path("./data-raw", "classificationS2.json"))

sitc2.desc <- sitc2.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# add 5 digit description
sitc2.desc.5d.1 <- sitc2.desc %>%
  filter(nchar(code) == 4) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc2.desc.5d.2 <- sitc2.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc2.desc.5d.3 <- sitc2.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc2.desc.5d.4 <- sitc2.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

# add 4-digit description
sitc2.desc.4d.1 <- sitc2.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc2.desc.4d.2 <- sitc2.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc2.desc.4d.3 <- sitc2.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

# add 3-digit description
sitc2.desc.3d.1 <- sitc2.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

sitc2.desc.3d.2 <- sitc2.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

# add 2-digit description
sitc2.desc.2d <- sitc2.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 2, side = "right", pad = "0"))

# combine
sitc2.desc <- rbind(sitc2.desc,
                    sitc2.desc.5d.1, sitc2.desc.5d.2, sitc2.desc.5d.3, sitc2.desc.5d.4,
                    sitc2.desc.4d.1, sitc2.desc.4d.2, sitc2.desc.4d.3,
                    sitc2.desc.3d.1, sitc2.desc.3d.2,
                    sitc2.desc.2d) %>%
  arrange(code) %>%
  distinct()

# check
sitc2.desc[duplicated(sitc2.desc$code),]

sitc2_desc <- sitc2.desc[!duplicated(sitc2.desc$code),]

# save
save(sitc2_desc,
     file = "./data/sitc2_desc.RData", compress = "xz")


################################################################################
## SITC3
################################################################################
# https://comtrade.un.org/data/cache/classificationS3.json
# read json
sitc3.desc.r <- fromJSON(file.path("./data-raw", "classificationS3.json"))

sitc3.desc <- sitc3.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# add 5 digit description
sitc3.desc.5d.1 <- sitc3.desc %>%
  filter(nchar(code) == 4) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc3.desc.5d.2 <- sitc3.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc3.desc.5d.3 <- sitc3.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc3.desc.5d.4 <- sitc3.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

# add 4-digit description
sitc3.desc.4d.1 <- sitc3.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc3.desc.4d.2 <- sitc3.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc3.desc.4d.3 <- sitc3.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

# add 3-digit description
sitc3.desc.3d.1 <- sitc3.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

sitc3.desc.3d.2 <- sitc3.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

# add 2-digit description
sitc3.desc.2d <- sitc3.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 2, side = "right", pad = "0"))

# combine
sitc3.desc <- rbind(sitc3.desc,
                    sitc3.desc.5d.1, sitc3.desc.5d.2, sitc3.desc.5d.3, sitc3.desc.5d.4,
                    sitc3.desc.4d.1, sitc3.desc.4d.2, sitc3.desc.4d.3,
                    sitc3.desc.3d.1, sitc3.desc.3d.2,
                    sitc3.desc.2d) %>%
  arrange(code) %>%
  distinct()

# check
sitc3.desc[duplicated(sitc3.desc$code),]

sitc3_desc <- sitc3.desc[!duplicated(sitc3.desc$code),]

# save
save(sitc3_desc,
     file = "./data/sitc3_desc.RData", compress = "xz")


################################################################################
## SITC4
################################################################################
# https://comtrade.un.org/data/cache/classificationS4.json
# read json
sitc4.desc.r <- fromJSON(file.path("./data-raw", "classificationS4.json"))

sitc4.desc <- sitc4.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

# add 5 digit description
sitc4.desc.5d.1 <- sitc4.desc %>%
  filter(nchar(code) == 4) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc4.desc.5d.2 <- sitc4.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc4.desc.5d.3 <- sitc4.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

sitc4.desc.5d.4 <- sitc4.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 5, side = "right", pad = "0"))

# add 4-digit description
sitc4.desc.4d.1 <- sitc4.desc %>%
  filter(nchar(code) == 3) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc4.desc.4d.2 <- sitc4.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

sitc4.desc.4d.3 <- sitc4.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 4, side = "right", pad = "0"))

# add 3-digit description
sitc4.desc.3d.1 <- sitc4.desc %>%
  filter(nchar(code) == 2) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

sitc4.desc.3d.2 <- sitc4.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0"))

# add 2-digit description
sitc4.desc.2d <- sitc4.desc %>%
  filter(nchar(code) == 1) %>%
  mutate(code = str_pad(code, width = 2, side = "right", pad = "0"))

# combine
sitc4.desc <- rbind(sitc4.desc,
                    sitc4.desc.5d.1, sitc4.desc.5d.2, sitc4.desc.5d.3, sitc4.desc.5d.4,
                    sitc4.desc.4d.1, sitc4.desc.4d.2, sitc4.desc.4d.3,
                    sitc4.desc.3d.1, sitc4.desc.3d.2,
                    sitc4.desc.2d) %>%
  arrange(code) %>%
  distinct()

# check
sitc4.desc[duplicated(sitc4.desc$code),]

sitc4_desc <- sitc4.desc[!duplicated(sitc4.desc$code),]

# save
save(sitc4_desc,
     file = "./data/sitc4_desc.RData", compress = "xz")


################################################################################
## BEC rev.4
################################################################################
# https://unstats.un.org/unsd/trade/classifications/correspondence-tables.asp
# https://comtrade.un.org/data/cache/classificationBEC.json
# read json
bec4.desc.r <- fromJSON(file.path("./data-raw", "classificationBEC.json"))

bec4.desc.3d <- bec4.desc.r$results %>%
  filter(parent != "#") %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  mutate(code = str_pad(code, width = 3, side = "right", pad = "0")) %>%
  select(code, desc) %>%
  arrange(code)

bec4.desc.2d <- bec4.desc.r$results %>%
  filter(nchar(id) == 2) %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

bec4.desc.1d <- bec4.desc.r$results %>%
  filter(nchar(id) == 1) %>%
  mutate(desc = str_replace_all(text, "^(.*?) - ", "")) %>%
  rename(code = id) %>%
  select(code, desc) %>%
  arrange(code)

bec4_desc <- rbind(bec4.desc.3d, bec4.desc.2d, bec4.desc.1d) %>%
  arrange(code)

# save
save(bec4_desc,
     file = "./data/bec4_desc.RData", compress = "xz")

################################################################################
## SIC 1987
## 2 digits and 3 
################################################################################
## https://www.bls.gov/oes/special-requests/oessic87.pdf
## https://www.census.gov/naics/?68967

### 4 digits codes come from BLS
des.raw4 <- read_csv("./data-raw/1987_SIC_BLS.csv", col_types = "c")
### 2 and 3 digits codes come from SICCODE.com
# des.raw23 <- read_csv("./data-raw/sic.decs.raw.csv", col_types = "c")

# clean 4 digits codes and descriptions
sic.des4 <- des.raw4 %>%
  select(digit4, SICdes) %>%
  na.omit() %>%
  rename(code = digit4, desc = SICdes)

# clean 3 digits codes and descriptions
sic.des3 <- des.raw4 %>%
  select(digit4, SICdes)%>%
  na.omit() %>%
  filter(grepl("0$", digit4)) %>%
  mutate(digit4 = gsub("0$", "", digit4)) %>%
  rename(code = digit4, desc = SICdes)

# clean 2 digits codes and descriptions
sic.des2 <- des.raw4 %>%
  select(digit4, SICdes)%>%
  na.omit() %>%
  filter(grepl("00$", digit4)) %>%
  mutate(digit4 = gsub("00$", "", digit4)) %>%
  rename(code = digit4, desc = SICdes)

# combine
sic87_desc <- rbind(sic.des2, sic.des3, sic.des4) %>%
  arrange(code) %>%
  distinct()
  # na.omit()

# first check if descriptions remained same in the Census files for different years
s87n97.data.r <- read_excel("./data-raw/1987_SIC_to_1997_NAICS.xls") %>%
  select(SIC, "SIC Titles and Part Descriptions") %>%
  rename(Censusdes = "SIC Titles and Part Descriptions")  
# check if we need to pad 0 to the left (only when there are 3-digit codes. Padding 0 to the left of 2-digit codes generates wrong codes like "0010")
table(nchar(s87n97.data.r$SIC))

n97s87.data.r <- read_excel("./data-raw/1997_NAICS_to_1987_SIC.xls") %>%
  select(SIC, "SIC Title and Part Description") %>%
  rename(Censusdes = "SIC Title and Part Description")
# check if we need to pad 0 to the left (only when there are 3-digit codes. Padding 0 to the left of 2-digit codes generates wrong codes like "0010")
table(nchar(n97s87.data.r$SIC))
n97s87.data.r <- n97s87.data.r %>%
  mutate(SIC = str_pad(SIC, width = 4, side = "left", pad = "0"))

Census8797 <- rbind(s87n97.data.r, n97s87.data.r) %>%
  arrange(SIC) %>%
  distinct() %>%
  na.omit() %>% 
  group_by(SIC) %>%
  slice(1) %>%
  ungroup()


s87n02.data.r <- read_excel("./data-raw/1987_SIC_to_2002_NAICS.xls") %>%
  select(SIC, "SIC Title (and note)") %>%
  rename(Censusdes = "SIC Title (and note)")
# check if we need to pad 0 to the left (only when there are 3-digit codes. Padding 0 to the left of 2-digit codes generates wrong codes like "0010")
table(nchar(s87n02.data.r$SIC))
s87n02.data.r <- s87n02.data.r %>%
  mutate(SIC = str_pad(SIC, width = 4, side = "left", pad = "0"))

n02s87.data.r <- read_excel("./data-raw/2002_NAICS_to_1987_SIC.xls") %>%
  select(SIC, "SIC Title (and note)") %>%
  rename(Censusdes = "SIC Title (and note)")
# check if we need to pad 0 to the left (only when there are 3-digit codes. Padding 0 to the left of 2-digit codes generates wrong codes like "0010")
table(nchar(n02s87.data.r$SIC))
n02s87.data.r <- n02s87.data.r%>%
  mutate(SIC = str_pad(SIC, width = 4, side = "left", pad = "0"))


Census8702 <- rbind(s87n02.data.r, n02s87.data.r) %>%
  arrange(SIC) %>%
  distinct() %>%
  na.omit() %>% 
  group_by(SIC) %>%
  slice(1) %>%
  ungroup()

CensusSIC <- rbind(Census8797, Census8702) %>%
  arrange(SIC) %>%
  distinct() %>%
  na.omit()

Cenfreq <- table(CensusSIC$SIC) %>%
  as.data.frame() %>%
  filter(Freq != 1) %>%
  rename(SIC = Var1)

Cenfdesc <- CensusSIC %>%
  semi_join(Cenfreq, by = c("SIC"))

result <- Cenfdesc %>%
  group_by(SIC) %>%
  mutate(first_five_chars = substr(Censusdes, 1, 5),
         has_shared_chars = duplicated(first_five_chars) | duplicated(first_five_chars, fromLast = TRUE)) %>%
  ungroup() %>%
  select(-first_five_chars)%>%
  filter(has_shared_chars != TRUE)
# the description remained same in all Census files


# check if bls, Census files have the same codes and descriptions
clean_census_sic <- CensusSIC %>%
  group_by(SIC) %>%
  slice(1) %>%
  ungroup() %>%
  rename(code = SIC, desc = Censusdes) %>%
  mutate(source = "census")

#check SIC codes frequency in BLS: no repeated codes
blsfreq <- table(sic87_desc$code) %>%
  as.data.frame()%>%
  filter(Freq != 1) 

# merge clean_census_sic and bls description to check desc differences
bls_sic <- sic87_desc %>%
  mutate(source = "bls")

all_sic <- rbind(clean_census_sic, bls_sic) %>%
  arrange(code) %>%
  distinct() %>%
  na.omit()

all_freq <- table(all_sic$code) %>%
  as.data.frame()%>%
  filter(Freq != 1) %>%
  rename(code = Var1)

allfdesc <- all_sic %>%
  semi_join(all_freq, by = c("code"))

all_result <- allfdesc %>%
  group_by(code) %>%
  mutate(first_five_chars = substr(desc, 1, 5),
         has_shared_chars = duplicated(first_five_chars) | duplicated(first_five_chars, fromLast = TRUE)) %>%
  ungroup() %>%
  select(-first_five_chars)%>%
  filter(has_shared_chars != TRUE)
# descriptions in the Census and BLS files are consistent

# save
save(sic87_desc,
     file = "./data/sic87_desc.RData", compress = "xz")


