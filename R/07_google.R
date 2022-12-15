# google
#> https://github.com/ipeaGIT/mob_acess_br2020/blob/master/R/GOOGLE_MOBILITY/03_plot_GOOGLE-MOBILITY.R


# Setup ----
rm(list=ls())
gc(reset = T)

library(tidyverse)
library(data.table)
library(magrittr)
library(stringi)
library(stringr)


# Download data ---------------------------------------------------------------


# static google url
link <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
google <- data.table::fread(input = link, encoding = "UTF-8")

summary(google$date)

# keep data for Brazil
google <- google[country_region %in% "Brazil",]

# keep data for 2021 and 2022
google <- google[ date >= as.Date("2020-08-01")]

# save to dara-raw
dir.create('../../data_raw/google')
fwrite(google, '../../data_raw/google/google_mobility.csv')



# Read and clean ---------------------------------------------------------------
rm(list=ls())
gc(reset = T)

google <- fread('../../data_raw/google/google_mobility.csv', encoding = "UTF-8")

# clean dates
google[,year := format(date,"%Y")]
google[,year_month := format(date,"%m/%Y")]
google[,day_month := format(date,"%d/%m")]
google[,day_month_id := .GRP,by = date]

summary(google$date)
# 
# 
# # keep only data on Sundays close to the election dates
#   # 1o turno:  2 de outubro de 2022
#   # 2o turno: 30 de outubro de 2022
#   sundays <- c(
#                "2020-11-01",
#                "2020-11-08",
#                "2020-11-15",
#                "2020-11-22",
#                "2020-11-29",
#                "2022-08-07",
#                "2022-08-14",
#                "2022-08-21",
#                "2022-08-28",
#                "2022-09-04",
#                "2022-09-11",
#                "2022-09-18",
#                "2022-09-25",
#                "2022-10-02",
#                "2022-10-09",
#                "2022-10-16",
#                "2022-10-23",
#                "2022-10-30") |> as.Date()
#              
#   google_election <- subset(google, date %in% sundays)
#   table(google_election$date)
#   
  google_election <- copy(google)
  
  # identify election rounds
  google_election[, NR_TURNO := fcase(date == as.IDate("2020-11-15"), 1,
                                      date == as.IDate("2020-11-29"), 2,
                                      date == as.IDate("2022-10-02"), 1,
                                      date == as.IDate("2022-10-30"), 2)
  ]
  table(google_election$NR_TURNO)
  
  
  

    
  # function to rename columns
  toupper_noaccent <- function(i){
   stringi::stri_trans_general(i,id = "Latin-ASCII") %>% 
    toupper() %>% stringr::str_replace_all("-"," ")  %>% 
    stringr::str_remove_all("'")
  }
  
  # first manipulation
  statebr <- geobr::read_state(code_state = "all") %>% data.table::setDT()
  statebr[,name_state := toupper_noaccent(name_state)]
  
  
  # clean names
  google_election[,sub_region_1_fix := toupper_noaccent(sub_region_1) %>% 
          stringr::str_remove_all("STATE OF ")]
  google_election[sub_region_1_fix %in% "FEDERAL DISTRICT",sub_region_1_fix := "DISTRITO FEDERAL"]
  google_election[statebr,on = c('sub_region_1_fix' = 'name_state'), state_abbrev := i.abbrev_state]
  google_election[, sub_region_2 := fifelse(state_abbrev == "DF", "Brasília", sub_region_2)]
  google_election[,sub_region_2_fix := toupper_noaccent(sub_region_2)]
  google_election[,name_muni := paste0(sub_region_2_fix," - ",state_abbrev)]
  


  

# check names-------------
#
google_election$sub_region_1 %>% unique()
google_election$sub_region_2 %>% unique()
google_election$sub_region_2 %>% uniqueN()
google_election$metro_area %>% unique()
google_election$iso_3166_2_code %>% unique()
google_election$census_fips_code %>% unique()
google_election$date %>% unique()
google_election$state_abbrev %>% unique()
google_election$name_muni %>% unique()


# drop columns
to_drop <- c('country_region_code', 'country_region', 'sub_region_1', 
             'metro_area', 'iso_3166_2_code', 'census_fips_code', 'place_id')
google_election[, c(to_drop) := NULL ]


# bring passe livre information
passe <- fread('../../data/passe_livre/passe_livre.csv', encoding = 'UTF-8')
head(passe)
head(passe$city_uf_ID)

passe[, table(passe_livre_t1, passe_livre_t2)]
#>                passe_livre_t2
#> passe_livre_t1   1
#>              0 297
#>              1  97

# merge
google_election_passe_livre <- left_join(google_election, passe, by=c('name_muni'='city_uf_ID'))
setDT(google_election_passe_livre)

table(passe$passe_livre_t1, useNA = 'always')
table(google_election_passe_livre$passe_livre_t1, useNA = 'always')




# we have info on passe livre for 432 cities
passe$city_uf_ID |> length()


# we have google info for 339 of those 400 cities
sum(passe$city_uf_ID %in% google_election$name_muni)



setnames(google_election_passe_livre, 'passe_livre_t1', 'passe_1')
setnames(google_election_passe_livre, 'passe_livre_t2', 'passe_2')
setnames(google_election_passe_livre, '1º e 2º Turno', 'passe_12')
# setnames(google_election_passe_livre, 'Nível metropolitano', 'passe_metropolitano')
# table(passe$Abrangência)
# table(passe$`Nível metropolitano`)


# melt data-------------

google1 <- data.table::melt(data = google_election_passe_livre,
                            id.vars = c('name_muni', 'year', 'date',
                                        'day_month_id','state_abbrev','sub_region_2',
                                        'passe_1', 'passe_2', 'passe_livre_always', 'metro_only'),
                            measure.vars =  c('retail_and_recreation_percent_change_from_baseline',
                                                              'grocery_and_pharmacy_percent_change_from_baseline',
                                                              'parks_percent_change_from_baseline',
                                                              'transit_stations_percent_change_from_baseline',
                                                              'workplaces_percent_change_from_baseline',
                                                              'residential_percent_change_from_baseline'))

head(google1)


# save data-------------
dir.create('../../data/google')
fwrite(google1, file = '../../data/google/google.csv')







