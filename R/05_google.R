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

`%nin%` = Negate(`%in%`)

# function to renamte columns
toupper_noaccent <- function(i){
  stringi::stri_trans_general(i,id = "Latin-ASCII") %>% 
    toupper() %>% stringr::str_replace_all("-"," ")  %>% 
    stringr::str_remove_all("'")
}


# Download data ---------------------------------------------------------------

ls_initial_list <- c("google","activities","%nin%","toupper_noaccent")

# first manipulation
statebr <- geobr::read_state(code_state = "all") %>% data.table::setDT()
statebr[,name_state := toupper_noaccent(name_state)]

# static google url
link <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
google <- data.table::fread(input = link, encoding = "UTF-8")
google <- google[country_region %in% "Brazil",]
google <- google[date < as.Date("2022-12-31"),]

# save to dara-raw
fwrite(google, './data_raw/google_mobility.csv')




# Read and clean ---------------------------------------------------------------

google <- fread('./data_raw/google_mobility.csv', encoding = "UTF-8")

# clean names
google[,sub_region_1_fix := toupper_noaccent(sub_region_1) %>% 
         stringr::str_remove_all("STATE OF ")]
google[sub_region_1_fix %in% "FEDERAL DISTRICT",sub_region_1_fix := "DISTRITO FEDERAL"]
google[, sub_region_2 := fifelse(state_abrev == "DF", "Brasília", sub_region_2)]
google[,sub_region_2_fix := toupper_noaccent(sub_region_2)]
google[statebr,on = c('sub_region_1_fix' = 'name_state'), state_abrev := i.abbrev_state]
google[,name_muni := paste0(sub_region_2_fix,"-",state_abrev)]

# clean dates
google[,date_fix := as.POSIXct(date,tz = "America/Bahia")]
google[,year_month := format(date_fix,"%m/%Y")]
google[,day_month := format(date_fix,"%d/%m")]
google[,day_month_id := .GRP,by = date_fix]


max(google$date)

# keep only data on the dates of 1st and 2nd voting days
  # 1o turno: 3 de agosto de 2022
  # 2o turno: 31 de outubro de 2022
  google_election <- subset(google, date %in% as.IDate(c("2020-08-03", "2020-10-31")))
  google_election[, NR_TURNO := fcase(date == as.IDate("2020-08-03"), 1,
                                      date == as.IDate("2020-10-31"), 1)
                                      ]

activities <- c("retail_and_recreation",
                #"grocery_and_pharmacy",
                #"parks",
                "transit_stations",
                "workplaces",
                "residential")
local_categories <- c('Varejo e lazer',
                      #'Mercados e farmácias',
                      #'Parques',
                      'Estações de transporte público',
                      'Locais de trabalho','Residencial')
description <- c('Tendências de mobilidade de lugares como restaurantes, cafés, \n shopping centers, parques temáticos, museus, bibliotecas e cinemas.',
                 #'Tendências de mobilidade de lugares como mercados, armazéns de \n alimentos, feiras, lojas especializadas em alimentos, drogarias e farmácias.',
                 #'Tendências de mobilidade de lugares como parques locais e nacionais, \n praias públicas, marinas, parques para cães, praças e jardins públicos.',
                 'Tendências de mobilidade de lugares como terminais de transporte público,\n tipo estações de metrô, ônibus e trem',
                 'Tendências de mobilidade de locais de trabalho',
                 'Tendências de mobilidade de áreas residenciais')



#
# check names-------------
#
google$sub_region_1 %>% unique()
google$sub_region_2 %>% unique()
google$sub_region_2 %>% uniqueN()
google$metro_area %>% unique()
google$iso_3166_2_code %>% unique()
google$census_fips_code %>% unique()
google$date %>% unique()
google$state_abrev %>% unique()





# melt data-------------

google1 <- data.table::melt(data = google,
                            id.vars = c('date_fix','day_month_id','state_abrev','sub_region_2'),
                            measure.vars =  list('change' = c('retail_and_recreation_percent_change_from_baseline',
                                                              'grocery_and_pharmacy_percent_change_from_baseline',
                                                              'parks_percent_change_from_baseline',
                                                              'transit_stations_percent_change_from_baseline',
                                                              'workplaces_percent_change_from_baseline',
                                                              'residential_percent_change_from_baseline')))


# save data-------------
fwrite(google1, file = './data/google.csv')

