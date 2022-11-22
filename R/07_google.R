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
library(ggsci)

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

summary(google$date)

google <- google[country_region %in% "Brazil",]
summary(google$date)

# save to dara-raw
dir.create('../../data_raw/google')
fwrite(google, '../../data_raw/google/google_mobility.csv')




# Read and clean ---------------------------------------------------------------

google <- fread('../../data_raw/google/google_mobility.csv', encoding = "UTF-8")

# clean dates
google[,date_fix := as.POSIXct(date,tz = "America/Bahia")]
google[,year_month := format(date_fix,"%m/%Y")]
google[,day_month := format(date_fix,"%d/%m")]
google[,day_month_id := .GRP,by = date_fix]

max(google$date)


# keep only data on Sundays close to the election dates
  # 1o turno:  2 de outubro de 2022
  # 2o turno: 30 de outubro de 2022
  sundays <- c("2022-09-04",
               "2022-09-11",
               "2022-09-18",
               "2022-09-25",
               "2022-10-02",
               "2022-10-09",
               "2022-10-16",
               "2022-10-23",
               "2022-10-30") |> as.Date()
  
  google_election <- subset(google, date %in% sundays)
  table(google_election$date)
  
  
  # identify election rounds
  google_election[, NR_TURNO := fcase(date == as.IDate("2022-10-02"), 1,
                                      date == as.IDate("2022-10-30"), 2)
                                      ]
  table(google_election$NR_TURNO)
  
  
  
  
  # clean names
  google_election[,sub_region_1_fix := toupper_noaccent(sub_region_1) %>% 
          stringr::str_remove_all("STATE OF ")]
  google_election[sub_region_1_fix %in% "FEDERAL DISTRICT",sub_region_1_fix := "DISTRITO FEDERAL"]
  google_election[statebr,on = c('sub_region_1_fix' = 'name_state'), state_abbrev := i.abbrev_state]
  google_election[, sub_region_2 := fifelse(state_abbrev == "DF", "Brasília", sub_region_2)]
  google_election[,sub_region_2_fix := toupper_noaccent(sub_region_2)]
  google_election[,name_muni := paste0(sub_region_2_fix," - ",state_abbrev)]
  

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
google_election$sub_region_1 %>% unique()
google_election$sub_region_2 %>% unique()
google_election$sub_region_2 %>% uniqueN()
google_election$metro_area %>% unique()
google_election$iso_3166_2_code %>% unique()
google_election$census_fips_code %>% unique()
google_election$date %>% unique()
google_election$state_abbrev %>% unique()
google_election$name_muni %>% unique()



# bring passe livre information
passe <- fread('../../data/passe_livre/passe_livre.csv')
head(passe$city_uf_ID)

passe[, table(`1º Turno`, `2º Turno`)]
#>         2º Turno
#>                1
#> 1º Turno   0 310
#>            1  82

# merge
google_election_passe_livre <- left_join(google_election, passe, by=c('name_muni'='city_uf_ID'))
setDT(google_election_passe_livre)

table(passe$`1º Turno`, useNA = 'always')
table(google_election_passe_livre$`1º Turno`, useNA = 'always')

# a <- google_election_passe_livre[is.na(`1º Turno`)]
# a <- subset(a, !is.na(sub_region_2))
# a <- subset(a, sub_region_2 != "")

# we have info on passe livre for 392 cities
passe$city_uf_ID |> length()
#> 392

# we have google info for 339 of those 392 cities
sum(passe$city_uf_ID %in% google_election$name_muni)
#> 339


setnames(google_election_passe_livre, '1º Turno', 'passe_1')
setnames(google_election_passe_livre, '2º Turno', 'passe_2')
setnames(google_election_passe_livre, '1º e 2º Turno', 'passe_12')
# setnames(google_election_passe_livre, 'Nível metropolitano', 'passe_metropolitano')
# table(passe$Abrangência)
# table(passe$`Nível metropolitano`)


# melt data-------------

google1 <- data.table::melt(data = google_election_passe_livre,
                            id.vars = c('name_muni', 'date','date_fix',
                                        'day_month_id','state_abbrev','sub_region_2',
                                        'passe_1', 'passe_2', 'passe_12'),
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




# plot -------------
library(ggsci)
library(ggplot2)

# keep only data on Sundays close to the election dates
# 1o turno:  2 de outubro de 2022
# 2o turno: 30 de outubro de 2022
sundays <- c("2022-09-04",
             "2022-09-11",
             "2022-09-18",
             "2022-09-25",
             "2022-10-02",
             "2022-10-09",
             "2022-10-16",
             "2022-10-23",
             "2022-10-30") |> as.Date()


google1 <- fread(file = '../../data/google/google.csv')


summary(google1$passe_1)
summary(google1$passe_2)
google1[, passe_1 := fifelse(is.na(passe_1), 0, passe_1)]
google1[, passe_2 := fifelse(is.na(passe_2), 0, passe_2)]

google1[, any_passe := fcase(passe_1==1 & passe_2==0, '1',
                             passe_1==0 & passe_2==1, '2',
                             passe_1==1 & passe_2==1, '1',
                             default = 'Never')]

google1[, any_passe := factor(any_passe, levels = c('Never', '1', '2'))]


temp_df <- subset(google1, variable %like% 'transit' & !is.na(sub_region_2))

temp_df2 <- temp_df[, .(mean_value=mean(value, na.rm=T),
                        p25 = quantile(value,0.25, na.rm=T),
                        p75 = quantile(value,0.75, na.rm=T)), by=.(variable, any_passe, date)]



ggplot() +
 geom_line(data=temp_df2, aes(x=as.Date(date), y=mean_value, color=any_passe),
           position = position_dodge2(width = 2)) +
 geom_pointrange(data=temp_df2,
                 position = position_dodge2(width = 2),
                 show.legend = FALSE,
                 aes(x=as.Date(date), y=mean_value, color=any_passe,
                 ymin = p25,
                 ymax = p75)) +
 labs(y='Mobility change\nagainst baseline', x = 'Sunday', color='Round with\nfree transit') +
 scale_x_date( date_labels =  "%d %b", breaks =  sundays) +
 scale_y_continuous(expand = c(0,0)) +
 annotate("rect", xmin = as.Date("2022-09-30"), xmax = as.Date("2022-10-04"), 
          ymin = min(temp_df2$p25 - 3, na.rm=T), ymax = max(temp_df2$p75, na.rm=T), alpha = .1) +
 # scale_color_npg() +
 #scale_color_uchicago() +
 scale_color_jama() +
 theme_classic()

ggsave('./figures/google_mobility.png', width = 16, height = 10, units='cm')






