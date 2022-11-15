library(ggsci)
library(ggplot2)
library(stringr)
library(data.table)


# Read data -------------
google1 <- fread(file = '../../data/google/google.csv')

# recode variable
google1[, variable := str_sub(variable, end=-30)]

# muni estimates
google1 <- subset(google1, name_muni != "NA - NA")
google1 <- subset(google1, !is.na(sub_region_2))



# determine treated and untreated
summary(google1$passe_1)
summary(google1$passe_2)
google1[, passe_1 := fifelse(is.na(passe_1), 0, passe_1)]
google1[, passe_2 := fifelse(is.na(passe_2), 0, passe_2)]



# PLOT  -------------

# keep only data on Sundays close to the election dates
# 1o turno:  2 de outubro de 2022
# 2o turno: 30 de outubro de 2022
sundays <- c(
             "2022-09-04",
             "2022-09-11",
             "2022-09-18",
             "2022-09-25",
             "2022-10-02",
             "2022-10-09"
             #"2022-10-16",
             # "2022-10-23",
             # "2022-10-30"
             ) |> as.Date()


# filter days
google_plot <- subset(google1, date %in% sundays)



temp_df <- google_plot[, .(mean_value=mean(value, na.rm=T),
                        p25 = quantile(value,0.25, na.rm=T),
                        p75 = quantile(value,0.75, na.rm=T)), by=.(variable, passe_1, date)]

temp_df <- subset(temp_df, variable %like% 'transit')

ggplot() +
 geom_line(data=temp_df, aes(x=as.Date(date), y=mean_value, color=factor(passe_1)),
           position = position_dodge2(width = 2)) +
 geom_pointrange(data=temp_df,
                 position = position_dodge2(width = 2),
                 show.legend = FALSE,
                 aes(x=as.Date(date), y=mean_value, color=factor(passe_1),
                     ymin = p25,
                     ymax = p75)) +
#  facet_wrap(~variable) +
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




# REGRESSION  -------------

sundays <- c(
 #"2022-09-04",
 #"2022-09-11",
 #"2022-09-18",
 "2022-09-25",
 "2022-10-02"
 #"2022-10-09",
 #"2022-10-16",
 # "2022-10-23",
 # "2022-10-30"
) |> as.Date()


# keep only 1st round and previous sunday
google_reg <- subset(google1, date %in% sundays)
google_reg <- subset(google_reg, variable %like% 'transit')



# cria dummy date
table(google_reg$date)
google_reg[, dummy_date := fifelse(date== as.Date('2022-09-25'), 0, 1)]
google_reg[, treated := max(passe_1), by = name_muni]

table(google_reg$dummy_date, google_reg$date)
table(google_reg$treated, google_reg$date)


r1 <- fixest::feols(value ~ dummy_date + treated + dummy_date:treated, 
                    data=google_reg,
                    fixef = 'name_muni', 
                    cluster = 'name_muni')
summary(r1)






