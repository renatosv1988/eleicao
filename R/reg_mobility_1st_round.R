

# plot -------------
library(ggsci)
library(ggplot2)

# keep only data on Sundays close to the election dates
# 1o turno:  2 de outubro de 2022
# 2o turno: 30 de outubro de 2022
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


google1 <- fread(file = '../../data/google/google.csv')

google1 <- subset(google1, name_muni != "NA - NA")
google1 <- subset(google1, date %in% sundays)

summary(google1$passe_1)
summary(google1$passe_2)
google1[, passe_1 := fifelse(is.na(passe_1), 0, passe_1)]
google1[, passe_2 := fifelse(is.na(passe_2), 0, passe_2)]



temp_df <- subset(google1, variable %like% 'transit' & !is.na(sub_region_2))

temp_df2 <- temp_df[, .(mean_value=mean(value, na.rm=T),
                        p25 = quantile(value,0.25, na.rm=T),
                        p75 = quantile(value,0.75, na.rm=T)), by=.(variable, passe_1, date)]



ggplot() +
 geom_line(data=temp_df2, aes(x=as.Date(date), y=mean_value, color=factor(passe_1)),
           position = position_dodge2(width = 2)) +
 geom_pointrange(data=temp_df2,
                 position = position_dodge2(width = 2),
                 show.legend = FALSE,
                 aes(x=as.Date(date), y=mean_value, color=factor(passe_1),
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


# cria dummy date
table(temp_df$date)
temp_df[, dummy_date := fifelse(date== as.Date('2022-09-25'), 0, 1)]
table(temp_df$dummy_date, temp_df$date)

temp_df[, treated := max(passe_1), by = name_muni]

r1 <- fixest::feols(value ~ dummy_date + treated + dummy_date:treated, 
                    data=temp_df,
                    fixef = 'name_muni', 
                    cluster = 'name_muni')
summary(r1)






