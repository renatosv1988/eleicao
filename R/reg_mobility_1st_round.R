library(ggsci)
library(ggplot2)
library(stringr)
library(data.table)
library(modelsummary)


# Read data -------------
google1 <- fread(file = '../../data/google/google.csv')

# recode variable
google1[, variable := str_sub(variable, end=-30)]

# muni estimates
google1 <- subset(google1, name_muni != "NA - NA")
google1 <- subset(google1, !is.na(sub_region_2))


# determine treated and untreated
google1[passe_livre_always==1, passe_1 := 1]
google1[passe_livre_always==1, passe_2 := 1]

google1[, passe_1 := fifelse(is.na(passe_1), 0, passe_1)]
google1[, passe_2 := fifelse(is.na(passe_2), 0, passe_2)]

table(google1$passe_1, useNA = 'always')
table(google1$passe_2, useNA = 'always')



# keep only municipalities that were eventually treated
google1 <- google1[passe_2==1, ]



# prepare PLOT  ----------------------------------------------------------------

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

temp_df <- google_plot[, .(mean_value=mean(value, na.rm=T),
                           p25 = mean(value, na.rm=T) - sd(value, na.rm=T),
                           p75 = mean(value, na.rm=T) + sd(value, na.rm=T)
                           ), by=.(variable, passe_1, date)]

temp_df[, passe_1 := ifelse(passe_1==1, 'Yes', 'No')]




# plot all  ----------------------------------------------------------------

fig_all <- ggplot() +
           annotate("rect", fill='gray95', xmin = as.Date("2022-09-30"), xmax = as.Date("2022-10-04"), 
                    ymin = min(temp_df$p25 - 3, na.rm=T), ymax = max(temp_df$p75, na.rm=T)) +
           geom_line(data=temp_df, aes(x=as.Date(date), y=mean_value, color=factor(passe_1)),
                     position = position_dodge2(width = 2)) +
           geom_pointrange(data=temp_df,
                           position = position_dodge2(width = 2),
                           show.legend = FALSE,
                           aes(x=as.Date(date), y=mean_value, color=factor(passe_1),
                               ymin = p25,
                               ymax = p75)) +
            facet_wrap(~variable) +
           labs(y='Mobility change against baseline', x = 'Sunday', color='Treated in\nround 1') +
           scale_x_date( date_labels =  "%d %b", breaks =  sundays[c(1,3,5)]) +
           scale_y_continuous(expand = c(0,0)) +
           #scale_color_uchicago() +
           #scale_color_jama() +
           theme_classic() +
           default_theme +
           scale_color_npg()

fig_all

ggsave(fig_all, file='./figures/google_mobility_all.png', 
       width = 17.1, height = 10, units='cm', dpi = 300)


# plot TRANSIT  ----------------------------------------------------------------


temp_df_T <- subset(temp_df, variable %like% 'transit')

fig_m <- ggplot() +
 annotate("rect", fill='gray95', xmin = as.Date("2022-09-30"), xmax = as.Date("2022-10-04"), 
          ymin = min(temp_df_T$p25 - 3, na.rm=T), ymax = max(temp_df_T$p75, na.rm=T)) +
 geom_line(data=temp_df_T, aes(x=as.Date(date), y=mean_value, color=factor(passe_1)),
           position = position_dodge2(width = 2)) +
 geom_pointrange(data=temp_df_T,
                 position = position_dodge2(width = 2),
                 show.legend = FALSE,
                 aes(x=as.Date(date), y=mean_value, color=factor(passe_1),
                     ymin = p25,
                     ymax = p75)) +
 facet_wrap(~variable) +
 labs(y='Mobility change\nagainst baseline', x = 'Sunday', color='Treated in\nround 1') +
 scale_x_date( date_labels =  "%d %b", breaks =  sundays[c(1,3,5)]) +
 scale_y_continuous(expand = c(0,0)) +
 #scale_color_uchicago() +
 #scale_color_jama() +
 theme_classic() +
 default_theme +
 scale_color_npg(guide = guide_legend()) +
 theme(legend.position="bottom")

fig_m


# REGRESSION  ----------------------------------------------------------------

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



# cria dummy date
table(google_reg$date)
google_reg[, dummy_election_day := fifelse(date== as.Date('2022-10-02'), 1, 0)]
google_reg[, treated := max(passe_1), by = name_muni]

table(google_reg$dummy_election_day, google_reg$date)
table(google_reg$treated, google_reg$dummy_election_day)


# Models
reg_on_mobility <- function(activity){ # activity = 'transit'
 
temp_df_reg <- subset(google_reg, variable %like% activity)

m_temp <- fixest::feols(value ~ dummy_election_day + treated + dummy_election_day:treated, 
                    fixef = 'name_muni', 
                    cluster = 'name_muni',
                    data=temp_df_reg)
return(m_temp)
}

reg_output_list <- lapply(X=unique(google_reg$variable), FUN = reg_on_mobility)
names(reg_output_list) <- unique(google_reg$variable)

modelsummary::modelsummary(reg_output_list, 
                           stars = T, 
                           conf_level= 0.95,
                           estimate  = "{estimate} [{conf.low}, {conf.high}]"
                           )

modelsummary::modelsummary(reg_output_list, stars = T, output = './tables/mobility_reg.tex')
