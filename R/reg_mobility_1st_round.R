library(ggsci)
library(ggplot2)
library(stringr)
library(data.table)
library(modelsummary)
library(fixest)


# Read data -------------
google1 <- fread(file = '../../data/google/google.csv')

# recode variable
google1[, variable := str_sub(variable, end=-30)]

# muni estimates
google1 <- subset(google1, name_muni != "NA - NA")
google1 <- subset(google1, !is.na(sub_region_2))


# excluir cidades que SEMPRE tiveram passe livre
google1 <- subset(google1, is.na(passe_livre_always))



# keep only municipalities that were eventually treated
google1 <- google1[passe_2==1, ]

# recode dummy
google1[, passe_1 := fifelse(passe_1 ==1, 1, 0)]
google1[, passe_2 := fifelse(passe_1 ==0, 1, 0)]

table(google1$passe_1, useNA = 'always')
table(google1$passe_2, useNA = 'always')






# prepare PLOT  ----------------------------------------------------------------

# keep only data on Sundays close to the election dates
# 1o turno:  2 de outubro de 2022
# 2o turno: 30 de outubro de 2022
sundays <- c(
             "2022-08-14",
             "2022-08-21",
             "2022-08-28",
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
# google_plot <- subset(google1, date > as.Date("2022-09-25"))
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
            geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
            labs(y='Mobility change against baseline', x = 'Sunday', color='Treated in\nround 1') +
           scale_x_date( date_labels =  "%d %b", breaks =  sundays[c(1,4,6,8,9)] ) +
           scale_y_continuous(expand = c(0,0)) +
           theme_classic() +
           scale_color_npg() +
           theme(text = element_text(size=9))

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
 # facet_wrap(~variable) +
 labs(y='Mobility change against baseline', x = 'Sunday', color='Treated in\nround 1') +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 scale_x_date( date_labels =  "%d %b", breaks =  sundays[c(1,4,6,8,9)] ) +
 scale_y_continuous(expand = c(0,0)) +
 theme_classic() +
 theme(text = element_text(size=9)) +
 scale_color_npg(guide = guide_legend()) +
 theme(legend.position="bottom")

fig_m


ggsave(fig_m, file='./figures/google_mobility_transit.png', 
       width = 17.1, height = 10, units='cm', dpi = 300)






# REGRESSION  ----------------------------------------------------------------

# keep only 1st round and previous sunday
# google_reg <- subset(google1, date > as.Date("2022-09-25"))
google_reg <- subset(google1, date %in% sundays)


# cria dummy date
table(google_reg$date)
google_reg[, dummy_election_day := fifelse(date == as.Date('2022-10-02'), 1, 0)]
google_reg[, treated := max(passe_1), by = name_muni]

table(google_reg$dummy_election_day, google_reg$date)
table(google_reg$treated, google_reg$dummy_election_day)



# recode treatment to use Sun & Abraham (2020)
# https://bcallaway11.github.io/did/articles/did-basics.html
# https://grantmcdermott.com/ggiplot/
google_reg[treated==1, date_treated := as.Date("2022-10-02")]
google_reg[, date_treated := fcase(treated==1, as.Date("2022-10-02"), 
                                   treated==0, as.Date("3000-01-01"))]

google_reg[, date := lubridate::ymd(date)]
google_reg[, date_treated := lubridate::ymd(date_treated)]
google_reg[, time_to_treatment := as.numeric(date - date_treated)]
google_reg[date_treated == as.Date('3000-01-01'), time_to_treatment := -1000]
# table(google_reg$treated)
# table(google_reg$date_treated)


# Models
reg_on_mobility2 <- function(activity){ # activity = 'transit'
 
 temp_df_reg <- subset(google_reg, variable %like% activity)

 m_temp = fixest::feols(value ~ i(date, treated, "2022-09-25") | name_muni+date,
                        cluster = 'name_muni',
                        data = temp_df_reg)
 
 m_temp2 = fixest::feols(value ~sunab(date_treated, date) | name_muni+date,
                        cluster = 'name_muni',
                        data = temp_df_reg)
 
 # iplot(object = m_temp)
 # iplot(object = m_temp2)

  # cleand df
  output_df <- broom::tidy(m_temp) 
  setDT(output_df)
  names(output_df) <- c('date', 'coef', 'se', 'statistic', 'p_value')
  output_df[, date := substring(date,7, 16) |> lubridate::ymd()]
  output_df[, ymin := coef - 1.96*se]
  output_df[, ymax := coef + 1.96*se]
  output_df[, activity := activity]
  
 # add reference date
 output_df <- rbind(output_df,
                    data.frame(date=as.Date("2022-09-25"),
                               coef = 0, se = 0, statistic = 0,
                               p_value = 0,
                               ymin = 0, ymax = 0,
                               activity =activity))
 
 return(output_df)
}

# reg_output_list <- lapply(X=unique(google_reg$variable), FUN = reg_on_mobility2)
# names(reg_output_list) <- unique(google_reg$variable)
# 
# modelsummary::modelsummary(reg_output_list,
#                            stars = T,
#                            conf_level= 0.95
#                           # estimate  = "{estimate} [{conf.low}, {conf.high}]"
#                            )


reg_output_list <- lapply(X=unique(google_reg$variable), FUN = reg_on_mobility2)
reg_output_df <- data.table::rbindlist(reg_output_list)


table(google_reg$date)
table(reg_output_df$date)

temp_df <- copy(reg_output_df)

# get max and min y values
max_y <- max(c(abs(temp_df$ymin), temp_df$ymax))
min_y <- -1*max_y


fig_all_reg <- 
ggplot(data = temp_df, aes(x= date, y=coef, color=activity)) +
 annotate("rect", fill='gray95', xmin = as.Date("2022-09-30"), xmax = as.Date("2022-10-04"), 
          ymin = min(temp_df$temp_df, na.rm=T), ymax = max(temp_df$temp_df, na.rm=T)) +
 scale_x_date( date_labels =  "%d %b", breaks =  sundays[c(1,4,6,8)] ) +
 labs(y='Estimate and 95% Conf. Int.', x = 'Sundays') +
 geom_vline(xintercept = as.Date("2022-09-25"), color='gray80', linetype = 'dashed') +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 geom_point(position = position_dodge2(width = 2)) +
 geom_pointrange(position = position_dodge2(width = 2),
                 aes(x=date, y=coef,
                     ymin = ymin,
                     ymax = ymax)) +
 ylim(c(min_y, max_y)) +
 facet_wrap(~activity) +
 theme_classic() +
 theme(legend.position="none") +
 scale_color_npg()


fig_all_reg

ggsave(fig_all_reg, file='./figures/google_mobility_all_reg.png', 
       width = 17.1, height = 15, units='cm', dpi = 300)




# plot TRANSIT  reg ----------------------------------------------------------------


temp_df <- subset(reg_output_df, activity %like% 'transit')

# get max and min y values
max_y <- max(c(abs(temp_df$ymin), temp_df$ymax))
min_y <- -1*max_y


fig_t <- 
 ggplot(data = temp_df, aes(x= date, y=coef, color=activity)) +
 annotate("rect", fill='gray95', xmin = as.Date("2022-09-30"), xmax = as.Date("2022-10-04"), 
          ymin = min(temp_df$temp_df, na.rm=T), ymax = max(temp_df$temp_df, na.rm=T)) +
 scale_x_date( date_labels =  "%d %b", breaks =  sundays[c(1,4,6,8)] ) +
 labs(y='Estimate and 95% Conf. Int.', x = 'Sundays') +
 geom_vline(xintercept = as.Date("2022-09-25"), color='gray80', linetype = 'dashed') +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 geom_point() +
 geom_pointrange(aes(x=date, y=coef,
                     ymin = ymin,
                     ymax = ymax)) +
 ylim(c(min_y, max_y)) +
 theme_classic() +
 theme(text = element_text(size=9),
       legend.position="none") +
 scale_color_npg()


fig_t

ggsave(fig_t, file='./figures/google_mobility_transit_reg.png', 
       width = 17.1, height = 10, units='cm', dpi = 300)


