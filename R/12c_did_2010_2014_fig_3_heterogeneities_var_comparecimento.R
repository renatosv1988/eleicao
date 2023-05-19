library(data.table)
library(ggplot2)
library(modelsummary)
library(purrr)
library(fixest)
library(ggsci)
library(patchwork)

options(scipen = 999)


# read data ----------------------------------------------------------------------

df <- fread("../../data/base_DiD2010_2022_secoes.csv", encoding = "Latin-1")

muni_ipw <- fread("../../data/ipw_municipalities.csv", encoding = "Latin-1")


# add ipw to electoral sections
st <- merge(df, muni_ipw, by="code_muni", all.x = T)





# Select observations ----------------------------------------------------------------------

# metro only 6666666666666666
# st <- subset(st, metro_only == 0)


# keep only municipalities that were eventually treated
st <- st[passe_livre_2==1, ]

# excluir cidades que SEMPRE tiveram passe livre
st <- subset(st, is.na(passe_livre_always))


# keep only 2018 and 2022
st <- st[ANO_ELEICAO %in% c(2018, 2022)]


# recode variables ----------------------------------------------------------------------

# create dummy for treatment in the 1st round
st[, pl1 := fifelse(passe_livre_1 == 1, 1, 0)]

# create dummy for treatment ONLY in the 2nd round
st[, pl2 := fifelse(passe_livre_1 == 0, 1, 0)]

# year as categorical var
st[, year := as.character(ANO_ELEICAO)]


# education quantiles
st[, educacao_1_decile := cut(educacao_1,
                              breaks = quantile(educacao_1, na.rm=T,
                                                probs = seq(0, 1 , by = .1)),
                              include.lowest = TRUE,
                              ordered_result = TRUE,
                              labels = 10:1),   by = code_muni]


st[, mean(educacao_1, na.rm=T), by=educacao_1_decile][order(V1)]


# density quantiles
st[, num_1000_rank := rank(num_1000, ties.method = "first"), by = code_muni]
st[, num_1000_decile := cut(num_1000_rank,
                            breaks = quantile(num_1000_rank, na.rm=T,
                                              probs = seq(0, 1 , by = .1)),
                            include.lowest = TRUE,
                            ordered_result = TRUE,
                            labels = 1:10),   by = code_muni]


st[, mean(num_1000, na.rm=T), by=num_1000_decile][order(V1)]



# elderly quantiles
st[, idade_60M_decile := cut(idade_60M,
                             breaks = quantile(idade_60M, na.rm=T,
                                               probs = seq(0, 1 , by = .1)),
                             include.lowest = TRUE,
                             ordered_result = TRUE,
                             labels = 1:10) ,   by = code_muni]


st[, mean(idade_60M, na.rm=T), by=idade_60M_decile][order(V1)]


# upper case
st[, zone := fcase(zone=='urban', 'Urban',
                   zone=='rural', 'Rural',
                   default = NA)]


# turnout quantiles
sth <- st[year==2022,c("id_secao","comparecimento_t1")]
colnames(sth) <- c("id_secao","comparecimento_hist")
st <- merge(st, sth, by="id_secao", all.x = T)

st[,summary(comparecimento_hist)]
st[,summary(idade_60M)]
st[,summary(educacao_1)]

st[, comparecimento_hist_decile := cut(comparecimento_hist,
                             breaks = quantile(comparecimento_hist, na.rm=T,
                                               probs = seq(0, 1 , by = .1)),
                             include.lowest = TRUE,
                             ordered_result = TRUE,
                             labels = 1:10) ,   by = code_muni]


st[, mean(comparecimento_hist, na.rm=T), by=comparecimento_hist_decile][order(V1)]




# by Urban Vs Rural ------------------------------------------


reg_urban <- function(z){  # z = 'Urban'
 
 # select group
 temp_df_section <- st[ zone == z, ]
 
 m1 = feols(variacao_comparecimento ~ i(year, pl1, "2018") | id_secao+year,
            cluster = 'code_muni',
            data = temp_df_section)
 
 m2 = feols(variacao_comparecimento ~ i(year, pl1, "2018") | id_secao+year,
            cluster = 'code_muni',
            weights = ~ipw2,
            data = temp_df_section)
 
 
 output_m1 <- data.frame(Weighted = 'No',
                         zone = z,
                         coef = m1$coeftable[1, 1],
                         se = m1$coeftable[1, 2])
 
 output_m2 <- data.frame(Weighted = 'Yes',
                         zone = z,
                         coef = m2$coeftable[1, 1],
                         se = m2$coeftable[1, 2])
 
 output <- rbind(output_m1, output_m2)
 setDT(output)
 output[, ymin := coef - 1.96*se]
 output[, ymax := coef + 1.96*se]
 return(output)
}

output_urban <- purrr::map(.x = c('Urban', 'Rural'),
                           .f = reg_urban) |> rbindlist()




# by Education ------------------------------------------

reg_edu <- function(e){  # e = 3
 
 # select group
 temp_df_section <- st[ educacao_1_decile == e, ]
 
 # step 2 regression
 m1 = feols(variacao_comparecimento ~ i(year, pl1, "2018") | id_secao+year,
            cluster = 'code_muni',
            data = temp_df_section)
 
 m2 = feols(variacao_comparecimento ~ i(year, pl1, "2018") | id_secao+year,
            cluster = 'code_muni',
            weights = ~ipw2,
            data = temp_df_section)
 
 
 output_m1 <- data.frame(Weighted = 'No',
                         edu_cat = as.numeric(e),
                         coef = m1$coeftable[1, 1],
                         se = m1$coeftable[1, 2])
 
 output_m2 <- data.frame(Weighted = 'Yes',
                         edu_cat = as.numeric(e),
                         coef = m2$coeftable[1, 1],
                         se = m2$coeftable[1, 2])
 
 output <- rbind(output_m1, output_m2)
 setDT(output)
 output[, ymin := coef - 1.96*se]
 output[, ymax := coef + 1.96*se]
 return(output)
}


output_edu <- purrr::map(.x = levels(st$educacao_1_decile),
                         .f = reg_edu) |> rbindlist()




# by Density -----------------------------------------


reg_dens <- function(q){  # q = 5
 message(q)
 temp_df_section <- st[ num_1000_decile == q]
 
 
 # step 2 regression
 m1 = feols(variacao_comparecimento ~ i(year, pl1, "2018") | id_secao+year,
            cluster = 'code_muni',
            data = temp_df_section)
 
 m2 = feols(variacao_comparecimento ~ i(year, pl1, "2018") | id_secao+year,
            cluster = 'code_muni',
            weights = ~ipw2,
            data = temp_df_section)
 
 
 output_m1 <- data.frame(Weighted = 'No',
                         dens_cat = q,
                         coef = m1$coeftable[1, 1],
                         se = m1$coeftable[1, 2])
 
 output_m2 <- data.frame(Weighted = 'Yes',
                         dens_cat = q,
                         coef = m2$coeftable[1, 1],
                         se = m2$coeftable[1, 2])
 
 output <- rbind(output_m1, output_m2)
 setDT(output)
 output[, ymin := coef - 1.96*se]
 output[, ymax := coef + 1.96*se]
 return(output)
}


# run regressions
output_dens <- purrr::map(.x = levels(st$num_1000_decile),
                          .f = reg_dens) |> rbindlist()




# by Age  ------------------------------------------


reg_age <- function(e){  # e = '(0.252,0.309]'
 
 # select group
 temp_df_section <- st[ idade_60M_decile == e, ]
 
 # step 2 regression
 m1 = feols(variacao_comparecimento ~ i(year, pl1, "2018") | id_secao+year,
            cluster = 'code_muni',
            data = temp_df_section)
 
 m2 = feols(variacao_comparecimento ~ i(year, pl1, "2018") | id_secao+year,
            cluster = 'code_muni',
            weights = ~ipw2,
            data = temp_df_section)
 
 
 output_m1 <- data.frame(Weighted = 'No',
                         age_cat = e,
                         coef = m1$coeftable[1, 1],
                         se = m1$coeftable[1, 2])
 
 output_m2 <- data.frame(Weighted = 'Yes',
                         age_cat = e,
                         coef = m2$coeftable[1, 1],
                         se = m2$coeftable[1, 2])
 
 output <- rbind(output_m1, output_m2)
 setDT(output)
 output[, ymin := coef - 1.96*se]
 output[, ymax := coef + 1.96*se]
 return(output)
}


output_age <- purrr::map(.x = levels(st$idade_60M_decile),
                         .f = reg_age) |> rbindlist()


# by turnout  ------------------------------------------


reg_tur <- function(e){  # e = '(0.252,0.309]'
 
 # select group
 temp_df_section <- st[ comparecimento_hist_decile == e, ]
 
 # step 2 regression
 m1 = feols(variacao_comparecimento ~ i(year, pl1, "2018") | id_secao+year,
            cluster = 'code_muni',
            data = temp_df_section)
 
 m2 = feols(variacao_comparecimento ~ i(year, pl1, "2018") | id_secao+year,
            cluster = 'code_muni',
            weights = ~ipw2,
            data = temp_df_section)
 
 
 output_m1 <- data.frame(Weighted = 'No',
                         tur_cat = e,
                         coef = m1$coeftable[1, 1],
                         se = m1$coeftable[1, 2])
 
 output_m2 <- data.frame(Weighted = 'Yes',
                         tur_cat = e,
                         coef = m2$coeftable[1, 1],
                         se = m2$coeftable[1, 2])
 
 output <- rbind(output_m1, output_m2)
 setDT(output)
 output[, ymin := coef - 1.96*se]
 output[, ymax := coef + 1.96*se]
 return(output)
}


output_tur <- purrr::map(.x = levels(st$comparecimento_hist_decile),
                         .f = reg_tur) |> rbindlist()





# Assemble figure with IPW --------------------------------------------------------------------

# get max and min y values
values <- lapply(c(output_urban$ymax, output_urban$ymin, 
                   output_edu$ymax, output_edu$ymin,
                   output_dens$ymax, output_dens$ymin,
                   output_age$ymax, output_age$ymin,
                   output_tur$ymax, output_tur$ymin),
                 FUN = base::abs)
values <- unlist(values)
max_y <- max(values)
min_y <- -1*max_y


default_theme <- list( ylim(c(min_y, max_y)) ,
                       scale_color_jama() ,
                       theme_classic() ,
                       theme(text = element_text(size=9),
                             
                             plot.title = element_text(hjust=0.5))
                       )

# urban
fig_urban_var <- 
 ggplot(data = output_urban, aes(x= zone, y=coef, color=Weighted)) +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 geom_point(position = position_dodge2(width = .2)) +
 geom_pointrange(position = position_dodge2(width = .2),
                 aes(x=zone, y=coef,
                     ymin = ymin,
                     ymax = ymax)) +
 labs(y='Estimate and 95% Conf. Int.', x = '') +
 default_theme + 
 theme(legend.position="none")






# education
fig_edu_var <- ggplot(data = output_edu, aes(x= edu_cat, y=coef, color=Weighted)) +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 geom_point(position = position_dodge2(width = .8)) +
 geom_pointrange(position = position_dodge2(width = .8),
                 aes(x= edu_cat, y=coef,
                     ymin = ymin,
                     ymax = ymax)) +
 labs(y='', x= 'Deciles of education') +
 scale_x_continuous(breaks = 1:10) +
 default_theme + 
 scale_color_jama(guide = guide_legend()) +
 theme(legend.position="bottom")



# density
fig_dens_var <- 
 ggplot(data = output_dens, aes(x= as.numeric(dens_cat), y=coef)) +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 geom_line(aes(color=Weighted)) +
 geom_ribbon(aes(fill=Weighted, ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
 labs(y='', x='Deciles of density\nof polling stations') +
 scale_x_continuous(breaks = 1:10) +
 default_theme + 
 theme(legend.position="none")



# age
ggplot(data = output_age, aes(x= age_cat, y=coef, color=Weighted)) +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 geom_point(position = position_dodge2(width = .3)) +
 geom_pointrange(position = position_dodge2(width = .3),
                 aes(x=age_cat, y=coef,
                     ymin = ymin,
                     ymax = ymax)) +
 labs(x= 'Deciles of low\neldery individuals') +
 theme_classic()




p_var <- fig_urban_var + fig_edu_var + fig_dens_var +
 plot_annotation(tag_levels = 'A') +
 plot_layout(ncol = 3)

p_var

 

# ##### save plot
# 
# ggsave(plot=p_var, file= './figures/fig_2_heterogeneity_var_ipw.pdf', 
#        width = 17.8, height = 8, units='cm', dpi = 300)
# 
# 





# NO ipw --------------------------------------------------------------------

output_urban <- subset(output_urban, Weighted == 'No')
output_edu <- subset(output_edu, Weighted == 'No')
output_dens <- subset(output_dens, Weighted == 'No')
output_age <- subset(output_age, Weighted == 'No')
output_tur <- subset(output_tur, Weighted == 'No')


# get max and min y values
values <- lapply(c(output_urban$ymax, output_urban$ymin, 
                   output_edu$ymax, output_edu$ymin,
                   output_dens$ymax, output_dens$ymin,
                   output_age$ymax, output_age$ymin,
                   output_tur$ymax, output_tur$ymin), FUN = base::abs)
values <- unlist(values)
max_y <- max(values)
max_y <- ifelse(max_y < 0.05, 0.05, max_y)
min_y <- -1*max_y


default_theme <- list(  scale_y_continuous(limits = c(min_y, max_y), labels = scales::percent),
                       scale_color_jama() ,
                       theme_classic() ,
                       theme(text = element_text(size=9),
                             plot.title = element_text(hjust=0.5))
)

# urban
fig_urban_var <- 
 ggplot(data = output_urban, aes(x= zone, y=coef)) +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 geom_point(color='#0e8bb1') +
 geom_pointrange(color='#0e8bb1',
                 aes(x=zone, y=coef,
                     ymin = ymin,
                     ymax = ymax)) +
 labs(y='Estimate and 95% Conf. Int.', x = '') +
 default_theme + 
 theme(legend.position="none")






# education
fig_edu_var <- 
 ggplot(data = output_edu, aes(x= edu_cat, y=coef, color=Weighted)) +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 geom_pointrange(size=.2,
                 color='#d6a525',
                 aes(x=edu_cat, y=coef,
                     ymin = ymin,
                     ymax = ymax)) +
 labs(y='', x= 'Deciles of\neducation attainment\nof polling stations',
      title="educational attainment") +
 scale_x_continuous(breaks = 1:10) +
 default_theme + 
 scale_color_jama(guide = guide_legend()) +
 theme(legend.position="bottom")




# density
fig_dens_var <- 
 ggplot(data = output_dens, aes(x= as.numeric(dens_cat), y=coef)) +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 # geom_line(color='#B24745FF') +
 # geom_ribbon(fill='#B24745FF', aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
 geom_pointrange(size=.2,
                 color='#B24745FF',
                 aes(ymin = ymin,
                     ymax = ymax)) +
 labs(y='', x='Deciles of density\nof polling stations', title="urban density") +
 scale_x_continuous(breaks = 1:10) +
 default_theme + 
 theme(legend.position="none")


# age
fig_age_var <- 
 ggplot(data = output_age, aes(x= as.numeric(age_cat), y=coef)) +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 # geom_line(color='#B24745FF') +
 # geom_ribbon(fill='#B24745FF', aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
 geom_pointrange(size=.2,
                 color='forestgreen',
                 aes(ymin = ymin,
                     ymax = ymax)) +
 labs(y='', x='Deciles of age\nby polling stations', title="voter's age") +
 scale_x_continuous(breaks = 1:10) +
 default_theme + 
 theme(legend.position="none")


# turnout
fig_tur_var <- 
 ggplot(data = output_tur, aes(x= as.numeric(tur_cat), y=coef)) +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 # geom_line(color='#B24745FF') +
 # geom_ribbon(fill='#B24745FF', aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
 geom_pointrange(size=.2,
                 color='navy',
                 aes(ymin = ymin,
                     ymax = ymax)) +
 labs(y='', x='Deciles of historical turnout\nby polling stations', title="historical turnout") +
 scale_x_continuous(breaks = 1:10) +
 default_theme + 
 theme(legend.position="none")


fig_edu_var
fig_dens_var
fig_age_var
fig_tur_var


saveRDS(fig_edu_var, file='./figures2/fig_3A.rds')
saveRDS(fig_dens_var, file='./figures2/fig_3B.rds')
saveRDS(fig_age_var, file='./figures2/fig_3C.rds')
saveRDS(fig_tur_var, file='./figures2/fig_3D.rds')



# age
ggplot(data = output_age, aes(x= age_cat, y=coef, color=Weighted)) +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 geom_point(position = position_dodge2(width = .3)) +
 geom_pointrange(position = position_dodge2(width = .3),
                 aes(x=age_cat, y=coef,
                     ymin = ymin,
                     ymax = ymax)) +
 labs(x= 'Deciles of low\neldery individuals') +
 theme_classic()




p_var <- fig_urban_var + fig_edu_var + fig_dens_var +
 plot_annotation(tag_levels = 'A') +
 plot_layout(ncol = 3)

p_var



# ##### save plot
# 
# ggsave(plot=p_var, file= './figures/fig_2_heterogeneity_var.pdf', 
#        width = 17.8, height = 8, units='cm', dpi = 300)


