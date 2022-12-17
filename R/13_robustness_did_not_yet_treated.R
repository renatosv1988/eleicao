library(data.table)
library(dplyr)
library(fixest)
library(DRDID)
library(ggplot2)
library(purrr)
library(modelsummary)
library(ggplot2)
library(ggsci)

options(scipen = 999)


# read data ----------------------------------------------------------------------

df_secoes_2022 <- fread("../../data/base_DiD2022_secoes.csv")

muni_ipw <- fread("../../data/ipw_municipalities.csv", encoding = "Latin-1")


# add ipw to electoral sections
st <- merge(df_secoes_2022, muni_ipw, by="code_muni", all.x = T)







# Select observations ----------------------------------------------------------------------

# metro only 6666666666666666
# st <- subset(st, metro_only == 0)


# determine passe livre in each round
st[, passe_livre_1 := max(NR_TURNO==1 & passe_livre==1), by = id_secao]
st[, passe_livre_2 := max(NR_TURNO==2 & passe_livre==1), by = id_secao]

# keep only municipalities that were eventually treated
st <- st[passe_livre_2==1, ]

# excluir cidades que SEMPRE tiveram passe livre
st <- subset(st, is.na(passe_livre_always))


# recode variables ----------------------------------------------------------------------


# create dummy for 2nd round
st[, turno2_dummy := fifelse(NR_TURNO==2, 1, 0)]
st[, turno1_dummy := fifelse(NR_TURNO==1, 1, 0)]




# determine who is treated
# passe livre in 1st round is always treated
st[, treated := fifelse(passe_livre_1 == 1, 0, 1)]



# check
st[id_secao == '4111 18 10', .(id_secao, NR_TURNO, turno2_dummy, passe_livre, passe_livre_1, passe_livre_2, treated)]
st[id_secao == '35 2 10', .(id_secao, NR_TURNO, turno2_dummy, passe_livre, passe_livre_1, passe_livre_2, treated)]




st[, table(treated, turno2_dummy)]
#>                turno2_dummy
#> passe_livre_2      0      1
#>             0  74474  74474
#>             1 135251 135251


st[, table(treated, passe_livre_2)]


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








# Model 1. Average effects ----------------------------------------------------------------------



# reg
step2_2022_ipw <- fixest::feols(comparecimento_2022~turno2_dummy + treated + turno2_dummy:treated, 
                     fixef = 'id_secao', 
                     cluster = 'code_muni',
                     weights = ~ipw2,
                     data = st)

step2_2022 <- fixest::feols(comparecimento_2022~turno2_dummy + treated + turno2_dummy:treated, 
                            fixef = 'id_secao', 
                            cluster = 'code_muni',
                           # weights = ~ipw2,
                            data = st)

step2_2018 <- fixest::feols(comparecimento_2018~turno2_dummy + treated + turno2_dummy:treated, 
                       fixef = 'id_secao', 
                       cluster = 'code_muni',
                       weights = ~ipw2,
                       data = st)


summary(step2_2022)
summary(step2_2018)

model_list <- list(step2_2022, step2_2022_ipw)
names(model_list) <- c('Unweighted', 'Weighted')


# export table
modelsummary::modelsummary(model_list, stars = T
                          # ,  output = './tables/si_table_s1.tex'
                           )





# Model 2. URBAN vs RURAL  ------------------------------------------


reg_urban <- function(z){  # z = 'Urban'
 
 # select group
 temp_df_section <- st[ zone == z, ]
 
 # step 2 regression
 step2_2022 <- fixest::feols(comparecimento_2022~turno2_dummy + treated + turno2_dummy:treated, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             # weights = ~ipw2,
                             data = temp_df_section)
 
 step2_2022_ipw <- fixest::feols(comparecimento_2022~turno2_dummy + treated + turno2_dummy:treated, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw2,
                             data = temp_df_section)
 
 step2_2018 <- fixest::feols(comparecimento_2018~turno2_dummy + treated + turno2_dummy:treated, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw2,
                             data = temp_df_section)
 
 output2022 <- data.frame(comp_year = 2022,
                          Weighted = 'No',
                          zone = z,
                          coef = step2_2022$coeftable[2, 1],
                          se = step2_2022$coeftable[2, 2])
 
 output2022_ipw <- data.frame(comp_year = 2022,
                              Weighted = 'Yes',
                              zone = z,
                              coef = step2_2022_ipw$coeftable[2, 1],
                              se = step2_2022_ipw$coeftable[2, 2])
 
 output2018 <- data.frame(comp_year = 2018,
                          Weighted = 'Yes',
                          zone = z,
                          coef = step2_2018$coeftable[2, 1],
                          se = step2_2018$coeftable[2, 2])
 
 output <- rbind(output2022, output2022_ipw, output2018)
 setDT(output)
 output[, ymin := coef - 1.96*se]
 output[, ymax := coef + 1.96*se]
 return(output)
 }

output_urban <- purrr::map(.x = unique(st$zone),
                                .f = reg_urban) |> rbindlist()





# modelo 3a - heterogenidade educacao ------------------------------------------



reg_edu <- function(e){  # e = 3
 
 # select group
 temp_df_section <- st[ educacao_1_decile == e, ]
 
 # step 2 regression
 step2_2022 <- fixest::feols(comparecimento_2022~turno2_dummy + treated + turno2_dummy:treated, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                         #    weights = ~ipw2,
                             data = temp_df_section)
 
 
 step2_2022_ipw <- fixest::feols(comparecimento_2022~turno2_dummy + treated + turno2_dummy:treated, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw2,
                             data = temp_df_section)
 
 step2_2018 <- fixest::feols(comparecimento_2018~turno2_dummy + treated + turno2_dummy:treated, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw2,
                             data = temp_df_section)
 
 output2022 <- data.frame(comp_year = 2022,
                          Weighted = 'No',
                          edu_cat = as.numeric(e),
                          coef = step2_2022$coeftable[2, 1],
                          se = step2_2022$coeftable[2, 2])
 
 output2022_ipw <- data.frame(comp_year = 2022,
                              Weighted = 'Yes',
                              edu_cat = as.numeric(e),
                              coef = step2_2022_ipw$coeftable[2, 1],
                              se = step2_2022_ipw$coeftable[2, 2])
 
 output2018 <- data.frame(comp_year = 2018,
                          Weighted = 'Yes',
                          edu_cat = as.numeric(e),
                          coef = step2_2018$coeftable[2, 1],
                          se = step2_2018$coeftable[2, 2])
 
 output <- rbind(output2022, output2022_ipw, output2018)
 setDT(output)
 output[, ymin := coef - 1.96*se]
 output[, ymax := coef + 1.96*se]
 return(output)
 }




output_edu <- purrr::map(.x = levels(st$educacao_1_decile),
                       .f = reg_edu) |> rbindlist()




# modelo 3b - heterogenidade densidade de secoes -----------------------------------------


reg_dens <- function(i){  # i = 5
 message(i)
 temp_df_section <- st[ num_1000_decile == i]
 
 
 # step 2 regression
 step2_2022 <- fixest::feols(comparecimento_2022~turno2_dummy + treated + turno2_dummy:treated, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             # weights = ~ipw2,
                             data = temp_df_section)
 
 step2_2022_ipw <- fixest::feols(comparecimento_2022~turno2_dummy + treated + turno2_dummy:treated, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw2,
                             data = temp_df_section)
 
 step2_2018 <- fixest::feols(comparecimento_2018~turno2_dummy + treated + turno2_dummy:treated, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw2,
                             data = temp_df_section)
 
 output2022 <- data.frame(comp_year = 2022,
                          Weighted = 'No',
                          dens_cat = i,
                          coef = step2_2022$coeftable[2, 1],
                          se = step2_2022$coeftable[2, 2])
 
 output2022_ipw <- data.frame(comp_year = 2022,
                              Weighted = 'Yes',
                              dens_cat = i,
                              coef = step2_2022_ipw$coeftable[2, 1],
                              se = step2_2022_ipw$coeftable[2, 2])
 
 
 output2018 <- data.frame(comp_year = 2018,
                          Weighted = 'Yes',
                          dens_cat = i,
                          coef = step2_2018$coeftable[2, 1],
                          se = step2_2018$coeftable[2, 2])
 
 output <- rbind(output2022, output2022_ipw, output2018)
 setDT(output)
 output[, ymin := coef - 1.96*se]
 output[, ymax := coef + 1.96*se]
 return(output)
 }


# run regressions
output_dens <- purrr::map(.x = levels(st$num_1000_decile),
                      .f = reg_dens) |> rbindlist()







# Assemble figure  WITH IPW --------------------------------------------------------------------

output_urban <- subset(output_urban, comp_year == 2022)
output_edu <- subset(output_edu, comp_year == 2022)
output_dens <- subset(output_dens, comp_year == 2022)


# get max and min y values
values <- lapply(c(output_urban$ymax, output_urban$ymin, 
                   output_edu$ymax, output_edu$ymin,
                   output_dens$ymax, output_dens$ymin), FUN = base::abs)
values <- unlist(values)
max_y <- max(values)
min_y <- -1*max_y


default_theme <- list( ylim(c(min_y, max_y)) ,
                       scale_color_jama() ,
                       theme_classic() ,
                       theme(text = element_text(size=9))
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





p <- fig_urban_var + fig_edu_var + fig_dens_var +
 plot_annotation(tag_levels = 'A') +
 plot_layout(ncol = 3)

p



##### save plot

ggsave(plot=p, file= './figures/si_2_not_yet_treated_ipw.pdf', 
       width = 17.8, height = 8, units='cm', dpi = 300)











# NO IPW  --------------------------------------------------------------------

output_urban <- subset(output_urban, comp_year == 2022)
output_edu <- subset(output_edu, comp_year == 2022)
output_dens <- subset(output_dens, comp_year == 2022)

output_urban <- subset(output_urban, Weighted == 'No')
output_edu <- subset(output_edu, Weighted == 'No')
output_dens <- subset(output_dens, Weighted == 'No')


# get max and min y values
values <- lapply(c(output_urban$ymax, output_urban$ymin, 
                   output_edu$ymax, output_edu$ymin,
                   output_dens$ymax, output_dens$ymin), FUN = base::abs)
values <- unlist(values)
max_y <- max(values)
max_y <- ifelse(max_y < 0.05, 0.05, max_y)
min_y <- -1*max_y


default_theme <- list( ylim(c(min_y, max_y)) ,
                       theme_classic() ,
                       theme(text = element_text(size=9))
                       )

# urban
fig_urban_var <- 
 ggplot(data = output_urban, aes(x= zone, y=coef, color=Weighted)) +
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
 geom_point(color='#d6a525') +
 geom_pointrange(color='#d6a525',
                 aes(x=edu_cat, y=coef,
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
 # geom_line(color='#B24745FF') +
 # geom_ribbon(fill='#B24745FF', aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
 geom_point(color='#B24745FF') +
 geom_pointrange(color='#B24745FF',
                 aes(ymin = ymin,
                     ymax = ymax)) +
 labs(y='', x='Deciles of density\nof polling stations') +
 scale_x_continuous(breaks = 1:10) +
 default_theme + 
 theme(legend.position="none")





p <- fig_urban_var + fig_edu_var + fig_dens_var +
 plot_annotation(tag_levels = 'A') +
 plot_layout(ncol = 3)

p



##### save plot

ggsave(plot=p, file= './figures/si_3_not_yet_treated.pdf', 
       width = 17.8, height = 8, units='cm', dpi = 300)

