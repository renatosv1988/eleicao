

# ipw balancing ----------------------------------------------------------------------
summary(df_muni$variacao_comparecimento_2018_muni)
summary(df_muni$votos_jair_muni_validos_p)
summary(df_muni$biometria)
table(df_muni$variacao_comparecimento_2018_muni)

l <- lm(passe_livre_1~ gov_2t + name_region  + 
         QT_APTOS_log + pib_log  +  votos_jair_muni_validos_p ,
        data = df_muni)

summary(l)

step1 <- glm(passe_livre_2~ gov_2t + name_region  + 
              QT_APTOS_log + pib_log   + votos_jair_muni_validos_p ,
             
             family = binomial(link = 'logit'),
             data = df_muni)


summary(step1)
df_muni[, ipw := (passe_livre_1 / fitted(step1)) + ((1 - passe_livre_1) / ( 1- fitted(step1)))]

hist(df_muni$ipw)
summary(df_muni$ipw)




# # balancing AFTER ipw 
# a <- df_muni[,   .(QT_APTOS          = weighted.mean(x=QT_APTOS, w = ipw),
#                    QT_APTOS_log      = weighted.mean(x=QT_APTOS_log, w = ipw),
#                    votos_jair_muni_total_p = weighted.mean(x=votos_jair_muni_total_p, w = ipw),
#                    mean_dist         = weighted.mean(x=mean_dist, w = ipw, na.rm=T),
#                    mean_dens_1000    = weighted.mean(x=mean_dens_1000, w = ipw),
#                    educacao_1        = weighted.mean(x=educacao_1, w = ipw),
#                    gov_2t            = weighted.mean(x=gov_2t, w = ipw),
#                    pib_log           = weighted.mean(x=pib_log, w = ipw)),
#              by=passe_livre_2]
# 
# 
# 
# # transpose
# c <- data.table::transpose(a)
# 
# # get row and colnames in order
# setnames(c, rownames(a))
# c$variable <- colnames(a)
# setcolorder(c, 'variable')
# 




# Model 1. Average effects ----------------------------------------------------------------------

# merge ipw info of muni to sections
setDT(df_sections)[df_muni, on='code_muni', ipw := i.ipw]



# reg
step2_2022 <- fixest::feols(comparecimento_2022~turno1_dummy + passe_livre_1 + turno1_dummy:passe_livre_1, 
                            fixef = 'id_secao', 
                            cluster = 'code_muni',
                            weights = ~ipw,
                            data = df_sections)

step2_2018 <- fixest::feols(comparecimento_2018~turno1_dummy + passe_livre_1 + turno1_dummy:passe_livre_1, 
                            fixef = 'id_secao', 
                            cluster = 'code_muni',
                            weights = ~ipw,
                            data = df_sections)


summary(step2_2022)
summary(step2_2018)

model_list <- list(step2_2022, step2_2018)
names(model_list) <- c('Turnout 2022', 'Turnout 2018')


# export table
modelsummary::modelsummary(model_list, stars = T, output = './tables/table_did_avg_comparecimento_T1_.html')


# export figure
temp_df22 <- df_sections[, .(mean_value=mean(comparecimento_2022, na.rm=T),
                             p25 =  quantile(comparecimento_2022,0.25, na.rm=T),
                             p75 =  quantile(comparecimento_2022,0.75, na.rm=T)), by=.(NR_TURNO, passe_livre_2)]


temp_df18 <- df_sections[, .(mean_value=mean(comparecimento_2018, na.rm=T),
                             p25 =  quantile(comparecimento_2018,0.25, na.rm=T),
                             p75 =  quantile(comparecimento_2018,0.75, na.rm=T)), by=.(NR_TURNO, passe_livre_2)]

temp_df22$comp_year <- 2022
temp_df18$comp_year <- 2018

temp_df <- rbind(temp_df22, temp_df18)
ggplot() +
 geom_line(data=temp_df, aes(x=NR_TURNO, y=mean_value, color=factor(passe_livre_2)),
           position = position_dodge2(width = .2)) +
 geom_pointrange(data=temp_df,
                 position = position_dodge2(width = .2),
                 show.legend = FALSE,
                 aes(x=NR_TURNO, y=mean_value, color=factor(passe_livre_2),
                     ymin = p25,
                     ymax = p75)) +
 facet_wrap(.~comp_year) +
 labs(y='Voter turnout', x = 'Election round', color='Free transit') +
 scale_x_continuous( breaks =  c(1, 2)) +
 scale_y_continuous(labels = scales::percent) +
 #scale_color_npg() +
 #scale_color_uchicago() +
 scale_color_jama() +
 theme_classic()

ggsave('./figures/turnout_2022_placebo2018_average.png', 
       height = 8, width = 14, 
       units='cm')



# Model 2. by REGIAO ------------------------------------------

reg_region <- function(r){  # r = 'Norte'
 
 # select group
 temp_df_muni <- df_muni[ name_region == r, ]
 temp_df_section <- df_sections[ name_region == r, ]
 
 # calculate ipw
 temp_step1 <- glm(passe_livre_1 ~ gov_2t + QT_APTOS_log + pib_log + votos_jair_muni_validos_p, 
                   family = binomial(link = 'logit'),
                   data = temp_df_muni)
 
 # merge ipw info of muni to sections
 temp_df_muni[, ipw := (passe_livre_1 / fitted(temp_step1)) + ((1 - passe_livre_1) / ( 1- fitted(temp_step1)))]
 setDT(temp_df_section)[temp_df_muni, on='code_muni', ipw := i.ipw]
 
 # step 2 regression
 step2_2022 <- fixest::feols(comparecimento_2022~turno1_dummy + passe_livre_1 + turno1_dummy:passe_livre_1, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw,
                             data = temp_df_section)
 
 step2_2018 <- fixest::feols(comparecimento_2018~turno1_dummy + passe_livre_1 + turno1_dummy:passe_livre_1, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw,
                             data = temp_df_section)
 
 output2022 <- data.frame(comp_year = 2022,
                          name_region = r,
                          coef = step2_2022$coeftable[2, 1],
                          se = step2_2022$coeftable[2, 2])
 
 output2018 <- data.frame(comp_year = 2018,
                          name_region = r,
                          coef = step2_2018$coeftable[2, 1],
                          se = step2_2018$coeftable[2, 2])
 
 output <- rbind(output2022, output2018)
 return(output)
}

output_did_region <- purrr::map(.x = unique(df_sections$name_region),
                                .f = reg_region) |> rbindlist()


ggplot(data = output_did_region, aes(x= name_region, y=coef, color=factor(comp_year))) +
 geom_point(position = position_dodge2(width = .3)) +
 geom_pointrange(position = position_dodge2(width = .3),
                 aes(x=name_region, y=coef,
                     ymin = coef - 1.96*se,
                     ymax = coef + 1.96*se)) +
 geom_hline(yintercept = 0, color='gray20') +
 theme_classic()

ggsave('./figures/turnout_2022_placebo2018_regions.png', 
       height = 8, width = 14, 
       units='cm')



# Model 2. URBAN vs RURAL  ------------------------------------------


reg_urban <- function(z){  # z = 'urban'
 
 # select group
 temp_df_section <- df_sections[ zone == z, ]
 
 # step 2 regression
 step2_2022 <- fixest::feols(comparecimento_2022~turno1_dummy + passe_livre_1 + turno1_dummy:passe_livre_1, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw,
                             data = temp_df_section)
 
 step2_2018 <- fixest::feols(comparecimento_2018~turno1_dummy + passe_livre_1 + turno1_dummy:passe_livre_1, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw,
                             data = temp_df_section)
 
 output2022 <- data.frame(comp_year = 2022,
                          zone = z,
                          coef = step2_2022$coeftable[2, 1],
                          se = step2_2022$coeftable[2, 2])
 
 output2018 <- data.frame(comp_year = 2018,
                          zone = z,
                          coef = step2_2018$coeftable[2, 1],
                          se = step2_2018$coeftable[2, 2])
 
 output <- rbind(output2022, output2018)
 return(output)
}

output_did_urban <- purrr::map(.x = unique(df_sections$zone),
                               .f = reg_urban) |> rbindlist()


ggplot(data = output_did_urban, aes(x= zone, y=coef, color=factor(comp_year))) +
 geom_point(position = position_dodge2(width = .3)) +
 geom_pointrange(position = position_dodge2(width = .3),
                 aes(x=zone, y=coef,
                     ymin = coef - 1.96*se,
                     ymax = coef + 1.96*se)) +
 geom_hline(yintercept = 0, color='gray20') +
 theme_classic()

ggsave('./figures/turnout_2022_placebo2018_urban.png', 
       height = 8, width = 14, 
       units='cm')




# modelo 3a - heterogenidade educacao ------------------------------------------

table(df_sections$educacao_1_quintile)


reg_edu <- function(e){  # e = 3
 
 # select group
 temp_df_section <- df_sections[ educacao_1_decile == e, ]
 
 # step 2 regression
 step2_2022 <- fixest::feols(comparecimento_2022~turno1_dummy + passe_livre_1 + turno1_dummy:passe_livre_1, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw,
                             data = temp_df_section)
 
 step2_2018 <- fixest::feols(comparecimento_2018~turno1_dummy + passe_livre_1 + turno1_dummy:passe_livre_1, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw,
                             data = temp_df_section)
 
 output2022 <- data.frame(comp_year = 2022,
                          edu_cat = as.numeric(e),
                          coef = step2_2022$coeftable[2, 1],
                          se = step2_2022$coeftable[2, 2])
 
 output2018 <- data.frame(comp_year = 2018,
                          edu_cat = as.numeric(e),
                          coef = step2_2018$coeftable[2, 1],
                          se = step2_2018$coeftable[2, 2])
 
 output <- rbind(output2022, output2018)
 return(output)
}




output3a <- purrr::map(.x = levels(df_sections$educacao_1_decile),
                       .f = reg_edu) |> rbindlist()




ggplot(data = output3a, aes(x= factor(edu_cat), y=coef, color=factor(comp_year))) +
 geom_point(position = position_dodge2(width = .3)) +
 geom_pointrange(position = position_dodge2(width = .3),
                 aes(x=factor(edu_cat), y=coef,
                     ymin = coef - 1.96*se,
                     ymax = coef + 1.96*se)) +
 geom_hline(yintercept = 0, color='gray20') +
 labs(x= 'Deciles of low\nsocioeconomic individuals') +
 theme_classic()

ggsave('./figures/turnout_2022_placebo2018_education.png', 
       height = 8, width = 14, 
       units='cm')



# modelo 3b - heterogenidade densidade de secoes -----------------------------------------

table(df_sections$num_1000_cat10)
table(df_sections$num_1000_decile)

reg_dens <- function(i){  # i = 5
 message(i)
 temp_df_section <- df_sections[ num_1000_decile == i]
 
 
 # step 2 regression
 step2_2022 <- fixest::feols(comparecimento_2022~turno1_dummy + passe_livre_1 + turno1_dummy:passe_livre_1, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw,
                             data = temp_df_section)
 
 step2_2018 <- fixest::feols(comparecimento_2018~turno1_dummy + passe_livre_1 + turno1_dummy:passe_livre_1, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw,
                             data = temp_df_section)
 
 output2022 <- data.frame(comp_year = 2022,
                          i = as.numeric(i),
                          coef = step2_2022$coeftable[2, 1],
                          se = step2_2022$coeftable[2, 2])
 
 output2018 <- data.frame(comp_year = 2018,
                          i = as.numeric(i),
                          coef = step2_2018$coeftable[2, 1],
                          se = step2_2018$coeftable[2, 2])
 
 output <- rbind(output2022, output2018)
 return(output)
}


# run regressions
output3b <- purrr::map(.x = levels(df_sections$num_1000_decile),
                       .f = reg_dens) |> rbindlist()


ggplot(data = output3b, aes(x= as.numeric(i), y=coef)) +
 geom_line(aes(color=factor(comp_year))) +
 geom_ribbon(aes(fill=factor(comp_year), ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.15) +
 geom_hline(yintercept = 0) +
 labs(x='Deciles of density of electoral sections') +
 theme_classic()


ggsave('./figures/turnout_2022_placebo2018_density.png', 
       height = 8, width = 14, 
       units='cm')



# 
# # modelo 3c - heterogenidade distancia e edu -----------------------------------------
# 
# # create socioeconomic status level
# df_sections[, ses := ifelse( educacao_1 >= .4, 'low', 'high')]
# 
# table(df_sections$ses, useNA = 'always')
# table(df_sections$num_1000_decile, useNA = 'always')
# 
# table(df_sections$ses, df_sections$num_1000_decile)
# 
# 
# # reg function
# reg_group_dist_edu <- function(i){  # i = 4 i = Inf
#  message(i)
#  temp_df <- df_sections[ num_1000_decile == i & !is.na(ses)]
# 
#  step2_low <- fixest::feols(comparecimento_2022~turno1_dummy + passe_livre_1 + turno1_dummy:passe_livre_1, 
#                         fixef = 'id_secao', 
#                         cluster = 'code_muni',
#                         weights = ~ipw,
#                         data = subset(temp_df, ses == 'low')
#                         )
#  
#  step2_high <- fixest::feols(comparecimento_2022~turno1_dummy + passe_livre_1 + turno1_dummy:passe_livre_1, 
#                             fixef = 'id_secao', 
#                             cluster = 'code_muni',
#                             weights = ~ipw,
#                             data = subset(temp_df, ses == 'high')
#                             )
#  
#  output_low <- data.frame(i = as.numeric(i),
#                           group = 'low',
#                           coef = step2_low$coeftable[2, 1],
#                           se = step2_low$coeftable[2, 2])
#  
#  output_high <- data.frame(i = as.numeric(i),
#                            group = 'high',
#                            coef = step2_high$coeftable[2, 1],
#                            se = step2_high$coeftable[2, 2])
#  
#  output <- rbind(output_low, output_high)
#  return(output)
# }
# 
# 
# # run regressions
# output3c <- purrr::map(.x = levels(df_sections$num_1000_decile),
#                      .f = reg_group_dist_edu) |> rbindlist()
# 
# 
# ggplot(data = output3c, aes(x= i, y=coef, color=group, fill=group)) +
#  geom_ribbon(aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
#  geom_line(show.legend = F) +
#  geom_hline(yintercept = 0, color='gray20') +
#  labs(x='Deciles of electoral section density', fill='SES', color='SES') +
#  scale_x_continuous( breaks =  1:10) +
#  scale_color_npg() +
#  scale_fill_npg() +
#  theme_classic()
# 





# modelo 5 - heterogenidade idosos ------------------------------------------

table(df_sections$idade_60M_decile)


reg_age <- function(e){  # e = '(0.0838,0.109]'
 
 # select group
 temp_df_section <- df_sections[ idade_60M_decile == e, ]
 
 # reg
 # step 2 regression
 step2_2022 <- fixest::feols(comparecimento_2022~turno1_dummy + passe_livre_1 + turno1_dummy:passe_livre_1, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw,
                             data = temp_df_section)
 
 step2_2018 <- fixest::feols(comparecimento_2018~turno1_dummy + passe_livre_1 + turno1_dummy:passe_livre_1, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw,
                             data = temp_df_section)
 
 output2022 <- data.frame(comp_year = 2022,
                          age_cat = e,
                          coef = step2_2022$coeftable[2, 1],
                          se = step2_2022$coeftable[2, 2])
 
 output2018 <- data.frame(comp_year = 2018,
                          age_cat = e,
                          coef = step2_2018$coeftable[2, 1],
                          se = step2_2018$coeftable[2, 2])
 
 output <- rbind(output2022, output2018)
 return(output)
}




output5 <- purrr::map(.x = levels(df_sections$idade_60M_decile),
                      .f = reg_age) |> rbindlist()


ggplot(data = output5, aes(x= age_cat, y=coef, color=factor(comp_year))) +
 geom_point(position = position_dodge2(width = .3)) +
 geom_pointrange(position = position_dodge2(width = .3),
                 aes(x=age_cat, y=coef,
                     ymin = coef - 1.96*se,
                     ymax = coef + 1.96*se)) +
 geom_hline(yintercept = 0, color='gray20') +
 labs(x= 'Deciles of low\neldery individuals') +
 theme_classic()

ggsave('./figures/turnout_2022_placebo2018_elderly.png', 
       height = 8, width = 14, 
       units='cm')








