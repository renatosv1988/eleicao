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

     # # 66666 
     # df_secoes_2022 <- df_secoes_2022[SG_UF != "SP"]

# percentage of voters living in cities with passe livre
df_secoes_2022[NR_TURNO==2 & passe_livre==1, sum(QT_APTOS)] / df_secoes_2022[NR_TURNO==2, sum(QT_APTOS)]
#> 0.4798155



# Select observations ----------------------------------------------------------------------

# excluir secoes de cidades sem sistema de ônibus
df_secoes_2022 <- subset(df_secoes_2022, dummy_pt==1)


# excluir cidades que SEMPRE tiveram passe livre
df_secoes_2022 <- subset(df_secoes_2022, is.na(passe_livre_always))
df_secoes_2022[, table(NR_TURNO, passe_livre)]

#>            passe_livre
#> NR_TURNO      0      1
#>        1 258217  73829
#>        2 129640 202406


# identify treated in the 1st round
df_secoes_2022[, passe_livre_1 := max(NR_TURNO==1 & passe_livre==1), by = id_secao]
table(df_secoes_2022$passe_livre_1)

df_secoes_2022[, table(NR_TURNO, passe_livre_1)]

#>          passe_livre_1
#> NR_TURNO      0      1
#>        1 258217  73829
#>        2 258217  73829


# drop always treated (1st round)
df_sections <- df_secoes_2022[ passe_livre_1 != 1]
df_sections[, table(NR_TURNO, passe_livre)]
#>            passe_livre
#> NR_TURNO      0      1
#>        1 258217      0
#>        2 129640 128577



# identify treated in turno 2
df_sections[, passe_livre_2 := max(passe_livre==1), by = id_secao]
df_sections[, turno2_dummy := fifelse(NR_TURNO==2, 1, 0)]

df_sections[, table(NR_TURNO, passe_livre_2)]
#>          passe_livre_2
#> NR_TURNO      0      1
#>        1 129640 128577
#>        2 129640 128577



# recode variables ----------------------------------------------------------------------

# share of PT votes
df_sections[, votos_lula_p := sum(votos_lula) / sum(votos_validos), by = .(ANO_ELEICAO, NR_TURNO, id_secao)]

      # # 66666 comparecimento baeyseano
      # df_sections <- df_sections[order(ANO_ELEICAO, id_secao, NR_TURNO)]
      # df_sections[, comparecimento_2022b := (comparecimento_2022 )/shift(comparecimento_2022), by =.(ANO_ELEICAO, id_secao)]
      # df_sections[NR_TURNO==1, comparecimento_2022b := 1]
      # 
      # summary(df_sections$comparecimento_2022b)
      # 
      # df_sections[1:4, .(NR_TURNO, comparecimento_2022, comparecimento_2022b)] |> View()
      # 
      # # 2018
      # eleicao_2018 <- fread('../../data/base_DiD2018_secoes.csv')
      # eleicao_2018 <- eleicao_2018[order(ANO_ELEICAO, id_secao, NR_TURNO)]
      # eleicao_2018[, comparecimento_2018b := (comparecimento )/shift(comparecimento), by =.(ANO_ELEICAO, id_secao)]
      # eleicao_2018[NR_TURNO==1, comparecimento_2018b := 1]
      # 
      # summary(eleicao_2018$comparecimento_2018b)
      # eleicao_2018[1:4, .(NR_TURNO, comparecimento, comparecimento_2018b)] |> View()
      # 
      # df_sections <- left_join(df_sections, eleicao_2018[, .(NR_TURNO, id_secao, comparecimento_2018b)],
      #                      by=c('NR_TURNO','id_secao'), all.x = T)
      # 
      # summary(df_sections$comparecimento_2018)
      # summary(df_sections$comparecimento_2018b)

# discretize edu
my_breaks <- seq(0, 0.7, by=.1)
my_breaks <- c(my_breaks, 1)
df_sections[, educacao_1_cat := cut(educacao_1,
                            breaks= my_breaks,
                            labels= my_breaks[-1])]

table(df_sections$educacao_1_cat, useNA = 'always')


# education quantiles
df_sections[, educacao_1_decile := cut(educacao_1,
                                      breaks = quantile(educacao_1, na.rm=T,
                                                        probs = seq(0, 1 , by = .1)),
                                      include.lowest = TRUE,
                                      ordered_result = TRUE,
                                      labels = 1:10) ]

df_sections[, educacao_1_quintile := cut(educacao_1,
                                 breaks = quantile(educacao_1, na.rm=T,
                                                   probs = seq(0, 1 , by = .2)),
                                 include.lowest = TRUE,
                                 ordered_result = TRUE,
                                 labels = 1:5)
                                 ]


table(df_sections$educacao_1_quintile)
table(df_sections$educacao_1_decile)


# discretize density by 10
my_breaks <- seq(0, 150, by=10)
my_breaks <- c(my_breaks, Inf)
df_sections[, num_1000_cat10 := cut(num_1000,
                          breaks= my_breaks,
                          labels= my_breaks[-1],
                          include.lowest = T)]


table(df_sections$num_1000_cat10, useNA = 'always')


# discretize density by 20
my_breaks <- seq(0, 140, by=20)
my_breaks <- c(my_breaks, Inf)
df_sections[, num_1000_cat20 := cut(num_1000,
                            breaks= my_breaks,
                            labels= my_breaks[-1],
                            include.lowest = T)]


table(df_sections$num_1000_cat20, useNA = 'always')


# density quantiles
df_sections[, num_1000_decile := cut(num_1000,
                                       breaks = quantile(num_1000, na.rm=T,
                                                         probs = seq(0, 1 , by = .1)),
                                       include.lowest = TRUE,
                                       ordered_result = TRUE,
                                       labels = 1:10) ]

# elderly quantiles
df_sections[, idade_60M_decile := cut(idade_60M,
                                      breaks = quantile(idade_60M, na.rm=T,
                                                        probs = seq(0, 1 , by = .1)),
                                      include.lowest = TRUE,
                                      ordered_result = TRUE #, labels = 1:10
                                      ) ]

summary(df_sections$idade_60M)
table(df_sections$idade_60M_decile)





# aggregate variables at muni level
df_muni <- df_sections[, .(QT_APTOS = sum(QT_APTOS[which(NR_TURNO==2)], na.rm=T),
                           QT_APTOS_log = log(sum(QT_APTOS[which(NR_TURNO==2)], na.rm=T)),
                           idade_60M = weighted.mean(x=idade_60M, w=QT_APTOS, na.rm=T),
                           biometria = weighted.mean(x=biometria, w=QT_APTOS, na.rm=T),
                           qt_biometria = sum(qt_biometria[which(NR_TURNO==2)], na.rm=T),
                           votos_jair_muni_total_p = sum(votos_jair[which(NR_TURNO==1)]) / sum(votos_total[which(NR_TURNO==1)]),
                           votos_jair_muni_validos_p = sum(votos_jair[which(NR_TURNO==1)]) / sum(votos_validos[which(NR_TURNO==1)]),
                           votos_lula_muni_validos_p = sum(votos_lula_p[which(NR_TURNO==1)]) / sum(votos_validos[which(NR_TURNO==1)]),
                           
                           mean_dist = weighted.mean(x=dist_sede, w=QT_APTOS, na.rm=T),      # pondera ou nao ?
                           mean_dens_1000 = weighted.mean(x=num_1000, w=QT_APTOS, na.rm=T),  # pondera ou nao ?
                           educacao_1 = weighted.mean(x=educacao_1, w=QT_APTOS, na.rm=T),
                           gov_2t = max(gov_2t),
                           PIB_PC = PIB_PC[1L],
                           pib_log = log(PIB_PC)[1L],
                           passe_livre = max(passe_livre), 
                           passe_livre_1 = max(passe_livre_1), 
                           passe_livre_2 = max(passe_livre_2)),
                   by= .(SG_UF, name_region, code_muni, variacao_comparecimento_2018_muni)]



head(df_muni)





# Descriptive analysis ----------------------------------------------------------------------

summary(df_muni)
summary(df_sections)


# check if numbers are balanced
df_sections[, table(passe_livre_2, turno2_dummy)]
#>                turno2_dummy
#> passe_livre_2      0      1
#>             0 129640 129640
#>             1 128577 128577


         # 
         # # balancing before ipw 
         # a <- df_muni[,   .(QT_APTOS          = weighted.mean(x=QT_APTOS),
         #                    QT_APTOS_log      = weighted.mean(x=QT_APTOS_log),
         #                    votos_jair_muni_total_p = weighted.mean(x=votos_jair_muni_total_p),
         #                    mean_dist         = weighted.mean(x=mean_dist, na.rm=T),
         #                    mean_dens_1000    = weighted.mean(x=mean_dens_1000),
         #                    educacao_1        = weighted.mean(x=educacao_1),
         #                    gov_2t             = weighted.mean(x=gov_2t),
         #                    pib_log           = weighted.mean(x=pib_log)),
         #              by=passe_livre_2]
         # 
         # 
         # 
         # # transpose
         # b <- data.table::transpose(a)
         # 
         # # get row and colnames in order
         # setnames(b, rownames(a))
         # b$variable <- colnames(a)
         # setcolorder(b, 'variable')



# 6666 add politica party
df_muni <- left_join(df_muni, df_partido,
                  by=c('code_muni'='id_municipio'), all.x = T)

# 6666
df_muni[is.na(dummy_partidobase_2020), dummy_partidobase_2020 := 1]
df_muni[is.na(dummy_partidobase_2016), dummy_partidobase_2016 := 1]
table(df_muni$dummy_partidobase_2016, useNA = 'always')





# ipw balancing ----------------------------------------------------------------------
summary(df_muni$variacao_comparecimento_2018_muni)
summary(df_muni$votos_jair_muni_validos_p)
summary(df_muni$biometria)
table(df_muni$variacao_comparecimento_2018_muni)

step1 <- glm(passe_livre_2 ~ variacao_comparecimento_2018_muni + gov_2t + QT_APTOS_log + pib_log  + votos_jair_muni_validos_p ,
             family = binomial(link = 'logit'),
             data = df_muni)


summary(step1)
df_muni[, ipw := (passe_livre_2 / fitted(step1)) + ((1 - passe_livre_2) / ( 1- fitted(step1)))]

hist(df_muni$ipw)
summary(df_muni$ipw)

plot(log(df_muni$ipw), df_muni$QT_APTOS_log)


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
step2_2022 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                     fixef = 'id_secao', 
                     cluster = 'code_muni',
                     weights = ~ipw,
                     data = df_sections)

step2_2018 <- fixest::feols(comparecimento_2018~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                       fixef = 'id_secao', 
                       cluster = 'code_muni',
                       weights = ~ipw,
                       data = df_sections)


summary(step2_2022)
summary(step2_2018)

model_list <- list(step2_2022, step2_2018)
names(model_list) <- c('Turnout 2022', 'Turnout 2018')


# export table
modelsummary::modelsummary(model_list, stars = T, output = './tables/table_did_avg.html')


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
 temp_step1 <- glm(passe_livre_2 ~ variacao_comparecimento_2018_muni+gov_2t + QT_APTOS_log + pib_log + votos_lula_muni_validos_p, 
              family = binomial(link = 'logit'),
              data = temp_df_muni)
 
 # merge ipw info of muni to sections
 temp_df_muni[, ipw := (passe_livre_2 / fitted(temp_step1)) + ((1 - passe_livre_2) / ( 1- fitted(temp_step1)))]
 setDT(temp_df_section)[temp_df_muni, on='code_muni', ipw := i.ipw]
 
 # step 2 regression
 step2_2022 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                        fixef = 'id_secao', 
                        cluster = 'code_muni',
                        weights = ~ipw,
                        data = temp_df_section)
 
 step2_2018 <- fixest::feols(comparecimento_2018~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
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
 step2_2022 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw,
                             data = temp_df_section)
 
 step2_2018 <- fixest::feols(comparecimento_2018~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
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
 step2_2022 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw,
                             data = temp_df_section)
 
 step2_2018 <- fixest::feols(comparecimento_2018~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
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
 step2_2022 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw,
                             data = temp_df_section)
 
 step2_2018 <- fixest::feols(comparecimento_2018~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
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
#  step2_low <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
#                         fixef = 'id_secao', 
#                         cluster = 'code_muni',
#                         weights = ~ipw,
#                         data = subset(temp_df, ses == 'low')
#                         )
#  
#  step2_high <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
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
 step2_2022 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw,
                             data = temp_df_section)
 
 step2_2018 <- fixest::feols(comparecimento_2018~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
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








 