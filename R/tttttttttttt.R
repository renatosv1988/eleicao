# remover municipios que SEMPRE tiveram passe livre, mesmo antes das eleicoes
# isso só deve afetar analise do 1o turno, mas mesmo assim importa

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


# percentage of voters living in cities with passe livre
df_secoes_2022[NR_TURNO==2 & passe_livre==1, sum(QT_APTOS)] / df_secoes_2022[NR_TURNO==2, sum(QT_APTOS)]
#> 0.4780362



# Select observations ----------------------------------------------------------------------

# excluir seções de cidades sem sistema de ônibus
df_secoes_2022 <- subset(df_secoes_2022, dummy_pt==1)
df_secoes_2022[, table(NR_TURNO, dummy_pt)]
df_secoes_2022[, table(NR_TURNO, passe_livre)]

#>            passe_livre
#> NR_TURNO      0      1
#>        1 260427  73829
#>        2 130466 203790


# identify treated in the 1st round
df_secoes_2022[, passe_livre_1 := max(NR_TURNO==1 & passe_livre==1), by = id_secao]
table(df_secoes_2022$passe_livre_1)

df_secoes_2022[, table(NR_TURNO, passe_livre_1)]

#>          passe_livre_1
#> NR_TURNO      0      1
#>        1 260427  73829
#>        2 260427  73829


# drop always treated (1st round)
df_sections <- df_secoes_2022[ passe_livre_1 != 1]
df_sections[, table(NR_TURNO, passe_livre)]
#>            passe_livre
#> NR_TURNO      0      1
#>        1 260427      0
#>        2 130466 129961


# identify treated in turno 2
df_sections[, passe_livre_2 := max(passe_livre==1), by = id_secao]
df_sections[, turno2_dummy := fifelse(NR_TURNO==2, 1, 0)]

df_sections[, table(NR_TURNO, passe_livre_2)]
#>          passe_livre_2
#> NR_TURNO      0      1
#>        1 130466 129961
#>        2 130466 129961



# recode variables ----------------------------------------------------------------------

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
#>             0 130466 130466
#>             1 129961 129961



# balancing before ipw 
a <- df_muni[,   .(QT_APTOS          = weighted.mean(x=QT_APTOS),
                   QT_APTOS_log      = weighted.mean(x=QT_APTOS_log),
                   votos_jair_muni_total_p = weighted.mean(x=votos_jair_muni_total_p),
                   mean_dist         = weighted.mean(x=mean_dist, na.rm=T),
                   mean_dens_1000    = weighted.mean(x=mean_dens_1000),
                   educacao_1        = weighted.mean(x=educacao_1),
                   gov_2t             = weighted.mean(x=gov_2t),
                   pib_log           = weighted.mean(x=pib_log)),
             by=passe_livre_2]



# transpose
b <- data.table::transpose(a)

# get row and colnames in order
setnames(b, rownames(a))
b$variable <- colnames(a)
setcolorder(b, 'variable')



# ipw balancing ----------------------------------------------------------------------
summary(df_muni$variacao_comparecimento_2018_muni)
summary(df_muni$votos_jair_muni_validos_p)
summary(df_muni$biometria)
table(df_muni$name_region)

# comp 2022 1 secao
# comp 2022 2 secao
# 
# comp 2022 1 muni
# comp 2022 2 muni
# 
# comp 2018 1 secao
# comp 2018 2 secao
# variavai 1-2 secao
# 
# comp 2018 1 muni
# comp 2018 2 muni
# variavai 1-2 muni

# 666 definir ipw

step1 <- glm(passe_livre_2 ~ gov_2t + QT_APTOS_log + pib_log + name_region + votos_jair_muni_validos_p ,
             family = binomial(link = 'logit'),
             data = df_muni)


summary(step1)
df_muni[, ipw := (passe_livre_2 / fitted(step1)) + ((1 - passe_livre_2) / ( 1- fitted(step1)))]

hist(df_muni$ipw)
summary(df_muni$ipw)


# balancing AFTER ipw 
a <- df_muni[,   .(QT_APTOS          = weighted.mean(x=QT_APTOS, w = ipw),
                   QT_APTOS_log      = weighted.mean(x=QT_APTOS_log, w = ipw),
                   votos_jair_muni_total_p = weighted.mean(x=votos_jair_muni_total_p, w = ipw),
                   mean_dist         = weighted.mean(x=mean_dist, w = ipw, na.rm=T),
                   mean_dens_1000    = weighted.mean(x=mean_dens_1000, w = ipw),
                   educacao_1        = weighted.mean(x=educacao_1, w = ipw),
                   gov_2t            = weighted.mean(x=gov_2t, w = ipw),
                   pib_log           = weighted.mean(x=pib_log, w = ipw)),
             by=passe_livre_2]



# transpose
c <- data.table::transpose(a)

# get row and colnames in order
setnames(c, rownames(a))
c$variable <- colnames(a)
setcolorder(c, 'variable')





# Model 1. Average effects ----------------------------------------------------------------------
# average effects with inverse probability weighting

# merge ipw info of muni to sections
setDT(df_sections)[df_muni, on='code_muni', ipw := i.ipw]

# reg
step2 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                     fixef = 'id_secao', 
                     cluster = 'code_muni',
                     weights = ~ipw,
                     data = df_sections)
summary(step2)
#


# modelsummary::modelsummary(step2, stars = T)


temp_df <- df_sections[, .(mean_value=mean(comparecimento_2022, na.rm=T),
                           p25 = quantile(comparecimento_2022,0.25, na.rm=T),
                           p75 = quantile(comparecimento_2022,0.75, na.rm=T)), by=.(NR_TURNO, passe_livre_2)]



ggplot() +
 geom_line(data=temp_df, aes(x=NR_TURNO, y=mean_value, color=factor(passe_livre_2)),
           position = position_dodge2(width = .2)) +
 geom_pointrange(data=temp_df,
                 position = position_dodge2(width = .2),
                 show.legend = FALSE,
                 aes(x=NR_TURNO, y=mean_value, color=factor(passe_livre_2),
                     ymin = p25,
                     ymax = p75)) +
 labs(y='Voter turnout', x = 'Election round', color='Free transit') +
 scale_x_continuous( breaks =  c(1, 2)) +
 scale_y_continuous(labels = scales::percent) +
 #scale_color_npg() +
 #scale_color_uchicago() +
  scale_color_jama() +
 theme_classic()


# sugestao renato PLACEBO
dd1x <- fixest::feols(comparecimento_2018~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2,
                      fixef = 'id_secao', 
                      cluster = 'code_muni',
                      weights = ~ipw,
                      data = df_sections)

summary(dd1x)





# Model 2. by REGIAO ------------------------------------------

reg_region <- function(r){  # r = 'Norte'
 
 # select group
 temp_df_muni <- df_muni[ name_region == r, ]
 temp_df_section <- df_sections[ name_region == r, ]
 
 # calculate ipw
 temp_step1 <- glm(passe_livre_2 ~ gov_2t + QT_APTOS_log + pib_log + votos_jair_muni_validos_p, 
              family = binomial(link = 'logit'),
              data = temp_df_muni)
 
 temp_df_muni[, ipw := (passe_livre_2 / fitted(temp_step1)) + ((1 - passe_livre_2) / ( 1- fitted(temp_step1)))]
 
 # summary(temp_step1)
 # hist(temp_df_muni$ipw)
 # summary(temp_df_muni$ipw)
 
 # step 2 regression
 # merge ipw info of muni to sections
 setDT(temp_df_section)[temp_df_muni, on='code_muni', ipw := i.ipw]
 
 # reg
 step2 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                        fixef = 'id_secao', 
                        cluster = 'code_muni',
                        weights = ~ipw,
                        data = temp_df_section)
 
 output <- data.frame(name_region = r,
                      coef = step2$coeftable[2, 1],
                      se = step2$coeftable[2, 2])

 return(output)
}

output_did_region <- purrr::map(.x = unique(df_sections$name_region),
                         .f = reg_region) |> rbindlist()


ggplot() +
 geom_point(data = output_did_region, aes(x= name_region, y=coef)) +
 geom_pointrange(data=output_did_region,
                 # show.legend = FALSE,
                 aes(x=name_region, y=coef,
                     ymin = coef - 1.96*se,
                     ymax = coef + 1.96*se)) +
 geom_hline(yintercept = 0, color='gray20') +
 theme_classic()




# Model 2. URBAN vs RURAL  ------------------------------------------


reg_urban <- function(z){  # z = 'urban'
 
 # select group
 temp_df_section <- df_sections[ zone == z, ]
 
 # reg
 step2 <- fixest::feols(comparecimento_2022~turno2_dummy +  passe_livre_2 + turno2_dummy:passe_livre_2, 
                        fixef = 'id_secao', 
                        cluster = 'code_muni',
                        weights = ~ipw,
                        data = temp_df_section)
 
 output <- data.frame(zone = z,
                      coef = step2$coeftable[2, 1],
                      se = step2$coeftable[2, 2]) 
 return(output)
}

output_did_urban <- purrr::map(.x = unique(df_sections$zone),
                                .f = reg_urban) |> rbindlist()


ggplot() +
 geom_point(data = output_did_urban, aes(x= zone, y=coef)) +
 geom_pointrange(data=output_did_urban,
                 # show.legend = FALSE,
                 aes(x=zone, y=coef,
                     ymin = coef - 1.96*se,
                     ymax = coef + 1.96*se)) +
 geom_hline(yintercept = 0, color='gray20') +
 theme_classic()




# modelo 3a - heterogenidade educacao ------------------------------------------

table(df_sections$educacao_1_quintile)


reg_edu <- function(e){  # e = '0.4'
 
 # select group
 temp_df_section <- df_sections[ educacao_1_decile == e, ]
 
 # reg
 step2 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                        fixef = 'id_secao', 
                        cluster = 'code_muni',
                        weights = ~ipw,
                        data = temp_df_section)
 
 output <- data.frame(edu_cat = as.numeric(e),
                      coef = step2$coeftable[2, 1],
                      se = step2$coeftable[2, 2])
 
 return(output)
}




output3a <- purrr::map(.x = levels(df_sections$educacao_1_decile),
                       .f = reg_edu) |> rbindlist()


ggplot(data = output3a, aes(x= factor(edu_cat), y=coef)) +
 geom_pointrange(data=output3a,
                 color='#0d6556',
                 # show.legend = FALSE,
                 aes(x=factor(edu_cat), y=coef,
                     ymin = coef - 1.96*se,
                     ymax = coef + 1.96*se)) +
 geom_point(color='#0d6556') +
 geom_hline(yintercept = 0, color='gray20') +
 labs(x= 'Deciles of low\nsocioeconomic individuals') +
 theme_classic()





# modelo 3b - heterogenidade densidade de secoes -----------------------------------------

table(df_sections$num_1000_cat10)
table(df_sections$num_1000_decile)

reg_dens <- function(i){  # i = 50
 message(i)
 temp_df <- df_sections[ num_1000_decile == i]
 
 
 step2 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                        fixef = 'id_secao', 
                        cluster = 'code_muni',
                        weights = ~ipw,
                        data = temp_df)
 
 output <- data.frame(i = i,
                      coef = step2$coeftable[2, 1],
                      se = step2$coeftable[2, 2]
 )
 
 return(output)
}


# run regressions
output3b <- purrr::map(.x = levels(df_sections$num_1000_decile),
                      .f = reg_dens) |> rbindlist()


ggplot(data = output3b, aes(x= as.numeric(i), y=coef)) +
 geom_line() +
 geom_ribbon(aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
 geom_hline(yintercept = 0) +
 labs(x='Deciles of density of electoral sections') +
 theme_classic()






# modelo 3c - heterogenidade distancia e edu -----------------------------------------

# create socioeconomic status level
df_sections[, ses := ifelse( educacao_1 >= .4, 'low', 'high')]

table(df_sections$ses, useNA = 'always')
table(df_sections$num_1000_decile, useNA = 'always')

table(df_sections$ses, df_sections$num_1000_decile)


# reg function
reg_group_dist_edu <- function(i){  # i = 4 i = Inf
 message(i)
 temp_df <- df_sections[ num_1000_decile == i & !is.na(ses)]

 step2_low <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                        fixef = 'id_secao', 
                        cluster = 'code_muni',
                        weights = ~ipw,
                        data = subset(temp_df, ses == 'low')
                        )
 
 step2_high <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                            fixef = 'id_secao', 
                            cluster = 'code_muni',
                            weights = ~ipw,
                            data = subset(temp_df, ses == 'high')
                            )
 
 output_low <- data.frame(i = as.numeric(i),
                          group = 'low',
                          coef = step2_low$coeftable[2, 1],
                          se = step2_low$coeftable[2, 2])
 
 output_high <- data.frame(i = as.numeric(i),
                           group = 'high',
                           coef = step2_high$coeftable[2, 1],
                           se = step2_high$coeftable[2, 2])
 
 output <- rbind(output_low, output_high)
 return(output)
}


# run regressions
output3c <- purrr::map(.x = levels(df_sections$num_1000_decile),
                     .f = reg_group_dist_edu) |> rbindlist()


ggplot(data = output3c, aes(x= i, y=coef, color=group, fill=group)) +
 geom_ribbon(aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
 geom_line(show.legend = F) +
 geom_hline(yintercept = 0, color='gray20') +
 labs(x='Deciles of electoral section density', fill='SES', color='SES') +
 scale_x_continuous( breaks =  1:10) +
 scale_color_npg() +
 scale_fill_npg() +
 theme_classic()






# modelo 5 - heterogenidade idosos ------------------------------------------

table(df_sections$idade_60M_decile)


reg_age <- function(e){  # e = '(0.0838,0.109]'
 
 # select group
 temp_df_section <- df_sections[ idade_60M_decile == e, ]
 
 # reg
 step2 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                        fixef = 'id_secao', 
                        cluster = 'code_muni',
                        weights = ~ipw,
                        data = temp_df_section)
 
 output <- data.frame(age_cat = e,
                      coef = step2$coeftable[2, 1],
                      se = step2$coeftable[2, 2])
 
 return(output)
}




output5 <- purrr::map(.x = levels(df_sections$idade_60M_decile),
                       .f = reg_age) |> rbindlist()


ggplot(data = output5, aes(x= factor(age_cat), y=coef)) +
 geom_pointrange(data=output5,
                 color='#0d6556',
                 # show.legend = FALSE,
                 aes(x=factor(age_cat), y=coef,
                     ymin = coef - 1.96*se,
                     ymax = coef + 1.96*se)) +
 geom_point(color='#0d6556') +
 geom_hline(yintercept = 0, color='gray20') +
 labs(x= 'Deciles of low\nsocioeconomic individuals') +
 theme_classic()






 