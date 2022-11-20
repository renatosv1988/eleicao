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

df <- fread("../../data/base_DiD2022_2018_secoes.csv")





# Select observations ----------------------------------------------------------------------

# excluir seções de cidades sem sistema de ônibus
df <- subset(df, dummy_pt==1)
df[, table(NR_TURNO, dummy_pt)]
df[, table(NR_TURNO, passe_livre)]

# excluir cidades que SEMPRE tiveram passe livre
df <- subset(df, is.na(passe_livre_always))

#>            passe_livre
#> NR_TURNO      0      1
#>        1 260427  73829
#>        2 130466 203790


# identify treated in the 1st round
df[, passe_livre_1 := max(NR_TURNO==1 & passe_livre==1), by = id_secao]
table(df$passe_livre_1)

df[, table(NR_TURNO, passe_livre_1)]

#>          passe_livre_1
#> NR_TURNO      0      1
#>        1 260427  73829
#>        2 260427  73829


# drop always treated (1st round)
df_sections <- df[ passe_livre_1 != 1]
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

df_sections[, passe := max(passe_livre), by= id_secao]
df_sections[, dummy_turno := fifelse(NR_TURNO==2,1,0)]
df_sections[, dummy_ano := fifelse(ANO_ELEICAO==2022,1,0)]

# share of PT votes
df_sections[, votos_lula_p := sum(votos_lula) / sum(votos_validos), by = .(ANO_ELEICAO, NR_TURNO, id_secao)]

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
                       by= .(SG_UF, name_region, code_muni, ANO_ELEICAO)]



head(df_muni)





# 
# # ipw balancing ----------------------------------------------------------------------
# summary(df_muni$variacao_comparecimento_2018_muni)
# summary(df_muni$votos_jair_muni_validos_p)
# summary(df_muni$biometria)
# table(df_muni$name_region)
# 
# # comp 2022 1 secao
# # comp 2022 2 secao
# # 
# # comp 2022 1 muni
# # comp 2022 2 muni
# # 
# # comp 2018 1 secao
# # comp 2018 2 secao
# # variavai 1-2 secao
# # 
# # comp 2018 1 muni
# # comp 2018 2 muni
# # variavai 1-2 muni
# 
# # 666 definir ipw
# 
 step1 <- glm(passe_livre_2 ~ gov_2t + QT_APTOS_log + pib_log + name_region + votos_jair_muni_validos_p ,
              family = binomial(link = 'logit'),
              data = df_muni)
# 
# 
 summary(step1)
 df_muni[, ipw := (passe_livre_2 / fitted(step1)) + ((1 - passe_livre_2) / ( 1- fitted(step1)))]
# 
 hist(df_muni$ipw)
 summary(df_muni$ipw)
# 
# 
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
# 
# 
# 
# # transpose
 c <- data.table::transpose(a)
# 
# # get row and colnames in order
 setnames(c, rownames(a))
 c$variable <- colnames(a)
 setcolorder(c, 'variable')
# 
# 
# 


# Model 1. Average effects ----------------------------------------------------------------------
# average effects with inverse probability weighting

# # merge ipw info of muni to sections
 setDT(df_sections)[df_muni, on='code_muni', ipw := i.ipw]

# reg
step2 <- fixest::feols( comparecimento ~ dummy_turno*dummy_ano*passe , 
                        fixef = 'id_secao', 
                        cluster = 'code_muni',
                        data = df_sections)

step2w <- fixest::feols( comparecimento ~ dummy_turno*dummy_ano*passe , 
                        fixef = 'id_secao', 
                        cluster = 'code_muni',
                        weights = ~ipw,
                        data = df_sections)

etable(step2, step2w,
       extralines=list("IPW"=c("No", "Yes")),
       tex=T, file="figures/triple_diff_principal.tex")
#



# Model 2. by REGIAO ------------------------------------------

reg_region <- function(r){  # r = 'Norte'
 
 # select group
 temp_df_section <- df_sections[ name_region == r, ]
 
 # reg
 step2 <- fixest::feols( comparecimento ~ dummy_turno*dummy_ano*passe , 
                         fixef = 'id_secao', 
                         cluster = 'code_muni',
                         data = temp_df_section)
 
 output <- data.frame(name_region = r,
                      coef = step2$coeftable[6, 1],
                      se = step2$coeftable[6, 2])
 
 return(output)
}



output_did_region <- purrr::map(.x = unique(df_sections$name_region),
                                .f = reg_region) |> rbindlist()

reg_region <- function(r){  # r = 'Norte'
 
 # select group
 temp_df_section <- df_sections[ name_region == r, ]
 
 # reg
 step2 <- fixest::feols( comparecimento ~ dummy_turno*dummy_ano*passe , 
                         fixef = 'id_secao', 
                         cluster = 'code_muni',
                         data = temp_df_section)
 
 output <- data.frame(name_region = r,
                      coef = step2$coeftable[6, 1],
                      se = step2$coeftable[6, 2])
 
 output$ipw="0"
 
 
 step2w <- fixest::feols( comparecimento ~ dummy_turno*dummy_ano*passe , 
                         fixef = 'id_secao', 
                         cluster = 'code_muni',
                         weights = ~ipw,
                         data = temp_df_section)
 
 outputw <- data.frame(name_region = r,
                      coef = step2w$coeftable[6, 1],
                      se = step2w$coeftable[6, 2])
 
 outputw$ipw="1"
 
 output <- rbind(output, outputw)
 
 return(output)
}



output_did_region <- purrr::map(.x = unique(df_sections$name_region),
                                .f = reg_region) |> rbindlist()


ggplot() +
 geom_point(data = output_did_region, aes(x= name_region, y=coef, color=ipw),
            position = position_dodge(width = 0.20)) +
 geom_pointrange(data=output_did_region,
                 # show.legend = FALSE,
                 aes(x=name_region, y=coef, color=ipw,
                     ymin = coef - 1.96*se,
                     ymax = coef + 1.96*se),
                 position = position_dodge(width = 0.20)) +
 geom_hline(yintercept = 0, color='gray20') +
 theme_classic()
ggsave("figures/triple_diff_regiao.png", w=10, h=6)



# modelo 3a - heterogenidade educacao ------------------------------------------

table(df_sections$educacao_1_quintile)


reg_edu <- function(e){  # e = 4
 
 # select group
 temp_df_section <- df_sections[ educacao_1_decile == e, ]
 
 # reg
 step2 <- fixest::feols( comparecimento ~ dummy_turno*dummy_ano*passe , 
                         fixef = 'id_secao', 
                         cluster = 'code_muni',
                         data = temp_df_section)
 
 output <- data.frame(edu_cat = as.numeric(e),
                      coef = step2$coeftable[6, 1],
                      se = step2$coeftable[6, 2])
 
 output$ipw="0"
 
 # reg
 step2w <- fixest::feols( comparecimento ~ dummy_turno*dummy_ano*passe , 
                         fixef = 'id_secao', 
                         cluster = 'code_muni',
                         weights = ~ipw,
                         data = temp_df_section)
 
 outputw <- data.frame(edu_cat = as.numeric(e),
                      coef = step2w$coeftable[6, 1],
                      se = step2w$coeftable[6, 2])
 outputw$ipw="1"
 
 output <- rbind(output, outputw)
 return(output)
}




output3a <- purrr::map(.x = levels(df_sections$educacao_1_decile),
                       .f = reg_edu) |> rbindlist()


ggplot(data = output3a, aes(x= factor(edu_cat), y=coef, color=ipw),
       position = position_dodge(width = 0.20)) +
 geom_pointrange(data=output3a,
                 # show.legend = FALSE,
                 aes(x=factor(edu_cat), y=coef,
                     ymin = coef - 1.96*se,
                     ymax = coef + 1.96*se, color=ipw),
                 position = position_dodge(width = 0.20)) +
 geom_hline(yintercept = 0, color='gray20') +
 labs(x= 'Deciles of low\nsocioeconomic individuals') +
 theme_classic()
ggsave("figures/triple_diff_educacao.png", w=10, h=6)




# modelo 3b - heterogenidade densidade de secoes -----------------------------------------

table(df_sections$num_1000_cat10)
table(df_sections$num_1000_decile)

reg_dens <- function(i){  # i = 5
 message(i)
 temp_df_section <- df_sections[ num_1000_decile == i]
 
 
 step2 <- fixest::feols( comparecimento ~ dummy_turno*dummy_ano*passe , 
                         fixef = 'id_secao', 
                         cluster = 'code_muni',
                         data = temp_df_section)
 
 output <- data.frame(i = i,
                      coef = step2$coeftable[6, 1],
                      se = step2$coeftable[6, 2]
 )
 
 
 output$ipw <- "0"
 
 step2w <- fixest::feols( comparecimento ~ dummy_turno*dummy_ano*passe , 
                         fixef = 'id_secao', 
                         cluster = 'code_muni',
                         weights = ~ipw,
                         data = temp_df_section)
 
 outputw <- data.frame(i = i,
                      coef = step2w$coeftable[6, 1],
                      se = step2w$coeftable[6, 2]
 )
 
 outputw$ipw <- "1"
 
 output <- rbind(output, outputw)
 return(output)
}


# run regressions
output3b <- purrr::map(.x = levels(df_sections$num_1000_decile),
                       .f = reg_dens) |> rbindlist()


ggplot() +
 #geom_line() +
 geom_line(aes(x=as.numeric(i), y = coef, color=ipw), data=output3b) +
 geom_ribbon(aes(x=as.numeric(i), y = coef, ymax=coef + 1.96*se, ymin=coef - 1.96*se,
                 color=ipw, group=ipw, fill=ipw), alpha=.1, data = output3b, linetype=2) +
 geom_hline(yintercept = 0) +
 labs(x='Deciles of density of electoral sections') +
 theme_classic()
ggsave("figures/triple_diff_densidade.png", w=10, h=6)








# modelo 5 - heterogenidade idosos ------------------------------------------

table(df_sections$idade_60M_decile)


reg_age <- function(e){  # e = '(0.312,0.381]'
 
 # select group
 temp_df_section <- df_sections[ idade_60M_decile == e, ]
 
 # reg
 step2 <- fixest::feols( comparecimento ~ dummy_turno*dummy_ano*passe , 
                         fixef = 'id_secao', 
                         cluster = 'code_muni',
                         data = temp_df_section)
 
 output <- data.frame(age_cat = e,
                      coef = step2$coeftable[6, 1],
                      se = step2$coeftable[6, 2])
 
 output$ipw <- "0"
 # reg
 step2w <- fixest::feols( comparecimento ~ dummy_turno*dummy_ano*passe , 
                         fixef = 'id_secao', 
                         cluster = 'code_muni',
                         weights = ~ipw,
                         data = temp_df_section)
 
 outputw <- data.frame(age_cat = e,
                      coef = step2w$coeftable[6, 1],
                      se = step2w$coeftable[6, 2])
 outputw$ipw <- "1"
 
 output <- rbind(output, outputw)
 return(output)
}




output5 <- purrr::map(.x = levels(df_sections$idade_60M_decile),
                      .f = reg_age) |> rbindlist()


ggplot(data = output5, aes(x= factor(age_cat), y=coef, color=ipw)) +
 geom_pointrange(data=output5,
                 # show.legend = FALSE,
                 aes(x=factor(age_cat), y=coef,
                     ymin = coef - 1.96*se,
                     ymax = coef + 1.96*se, color=ipw),
                 position = position_dodge(width = 0.20)) +
 #geom_point(color='#0d6556') +
 geom_hline(yintercept = 0, color='gray20') +
 labs(x= 'Deciles of low\nsocioeconomic individuals') +
 theme_classic()
ggsave("figures/triple_diff_idade60.png", w=10, h=6)











# test 1o vs 1o comparecimento -------------------------------------

# keep only 1st round
temp_df1 <- subset(df_sections, NR_TURNO == 1)

# checks
table(temp_df1$dummy_ano)
table(temp_df1$NR_TURNO)

table(temp_df1$passe_livre_1, temp_df1$dummy_ano)

output_1 <- fixest::feols( comparecimento ~ dummy_ano*passe , 
                           fixef = 'id_secao', 
                           cluster = 'code_muni',
                           #weights = ~ipw,
                           data = temp_df1)
summary(output_1)



# test 2o vs 2o comparecimento -------------------------------------

# keep only 1st round
temp_df2 <- subset(df_sections, NR_TURNO == 2)

# checks
table(temp_df2$dummy_ano)
table(temp_df2$NR_TURNO)

table(temp_df2$passe_livre, temp_df2$dummy_ano)

output_2 <- fixest::feols(comparecimento ~ dummy_ano*passe , 
                          fixef = 'id_secao', 
                          cluster = 'code_muni',
                          #weights = ~ipw,
                          data = temp_df2)
summary(output_2)




etable(list(output_1, output_2))



# test 1o vs 1o Lula -------------------------------------

# keep only 1st round
temp_df1 <- subset(df_sections, NR_TURNO == 1)

# checks
table(temp_df1$dummy_ano)
table(temp_df1$NR_TURNO)

table(temp_df1$passe_livre_1, temp_df1$dummy_ano)

output_3 <- fixest::feols( votos_lula_p ~ dummy_ano*passe , 
                           fixef = 'id_secao', 
                           cluster = 'code_muni',
                           #weights = ~ipw,
                           data = temp_df1)
summary(output_3)



# test 2o vs 2o Lula -------------------------------------

# keep only 1st round
temp_df2 <- subset(df_sections, NR_TURNO == 2)

# checks
table(temp_df2$dummy_ano)
table(temp_df2$NR_TURNO)

table(temp_df2$passe_livre, temp_df2$dummy_ano)

output_4 <- fixest::feols( votos_lula_p ~ dummy_ano*passe_livre , 
                           fixef = 'id_secao', 
                           cluster = 'code_muni',
                           weights = ~ipw,
                           data = temp_df2)
summary(output_4)
