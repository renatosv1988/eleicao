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



# percentage of voters living in cities with passe livre on the second round
df_secoes_2022[NR_TURNO==2 & passe_livre==1, sum(QT_APTOS)] / df_secoes_2022[NR_TURNO==2, sum(QT_APTOS)]
#> 0.4798155



# Select observations ----------------------------------------------------------------------

# keep only cities with public transport
df_secoes_2022[, passe_livre_any := max(passe_livre, passe_livre_always, na.rm = T), by = id_secao]
df_sections <- subset(df_secoes_2022, dummy_pt==1 | passe_livre_any ==1)
nrow(df_sections)

# # excluir cidades que SEMPRE tiveram passe livre
# df_sections <- subset(df_sections, is.na(passe_livre_always))


# create dummy for 2nd round
df_sections[, turno2_dummy := fifelse(NR_TURNO==2, 1, 0)]
df_sections[, turno1_dummy := fifelse(NR_TURNO==1, 1, 0)]


# identify treated in the 1st round
df_sections[, passe_livre_1 := max(NR_TURNO==1 & passe_livre==1), by = id_secao]
df_sections[, passe_livre_2 := max(NR_TURNO==2 & passe_livre==1), by = id_secao]

table(df_sections$passe_livre_1)
table(df_sections$passe_livre_2)

df_sections[, table(NR_TURNO, passe_livre_1)]
df_sections[, table(NR_TURNO, passe_livre_2)]



# # drop always treated (1st round)
# df_sections <- df_secoes_2022[ passe_livre_1 != 1]
# df_sections[, table(NR_TURNO, passe_livre)]
# #>            passe_livre
# #> NR_TURNO      0      1
# #>        1 267408      0
# #>        2 132157 135251


# # drop NEVER treated
# df_sections <- df_sections[ passe_livre != 0]


# reclassify
df_sections[, passe_livre_2 := ifelse( passe_livre_1 ==1 , 0, passe_livre_2)]
df_sections[, table(turno2_dummy, passe_livre_2)]

# check
df_sections[id_secao == '4111 18 10', .(id_secao, NR_TURNO, turno2_dummy, passe_livre, passe_livre_1, passe_livre_2)]
df_sections[id_secao == '35 2 10', .(id_secao, NR_TURNO, turno2_dummy, passe_livre, passe_livre_1, passe_livre_2)]




df_sections[, table(passe_livre_2, turno2_dummy)]
#>                turno2_dummy
#> passe_livre_2      0      1
#>             0 208841 208841
#>             1 135251 135251



# recode variables ----------------------------------------------------------------------

# share of PT votes
df_sections[, votos_lula_p := sum(votos_lula) / sum(votos_validos), by = .(ANO_ELEICAO, NR_TURNO, id_secao)]
df_sections[, votos_jair_p := sum(votos_jair) / sum(votos_validos), by = .(ANO_ELEICAO, NR_TURNO, id_secao)]

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
                           votos_jair_muni_validos_p = sum(votos_jair[which(NR_TURNO==1)], na.rm = T) / sum(votos_validos[which(NR_TURNO==1)], na.rm = T),
                           votos_lula_muni_validos_p = sum(votos_lula_p[which(NR_TURNO==1)], na.rm = T) / sum(votos_validos[which(NR_TURNO==1)], na.rm = T),
                           
                           mean_dist = weighted.mean(x=dist_sede, w=QT_APTOS, na.rm=T),      # pondera ou nao ?
                           mean_dens_1000 = weighted.mean(x=num_1000, w=QT_APTOS, na.rm=T),  # pondera ou nao ?
                           educacao_1 = weighted.mean(x=educacao_1, w=QT_APTOS, na.rm=T),
                           gov_2t = max(gov_2t),
                           # gov_2t2018 = max(gov_2t2018),
                           PIB_PC = PIB_PC[1L],
                           pib_log = log(PIB_PC[1L]),
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

df_sections[, table(passe_livre_1, turno1_dummy)]

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



# # 6666 add politica party
# df_muni <- left_join(df_muni, df_partido,
#                   by=c('code_muni'='id_municipio'), all.x = T)
# 
# summary(df_muni$dummy_partidobase_2016)
# summary(df_muni$dummy_partidobase_2020)
# # 6666
# df_muni[is.na(dummy_partidobase_2020), dummy_partidobase_2020 := 1]
# df_muni[is.na(dummy_partidobase_2016), dummy_partidobase_2016 := 1]
# table(df_muni$dummy_partidobase_2016, useNA = 'always')
# 
#  



# ipw balancing ----------------------------------------------------------------------
summary(df_muni$variacao_comparecimento_2018_muni)
summary(df_muni$votos_jair_muni_validos_p)
summary(df_muni$biometria)
table(df_muni$variacao_comparecimento_2018_muni)

l <- lm(passe_livre_2~ gov_2t + name_region  + 
              QT_APTOS_log + pib_log  +  votos_jair_muni_validos_p ,
             data = df_muni)

summary(l)

step1 <- glm(passe_livre_2~ gov_2t + name_region  + 
              QT_APTOS_log + pib_log   + votos_jair_muni_validos_p ,
             
             family = binomial(link = 'logit'),
             data = df_muni)


summary(step1)
df_muni[, ipw := (passe_livre_2 / fitted(step1)) + ((1 - passe_livre_2) / ( 1- fitted(step1)))]

hist(df_muni$ipw)
summary(df_muni$ipw)


fwrite(df_muni[,c("code_muni","ipw")], "../../data/pesos_municipio.csv")
