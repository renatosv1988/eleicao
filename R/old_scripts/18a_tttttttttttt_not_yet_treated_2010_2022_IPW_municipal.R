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

# ler dados
df <- fread("../../data/base_DiD2010_2022_secoes.csv", encoding = "Latin-1")

df_secoes_2022 <- fread("../../data/base_DiD2022_secoes.csv")



# percentage of voters living in cities with passe livre on the second round
df_secoes_2022[NR_TURNO==2 & passe_livre==1, sum(QT_APTOS)] / df_secoes_2022[NR_TURNO==2, sum(QT_APTOS)]
#> 0.4798155



# Select observations ----------------------------------------------------------------------

# keep only cities with public transport
df_secoes_2022[, passe_livre_any := max(passe_livre, passe_livre_always, na.rm = T), by = id_secao]
df_secoes_2022 <- subset(df_secoes_2022, dummy_pt==1 | passe_livre_any ==1)
nrow(df_secoes_2022)

df[, passe_livre_any := max(passe_livre, passe_livre_always, na.rm = T), by = id_secao]
df <- subset(df, dummy_pt==1 | passe_livre_any ==1)
nrow(df)



# # excluir cidades que SEMPRE tiveram passe livre
# df_secoes_2022 <- subset(df_secoes_2022, is.na(passe_livre_always))
# df <- subset(df, is.na(passe_livre_always))



# identify treated in the 1st round
df_secoes_2022[, passe_livre_1 := max(NR_TURNO==1 & passe_livre==1), by = id_secao]
df_secoes_2022[, passe_livre_2 := max(NR_TURNO==2 & passe_livre==1), by = id_secao]


# keep only municipalities that were eventually treated
df_secoes_2022 <- df_secoes_2022[ passe_livre_1 == 1 | passe_livre_2 ==1]
df <- df[ passe_livre_1 == 1 | passe_livre_2 ==1]

# 666666666666666666



# recode variables ----------------------------------------------------------------------

# education quantiles
df[, educacao_1_decile := cut(educacao_1,
                                      breaks = quantile(educacao_1, na.rm=T,
                                                        probs = seq(0, 1 , by = .1)),
                                      include.lowest = TRUE,
                                      ordered_result = TRUE,
                                      labels = 1:10), by = ANO_ELEICAO]

df[, educacao_1_quintile := cut(educacao_1,
                                 breaks = quantile(educacao_1, na.rm=T,
                                                   probs = seq(0, 1 , by = .2)),
                                 include.lowest = TRUE,
                                 ordered_result = TRUE,
                                 labels = 1:5), by = ANO_ELEICAO]


# density quantiles
df[, num_1000_decile := cut(num_1000,
                                       breaks = quantile(num_1000, na.rm=T,
                                                         probs = seq(0, 1 , by = .1)),
                                       include.lowest = TRUE,
                                       ordered_result = TRUE,
                                       labels = 1:10), by = ANO_ELEICAO]

# elderly quantiles
df[, idade_60M_decile := cut(idade_60M,
                                      breaks = quantile(idade_60M, na.rm=T,
                                                        probs = seq(0, 1 , by = .1)),
                                      include.lowest = TRUE,
                                      ordered_result = TRUE #, labels = 1:10
                                      ), by = ANO_ELEICAO]


# aggregate variables at muni level
df_muni <- df_secoes_2022[, .(QT_APTOS = sum(QT_APTOS[which(NR_TURNO==1)], na.rm=T),
                              QT_APTOS_log = log(sum(QT_APTOS[which(NR_TURNO==1)], na.rm=T)),
                              idade_60M = weighted.mean(x=idade_60M, w=QT_APTOS, na.rm=T),
                              biometria = weighted.mean(x=biometria, w=QT_APTOS, na.rm=T),
                              votos_jair_validos_t1 = sum(votos_jair[which(NR_TURNO==1)], na.rm = T) / sum(votos_validos[which(NR_TURNO==1)], na.rm = T),
                              votos_jair_validos_t2 = sum(votos_jair[which(NR_TURNO==2)], na.rm = T) / sum(votos_validos[which(NR_TURNO==2)], na.rm = T),
                              mean_dens_1000 = weighted.mean(x=num_1000, w=QT_APTOS, na.rm=T),
                              educacao_1 = weighted.mean(x=educacao_1, w=QT_APTOS, na.rm=T),
                              gov_2t = max(gov_2t),
                              PIB_PC = PIB_PC[1L],
                              pib_log = log(PIB_PC[1L]),
                              passe_livre = passe_livre[1L], 
                              passe_livre_1 = passe_livre_1[1L], 
                              passe_livre_2 = passe_livre_2[1L]
                              ),
                          by= .(SG_UF, name_region, code_muni, variacao_comparecimento_2018_muni)]



head(df_muni)








# ipw balancing ----------------------------------------------------------------------

l <- lm(passe_livre_1~ gov_2t + name_region  + QT_APTOS_log + pib_log  +  votos_jair_validos_t1,
             data = df_muni)

summary(l)

step1 <- glm(passe_livre_1~ gov_2t + name_region  + QT_APTOS_log + pib_log  + votos_jair_validos_t1,
             
             family = binomial(link = 'logit'),
             data = df_muni)


summary(step1)
df_muni[, ipw := (passe_livre_2 / fitted(step1)) + ((1 - passe_livre_2) / ( 1- fitted(step1)))]

hist(df_muni$ipw)
summary(df_muni$ipw)


# export ipw
fwrite(df_muni[,.(code_muni,ipw)], "../../data/ipw_municipalities.csv")
