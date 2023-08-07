library(data.table)

options(scipen = 999)


# read data ----------------------------------------------------------------------

df_secoes_2022 <- fread("../../data/base_DiD2022_secoes.csv")



# percentage of voters living in cities with passe livre on the second round
df_secoes_2022[NR_TURNO==2 & passe_livre==1, sum(QT_APTOS)] / df_secoes_2022[NR_TURNO==2, sum(QT_APTOS)]
#> 0.4798155



# Select observations ----------------------------------------------------------------------

# identify treated cities
df_secoes_2022[, passe_livre_1 := max(NR_TURNO==1 & passe_livre==1), by = id_secao]
df_secoes_2022[, passe_livre_2 := max(NR_TURNO==2 & passe_livre==1), by = id_secao]


# keep only municipalities that were eventually treated
#df_secoes_2022 <- df_secoes_2022[ passe_livre_2 ==1]


# excluir cidades que SEMPRE tiveram passe livre
df_secoes_2022 <- subset(df_secoes_2022, is.na(passe_livre_always))

# aggregate variables at muni level --------------------------------------------
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
                          by= .(SG_UF, name_region, code_muni)]

# ipw balancing ----------------------------------------------------------------

df_muni[, passe_livre_only_2 := passe_livre_2 - passe_livre_1]

#modelo_t1 <- glm(passe_livre_1~ name_region + QT_APTOS_log + pib_log ,
#             family = binomial(link = 'logit'),
#             data = df_muni)

#modelo_t2 <- glm(passe_livre_only_2~ gov_2t + name_region + QT_APTOS_log + pib_log + votos_jair_validos_t1,
#             family = binomial(link = 'logit'),
#             data = df_muni)

modelo_t3 <- glm(passe_livre~ gov_2t + name_region + QT_APTOS_log + pib_log + votos_jair_validos_t1,
             family = binomial(link = 'logit'),
             data = df_muni)


#df_muni[, ipw1 := (passe_livre_1 / fitted(modelo_t1)) + ((1 - passe_livre_1) / ( 1- fitted(modelo_t1)))]
#df_muni[, ipw2 := (passe_livre_2 / fitted(modelo_t2)) + ((1 - passe_livre_2) / ( 1- fitted(modelo_t2)))]
df_muni[, ipw3 := (passe_livre / fitted(modelo_t3)) + ((1 - passe_livre) / ( 1- fitted(modelo_t3)))]


#summary(df_muni$ipw1)
#summary(df_muni$ipw2)


# export ipw
fwrite(df_muni[,.(code_muni,ipw3)], "../../data/ipw_municipalities_never_treated.csv")
