library(did)
library(fixest)


# ler base de eleições
BD <- fread("data/base_DiD2022_secoes.csv")

# excluir seções de cidades sem sistema de ônibus
BD <- subset(BD, dummy_pt==1)

# criar grupos de tratamento
# períodos tratados
BD$periodos_tratados <- ave(BD$passe_livre, BD$id_secao, FUN=sum)
BD[, grupo := fcase(periodos_tratados==2, 1,
                    periodos_tratados==1, 2,
                    default = 0)]
BD$pl_1 <- ifelse(BD$periodos_tratados==2, 1, 0)
BD$pl_2 <- ifelse(BD$periodos_tratados==1, 1, 0)

# DiD básicos

# periodo
BD$t2 <- ifelse(BD$NR_TURNO==2,1,0)
# interação:  período 2 X always treated
BD$pl_1_t2 <- ifelse(BD$grupo==1 & BD$NR_TURNO==2,1,0)
# interação:  período 2 X treated em t2
BD$pl_2_t2 <- ifelse(BD$grupo==2 & BD$NR_TURNO==2,1,0)

#valores de corte para iterações
BD$educ1 <- ifelse(BD$educacao_1>median(BD$educacao_1, na.rm = T),1,0)
BD$mulh <- ifelse(BD$mulheres>median(BD$mulheres, na.rm = T),1,0)
BD$pib <- ifelse(BD$PIB_PC>median(BD$PIB_PC, na.rm = T),1,0)


# interação:  período 2 X always treated X baixa educação
BD$pl_1_t2_educ1 <- ifelse(BD$grupo==1 & BD$NR_TURNO==2 & BD$educ1==1,1,0)
BD$pl_1_t2_educ0 <- ifelse(BD$grupo==1 & BD$NR_TURNO==2 & BD$educ1==0,1,0)
BD$pl_2_t2_educ1 <- ifelse(BD$grupo==2 & BD$NR_TURNO==2 & BD$educ1==1,1,0)
BD$pl_2_t2_educ0 <- ifelse(BD$grupo==2 & BD$NR_TURNO==2 & BD$educ1==0,1,0)

# interação:  período 2 X treated em t2 X mulheres
BD$pl_1_t2_mulh1 <- ifelse(BD$grupo==1 & BD$NR_TURNO==2 & BD$mulh==1,1,0)
BD$pl_1_t2_mulh0 <- ifelse(BD$grupo==1 & BD$NR_TURNO==2 & BD$mulh==0,1,0)
BD$pl_2_t2_mulh1 <- ifelse(BD$grupo==2 & BD$NR_TURNO==2 & BD$mulh==1,1,0)
BD$pl_2_t2_mulh0 <- ifelse(BD$grupo==2 & BD$NR_TURNO==2 & BD$mulh==0,1,0)

# interação:  período 2 X treated em t2 X alto PIB
BD$pl_1_t2_pib1 <- ifelse(BD$grupo==1 & BD$NR_TURNO==2 & BD$pib==1,1,0)
BD$pl_1_t2_pib0 <- ifelse(BD$grupo==1 & BD$NR_TURNO==2 & BD$pib==0,1,0)
BD$pl_2_t2_pib1 <- ifelse(BD$grupo==2 & BD$NR_TURNO==2 & BD$pib==1,1,0)
BD$pl_2_t2_pib0 <- ifelse(BD$grupo==2 & BD$NR_TURNO==2 & BD$pib==0,1,0)






# modelo básico
dd1 <- feols(comparecimento_2022~t2+pl_1_t2+pl_2_t2|id_secao, data = BD)
# incluindo controles que variam no tempo
dd2 <- feols(comparecimento_2022~t2+pl_1_t2+pl_2_t2+gov_2t|id_secao, data = BD)
# Clustering SE por municipio
dd3 <- feols(comparecimento_2022~t2+pl_1_t2+pl_2_t2|id_secao, data = BD, cluster = BD$CD_MUNICIPIO )
# incluindo controles que variam no tempo
dd4 <- feols(comparecimento_2022~t2+pl_1_t2+pl_2_t2+gov_2t|id_secao, data = BD, cluster = BD$CD_MUNICIPIO)
etable(dd1, dd2, dd3, dd4)


# interagindo tratamento com covariaveis
dd5 <- feols(comparecimento_2022~t2+
              pl_1_t2_educ0 + pl_1_t2_educ1 + pl_2_t2_educ0 + pl_2_t2_educ1 |
              id_secao, data = BD, cluster = BD$CD_MUNICIPIO)
dd6 <- feols(comparecimento_2022~t2+
              pl_1_t2_mulh0 + pl_1_t2_mulh1 + pl_2_t2_mulh0 + pl_2_t2_mulh1 |
              id_secao, data = BD, cluster = BD$CD_MUNICIPIO)
dd7 <- feols(comparecimento_2022~t2+
              pl_1_t2_pib0 + pl_1_t2_pib1 + pl_2_t2_pib0 + pl_2_t2_pib1 |
              id_secao, data = BD, cluster = BD$CD_MUNICIPIO)
etable(dd5, dd6, dd7)
