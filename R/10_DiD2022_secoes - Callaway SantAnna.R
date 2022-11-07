library(did)
library(fixest)
library(data.table)

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


# calcular variáveis municipais
BD$eleitores <- ave(BD$QT_APTOS, paste(BD$CD_MUNICIPIO, BD$NR_TURNO), FUN=sum)
BD$id_secao <- as.numeric(as.factor(BD$id_secao))

# CALLAWAY & SANTTANA ----------------------------------------------------------
# modelo básico
cs1 <- att_gt(yname = "comparecimento_2022",
             tname = "t",
             idname = "id_secao",
             gname = "grupo",
             xformla = ~1,
             data = BD)
summary(cs1)
ggdid(cs1)


# SE cluster por municipio
cs2 <- att_gt(yname = "comparecimento_2022",
              tname = "t",
              idname = "id_secao",
              gname = "grupo",
              xformla = ~1,
              clustervars = "CD_MUNICIPIO",
              data = BD)
summary(cs2)
ggdid(cs2)

# incluir variáveis municipais invariantes como X em xformula  !!!!!!!!!!!! ERRO 
cs3 <- att_gt(yname = "comparecimento_2022",
              tname = "t",
              idname = "id_secao",
              gname = "grupo",
              xformla = ~eleitores,
              data = BD)
summary(cs3)

# incluir variáveis municipais que variam no tempo como X em xformula  !!!!!!!!!!!! ERRO 
cs4 <- att_gt(yname = "comparecimento_2022",
              tname = "t",
              idname = "id_secao",
              gname = "grupo",
              xformla = ~gov_2t,
              data = BD)
summary(cs4)
