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
BD$pl_1 <- ifelse(BD$periodos_tratados==2, 1, 0)
BD$pl_2 <- ifelse(BD$periodos_tratados==1, 1, 0)


# correlações com a variação do comparecimento por seção
BD2 <- BD[BD$NR_TURNO==2,]
BD1 <- BD[BD$NR_TURNO==1,c("id_secao","QT_COMPARECIMENTO")]
colnames(BD1) <- c("id_secao","QT_COMPARECIMENTO_T1")

BU <- merge(BD1, BD2, by="id_secao", all.x = T) 
BU$var_comp <- (BU$QT_COMPARECIMENTO - BU$QT_COMPARECIMENTO_T1)/BU$QT_APTOS


BU$log_ele <- log(ave(BU$QT_APTOS, BU$CD_MUNICIPIO, FUN=sum))
BU$log_pib <- log(BU$PIB_PC)

# normalizar variáveis
BU$mulheres <- scale(BU$mulheres)
BU$educacao_1 <- scale(BU$educacao_1)
BU$idade_16_17 <- scale(BU$idade_16_17)
BU$idade_18_24 <- scale(BU$idade_18_24)
BU$idade_60M <- scale(BU$idade_60M)
BU$log_ele <- scale(BU$log_ele)
BU$log_pib <- scale(BU$log_pib)


lm <- feols(var_comp~mulheres + educacao_1 + idade_16_17 + idade_18_24 +
             idade_60M +pl_1 + pl_2 + gov_2t + log_ele + log_pib,
            data = BU, cluster = BU$CD_MUNICIPIO)
etable(lm)

