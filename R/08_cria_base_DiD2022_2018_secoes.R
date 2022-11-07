library(geobr)
library(data.table)
library(ggplot2)
library(scales)
library(fixest)
library(basedosdados)


#cidades <- read_municipal_seat()
eleicao_2022 <- fread('./data/secoes/secoes_2022.csv')
eleicao_2018 <- fread('./data/secoes/secoes_2018.csv')
passe_livre <- fread('data/passe_livre/passe_livre_resumo.csv')
espacial <- fread('./data/spatial/electoral_sections_spatial.csv')
munic <- fread('./data/munic/munic_dummy_pt.csv')
corr_ibge_tse <- fread("./data_raw/tse_ibge/correspondencia_IBGE_TSE.csv", encoding = "UTF-8")
pib <- fread("./data_raw/IBGE/PIBPC_2019_municipios.csv", encoding = "UTF-8")
perfil <- fread('./data/secoes/secoes_perfil_2022.csv')

# juntar 2018 e 2022
eleicao_2022 <- eleicao_2022[,-c("NM_LOCAL_VOTACAO","DS_LOCAL_VOTACAO_ENDERECO")]
setnames(eleicao_2022,
         old = c('comparecimento_2022','abstencao_2022'),
         new = c('comparecimento', 'abstencao'))
setnames(eleicao_2018,
         old = c('comparecimento_2018','abstencao_2018'),
         new = c('comparecimento', 'abstencao'))
eleicao <- rbind(eleicao_2018, eleicao_2022)

# MERGE INFORMAÇÕES ESPACIAIS --------------------------------------------------
eleicao[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
espacial[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
espacial <- espacial[,c("closest_dist_any", "closest_dist", "num_0500",
                        "num_1000", "num_3000","num_5000","num_10000",
                        "id_secao")]
eleicao <- merge(eleicao, espacial, by="id_secao", all.x = T)

# MERGE MUNIC ------------------------------------------------------------------
corr_ibge_tse <- corr_ibge_tse[,c("codigo_tse", "codigo_ibge")]
colnames(corr_ibge_tse) <- c("CD_MUNICIPIO", "code_muni")
munic <- merge(munic, corr_ibge_tse, by="code_muni", all.x = T)
eleicao <- merge(eleicao, munic[,c("CD_MUNICIPIO", "dummy_pt", "code_muni")],
                      by="CD_MUNICIPIO", all.x = T)

# MERGE perfil -----------------------------------------------------------------
perfil$mulheres <- perfil$mulher/perfil$qt_perfil
perfil$educacao_1 <- perfil$educ_prim/perfil$qt_perfil
perfil$idade_16_17 <- perfil$idade_16_17/perfil$qt_perfil
perfil$idade_18_24 <- perfil$idade_18_24/perfil$qt_perfil
perfil$idade_60M <- perfil$idade_60M/perfil$qt_perfil

eleicao <- merge(eleicao, perfil[,c("id_secao","mulheres","educacao_1",
                                              "idade_16_17","idade_18_24","idade_60M")],
                      by="id_secao", all.x = T)

# MERGE PASSE LIVRE ------------------------------------------------------------
# separa bases por ano e turno
t1o <- subset(eleicao, NR_TURNO==1 & ANO_ELEICAO==2018)
t2o <- subset(eleicao, NR_TURNO==2 & ANO_ELEICAO==2018)
t1 <- subset(eleicao, NR_TURNO==1 & ANO_ELEICAO==2022)
t2 <- subset(eleicao, NR_TURNO==2 & ANO_ELEICAO==2022)

passe_livre_t1 <- passe_livre[,c("CD_MUNICIPIO", "passe_livre_t1")]
colnames(passe_livre_t1) <-c("CD_MUNICIPIO", "passe_livre") 
passe_livre_t2 <- passe_livre[,c("CD_MUNICIPIO", "passe_livre_t2")]
colnames(passe_livre_t2) <-c("CD_MUNICIPIO", "passe_livre") 
# cria variável de tempo calendário
t1o$t <- -1
t2o$t <- 0
t1$t <- 1
t2$t <- 2

# merge data
t1 <- merge(t1, passe_livre_t1, by="CD_MUNICIPIO", all.x = T)
t2 <- merge(t2, passe_livre_t2, by="CD_MUNICIPIO", all.x = T)
t1o$passe_livre <- 0
t2o$passe_livre <- 0

# ajustar NAs
t1$passe_livre[is.na(t1$passe_livre)] <- 0
t2$passe_livre[is.na(t2$passe_livre)] <- 0

# junta as bases novamente
eleicao <- rbind(t1o, t2o,t1, t2)

# MERGE PIB_PC 2019 ------------------------------------------------------------
eleicao <- merge(eleicao, pib, by="code_muni")


# salvar arquivo final----------------------------------------------------------
# seleciona apenas variáveis que serão usadas
my_var <- c("id_secao",  "CD_MUNICIPIO","NR_ZONA", "NR_SECAO",
            "ANO_ELEICAO","NR_TURNO", "SG_UF","CD_CARGO",
            
            "code_muni",
            
            "QT_APTOS","QT_COMPARECIMENTO","QT_ABSTENCOES","QT_VOTOS_NOMINAIS",
            "QT_VOTOS_BRANCOS","QT_VOTOS_NULOS", "QT_VOTOS_LEGENDA",
            "QT_VOTOS_ANULADOS_APU_SEP","NR_LOCAL_VOTACAO",
            
            "gov_2t",
            "mulheres","educacao_1",
            "idade_16_17","idade_18_24","idade_60M",
            "comparecimento","abstencao",
            
            "closest_dist_any", "closest_dist",
            "num_0500", "num_1000","num_3000",
            "num_5000","num_10000",
            
            "dummy_pt", "t", "passe_livre","PIB_PC")

eleicao <- eleicao[, ..my_var]

fwrite(eleicao, "data/base_DiD2022_2018_secoes.csv")