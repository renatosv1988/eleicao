library(geobr)
library(data.table)
library(ggplot2)
library(scales)
library(fixest)
#library(basedosdados)


#cidades <- read_municipal_seat()
passe_livre <- fread('../../data/passe_livre/passe_livre_resumo.csv')
espacial <- fread('../../data/spatial/electoral_sections_spatial.csv')
munic <- fread('../../data/munic/munic_dummy_pt.csv')
corr_ibge_tse <- fread("../../data_raw/tse_ibge/correspondencia_IBGE_TSE.csv", encoding = "UTF-8")
pib <- fread("../../data_raw/IBGE/PIBPC_2019_municipios.csv", encoding = "UTF-8")
perfil <- fread('../../data/secoes/secoes_perfil_2022.csv')
eleicao_2022 <- fread('../../data/secoes/secoes_2022.csv')

eleicao_2022[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]


# MERGE spatial info --------------------------------------------------
espacial[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
espacial <- espacial[,c("dist_sede", "closest_dist_any", "closest_dist", "num_0500",
                        "num_1000", "num_3000","num_5000","num_10000",
                        "id_secao")]
eleicao_2022 <- merge(eleicao_2022, espacial, by="id_secao", all.x = T)

# MERGE MUNIC ------------------------------------------------------------------
corr_ibge_tse <- corr_ibge_tse[,c("codigo_tse", "codigo_ibge")]
colnames(corr_ibge_tse) <- c("CD_MUNICIPIO", "code_muni")
munic <- merge(munic, corr_ibge_tse, by="code_muni", all.x = T)
eleicao_2022 <- merge(eleicao_2022, munic[,c("CD_MUNICIPIO", "dummy_pt", "code_muni")],
                      by="CD_MUNICIPIO", all.x = T)

# MERGE perfil -----------------------------------------------------------------
perfil[, mulheres := mulher / qt_perfil ]
perfil[, educacao_1 := educ_prim / qt_perfil ]
perfil[, idade_16_17 := idade_16_17 / qt_perfil ]
perfil[, idade_18_24 := idade_18_24 / qt_perfil ]
perfil[, idade_60M := idade_60M / qt_perfil ]

summary(perfil$educacao_1)

eleicao_2022 <- merge(eleicao_2022, perfil[,c("id_secao","mulheres","educacao_1",
                                              "idade_16_17","idade_18_24","idade_60M")],
                      by="id_secao", all.x = T)

summary(eleicao_2022$educacao_1)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>  0.0000  0.2887  0.4150  0.4189  0.5494  1.0000     672



# MERGE PASSE LIVRE ------------------------------------------------------------

# separa bases por turno
t1 <- subset(eleicao_2022, NR_TURNO==1)
t2 <- subset(eleicao_2022, NR_TURNO==2)
passe_livre_t1 <- passe_livre[,c("CD_MUNICIPIO", "passe_livre_t1")]
colnames(passe_livre_t1) <-c("CD_MUNICIPIO", "passe_livre") 
passe_livre_t2 <- passe_livre[,c("CD_MUNICIPIO", "passe_livre_t2")]
colnames(passe_livre_t2) <-c("CD_MUNICIPIO", "passe_livre") 

# cria variavel de tempo calendario
t1$t <- 1
t2$t <- 2

# merge data
t1 <- merge(t1, passe_livre_t1, by="CD_MUNICIPIO", all.x = T)
t2 <- merge(t2, passe_livre_t2, by="CD_MUNICIPIO", all.x = T)

# ajustar NAs
t1$passe_livre[is.na(t1$passe_livre)] <- 0
t2$passe_livre[is.na(t2$passe_livre)] <- 0

# junta as bases novamente
eleicao_2022 <- rbind(t1, t2)

# MERGE PIB_PC 2019 ------------------------------------------------------------
eleicao_2022 <- merge(eleicao_2022, pib, by="code_muni")


summary(eleicao_2022$educacao_1)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>  0.0000  0.2887  0.4150  0.4189  0.5494  1.0000     672

# adicionar votação por candidato ----------------------------------------------
dir_urnas <- '../../data_raw/urnas'
# UFs
my_uf <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG", "MS", "MT",
           "PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")

# listas de resultados
lista_T1 <- list()
lista_T2 <- list()
for(i in 1:27){# i <- 2
 st <- Sys.time()
  
 
  # caminhos dos CSVs nos Zips
  path_zip_T1 <- paste0(dir_urnas,"/urnas_", my_uf[i], "_2022_1T.zip")
  path_csv_T1  <- unzip(path_zip_T1, list=T)$Name[grep(pattern=".csv", files)]
  
  path_zip_T2 <- paste0(dir_urnas,"/urnas_", my_uf[i], "_2022_2T.zip")
  path_csv_T2  <- unzip(path_zip_T2, list=T)$Name[grep(pattern=".csv", files)]
  
  # ler dados das urnas
  urnas_T1 <- read.csv(unz(path_zip_T1, path_csv_T1), sep = ";", encoding = "Latin-1")
  urnas_T2 <- read.csv(unz(path_zip_T2, path_csv_T2), sep = ";", encoding = "Latin-1")
  
  # filtrar para eleição presidencial apenas
  urnas_T1 <- as.data.table(subset(urnas_T1, DS_CARGO_PERGUNTA=="Presidente"))
  urnas_T2 <- as.data.table(subset(urnas_T2, DS_CARGO_PERGUNTA=="Presidente"))
  
  # criar id_secao
  urnas_T1[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
  urnas_T2[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
  # votos por candidato
  urnas_T1$votos_lula <- ifelse(urnas_T1$NM_VOTAVEL=="LULA", urnas_T1$QT_VOTOS, 0)
  urnas_T1$votos_jair <- ifelse(urnas_T1$NM_VOTAVEL=="JAIR BOLSONARO", urnas_T1$QT_VOTOS, 0)
  urnas_T2$votos_lula <- ifelse(urnas_T2$NM_VOTAVEL=="LULA", urnas_T2$QT_VOTOS, 0)
  urnas_T2$votos_jair <- ifelse(urnas_T2$NM_VOTAVEL=="JAIR BOLSONARO", urnas_T2$QT_VOTOS, 0)
  # agregar por secao
  votos_T1 <- urnas_T1[, .(votos_lula = sum(votos_lula),
                           votos_jair = sum(votos_jair)), by = .(id_secao)]
  votos_T2 <- urnas_T2[, .(votos_lula = sum(votos_lula),
                           votos_jair = sum(votos_jair)), by = .(id_secao)]

  lista_T1[[i]] <- votos_T1
  lista_T2[[i]] <- votos_T2
  
  et <- Sys.time() - st
  
  cat(my_uf[i], " ", et, " ")
}

votos_T1 <- do.call(rbind, lista_T1)
votos_T2 <- do.call(rbind, lista_T2)


# merge data
# separa bases por turno
t1 <- subset(eleicao_2022, NR_TURNO==1)
t2 <- subset(eleicao_2022, NR_TURNO==2)

# merge data
t1 <- merge(t1, votos_T1, by="id_secao", all.x = T)
t2 <- merge(t2, votos_T2, by="id_secao", all.x = T)

# junta as bases novamente
eleicao_2022 <- rbind(t1, t2)

# ajustar NAs

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
            "comparecimento_2022","abstencao_2022",
            
            "dist_sede", "closest_dist_any", "closest_dist",
            "num_0500", "num_1000","num_3000",
            "num_5000","num_10000",
            "votos_lula", "votos_jair",
            
            "dummy_pt", "t", "passe_livre","PIB_PC")

eleicao_2022 <- eleicao_2022[, ..my_var]

fwrite(eleicao_2022, "../../data/base_DiD2022_secoes.csv")

