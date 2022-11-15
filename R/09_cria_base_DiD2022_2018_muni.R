library(geobr)
library(data.table)
library(ggplot2)


# read data ----------------------------------------------------------------------

eleicao_2022 <- fread('../../data/secoes/secoes_2022.csv')
eleicao_2018 <- fread('../../data/secoes/secoes_2018.csv')
passe_livre <- fread('../../data/passe_livre/passe_livre_resumo.csv')
espacial <- fread('../../data/spatial/electoral_sections_spatial.csv')
munic <- fread('../../data/munic/munic_dummy_pt.csv')
corr_ibge_tse <- fread("../../data_raw/tse_ibge/correspondencia_IBGE_TSE.csv", encoding = "UTF-8")
pib <- fread("../../data_raw/IBGE/PIBPC_2019_municipios.csv", encoding = "UTF-8")
perfil <- fread('../../data/secoes/secoes_perfil_2022.csv')



# gather section data ----------------------------------------------------------------------

# check
eleicao_2022[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
eleicao_2018[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]


sum(eleicao_2022$id_secao %in% eleicao_2018$id_secao) / nrow(eleicao_2022)
#> 0.9281777


# juntar 2018 e 2022
eleicao_2022 <- eleicao_2022[,-c("NM_LOCAL_VOTACAO","DS_LOCAL_VOTACAO_ENDERECO")]

setnames(eleicao_2022,
         old = c('comparecimento_2022','abstencao_2022'),
         new = c('comparecimento', 'abstencao'))
setnames(eleicao_2018,
         old = c('comparecimento_2018','abstencao_2018'),
         new = c('comparecimento', 'abstencao'))

eleicao <- rbind(eleicao_2018, eleicao_2022)



# MERGE MUNIC ------------------------------------------------------------------
corr_ibge_tse <- corr_ibge_tse[,c("codigo_tse", "codigo_ibge")]
colnames(corr_ibge_tse) <- c("CD_MUNICIPIO", "code_muni")
munic <- merge(munic, corr_ibge_tse, by="code_muni", all.x = T)

eleicao <- merge(eleicao, munic[,c("CD_MUNICIPIO", "dummy_pt", "code_muni")],
                 by="CD_MUNICIPIO", all.x = T)




# MERGE spatial info --------------------------------------------------
espacial[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
espacial <- espacial[,c("dist_sede","closest_dist_any", "closest_dist", "num_0500",
                        "num_1000", "num_3000","num_5000","num_10000",
                        "id_secao")]
eleicao <- merge(eleicao, espacial, by="id_secao", all.x = T)



# aggregate by muni ------------------------------------------------------------

# aggregate variables at muni level
df_muni <- eleicao[, .(QT_APTOS = sum(QT_APTOS),
                       QT_APTOS_log = log(sum(QT_APTOS)),
                       dummy_pt = max(dummy_pt),
                       gov_2t = max(gov_2t),
                       # pib_log = log(PIB_PC[1L]),
                       # votos_jair = sum(votos_jair),
                       # votos_lula = sum(votos_lula),
                       # votos_total = sum(votos_total),
                       num_1000 = mean(num_1000),
                       comparecimento = weighted.mean(comparecimento, w=QT_APTOS)
                       ),
                   by = .(code_muni, CD_MUNICIPIO, ANO_ELEICAO, NR_TURNO)]

head(df_muni)

# MERGE PASSE LIVRE ------------------------------------------------------------

# # separa bases por ano e turno
# t1o <- subset(df_muni, NR_TURNO==1 & ANO_ELEICAO==2018)
# t2o <- subset(df_muni, NR_TURNO==2 & ANO_ELEICAO==2018)
# t1 <- subset(df_muni, NR_TURNO==1 & ANO_ELEICAO==2022)
# t2 <- subset(df_muni, NR_TURNO==2 & ANO_ELEICAO==2022)

# passe_livre_t1 <- passe_livre[,c("CD_MUNICIPIO", "passe_livre_t1")]
# # colnames(passe_livre_t1) <- c("CD_MUNICIPIO", "passe_livre") 
# passe_livre_t2 <- passe_livre[,c("CD_MUNICIPIO", "passe_livre_t2")]
# colnames(passe_livre_t2) <- c("CD_MUNICIPIO", "passe_livre") 


# merge data
df_muni2 <- merge(df_muni, passe_livre, by="CD_MUNICIPIO", all.x = T)


# replace NAs with 0
df_muni2[is.na(passe_livre_t1), passe_livre_t1 := 0]
df_muni2[is.na(passe_livre_t2), passe_livre_t2 := 0]

head(df_muni2)


# MERGE PIB_PC 2019 ------------------------------------------------------------
df_muni2 <- merge(df_muni2, pib, by="code_muni")




# MERGE perfil -----------------------------------------------------------------
perfil[, mulheres :=  mulher / qt_perfil ]
perfil[, educacao_1 :=  educ_prim / qt_perfil ]
perfil[, idade_16_17 :=  idade_16_17 / qt_perfil ]
perfil[, idade_18_24 :=  idade_18_24 / qt_perfil ]
perfil[, idade_60M :=  idade_60M / qt_perfil ]

summary(perfil$educacao_1)
#>   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.0000  0.2899  0.4179  0.4215  0.5534  1.0000 

perfil_muni <- perfil[, .(educacao_1 = weighted.mean(educacao_1, w=qt_perfil)),
                      by = CD_MUNICIPIO]


df_muni2 <- merge(df_muni2, perfil_muni, by="CD_MUNICIPIO", all.x = T)


summary(df_muni2$educacao_1)
#>   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.1693  0.4627  0.5311  0.5237  0.5924  0.8102 





# salvar arquivo final----------------------------------------------------------



fwrite(df_muni2, "../../data/base_DiD2022_2018_muni.csv")
