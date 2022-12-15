library(geobr)
library(data.table)
library(ggplot2)
library(scales)
library(fixest)
library(vroom)
library(purrr)
library(dplyr)

`%nin%` <- Negate(`%in%`)
`%unlike%` <- Negate(`%like%`)


# load data -------------------------------------------------------------------

passe_livre <- fread('../../data/passe_livre/passe_livre_resumo.csv')
espacial <- fread('../../data/spatial/electoral_sections_spatial.csv')
munic <- fread('../../data/munic/munic_dummy_pt.csv', encoding = 'UTF-8')
corr_ibge_tse <- fread("../../data_raw/tse_ibge/correspondencia_IBGE_TSE.csv", encoding = "UTF-8")
pib <- fread("../../data_raw/IBGE/PIBPC_2019_municipios.csv", encoding = "UTF-8")
perfil <- fread('../../data/secoes/secoes_perfil_2022.csv')
votos_T1 <- fread('../../data/votes_2022/votos_T1.csv')
votos_T2 <- fread('../../data/votes_2022/votos_T2.csv')
SE18 <- fread('../../data/base_DiD2018_secoes.csv')

eleicao_2022_raw <- fread('../../data/secoes/secoes_2022.csv')
eleicao_2022_raw[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]

gc()


# #check linkage of sections between 2018 and 2022
# 
# (unique(SE18$id_secao) %in% unique(eleicao_2022_raw$id_secao) |>sum()) / length(unique(SE18$id_secao))
# # 0.9635454
# 
# (unique(eleicao_2022_raw$id_secao) %in% unique(SE18$id_secao) |>sum()) / length(unique(eleicao_2022_raw$id_secao))
# # 0.9273243


# filter  --------------------------------------------------

# remove detention centers
nrow(eleicao_2022_raw)
#> 942020

detention_centers <- c('PRISIONAL|SÓCIO EDUCATIVO|PENITENCIÁRIO|INTERNAÇÃO|UPR DE|UI/UIP|PRESÍDIO|DETENÇÃO|PRIVAÇÃO|PENAL|PENITENCIARIA|PENITENCIÁRIA|SOCIOEDUCATIVO|CADEIA|FUNASE|CADE|CASE|VOTO EM TRÂNSITO|PRESIDIO|PRISIONAL|CENTRO DE RECUPERAÇÃO|SOCIO EDUCATIVO|IASES|CDPSM|FUNDAÇÃO CASA|CDP|SÓCIOEDUCATIVO|RESSOCIALIZAÇÃO|PENINTENCIÁRIO|RESSOCIALIZAÇÃO')
eleicao_2022 <- eleicao_2022_raw[ NM_LOCAL_VOTACAO %unlike% detention_centers]
nrow(eleicao_2022)
#> 940932




# MERGE spatial info --------------------------------------------------
espacial[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
espacial <- espacial[,c("dist_sede", "closest_dist_any", "closest_dist", "num_0500",
                        "num_1000", "num_3000","num_5000","num_10000",
                        "id_secao", 'zone')]
eleicao_2022 <- merge(eleicao_2022, espacial, by="id_secao", all.x = T)

summary(eleicao_2022$num_1000)
#> Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 1.00   15.00   34.00   45.43   65.00  313.00 

table(eleicao_2022$zone, useNA = 'always')
#>   rural  urban   <NA> 
#>  100286 840646      0 


# MERGE MUNIC ------------------------------------------------------------------
corr_ibge_tse <- corr_ibge_tse[,c("codigo_tse", "codigo_ibge")]
colnames(corr_ibge_tse) <- c("CD_MUNICIPIO", "code_muni")

munic <- merge(munic, corr_ibge_tse, by="code_muni", all.x = T)
eleicao_2022 <- merge(eleicao_2022, munic[,c("CD_MUNICIPIO", "dummy_pt", "code_muni")],
                      by="CD_MUNICIPIO", all.x = T)

table(eleicao_2022$dummy_pt, useNA = 'always')
#>       0      1   <NA> 
#>  272420 668512      0 


# MERGE perfil -----------------------------------------------------------------
perfil[, mulheres := mulher / qt_perfil ]
perfil[, educacao_1 := educ_prim / qt_perfil ]
perfil[, idade_16_17 := idade_16_17 / qt_perfil ]
perfil[, idade_18_24 := idade_18_24 / qt_perfil ]
perfil[, idade_60M := idade_60M / qt_perfil ]
perfil[, biometria := qt_biometria / qt_perfil ]


summary(perfil$educacao_1)
summary(perfil$biometria)

eleicao_2022 <- merge(eleicao_2022, perfil[,c("id_secao","mulheres","educacao_1",
                                              "idade_16_17","idade_18_24","idade_60M", 
                                              "biometria", "qt_biometria")],
                      by="id_secao", all.x = T)

summary(eleicao_2022$educacao_1)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>  0.0000  0.2887  0.4150  0.4189  0.5494  1.0000     210   # pq ????





# MERGE PASSE LIVRE ------------------------------------------------------------

# separa bases por turno
t1 <- subset(eleicao_2022, NR_TURNO==1)
t2 <- subset(eleicao_2022, NR_TURNO==2)

passe_livre_t1 <- passe_livre[,c("CD_MUNICIPIO", "passe_livre_t1", "passe_livre_always")]
colnames(passe_livre_t1) <-c("CD_MUNICIPIO", "passe_livre", "passe_livre_always") 

passe_livre_t2 <- passe_livre[,c("CD_MUNICIPIO", "passe_livre_t2", "passe_livre_always")]
colnames(passe_livre_t2) <-c("CD_MUNICIPIO", "passe_livre", "passe_livre_always") 

# merge data
t1 <- merge(t1, passe_livre_t1, by="CD_MUNICIPIO", all.x = T)
t2 <- merge(t2, passe_livre_t2, by="CD_MUNICIPIO", all.x = T)

# junta as bases novamente
eleicao_2022 <- rbind(t1, t2)
table(eleicao_2022$passe_livre, useNA = 'always')

# replace NAs with 0
eleicao_2022[is.na(passe_livre), passe_livre := 0]

table(eleicao_2022$passe_livre, useNA = 'always')
#>       0      1   <NA> 
#>  652313 288619      0 


table(eleicao_2022$passe_livre_always, useNA = 'always')
#>     1   <NA> 
#>  4420 936512 




# MERGE PIB_PC 2019 ------------------------------------------------------------
eleicao_2022 <- merge(eleicao_2022, pib, by="code_muni")

summary(eleicao_2022$PIB_PC)
#> Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 4483   16152   28042   34611   47749  464884 



# add regions  -----------------------------------------------------------------
regions <- geobr::read_region()
regions$geom <- NULL

# add region to section data
eleicao_2022[, code_region := substring(code_muni, 1, 1) |> as.numeric() ]
eleicao_2022 <- dplyr::left_join(eleicao_2022, regions, by=c('code_region'))
table(eleicao_2022$name_region)



# adicionar votacao por candidato ----------------------------------------------

# merge data
# separa bases por turno
t1 <- subset(eleicao_2022, NR_TURNO==1)
t2 <- subset(eleicao_2022, NR_TURNO==2)

# temp 6666
votos_T1 <- votos_T1[, -c('QT_APTOS','QT_ABSTENCOES','QT_COMPARECIMENTO')]
votos_T2 <- votos_T2[, -c('QT_APTOS','QT_ABSTENCOES','QT_COMPARECIMENTO')]


# merge data
t1 <- merge(t1, votos_T1, by="id_secao", all.x = T)
t2 <- merge(t2, votos_T2, by="id_secao", all.x = T)

# junta as bases novamente
eleicao_2022 <- rbind(t1, t2)


summary(eleicao_2022$votos_lula)
#> Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.0    89.0   119.0   124.6   156.0   423.0 

summary(eleicao_2022$votos_jair)
#> Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.0    78.0   118.0   115.8   153.0   384.0 

summary(eleicao_2022$votos_total)
#> Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 10.0   228.0   269.0   262.6   302.0   508.0 

summary(eleicao_2022$comparecimento_2022)
#>   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.1330  0.7576  0.8006  0.7939  0.8361  1.0000 




# adicionar variacao de comparecimento em 2018 por secao  -------------------
summary(SE18$comparecimento_2018)

#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.0000  0.7500  0.8009  0.7940  0.8449  1.0000

# share of PT votes
SE18[, votos_lula_2018 := sum(votos_lula) / sum(votos_validos), by = .(ANO_ELEICAO, NR_TURNO, id_secao)]
summary(SE18$votos_lula_2018)

eleicao_2022 <- merge(eleicao_2022,
                      SE18[, .(id_secao, NR_TURNO, comparecimento_2018, variacao_comparecimento_2018, votos_lula_2018)], 
                      by=c('id_secao', 'NR_TURNO'), all.x = T)

summary(eleicao_2022$comparecimento_2018)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>    0.00    0.75    0.80    0.79    0.84    1.00   67372 

eleicao_2022[ is.na(comparecimento_2018) , .N] / nrow(eleicao_2022)
#> 0.07160347



# adicionar comparecimento em 2018 por municipio  -------------------

comparecimento_2018 <-   SE18[, .(comparecimento_2018_muni = weighted.mean(comparecimento_2018, w=QT_APTOS)),
                                  by = .(CD_MUNICIPIO, NR_TURNO) ]

summary(comparecimento_2018$comparecimento_2018_muni)
#>   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.5273  0.7400  0.7869  0.7828  0.8321  0.9401 


# merge
eleicao_2022 <- merge(eleicao_2022,
                      comparecimento_2018,
                      by=c('CD_MUNICIPIO', 'NR_TURNO'), all.x = T)

summary(eleicao_2022$comparecimento_2018_muni)


# adicionar variacao de comparecimento em 2018 por municipio  -------------------

var_compar_2018_muni <- SE18[, .(variacao_comparecimento_2018_muni = weighted.mean(variacao_comparecimento_2018, w=QT_APTOS)), 
                                  by=CD_MUNICIPIO]

summary(var_compar_2018_muni$variacao_comparecimento_2018_muni)
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -0.236164 -0.026915 -0.012099 -0.018386 -0.003857  0.132907 

# merge
eleicao_2022 <- merge(eleicao_2022,
                      var_compar_2018_muni,
                      by="CD_MUNICIPIO", all.x = T)

summary(eleicao_2022$variacao_comparecimento_2018_muni)


# total number o voters in 2022
eleicao_2022[NR_TURNO==1, sum(QT_APTOS)]
#> 155.631.548

# salvar arquivo final----------------------------------------------------------
# seleciona apenas variáveis que serão usadas
my_var <- c("id_secao",  "CD_MUNICIPIO","NR_ZONA", "NM_LOCAL_VOTACAO", "NR_SECAO",
            "ANO_ELEICAO","NR_TURNO", "SG_UF", "NM_MUNICIPIO", "CD_CARGO",
            
            "code_muni", "name_region",
            
            "QT_APTOS","QT_COMPARECIMENTO","QT_ABSTENCOES","QT_VOTOS_NOMINAIS",
            "QT_VOTOS_BRANCOS","QT_VOTOS_NULOS", "QT_VOTOS_LEGENDA",
            "QT_VOTOS_ANULADOS_APU_SEP","NR_LOCAL_VOTACAO",
            
            "gov_2t",
            "mulheres","educacao_1",
            "idade_16_17","idade_18_24","idade_60M",
            "biometria", "qt_biometria",
            "comparecimento_2022","abstencao_2022",
            
            "comparecimento_2018", "comparecimento_2018_muni", 'votos_lula_2018',
            "variacao_comparecimento_2018", "variacao_comparecimento_2018_muni",
             
            
            "dist_sede", "closest_dist_any", "closest_dist",
            "num_0500", "num_1000","num_3000",
            "num_5000","num_10000", 'zone',
            "votos_lula", "votos_jair", "votos_total",
            "votos_nulo", "votos_branco", "votos_validos",
            "dummy_pt", "passe_livre", "passe_livre_always", "PIB_PC")

eleicao_2022 <- eleicao_2022[, ..my_var]

fwrite(eleicao_2022, "../../data/base_DiD2022_secoes.csv")
saveRDS(eleicao_2022, "../../data/base_DiD2022_secoes.rds",compress = T)

summary(eleicao_2022$comparecimento_2022)
summary(eleicao_2022$comparecimento_2018)
