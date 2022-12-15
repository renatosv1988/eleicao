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
munic <- fread('../../data/munic/munic_dummy_pt.csv')
corr_ibge_tse <- fread("../../data_raw/tse_ibge/correspondencia_IBGE_TSE.csv", encoding = "UTF-8")
pib <- fread("../../data_raw/IBGE/PIBPC_2019_municipios.csv", encoding = "UTF-8")
perfil <- fread('../../data/secoes/secoes_perfil_2014.csv')
votos <- fread('../../data/votes_2014/votos_T1_T2.csv')

eleicao_2014_raw <- fread('../../data/secoes/secoes_2014.csv')
eleicao_2014_raw[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]

gc()





# filter  --------------------------------------------------

# remove detention centers
eleicao_2022_raw <- fread('../../data/secoes/secoes_2022.csv')
eleicao_2022_raw[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]

# idetify detention centers
detention_centers <- c('PRISIONAL|SÓCIO EDUCATIVO|PENITENCIÁRIO|INTERNAÇÃO|UPR DE|UI/UIP|PRESÍDIO|DETENÇÃO|PRIVAÇÃO|PENAL|PENITENCIARIA|PENITENCIÁRIA|SOCIOEDUCATIVO|CADEIA|FUNASE|CADE|CASE|VOTO EM TRÂNSITO|PRESIDIO|PRISIONAL|CENTRO DE RECUPERAÇÃO|SOCIO EDUCATIVO|IASES|CDPSM|FUNDAÇÃO CASA|CDP|SÓCIOEDUCATIVO|RESSOCIALIZAÇÃO|PENINTENCIÁRIO|RESSOCIALIZAÇÃO')
eleicao_2022_raw[, detention_dummy := fifelse(NM_LOCAL_VOTACAO %like% detention_centers, 1, 0)]

id_secao_detention2022 <- eleicao_2022_raw[detention_dummy==1]$id_secao
id_secao_detention2022 |> length()
#> 1088

eleicao_2014 <- eleicao_2014_raw[ id_secao %nin% id_secao_detention2022]
nrow(eleicao_2014)
#> 855408

# MERGE spatial info --------------------------------------------------
espacial[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
espacial <- espacial[,c("dist_sede",
                         "closest_dist_any", "closest_dist", "num_0500",
                        "num_1000", "num_3000","num_5000","num_10000",
                        "id_secao", 'zone'
                        )]
eleicao_2014 <- merge(eleicao_2014, espacial, by="id_secao", all.x = T)

summary(eleicao_2014$num_1000)
#> Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>  1.0    16.0    35.0    46.4    67.0   313.0   14440


eleicao_2014[is.na(num_1000)] |> nrow() / nrow(eleicao_2014)
# 0.1790514


# MERGE MUNIC ------------------------------------------------------------------
corr_ibge_tse <- corr_ibge_tse[,c("codigo_tse", "codigo_ibge")]
colnames(corr_ibge_tse) <- c("CD_MUNICIPIO", "code_muni")
munic <- merge(munic, corr_ibge_tse, by="code_muni", all.x = T)
eleicao_2014 <- merge(eleicao_2014, munic[,c("CD_MUNICIPIO", "dummy_pt", "code_muni")],
                      by="CD_MUNICIPIO", all.x = T)

table(eleicao_2014$dummy_pt, useNA = 'always')
#>       0      1   <NA> 
#>  244096 611312      0 



# MERGE perfil -----------------------------------------------------------------
perfil[, mulheres := mulher / qt_perfil ]
perfil[, educacao_1 := educ_prim / qt_perfil ]
perfil[, idade_16_17 := idade_16_17 / qt_perfil ]
perfil[, idade_18_24 := idade_18_24 / qt_perfil ]
perfil[, idade_60M := idade_60M / qt_perfil ]

summary(perfil$educacao_1)

eleicao_2014 <- merge(eleicao_2014, perfil[,c("id_secao","mulheres","educacao_1",
                                              "idade_16_17","idade_18_24","idade_60M")],
                      by="id_secao", all.x = T)

summary(eleicao_2014$educacao_1)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>  0.0000  0.4091  0.5801  0.5590  0.7220  1.0000     432 

# proportion of missing
eleicao_2014[ is.na(educacao_1) , .N] / nrow(eleicao_2014)
#> 0.0005050222


# MERGE PASSE LIVRE ------------------------------------------------------------

# separa bases por turno
t1 <- subset(eleicao_2014, NR_TURNO==1)
t2 <- subset(eleicao_2014, NR_TURNO==2)

passe_livre_t1 <- passe_livre[,c("CD_MUNICIPIO", "passe_livre_t1", "passe_livre_always", "metro_only")]
colnames(passe_livre_t1) <-c("CD_MUNICIPIO", "passe_livre", "passe_livre_always", "metro_only") 

passe_livre_t2 <- passe_livre[,c("CD_MUNICIPIO", "passe_livre_t2", "passe_livre_always", "metro_only")]
colnames(passe_livre_t2) <-c("CD_MUNICIPIO", "passe_livre", "passe_livre_always", "metro_only") 

# merge data
t1 <- merge(t1, passe_livre_t1, by="CD_MUNICIPIO", all.x = T)
t2 <- merge(t2, passe_livre_t2, by="CD_MUNICIPIO", all.x = T)

# junta as bases novamente
eleicao_2014 <- rbind(t1, t2)

# replace NAs with 0
eleicao_2014[is.na(passe_livre), passe_livre := 0]


table(eleicao_2014$passe_livre, useNA = 'always')
#>       0      1   <NA> 
#>  596685 258723      0 

table(eleicao_2014$passe_livre_always, useNA = 'always')
#>     1   <NA> 
#>  3516 851892 


# MERGE PIB_PC 2019 ------------------------------------------------------------
eleicao_2014 <- merge(eleicao_2014, pib, by="code_muni")

summary(eleicao_2014$PIB_PC)
#> Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 4483   15979   27798   34275   47385  464884



# add regions  -----------------------------------------------------------------
regions <- geobr::read_region()
regions$geom <- NULL

# add region to section data
eleicao_2014[, code_region := substring(code_muni, 1, 1) |> as.numeric() ]
eleicao_2014 <- left_join(eleicao_2014, regions, by=c('code_region'))
table(eleicao_2014$name_region)

# adicionar votacao por candidato ----------------------------------------------

# merge data
eleicao_2014 <- merge(eleicao_2014, 
                          votos, 
                          by=c("ANO_ELEICAO", "SG_UF", "CD_MUNICIPIO", "NR_TURNO","id_secao"), 
                          all.x = T)

summary(eleicao_2014$votos_lula)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's  
#>     0.0    75.0   109.0   114.2   149.0   429.0       6

summary(eleicao_2014$votos_total)
#>   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>    1.0   228.0   279.0   265.8   313.0   607.0       6 

summary(eleicao_2014$comparecimento_2014)
#>   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.0000  0.7575  0.8042  0.7981  0.8458  1.0000 


# adicionar variacao de comparecimento por municipio em 2014 -------------------

# separar por turno
T1 <- eleicao_2014[NR_TURNO==1,]
T2 <- eleicao_2014[NR_TURNO==2,]

# calcular comparecimento por secao por turno
T1[, comparecimento_t1 := sum(QT_COMPARECIMENTO) / sum(QT_APTOS), by = id_secao]
T2[, comparecimento_t2 := sum(QT_COMPARECIMENTO) / sum(QT_APTOS), by = id_secao]

summary(T1$comparecimento_t1)

# juntar turnos
MM <- merge(T1[, .(id_secao, comparecimento_t1)], 
            T2[, .(id_secao, comparecimento_t2)], 
            by="id_secao", all.x = T)

# calculate variation in turnout
MM[, variacao_comparecimento_2014 := comparecimento_t2 - comparecimento_t1]

# merge na base principal
eleicao_2014 <- merge(eleicao_2014,
                      MM[,c("id_secao","variacao_comparecimento_2014")],
                      by="id_secao", all.x = T)

summary(eleicao_2014$variacao_comparecimento_2014)

# salvar arquivo final----------------------------------------------------------
# seleciona apenas variaveis que serao usadas
my_var <- c("id_secao",  "CD_MUNICIPIO","NR_ZONA", "NR_SECAO",
            "ANO_ELEICAO","NR_TURNO", "SG_UF","CD_CARGO",
            
            "code_muni", "name_region",
            
            "QT_APTOS","QT_COMPARECIMENTO","QT_ABSTENCOES","QT_VOTOS_NOMINAIS",
            "QT_VOTOS_BRANCOS","QT_VOTOS_NULOS", "QT_VOTOS_LEGENDA",
            "QT_VOTOS_ANULADOS_APU_SEP","NR_LOCAL_VOTACAO",
            
            "gov_2t",
            "mulheres","educacao_1",
            "idade_16_17","idade_18_24","idade_60M",
            "comparecimento_2014","abstencao_2014",
            
            "variacao_comparecimento_2014",  
            
            "dist_sede",
            "closest_dist_any", "closest_dist",
            "num_0500", "num_1000","num_3000",
            "num_5000","num_10000", 'zone',
            "votos_lula", "votos_total",
            "votos_nulo" , "votos_validos", "votos_branco", 
            "dummy_pt", "passe_livre", "passe_livre_always", "metro_only", "PIB_PC")

eleicao_2014 <- eleicao_2014[, ..my_var]

fwrite(eleicao_2014, "../../data/base_DiD2014_secoes.csv")
saveRDS(eleicao_2014, "../../data/base_DiD2014_secoes.rds",compress = T)
