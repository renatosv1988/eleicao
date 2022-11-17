library(geobr)
library(data.table)
library(ggplot2)
library(scales)
library(fixest)
library(vroom)
library(purrr)

`%nin%` <- negate(`%in%`)
`%unlike%` <- negate(`%like%`)


# load data -------------------------------------------------------------------

passe_livre <- fread('../../data/passe_livre/passe_livre_resumo.csv')
espacial <- fread('../../data/spatial/electoral_sections_spatial.csv')
munic <- fread('../../data/munic/munic_dummy_pt.csv', encoding = 'UTF-8')
corr_ibge_tse <- fread("../../data_raw/tse_ibge/correspondencia_IBGE_TSE.csv", encoding = "UTF-8")
pib <- fread("../../data_raw/IBGE/PIBPC_2019_municipios.csv", encoding = "UTF-8")
perfil <- fread('../../data/secoes/secoes_perfil_2022.csv')
votos_T1 <- fread('../../data/votes/votos_T1.csv')
votos_T2 <- fread('../../data/votes/votos_T2.csv')
SE18 <- fread('../../data/secoes/secoes_2018.csv')
eleicao_2022_raw <- fread('../../data/secoes/secoes_2022.csv')
eleicao_2022_raw[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]

gc()

# filter  --------------------------------------------------

# remove detention centers
nrow(eleicao_2022_raw)
#> 942020

detention_centers <- c('PRISIONAL|SÓCIO EDUCATIVO|PENITENCIÁRIO|INTERNAÇÃO|UPR DE|UI/UIP|PRESÍDIO|DETENÇÃO|PRIVAÇÃO|PENAL|PENITENCIARIA|PENITENCIÁRIA|SOCIOEDUCATIVO|CADEIA|FUNASE|CADE|CASE|VOTO EM TRÂNSITO|PRESIDIO|PRISIONAL|CENTRO DE RECUPERAÇÃO|SOCIO EDUCATIVO|IASES|CDPSM|FUNDAÇÃO CASA|CDP|SÓCIOEDUCATIVO|RESSOCIALIZAÇÃO|PENINTENCIÁRIO|RESSOCIALIZAÇÃO')
eleicao_2022 <- eleicao_2022_raw[ NM_LOCAL_VOTACAO %unlike% detention_centers]
nrow(eleicao_2022)
#> 940932



espacial$NR_ZONA
# MERGE spatial info --------------------------------------------------
espacial[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
espacial <- espacial[,c("dist_sede", "closest_dist_any", "closest_dist", "num_0500",
                        "num_1000", "num_3000","num_5000","num_10000",
                        "id_secao", 'zone'
                        )]
eleicao_2022 <- merge(eleicao_2022, espacial, by="id_secao", all.x = T)

summary(eleicao_2022$num_1000)
#> Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 1.00   15.00   34.00   45.43   65.00  313.00 

table(eleicao_2022$zone)



# MERGE MUNIC ------------------------------------------------------------------
corr_ibge_tse <- corr_ibge_tse[,c("codigo_tse", "codigo_ibge")]
colnames(corr_ibge_tse) <- c("CD_MUNICIPIO", "code_muni")
munic <- merge(munic, corr_ibge_tse, by="code_muni", all.x = T)
eleicao_2022 <- merge(eleicao_2022, munic[,c("CD_MUNICIPIO", "dummy_pt", "code_muni")],
                      by="CD_MUNICIPIO", all.x = T)

summary(eleicao_2022$dummy_pt)
#>   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.0000  0.0000  1.0000  0.7105  1.0000  1.0000 


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

#  check NAs
# eleicao_2022[is.na(educacao_1)] |> View()



# MERGE PASSE LIVRE ------------------------------------------------------------

# separa bases por turno
t1 <- subset(eleicao_2022, NR_TURNO==1)
t2 <- subset(eleicao_2022, NR_TURNO==2)

passe_livre_t1 <- passe_livre[,c("CD_MUNICIPIO", "passe_livre_t1")]
colnames(passe_livre_t1) <-c("CD_MUNICIPIO", "passe_livre") 
passe_livre_t2 <- passe_livre[,c("CD_MUNICIPIO", "passe_livre_t2")]
colnames(passe_livre_t2) <-c("CD_MUNICIPIO", "passe_livre") 

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
#>  655349 285583      0 






# MERGE PIB_PC 2019 ------------------------------------------------------------
eleicao_2022 <- merge(eleicao_2022, pib, by="code_muni")

summary(eleicao_2022$PIB_PC)
#> Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 4483   16152   28042   34611   47749  464884 




# adicionar votação por candidato ----------------------------------------------


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


summary(eleicao_2022$votos_validos)

# adicionar variação de comparecimento por municipio em 2018 -------------------
# separar por turno
T1 <- SE18[NR_TURNO==1,]
T2 <- SE18[NR_TURNO==2,]

# calcular comparecimento por municipio por turno
M1 <- T1[,.(comparecimento_t1 = sum(QT_COMPARECIMENTO),
            aptos_t1 = sum(QT_APTOS)),
         by=CD_MUNICIPIO]

M2 <- T2[,.(comparecimento_t2 = sum(QT_COMPARECIMENTO),
            aptos_t2 = sum(QT_APTOS)),
         by=CD_MUNICIPIO]

M1$comp_t1 <- M1$comparecimento_t1/M1$aptos_t1
M2$comp_t2 <- M2$comparecimento_t2/M2$aptos_t2

# juntar turnos
MM <- merge(M1, M2[,c("CD_MUNICIPIO","comp_t2")], by="CD_MUNICIPIO", all.x = T)

# calcular variação
MM$variacao_comparecimento_2018 <- MM$comp_t2 - MM$comp_t1

# merge na base principal
eleicao_2022 <- merge(eleicao_2022,
                      MM[,c("CD_MUNICIPIO","variacao_comparecimento_2018")],
                      by="CD_MUNICIPIO", all.x = T)

# adicionar votação por turno (p/ PLACEBO) em 2018 -----------------------------

# separar por turno

SE18[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]

T1 <- subset(eleicao_2022, NR_TURNO==1)
T2 <- subset(eleicao_2022, NR_TURNO==2)
T1_18 <- subset(SE18, NR_TURNO==1)
T2_18 <- subset(SE18, NR_TURNO==2)

T1 <- merge(T1, T1_18[,c("id_secao","comparecimento_2018")], by="id_secao", all.x=T)
T2 <- merge(T2, T2_18[,c("id_secao","comparecimento_2018")], by="id_secao", all.x=T)

eleicao_2022 <- rbind(T1, T2)



# salvar arquivo final----------------------------------------------------------
# seleciona apenas variáveis que serão usadas
my_var <- c("id_secao",  "CD_MUNICIPIO","NR_ZONA", "NM_LOCAL_VOTACAO", "NR_SECAO",
            "ANO_ELEICAO","NR_TURNO", "SG_UF", "NM_MUNICIPIO", "CD_CARGO",
            
            "code_muni",
            
            "QT_APTOS","QT_COMPARECIMENTO","QT_ABSTENCOES","QT_VOTOS_NOMINAIS",
            "QT_VOTOS_BRANCOS","QT_VOTOS_NULOS", "QT_VOTOS_LEGENDA",
            "QT_VOTOS_ANULADOS_APU_SEP","NR_LOCAL_VOTACAO",
            
            "gov_2t",
            "mulheres","educacao_1",
            "idade_16_17","idade_18_24","idade_60M",
            "biometria", "qt_biometria",
            "comparecimento_2022","abstencao_2022",
            "comparecimento_2018",
            
            "variacao_comparecimento_2018",
             
            "dist_sede", "closest_dist_any", "closest_dist",
            "num_0500", "num_1000","num_3000",
            "num_5000","num_10000",
            'zone',
            "votos_lula", "votos_jair", "votos_total",
            "votos_nulo", "votos_branco", "votos_validos",
            "dummy_pt", "passe_livre","PIB_PC")

eleicao_2022 <- eleicao_2022[, ..my_var]

fwrite(eleicao_2022, "../../data/base_DiD2022_secoes.csv")

