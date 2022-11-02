library(data.table)
library(dplyr)


#### eleitorado --------------------------------------------------
#' perfil de escolaridade em cada zona


# read data
files_eleitorado <- list.files(path = './data_raw/eleitorado', pattern = '2018.csv', full.names = T)
eleitorado <- fread( files_eleitorado, 
                     # nrows = Inf, 
                     encoding = "Latin-1")


table(eleitorado$DS_GRAU_ESCOLARIDADE, useNA = 'always')

# remove eleitores fora do Brasil
sum(eleitorado$QT_ELEITORES_PERFIL)
#> 156.454.011

eleitorado <- subset(eleitorado, SG_UF != 'ZZ')

# conta quantidade de eleitores por grau de escolaridade em cada zona  de cada municipio
df_eleitorado <- eleitorado[, .(QT_ELEITORES_PERFIL = sum(QT_ELEITORES_PERFIL)), 
                            by = .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, 
                                   NR_ZONA, 
                                   DS_GRAU_ESCOLARIDADE)]

head(df_eleitorado)

sum(df_eleitorado$QT_ELEITORES_PERFIL)
#> 155.756.933

# distribuicao de escolaridade
df_eleitorado[, 100 * sum(QT_ELEITORES_PERFIL) / sum(df_eleitorado$QT_ELEITORES_PERFIL), 
              by = DS_GRAU_ESCOLARIDADE] 



# calcula proporcao de pop por educacao em cada zona

# get total population in each zone
df_eleitorado[, pop_zona := sum(QT_ELEITORES_PERFIL), by = c('SG_UF', 'CD_MUNICIPIO', 'NM_MUNICIPIO', 'NR_ZONA')]
df_eleitorado[, pop_muni := sum(QT_ELEITORES_PERFIL), by = c('SG_UF', 'CD_MUNICIPIO', 'NM_MUNICIPIO')]

# sum pop in each edu level
ate_fundamental_completo <- c('LÊ E ESCREVE', 'ENSINO FUNDAMENTAL INCOMPLETO', 'ENSINO FUNDAMENTAL COMPLETO', 'ANALFABETO')

df_zona_educacao <- df_eleitorado[, .(
                      edu_superior = sum(QT_ELEITORES_PERFIL[which(DS_GRAU_ESCOLARIDADE == 'SUPERIOR COMPLETO')]) / pop_zona[1L], 
                      edu_ate_fundamental = sum(QT_ELEITORES_PERFIL[which(DS_GRAU_ESCOLARIDADE %in% ate_fundamental_completo)]) / pop_zona[1L]), 
                      by = c('SG_UF', 'CD_MUNICIPIO', 'NM_MUNICIPIO', 'NR_ZONA')]



df_muni_educacao <- df_eleitorado[, .(
  edu_superior = sum(QT_ELEITORES_PERFIL[which(DS_GRAU_ESCOLARIDADE == 'SUPERIOR COMPLETO')]) / pop_muni[1L], 
  edu_ate_fundamental = sum(QT_ELEITORES_PERFIL[which(DS_GRAU_ESCOLARIDADE %in% ate_fundamental_completo)]) / pop_muni[1L]), 
  by = c('SG_UF', 'CD_MUNICIPIO', 'NM_MUNICIPIO')]



head(df_muni_educacao)
head(df_zona_educacao)


sum(df_eleitorado$QT_ELEITORES_PERFIL)
#' 155.756.933



# clean memory
rm(files_eleitorado, eleitorado)
gc()




#### zonas --------------------------------------------------
#' quantidade de eleitores registrados em cada secao e cada zona

# read data
files_zonas <- list.files(path = './data_raw/zonas', pattern = '2018.csv', full.names = T)
zonas <- fread( files_zonas, nrows = Inf)

# remove zonas fora do Brasil
zonas <- subset(zonas, SG_UF != 'ZZ')

# contar quantidade de eleitores por grau de escolaridade em cada zona  de cada municipio
df_zonas <- zonas[, .(QT_ELEITOR_SECAO=sum(QT_ELEITOR),
                      QT_ELEITOR_ELEICAO_FEDERAL=sum(QT_ELEITOR_ELEICAO)), 
                  by = .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, 
                         NR_ZONA,  # NR_SECAO,
                         NR_TURNO
                         # , NR_LONGITUDE, NR_LATITUDE
                  )]

df_zonas_aggreg_mun <- zonas[, .(QT_ELEITOR_SECAO=sum(QT_ELEITOR),
                                 QT_ELEITOR_ELEICAO_FEDERAL=sum(QT_ELEITOR_ELEICAO)), 
                             by = .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_TURNO
                                    # , NR_LONGITUDE, NR_LATITUDE
                                    )]
head(df_zonas)



df_zonas[, .(QT_ELEITOR_ELEICAO_FEDERAL = sum(QT_ELEITOR_ELEICAO_FEDERAL)), by=NR_TURNO]
#>      NR_TURNO QT_ELEITOR_ELEICAO_FEDERAL
#> 1:        1                  155.758.656
#> 2:        2                  155.758.433



# clean memory
rm(files_zonas, zonas)
gc()





#### votacao --------------------------------------------------
#' quantidade de votos nominais e validos

# read
files_votacao <- list.files(path = './data_raw/votacao', 
                            pattern = 'votacao_candidato_munzona_2018_BR.csv', 
                            full.names = T)

votacao <- fread(files_votacao,
                  # nrows = Inf, 
                  encoding = "Latin-1")
  votacao$QT_VOTOS_NOMINAIS_VALIDOSO


votacao[, .(QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS)), by=NR_TURNO]

# mantem apenas votos para presidente no BR
votacao_pres <- subset(votacao, DS_CARGO == 'Presidente' & SG_UF != 'ZZ')

votacao_pres$QT_VOTOS_NOMINAIS

# contage de votos
df_votacao_municipio <- votacao_pres[, .(QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS),
                               QT_VOTOS_NOMINAIS_VALIDOS = NA), 
                      by = .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, 
                             # NR_ZONA,
                             NR_TURNO, DS_CARGO
                             #, SG_PARTIDO,
                             # ST_VOTO_EM_TRANSITO,
                             # DS_SIT_TOT_TURNO
                             )]

df_votacao_zonas <- votacao_pres[, .(QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS),
                                         QT_VOTOS_NOMINAIS_VALIDOS = NA), 
                                     by = .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, 
                                            NR_ZONA,
                                            NR_TURNO, DS_CARGO
                                            #, SG_PARTIDO,
                                            # ST_VOTO_EM_TRANSITO,
                                            # DS_SIT_TOT_TURNO
                                     )]


head(df_votacao_municipio)

df_votacao_municipio[, .(QT_VOTOS_NOMINAIS = sum(QT_VOTOS_NOMINAIS)), by=NR_TURNO]
#'   NR_TURNO QT_VOTOS_NOMINAIS
#'1:        1         117.935.194
#'2:        2         118.254.184


# # clean memory
rm(files_votacao, votacao)
gc()






#### comparecimento nacional --------------------------------------------------

df_votacao_municipio[, .(comparecimento = sum(QT_VOTOS_NOMINAIS) / sum(df_eleitorado$QT_ELEITORES_PERFIL) ), 
           by= NR_TURNO]
#'    NR_TURNO comparecimento
#' 1:        1      0.7571746
#' 2:        2      0.7592226


# abstencao
df_votacao_municipio[, .(abstencao = 1- sum(QT_VOTOS_NOMINAIS) / sum(df_eleitorado$QT_ELEITORES_PERFIL)), 
                     by= NR_TURNO]

#' https://www.bbc.com/portuguese/brasil-63421487
#' Turno 1: 20.95
#' Turno 2: 20.57




#### comparecimento municipal --------------------------------------------------


# por municipio
compar_muni <- left_join(df_zonas_aggreg_mun, df_votacao_municipio, 
                           by=c("SG_UF", "CD_MUNICIPIO", "NR_TURNO")) |> setDT()
head(compar_muni)

compar_muni[is.na(QT_VOTOS_NOMINAIS)]

compar_muni[, comparecimento := QT_VOTOS_NOMINAIS / QT_ELEITOR_SECAO]

summary(compar_muni$comparecimento)
#'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#' 0.5180  0.7265  0.7578  0.7559  0.7895  0.8897







#### comparecimento zonas --------------------------------------------------

compar_zona <- left_join(df_zonas, df_votacao_zonas, 
                           by=c("SG_UF", "CD_MUNICIPIO", "NR_ZONA","NR_TURNO")) |> setDT()
head(compar_zona)


compar_zona[, comparecimento := QT_VOTOS_NOMINAIS / QT_ELEITOR_SECAO]


summary(compar_zona$comparecimento)
#'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#' 0.5180  0.7285  0.7582  0.7568  0.7897  0.8897 


# add education info
compar_zona_edu <- left_join(compar_zona, df_zona_educacao, by = c("SG_UF", "CD_MUNICIPIO", "NR_ZONA"))
compar_muni_edu <- left_join(compar_muni, df_muni_educacao, by = c("SG_UF", "CD_MUNICIPIO"))




#### save outputs --------------------------------------------------
dir.create(path = './data')

# comparecimento municipios
fwrite(compar_muni_edu, './data/comparecimento_munis_2018.csv')
fwrite(compar_zona_edu, './data/comparecimento_zonas_2018.csv')

