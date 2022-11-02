# spatial is special

library(sf)
library(data.table)
library(sfheaders)
library(mapview)


#' uma mesma zona pode ter seções eleitorais 'NR_SECAO' afastas uma da outra. exmeplo de
#' Abadiania de goias
#' a gente consegue votos por seção?
  

#### zonas --------------------------------------------------


# read data
files_zonas <- list.files(path = './data_raw/zonas', pattern = '.csv', full.names = T)
zonas <- fread( files_zonas, nrows = Inf)

# remove zonas fora do Brasil
zonas <- subset(zonas, SG_UF != 'ZZ')

# mantem apenas zonas que tiveram eleitores vontando pra federal no 2nd turno
zonas <- subset(zonas, QT_ELEITOR_ELEICAO_FEDERAL > 0)
zonas <- subset(zonas, NR_TURNO == 2)


# contar quantidade de eleitores por grau de escolaridade em cada zona  de cada municipio
df_zonas <- zonas[, .(QT_ELEITOR_ELEICAO_FEDERAL = sum(QT_ELEITOR_ELEICAO_FEDERAL)), 
                  by = .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, 
                         NR_ZONA, NR_TURNO, NR_LONGITUDE, NR_LATITUDE)]

# calculate number of zone per municipality
df_zonas[, number_of_zones := .N, by= .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO)]

head(df_zonas)


df <- subset(df_zonas, CD_MUNICIPIO == 93360)


sf <- sfheaders::sf_point(df, x = 'NR_LONGITUDE', y='NR_LATITUDE', keep = T)

# projection
st_crs(sf) <- 4674


mapview(sf)








