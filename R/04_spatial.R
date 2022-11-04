# spatial is special

library(sf)
library(data.table)
library(sfheaders)
library(mapview)
library(geodist)
library(pbapply)
library(furrr)
library(future)
library(geobr)
library(geosphere)

options(scipen = 999)


# distance to closest electoral section
# number of other sections within a 1, 3, 5 Km radius



#### read electoral sections data --------------------------------------------------

# base dos dados
# qual coluna de lat lon usar?????????????????????????????????????????????????????
# aa <- fread('./data_raw/geolocal_11out.csv', nrow=100)




# read data
files_zonas <- list.files(path = './data_raw/zonas', pattern = '.csv', full.names = T)
zonas <- fread( files_zonas, nrows = Inf)

# remove zonas fora do Brasil
zonas <- subset(zonas, SG_UF != 'ZZ')

# mantem apenas zonas que tiveram eleitores vontando pra federal no 2nd turno
zonas <- subset(zonas, QT_ELEITOR_ELEICAO_FEDERAL > 0)
zonas <- subset(zonas, NR_TURNO == 2)


# contar quantidade de eleitores por grau de escolaridade em cada zona  de cada municipio
df_secoes <- zonas[, .(QT_ELEITOR_ELEICAO_FEDERAL = sum(QT_ELEITOR_ELEICAO_FEDERAL)), 
                  by = .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NR_SECAO, 
                         NR_TURNO, NR_LONGITUDE, NR_LATITUDE)]

# df_secoes <- subset(df_secoes, CD_MUNICIPIO == 93360)


# calculate number of zones and electoral sections per municipality
df_secoes[, number_of_zones := length(unique(NR_ZONA)), by= .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO)]
df_secoes[, number_of_sections := length(unique(NR_SECAO)), by= .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO)]




                  # # function to identify the appropriate EPSG code for the utm projection of each section
                  # # Universal Transverse Mercator (UTM)
                  # # https://bookdown.org/robinlovelace/geocompr/reproj-geo-data.html
                  # 
                  # lonlat2UTM = function(lonlat) {
                  #   utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
                  #   if(lonlat[2] > 0) {
                  #     utm + 32600
                  #   } else{
                  #     utm + 32700
                  #   }
                  # }
                  # 
                  # # find EPSG utm
                  # df_secoes[, utm := lonlat2UTM(c(NR_LONGITUDE, NR_LATITUDE)) ]
                  # head(df_secoes)
                  # unique(df_secoes$utm)


# df_secoes2 <- subset(df_secoes, CD_MUNICIPIO == 93360)


# convert to spatial data
sf <- sfheaders::sf_point(df_secoes, x = 'NR_LONGITUDE', y='NR_LATITUDE', keep = T)

# projection
st_crs(sf) <- 4674
# mapview(sf)

head(sf)
nrow(sf)


#### distance matrix --------------------------------------------------

# # reproject
# temp_utm <- sf$utm[1]
# sf_utm <- st_transform(sf, temp_utm)

# get coordinates
coords <- st_coordinates(sf)


# function to calculate spatial info --------------------------------------------------

get_dist_info <- function(i){  # i = 1   i = 7286
  
# calculates distance matrix in meters using euclidean distance
  # tic()
  # dist_matrix <- st_distance(sf[i,], sf)
  # toc()
  
  
  # fast parallel distances many to many
   # tic()
   dist_matrix <- suppressMessages(geodist::geodist(coords[i,], coords, measure = "cheap"))
   # toc()
   
   
   
   
################# distance to closest electoral section
#' https://stackoverflow.com/questions/21977720/r-finding-closest-neighboring-point-and-number-of-neighbors-within-a-given-rad


# Calculate nearest distance
  #    tic()
  #    closest_dist <- apply(dist_matrix, 1, function(x) {
  #   return(sort(x, partial = 2)[2])
  # })
  # toc()
  # 
  # 
  # tic()
  # f <- function(v){min(v[v > 0])}
  # closest_dist <- apply(dist_matrix, 1, FUN=f)
  # toc()

   closest_dist_any <- min(dist_matrix)
   closest_dist <- min(dist_matrix[dist_matrix > 0])

################# number of other sections within a 1, 3, 5 Km radius

# Subtract 1 to exclude the point itself
num_0500 <- apply(dist_matrix, 1, function(x) {
  sum(x < 500) - 1
})

num_1000 <- apply(dist_matrix, 1, function(x) {
  sum(x < 1000) - 1
})

num_3000 <- apply(dist_matrix, 1, function(x) {
  sum(x < 3000) - 1
})

num_5000 <- apply(dist_matrix, 1, function(x) {
  sum(x < 5000) - 1
})

num_10000 <- apply(dist_matrix, 1, function(x) {
  sum(x < 10000) - 1
})

temp_df <- data.frame(i = i,
                      closest_dist_any = closest_dist_any,
                      closest_dist = closest_dist,
                      num_0500 = num_0500[1],
                      num_1000 = num_1000[1],
                      num_3000 = num_3000[1],
                      num_5000 = num_5000[1],
                      num_10000 = num_10000[1]
)

return(temp_df)


}


# apply function
  # single thread
  # df <- pblapply(X= 1:nrow(sf), FUN=get_dist_info) |> rbindlist()

# parallel
  
  plan(multisession, workers = 4)
  
  df2 <- furrr::future_map(.x = 1:nrow(sf), 
                           .f = get_dist_info, 
                           .progress = TRUE) |> rbindlist()

  head(df2)

  # bring back the info on electoral sections id
  sf$i <- 1:nrow(sf)
  sf2 <- dplyr::left_join(sf, df2)
  df3 <- sfheaders::sf_to_df(sf2, fill = TRUE)
  head(df3)

  
    
# save output --------------------------------------------------

fwrite(df3, './data/electoral_sections_spatial.csv')
  
  
  
# calculate distance to city center --------------------------------------------------

 # get data of city centers
  city_centers_sf <- geobr::read_municipal_seat(year = 2010)
  city_centers_df <- sfheaders::sf_to_df(city_centers_sf, fill = TRUE)
  
  # zones data
  df3 <- fread('./data/electoral_sections_spatial.csv')

  # correspondence table of municipality codes
  rosetta <-  fread('./data_raw/br_bd_diretorios_brasil_municipio.csv', encoding = "UTF-8")
  head(rosetta)


  # bring IBGE code_muni
  df3[rosetta, on = c('CD_MUNICIPIO'='id_municipio_tse'), code_muni := i.id_municipio]
  head(df3)
  
  # bring sede muni coordinate
  df3[city_centers_df, on = 'code_muni', c('x_sede', 'y_sede') := list(i.x, i.y)]
  head(df3)
  
  # calculate distances
  df3[ , dist_sede := geosphere::distGeo(matrix(c(x, y), ncol= 2), 
                                  matrix(c(x_sede, y_sede), ncol = 2))]
  

head(df3)

# save output --------------------------------------------------

fwrite(df3, './data/electoral_sections_spatial.csv')

