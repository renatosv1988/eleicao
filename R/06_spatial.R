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


# read data
files_zonas <- list.files(path = '../../data_raw/zonas', pattern = '.csv', full.names = T)
zonas <- fread( files_zonas[ files_zonas%like% 2022], nrows = Inf)

# remove zonas fora do Brasil
zonas <- subset(zonas, SG_UF != 'ZZ')

# mantem apenas zonas que tiveram eleitores vontando pra federal no 2nd turno
zonas <- subset(zonas, NR_TURNO == 2)


# contar quantidade de eleitores em cada zona  de cada municipio
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


#### Fix problematic coordinates ------------------------------------------------
geocode <- fread('../../data_raw/geolocal_11out.csv')
head(geocode)

#' tse_ original
#' comp_tse (meio inutil em 2022)  pode ignorar
#' inep_ (fuzzy mathcing en nonme da secao e nome do censo escolar)
#' google_ geocode do api do google do endereco da secao
#' local_ usando uma base de dados local
#' place_ geocode do api do google nome da secao (place name)
#' google_aprox_ google apli com baixa precisao 
#' ibge_approx_ centroide da faces de quadra



summary(geocode$tse_lat)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>  -33.73  -23.52  -20.48  -16.45   -7.55    5.08  115280 


geocode[, lat := fifelse(!is.na(tse_lat), tse_lat,
                 fifelse(is.na(tse_lat) & !is.na(comp_tse_lat), comp_tse_lat,     
                 fifelse(is.na(tse_lat) & is.na(comp_tse_lat) & !is.na(inep_lat), inep_lat,     
                 fifelse(is.na(tse_lat) & is.na(comp_tse_lat) & is.na(inep_lat) & !is.na(google_lat), google_lat,      
                 fifelse(is.na(tse_lat) & is.na(comp_tse_lat) & is.na(inep_lat) & is.na(google_lat) & !is.na(local_lat), local_lat,
                 fifelse(is.na(tse_lat) & is.na(comp_tse_lat) & is.na(inep_lat) & is.na(google_lat) & is.na(local_lat) & !is.na(places_lat), places_lat,
                 fifelse(is.na(tse_lat) & is.na(comp_tse_lat) & is.na(inep_lat) & is.na(google_lat) & is.na(local_lat) & is.na(places_lat) & !is.na(google_approx_lat), google_approx_lat,
                 fifelse(is.na(tse_lat) & is.na(comp_tse_lat) & is.na(inep_lat) & is.na(google_lat) & is.na(local_lat) & is.na(places_lat) & is.na(google_approx_lat) & !is.na(ibge_approx_lat), ibge_approx_lat, -666
                         ))))))))]

geocode[, lon := fifelse(!is.na(tse_lon), tse_lon,
                 fifelse(is.na(tse_lon) & !is.na(comp_tse_lon), comp_tse_lon,     
                 fifelse(is.na(tse_lon) & is.na(comp_tse_lon) & !is.na(inep_lon), inep_lon,     
                 fifelse(is.na(tse_lon) & is.na(comp_tse_lon) & is.na(inep_lon) & !is.na(google_lon), google_lon,      
                 fifelse(is.na(tse_lon) & is.na(comp_tse_lon) & is.na(inep_lon) & is.na(google_lon) & !is.na(local_lon), local_lon,
                 fifelse(is.na(tse_lon) & is.na(comp_tse_lon) & is.na(inep_lon) & is.na(google_lon) & is.na(local_lon) & !is.na(places_lon), places_lon,
                 fifelse(is.na(tse_lon) & is.na(comp_tse_lon) & is.na(inep_lon) & is.na(google_lon) & is.na(local_lon) & is.na(places_lon) & !is.na(google_approx_lon), google_approx_lon,
                 fifelse(is.na(tse_lon) & is.na(comp_tse_lon) & is.na(inep_lon) & is.na(google_lon) & is.na(local_lon) & is.na(places_lon) & is.na(google_approx_lon) & !is.na(ibge_approx_lon), ibge_approx_lon, -666
                         ))))))))]

geocode[lat==-666, lat :=  NA]
geocode[lon==-666, lon :=  NA]
summary(geocode$lat)
summary(geocode$lon)




# keep original TSE data
df_secoes_good <- subset(df_secoes, NR_LONGITUDE!=-1 & NR_LATITUDE!=-1)
df_secoes_good[, c('lat', 'lon') := list(NR_LATITUDE, NR_LONGITUDE) ]

# fix missing data
df_secoes_bad <- subset(df_secoes, NR_LONGITUDE==-1 & NR_LATITUDE==-1)

df_secoes_bad[geocode, on=c('SG_UF'='uf',
                            'NR_ZONA'='zona',
                            'NR_SECAO'='secao'), c('lat', 'lon') := list(i.lat, i.lon)]
df_secoes[geocode, on=c('SG_UF'='uf',
                        'NR_ZONA'='zona',
                        'NR_SECAO'='secao'), c('lat', 'lon') := list(i.lat, i.lon)]

summary(df_secoes_bad$lat)
summary(df_secoes_bad$lon)


# case check
subset(df_secoes, SG_UF=='MG' & NR_ZONA==87 & NR_SECAO==183)
subset(geocode,  uf=='MG' & zona==87 & secao==183)
subset(df_secoes_bad, SG_UF=='MG' & NR_ZONA==87 & NR_SECAO==183)


# check number of missing and recovered cases

df_secoes |> count(is.na(NR_LATITUDE) | NR_LATITUDE ==-1 )
# 1: FALSE 378.655
# 2:  TRUE 116.004

geocode |> count(is.na(tse_lat))
# 1: FALSE 374.054
# 2:  TRUE 115.280

geocode[is.na(lat)] |> nrow()
# 30.278

df_secoes[is.na(lat)] |> nrow()
# 35.603

df_secoes_fixed <- rbind(df_secoes_good, df_secoes_bad )
df_secoes_fixed[is.na(lat)] |> nrow()
#> 32.386
# 6% sem recuperacao

# remove missing values
df_secoes_fixed <- subset(df_secoes_fixed, ! is.na(lat))


# clean memory
rm(df_secoes_good, df_secoes_bad, zonas, df_secoes, geocode, files_zonas)
gc()


# convert to spatial data
sf <- sfheaders::sf_point(df_secoes_fixed, x = 'lon', y='lat', keep = T)

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
                       num_10000 = num_10000[1])
 
 return(temp_df)

}


# apply function
# single thread
# df <- pblapply(X= 1:nrow(sf), FUN=get_dist_info) |> rbindlist()

tictoc::tic()
# parallel
cores <- data.table::getDTthreads() 
plan(multisession, workers = cores)

df2 <- furrr::future_map(.x = 1:nrow(sf), 
                         .f = get_dist_info, 
                         .progress = TRUE) |> rbindlist()

head(df2)
tictoc::toc()
#> 51357.86 sec elapsed


# bring back the info on electoral sections id
sf$i <- 1:nrow(sf)
sf2 <- dplyr::left_join(sf, df2)
df3 <- sfheaders::sf_to_df(sf2, fill = TRUE)
head(df3)


summary(df3$closest_dist_any)
summary(df3$closest_dist)
#> Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>    0     215     436    1268     816 5429502 

# save output --------------------------------------------------
dir.create('../../data/spatial')
fwrite(df3, '../../data/spatial/electoral_sections_spatial.csv')



# calculate distance to city center --------------------------------------------------

# get data of city centers
city_centers_sf <- geobr::read_municipal_seat(year = 2010)
city_centers_df <- sfheaders::sf_to_df(city_centers_sf, fill = TRUE)

# zones data
df3 <- fread('../../data/spatial/electoral_sections_spatial.csv')

# correspondence table of municipality codes
rosetta <-  fread('../../data_raw/br_bd_diretorios_brasil_municipio.csv', encoding = "UTF-8")
head(rosetta)


# bring IBGE code_muni
df3[rosetta, on = c('CD_MUNICIPIO'='id_municipio_tse'), code_muni := i.id_municipio]
head(df3)

# bring sede muni coordinate
df3[city_centers_df, on = 'code_muni', c('x_sede', 'y_sede') := list(i.x, i.y)]
head(df3)

summary(df3$x)
summary(df3$x_sede)

# calculate distances
df3[ , dist_sede := geosphere::distGeo(matrix(c(x, y), ncol= 2), 
                                       matrix(c(x_sede, y_sede), ncol = 2))]


head(df3)

summary(df3$dist_sede)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>       3     885    2779    6814    8298 6277098     114 


# save output --------------------------------------------------

fwrite(df3, '../../data/spatial/electoral_sections_spatial.csv')


hist(df3$closest_dist)
hist(df3$dist_sede)
hist(df3$num_0500)
hist(df3$num_1000)
hist(df3$num_3000)
hist(df3$num_5000)
hist(df3$num_10000)


summary(df3$closest_dist)
summary(df3$dist_sede)
summary(df3$num_0500)
summary(df3$num_1000)
summary(df3$num_3000)
summary(df3$num_5000)
summary(df3$num_10000)

