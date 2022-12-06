# spatial is special

library(sf)
library(data.table)
library(sfheaders)
library(mapview)
library(geodist)
library(pbapply)
library(dplyr)
library(furrr)
library(future)
library(geobr)
library(tictoc)
library(geosphere)

options(scipen = 999)


# distance to closest electoral section
# number of other sections within a 1, 3, 5 Km radius



#### read electoral sections data --------------------------------------------------

# unzip data
temp_dir_1 <- tempdir()
all_zip_files <- list.files("../../data_raw/zonas", full.names = T, pattern = '.zip')
temp_zip <- all_zip_files[all_zip_files %like% 2022]
files <- unzip(temp_zip, list=T)$Name
path_csv_T1  <- files[files %like% '.csv']
unzip(temp_zip, files  = path_csv_T1, exdir = temp_dir_1)
path_csv <- paste0(temp_dir_1,'/',path_csv_T1)

# read data
zonas <- fread( path_csv)

# remove temp files
file.remove(path_csv)
gc()

# remove zonas fora do Brasil
zonas <- subset(zonas, SG_UF != 'ZZ')

# mantem apenas zonas que tiveram eleitores vontando pra federal no 2nd turno
zonas <- subset(zonas, NR_TURNO == 2)

# create section id
zonas[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]

# contar quantidade de eleitores em cada zona  de cada municipio
df_secoes <- zonas[, .(QT_ELEITOR_ELEICAO_FEDERAL = sum(QT_ELEITOR_ELEICAO_FEDERAL)), 
                   by = .(id_secao, SG_UF, CD_MUNICIPIO, 
                          NM_MUNICIPIO, NM_LOCAL_VOTACAO, DS_ENDERECO, NM_BAIRRO, NR_CEP,
                          NR_ZONA, NR_SECAO, NR_LOCAL_VOTACAO,
                          NR_TURNO, NR_LONGITUDE, NR_LATITUDE)]


# calculate number of zones and electoral sections per municipality
df_secoes[, number_of_zones := length(unique(NR_ZONA)), by= .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO)]
df_secoes[, number_of_sections := length(unique(NR_SECAO)), by= .(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO)]







#### Fix problematic coordinates ------------------------------------------------
geocode <- fread('../../data_raw/spatial/geolocal_11out.csv')
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
# 1: FALSE 378.479
# 2:  TRUE 116.180

geocode |> count(is.na(tse_lat))
# 1: FALSE 374.054
# 2:  TRUE 115.280

geocode[is.na(lat)] |> nrow()
# 30.278

df_secoes[is.na(lat)] |> nrow()
# 35.603

temp_fixed <- rbind(df_secoes_good, df_secoes_bad )
temp_fixed[is.na(lat) | is.na(lon)] |> nrow()
#> 32.386
# 6% sem recuperacao





############### get geocode from capivara ------------------

# sections not geocoded in the 1st round
secoes_capivara <- temp_fixed[is.na(lat)]
summary(secoes_capivara$lat)

# get data from Marcelo Oliveira
capivara <- fread('../../data_raw/spatial/geocoded_voting_places_001.csv')

# merge
secoes_capivara[capivara, on=c('CD_MUNICIPIO', 'NR_ZONA', 'NR_LOCAL_VOTACAO'), c('lat', 'lon') := list(i.NR_LATITUDE, i.NR_LONGITUDE)]
summary(secoes_capivara$lat)

# 666666666666
df_secoes_fixed <- rbind( rbind(temp_fixed[!is.na(lat) ], secoes_capivara ) )
df_secoes_fixed[is.na(lat) | is.na(lon)] |> nrow()
#> 0

nrow(df_secoes_fixed) == nrow(df_secoes)
#> TRUE

summary(df_secoes_fixed$lat)
#>   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> -33.73  -23.34  -19.87  -16.72   -8.71   38.31 


# compare geocode results-----------------------------------------
# library(data.table)
# library(mapview)
# library(leafgl)
# library(sf)
# mapviewOptions(platform = 'leafgl')
# 
# 
# 
# 
# # calculate distances
# df_secoes_fixed[ , dist_error := geosphere::distGeo(matrix(c(lon, lat), ncol= 2), 
#                                                     matrix(c(lon2, lat2), ncol = 2))]
# 
# summary(df_secoes_fixed$dist_error)
# 
# sum(df_secoes_fixed$dist_error > 1000, na.rm=T) / nrow(df_secoes_fixed)

# a <- df_secoes_fixed[ dist_error >10000]
# a2 <- subset(a, NM_MUNICIPIO=='POUSO ALEGRE')
# 
# t2 <- sfheaders::sf_point(a2, 'lon2', 'lat2', keep = T)
# t3 <- sfheaders::sf_point(a2, 'lon', 'lat', keep = T) # 1
# st_crs(t2) <- 4674
# st_crs(t3) <- 4674
# 
# mapview(  t2) + t3










# clean memory
rm(list=setdiff(ls(), "df_secoes_fixed"))
gc()


############### distance matrix data.table ------------------

# get unique voting places
voting_sections <- df_secoes_fixed[, .(id_secao, lat, lon)]


# support functions
# https://stackoverflow.com/questions/36817423/how-to-efficiently-calculate-distance-between-pair-of-coordinates-using-data-tab
   # library(Rcpp)
   # Rcpp::sourceCpp("distance_calcs.cpp", rebuild = F)

dt.haversine <- function(lat_from, lon_from, lat_to, lon_to, r = 6378137){
 radians <- pi/180
 lat_to <- lat_to * radians
 lat_from <- lat_from * radians
 lon_to <- lon_to * radians
 lon_from <- lon_from * radians
 dLat <- (lat_to - lat_from)
 dLon <- (lon_to - lon_from)
 a <- (sin(dLat/2)^2) + (cos(lat_from) * cos(lat_to)) * (sin(dLon/2)^2)
 return(2 * atan2(sqrt(a), sqrt(1 - a)) * r)
}

# function to calculate spatial info
dist_dt <- function(id) { # id = voting_sections$id_secao[1]

# select place of origin
o <- voting_sections[ id_secao ==id, ]

# pair it with all other voting places into a data.table
temp_df <- cbind(o, voting_sections)
names(temp_df)[4:6] <- paste0(names(temp_df)[4:6], '_dest')
head(temp_df)

# temp_df[, dist := geosphere::distGeo(matrix(c(lon, lat), ncol= 2),
#                                matrix(c(lon_dest, lat_dest), ncol = 2))]

 # tic()
 temp_df[, dist := dt.haversine(lat, lon, lat_dest, lon_dest)]
 # toc()
 # temp_df[, dist := rcpp_distance_haversine(lat, lon, 
 #                                            lat_dest, lon_dest, 
 #                                           tolerance = 100000.0)]

 # # get values
 # closest_dist_any <- min(temp_df$dist)
 # closest_dist <- min(temp_df$dist[temp_df$dist > 0])
 # num_0500  <- temp_df[dist < 0500, ] |> nrow()
 # num_1000  <- temp_df[dist < 1000, ] |> nrow()
 # num_3000  <- temp_df[dist < 3000, ] |> nrow()
 # num_5000  <- temp_df[dist < 5000, ] |> nrow()
 # num_10000 <- temp_df[dist < 10000, ] |> nrow()
 # 
 # closest_dist_any  <-  min(temp_df$dist)
 # closest_dist <-  min(temp_df$dist[temp_df$dist > 0])
 # num_0500 <- sum(temp_df$dist < 500)
 # num_1000 <- sum(temp_df$dist < 1000)
 # num_3000 <- sum(temp_df$dist < 3000)
 # num_5000 <- sum(temp_df$dist < 5000)
 # num_10000 <- sum(temp_df$dist < 10000)
 # 
 # 
 # # build output
 # output <- data.frame(id = id,
 #                       closest_dist_any = closest_dist_any,
 #                       closest_dist = closest_dist,
 #                       num_0500 = num_0500,
 #                       num_1000 = num_1000,
 #                       num_3000 = num_3000,
 #                       num_5000 = num_5000,
 #                       num_10000 = num_10000)
 
 output <-  temp_df[, .(closest_dist_any = min(dist),
                        closest_dist = min(dist[dist > 0]),
                        num_0500  = sum(dist < 0500),
                        num_1000  = sum(dist < 1000),
                        num_3000  = sum(dist < 3000),
                        num_5000  = sum(dist < 5000),
                        num_10000 = sum(dist < 10000))]

 output$id_secao <- id
  
 return(output)
 }

# test
system.time( dist_dt(voting_sections$id_secao[3]) )

# parallel
tictoc::tic()
plan(multisession)

df2 <- furrr::future_map(.x = voting_sections$id_secao,
                         .f = dist_dt,
                         .progress = TRUE) |> rbindlist()

#head(df2)
tictoc::toc()
#> 29.02 sec | 0100 points | geodist parallel
#> 09.92 sec | 0100 points | dt.haversine parallel
#> 29.86 sec | 0500 points | dt.haversine parallel
#> 56.35 sec | 1000 points | dt.haversine parallel
#> 15884 sec | ALL points | dt.haversine parallel
#> error sec | 0100 points | rcpp_distance_haversine parallel
#> error sec | 0500 points | rcpp_distance_haversine parallel



#' #### distance matrix original --------------------------------------------------
#' 
#' 
#' # convert to spatial data
#' sf <- sfheaders::sf_point(df_secoes_fixed, x = 'lon', y='lat', keep = T)
#' 
#' # projection
#' st_crs(sf) <- 4674
#' # mapview(sf)
#' 
#' head(sf)
#' nrow(sf)
#' 
#' 
#' # get coordinates
#' coords <- st_coordinates(sf)
#' 
#' 
#' # function to calculate spatial info
#' get_dist_info <- function(i){  # i = 1   i = 7286
#'  
#'  # calculates distance matrix in meters using euclidean distance
#'  # tic()
#'  # dist_matrix <- st_distance(sf[i,], sf)
#'  # toc()
#'  
#'  
#'  # fast parallel distances many to many
#'  # tic()
#'  dist_matrix <- suppressMessages(geodist::geodist(coords[i,], coords, measure = "cheap"))
#'  # toc()
#'  
#'  
#'  
#'  
#'  ################# distance to closest electoral section
#'  #' https://stackoverflow.com/questions/21977720/r-finding-closest-neighboring-point-and-number-of-neighbors-within-a-given-rad
#'  
#'  
#'  # Calculate nearest distance
#'  #    tic()
#'  #    closest_dist <- apply(dist_matrix, 1, function(x) {
#'  #   return(sort(x, partial = 2)[2])
#'  # })
#'  # toc()
#'  # 
#'  # 
#'  # tic()
#'  # f <- function(v){min(v[v > 0])}
#'  # closest_dist <- apply(dist_matrix, 1, FUN=f)
#'  # toc()
#'  
#'  closest_dist_any <- min(dist_matrix)
#'  closest_dist <- min(dist_matrix[dist_matrix > 0])
#'  
#'  ################# number of other sections within a 1, 3, 5 Km radius
#'  
#'  # Subtract 1 to exclude the point itself
#'  num_0500 <- sum(dist_matrix < 500)
#'  
#'  num_1000 <- sum(dist_matrix < 1000)
#'  
#'  num_3000 <- sum(dist_matrix < 3000)
#'  
#'  num_5000 <- sum(dist_matrix < 5000)
#'  
#'  num_10000 <- sum(dist_matrix < 10000)
#'  
#'  temp_df <- data.frame(i = i,
#'                        closest_dist_any = closest_dist_any,
#'                        closest_dist = closest_dist,
#'                        num_0500 = num_0500,
#'                        num_1000 = num_1000,
#'                        num_3000 = num_3000,
#'                        num_5000 = num_5000,
#'                        num_10000 = num_10000)
#'  
#'  return(temp_df)
#' 
#' }
#' 
#' 
#' # apply function
#' # single thread
#' # df <- pblapply(X= 1:nrow(sf), FUN=get_dist_info) |> rbindlist()
#' 
#' tictoc::tic()
#' # parallel
#' plan(multisession)
#' 
#' df1 <- furrr::future_map(.x = 1:1000, 
#'                          .f = get_dist_info, 
#'                          .progress = TRUE) |> rbindlist()
#' 
#' # head(df1)
#' tictoc::toc()
#' #> 08.34 sec | 0100 points
#' #> 31.24 sec | 0500 points
#' #> 62.94 sec | 1000 points
#' #> 51357.86 sec | ALL points
#' 
#' 
#' 
#' 
#' 
#' # # bring back the info on electoral sections id
#' # sf$i <- 1:nrow(sf)
#' # sf2 <- dplyr::left_join(sf, df2)
#' # df3 <- sfheaders::sf_to_df(sf2, fill = TRUE)
#' # head(df3)
#' # 
#' # 
#' # summary(df3$closest_dist_any)
#' # summary(df3$closest_dist)
#' # #> Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#' # #>    0     215     436    1268     816 5429502 
#' 
#' 


# bring back the info on electoral sections id
df3 <- dplyr::left_join(df_secoes_fixed, df2, by = "id_secao")
head(df3)
summary(df3$closest_dist)
#> Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#>    0      192      396     1171      740  5339190 

summary(df3$num_1000)
#>  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 1.00   15.00   34.00   45.12   65.00  313.00 


# save output --------------------------------------------------
dir.create('../../data/spatial')
fwrite(df3, '../../data/spatial/electoral_sections_spatial.csv')






# calculate distance to city center --------------------------------------------------

# get data of city centers
city_centers_sf <- geobr::read_municipal_seat(year = 2010)
city_centers_df <- sfheaders::sf_to_df(city_centers_sf, fill = TRUE)

# zones data
df_sections <- fread('../../data/spatial/electoral_sections_spatial.csv')

# correspondence table of municipality codes
rosetta <-  fread('../../data_raw/br_bd_diretorios_brasil_municipio.csv', encoding = "UTF-8")
head(rosetta)


# # bring IBGE code_muni
df_sections[rosetta, on = c('CD_MUNICIPIO'='id_municipio_tse'), code_muni := i.id_municipio]
head(df_sections)
summary(df_sections)

# bring sede muni coordinate
df_sections[city_centers_df, on = 'code_muni', c('x_sede', 'y_sede') := list(i.x, i.y)]
summary(df_sections$code_muni)


# calculate distances
# df_sections[ , dist_sede := geosphere::distGeo(matrix(c(lon, lat), ncol= 2), 
#                                        matrix(c(x_sede, y_sede), ncol = 2))]

df_sections[, dist_sede := dt.haversine(lat, lon, y_sede, x_sede)]

head(df_sections)

summary(df_sections$dist_sede)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>       3     853    2699    7625    8214 6293041     180

summary(df_sections$num_1000)












##### determine section in urban and rural areas -------------------------------

# library(data.table)
# library(mapview)
# library(leafgl)
# library(sf)
# mapviewOptions(platform = 'leafgl')
# 
# 
# read data
tracts <- geobr::read_census_tract(code_tract = 'all', simplified = FALSE, year=2010)
tracts <- dplyr::select(tracts, 'code_tract',  'zone', 'code_muni', 'name_muni')
tracts_urban <- subset(tracts, zone == 'URBANO')
gc()


# sections
df_sections[, id_local_votacao := paste(CD_MUNICIPIO, NR_ZONA, NR_LOCAL_VOTACAO)]

df_place <- df_sections[, .(code_muni, id_local_votacao, lon, lat)] 
df_place <- unique(df_place)

sf_place <- sfheaders::sf_point(df_place, x='lon', y='lat', keep = T)
st_crs(sf_place) <- 4674



get_urban <- function(c_muni){ # c_muni <- df$code_muni[1]
 
 # subset
 temp_tracts <- subset(tracts_urban ,  code_muni == c_muni)
 temp_tracts <- sf::st_make_valid(temp_tracts)
 temp_sf <- subset(sf_place ,  code_muni == c_muni)
 
 inter_sf <- st_join(temp_sf, temp_tracts)
 inter_df <- sfheaders::sf_to_df(inter_sf, fill = T)
 return(inter_df)
 }

tictoc::tic()
# parallel
plan(multisession)

sf_place_urban <- furrr::future_map(.x = unique(sf_place$code_muni), 
                         .f = get_urban, 
                         .progress = TRUE) |> rbindlist()

tictoc::toc()
#> 154.27 sec elapsed


# Format data
head(sf_place_urban)

sf_place_urban[, zone := fifelse(zone=='URBANO', 'urban', 'rural', na='rural')]
table(sf_place_urban$zone)

# bring zone info to sections
df_sections[sf_place_urban, on='id_local_votacao', zone := i.zone]
head(df_sections)




# save output --------------------------------------------------

fwrite(df_sections, '../../data/spatial/electoral_sections_spatial.csv')

hist(df_sections$closest_dist)
hist(df_sections$dist_sede)
hist(df_sections$num_0500)
hist(df_sections$num_1000)
hist(df_sections$num_3000)
hist(df_sections$num_5000)
hist(df_sections$num_10000)


summary(df_sections$closest_dist)
summary(df_sections$dist_sede)
summary(df_sections$num_0500)
summary(df_sections$num_1000)
summary(df_sections$num_3000)
summary(df_sections$num_5000)
summary(df_sections$num_10000)
summary(df_sections$lat)

table(df_sections$zone, useNA = 'always')
#>  rural  urban   <NA> 
#>  57011 437648      0 

