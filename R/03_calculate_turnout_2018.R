library(data.table)
library(dplyr)


#### secoes --------------------------------------------------
#' quantidade de votos nominais e validos

# unzip data
temp_dir_1 <- tempdir()
all_zip_files <- list.files("../../data_raw/secoes", full.names = T, pattern = '.zip')
temp_zip <- all_zip_files[all_zip_files %like% 2018]
files <- unzip(temp_zip, list=T)$Name
path_csv_T1  <- files[files %like% '.csv']
path_csv_T1 <- path_csv_T1[path_csv_T1 %like% '2018_BRASIL.csv']
unzip(temp_zip, files  = path_csv_T1, exdir = temp_dir_1)
path_csv <- paste0(temp_dir_1,'/',path_csv_T1)


# read
secao <- fread(path_csv,
               # nrows = Inf, 
               encoding = "Latin-1")

# remove temp files
unlink(temp_dir_1, recursive = T)
gc()


# manter apenas votos para presidente no BR
secao_br <- subset(secao, DS_CARGO == 'Presidente' & SG_UF != 'ZZ')

# calcular comparecimento e abstenção
secao_br[, comparecimento_2018 := QT_COMPARECIMENTO / QT_APTOS]
secao_br[, abstencao_2018 := QT_ABSTENCOES / QT_APTOS]

# incluir 2º turno governador
gov <- subset(secao, DS_CARGO == 'GOVERNADOR' & SG_UF != 'ZZ' & NR_TURNO==2)
gov$gov_2t <- 1
gov <- gov[, .(gov_2t = mean(gov_2t)), by = SG_UF]
secao_br <- merge(secao_br, gov, by ="SG_UF", all.x = T)
secao_br$gov_2t[is.na(secao_br$gov_2t)] <- 0
secao_br$gov_2t <- ifelse(secao_br$NR_TURNO==2, secao_br$gov_2t, 0)




#### save outputs --------------------------------------------------
dir.create(path = '../../data/secoes')

# comparecimento municipios
fwrite(secao_br, '../../data/secoes/secoes_2018.csv')

