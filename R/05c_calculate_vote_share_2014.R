library(data.table)
library(dplyr)


#### secoes --------------------------------------------------
#' quantidade de votos nominais e validos

# unzip data
temp_dir_1 <- tempdir()
all_zip_files <- list.files("../../data_raw/urnas_2014", full.names = T, pattern = '.zip')
temp_zip <- all_zip_files[all_zip_files %like% 2014]
files <- unzip(temp_zip, list=T)$Name
path_csv_T1  <- files[files %like% '.csv']
unzip(temp_zip, files  = path_csv_T1, exdir = temp_dir_1)
path_csv <- paste0(temp_dir_1,'/',path_csv_T1)


# read
urnas <- fread(path_csv,
               # nrows = Inf, 
               encoding = "Latin-1")

# remove temp files
unlink(temp_dir_1, recursive = T)
rm(list=setdiff(ls(), "urnas"))
gc()



# filtrar para eleicao presidencial apenas
urnas <- subset(urnas, DS_CARGO=="Presidente")


# criar id_secao
urnas[, id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]

unique(urnas$NM_VOTAVEL)
# votos por candidato
urnas[, votos_lula := fifelse(NM_VOTAVEL=="DILMA VANA ROUSSEFF", QT_VOTOS, 0)]
urnas[, votos_nulo := fifelse(NM_VOTAVEL=="VOTO NULO", QT_VOTOS, 0)]
urnas[, votos_branco := fifelse(NM_VOTAVEL=="VOTO BRANCO", QT_VOTOS, 0)]
urnas[, votos_validos := fifelse(NM_VOTAVEL != "VOTO BRANCO" & NM_VOTAVEL != "VOTO NULO", QT_VOTOS, 0)]


# agregar por secao
votos <- urnas[, .(votos_lula = sum(votos_lula),
                   votos_nulo = sum(votos_nulo),
                   votos_branco = sum(votos_branco),
                   votos_validos = sum(votos_validos),
                   votos_total = sum(QT_VOTOS)
                   ),
               by = .(ANO_ELEICAO, SG_UF, NM_MUNICIPIO, CD_MUNICIPIO, id_secao, NR_TURNO)]

head(votos)

table(votos$NR_TURNO)



#### save output --------------------------------------------------

dir.create('../../data/votes_2014')
fwrite(votos, '../../data/votes_2014/votos_T1_T2.csv')






# validos
validos <- sum(votos$votos_validos)


# branco
branco <- sum(votos$votos_branco)


# nulo
nulo <- sum(votos$votos_nulo)


# total
total <- sum(votos$votos_total)


# check
validos == total - (nulo + branco)





