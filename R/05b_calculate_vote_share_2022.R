library(data.table)
library(vroom)
library(tictoc)


# adicionar votação por candidato ----------------------------------------------
dir_urnas <- '../../data_raw/urnas_2022'

# UFs
my_uf <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG", "MS", "MT",
           "PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")

# listas de resultados
lista_T1 <- list()
lista_T2 <- list()

for(i in 1:27){ # i <- 1
tictoc::tic()
 
 # caminhos dos CSVs nos Zips
 temp_dir_1 <- tempdir()
 path_zip_T1 <- paste0(dir_urnas,"/urnas_", my_uf[i], "_2022_1T.zip")
 files <- unzip(path_zip_T1, list=T)$Name
 path_csv_T1  <- files[files %like% '.csv']
 unzip(path_zip_T1, files  = path_csv_T1, exdir = temp_dir_1)
 path_csv_T1 <- paste0(temp_dir_1,'/',path_csv_T1)
  
 
 temp_dir_2 <- tempdir()
 path_zip_T2 <- paste0(dir_urnas,"/urnas_", my_uf[i], "_2022_2T.zip")
 files <- unzip(path_zip_T2, list=T)$Name
 path_csv_T2  <- files[files %like% '.csv']
 unzip(path_zip_T2, files  = path_csv_T2, exdir = temp_dir_2)
 path_csv_T2 <- paste0(temp_dir_2,'/',path_csv_T2)
 
 # ler dados das urnas
 urnas_T1 <- fread( path_csv_T1 , sep = ";", encoding = "Latin-1")
 urnas_T2 <- fread( path_csv_T2, sep = ";", encoding = "Latin-1")

 # remove temp files
 unlink(temp_dir_1, recursive = T)
 unlink(temp_dir_2, recursive = T)
 rm(list=setdiff(ls(), c("urnas_T1", "urnas_T2")))
 gc()
 
 # filtrar para eleicao presidencial apenas
 urnas_T1 <- setDT(subset(urnas_T1, DS_CARGO_PERGUNTA=="Presidente"))
 urnas_T2 <- setDT(subset(urnas_T2, DS_CARGO_PERGUNTA=="Presidente"))
 
 # criar id_secao
 urnas_T1[, id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
 urnas_T2[, id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
 
 # votos por candidato
 urnas_T1[, votos_lula := fifelse(NM_VOTAVEL=="LULA", QT_VOTOS, 0)]
 urnas_T1[, votos_jair := fifelse(NM_VOTAVEL=="JAIR BOLSONARO", QT_VOTOS, 0)]
 urnas_T1[, votos_nulo := fifelse(NM_VOTAVEL=="Nulo", QT_VOTOS, 0)]
 urnas_T1[, votos_branco := fifelse(NM_VOTAVEL=="Branco", QT_VOTOS, 0)]
 urnas_T1[, votos_validos := fifelse(NM_VOTAVEL != "Branco" & NM_VOTAVEL != "Nulo", QT_VOTOS, 0)]
 
 urnas_T2[, votos_lula := fifelse(NM_VOTAVEL=="LULA", QT_VOTOS, 0)]
 urnas_T2[, votos_jair := fifelse(NM_VOTAVEL=="JAIR BOLSONARO", QT_VOTOS, 0)]
 urnas_T2[, votos_nulo := fifelse(NM_VOTAVEL=="Nulo", QT_VOTOS, 0)]
 urnas_T2[, votos_branco := fifelse(NM_VOTAVEL=="Branco", QT_VOTOS, 0)]
 urnas_T2[, votos_validos := fifelse(NM_VOTAVEL != "Branco" & NM_VOTAVEL != "Nulo", QT_VOTOS, 0)]
 
 # agregar por secao
 votos_T1 <- urnas_T1[, .(votos_lula = sum(votos_lula),
                          votos_jair = sum(votos_jair),
                          votos_nulo = sum(votos_nulo),
                          votos_branco = sum(votos_branco),
                          votos_validos = sum(votos_validos),
                          votos_total = sum(QT_VOTOS)
                          # QT_APTOS = QT_APTOS[1L],
                          # QT_ABSTENCOES = QT_ABSTENCOES[1L],
                          # QT_COMPARECIMENTO = QT_COMPARECIMENTO[1L]
                          ), 
                      by = .(id_secao)]
 
 votos_T2 <- urnas_T2[, .(votos_lula = sum(votos_lula),
                          votos_jair = sum(votos_jair),
                          votos_nulo = sum(votos_nulo),
                          votos_branco = sum(votos_branco),
                          votos_validos = sum(votos_validos),
                          votos_total = sum(QT_VOTOS)
                          # QT_APTOS = QT_APTOS[1L],
                          # QT_ABSTENCOES = QT_ABSTENCOES[1L],
                          # QT_COMPARECIMENTO = QT_COMPARECIMENTO[1L]
                          ),
                      by = .(id_secao)]
 

 lista_T1[[i]] <- votos_T1
 lista_T2[[i]] <- votos_T2
 
 et <- tictoc::toc(quiet = TRUE)
 
 cat(my_uf[i], " ", et$callback_msg, " \n")
}


votos_T1 <- data.table::rbindlist(lista_T1)
votos_T2 <- data.table::rbindlist(lista_T2)

# save to data dir
dir.create('../../data/votes_2022')
fwrite(votos_T1, '../../data/votes_2022/votos_T1.csv')
fwrite(votos_T2, '../../data/votes_2022/votos_T2.csv')





# aptos
aptos <- sum(votos_T2$QT_APTOS)
587765

# comparecimento
comparecimento <- sum(votos_T2$QT_COMPARECIMENTO)
420760



# validos
validos <- sum(votos_T2$votos_validos)
409316

# branco
branco <- sum(votos_T2$votos_branco)
4722

# nulo
nulo <- sum(votos_T2$votos_nulo)
6722

# total
total <- sum(votos_T2$votos_total)
420760


# check 1
total == comparecimento

# check 2
validos == total - (nulo + branco)



