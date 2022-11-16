library(data.table)
library(dplyr)


#### eleitorado --------------------------------------------------
#' perfil demografico secao

# download data
my_uf <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG", "MS", "MT",
           "PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")

# create list of results
AG <- list()
for(i in 1:27){ # i <- 1
 
 # read data
 DF <- fread(paste0("../../data_raw/eleitorado_secao/perfil_eleitor_secao_2022_",my_uf[i],".csv"))
 
 # recode
 DF[, mulher := fifelse(CD_GENERO %in% 4, QT_ELEITORES_PERFIL, 0)]
 DF[, educ_prim := fifelse(CD_GRAU_ESCOLARIDADE %in% 1:4, QT_ELEITORES_PERFIL, 0)]
 DF[, idade_16_17 := fifelse(CD_FAIXA_ETARIA %in% c(1600,1700), QT_ELEITORES_PERFIL, 0)]
 DF[, idade_18_24 := fifelse(CD_FAIXA_ETARIA %in% c(1800,1900,2000,2124),QT_ELEITORES_PERFIL, 0)]
 DF[, idade_60M := fifelse(CD_FAIXA_ETARIA %in% c(6064,6569,7074,7579,8084,8589,
                                                   9094,9599,9999), QT_ELEITORES_PERFIL, 0)]
 
 # create unique section id
 DF[,id_secao := paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
 
 # mm <- DF[!duplicated(DF$DS_FAIXA_ETARIA),]
 
 # summarize demographics by section
 AG[[i]] <- DF[, .(
  mulher = sum(mulher),
  educ_prim = sum(educ_prim),
  idade_16_17 = sum(idade_16_17),
  idade_18_24 = sum(idade_18_24),
  idade_60M = sum(idade_60M),
  qt_perfil = sum(QT_ELEITORES_PERFIL),
  qt_biometria = sum(QT_ELEITORES_BIOMETRIA)), 
  by = .(CD_MUNICIPIO, NR_ZONA, NR_SECAO, id_secao)]
 
 cat(my_uf[i], " ")
}
 
# BR <- do.call(rbind, AG)
BR <- data.table::rbindlist(AG)

fwrite(BR, '../../data/secoes/secoes_perfil_2022.csv')

