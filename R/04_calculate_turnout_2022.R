library(data.table)
library(dplyr)


#### secoes --------------------------------------------------
#' quantidade de votos nominais e validos

# read
files_secao <- list.files(path = '../../data_raw/secoes', 
                          pattern = '2022_BRASIL.csv', 
                          full.names = T)

secao <- fread(files_secao,
               # nrows = Inf, 
               encoding = "Latin-1")

# manter apenas votos para presidente no BR
secao_br <- subset(secao, DS_CARGO == 'PRESIDENTE' & SG_UF != 'ZZ')

# calcular comparecimento e abstenção
secao_br[, comparecimento_2022 := QT_COMPARECIMENTO / QT_APTOS]
secao_br[, abstencao_2022 := QT_ABSTENCOES / QT_APTOS]

# excluir seção onde comparecimento + abstenção = 0 (COMPLEXO PENITENCIÁRIO DO PURAQUEQUARA)
secao_br <- subset(secao_br, abstencao_2022+comparecimento_2022 != 0)

# incluir 2º turno governador
gov <- subset(secao, DS_CARGO == 'GOVERNADOR' & SG_UF != 'ZZ' & NR_TURNO==2)
gov$gov_2t <- 1
gov <- gov[, .(gov_2t = mean(gov_2t)), by = SG_UF]
secao_br <- merge(secao_br, gov, by ="SG_UF", all.x = T)
secao_br$gov_2t[is.na(secao_br$gov_2t)] <- 0
secao_br$gov_2t <- ifelse(secao_br$NR_TURNO==2, secao_br$gov_2t, 0)

#### save outputs --------------------------------------------------
# comparecimento municipios
fwrite(secao_br, '../../data/secoes/secoes_2022.csv')

