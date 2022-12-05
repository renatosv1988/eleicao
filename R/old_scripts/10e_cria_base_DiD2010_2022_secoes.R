library(geobr)
library(data.table)
library(ggplot2)
library(scales)
library(fixest)
library(basedosdados)


# read data
eleicao_2022 <- fread('../../data/base_DiD2022_secoes.csv')
eleicao_2018 <- fread('../../data/base_DiD2018_secoes.csv')
eleicao_2014 <- fread('../../data/base_DiD2014_secoes.csv')
eleicao_2010 <- fread('../../data/base_DiD2010_secoes.csv')

summary(eleicao_2022$comparecimento_2022)
summary(eleicao_2018$comparecimento)

setdiff(names(eleicao_2022), names(eleicao_2018))
setdiff(names(eleicao_2018), names(eleicao_2022))


# drop columns
eleicao_2022[, c('NM_LOCAL_VOTACAO', 'NM_MUNICIPIO', 'votos_lula_2018', 'comparecimento_2018_muni', 'variacao_comparecimento_2018_muni') := NULL]

# rename columns
setnames(eleicao_2022,
         old = c('comparecimento_2022','abstencao_2022'),
         new = c('comparecimento', 'abstencao'))


setnames(eleicao_2018,
         old = c('comparecimento_2018','abstencao_2018'),
         new = c('comparecimento', 'abstencao'))


# juntar 2018 e 2022
eleicao <- rbind(eleicao_2018, eleicao_2022, fill=TRUE)




# salvar arquivo final----------------------------------------------------------
fwrite(eleicao, "../../data/base_DiD2022_2018_secoes.csv")
saveRDS(eleicao, "../../data/base_DiD2022_2018_secoes.rds",compress = T)
