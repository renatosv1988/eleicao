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


prep_data <- function(data){
 
 dt <- copy(data)
 setDT(dt)
# calculate vote share
 dt[, votos_lula_p := sum(votos_lula) / sum(votos_validos), by = .(ANO_ELEICAO, NR_TURNO, id_secao)]

# determine passe livre in each round
 dt[, passe_livre_1 := max(NR_TURNO==1 & passe_livre==1), by = id_secao]
 dt[, passe_livre_2 := max(NR_TURNO==2 & passe_livre==1), by = id_secao]

 # rename column
 if(dt$ANO_ELEICAO[1L]==2022){ setnames(dt, 'comparecimento_2022', 'comparecimento') }
 if(dt$ANO_ELEICAO[1L]==2018){ setnames(dt, 'comparecimento_2018', 'comparecimento') }
 if(dt$ANO_ELEICAO[1L]==2014){ setnames(dt, 'comparecimento_2014', 'comparecimento') }

  
 
 temp_dt <- dt[, .(comparecimento_t1 = comparecimento[which(NR_TURNO==1)],
                   comparecimento_t2 = comparecimento[which(NR_TURNO==2)],
                   votos_lula_p_t1 = votos_lula_p[which(NR_TURNO==1)],
                   votos_lula_p_t2 = votos_lula_p[which(NR_TURNO==2)],
                   # biometria  = mean(biometria), 
                   idade_60M  = idade_60M[1L], 
                   educacao_1 = educacao_1[1L],
                   num_1000  = num_1000[1L],
                   passe_livre_1 = passe_livre_1[1L],
                   passe_livre_2 = passe_livre_2[1L],
                   metro_only = metro_only[1L]
 ),
               by=.(ANO_ELEICAO, SG_UF, CD_MUNICIPIO, code_muni, 
                    PIB_PC, id_secao, passe_livre_always, dummy_pt)]

temp_dt[, variacao_comparecimento := comparecimento_t2 - comparecimento_t1]
temp_dt[, variacao_luva := votos_lula_p_t2 - votos_lula_p_t1]
return(temp_dt)
}

output_list <- lapply(X = list(eleicao_2014, eleicao_2018, eleicao_2022),
                      FUN = prep_data)

df <- data.table::rbindlist(output_list)

# salvar arquivo final----------------------------------------------------------

head(df)