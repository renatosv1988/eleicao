library(geobr)
library(data.table)
library(ggplot2)


# read data ----------------------------------------------------------------------

eleicao <- fread("../../data/base_DiD2022_2018_secoes.csv")

table(eleicao$ANO_ELEICAO)
#>   2018   2022 
#> 906608 940932 





# aggregate by muni ------------------------------------------------------------

# aggregate variables at muni level
df_muni <- eleicao[, .(QT_APTOS = sum(QT_APTOS),
                       dummy_pt = max(dummy_pt),
                       gov_2t = max(gov_2t),
                       biometria = weighted.mean(x=biometria, w=QT_APTOS, na.rm=T),
                       qt_biometria = sum(qt_biometria, na.rm=T),
                       votos_jair = sum(votos_jair),
                       votos_lula = sum(votos_lula),
                       votos_total = sum(votos_total),
                       votos_branco = sum(votos_branco),
                       votos_nulo = sum(votos_nulo),
                       votos_validos = sum(votos_validos),
                       num_1000 = mean(num_1000),
                       comparecimento = weighted.mean(comparecimento, w=QT_APTOS),
                       mean_dist = weighted.mean(x=dist_sede, w=QT_APTOS, na.rm=T),
                       mean_dens_1000 = weighted.mean(x=num_1000, w=QT_APTOS, na.rm=T),
                       educacao_1 = weighted.mean(x=educacao_1, w=QT_APTOS, na.rm=T),
                       passe_livre = max(passe_livre)
                       ),
                   by = .(SG_UF, name_region, ANO_ELEICAO, NR_TURNO, code_muni, CD_MUNICIPIO, PIB_PC)]

head(df_muni)


                

            
            

# recode variables ------------------------------------------------------------

df_muni[, QT_APTOS_log := log(QT_APTOS)]
df_muni[, pib_log := log(PIB_PC)]




# salvar arquivo final----------------------------------------------------------
fwrite(df_muni, "../../data/base_DiD2022_2018_muni.csv")
