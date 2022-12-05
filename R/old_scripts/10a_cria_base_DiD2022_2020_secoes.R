library(geobr)
library(data.table)
library(ggplot2)
library(scales)
library(fixest)
library(basedosdados)

# function to rename columns
toupper_noaccent <- function(i){
 stringi::stri_trans_general(i,id = "Latin-ASCII") %>% 
  toupper() %>% stringr::str_replace_all("-"," ")  %>% 
  stringr::str_remove_all("'")
}


# google mobility --------------------------------------------------------------

google <- fread(file = '../../data/google/google.csv')
# 
# # identify election rounds
# google[, NR_TURNO := fcase(date == as.IDate("2020-11-15"), 1,
#                                     date == as.IDate("2020-11-29"), 2,
#                                     date == as.IDate("2022-10-02"), 1,
#                                     date == as.IDate("2022-10-30"), 2)
#                 ]


table(google$NR_TURNO, google$year)


google1 <- subset(google, variable %like% 'transit') # transit residential
google1 <- subset(google1, name_muni != "NA - NA")

google_t1 <- google1[, .(mob_t1_2020 = value[which(date==as.IDate("2020-11-15"))],
                        mob_t2_2020 = value[which(date==as.IDate("2020-11-29"))],
                        mob_t1_2022 = value[which(date==as.IDate("2022-10-025"))]
                        ),
                    by = .(name_muni, sub_region_2 )]

head(google_t1)



# election data  --------------------------------------------------------------
eleicao_2022 <- fread('../../data/base_DiD2022_secoes.csv')
eleicao_2020 <- fread('../../data/secoes/secoes_2020.csv')


eleicao_2022_muni <- eleicao_2022[, .(QT_APTOS = sum(QT_APTOS),
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
                                            comparecimento_2022 = weighted.mean(comparecimento_2022, w=QT_APTOS),
                                            mean_dist = weighted.mean(x=dist_sede, w=QT_APTOS, na.rm=T),
                                            mean_dens_1000 = weighted.mean(x=num_1000, w=QT_APTOS, na.rm=T),
                                            educacao_1 = weighted.mean(x=educacao_1, w=QT_APTOS, na.rm=T),
                                            passe_livre = max(passe_livre),
                                            passe_livre_always = max(passe_livre_always)
                                      ),
                                  by = .(SG_UF, NM_MUNICIPIO, name_region, ANO_ELEICAO, NR_TURNO, code_muni, CD_MUNICIPIO, PIB_PC)]


eleicao_2020_muni <- eleicao_2020[, .(QT_APTOS = sum(QT_APTOS),
                                      comparecimento_2020 = weighted.mean(comparecimento_2020, w=QT_APTOS)),
                                  by = .(SG_UF, ANO_ELEICAO, NR_TURNO, CD_MUNICIPIO)]



df_t1 <- left_join(eleicao_2022_muni[NR_TURNO == 1, .(SG_UF, NM_MUNICIPIO, code_muni, CD_MUNICIPIO, comparecimento_2022, passe_livre, PIB_PC, QT_APTOS, mean_dens_1000, passe_livre_always)],
                   eleicao_2020_muni[NR_TURNO == 1, .(CD_MUNICIPIO, comparecimento_2020)],
                  by = 'CD_MUNICIPIO' )



# merge mobility data ------------------------------------------------------------
setDT(df_t1)
df_t1[, NM_MUNICIPIO := toupper_noaccent(NM_MUNICIPIO)]
df_t1[, name_muni := paste0(NM_MUNICIPIO, ' - ', SG_UF)]




df_t1_mob <- left_join(df_t1, google_t1, by = "name_muni")
head(df_t1_mob)

df_t1_mob2 <- subset(df_t1_mob, !is.na(mob_t1_2020))
df_t1_mob2 <- subset(df_t1_mob2, !is.na(mob_t1_2022))

summary(google_t1$mob_t1_2020)


df_t1_mob2 <- subset(df_t1_mob2, is.na(passe_livre_always))

# recode variables ----------------------------------------------------------

df_t1_mob2[mob_t1_2022 == 0, mob_t1_2022 := 1]
df_t1_mob2[mob_t1_2020 == 0, mob_t1_2020 := 1]

df_t1_mob2[, var_mob := mob_t1_2022 / mob_t1_2020]
df_t1_mob2[, var_com := comparecimento_2022 - comparecimento_2020]


summary(df_t1_mob2$mob_t1_2022)
summary(df_t1_mob2$mob_t1_2020)
summary(df_t1_mob2$var_mob)
summary(df_t1_mob2$var_com)

step1 <- lm( var_mob ~  passe_livre , 
             data = df_t1_mob2, na.action = na.omit)
 
summary(step1)
 
step2 <- lm( var_com ~ var_mob + passe_livre + log(QT_APTOS) + log(PIB_PC), 
         data = df_t1_mob2, na.action = na.omit)


summary(step2)



# # salvar arquivo final----------------------------------------------------------
# fwrite(eleicao, "../../data/base_DiD2022_2018_secoes.csv")
# saveRDS(eleicao, "../../data/base_DiD2022_2018_secoes.rds",compress = T)
