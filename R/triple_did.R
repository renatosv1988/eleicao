df <- fread("../../data/base_DiD2022_2018_secoes.csv")


# depois balancer
t <- table(df$id_secao) |> as.data.table()
t <- t[N ==4]

df2 <- df[ id_secao %in% t$V1]

df2[, passe := max(passe_livre), by= id_secao]
df2[, dummy_turno := fifelse(NR_TURNO==2,1,0)]
df2[, dummy_ano := fifelse(ANO_ELEICAO==2022,1,0)]

# share of PT votes
df2[, votos_lula_p := sum(votos_lula) / sum(votos_validos), by = .(ANO_ELEICAO, NR_TURNO, id_secao)]

# excluir seções de cidades sem sistema de ônibus
df2 <- subset(df2, dummy_pt==1)

# excluir cidades que SEMPRE tiveram passe livre
df2 <- subset(df2, is.na(passe_livre_always))


# identify treated in the 1st round
df2[, passe_livre_1 := max(NR_TURNO==1 & passe_livre==1), by = id_secao]
table(df2$passe_livre_1)


# triple DiD comparecimento -------------------------------------

# drop always treated (1st round)
df3 <- df2[ passe_livre_1 != 1]
df3[, table(NR_TURNO, passe_livre)]



d3 <- fixest::feols( comparecimento ~ dummy_turno*dummy_ano*passe , 
               fixef = 'id_secao', 
               cluster = 'code_muni',
               data = df3)

summary(d3)

d4 <- fixest::feols( votos_lula_p ~ dummy_turno*dummy_ano*passe , 
                     fixef = 'id_secao', 
                     cluster = 'code_muni',
                     data = df3)

summary(d4)





# ipw  -------------------------------------


# aggregate variables at muni level
df_muni <- df2[, .(QT_APTOS = sum(QT_APTOS[which(NR_TURNO==2)], na.rm=T),
                           QT_APTOS_log = log(sum(QT_APTOS[which(NR_TURNO==2)], na.rm=T)),
                           idade_60M = weighted.mean(x=idade_60M, w=QT_APTOS, na.rm=T),
                           biometria = weighted.mean(x=biometria, w=QT_APTOS, na.rm=T),
                           qt_biometria = sum(qt_biometria[which(NR_TURNO==2)], na.rm=T),
                           votos_jair_muni_total_p = sum(votos_jair[which(NR_TURNO==1)]) / sum(votos_total[which(NR_TURNO==1)]),
                           votos_jair_muni_validos_p = sum(votos_jair[which(NR_TURNO==1)]) / sum(votos_validos[which(NR_TURNO==1)]),
                           mean_dist = weighted.mean(x=dist_sede, w=QT_APTOS, na.rm=T),      # pondera ou nao ?
                           mean_dens_1000 = weighted.mean(x=num_1000, w=QT_APTOS, na.rm=T),  # pondera ou nao ?
                           educacao_1 = weighted.mean(x=educacao_1, w=QT_APTOS, na.rm=T),
                           gov_2t = max(gov_2t),
                           PIB_PC = PIB_PC[1L],
                           pib_log = log(PIB_PC)[1L],
                           passe_livre = max(passe_livre)
                   # passe_livre_1 = max(passe_livre_1), 
                           # passe_livre_2 = max(passe_livre_2)
                   ),
                       by= .(SG_UF, name_region, code_muni)]

step1 <- glm(passe_livre ~ QT_APTOS_log + pib_log + name_region + votos_jair_muni_validos_p ,
             family = binomial(link = 'logit'),
             data = df_muni)

df_muni[, ipw := (passe_livre / fitted(step1)) + ((1 - passe_livre) / ( 1- fitted(step1)))]
summary(df_muni$ipw)

# merge ipw info of muni to sections
setDT(df2)[df_muni, on='code_muni', ipw := i.ipw]





# test 1o vs 1o comparecimento -------------------------------------

# keep only 1st round
temp_df1 <- subset(df2, NR_TURNO == 1)

# checks
table(temp_df1$dummy_ano)
table(temp_df1$NR_TURNO)

table(temp_df1$passe_livre_1, temp_df1$dummy_ano)

output_1 <- fixest::feols( comparecimento ~ dummy_ano*passe_livre_1 , 
                     fixef = 'id_secao', 
                     cluster = 'code_muni',
                     weights = ~ipw,
                     data = temp_df1)
summary(output_1)



# test 2o vs 2o comparecimento -------------------------------------

# keep only 1st round
temp_df2 <- subset(df2, NR_TURNO == 2)

# checks
table(temp_df2$dummy_ano)
table(temp_df2$NR_TURNO)

table(temp_df2$passe_livre, temp_df2$dummy_ano)

output_2 <- fixest::feols(comparecimento ~ dummy_ano*passe_livre , 
                          fixef = 'id_secao', 
                          cluster = 'code_muni',
                          weights = ~ipw,
                          data = temp_df2)
summary(output_2)








# test 1o vs 1o Lula -------------------------------------

# keep only 1st round
temp_df1 <- subset(df2, NR_TURNO == 1)

# checks
table(temp_df1$dummy_ano)
table(temp_df1$NR_TURNO)

table(temp_df1$passe_livre_1, temp_df1$dummy_ano)

output_3 <- fixest::feols( votos_lula_p ~ dummy_ano*passe_livre_1 , 
                           fixef = 'id_secao', 
                           cluster = 'code_muni',
                           weights = ~ipw,
                           data = temp_df1)
summary(output_3)



# test 2o vs 2o Lula -------------------------------------

# keep only 1st round
temp_df2 <- subset(df2, NR_TURNO == 2)

# checks
table(temp_df2$dummy_ano)
table(temp_df2$NR_TURNO)

table(temp_df2$passe_livre, temp_df2$dummy_ano)

output_4 <- fixest::feols( votos_lula_p ~ dummy_ano*passe_livre , 
                           fixef = 'id_secao', 
                           cluster = 'code_muni',
                           weights = ~ipw,
                           data = temp_df2)
summary(output_4)
