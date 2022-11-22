partidos <- fread("../../data_raw/partido_prefeitos.csv")

partidos[, sigla_partido := toupper(sigla_partido)]
head(partidos)

nrow(partidos)
partidos <- unique(partidos)


df2016 <- subset(partidos, ano==2016)
df2020 <- subset(partidos, ano==2020)


partidos_base <- c("PT", "PSOL", "PV", "PT DO B", "PC DO B",
                   "PCO", "PCB", "PDT", "PSB", "PTB")


df2016[, dummy_partidobase_2016 := fifelse(sigla_partido %in% partidos_base, 1, 0)]
df2020[, dummy_partidobase_2020 := fifelse(sigla_partido %in% partidos_base, 1, 0)]


# select columns and merge data
df2016 <- df2016[, .(id_municipio ,dummy_partidobase_2016)]
df2020 <- df2020[,  .(id_municipio ,dummy_partidobase_2020)]

df_partido <- merge(df2016, df2020)
df_partido <- unique(df_partido)
head(df_partido)

nrow(df_partido)

df_partido <- df_partido[, .(dummy_partidobase_2016 = dummy_partidobase_2016[1L],
               dummy_partidobase_2020 = dummy_partidobase_2020[1L]), by=id_municipio ]

# 
# a <- table(df_partido$id_municipio) |> as.data.table()
# a <- a[ N==1]
# 
# subset(partidos , id_municipio ==1100106 & ano==2016)
# 
# 
# df_partido <- subset(df_partido, id_municipio %in% a$V1)
# 




a <- fixest::feols(comparecimento_2022 ~ turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2 +
                    gov_2t + log(QT_APTOS) + log(PIB_PC) + name_region + votos_lula_p,
                   fixef = 'id_secao', 
                   cluster = 'code_muni',
                   data= df_sections)
                   
summary(a)                   
df_sections$votos_lula_p


                   
# chance de ser RM
