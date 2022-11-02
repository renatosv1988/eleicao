library(readxl)
library(data.table)
library(stringr)

# read passe livre
passe_livre <- read_excel("../Passe Livre nas Eleições.xlsx", range = "B3:N396")

# read zonas 
files_zonas <- list.files(path = './data_raw/zonas', pattern = '.csv', full.names = T)
zonas <- fread( files_zonas, nrows = Inf)

# create data frame of municipalities with codes
municipios <- zonas[!duplicated(zonas$CD_MUNICIPIO,),
                    c("CD_MUNICIPIO", "NM_MUNICIPIO", "SG_UF") ]
#remove foreign cities
municipios <- municipios[municipios$SG_UF!="ZZ",]

# create city - UF ID
municipios$city_uf_ID <- paste(municipios$NM_MUNICIPIO, municipios$SG_UF, sep =" - ")
passe_livre$city_uf_ID <- paste(passe_livre$Cidade, passe_livre$UF, sep =" - ")

# clean special characters and make all upper case
municipios$city_uf_ID <- iconv(municipios$city_uf_ID, to = "ASCII//TRANSLIT")
municipios$city_uf_ID <- toupper(municipios$city_uf_ID)
passe_livre$city_uf_ID <- iconv(passe_livre$city_uf_ID, from = "UTF-8", to = "ASCII//TRANSLIT")
passe_livre$city_uf_ID <- toupper(passe_livre$city_uf_ID)

## ADDITIONAL MANUAL FIXES
# remove double spaces
passe_livre$city_uf_ID <- str_squish(passe_livre$city_uf_ID)
# typos
passe_livre$city_uf_ID <- gsub("SANTO ANTONIO DA POSSE","SANTO ANTONIO DE POSSE",passe_livre$city_uf_ID)
passe_livre$city_uf_ID <- gsub("SAO LUIZ DO PARAITINGA","SAO LUIS DO PARAITINGA",passe_livre$city_uf_ID)

# merge CD_MUNICIPIO to passe_livre
passe_livre <- merge(passe_livre, municipios[,c("city_uf_ID", "CD_MUNICIPIO")], by= "city_uf_ID", all.x = T)

# convert S/N to 1/0
passe_livre$`1º Turno`[passe_livre$`1º Turno`%in%"S"] <- 1
passe_livre$`1º Turno`[passe_livre$`1º Turno`%in%"N"] <- 0
passe_livre$`2º Turno`[passe_livre$`2º Turno`%in%"S"] <- 1
passe_livre$`2º Turno`[passe_livre$`2º Turno`%in%"N"] <- 0

# convert NAs to 0
passe_livre$`1º Turno`[is.na(passe_livre$`1º Turno`)] <- 0
passe_livre$`2º Turno`[is.na(passe_livre$`2º Turno`)] <- 0

# remove SUMARÉ (NA both rounds)
passe_livre <- passe_livre[!passe_livre$city_uf_ID=="SUMARE - SP",]

# criar versão simplificada do arquivo
passe_livre_resumo <- passe_livre[,c("CD_MUNICIPIO", "1º Turno","2º Turno")]
colnames(passe_livre_resumo) <- c("CD_MUNICIPIO", "passe_livre_t1","passe_livre_t2")

# remove SUMARÉ (NA both rounds)
dir_passe_livre <- './data_raw/passe_livre'
dir.create(dir_passe_livre)
write.csv(passe_livre, paste0(dir_passe_livre,"/passe_livre.csv"), row.names = F)
write.csv(passe_livre_resumo, paste0(dir_passe_livre,"/passe_livre_resumo.csv"), row.names = F)
