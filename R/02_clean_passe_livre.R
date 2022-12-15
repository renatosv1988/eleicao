library(readxl)
library(data.table)
library(stringr)

`%nin%` <- Negate(`%in%`)


# read passe livre
passe_livre <- read_excel("../../data_raw/passe_livre/Passe Livre nas Eleições.xlsx", range = "B3:N397")
setDT(passe_livre)

# municipios with passe livre all year
passe_livre_sempre <- read_excel("../../data_raw/passe_livre/Passe Livre nas Eleições.xlsx", range = "B427:C480")
setDT(passe_livre_sempre)
setnames(passe_livre_sempre, 'Estado', 'UF')




### read zonas 

 # unzip data
 temp_dir_1 <- tempdir()
 all_zip_files <- list.files("../../data_raw/zonas", full.names = T, pattern = '.zip')
 temp_zip <- all_zip_files[all_zip_files %like% 2022]
 files <- unzip(temp_zip, list=T)$Name
 path_csv_T1  <- files[files %like% '.csv']
 unzip(temp_zip, files  = path_csv_T1, exdir = temp_dir_1)
 path_csv <- paste0(temp_dir_1,'/',path_csv_T1)
 
 # read zonas
 zonas <- fread( path_csv)



# create data frame of municipalities with codes
municipios <- zonas[!duplicated(zonas$CD_MUNICIPIO,),
                    c("CD_MUNICIPIO", "NM_MUNICIPIO", "SG_UF") ]
#remove foreign cities
municipios <- municipios[municipios$SG_UF!="ZZ",]

# create city - UF ID
municipios[, city_uf_ID := paste( NM_MUNICIPIO, SG_UF, sep =" - ")]
passe_livre[, city_uf_ID := paste( Cidade, UF, sep =" - ")]
passe_livre_sempre[, city_uf_ID := paste( Cidade, UF, sep =" - ")]


# clean special characters and make all upper case
municipios[, city_uf_ID := iconv(city_uf_ID, to = "ASCII//TRANSLIT")]
municipios[, city_uf_ID := toupper(city_uf_ID)]
passe_livre[, city_uf_ID := iconv(city_uf_ID, from = "UTF-8", to = "ASCII//TRANSLIT")]
passe_livre[, city_uf_ID := toupper(city_uf_ID)]
passe_livre_sempre[, city_uf_ID := iconv(city_uf_ID, from = "UTF-8", to = "ASCII//TRANSLIT")]
passe_livre_sempre[, city_uf_ID := toupper(city_uf_ID)]





## ADDITIONAL MANUAL FIXES
# remove double spaces
passe_livre[, city_uf_ID := str_squish(city_uf_ID)]
passe_livre_sempre[, city_uf_ID := str_squish(city_uf_ID)]

# typos
passe_livre[, city_uf_ID := gsub("SANTO ANTONIO DA POSSE","SANTO ANTONIO DE POSSE", city_uf_ID)]
passe_livre[, city_uf_ID := gsub("SAO LUIZ DO PARAITINGA","SAO LUIS DO PARAITINGA", city_uf_ID)]
passe_livre[, city_uf_ID := gsub("ARTHUR NOGUEIRA - SP","ARTUR NOGUEIRA - SP", city_uf_ID)]
passe_livre_sempre[, city_uf_ID := gsub("SANTO ANTONIO DA POSSE","SANTO ANTONIO DE POSSE", city_uf_ID)]
passe_livre_sempre[, city_uf_ID := gsub("SAO LUIZ DO PARAITINGA","SAO LUIS DO PARAITINGA", city_uf_ID)]
passe_livre_sempre[, city_uf_ID := gsub("ARTHUR NOGUEIRA - SP","ARTUR NOGUEIRA - SP", city_uf_ID)]




# Update of munis with passe livre all year
passe_livre <- rbindlist(fill = T,
                         list(passe_livre,  # avoid repetition
                              subset(passe_livre_sempre, city_uf_ID %nin% passe_livre$city_uf_ID)
                         )
)

# rename columns
setnames(passe_livre, '1º Turno', 'passe_livre_t1')
setnames(passe_livre, '2º Turno', 'passe_livre_t2')


# update passe livre info
passe_livre[ city_uf_ID %in% passe_livre_sempre$city_uf_ID, passe_livre_t1 := 'S' ]
passe_livre[ city_uf_ID %in% passe_livre_sempre$city_uf_ID, passe_livre_t2 := 'S' ]

sum(passe_livre$passe_livre_t1 == 'S' , na.rm=T)
#> 135

sum(passe_livre$passe_livre_t2 == 'S' , na.rm=T)
#> 432

# MAJOR DISCOUNT ???? ON 1ST ROUND
passe_livre[ city_uf_ID %like% 'RIO BRANCO']
passe_livre[ city_uf_ID %like% 'NATAL']
passe_livre[ city_uf_ID %like% 'CUIABA']
passe_livre[ city_uf_ID %like% 'JUNDIAI']

passe_livre[ city_uf_ID %like% 'AMERICANA'] # passe metropolitano mas nao muni

# merge CD_MUNICIPIO to passe_livre
passe_livre <- merge(passe_livre, municipios[,c("city_uf_ID", "CD_MUNICIPIO")], by= "city_uf_ID", all.x = T)



passe_livre[, passe_livre_t1 := fcase(passe_livre_t1=='S',1,
                                      passe_livre_t1=='N', 0,
                                      default = 0)]

passe_livre[, passe_livre_t2 := fcase(passe_livre_t2=='S',1,
                                      passe_livre_t2=='N', 0,
                                      default = 0)]


# SUMARÉ teve no 2o turno
passe_livre[city_uf_ID =="SUMARE - SP", passe_livre_t2 := 1]
#' https://g1.globo.com/sp/campinas-regiao/eleicoes/2022/noticia/2022/10/29/eleicoes-18-cidades-da-regiao-de-campinas-terao-gratuidade-no-transporte-no-2o-turno-veja-quais.ghtml
# criar versão simplificada do arquivo

# identify cities with permanent passe livre
passe_livre[city_uf_ID %in% passe_livre_sempre$city_uf_ID, passe_livre_always := 1]

table(passe_livre$passe_livre_t1, useNA = 'always')
#>    0    1 <NA> 
#>  297  135    0 

table(passe_livre$passe_livre_t2, useNA = 'always')
#>    1 <NA> 
#>  432    0


table(passe_livre$passe_livre_always, useNA = 'always')
#>   1 <NA> 
#>  53  379 

# get population of municipalities that only adopted PL on the 2nd round
pop <- passe_livre[ passe_livre_t2==1 & is.na(passe_livre_always), ]
setnames(pop, 'População (2020)', 'pop2020')

summary(pop$pop2020)

pop[ is.na(pop2020)]
pop[city_uf_ID=='MURIAE - MG', pop2020 := 109392]

sum(pop$pop2020, na.rm=T)
#> 102.551.295

# subset of columns
passe_livre_resumo <- passe_livre[,c("CD_MUNICIPIO", "passe_livre_t1","passe_livre_t2", "passe_livre_always")]

# save
dir_passe_livre <- '../../data/passe_livre'
dir.create(dir_passe_livre)
fwrite(passe_livre, paste0(dir_passe_livre,"/passe_livre.csv"), row.names = F)
fwrite(passe_livre_resumo, paste0(dir_passe_livre,"/passe_livre_resumo.csv"), row.names = F)


# https://g1.globo.com/sp/campinas-regiao/eleicoes/2022/noticia/2022/10/29/eleicoes-18-cidades-da-regiao-de-campinas-terao-gratuidade-no-transporte-no-2o-turno-veja-quais.ghtml
# 
# ano todo
# Holambra
# Louveira (ano todo, mas idec fala soh 2)
# Morungaba
# 
passe_livre[ city_uf_ID %like% 'LOUVEIRA']
# 
# Americana
# Jaguariúna (intermunicipal)
# Paulínia (intermunicipal)
# Vinhedo

