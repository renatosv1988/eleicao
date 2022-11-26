# download and unzip data

library(openxlsx)
library(data.table)
library(httr)



#' #### eleitorado ZONA  ----------------------------------------------------------
#' #' perfil de escolaridade em cada zona
#' #' https://dadosabertos.tse.jus.br/dataset/eleitorado-2022
#' 
#' # create dir
#' dir.create('../../data_raw')
#' dir_eleitorado <- '../../data_raw/eleitorado'
#' dir.create(dir_eleitorado)
#' 
#' # download data
#' download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitorado/perfil_eleitorado_2022.zip",
#'               destfile = paste0(dir_eleitorado, "/eleitorado_2022.zip"))
#' download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitorado/perfil_eleitorado_2018.zip",
#'               destfile = paste0(dir_eleitorado, "/eleitorado_2018.zip"))
#' 
#' # unzip data to local dir
#' unzip(paste0(dir_eleitorado, "/eleitorado_2022.zip"), exdir = dir_eleitorado)
#' unzip(paste0(dir_eleitorado, "/eleitorado_2018.zip"), exdir = dir_eleitorado)
# 666 delete this. I think we don't use this data because we get the same info from 'eleitorado SECAO'




#### eleitorado secao (perfil) ----------------------------------------------------------
#' perfil de escolaridade em cada zona
#' https://dadosabertos.tse.jus.br/dataset/eleitorado-2022

# create dir
dir_eleitorado <- '../../data_raw/eleitorado_secao'
dir.create(dir_eleitorado)

# download data
my_uf <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG", "MS", "MT",
           "PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")

for(i in 1:27){
  download.file(paste0("https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitor_secao/perfil_eleitor_secao_2022_",
                       my_uf[i],".zip"),
                destfile = paste0(dir_eleitorado, paste0("/eleitorado_",my_uf[i],"_2022.zip")))
}
for(i in 1:27){
# unzip data to local dir
unzip(paste0(dir_eleitorado, paste0("/eleitorado_",my_uf[i],"_2022.zip")), exdir = dir_eleitorado)
}



#### eleitorado secao (perfil 2018) --------------------------------------------
#' perfil de escolaridade em cada zona
#' https://dadosabertos.tse.jus.br/dataset/eleitorado-2018

# create dir
dir_eleitorado <- '../../data_raw/eleitorado_secao_2018'
dir.create(dir_eleitorado)

# download data
my_uf <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG", "MS", "MT",
           "PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")

for(i in 1:27){
 download.file(paste0("https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitor_secao/perfil_eleitor_secao_2018_",
                      my_uf[i],".zip"),
               destfile = paste0(dir_eleitorado, paste0("/eleitorado_",my_uf[i],"_2018.zip")))
}


#### zonas (spatial) --------------------------------------------------
#' quantidade de eleitores registrados em cada secao e cada zona
#' https://dadosabertos.tse.jus.br/dataset/eleitorado-2022

# create dir
dir_zonas <- '../../data_raw/zonas'
dir.create(dir_zonas)

# download data
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/eleitorado_locais_votacao/eleitorado_local_votacao_2022.zip",
              destfile = paste0(dir_zonas, "/zonas_2022.zip"))
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/eleitorado_locais_votacao/eleitorado_local_votacao_2018.zip",
              destfile = paste0(dir_zonas, "/zonas_2018.zip"))

# unzip data to local dir
unzip(paste0(dir_zonas,"/zonas_2022.zip"), exdir = dir_zonas)
unzip(paste0(dir_zonas,"/zonas_2018.zip"), exdir = dir_zonas)





#### secoes --------------------------------------------------
#' Detalhe da apuração por seção eleitoral
#' https://dadosabertos.tse.jus.br/dataset/resultados-2022

# create dir
dir_secoes <- '../../data_raw/secoes'
dir.create(dir_secoes)

# download data
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_secao/detalhe_votacao_secao_2022.zip",
              destfile = paste0(dir_secoes, "/secoes_2022.zip"))
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_secao/detalhe_votacao_secao_2018.zip",
              destfile = paste0(dir_secoes, "/secoes_2018.zip"))

# unzip data to local dir
unzip(paste0(dir_secoes, "/secoes_2022.zip"), exdir = dir_secoes)
unzip(paste0(dir_secoes, "/secoes_2018.zip"), exdir = dir_secoes)






#### google mobility --------------------------------------------------
#' https://www.google.com/covid19/mobility/

# create dir
dir_google <- '../../data_raw/google'
dir.create(dir_google)

# download data
download.file("https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip",
              destfile = paste0(dir_google, "/google_mobility.zip"))

# httr::GET(url="https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip",
#           httr::write_disk( paste0(dir_google, "/google_mobility.zip"), overwrite = T))

          
# unzip data to local dir
unzip(paste0(dir_google, "/google_mobility.zip"), exdir = dir_google,
      files = '2020_BR_Region_Mobility_Report.csv')




#### MUNIC data --------------------------------------------------
#' https://www.ibge.gov.br/estatisticas/sociais/saude/10586-pesquisa-de-informacoes-basicas-municipais.html?=&t=downloads

# create dir
dir_munic_2020 <- '../../data_raw/munic'
dir.create(dir_munic_2020)

# download data
download.file("https://ftp.ibge.gov.br/Perfil_Municipios/2020/Base_de_Dados/Base_MUNIC_2020.xlsx",
              destfile = paste0(dir_munic_2020,"/Base_MUNIC_2020.xlsx"), mode="wb")

#httr::GET(url = "https://ftp.ibge.gov.br/Perfil_Municipios/2020/Base_de_Dados/Base_MUNIC_2020.xlsx",
#          httr::write_disk(paste0(dir_munic_2020,"/Base_MUNIC_2020.xlsx")))

# read data
munic <- openxlsx::read.xlsx(xlsxFile = '../../data_raw/munic/Base_MUNIC_2020.xlsx',
                         sheet = 'Transporte')

setDT(munic)


table(munic$Mtra19, useNA = 'always')

# error types 1 and 2
a  <- munic[Mtra19 %like% 'Recusa|Não', .(UF, CodMun, Mun, Faixa_pop, Mtra19)]
a  <- munic[Mtra19 %like% 'Sim', .(UF, CodMun, Mun, Faixa_pop, Mtra19)]

# Fix Municipio of Ananindeua did not answer the survey, but they do have public transport
cities_with_pt <- c(1500800, # Ananindeua
                    2706703, # Penedo
                    4211900, # Palhoca
                    1709500, # Gurupi
                    2907509, # Catu
                    3304524, # Rio das Ostras
                    3138203, # Lavras
                    1503606, # Itaituba
                    1303403, # Parintins
                    2306405, # Itapipoca
                    2600054, # Abreu e Lima
                    2603454, # Camaragibe
                    2900702, # Alagoinhas
                    3302858, # Mesquita
                    3545803, # Santa Bárbara dOeste
                    4107652, # Fazenda Rio Grande
                    4119152 # Pinhais
                    )

munic[, Mtra19 := fifelse(CodMun %in% cities_with_pt, 'Sim', Mtra19)]



# create dummy
munic[, dummy_pt := fifelse(Mtra19 == 'Sim', 1, 0)]

# subset and rename columns
munic <- munic[, .(CodMun, UF, Cod.UF, Mun, dummy_pt)]
setnames(munic, c('code_muni', 'UF', 'code_state', 'name_muni', 'dummy_pt'))

# save
dir.create('./data')
dir_munic <- '../../data/munic'
dir.create(dir_munic)
fwrite(munic, '../../data/munic/munic_dummy_pt.csv')




#### correspondencia IBGE TSE MUNICIPIOS ---------------------------------------
dir_tse_ibge <- '../../data_raw/tse_ibge'
dir.create(dir_tse_ibge)
download.file(url = "https://raw.githubusercontent.com/betafcc/Municipios-Brasileiros-TSE/master/municipios_brasileiros_tse.csv",
              destfile="../../data_raw/tse_ibge/correspondencia_IBGE_TSE.csv")





#' 
#' #### votacao --------------------------------------------------
#' #' https://dadosabertos.tse.jus.br/dataset/resultados-2022
#' 
#' # create dir
#' dir_votacao <- '../../data_raw/votacao'
#' dir.create(dir_votacao)
#' 
#' # download data
#' download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2022.zip",
#'               destfile = paste0(dir_votacao,"/votacao_2022.zip"))
#' download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2018.zip",
#'               destfile = paste0(dir_votacao,"/votacao_2018.zip"))
#' 
#' # unzip data to local dir
#' unzip(paste0(dir_votacao,"/votacao_2022.zip"), exdir = dir_votacao)
#' unzip(paste0(dir_votacao,"/votacao_2018.zip"), exdir = dir_votacao)
# 666 delete this. I think we don't use this data because we get the same info from 'urnas'




#### urnas 2014 ---------------------------------------------------------------------
# 6666666666666666
# https://dadosabertos.tse.jus.br/dataset/resultados-2014/resource/55f1f786-cbf8-4cec-a003-f349d1dfb77f


#### urnas 2020 ---------------------------------------------------------------------
# 6666666666666666
# https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_partido_munzona/votacao_partido_munzona_2020.zip
dir_urnas <- '../../data_raw/urnas_2020'
dir.create(dir_urnas)

# download data
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_secao/detalhe_votacao_secao_2020.zip",
              destfile = paste0(dir_urnas,"/votacao_2020.zip"))


#### urnas 2022 ---------------------------------------------------------------------
dir_urnas <- '../../data_raw/urnas_2022'
dir.create(dir_urnas)


# download data
my_uf <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG", "MS", "MT",
           "PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")
options(timeout=100000)
for(i in 1:27){# i <- 26
 download.file(paste0("https://cdn.tse.jus.br/estatistica/sead/eleicoes/eleicoes2022/buweb/bweb_1t_",
                      my_uf[i],"_051020221321.zip"),
               destfile = paste0(dir_urnas, paste0("/urnas_",my_uf[i],"_2022_1T.zip")))
 download.file(paste0("https://cdn.tse.jus.br/estatistica/sead/eleicoes/eleicoes2022/buweb/bweb_2t_",
                      my_uf[i],"_311020221535.zip"),
               destfile = paste0(dir_urnas, paste0("/urnas_",my_uf[i],"_2022_2T.zip")))
 # list csvs from zip
 
 
 cat(my_uf[i], " ")
}


#### urnas 2018 ----------------------------------------------------------------
dir_urnas_2018 <- '../../data_raw/urnas_2018'
dir.create(dir_urnas_2018)


# download data
my_uf <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG", "MS", "MT",
           "PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")
options(timeout=100000)
for(i in 1:27){# i <- 26
 download.file(paste0("https://cdn.tse.jus.br/estatistica/sead/eleicoes/eleicoes2018/buweb/BWEB_1t_",
                      my_uf[i],"_101020181938.zip"),
               destfile = paste0(dir_urnas_2018, paste0("/urnas_",my_uf[i],"_2018_1T.zip")))
 download.file(paste0("https://cdn.tse.jus.br/estatistica/sead/eleicoes/eleicoes2018/buweb/BWEB_2t_",
                      my_uf[i],"_301020181744.zip"),
               destfile = paste0(dir_urnas_2018, paste0("/urnas_",my_uf[i],"_2018_2T.zip")))
 # list csvs from zip
 
 
 cat(my_uf[i], " ")
}

# onibus Sao Paulo -------------------------------------------------------------
dir_sp <- '../../data_raw/sp'
dir.create(dir_sp)

files_2018 <- c("https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/02092018.xls",
                "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/09092018.xls",
                "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/16092018.xls",
                "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/23092018_Dados%20para%20Transpar%C3%AAncia.xls",
                "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/30092018_Dados%20para%20Transpar%C3%AAncia.xls",
                "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/07102018_Dados%20para%20Transpar%C3%AAncia.xls",
                "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/14102018_Dados%20para%20Transpar%C3%AAncia.xls",
                "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/21102018_Dados%20para%20Transpar%C3%AAncia.xls",
                "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/28102018_Dados%20para%20Transpar%C3%AAncia.xls",
                "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/04112018_Dados%20para%20Transpar%C3%AAncia.xls",
                "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/11112018_Dados%20para%20Transpar%C3%AAncia.xls")



files <- c("https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/04SET2022.xls",
           "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/11SET2022.xls",
           "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/18SET2022.xls",
           "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/25SET2022.xls",
           "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/02OUT2022.xls",
           "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/09OUT2022.xls",
           "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/16OUT2022.xls",
           "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/23OUT2022.xls")

my_dates18 <- seq.Date(from=as.Date("2018-09-02"), to=as.Date("2022-11-11"), by="7 days")
for(i in 1:length(files_2018)){ #i<-1
 download.file(files_2018[i],
               destfile = paste0(dir_sp, paste0("/", my_dates18[i], ".xls")))
}

my_dates <- seq.Date(from=as.Date("2022-09-04"), to=as.Date("2022-10-23"), by="7 days")
for(i in 1:length(files)){ #i<-1
download.file(files[i],
 destfile = paste0(dir_sp, paste0("/", my_dates[i], ".xls")))
}




# spatial coordinates -------------------------------------------------------------
#' by Marcelo Oliveira
#' https://twitter.com/Capyvara/status/1590918812937117696

dir_spatial <- '../../data_raw/spatial'
dir.create(dir_spatial)


# download data
download.file("https://storage.googleapis.com/capyvara_public/tse/geocoded_voting_places_001.csv",
              destfile = paste0(dir_spatial, "/geocoded_voting_places_001.csv"))
