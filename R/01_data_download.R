# download and unzip data

library(openxlsx)
library(data.table)
library(httr)



#### eleitorado ZONA  ----------------------------------------------------------
#' perfil de escolaridade em cada zona
#' https://dadosabertos.tse.jus.br/dataset/eleitorado-2022

# create dir
dir.create('../../data_raw')
dir_eleitorado <- '../../data_raw/eleitorado'
dir.create(dir_eleitorado)

# download data
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitorado/perfil_eleitorado_2022.zip",
              destfile = paste0(dir_eleitorado, "/eleitorado_2022.zip"))
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitorado/perfil_eleitorado_2018.zip",
              destfile = paste0(dir_eleitorado, "/eleitorado_2018.zip"))

# unzip data to local dir
unzip(paste0(dir_eleitorado, "/eleitorado_2022.zip"), exdir = dir_eleitorado)
unzip(paste0(dir_eleitorado, "/eleitorado_2018.zip"), exdir = dir_eleitorado)


#### eleitorado SECAO  ----------------------------------------------------------
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



#### zonas --------------------------------------------------
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




#### votacao --------------------------------------------------
#' https://dadosabertos.tse.jus.br/dataset/resultados-2022

# create dir
dir_votacao <- '../../data_raw/votacao'
dir.create(dir_votacao)

# download data
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2022.zip",
              destfile = paste0(dir_votacao,"/votacao_2022.zip"))
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/votacao_candidato_munzona/votacao_candidato_munzona_2018.zip",
              destfile = paste0(dir_votacao,"/votacao_2018.zip"))

# unzip data to local dir
unzip(paste0(dir_votacao,"/votacao_2022.zip"), exdir = dir_votacao)
unzip(paste0(dir_votacao,"/votacao_2018.zip"), exdir = dir_votacao)



#### seções --------------------------------------------------
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


# Fix Municipio of Ananindeua did not answer the survey, but they do have public transport
setDT(munic)[, Mtra19 := fifelse(CodMun == 1500800, 'Sim', Mtra19)]

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


#### urnas ---------------------------------------------------------------------
dir_urnas <- '../../data_raw/urnas'
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


# onibus São Paulo -------------------------------------------------------------
dir_sp <- '../../data_raw/sp'
dir.create(dir_sp)

files <- c("https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/04SET2022.xls",
           "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/11SET2022.xls",
           "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/18SET2022.xls",
           "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/25SET2022.xls",
           "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/02OUT2022.xls",
           "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/09OUT2022.xls",
           "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/16OUT2022.xls",
           "https://www.prefeitura.sp.gov.br/cidade/secretarias/upload/23OUT2022.xls")

my_dates <- seq.Date(from=as.Date("2022-09-04"), to=as.Date("2022-10-23"), by="7 days")
for(i in 1:length(files)){ #i<-1
download.file(files[i],
 destfile = paste0(dir_sp, paste0("/", my_dates[i], ".xls")))
}
