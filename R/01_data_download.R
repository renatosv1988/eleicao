# download and unzip data

library(openxlsx)
library(data.table)
library(httr)



#### eleitorado --------------------------------------------------
#' perfil de escolaridade em cada zona
#' https://dadosabertos.tse.jus.br/dataset/eleitorado-2022

# create dir
dir_eleitorado <- './data_raw/eleitorado'
dir.create(dir_eleitorado)

# download data
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitorado/perfil_eleitorado_2022.zip",
              destfile = paste0(dir_eleitorado, "/eleitorado_2022.zip"))
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/perfil_eleitorado/perfil_eleitorado_2018.zip",
              destfile = paste0(dir_eleitorado, "/eleitorado_2018.zip"))

# unzip data to local dir
unzip(paste0(dir_eleitorado, "/eleitorado_2022.zip"), exdir = dir_eleitorado)
unzip(paste0(dir_eleitorado, "/eleitorado_2018.zip"), exdir = dir_eleitorado)

#### zonas --------------------------------------------------
#' quantidade de eleitores registrados em cada secao e cada zona
#' https://dadosabertos.tse.jus.br/dataset/eleitorado-2022

# create dir
dir_zonas <- './data_raw/zonas'
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
dir_votacao <- './data_raw/votacao'
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
dir_secoes <- './data_raw/secoes'
dir.create(dir_secoes)

# download data
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_secao/detalhe_votacao_secao_2022.zip",
              destfile = paste0(dir_secoes, "/secoes_2022.zip"))
download.file("https://cdn.tse.jus.br/estatistica/sead/odsele/detalhe_votacao_secao/detalhe_votacao_secao_2018.zip",
              destfile = paste0(dir_secoes, "/secoes_2018.zip"))

# unzip data to local dir
unzip(paste0(dir_secoes, "/secoes_2022.zip"), exdir = dir_secoes)
unzip(paste0(dir_secoes, "/secoes_2018.zip"), exdir = dir_secoes)




#### MUNIC data --------------------------------------------------
#' https://www.ibge.gov.br/estatisticas/sociais/saude/10586-pesquisa-de-informacoes-basicas-municipais.html?=&t=downloads

# create dir
dir_munic_2020 <- './data_raw/munic'
dir.create(dir_munic_2020)

# download data
httr::GET(url = "https://ftp.ibge.gov.br/Perfil_Municipios/2020/Base_de_Dados/Base_MUNIC_2020.xlsx",
          httr::write_disk(paste0(dir_munic_2020,"/Base_MUNIC_2020.xlsx")))

# read data
munic <- openxlsx::read.xlsx(xlsxFile = './data_raw/munic/Base_MUNIC_2020.xlsx',
                         sheet = 'Transporte')


# Fix Municipio of Ananindeua did not answer the survey, but they do have public transport
setDT(munic)[, Mtra19 := fifelse(CodMun == 1500800, 'Sim', Mtra19)]


table(munic$Mtra19)
#>  Não Não informou       Recusa          Sim 
#> 3740           13           89         1728 

# create dummy
munic[, dummy_pt := fifelse(Mtra19 == 'Sim', 1, 0)]

# subset and rename columns
munic <- munic[, .(CodMun, UF, Cod.UF, Mun, dummy_pt)]
setnames(munic, c('code_muni', 'UF', 'code_state', 'name_muni', 'dummy_pt'))

# save
fwrite(munic, './data/munic_dummy_pt.csv')
