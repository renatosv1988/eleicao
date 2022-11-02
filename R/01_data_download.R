# download and unzip data


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