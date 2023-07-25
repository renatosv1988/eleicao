# Tabelas descritivas dos municipios
# Autor: Rogerio J Barbosa
# Data: 24/07/2023

# Setup ------------------------------------------------------------------------

rm(list = ls())
options(scipen = 999)

# Carrega Pacotes --------------------------------------------------------------

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# Carrega Dados ----------------------------------------------------------------

# ler base de eleições
#BD <- fread("../../data/base_DiD2022_secoes.csv")
BD <- fread("data/base_DiD2022_secoes.csv")

# Recodificacao ----------------------------------------------------------------

# criar grupos de tratamento
# períodos tratados
BD$periodos_tratados <- ave(BD$passe_livre, BD$id_secao, FUN=sum)
BD[, grupo := fcase(periodos_tratados==2, 1,
                    periodos_tratados==1, 2,
                    default = 0)]
BD$pl_1 <- ifelse(BD$periodos_tratados==2, 1, 0)
BD$pl_2 <- ifelse(BD$periodos_tratados==1, 1, 0)

# base por municipio
BM <- BD[, .(QT_APTOS = sum(QT_APTOS[which(NR_TURNO==1)], na.rm=T),
             QT_APTOS_log = log(sum(QT_APTOS[which(NR_TURNO==1)], na.rm=T)),
             idade_60M = weighted.mean(x=idade_60M, w=QT_APTOS, na.rm=T),
             biometria = weighted.mean(x=biometria, w=QT_APTOS, na.rm=T),
             votos_jair_validos_t1 = sum(votos_jair[which(NR_TURNO==1)], na.rm = T) / sum(votos_validos[which(NR_TURNO==1)], na.rm = T),
             votos_jair_validos_t2 = sum(votos_jair[which(NR_TURNO==2)], na.rm = T) / sum(votos_validos[which(NR_TURNO==2)], na.rm = T),
             mean_dens_1000 = weighted.mean(x=num_1000, w=QT_APTOS, na.rm=T),
             educacao_1 = weighted.mean(x=educacao_1, w=QT_APTOS, na.rm=T),
             gov_2t = max(gov_2t),
             PIB_PC = PIB_PC[1L],
             pib_log = log(PIB_PC[1L]),
             passe_livre = passe_livre[1L], 
             passe_livre_1 = pl_1[1L],
             passe_livre_2 = pl_2[1L]),
         by= .(SG_UF, name_region, code_muni, dummy_pt, grupo)]

BM[, grupos_labels := fcase(dummy_pt == 0, "1. No public transportation",
                            grupo    == 0, "2. No free public transit",
                            grupo    == 1, "3. Free public transit in both rounds",
                            grupo    == 2, "4. Free public transit only in 2nd round")]


BM_renamed = BM %>%
 select(-SG_UF, -code_muni, -grupo, -QT_APTOS_log, -pib_log,
          -passe_livre, -passe_livre_1, -passe_livre_2,
          -name_region, -dummy_pt) %>%
 mutate(QT_APTOS = log(QT_APTOS),
        PIB_PC   = log(PIB_PC)) %>%
 rename(`Groups`                             = grupos_labels,
        `log(Number of Voters)`              = QT_APTOS,
        `% Voters 60+ years-old`             = idade_60M,
        `% Education: 8 years or less `      = educacao_1,
        `log(GDP per capita)`                = PIB_PC,
        `% Biometry`                         = biometria,
        `% had 2nd round for Gov. Elections` = gov_2t,
        `Votes for Bolsonaro (1st Round)`    = votos_jair_validos_t1,
        `Votes for Bolsonaro (2nd Round)`    = votos_jair_validos_t2,
        `Density of polling stations`        = mean_dens_1000)

# ------------------------------------------------------------------------------

auto_round = function(x) {
 max_x = max(x, na.rm = T)
 
 if(max_x > 1){
  y = format(x, big.mark = ",", decimal.mark = ".", digits = 1, nsmall = 1)
 }else{
  y = round(x, digits = 3)
  y = str_pad(y, width = 5, pad = "0", side = "right")
 }
 
 y
}


tab = 
 BM_renamed %>%
 pivot_longer(cols = -Groups) %>%
 group_by(name) %>%
 mutate(value_min = min(value),
        value_max = max(value)) %>%
 ungroup() %>%
 arrange(name, Groups) %>%
 nest_by(name, Groups) %>%
 mutate(Min    = auto_round(min   (data$value)), 
        Mean   = auto_round(mean  (data$value)), 
        Median = auto_round(median(data$value)),
        Max    = auto_round(max   (data$value)), 
        SD     = auto_round(sd    (data$value)), 
        plot   = list(ggplot(data, aes(value,
                                       after_stat(ndensity))) + 
                       geom_histogram(bins = 20) +
                       xlim(data$value_min[1]-1, data$value_max[1]+1) +
                       theme_void() +
                       theme(axis.text = element_blank()) +
                       labs(x = NULL,
                            y = NULL)
                       ),
        .keep = "unused") %>%
 ungroup() %>%
 mutate(Histogram = NA) %>%
 {dat <- .
 dat %>%
  select(-plot) %>%
  gt(rowname_col   = "Groups",
     groupname_col = "name") %>%
  text_transform(locations = cells_body(c(Histogram)),
                 fn = function(x) {
                  map(dat$plot, 
                      ggplot_image, 
                      height = px(40), 
                      aspect_ratio = 2.5)
                 })
 }
 
#install.packages("webshot2") 
gtsave(tab, 
       filename = "figures/Descriptive_Table.pdf")

gtsave(tab, 
       filename = "figures/Descriptive_Table.png")

gtsave(tab, 
       filename = "figures/Descriptive_Table.rtf")

gtsave(tab, 
       filename = "figures/Descriptive_Table.tex")
