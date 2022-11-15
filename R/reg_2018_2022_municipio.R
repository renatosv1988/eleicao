#' checar se o ipw mantem balancing de municipois por diversas caracteristicas (demanda do renato)
#' vimos q sim

#' renato
#' calcular vote share to bozo e do lugar nos turnos 1 e 2 de 2018



#' renato concertar script q gera base de 2018 - 2022
#' 
#' rafa quebrar densidade por quantil
#' rafa setor urbano

######################## findings ######################## 
#' Municipio
#' sem efeito no 1o e nem no 2o

#' Seção
#' nao tem efeito medio
#' tem efeito heterogeneo pequeno mais pobres e regioes de baixa densidade (pelo menos no 2o)


#' efeito grande e homogeno sobre o resultado eleitoral 1%
# chega a 3% pra mais densas
# igual socioeconomico




library(data.table)
library(dplyr)
library(fixest)
library(DRDID)
library(ggplot2)
library(purrr)
library(modelsummary)
library(ggplot2)
library(tidylog)

options(scipen = 999)


# read data ----------------------------------------------------------------------

df <- fread("../../data/base_DiD2022_2018_muni.csv")


# Select observations ----------------------------------------------------------------------

# excluir seções de cidades sem sistema de ônibus
df <- subset(df, dummy_pt==1)
df[, table(NR_TURNO, dummy_pt)]
df[, table(NR_TURNO, passe_livre)]


# check balanced panel
df[, table(ANO_ELEICAO, passe_livre_t1)]
df[, table(ANO_ELEICAO, passe_livre_t2)]




# recode variables ----------------------------------------------------------------------


# add regions
regions <- geobr::read_region()
regions$geom <- NULL

# add region to section data
df[, code_region := substring(code_muni, 1, 1) |> as.numeric() ]
df <- left_join(df, regions, by=c('code_region'))
table(df$name_region, df$passe_livre_t2)


setDT(df)
df[, pib_log := log(PIB_PC)]


# create code_state
df[, code_state := substring(code_muni, 1, 2)]



# Descriptive analysis ----------------------------------------------------------------------

summary(df)


# balancing before ipw 
a <- df[,   .(QT_APTOS          = weighted.mean(x=QT_APTOS),
                   QT_APTOS_log      = weighted.mean(x=QT_APTOS_log),
              #      votos_jair_muni_p = weighted.mean(x=votos_jair_muni_p),
                   mean_dist         = weighted.mean(x=mean_dist, na.rm=T),
                   mean_dens_1000    = weighted.mean(x=mean_dens_1000),
                   educacao_1        = weighted.mean(x=educacao_1),
              #     gov_2t             = weighted.mean(x=gov_2t),
                   pib_log           = weighted.mean(x=pib_log)),
             by=passe_livre_2]



# transpose
b <- data.table::transpose(a)

# get row and colnames in order
setnames(b, rownames(a))
b$variable <- colnames(a)
setcolorder(b, 'variable')


#### 1st round  ----------------------------------------------------------------------

df_2018_r1 <- subset(df, ANO_ELEICAO==2018 & NR_TURNO==1)
table(df_2018_r1$NR_TURNO, df_2018_r1$ANO_ELEICAO)


# ipw balancing
step1_t1 <- glm(passe_livre_t1 ~ QT_APTOS_log + pib_log + code_state + gov_2t,#+ votos_jair_muni_p + gov_2t , # + mean_dens_1000, 
             family = binomial(link = 'logit'),
             data = df_2018_r1)

summary(step1_t1)
df_2018_r1[, ipw := (passe_livre_t1 / fitted(step1_t1)) + ((1 - passe_livre_t1) / ( 1- fitted(step1_t1)))]

hist(df_2018_r1$ipw)
summary(df_2018_r1$ipw)


# # balancing AFTER ipw 
# a <- df_muni[,   .(QT_APTOS          = weighted.mean(x=QT_APTOS, w = ipw),
#                    QT_APTOS_log      = weighted.mean(x=QT_APTOS_log, w = ipw),
#                    votos_jair_muni_p = weighted.mean(x=votos_jair_muni_p, w = ipw),
#                    mean_dist         = weighted.mean(x=mean_dist, w = ipw, na.rm=T),
#                    mean_dens_1000    = weighted.mean(x=mean_dens_1000, w = ipw),
#                    educacao_1        = weighted.mean(x=educacao_1, w = ipw),
#                    gov_2t            = weighted.mean(x=gov_2t, w = ipw),
#                    pib_log           = weighted.mean(x=pib_log, w = ipw)),
#              by=passe_livre_2]
# 
# 
# 
# # transpose
# c <- data.table::transpose(a)
# 
# # get row and colnames in order
# setnames(c, rownames(a))
# c$variable <- colnames(a)
# setcolorder(c, 'variable')
# 


# Model 1. Average effects
# average effects with inverse probability weighting

df_round1 <- df[NR_TURNO==1,]
df_round1 <- left_join(df_round1, df_2018_r1[, .(code_muni, ipw)], by = 'code_muni')

# reg
step2_r1 <- fixest::feols(comparecimento~ANO_ELEICAO + passe_livre_t1 + ANO_ELEICAO:passe_livre_t1, 
                       fixef = 'code_muni', 
                       cluster = 'code_muni',
                       weights = ~ipw,
                       data = df_round1)
summary(step2_r1)


df_round1[, mean(comparecimento), by = .(passe_livre_t1, ANO_ELEICAO)]




modelsummary::modelsummary(step2_r1, stars = T)


temp_df <- df_round1[, .(mean_value=mean(comparecimento, na.rm=T),
                           p25 = quantile(comparecimento,0.25, na.rm=T),
                           p75 = quantile(comparecimento,0.75, na.rm=T)), by=.(ANO_ELEICAO, passe_livre_t1)]

ggplot() +
 geom_line(data=temp_df, aes(x=ANO_ELEICAO, y=mean_value, color=factor(passe_livre_t1)),
           position = position_dodge2(width = 1)) +
 geom_pointrange(data=temp_df,
                 position = position_dodge2(width = 1),
                 show.legend = FALSE,
                 aes(x=ANO_ELEICAO, y=mean_value, color=factor(passe_livre_t1),
                     ymin = p25,
                     ymax = p75)) +
 labs(y='Voter turnout', x = '1st round', color='Free transit') +
 scale_x_continuous( breaks =  c(2018, 2022)) +
 scale_y_continuous(labels = scales::percent) +
  scale_color_npg() +
 #scale_color_uchicago() +
 # scale_color_jama() +
 theme_classic()




#### 2nd round  ----------------------------------------------------------------------

df_2018_r2 <- subset(df, ANO_ELEICAO==2018 & NR_TURNO==2)
table(df_2018_r2$NR_TURNO, df_2018_r2$ANO_ELEICAO)


# ipw balancing
step1_t2 <- glm(passe_livre_t2 ~ QT_APTOS_log + pib_log + code_state + gov_2t,#+ votos_jair_muni_p + gov_2t , # + mean_dens_1000, 
                family = binomial(link = 'logit'),
                data = df_2018_r2)

summary(step1_t2)
df_2018_r2[, ipw := (passe_livre_t2 / fitted(step1_t2)) + ((1 - passe_livre_t2) / ( 1- fitted(step1_t2)))]

hist(df_2018_r2$ipw)
summary(df_2018_r2$ipw)


# # balancing AFTER ipw 
# a <- df_muni[,   .(QT_APTOS          = weighted.mean(x=QT_APTOS, w = ipw),
#                    QT_APTOS_log      = weighted.mean(x=QT_APTOS_log, w = ipw),
#                    votos_jair_muni_p = weighted.mean(x=votos_jair_muni_p, w = ipw),
#                    mean_dist         = weighted.mean(x=mean_dist, w = ipw, na.rm=T),
#                    mean_dens_1000    = weighted.mean(x=mean_dens_1000, w = ipw),
#                    educacao_1        = weighted.mean(x=educacao_1, w = ipw),
#                    gov_2t            = weighted.mean(x=gov_2t, w = ipw),
#                    pib_log           = weighted.mean(x=pib_log, w = ipw)),
#              by=passe_livre_2]
# 
# 
# 
# # transpose
# c <- data.table::transpose(a)
# 
# # get row and colnames in order
# setnames(c, rownames(a))
# c$variable <- colnames(a)
# setcolorder(c, 'variable')
# 


# Model 1. Average effects
# average effects with inverse probability weighting

df_round2 <- df[NR_TURNO==2,]
df_round2 <- left_join(df_round2, df_2018_r2[, .(code_muni, ipw)], by = 'code_muni')

# reg
step2_r2 <- fixest::feols(comparecimento~ANO_ELEICAO + passe_livre_t2 + ANO_ELEICAO:passe_livre_t2, 
                          fixef = 'code_muni', 
                          cluster = 'code_muni',
                          weights = ~ipw,
                          data = df_round2)
summary(step2_r2)



# output model table
model_list <- list(step2_r1,  step2_r2)
names(model_list) <- c('round_1', 'round_2')
modelsummary::modelsummary(model_list, stars = T)

# plot
temp_df <- df_round2[, .(mean_value=mean(comparecimento, na.rm=T),
                         p25 = quantile(comparecimento,0.25, na.rm=T),
                         p75 = quantile(comparecimento,0.75, na.rm=T)), by=.(ANO_ELEICAO, passe_livre_t2)]

ggplot() +
 geom_line(data=temp_df, aes(x=ANO_ELEICAO, y=mean_value, color=factor(passe_livre_t2)),
           position = position_dodge2(width = 1)) +
 geom_pointrange(data=temp_df,
                 position = position_dodge2(width = 1),
                 show.legend = FALSE,
                 aes(x=ANO_ELEICAO, y=mean_value, color=factor(passe_livre_t2),
                     ymin = p25,
                     ymax = p75)) +
 labs(y='Voter turnout', x = '2nd round', color='Free transit') +
 scale_x_continuous( breaks =  c(2018, 2022)) +
 scale_y_continuous(labels = scales::percent) +
 scale_color_npg() +
 #scale_color_uchicago() +
 # scale_color_jama() +
 theme_classic()




# Model 2. by REGIAO round 1 ------------------------------------------

reg_region1 <- function(r){  # r = 'Norte'
 
 df_2018_r1 <- subset(df, ANO_ELEICAO==2018 & NR_TURNO==1 & name_region == r)
 table(df_2018_r1$NR_TURNO, df_2018_r1$ANO_ELEICAO)
 
 
 # ipw balancing
 step1_t1 <- glm(passe_livre_t1 ~ QT_APTOS_log + pib_log + code_state + gov_2t,#+ votos_jair_muni_p + gov_2t , # + mean_dens_1000, 
                 family = binomial(link = 'logit'),
                 data = df_2018_r1)
 
 summary(step1_t1)
 df_2018_r1[, ipw := (passe_livre_t1 / fitted(step1_t1)) + ((1 - passe_livre_t1) / ( 1- fitted(step1_t1)))]
 
 
 # step 2
 df_round1 <- df[NR_TURNO==1 & name_region == r,]
 df_round1 <- left_join(df_round1, df_2018_r1[, .(code_muni, ipw)], by = 'code_muni')
 
 # reg
 step2_r1 <- fixest::feols(comparecimento~ANO_ELEICAO + passe_livre_t1 + ANO_ELEICAO:passe_livre_t1, 
                           fixef = 'code_muni', 
                           cluster = 'code_muni',
                           weights = ~ipw,
                           data = df_round1)
 
 
 
 output <- data.frame(name_region = r,
                      round = 1,
                      coef = step2_r1$coeftable[2, 1],
                      se = step2_r1$coeftable[2, 2])
 
 return(output)
}

output_did_region1 <- purrr::map(.x = unique(df$name_region),
                                .f = reg_region1) |> rbindlist()


ggplot() +
 geom_point(data = output_did_region1, aes(x= name_region, y=coef)) +
 geom_pointrange(data=output_did_region1,
                 # show.legend = FALSE,
                 aes(x=name_region, y=coef,
                     ymin = coef - 1.96*se,
                     ymax = coef + 1.96*se)) +
 geom_hline(yintercept = 0, color='gray20') +
 theme_classic()





# Model 2. by REGIAO round 2 ------------------------------------------

reg_region2 <- function(r){  # r = 'Norte'
 
 df_2018_r2 <- subset(df, ANO_ELEICAO==2018 & NR_TURNO==2 & name_region == r)
 table(df_2018_r2$NR_TURNO, df_2018_r2$ANO_ELEICAO)
 
 
 # ipw balancing
 step1_t2 <- glm(passe_livre_t2 ~ QT_APTOS_log + pib_log + code_state + gov_2t,#+ votos_jair_muni_p + gov_2t , # + mean_dens_1000, 
                 family = binomial(link = 'logit'),
                 data = df_2018_r2)
 
 summary(step1_t2)
 df_2018_r2[, ipw := (passe_livre_t2 / fitted(step1_t2)) + ((1 - passe_livre_t2) / ( 1- fitted(step1_t2)))]
 
 
 # step 2
 df_round2 <- df[NR_TURNO==2 & name_region == r,]
 df_round2 <- left_join(df_round2, df_2018_r2[, .(code_muni, ipw)], by = 'code_muni')
 
 # reg
 step2_r2 <- fixest::feols(comparecimento~ANO_ELEICAO + passe_livre_t2 + ANO_ELEICAO:passe_livre_t2, 
                           fixef = 'code_muni', 
                           cluster = 'code_muni',
                           weights = ~ipw,
                           data = df_round2)
 
 
  
 output <- data.frame(name_region = r,
                      round = 2,
                      coef = step2_r2$coeftable[2, 1],
                      se = step2_r2$coeftable[2, 2])
 
 return(output)
}

output_did_region2 <- purrr::map(.x = unique(df$name_region),
                                .f = reg_region2) |> rbindlist()


ggplot() +
 geom_point(data = output_did_region2, aes(x= name_region, y=coef)) +
 geom_pointrange(data=output_did_region2,
                 # show.legend = FALSE,
                 aes(x=name_region, y=coef,
                     ymin = coef - 1.96*se,
                     ymax = coef + 1.96*se)) +
 geom_hline(yintercept = 0, color='gray20') +
 theme_classic()


modelsummary::modelsummary(r1, stars = T)




