#' checar se o ipw mantem balancing de municipois por diversas caracteristicas (demanda do renato)
#' vimos q sim


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

df_raw <- fread("../../data/base_DiD2022_2018_muni.csv")

# 666 só 2022
df_2022 <- df_raw[ANO_ELEICAO==2022]
df_2018 <- df_raw[ANO_ELEICAO==2018]


# Select observations ----------------------------------------------------------------------

# keep only cities with public transport
df_2022[, passe_livre_any := max(passe_livre), by = code_muni]
df_2022 <- subset(df_2022, dummy_pt==1 | passe_livre_any ==1)


# excluir cidades que SEMPRE tiveram passe livre
df <- subset(df_2022, is.na(passe_livre_always))


# identify treated in the 1st round
df[, passe_livre_1 := max(NR_TURNO==1 & passe_livre==1), by = code_muni]
table(df$passe_livre_1)

# drop always treated (1st round)
df <- df[ passe_livre_1 != 1]
df[, table(NR_TURNO, passe_livre)]


# identify treated in turno 2
df[, passe_livre_2 := max(passe_livre==1), by = code_muni]
df[, turno2_dummy := fifelse(NR_TURNO==2, 1, 0)]


# check balanced panel
df[, table(NR_TURNO, passe_livre_2)]





# recode variables ----------------------------------------------------------------------

setDT(df)

# create code_state
df[, code_state := substring(code_muni, 1, 2)]


# share of PT votes
df[, votos_lula_p := sum(votos_lula) / sum(votos_validos), by = .(ANO_ELEICAO, NR_TURNO, code_muni)]
df[, votos_jair_p := sum(votos_jair) / sum(votos_validos), by = .(ANO_ELEICAO, NR_TURNO, code_muni)]



# bring turnout of 2018
df_2018[, comparecimento_2018 := comparecimento]
summary(df_2018$comparecimento)
summary(df_2018$comparecimento_2018)

df2 <- left_join(df,
                 df_2018[,.(code_muni, NR_TURNO, comparecimento_2018)], 
                 by=c('NR_TURNO', 'code_muni'))


head(df2)
summary(df2$comparecimento_2018)

# Descriptive analysis ----------------------------------------------------------------------




# ipw balancing ----------------------------------------------------------------------
summary(df2$variacao_comparecimento_2018_muni)
summary(df2$biometria)
table(df2$variacao_comparecimento_2018_muni)

l <- lm(passe_livre_2~ gov_2t + name_region +  biometria+
         QT_APTOS_log + pib_log  + votos_lula_p ,
        data = df2)

summary(l)

step1 <- glm(passe_livre_2~ gov_2t + name_region +  biometria+
              QT_APTOS_log + pib_log  + votos_lula_p ,
             
             family = binomial(link = 'logit'),
             data = df)


summary(step1)
setDT(df2)[, ipw := (passe_livre_2 / fitted(step1)) + ((1 - passe_livre_2) / ( 1- fitted(step1)))]
summary(df2$ipw)






# Model 1. Average effects   ----------------------------------------------------------------------
# average effects with inverse probability weighting

# reg
step2_r2 <- fixest::feols(comparecimento_2018~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                          fixef = 'code_muni', 
                          cluster = 'code_muni',
                          weights = ~ipw,
                          data = df2)
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




