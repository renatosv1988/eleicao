
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

# separate years
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


# drop always treated (1st round)
df <- df[ passe_livre_1 != 1]
df[, table(NR_TURNO, passe_livre)]


# identify treated in turno 2
df[, passe_livre_2 := max(NR_TURNO==2 & passe_livre==1), by = code_muni]

# create dummy of 2nd rounds
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

# votes for lula in the first round
df[, votos_lula_T1_p := sum(votos_lula[which(NR_TURNO==1)]) / sum(votos_validos[which(NR_TURNO==1)]), by = .(ANO_ELEICAO, code_muni)]


# bring turnout of 2018
df_2018[, comparecimento_2018 := comparecimento]
df_2018[, votos_lula_2018 := sum(votos_lula) / sum(votos_validos), by = .(ANO_ELEICAO, NR_TURNO, code_muni)]

summary(df_2018$comparecimento)
summary(df_2018$comparecimento_2018)
summary(df_2018$votos_lula_2018)

df2 <- left_join(df,
                 df_2018[,.(code_muni, NR_TURNO, comparecimento_2018, votos_lula_2018)], 
                 by=c('NR_TURNO', 'code_muni'))


head(df2)
summary(df2$comparecimento)
summary(df2$comparecimento_2018)




# Descriptive analysis ----------------------------------------------------------------------




# ipw balancing ----------------------------------------------------------------------
summary(df2$variacao_comparecimento_2018_muni)
summary(df2$biometria)
table(df2$variacao_comparecimento_2018_muni)

l <- lm(passe_livre_2~ gov_2t + name_region +  biometria+
         QT_APTOS_log + pib_log  + votos_lula_T1_p ,
        data = df2)

summary(l)

step1 <- glm(passe_livre_2~ gov_2t + name_region +  biometria +
              QT_APTOS_log + pib_log  + votos_lula_T1_p ,
             
             family = binomial(link = 'logit'),
             data = df)


summary(step1)
setDT(df2)[, ipw := (passe_livre_2 / fitted(step1)) + ((1 - passe_livre_2) / ( 1- fitted(step1)))]
summary(df2$ipw)






# Model 1. Average effects   ----------------------------------------------------------------------
# average effects with inverse probability weighting

# reg
step2_2022 <- fixest::feols(comparecimento~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                            fixef = 'code_muni', 
                            cluster = 'code_muni',
                            weights = ~ipw,
                            data = df2)

step2_2018 <- fixest::feols(comparecimento_2018~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                            fixef = 'code_muni', 
                            cluster = 'code_muni',
                            weights = ~ipw,
                            data = df2)


summary(step2_2022)
summary(step2_2018)



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






# Model 1. Average effects  LULA  ----------------------------------------------------------------------
# average effects with inverse probability weighting

# reg
step2_2022 <- fixest::feols(votos_lula_p~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                            fixef = 'code_muni', 
                            cluster = 'code_muni',
                            weights = ~ipw,
                            data = df2)

step2_2018 <- fixest::feols(votos_lula_2018~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                            fixef = 'code_muni', 
                            cluster = 'code_muni',
                            weights = ~ipw,
                            data = df2)


summary(step2_2022)
summary(step2_2018)


