######################## findings ######################## 

#' 1st round of 2022 Vs 2018: negative non significant
#' 2nd round of 2022 Vs 2018: negative non significant


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



# recode variables ----------------------------------------------------------------------

# create code_state
df_raw[, code_state := substring(code_muni, 1, 2)]


# share of PT votes
df_raw[, votos_lula_p := sum(votos_lula) / sum(votos_validos), by = .(ANO_ELEICAO, NR_TURNO, code_muni)]
df_raw[, votos_jair_p := sum(votos_jair) / sum(votos_validos), by = .(ANO_ELEICAO, NR_TURNO, code_muni)]


# dummies for election rounds
df_raw[, turno2_dummy := fifelse(NR_TURNO==2, 1, 0)]
df_raw[, turno1_dummy := fifelse(NR_TURNO==1, 1, 0)]




# Select observations ----------------------------------------------------------------------

# keep only cities with public transport
df_raw[, passe_livre_any := max(passe_livre), by = code_muni]
df_raw <- subset(df_raw, dummy_pt==1 | passe_livre_any ==1)


# excluir cidades que SEMPRE tiveram passe livre
df_raw <- subset(df_raw, is.na(passe_livre_always))


# identify treated in the 1st and 2nd round
df_raw[, passe_livre_1 := max(NR_TURNO==1 & passe_livre==1), by = code_muni]
df_raw[, passe_livre_2 := max(NR_TURNO==2 & passe_livre==1), by = code_muni]


#### Separate 1st and 2nd rounds

df_t1 <- df_raw[NR_TURNO==1]
df_t2 <- df_raw[NR_TURNO==2]


# check balanced panel
df_t1[, table(ANO_ELEICAO, passe_livre_1)]
df_t2[, table(ANO_ELEICAO, passe_livre_2)]



df_t1[, ano_2022_dummy := ifelse(ANO_ELEICAO==2022,1,0)]
df_t2[, ano_2022_dummy := ifelse(ANO_ELEICAO==2022,1,0)]




# Descriptive analysis ----------------------------------------------------------------------





#### 1st round  ----------------------------------------------------------------------

df_2022_r1 <- subset(df_t1, ANO_ELEICAO==2022 & NR_TURNO==1)


# ipw balancing
step1_t1 <- glm(passe_livre_1 ~ QT_APTOS_log + pib_log + biometria + code_state , # name_region
             family = binomial(link = 'logit'),
             data = df_2022_r1)

summary(step1_t1)
df_2022_r1[, ipw := (passe_livre_1 / fitted(step1_t1)) + ((1 - passe_livre_1) / ( 1- fitted(step1_t1)))]

summary(df_2022_r1$ipw)




# Model 1. Average effects
# average effects with inverse probability weighting

# merge ipw info of muni to sections
setDT(df_t1)[df_2022_r1, on='code_muni', ipw := i.ipw]

# reg
step2_r1 <- fixest::feols(comparecimento~ano_2022_dummy + passe_livre_1 + ano_2022_dummy:passe_livre_1,
                       fixef = 'code_muni',
                       cluster = 'code_muni',
                       weights = ~ipw,
                       data = df_t1)
summary(step2_r1)




temp_df <- df_t1[, .(mean_value=mean(comparecimento, na.rm=T),
                           p25 = quantile(comparecimento,0.25, na.rm=T),
                           p75 = quantile(comparecimento,0.75, na.rm=T)), by=.(ANO_ELEICAO, passe_livre_1)]

ggplot() +
 geom_line(data=temp_df, aes(x=ANO_ELEICAO, y=mean_value, color=factor(passe_livre_1)),
           position = position_dodge2(width = 1)) +
 geom_pointrange(data=temp_df,
                 position = position_dodge2(width = 1),
                 show.legend = FALSE,
                 aes(x=ANO_ELEICAO, y=mean_value, color=factor(passe_livre_1),
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

df_2022_r2 <- subset(df_t2, ANO_ELEICAO==2022 & NR_TURNO==2)



# ipw balancing
step1_t2 <- glm(passe_livre_2 ~ QT_APTOS_log + pib_log + code_state + gov_2t,#+ votos_jair_muni_p +
                family = binomial(link = 'logit'),
                data = df_2022_r2)


summary(step1_t2)
df_2022_r2[, ipw := (passe_livre_2 / fitted(step1_t2)) + ((1 - passe_livre_2) / ( 1- fitted(step1_t2)))]

summary(df_2022_r2$ipw)




# Model 1. Average effects
# average effects with inverse probability weighting

# merge ipw info of muni to sections
setDT(df_t2)[df_2022_r2, on='code_muni', ipw := i.ipw]

# reg
step2_r2 <- fixest::feols(comparecimento~ano_2022_dummy + passe_livre_2 + ano_2022_dummy:passe_livre_2,
                          fixef = 'code_muni',
                          cluster = 'code_muni',
                          weights = ~ipw,
                          data = df_t2)
summary(step2_r2)




temp_df <- df_t2[, .(mean_value=mean(comparecimento, na.rm=T),
                     p25 = quantile(comparecimento,0.25, na.rm=T),
                     p75 = quantile(comparecimento,0.75, na.rm=T)), by=.(ANO_ELEICAO, passe_livre_2)]

ggplot() +
 geom_line(data=temp_df, aes(x=ANO_ELEICAO, y=mean_value, color=factor(passe_livre_2)),
           position = position_dodge2(width = 1)) +
 geom_pointrange(data=temp_df,
                 position = position_dodge2(width = 1),
                 show.legend = FALSE,
                 aes(x=ANO_ELEICAO, y=mean_value, color=factor(passe_livre_2),
                     ymin = p25,
                     ymax = p75)) +
 labs(y='Voter turnout', x = '1st round', color='Free transit') +
 scale_x_continuous( breaks =  c(2018, 2022)) +
 scale_y_continuous(labels = scales::percent) +
 scale_color_npg() +
 #scale_color_uchicago() +
 # scale_color_jama() +
 theme_classic()




