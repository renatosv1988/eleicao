#' rogerio
#' como captar efeitos heterogeneos
#' 

#' rafa
#' calcular distancias
#' resolver missing de educacao_1
#' checar se mudou endereco de secoes entre 2018 e 2022

#' renato
#' calcular vote share to bozo e do lugar nos turnos 1 e 2

#' parear secoes entre 2018 e 2022 ????


library(data.table)
library(dplyr)
library(fixest)
library(DRDID)
library(ggplot2)
library(purrr)

options(scipen = 999)

# ler base de eleições
BD <- fread("../../data/base_DiD2022_secoes.csv")


# convert selected columns to log
BD[, pib_log := log(PIB_PC)]
BD[, QT_APTOS_log := log(QT_APTOS)]

# excluir seções de cidades sem sistema de ônibus
BD <- subset(BD, dummy_pt==1)
BD[, table(NR_TURNO, passe_livre)]


# identify treated in the 1st round
BD[, passe_livre_1 := max(NR_TURNO==1 & passe_livre==1), by = id_secao]
table(BD$passe_livre_1)

BD[, table(NR_TURNO, passe_livre_1)]


BD[NR_TURNO==2, table(passe_livre_1, passe_livre)]

# drop always treated
df2 <- BD[ passe_livre_1 != 1]
df2[, table(NR_TURNO, passe_livre_1)]


# identify treated in turno 2
df2[, passe_livre_2 := max(passe_livre==1), by = id_secao]
df2[, turno2_dummy := fifelse(NR_TURNO==2, 1, 0)]


df2[, table(passe_livre_2, turno2_dummy)]

summary(df2$comparecimento_2022)




# modelo 1 ----------------------------------------------------------------------
# average city effects (no inverse probability weighting)

dd1 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
             fixef = 'id_secao', 
             cluster = 'code_muni',
             data = df2)

summary(dd1)

# interagindo com a variação observada em 2018
#dd1x <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2
#                      + turno2_dummy:variacao_comparecimento_2018, 
#                     fixef = 'id_secao', 
#                     cluster = 'code_muni',
#                     data = df2)
#summary(dd1x)
#etable(dd1, dd1x)


# modelo 2a ----------------------------------------------------------------------
# average city effects WITH MUNI inverse probability weighting
summary(df2$QT_APTOS)
summary(df2$pib_log)
summary(df2$votos_jair)
summary(df2$votos_total)
summary(df2$gov_2t)
summary(df2$passe_livre_2)
summary(df2$dist_sede)
summary(df2$num_0500)
summary(df2$educacao_1)


# # remove NAs
# df2 <- df2[!is.na(dist_sede)]

# 6666666666666666 fazer pra 1o turno
df_muni <- df2[NR_TURNO==2, .(QT_APTOS = sum(QT_APTOS, na.rm=T),
                              votos_jair = sum(votos_jair) / sum(votos_total),
                              mean_dist = mean(dist_sede, na.rm=T),
                              mean_dens = mean(num_1000, na.rm=T),
                              educacao_1 = mean(educacao_1, na.rm=T),
                              SG_UF = SG_UF[1L],
                              gov_2t = max(gov_2t),
                              pib_log = pib_log[1L],
                              passe_livre_2 = max(passe_livre_2)), 
               by=code_muni]

# df_muni <- na.omit(df_muni)     

lm(passe_livre_2 ~ gov_2t + log(QT_APTOS) + votos_jair + pib_log + mean_dens,
             data = df_muni) |> summary()

step1 <- glm(passe_livre_2 ~ gov_2t + log(QT_APTOS) + votos_jair + pib_log + mean_dens, 
             family = binomial(link = 'logit'),
             data = df_muni)

summary(step1)
df_muni$ipw <- 1 / fitted(step1)

hist(df_muni$ipw)
summary(df_muni$ipw)

# merge bring ipw info to sections
df2[df_muni, on='code_muni', ipw := i.ipw]

# reg
step2 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                     fixef = 'id_secao', 
                     cluster = 'code_muni',
                     weights = ~ipw,
                     data = df2)
summary(step2)












# modelo 2b ----------------------------------------------------------------------
# average section effects WITH inverse probability weighting

# average number of voters between rounds in each muni 
df2[, QT_APTOS_muni := sum(QT_APTOS)/2, by=code_muni]

lm(passe_livre_2 ~ gov_2t + log(QT_APTOS_muni) + votos_jair + pib_log, 
   data = df2) |> summary()

step1 <- glm(passe_livre_2 ~ log(QT_APTOS_muni) + votos_jair + pib_log, 
             family = binomial(link = 'logit'),
             data = df2)

summary(step1)
df2$ipw <- 1 / fitted(step1)
head(df2)

hist(df2$ipw)
summary(df2$ipw)


# reg
step2 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                       fixef = 'id_secao', 
                       cluster = 'code_muni',
                       weights = ~ipw,
                       data = subset(df2, ipw <20))
summary(step2)




# modelo 3a - heterogenidade educacao ------------------------------------------

# discretize edu
my_breaks <- seq(0, 0.7, by=.1)
my_breaks <- c(my_breaks, 1)
df2[, educacao_1_cat := cut(educacao_1,
                            breaks= my_breaks,
                            labels= my_breaks[-1])]

table(df2$educacao_1_cat, useNA = 'always')

get_reg_edu <- function(e){  # e = .4
 
 temp_df <- df2[ educacao_1_cat == e]
 
 
 step2 <- fixest::feols(comparecimento_2022~turno2_dummy + closest_dist + passe_livre_2 + turno2_dummy:passe_livre_2, 
                        fixef = 'id_secao', 
                        cluster = 'code_muni',
                        weights = ~ipw,
                        data = temp_df)
 
 output <- data.frame(e = e,
                      coef = step2$coeftable[2, 1],
                      se = step2$coeftable[2, 2]
 )
 
 return(output)
}

output3a <- purrr::map(.x = my_breaks[-1],
                       .f = get_reg_edu) |> rbindlist()

ggplot(data = output3a, aes(x= e, y=coef)) +
 geom_ribbon(aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.5, fill='gray') +
 geom_line() +
 geom_hline(yintercept = 0, color='gray20') +
 labs(x= 'Percentage of low\nsocioeconomic individuals') +
 scale_x_continuous(labels = scales::percent) +
 theme_classic()






# modelo 3b - heterogenidade densidade de secoes -----------------------------------------
hist(df2$num_0500)
hist(df2$num_1000)
summary(df2$num_1000)

# discretize density
my_breaks <- seq(0, 190, by=10)
my_breaks <- c(my_breaks, Inf)
df2[, num_1000_cat := cut(num_1000,
                          breaks= my_breaks,
                          labels= my_breaks[-1],
                          include.lowest = T
                          )
    ]



df2[num_1000 == 0, .(num_1000, num_1000_cat)]
df2[num_1000_cat==Inf, .(num_1000, num_1000_cat)]

table(df2$num_1000_cat, useNA = 'always')


reg_group_dist <- function(i){  # i =50
 message(i)
 temp_df <- df2[ num_1000_cat == i]
 
 
 step2 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                        fixef = 'id_secao', 
                        cluster = 'code_muni',
                        weights = ~ipw,
                        data = temp_df)
 
 output <- data.frame(i = i,
                      coef = step2$coeftable[2, 1],
                      se = step2$coeftable[2, 2]
 )
 
 return(output)
}


# run regressions
output3b <- purrr::map(.x = my_breaks[-1],
                      .f = reg_group_dist) |> rbindlist()


ggplot(data = output3b, aes(x= i, y=coef)) +
 geom_line() +
 geom_ribbon(aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
 geom_hline(yintercept = 0)






# modelo 3c - heterogenidade distancia e edu -----------------------------------------
hist(df2$num_0500)
hist(df2$num_1000)
hist(df2$num_3000)
summary(df2$num_3000)

# discretize distance
my_breaks <- seq(0, 140, by=10)
my_breaks <- c(my_breaks, Inf)
df2[, num_1000_cat := cut(num_1000,
                          breaks= my_breaks,
                          labels= my_breaks[-1])]

table(df2$num_1000_cat, useNA = 'always')


reg_group_dist_edu <- function(i){  # i =50 i = Inf
 message(i)
 temp_df <- df2[ num_1000_cat == i]
 
 
 step2_low <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                        fixef = 'id_secao', 
                        cluster = 'code_muni',
                        weights = ~ipw,
                        data = subset(temp_df, educacao_1 >=  0.5)
                        )
 
 step2_high <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                            fixef = 'id_secao', 
                            cluster = 'code_muni',
                            weights = ~ipw,
                            data = subset(temp_df, educacao_1 < 0.5)
                            )
 
 output_low <- data.frame(i = i,
                          group = 'low',
                          coef = step2_low$coeftable[2, 1],
                          se = step2_low$coeftable[2, 2])
 
 output_high <- data.frame(i = i,
                           group = 'high',
                           coef = step2_high$coeftable[2, 1],
                           se = step2_high$coeftable[2, 2])
 
 output <- rbind(output_low, output_high)
 return(output)
}


# run regressions
output3c <- purrr::map(.x = my_breaks[-1],
                     .f = reg_group_dist_edu) |> rbindlist()


ggplot(data = output3c, aes(x= i, y=coef, color=group, fill=group)) +
 geom_ribbon(aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
 geom_line() +
 geom_hline(yintercept = 0, color='gray20') +
 labs(x= 'density') +
 theme_classic()





# modelo 3d - heterogenidade educacao e dist ------------------------------------------

# discretize edu
my_breaks <- seq(0, 0.9, by=.1)
my_breaks <- c(my_breaks, 1)
df2[, educacao_1_cat := cut(educacao_1,
                          breaks= my_breaks,
                          labels= my_breaks[-1])]

table(df2$educacao_1_cat, useNA = 'always')


reg_group_edu_dist <- function(e, d){  # e = .5 ; d = 100

 message(paste0(e, ' - ', d))
 temp_df <- df2[ educacao_1_cat == e]
 
 
 step2_above <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                            fixef = 'id_secao', 
                            cluster = 'id_secao',
                            weights = ~ipw,
                            data = subset(temp_df, num_1000 >=  d)
                            )
 
 step2_below <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                             fixef = 'id_secao', 
                             cluster = 'id_secao',
                             weights = ~ipw,
                             data = subset(temp_df, num_1000 < d)
                             )
 
 output_above <- data.frame(e = e,
                            d = d,
                          group = 'above',
                          coef = step2_above$coeftable[2, 1],
                          se = step2_above$coeftable[2, 2])
 
 output_below <- data.frame(e = e,
                            d = d,
                            group = 'below',
                            coef = step2_below$coeftable[2, 1],
                            se = step2_below$coeftable[2, 2])
  
 output <- rbind(output_above, output_below)
 return(output)
}



# run regressions
all_combinations <- expand.grid(my_breaks[-length(my_breaks)][-1],
                                seq(10, 90, by=10))

output3d <- purrr::map2(.x = all_combinations$Var1,
                        .y = all_combinations$Var2,
                        .f = reg_group_edu_dist) |> rbindlist()


ggplot(data = output3d, aes(x= e, y=coef, color=group, fill=group)) +
 geom_ribbon(aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
 geom_line() +
 geom_hline(yintercept = 0, color='gray20') +
 labs(x= 'Percentage of low\nsocioeconomic individuals') +
 facet_wrap(~d)+
 theme_classic()


ggplot(data = output3d, aes(x= e, y=coef, color=group, fill=group)) +
 geom_line() +
 facet_wrap(.~d)





# modelo 4 Sant'anna & Zhao ----------------------------------------------------------------------
# average city effects WITH inverse probability weighting

# create numeric id
df2[, id := .GRP, by = id_secao ]
head(df2)


# average number of voters between rounds in each muni 
df2[, QT_APTOS_muni := sum(QT_APTOS)/2, by=code_muni]

output_drdid <- drdid( yname = 'comparecimento_2022', 
                  tname = 'turno2_dummy', 
                  idname = 'id',
                  dname = 'passe_livre_2', 
                  xformla = ~log(QT_APTOS_muni) + pib_log,
                  data = df2)

summary(output_drdid)




# 6666
# 1. inverse probability weighting
# 2. outcome regression
# 3. doubly robust
# 



# modelo 5 Sant'anna & Zhao ----------------------------------------------------------------------
# average city effects WITH inverse probability weighting

# create numeric id
df2[, id := .GRP, by = id_secao ]
head(df2)

# average number of voters between rounds in each muni 
df2[, QT_APTOS_muni := sum(QT_APTOS)/2, by=code_muni]

output_drdid <- drdid(yname = 'comparecimento_2022', 
                       tname = 'turno2_dummy', 
                       idname = 'id',
                       dname = 'passe_livre_2', 
                       xformla = ~log(QT_APTOS_muni) + pib_log,
                       data = df2)

summary(output_drdid)

library(did)

# average number of voters between rounds in each muni 
df2[, QT_APTOS_muni_log := log(sum(QT_APTOS)/2), by=code_muni]

# create numeric id
df2[, id := .GRP, by = id_secao ]

output_did_clust <- did::att_gt(yname = 'comparecimento_2022', 
                          tname = 'turno2_dummy', 
                          idname = 'id',
                          gname = 'passe_livre_2', 
                          xformla = ~QT_APTOS_muni_log + pib_log + educacao_1,
                          clustervars = 'code_muni',
                          cores = 4,
                          data = df2)


summary(output_did)
summary(output_did_clust)


