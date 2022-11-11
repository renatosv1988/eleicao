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



# modelo 2a ----------------------------------------------------------------------
# average city effects WITH inverse probability weighting

# share bolso votes
df_muni <- df2[NR_TURNO==2, .(QT_APTOS = sum(QT_APTOS),
                              # educacao_1 = mean(educacao_1),
                              gov_2t = max(gov_2t),
                              pib_log = pib_log[1L],
                              passe_livre_2 = max(passe_livre_2)), 
               by=code_muni]


step1 <- glm(passe_livre_2 ~ gov_2t + log(QT_APTOS)  + pib_log, 
             family = binomial(link = 'logit'),
             data = df_muni)

summary(step1)
df_muni$ipw <- 1 / fitted(step1)
head(df_muni)

hist(df_muni$ipw)
summary(df_muni$ipw)

# merge to bring ipw info
df2[df_muni, on='code_muni', ipw := i.ipw]

# reg
step2 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                     fixef = 'id_secao', 
                     cluster = 'code_muni',
                     weights = ~ipw,
                     data = df2)
summary(step2)












# modelo 2b ----------------------------------------------------------------------
# average city effects WITH inverse probability weighting

# average number of voters between rounds in each muni 
df2[, QT_APTOS_muni := sum(QT_APTOS)/2, by=code_muni]

step1 <- glm(passe_livre_2 ~ log(QT_APTOS_muni) + pib_log, 
             family = binomial(link = 'logit'),
             data = df2)

summary(step1)
df2$ipw <- 1 / fitted(step1)
head(df2)

hist(df2$ipw)
summary(df2$ipw)

quantile(df2$ipw, probs = seq(0,1,.001))

# reg
step2 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                       fixef = 'id_secao', 
                       cluster = 'code_muni',
                       weights = ~ipw,
                       data = subset(df2, ipw <20))
summary(step2)





# modelo 3a - heterogenidade educacao ------------------------------------------

get_reg_group <- function(l, u){  # l = 0; u = .1 

 temp_df <- df2[ educacao_1 >= l & educacao_1 <= u]

 
 step2 <- fixest::feols(comparecimento_2022~turno2_dummy + closest_dist + passe_livre_2 + turno2_dummy:passe_livre_2, 
                        fixef = 'id_secao', 
                        cluster = 'code_muni',
                        weights = ~ipw,
                        data = temp_df)

 output <- data.frame(l = l,
                      u = u,
                      coef = step2$coeftable[2, 1],
                      se = step2$coeftable[2, 2]
                      )

 return(output)
}




output <- purrr::map2(.x = seq(0, .9, by=.1),
             .y =seq(0.1, 1, by=.1),
             .f = get_reg_group) |> rbindlist()


ggplot(data = output, aes(x= l, y=coef)) +
 geom_line() +
 geom_ribbon(aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
 geom_hline(yintercept = 0)









# modelo 3b - heterogenidade distancia -----------------------------------------
hist(df2$num_0500)
hist(df2$num_1000)
summary(df2$num_1000)

# discretaize distance
my_breaks <- seq(0, 190, by=10)
my_breaks <- c(my_breaks, Inf)
df2[, num_1000_cat := cut(num_1000,
                          breaks= my_breaks,
                          labels= my_breaks[-1])]

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
summary(df2$num_1000)

# discretaize distance
my_breaks <- seq(0, 190, by=10)
my_breaks <- c(my_breaks, Inf)
df2[, num_1000_cat := cut(num_1000,
                          breaks= my_breaks,
                          labels= my_breaks[-1])]

table(df2$num_1000_cat, useNA = 'always')


reg_group_dist_edu <- function(i){  # i =50
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
                            data = subset(temp_df, educacao_1 <= 0.3)
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
 geom_line() +
 geom_ribbon(aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
 geom_hline(yintercept = 0)





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









# modelo 5 Sant'anna & Zhao ----------------------------------------------------------------------
# average city effects WITH inverse probability weighting

# create numeric id
df2[, id := .GRP, by = id_secao ]
head(df2)

# average number of voters between rounds in each muni 
df2[, QT_APTOS_muni := sum(QT_APTOS)/2, by=code_muni]

output_drdid_low <- drdid( yname = 'comparecimento_2022', 
                       tname = 'turno2_dummy', 
                       idname = 'id',
                       dname = 'passe_livre_2', 
                       xformla = ~log(QT_APTOS_muni) + pib_log,
                       data = subset(df2, educacao_1 > .85))

summary(output_drdid_low)




