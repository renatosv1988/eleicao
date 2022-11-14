

#' checar se o ipw mantem balancing de municipois por diversas caracteristicas (demanda do renato)

#' renato
#' calcular vote share to bozo e do lugar nos turnos 1 e 2

#' parear secoes entre 2018 e 2022 ????


library(data.table)
library(dplyr)
library(fixest)
library(DRDID)
library(ggplot2)
library(purrr)
library(modelsummary)
library(ggplot2)

options(scipen = 999)


# read data ----------------------------------------------------------------------

BD <- fread("../../data/base_DiD2022_secoes.csv")



# Select observations ----------------------------------------------------------------------

# excluir seções de cidades sem sistema de ônibus
BD <- subset(BD, dummy_pt==1)
BD[, table(NR_TURNO, dummy_pt)]
BD[, table(NR_TURNO, passe_livre)]


# identify treated in the 1st round
BD[, passe_livre_1 := max(NR_TURNO==1 & passe_livre==1), by = id_secao]
table(BD$passe_livre_1)

BD[, table(NR_TURNO, passe_livre_1)]

BD[NR_TURNO==2, table(passe_livre_1, passe_livre)]

# drop always treated
df_sections <- BD[ passe_livre_1 != 1]
df_sections[, table(NR_TURNO, passe_livre_1)]


# identify treated in turno 2
df_sections[, passe_livre_2 := max(passe_livre==1), by = id_secao]
df_sections[, turno2_dummy := fifelse(NR_TURNO==2, 1, 0)]





# recode variables ----------------------------------------------------------------------

# create unique id for voting place
df_sections[, id_place := paste(code_muni, NR_ZONA, NR_LOCAL_VOTACAO)]
head(df_sections$id_place)


# aggregate by voting place
df_place <- df_sections[, .(QT_APTOS = sum(QT_APTOS),
                          QT_APTOS_log = log(sum(QT_APTOS)),
                          pib_log = log(PIB_PC[1L]),
                          votos_jair = sum(votos_jair),
                          votos_lula = sum(votos_lula),
                          votos_total = sum(votos_total),
                          num_1000 = mean(num_1000),
                          passe_livre_2 = passe_livre_2[1L],
                          educacao_1 = weighted.mean(educacao_1, w=QT_APTOS, na.rm=T),
                          comparecimento_2022 = weighted.mean(comparecimento_2022, w=QT_APTOS)
                          ), 
                      by = .(id_place, turno2_dummy, code_muni)]

head(df_place)

# discretize edu
my_breaks <- seq(0, 0.7, by=.1)
my_breaks <- c(my_breaks, 1)
df_place[, educacao_1_cat := cut(educacao_1,
                            breaks= my_breaks,
                            labels= my_breaks[-1])]

table(df_place$educacao_1_cat, useNA = 'always')


# education quantiles
df_place[, educacao_1_decile := cut(educacao_1,
                                      breaks = quantile(educacao_1, na.rm=T,
                                                        probs = seq(0, 1 , by = .1)),
                                      include.lowest = TRUE,
                                      ordered_result = TRUE)
]

df_place[, educacao_1_quintile := cut(educacao_1,
                                 breaks = quantile(educacao_1, na.rm=T,
                                                   probs = seq(0, 1 , by = .2)),
                                 include.lowest = TRUE,
                                 ordered_result = TRUE)
                                 ]


table(df_place$educacao_1_quintile)
table(df_place$educacao_1_decile)


# discretize density by 10
my_breaks <- seq(0, 150, by=10)
my_breaks <- c(my_breaks, Inf)
df_place[, num_1000_cat10 := cut(num_1000,
                          breaks= my_breaks,
                          labels= my_breaks[-1],
                          include.lowest = T)]


table(df_place$num_1000_cat10, useNA = 'always')


# discretize density by 20
my_breaks <- seq(0, 140, by=20)
my_breaks <- c(my_breaks, Inf)
df_place[, num_1000_cat20 := cut(num_1000,
                            breaks= my_breaks,
                            labels= my_breaks[-1],
                            include.lowest = T)]


table(df_place$num_1000_cat20, useNA = 'always')

# add regions
regions <- geobr::read_region()
regions$geom <- NULL

# add region to place data
df_place[, code_region := substring(code_muni, 1, 1) |> as.numeric() ]
df_place <- left_join(df_place, regions, by=c('code_region'))
table(df_place$name_region, df_place$passe_livre_2)

# add region to section data
df_sections[, code_region := substring(code_muni, 1, 1) |> as.numeric() ]
df_sections <- left_join(df_sections, regions, by=c('code_region'))
table(df_sections$name_region, df_sections$passe_livre_2)


# aggregate variables at muni level
df_muni <- df_sections[, .(QT_APTOS = sum(QT_APTOS[which(NR_TURNO==2)], na.rm=T),
                   QT_APTOS_log = log(sum(QT_APTOS[which(NR_TURNO==2)], na.rm=T)),
                   votos_jair_muni_p = sum(votos_jair[which(NR_TURNO==1)]) / sum(votos_total[which(NR_TURNO==1)]),
                   mean_dist = mean(dist_sede, na.rm=T),
                   mean_dens_1000 = mean(num_1000, na.rm=T),
                   educacao_1 = weighted.mean(x=educacao_1, w=QT_APTOS, na.rm=T),
                   SG_UF = SG_UF[1L],
                   name_region = name_region[1L],
                   gov_2t = max(gov_2t),
                   pib_log = log(PIB_PC)[1L],
                   passe_livre_2 = max(passe_livre_2)), 
               by=code_muni]


head(df_muni)





# Descriptive analysis ----------------------------------------------------------------------

summary(df_muni)
summary(df_place)


# check if numbers are balanced
df_place[, table(passe_livre_2, turno2_dummy)]



# balancing before ipw 
a <- df_muni[,   .(QT_APTOS          = weighted.mean(x=QT_APTOS),
                   QT_APTOS_log      = weighted.mean(x=QT_APTOS_log),
                   votos_jair_muni_p = weighted.mean(x=votos_jair_muni_p),
                   mean_dist         = weighted.mean(x=mean_dist, na.rm=T),
                   mean_dens_1000    = weighted.mean(x=mean_dens_1000),
                   educacao_1        = weighted.mean(x=educacao_1),
                   gov_2t             = weighted.mean(x=gov_2t),
                   pib_log           = weighted.mean(x=pib_log)),
             by=passe_livre_2]



# transpose
b <- data.table::transpose(a)

# get row and colnames in order
setnames(b, rownames(a))
b$variable <- colnames(a)
setcolorder(b, 'variable')



# ipw balancing ----------------------------------------------------------------------

step1 <- glm(passe_livre_2 ~ QT_APTOS_log + pib_log + votos_jair_muni_p + gov_2t , # + mean_dens_1000, 
             family = binomial(link = 'logit'),
             data = df_muni)

summary(step1)
df_muni[, ipw := (passe_livre_2 / fitted(step1)) + ((1 - passe_livre_2) / ( 1- fitted(step1)))]


hist(df_muni$ipw)
summary(df_muni$ipw)


# balancing AFTER ipw 
a <- df_muni[,   .(QT_APTOS          = weighted.mean(x=QT_APTOS, w = ipw),
                   QT_APTOS_log      = weighted.mean(x=QT_APTOS_log, w = ipw),
                   votos_jair_muni_p = weighted.mean(x=votos_jair_muni_p, w = ipw),
                   mean_dist         = weighted.mean(x=mean_dist, w = ipw, na.rm=T),
                   mean_dens_1000    = weighted.mean(x=mean_dens_1000, w = ipw),
                   educacao_1        = weighted.mean(x=educacao_1, w = ipw),
                   gov_2t            = weighted.mean(x=gov_2t, w = ipw),
                   pib_log           = weighted.mean(x=pib_log, w = ipw)),
             by=passe_livre_2]



# transpose
c <- data.table::transpose(a)

# get row and colnames in order
setnames(c, rownames(a))
c$variable <- colnames(a)
setcolorder(c, 'variable')





# Model 1. Average effects ----------------------------------------------------------------------
# average effects with inverse probability weighting

# merge ipw info of muni to sections
setDT(df_place)[df_muni, on='code_muni', ipw := i.ipw]

# reg
step2 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                     fixef = 'id_place', 
                     cluster = 'code_muni',
                     weights = ~ipw,
                     data = df_place)
summary(step2)







# Model 2. by REGIAO ------------------------------------------


reg_region <- function(r){  # r = 'Norte'
 
 # select group
 temp_df_muni <- df_muni[ name_region == r, ]
 temp_df_place <- df_place[ name_region == r, ]
 
 # calculate ipw
 temp_step1 <- glm(passe_livre_2 ~ QT_APTOS_log + pib_log + votos_jair_muni_p + gov_2t, # + mean_dens_1000, 
              family = binomial(link = 'logit'),
              data = temp_df_muni)
 
 temp_df_muni[, ipw := (passe_livre_2 / fitted(temp_step1)) + ((1 - passe_livre_2) / ( 1- fitted(temp_step1)))]
 
 # summary(temp_step1)
 # hist(temp_df_muni$ipw)
 # summary(temp_df_muni$ipw)
 
 # step 2 regression
 # merge ipw info of muni to sections
 setDT(temp_df_place)[temp_df_muni, on='code_muni', ipw := i.ipw]
 
 # reg
 step2 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                        fixef = 'id_place', 
                        cluster = 'code_muni',
                        weights = ~ipw,
                        data = temp_df_place)
 
 output <- data.frame(name_region = r,
                      coef = step2$coeftable[2, 1],
                      se = step2$coeftable[2, 2])

 return(output)
}

output_did_region <- purrr::map(.x = unique(df_place$name_region),
                         .f = reg_region) |> rbindlist()


ggplot() +
 geom_point(data = output_did_region, aes(x= name_region, y=coef)) +
 geom_pointrange(data=output_did_region,
                 # show.legend = FALSE,
                 aes(x=name_region, y=coef,
                     ymin = coef - 1.96*se,
                     ymax = coef + 1.96*se)) +
 geom_hline(yintercept = 0, color='gray20') +
 theme_classic()







# modelo 3a - heterogenidade educacao ------------------------------------------

table(df_place$educacao_1_quintile)


reg_edu <- function(e){  # e = '0.4'
 
 # select group
 temp_df_place <- df_place[ educacao_1_decile == e, ]
 
 # reg
 step2 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                        fixef = 'id_place', 
                        cluster = 'code_muni',
                        weights = ~ipw,
                        data = temp_df_place)
 
 output <- data.frame(edu_cat = e,
                      coef = step2$coeftable[2, 1],
                      se = step2$coeftable[2, 2])
 
 return(output)
}

output3a <- purrr::map(.x = levels(df_place$educacao_1_decile),
                       .f = reg_edu) |> rbindlist()


ggplot(data = output3a, aes(x= edu_cat, y=coef)) +
 geom_pointrange(data=output3a,
                 # show.legend = FALSE,
                 aes(x=edu_cat, y=coef,
                     ymin = coef - 1.96*se,
                     ymax = coef + 1.96*se)) +
 geom_point() +
 geom_hline(yintercept = 0, color='gray20') +
 labs(x= 'Percentage of low\nsocioeconomic individuals') +
 theme_classic()



educacao_1_quintile


# modelo 3b - heterogenidade densidade de secoes -----------------------------------------

table(df_place$num_1000_cat10)

reg_dens <- function(i){  # i = 50
 message(i)
 temp_df <- df_place[ num_1000_cat10 == i]
 
 
 step2 <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                        fixef = 'id_place', 
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
output3b <- purrr::map(.x = levels(df_place$num_1000_cat10),
                      .f = reg_dens) |> rbindlist()


ggplot(data = output3b, aes(x= as.numeric(i), y=coef)) +
 geom_line() +
 geom_ribbon(aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
 geom_hline(yintercept = 0) +
 labs(x='Density of electoral sections') +
 theme_classic()






# modelo 3c - heterogenidade distancia e edu -----------------------------------------


table(df_place$ses, useNA = 'always')
table(df_place$num_1000_cat20, useNA = 'always')

# create socioeconomic status level
df_place[, ses := ifelse( educacao_1 >= .3, 'low', 'high')]



# reg function
reg_group_dist_edu <- function(i){  # i = '60' i = Inf
 message(i)
 temp_df <- df_place[ num_1000_cat20 == i & !is.na(educacao_1)]

 step2_low <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                        fixef = 'id_place', 
                        cluster = 'code_muni',
                        weights = ~ipw,
                        data = subset(temp_df, ses == 'low')
                        )
 
 step2_high <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                            fixef = 'id_place', 
                            cluster = 'code_muni',
                            weights = ~ipw,
                            data = subset(temp_df, ses == 'high')
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
 geom_line(show.legend = F) +
 geom_hline(yintercept = 0, color='gray20') +
 labs(x='Density of electoral sections', fill='SES', color='SES') +
 theme_classic()



ggplot(data = output3c, aes(x= i, y=coef, color=group, fill=group)) +
 geom_ribbon(aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
 geom_line(show.legend = F) +
 geom_hline(yintercept = 0, color='gray20') +
 labs(x='Density of electoral sections', fill='SES', color='SES') +
 theme_classic() +
 xlim('20', '60')


# modelo 3d - heterogenidade educacao e dist ------------------------------------------

# discretize edu
my_breaks <- seq(0, 0.8, by=.2)
my_breaks <- c(my_breaks, 1)
df_place[, educacao_1_cat := cut(educacao_1,
                          breaks= my_breaks,
                          labels= my_breaks[-1])]

table(df_place$educacao_1_cat, useNA = 'always')

# fun
reg_group_edu_dist <- function(e, d){  # e = .8 ; d = 60

 message(paste0(e, ' - ', d))
 temp_df <- df_place[ educacao_1_cat == e]
 
 
 step2_above <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                            fixef = 'id_secao', 
                            cluster = 'code_muni',
                            weights = ~ipw,
                            data = subset(temp_df, num_1000_cat ==  d)
                            )
 
 step2_below <- fixest::feols(comparecimento_2022~turno2_dummy + passe_livre_2 + turno2_dummy:passe_livre_2, 
                             fixef = 'id_secao', 
                             cluster = 'code_muni',
                             weights = ~ipw,
                             data = subset(temp_df, num_1000_cat ==   d)
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
                               unique(df_place$num_1000_cat))

output3d <- purrr::map2(.x = all_combinations$Var1,
                        .y = all_combinations$Var2,
                        .f = reg_group_edu_dist) |> rbindlist()


ggplot(data = output3d, aes(x= e, y=coef, color=group, fill=group)) +
# geom_ribbon(aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.2) +
 geom_line() +
 geom_hline(yintercept = 0, color='gray20') +
 labs(x= 'Percentage of low\nsocioeconomic individuals') +
 geom_hline(yintercept = 0, color='gray20') +
 facet_wrap(~d)+
 theme_classic()






# modelo 4 Sant'anna & Zhao ----------------------------------------------------------------------
# average city effects WITH inverse probability weighting

# create numeric id
df_place[, id := .GRP, by = id_secao ]
head(df_place)


# average number of voters between rounds in each muni 
df_place[, QT_APTOS_muni := sum(QT_APTOS)/2, by=code_muni]

output_drdid <- drdid( yname = 'comparecimento_2022', 
                  tname = 'turno2_dummy', 
                  idname = 'id',
                  dname = 'passe_livre_2', 
                  xformla = ~log(QT_APTOS_muni) + pib_log,
                  data = df_place)

summary(output_drdid)




# 6666
# 1. inverse probability weighting
# 2. outcome regression
# 3. doubly robust
# 



# modelo 5 Sant'anna & Zhao ----------------------------------------------------------------------
# average city effects WITH inverse probability weighting

# create numeric id
df_place[, id := .GRP, by = id_secao ]
head(df_place)

# average number of voters between rounds in each muni 
df_place[, QT_APTOS_muni := sum(QT_APTOS)/2, by=code_muni]

output_drdid <- drdid(yname = 'comparecimento_2022', 
                       tname = 'turno2_dummy', 
                       idname = 'id',
                       dname = 'passe_livre_2', 
                       xformla = ~log(QT_APTOS_muni) + pib_log,
                       data = df_place)

summary(output_drdid)



library(did)


# create numeric id
df_place[, id := .GRP, by = id_secao ]

tictoc::tic()
output_did_clust <- did::att_gt(yname = 'comparecimento_2022', 
                          tname = 'turno2_dummy', 
                          idname = 'id',
                          gname = 'passe_livre_2', 
                          xformla = ~QT_APTOS_muni_log + pib_log  + votos_jair_p_muni , # mean_dens
                          clustervars = 'code_muni',
                          cores = data.table::getDTthreads(),
                          data = df_place)

tictoc::toc()

summary(output_did_clust)


Group Time ATT(g,t) Std. Error [95% Pointwise  Conf. Band]  
1    1   0.0023      0.001          0.0004      0.0041 *
 
 
 
 
 
 
 
 
 
 # modelo 5 Sant'anna & Zhao +  heterogenidade educacao ------------------------------------------


get_reg_edu <- function(e){  # e = .1
 
 temp_df <- df_place[ educacao_1_cat == e]
 
 
 output_did_clust <- did::att_gt(yname = 'comparecimento_2022', 
                                 tname = 'turno2_dummy', 
                                 idname = 'id',
                                 gname = 'passe_livre_2', 
                                 xformla = ~QT_APTOS_muni_log + pib_log + votos_jair_p_muni , # mean_dens
                                 clustervars = 'code_muni',
                                 cores = data.table::getDTthreads(),
                                 data = df_place)
 
 output <- data.frame(e = e,
                      coef = output_did_clust$att,
                      se = output_did_clust$se
 )
 
 return(output)
}

output_did <- purrr::map(.x = my_breaks[-1],
                         .f = get_reg_edu) |> rbindlist()

ggplot(data = output_did, aes(x= e, y=coef)) +
 geom_ribbon(aes(ymax=coef + 1.96*se, ymin=coef - 1.96*se), alpha=.5, fill='gray') +
 geom_line() +
 geom_hline(yintercept = 0, color='gray20') +
 labs(x= 'Percentage of low\nsocioeconomic individuals') +
 scale_x_continuous(labels = scales::percent) +
 theme_classic()






 