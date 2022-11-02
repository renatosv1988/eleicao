library(geobr)
library(data.table)
library(ggplot2)
library(scales)
library(fixest)

cidades <- read_municipal_seat()
eleicao_2018 <- fread('./data/comparecimento_munis_2018.csv')
eleicao_2022 <- fread('./data/comparecimento_munis_2022.csv')
passe_livre <- fread('data_raw/passe_livre/passe_livre_resumo.csv')

# 1 - descritivo dos municipios tratados
# criar base de municipio a partir do 1º turno de 2022
MUN <- eleicao_2022[eleicao_2022$NR_TURNO==1,]
# merge passe livre
MUN <- merge(MUN, passe_livre, by="CD_MUNICIPIO", all.x = T)
# convert NAs to 0s
MUN$passe_livre_t1[is.na(MUN$passe_livre_t1)] <- 0
MUN$passe_livre_t2[is.na(MUN$passe_livre_t2)] <- 0

# criar grupos
MUN$grupo <- ifelse(MUN$passe_livre_t1==1, "passe livre 1",
                    ifelse(MUN$passe_livre_t2==1, "passe livre 2","outros"))
MUN$br <- "BR"

# calcular médias por grupo
aggregate(cbind(comparecimento, QT_ELEITOR_SECAO, edu_ate_fundamental, edu_superior) ~ grupo,
                  data = MUN, mean)
aggregate(cbind(comparecimento, QT_ELEITOR_SECAO, edu_ate_fundamental, edu_superior) ~ br,
          data = MUN, mean)
weighted.mean(MUN$edu_ate_fundamental, MUN$QT_ELEITOR_SECAO)
weighted.mean(MUN$edu_superior, MUN$QT_ELEITOR_SECAO)
aggregate(cbind(QT_ELEITOR_SECAO) ~ grupo, data = MUN, sum)



MUN$log_ele <- log(MUN$QT_ELEITOR_SECAO)
ggplot() +
  geom_density(aes(x = log_ele, color=grupo), data=MUN) +
  theme_classic() +
  scale_x_continuous(breaks = c(log(1000), log(10000), log(100000), log(1000000), log(10000000)),
                     labels = c("1,000", "10,000", "100,000", "1,000,000", "10,000,000")) +
  labs(x="eligible voters", y="density")


# calcular comparecimento por turno
el1 <- eleicao_2018[eleicao_2018$NR_TURNO==1,c("CD_MUNICIPIO", "comparecimento", "QT_ELEITOR_SECAO")]
el1 <- merge(el1, MUN[,c("CD_MUNICIPIO", "grupo")], by="CD_MUNICIPIO", all.x = T)
el1w <- el1[,list(comparecimento = weighted.mean(comparecimento,QT_ELEITOR_SECAO)),by=grupo]
el1s <- aggregate(comparecimento~grupo, data = el1, FUN=mean)
el1s$t <- 1
el1w$t <- 1
el1$t <- 1

el2 <- eleicao_2018[eleicao_2018$NR_TURNO==2,c("CD_MUNICIPIO", "comparecimento", "QT_ELEITOR_SECAO")]
el2 <- merge(el2, MUN[,c("CD_MUNICIPIO", "grupo")], by="CD_MUNICIPIO", all.x = T)
el2w <- el2[,list(comparecimento = weighted.mean(comparecimento,QT_ELEITOR_SECAO)),by=grupo]
el2s <- aggregate(comparecimento~grupo, data = el2, FUN=mean)
el2s$t <- 2
el2w$t <- 2
el2$t <- 2

el3 <- eleicao_2022[eleicao_2022$NR_TURNO==1,c("CD_MUNICIPIO", "comparecimento", "QT_ELEITOR_SECAO")]
el3 <- merge(el3, MUN[,c("CD_MUNICIPIO", "grupo")], by="CD_MUNICIPIO", all.x = T)
el3w <- el3[,list(comparecimento = weighted.mean(comparecimento,QT_ELEITOR_SECAO)),by=grupo]
el3s <- aggregate(comparecimento~grupo, data = el3, FUN=mean)
el3s$t <- 3
el3w$t <- 3
el3$t <- 3

el4 <- eleicao_2022[eleicao_2022$NR_TURNO==2,c("CD_MUNICIPIO", "comparecimento", "QT_ELEITOR_SECAO")]
el4 <- merge(el4, MUN[,c("CD_MUNICIPIO", "grupo")], by="CD_MUNICIPIO", all.x = T)
el4w <- el4[,list(comparecimento = weighted.mean(comparecimento,QT_ELEITOR_SECAO)),by=grupo]
el4s <- aggregate(comparecimento~grupo, data = el4, FUN=mean)
el4s$t <- 4
el4w$t <- 4
el4$t <- 4


el <- rbind(el1,el2,el3,el4)
els <- rbind(el1s,el2s,el3s,el4s)
elw <- rbind(el1w,el2w,el3w,el4w)

ggplot() +
  geom_point(aes(x=t, y=comparecimento, color=grupo),data=els) +
  geom_line(aes(x=t, y=comparecimento, color=grupo),linetype = 2, data=els) +
  theme_classic() +
  scale_y_continuous(labels=percent) +
  scale_x_continuous(labels=c("2018 1ºT.","2018 2ºT.","2022 1ºT.","2022 2ºT.")) +
  coord_cartesian(ylim=c(0.70,0.8))

ggplot() +
  geom_point(aes(x=t, y=comparecimento, color=grupo),data=elw) +
  geom_line(aes(x=t, y=comparecimento, color=grupo),linetype = 2, data=elw) +
  theme_classic() +
  scale_y_continuous(labels=percent) +
  scale_x_continuous(labels=c("2018 1ºT.","2018 2ºT.","2022 1ºT.","2022 2ºT.")) +
  coord_cartesian(ylim=c(0.70,0.8))


el22s <- els[els$t%in%3:4,]
ggplot() +
  geom_point(aes(x=t, y=comparecimento, color=grupo),data=el22s) +
  geom_line(aes(x=t, y=comparecimento, color=grupo),linetype = 2, data=el22s) +
  theme_classic() +
  scale_y_continuous(labels=percent) +
  scale_x_continuous(breaks = c(3,4), labels=c("2022 1ºT.","2022 2ºT.")) +
  coord_cartesian(ylim=c(0.75,0.77))



el22w <- elw[elw$t%in%3:4,]
ggplot() +
  geom_point(aes(x=t, y=comparecimento, color=grupo),data=el22w) +
  geom_line(aes(x=t, y=comparecimento, color=grupo),linetype = 2, data=el22w) +
  theme_classic() +
  scale_y_continuous(labels=percent) +
  scale_x_continuous(breaks = c(3,4), labels=c("2022 1ºT.","2022 2ºT.")) +
  coord_cartesian(ylim=c(0.75,0.77))


el22 <- el[el$t%in%3:4,]

el22$t2 <- ifelse(el22$t==4,1,0)
el22$pl1_t2 <- ifelse(el22$grupo=="passe livre 1" & el22$t2==1,1,0)
el22$pl2_t2 <- ifelse(el22$grupo=="passe livre 2" & el22$t2==1,1,0)


lm <- feols(comparecimento ~ t2 + pl1_t2 + pl2_t2 | CD_MUNICIPIO, data = el22 )
lm2 <- feols(comparecimento ~ t2 + pl1_t2 + pl2_t2 | CD_MUNICIPIO, data = el22, weights = el22$QT_ELEITOR_SECAO )
etable(lm, lm2)