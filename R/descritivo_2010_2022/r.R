library(data.table)


d22 <- fread("2022/detalhe_votacao_munzona_2022_BRASIL.csv", encoding = "Latin-1")
d18 <- fread("2018/detalhe_votacao_munzona_2018_BRASIL.csv", encoding = "Latin-1")
d14 <- fread("2014/detalhe_votacao_munzona_2014_BRASIL.csv", encoding = "Latin-1")
d10 <- fread("2010/detalhe_votacao_munzona_2010_BRASIL.csv", encoding = "Latin-1")


PL <- fread("passe_livre_resumo.csv")
MU <- fread("munic_dummy_pt.csv")
MA <- fread("correspondencia_IBGE_TSE.csv")

MU <- merge(MU, MA[,c("codigo_tse", "codigo_ibge")], by.x="code_muni", by.y = "codigo_ibge", all.x = T)

my_mun <- MU$codigo_tse

# filtrar presidente
d22 <- d22[d22$DS_CARGO=="Presidente",]
d18 <- d18[d18$DS_CARGO=="Presidente",]
d14 <- d14[d14$DS_CARGO=="Presidente",]
d10 <- d10[d10$DS_CARGO=="Presidente",]


# excluir exterior
d22 <- d22[d22$SG_UF!="ZZ",]
d18 <- d18[d18$SG_UF!="ZZ",]
d14 <- d14[d14$SG_UF!="ZZ",]
d10 <- d10[d10$SG_UF!="ZZ",]


# calcular comparecimento por municipio por turnop
d22$comp <- d22$QT_COMPARECIMENTO/d22$QT_APTOS
d22$comp_mun_t <- ave(d22$comp, paste(d22$NR_TURNO, d22$CD_MUNICIPIO), FUN=mean)
d22 <- d22[,c("NR_TURNO", "CD_MUNICIPIO", "ANO_ELEICAO", "comp_mun_t")]

d18$comp <- d18$QT_COMPARECIMENTO/d18$QT_APTOS
d18$comp_mun_t <- ave(d18$comp, paste(d18$NR_TURNO, d18$CD_MUNICIPIO), FUN=mean)
d18 <- d18[,c("NR_TURNO", "CD_MUNICIPIO", "ANO_ELEICAO", "comp_mun_t")]

d14$comp <- d14$QT_COMPARECIMENTO/d14$QT_APTOS
d14$comp_mun_t <- ave(d14$comp, paste(d14$NR_TURNO, d14$CD_MUNICIPIO), FUN=mean)
d14 <- d14[,c("NR_TURNO", "CD_MUNICIPIO", "ANO_ELEICAO", "comp_mun_t")]

d10$comp <- d10$QT_COMPARECIMENTO/d10$QT_APTOS
d10$comp_mun_t <- ave(d10$comp, paste(d10$NR_TURNO, d10$CD_MUNICIPIO), FUN=mean)
d10 <- d10[,c("NR_TURNO", "CD_MUNICIPIO", "ANO_ELEICAO", "comp_mun_t")]

dd <- rbind(d22,d18,d14,d10)

dd <- dd[dd$CD_MUNICIPIO%in%my_mun,]

dd$comp_t1 <- ifelse(dd$NR_TURNO==1, dd$comp_mun_t, 0)
dd$comp_t2 <- ifelse(dd$NR_TURNO==2, dd$comp_mun_t, 0)
dd$comp_t1 <- ave(dd$comp_t1, paste(dd$CD_MUNICIPIO, dd$ANO_ELEICAO), FUN=sum)
dd$comp_t2 <- ave(dd$comp_t2, paste(dd$CD_MUNICIPIO, dd$ANO_ELEICAO), FUN=sum)
dd$d_comp <- dd$comp_t2 - dd$comp_t1

dd <- dd[!duplicated(paste(dd$ANO_ELEICAO, dd$CD_MUNICIPIO)),]



dd<-merge(dd,PL, by="CD_MUNICIPIO", all.x = T)

dd$passe_livre_t1[is.na(dd$passe_livre_t1)] <- 0
dd$passe_livre_t2[is.na(dd$passe_livre_t2)] <- 0

dd$gr <- ifelse(dd$passe_livre_t1==1 &dd$passe_livre_t2==1, "tr1",
         ifelse(dd$passe_livre_t1==0 &dd$passe_livre_t2==1, "tr2","co"))

dd <- merge(dd, MU[,c("codigo_tse", "dummy_pt")], by.x = "CD_MUNICIPIO", by.y="codigo_tse", all.x = T)

dd <- dd[dd$dummy_pt%in%1,]

dd$ano <- as.character(dd$ANO_ELEICAO)
library(fixest)
m1 <- feols(d_comp~-1 + ano:gr, data=dd)
summary(m1)

ss <- as.data.frame(summary(m1)$coeftable)
ss$gr <- c(rep("co", 4),rep("tr1", 4),rep("tr2", 4))
ss$x <- rep(c(2010,2014,2018,2022),3)
ss$up <- ss$Estimate + 1.96*ss$`Std. Error`
ss$do <- ss$Estimate - 1.96*ss$`Std. Error`

#ss <- ss[ss$gr%in%c("tr2","co"),]
library(ggplot2)
ggplot() +
 geom_point(aes(x=x,y=Estimate, color=gr),data=ss,
            position = position_dodge(width = 0.20)) +
 geom_errorbar(aes(ymin=do, ymax=up, x=x, color=gr),data=ss,width=0,
               position = position_dodge(width = 0.20)) +
 theme_classic()