library(data.table)
library(fixest)
library(ggplot2)

# ler dados
df <- fread("../../data/base_DiD2022_secoes.csv", encoding = "Latin-1")

# calcular variação de comparecimento e votos Lula em 2022
df$comparecimento_2022 <- df$QT_COMPARECIMENTO/df$QT_APTOS
df$votos_lula_2022 <-df$votos_lula/df$QT_APTOS

# separar por turno
df2 <- df[df$NR_TURNO==2,
          c("id_secao",
            "comparecimento_2022","comparecimento_2018","comparecimento_2014","comparecimento_2010",
            "votos_lula_2022","votos_lula_2018","votos_lula_2014","votos_lula_2010")]
colnames(df2)<- c("id_secao",
               "comparecimento_2022_T2","comparecimento_2018_T2","comparecimento_2014_T2","comparecimento_2010_T2",
               "votos_lula_2022_T2","votos_lula_2018_T2","votos_lula_2014_T2","votos_lula_2010_T2")

df1 <- df[df$NR_TURNO==1,]

df <- merge(df1, df2, by="id_secao", all.x = T)

df$v_comp_2022 <- df$comparecimento_2022_T2 - df$comparecimento_2022
df$v_comp_2018 <- df$comparecimento_2018_T2 - df$comparecimento_2018
df$v_comp_2014 <- df$comparecimento_2014_T2 - df$comparecimento_2014
df$v_comp_2010 <- df$comparecimento_2010_T2 - df$comparecimento_2010

df$v_pt_2022 <- df$votos_lula_2022_T2 - df$votos_lula_2022
df$v_pt_2018 <- df$votos_lula_2018_T2 - df$votos_lula_2018
df$v_pt_2014 <- df$votos_lula_2014_T2 - df$votos_lula_2014
df$v_pt_2010 <- df$votos_lula_2010_T2 - df$votos_lula_2010

# criar painel longo
df$educacao_1


summary(df$votos_lula_2018)


df$variacao_comparecimento_2012 <- df$QT_COMPARECIMENTO_2T - df$QT_COMPARECIMENTO_1T

my_vars <- c("NR_TURNO", "CD_MUNICIPIO", "ANO_ELEICAO", "QT_COMPARECIMENTO",
             "QT_APTOS","DS_CARGO")
# manter somente variáveis de interesse
d22 <- d22[, ..my_vars]
d18 <- d18[, ..my_vars]
d14 <- d14[, ..my_vars]
d10 <- d10[, ..my_vars]

# juntar bases
dd <- rbind(d22, d18, d14, d10)

# filtrar p/ presidente
dd <- dd[dd$DS_CARGO=="Presidente",]

# juntar código IBGE
MU <- merge(MU, MA[,c("codigo_tse", "codigo_ibge")], by.x="code_muni", by.y = "codigo_ibge", all.x = T)
# listar municipios
my_mun <- MU$codigo_tse
# filtrar municipios
dd <- dd[dd$CD_MUNICIPIO%in%my_mun,]

# somar variáveis por municipio-ano-turno
dd$QT_APTOS <- ave(dd$QT_APTOS,
                   paste(dd$CD_MUNICIPIO, dd$ANO_ELEICAO, dd$NR_TURNO),
                   FUN=sum)
dd$QT_COMPARECIMENTO <- ave(dd$QT_COMPARECIMENTO,
                   paste(dd$CD_MUNICIPIO, dd$ANO_ELEICAO, dd$NR_TURNO),
                   FUN=sum)
# agregar por municipio-ano-turno
dd<-dd[!duplicated(paste(dd$CD_MUNICIPIO, dd$ANO_ELEICAO, dd$NR_TURNO)),]

# separa por turnos
dd$mun_ano <- paste(dd$CD_MUNICIPIO, dd$ANO_ELEICAO)
dd1 <- dd[dd$NR_TURNO==1,]
dd2 <- dd[dd$NR_TURNO==2,]

# juntar commparecimento do 2º turno na base do 1º
dd2<- dd2[,c("mun_ano", "QT_COMPARECIMENTO")]
colnames(dd2) <- c("mun_ano", "QT_COMPARECIMENTO_T2")

# juntar turnos novamente no formato wide
dd <- merge(dd1, dd2, by="mun_ano", all.x = T)


# calcular variação de comparecimento por 2 formas
dd$d_comp1 <- (dd$QT_COMPARECIMENTO_T2 - dd$QT_COMPARECIMENTO)/dd$QT_APTOS
dd$d_comp2 <- (dd$QT_COMPARECIMENTO_T2 - dd$QT_COMPARECIMENTO)/dd$QT_COMPARECIMENTO

summary(dd$d_comp1)
summary(dd$d_comp2)

# juntar info sobre passe livre
dd<-merge(dd,PL, by="CD_MUNICIPIO", all.x = T)
dd$passe_livre_t1[is.na(dd$passe_livre_t1)] <- 0
dd$passe_livre_t2[is.na(dd$passe_livre_t2)] <- 0
dd$gr <- ifelse(dd$passe_livre_t1==1 &dd$passe_livre_t2==1, "tr1",
         ifelse(dd$passe_livre_t1==0 &dd$passe_livre_t2==1, "tr2","co"))
dd <- merge(dd, MU[,c("codigo_tse", "dummy_pt")], by.x = "CD_MUNICIPIO", by.y="codigo_tse", all.x = T)
# criar grupo para municipios sem transporte público
dd$gr <- ifelse(dd$dummy_pt==0, "st", dd$gr)

# criar caracter de ano
dd$ano <- as.character(dd$ANO_ELEICAO)
dd$ano <- factor(dd$ano, levels = c("2018", "2022", "2010", "2014"))
dd$gr <- factor(dd$gr, levels = c("co","tr1", "tr2", "st"))

# regressão descritiva por grupo/ano
m1 <- feols(d_comp1~ ano + gr + ano*gr, data=dd)
summary(m1)

# criar base de resultados para ggplot
ss <- as.data.frame(summary(m1)$coeftable)
ss <- ss[5:16,]

ss$gr <- c(c("tr1","tr2","st"),
           rep("tr1", 3),rep("tr2", 3), rep("st",3))

ss$x <- c(rep(2018,3),
          rep(c(2022,2010,2014),3))
ss$up <- ss$Estimate + 1.96*ss$`Std. Error`
ss$do <- ss$Estimate - 1.96*ss$`Std. Error`

# plotar resultado
ggplot() +
 geom_hline(yintercept = 0, linetype=2, alpha=0.2) +
 geom_point(aes(x=x,y=Estimate, color=gr),data=ss,
            position = position_dodge(width = 0.40)) +
 geom_errorbar(aes(ymin=do, ymax=up, x=x, color=gr),data=ss,width=0,
               position = position_dodge(width = 0.40)) +
 theme_classic() +
 labs(x="eleição", y="variação no comparecimento (2ºT - 1ºT)\ncom relação ao grupo controle") +
 coord_cartesian(ylim=c(-0.03,0.03)) +
 scale_x_continuous(breaks = c(2010,2014,2018,2022)) +
 #scale_y_continuous(breaks = c(-0.15,-0.1, -0.05, 0, 0.05, 0.1,0.15)) +
 scale_color_manual(name = "Passe livre em 2022:    ",
                    values = c("tr1"="pink","tr2"="red","co"="black", "st"="gray"),
                    labels = c("tr1"="1ºT e 2ºT","tr2"="apenas 2ºT","co"="nenhum", "st"="sem TP")) +
 theme(legend.position = "top")



