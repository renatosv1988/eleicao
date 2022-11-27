library(data.table)
library(fixest)
library(ggplot2)

# ler dados
df <- fread("../../data/base_DiD2010_2022_secoes.csv", encoding = "Latin-1")
table(df$ANO_ELEICAO)
# filtrar apenas para seções tratadas
st <- df[df$passe_livre_2==1,]

# criar dummy de tratamento já no 1º turno
st$pl1 <- ifelse(st$passe_livre_1==1,1,0)
# criar dummy de tratamento apenas no 2º turno
st$pl2 <- ifelse(st$passe_livre_1==0,1,0)

# ano como categorica
st$ano <- as.character(st$ANO_ELEICAO)
# regressão 1º turno
m1 <- feols(comparecimento_t1~ ano + ano*pl1 | id_secao, data=st, cluster = st$CD_MUNICIPIO)
summary(m1)

# regressão descritiva por grupo/ano
m1 <- feols(variacao_comparecimento~ ano + pl2 + ano*gr, data=dd)
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



