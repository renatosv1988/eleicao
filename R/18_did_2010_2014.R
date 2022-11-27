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
st$ano <- factor(st$ano, levels = c("2018", "2022", "2010", "2014"))

# criar dummies explicitamente
st$ano2010 <- ifelse(st$ano=="2010",1,0)
st$ano2014 <- ifelse(st$ano=="2014",1,0)
st$ano2018 <- ifelse(st$ano=="2018",1,0)
st$ano2022 <- ifelse(st$ano=="2022",1,0)



# regressão 1º turno
m1 <- feols(comparecimento_t1~ 
             ano2010 + ano2014 + ano2022 +
             ano2010:pl1 + ano2014:pl1  + ano2022:pl1 |
             id_secao, data=st, cluster = st$CD_MUNICIPIO)
summary(m1)

# regressão 1º turno
m2 <- feols(votos_lula_p_t1~ 
             ano2010 + ano2014 + ano2022 +
             ano2010:pl1 + ano2014:pl1  + ano2022:pl1 |
             id_secao, data=st, cluster = st$CD_MUNICIPIO)
summary(m2)

# regressão variação entre turnos
m3 <- feols(variacao_comparecimento~
             ano2010 + ano2014 + ano2022 +
             ano2010:pl2 + ano2014:pl2  + ano2022:pl2 |
             id_secao, data=st, cluster = st$CD_MUNICIPIO)
summary(m3)

# regressão 1º turno
m4 <- feols(variacao_luva~ 
             ano2010 + ano2014 + ano2022 +
             ano2010:pl2 + ano2014:pl2  + ano2022:pl2 |
             id_secao, data=st, cluster = st$CD_MUNICIPIO)
summary(m4)

etable(m1, m2, m3, m4)




# criar base de resultados para ggplot
ss3 <- as.data.frame(summary(m3)$coeftable)
s03 <- ss3[1,]
s03[1,] <- 0
ss3 <- ss3[4:6,]
ss3 <- rbind(s03,ss3)
ss3$y <- "comp"
# criar base de resultados para ggplot
ss4 <- as.data.frame(summary(m4)$coeftable)
s04 <- ss4[1,]
s04[1,] <- 0
ss4 <- ss4[4:6,]
ss4 <- rbind(s04,ss4)
ss4$y <- "lula"

ss <- rbind(ss3, ss4)
 
ss$gr <- rep("tr2", 8)

ss$x <- c(2018,2010,2014,2022,2018,2010,2014,2022)
ss$up <- ss$Estimate + 1.96*ss$`Std. Error`
ss$do <- ss$Estimate - 1.96*ss$`Std. Error`

# plotar resultado
ggplot() +
 geom_hline(yintercept = 0, linetype=2, alpha=0.2) +
 geom_vline(xintercept = 2021.8, linetype=3, alpha=0.2) +
 geom_point(aes(x=x,y=Estimate, color=y),data=ss,
            position = position_dodge(width = 0.40)) +
 geom_errorbar(aes(ymin=do, ymax=up, x=x, color=y),data=ss,width=0,
               position = position_dodge(width = 0.40)) +
 theme_classic() +
 labs(x="eleição", y="variação entre turnos (2ºT - 1ºT)\ncom relação ao grupo controle\n(passe livre no 1ºT de 2022)") +
 coord_cartesian(ylim=c(-0.05,0.05)) +
 scale_x_continuous(breaks = c(2010,2014,2018,2022)) +
 #scale_y_continuous(breaks = c(-0.15,-0.1, -0.05, 0, 0.05, 0.1,0.15)) +
 scale_color_manual(name = "Variável dependente:    ",
                    values = c("comp"="black","lula"="red"),
                    labels = c("comp"="comparecimento","lula"="votos PT")) +
 theme(legend.position = "top")



