library(did)
library(fixest)
library(data.table)
library(ggplot2)
library(cowplot)
library(geobr)

# ler base de eleições
BD <- fread("../../data/base_DiD2022_secoes.csv")

# criar grupos de tratamento
# períodos tratados
BD$periodos_tratados <- ave(BD$passe_livre, BD$id_secao, FUN=sum)
BD[, grupo := fcase(periodos_tratados==2, 1,
                    periodos_tratados==1, 2,
                    default = 0)]
BD$pl_1 <- ifelse(BD$periodos_tratados==2, 1, 0)
BD$pl_2 <- ifelse(BD$periodos_tratados==1, 1, 0)

# truno como categórica
BD$turno <-  as.character(BD$NR_TURNO)

# base por turno
T1 <- BD[BD$NR_TURNO==1,]
T2 <- BD[BD$NR_TURNO==2,]


# calcular variação de comparecimento por seção (incluindo cidades sem ônibus)
T1$comparecimento_t1 <- T1$QT_COMPARECIMENTO
T2$comparecimento_t2 <- T2$QT_COMPARECIMENTO

DD <- merge(T1, T2[,c("id_secao","comparecimento_t2")], by="id_secao", all.x = T)

MM <- DD[,.(comparecimento_t1=sum(comparecimento_t1),
            comparecimento_t2=sum(comparecimento_t2),
            grupo=mean(grupo),
            dummy_pt=mean(dummy_pt),
            aptos=sum(QT_APTOS)),by=code_muni]

MM$c1 <- MM$comparecimento_t1/MM$aptos
MM$c2 <- MM$comparecimento_t2/MM$aptos
MM$d_c22 <- MM$c2 - MM$c1

MMC <- merge(MC, MM[,c("code_muni","d_c22", "aptos", "grupo", "dummy_pt")], by.x="code_muni", by.y="code_muni")


# incluir 2018 -----------------------------------------------------------------
# ler base de eleições
BD <- fread("../../data/base_DiD2018_secoes.csv")

# base por turno
T1 <- BD[BD$NR_TURNO==1,]
T2 <- BD[BD$NR_TURNO==2,]


# calcular variação de comparecimento por seção (incluindo cidades sem ônibus)
T1$comparecimento_t1 <- T1$QT_COMPARECIMENTO
T2$comparecimento_t2 <- T2$QT_COMPARECIMENTO

DD <- merge(T1, T2[,c("id_secao","comparecimento_t2")], by="id_secao", all.x = T)

MM <- DD[,.(comparecimento_t1=sum(comparecimento_t1),
            comparecimento_t2=sum(comparecimento_t2),
            aptos=sum(QT_APTOS)),by=code_muni]
MM$c1 <- MM$comparecimento_t1/MM$aptos
MM$c2 <- MM$comparecimento_t2/MM$aptos
MM$d_c18 <- MM$c2 - MM$c1

MA <- merge(MMC, MM[,c("code_muni","d_c18")], by.x="code_muni", by.y="code_muni")


ggplot() +
 geom_hline(yintercept=0, linetype=2, alpha=0.2) +
 geom_vline(xintercept=0, linetype=2, alpha=0.2) +
 geom_point(aes(x=d_c18, y=d_c22), data=MA, alpha=0.15, size=0.5) +
 geom_smooth(aes(x=d_c18, y=d_c22), method = "lm", data=MA) +
 theme_classic() +
 labs(title = "Variação no comparecimento (2ºT - 1ºT)", x="2018", y="2022")

ggplot() +
 geom_hline(yintercept=0, linetype=2, alpha=0.2) +
 geom_vline(xintercept=0, linetype=2, alpha=0.2) +
 geom_abline(intercept=0, slope=1, linetype=2, alpha=0.2) +
 geom_point(aes(x=d_c18, y=d_c22), data=MA[MA$grupo==0 & MA$dummy_pt==1,], alpha=0.15, size=1.5) +
 geom_point(aes(x=d_c18, y=d_c22), data=MA[MA$grupo==2,], alpha=0.15, size=1.5, color="red") +
 theme_classic() +
 labs(title = "Variação no comparecimento (2ºT - 1ºT)", x="2018", y="2022")
 
 


MA$pl1 <- ifelse(MA$grupo==1, 1,0)
MA$pl2 <- ifelse(MA$grupo==2, 1,0)

m1 <- feols(d_c22~d_c18, data = MA)
m2 <- feols(d_c22~d_c18+pl1+pl2, data = MA)
etable(m1, m2)