library(did)
library(fixest)
library(data.table)
library(ggplot2)
library(cowplot)
library(geobr)

# ler base de eleições
BD <- fread("../../data/base_DiD2018_secoes.csv")

# diferença de percentual Lula-Jair
BD$pct_lula <-  (BD$votos_lula)/BD$QT_APTOS

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

# eleitores
sum(T1$QT_APTOS)
#[1] 155758572
sum(T2$QT_APTOS)
#[1] 155758572

# comparecimento
sum(T1$QT_COMPARECIMENTO)
#[1] 123378340
sum(T2$QT_COMPARECIMENTO)
#[1] 123942648


# votos LULA 2T
sum(T2$votos_lula)
#[1] 123942648

# votos jair 2T
sum(T2$votos_jair)

# votos nulos e brancos
sum(T2$QT_VOTOS_NULOS) + sum(T2$QT_VOTOS_BRANCOS)


# TABELA 1 calcular municipios, eleitores e comparecimento por grupo de tratament
# base de municípios
BF <- subset(BD, dummy_pt==1)
MU <- BD[!duplicated(BD$CD_MUNICIPIO),]
MT <- BF[!duplicated(BF$CD_MUNICIPIO),]

table(MU$dummy_pt)
T1[,.(eleitores=sum(QT_APTOS)),by=dummy_pt]

sum(T1$QT_COMPARECIMENTO)/sum(T1$QT_APTOS)
sum(T1$QT_COMPARECIMENTO[T1$dummy_pt==0])/sum(T1$QT_APTOS[T1$dummy_pt==0])
sum(T1$QT_COMPARECIMENTO[T1$dummy_pt==1])/sum(T1$QT_APTOS[T1$dummy_pt==1])

sum(T2$QT_COMPARECIMENTO)/sum(T2$QT_APTOS)
sum(T2$QT_COMPARECIMENTO[T2$dummy_pt==0])/sum(T2$QT_APTOS[T2$dummy_pt==0])
sum(T2$QT_COMPARECIMENTO[T2$dummy_pt==1])/sum(T2$QT_APTOS[T2$dummy_pt==1])

table(MT$grupo)
BF1 <- BF[BF$NR_TURNO==1,]
BF2 <- BF[BF$NR_TURNO==2,]
BF1[,.(eleitores=sum(QT_APTOS)),by=grupo] 

sum(BF1$QT_COMPARECIMENTO[BF1$grupo==0])/sum(BF1$QT_APTOS[BF1$grupo==0])
sum(BF1$QT_COMPARECIMENTO[BF1$grupo==1])/sum(BF1$QT_APTOS[BF1$grupo==1])
sum(BF1$QT_COMPARECIMENTO[BF1$grupo==2])/sum(BF1$QT_APTOS[BF1$grupo==2])

sum(BF2$QT_COMPARECIMENTO[BF2$grupo==0])/sum(BF2$QT_APTOS[BF2$grupo==0])
sum(BF2$QT_COMPARECIMENTO[BF2$grupo==1])/sum(BF2$QT_APTOS[BF2$grupo==1])
sum(BF2$QT_COMPARECIMENTO[BF2$grupo==2])/sum(BF2$QT_APTOS[BF2$grupo==2])


# calcular variação de comparecimento por seção (incluindo cidades sem ônibus)
T1$comparecimento_t1 <- T1$QT_COMPARECIMENTO
T2$comparecimento_t2 <- T2$QT_COMPARECIMENTO

DD <- merge(T1, T2[,c("id_secao","comparecimento_t2")], by="id_secao", all.x = T)

# MAPA POR UF ------------------------------------------------------------------
UF <- DD[,.(comparecimento_t1=sum(comparecimento_t1),
            comparecimento_t2=sum(comparecimento_t2),
            aptos=sum(QT_APTOS)),by=SG_UF]
UF$c1 <- UF$comparecimento_t1/UF$aptos
UF$c2 <- UF$comparecimento_t2/UF$aptos
UF$d_c <- UF$c2 - UF$c1

UFM <- read_state()

UFM <- merge(UFM, UF[,c("SG_UF","d_c")], by.x="abbrev_state", by.y="SG_UF")

# limitar valores extremos
UFM$d_c[UFM$d_c < -0.02] <- -0.02
UFM$d_c[UFM$d_c > 0.02] <- 0.02
# plot
ggplot() +
 geom_sf(aes(fill=d_c*100),data=UFM, color="gray", size=.15) +
 labs(subtitle="comparecimento (2ºT - 1ºT)", size=8) +
 theme_void() +
 scale_fill_gradient2(low="firebrick", high="royalblue", limits = c(-2,2)) +
 labs(fill="(p.p.)") +
 theme(legend.position = "bottom")

# MAPA POR Municipio ------------------------------------------------------------------
MM <- DD[,.(comparecimento_t1=sum(comparecimento_t1),
            comparecimento_t2=sum(comparecimento_t2),
            aptos=sum(QT_APTOS)),by=code_muni]
MM$c1 <- MM$comparecimento_t1/MM$aptos
MM$c2 <- MM$comparecimento_t2/MM$aptos
MM$d_c <- MM$c2 - MM$c1

MC <- read_municipal_seat()

MMC <- merge(MC, MM[,c("code_muni","d_c", "aptos")], by.x="code_muni", by.y="code_muni")
summary(MC$aptos)
# Remove plot axis
# limitar valores extremos

MMC$d_c[MMC$d_c < -0.03] <- -0.03
MMC$d_c[MMC$d_c > 0.01] <- 0.01
# plot
ggplot() +
 geom_sf(fill="white",data=UFM, color="gray", size=.15) +
 geom_point(aes(color=d_c*100, size=aptos, geometry=geometry),data=MMC, stat = "sf_coordinates") +
 labs(subtitle="comparecimento (2ºT - 1ºT)", size=8) +
 theme_void() +
 scale_color_gradient2(low="firebrick", high="royalblue", limits = c(-3,1), midpoint = -1) +
 labs(color="(p.p.)") +
 theme(legend.position = "bottom") +
 scale_size_continuous(range=c(0.5, 10), breaks=c(10^3, 10^4, 10^5, 10^6, 10^7), guide="none")

# MAPA DOS MUNICIPIOS POR Grupo de Tratamento ----------------------------------
MM <- DD[,.(comparecimento_t1=sum(comparecimento_t1),
            comparecimento_t2=sum(comparecimento_t2),
            aptos=sum(QT_APTOS),
            grupo=mean(grupo),
            pt=mean(dummy_pt)),by=code_muni]

MC <- read_municipal_seat()

MMC <- merge(MC, MM[,c("code_muni","pt",  "grupo", "aptos")], by.x="code_muni", by.y="code_muni")
# Remove plot axis
MMC$grupo <- ifelse(MMC$pt==0, "sem tp",
             ifelse(MMC$grupo==0, "sem pl",
             ifelse(MMC$grupo==2, "pl 2","pl 1")))
                    
MMC <- MMC[order(MMC$aptos),]
# plot
ggplot() +
 geom_sf(fill="white",data=UFM, color="gray", size=.15) +
 geom_point(aes(color=grupo, size=aptos, geometry=geometry),data=MMC, stat = "sf_coordinates") +
 theme_void() +
 scale_color_manual(values = c("sem tp"="gray","sem pl"="black","pl 2"="firebrick","pl 1"="pink"),
                    labels = c("sem tp"="sem TP","sem pl"="sem PL","pl 2"="PL no 2ºT","pl 1"="PL no 1T e 2T"),) +
 labs(color="Grupos: ") +
 theme(legend.position = "bottom") +
 scale_size_continuous(range=c(0.5, 5), breaks=c(10^3, 10^4, 10^5, 10^6, 10^7), guide="none")



# Variação do comparecimento por característica da seção
DD$diff_comparecimento <- (DD$comparecimento_t2 - DD$comparecimento_t1)/DD$QT_APTOS

ggplot() +
 geom_hline(yintercept = 0, linetype=2, color="gray") +
 geom_smooth(aes(x=idade_60M, y = diff_comparecimento), data = DD, se=F) +
 theme_classic() +
 labs(x="% idosos", y="diferença de comparecimento (2T - 1T)/aptos") +
 coord_cartesian(xlim=c(0,0.5), ylim = c(-0.03,0.03))

ggplot() +
 geom_hline(yintercept = 0, linetype=2, color="gray") +
 geom_smooth(aes(x=idade_60M, y = diff_comparecimento, color="co"), data = DD[DD$grupo==0,], se=F) +
 geom_smooth(aes(x=idade_60M, y = diff_comparecimento, color="tr"), data = DD[DD$grupo==1,], se=F) +
 theme_classic() +
 labs(x="% idosos", y="diferença de comparecimento (2T - 1T)/aptos") +
 coord_cartesian(xlim=c(0,0.5), ylim = c(-0.03,0.03)) +
 scale_color_manual(values = c("tr"="firebrick","co"="black"),
                    labels = c("tr"="PL no 2T","co"="sem PL"),
                    name="Grupos") +
 theme(legend.position = "bottom")
 

mean(DD$idade_60M[DD$grupo==0 & DD$dummy_pt==1], na.rm=T)
mean(DD$idade_60M[DD$grupo==2 & DD$dummy_pt==1], na.rm=T)

summary(DD$idade_60M)

geom_smooth(aes(x=idade_60M, y = diff_comparecimento), data = DD[DD$grupo==2,], color="red", se=F) +

p2 <- ggplot() +
 geom_smooth(aes(x=idade_60M, y = comparecimento_2022, color=turno), data = ID[ID$grupo==0]) +
 theme_classic() +
 labs(x="% idosos", y="comparecimento", title = "sem passe-livre") +
 scale_color_manual(values = c("1"="black", "2"="goldenrod"))

p3 <- ggplot() +
 geom_smooth(aes(x=idade_60M, y = comparecimento_2022, color=turno), data = ID[ID$grupo==1]) +
 theme_classic() +
 labs(x="% idosos", y="comparecimento", title = "passe-livre em ambos os turnos") +
 scale_color_manual(values = c("1"="black", "2"="goldenrod"))

p4 <- ggplot() +
 geom_smooth(aes(x=idade_60M, y = comparecimento_2022, color=turno), data = ID[ID$grupo==2]) +
 theme_classic() +
 labs(x="% idosos", y="comparecimento", title = "passe-livre apenas no 2º turno") +
 scale_color_manual(values = c("1"="black", "2"="goldenrod"))


pp <- plot_grid(p1,p2,p3,p4, ncol = 2, nrow=2)
save_plot("figures/histgrama_idosos.png",pp, base_height = 10, base_width = 10)



# DiD básicos

# periodo
BD$t2 <- ifelse(BD$NR_TURNO==2,1,0)
# interação:  período 2 X always treated
BD$pl_1_t2 <- ifelse(BD$grupo==1 & BD$NR_TURNO==2,1,0)
# interação:  período 2 X treated em t2
BD$pl_2_t2 <- ifelse(BD$grupo==2 & BD$NR_TURNO==2,1,0)

#valores de corte para iterações
BD$educ1 <- ifelse(BD$educacao_1>median(BD$educacao_1, na.rm = T),1,0)
BD$mulh <- ifelse(BD$mulheres>median(BD$mulheres, na.rm = T),1,0)
BD$pib <- ifelse(BD$PIB_PC>median(BD$PIB_PC, na.rm = T),1,0)


# interação:  período 2 X always treated X baixa educação
BD$pl_1_t2_educ1 <- ifelse(BD$grupo==1 & BD$NR_TURNO==2 & BD$educ1==1,1,0)
BD$pl_1_t2_educ0 <- ifelse(BD$grupo==1 & BD$NR_TURNO==2 & BD$educ1==0,1,0)
BD$pl_2_t2_educ1 <- ifelse(BD$grupo==2 & BD$NR_TURNO==2 & BD$educ1==1,1,0)
BD$pl_2_t2_educ0 <- ifelse(BD$grupo==2 & BD$NR_TURNO==2 & BD$educ1==0,1,0)

# interação:  período 2 X treated em t2 X mulheres
BD$pl_1_t2_mulh1 <- ifelse(BD$grupo==1 & BD$NR_TURNO==2 & BD$mulh==1,1,0)
BD$pl_1_t2_mulh0 <- ifelse(BD$grupo==1 & BD$NR_TURNO==2 & BD$mulh==0,1,0)
BD$pl_2_t2_mulh1 <- ifelse(BD$grupo==2 & BD$NR_TURNO==2 & BD$mulh==1,1,0)
BD$pl_2_t2_mulh0 <- ifelse(BD$grupo==2 & BD$NR_TURNO==2 & BD$mulh==0,1,0)

# interação:  período 2 X treated em t2 X alto PIB
BD$pl_1_t2_pib1 <- ifelse(BD$grupo==1 & BD$NR_TURNO==2 & BD$pib==1,1,0)
BD$pl_1_t2_pib0 <- ifelse(BD$grupo==1 & BD$NR_TURNO==2 & BD$pib==0,1,0)
BD$pl_2_t2_pib1 <- ifelse(BD$grupo==2 & BD$NR_TURNO==2 & BD$pib==1,1,0)
BD$pl_2_t2_pib0 <- ifelse(BD$grupo==2 & BD$NR_TURNO==2 & BD$pib==0,1,0)






# modelo básico
dd1 <- feols(pct_lula~t2+pl_1_t2+pl_2_t2|id_secao, data = BD)
# incluindo controles que variam no tempo
dd2 <- feols(pct_lula~t2+pl_1_t2+pl_2_t2+gov_2t|id_secao, data = BD)
# Clustering SE por municipio
dd3 <- feols(pct_lula~t2+pl_1_t2+pl_2_t2|id_secao, data = BD, cluster = BD$CD_MUNICIPIO )
# incluindo controles que variam no tempo
dd4 <- feols(pct_lula~t2+pl_1_t2+pl_2_t2+gov_2t|id_secao, data = BD, cluster = BD$CD_MUNICIPIO)
etable(dd1, dd2, dd3, dd4)


# interagindo tratamento com covariaveis
dd5 <- feols(pct_lula~t2+
              pl_1_t2_educ0 + pl_1_t2_educ1 + pl_2_t2_educ0 + pl_2_t2_educ1 |
              id_secao, data = BD, cluster = BD$CD_MUNICIPIO)
dd6 <- feols(pct_lula~t2+
              pl_1_t2_mulh0 + pl_1_t2_mulh1 + pl_2_t2_mulh0 + pl_2_t2_mulh1 |
              id_secao, data = BD, cluster = BD$CD_MUNICIPIO)
dd7 <- feols(pct_lula~t2+
              pl_1_t2_pib0 + pl_1_t2_pib1 + pl_2_t2_pib0 + pl_2_t2_pib1 |
              id_secao, data = BD, cluster = BD$CD_MUNICIPIO)
etable(dd5, dd6, dd7)

