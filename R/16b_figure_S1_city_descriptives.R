library(ggplot2)
library(cowplot)



library(data.table)

options(scipen = 999)


# read data ----------------------------------------------------------------------

df_secoes_2022 <- fread("../../data/base_DiD2022_secoes.csv")


# identify treated cities
df_secoes_2022[, passe_livre_1 := max(NR_TURNO==1 & passe_livre==1), by = id_secao]
df_secoes_2022[, passe_livre_2 := max(NR_TURNO==2 & passe_livre==1), by = id_secao]

# excluir cidades que SEMPRE tiveram passe livre
df_secoes_2022 <- subset(df_secoes_2022, is.na(passe_livre_always))


# aggregate variables at muni level --------------------------------------------
df_muni <- df_secoes_2022[, .(QT_APTOS = sum(QT_APTOS[which(NR_TURNO==1)], na.rm=T),
                              QT_APTOS_log = log(sum(QT_APTOS[which(NR_TURNO==1)], na.rm=T)),
                              idade_60M = weighted.mean(x=idade_60M, w=QT_APTOS, na.rm=T),
                              biometria = weighted.mean(x=biometria, w=QT_APTOS, na.rm=T),
                              votos_jair_validos_t1 = sum(votos_jair[which(NR_TURNO==1)], na.rm = T) / sum(votos_validos[which(NR_TURNO==1)], na.rm = T),
                              votos_jair_validos_t2 = sum(votos_jair[which(NR_TURNO==2)], na.rm = T) / sum(votos_validos[which(NR_TURNO==2)], na.rm = T),
                              mean_dens_1000 = weighted.mean(x=num_1000, w=QT_APTOS, na.rm=T),
                              educacao_1 = weighted.mean(x=educacao_1, w=QT_APTOS, na.rm=T),
                              gov_2t = max(gov_2t),
                              PIB_PC = PIB_PC[1L],
                              pib_log = log(PIB_PC[1L]),
                              passe_livre = passe_livre[1L], 
                              passe_livre_1 = passe_livre_1[1L],
                              passe_livre_2 = passe_livre_2[1L],
                              dummy_pt = dummy_pt[1L]
),
by= .(SG_UF, name_region, code_muni)]


df_muni$plpl <-  paste(df_muni$passe_livre_1, df_muni$passe_livre_2)

df_muni$gr <-  ifelse(df_muni$plpl=="0 1", "FF2",
                      ifelse(df_muni$plpl=="1 1", "FF12",
                             ifelse(df_muni$dummy_pt==0, "NT","NFF")))

df_gr <- df_muni[, .(QT_APTOS = mean(QT_APTOS, na.rm=T),
                     idade_60M = mean(idade_60M, na.rm=T),
                     biometria = mean(biometria, na.rm=T),
                     votos_jair_validos_t1 = mean(votos_jair_validos_t1, na.rm=T),
                     mean_dens_1000 = mean(mean_dens_1000, na.rm=T),
                     educacao_1 = mean(educacao_1, na.rm=T),
                     PIB_PC = mean(PIB_PC, na.rm=T),
                     
                     QT_APTOS_sd = sd(QT_APTOS, na.rm=T),
                     idade_60M_sd = sd(idade_60M, na.rm=T),
                     biometria_sd = sd(biometria, na.rm=T),
                     votos_jair_validos_t1_sd = sd(votos_jair_validos_t1, na.rm=T),
                     mean_dens_1000_sd = sd(mean_dens_1000, na.rm=T),
                     educacao_1_sd = sd(educacao_1, na.rm=T),
                     PIB_PC_sd = sd(PIB_PC, na.rm=T)),
                 by= .(gr)]


# base de dados
df <- data.frame(gr = c("NT",
                        "NFF",
                        "FF12",
                        "FF2"),
                 grl = c("no public transportation",
                        "no free-fare",
                        "free-fare on 2nd round only",
                        "free-fare on both rounds"))
df_muni <- merge(df_muni, df, by="gr")



# população ------------------------------------------------------------------

#plotar gráfico
pa <- ggplot() +
 geom_boxplot(aes(x=QT_APTOS, y=grl, color=gr),data=df_muni,outlier.alpha = 0.5, outlier.size=0.25) +
 theme_classic() +
 scale_color_manual(values=c("NT"="lightgray",
                             "NFF"="darkgray",
                             "FF2"="goldenrod",
                             "FF12"="firebrick"),
                    labels = df$grl) +
 theme(legend.position = "none", axis.text.y = element_blank()) +
 coord_cartesian(xlim=c(100,1200000)) +
 labs(x="voters", y="treatment group", title="A: Number of Voters") +
 scale_x_log10()
pa


# GDP ------------------------------------------------------------------
#plotar gráfico
pb <- ggplot() +
 geom_boxplot(aes(x=PIB_PC, y=grl, color=gr),data=df_muni,outlier.alpha = 0.5, outlier.size=0.25) +
 theme_classic() +
 scale_color_manual(values=c("NT"="lightgray",
                             "NFF"="darkgray",
                             "FF2"="goldenrod",
                             "FF12"="firebrick"),
                    labels = df$grl,
                    name="treatment group:  ") +
 theme(legend.position = "none", axis.text.y = element_blank()) +
 coord_cartesian(xlim=c(1000,200000)) +
 labs(x="GDP per Capita", y="treatment group", title="B: GDP per Capita") +
 scale_x_log10()
pb



# education ------------------------------------------------------------------
#plotar gráfico
pc <- ggplot() +
 #geom_point(aes(x=m,y=grl, color=gr),data=df) +
 #geom_errorbarh(aes(xmin=m-dp, xmax=m+dp, y=grl, color=gr),data=df, height=0) +
 geom_boxplot(aes(x=educacao_1*100, y=grl, color=gr),data=df_muni,outlier.alpha = 0.5, outlier.size=0.25) +
 theme_classic() +
 scale_color_manual(values=c("NT"="lightgray",
                             "NFF"="darkgray",
                             "FF2"="goldenrod",
                             "FF12"="firebrick"),
                    labels = df$grl) +
 theme(legend.position = "none", axis.text.y = element_blank()) +
 coord_cartesian(xlim=c(0,100)) +
 labs(x="% of voters with <8yrs. of educational attainment", y="treatment group", title="C: Lower Educational Attainment")
pc

# Age ------------------------------------------------------------------
#plotar gráfico
pd <- ggplot() +
 geom_boxplot(aes(x=idade_60M*100, y=grl, color=gr),data=df_muni,outlier.alpha = 0.5, outlier.size=0.25) +
 theme_classic() +
 scale_color_manual(values=c("NT"="lightgray",
                             "NFF"="darkgray",
                             "FF2"="goldenrod",
                             "FF12"="firebrick"),
                    labels = df$grl) +
 theme(legend.position = "none", axis.text.y = element_blank()) +
 coord_cartesian(xlim=c(0,50)) +
 labs(x="% of voters >60yrs. old", y="treatment group", title="D: Population ages 60+")
pd

# Bolso ------------------------------------------------------------------
#plotar gráfico
pe <- ggplot() +
 geom_boxplot(aes(x=votos_jair_validos_t1*100, y=grl, color=gr),data=df_muni,outlier.alpha = 0.5, outlier.size=0.25) +
 theme_classic() +
 scale_color_manual(values=c("NT"="lightgray",
                             "NFF"="darkgray",
                             "FF2"="goldenrod",
                             "FF12"="firebrick"),
                    labels = df$grl) +
 theme(legend.position = "none", axis.text.y = element_blank()) +
 coord_cartesian(xlim=c(0,100)) +
 labs(x="% of votes for Bolsonaro (1st round)", y="treatment group", title="E: % of Votes for Bolsonaro")
pe

# Biometria ------------------------------------------------------------------
pf <- ggplot() +
 geom_boxplot(aes(x=biometria, y=grl, color=gr),data=df_muni,outlier.alpha = 0.5, outlier.size=0.25) +
 theme_classic() +
 scale_color_manual(values=c("NT"="lightgray",
                             "NFF"="darkgray",
                             "FF2"="goldenrod",
                             "FF12"="firebrick"),
                    labels = df$grl) +
 theme(legend.position = "none", axis.text.y = element_blank())+
 coord_cartesian(xlim=c(0,1)) +
 labs(x="% of voters", y="treatment group", title="F: % of Electorate with Biometry")
pf

# densidade ------------------------------------------------------------------
#plotar gráfico
pg <- ggplot() +
 geom_boxplot(aes(x=mean_dens_1000, y=grl, color=gr),data=df_muni,outlier.alpha = 0.5, outlier.size=0.25) +
 theme_classic() +
 scale_color_manual(values=c("NT"="lightgray",
                             "NFF"="darkgray",
                             "FF2"="goldenrod",
                             "FF12"="firebrick"),
                    labels = df$grl,
                    name="treatment group: ") +
 theme(legend.position = "none", axis.text.y = element_blank())  +
 coord_cartesian(xlim=c(0,160)) +
 labs(x="Polling Stations per km²", y="treatment group", title="G: Polling Station Density")
pg



ll <- get_legend(pg + theme(legend.position = "right"))


# juntar gráficos
pp <- plot_grid(pa,pb,pc,pd, pe,pf, pg,ll, nrow = 4, ncol=2)
# gráfico final
pp
ggsave(plot=pp, file= './figures2/fs1.pdf', 
       width = 21, height = 21, units='cm', dpi = 300)