library(data.table)
library(fixest)
library(ggplot2)
library(remotes)
#remotes::install_github("grantmcdermott/ggiplot")
library(remotes)
library(ggiplot)
library(cowplot)

# ler dados
df <- fread("../../data/base_DiD2010_2022_secoes.csv", encoding = "Latin-1")
table(df$ANO_ELEICAO)

# filtrar apenas seções tratadas
st <- df[df$passe_livre_2==1,]

# criar dummy de tratamento já no 1º turno
st$pl1 <- ifelse(st$passe_livre_1==1,1,0)

# criar dummy de tratamento apenas no 2º turno
st$pl2 <- ifelse(st$passe_livre_1==0,1,0)

# ano como categorica
st$ano <- as.character(st$ANO_ELEICAO)
#st$ano <- factor(st$ano, levels = c("2018", "2022", "2010", "2014"))


# ler pesos
WM <- fread("R/descritivo_2010_2022/ipow_muni.csv")
st <- merge(st, WM[,c("code_muni","ipw")], by="code_muni", all.x = T)


# modelo de diferença de comparecimento
m1 = feols(variacao_comparecimento ~ i(ano, pl2, "2018") | id_secao+ano, st,
                cluster = st$CD_MUNICIPIO)
m2 = feols(variacao_comparecimento ~ i(ano, pl2, "2018") | id_secao+ano, st,
                cluster = st$CD_MUNICIPIO, weights = ~ipw)

p1 <- ggiplot(list("sem pesos"=m1,"ipw"=m2)) +
 theme_classic() +
 coord_cartesian(ylim=c(-0.1,0.1)) +
 labs(title = "variação de comparecimento, 2ºT - 1ºT.") +
 theme(legend.position = "bottom")

# modelo de diferença de votos PT
m1 = feols(variacao_luva ~ i(ano, pl2, "2018") | id_secao+ano, st,
           cluster = st$CD_MUNICIPIO)
m2 = feols(variacao_luva ~ i(ano, pl2, "2018") | id_secao+ano, st,
           cluster = st$CD_MUNICIPIO, weights = ~ipw)
p2 <- ggiplot(list("sem pesos"=m1,"ipw"=m2)) +
 theme_classic() +
 coord_cartesian(ylim=c(-0.1,0.1)) +
 labs(title = "variação de votos PT, 2ºT - 1ºT.") +
 theme(legend.position = "bottom")


# modelo de comparecimento 1ºT.
m1 = feols(comparecimento_t1 ~ i(ano, pl1, "2018") | id_secao+ano, st,
           cluster = st$CD_MUNICIPIO)
m2 = feols(comparecimento_t1 ~ i(ano, pl1, "2018") | id_secao+ano, st,
           cluster = st$CD_MUNICIPIO, weights = ~ipw)
p3 <- ggiplot(list("sem pesos"=m1,"ipw"=m2)) +
 theme_classic() +
 coord_cartesian(ylim=c(-0.1,0.1)) +
 labs(title = "comparecimento, 1º T.") +
 theme(legend.position = "bottom")


# modelo de votos PT 1ºT.
m1 = feols(votos_lula_p_t1 ~ i(ano, pl1, "2018") | id_secao+ano, st,
           cluster = st$CD_MUNICIPIO)
m2 = feols(votos_lula_p_t1 ~ i(ano, pl1, "2018") | id_secao+ano, st,
           cluster = st$CD_MUNICIPIO, weights = ~ipw)
p4 <- ggiplot(list("sem pesos"=m1,"ipw"=m2)) +
 theme_classic() +
 coord_cartesian(ylim=c(-0.1,0.1)) +
 labs(title = "Votos no PT, 1º T.") +
 theme(legend.position = "bottom")
 

ap <- plot_grid(p1,p2,p3,p4, nrow=2, ncol=2)
save_plot("figures/DD_2010_2014_base_results.png", ap, base_width = 12, base_height = 10)
