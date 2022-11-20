
library(patchwork)


# read and filter data  ---------------------------------------------------------------

df <- fread("../../data/base_DiD2022_2018_secoes.csv")


# depois balancer
t <- table(df$id_secao) |> as.data.table()
t <- t[N ==4]

df2 <- df[ id_secao %in% t$V1]

df2[, passe := max(passe_livre), by= id_secao]
df2[, dummy_turno := fifelse(NR_TURNO==2,1,0)]
df2[, dummy_ano := fifelse(ANO_ELEICAO==2022,1,0)]

# share of PT votes
df2[, votos_lula_p := sum(votos_lula) / sum(votos_validos), by = .(ANO_ELEICAO, NR_TURNO, id_secao)]

# excluir seções de cidades sem sistema de ônibus
df2 <- subset(df2, dummy_pt==1)

# excluir cidades que SEMPRE tiveram passe livre
df2 <- subset(df2, is.na(passe_livre_always))




# excluir seções de cidades sem sistema de ônibus
df2 <- subset(df2, dummy_pt==1)

# excluir cidades que SEMPRE tiveram passe livre
df2 <- subset(df2, is.na(passe_livre_always))



# comparecimento ---------------------------------------------------------------

a <- df2[, .(comparecimento = weighted.mean(comparecimento, w = QT_APTOS),
             p25 = quantile(comparecimento,0.25, na.rm=T),
             p75 = quantile(comparecimento,0.75, na.rm=T)
             ),
         by= .(ANO_ELEICAO, NR_TURNO, passe_livre)]

a[, t := paste0(ANO_ELEICAO, '_', NR_TURNO)]
a[, t2 := paste0(NR_TURNO , '_', ANO_ELEICAO)]
head(a)


f_comp <- ggplot() +
           geom_pointrange(data=a,
                           position = position_dodge2(width = .2),
                           show.legend = FALSE,
                           aes(x=t2, y=comparecimento, color=factor(passe_livre),
                               ymin = p25,
                               ymax = p75)) +
           geom_line(data=a,
                     position = position_dodge2(width = .2),
                     aes(x=t2, y=comparecimento,
                                 color=factor(passe_livre),
                                 group=passe_livre)) +
           labs(y='% voter turnout', color = 'treament') +
           ylim(.7, .88) +
           theme_classic()






# lula ---------------------------------------------------------------





b <- df2[, .(comparecimento = weighted.mean(votos_lula_p, w = QT_APTOS),
             p25 = quantile(votos_lula_p,0.25, na.rm=T),
             p75 = quantile(votos_lula_p,0.75, na.rm=T)
),
by= .(ANO_ELEICAO, NR_TURNO, passe_livre)]

b[, t := paste0(ANO_ELEICAO, '_', NR_TURNO)]
head(a)


f_lula <- ggplot() +
           geom_pointrange(data=b,
                           position = position_dodge2(width = .2),
                           show.legend = FALSE,
                           aes(x=t, y=comparecimento, color=factor(passe_livre),
                               ymin = p25,
                               ymax = p75)) +
           geom_line(data=b,
                     position = position_dodge2(width = .2),
                     aes(x=t, y=comparecimento,
                         color=factor(passe_livre),
                         group=passe_livre)) +
           labs(y='% of votes dor Lula', color = 'treament') +
           # ylim(.7, .88) +
           theme_classic()


f_comp / f_lula
