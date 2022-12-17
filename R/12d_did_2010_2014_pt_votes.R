library(data.table)
library(ggplot2)
library(modelsummary)
library(purrr)
library(fixest)
library(ggsci)
library(patchwork)

options(scipen = 999)


# read data ----------------------------------------------------------------------

df <- fread("../../data/base_DiD2010_2022_secoes.csv", encoding = "Latin-1")

muni_ipw <- fread("../../data/ipw_municipalities.csv", encoding = "Latin-1")


# add ipw to electoral sections
st <- merge(df, muni_ipw, by="code_muni", all.x = T)





# Select observations ----------------------------------------------------------------------


# keep only municipalities that were eventually treated
st <- st[passe_livre_2==1, ]

# excluir cidades que SEMPRE tiveram passe livre
st <- subset(st, is.na(passe_livre_always))





# recode variables ----------------------------------------------------------------------

# create dummy for treatment in the 1st round
st[, pl1 := fifelse(passe_livre_1 == 1, 1, 0)]

# create dummy for treatment ONLY in the 2nd round
st[, pl2 := fifelse(passe_livre_1 == 0, 1, 0)]

# year as categorical var
st[, year := as.character(ANO_ELEICAO)]





# Difference in turnout btw t2 and t1 -----------------------------------------

m1_b = feols(variacao_lula ~ i(year, pl2, "2018") | id_secao+year, 
             cluster = 'code_muni',
             data = st)

m2_b = feols(variacao_lula ~ i(year, pl2, "2018") | id_secao+year,
             cluster = 'code_muni',
             weights = ~ipw2,
             data = st)


output_m1_b <- data.frame(Weighted = 'No',
                          year = c(2010, 2014, 2022, 2018),
                          coef = c(m1_b$coefficients, 0),
                          se = c(m1_b$se,0))

output_m2_b <- data.frame(Weighted = 'Yes',
                          year = c(2010, 2014, 2022, 2018),
                          coef = c(m2_b$coefficients, 0),
                          se = c(m2_b$se,0))

output_b <- rbind(output_m1_b, output_m2_b)
setDT(output_b)
output_b[, ymin := coef - 1.96*se]
output_b[, ymax := coef + 1.96*se]





# Assemble figure IPW  --------------------------------------------------------------------

# get max and min y values
values <- lapply(c( output_b$ymax, output_b$ymin), FUN=base::abs)
values <- unlist(values)
max_y <- max(values)
min_y <- -1*max_y


default_theme <- list( theme_classic() ,
                       theme(text = element_text(size=9))
                       )



# Figure
fig_ipw <- 
 ggplot(data = output_b, aes(x= year, y=coef, color=Weighted)) +
 geom_vline(xintercept = 2018, color='gray80', linetype = 'dashed') +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 geom_point(position = position_dodge2(width = 1)) +
 geom_pointrange(position = position_dodge2(width = 1),
                 aes(x=year, y=coef,
                     ymin = ymin,
                     ymax = ymax)) +
 scale_x_continuous(breaks = c(2010, 2014, 2018, 2022)) +
 labs(y='', x = 'Year') +
 ylim(c(min_y, max_y)) +
 default_theme + 
 scale_color_manual(values=c('#0e8bb1', '#df8f45'), guide = guide_legend()) +
 theme(legend.position="bottom")



fig_ipw


# ##### save plot
# 
# ggsave(plot=fig_ipw, file= './figures/si_1_avg_effects_on_pt_vote_ipw.pdf', 
#        width = 8.7, height = 7, units='cm', dpi = 300)
# 
# 
# 





# NO IPW  --------------------------------------------------------------------

output_b <- subset(output_b, Weighted == 'No')


# get max and min y values
values <- lapply(c(output_b$ymax, output_b$ymin), FUN=base::abs)
values <- unlist(values)
max_y <- max(values)
max_y <- ifelse(max_y < 0.05, 0.05, max_y)
min_y <- -1*max_y


default_theme <- list( theme_classic() ,
                       theme(text = element_text(size=9))
)




# Figure C
fig_1b <- 
 ggplot(data = output_b, aes(x= year, y=coef, color=Weighted)) +
 annotate("rect", fill='gray95', xmin = 2021, xmax = 2023, 
          ymin = -Inf, ymax = Inf) +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +

  geom_pointrange(size=.2, color= '#79af97',
                 aes(x=year, y=coef,
                     ymin = ymin,
                     ymax = ymax)) +
 scale_x_continuous(breaks = c(2010, 2014, 2018, 2022)) +
 scale_y_continuous(limits = c(min_y, max_y), labels = scales::percent) +
 labs(y='', x = 'Year') +
 default_theme + 
 theme(legend.position='none')


fig_1b





##### save plot


saveRDS(fig_1b,file='./figures/fig_1B.rds')


 