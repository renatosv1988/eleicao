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


    


# get total voter by muni and treated condition
temp_check1 <- st[ANO_ELEICAO==2022,]
temp_check1 <- temp_check1[, .( QT_APTOS = sum(QT_APTOS)), by = .(code_muni, passe_livre_2, passe_livre_1)]

total_voters <- sum(temp_check1$QT_APTOS)
total_voters
#> 155.631.548

temp_check1[, .(sum(QT_APTOS) ), by= passe_livre_2]
temp_check1[, .(sum(QT_APTOS) / total_voters), by= passe_livre_2]
#>    passe_livre_2       V1
#> 1:             0 0.512633
#> 2:             1 0.487367

temp_check1[, .(sum(QT_APTOS) ), by= passe_livre_1]
temp_check1[, .(sum(QT_APTOS) / total_voters), by= passe_livre_1]
#>    passe_livre_1        V1
#> 1:             0 0.8148314
#> 2:             1 0.1851686


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




# number of municipalities with fare-free transit policy 
 temp_check <- st[year==2022]
 
 # 1st round
 pl_1 <- unique(temp_check$code_muni[which(temp_check$passe_livre_1==1)])
 length(pl_1)
 #> 82
 
 # second round
 pl_2 <- unique(temp_check$code_muni[which(temp_check$passe_livre_2==1)])
 length(pl_2)
 #> 379

 length(unique(c(pl_1, pl_2)))


 # metro only 6666666666666666
 # st <- subset(st, metro_only == 0)
 
  
# A) Turnout in 1st round -----------------------------------------

m1_a = feols(comparecimento_t1 ~ i(year, pl1, "2018") | id_secao+year, 
             cluster = 'code_muni',
             data = st)

m2_a = feols(comparecimento_t1 ~ i(year, pl1, "2018") | id_secao+year,
             cluster = 'code_muni',
             weights = ~ipw1,
             data = st)


output_m1_a <- data.frame(Weighted = 'No',
                          year = c(2010, 2014, 2022, 2018),
                          coef = c(m1_a$coefficients, 0),
                          se = c(m1_a$se,0))

output_m2_a <- data.frame(Weighted = 'Yes',
                          year = c(2010, 2014, 2022, 2018),
                          coef = c(m2_a$coefficients, 0),
                          se = c(m2_a$se,0))

output_a <- rbind(output_m1_a, output_m2_a)
setDT(output_a)
output_a[, ymin := coef - 1.96*se]
output_a[, ymax := coef + 1.96*se]






# B) Difference in turnout btw t2 and t1 -----------------------------------------

m1_b = feols(variacao_comparecimento ~ i(year, pl2, "2018") | id_secao+year, 
             cluster = 'code_muni',
             data = st)

m2_b = feols(variacao_comparecimento ~ i(year, pl2, "2018") | id_secao+year,
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
 
 
 
 

# Assemble figure  --------------------------------------------------------------------

# get max and min y values
values <- lapply(c(output_a$ymax, output_b$ymax, output_a$ymin, output_b$ymin), FUN=base::abs)
values <- unlist(values)
max_y <- max(values)
min_y <- -1*max_y


default_theme <- list( scale_color_jama() ,
                       theme_classic() ,
                       theme(text = element_text(size=9))
                       )


# Figure B
fig_b <- 
 
 ggplot() +
 geom_vline(xintercept = 2018, color='gray80', linetype = 'dashed') +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 geom_point(data = output_a, aes(x= year, y=coef, color=Weighted),
            position = position_dodge2(width = 1)) +
 geom_pointrange(data = output_a,
                 position = position_dodge2(width = 1),
                 aes(x=year, y=coef, color=Weighted,
                     ymin = ymin,
                     ymax = ymax)) +
 scale_x_continuous(breaks = c(2010, 2014, 2018, 2022)) +
 labs(y='Estimate and 95% Conf. Int.', x = 'Year') +
 ylim(c(min_y, max_y)) +
 default_theme + 
 theme(legend.position="none")


# Figure C
fig_a <- 
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
 scale_color_jama(guide = guide_legend()) +
 theme(legend.position="bottom")





# fig_t
fig_t <- readRDS(file='./figures/fig_1A.rds')


p <- fig_a + fig_b +  fig_t +
     plot_annotation(tag_levels = 'A') +
     plot_layout(ncol = 3)

p



##### save plot --------------------------------------------------------------------


ggsave(plot=p, file= './figures/fig_1_avg_effects_ipw.pdf', 
       width = 17.8, height = 8, units='cm', dpi = 300)












##### NO IPW  --------------------------------------------------------------------

output_a <- subset(output_a, Weighted == 'No')
output_b <- subset(output_b, Weighted == 'No')

# get max and min y values
values <- lapply(c(output_a$ymax, output_b$ymax, output_a$ymin, output_b$ymin), FUN=base::abs)
values <- unlist(values)
max_y <- max(values)
max_y <- ifelse(max_y < 0.05, 0.05, max_y)
min_y <- -1*max_y




# Figure a
fig_a <- 
 ggplot(data = output_b, aes(x= year, y=coef)) +
 annotate("rect", fill='gray95', xmin = 2021, xmax = 2023, 
          ymin = -Inf, ymax = Inf) +
 # geom_vline(xintercept = 2018, color='gray80', linetype = 'dashed') +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 geom_point(color='#0e8bb1') +
 geom_pointrange(color='#0e8bb1',
                 aes(x=year, y=coef,
                     ymin = ymin,
                     ymax = ymax)) +
 scale_x_continuous(breaks = c(2010, 2014, 2018, 2022)) +
 labs(y='', x = 'Year') +
 ylim(c(min_y, max_y)) +
 default_theme + 
 scale_color_jama(guide = guide_legend()) +
 theme(legend.position="bottom")



# Figure b
fig_b <- 
 ggplot() +
 # geom_vline(xintercept = 2018, color='gray80', linetype = 'dashed') +
 annotate("rect", fill='gray95', xmin = 2021, xmax = 2023, 
          ymin = -Inf, ymax = Inf) +
 geom_hline(yintercept = 0, color='gray80', linetype = 'dashed') +
 geom_point(data = output_a, aes(x= year, y=coef), color='#0e8bb1') +
 geom_pointrange(data = output_a,  color='#0e8bb1',
                 aes(x=year, y=coef,
                     ymin = ymin,
                     ymax = ymax)) +
 scale_x_continuous(breaks = c(2010, 2014, 2018, 2022)) +
 labs(y='Estimate and 95% Conf. Int.', x = 'Year') +
 ylim(c(min_y, max_y)) +
 default_theme + 
 theme(legend.position="none")







# fig_t
fig_t <- readRDS(file='./figures/fig_1A.rds')


p2 <- fig_a + fig_b + fig_t +
 plot_annotation(tag_levels = 'A') +
 plot_layout(ncol = 3)

p2



##### save plot --------------------------------------------------------------------


ggsave(plot=p2, file= './figures/fig_1_avg_effects.pdf', 
       width = 17.8, height = 8, units='cm', dpi = 300)

