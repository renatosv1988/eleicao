library(ggdag)
library(dagitty)
library(ggm)
library(fixest)
library(stargazer)


# dag model ----------------------------------
#' http://dagitty.net/dags.html?id=Uxpjl7



d <- dagitty('dag {
bb="0,0,1,1"
"Comparecimento T1" [pos="0.735,0.066"]
"Comparecimento T2" [outcome,pos="0.735,0.667"]
"Distância média" [pos="0.639,0.243"]
"Partido Prefeito" [pos="0.278,0.232"]
"Passe Livre T1" [pos="0.137,0.089"]
"Passe Livre T2" [exposure,pos="0.151,0.674"]
"Votos Bozo T1" [pos="0.418,0.492"]
SES [pos="0.477,0.207"]
"Comparecimento T1" -> "Comparecimento T2"
"Distância média" -> "Comparecimento T1"
"Distância média" -> "Comparecimento T2"
"Partido Prefeito" -> "Passe Livre T1"
"Partido Prefeito" -> "Passe Livre T2"
"Partido Prefeito" -> "Votos Bozo T1"
"Passe Livre T1" -> "Comparecimento T1"
"Passe Livre T1" -> "Passe Livre T2"
"Passe Livre T2" -> "Comparecimento T2"
"Votos Bozo T1" -> "Comparecimento T2"
"Votos Bozo T1" -> "Passe Livre T2"
SES -> "Comparecimento T1"
SES -> "Comparecimento T2"
SES -> "Distância média"
SES -> "Partido Prefeito"
SES -> "Votos Bozo T1"}')
  
#### test Implications ---------------------
test <- dagitty::localTests(x = d, data = cov( df_fuel ))
setDT(test)[p.value > 0.01] %>% nrow() / nrow(test)
setDT(test)[p.value > 0.05] %>% nrow() / nrow(test)
setDT(test)[p.value > 0.10] %>% nrow() / nrow(test)

testImplications <- function( covariance.matrix, sample.size ){
  library(ggm)
  tst <- function(i){ pcor.test( pcor(i,covariance.matrix), length(i)-2, sample.size )$pvalue }
  tos <- function(i){ paste(i,collapse=" ") }
  implications <- list(c("bozo_votes_1","close_election_gov1"),
                       c("bozo_votes_1","distance_polling_station"),
                       c("bozo_votes_1","turnout_1","income_polling_station"),
                       c("bozo_votes_1","turnout_2","close_election_gov1","free_transport","income_polling_station"),
                       c("close_election_gov1","distance_polling_station"),
                       c("close_election_gov1","income_polling_station"),
                       c("close_election_gov1","turnout_1"),
                       c("distance_polling_station","free_transport"),
                       c("distance_polling_station","income_polling_station"),
                       c("free_transport","income_polling_station","bozo_votes_1"),
                       c("free_transport","turnout_1","income_polling_station"),
                       c("free_transport","turnout_1","bozo_votes_1"))
  data.frame( implication=unlist(lapply(implications,tos)),
              pvalue=unlist( lapply( implications, tst ) ) )
  
}



test <- testImplications( covariance.matrix = cov( df_fuel ),
                          sample.size = nrow(df_fuel))

setDT(test)[pvalue > 0.05] %>% nrow() / nrow(test)
cor(df_fuel$x_pop_2010  , df_fuel$x_normalized_closeness_centrality_avg)



# 
# # vars mais repetidas
# implication <- setDT(test)[pvalue < 0.1]$implication
# 
# aaa <- test[implication %like% 'street_pop']
# 
# all_vars <- str_split(implication, pattern = ' ')
# all_vars <- unlist(all_vars)
# table(all_vars) %>% sort()
# 
# ggplot(data=df_fuel, aes(y=y_energy_per_capita, x=x_wghtd_mean_household_income_per_capita)) +
#   geom_point() +
#   geom_smooth() +
#   theme_classic()
# 
# ggplot(data=df_fuel, aes(x=y_energy_per_capita)) +
#   geom_density()



#### DAG Figure ---------------------
#
# # convert DAG to tidy df
# dag_tidy <- tidy_dagitty(d)
#
# # label of variables
# vars_urban_form <- c('f_compact_contig_inter_dens', 'x_circuity_avg', 'x_density_pop_02km_2014', 'x_land_use_mix', 'x_normalized_closeness_centrality_avg', 'I(x_pop_2010 * f_compact_contig_inter_dens)')
# setDT(dag_tidy$data)
# dag_tidy$data[, var_type := fcase(name %in% 'y_energy_per_capita', 'outcome',
#                                     name %in% vars_urban_form, 'expousure')]
#
# # factor variables with lables
# dag_tidy$data$name <- factor(dag_tidy$data$name,
#                              levels= unique(dag_tidy$data$name),
#                              labels=c('VKT'
#                                     , 'f.compact_contg_interDenst'
#                                     , 'sinuosidade'
#                                     , 'densidade_pop'
#                                     , 'mix_uso_solo'
#                                     , 'idade_frota'
#                                     , 'declividade'
#                                     , 'closeness_centrl'
#                                     , 'pop_total'
#                                     , 'prop_dom_auto'
#                                     , 'prop_dom_urban'
#                                     , 'prop_alta_escol'
#                                     , 'pop_razao_dependnc'
#                                     , 'unidade_federacao'
#                                     , 'ruas_km_por_pop'
#                                     , 'pop_cresc_1990-2014'
#                                     , 'renda'
#                                     , 'energia_pct')
#                              )
#
#
# temp_fig <-ggplot(data = dag_tidy,
#                    aes( x = x, y = y, xend = xend, yend = yend, color=var_type), ) +
#               geom_dag_point(alpha=.7, size=10) +
#               geom_dag_edges( edge_colour='gray40', edge_width=.6, edge_alpha=.7,
#                               arrow_directed = grid::arrow(length = grid::unit(5, "pt"), type = "closed"),
#                               ) +
#               geom_dag_text(size=2.5, col = "black", nudge_y=-.06) +
#               theme_dag() +
#   theme(legend.position='none')
#
#
# ggsave(temp_fig, file='./figures/dag_simple.png', dpi=300,
#        width = 16, height = 12, units = 'cm')


#### determine adjustments (model specification) ---------------------


cols_canonical <- dagitty::adjustmentSets(d,
                                exposure = 'Passe Livre T2',
                                type = 'canonical', # 'minimal' 'canonical'
                                effect = 'total')

cols_minimal <- dagitty::adjustmentSets(d,
                                        exposure = 'Passe Livre T2',
                                        type = 'minimal', # 'minimal' 'canonical'
                                        effect = 'total')








### check  heteroscedasticity-------------
#' https://evalf21.classes.andrewheiss.com/example/standard-errors/

# run base model
model_output_all_lm <- lm( formula(model_spec_all[[1]]), data=df_fuel)

# see results
summary(model_output_all_lm)
stargazer::stargazer( model_output_all_lm,   type = 'text')

# check 1
performance::check_heteroscedasticity(model_output_all_lm)

# check 2
# There is heteroscedasticity if p-values are smaller than 0.05
lmtest::bptest(model_output_all_lm) # Breusch-Pagan test
car::ncvTest(model_output_all_lm)  # NCV Test

# check 3
# top and bottom left: if cloud of points are random, then no heteroscedasticity
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(model_output_all_lm)

# check 4
fitted_data <- broom::augment(model_output_all_lm, data = df_log)

# check 4.1
# if distribution is normal,  then isn’t actually heteroskedastic
ggplot(fitted_data, aes(x = .resid)) +
  geom_histogram(binwidth = .05, color = "white", boundary = 1000)

# check 4.2
# Look at relationship between fitted values and residuals
# to check if errors are clustered
ggplot(fitted_data, aes(x = .fitted, y = .resid)) +
  geom_point(aes(color = x_state)) +
  facet_wrap(. ~i_name_region) +            # x_state i_name_region
  geom_smooth(method = "lm") +
  # scale_color_distiller(palette = 'grays') +
  theme_minimal()

#>>>> It seems errors are clustered by region


# in case there is heteroscedasticity, use ols with robust standard errors
mrobust <- feols(formula(model_spec_all[[1]]), data = df_log, cluster = "i_name_region")
summary(mrobust)



### check  multicollinearity-------------
library(mctest)
library(qgraph)

#VIF
vif_test <- mctest::imcdiag(model_output_all_lm, method ='VIF')
vif_test

# identify multicolinear variables
colinear_varnames <- subset(as.data.frame(vif_test$alldiag), VIF   ==T) %>% rownames()
colinear_varnames <- names(df_fuel)[names(df_fuel) %in% colinear_varnames]
qgraph::qgraph( cor( dplyr::select(df_fuel, colinear_varnames) ) ,
                #theme='gray',
                vsize=10,
                label.cex=4,
                labels=names(dplyr::select(df_fuel, colinear_varnames)),
                edge.labels = TRUE,
                layout='spring')


# check partial correlations
library(ppcor)
pcor(df_fuel, method = "pearson")



# check state Vs region controls / clusters --------------------------
ct_region_cluster_region <- "y_energy_per_capita ~f_compact_contig_inter_dens+x_circuity_avg+x_density_pop_02km_2014+x_land_use_mix+x_normalized_closeness_centrality_avg+ x_mean_fleet_age+ x_mean_slope+ x_pop_2010+ x_prop_dom_urban+ x_prop_high_educ+ x_prop_razao_dep+ x_total_pop_growth_1990_2014+ x_wghtd_mean_household_income_per_capita + i_name_region + x_pop_2010*f_compact_contig_inter_dens"
ct_state_cluster_state <- "y_energy_per_capita ~f_compact_contig_inter_dens+x_circuity_avg+x_density_pop_02km_2014+x_land_use_mix+x_normalized_closeness_centrality_avg+ x_mean_fleet_age+ x_mean_slope+ x_pop_2010+ x_prop_dom_urban+ x_prop_high_educ+ x_prop_razao_dep+ x_total_pop_growth_1990_2014+ x_wghtd_mean_household_income_per_capita + x_state       + x_pop_2010*f_compact_contig_inter_dens"

# i_name_region
# x_state

ct_region_cluster_region <- feols(formula(ct_region_cluster_region), data = df_log, cluster = "i_name_region")
ct_state_cluster_region  <- feols(formula(ct_state_cluster_state), data = df_log, cluster = "i_name_region")
ct_state_cluster_state   <- feols(formula(ct_state_cluster_state), data = df_log, cluster = "x_state")

ll <- list(ct_region_cluster_region, ct_state_cluster_region , ct_state_cluster_state)
etable(ll)

# com interaction
# controle por regiao, cluster por regiao: land use and density ok sig.
# controle por estado, cluster por regiao: land use and density ok sig.
# controle por estado, cluster por estado: no sig

# sem interaction
# controle por regiao, cluster por regiao: land use and density ok sig.
# controle por estado, cluster por regiao: no sig land use
# controle por estado, cluster por estado: land use and density ok sig.


#### check interaction --------------
#' cor(df_fuel$x_pop_2010  , df_fuel$f_compact_contig_inter_dens)
library(interactions)
a <- lm(formula(model_spec_all[[1]]), data = df_log)
interact_plot(a, pred = 'f_compact_contig_inter_dens',
              modx = 'x_pop_2010' ,
              plot.points = TRUE) # interval = TRUE

interact_plot(a, pred = 'f_compact_contig_inter_dens', modx = 'x_pop_2010', linearity.check = TRUE,
              plot.points = TRUE)



### 666 feols pra valer ------------------------------------

# feols( formula(  all_models_specs[[1]][[1]] ) , data=df_log, cluster = "i_name_region")

for ( i in names(all_models_specs) ){# i = names(all_models_specs)[1]
  
  specs <- all_models_specs[i]
  specs <- unlist(specs)
  temp_model <- lapply(X=specs, FUN=function(m){feols( formula(m), data=df_log, cluster='i_name_region')})
  saveRDS(temp_model, file = paste0('./output/regression_output/', i,'.rds'))
  
}


# # check marginal effects ----------------------

library(ggplot2)
library(margins)
library(marginaleffects)

# inverse hyperbolic sine
invs <- function(x){ log(x + sqrt(x^2 + 1) ) }


all_models_specs[1]

mlog <- feols(fml = log(y_energy_per_capita) ~ invs(f_compact_contig_inter_dens) + log(x_circuity_avg) + log(x_density_pop_02km_2014) + log(x_land_use_mix) + log(x_normalized_closeness_centrality_avg) + log(x_mean_fleet_age) + log(x_mean_slope) + log(x_pop_2010) + log(x_prop_dom_urban) + log(x_prop_high_educ) + log( x_prop_razao_dep) + log(x_state) + invs(x_total_pop_growth_1990_2014) + log(x_wghtd_mean_household_income_per_capita) + I(log(x_pop_2010)*invs(f_compact_contig_inter_dens))
              , data=df_raw, cluster='i_name_region')

summary(mlog)

plot_cme(mlog, effect = "f_compact_contig_inter_dens",
         condition = 'x_pop_2010')



# # compare models performance ----------------------
# library(performance)
#
# l <- list(ct_region_cluster_region, ct_state_cluster_state)
#
# tem_model <- lapply(X=l, FUN= lm, data=df_log)
# performance::check_model(tem_model)
# performance::compare_performance(tem_model, rank = T)  %>% plot()










  