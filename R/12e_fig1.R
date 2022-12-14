# figure 1 EN ------------------------------------

fig1_a <- readRDS('./figures/fig_1A.rds')
fig1_b <- readRDS('./figures/fig_1B.rds')
fig1_c <- readRDS('./figures/fig_1C.rds')
fig1_d <- readRDS('./figures/fig_1D.rds')

p1 <- fig1_a + fig1_b + fig1_c + fig1_d +
       plot_annotation(tag_levels = 'A') +
       plot_layout(ncol = 4)



p1

ggsave(plot=p1, file= './figures/f1_final.pdf', 
       width = 21, height = 7, units='cm', dpi = 300)


# figure 1 PT  ------------------------------------


# translate
fig1_a <- fig1_a + labs(y='Estimativa e I.C. a 95%', x = 'Ano da eleição')
fig1_b <- fig1_b + labs(x = 'Ano da eleição')
fig1_c <- fig1_c + labs(x = 'Decis de escolaridade\nde seções eleitorais')
fig1_d <- fig1_d + labs(x = 'Decis de densidade\nde seções eleitorais')


p1_pt <- fig1_a + fig1_b + fig1_c + fig1_d +
 plot_annotation(tag_levels = 'A') +
 plot_layout(ncol = 4)

p1_pt

ggsave(plot=p1_pt, file= './figures/f1_final_pt.pdf', 
       width = 21, height = 7, units='cm', dpi = 300)

