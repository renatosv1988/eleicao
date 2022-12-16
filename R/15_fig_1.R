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

