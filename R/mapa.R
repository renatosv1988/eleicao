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

# base por municipio
BD <- BD[BD$NR_TURNO==1,]
BD$aptos <- ave(BD$QT_APTOS, BD$code_muni, FUN=sum)
BM <- BD[!duplicated(BD$code_muni),]

# MAPA POR UF ------------------------------------------------------------------
UFM <- read_state()
# MAPA DOS MUNICIPIOS POR Grupo de Tratamento ----------------------------------
MC <- read_municipal_seat()

MMC <- merge(MC, BM[,c("code_muni","dummy_pt",  "grupo", "aptos")],
             by.x="code_muni", by.y="code_muni")

# Redefine grupos
MMC$grupo <- ifelse(MMC$dummy_pt==0, "sem tp",
             ifelse(MMC$grupo==0, "sem pl",
             ifelse(MMC$grupo==2, "pl 2","pl 1")))
                    
MMC <- MMC[order(MMC$aptos),]

# plot map
maps <- ggplot() +
 geom_sf(fill="white",data=UFM, color="gray", size=.15) +
 geom_point(aes(color=grupo, size=aptos, geometry=geometry),
            data=MMC[MMC$grupo=="sem tp",], stat = "sf_coordinates", alpha = 0.75) +
 geom_point(aes(color=grupo, size=aptos, geometry=geometry),
            data=MMC[MMC$grupo=="sem pl",], stat = "sf_coordinates", alpha = 0.75) +
 geom_point(aes(color=grupo, size=aptos, geometry=geometry),
            data=MMC[MMC$grupo=="pl 2",], stat = "sf_coordinates", alpha = 0.75) +
 geom_point(aes(color=grupo, size=aptos, geometry=geometry),
            data=MMC[MMC$grupo=="pl 1",], stat = "sf_coordinates", alpha = 0.75) +
 theme_void() +
 scale_color_manual(values = c("sem tp"="lightgray","sem pl"="slategray","pl 2"="darkgoldenrod","pl 1"="firebrick"),
                    labels = c("sem tp"="sem TP","sem pl"="sem PL","pl 2"="PL no 2ºT","pl 1"="PL no 1T e 2T"),) +
 labs(color="Grupos: ") +
 theme(legend.position = "none") +
 scale_size_continuous(range=c(0.01, 8), breaks=c(10^4, 10^5, 10^6, 10^7), guide="none") +
 coord_sf(xlim = c(-74.15,-34.73), ylim = c(-33.79,5.46))

# plot bars
df <- data.frame(grupo=c("sem tp","sem pl", "pl 2", "pl 1"),
                 pop=c(sum(MMC$aptos[MMC$grupo=="sem tp"]),
                       sum(MMC$aptos[MMC$grupo=="sem pl"]),
                       sum(MMC$aptos[MMC$grupo=="pl 2"]),
                       sum(MMC$aptos[MMC$grupo=="pl 1"])))

bars <- ggplot() +
 geom_col(aes(x=pop/1000000, y=grupo, fill=grupo), data=df, alpha=0.9) +
 theme_classic() +
 scale_fill_manual(values = c("sem tp"="lightgray","sem pl"="slategray",
                              "pl 2"="darkgoldenrod","pl 1"="firebrick")) +
 theme(legend.position = "none",
       plot.background = element_rect(fill='transparent', color=NA),
       panel.background = element_rect(fill='transparent')) +
 labs(y="",x="electorate (million)", title = "total electorate",
      subtitle = "per treatment group") +
 scale_y_discrete(limits = c("sem tp","sem pl", "pl 2", "pl 1"),
                  labels = c("sem tp"="no public\ntransportation",
                             "sem pl"="no free-fare",
                             "pl 2"="free-fare on\n2nd round only",
                             "pl 1"="free-fare on\nboth rounds")) +
 coord_cartesian(xlim=c(0,50))


# combine plots
base_square <- data.frame(id = rep(1,4),
 x = c(-84.15, -74.15,-34.73,-34.73),
 y = c(-33.79, 5.46, 5.46, -33.79))

base_map <- ggplot() +
 geom_polygon(aes(x=x,y=y),data=base_square, fill="white") +
 theme_void() +
 coord_cartesian(expand = c(0))

ggdraw(base_map) +
 draw_plot(maps, x=0.1, y=0, width=0.9, height=1) +
 draw_plot(bars, x=0, y=0.04, width=.45, height=.5)

ggsave(file= 'mapa.pdf', 
       width = 17.8, height = 17.8, units='cm', dpi = 300)

