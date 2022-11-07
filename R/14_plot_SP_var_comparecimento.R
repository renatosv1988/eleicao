library(did)
library(fixest)
library(data.table)

# ler base de eleições
BD <- fread("data/base_DiD2022_secoes.csv")
ZZ <- fread("data_raw/zonas/eleitorado_local_votacao_2022.csv")

# add lat lon
ZZ[,id_secao:= paste(CD_MUNICIPIO, NR_ZONA, NR_SECAO)]
ZZ <- ZZ[!duplicated(ZZ$id_secao), c("id_secao","NR_LATITUDE","NR_LONGITUDE")]
BD<- merge(BD, ZZ, by="id_secao", all.x = T)


# correlações com a variação do comparecimento por seção
BD2 <- BD[BD$NR_TURNO==2,]
BD1 <- BD[BD$NR_TURNO==1,c("id_secao","QT_COMPARECIMENTO")]
colnames(BD1) <- c("id_secao","QT_COMPARECIMENTO_T1")
BU <- merge(BD1, BD2, by="id_secao", all.x = T) 
BU$var_comp <- (BU$QT_COMPARECIMENTO - BU$QT_COMPARECIMENTO_T1)

sum(SP$QT_COMPARECIMENTO)/
sum(SP$QT_COMPARECIMENTO_T1)

# selecionar apenas seçõpes de São Paulo
SP <- BU[BU$code_muni==3550308,]

SP$size <- abs(SP$var_comp)
SP$dir <- ifelse(SP$var_comp<0, "neg", "pos")
SPX <- subset(SP, NR_LATITUDE < -1)

ggplot() + geom_point(aes(x=NR_LONGITUDE, y=NR_LATITUDE, size=size, color=dir, fill=dir),data=SPX, alpha=0.2) +
 scale_color_manual(values =c("neg"="firebrick","pos"="royalblue")) +
 scale_fill_manual(values =c("neg"="firebrick","pos"="royalblue")) +
 scale_size_area() + theme_void()