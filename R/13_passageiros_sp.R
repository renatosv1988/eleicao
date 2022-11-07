library(readxl)
library(data.table)
library(stringr)
library(ggplot2)
library(scales)

#minhas datas
my_dates <- seq.Date(as.Date("2022-09-04"),as.Date("2022-10-23"),"7 days")

# read data
data <- list()
my_cols <-c("Data","Passageiros Pagantes", "Passageiros Com Gratuidade", "Tot Passageiros Transportados")
for (i in 1 :length(my_dates)){# i<-1
 temp <- read_excel(paste0("./data_raw/sp/", my_dates[i],".xls"), skip = 2)
 temp <- temp[,my_cols]
 colnames(temp) <- c("Data", "pag", "grat", "tot")
 data[[i]] <- temp
}

all_dates <- as.data.table(do.call(rbind, data))

DD <- all_dates[, .(pag=sum(pag)/1000000,
                    grat=sum(grat)/1000000,
                    tot=sum(tot)/1000000), by = Data]
DD$date <- as.Date(DD$Data,format = "%d/%m/%Y")

ggplot() + 
 geom_line(aes(x=date,y=tot), data = DD, color="black", linetype=2) +
 geom_point(aes(x=date,y=tot), data = DD, color="black") +
 geom_line(aes(x=date,y=pag), data = DD, color="forestgreen", linetype=2) +
 geom_point(aes(x=date,y=pag), data = DD, color="forestgreen") +
 geom_line(aes(x=date,y=grat), data = DD, color="firebrick", linetype=2) +
 geom_point(aes(x=date,y=grat), data = DD, color="firebrick") +
 theme_classic() +
 coord_cartesian(ylim=c(0,3)) +
 geom_vline(xintercept = as.Date("2022-10-02"), linetype=3, alpha = 0.5) +
 scale_y_continuous(labels=comma) +
 scale_x_date(breaks = my_dates, date_labels = "%d-%b") +
 labs(x = "Date", y="passengers (million)")
