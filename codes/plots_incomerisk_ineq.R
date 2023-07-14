# ---------------------------------------------------------------------------- #
# -------------------- PLOTS ON INCOME RISK AND INEQUALITY ------------------- #
# ---------------------------------------------------------------------------- #

wd <- "C:/Users/Hriday/Dropbox/Hriday/IRRF/Replication"
setwd(wd)

# Install and load packages
list.of.packages <- c("ggplot2","ggthemes", "ggpubr", "haven","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
rm(list.of.packages,new.packages) # con esto borro los objetos

# load libraries
Packages <- c("ggplot2","ggthemes", "ggpubr", "haven","dplyr")
lapply(Packages, library, character.only = TRUE)


irisk <- readxl::read_xlsx("data/gos-jpe2014-data.xlsx",sheet="Figure 5",skip=4)
ineq_data <- read_dta("data/WIID_31MAY2021.dta")

ineq_data <- ineq_data %>% rename(p20p20 = ratio_top20bottom20)
ineq_data <- ineq_data %>% filter(country=="United States" &source==6 & resource==1 & scale==2)
# source=6
irisk <- irisk %>% filter(year>=1993)
irisk <- irisk[-nrow(irisk),]
ineq_data <- ineq_data %>% filter(year<=2011)
ineq_data$gini <- ineq_data$gini/100

ggplot() + geom_point(aes(x=irisk$std1,y=ineq_data$gini)) + theme_classic() + 
    xlab("Income Risk - Std Dev 1") + ylab("Income Inequality")
ggplot() + geom_point(aes(x=irisk$std5,y=ineq_data$gini)) + theme_classic() + 
    xlab("Income Risk - Std Dev 5") + ylab("Income Inequality")
ggplot() + geom_point(aes(x=irisk$skew1,y=ineq_data$gini)) + theme_classic() + 
    xlab("Income Risk - Skew 1") + ylab("Income Inequality")
ggplot() + geom_point(aes(x=irisk$skew5,y=ineq_data$gini)) + theme_classic() + 
    xlab("Income Risk - Skew 5") + ylab("Income Inequality")

# Correlations
cor.test(irisk$std1,ineq_data$gini)
cor.test(irisk$std5,ineq_data$gini)
cor.test(irisk$skew1,ineq_data$gini)
cor.test(irisk$skew5,ineq_data$gini)

# Skew 1y
cor <- cor.test(irisk$skew1,ineq_data$gini)
text <- paste0("Correlation is ",round(cor$estimate,3)," with a p-value of ",round(cor$p.value,3))
png(file="tablas_figuras/irisk_ineq_skew1.png",width=600, height=350)
plot(irisk$year,irisk$skew1,col="blue",type="b",pch=4,xlab="Year",ylab="Income Risk - Skew 1y")
par(new=TRUE)
plot(irisk$year,ineq_data$gini,type="b",col="red",pch=2,axes=F,ylab="",xlab="",sub=text)
axis(4, at=pretty((ineq_data$gini)), col.ticks="red", col.axis="red")
legend("topleft",legend=c("Income Risk","Income Inequality"),text.col=c("blue","red"),pch=c(4,2))
dev.off()

# Skew 5y
cor <- cor.test(irisk$skew5,ineq_data$gini)
text <- paste0("Correlation is ",round(cor$estimate,3)," with a p-value of ",round(cor$p.value,3))
png(file="tablas_figuras/irisk_ineq_skew5.png",width=600, height=350)
plot(irisk$year,irisk$skew5,col="blue",type="b",pch=4,xlab="Year",ylab="Income Risk - Skew 5y")
par(new=TRUE)
plot(irisk$year,ineq_data$gini,type="b",col="red",pch=2,axes=F,ylab="",xlab="",sub=text)
axis(4, at=pretty((ineq_data$gini)), col.ticks="red", col.axis="red")
legend("topleft",legend=c("Income Risk","Income Inequality"),text.col=c("blue","red"),pch=c(4,2))
dev.off()

# Std Dev. 1y
cor <- cor.test(irisk$std1,ineq_data$gini)
text <- paste0("Correlation is ",round(cor$estimate,3)," with a p-value of ",round(cor$p.value,3))
png(file="tablas_figuras/irisk_ineq_std1.png",width=600, height=350)
plot(irisk$year,irisk$std1,col="blue",type="b",pch=4,xlab="Year",ylab="Income Risk - Std. Dev. 5y")
par(new=TRUE)
plot(irisk$year,ineq_data$gini,type="b",col="red",pch=2,axes=F,ylab="",xlab="",sub=text)
axis(4, at=pretty((ineq_data$gini)), col.ticks="red", col.axis="red")
legend("topleft",legend=c("Income Risk","Income Inequality"),text.col=c("blue","red"),pch=c(4,2))
dev.off()

# Std Dev. 5y
cor <- cor.test(irisk$std5,ineq_data$gini)
text <- paste0("Correlation is ",round(cor$estimate,3)," with a p-value of ",round(cor$p.value,3))
png(file="tablas_figuras/irisk_ineq_std5.png",width=600, height=350)
plot(irisk$year,irisk$std5,col="blue",type="b",pch=4,xlab="Year",ylab="Income Risk - Std. Dev. 5y")
par(new=TRUE)
plot(irisk$year,ineq_data$gini,type="b",col="red",pch=2,axes=F,ylab="",xlab="",sub=text)
axis(4, at=pretty((ineq_data$gini)), col.ticks="red", col.axis="red")
legend("topleft",legend=c("Income Risk","Income Inequality"),text.col=c("blue","red"),pch=c(4,2))
dev.off()

