# ---------------------------------------------------------------------------- #
# ------------------------- PANEL INTERACTIONS ------------------------------- #
# ---------------------------------------------------------------------------- #

options(scipen=999)
rm(list=ls())

# Load libraries
library(readxl);library(data.table);library(dplyr);library(haven);library(mFilter);
library(fixest);library(texreg);library(tidyquant);library(doBy)

# Install and load packages
list.of.packages <- c("readxl","data.table", "dplyr", "haven","mFilter", "fixest","texreg",
                      "tidyquant","doBy")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
rm(list.of.packages,new.packages) # con esto borro los objetos

# load libraries
Packages <- c("readxl","data.table", "dplyr", "haven","mFilter", "fixest","texreg",
                      "tidyquant","doBy")
lapply(Packages, library, character.only = TRUE)


# ------------------- Set working directory ---------------------------------- #
# If you want to work in other directory, just change variable wd
wd <- "C:/Users/Hriday/Dropbox/Hriday/IRRF/Replication"
setwd(wd)
rdata1 <- paste0(wd,"/data/")
results1 <- paste0(wd,"/results/")
figures1 <- paste0(wd,"/tablas_figuras/")
tex1 <- paste0(wd,"/tex/")

# -------------------------- Set locals -------------------------------------- #

# Filter
#  "HP" / "BK"
filter <- "BK"

# Lags for BP method (AG method will use lags_BP/2)
# 1:2 = from 1 to 2 / 1:4 = from 1 to 4 / 1:6 = from 1 to 6
lags <- 1:4

# Definition of IRRF
# 4 = X periods of impact (depends on lags) / 0 = only impact period
# haven't created 4 period impact yet
def_irrf <- 4


# Set respone, shock and ineq measures to use
# respvar <- "dbond_yields"
# shockvar <- "dlng"
# interactionsvar <- c("p90p10","p20p20","gini_wiid")
# Determine if we include policy rate as an indep. variable in regressions
# yes = yes in AG and BP / no = no in AG and BP, / BP = yes in BP no in AG
inc.polrate <- "yes"

# Use normalized or level variables: "normalized" / "level"
normalized <- "normalized"
# Errors clustered or robust
errors <- "cluster"

# Set usage of cumulative responses: type "cum_" if you want to use cumulative response
# if you don't want to use, type "". dep_Var refers to cumulative path of the dependent var.
# var_int refers to a cumulative path over the variable of interest (G shocks)
dep_var <- ""
var_int <- ""
# -------------------- Create directories ----------------------------------- #

rdata <- paste0(rdata1,filter,"_",length(lags),"lags_",def_irrf,"per/")
results <- paste0(results1,filter,"_",length(lags),"lags_",def_irrf,"per/")
figures <- paste0(figures1,filter,"_",length(lags),"lags_",def_irrf,"per/")
tex <- paste0(tex1,filter,"_",length(lags),"lags_",def_irrf,"per/")

dir.create(rdata)
dir.create(results)
dir.create(figures)
dir.create(tex)

rdata <- paste0(rdata1,filter,"_",length(lags),"lags_",def_irrf,"per/",inc.polrate,"polrate/")
results <- paste0(results1,filter,"_",length(lags),"lags_",def_irrf,"per/",inc.polrate,"polrate/")
figures <- paste0(figures1,filter,"_",length(lags),"lags_",def_irrf,"per/",inc.polrate,"polrate/")
tex <- paste0(tex1,filter,"_",length(lags),"lags_",def_irrf,"per/",inc.polrate,"polrate/")
rowname <- paste0(filter,"_",length(lags),"lags_",def_irrf,"per_",inc.polrate,"polrate/")

dir.create(rdata)
dir.create(results)
dir.create(figures)
dir.create(tex)

rdata <- paste0(rdata1,filter,"_",length(lags),"lags_",def_irrf,"per/",inc.polrate,"polrate/",errors)
results <- paste0(results1,filter,"_",length(lags),"lags_",def_irrf,"per/",inc.polrate,"polrate/",errors)
figures <- paste0(figures1,filter,"_",length(lags),"lags_",def_irrf,"per/",inc.polrate,"polrate/",errors)
tex <- paste0(tex1,filter,"_",length(lags),"lags_",def_irrf,"per/",inc.polrate,"polrate/",errors)
rowname <- paste0(filter,"_",length(lags),"lags_",def_irrf,"per_",inc.polrate,"polrate/",errors)

dir.create(rdata)
dir.create(results)
dir.create(figures)
dir.create(tex)

rdata <- paste0(rdata1,filter,"_",length(lags),"lags_",def_irrf,"per/",inc.polrate,"polrate/",errors,"/",normalized)
results <- paste0(results1,filter,"_",length(lags),"lags_",def_irrf,"per/",inc.polrate,"polrate/",errors,"/",normalized)
figures <- paste0(figures1,filter,"_",length(lags),"lags_",def_irrf,"per/",inc.polrate,"polrate/",errors,"/",normalized)
tex <- paste0(tex1,filter,"_",length(lags),"lags_",def_irrf,"per/",inc.polrate,"polrate/",errors,"/",normalized)
rowname <- paste0(filter,"_",length(lags),"lags_",def_irrf,"per_",inc.polrate,"polrate",errors,"/",normalized)

dir.create(rdata)
dir.create(results)
dir.create(figures)
dir.create(tex)

write.table(rdata, file = paste0(rdata1,"/rdata.txt"), sep = "",row.names = F)
write.table(results, file = paste0(results1,"/results.txt"), sep = "",row.names = F)
write.table(tex, file = paste0(tex1,"/tex.txt"), sep = "",row.names = F)
write.table(figures, file = paste0(figures1,"/figures.txt"), sep = "",row.names = F)
write.table(rowname, file = paste0(results1,"/rownames.txt"), sep = "",row.names = F)

lags.1 <- 1/length(lags)
aglags <- 1:(length(lags)/2)
ag.lags1 <- 1/length(aglags)

# --------------------------- Read and clean data ---------------------------- #

paneldata <- read_xlsx("data/panel_data_july21.xlsx")
# change variable name
setnames(paneldata,old="bis_cons_credit_gdp",new="credit_gdp")

# set variables as numeric
columns <- c("quarter","year","real_credit","rgdp_cyc","policy_rate","bond_yields","rgdp","inflation","c_real","real_credit_cyc")
paneldata <- paneldata %>% mutate_at(columns, as.numeric)

# set NA when value is 0
paneldata$policy_rate <- ifelse(paneldata$policy_rate == 0 | is.nan(paneldata$policy_rate) & 
                                    paneldata$year < 2000, NA,paneldata$policy_rate)
paneldata$credit_gdp <- ifelse(paneldata$credit_gdp == 0, NA,paneldata$credit_gdp)
paneldata$real_credit <- ifelse(paneldata$real_credit == 0, NA,paneldata$real_credit)
paneldata$real_credit_cyc <- ifelse(paneldata$real_credit_cyc == 0, NA,paneldata$real_credit_cyc)
# AGREGUE ESTO: 
paneldata$rgdp[is.nan(paneldata$rgdp)] <- NA
# generate g spread
paneldata$gspread <- paneldata$bond_yields - paneldata$policy_rate

# order by country
paneldata <- paneldata[order(paneldata$country),]
# keep data until 2007
# paneldata <- paneldata %>% filter(year<= 2007)
# generate copy (it will be used for AG later)
AGpaneldata <- paneldata
# read some country variables
topanel <- read_dta("data/topanel.dta")

# merge df
mpanel <- merge(paneldata,topanel,by = "country")

# drop estonia
mpanel <- mpanel %>% filter(country != "Estonia")

# generate new variables
mpanel <- mpanel %>% mutate(c_real=c_real/1000,real_credit=real_credit/1000,rgdp_cyc=rgdp_cyc/1000,
                            real_credit_cyc=real_credit_cyc/1000,
                            lng = log(G),lngdp=log(rgdp),lnc=log(c_real),lreal_credit=log(real_credit),
                            p90p10=linequality) 

countries <- unique(mpanel$country)
mpanelsub3 <- list();mpanelsub4<-NA
for (i in 1:28) {
    # test; i=1
    mpanelsub <- mpanel %>% filter(country == countries[i])
    
    # Fix NA problem
    df1 <- mpanelsub %>% filter(!is.na(mpanelsub$rgdp)) %>% 
        select(country,year,quarter,rgdp)
    df2 <-  hpfilter(df1$rgdp,freq=1600,drift=TRUE)
    df3 <- data.frame(df1, cycle_hp = df2$cycle)
    df3$trend_rgdp <- df3$rgdp- df3$cycle_hp
    
    mpanelsub2 <- merge(mpanelsub,df3,by = c("country","year","quarter","rgdp"),all = T,)
    
    df2 <- bkfilter(df1$rgdp,pl = 6,pu=32,nfix = 12,drift = T)
    df3 <- data.frame(df1, cycle_bk = df2$cycle)
    df3$trend_rgdpBK <- df3$rgdp - df3$cycle_bk
    mpanelsub3[[i]] <- merge(mpanelsub2,df3,by = c("country","year","quarter","rgdp"),all = T)
    
    mpanelsub4 <- rbind(mpanelsub4,mpanelsub3[[i]])
}
mpanelsub4 <- mpanelsub4[-1,]
# mpanelsub4$trend_rgdpBK <- ifelse(is.na(mpanelsub4$trend_rgdpBK), mpanelsub4$trend_rgdp, mpanelsub4$trend_rgdpBK)

if (filter == "HP") {mpanelsub4 <- mpanelsub4 %>% mutate(dlng = log((G)/trend_rgdp), dlnc = log((c_real)/trend_rgdp),
                                                         dlngdp = log((rgdp)/trend_rgdp))
} else {mpanelsub4 <- mpanelsub4 %>% mutate(dlng = log((G)/trend_rgdpBK), dlnc = log((c_real)/trend_rgdpBK),
                                            dlngdp = log((rgdp)/trend_rgdpBK))}


# Generate cumulative responses of G shock and bond yields
mpanelsub4 <- mpanelsub4 %>% mutate(date = paste0(year,"-",quarter))
mpanelsub4$date <- as.Date(as.yearqtr(mpanelsub4$date))
window <- 4
mpanelsub4 <- mpanelsub4 %>% group_by(country) %>% tq_mutate(select = dlng, mutate_fun = rollapply,
                                                            width = window, FUN = sum, col_rename = "cum_dlng")
mpanelsub4 <- mpanelsub4 %>% group_by(country) %>% tq_mutate(select = bond_yields, mutate_fun = rollapply,
                                                            width = window, FUN = sum, col_rename = "cum_bond_yields")

# Create function to compute lags
L. <- function(x, k) {
    res <- as.matrix(dplyr::bind_cols(lapply(k, function(k) dplyr::lag(x, k))))
    colnames(res) <- paste0("_lag", seq_along(1:ncol(res)))
    res
}

# Load inequality data
new_ineq <- read_dta(paste0(rdata1,"new_ineq.dta"))
mpanelsub4 <- merge(mpanelsub4,new_ineq,by=c("country"))

# Include Nichols and Rehm (2014) income risk data
dfirisk <- read_dta(paste0(rdata1,"irisk_NR.dta"))
dfirisk <- dfirisk %>% mutate(country = recode(country, US= "United States", UK = "United Kingdom",
                                               "Czech Rep."= "Czech Republic","Slovakia"="Slovak Republic"))

irisk <- dfirisk %>% select(country,iso3c,year,R3n,I3n,V3n,M3n) %>% 
    filter(year>=2001 & year<=2006) %>%group_by(country) %>% 
    summarise(iriskRC = mean(R3n,na.rm=T),iriskIC = mean(I3n,na.rm=T),iriskVC = mean(V3n,na.rm=T),iriskMC = mean(M3n,na.rm=T))
irisk <- irisk %>% mutate(iriskWC = (iriskVC+iriskMC))

mpanelsub4 <- merge(mpanelsub4,irisk,by="country",all.x=T)
mpanelsub4 <- mpanelsub4[order(mpanelsub4$country,mpanelsub4$year,mpanelsub4$quarter),]

# save data frame for AG panel regression
AGpaneldata <- mpanelsub4

# take slovenia out of the sample (it is already out of AG shocks)
if (slov=="no") {
    mpanelsub4 <- mpanelsub4 %>% filter(country!="Slovenia")
}

# -------------------- GENERATE INTERACTION DUMMIES ------------------------- #

# Dummy=1 if inequality measure of country $i$ is above or below median (for split regression)
mpanelsub4 <- mpanelsub4 %>% mutate(dummy_p20p20 = ifelse(p20p20>median(p20p20),1,0),
                                    dummy_p90p10 = ifelse(p90p10>median(p90p10),1,0),
                                    dummy_gini_wiid = ifelse(gini_wiid>median(gini_wiid),1,0))
# Interact the dummy from above with shock variable
mpanelsub4 <- mpanelsub4 %>% mutate(int_p20p20_high = dummy_p20p20*dlng,
                                    int_p90p10_high = dummy_p90p10*dlng,
                                    int_gini_wiid_high = dummy_gini_wiid*dlng,
                                    cum_int_p20p20_high = dummy_p20p20*cum_dlng,
                                    cum_int_p90p10_high = dummy_p90p10*cum_dlng,
                                    cum_int_gini_wiid_high = dummy_gini_wiid*cum_dlng,
                                    cum_int_p20p20_high_bond_yields = dummy_p20p20*cum_bond_yields,
                                    cum_int_p90p10_high_bond_yields = dummy_p90p10*cum_bond_yields,
                                    cum_int_gini_wiid_high_bond_yields = dummy_gini_wiid*cum_bond_yields)

if (normalized=="normalized") {
mpanelsub4 <- mpanelsub4 %>% mutate(p20p20 = (p20p20-mean(p20p20,na.rm=T))/sd(p20p20,na.rm=T),
                                    p90p10 = (p90p10-mean(p90p10,na.rm=T))/sd(p90p10,na.rm=T),
                                    gini_wiid = (gini_wiid-mean(gini_wiid,na.rm=T))/sd(gini_wiid,na.rm=T))
} else {mpanelsub4 <- mpanelsub4 %>% mutate(p20p20 = p20p20,p90p10 = p90p10,gini_wiid = gini_wiid)}    

mpanelsub4 <- mpanelsub4 %>% mutate(int_p20p20 = dlng*p20p20,int_p90p10 = dlng*p90p10,int_gini_wiid=dlng*gini_wiid,
                                    cum_int_p20p20 = cum_dlng*p20p20,cum_int_p90p10 = cum_dlng*p90p10,
                                    cum_int_gini_wiid=cum_dlng*gini_wiid,
                                    cum_int_p20p20_bond_yields = cum_bond_yields*p20p20,cum_int_p90p10_bond_yields = cum_bond_yields*p90p10,
                                    cum_int_gini_wiid_bond_yields=cum_bond_yields*gini_wiid)    

if (inc.polrate == "yes") {controls <- c("bond_yields","dlng","policy_rate","dlngdp")
} else{controls <- c("bond_yields","dlng","dlngdp")}

for (ineq in c("p20p20","p90p10","gini_wiid")) {
    for (con in controls) {
        dummy <- paste0("dummy_",ineq)
        interaction <- data.frame(mpanelsub4[,con]*mpanelsub4[,ineq])
        interaction2 <- data.frame(mpanelsub4[,con]*mpanelsub4[,dummy])
        colnames(interaction) <- paste0("int_",ineq,"_",con)
        colnames(interaction2) <- paste0("int_",ineq,"_high","_",con)
        mpanelsub4 <- cbind(mpanelsub4,interaction)
        mpanelsub4 <- cbind(mpanelsub4,interaction2)
    }  
}

# ---------------------------------------------------------------------------- #
# --------------------------- REGRESSIONS ------------------------------------ #
# ---------------------------------------------------------------------------- #

# Regression with interactions

# Specification 1: Interaction as dummy above or below median ineq * variable
model1 <- function(ineq_measure,cum_depvar=dep_var,cum_varint=var_int){ if (inc.polrate=="yes") {
    ineq_model <- paste0(cum_depvar,"bond_yields ~ ",cum_varint,"dlng + L.(",cum_depvar,"bond_yields,lags) + L.(dlngdp,lags) + 
    L.(",cum_varint,"dlng,lags) + L.(policy_rate,lags) + ",cum_varint,"int_",ineq_measure,
    "_high +L.(",cum_depvar,"int_",ineq_measure,"_high_bond_yields,lags) + L.(",cum_varint,"int_",ineq_measure,"_high,lags)+ 
                    L.(int_",ineq_measure,"_high_policy_rate,lags) +L.(int_",ineq_measure,"_high_dlngdp,lags)|country+Period")
} else{
    ineq_model <- paste0("cum_bond_yields ~ dlng + L.(cum_bond_yields,lags) + L.(dlngdp,lags) + L.(dlng,lags) +  
        int_",ineq_measure,"_high +L.(int_",ineq_measure,"_high_bond_yields,lags) + L.(int_",ineq_measure,"_high_dlng,lags)+ 
                    L.(int_",ineq_measure,"_high_dlngdp,lags)|country+Period")
}}

error1 <- ifelse(errors=="cluster","cluster","HC1")

reg9010s1 <- feols(formula(model1("p90p10")),vcov=error1,mpanelsub4)
reg2020s1 <- feols(formula(model1("p20p20")),vcov = error1,mpanelsub4)
regginis1 <- feols(formula(model1("gini_wiid")),vcov = error1,mpanelsub4)

summary(reg9010s1)
summary(reg2020s1)
summary(regginis1)

if (dep_var=="") {
    texreg(list(reg9010s1,reg2020s1,regginis1),stars = c(0.01,0.05,0.1),file = paste0(tex,"/interactions_dummy.tex"), caption.above = T,
           custom.coef.map = list("dlng"="$g$","int_p90p10_high" = "$g$* D 90th/10th",
                                  "int_p20p20_high" = "$g$*D 80th/20th","int_gini_wiid_high"="$g$ * D Gini"),
           include.ci=FALSE,digits=3,caption="IRRF and inequality: panel evidence",custom.model.names = c("(1) IRRF","(2) IRRF","(3) IRRF"),
           custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes"),
            "Year FE" = c("Yes","Yes","Yes")),reorder.gof = c(3,7,1,2,4,5,6,8,9,10),float.pos = "H")
} else{
if (var_int=="") {
    texreg(list(reg9010s1,reg2020s1,regginis1),stars = c(0.01,0.05,0.1),file = paste0(tex,"/interactions_dummy_nocumvarint.tex"), caption.above = T,
           custom.coef.map = list("dlng"="$g$","int_p90p10_high" = "$g$* D 90th/10th",
                                  "int_p20p20_high" = "$g$*D 80th/20th","int_gini_wiid_high"="$g$ * D Gini"),
           include.ci=FALSE,digits=3,caption="IRRF and inequality: panel evidence",custom.model.names = c("(1) IRRF","(2) IRRF","(3) IRRF"),
           custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes"),
            "Year FE" = c("Yes","Yes","Yes")),reorder.gof = c(3,7,1,2,4,5,6,8,9,10),float.pos = "H")
} else{
    texreg(list(reg9010s1,reg2020s1,regginis1),stars = c(0.01,0.05,0.1),file = paste0(tex,"/interactions_dummy_cumvarint.tex"), caption.above = T,
           custom.coef.map = list("cum_dlng"="$g$","cum_int_p90p10_high" = "$g$* D 90th/10th",
                                  "cum_int_p20p20_high" = "$g$*D 80th/20th","cum_int_gini_wiid_high"="$g$ * D Gini"),
           include.ci=FALSE,digits=3,caption="IRRF and inequality: panel evidence",custom.model.names = c("(1) IRRF","(2) IRRF","(3) IRRF"),
           custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes"),
            "Year FE" = c("Yes","Yes","Yes")),reorder.gof = c(3,7,1,2,4,5,6,8,9,10),float.pos = "H")
}}


# -----------------------------------------------------------------------------
# Table 3 and 4 of final paper. 
# It creates table 3 if depvar="", it creates table 4 if depvar="cum_".
# You have to change the local variable defined at the beginning of this code.
# -----------------------------------------------------------------------------
# Specification 2: Interaction as inequality * variable
model2 <- function(ineq_measure,cum_depvar=dep_var,cum_varint=var_int){ if (inc.polrate=="yes") {
    # ineq_measure="p90p10";cum_depvar=dep_var;cum_varint=var_int
    ineq_model <- paste0(cum_depvar,"bond_yields ~ ",cum_varint,"dlng + L.(",cum_depvar,"bond_yields,lags) + L.(dlngdp,lags) + 
    L.(",cum_varint,"dlng,lags) + L.(policy_rate,lags) +",cum_varint,"int_",ineq_measure,
    " +L.(",cum_depvar,"int_",ineq_measure,"_bond_yields,lags) + L.(",cum_varint,"int_",ineq_measure,",lags)+ 
                    L.(int_",ineq_measure,"_policy_rate,lags) +L.(int_",ineq_measure,"_dlngdp,lags)|country+Period")
} else{
    ineq_model <- paste0(cum_depvar,"bond_yields ~ ",cum_varint,"dlng + L.(",cum_depvar,"bond_yields,lags) + L.(dlngdp,lags) + 
    L.(",cum_varint,"dlng,lags) +",cum_varint," 
        int_",ineq_measure," +L.(",cum_depvar,"int_",ineq_measure,"_bond_yields,lags) + L.(",cum_varint,"int_",ineq_measure,",lags)+ 
                    L.(int_",ineq_measure,"_dlngdp,lags)|country+Period")
}}

regrows <- c("$g$","$g$ p9010","$g$ p2020","$g$ Gini")

reg9010s2 <- feols(formula(model2("p90p10")),vcov=error1,mpanelsub4)
reg2020s2 <- feols(formula(model2("p20p20")),vcov=error1,mpanelsub4)
regginis2 <- feols(formula(model2("gini_wiid")),vcov=error1,mpanelsub4)

summary(reg9010s2)
summary(reg2020s2)
summary(regginis2)

if (dep_var=="") {
    texreg(list(reg9010s2,reg2020s2,regginis2),stars = c(0.01,0.05,0.1),file = paste0(tex,"/TAB_3_interactions_ineq.tex"), caption.above = T,
           custom.coef.map = list("dlng"="$g$","int_p90p10" = "$g$ * 90th/10th","int_p20p20" = "$g$ * 80th/20th","int_gini_wiid"="$g$ * Gini"),
           include.ci=FALSE,digits=3,caption="IRRF and inequality: panel evidence",custom.model.names = c("(1) IRRF","(2) IRRF","(3) IRRF"),
           float.pos = "H",custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),
           custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes"),"Year FE" = c("Yes","Yes","Yes")),
           reorder.gof = c(3,7,1,2,4,5,6,8,9,10))
} else{
if (var_int=="") {
    texreg(list(reg9010s2,reg2020s2,regginis2),stars = c(0.01,0.05,0.1),file = paste0(tex,"/_interactions_ineq_nocumvarint.tex"), caption.above = T,
           custom.coef.map = list("dlng"="$g$","int_p90p10" = "$g$ * 90th/10th","int_p20p20" = "$g$ * 80th/20th","int_gini_wiid"="$g$ * Gini"),
           include.ci=FALSE,digits=3,caption="IRRF and inequality: panel evidence",custom.model.names = c("(1) IRRF","(2) IRRF","(3) IRRF"),
           float.pos = "H",custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),
           custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes"),"Year FE" = c("Yes","Yes","Yes")),
           reorder.gof = c(3,7,1,2,4,5,6,8,9,10))
} else {
    texreg(list(reg9010s2,reg2020s2,regginis2),stars = c(0.01,0.05,0.1),file = paste0(tex,"/TAB4_interactions_ineq_cumvarint.tex"), caption.above = T,
           custom.coef.map = list("cum_dlng"="$g$","cum_int_p90p10" = "$g$ * 90th/10th","cum_int_p20p20" = "$g$ * 80th/20th",
                                  "cum_int_gini_wiid"="$g$ * Gini"),
           include.ci=FALSE,digits=3,caption="IRRF and inequality: panel evidence",custom.model.names = c("(1) IRRF","(2) IRRF","(3) IRRF"),
           float.pos = "H",custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),
           custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes"),"Year FE" = c("Yes","Yes","Yes")),
           reorder.gof = c(3,7,1,2,4,5,6,8,9,10))
    
}}


# Specification 3: Sample split - Regress for sample above ineq and below ineq
if (inc.polrate=="yes" & dep_var=="" & var_int=="") {
        models3 <- formula("bond_yields ~ dlng + L.(bond_yields,lags) + L.(dlngdp,lags) + 
                   L.(dlng,lags) + L.(policy_rate,lags)|country+Period")
    } else if (inc.polrate=="yes" & var_int=="" & dep_var=="cum_") {
    models3 <- formula("cum_bond_yields ~ dlng + L.(cum_bond_yields,lags) + L.(dlngdp,lags) + 
                   L.(dlng,lags) + L.(policy_rate,lags)|country+Period")
    } else if (inc.polrate=="yes" & var_int=="cum_" & dep_var=="cum_") {
        models3 <- formula("cum_bond_yields ~ cum_dlng + L.(cum_bond_yields,lags) + L.(dlngdp,lags) + 
                   L.(cum_dlng,lags) + L.(policy_rate,lags)|country+Period")
    } else if (inc.polrate=="no" & var_int=="cum_" & dep_var=="cum_") {
    models3 <- formula("cum_bond_yields ~ cum_dlng + L.(cum_bond_yields,lags) + L.(dlngdp,lags) + 
                   L.(cum_dlng,lags) + L.(policy_rate,lags)|country+Period")
    } else if (inc.polrate=="no" & var_int=="" & dep_var=="cum_"){
        models3 <- formula("cum_bond_yields ~ dlng + L.(cum_bond_yields,lags) + L.(dlngdp,lags) + 
                   L.(dlng,lags)|country+Period")
    } else if (inc.polrate=="no" & var_int=="" & dep_var==""){
    models3 <- formula("bond_yields ~ dlng + L.(bond_yields,lags) + L.(dlngdp,lags) + 
                   L.(dlng,lags)|country+Period")
}

above_ineq_p20p20 <- mpanelsub4 %>% filter(dummy_p20p20==1) %>% feols(models3)
below_ineq_p20p20 <- mpanelsub4 %>% filter(dummy_p20p20==0) %>% feols(models3) 
summary(above_ineq_p20p20)
summary(below_ineq_p20p20)

above_ineq_p90p10 <- mpanelsub4 %>% filter(dummy_p90p10==1) %>% feols(models3)
below_ineq_p90p10 <- mpanelsub4 %>% filter(dummy_p90p10==0) %>% feols(models3)
summary(above_ineq_p90p10)
summary(below_ineq_p90p10)

above_ineq_gini_wiid <- mpanelsub4 %>% filter(dummy_gini_wiid==1) %>% feols(models3)
below_ineq_gini_wiid <- mpanelsub4 %>% filter(dummy_gini_wiid==0) %>% feols(models3)
summary(above_ineq_gini_wiid)
summary(below_ineq_gini_wiid)

if (dep_var=="") {
    texreg(list(above_ineq_p20p20,below_ineq_p20p20,above_ineq_p90p10,below_ineq_p90p10,above_ineq_gini_wiid,below_ineq_gini_wiid),
           stars = c(0.01,0.05,0.1),file = paste0(tex,"/interactions_split.tex"), caption.above = T,
           custom.coef.map = list("dlng"="$g$"),include.ci=FALSE,digits=3,caption="IRRF and inequality: panel evidence",
           custom.model.names = c("High 80th/20th","Low 80th/20th","High 90th/10th",
                                  "Low 90th/10th","High Gini","Low Gini"),
           float.pos = "H",custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes","Yes","Yes","Yes"),
                                                  "Year FE" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
           custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),reorder.gof = c(3,7,1,2,4,5,6,8,9,10))
} else{
    if (var_int=="") {
        texreg(list(above_ineq_p20p20,below_ineq_p20p20,above_ineq_p90p10,below_ineq_p90p10,above_ineq_gini_wiid,below_ineq_gini_wiid),
               stars = c(0.01,0.05,0.1),file = paste0(tex,"/interactions_split_nocumvarint.tex"), caption.above = T,
               custom.coef.map = list("dlng"="$g$"),include.ci=FALSE,digits=3,caption="IRRF and inequality: panel evidence",
               custom.model.names = c("High 80th/20th","Low 80th/20th","High 90th/10th",
                                      "Low 90th/10th","High Gini","Low Gini"),
               float.pos = "H",custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes","Yes","Yes","Yes"),
                                                      "Year FE" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
               custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),reorder.gof = c(3,7,1,2,4,5,6,8,9,10))
    } else {
        texreg(list(above_ineq_p20p20,below_ineq_p20p20,above_ineq_p90p10,below_ineq_p90p10,above_ineq_gini_wiid,below_ineq_gini_wiid),
               stars = c(0.01,0.05,0.1),file = paste0(tex,"/interactions_split_cumvarint.tex"), caption.above = T,
               custom.coef.map = list("cum_dlng"="$g$"),include.ci=FALSE,digits=3,caption="IRRF and inequality: panel evidence",
               custom.model.names = c("High 80th/20th","Low 80th/20th","High 90th/10th",
                                      "Low 90th/10th","High Gini","Low Gini"),
               float.pos = "H",custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes","Yes","Yes","Yes"),
                                                      "Year FE" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
               custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),reorder.gof = c(3,7,1,2,4,5,6,8,9,10))
    }
}

# ---------------------------------------------------------------------------- #
# ----------- Test regression with income risk interaction ------------------- #
# ---------------------------------------------------------------------------- #

# Regression with interactions

error1 <- ifelse(errors=="cluster","cluster","HC1")

# Specification 2: Interaction as inequality * variable
model2 <- function(ineq_measure,cum_depvar=dep_var,cum_varint=var_int){ if (inc.polrate=="yes") {
    # ineq_measure="p90p10";cum_depvar=dep_var;cum_varint=var_int
    ineq_model <- paste0(cum_depvar,"bond_yields ~ ",cum_varint,"dlng + L.(",cum_depvar,"bond_yields,lags) + L.(dlngdp,lags) + 
    L.(",cum_varint,"dlng,lags) + L.(policy_rate,lags) +",cum_varint,"int_",ineq_measure,
                         " +L.(",cum_depvar,"int_",ineq_measure,"_bond_yields,lags) + L.(",cum_varint,"int_",ineq_measure,",lags)+ 
                    L.(int_",ineq_measure,"_policy_rate,lags) +L.(int_",ineq_measure,"_dlngdp,lags)|country+Period")
} else{
    ineq_model <- paste0(cum_depvar,"bond_yields ~ ",cum_varint,"dlng + L.(",cum_depvar,"bond_yields,lags) + L.(dlngdp,lags) + 
    L.(",cum_varint,"dlng,lags) +",cum_varint," 
        int_",ineq_measure," +L.(",cum_depvar,"int_",ineq_measure,"_bond_yields,lags) + L.(",cum_varint,"int_",ineq_measure,",lags)+ 
                    L.(int_",ineq_measure,"_dlngdp,lags)|country+Period")
}}

regrows <- c("$g$","$g$ p9010","$g$ p2020","$g$ Gini")

reg9010s2 <- feols(formula(model2("iriskRC")),vcov=error1,mpanelsub4)
reg2020s2 <- feols(formula(model2("iriskWC")),vcov=error1,mpanelsub4)
# regginis2 <- feols(formula(model2("gini_wiid")),vcov=error1,mpanelsub4)

summary(reg9010s2)
summary(reg2020s2)
summary(regginis2)


# Specification 2: Interaction as inequality * variable
model2 <- function(ineq_measure,irisk_meas,cum_depvar=dep_var,cum_varint=var_int){ if (inc.polrate=="yes") {
    # ineq_measure="p90p10";irisk_meas="iriskRC";cum_depvar=dep_var;cum_varint=var_int
    ineq_model <- paste0(cum_depvar,"bond_yields ~ ",cum_varint,"dlng + L.(",cum_depvar,"bond_yields,lags) + L.(dlngdp,lags) + 
    L.(",cum_varint,"dlng,lags) + L.(policy_rate,lags) + ",cum_varint,"int_",ineq_measure," + ",cum_varint,"int_",irisk_meas,
                         " + L.(",cum_depvar,"int_",ineq_measure,"_bond_yields,lags) + L.(",cum_varint,"int_",ineq_measure,",lags) + 
                    L.(int_",ineq_measure,"_policy_rate,lags) + L.(int_",ineq_measure,"_dlngdp,lags) + 
       L.(",cum_depvar,"int_",irisk_meas,"_bond_yields,lags) + L.(",cum_varint,"int_",irisk_meas,",lags) + 
                              L.(int_",irisk_meas,"_policy_rate,lags) + L.(int_",irisk_meas,"_dlngdp,lags)|country+Period")
} else{
    ineq_model <- paste0(cum_depvar,"bond_yields ~ ",cum_varint,"dlng + L.(",cum_depvar,"bond_yields,lags) + L.(dlngdp,lags) + 
    L.(",cum_varint,"dlng,lags) +",cum_varint," 
        int_",ineq_measure," +L.(",cum_depvar,"int_",ineq_measure,"_bond_yields,lags) + L.(",cum_varint,"int_",ineq_measure,",lags)+ 
                    L.(int_",ineq_measure,"_dlngdp,lags)|country+Period")
}}

reg9010s2 <- feols(formula(model2("p20p20","iriskRC")),vcov=error1,mpanelsub4)
reg2020s2 <- feols(formula(model2("p20p20","iriskWC")),vcov=error1,mpanelsub4)
regginis2 <- feols(formula(model2("gini_wiid","iriskRC")),vcov=error1,mpanelsub4)

summary(reg9010s2)
summary(reg2020s2)
summary(regginis2)


if (dep_var=="") {
    texreg(list(reg9010s2,reg2020s2),stars = c(0.01,0.05,0.1),file = paste0(tex,"/interactions_irisk_ineq.tex"), caption.above = T,
           custom.coef.map = list("dlng"="$g$","int_p20p20" = "$g$ * 80th/20th","int_iriskRC" = "$g$ * Aggreg. Income Risk",
                                  "int_iriskWC" = "$g$ * Within Income Risk"),
           include.ci=FALSE,digits=3,caption="IRRF and Income Risk: panel evidence",custom.model.names = c("(1) IRRF","(2) IRRF"),
           float.pos = "H",custom.gof.names = c(NA,NA,"Within $R^2$",NA,"N. Obs",NA,NA,NA),
           custom.gof.rows = list("Country FE" = c("Yes","Yes"),"Year FE" = c("Yes","Yes")),
           reorder.gof = c(3,7,1,2,4,5,6,8,9,10))
} else{
    if (var_int=="") {
        texreg(list(reg9010s2,reg2020s2,regginis2),stars = c(0.01,0.05,0.1),file = paste0(tex,"/interactions_ineq_nocumvarint.tex"), caption.above = T,
               custom.coef.map = list("dlng"="$g$","int_p90p10" = "$g$ * 90th/10th","int_p20p20" = "$g$ * 80th/20th","int_gini_wiid"="$g$ * Gini"),
               include.ci=FALSE,digits=3,caption="IRRF and inequality: panel evidence",custom.model.names = c("(1) IRRF","(2) IRRF","(3) IRRF"),
               float.pos = "H",custom.gof.names = c(NA,NA,"Within $R^2$",NA,"N. Obs",NA,NA,NA),
               custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes"),"Year FE" = c("Yes","Yes","Yes")),
               reorder.gof = c(3,7,1,2,4,5,6,8,9,10))
    } else {
        texreg(list(reg9010s2,reg2020s2,regginis2),stars = c(0.01,0.05,0.1),file = paste0(tex,"/interactions_ineq_cumvarint.tex"), caption.above = T,
               custom.coef.map = list("cum_dlng"="$g$","cum_int_p90p10" = "$g$ * 90th/10th","cum_int_p20p20" = "$g$ * 80th/20th",
                                      "cum_int_gini_wiid"="$g$ * Gini"),
               include.ci=FALSE,digits=3,caption="IRRF and inequality: panel evidence",custom.model.names = c("(1) IRRF","(2) IRRF","(3) IRRF"),
               float.pos = "H",custom.gof.names = c(NA,NA,"Within $R^2$",NA,"N. Obs",NA,NA,NA),
               custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes"),"Year FE" = c("Yes","Yes","Yes")),
               reorder.gof = c(3,7,1,2,4,5,6,8,9,10))
        
    }}


# ---------------------------------------------------------------------------- #
# ------------------------- AG PANEL DATA ------------------------------------ #
# ---------------------------------------------------------------------------- #

AGpaneldata$year_half <- ifelse(AGpaneldata$quarter==1|AGpaneldata$quarter==2,1,2)
countries <- unique(AGpaneldata$country)
# Create contry code
cty_code <- c("AUS","AUT","BEL","CAN","CZE","DNK","FIN","FRA","DEU","GRC","HUN",
              "ISL","IRL","ITA","JPN","KOR","NLD","NZL","NOR","POL","PRT","SVK",
              "SVN","ESP","SWE","CHE","GBR","USA")
AGpaneldata$location <- 0
# AGpaneldata$location_id <- 0
for (i in 1:28) {
    AGpaneldata$location <- ifelse(AGpaneldata$country==countries[i],cty_code[i],AGpaneldata$location)
}

mean_na <- function(x)base::mean(x,na.rm=T)
gshocks <- read_dta(paste0(rdata1,"g_shocks_oecd_forecast_AGAERPP13.dta"))
AGpanel <- summaryBy(bond_yields + policy_rate + gspread + rgdp ~ location + year + year_half,
                     keep.names = T,data = AGpaneldata,FUN = mean_na)

AGpanel <- merge(x= AGpanel,y = gshocks, by=c("location","year","year_half"))
AGpanel <- AGpanel[order(AGpanel$location,AGpanel$year),]

AGpanel <- AGpanel %>% group_by(location) %>% mutate(rgdp_g0 = 100*(rgdp-shift(rgdp))/shift(rgdp))

AGpanel <- AGpanel %>% group_by(location) %>% mutate(dropshock = mean(pure2007_FE_0,na.rm=T))
AGpanel <- AGpanel[!(is.nan(AGpanel$dropshock)),]
AGpanel <- AGpanel[!(AGpanel$location == "POL"),]
AGpanel <- AGpanel[order(AGpanel$location,AGpanel$year,AGpanel$year_half),]

ineq_AG <- readxl::read_excel(paste0(rdata1,"/new_ineq_toAG.xlsx"))
ineq_AG <- ineq_AG %>% rename(inequality = p90_p10)

AGpanel <- merge(AGpanel, ineq_AG,by = "location")

AGpanel <- rename(AGpanel,"purefe"="pure2007_FE_1")

AGpanel <- AGpanel %>% mutate(date = paste0(year,"-",year_half))
AGpanel$date <- as.Date(as.yearqtr(AGpanel$date))
window <- 4
AGpanel <- AGpanel %>% group_by(country) %>% tq_mutate(select = purefe, mutate_fun = rollapply,
                                                             width = window, FUN = sum, col_rename = "cum_purefe")
AGpanel <- AGpanel %>% group_by(country) %>% tq_mutate(select = bond_yields, mutate_fun = rollapply,
                                                             width = window, FUN = sum, col_rename = "cum_bond_yields")

AGpanel$p90p10 <- AGpanel$inequality
# ---------------------------------------------------------------------------- #
# -------------------------- INTERACTIONS AG --------------------------------- #
# ---------------------------------------------------------------------------- #
# Dummy=1 if inequality measure of country $i$ is above or below median (for split regression)
AGpanel$dummy_p20p20  <- ifelse(AGpanel$p20p20>median(AGpanel$p20p20),1,0)
AGpanel$dummy_p90p10  <- ifelse(AGpanel$p90p10>median(AGpanel$p90p10),1,0)
AGpanel$dummy_gini_wiid  <- ifelse(AGpanel$gini_wiid>median(AGpanel$gini_wiid),1,0)

# Interact the dummy from above with shock variable
AGpanel <- AGpanel %>% mutate(int_p20p20_high = dummy_p20p20*purefe,
                                    int_p90p10_high = dummy_p90p10*purefe,
                                    int_gini_wiid_high = dummy_gini_wiid*purefe,
                                    cum_int_p20p20_high = dummy_p20p20*cum_purefe,
                                    cum_int_p90p10_high = dummy_p90p10*cum_purefe,
                                    cum_int_gini_wiid_high = dummy_gini_wiid*cum_purefe,
                                    cum_int_p20p20_high_bond_yields = dummy_p20p20*cum_bond_yields,
                                    cum_int_p90p10_high_bond_yields = dummy_p90p10*cum_bond_yields,
                                    cum_int_gini_wiid_high_bond_yields = dummy_gini_wiid*cum_bond_yields)

if (normalized=="normalized") {
    AGpanel$p20p20 <- (AGpanel$p20p20-mean(AGpanel$p20p20,na.rm=T))/sd(AGpanel$p20p20,na.rm=T)
    AGpanel$p90p10 <- (AGpanel$p90p10-mean(AGpanel$p90p10,na.rm=T))/sd(AGpanel$p90p10,na.rm=T)
    AGpanel$gini_wiid <- (AGpanel$gini_wiid-mean(AGpanel$gini_wiid,na.rm=T))/sd(AGpanel$gini_wiid,na.rm=T)
} else {AGpanel <- AGpanel}    

AGpanel <- AGpanel %>% mutate(int_p20p20 = purefe*p20p20,int_p90p10 = purefe*p90p10,int_gini_wiid=purefe*gini_wiid,
                                    cum_int_p20p20 = cum_purefe*p20p20,cum_int_p90p10 = cum_purefe*p90p10,
                                    cum_int_gini_wiid=cum_purefe*gini_wiid,
                                    cum_int_p20p20_bond_yields = cum_bond_yields*p20p20,cum_int_p90p10_bond_yields = cum_bond_yields*p90p10,
                                    cum_int_gini_wiid_bond_yields=cum_bond_yields*gini_wiid)    

if (inc.polrate == "yes") {controls <- c("bond_yields","policy_rate")
} else{controls <- c("bond_yields")}

for (ineq in c("p20p20","p90p10","gini_wiid")) {
    for (con in controls) {
        dummy <- paste0("dummy_",ineq)
        interaction <- data.frame(AGpanel[,con]*AGpanel[,ineq])
        interaction2 <- data.frame(AGpanel[,con]*AGpanel[,dummy])
        colnames(interaction) <- paste0("int_",ineq,"_",con)
        colnames(interaction2) <- paste0("int_",ineq,"_high","_",con)
        AGpanel <- cbind(AGpanel,interaction)
        AGpanel <- cbind(AGpanel,interaction2)
    }  
}

# ---------------------------------------------------------------------------- #
# --------------------------- REGRESSIONS AG --------------------------------- #
# ---------------------------------------------------------------------------- #

# Regression with interactions

# inc.polrate="yes"

# Specification 1: Interaction as dummy above or below median ineq * variable
model1 <- function(ineq_measure, cum_depvar = dep_var,cum_varint = var_int){ if (inc.polrate=="yes") {
    ineq_model <- paste0(cum_depvar,"bond_yields ~",cum_varint,"purefe + L.(policy_rate,aglags) + ",cum_varint,"int_",ineq_measure,
                         "_high + L.(int_",ineq_measure,"_high_policy_rate,aglags)|country+date")
} else{
    ineq_model <- paste0(cum_depvar,"bond_yields ~",cum_varint,"purefe +",cum_varint,"int_",ineq_measure,"_high|country+date")
}}

error1 <- ifelse(errors=="cluster","cluster","HC1")

reg9010s1 <- feols(formula(model1("p90p10")),vcov=error1,AGpanel)
reg2020s1 <- feols(formula(model1("p20p20")),vcov = error1,AGpanel)
regginis1 <- feols(formula(model1("gini_wiid")),vcov = error1,AGpanel)

summary(reg9010s1)
summary(reg2020s1)
summary(regginis1)

if (dep_var=="") {
    texreg(list(reg9010s1,reg2020s1,regginis1),stars=c(0.01,0.05,0.1),file=paste0(tex,"/interactions_dummy_AG.tex"), caption.above = T,
           custom.coef.map = list("purefe"="$g$","int_p90p10_high" = "$g$* D 90th/10th",
                                  "int_p20p20_high" = "$g$*D 80th/20th","int_gini_wiid_high"="$g$ * D Gini"),
           include.ci=FALSE,digits=3,caption="IRRF AG and inequality: panel evidence",custom.model.names = c("(1) IRRF AG","(2) IRRF AG","(3) IRRF AG"),
           custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes"),
                "Year FE" = c("Yes","Yes","Yes")),reorder.gof = c(3,7,1,2,4,5,6,8,9,10),float.pos = "H")
} else{
    if (var_int=="") {
        texreg(list(reg9010s1,reg2020s1,regginis1),stars = c(0.01,0.05,0.1),file = paste0(tex,"/interactions_dummy_nocumvarint_AG.tex"), caption.above = T,
               custom.coef.map = list("AGpanel"="$g$","int_p90p10_high" = "$g$* D 90th/10th",
                                      "int_p20p20_high" = "$g$*D 80th/20th","int_gini_wiid_high"="$g$ * D Gini"),
               include.ci=FALSE,digits=3,caption="IRRF AG and inequality: panel evidence",custom.model.names = c("(1) IRRF AG","(2) IRRF AG","(3) IRRF AG"),
               custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes"),
                    "Year FE" = c("Yes","Yes","Yes")),reorder.gof = c(3,7,1,2,4,5,6,8,9,10),float.pos = "H")
    } else{
        texreg(list(reg9010s1,reg2020s1,regginis1),stars = c(0.01,0.05,0.1),file = paste0(tex,"/interactions_dummy_cumvarint_AG.tex"), caption.above = T,
               custom.coef.map = list("cum_dlng"="$g$","cum_int_p90p10_high" = "$g$* D 90th/10th",
                                      "cum_int_p20p20_high" = "$g$*D 80th/20th","cum_int_gini_wiid_high"="$g$ * D Gini"),
               include.ci=FALSE,digits=3,caption="IRRF AGand inequality: panel evidence",custom.model.names = c("(1) IRRF AG","(2) IRRF AG","(3) IRRF AG"),
               custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes"),
                    "Year FE" = c("Yes","Yes","Yes")),reorder.gof = c(3,7,1,2,4,5,6,8,9,10),float.pos = "H")
    }}

# -----------------------------------------------------------------------------
# Table A.6 and A.7 of final paper
# It creates table A.6 if depvar="", it creates table a.7 if depvar="cum_".
# You have to change the local variable defined at the beginning of this code.
# -----------------------------------------------------------------------------
# Specification 2: Interaction as inequality * variable
model2 <- function(ineq_measure,cum_depvar=dep_var,cum_varint=var_int){ if (inc.polrate=="yes") {
    ineq_model <- paste0(cum_depvar,"bond_yields ~",cum_varint,"purefe + L.(policy_rate,aglags) + ",cum_varint,"int_",ineq_measure,
                         " + L.(int_",ineq_measure,"_policy_rate,aglags)|country+date")
} else{
    ineq_model <- paste0(cum_depvar,"bond_yields ~",cum_varint,"purefe +",cum_varint,"int_",ineq_measure,"|country+date")
}}

reg9010s2 <- feols(formula(model2("p90p10")),vcov=error1,AGpanel)
reg2020s2 <- feols(formula(model2("p20p20")),vcov=error1,AGpanel)
regginis2 <- feols(formula(model2("gini_wiid")),vcov=error1,AGpanel)

summary(reg9010s2)
summary(reg2020s2)
summary(regginis2)

if (dep_var=="") {
    texreg(list(reg9010s2,reg2020s2,regginis2),stars = c(0.01,0.05,0.1),file = paste0(tex,"/TAB_A6_interactions_ineq_AG.tex"), caption.above = T,
           custom.coef.map = list("purefe"="$g$","int_p90p10" = "$g$ * 90th/10th","int_p20p20" = "$g$ * 80th/20th","int_gini_wiid"="$g$ * Gini"),
           include.ci=FALSE,digits=3,caption="IRRF AG and inequality: panel evidence",custom.model.names = c("(1) IRRF AG","(2) IRRF AG","(3) IRRF AG"),
           float.pos = "H",custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),
           custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes"),"Year FE" = c("Yes","Yes","Yes")),
           reorder.gof = c(3,7,1,2,4,5,6,8,9,10))
} else{
    if (var_int=="") {
        texreg(list(reg9010s2,reg2020s2,regginis2),stars = c(0.01,0.05,0.1),file = paste0(tex,"/interactions_ineq_nocumvarint_AG.tex"), caption.above = T,
               custom.coef.map = list("purefe"="$g$","int_p90p10" = "$g$ * 90th/10th","int_p20p20" = "$g$ * 80th/20th","int_gini_wiid"="$g$ * Gini"),
               include.ci=FALSE,digits=3,caption="IRRF AG and inequality: panel evidence",custom.model.names = c("(1) IRRF AG","(2) IRRF AG","(3) IRRF AG"),
               float.pos = "H",custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),
               custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes"),"Year FE" = c("Yes","Yes","Yes")),
               reorder.gof = c(3,7,1,2,4,5,6,8,9,10))
    } else {
        texreg(list(reg9010s2,reg2020s2,regginis2),stars = c(0.01,0.05,0.1),file = paste0(tex,"/TAB_A7_interactions_ineq_cumvarint_AG.tex"), caption.above = T,
               custom.coef.map = list("cum_purefe"="$g$","cum_int_p90p10" = "$g$ * 90th/10th","cum_int_p20p20" = "$g$ * 80th/20th",
                                      "cum_int_gini_wiid"="$g$ * Gini"),
               include.ci=FALSE,digits=3,caption="IRRF AG and inequality: panel evidence",custom.model.names = c("(1) IRRF AG","(2) IRRF AG","(3) IRRF AG"),
               float.pos = "H",custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),
               custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes"),"Year FE" = c("Yes","Yes","Yes")),
               reorder.gof = c(3,7,1,2,4,5,6,8,9,10))
        
    }}


# Specification 3: Sample split - Regress for sample above ineq and below ineq
if (inc.polrate=="yes" & dep_var=="" & var_int=="") {
    models3 <- formula("bond_yields ~ purefe + L.(policy_rate,aglags)|country+date")
} else if (inc.polrate=="yes" & var_int=="" & dep_var=="cum_") {
    models3 <- formula("cum_bond_yields ~ purefe + L.(policy_rate,aglags)|country+date")
} else if (inc.polrate=="yes" & var_int=="cum_" & dep_var=="cum_") {
    models3 <- formula("cum_bond_yields ~ cum_purefe + L.(policy_rate,aglags)|country+date")
} else if (inc.polrate=="no" & var_int=="cum_" & dep_var=="cum_") {
    models3 <- formula("cum_bond_yields ~ cum_purefe + L.(policy_rate,aglags)|country+date")
} else if (inc.polrate=="no" & var_int=="" & dep_var=="cum_"){
    models3 <- formula("cum_bond_yields ~ purefe|country+date")
} else if (inc.polrate=="no" & var_int=="" & dep_var=="") {
    models3 <- formula("bond_yields ~ purefe|country+date")
}

above_ineq_p20p20 <- AGpanel %>% filter(dummy_p20p20==1) %>% feols(models3)
below_ineq_p20p20 <- AGpanel %>% filter(dummy_p20p20==0) %>% feols(models3) 
summary(above_ineq_p20p20)
summary(below_ineq_p20p20)

above_ineq_p90p10 <- AGpanel %>% filter(dummy_p90p10==1) %>% feols(models3)
below_ineq_p90p10 <- AGpanel %>% filter(dummy_p90p10==0) %>% feols(models3)
summary(above_ineq_p90p10)
summary(below_ineq_p90p10)

above_ineq_gini_wiid <- AGpanel %>% filter(dummy_gini_wiid==1) %>% feols(models3)
below_ineq_gini_wiid <- AGpanel %>% filter(dummy_gini_wiid==0) %>% feols(models3)
summary(above_ineq_gini_wiid)
summary(below_ineq_gini_wiid)

if (dep_var=="") {
    texreg(list(above_ineq_p20p20,below_ineq_p20p20,above_ineq_p90p10,below_ineq_p90p10,above_ineq_gini_wiid,below_ineq_gini_wiid),
           stars = c(0.01,0.05,0.1),file = paste0(tex,"/interactions_split_AG.tex"), caption.above = T,
           custom.coef.map = list("purefe"="$g$"),include.ci=FALSE,digits=3,caption="IRRF AG and inequality: panel evidence",
           custom.model.names = c("High 80th/20th","Low 80th/20th","High 90th/10th",
                                  "Low 90th/10th","High Gini","Low Gini"),
           float.pos = "H",custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes","Yes","Yes","Yes"),
                                                  "Year FE" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
           custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),reorder.gof = c(3,7,1,2,4,5,6,8,9,10))
} else{
    if (var_int=="") {
        texreg(list(above_ineq_p20p20,below_ineq_p20p20,above_ineq_p90p10,below_ineq_p90p10,above_ineq_gini_wiid,below_ineq_gini_wiid),
               stars = c(0.01,0.05,0.1),file = paste0(tex,"/interactions_split_nocumvarint_AG.tex"), caption.above = T,
               custom.coef.map = list("purefe"="$g$"),include.ci=FALSE,digits=3,caption="IRRF and inequality: panel evidence",
               custom.model.names = c("High 80th/20th","Low 80th/20th","High 90th/10th",
                                      "Low 90th/10th","High Gini","Low Gini"),
               float.pos = "H",custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes","Yes","Yes","Yes"),
                                                      "Year FE" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
               custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),reorder.gof = c(3,7,1,2,4,5,6,8,9,10))
    } else {
        texreg(list(above_ineq_p20p20,below_ineq_p20p20,above_ineq_p90p10,below_ineq_p90p10,above_ineq_gini_wiid,below_ineq_gini_wiid),
               stars = c(0.01,0.05,0.1),file = paste0(tex,"/interactions_split_cumvarint_AG.tex"), caption.above = T,
               custom.coef.map = list("cum_purefe"="$g$"),include.ci=FALSE,digits=3,caption="IRRF AG and inequality: panel evidence",
               custom.model.names = c("High 80th/20th","Low 80th/20th","High 90th/10th",
                                      "Low 90th/10th","High Gini","Low Gini"),
               float.pos = "H",custom.gof.rows = list("Country FE" = c("Yes","Yes","Yes","Yes","Yes","Yes"),
                                                      "Year FE" = c("Yes","Yes","Yes","Yes","Yes","Yes")),
               custom.gof.names = c(NA,NA,NA,NA,"N. Obs",NA,NA,NA),reorder.gof = c(3,7,1,2,4,5,6,8,9,10))
    }
}
