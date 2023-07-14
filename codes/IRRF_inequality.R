# ---------------------------------------------------------------------------- #

# ----------- Panel regression by country: Local projection Method ----------- #
# --------- Estimation of the relationship between IRRF and inequality ------- #

# Hriday Karnani 
# Date: 03/16/2022

# IMPORTANT!! To run correctly this script you need to run panel_bycountry.R first

# ---------------------------------------------------------------------------- #

rm(list=ls())
# show results without without e+00
options(scipen=999)

# ---------------------------------------------------------------------------- #
# Set working directory
wd <- "C:/Users/Hriday/Dropbox/Hriday/IRRF/Replication"
setwd(wd)
rdata1 <- paste0(wd,"/data/")
results1 <- paste0(wd,"/results/")
figures1 <- paste0(wd,"/tablas_figuras/")
tex1 <- paste0(wd,"/tex/")

rdata <- as.character(read.csv(paste0(rdata1,"/rdata.txt")))
results <- as.character(read.csv(paste0(results1,"/results.txt")))
figures <- as.character(read.csv(paste0(figures1,"/figures.txt")))
tex <- as.character(read.csv(paste0(tex1,"/tex.txt")))


# ---------------------------------------------------------------------------- #
# Set if we consider Slovenia
# type "yes" or "no"
slov <- "no"
# ---------------------------------------------------------------------------- #

# Install and load packages
list.of.packages <- c("haven","dplyr", "readxl", "data.table","mFilter", "plm","texreg",
                      "zoo","sandwich","lmtest","ggplot2","ggthemes","doBy","estimatr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
rm(list.of.packages,new.packages) # con esto borro los objetos

# load libraries
Packages <- c("haven","dplyr", "readxl", "data.table","mFilter", "plm","texreg",
              "zoo","sandwich","lmtest","ggplot2","ggthemes","doBy","estimatr")
lapply(Packages, library, character.only = TRUE)

# read and delete last row (blank row)
db_ineq <- read_excel(paste0(rdata1,"database_cross_section_july21.xlsx"),sheet = "Sheet1")[-35,]

# delete non oecd countries
nooecd <- c("CHI","MEX","EST","ISR","TUR","LUX")
db_ineq <- db_ineq[!(db_ineq$location %in% nooecd),]
db_ineq <- db_ineq %>% select(!debt_inc)

db_ineq <- rename(db_ineq, fin_open = fina_openness,trade_open = trade_openness,country_code = location,
                  debt_inc = av_debt_inc)

# generate new log variables
db_ineq$l_infl_vol <- log(db_ineq$infl_vol)
db_ineq$lcredit_bis_gdp <- log(db_ineq$credit_bis_gdp)
db_ineq$lpop95 <- log(db_ineq$pop95)
db_ineq$lgdppc <- log(db_ineq$gdp_pc)

# generate sov risk
db_ineq$sov_risk <- 0
db_ineq$sov_risk <- ifelse(db_ineq$country_code == "CZE"| db_ineq$country_code == "HUN"|
                               db_ineq$country_code == "POL"|db_ineq$country_code == "SVN",1,db_ineq$sov_risk)
db_ineq$sov_risk <- ifelse(db_ineq$country_code == "SVK",2,db_ineq$sov_risk)

# load ineq, irrf and prrf df
new_ineq <- read_dta(paste0(rdata1,"new_ineq.dta"))
irrf <- read_xlsx(paste0(results,"irrf_lp_country.xlsx"))
prrf <- read_xlsx(paste0(results,"prrf_lp_country.xlsx"))
crf <- read_xlsx(paste0(results,"crf_lp_country.xlsx"))
gov_debt <- read_dta(paste0(rdata1,"gov_debt.dta"))

# merge df
db_newineq <- merge(db_ineq,new_ineq)
db_newineq <- merge(db_newineq,irrf)
db_newineq <- merge(db_newineq,prrf)
db_newineq <- merge(db_newineq,crf)
db_newineq <- merge(db_newineq,gov_debt)

# gen and save subset of df
db_newineq$location <- db_newineq$country_code 
db_newineq$inequality <- db_newineq$p90_p10 
new_ineq_toAG <- db_newineq %>% select(country,location,p90_p10,gini_wiid,p20p20)
writexl::write_xlsx(new_ineq_toAG,paste0(results,"new_ineq_toAG.xlsx"))

# ---------------------------------------- PLOTS ---------------------------- #

# Income risk vs Inequality PLOT
reg2 <- lm_robust(p20p20 ~ irisk,data=db_newineq, se_type = "HC1")
ggplot(db_newineq) + geom_point(aes(x=p20p20,y=irisk,label = country_code),color = "darkred",size=3) + 
    theme_stata(scheme = "s1mono") + geom_smooth(aes(x=p20p20,y=irisk),method = lm,se=F) + 
    geom_text(aes(x=p20p20,y=irisk,label = country_code),hjust=-0.3,vjust=0,size=3) + 
    theme(panel.background = element_blank()) + 
    xlab("Ratio income 80th/20th percentiles") + ylab("Income risk") + 
    labs(title = "Income risk and Inequality",
         caption = paste0("Note: The slope of the regression is ",round(reg2$coefficients[2],3),
                          " (robust standard error of ", round(reg2$std.error[2],3),")"))
ggsave(paste0(figures,"irisk_p80p20_lp.png"))

# test
db_newineq1 <- db_newineq %>% arrange(desc(country))

# -----------------------------------------------------------------------------
# FIGURE 2 OF THE FINAL PAPER
# -----------------------------------------------------------------------------
# IRRF by country
bp_plot <- ggplot(db_newineq1,aes(x=country,y=IRRF)) +  geom_bar(aes(y=IRRF),stat = "identity",width = 0.75) + 
     theme_stata(scheme = "s1mono") + labs(title = "IRRF") + xlab("") + ylab("") + coord_flip() + 
    scale_x_discrete(limits = db_newineq1$country) +
    theme(axis.text.y = element_text(angle = 360),panel.grid.major  = element_blank(),
          panel.background = element_blank())  #+
#    ylim(c(-40,40))
bp_plot
ggsave(paste0(figures,"FIG_2_hbar_g_i_bondLP.png"),plot = bp_plot)

# -----------------------------------------------------------------------------
# FIGURE 3 (ineq vs irrf)
# -----------------------------------------------------------------------------
reg2 <- lm_robust(IRRF ~ p20p20, data=db_newineq, se_type = "HC1")
ggplot(db_newineq) + geom_point(aes(x=p20p20,y=IRRF,label = country_code),color = "darkred",size=3) + 
    theme_stata(scheme = "s1mono") + geom_smooth(aes(x=p20p20,y=IRRF),color="blue",method = lm,se=F) + 
    geom_text(aes(x=p20p20,y=IRRF,label = country_code),hjust=-0.3,vjust=0,size=3) + xlim(c(3,8)) + #ylim(c(-40,40)) +
    theme(panel.background = element_blank()) + xlab("Ratio income 80th/20th percentiles") + 
    ylab("IRRF")  + labs(title = "IRRF and Inequality",
    caption = paste0("Note: The slope of the regression is ",round(reg2$coefficients[2],3),
                                " (robust standard error of ", round(reg2$std.error[2],3),")"),color="Legend")
ggsave(paste0(figures,"FIG_3_IRRF_p80p20_lp.png"))


# Policy rates plots
reg2 <- lm_robust(PRRF ~ inequality, data=db_newineq, se_type = "HC1")
ggplot(db_newineq) + geom_point(aes(x=inequality,y=PRRF,label = country_code),color = "darkred",size=3) + 
    theme_stata(scheme = "s1mono") + geom_smooth(aes(x=inequality,y=PRRF),color="blue",method = lm,se=F) + 
    geom_text(aes(x=inequality,y=PRRF,label = country_code),hjust=-0.3,vjust=0,size=3)  + #ylim(c(-25,25)) +
    theme(panel.background = element_blank()) + xlab("Ratio income 90th/10th percentiles") + 
    ylab("PRRF") + labs(title = "PRRF and Inequality",
                        caption = paste0("Note: The slope of the regression is ",round(reg2$coefficients[2],3),
                                         " (robust standard error of ", round(reg2$std.error[2],3),")"),color="Legend")
ggsave(paste0(figures,"PRRF_p90p10_lp.png"))

# Exclude Slovenia
reg2 <- lm_robust(PRRF ~ inequality, data=db_newineq[!(db_newineq$country %in% c("Slovenia")),], se_type = "HC1")
ggplot(db_newineq[!(db_newineq$country %in% c("Slovenia")),]) + 
    geom_point(aes(x=inequality,y=PRRF,label = country_code),color = "darkred",size=3) + 
    theme_stata(scheme = "s1mono") + geom_smooth(aes(x=inequality,y=PRRF),color="blue",method = lm,se=F) + 
    geom_text(aes(x=inequality,y=PRRF,label = country_code),hjust=-0.3,vjust=0,size=3)  + #ylim(c(-25,25)) +
    theme(panel.background = element_blank()) + xlab("Ratio income 90th/10th percentiles") + 
    ylab("PRRF") + labs(title = "PRRF and Inequality",
                        caption = paste0("Note: The slope of the regression is ",round(reg2$coefficients[2],3),
                                         " (robust standard error of ", round(reg2$std.error[2],3),")"),color="Legend")
ggsave(paste0(figures,"PRRF_p90p10_lp_wslovenia.png"))


# -----------------------------------------------------------------------------
# FIGURE A.2 
# -----------------------------------------------------------------------------
# CRF PLOTS
# CRF by country
cbp_plot <- ggplot(db_newineq1,aes(x=country,y=CRF)) +  geom_bar(aes(y=CRF),stat = "identity",width = 0.75) + 
    theme_stata(scheme = "s1mono") + labs(title = "CRF") + xlab("") + ylab("") + coord_flip() + 
    scale_x_discrete(limits = db_newineq1$country) +
    theme(axis.text.y = element_text(angle = 360),panel.grid.major  = element_blank(),
          panel.background = element_blank())  #+
#    ylim(c(-40,40))
cbp_plot
ggsave(paste0(figures,"FIG_A2_hbar_g_i_consLP.png"),plot = cbp_plot)

#  (ineq vs CRF)
reg2 <- lm_robust(CRF ~ p20p20, data=db_newineq, se_type = "HC1")
ggplot(db_newineq) + geom_point(aes(x=p20p20,y=CRF,label = country_code),color = "darkred",size=3) + 
    theme_stata(scheme = "s1mono") + geom_smooth(aes(x=p20p20,y=CRF),color="blue",method = lm,se=F) + 
    geom_text(aes(x=p20p20,y=CRF,label = country_code),hjust=-0.3,vjust=0,size=3) + xlim(c(3,8)) + #ylim(c(-40,40)) +
    theme(panel.background = element_blank()) + xlab("Ratio income 80th/20th percentiles") + 
    ylab("CRF")  + labs(title = "CRF and Inequality",
                         caption = paste0("Note: The slope of the regression is ",round(reg2$coefficients[2],3),
                                          " (robust standard error of ", round(reg2$std.error[2],3),")"),color="Legend")
ggsave(paste0(figures,"CRF_p80p20_lp.png"))

#  (ineq vs CRF)
reg2 <- lm_robust(IRRF ~ CRF, data=db_newineq, se_type = "HC1")
ggplot(db_newineq) + geom_point(aes(x=CRF,y=IRRF,label = country_code),color = "darkred",size=3) + 
    theme_stata(scheme = "s1mono") + geom_smooth(aes(x=CRF,y=IRRF),color="blue",method = lm,se=F) + 
    geom_text(aes(x=CRF,y=IRRF,label = country_code),hjust=-0.3,vjust=0,size=3) +# xlim(c(3,8)) + #ylim(c(-40,40)) +
    theme(panel.background = element_blank()) + xlab("CRF") + 
    ylab("IRRF")  + labs(title = "CRF and IRRF",
                        caption = paste0("Note: The slope of the regression is ",round(reg2$coefficients[2],3),
                                         " (robust standard error of ", round(reg2$std.error[2],3),")"),color="Legend")
ggsave(paste0(figures,"CRF_IRRF_lp.png"))


# ---------------------------- REGRESSIONS ----------------------------------- #

db_newineq <- rename(db_newineq, inequality9607 = p90p10_9607base11)

db_newineq2 <- db_newineq
# adjust by their SD
db_newineq$inequality <- db_newineq$inequality/sd(db_newineq$inequality)
db_newineq$p20p20 <- db_newineq$p20p20/sd(db_newineq$p20p20)
db_newineq$gini_wiid <- db_newineq$gini_wiid/sd(db_newineq$gini_wiid)
db_newineq$fin_open <- db_newineq$fin_open/sd(db_newineq$fin_open)
db_newineq$infl_vol <- db_newineq$infl_vol/sd(db_newineq$infl_vol)
db_newineq$ext_g_debt <- db_newineq$ext_g_debt/sd(db_newineq$ext_g_debt,na.rm = T)
db_newineq$gdp_pc <- db_newineq$gdp_pc/sd(db_newineq$gdp_pc,)
db_newineq$lpop95 <- db_newineq$lpop95/sd(db_newineq$lpop95)
db_newineq$lgdppc <- db_newineq$lgdppc/sd(db_newineq$lgdppc)
db_newineq$gov_debt_av <- db_newineq$gov_debt_av/sd(db_newineq$gov_debt_av)
db_newineq$irisk <- db_newineq$irisk/sd(db_newineq$irisk,na.rm=T)


if (slov == "no") {db_newineq3 <-  db_newineq[!(db_newineq$country %in% "Slovenia"),]
}  else {db_newineq3 <-  db_newineq}

# IRRF vs Ineq
# R uses analytics weights by default
# base model
wreg1 <- lm_robust(IRRF ~ p20p20 + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
wreg2 <- lm_robust(IRRF ~ inequality + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
wreg3 <- lm_robust(IRRF ~ gini_wiid + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
wreg1; wreg2; wreg3

regrows <- c("Income ratio 80th/20th","Income ratio 90th/10th","Income Gini")
texreg(list(wreg1,wreg2,wreg3),stars = c(0.01,0.05,0.1),file = paste0(tex,"/cross_section_LPall.tex"), caption.above = T,
       omit.coef = c("Intercept|gdp_pc|fin_open|gov_debt_av"),include.ci=FALSE,custom.coef.names = 
           c(regrows),caption="IRRF and Inequality",custom.model.names = c("(1) IRRF","(2) IRRF","(3) IRRF"),
       include.adjrs=F,include.rmse=F,float.pos = "H")

# -----------------------------------------------------------------------------
# TABLE 1 OF FINAL PAPER
# It can also create table A.2 if you run panel_bycountry.R with lags<-1:2
# It can also create table A.3 if you run panel_bycountry.R with lags<-1:6
# -----------------------------------------------------------------------------
# OLS instead of WLS
reg1 <- lm_robust(IRRF ~ p20p20 + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, se_type = "HC1")
reg2 <- lm_robust(IRRF ~ inequality + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, se_type = "HC1")
reg3 <- lm_robust(IRRF ~ gini_wiid + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, se_type = "HC1")
reg1; reg2; reg3
texreg(list(reg1,reg2,reg3),stars = c(0.01,0.05,0.1),file = paste0(tex,"/TAB_1_cross_section_LPall_OLS.tex"), caption.above = T,
       omit.coef = c("Intercept|gdp_pc|fin_open|gov_debt_av"),include.ci=FALSE,custom.coef.names = 
           c(regrows),caption="IRRF and Inequality (OLS)",custom.model.names = c("(1) IRRF","(2) IRRF","(3) IRRF"),
       include.adjrs=F,include.rmse=F,float.pos = "H")


# base model + debt
wreg1 <- lm_robust(IRRF ~ p20p20 + debt_inc + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
wreg2 <- lm_robust(IRRF ~ inequality + debt_inc + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
wreg3 <- lm_robust(IRRF ~ gini_wiid + debt_inc + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
wreg1; wreg2; wreg3

# base model + irisk
wreg1 <- lm_robust(IRRF ~ p20p20 + irisk + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
wreg2 <- lm_robust(IRRF ~ inequality + irisk + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
wreg3 <- lm_robust(IRRF ~ gini_wiid + irisk + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
wreg1; wreg2; wreg3

texreg(list(wreg1,wreg2,wreg3),stars = c(0.01,0.05,0.1),file = paste0(tex,"/cross_section_LPall_risk.tex"), caption.above = T,
       omit.coef = c("Intercept|irisk|gdp_pc|fin_open|gov_debt_av"),include.ci=FALSE,custom.coef.names = 
           c(regrows),caption="IRRF and Inequality",custom.model.names = c("(1) IRRF","(2) IRRF","(3) IRRF"),
       include.adjrs=F,include.rmse=F)

wreg1 <- lm_robust(IRRF ~ gini_wiid + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
#wreg2 <- lm_robust(IRRF ~ gini_wiid + iriskRC + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
#wreg3 <- lm_robust(IRRF ~ gini_wiid + iriskIC + iriskW  + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
wreg5 <- lm_robust(IRRF ~ iriskW  + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
wreg4 <- lm_robust(IRRF ~ gini_wiid + iriskW + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
wreg1;wreg4;wreg5

regrows1 <- c("Gini","Within Income Risk")
texreg(list(wreg1,wreg5,wreg4),stars = c(0.01,0.05,0.1),file = paste0(tex,"/cross_section_LPall_risk_ORIGC_GINI.tex"), caption.above = T,
       omit.coef = c("Intercept|gdp_pc|fin_open|gov_debt_av"),include.ci=FALSE,custom.coef.names=c(regrows1),
       caption="IRRF and Inequality",custom.model.names = c("(1) IRRF","(2) IRRF","(3) IRRF"),
       include.adjrs=F,include.rmse=F)

# -----------------------------------------------------------------------------
# TABLES A.9 until A.11, the only difference is the inequality measure, in order
# to create tables A.9 or A.11, you just have to change "gini_wiid" for p20p20 or p90p10
# -----------------------------------------------------------------------------
# OLS for irisk
reg1 <- lm_robust(IRRF ~ gini_wiid + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, se_type = "HC1")
#wreg2 <- lm_robust(IRRF ~ gini_wiid + iriskRC + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
#wreg3 <- lm_robust(IRRF ~ gini_wiid + iriskIC + iriskW  + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = weights,se_type = "HC1")
reg5 <- lm_robust(IRRF ~ iriskW  + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, se_type = "HC1")
reg4 <- lm_robust(IRRF ~ gini_wiid + iriskW + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, se_type = "HC1")
reg1;reg4;reg5

regrows1 <- c("Gini","Within Income Risk")
texreg(list(reg1,reg5,reg4),stars = c(0.01,0.05,0.1),file = paste0(tex,"/TAB_A9A10A11_cross_section_LPall_risk_OLS_ORIGC_gini.tex"), caption.above = T,
       omit.coef = c("Intercept|gdp_pc|fin_open|gov_debt_av"),include.ci=FALSE,custom.coef.names=c(regrows1),
       caption="IRRF and Inequality",custom.model.names = c("(1) IRRF","(2) IRRF","(3) IRRF"),
       include.adjrs=F,include.rmse=F)


# PRRF vs Ineq (base model)
wreg1 <- lm_robust(PRRF ~ p20p20 + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = Pweights,se_type = "HC1")
wreg2 <- lm_robust(PRRF ~ inequality + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = Pweights,se_type = "HC1")
wreg3 <- lm_robust(PRRF ~ gini_wiid + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, weights = Pweights,se_type = "HC1")
wreg1; wreg2; wreg3

texreg(list(wreg1,wreg2,wreg3),stars = c(0.01,0.05,0.1),file = paste0(tex,"/cross_section_LPall_PRRF.tex"), caption.above = T,
       omit.coef = c("Intercept|gdp_pc|fin_open|gov_debt_av"),include.ci=FALSE,custom.coef.names = 
           c(regrows),caption="PRRF and Inequality",custom.model.names = c("(1) PRRF","(2) PRRF","(3) PRRF"),
       include.adjrs=F,include.rmse=F,float.pos = "H")

# OLS instead of WLS
reg1 <- lm_robust(PRRF ~ p20p20 + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, se_type = "HC1")
reg2 <- lm_robust(PRRF ~ inequality + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, se_type = "HC1")
reg3 <- lm_robust(PRRF ~ gini_wiid + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, se_type = "HC1")
reg1; reg2; reg3

texreg(list(reg1,reg2,reg3),stars = c(0.01,0.05,0.1),file = paste0(tex,"/cross_section_LPall_PRRF_OLS.tex"), caption.above = T,
       omit.coef = c("Intercept|gdp_pc|fin_open|gov_debt_av"),include.ci=FALSE,custom.coef.names = 
           c(regrows),caption="PRRF and Inequality (OLS)",custom.model.names = c("(1) PRRF","(2) PRRF","(3) PRRF"),
       include.adjrs=F,include.rmse=F,float.pos = "H")


# -----------------------------------------------------------------------------
# TABLE 2 OF FINAL PAPER
# -----------------------------------------------------------------------------

# OLS instead of WLS
reg1 <- lm_robust(PRRF ~ p20p20 + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, se_type = "HC1")
reg2 <- lm_robust(PRRF ~ inequality + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, se_type = "HC1")
reg3 <- lm_robust(PRRF ~ gini_wiid + gdp_pc + fin_open + gov_debt_av, data = db_newineq3, se_type = "HC1")
reg1; reg2; reg3

texreg(list(reg1,reg2,reg3),stars = c(0.01,0.05,0.1),file = paste0(tex,"/TAB_2_cross_section_LPall_PRRF_OLS.tex"), caption.above = T,
       omit.coef = c("Intercept|gdp_pc|fin_open|gov_debt_av"),include.ci=FALSE,custom.coef.names = 
           c(regrows),caption="PRRF and Inequality (OLS)",custom.model.names = c("(1) PRRF","(2) PRRF","(3) PRRF"),
       include.adjrs=F,include.rmse=F,float.pos = "H")


# ---------------------------------------------------------------------------- #
# --------------------------- LP with AG13 METHOD ---------------------------- #
# ---------------------------------------------------------------------------- #
db_newineq2$debt_to_asset_indebted <- db_newineq2$`Median debt-to-assets ratio of indebted households`
db_newineq2$debt_to_income_indebted <- db_newineq2$`Median debt-to-income ratio of indebted households` 
db_newineq2$shr_indebted <- db_newineq2$`Share of indebted households` 
db_newineq2$wealth_bott60 <- db_newineq2$`Share of bottom 60% of wealth`


db_newineq2 <- mutate(db_newineq2, g_i_adj_bond =  g_i_bond07/(g_i_bond_ub07-g_i_bond_lb07),
                     mult4adj = mul4/(mulub4-mullb4), wealth_bott60 = wealth_bott60/100,
                     debt_assets = (debt_to_asset_indebted/100)*(shr_indebted/100))

db_newineq2$sov_risk <- 0
db_newineq2$sov_risk <- ifelse(db_ineq$country_code == "GRC",1,db_ineq$sov_risk)
db_newineq2$sov_risk <- ifelse(db_ineq$country_code == "ITA",4,db_ineq$sov_risk)
db_newineq2$sov_risk <- ifelse(db_ineq$country_code == "ESP",5,db_ineq$sov_risk)
db_newineq2$sov_risk <- ifelse(db_ineq$country_code == "IRL",7,db_ineq$sov_risk)
db_newineq2$sov_risk <- ifelse(db_ineq$country_code == "BEL"|db_ineq$country_code == "BEL",8,db_ineq$sov_risk)
db_newineq2$sov_risk <- ifelse(db_ineq$country_code == "AUT"|db_ineq$country_code == "FIN",9,db_ineq$sov_risk)
db_newineq2$sov_risk <- ifelse(db_ineq$country_code == "DEU"|db_ineq$country_code == "NLD",10,db_ineq$sov_risk)

db_newineq2 <- db_newineq2[order(db_newineq$country_code),]
db_newineq2$location <- 1:28

iirf_oat <- read.csv(paste0(rdata,"iirf_oneatatime.csv"))
prrf_oat <- read.csv(paste0(rdata,"prrf_oneatatime.csv"))

ineq_rf <- merge(db_newineq2,iirf_oat,by = "country_code")
ineq_rf <- merge(ineq_rf,prrf_oat,by = "country_code")

ineq_rf <- mutate(ineq_rf, IIRF_bond1_adj = 0.25*IIRF_bond1_/(IIRF_bond1_ub_-IIRF_bond1_lb_),
                  weights1 = 1/(IIRF_bond1_ub_-IIRF_bond1_lb_),
                  weights1_p = 1/(PRRF_bond1_ub_-PRRF_bond1_lb_),
                  IIRF_bond1_ = IIRF_bond1_/4)

ineq_rf$euro <- ifelse(ineq_rf$country == "Austria"|ineq_rf$country == "Belgium"|
                           ineq_rf$country == "France"|ineq_rf$country == "Germany"|
                           ineq_rf$country == "Finland"|ineq_rf$country == "Greece"|
                           ineq_rf$country == "Ireland"|ineq_rf$country == "Italy"|
                           ineq_rf$country == "Netherlands"|ineq_rf$country == "Slovenia"|
                           ineq_rf$country == "Spain",1,0)

ineq_rf$euro <- ifelse(ineq_rf$country == "Austria"|ineq_rf$country == "Belgium"|
                           ineq_rf$country == "France"|ineq_rf$country == "Germany"|
                           ineq_rf$country == "Finland"|ineq_rf$country == "Greece"|
                           ineq_rf$country == "Ireland"|ineq_rf$country == "Italy"|
                           ineq_rf$country == "Netherlands"|ineq_rf$country == "Spain",1,0)

ineq_rf <- mutate(ineq_rf,IIRF_bond1_ = IIRF_bond1_*100,PRRF_bond1_ = PRRF_bond1_*100)

ineq_rf1 <- ineq_rf %>% arrange(desc(country))

# ------------------------------- PLOT IRRF AG METHOD ------------------------ #

# -----------------------------------------------------------------------------
# FIGURE A.1 of final paper
# -----------------------------------------------------------------------------
# IRRF by country
ag_plot <- ggplot(ineq_rf1,aes(x=country,y=IIRF_bond1_)) + geom_bar(aes(y=IIRF_bond1_),stat = "identity",width = 0.75) + 
    theme_stata(scheme = "s1mono") + labs(title = "IRRF (Auerbach and Gorodnichenko (2013))") + xlab("") + ylab("") +  coord_flip() + 
    scale_x_discrete(limits = ineq_rf1$country) + theme(axis.text.y = element_text(angle = 360),panel.grid.major = element_blank(),
          panel.background = element_blank()) # + ylim(c(-12,12))
ag_plot
ggsave(paste0(figures,"FIG_A1_hbar_gibond1_AG13.png"),ag_plot)

reg2 <- lm_robust(IIRF_bond1_ ~ p20p20, data=ineq_rf, se_type = "HC1")
ggplot(ineq_rf) + geom_point(aes(x=p20p20,y=IIRF_bond1_,label = country_code),color = "darkred",size=3) + 
    theme_stata(scheme = "s1mono") + geom_smooth(aes(x=p20p20,y=IIRF_bond1_),color="blue",method = lm,se=F) + 
    geom_text(aes(x=p20p20,y=IIRF_bond1_,label = country_code),hjust=-0.3,vjust=0,size=3) +# xlim(c(3,8)) + ylim(c(-12,12)) + 
    xlab("Ratio income 80th/20th percentiles") + ylab("IRRF AG") + theme(panel.background = element_blank()) +
    labs(title = "IRRG AG and Inequality",
         caption = paste0("Note: The slope of the regression is ",round(reg2$coefficients[2],3),
                          " (robust standard error of ", round(reg2$std.error[2],3),")"),color="Legend")
ggsave(paste0(figures,"gi_bond_p20p20_AG.png"))


# adjust by sd to regress
ineq_rf$inequality <- ineq_rf$inequality/sd(ineq_rf$inequality)
ineq_rf$p20p20 <- ineq_rf$p20p20/sd(ineq_rf$p20p20)
ineq_rf$gini_wiid <- ineq_rf$gini_wiid/sd(ineq_rf$gini_wiid)
ineq_rf$fin_open <- ineq_rf$fin_open/sd(ineq_rf$fin_open)
ineq_rf$ext_g_debt <- ineq_rf$ext_g_debt/sd(ineq_rf$ext_g_debt,na.rm = T)
ineq_rf$lpop95 <- ineq_rf$lpop95/sd(ineq_rf$lpop95)
ineq_rf$lgdppc <- ineq_rf$lgdppc/sd(ineq_rf$lgdppc)
ineq_rf$gov_debt_av <- ineq_rf$gov_debt_av/sd(ineq_rf$gov_debt_av)
ineq_rf$irisk <- ineq_rf$irisk/sd(ineq_rf$irisk,na.rm=T)

#--------------------------------- REGRESSIONS AG ----------------------------- # 

# IRRF AG vs Ineq
# R uses analytics weights by default
# base model
wreg1 <- lm_robust(IIRF_bond1_ ~ p20p20 + gdp_pc + fin_open + gov_debt_av, data = ineq_rf, weights = weights1,se_type = "HC1")
wreg2 <- lm_robust(IIRF_bond1_ ~ inequality + gdp_pc + fin_open + gov_debt_av, data = ineq_rf, weights = weights1,se_type = "HC1")
wreg3 <- lm_robust(IIRF_bond1_ ~ gini_wiid + gdp_pc + fin_open + gov_debt_av, data = ineq_rf, weights = weights1,se_type = "HC1")
wreg1; wreg2; wreg3

texreg(list(wreg1,wreg2,wreg3),stars = c(0.01,0.05,0.1),file = paste0(tex,"cross_sectionLP_AGall.tex"), caption.above = T,
       omit.coef = c("Intercept|gdp_pc|fin_open|gov_debt_av"),include.ci=FALSE,custom.coef.names = 
           c(regrows),caption="IRRF AG and Inequality",custom.model.names = c("(1) IRRF AG","(2) IRRF AG","(3) IRRF AG"),
       include.adjrs=F,include.rmse=F,float.pos = "H")

# -----------------------------------------------------------------------------
# TABLE A.4
# -----------------------------------------------------------------------------
# OLS instead of WLS
reg1 <- lm_robust(IIRF_bond1_ ~ p20p20 + gdp_pc + fin_open + gov_debt_av, data = ineq_rf,se_type = "HC1")
reg2 <- lm_robust(IIRF_bond1_ ~ inequality + gdp_pc + fin_open + gov_debt_av, data = ineq_rf,se_type = "HC1")
reg3 <- lm_robust(IIRF_bond1_ ~ gini_wiid + gdp_pc + fin_open + gov_debt_av, data = ineq_rf,se_type = "HC1")
reg1; reg2; reg3

texreg(list(reg1,reg2,reg3),stars = c(0.01,0.05,0.1),file = paste0(tex,"TAB_A4_cross_sectionLP_AGall_OLS.tex"), caption.above = T,
       omit.coef = c("Intercept|gdp_pc|fin_open|gov_debt_av"),include.ci=FALSE,custom.coef.names = 
           c(regrows),caption="IRRF AG and Inequality (OLS)",custom.model.names = c("(1) IRRF AG","(2) IRRF AG","(3) IRRF AG"),
       include.adjrs=F,include.rmse=F,float.pos = "H")

# -----------------------------------------------------------------------------
# TABLE A.5
# ----------------------------------------------------------------------------
# PRRF AG vs Ineq
wreg1 <- lm_robust(PRRF_bond1_ ~ p20p20 + gdp_pc + fin_open + gov_debt_av, data = ineq_rf, weights = weights1_p,se_type = "HC1")
wreg2 <- lm_robust(PRRF_bond1_ ~ inequality + gdp_pc + fin_open + gov_debt_av, data = ineq_rf, weights = weights1_p,se_type = "HC1")
wreg3 <- lm_robust(PRRF_bond1_ ~ gini_wiid + gdp_pc + fin_open + gov_debt_av, data = ineq_rf, weights = weights1_p,se_type = "HC1")
wreg1; wreg2; wreg3

texreg(list(wreg1,wreg2,wreg3),stars = c(0.01,0.05,0.1),file = paste0(tex,"TAB_A5_cross_sectionLP_AGall_PRRF.tex"), caption.above = T,
       omit.coef = c("Intercept|gdp_pc|fin_open|gov_debt_av"),include.ci=FALSE,custom.coef.names = 
           c(regrows),caption="PRRF AG and Inequality",custom.model.names = c("(1) PRRF AG","(2) PRRF AG","(3) PRRF AG"),
       include.adjrs=F,include.rmse=F,float.pos = "H")

# OLS instead of WLS
reg1 <- lm_robust(PRRF_bond1_ ~ p20p20 + gdp_pc + fin_open + gov_debt_av, data = ineq_rf, se_type = "HC1")
reg2 <- lm_robust(PRRF_bond1_ ~ inequality + gdp_pc + fin_open + gov_debt_av, data = ineq_rf, se_type = "HC1")
reg3 <- lm_robust(PRRF_bond1_ ~ gini_wiid + gdp_pc + fin_open + gov_debt_av, data = ineq_rf, se_type = "HC1")
reg1; reg2; reg3

texreg(list(reg1,reg2,reg3),stars = c(0.01,0.05,0.1),file = paste0(tex,"cross_sectionLP_AGall_PRRF_OLS.tex"), caption.above = T,
       omit.coef = c("Intercept|gdp_pc|fin_open|gov_debt_av"),include.ci=FALSE,custom.coef.names = 
           c(regrows),caption="PRRF AG and Inequality (OLS)",custom.model.names = c("(1) PRRF AG","(2) PRRF AG","(3) PRRF AG"),
       include.adjrs=F,include.rmse=F,float.pos = "H")


# -------------------------- PLOT BP vs AG ----------------------------------- # 
cor_agbp <- cor((ineq_rf$IRRF),(ineq_rf$IIRF_bond1_))
cort <- cor.test((ineq_rf$IRRF),(ineq_rf$IIRF_bond1_))
rowname <- as.character(read.csv(paste0(results1,"/rownames.txt")))
export_cor <- cbind(rowname,round(cort$estimate,3),round(cort$p.value,3))
write.table(export_cor,paste0(results1,"correlations_AGvBP.csv"),append = TRUE,row.names = F,
            col.names=F)
?write.table
range <- abs(max(c(abs(ineq_rf$IRRF),abs(ineq_rf$IIRF_bond1_)),na.rm = T))+1
axis <- c(-range,range)
reg2 <- lm_robust(IIRF_bond1_ ~ IRRF, data = ineq_rf,se_type = "HC1")
ggplot(ineq_rf) + geom_point(aes(x=IRRF, y= IIRF_bond1_)) + geom_smooth(aes(x=IRRF,y=IIRF_bond1_,color="darkred"),method="lm",se=F) + 
     geom_abline(aes(intercept=0,slope=1,color = "black"),linetype="dashed") + xlim(axis) + ylim(axis) +
    geom_text(aes(x=IRRF,y=IIRF_bond1_,label = country_code),hjust=-0.3,vjust=0,size=3) + theme_stata(scheme = "s1mono") + 
    labs(title = "IRRF BP vs IRRF AG",
         caption = paste0("Note: The slope of the regression is ",round(reg2$coefficients[2],3),
                          " (robust standard error of ", round(reg2$std.error[2],3),")")) +
    xlab("IRRF BP") + ylab("IRRF AG") + theme(panel.background = element_blank()) + 
     scale_color_identity(name = "", breaks = c("darkred", "black"),
                          labels = c("Fitted values", "45° degree line"),guide = "legend")
ggsave(paste0(figures,"irrfbp_irrfag_45d.png"))

reg2 <- lm_robust( IIRF_bond1_~ IRRF, data = ineq_rf,se_type = "HC1")
ggplot(ineq_rf) + geom_point(aes(x=IRRF, y= IIRF_bond1_)) + geom_smooth(aes(x=IRRF,y=IIRF_bond1_,color="darkred"),method="lm",se=F) + 
    geom_text(aes(x=IRRF,y=IIRF_bond1_,label = country_code),hjust=-0.3,vjust=0,size=3) + theme_stata(scheme = "s1mono") + 
    labs(title = "IRRF BP vs IRRF AG",
         caption = paste0("Note: The slope of the regression is ",round(reg2$coefficients[2],3),
                          " (robust standard error of ", round(reg2$std.error[2],3),")")) +
    xlab("IRRF BP") + ylab("IRRF AG") + theme(panel.background = element_blank()) + 
    scale_color_identity(name = "", breaks = c("darkred", "black"),
                         labels = c("Fitted values", "45° degree line"),guide = "legend")
ggsave(paste0(figures,"irrfbp_irrfag.png"))

ggplot(ineq_rf) + geom_point(aes(x=IRRF, y= IIRF_bond1_)) +
    geom_text(aes(x=IRRF,y=IIRF_bond1_,label = country_code),hjust=-0.3,vjust=0,size=3) + theme_stata(scheme = "s1mono") +  
    geom_abline(aes(intercept=0,slope=1,color = "black"),linetype="dashed") + xlim(axis) + ylim(axis) +
    labs(title = "IRRF BP vs IRRF AG",
         caption = paste0("Note: The correlation is ",round(cort$estimate,3),
                          " (p-value of ", round(cort$p.value,3),")")) +
    xlab("IRRF BP") + ylab("IRRF AG") + theme(panel.background = element_blank()) + 
    scale_color_identity(name = "", breaks = c("darkred", "black"),
                         labels = c("Fitted values", "45° degree line"),guide = "legend")
ggsave(paste0(figures,"irrfbp_irrfag_cor.png"))


ggplot(ineq_rf) + geom_point(aes(x=IRRF, y= IIRF_bond1_)) +
    xlim(c(-35,35)) + ylim(c(-35,35)) + geom_abline(aes(intercept=0,slope=1,color = "black"),linetype="dashed") +
    geom_text(aes(x=IRRF,y=IIRF_bond1_,label = country_code),hjust=-0.3,vjust=0,size=3) + theme_stata(scheme = "s1mono") +
    labs(title = "IRRF BP vs IRRF AG",caption = "Note: The slope of the regression is -0.18 (robust standard error of 0.36)") +
    xlab("IRRF BP") + ylab("IRRF AG") + theme(panel.background = element_blank()) +
    scale_color_identity(name = "", breaks = c("darkred", "black"),
                         labels = c("Fitted values", "45° degree line"),guide = "legend")
ggsave(paste0(figures,"irrfbp_irrfag_nofit.png"))

# ggplot(ineq_rf) + geom_point(aes(x=IRRF, y= IIRF_bond1_)) + 
#     xlim(c(-35,35)) + ylim(c(-12,12)) + geom_abline(aes(intercept=0,slope=1/3,color = "black"),linetype="dashed") + 
#     geom_text(aes(x=IRRF,y=IIRF_bond1_,label = country_code),hjust=-0.3,vjust=0,size=3) + theme_stata(scheme = "s1mono") + 
#     labs(title = "IRRF BP vs IRRF AG",caption = "Note: The slope of the regression is -0.18 (robust standard error of 0.36)") +
#     xlab("IRRF BP") + ylab("IRRF AG") + theme(panel.background = element_blank()) + 
#     scale_color_identity(name = "", breaks = c("darkred", "black"),
#                          labels = c("Fitted values", "1/3 Slope line (18° degrees)"),guide = "legend")
# ggsave(paste0(figures,"irrfbp_irrfag_nofit_033slope.png"))
