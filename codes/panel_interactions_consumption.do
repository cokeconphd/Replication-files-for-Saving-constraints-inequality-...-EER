*-------Panel data: IIRF vs country characteristics---------*
*MMWY17: Debt Burdens and the Interest Rate Response...*


clear all
macro drop _all


global maindir "C:/Users/Hriday/Dropbox/Hriday/IRRF/Replication"
*global maindir "/Users/acastilloa/Dropbox/BC/02_JorgeMP/Hriday/IRRF/adcastillo"
global data "${maindir}/data/"
global results "${maindir}/resultados/"
global figures "${maindir}/tablas_figuras/"


// Setting Responses, Sample and Shocks Variables 

local respvar "cumdlnc"  	// dependent variable
local shockvar "dlng" 			// shock variable
local interactionsvar "p90p10 p20p20 gini_wiid"	// interaction variables

// Conditions

local nlags = 4			// number of lags
local nleads = 4		// number of leads

local normalized = 1 	// 1 to use normalized variables
local errors 	 = "cluster"	// "cluster" or "robust"
local filter 	 = 2	// 1=HP; 2= BK; 3=lagged (4 quarters) RGDP; 4= no filter
local mpcontrol 	 = 1	// 1=controls for MP; 0=does not control foe


// Generate extensions
if `normalized' == 1 {
	local extension1 = "_normalized"
}
else {
	local extension1 = "_level"
}	
	
if "`errors'" == "cluster" {
	local extension2 = "_cluster"
	local vce "vce(cluster id)"		// modificar id para cambiar la clusterizacion
	
} 
else {
	local extension2 = "_robust"
local vce "vce(robust)"
}

// gen results folder
capture mkdir "${maindir}/tables/nlags_`nlags'"
capture mkdir "${maindir}/tables/nlags_`nlags'/`extension2'"
capture mkdir "${maindir}/tables/nlags_`nlags'/`extension2'/`extension1'"
global tables "${maindir}/tables/nlags_`nlags'/`extension2'/`extension1'"


/* =====================================================================
				Read data and prepare variables
===================================================================== */

import excel "${data}/panel_data_july21.xlsx", sheet("panel2_2007")  cellrange(A1:S6932) firstrow clear

set more off 

rename bis_cons_credit_gdp credit_gdp

ds Period country, not
foreach var of var `r(varlist)' {
destring `var', replace force
}

replace policy_rate=. if policy_rate==0 & year<2000
replace real_credit=. if real_credit==0
replace credit_gdp=. if credit_gdp==0
replace real_credit_cyc=. if real_credit_cyc==0
g gspread=bond_yields-policy_rate


	
/*
			Load cross country characteristics
*/

sort country
joinby country using "${data}/topanel.dta" , unmatched(both)
keep if _merge==3
clonevar p90p10 = linequality

drop if  country=="Estonia" /*same sample as in SVAR cross section*/
*drop if country=="Slovak Republic" |  country=="Slovenia" | country=="Iceland" 
drop _merge

encode country, g(count)
bysort count: g per=_n

gen time=yq(year, quarter)
egen id=group(country)
xtset id time

foreach var of varlist c_real real_credit rgdp_cyc real_credit_cyc {
	replace `var' = `var'/1000 
}

tsfilter hp rgdp_c=rgdp, smooth(1600)
tsfilter hp credit_c=real_credit, smooth(1600)
tsfilter hp credit_gdp_c=credit_gdp, smooth(1600)

g lreal_credit=log(real_credit)
g lng=log(G)
g lngdp=log(rgdp)
g lnc=log(c_real)
xtset id time

// Adjust G, C, GDP, by lagged GDP

*sum count
*forvalues i=1(1)`r(max)' {
*if count==`i'& `filter' == 1  {
if `filter' == 1  {
tsfilter hp rgdp_hp = rgdp , smooth(1600)
g trend_rgdp=rgdp-rgdp_hp
}

if `filter' == 2  {
tsfilter bk rgdp_bk = rgdp , trend(trend_rgdp)
}

if `filter' == 3  {
g trend_rgdp=L1.rgdp
}

if `filter' == 4  {
g trend_rgdp=1
}

// Normalized by HP GDP
g dlng=log((G)/trend_rgdp)
g dlnc=log((c_real)/trend_rgdp)
g dlngdp=log((rgdp)/trend_rgdp)
g dbond_yields=(bond_yields)
g dpolicy_rate =(policy_rate)


if `mpcontrol'==0 {
local controls ""
forvalues i=1(1)`nlags' {
g lngl`i'=L`i'.dlng
g lncl`i'=L`i'.dlnc
g lngdpl`i'=L`i'.dlngdp
g bond_yields_l`i'=L`i'.dbond_yields
*g policy_rate_l`i' =L`i'.dpolicy_rate
local controls "`controls'  lngl`i' lngdpl`i' bond_yields_l`i'"
}
}
else {
local controls ""
forvalues i=1(1)`nlags' {
g lngl`i'=L`i'.dlng
g lncl`i'=L`i'.dlnc
g lngdpl`i'=L`i'.dlngdp
g bond_yields_l`i'=L`i'.dbond_yields
g policy_rate_l`i' =L`i'.dpolicy_rate
local controls "`controls'  lngl`i' lngdpl`i' bond_yields_l`i' policy_rate_l`i'"
}
}



forvalues i=1(1)`nleads' {
g bond_yieldsf`i'=F`i'.dbond_yields
g lncf`i'=F`i'.dlnc
}

//UPDATE THIS for different lags
*local controls "lngl1 lngl2 lngl3 lngl4 bond_yields_l1 bond_yields_l2 bond_yields_l3 bond_yields_l4  policy_rate_l1  policy_rate_l2 policy_rate_l3 policy_rate_l4 lngdpl1 lngdpl2 lngdpl3 lngdpl4"



joinby country using "${data}/new_ineq.dta", unmatched(both) 
keep if _merge==3
drop _merge



/*
		Generate interactions or dummies for sample splitting
*/

sort count
by count : gen uniqcountry = _n == 1

// dummies for sample split

foreach var of local interactionsvar {

sum `var' if uniqcountry == 1, d
gen  dummy_`var' = 1 if `var' > `r(p50)'
replace dummy_`var' = 0 if missing(dummy_`var')
label variable dummy_`var' "dummy `var' > median"
}

bysort country: gen sumdlnc = sum(dlnc)
bysort country: gen cumdlnc = sumdlnc - sumdlnc[_n-4]	 
/* =======================================================================
					dummies for below/above median
======================================================================= */

foreach var of local interactionsvar {
	
sum `var' if uniqcountry == 1, d
gen  dummy_`var'_high = 1 if `var' >= `r(p50)'

replace dummy_`var'_high = 0 if missing(dummy_`var'_high)

gen  int_`var'_high = `shockvar'*dummy_`var'_high 
}

// generate locals
foreach var of local interactionsvar {
local int_cont_high_`var' ""
}

// generate interaction controls x inequality measures

foreach con of local controls {
foreach var of local interactionsvar {

gen  i_dummy_`con'_`var' =  `con'*dummy_`var'_high

local int_cont_high_`var' " `int_cont_high_`var''   i_dummy_`con'_`var'"

}
}


/* =======================================================================
						Interactions
======================================================================= */


foreach var of local interactionsvar {
	if (`normalized' == 1) { 
	sum `var' if uniqcount == 1, d
	tempvar temp_`var'
	gen  `temp_`var'' = (`var' - `r(mean)')/`r(sd)'
	gen int_`var' = `shockvar'*(`temp_`var'')
	label variable int_`var' "G shock x `var'"
	}
	else {
	gen int_`var' = `shockvar'*(`var')
	}
}


// generate locals
foreach var of local interactionsvar {
local int_cont_`var' ""
}

// generate interaction controls x inequality measures

local con = "lngl1"
local var = "p20p20"

foreach con of local controls {
foreach var of local interactionsvar {

	sum `var' if uniqcount == 1, d
	tempvar temp2_`var'
	gen  `temp2_`var'' = (`var' - `r(mean)')/`r(sd)'

gen  i_`con'_`var' =  `con'*`temp2_`var''

local int_cont_`var' " `int_cont_`var''   i_`con'_`var'"

}
}

sum `int_cont_p90p10'

/*
foreach var of local controls {
	if (`normalized' == 1) { 
	sum `var' if uniqcount == 1, d
	tempvar temp_`var'
	gen  `temp_`var'' = (`var' - `r(mean)')/`r(sd)'
	gen int_`var' = `shockvar'*(`temp_`var'')
	label variable int_`var' "G shock x `var'"
	}
	else {
	gen int_`var' = `shockvar'*(`var')
	}
}
*/


/* =========================================================================
							Local proyections
========================================================================= */

label variable dlng "G shock"


/*
					regressions with interactions
*/

xtset id time


foreach inter of local interactionsvar {
	
reghdfe `respvar'  `shockvar' int_`inter' `controls' `int_cont_`inter'' , a(time id) `vce'
outreg2 using "${tables}/interactions.tex",  addtext(Country FE, Yes, Year FE, Yes) ctitle(CRF) nocons nonotes stats(coef se)  bdec(3) pdec(3) tex(fragment) keep(`shockvar' int_`inter') append 
*reghdfe f2.`respvar'  `shockvar' int_`inter' `controls' `int_cont_`inter'' , a(time id) `vce'
*outreg2 using "${tables}/interactions.tex",  addtext(Country FE, Yes, Year FE, Yes) ctitle(rt) nocons nonotes stats(coef se)  bdec(3) pdec(3) tex(fragment) keep(`shockvar' int_`inter') append 
}



/*
					High and low dummies
*/


*i_lngl1_`inter'_l

foreach inter of local interactionsvar {
	
reghdfe `respvar' `shockvar' int_`inter'_high `controls' `int_cont_high_`inter'' , a(time id ) `vce'
outreg2 using "${tables}/dummies_high_low.tex",  addtext(Country FE, Yes, Year FE, Yes) ctitle(CRF) nocons nonotes stats(coef se)  bdec(3) pdec(3) tex(fragment) keep(`shockvar' int_`inter'_high) append 
}



/*
					Sample split regressions
*/


foreach var of local interactionsvar {

// regression for countries with inequality measure above the median
reghdfe `respvar'  `shockvar' `controls' if dummy_`var' == 1, a(time id) `vce'
outreg2 using "${tables}/split_`var'.tex",  addtext(Country FE, Yes, Year FE, Yes) ctitle(rt) nocons nonotes stats(coef se)  bdec(3) pdec(3) tex(fragment) keep(`shockvar') replace

// regression for countries with inequality below the median
reghdfe `respvar'  `shockvar' `controls' if dummy_`var'== 0, a(time id) `vce'
outreg2 using "${tables}/split_`var'.tex",  addtext(Country FE, Yes, Year FE, Yes) ctitle(rt) nocons nonotes stats(coef se)  bdec(3) pdec(3) tex(fragment) keep(`shockvar') append 
}


	
	
*********************************************************+
/*


outreg2 using "${tables}/local_proj_BP_panel.tex", lab  addtext(Country FE, Yes, Year FE, Yes) ctitle(rt) nocons nonotes stats(coef se)  bdec(3) pdec(3) tex(fragment) keep(dlng dlng_p20p20 ) replace



reghdfe dbond_yields  bond_yields_l*   policy_rate_l* lngdpl* dlng dlng_p20p20  lngl*  , a(time id) vce(cluster id)
outreg2 using "${tables}/local_proj_BP_panel.tex", lab  addtext(Country FE, Yes, Year FE, Yes) ctitle(rt) nocons nonotes stats(coef se)  bdec(3) pdec(3) tex(fragment) keep(dlng dlng_p20p20 ) replace

*reghdfe dbond_yields  bond_yields_l*   policy_rate_l* lngdpl* dlng  dlng_p20p20 lngl*  if ineq_above_med==0, a(time id) vce(cluster id)
*outreg2 using "local_proj_BP_panel.tex", lab  addtext(Country FE, Yes, Year FE, Yes) ctitle(rt low) nocons nonotes stats(coef se)  bdec(3) pdec(3) tex(fragment) keep(dlng dlng_p20p20) append

*reghdfe dbond_yields  bond_yields_l*   policy_rate_l* lngdpl* dlng dlng_p20p20 lngl*  if ineq_above_med==1, a(time id) vce(cluster id)
*outreg2 using "local_proj_BP_panel.tex", lab  addtext(Country FE, Yes, Year FE, Yes) ctitle(rt high) nocons nonotes stats(coef se)  bdec(3) pdec(3) tex(fragment) keep(dlng dlng_p20p20) append

reghdfe dbond_yields  bond_yields_l*   policy_rate_l* lngdpl* dlng   lngl*  if ineq_above_med==0, a(time id) vce(cluster id)
outreg2 using "${tables}/local_proj_BP_panel.tex", lab  addtext(Country FE, Yes, Year FE, Yes) ctitle(rt low ineq.) nocons nonotes stats(coef se)  bdec(3) pdec(3) tex(fragment) keep(dlng ) append

reghdfe dbond_yields  bond_yields_l*   policy_rate_l* lngdpl* dlng  lngl*  if ineq_above_med==1, a(time id) vce(cluster id)
outreg2 using "${tables}/local_proj_BP_panel.tex", lab  addtext(Country FE, Yes, Year FE, Yes) ctitle(rt high ineq.) nocons nonotes stats(coef se)  bdec(3) pdec(3) tex(fragment) keep(dlng ) append

reghdfe dbond_yields  bond_yields_l*   policy_rate_l* lngdpl* dlng  lngl*  if p20p20>5.48, a(time id) vce(cluster id)
outreg2 using "${tables}/local_proj_BP_panel.tex", lab  addtext(Country FE, Yes, Year FE, Yes) ctitle(rt very high ineq.) nocons nonotes stats(coef se)  bdec(3) pdec(3) tex(fragment) keep(dlng ) append

