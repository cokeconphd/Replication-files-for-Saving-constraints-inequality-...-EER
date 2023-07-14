use "C:\Users\Hriday\Dropbox\Hriday\IRRF\Replication\data\WIID_31MAY2021.dta" , clear

keep if oecd==1
keep if year>=1990 & year<=2019

****Keep Luxemburg data for all those that have it and use OECD for those that do not

/*
*keep if source==1 | (country=="Japan" &source==6)  |(country=="New Zealand"&source==5)|(country=="Portugal"&source==2) | (country=="Korea, Republic of" & source==4)
*/

*******Eurostat-national statistic authority-UN-or next one with more obs

encode source_detailed, g(source_detailed2)
		
keep if (source==2 & source_detailed=="Eurostat microdata")| (country=="Australia"&source==5) |(country=="New Zealand"&source==5) | (country=="Canada"&source==6) | (country=="Korea, Republic of" &source==4)|(country=="Japan" &source==6) |(country=="United States" &source==6)

keep if resource==1
keep if scale==2

drop if country=="Chile" | country=="Israel" | country=="Colombia"|country=="Estonia"| country=="Greenland"| country=="Latvia"| country=="Lithuania"| country=="Luxembourg"| country=="Mexico"| country=="Turkey"


encode country, g(count)
encode currency, g(currency2)

bysort count source source_detailed2 currency: egen av_gini=mean(gini)
bysort count source source_detailed2 currency: egen av_p80p20=mean(ratio_top20bottom20)

*keep if source==1
*bysort count source : g obs0=_N

bysort count source currency: g obs=_N

bysort count : egen max_obs=max(obs)


keep if obs==max_obs | (country=="Australia" )
drop if country=="Australia" &currency2!=1 & source_detailed2!=8
drop if country=="Australia" & source_detailed2!=8


keep country source source_detailed2  currency  av_gini av_p80p20
duplicates drop

rename av_gini  gini_wiid
rename av_p80p20 p20p20


/*
preserve
forvalues i=1(1)28 {
sum p20p20  if count==`i'
g   sd_p20p20_`i'=r(sd) 
sum gini if count==`i'
g   sd_gini_`i'=r(sd) 
}
keep count sd_p20p20_* sd_gini_*
drop count
duplicates drop
g count=1
 reshape long sd_p20p20_ sd_gini_, i(count) j(id)
sum sd_p20p20_ sd_gini_, d

/*    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
  sd_p20p20_ |         28    1.414044    .8074438   .9216037   5.122139
    sd_gini_ |         28    5.030937    1.388121   2.653835    9.17574

*/

restore
*/


*collapse (mean) gini_wiid p20p20 palma, by(country)

sort country

summarize p20p20 gini_wiid

replace country="Czech Republic" if country=="Czechia"
replace country="Korea" if country=="Korea, Republic of"
replace country="Slovak Republic"  if country=="Slovakia"


save  "C:\Users\Hriday\Dropbox\Hriday\IRRF\Replication\new_ineq.dta", replace

