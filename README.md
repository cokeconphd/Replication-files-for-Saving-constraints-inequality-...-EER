# Replication-files-for-Saving-constraints-inequality-...-EER
Files to replicate the empirical results in "Saving constraints, inequality, and the credit market response to fiscal stimulus" published at the European Economic Review on January 2023
Data and Program Descriptions (README) for “Saving Constraints, Inequality, and the Credit Market Response to Fiscal Stimulus”
November 22nd, 2022
Jorge Miranda-Pinto, Daniel Murphy, Kieran James Walsh, and Eric R. Young

This README describes data and programs for “Saving Constraints, Inequality, and the Credit Market Response to Fiscal Stimulus” by Miranda-Pinto, Murphy, Walsh and Young (European Economic Review) in accordance with the Replication Policy of the European Economic Review.

Software:
-	R version 4.0.3
-	Stata/SE 16
-	Excel 2016

Data and codes for the empirical section:
Data sources:
o	panel_data_july21.xlsx – This dataset has panel data by country on government spending, government bond yields, monetary policy rate, credit of households, GDP, and other control variables, between 1959 and 2015. Data comes from Haver/IMF, OECD.

o	topanel.dta – Average country-specific variables including inequality measures, credit rating, trade openness and financial openness. 

o	g_shocks_oecd_forecast_AGAERPP13.dta – This dataset has government spending shocks by country, data from Auerbach and Gorodnichenko (2013).

o	database_cross_section_july21.xlsx – Data on income risk (from Nichols and Rehm (2014)), ratio 90th/10th percentiles, financial openness (Lane and Milesi-Ferretti (2007)), credit of households, government spending shocks (from Auerbach and Gorodnichenko (2013)), GDP per capita and other variables for each country. Sources are mainly Haver/IMF and OECD. 

o	gov_debt_gdp_1980_2010.xlsx – Total central government debt (as % of GDP) by country between 1980 and 2010. Source: OECD.Stat Metadata Viewer

o	gov_debt.dta – Median and average total central government debt (as % of GDP) of each country, constructed from “gov_debt_gdp_1980_2010.xlsx”.  

o	WIID_31MAY2021.dta – Data on inequality of each country from the World Income Inequality Database as of 31st May, 2021. It contains Gini index measure and Ratio income 80th/20th percentiles. Source: UNU-WIDER : World Income Inequality Database - WIID

o	new_ineq.dta – Dataset with the average Gini and Ratio income 80th/20th percentiles, calculated from “WIID_31MAY2021.dta”. 

o	new_ineq_toAG.xlsx – Dataset with average Gini and Ratio income 80th/20th and 90th/10th percentiles, including country names and 3 letter code. Constructed with “database_cross_section_july21.xlsx” and “new_ineq.dta”


o	iirf_oneatatime.csv – Dataset with IRRF’s by country with Auerbach and Gorodnichenko (2013) shocks generated in panel_bycountry.R 

o	prrf_oneatatime.csv - Dataset with PRRF’s from Auerbach and Gorodnichenko (2013) shocks generated in panel_bycountry.R

Codes with main results:
o	panel_interactions.R – Generates full panel regression results of the IRRF where we interact government spending shocks with different measures of inequality. It generates Tables 3, 4, A.6, A.7, A.8, A.9, A.10, A.11.

o	panel_bycountry.R – Generates local IRRF’s, PRRF’s and CRF’s by country, for Blanchard and Perotti (2002) shocks and Auerbach and Gorodnichenko (2013) shocks. It does not generate any figures or tables, but it does provide estimations used in IRRF_inequality.R.

o	IRRF_inequality.R – Generates local projection regression results and the main plots, such as a bar plot with IRRF by country, a scatter plot comparing IRRF of each country with their inequality, for Blanchard and Perotti (2002) shocks and Auerbach and Gorodnichenko (2013) shocks. “panel_bycountry.R” has to be executed before executing this script (IRRF_inequality.R). It generates Figures 1, 2, 3, A.1, A.2, A.3, A.4, Tables 1, 2, A.1, A.2, A.3, A.4, A.5.

o	panel_interactions_consumption.do – Generates full panel regression results of the CRF where we interact government spending shocks with different measures of inequality. 

o	new_ineq_data_WIID.do – Takes WIID_31MAY2021.dta and generates average Gini index and Ratio Income 80th/20th percentiles for each country. It does not generate figures or tables, it provides data used in IRRF_inequality.R, panel_interactions.R and panel_interactions_consumption.do. 

Note: To be able to run each code, the user just needs to change the variable “wd” of each script for R codes or the “global maindir” in Stata do files and create folders named “data”, “results”, “tablas_figuras”, “tables” and “tex”.	

References
Auerbach, A. and Gorodnichenko, Y. (2013). Output spillovers from fiscal policy. American Economic Review: Papers and Proceedings, 103(3).
Blanchard, O. and Perotti, R. (2002). An empirical characterization of the dynamic effects of changes in government spending and taxes on output. The Quarterly Journal of Economics, 117(4):1329.
Guvenen, F, Ozkan, S. and Song, J. (2014). The nature of countercyclical income risk. Journal of Political Economy, 122 (3), 621–660.
Lane, P. R. and Milesi-Ferretti, G. M. (2007). The external wealth of nations mark II: Revised and extended estimates of foreign assets and liabilities, 1970-2004. Journal of International Economics, 73(2):223–250.
Nichols, A. and Rehm, P. (2014). Income risk in 30 countries. Review of Income and Wealth, 60(S1):S98–S116.
