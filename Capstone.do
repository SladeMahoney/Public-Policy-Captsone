*Patrick Mahoney 

* --------------------------------------------------------
* (1) do-file basics
* --------------------------------------------------------

capture log close

set more off
clear


log using "Patrick_Mahoney_Capstone.smcl", replace

cd /Users/patrickslademahoney/Desktop/Capstone
* import data

import delimited "merged.csv", clear
*save "COPYget_it_done_2018.dta", replace


rename v1 Department
rename v2 Municipality
rename v3 Mun_ID
rename v4 year
rename v5 month
rename v6 repatriations
rename v7 criminals
rename v8 homicides
rename v9 assaults
rename v10 mean_household_income
rename v11 mean_inc_percap

destring mean_household_income, force replace
destring mean_inc_percap, force replace

generate non_crims = repatriations-criminals
gen int date = ym(year, month)
format date %tm

xtset Mun_ID date
mean(homicides)
mean(assaults)
mean(non_crims)
mean(criminals)

hist homicides
clonevar homicides_w=homicides
su homicides_w, d
replace homicides_w= r(p95) if homicides_w>=r(p95) & homicides_w< .
replace homicides_w= r(p5) if homicides_w<=r(p5) 
hist homicides_w


hist assaults
clonevar assaults_w= assaults
su assaults_w, d
replace assaults_w= r(p95) if assaults_w>=r(p95) & assaults_w< .
replace assaults_w= r(p5) if assaults_w<=r(p5) 
hist assaults_w




// sort the data by municipality and time
sort year 

// preserve the data
preserve

// compute mean values by year
collapse (mean) mean_homicides = homicides mean_assaults=assaults mean_repatriations = repatriations, by(year)

twoway (line mean_homicides year) (line mean_repatriations year), xtitle("Year") ytitle("Variable values") legend(label(1 "Homicides") label(2 "Repatriations"))


// restore the data
restore
* --------------------------------------------------------
* (2) OLS Regressions
* --------------------------------------------------------

eststo d1, title("OLS Model: Effects of Repatriations on Homicides"):regress homicides non_crims criminals mean_household_income, 
quietly estadd local fixedx "No", replace
quietly estadd local fixedy "No", replace
estadd ysumm, replace
unique Municipality if e(sample)
mean non_crims if e(sample)
mean criminals if e(sample)


eststo d2, title("OLS Model: Effects of Repatriations on Assaults"): regress assaults non_crims criminals  mean_household_income 
quietly estadd local fixedx "No", replace
quietly estadd local fixedy "No", replace
estadd ysumm, replace
unique Municipality if e(sample)
mean non_crims if e(sample)
mean criminals if e(sample)

* --------------------------------------------------------
* (3) FE Regressions
* --------------------------------------------------------
eststo d11, title("Fixed Effects Model 1: Effects of Repatriations on Homicides"):xtreg homicides non_crims criminals  mean_household_income, fe vce(robust) 
quietly estadd local fixedx "Yes", replace
quietly estadd local fixedy "No", replace
estadd ysumm, replace
unique Municipality if e(sample)
mean non_crims if e(sample)
mean criminals if e(sample)

eststo d12, title("Fixed Effects Model 2: Effects of Repatriations on Homicides"):xtreg homicides non_crims criminals  mean_household_income i.year, fe 
quietly estadd local fixedx "Yes", replace
quietly estadd local fixedy "Yes", replace
estadd ysumm
unique Municipality if e(sample)
mean non_crims if e(sample)
mean criminals if e(sample)
hausman d12



** Robsutness Check by Winsorizing
eststo d111, title("Fixed Effects Model 2: Effects of Repatriations on Homicides"):xtreg homicides_w non_crims criminals  mean_household_income i.year, fe vce(robust) 
quietly estadd local fixedx "Yes", replace
quietly estadd local fixedy "Yes", replace
estadd ysumm
unique Municipality if e(sample)
mean non_crims if e(sample)
mean criminals if e(sample)

eststo d21, title("Fixed Effects Model 1: Effects of Repatriations on Assaults"):xtreg assaults non_crims criminals  mean_household_income, fe robust
quietly estadd local fixedx "Yes", replace
quietly estadd local fixedy "No", replace
estadd ysumm, replace 
unique Municipality if e(sample)
mean non_crims if e(sample)
mean criminals if e(sample)

eststo d22, title("Fixed Effects Model 2: Effects of Repatriations on Assaults"):xtreg assaults non_crims criminals mean_household_income i.year, fe 
quietly estadd local fixedx "Yes", replace
quietly estadd local fixedy "Yes", replace
estadd ysumm, replace 
unique Municipality if e(sample)
mean non_crims if e(sample)
mean criminals if e(sample)
hausman d22

** Robsutness Check by Winsorizing
eststo d111, title("Fixed Effects Model 2: Effects of Repatriations on Homicides"):xtreg assaults_w non_crims criminals  mean_household_income i.year, fe vce(robust) 
quietly estadd local fixedx "Yes", replace
quietly estadd local fixedy "Yes", replace
estadd ysumm
unique Municipality if e(sample)
mean non_crims if e(sample)
mean criminals if e(sample)

* --------------------------------------------------------
* (4) Lagged Regressions
* --------------------------------------------------------

* Homicides
eststo d13, title("Lagged Model 1: Effects of Repatriations on Homicides"):xtreg homicides L1.non_crims L2.non_crims L3.non_crims L.criminals L2.criminals L3.criminals mean_household_income i.year, fe 
quietly estadd local fixedx "Yes", replace
quietly estadd local fixedy "Yes", replace
estadd ysumm, replace 
unique Municipality if e(sample)
mean non_crims if e(sample)
mean criminals if e(sample)


* Assaults
eststo d23, title("Lagged with Fixed Effects Model 1: Effects of Repatriations on Assaults"):xtreg assaults L1.non_crims L2.non_crims L3.non_crims L.criminals L2.criminals L3.criminals mean_household_income i.year, fe 
quietly estadd local fixedx "Yes", replace
quietly estadd local fixedy "Yes", replace
estadd ysumm, replace 
unique Municipality if e(sample)
mean non_crims if e(sample)
mean criminals if e(sample)

#delimit; 
esttab d1 d11 d12 using "Regression_Table_1.rtf", replace se label s(N N  ymean ysd xmean xmean fixdex fixedy , label("Observations" "Number of Municipalities"  "Mean Homicides per Month" "SD Homicides per Month" "Mean Repatriations per Month" "Mean Percent of Repatriations that are Criminals" "Municipality FE" "Year FE" )) onecell compress nogaps ar2 star nolines noeqlines rtf mtitles


esttab d2 d21 d22 using "Regression_Table_2.rtf", replace se label s(N N  ymean ysd xmean xmean fixdex fixedy , label("Observations" "Number of Municipalities"  "Mean Assaults per Month" "SD Assaults per Month" "Mean Repatriations per Month" "Mean Percent of Repatriations that are Criminals" "Municipality FE" "Year FE" )) onecell compress nogaps ar2 star nolines noeqlines rtf mtitles 

esttab d23 d24 d25 using "Regression_Table_21.rtf", replace se label onecell compress nogaps ar2 star rtf mtitles 


* Tests for Robustness
import delimited "merged2.csv", clear 

rename v1 Department
rename v2 Municipality
rename v3 Mun_ID
rename v4 year
rename v5 month
rename v6 repatriations
rename v7 criminals
rename v8 homicides
rename v9 rural_homs
rename v10 urban_homs
rename v11 assaults
rename v12 rural_assaults
rename v13 urban_assaults
rename v14 mean_hh_income
rename v15 mean_inc_percap

destring mean_hh_income, force replace
destring mean_inc_percap, force replace
destring rural_homs, force replace
destring rural_assaults, force replace
destring urban_assaults, force replace
destring urban_homs, force replace

generate non_crims = repatriations-criminals
gen int date = ym(year, month)
format date %tm

xtset Mun_ID date
mean(homicides)
mean(assaults)
mean(non_crims)
mean(criminals)
mean(rural_assaults)
mean(urban_assaults)
mean(rural_homs)
mean(urban_homs)


collapse (mean) rural_homs urban_homs rural_assaults urban_assaults mean_hh_income (sum) homicides assaults non_crims criminals, by(year Mun_ID)

eststo d3, title("Rural vs Urban Homicides"):xtreg rural_homs non_crims criminals mean_hh_income i.year, fe robust 
quietly estadd local fixedx "Yes", replace
quietly estadd local fixedy "Yes", replace
estadd ysumm, replace 
unique Mun_ID if e(sample)
mean non_crims if e(sample)
mean criminals if e(sample)

eststo d31, title("Rural vs Urban Homicides"):xtreg urban_homs non_crims criminals mean_hh_income i.year, fe robust 
quietly estadd local fixedx "Yes", replace
quietly estadd local fixedy "Yes", replace
estadd ysumm, replace 
unique Mun_ID if e(sample)
mean non_crims if e(sample)
mean criminals if e(sample)

eststo d4, title("Rural vs Urban Assaults"):xtreg rural_assaults non_crims criminals mean_hh_income i.year, fe robust 
quietly estadd local fixedx "Yes", replace
quietly estadd local fixedy "Yes", replace
estadd ysumm, replace 
unique Mun_ID if e(sample)
mean non_crims if e(sample)
mean criminals if e(sample)

eststo d4, title("Rural vs Urban Assaults"):xtreg urban_assaults non_crims criminals mean_hh_income i.year, fe robust 
quietly estadd local fixedx "Yes", replace
quietly estadd local fixedy "Yes", replace
estadd ysumm, replace 
unique Mun_ID if e(sample)
mean non_crims if e(sample)
mean criminals if e(sample)

clear
