clear all
cd "P:\IHWAP\"
set more off, perm

use "./Temp Data/clean_state_utility.dta"

keep jobnumber fueltype meterstart meterend meterdays mmbtu ///
		HDD60 HDD65 CDD75 utility estimated

sort jobnumber meterend fueltype
gen hasgas = 1 if fueltype=="gas"
gen haselectric = 1 if fueltype=="electric"
by jobnumber : egen gas_data = max(hasgas)
by jobnumber : egen elec_data = max(haselectric)

* differnce between billing cycles for gas and electric data
by jobnumber : gen cyclediff = meterend - meterend[_n+1] if fueltype=="electric" & fueltype[_n+1]=="gas"
replace cyclediff =abs(cyclediff)
/* for Ameren, bill cycle differences are small, but for other utilities they are significant */

* Adding gas and electric, only for homes that are in same bill cycle
sort jobnumber meterend fueltype
by jobnumber : gen total_mmbtu = mmbtu + mmbtu[_n+1] if fueltype=="electric" & fueltype[_n+1]=="gas" & cyclediff<=7
order total_mmbtu, after(mmbtu)

* separate electric and gas data
gen electric_mmbtu = mmbtu if fueltype=="electric" & total_mmbtu!=.
gen gas_mmbtu = total_mmbtu - electric_mmbtu

* shares of gas and electricity usage
gen electric_share = mmbtu/total_mmbtu
gen gas_share = 1-electric_share

* transforming some long variables to wide, so we can collapse data
gen utility_gas = utility if fueltype=="gas"
gen utility_electric = utility if fueltype=="electric"
sort jobnumber utility_gas
by jobnumber : replace utility_gas = utility_gas[_N]

gen is_estimated_gas = estimated if fueltype=="gas"
gen is_estimated_electric = estimated if fueltype=="electric"
sort jobnumber meterend fueltype
by jobnumber : replace is_estimated_gas = is_estimated_gas[_n+1] if fueltype=="electric" & total_mmbtu!=.

replace meterdays = meterend-meterstart if meterdays==.

* removing duplicated information
drop utility fueltype mmbtu estimated hasgas haselectric gas_data elec_data
keep if total_mmbtu!=.
duplicates drop

* Finally merge with State data
merge m:1 jobnumber using "./Temp Data/IHWAP_State.dta"
keep if _merge==3
drop _merge

* some auxiliary variables
gen start_day = day(meterstart)
gen start_month = month(meterstart)
gen start_year = year(meterstart)
gen end_day = day(meterend)
gen end_month = month(meterend)
gen end_year = year(meterend)

gen audit_day = day(AuditDate)
gen audit_month = month(AuditDate)
gen audit_year = year(AuditDate)

sort Household meterend
by Household : gen treated = 0 if meterend < AuditDate
by Household : replace treated = 1 if meterstart > FinalDate+5 & meterstart!=.

******** Import data about contractor fixed effects
tempfile gaselec
save `gaselec'

clear all
use "./Temp Data/IHWAP_Measures4.dta"

keep jobnumber Contractor
duplicates drop

egen ContractorID = group(Contractor)

sort jobnumber
by jobnumber : gen obs = _n

drop if ContractorID==.

forval i = 1/7 {
gen contractor`i' = ContractorID if obs==`i'
}

forval i = 1/7 {
by jobnumber : egen ContractorID`i' = max(contractor`i')
}

forval i = 1/7 {
replace ContractorID`i' = 0 if ContractorID`i'==.
}

keep jobnumber ContractorID*
drop ContractorID
duplicates drop

merge 1:m jobnumber using `gaselec'
keep if _merge==3
drop _merge

* Save point here before merging with weather data
save "./Temp Data/IHWAP_gaselec.dta", replace

ssc install todate, replace
*** merging with more weather data
clear all
import delimited "./Machine Learning/Weather/ihwap_precip.csv", varnames(1) case(preserve)
drop v1
rename meterstart metstart
replace metstart = subinstr(metstart, "-", "", .)
drop if metstart=="NA"
todate metstart, pattern(yyyymmdd) format(%d) gen(meterstart)
drop metstart
replace precip = "" if precip=="NA"
destring precip, replace
merge m:m jobnumber meterstart using "./Temp Data/IHWAP_gaselec.dta", nogen keep(1 2 3)

tempfile ihwapmerge
save `ihwapmerge'

clear all
import delimited "./Machine Learning/Weather/ihwap_tmin.csv", varnames(1) case(preserve)
drop v1
rename meterstart metstart
replace metstart = subinstr(metstart, "-", "", .)
drop if metstart=="NA"
todate metstart, pattern(yyyymmdd) format(%d) gen(meterstart)
drop metstart
replace tmin = "" if tmin=="NA"
destring tmin, replace
merge m:m jobnumber meterstart using `ihwapmerge', nogen keep(1 2 3)

tempfile ihwapmerge
save `ihwapmerge'

clear all
import delimited "./Machine Learning/Weather/ihwap_tmax.csv", varnames(1) case(preserve)
drop v1
rename meterstart metstart
replace metstart = subinstr(metstart, "-", "", .)
drop if metstart=="NA"
todate metstart, pattern(yyyymmdd) format(%d) gen(meterstart)
drop metstart
replace tmax = "" if tmax=="NA"
destring tmax, replace
merge m:m jobnumber meterstart using `ihwapmerge', nogen keep(1 2 3)

* Save this data to be used for Machine Learning
save "./Temp Data/ihwap_merged.dta", replace


/*

RUN MACHINE LEARNING CODE AND GET COUNTERFACTUAL PREDICTIONS OF USAGE PRE TREATMENT:

preprocess_ML.R


*/


********************************************************************************
********************************************************************************
********************************************************************************
********* AVERAGE TREATMENT EFFECTS ON THE TREATED (ATT) ***********************

clear all
cd "P:\IHWAP\"

** Importing results from Machine Learning 
import delimited ".\Machine Learning\Model Outputs\CV_predictpre_best.csv", varnames(1) case(preserve)

gen meterend = mdy(end_month, end_day, end_year)
format meterend %d

drop end_day end_month end_year

**** prediction errors
** cross validated errors for
foreach i in 1 {
	gen cvpreds`i' = .
	forval j = 1/5 {
		replace cvpreds`i' = pred`i'_`j' if kfold2==`j'
	}
	gen cvresids`i' = total_mmbtu - cvpreds`i'
	gen cverrors`i' = 100*cvresids`i'/total_mmbtu
}

** in sample errors
foreach i in 1  {
	forval j = 1/5 {
		replace pred`i'_`j' = . if kfold2==`j'
	}
	egen inpreds`i' = rowmean(pred`i'_1 - pred`i'_5)
	gen inresids`i' = total_mmbtu - inpreds`i'
	gen inerrors`i' = 100*inresids`i'/total_mmbtu
}

gen inresids_full = total_mmbtu - pred_full
gen inerrors_full = 100*inresids_full/total_mmbtu

keep Household meterend kfold-inerrors_full

tempfile mlpredictions
save `mlpredictions'

/*
** Importing results from bootstrapping
forval i = 1/50 {
	clear
	import delimited "P:\IHWAP\Machine Learning\Model Outputs\bootstrap_predictions`i'.csv", varnames(1) case(preserve)

	gen meterend = mdy(end_month, end_day, end_year)
	format meterend %d

	drop v1 end_day end_month end_year total_mmbtu
	rename predict_mmbtu bootstrap_model`i'
	
	duplicates tag, gen(tagdups)
	gen weight`i' = tagdups+1
	drop tagdups
	duplicates drop

	merge 1:1 Household meterend using `mlpredictions', nogen
	tempfile mlpredictions
	save `mlpredictions'
}
save "bootstrap_predictions1.dta", replace


clear all
cd "P:\IHWAP\Temp Data"

** Importing results from Machine Learning 

import delimited "P:\IHWAP\Machine Learning\Model Outputs\ml_predictions_pre.csv", varnames(1) case(preserve)

gen meterend = mdy(end_month, end_day, end_year)
format meterend %d

drop v1 end_day end_month end_year

tempfile mlpredictions
save `mlpredictions'

forval i = 51/100 {
	clear
	import delimited "P:\IHWAP\Machine Learning\Model Outputs\bootstrap_predictions`i'.csv", varnames(1) case(preserve)

	gen meterend = mdy(end_month, end_day, end_year)
	format meterend %d

	drop v1 end_day end_month end_year total_mmbtu
	rename predict_mmbtu bootstrap_model`i'
	
	duplicates tag, gen(tagdups)
	gen weight`i' = tagdups+1
	drop tagdups
	duplicates drop

	merge 1:1 Household meterend using `mlpredictions', nogen
	tempfile mlpredictions
	save `mlpredictions'
}
save "bootstrap_predictions2.dta", replace

clear all
cd "P:\IHWAP\Temp Data"

** Importing results from Machine Learning 
import delimited "P:\IHWAP\Machine Learning\Model Outputs\ml_predictions_pre.csv", varnames(1) case(preserve)

gen meterend = mdy(end_month, end_day, end_year)
format meterend %d

drop v1 end_day end_month end_year

tempfile mlpredictions
save `mlpredictions'

forval i = 101/150 {
	clear
	import delimited "P:\IHWAP\Machine Learning\Model Outputs\bootstrap_predictions`i'.csv", varnames(1) case(preserve)

	gen meterend = mdy(end_month, end_day, end_year)
	format meterend %d

	drop v1 end_day end_month end_year total_mmbtu
	rename predict_mmbtu bootstrap_model`i'
	
	duplicates tag, gen(tagdups)
	gen weight`i' = tagdups+1
	drop tagdups
	duplicates drop

	merge 1:1 Household meterend using `mlpredictions', nogen
	tempfile mlpredictions
	save `mlpredictions'
}
save "bootstrap_predictions3.dta", replace

**** finally combine all bootstrap and ML predictions with original IHWAP data
clear all
cd "P:\IHWAP\Temp Data"

use "bootstrap_predictions1.dta"
merge 1:1 Household meterend using "bootstrap_predictions2.dta", nogen
merge 1:1 Household meterend using "bootstrap_predictions3.dta", nogen
merge 1:1 Household meterend using "ihwap_merged.dta", nogen

*/

merge 1:1 Household meterend using ".\Temp Data\ihwap_merged.dta", nogen

save "./Temp Data/newIHWAP_ml.dta", replace


********************************************************************************
** log transform energy usage variables
forval i = 1/150 {
gen log_bootstrap_model`i' = log(bootstrap_model`i')
}

gen log_total_mmbtu = log(total_mmbtu)
gen log_electric_mmbtu = log(electric_mmbtu)
/* gas usage alone might have a lot of informative zeros (during summer)
, so we add 0.1 to gas usage before taking the log */
gen log_gas_mmbtu = log(gas_mmbtu+0.1)

** some auxiliary variables
gen aux1 = AuditDate - meterend
gen aux2 = meterend - FinalDate
gen twoyears_prepost=1 if aux1>0 & aux1<=730
replace twoyears_prepost=1 if aux2>0 & aux2<=730


* normalizing months since treatment
gen mst = ""
forval i = 30(30)360 {
local ival = `i'/30
replace mst = "`ival'" if aux2>=`i' & aux2<`i' + 30
}
forval i = 0(30)360 {
local ival = `i'/30 + 1
replace mst = "-`ival'" if aux1>=`i' & aux1<`i' + 30
}
drop aux1 aux2

gen counter = 1
gen months_since_treat = .
forval i = -12(1)-1 {
replace months_since_treat = counter if mst=="`i'"
replace counter = counter+1
}
forval i = 1(1)12 {
replace months_since_treat = counter if mst=="`i'"
replace counter = counter+1
}
labmask months_since_treat, values(mst)

* restrict sample to monthly observations
gen meterdays_monthly = meterdays
replace meterdays_monthly = . if meterdays<25 | meterdays>36

label define treat 0 "Not treated" 1 "WAP Treatment"
label values treated treat

**** machine learning savings
gen tau_ml = total_mmbtu - ml_predictions
ssc install winsor2
winsor2 tau_ml, cuts(0.5 99.5) trim replace

forval i = 1/150 {
gen tau`i' = total_mmbtu - bootstrap_model`i'
winsor2 tau`i', cuts(0.5 99.5) trim replace
}

* in logs
gen pct_tau_ml = (total_mmbtu - ml_predictions)/ml_predictions
winsor2 pct_tau_ml, cuts(0.5 99.5) trim replace

forval i = 1/150 {
gen pct_tau`i' = (total_mmbtu - bootstrap_model`i')/bootstrap_model`i'
winsor2 pct_tau`i', cuts(0.5 99.5) trim replace
}

** performance metrics
* in sample RMSE
gen tau_ml_sq = tau_ml^2
sum tau_ml_sq if treated==0 & twoyears_prepost==1 & meterdays_monthly!=.
sca mean_sq_err = `r(mean)'
di "MSE = " mean_sq_err
sca rmse = sqrt(mean_sq_err)
di "RMSE = " rmse

* simple errors
sum tau_ml if treated==0 & twoyears_prepost==1 & meterdays_monthly!=., detail
sum pct_tau_ml if treated==0 & twoyears_prepost==1 & meterdays_monthly!=., detail


save "IHWAP_ml.dta", replace



********************************************************************************
********************* ML MODEL PERFORMANCE *************************************
clear all
cd "P:\IHWAP\Temp Data"
use "IHWAP_ml.dta"

** binning the mmbtu outcome variable
gen bin_mmbtu = .
forval i = 0(2)38 {
replace bin_mmbtu = `i' if total_mmbtu>`i' & total_mmbtu<=`i'+2
label define mmbtulabs `i' "(`i' `=scalar(`i')+2']", add
}
replace bin_mmbtu = 40 if total_mmbtu>40 & total_mmbtu!=.
label define mmbtulabs 40 ">40", add
label values bin_mmbtu mmbtulabs

* generate residuals and percent errors
gen resids = total_mmbtu - ml_predictions
gen pct_error = 100*(total_mmbtu - ml_predictions)/total_mmbtu

* regressions correlating model performance with the outcome
reg resids ibn.bin_mmbtu if treated==0 & twoyears_prepost==1 & meterdays_monthly!=., nocons vce(cluster Household)
est sto residsfull

reg pct_error ibn.bin_mmbtu if treated==0 & twoyears_prepost==1 & meterdays_monthly!=. & bin_mmbtu>0, nocons vce(cluster Household)
est sto errorsfull

** plotting performance
coefplot residsfull, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Residuals (MMBtu)") ///
		xtitle("Monthly Energy Usage (MMBtu)") ///
		graphregion(color(white)) bgcolor(white)
graph export "./Regression Outputs/Heterogeneity/mlresids.png", replace width(5000)

coefplot errorsfull, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Monthly Energy Usage (MMBtu)") ///
		graphregion(color(white)) bgcolor(white)
graph export "./Regression Outputs/Heterogeneity/mlerrors.png", replace width(5000)

gen histmmbtu = total_mmbtu
replace histmmbtu=41 if total_mmbtu>40 & total_mmbtu!=.

histogram histmmbtu if treated==0 & twoyears_prepost==1 & meterdays_monthly!=., ///
	start(0) width(2) percent graphregion(color(white)) ///
	bgcolor(white) bcolor(blue%30) xlabel(0(2)40) ///
	xtitle("Monthly Energy Usage (MMBtu)") ytitle("Percent of Sample")
graph export "./Regression Outputs/Heterogeneity/usage_histogram.png", replace width(5000)


twoway (histogram histmmbtu if treated==0 & twoyears_prepost==1 & meterdays_monthly!=., ///
	start(0) width(2) percent graphregion(color(white)) ///
	bgcolor(white) bcolor(blue%30) xlabel(0(2)40) ///
	xtitle("Monthly Energy Usage (MMBtu)") ytitle("Percent of Sample")) ///
	(histogram histmmbtu if treated==1 & twoyears_prepost==1 & meterdays_monthly!=., ///
	start(0) width(2) percent graphregion(color(white)) ///
	bgcolor(white) bcolor(red%30) xlabel(0(2)40) ///
	xtitle("Monthly Energy Usage (MMBtu)") ytitle("Percent of Sample")), ///
	legend(order(1 "Pre-WAP" 2 "Post-WAP"))
graph export "./Regression Outputs/Heterogeneity/usage_histogram_septreat.png", replace width(5000)


*** evolution of stadard deviation of bootstrap iterations
sum pct_tau1 if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
sca effect = `r(mean)'
matrix avgs = (effect)
matrix sds = (1, 0)
mat rownames sds="1"

forval i = 2/150 {
sum pct_tau`i' if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
sca effect = `r(mean)'
matrix avgs = (avgs \ effect)
mata : st_matrix("stddev", sqrt(variance(st_matrix("avgs"))))
matrix iter = (`i', stddev)
mat rownames iter="`i'"
matrix sds = (sds \ iter)
}

coefplot matrix(sds[.,2]), vertical xlabel(0(10)150) ///
	graphregion(color(white)) bgcolor(white) xtitle("Iteration") ///
	ytitle("Standard Deviation")
graph export "./Regression Outputs/Heterogeneity/bootstrap_SD.png", replace width(5000)
	




********************************************************************************
************************ REGRESSIONS *******************************************
clear all
cd "P:\IHWAP\Temp Data"
use "IHWAP_ml.dta"

********************************************************************************
** regressions for average treatment effects on treated - standard econometrics
reghdfe log_total_mmbtu i.treated c.HDD60 c.HDD60#c.HDD60 c.CDD75 c.CDD75#c.CDD75 c.precip c.tmax c.tmin if twoyears_prepost==1 & meterdays_monthly!=., absorb(i.Household i.month_year) vce(cluster Household end_month)
nlcom exp(_b[1.treated])-1, post
est sto att_simple
est save ".\Regression Outputs\Heterogeneity\att_simple", replace
reghdfe log_total_mmbtu i.treated c.HDD60 c.HDD60#c.HDD60 c.CDD75 c.CDD75#c.CDD75 c.precip c.tmax c.tmin if twoyears_prepost==1 & meterdays_monthly!=., absorb(i.Household#i.end_month i.month_year) vce(cluster Household end_month)
nlcom exp(_b[1.treated])-1, post
est sto att_hhmonth
est save ".\Regression Outputs\Heterogeneity\att_hhmonth", replace
reghdfe log_total_mmbtu i.treated c.HDD60 c.HDD60#c.HDD60 c.CDD75 c.CDD75#c.CDD75 c.precip c.tmax c.tmin if twoyears_prepost==1 & meterdays_monthly!=., absorb(i.Household#i.end_month i.month_year#i.CountyID) vce(cluster Household end_month)
nlcom exp(_b[1.treated])-1, post
est sto att_hhmonthcounty
est save ".\Regression Outputs\Heterogeneity\att_hhmonthcounty", replace
gen regsample = e(sample)

* regressions in levels
reghdfe total_mmbtu i.treated c.HDD60 c.HDD60#c.HDD60 c.CDD75 c.CDD75#c.CDD75 c.precip c.tmax c.tmin if twoyears_prepost==1 & meterdays_monthly!=., absorb(i.Household i.month_year) vce(cluster Household end_month)
est sto att_simple_levels
est save ".\Regression Outputs\Heterogeneity\att_simple_levels", replace
reghdfe total_mmbtu i.treated c.HDD60 c.HDD60#c.HDD60 c.CDD75 c.CDD75#c.CDD75 c.precip c.tmax c.tmin if twoyears_prepost==1 & meterdays_monthly!=., absorb(i.Household#i.end_month i.month_year) vce(cluster Household end_month)
est sto att_hhmonth_levels
est save ".\Regression Outputs\Heterogeneity\att_hhmonth_levels", replace
reghdfe total_mmbtu i.treated c.HDD60 c.HDD60#c.HDD60 c.CDD75 c.CDD75#c.CDD75 c.precip c.tmax c.tmin if twoyears_prepost==1 & meterdays_monthly!=., absorb(i.Household#i.end_month i.month_year#i.CountyID) vce(cluster Household end_month)
est sto att_hhmonthcounty_levels
est save ".\Regression Outputs\Heterogeneity\att_hhmonthcounty_levels", replace

* checking parallel trends 
reghdfe log_total_mmbtu ib12.months_since_treat c.HDD60 c.HDD60#c.HDD60 c.CDD75 c.CDD75#c.CDD75 c.precip c.tmax c.tmin if twoyears_prepost==1 & meterdays_monthly!=., absorb(i.Household#i.end_month i.month_year#i.CountyID) vce(cluster Household end_month)
est sto att_eventstudy
est save ".\Regression Outputs\Heterogeneity\att_eventstudy", replace

coefplot (att_eventstudy), keep(*months_since_treat) baselevels  ///
			vertical label ytitle("Estimated Energy Savings") ///
			xtitle("Months From Treatment (Weatherization)") xline(12.5) ///
			xlabel(, labsize(small)) yline(0) ///
			graphregion(color(white)) bgcolor(white)
graph export "./Regression Outputs/Heterogeneity/ATT_paralleltrends.png", replace



************************** ML average treatment effects
** program that produces nice estimation table of bootstrap results - simple point estimates
prog def bootstrapres, eclass
	args b V N /* input coef matrix, variance matrix, and number of obs */
	mat colnames `b'="treated"
	mat colnames `V'="treated"
	mat rownames `V'="treated"
	scalar obs = `N'
	ereturn post `b' `V', obs(`=scalar(obs)')
	ereturn local cmd "bootstrapres"
	ereturn local properties "b V"
	ereturn display
end

* ml pct savings
sum pct_tau_ml if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
matrix ml_ate = (`r(mean)')
sca nobs = `r(N)'

* bootstrapped SEs for ml pct savings
matrix input ml_ates = (.)
forval i = 1/150 {
sum pct_tau`i' [fweight = weight`i'] if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
sca avgsaved`i' = `r(mean)'
matrix ml_ates = (ml_ates \ avgsaved`i')
}
mata : st_matrix("ml_ate_sd", sqrt(variance(st_matrix("ml_ates"))))
mata : st_matrix("ml_ate_var", variance(st_matrix("ml_ates")))
bootstrapres ml_ate ml_ate_var nobs
est save ".\Regression Outputs\Heterogeneity\ml_simple", replace
est store ml_simple

* ml savings in levels
sum tau_ml if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
matrix ml_ate = (`r(mean)')
sca nobs = `r(N)'

* bootstrapped SEs for ml savings in levels
matrix input ml_ates = (.)
forval i = 1/150 {
sum tau`i' [fweight = weight`i'] if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
sca avgsaved`i' = `r(mean)'
matrix ml_ates = (ml_ates \ avgsaved`i')
}
mata : st_matrix("ml_ate_sd", sqrt(variance(st_matrix("ml_ates"))))
mata : st_matrix("ml_ate_var", variance(st_matrix("ml_ates")))
bootstrapres ml_ate ml_ate_var nobs
est save ".\Regression Outputs\Heterogeneity\ml_simple_levels", replace
est store ml_simple_levels

**** "Event study" graphs for ML method
** program that produces nice estimation table of bootstrap results - matrix labels already predefined
prog def bootstraplabelled, eclass
	args b V N /* input coef matrix, variance matrix, and number of obs */
	ereturn post `b' `V', obs(`=scalar(`N')')
	ereturn local cmd "bootstrapres"
	ereturn local properties "b V"
	ereturn display
end

reg pct_tau_ml ibn.months_since_treat if twoyears_prepost==1 & meterdays_monthly!=. , nocons
matrix ml_ate_mst = e(b)
scalar nobs = e(N)
matrix input ml_mst = (., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., .,)
forval i = 1/150 {
reg pct_tau`i' ibn.months_since_treat [fweight = weight`i'] if twoyears_prepost==1 & meterdays_monthly!=. , nocons
matrix tempmat = e(b)
matrix ml_mst = (ml_mst \ tempmat)
}
ssc install moremata
mata : st_matrix("ml_mst_var", mm_colvar(st_matrix("ml_mst")))
matrix ml_mst_var = diag(ml_mst_var)
local colnames : colnames ml_ate_mst
mat colnames ml_mst_var=`colnames'
mat rownames ml_mst_var=`colnames'

bootstraplabelled ml_ate_mst ml_mst_var nobs
est save ".\Regression Outputs\Heterogeneity\ml_eventstudy", replace
est store ml_eventstudy

coefplot ml_eventstudy, vertical xtitle("Months From Treatment (Weatherization)") ytitle("Model Prediction Errors (MMBtu)") ///
						yline(0) xline(12.5) graphregion(color(white)) bgcolor(white) ///
						xlabel(, labsize(small)) 
graph export "./Regression Outputs/Heterogeneity/ML_paralleltrends.png", replace

**** comparing true usage versus predicted usage
reg total_mmbtu ibn.months_since_treat if twoyears_prepost==1 , vce(cluster Household) nocons
est sto totaluse

reg ml_predictions ibn.months_since_treat if twoyears_prepost==1 & meterdays_monthly!=. , nocons
matrix ml_ate_mst = e(b)
scalar nobs = e(N)
matrix input ml_mst = (., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., .,)
forval i = 1/150 {
reg bootstrap_model`i' ibn.months_since_treat [fweight = weight`i'] if twoyears_prepost==1 & meterdays_monthly!=. , nocons
matrix tempmat = e(b)
matrix ml_mst = (ml_mst \ tempmat)
}
mata : st_matrix("ml_mst_var", mm_colvar(st_matrix("ml_mst")))
matrix ml_mst_var = diag(ml_mst_var)
local colnames : colnames ml_ate_mst
mat colnames ml_mst_var=`colnames'
mat rownames ml_mst_var=`colnames'

bootstraplabelled ml_ate_mst ml_mst_var nobs
est save ".\Regression Outputs\Heterogeneity\ml_predictuse", replace
est store predictuse

coefplot (totaluse, msymbol(Th)) predictuse, vertical xtitle("Months From Treatment (Weatherization)") ytitle("Energy Usage (MMBtu)") ///
						xline(12.5) graphregion(color(white)) bgcolor(white) ///
						xlabel(, labsize(small)) legend(order(2 "Realized" 4 "Predicted (ML)"))
graph export "./Regression Outputs/Heterogeneity/ML_trueVSpredict.png", replace

*** difference between true usage and prediction for each home

reg tau_ml ibn.months_since_treat if twoyears_prepost==1 & meterdays_monthly!=. & Household==28787, nocons
matrix ml_ate_mst = e(b)
scalar nobs = e(N)
*matrix input ml_mst = (., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., .,)
matrix input ml_mst = (., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., .)
forval i = 1/150 {
capture reg tau`i' ibn.months_since_treat if twoyears_prepost==1 & meterdays_monthly!=. & Household==20317, nocons
capture matrix tempmat = e(b)
capture matrix ml_mst = (ml_mst \ tempmat)
}
mata : st_matrix("ml_mst_var", mm_colvar(st_matrix("ml_mst")))
matrix ml_mst_var = diag(ml_mst_var)
local colnames : colnames ml_ate_mst
mat colnames ml_mst_var=`colnames'
mat rownames ml_mst_var=`colnames'

bootstraplabelled ml_ate_mst ml_mst_var nobs
est store predictuse

coefplot predictuse, vertical xtitle("Months From Treatment (Weatherization)") ytitle("Energy Usage (MMBtu)") ///
						graphregion(color(white)) bgcolor(white) ///
						xlabel(, labsize(small)) legend(order(2 "Realized" 4 "Predicted (ML)"))


********** projected (engineering) energy savings
* Separating usage pre and post WAP, and creating artificial treatment variable
replace regsample = 2 if regsample==0
sort jobnumber regsample
by jobnumber : gen job_obs = _n
gen usage = ExistingConsumption/1000000 if job_obs==1 & regsample==1
replace usage = ProjectedConsumption/1000000 if job_obs==2 & regsample==1
replace usage = usage/12 /* transforming yearly to monthly */
winsor2 usage, cuts(0.5 99.5) trim replace
gen log_usage = log(usage)
gen ww_treat = 0 if job_obs==1 & regsample==1
replace ww_treat = 1 if job_obs==2 & regsample==1
label values ww_treat treat

* regression for projected energy savings - logs
reghdfe log_usage i.ww_treat , absorb(i.Household) vce(cluster Household)
nlcom exp(_b[1.ww_treat])-1, post
est sto projected_simple
est save ".\Regression Outputs\Heterogeneity\projected_simple", replace

* regression for projected energy savings - levels
reghdfe usage i.ww_treat , absorb(i.Household) vce(cluster Household)
est sto projected_simple_levels
est save ".\Regression Outputs\Heterogeneity\projected_simple_levels", replace

/* all ATT in same table
foreach x in simple hhmonth hhmonthcounty {
est use ".\Regression Outputs\Heterogeneity\pctatt_`x'"
est sto att_`x'
est use ".\Regression Outputs\Heterogeneity\att_`x'_levels"
est sto att_`x'_levels
}
est use ".\Regression Outputs\Heterogeneity\ml_simple"
est sto ml_simple
est use ".\Regression Outputs\Heterogeneity\ml_simple_levels"
est sto ml_simple_levels
est use ".\Regression Outputs\Heterogeneity\pctprojected_simple"
est sto pctprojected_simple
est use ".\Regression Outputs\Heterogeneity\projected_simple_levels"
est sto projected_simple_levels
*/
	
******* realization rates
gen constant = 1
reghdfe log_total_mmbtu i.treated c.HDD60 c.HDD60#c.HDD60 c.CDD75 c.CDD75#c.CDD75 c.precip c.tmax c.tmin if twoyears_prepost==1 & meterdays_monthly!=., absorb(i.Household#i.end_month i.month_year#i.CountyID) residuals(res1)
est sto simple
reg pct_tau_ml constant if treated==1 & twoyears_prepost==1 & meterdays_monthly!=., nocons
est sto ml
reghdfe log_usage i.ww_treat , absorb(i.Household) residuals(res3)
est sto projected

reghdfe total_mmbtu i.treated c.HDD60 c.HDD60#c.HDD60 c.CDD75 c.CDD75#c.CDD75 c.precip c.tmax c.tmin if twoyears_prepost==1 & meterdays_monthly!=., absorb(i.Household#i.end_month i.month_year#i.CountyID) residuals(res1_levels)
est sto simple_levels
reg tau_ml constant if treated==1 & twoyears_prepost==1 & meterdays_monthly!=., nocons
est sto ml_levels
reghdfe usage i.ww_treat , absorb(i.Household) residuals(res3_levels)
est sto projected_levels

suest simple projected, vce(cluster Household)
nlcom (exp([simple]1.treated)-1)/(exp([projected]1.ww_treat)-1)
est sto att_realization
matrix rr1 = r(b)
scalar rr1 = rr1[1,1]
estadd scalar rr = rr1 : att_hhmonthcounty

suest ml projected, vce(cluster Household)
nlcom ([ml_mean]constant)/(exp([projected]1.ww_treat)-1), post
est sto ml_realization
matrix rr2 = r(b)
scalar rr2 = rr2[1,1]
estadd scalar rr = rr2 : ml_simple

suest simple_levels projected_levels, vce(cluster Household)
nlcom [simple_levels]1.treated/[projected_levels]1.ww_treat
est sto att_realization
matrix rr1 = r(b)
scalar rr1 = rr1[1,1]
estadd scalar rr_lev = rr1 : att_hhmonthcounty_levels

suest ml_levels projected_levels, vce(cluster Household)
nlcom [ml_levels_mean]constant/[projected_levels]1.ww_treat, post
est sto ml_realization
matrix rr2 = r(b)
scalar rr2 = rr2[1,1]
estadd scalar rr_lev = rr2 : ml_simple_levels


esttab projected_simple ml_simple att_simple att_hhmonth att_hhmonthcounty ///
				using "./Regression Outputs/Heterogeneity/ATT_fulltable.tex", ///
				rename(1.ww_treat a 1.treated a _nl_1 a constant a treated a) ///
				replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01) ///
				keep(a) coeflabels(a "WAP Treatment") nonotes ///				
				stats(rr N, fmt(%9.4g %9.0gc %9.4g) labels("Realization Rate" "Observations"))

esttab projected_simple_levels ml_simple_levels att_simple_levels att_hhmonth_levels att_hhmonthcounty_levels ///
				using "./Regression Outputs/Heterogeneity/ATT_fulltable_levels.tex", ///
				rename(1.ww_treat a 1.treated a _nl_1 a constant a treated a) ///
				replace b(4) se(4) star(* 0.10 ** 0.05 *** 0.01)  ///
				keep(a) coeflabels(a "WAP Treatment") nonotes ///
				stats(rr_lev N, fmt(%9.4g %9.0gc %9.4g) labels("Realization Rate" "Observations"))





********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
***************** HETEROGENEOUS TREATMENT EFFECTS ******************************

clear
cd "P:\IHWAP\Temp Data"
use "IHWAP_State.dta"
set more off

* Bringing contractor information
merge 1:1 jobnumber using "Contractors_by_jobnumber.dta"
keep if _merge==3
drop _merge
keep if BuildingType=="Single Family"
keep if MainHeatFuel=="1 - Natural Gas"


forval i = 1/845 {
egen obs_Contractor`i' = total(tab_Contractor`i')
}

forval i = 1/845 {
replace tab_Contractor`i' = . if obs_Contractor`i'<5
}

drop obs_Contractor*

ssc install ds3
ds3 tab_Contractor*, all(missing(X)) 
drop `r(varlist)'

/*
note: 1.tab_Contractor555 omitted because of collinearity
note: 1.tab_Contractor578 identifies no observations in the sample
note: 1.tab_Contractor609 omitted because of collinearity
note: 1.tab_Contractor616 omitted because of collinearity
note: 1.tab_Contractor699 identifies no observations in the sample
note: 1.tab_Contractor790 omitted because of collinearity

** note: with sample restrictions we have collinearity issues for the following contractors
tab_Contractor269 == tab_Contractor616 -> these contractors did the exact same homes
554 == 555
2 == 609 == 790
*/
drop tab_Contractor555 tab_Contractor578 tab_Contractor609 tab_Contractor616 tab_Contractor699 tab_Contractor790

* binned dollars spent
foreach x in Real_tot_actAirSeal Real_tot_actBaseload Real_tot_actDoor Real_tot_actHealSfty Real_tot_actWtHtr {
	gen bin`x' = .
	forval i = 0(100)900 {
		replace bin`x' = `i' if `x'>`i'-100 & `x'<=`i'
	}
	replace bin`x' = 1000 if `x'>900 & `x'!=.
}
foreach x in Real_tot_actAttic Real_tot_actFoundation Real_tot_actGeneral Real_tot_actWindow {
	gen bin`x' = .
	forval i = 0(200)2000 {
		replace bin`x' = `i' if `x'>`i'-200 & `x'<=`i'
	}
	replace bin`x' = 2200 if `x'>2000 & `x'!=.
}
foreach x in Real_oth_cost Real_tot_actFurnace Real_tot_actWallIns {
	gen bin`x' = .
	forval i = 0(300)3000 {
		replace bin`x' = `i' if `x'>`i'-300 & `x'<=`i'
	}
	replace bin`x' = 3300 if `x'>3000 & `x'!=.
}
gen bin_TotalCost = .
forval i = 0(500)9000 {
	replace bin_TotalCost = `i' if Real_total_cost>`i'-500 & Real_total_cost<=`i'
}
replace bin_TotalCost = 9500 if Real_total_cost>9000 & Real_total_cost!=.
replace bin_TotalCost = 500 if bin_TotalCost==0

foreach var of varlist binReal_tot_actAirSeal-binReal_tot_actWallIns {
	replace `var' = 1 if `var'==0
}
foreach x in Real_tot_actAirCon {
	gen bin`x' = .
	forval i = 0(100)500 {
		replace bin`x' = `i' if `x'>`i'-100 & `x'<=`i'
	}
	replace bin`x' = 600 if `x'>500 & `x'!=.
	replace bin`x' = 1 if bin`x'==0
}

drop binReal_oth_cost

foreach x in Attic AirCon AirSeal Baseload Door HealSfty WtHtr Foundation General Window Furnace WallIns {
rename binReal_tot_act`x' bin`x'
}

*** merge back with energy data
merge 1:m jobnumber using "IHWAP_ml.dta"
drop _merge

* Separating usage pre and post WAP, and creating artificial treatment variable
reghdfe log_total_mmbtu i.treated c.HDD60 c.HDD60#c.HDD60 c.CDD75 c.CDD75#c.CDD75 if twoyears_prepost==1 & meterdays_monthly!=., absorb(i.Household#i.end_month i.month_year#i.CountyID) vce(cluster Household end_month)
gen regsample = e(sample)

replace regsample = 2 if regsample==0
sort jobnumber regsample
by jobnumber : gen job_obs = _n
gen usage = ExistingConsumption/1000000 if job_obs==1 & regsample==1
replace usage = ProjectedConsumption/1000000 if job_obs==2 & regsample==1
replace usage = usage/12 /* transforming yearly to monthly */
winsor2 usage, cuts(0.5 99.5) trim replace
gen log_usage = log(usage)
gen ww_treat = 0 if job_obs==1 & regsample==1
replace ww_treat = 1 if job_obs==2 & regsample==1
label values ww_treat treat

* number of observations per home
bysort Household treated : gen nobs = _n
bysort Household treated : egen totobs = max(nobs)

***************** average savings per home
/*
sort Household end_month
by Household end_month : egen monthly_real_save = mean(pct_tau_ml) if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
forval i = 1/12 {
by Household : egen save_month`i' = max(monthly_real_save) if end_month==`i'
by Household : egen real_save_month`i' = max(save_month`i')
}
egen num_months = rownonmiss(real_save_month*)
egen realized_savings2 = rowmean(real_save_month*) if num_months==12
*/

* average savings per home
sort Household meterend
by Household : egen real_save = mean(pct_tau_ml) if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
by Household : egen realized_savings = max(real_save)
drop real_save

* average savings per home - levels (mmbtu)
by Household : egen real_save_mmbtu = mean(tau_ml) if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
by Household : egen realized_savings_mmbtu = max(real_save_mmbtu)
drop real_save_mmbtu

* projected savings per home
gen projected_savings = (ProjectedConsumption - ExistingConsumption)/ExistingConsumption

* calculate the GAP
gen savings_gap = realized_savings - projected_savings

**** bootstrap loog for savings gap
forval i = 1/150 {
* average savings per home
by Household : egen real_save`i' = mean(pct_tau`i') if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
by Household : egen realized_savings`i' = max(real_save`i')
drop real_save`i'
* average savings per home - levels (mmbtu)
by Household : egen real_save_mmbtu`i' = mean(tau`i') if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
by Household : egen realized_savings_mmbtu`i' = max(real_save_mmbtu`i')
drop real_save_mmbtu`i'
* calculate the GAP
gen savings_gap`i' = realized_savings`i' - projected_savings
}

/*
sum realized_savings if ww_treat==1, detail
sum realized_savings_mmbtu if ww_treat==1, detail
sum projected_savings if ww_treat==1, detail
sum savings_gap if ww_treat==1, detail
*/

*** demographic bundles
sum Real_income1000 if ww_treat==1 & realized_savings!=., detail
gen income_level = 1 if Real_income1000<r(p25)
replace income_level = 2 if Real_income1000>=r(p25) & Real_income1000<r(p75)
replace income_level = 3 if Real_income1000>=r(p75) & Real_income1000!=.

sum Age if ww_treat==1 & realized_savings!=., detail
gen age_level = 1 if Age<r(p25)
replace age_level = 2 if Age>=r(p25) & Age<r(p75)
replace age_level = 3 if Age>=r(p75) & Age!=.

gen hhsize = 1 if noccupants==1
replace hhsize = 2 if noccupants>=2 & noccupants<=3
replace hhsize = 3 if noccupants>=4 & noccupants!=.

* grouping demographics
egen group_demographics = group(income_level age_level hhsize)

* grouping contractors:
egen group_contractor = group(tab_Contractor*)


***** bundles of amount spent in the 4 major categories
foreach x in AirSeal Attic Furnace WallIns {
gen `x'_cat = 1 if tot_act`x'==0
}

replace Furnace_cat = 0 if tot_actFurnace>900 & tot_actFurnace<=1800
replace Furnace_cat = 2 if tot_actFurnace>0 & tot_actFurnace<=900
replace Furnace_cat = 3 if tot_actFurnace>1800

replace Attic_cat = 0 if tot_actAttic>600 & tot_actAttic<=1600
replace Attic_cat = 2 if tot_actAttic>0 & tot_actAttic<=600
replace Attic_cat = 3 if tot_actAttic>1600

replace AirSeal_cat = 0 if tot_actAirSeal>200 & tot_actAirSeal<=500
replace AirSeal_cat = 2 if tot_actAirSeal>0 & tot_actAirSeal<=200
replace AirSeal_cat = 3 if tot_actAirSeal>500

replace WallIns_cat = 0 if tot_actWallIns>900 & tot_actWallIns<=1500
replace WallIns_cat = 2 if tot_actWallIns>0 & tot_actWallIns<=900
replace WallIns_cat = 3 if tot_actWallIns>1500

egen spending_cat = group(AirSeal_cat Attic_cat Furnace_cat WallIns_cat)
replace spending_cat = 0 if AirSeal_cat==0 & Attic_cat==0 & Furnace_cat==0 & WallIns_cat==0

label define airseallab 0 "Med. AirSeal" 1 "Zero AirSeal" 2 "Low AirSeal" 3 "High AirSeal"
label define atticlab 0 "Med. Attic" 1 "Zero Attic" 2 "Low Attic" 3 "High Attic"
label define furnacelab 0 "Med. Furnace" 1 "Zero Furnace" 2 "Low Furnace" 3 "High Furnace"
label define wallinslab 0 "Med. WallIns" 1 "Zero WallIns" 2 "Low WallIns" 3 "High WallIns"

label values AirSeal_cat airseallab
label values Attic_cat atticlab
label values Furnace_cat furnacelab
label values WallIns_cat wallinslab


* merging with information about PRISM engineering model
tempfile ihwapgap
save `ihwapgap'

clear
use "prism_gas_step1.dta"

keep jobnumber max HDDbest treated
duplicates drop

merge 1:m jobnumber treated using `ihwapgap', gen(prismmerge)
drop if prismmerge==1
drop prismmerge

save "IHWAP_gap.dta", replace


********************************************************************************
********************************************************************************
********************* Regression on the gap

*** first we create a measure of contractor performance, based on CFM reduced
clear
cd "P:\IHWAP\Temp Data"
use "IHWAP_State.dta"


* Bringing contractor information
merge 1:1 jobnumber using "Contractors_by_jobnumber.dta"
keep if _merge==3
drop _merge
keep if BuildingType=="Single Family"
keep if MainHeatFuel=="1 - Natural Gas"


forval i = 1/845 {
egen obs_Contractor`i' = total(tab_Contractor`i')
}

forval i = 1/845 {
replace tab_Contractor`i' = . if obs_Contractor`i'<5
}

drop obs_Contractor*

set more off
ds3 tab_Contractor*, all(missing(X)) 
drop `r(varlist)'

/*
note: 1.tab_Contractor555 omitted because of collinearity
note: 1.tab_Contractor578 identifies no observations in the sample
note: 1.tab_Contractor609 omitted because of collinearity
note: 1.tab_Contractor616 omitted because of collinearity
note: 1.tab_Contractor699 identifies no observations in the sample
note: 1.tab_Contractor790 omitted because of collinearity

** note: with sample restrictions we have collinearity issues for the following contractors
tab_Contractor269 == tab_Contractor616 -> these contractors did the exact same homes
554 == 555
2 == 609 == 790
*/
drop tab_Contractor555 tab_Contractor578 tab_Contractor609 tab_Contractor616 tab_Contractor699 tab_Contractor790


*** demographic bundles
sum Real_income1000, detail
gen income_level = 1 if Real_income1000<r(p25)
replace income_level = 2 if Real_income1000>=r(p25) & Real_income1000<r(p75)
replace income_level = 3 if Real_income1000>=r(p75) & Real_income1000!=.

sum Age, detail
gen age_level = 1 if Age<r(p25)
replace age_level = 2 if Age>=r(p25) & Age<r(p75)
replace age_level = 3 if Age>=r(p75) & Age!=.

gen hhsize = 1 if noccupants==1
replace hhsize = 2 if noccupants>=2 & noccupants<=3
replace hhsize = 3 if noccupants>=4 & noccupants!=.


* grouping demographics
egen group_demographics = group(income_level age_level hhsize)

foreach x in 1 27 3 21 24 12 20 23 {
tab hhsize if group_demographics==`x'
tab income_level if group_demographics==`x'
tab age_level if group_demographics==`x'
}

* grouping contractors:
egen group_contractor = group(tab_Contractor*)

* binned dollars spent
foreach x in Real_tot_actAirSeal Real_tot_actBaseload Real_tot_actDoor Real_tot_actHealSfty Real_tot_actWtHtr Real_tot_actAirCon {
	gen bin`x' = .
	forval i = 0(100)900 {
		replace bin`x' = `i' if `x'>`i'-100 & `x'<=`i'
	}
	replace bin`x' = 1000 if `x'>900 & `x'!=.
}
foreach x in Real_tot_actAttic Real_tot_actFoundation Real_tot_actGeneral Real_tot_actWindow {
	gen bin`x' = .
	forval i = 0(200)2000 {
		replace bin`x' = `i' if `x'>`i'-200 & `x'<=`i'
	}
	replace bin`x' = 2200 if `x'>2000 & `x'!=.
}
foreach x in Real_oth_cost Real_tot_actFurnace Real_tot_actWallIns {
	gen bin`x' = .
	forval i = 0(300)3000 {
		replace bin`x' = `i' if `x'>`i'-300 & `x'<=`i'
	}
	replace bin`x' = 3300 if `x'>3000 & `x'!=.
}
gen bin_TotalCost = .
forval i = 0(500)9000 {
	replace bin_TotalCost = `i' if Real_total_cost>`i'-500 & Real_total_cost<=`i'
}
replace bin_TotalCost = 9500 if Real_total_cost>9000 & Real_total_cost!=.
replace bin_TotalCost = 500 if bin_TotalCost==0

foreach var of varlist binReal_tot_actAirSeal-binReal_tot_actWallIns {
	replace `var' = 1 if `var'==0
}

drop binReal_oth_cost

foreach x in Attic AirCon AirSeal Baseload Door HealSfty WtHtr Foundation General Window Furnace WallIns {
rename binReal_tot_act`x' bin`x'
}

* contractor quality regression
set matsize 10000
reg BlowerReduc ib1.binFurnace ib1.binAirSeal ib1.binAttic ib1.binWallIns ///
		ib1.binBaseload ib1.binDoor ib1.binWtHtr ib1.binHealSfty ib1.binWtHtr ///
		ib1.binAirCon ib1.binFoundation ib1.binGeneral ib1.binWindow ///
		ib14.group_demographics i.BlowerPreBins i.AgencyID ///
		i.priority i.white i.black i.hispanic i.otherrace i.haselderly i.hasminor ///
		i.roundsqfeet i.nstories i.roundAtticR i.builddate ///
		i.ProgramYear 1.tab_Contractor*
est save "./Regression Outputs/Heterogeneity/contCFM_perform", replace
parmest, saving("./Regression Outputs/Heterogeneity/contCFM_perform.dta", replace)


**** graphing results for contractor quality based on CFM
clear all
use "./Regression Outputs/Heterogeneity/contCFM_perform.dta"

*findit multproc
keep if p!=.
keep if substr(parm, 1, 12)=="1.tab_Contra"
gen ContractorID = substr(parm, 17, .)
destring ContractorID, replace

** Corrections for multiple hypothesis testing
sort p
multproc, puncor(0.05) pvalue(p) method(holm) critical(newp_holm) reject(reject_holm)
multproc, puncor(0.05) pvalue(p) method(bonferroni) critical(newp_bonfer) reject(reject_bonfer)
multproc, puncor(0.05) pvalue(p) method(hochberg) critical(newp_hoch) reject(reject_hoch)
multproc, puncor(0.05) pvalue(p) method(krieger) critical(newp_krieg) reject(reject_krieg)
multproc, puncor(0.05) pvalue(p) method(liu2) critical(newp_liu) reject(reject_liu)
multproc, puncor(0.05) pvalue(p) method(simes) critical(newp_simes) reject(reject_simes)


** auxiliary variables for graphing
sort estimate

cumul estimate, gen(pctile)
replace pctile = round(pctile, 0.01)
tostring pctile, replace force format(%9.2f)

gen obsid = _n
labmask obsid, values(pctile)

count
sca limit = `r(N)'

eclplot estimate min95 max95 obsid, ///
	estopts(mcolor(blue) msymbol(Th))  ///
	graphregion(color(white)) bgcolor(white) ///
	xtitle("Contractor Percentile Rank: best CFM50 reductions {&rarr}") ///
	ytitle("Effect on CFM50 Reductions" "(compared to average reductions of 1100)", size(small))  ///
	xlabel(0(98)`=scalar(limit)', valuelabel) ///
	ciopts(lcolor(blue)) yline(0)
graph export "./Regression Outputs/Heterogeneity/contractor_quality_cfm.png", replace width(5000)


eclplot estimate min95 max95 obsid, ///
    supby(reject_simes) ///
	estopts1(mcolor(blue) msymbol(Th))  ///
	estopts2(mcolor(red) msymbol(S)) ///
	legend(on rows(2) order(2  "Non-Significant Coefficients" 4 "Significant Coefficients (after FDR corrections)")) ///
	graphregion(color(white)) bgcolor(white) ///
	xtitle("Contractor Percentile Rank: best CFM50 reductions {&rarr}") ///
	ytitle("Effect on CFM50 Reductions" "(compared to average reductions of 1100)", size(small))  ///
	xlabel(0(98)`=scalar(limit)', valuelabel) ///
	ciopts1(lcolor(blue)) ///
	ciopts2(lcolor(red)) yline(0)
graph export "./Regression Outputs/Heterogeneity/contractor_quality_cfm2.png", replace width(5000)


***** empirical bayes correction
gen cont_quality = estimate 
gen cont_se = stderr
gen cont_var = stderr^2

ebayes cont_quality cont_se, gen(cont_quality_bayes) bee(shrinkage) theta(theta_bayes) tol(0.0001)
sum theta_bayes

gen sigma = 7562
gen avg_var = 12573
gen newbee = ((692-2)/692)*cont_var/(cont_var+sigma)
gen vee = (2/(692-2))*(shrinkage^2)*(avg_var+sigma)/(cont_var+sigma)
gen cont_var_bayes = cont_var*(1-shrinkage)+vee*(cont_quality-theta_bayes)^2
gen cont_var_bayes2 = cont_var*(1-shrinkage) /* uncorrected for unequal variances */
gen cont_se_bayes = sqrt(cont_var_bayes)

gen min95_bayes = cont_quality_bayes + (-1.96)*cont_se_bayes
gen max95_bayes = cont_quality_bayes + (1.96)*cont_se_bayes
sort cont_quality_bayes

cumul cont_quality_bayes, gen(pctile_bayes)
replace pctile_bayes = round(pctile_bayes, 0.01)
tostring pctile_bayes, replace force format(%9.2f)

gen obsid_bayes = _n
labmask obsid_bayes, values(pctile_bayes)

count
sca limit = `r(N)'


eclplot cont_quality_bayes min95_bayes max95_bayes obsid_bayes, ///
	estopts(mcolor(blue) msymbol(Th))  ///
	graphregion(color(white)) bgcolor(white) ///
	xtitle("Contractor Percentile Rank: best CFM50 reductions {&rarr}") ///
	ytitle("Effect on CFM50 Reductions" "(after Bayes Shrinkage)", size(small))  ///
	xlabel(0(98)`=scalar(limit)', valuelabel) ///
	ciopts(lcolor(blue)) yline(0)
graph export "./Regression Outputs/Heterogeneity/contractor_quality_cfm_bayes.png", replace width(5000)


keep ContractorID cont_quality cont_quality_bayes
** percentiles of contractor performance
xtile cont_decile = cont_quality, nq(10)
xtile cont_quintile = cont_quality, nq(5)
xtile cont_decile_bayes = cont_quality_bayes, nq(10)
xtile cont_quintile_bayes = cont_quality_bayes, nq(5)

** getting jobnumbers associated with contractors
merge 1:m ContractorID using "Contractors_by_jobnumber_long.dta"
keep if _merge==3
drop _merge

sort jobnumber
by jobnumber : egen contractor_quality = max(cont_quality)
by jobnumber : egen contractor_quality_bayes = max(cont_quality_bayes)
by jobnumber : egen contractor_decile = min(cont_decile)
by jobnumber : egen contractor_quintile = min(cont_quintile)
by jobnumber : egen contractor_decile_bayes = min(cont_decile_bayes)
by jobnumber : egen contractor_quintile_bayes = min(cont_quintile_bayes)

keep jobnumber contractor_quality contractor_quality_bayes contractor_decile ///
	contractor_quintile contractor_decile_bayes contractor_quintile_bayes
duplicates drop

save "./Regression Outputs/Heterogeneity/contractor_rank_cfm.dta", replace


***********************
clear
cd "P:\IHWAP\Temp Data"
use "IHWAP_gap.dta"

keep if ww_treat==1  & ProgramYear>=2009
keep if savings_gap!=.

ds3 tab_Contractor*, all(X==0) 
drop `r(varlist)'

ds3 tab_Contractor*
foreach cont in `r(varlist)' {
egen obs_`cont' = total(`cont')
replace `cont' = . if obs_`cont'<5
drop obs_`cont'
}

set more off
ds3 tab_Contractor*, all(missing(X)) 
drop `r(varlist)'


ssc install parmest, replace
reg savings_gap 1.tab_Contractor*, nocons
parmest, saving("./Regression Outputs/Heterogeneity/contractor_ordering.dta", replace)
** note: identified contractor 749 as the median contractor

set matsize 10000
reg savings_gap ibn.group_contractor, nocons
parmest, saving("./Regression Outputs/Heterogeneity/contractorgroup_ordering.dta", replace)
** note: identified group 630 as the median contractor

** fixing builddate to avoid omitted variables
replace builddate = 11 if tab_builddate11==1 | tab_builddate12==1 | tab_builddate13==1

** fixing sqfeet to avoid omitted variables
drop roundsqfeet
gen roundsqfeet = .
	forval i = 0(500)2500 {
		replace roundsqfeet = `i' if sqfeet>`i' & sqfeet<=`i'+500
	}
replace roundsqfeet = 3000 if sqfeet>3000 & sqfeet!=.

** fixing air conditioning spending to avoid omitted variables
drop binAirCon
gen binAirCon = .
replace binAirCon=0 if Real_tot_actAirCon==0
replace binAirCon=1 if Real_tot_actAirCon>0 & Real_tot_actAirCon!=.

** demographic variables
*tabulate group_demographics, gen(tab_demog)
*drop tab_demog14
gen simpocc = noccupants
replace simpocc = 6 if noccupants>=6 & noccupants!=.

gen binAge = .
forval i = 20(10)80 {
replace binAge = `i' if Age>`i' & Age<=`i'+10
}

gen binIncome = .
forval i = 5000(5000)35000 {
replace binIncome = `i' if Real_income>`i' & Real_income<=`i'+5000
}
replace binIncome = 1 if Real_income<=5000
replace binIncome = 40000 if Real_income>40000 & Real_income!=.


*** housing structure
gen binSqft = .
forval i = 600(300)2700 {
replace binSqft = `i' if sqfeet>`i' & sqfeet<=`i'+300
}
replace binSqft = 1 if sqfeet<=600
replace binSqft = 3000 if sqfeet>3000 & sqfeet!=.

drop BlowerPreBins
gen BlowerPreBins = .
forval i = 2000(1000)9000 {
replace BlowerPreBins = `i' if Blower_Pre>`i' & Blower_Pre<=`i'+1000
}
replace BlowerPreBins = 1 if Blower_Pre<=2000
replace BlowerPreBins = 10000 if Blower_Pre>10000 & Blower_Pre!=.

** merge with contractor CFM performance
merge 1:1 jobnumber using "./Regression Outputs/Heterogeneity/contractor_rank_cfm.dta", nogen keep(3)

/*
** merge with contractor experience info ** look for 'Tabulation with Measures 3'
merge 1:1 jobnumber using "AggContractor_Experience.dta", nogen keep(3)
* binning contractor experience
gen cont_experience = .
forval i = 0(50)450 {
replace cont_experience = `i' if avgnjobs>=`i' & avgnjobs<`i'+50
}
replace cont_experience = 500 if avgnjobs>=500 & avgnjobs!=.
*/


********************************************************************************
****** finally, regression for the gap
* transforming decimals in percent
replace savings_gap = savings_gap*100
forval i = 1/150 {
replace savings_gap`i' = 100*savings_gap`i'
}

** specification with contractor fixed effects
set matsize 10000
reg savings_gap ib900.binFurnace ib300.binAirSeal ib1000.binAttic ib1.binWallIns ///
		ib100.binBaseload ib200.binDoor ib1.binWtHtr ib500.binHealSfty ///
		ib1.binAirCon ib200.binFoundation ib1.binGeneral ib400.binWindow ///
		ib3000.BlowerPreBins  ///
		ib2.simpocc ib50.binAge ib15000.binIncome ///
		i.white i.black i.hispanic i.otherrace i.haselderly i.hasminor ///
		ib1500.binSqft i.nstories i.roundAtticR i.builddate ///
		i.ProgramYear 1.tab_Contractor*
matrix gap_coefs = e(b)
scalar nobs = e(N)

** specification with contractor rank
set matsize 10000
reg savings_gap ib900.binFurnace ib300.binAirSeal ib1000.binAttic ib1.binWallIns ///
		ib100.binBaseload ib200.binDoor ib1.binWtHtr ib500.binHealSfty ///
		ib1.binAirCon ib200.binFoundation ib1.binGeneral ib400.binWindow ///
		ib3000.BlowerPreBins  ///
		ib2.simpocc ib50.binAge ib15000.binIncome ///
		i.white i.black i.hispanic i.otherrace i.haselderly i.hasminor ///
		ib1500.binSqft i.nstories i.roundAtticR i.builddate ///
		i.ProgramYear ib5.contractor_quintile_bayes
matrix gap_coefs2 = e(b)

/*		
set matsize 10000
reg savings_gap ib900.binFurnace ib300.binAirSeal ib1000.binAttic ib1.binWallIns ///
		ib100.binBaseload ib200.binDoor ib1.binWtHtr ib500.binHealSfty ///
		ib1.binAirCon ib200.binFoundation ib1.binGeneral ib400.binWindow ///
		ib3000.BlowerPreBins  ///
		ib2.simpocc ib50.binAge ib15000.binIncome ///
		i.white i.black i.hispanic i.otherrace i.haselderly i.hasminor ///
		ib1500.binSqft i.nstories i.roundAtticR i.builddate ///
		i.ProgramYear ib0.cont_experience	
			

set matsize 10000
reg savings_gap ib900.binFurnace ib300.binAirSeal ib1000.binAttic ib1.binWallIns ///
		ib100.binBaseload ib200.binDoor ib1.binWtHtr ib500.binHealSfty ///
		ib1.binAirCon ib200.binFoundation ib1.binGeneral ib400.binWindow ///
		1.tab_demog* ib3500.BlowerPreBins ib5.priority ///
		i.white i.black i.hispanic i.otherrace i.haselderly i.hasminor ///
		ib1500.roundsqfeet i.nstories i.roundAtticR i.builddate ///
		i.ProgramYear 1.tab_Contractor*
matrix gap_coefs = e(b)
scalar nobs = e(N)
*/

*putexcel set "test2"
*putexcel A2 = matrix(gap_coefs'), rownames


*** bootstrapping SEs for gap regression
** program that produces nice estimation table of bootstrap results - matrix labels already predefined
prog def bootstraplabelled, eclass
	args b V N /* input coef matrix, variance matrix, and number of obs */
	ereturn post `b' `V', obs(`=scalar(`N')')
	ereturn local cmd "bootstrapres"
	ereturn local properties "b V"
	ereturn display
end

** specification with contractor fixed effects
local dim `= rowsof(gap_coefs)',`=colsof(gap_coefs)'
matrix gap_coefs_boot = J(`dim',.)
forval i = 1/150 {
reg savings_gap`i' ib900.binFurnace ib300.binAirSeal ib1000.binAttic ib1.binWallIns ///
		ib100.binBaseload ib200.binDoor ib1.binWtHtr ib500.binHealSfty ///
		ib1.binAirCon ib200.binFoundation ib1.binGeneral ib400.binWindow ///
		ib3000.BlowerPreBins  ///
		ib2.simpocc ib50.binAge ib15000.binIncome ///
		i.white i.black i.hispanic i.otherrace i.haselderly i.hasminor ///
		ib1500.binSqft i.nstories i.roundAtticR i.builddate ///
		i.ProgramYear 1.tab_Contractor* [fweight=weight`i']
matrix tempmat = e(b)
matrix gap_coefs_boot = (gap_coefs_boot \ tempmat)
}
ssc install moremata, replace
matrix bootstrapcoefs = gap_coefs_boot
mata : st_matrix("bootstrapcoefs", editvalue(st_matrix("bootstrapcoefs"), 0, .))
mata : st_matrix("gap_var_boot", mm_colvar(st_matrix("bootstrapcoefs")))
matrix gap_var_boot = diag(gap_var_boot)
mata : st_matrix("gap_var_boot", editvalue(st_matrix("gap_var_boot"), ., 0))
local colnames : colnames gap_coefs
mat colnames gap_var_boot=`colnames'
mat rownames gap_var_boot=`colnames'
bootstraplabelled gap_coefs gap_var_boot nobs
est save "./Regression Outputs/Heterogeneity/new_decompose_gap", replace
parmest, saving("./Regression Outputs/Heterogeneity/new_decompose_gap.dta", replace)

** specification with contractor rank
local dim `= rowsof(gap_coefs2)',`=colsof(gap_coefs2)'
matrix gap_coefs_boot2 = J(`dim',.)
forval i = 1/150 {
reg savings_gap`i' ib900.binFurnace ib300.binAirSeal ib1000.binAttic ib1.binWallIns ///
		ib100.binBaseload ib200.binDoor ib1.binWtHtr ib500.binHealSfty ///
		ib1.binAirCon ib200.binFoundation ib1.binGeneral ib400.binWindow ///
		ib3000.BlowerPreBins  ///
		ib2.simpocc ib50.binAge ib15000.binIncome ///
		i.white i.black i.hispanic i.otherrace i.haselderly i.hasminor ///
		ib1500.binSqft i.nstories i.roundAtticR i.builddate ///
		i.ProgramYear ib5.contractor_quintile_bayes [fweight=weight`i']
matrix tempmat = e(b)
matrix gap_coefs_boot2 = (gap_coefs_boot2 \ tempmat)
}
ssc install moremata, replace
matrix bootstrapcoefs2 = gap_coefs_boot2
mata : st_matrix("bootstrapcoefs2", editvalue(st_matrix("bootstrapcoefs2"), 0, .))
mata : st_matrix("gap_var_boot2", mm_colvar(st_matrix("bootstrapcoefs2")))
matrix gap_var_boot2 = diag(gap_var_boot2)
mata : st_matrix("gap_var_boot2", editvalue(st_matrix("gap_var_boot2"), ., 0))
local colnames : colnames gap_coefs2
mat colnames gap_var_boot2=`colnames'
mat rownames gap_var_boot2=`colnames'
bootstraplabelled gap_coefs2 gap_var_boot2 nobs
est save "./Regression Outputs/Heterogeneity/new_decompose_gap2", replace
parmest, saving("./Regression Outputs/Heterogeneity/new_decompose_gap2.dta", replace)


**** HISTOGRAMS FOR DOLLARS SPENT IN EACH BIN
gen newbinAirSeal = binAirSeal - 100
replace newbinAirSeal = -100 if newbinAirSeal==-99
gen newbinAttic = binAttic - 200
replace newbinAttic = -200 if newbinAttic==-199
gen newbinWallIns = binWallIns - 300
replace newbinWallIns = -300 if newbinWallIns==-299
gen newbinFurnace = binFurnace - 300
replace newbinFurnace = -300 if newbinFurnace==-299

foreach x of varlist binFurnace binAirSeal binAttic binWallIns {
bysort new`x' : gen count`x' = _n if ProgramYear>=2009
bysort new`x' : egen freq_`x' = max(count`x')
drop count`x'
}						

label var Real_tot_actAirCon "Air Conditioning"
label var Real_tot_actAirSeal "Air Sealing"
label var Real_tot_actAttic "Attic"
label var Real_tot_actBaseload "Baseload"
label var Real_tot_actDoor "Door"
label var Real_tot_actFoundation "Foundation"
label var Real_tot_actFurnace "Furnace"
label var Real_tot_actGeneral "General"
label var Real_tot_actHealSfty "Health and Safety"
label var Real_tot_actWallIns "Wall Insulation"
label var Real_tot_actWindow "Window"
label var Real_tot_actWtHtr "Water Heater"

label define furnacelab2 -300 "0" 0 "1-300" 300 "301-600" 600 "601-900" 900 "901-1200" 1200 "1201-1500" ///
		1500 "1501-1800" 1800 "1801-2100" 2100 "2101-2400" 2400 "2401-2700" 2700 "2701-3000" 3000 ">3000"
label values newbinFurnace furnacelab2
label values newbinWallIns furnacelab2

label define airseallab2 -100 "0" 0 "1-100" 100 "101-200" 200 "201-300" 300 "301-400" 400 "401-500" ///
		500 "501-600" 600 "601-700" 700 "701-800" 800 "801-900" 900 ">900"
label values newbinAirSeal airseallab2
		
label define atticlab2 -200"0" 0 "1-200" 200 "201-400" 400 "401-600" 600 "601-800" 800 "801-1000" ///
		1000 "1001-1200" 1200 "1201-1400" 1400 "1401-1600" 1600 "1601-1800" 1800 "1801-2000" 2000 ">2000"
label values newbinAttic atticlab2	


count
sca totsamp = r(N)
foreach x in WallIns {
sort newbin`x'
local varlab : variable label Real_tot_act`x'
count if Real_tot_act`x'>0
sca `x'_samp = r(N)
sca `x'_percsamp = round((`x'_samp/totsamp)*100, 0.1)
local samppercent: display %2.1f `x'_percsamp
twoway bar freq_bin`x' newbin`x' if newbin`x'>=0 , ///
							fcolor(blue%30) fintensity(inten30) lwidth(none) barwidth(150) ///
							graphregion(color(white)) bgcolor(white) ///
							ylabel(, format(%8.0fc)) ///
							ytitle("Number of Homes") xtitle("Amount Spent (US$) on `varlab'" "(for `samppercent'% of sample with non-zero spending in measure)") ///
							xscale(range(-300 3000)) xlabel(0(300)3000, valuelabels labsize(vsmall) angle(45) format(%8.0fc))
graph export ".\Regression Outputs\Heterogeneity\newhist_spent_`x'.png", replace
}

foreach x in Furnace {
sort newbin`x'
local varlab : variable label Real_tot_act`x'
count if Real_tot_act`x'!=.
sca `x'_samp = r(N)
sca `x'_percsamp = round((`x'_samp/totsamp)*100, 0.1)
local samppercent: display %2.1f `x'_percsamp
twoway bar freq_bin`x' newbin`x' if newbin`x'!=. , ///
							fcolor(blue%30) fintensity(inten30) lwidth(none) barwidth(150) ///
							graphregion(color(white)) bgcolor(white) ///
							ylabel(, format(%8.0fc)) ///
							ytitle("Number of Homes") xtitle("Amount Spent (US$) on `varlab'") ///
							xscale(range(-300 3000)) xlabel(-300(300)3000, valuelabels labsize(vsmall) angle(45) format(%8.0fc))
graph export ".\Regression Outputs\Heterogeneity\newhist_spent_`x'.png", replace
}

foreach x in AirSeal {
sort newbin`x'
local varlab : variable label Real_tot_act`x'
count if Real_tot_act`x'!=.
sca `x'_samp = r(N)
sca `x'_percsamp = round((`x'_samp/totsamp)*100, 0.1)
local samppercent: display %2.1f `x'_percsamp
twoway bar freq_bin`x' newbin`x' if newbin`x'!=., ///
							fcolor(blue%30) fintensity(inten30) lwidth(none) barwidth(50) ///
							graphregion(color(white)) bgcolor(white) ///
							bcolor(blue%30) ///
							ylabel(, format(%8.0fc)) ///
							ytitle("Number of Homes") xtitle("Amount Spent (US$) on `varlab'") ///
							xscale(range(-100 900)) xlabel(-100(100)900, valuelabels labsize(vsmall) angle(45) format(%8.0fc))
graph export ".\Regression Outputs\Heterogeneity\newhist_spent_`x'.png", replace
}

foreach x in Attic {
sort newbin`x'
local varlab : variable label Real_tot_act`x'
count if Real_tot_act`x'!=.
sca `x'_samp = r(N)
sca `x'_percsamp = round((`x'_samp/totsamp)*100, 0.1)
local samppercent: display %2.1f `x'_percsamp
twoway bar freq_bin`x' newbin`x' if newbin`x'!=.,  ///
							fcolor(blue%30) fintensity(inten30) lwidth(none) barwidth(100) ///
							graphregion(color(white)) bgcolor(white) ///
							bcolor(blue%30) ///
							ylabel(, format(%8.0fc)) ///
							ytitle("Number of Homes") xtitle("Amount Spent (US$) on `varlab'") ///
							xscale(range(-200 2000)) xlabel(-200(200)2000, valuelabels labsize(vsmall) angle(45) format(%8.0fc))
graph export ".\Regression Outputs\Heterogeneity\newhist_spent_`x'.png", replace
}


********************************************************************************
**** graphing results from gap regressions

*** for contractors
clear all
cd "P:\IHWAP\Temp Data"
use "./Regression Outputs/Heterogeneity/new_decompose_gap.dta"

*findit multproc
keep if p!=.
keep if substr(parm, 1, 12)=="1.tab_Contra"
gen ContractorID = substr(parm, 17, .)
destring ContractorID, replace

** Corrections for multiple hypothesis testing
sort p
multproc, puncor(0.05) pvalue(p) method(holm) critical(newp_holm) reject(reject_holm)
multproc, puncor(0.05) pvalue(p) method(bonferroni) critical(newp_bonfer) reject(reject_bonfer)
multproc, puncor(0.05) pvalue(p) method(hochberg) critical(newp_hoch) reject(reject_hoch)
multproc, puncor(0.05) pvalue(p) method(krieger) critical(newp_krieg) reject(reject_krieg)
multproc, puncor(0.05) pvalue(p) method(liu2) critical(newp_liu) reject(reject_liu)
multproc, puncor(0.05) pvalue(p) method(simes) critical(newp_simes) reject(reject_simes)

** only contractor 455 has a significant coefficient

** auxiliary variables for graphing
gsort -estimate
gen neg_estimate = -estimate

cumul neg_estimate, gen(pctile)
replace pctile = round(pctile, 0.01)
tostring pctile, replace force format(%9.2f)

gen obsid = _n
labmask obsid, values(pctile)

count
sca limit = `r(N)'

sum estimate
sca avgcoef = round(r(mean), 0.1)
gen cont_quality = estimate 


eclplot estimate min95 max95 obsid, ///
    supby(reject_simes) ///
	estopts1(mcolor(blue) msymbol(Th))  ///
	estopts2(mcolor(red) msymbol(S)) ///
	legend(on rows(2) order(2  "Non-Significant Coefficients" 4 "Significant Coefficients (after FDR corrections)")) ///
	graphregion(color(white)) bgcolor(white) ///
	xtitle("Contractor Percentile Rank: smaller gap {&rarr}") ///
	ytitle("Effect on Gap (%)" "(compared to average gap of 12.5%)")  ///
	xlabel(0(79)`=scalar(limit)', valuelabel) ylabel(-75.0 -50.0 -25.0 0 25.0 50.0 75.0) ///
	ciopts1(lcolor(blue)) ///
	ciopts2(lcolor(red)) yline(0) ///
	note("Note: 95% confidence intervals based on bootstrapped standard errors.")
graph export "./Regression Outputs/Heterogeneity/gap_contractor.png", replace width(5000)

sort cont_quality
save "./Regression Outputs/Heterogeneity/contractor_rank.dta", replace


*** for contractors - quality based on CFM reductions
clear all
cd "P:\IHWAP\Temp Data"
use "./Regression Outputs/Heterogeneity/new_decompose_gap2.dta"

*findit multproc
keep if substr(parm, -6, .)=="_bayes"
gen id = _n
gsort -id
gen newid = _n

** Corrections for multiple hypothesis testing
sort p
multproc, puncor(0.05) pvalue(p) method(holm) critical(newp_holm) reject(reject_holm)
multproc, puncor(0.05) pvalue(p) method(bonferroni) critical(newp_bonfer) reject(reject_bonfer)
multproc, puncor(0.05) pvalue(p) method(hochberg) critical(newp_hoch) reject(reject_hoch)
multproc, puncor(0.05) pvalue(p) method(krieger) critical(newp_krieg) reject(reject_krieg)
multproc, puncor(0.05) pvalue(p) method(liu2) critical(newp_liu) reject(reject_liu)
multproc, puncor(0.05) pvalue(p) method(simes) critical(newp_simes) reject(reject_simes)

eclplot estimate min95 max95 newid, ///
	ciopts(blcolor(blue)) estopts(mcolor(blue) msymbol(Th))  ///
	xtitle("Contractor Performance Quintile Rank" "(based on average CFM50 reduced)") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("Effect on Gap (%)" "(compared to worst contractors)") yline(0) ///
	xlabel(1 "5 - worst" 2 "2" 3 "3" 4 "4" 5 "1 - best" , ///
	labsize(small) angle(45)) ///
	note("Note: 95% confidence intervals based on bootstrapped standard errors." "None of the coefficients are significant after FDR corrections.")
graph export "./Regression Outputs/Heterogeneity/gap_contractor2.png", replace width(5000)


*** for demographics  - HH age
clear all
use "./Regression Outputs/Heterogeneity/new_decompose_gap.dta"

keep if substr(parm, -6, .)=="binAge"
gen id = _n

** Corrections for multiple hypothesis testing
sort p
multproc, puncor(0.05) pvalue(p) method(holm) critical(newp_holm) reject(reject_holm)
multproc, puncor(0.05) pvalue(p) method(bonferroni) critical(newp_bonfer) reject(reject_bonfer)
multproc, puncor(0.05) pvalue(p) method(hochberg) critical(newp_hoch) reject(reject_hoch)
multproc, puncor(0.05) pvalue(p) method(krieger) critical(newp_krieg) reject(reject_krieg)
multproc, puncor(0.05) pvalue(p) method(liu2) critical(newp_liu) reject(reject_liu)
multproc, puncor(0.05) pvalue(p) method(simes) critical(newp_simes) reject(reject_simes)
** none of the coefficients are significant

eclplot estimate min95 max95 id, ///
	ciopts(blcolor(blue)) estopts(mcolor(blue) msymbol(Th))  ///
	xtitle("Householder Age") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("Effect on Gap (%)" "(compared to median age)") yline(0) ///
	xlabel(1 "21-30" 2 "31-40" 3 "41-50" 4 "51-60" 5 "61-70" 6 "71-80" 7 ">80", ///
	labsize(small) angle(45)) ///
	note("Note: 95% confidence intervals based on bootstrapped standard errors." "None of the coefficients are significant after FDR corrections.")
graph export "./Regression Outputs/Heterogeneity/gap_age.png", replace width(5000)


*** for demographics  - income
clear all
use "./Regression Outputs/Heterogeneity/new_decompose_gap.dta"

keep if substr(parm, -6, .)=="Income"
gen id = _n

** Corrections for multiple hypothesis testing
sort p
multproc, puncor(0.05) pvalue(p) method(holm) critical(newp_holm) reject(reject_holm)
multproc, puncor(0.05) pvalue(p) method(bonferroni) critical(newp_bonfer) reject(reject_bonfer)
multproc, puncor(0.05) pvalue(p) method(hochberg) critical(newp_hoch) reject(reject_hoch)
multproc, puncor(0.05) pvalue(p) method(krieger) critical(newp_krieg) reject(reject_krieg)
multproc, puncor(0.05) pvalue(p) method(liu2) critical(newp_liu) reject(reject_liu)
multproc, puncor(0.05) pvalue(p) method(simes) critical(newp_simes) reject(reject_simes)
** none of the coefficients are significant

eclplot estimate min95 max95 id, ///
	ciopts(blcolor(blue)) estopts(mcolor(blue) msymbol(Th))  ///
	xtitle("Household Income (US$ 1,000)") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("Effect on Gap (%)" "(compared to median income)") yline(0) ///
	xlabel(1 "<5" 2 "[5 10)" 3 "[10 15)" 4 "[15 20)" 5 "[20 25)" 6 "[25 30)" 7 "[30 35)" ///
	8 "[35 40)" 9 ">40", labsize(small) angle(45)) ///
	note("Note: 95% confidence intervals based on bootstrapped standard errors." "None of the coefficients are significant after FDR corrections.")
graph export "./Regression Outputs/Heterogeneity/gap_income.png", replace width(5000)



*** for demographics  - family size
clear all
use "./Regression Outputs/Heterogeneity/new_decompose_gap.dta"

keep if substr(parm, -7, .)=="simpocc"
gen id = _n

** Corrections for multiple hypothesis testing
sort p
multproc, puncor(0.05) pvalue(p) method(holm) critical(newp_holm) reject(reject_holm)
multproc, puncor(0.05) pvalue(p) method(bonferroni) critical(newp_bonfer) reject(reject_bonfer)
multproc, puncor(0.05) pvalue(p) method(hochberg) critical(newp_hoch) reject(reject_hoch)
multproc, puncor(0.05) pvalue(p) method(krieger) critical(newp_krieg) reject(reject_krieg)
multproc, puncor(0.05) pvalue(p) method(liu2) critical(newp_liu) reject(reject_liu)
multproc, puncor(0.05) pvalue(p) method(simes) critical(newp_simes) reject(reject_simes)

replace reject_simes = 0 if p==.

eclplot estimate min95 max95 id, ///
    supby(reject_simes) ///
	estopts1(mcolor(blue) msymbol(Th))  ///
	estopts2(mcolor(red) msymbol(S)) ///
	ciopts1(lcolor(blue)) ///
	ciopts2(lcolor(red)) yline(0) ///
	legend(on rows(2) order(2  "Non-Significant Coefficients" 4 "Significant Coefficients (after FDR corrections)")) ///
	xtitle("Family Size (number of occupants)") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("Effect on Gap (%)" "(compared to median family size)") ///
	xlabel(1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "{&ge}6", labsize(small) angle(45)) ///
	note("Note: 95% confidence intervals based on bootstrapped standard errors.")
graph export "./Regression Outputs/Heterogeneity/gap_famsize.png", replace width(5000)


*** for furnace
clear all
use "./Regression Outputs/Heterogeneity/new_decompose_gap.dta"

keep if substr(parm, -7, .)=="Furnace"
gen id = _n


** Holm correction for multiple hypothesis testing
sort p
multproc, puncor(0.05) pvalue(p) method(holm) critical(newp_holm) reject(reject_holm)
multproc, puncor(0.05) pvalue(p) method(bonferroni) critical(newp_bonfer) reject(reject_bonfer)
multproc, puncor(0.05) pvalue(p) method(hochberg) critical(newp_hoch) reject(reject_hoch)
multproc, puncor(0.05) pvalue(p) method(krieger) critical(newp_krieg) reject(reject_krieg)
multproc, puncor(0.05) pvalue(p) method(liu2) critical(newp_liu) reject(reject_liu)
multproc, puncor(0.05) pvalue(p) method(simes) critical(newp_simes) reject(reject_simes)


replace reject_simes = 0 if p==.

eclplot estimate min95 max95 id, ///
    supby(reject_simes) ///
	estopts1(mcolor(blue) msymbol(Th))  ///
	estopts2(mcolor(red) msymbol(S)) ///
	ciopts1(lcolor(blue)) ///
	ciopts2(lcolor(red)) yline(0) ///
	note("Note: 95% confidence intervals based on bootstrapped standard errors.") ///
	legend(on rows(2) order(2  "Non-Significant Coefficients" 4 "Significant Coefficients (after FDR corrections)")) ///
	xtitle("Amount Spent (US$) on Furnace") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("Effect on Gap (%)" "(compared to median spending)") yline(0) ///
	xlabel(1 "0" 2 "1-300" 3 "301-600" 4 "601-900" 5 "901-1200" 6 "1201-1500" ///
	7 "1501-1800" 8 "1801-2100" 9 "2101-2400" 10 "2401-2700" 11 "2701-3000" 12 ">3000" , ///
	labsize(small) angle(45))
graph export "./Regression Outputs/Heterogeneity/gap_furnace.png", replace width(5000)



*** for air sealing
clear all
use "./Regression Outputs/Heterogeneity/new_decompose_gap.dta"

keep if substr(parm, -7, .)=="AirSeal"
gen id = _n

** Holm correction for multiple hypothesis testing
sort p
multproc, puncor(0.05) pvalue(p) method(holm) critical(newp_holm) reject(reject_holm)
multproc, puncor(0.05) pvalue(p) method(bonferroni) critical(newp_bonfer) reject(reject_bonfer)
multproc, puncor(0.05) pvalue(p) method(hochberg) critical(newp_hoch) reject(reject_hoch)
multproc, puncor(0.05) pvalue(p) method(krieger) critical(newp_krieg) reject(reject_krieg)
multproc, puncor(0.05) pvalue(p) method(liu2) critical(newp_liu) reject(reject_liu)
multproc, puncor(0.05) pvalue(p) method(simes) critical(newp_simes) reject(reject_simes)
*** none of the coefficients are significant


eclplot estimate min95 max95 id, ///
	ciopts(blcolor(blue)) estopts(mcolor(blue) msymbol(Th))  ///
	xtitle("Amount Spent (US$) on Air Sealing") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("Effect on Gap (%)" "(compared to median spending)")  yline(0) ///
	xlabel(1 "0" 2 "1-100" 3 "101-200" 4 "201-300" 5 "301-400" ///
	6 "401-500" 7 "501-600" 8 "601-700" 9 "701-800" 10 "801-900" 11 ">900", ///
	labsize(small) angle(45)) ///
	note("Note: 95% confidence intervals based on bootstrapped standard errors." "None of the coefficients are significant after FDR corrections.")
graph export "./Regression Outputs/Heterogeneity/gap_airseal.png", replace width(5000)


*** for attic
clear all
use "./Regression Outputs/Heterogeneity/new_decompose_gap.dta"

keep if substr(parm, -5, .)=="Attic"
gen id = _n


** Holm correction for multiple hypothesis testing
sort p
multproc, puncor(0.05) pvalue(p) method(holm) critical(newp_holm) reject(reject_holm)
multproc, puncor(0.05) pvalue(p) method(bonferroni) critical(newp_bonfer) reject(reject_bonfer)
multproc, puncor(0.05) pvalue(p) method(hochberg) critical(newp_hoch) reject(reject_hoch)
multproc, puncor(0.05) pvalue(p) method(krieger) critical(newp_krieg) reject(reject_krieg)
multproc, puncor(0.05) pvalue(p) method(liu2) critical(newp_liu) reject(reject_liu)
multproc, puncor(0.05) pvalue(p) method(simes) critical(newp_simes) reject(reject_simes)
** none of the coefficients are significant



/*
replace reject_simes = 0 if p==.

eclplot estimate min95 max95 id, ///
    supby(reject_hoch) ///
	estopts1(mcolor(blue) msymbol(Th))  ///
	estopts2(mcolor(red) msymbol(S)) ///
	ciopts1(lcolor(blue)) ///
	ciopts2(lcolor(red)) yline(0) ///
	note("Note: 95% confidence intervals based on bootstrapped standard errors.") ///
	legend(on rows(2) order(2  "Non-Significant Coefficients" 4 "Significant Coefficients (according to Hochenberg's procedure)")) ///
	xtitle("Amount Spent (US$) on Attic") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("Effect on Gap (%)" "(compared to median spending)")  yline(0) ///
	xlabel(1 "0" 2 "1-200" 3 "201-400" 4 "401-600" 5 "601-800" 6 "801-1000" ///
	7 "1001-1200" 8 "1201-1400" 9 "1401-1600" 10 "1601-1800" 11 "1801-2000" 12 ">2000" , ///
	labsize(small) angle(45))
*/

eclplot estimate min95 max95 id, ///
	estopts(mcolor(blue) msymbol(Th))  ///
	ciopts(lcolor(blue)) yline(0) ///
	note("Note: 95% confidence intervals based on bootstrapped standard errors." "None of the coefficients are significant after FDR corrections.") ///
	xtitle("Amount Spent (US$) on Attic") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("Effect on Gap (%)" "(compared to median spending)")  yline(0) ///
	xlabel(1 "0" 2 "1-200" 3 "201-400" 4 "401-600" 5 "601-800" 6 "801-1000" ///
	7 "1001-1200" 8 "1201-1400" 9 "1401-1600" 10 "1601-1800" 11 "1801-2000" 12 ">2000" , ///
	labsize(small) angle(45))
graph export "./Regression Outputs/Heterogeneity/gap_attic.png", replace width(5000)


*** for wall insulation
clear all
use "./Regression Outputs/Heterogeneity/new_decompose_gap.dta"

keep if substr(parm, -7, .)=="WallIns"
gen id = _n


** Holm correction for multiple hypothesis testing
sort p
multproc, puncor(0.05) pvalue(p) method(holm) critical(newp_holm) reject(reject_holm)
multproc, puncor(0.05) pvalue(p) method(bonferroni) critical(newp_bonfer) reject(reject_bonfer)
multproc, puncor(0.05) pvalue(p) method(hochberg) critical(newp_hoch) reject(reject_hoch)
multproc, puncor(0.05) pvalue(p) method(krieger) critical(newp_krieg) reject(reject_krieg)
multproc, puncor(0.05) pvalue(p) method(liu2) critical(newp_liu) reject(reject_liu)
multproc, puncor(0.05) pvalue(p) method(simes) critical(newp_simes) reject(reject_simes)

replace reject_simes = 0 if p==.

eclplot estimate min95 max95 id, ///
    supby(reject_simes) ///
	estopts1(mcolor(blue) msymbol(Th))  ///
	estopts2(mcolor(red) msymbol(S)) ///
	ciopts1(lcolor(blue)) ///
	ciopts2(lcolor(red)) yline(0) ///
	note("Note: 95% confidence intervals based on bootstrapped standard errors.") ///
	legend(on rows(2) order(2  "Non-Significant Coefficients" 4 "Significant Coefficients (after FDR corrections)")) ///
	xtitle("Amount Spent (US$) on Wall Insulation") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("Effect on Gap (%)" "(compared to zero spending)") yline(0) ///
	xlabel(1 "0" 2 "1-300" 3 "301-600" 4 "601-900" 5 "901-1200" 6 "1201-1500" ///
	7 "1501-1800" 8 "1801-2100" 9 "2101-2400" 10 "2401-2700" 11 "2701-3000" 12 ">3000" , ///
	labsize(small) angle(45))
graph export "./Regression Outputs/Heterogeneity/gap_wallins.png", replace width(5000)


*** for housing structure - floor area
clear all
use "./Regression Outputs/Heterogeneity/new_decompose_gap.dta"

keep if substr(parm, -7, .)=="binSqft"
gen id = _n


** Holm correction for multiple hypothesis testing
sort p
multproc, puncor(0.05) pvalue(p) method(holm) critical(newp_holm) reject(reject_holm)
multproc, puncor(0.05) pvalue(p) method(bonferroni) critical(newp_bonfer) reject(reject_bonfer)
multproc, puncor(0.05) pvalue(p) method(hochberg) critical(newp_hoch) reject(reject_hoch)
multproc, puncor(0.05) pvalue(p) method(krieger) critical(newp_krieg) reject(reject_krieg)
multproc, puncor(0.05) pvalue(p) method(liu2) critical(newp_liu) reject(reject_liu)
multproc, puncor(0.05) pvalue(p) method(simes) critical(newp_simes) reject(reject_simes)

replace reject_simes = 0 if p==.

eclplot estimate min95 max95 id, ///
    supby(reject_simes) ///
	estopts1(mcolor(blue) msymbol(Th))  ///
	estopts2(mcolor(red) msymbol(S)) ///
	ciopts1(lcolor(blue)) ///
	ciopts2(lcolor(red)) yline(0) ///
	note("Note: 95% confidence intervals based on bootstrapped standard errors.") ///
	legend(on rows(2) order(2  "Non-Significant Coefficients" 4 "Significant Coefficients (after FDR corrections)")) ///
	xtitle("Floor Area (sqft)") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("Effect on Gap (%)" "(compared to median floor area)") yline(0) ///
	xlabel(1 "<600" 2 "601-900" 3 "901-1200" 4 "1201-1500" 5 "1501-1800" 6 "1801-2100" ///
	7 "2101-2400" 8 "2401-2700" 9 "2701-3000" 10 ">3000" , ///
	labsize(small) angle(45))
graph export "./Regression Outputs/Heterogeneity/gap_sqft.png", replace width(5000)



*** for housing structure - blower door pre
clear all
use "./Regression Outputs/Heterogeneity/new_decompose_gap.dta"

keep if substr(parm, -7, .)=="PreBins"
gen id = _n


** Holm correction for multiple hypothesis testing
sort p
multproc, puncor(0.05) pvalue(p) method(holm) critical(newp_holm) reject(reject_holm)
multproc, puncor(0.05) pvalue(p) method(bonferroni) critical(newp_bonfer) reject(reject_bonfer)
multproc, puncor(0.05) pvalue(p) method(hochberg) critical(newp_hoch) reject(reject_hoch)
multproc, puncor(0.05) pvalue(p) method(krieger) critical(newp_krieg) reject(reject_krieg)
multproc, puncor(0.05) pvalue(p) method(liu2) critical(newp_liu) reject(reject_liu)
multproc, puncor(0.05) pvalue(p) method(simes) critical(newp_simes) reject(reject_simes)

replace reject_simes = 0 if p==.

eclplot estimate min95 max95 id, ///
    supby(reject_simes) ///
	estopts1(mcolor(blue) msymbol(Th))  ///
	estopts2(mcolor(red) msymbol(S)) ///
	ciopts1(lcolor(blue)) ///
	ciopts2(lcolor(red)) yline(0) ///
	note("Note: 95% confidence intervals based on bootstrapped standard errors.") ///
	legend(on rows(2) order(2  "Non-Significant Coefficients" 4 "Significant Coefficients (after FDR corrections)")) ///
	xtitle("Pre-Treatment Blower Door (CFM50)") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("Effect on Gap (%)" "(compared to median pre blower door)") yline(0) ///
	xlabel(1 "<2000" 2 "2001-3000" 3 "3001-4000" 4 "4001-5000" 5 "5001-6000" 6 "6001-7000" ///
	7 "7001-8000" 8 "8001-9000" 9 "9001-10000" 10 ">10000" , labsize(small) angle(45))
graph export "./Regression Outputs/Heterogeneity/gap_blowerpre.png", replace width(5000)


********************************************************************************
********************************************************************************
********************************************************************************
********************* DESCRIPTIVE STATISTICS ***********************************

clear all
cd "P:\IHWAP\Temp Data"
use "IHWAP_gap.dta"

matrix drop _all
matrix input descr_mat1 = (.,.,.,.)
foreach x in total_mmbtu gas_mmbtu electric_mmbtu tmin tmax precip {
ttest `x' if regsample==1, by(treated) unequal
sca bot`x' = `r(mu_1)'
sca top`x' = `r(mu_2)'
sca diff`x' = `r(mu_1)' - `r(mu_2)'
sca pval`x' = `r(p)'
matrix mat`x' = (bot`x' , top`x', diff`x', pval`x')
matrix descr_mat1 = (descr_mat1 \ mat`x')
}
count if regsamp==1 & treated==0
sca obs_pre = `r(N)'
count if regsamp==1 & treated==1
sca obs_post = `r(N)'
mat obs = (obs_pre, obs_post, . , .)
matrix descr_mat1 = (descr_mat1 \ obs)

matrix colnames descr_mat1 = "Pre-WAP" "Post-WAP" "Difference" "P-value of Diff." 

matrix rownames descr_mat1 = "" "Monthly Energy Usage (MMBtu)" "Monthly Gas Usage (MMBtu)" "Monthly Elec. Usage (MMBtu)" ///
		 "Min. Temperature (C)" "Max. Temperature (C)" "Monthly Precipitation (in.)" "Number of Observations"

esttab matrix(descr_mat1, fmt("%9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.0fc" /// 
	"%9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.0fc" ///
	"%9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.0fc" ///
	"%9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %9.3f")) ///
	using "./Regression Outputs/Heterogeneity/ttests_avguse.tex", replace


*** covariate means with state sample
count if ww_treat==1 & ProgramYear>=2009
sca homes_util = `r(N)'

replace sqfeet = . if sqfeet==0
winsor2 sqfeet, cuts(0.5 99.5) trim replace

gen pct_BlowerReduc = BlowerReduc/Blower_Pre

gen atticR = Attic_RValue
replace atticR = . if atticR==0

gen liheap = 0 if liheapemergency=="False"
replace liheap = 1 if liheapemergency=="True"

gen tabbuilddateX = 0 if builddate!=.
replace tabbuilddateX = 1 if tab_builddate11==1 | tab_builddate12==1 | tab_builddate13==1

matrix input ttest_mat = (.,.,.,.)
foreach x in Real_income1000 occupants Age female renter white black hispanic nativeamerican otherrace ///
		haselderly hasminor priority liheap Blower_Pre Blower_Post BlowerReduc pct_BlowerReduc ///
		atticR sqfeet bedrooms nstories tab_builddate1 tab_builddate2 tab_builddate3 tab_builddate4 ///
		tab_builddate5  tab_builddate6 tab_builddate7 tab_builddate8 tab_builddate9 tab_builddate10 tabbuilddateX ///
		Real_total_cost Real_tot_actAirCon Real_tot_actAirSeal Real_tot_actAttic  Real_tot_actBaseload Real_tot_actDoor ///
		Real_tot_actFoundation Real_tot_actFurnace Real_tot_actGeneral Real_tot_actHealSfty ///
		Real_tot_actWallIns Real_tot_actWtHtr Real_tot_actWindow {

sum `x' if ww_treat==1 & ProgramYear>=2009 , detail
sca mean`x' = `r(mean)'
sca sd`x' = `r(sd)'
sca min`x' = `r(min)'
sca max`x' = `r(max)'
matrix mat`x' = (mean`x' , sd`x', min`x', max`x')
matrix ttest_mat = (ttest_mat \ mat`x')
}
matrix homes = (homes_util,.,.,.)
matrix ttest_mat = (ttest_mat \ homes)

matrix colnames ttest_mat = "Average" "Standard Deviation" "Min" "Max" 

matrix rownames ttest_mat = "" "Income ($/1000)" "N Occupants" "Householder Age" "Female Householder (%)" ///
		"Renter (%)" "White (%)" "Black (%)" "Hispanic (%)" "Native American (%)" "Other Race (%)" ///
		"Seniors 65+ (%)" "Children Under 18 (%)" "Priority" "LIHEAP (%)" "Blower Door Pre (CFM50)" ///
		"Blower Door Post (CFM50)" "Blower Door Reduced (CFM50)" "Percent Blower Door Reduced (%)" ///
		"Attic R-Value" "Floor Area (sqft)" "N Bedrooms" "Has Multiple Stories (%)" "Built Pre-1900 (%)" ///
		"Built 1900-1909 (%)" "Built 1910-1919 (%)" "Built 1920-1929 (%)" "Built 1930-1939 (%)" ///
		"Built 1940-1949 (%)" "Built 1950-1959 (%)" "Built 1960-1969 (%)" "Built 1970-1977 (%)" ///
		"Built 1978-1989 (%)" "Built 1990-Present (%)" "Total" "Air Conditioning" "Air Sealing" ///
		"Attic" "Baseload" "Door" "Foundation" "Furnace" "General" "Health and Safety" "Wall Insulation" ///
		"Water Heater" "Window" "Number of Homes"

esttab matrix(ttest_mat, ///
	fmt("%9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f %9.0fc")) ///
	using "./Regression Outputs/Heterogeneity/ttests_descriptives.tex", replace


************************************************	
**** GRAPH ILLUSTRATING WEATHERWORKS BIAS
sort Household meterend

** identify homes with a full year of pre or post treatment data
by Household : gen days_diff = meterend - meterend[_n-12]
by Household : gen days_diff_post = meterend[_n+12] - meterend
by Household : gen year_home_pre = 1 if days_diff>=360 & days_diff<=370 & treated==0 & treated[_n+1]==.
by Household : egen full_year_pre = max(year_home_pre)
drop year_home_pre
by Household : gen year_home_post = 1 if days_diff_post>=360 & days_diff_post<=370 & treated==1 & treated[_n-1]==.
by Household : egen full_year_post = max(year_home_post)
drop year_home_post

** total energy consumption for a full year pre or post treatment
gen mmbtu_year_pre = total_mmbtu + total_mmbtu[_n-1] + total_mmbtu[_n-2] + total_mmbtu[_n-3] + total_mmbtu[_n-4] + total_mmbtu[_n-5] + ///
	total_mmbtu[_n-6] + total_mmbtu[_n-7] + total_mmbtu[_n-8] + total_mmbtu[_n-9] + total_mmbtu[_n-10] + total_mmbtu[_n-11] ///
	if treated==0 & treated[_n+1]==. & full_year_pre==1

gen mmbtu_year_post = total_mmbtu + total_mmbtu[_n+1] + total_mmbtu[_n+2] + total_mmbtu[_n+3] + total_mmbtu[_n+4] + total_mmbtu[_n+5] + ///
	total_mmbtu[_n+6] + total_mmbtu[_n+7] + total_mmbtu[_n+8] + total_mmbtu[_n+9] + total_mmbtu[_n+10] + total_mmbtu[_n+11] ///
	if treated==1 & treated[_n-1]==. & full_year_post==1

gen usageratio_pre = (ExistingConsumption/1000000)/(mmbtu_year_pre)
gen usageratio_post = (ProjectedConsumption/1000000)/(mmbtu_year_post)

foreach x in usageratio_pre usageratio_post {
winsor2 `x', replace cuts(0.5 99.5)
}

****** Plotting the graphs of ratio Wx Usage/True Usage
**** True usage on X-axis
gen tot_year_prebins = .
forval i = 40(20)180 {
replace tot_year_prebins = `i' if mmbtu_year_pre>=`i' & mmbtu_year_pre<`i'+20
}
replace tot_year_prebins = 20 if mmbtu_year_pre<40
replace tot_year_prebins = 200 if mmbtu_year_pre>=200 & mmbtu_year_pre!=.
bysort Household : egen total_year_prebins = max(tot_year_prebins)
drop tot_year_prebins

reg usageratio_pre ibn.total_year_prebins, nocons
est sto usageratio_pre
reg usageratio_post ibn.total_year_prebins, nocons
est sto usageratio_post

coefplot (usageratio_pre, msymbol(S)) (usageratio_post), offset(0) vertical xlabel(1 "<40" 2 "40-60" 3 "60-80" 4 "80-100" 5 "100-120" ///
										6 "120-140" 7 "140-160" 8 "160-180" 9 "180-200" 10 ">=200", labsize(small) angle(45)) ///
										ylabel(0(1)6) ///
										xtitle("Average Yearly Pre-Treatment Energy Consumption (MMBtu)") ///
										ytitle("Ratio = Modelled/Actual Energy Usage") legend(order(2 "Pre-Treatment" 4 "Post-Treatment")) ///
										rename(*.total_year_prebins = .total_year_postbins) ///
										yline(1) graphregion(color(white)) bgcolor(white)
graph export "./Regression Outputs/Heterogeneity/ratio_totalbins.png", replace

********************************************************************************
********************************************************************************
********************************************************************************
********************** COST-BENEFIT ANALYSIS ***********************************

/* Note: for CBA we do not consider Health and Safety costs 
(will be excluded from net present benefit calculations) */

clear all
cd "P:\IHWAP\Temp Data"
use "IHWAP_gap.dta"


* cost share for each upgrade category (excluding H&S
foreach x in Attic AirCon AirSeal Baseload Door WtHtr Foundation General Window Furnace WallIns {
gen `x'_costshare = tot_act`x'/nonHS_cost
}
egen sharecheck = rowtotal(*costshare)

** cost-weighted average of lifetime of retrofits for each home
* based on WeatherWorks documentation: "Table SIR1 - Retrofit Service Lives"
gen avg_lifetime = 20*AirSeal_costshare + 25*WallIns_costshare + 25*Attic_costshare + ///
	15*Window_costshare + 15*Door_costshare + 15*Baseload_costshare + 15*AirCon_costshare + ///
	20*Furnace_costshare + 25*Foundation_costshare + 10*General_costshare + 15*WtHtr_costshare ///
	if sharecheck>0.98
gen round_lifetime = round(avg_lifetime)
gen avg_lifetime_months = avg_lifetime*12
replace avg_lifetime_months = round(avg_lifetime_months)


** gas savings vs electric savings
reghdfe total_mmbtu i.treated c.HDD60 c.HDD60#c.HDD60 c.CDD75 c.CDD75#c.CDD75 c.tmin c.tmax c.precip if twoyears_prepost==1 & meterdays_monthly!=., absorb(i.Household#i.end_month i.month_year#i.CountyID) vce(cluster Household end_month)
matrix temp = e(b)
sca tot_save = temp[1,2]
reghdfe gas_mmbtu i.treated c.HDD60 c.HDD60#c.HDD60 c.CDD75 c.CDD75#c.CDD75 c.tmin c.tmax c.precip if twoyears_prepost==1 & meterdays_monthly!=., absorb(i.Household#i.end_month i.month_year#i.CountyID) vce(cluster Household end_month)
matrix temp = e(b)
sca gas_save = temp[1,2]
reghdfe electric_mmbtu i.treated c.HDD60 c.HDD60#c.HDD60 c.CDD75 c.CDD75#c.CDD75 c.tmin c.tmax c.precip if twoyears_prepost==1 & meterdays_monthly!=., absorb(i.Household#i.end_month i.month_year#i.CountyID) vce(cluster Household end_month)
matrix temp = e(b)
sca elec_save = temp[1,2]

* references electric: 
* https://www.eia.gov/electricity/state/illinois/
* https://www.eia.gov/energyexplained/index.php?page=about_energy_conversion_calculator#eleccalc
sca elec_cents_mmbtu = 9.49 / 0.003412
sca elec_doll_mmbtu = elec_cents_mmbtu/100

* references gas: 
* https://www.eia.gov/dnav/ng/ng_pri_sum_dcu_SIL_a.htm
* https://www.eia.gov/tools/faqs/faq.php?id=45&t=8
sca gas_doll_mmbtu = 8.83 / 1.037

* price escalation based on "Energy Price Indices and Discount Factors..."
* https://nvlpubs.nist.gov/nistpubs/ir/2018/NIST.IR.85-3273-33.pdf
local counter = 0
foreach x in 1.02 1.07 1.09 1.09 1.09 1.09 1.10 1.10 1.09 1.09 1.09 1.09 1.10 1.10 1.10 ///
	1.10 1.09 1.09 1.09 1.09 1.09 1.08 1.08 1.08 1.07 1.07 1.07 1.07 1.06 1.06 {
	local counter = `counter'+1
	sca elec_doll_mmbtu`counter' = `x'*elec_doll_mmbtu
	di elec_doll_mmbtu`counter'
}
local counter = 0
foreach x in 1.03 1.06 1.08 1.09 1.12 1.13 1.14 1.15 1.16 1.16 1.17 1.17 1.17 1.18 1.18 ///
	1.19 1.19 1.20 1.21 1.22 1.22 1.23 1.23 1.24 1.25 1.25 1.26 1.27 1.28 1.29 {
	local counter = `counter'+1
	sca gas_doll_mmbtu`counter' = `x'*gas_doll_mmbtu
	di gas_doll_mmbtu`counter'
}

******** Program-wide cost-effectiveness
**** for "baseline" assuming weighted avg. lifetime of retrofits, and 3% discount
sca elec_contrib = elec_save/tot_save
sca rate = 0.03

gen tot_baseline_benefits = 0 if ww_treat==1
forval i = 1/25 {
gen baseline_benefits`i' = elec_contrib*(-12)*realized_savings_mmbtu*elec_doll_mmbtu`i'/((1+rate)^`i') ///
			+ (1-elec_contrib)*(-12)*realized_savings_mmbtu*gas_doll_mmbtu`i'/((1+rate)^`i') if ww_treat==1
replace baseline_benefits`i' = 0 if round_lifetime<`i' & baseline_benefits`i'!=.
replace tot_baseline_benefits = tot_baseline_benefits + baseline_benefits`i'
drop baseline_benefits`i'	
}
gen baseline_npv = tot_baseline_benefits - Real_nonHS_cost if ww_treat==1
sum baseline_npv if ww_treat==1, detail

* bootstrap baseline NPV
forval j = 1/150 {
	gen tot_baseline_benefits`j' = 0 if ww_treat==1
	forval i = 1/25 {
	gen baseline_benefits`i' = elec_contrib*(-12)*realized_savings_mmbtu`j'*elec_doll_mmbtu`i'/((1+rate)^`i') ///
				+ (1-elec_contrib)*(-12)*realized_savings_mmbtu`j'*gas_doll_mmbtu`i'/((1+rate)^`i') if ww_treat==1
	replace baseline_benefits`i' = 0 if round_lifetime<`i' & baseline_benefits`i'!=.
	replace tot_baseline_benefits`j' = tot_baseline_benefits`j' + baseline_benefits`i'	
	drop baseline_benefits`i'	
	}
	gen baseline_npv`j' = tot_baseline_benefits`j' - Real_nonHS_cost if ww_treat==1
	sum baseline_npv`j' if ww_treat==1, detail
}


**** 6% discount rate
sca elec_contrib = elec_save/tot_save
sca rate = 0.06
gen tot_disc6_benefits = 0 if ww_treat==1
forval i = 1/25 {
gen disc6_benefits`i' = elec_contrib*(-12)*realized_savings_mmbtu*elec_doll_mmbtu`i'/((1+rate)^`i') ///
			+ (1-elec_contrib)*(-12)*realized_savings_mmbtu*gas_doll_mmbtu`i'/((1+rate)^`i') if ww_treat==1
replace disc6_benefits`i' = 0 if round_lifetime<`i' & disc6_benefits`i'!=.
replace tot_disc6_benefits = tot_disc6_benefits + disc6_benefits`i'
drop disc6_benefits`i'
}
gen disc6_npv = tot_disc6_benefits - Real_nonHS_cost if ww_treat==1
sum disc6_npv if ww_treat==1, detail

/* will not bootstrap alternative scenarios for now
forval j = 1/150 {
	gen tot_disc6_benefits`j' = 0 if ww_treat==1
	forval i = 1/25 {
	gen disc6_benefits`i' = elec_contrib*(-12)*realized_savings_mmbtu*elec_doll_mmbtu/((1+rate)^`i') ///
				+ (1-elec_contrib)*(-12)*realized_savings_mmbtu*gas_doll_mmbtu/((1+rate)^`i') if ww_treat==1
	replace disc6_benefits`i' = 0 if round_lifetime<`i' & disc6_benefits`i'!=.
	replace tot_disc6_benefits`j' = tot_disc6_benefits`j' + disc6_benefits`i'
	drop disc6_benefits`i'
	}
	gen disc6_npv`j' = tot_disc6_benefits`j' - Real_nonHS_cost if ww_treat==1
	sum disc6_npv`j' if ww_treat==1, detail
}
*/

**** 0% discount rate
sca elec_contrib = elec_save/tot_save
sca rate = 0
gen tot_disc0_benefits = 0 if ww_treat==1
forval i = 1/25 {
gen disc0_benefits`i' = elec_contrib*(-12)*realized_savings_mmbtu*elec_doll_mmbtu`i'/((1+rate)^`i') ///
			+ (1-elec_contrib)*(-12)*realized_savings_mmbtu*gas_doll_mmbtu`i'/((1+rate)^`i') if ww_treat==1
replace disc0_benefits`i' = 0 if round_lifetime<`i' & disc0_benefits`i'!=.
replace tot_disc0_benefits = tot_disc0_benefits + disc0_benefits`i'
drop disc0_benefits`i'
}
gen disc0_npv = tot_disc0_benefits - Real_nonHS_cost if ww_treat==1
sum disc0_npv if ww_treat==1, detail


**** 30 years lifetime
sca elec_contrib = elec_save/tot_save
sca rate = 0.03
gen tot_year30_benefits = 0 if ww_treat==1
forval i = 1/30 {
gen year30_benefits`i' = elec_contrib*(-12)*realized_savings_mmbtu*elec_doll_mmbtu`i'/((1+rate)^`i') ///
			+ (1-elec_contrib)*(-12)*realized_savings_mmbtu*gas_doll_mmbtu`i'/((1+rate)^`i') if ww_treat==1
replace tot_year30_benefits = tot_year30_benefits + year30_benefits`i'
drop year30_benefits`i'
}
gen year30_npv = tot_year30_benefits - Real_nonHS_cost if ww_treat==1
sum year30_npv if ww_treat==1, detail


**** 10 years lifetime
sca elec_contrib = elec_save/tot_save
sca rate = 0.03
gen tot_year10_benefits = 0 if ww_treat==1
forval i = 1/10 {
gen year10_benefits`i' = elec_contrib*(-12)*realized_savings_mmbtu*elec_doll_mmbtu`i'/((1+rate)^`i') ///
			+ (1-elec_contrib)*(-12)*realized_savings_mmbtu*gas_doll_mmbtu`i'/((1+rate)^`i') if ww_treat==1
replace tot_year10_benefits = tot_year10_benefits + year10_benefits`i'
drop year10_benefits`i'
}
gen year10_npv = tot_year10_benefits - Real_nonHS_cost if ww_treat==1
sum year10_npv if ww_treat==1, detail

save "tempdata.dta", replace


************************ graphs illustrating CBA
clear all
set maxvar 100000
cd "P:\IHWAP\Temp Data"
use "tempdata.dta"

gen rsq_pre = max if treated==0
bysort jobnumber : egen rsquare_pre = max(rsq_pre)
gen rsq_post = max if treated==1
bysort jobnumber : egen rsquare_post = max(rsq_post)

gen hd_pre = HDDbest if treated==0
bysort jobnumber : egen hdd_pre = max(hd_pre)
gen hd_post = HDDbest if treated==1
bysort jobnumber : egen hdd_post = max(hd_post)

keep if ww_treat==1
keep jobnumber Household baseline_npv* disc6_npv disc0_npv year30_npv year10_npv ///
	tot_baseline_benefits* tot_disc6_benefits tot_disc0_benefits tot_year30_benefits tot_year10_benefits ///
	Real_total_cost Real_nonHS_cost savings_gap rsquare_pre rsquare_post hdd_pre hdd_post
duplicates drop
keep if baseline_npv!=.

sort baseline_npv
cumul baseline_npv, gen(pctile)
replace pctile = round(pctile, 0.01)
tostring pctile, replace force format(%9.2f)

sort year30_npv
cumul year30_npv, gen(pctile_year30)
replace pctile_year30 = round(pctile_year30, 0.01)
tostring pctile_year30, replace force format(%9.2f)

sort year10_npv
cumul year10_npv, gen(pctile_year10)
replace pctile_year10 = round(pctile_year10, 0.01)
tostring pctile_year10, replace force format(%9.2f)

sort disc6_npv
cumul disc6_npv, gen(pctile_disc6)
replace pctile_disc6 = round(pctile_disc6, 0.01)
tostring pctile_disc6, replace force format(%9.2f)

sort disc0_npv
cumul disc0_npv, gen(pctile_disc0)
replace pctile_disc0 = round(pctile_disc0, 0.01)
tostring pctile_disc0, replace force format(%9.2f)

sort baseline_npv
gen obsid = _n
*findit labmask
labmask obsid, values(pctile)

sort year30_npv
gen obsid30 = _n

sort year10_npv
gen obsid10 = _n

sort disc6_npv
gen obsid6 = _n

sort disc0_npv
gen obsid0 = _n

replace baseline_npv = baseline_npv/1000
replace year30_npv = year30_npv/1000
replace year10_npv = year10_npv/1000
replace disc6_npv = disc6_npv/1000
replace disc0_npv = disc0_npv/1000

gsort -baseline_npv
gen baseline_ce = sum(baseline_npv)
* obsid 722
sort baseline_ce
sum obsid if baseline_ce>0 & baseline_ce[_n-1]<0
sca obs_base_ce = `r(mean)'

gsort -year30_npv
gen year30_ce = sum(year30_npv)
sort year30_ce
sum obsid if year30_ce>0 & year30_ce[_n-1]<0
sca obs_year30_ce = `r(mean)'

gsort -year10_npv
gen year10_ce = sum(year10_npv)
sort year10_ce
sum obsid if year10_ce>0 & year10_ce[_n-1]<0
sca obs_year10_ce = `r(mean)'

gsort -disc6_npv
gen disc6_ce = sum(disc6_npv)
* obsid 2116
sort disc6_ce
sum obsid if disc6_ce>0 & disc6_ce[_n-1]<0
sca obs_disc6_ce = `r(mean)'

gsort -disc0_npv
gen disc0_ce = sum(disc0_npv)
sort disc0_ce
sum obsid if disc0_ce>0 & disc0_ce[_n-1]<0
*sca obs_disc0_ce = `r(mean)'
* all homes are cost-effective


twoway (scatter baseline_npv obsid, msize(vsmall) mcolor(black)) || ///
	(scatter year30_npv obsid30, msize(vsmall) mcolor(green)) || ///
	(scatter year10_npv obsid10, msize(vsmall) mcolor(purple)) || ///
	(scatter disc6_npv obsid6, msize(vsmall) mcolor(red)) ///	
	, xtitle("Home Percentile Rank") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("WAP Net Present Benefits ($1000)") ///
	xlabel(30(500)7530, labsize(small) angle(45) valuelabel) ///
	ylabel(-40(10)35, labsize(small)) ///
	xline(`=scalar(obs_base_ce)', lcolor(black)) ///
	xlabel(`=scalar(obs_base_ce)', add custom labcolor(black) labsize(vsmall) format(%6.0f) angle(45)) ///
	text(20 `=scalar(obs_base_ce)-0.05' "Baseline", placement(w) color(black) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_disc6_ce)',  lcolor(red) lpattern(dash_dot)) ///
	xlabel(`=scalar(obs_disc6_ce)', add custom labcolor(red) labsize(vsmall) format(%6.0f) angle(45)) ///
	text(20 `=scalar(obs_disc6_ce)-0.05' "6% Discount", placement(w) color(red) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_year10_ce)',  lcolor(purple) lpattern(shortdash)) ///
	xlabel(`=scalar(obs_year10_ce)', add custom labcolor(purple) labsize(vsmall) format(%6.0f) angle(45)) ///
	text(20 `=scalar(obs_year10_ce)-0.05' "10 Years Lifetime", placement(w) color(purple) size(small) just(left) orient(vertical)) ///
	xline(0,  lcolor(green) lpattern(dash)) ///
	text(20 `=0+0.03' "30 Years Lifetime" "or No Discount", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	legend(order(1 "Baseline" 2 "30 Years Lifetime" 3 "10 Years Lifetime" 4 "6% Discount"))
graph export "./Regression Outputs/Heterogeneity/programwideCE.png", replace


gsort -baseline_npv
sum obsid if pctile=="0.25" & pctile[_n-1]=="0.26"
sca obs_bot = `r(mean)'
dis obs_bot

sum obsid if pctile=="0.75" & pctile[_n-1]=="0.76"
sca obs_top = `r(mean)'
dis obs_top

twoway (scatter baseline_npv obsid, msize(vsmall) mcolor(black)) || ///
	(pcarrowi 5 `=scalar(obs_bot-300)' 5 `=scalar(obs_bot)-600', color(red)) ///
	(pcarrowi 5 `=scalar(obs_top+300)' 5 `=scalar(obs_top)+600', color(green)) ///
	, xtitle("Home Percentile Rank") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("WAP Net Present Benefits ($1000)") ///
	xlabel(0(1100)7800, labsize(small) angle(45) valuelabel) ///
	ylabel(-40(10)35, labsize(small)) ///
	xline(`=scalar(obs_bot)', lcolor(red) lpattern(dash)) ///
	xlabel(`=scalar(obs_bot)', add custom labcolor(red) labsize(vsmall) format(%6.0f) angle(45)) ///
	text(20 `=scalar(obs_bot)-0.07' "Least cost-effective quartile", placement(w) color(red) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_top)', lcolor(green) lpattern(shortdash_dot)) ///
	xlabel(`=scalar(obs_top)', add custom labcolor(green) labsize(vsmall) format(%6.0f) angle(45)) ///
	text(20 `=scalar(obs_top)+0.07' "Most cost-effective quartile", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	legend(order(1 "Home NPV (Baseline Assumptions)"))
graph export "./Regression Outputs/Heterogeneity/programwideCE_cutoffs.png", replace


gsort -baseline_npv
sum obsid if pctile=="0.25" & pctile[_n-1]=="0.26"
sca obs_bot25 = `r(mean)'
dis obs_bot25

sum obsid if pctile=="0.10" & pctile[_n-1]=="0.11"
sca obs_bot10 = `r(mean)'
dis obs_bot10

sum obsid if pctile=="0.05" & pctile[_n-1]=="0.06"
sca obs_bot5 = `r(mean)'
dis obs_bot5

sum obsid if pctile=="0.01" & pctile[_n-1]=="0.02"
sca obs_bot1 = `r(mean)'
dis obs_bot1

sum obsid if pctile=="0.75" & pctile[_n-1]=="0.76"
sca obs_top25 = `r(mean)'
dis obs_top25

sum obsid if pctile=="0.90" & pctile[_n-1]=="0.91"
sca obs_top10 = `r(mean)'
dis obs_top10

sum obsid if pctile=="0.95" & pctile[_n-1]=="0.96"
sca obs_top5 = `r(mean)'
dis obs_top5

sum obsid if pctile=="0.99" & pctile[_n-1]=="1.00"
sca obs_top1 = `r(mean)'
dis obs_top1


egen max_ce = max(baseline_ce)
sum obsid if max_ce==baseline_ce, detail
sca obs_maxce = `r(p1)'
dis obs_maxce

sum baseline_npv, detail
sca meannpv = round(`r(mean)', 0.1)
dis meannpv

twoway (scatter baseline_npv obsid, msize(vsmall) mcolor(black)) ///
	(pcarrowi -15 `=scalar(obs_top25+50)' -15 `=scalar(obs_top25)+350', color(green)) ///
	(pcarrowi -10 `=scalar(obs_maxce+50)' -10 `=scalar(obs_maxce)+350', color(red)) ///	
	(pcarrowi 5 `=scalar(obs_bot25-50)' 5 `=scalar(obs_bot25)-350', color(green)) ///		
	, xtitle("Home Percentile Rank") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("WAP Net Present Benefits ($1000)") ///
	xlabel(0(1190)7140, labsize(small) angle(45) valuelabel) ///
	ylabel(-40(10)35, labsize(small)) ///
	xline(`=scalar(obs_top25)', lcolor(green) lpattern(shortdash_dot)) ///
	text(-30 `=scalar(obs_top25)+0.07' "Top 25% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_top10)', lcolor(green) lpattern(shortdash_dot)) ///
	text(-30 `=scalar(obs_top10)+0.07' "Top 10% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_top5)', lcolor(green) lpattern(shortdash_dot)) ///
	text(-30 `=scalar(obs_top5)+0.07' "Top 5% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_top1)', lcolor(green) lpattern(shortdash_dot)) ///
	text(-30 `=scalar(obs_top1)+0.07' "Top 1% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_bot25)', lcolor(green) lpattern(shortdash_dot)) ///
	text(20 `=scalar(obs_bot25)-0.07' "Bottom 25% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_bot10)', lcolor(green) lpattern(shortdash_dot)) ///
	text(20 `=scalar(obs_bot10)-0.07' "Bottom 10% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_bot5)', lcolor(green) lpattern(shortdash_dot)) ///
	text(20 `=scalar(obs_bot5)-0.07' "Bottom 5% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_bot1)', lcolor(green) lpattern(shortdash_dot)) ///
	text(20 `=scalar(obs_bot1)-0.07' "Bottom 1% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_maxce)', lcolor(red) lpattern(dash)) ///
	text(20 `=scalar(obs_maxce)+0.07' "Max. Net Present Benefits", placement(e) color(red) size(small) just(left) orient(vertical)) ///
	yline(`=scalar(meannpv)', lcolor(red)) ///
	text(-2 `=scalar(obs_top25)+0.07' "Avg. Benefits = `=scalar(meannpv)'", placement(s) color(red) size(small) just(left)) ///
	legend(off) 	
graph export "./Regression Outputs/Heterogeneity/programwideCE_tophomes.png", replace width(5000)


***** sorted NPV with sample restrictions based on PRISM
gen prismrest = 0 if rsquare_pre!=. & rsquare_post!=. & hdd_pre!=. & hdd_post!=.
replace prismrest = 1 if hdd_pre>=43 & hdd_pre<73 & hdd_post>=43 & hdd_post<73 & ///
		rsquare_pre>=0.85 & rsquare_pre!=. & rsquare_post>=0.85 & rsquare_post!=.
		
sort baseline_npv
cumul baseline_npv if prismrest==1, gen(prism_pctile)
replace prism_pctile = round(prism_pctile, 0.01)
tostring prism_pctile, replace force format(%9.2f)

sort prism_pctile baseline_npv
gen prism_obsid = _n if prism_pctile!="."
labmask prism_obsid, values(prism_pctile)

gsort -prism_pctile -baseline_npv
sum prism_obsid if prism_pctile=="0.25" & prism_pctile[_n-1]=="0.26"
sca obs_bot25 = `r(mean)'
dis obs_bot25

sum prism_obsid if prism_pctile=="0.10" & prism_pctile[_n-1]=="0.11"
sca obs_bot10 = `r(mean)'
dis obs_bot10

sum prism_obsid if prism_pctile=="0.05" & prism_pctile[_n-1]=="0.06"
sca obs_bot5 = `r(mean)'
dis obs_bot5

sum prism_obsid if prism_pctile=="0.01" & prism_pctile[_n-1]=="0.02"
sca obs_bot1 = `r(mean)'
dis obs_bot1

sum prism_obsid if prism_pctile=="0.75" & prism_pctile[_n-1]=="0.76"
sca obs_top25 = `r(mean)'
dis obs_top25

sum prism_obsid if prism_pctile=="0.90" & prism_pctile[_n-1]=="0.91"
sca obs_top10 = `r(mean)'
dis obs_top10

sum prism_obsid if prism_pctile=="0.95" & prism_pctile[_n-1]=="0.96"
sca obs_top5 = `r(mean)'
dis obs_top5

sum prism_obsid if prism_pctile=="0.99" & prism_pctile[_n-1]=="1.00"
sca obs_top1 = `r(mean)'
dis obs_top1

gen prism_baseline_ce = sum(baseline_npv)
egen prism_max_ce = max(prism_baseline_ce) if prismrest==1
sum prism_obsid if prism_max_ce==prism_baseline_ce, detail
sca obs_maxce = `r(p1)'
dis obs_maxce

sum baseline_npv if prismrest==1, detail
sca meannpv = round(`r(mean)', 0.01)
dis meannpv

sort prism_obsid
twoway (scatter baseline_npv prism_obsid if prismrest==1, msize(vsmall) mcolor(black)) ///
	(pcarrowi -15 `=scalar(obs_top25+50)' -15 `=scalar(obs_top25)+350', color(green)) ///
	(pcarrowi -10 `=scalar(obs_maxce+50)' -10 `=scalar(obs_maxce)+350', color(red)) ///	
	(pcarrowi 5 `=scalar(obs_bot25-50)' 5 `=scalar(obs_bot25)-350', color(green)) ///		
	, xtitle("Home Percentile Rank") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("WAP Net Present Benefits ($1000)") ///
	xlabel(4447(200)7916, labsize(small) angle(45) valuelabel) ///
	ylabel(-40(10)35, labsize(small)) ///
	xline(`=scalar(obs_top25)', lcolor(green) lpattern(shortdash_dot)) ///
	text(-30 `=scalar(obs_top25)+0.07' "Top 25% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_top10)', lcolor(green) lpattern(shortdash_dot)) ///
	text(-30 `=scalar(obs_top10)+0.07' "Top 10% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_top5)', lcolor(green) lpattern(shortdash_dot)) ///
	text(-30 `=scalar(obs_top5)+0.07' "Top 5% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_top1)', lcolor(green) lpattern(shortdash_dot)) ///
	text(-30 `=scalar(obs_top1)+0.07' "Top 1% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_bot25)', lcolor(green) lpattern(shortdash_dot)) ///
	text(20 `=scalar(obs_bot25)-0.07' "Bottom 25% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_bot10)', lcolor(green) lpattern(shortdash_dot)) ///
	text(20 `=scalar(obs_bot10)-0.07' "Bottom 10% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_bot5)', lcolor(green) lpattern(shortdash_dot)) ///
	text(20 `=scalar(obs_bot5)-0.07' "Bottom 5% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_bot1)', lcolor(green) lpattern(shortdash_dot)) ///
	text(20 `=scalar(obs_bot1)-0.07' "Bottom 1% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_maxce)', lcolor(red) lpattern(dash)) ///
	text(20 `=scalar(obs_maxce)+0.07' "Max. Net Present Benefits", placement(e) color(red) size(small) just(left) orient(vertical)) ///
	yline(`=scalar(meannpv)', lcolor(red)) ///
	text(-2 `=scalar(obs_top25)+0.07' "Avg. Benefits = `=scalar(meannpv)'", placement(s) color(red) size(small) just(left)) ///
	legend(off) 	
graph export "./Regression Outputs/Heterogeneity/prism_programwideCE.png", replace width(5000)



*****************************************************
********** histogram of gap by benefits
gen pct_gap = savings_gap*100
sum pct_gap
sca pctgap = round(`r(mean)', 0.1)
dis pctgap

gen pct_gap25 = pct_gap if pctile<"0.25"
gen pct_gap75 = pct_gap if pctile>="0.75" & pctile!=""
gen pct_gap50 = pct_gap if pctile>="0.25" & pctile<"0.75"

gen gap_bins = .
forval i = -40(10)80 {
	replace gap_bins = `i' if pct_gap>`i'-10 & pct_gap<=`i'
}
replace gap_bins = -50 if pct_gap<-50
replace gap_bins = 90 if pct_gap>80 & pct_gap!=.

gen gapbinlab = "<-50%" if gap_bins==-50
replace gapbinlab = "[-50% -40%)" if gap_bins==-40
replace gapbinlab = "[-40% -30%)" if gap_bins==-30
replace gapbinlab = "[-30% -20%)" if gap_bins==-20
replace gapbinlab = "[-20% -10%)" if gap_bins==-10
replace gapbinlab = "[-10% 0%)" if gap_bins==0
replace gapbinlab = "[0% 10%)" if gap_bins==10
replace gapbinlab = "[10% 20%)" if gap_bins==20
replace gapbinlab = "[20% 30%)" if gap_bins==30
replace gapbinlab = "[30% 40%)" if gap_bins==40
replace gapbinlab = "[40% 50%)" if gap_bins==50
replace gapbinlab = "[50% 60%)" if gap_bins==60
replace gapbinlab = "[60% 70%)" if gap_bins==70
replace gapbinlab = "[70% 80%)" if gap_bins==80
replace gapbinlab = ">80%" if gap_bins==90

labmask gap_bins, values(gapbinlab)

sum pct_gap

graph bar (count) pct_gap25 pct_gap50 pct_gap75 ///
		, over(gap_bins, label(angle(45) labsize(small))) stack graphregion(color(white)) bgcolor(white) ///
		legend(style(column) title("Home Cost-Effectiveness:", size(small)) ring(0) position(1) bmargin(large) ///
		order(1 "Bottom Quartile" 2 "Interquartile" 3 "Top Quartile") ///
		symysize(5)  symxsize(5)) bar(1, color(maroon)) bar(2, color(navy)) bar(3, color(forest_green)) ///
		ytitle("Number of Homes") b1title("Performance Gap (%)" "(projected - realized savings)")
graph export "./Regression Outputs/Heterogeneity/gap_histogram.png", replace width(5000)
		
/*
twoway (histogram pct_gap if ww_treat==1 & pct_gap>-50 & pct_gap<100 ///
			, frequency start(-50) width(5) ///
			graphregion(color(white)) bgcolor(white) bcolor(blue%30) ///
			xline(`=scalar(pctgap)') ///
			xlabel(-50(25)100, format(%6.0f)) ///
			xlabel(`=scalar(pctgap)', add custom labcolor(red) labsize(vsmall) format(%6.1f)) ///
			ylabel(, format(%8.0fc)) ytitle("Number of Homes") ///
			xtitle("Percent Gap Between Realized and Projected Savings")) ///
		(histogram pct_gap if ww_treat==1 & pct_gap>-50 & pct_gap<100 & pct_savings>=bottom25 & pct_savings!=. ///
			, frequency start(-50) width(5) ///
			graphregion(color(white)) bgcolor(white) bcolor(red%30)) ///
		(histogram pct_gap if ww_treat==1 & pct_gap>-50 & pct_gap<100 & pct_savings<top25 ///
			, frequency start(-50) width(5) ///
			graphregion(color(white)) bgcolor(white) bcolor(green%30)) ///
		, legend(order(1 "All Homes" 2 "Bottom 25% Cost-Effective" 3 "Top 25% Cost-Effective")) 
graph export "./Regression Outputs/Heterogeneity/gap_histogram2.png", replace
*/



*******************************************************
***** table for program-wide CBA

gen pctile_baseline = pctile
*** cumulative costs and benefits
foreach y in baseline year30 year10 disc6 disc0 {
sort pctile_`y'
gen tot_costs = sum(Real_nonHS_cost)
count
sca nhomes = `r(N)'
egen tot_`y'_costs = max(tot_costs)
replace tot_`y'_costs = tot_`y'_costs/1000000
drop tot_costs

foreach x in 01 05 10 25 {
gen tot_costs = sum(Real_nonHS_cost) if pctile_`y'>="0.`x'"
count if pctile_`y'>="0.`x'"
sca nhomes`x' = `r(N)'
egen tot_`y'_costs`x' = max(tot_costs)
replace tot_`y'_costs`x' = tot_`y'_costs`x'/1000000
drop tot_costs
}

gen tot_benefits = sum(tot_`y'_benefits)
egen cumul_`y'_benefits = max(tot_benefits)
replace cumul_`y'_benefits = cumul_`y'_benefits/1000000
drop tot_benefits

foreach x in 01 05 10 25 {
gen tot_benefits = sum(tot_`y'_benefits) if pctile_`y'>="0.`x'"
egen cumul_`y'_benefits`x' = max(tot_benefits)
replace cumul_`y'_benefits`x' = cumul_`y'_benefits`x'/1000000
drop tot_benefits
}

gen cumul_`y'_npv = cumul_`y'_benefits - tot_`y'_costs 

foreach x in 01 05 10 25 {
gen cumul_`y'_npv`x' = cumul_`y'_benefits`x' - tot_`y'_costs`x'
}
}


foreach y in baseline year30 year10 disc6 disc0 {
matrix input summary_mat = (.,.,.,.,.)
foreach x in tot_`y'_costs cumul_`y'_benefits cumul_`y'_npv {
sum `x'
sca `x'all = `r(mean)'
sum `x'01
sca `x'01 = `r(mean)'
sum `x'05
sca `x'05 = `r(mean)'
sum `x'10
sca `x'10 = `r(mean)'
sum `x'25
sca `x'25 = `r(mean)'
matrix mat`x' = (`x'all, `x'01, `x'05, `x'10, `x'25)
matrix summary_mat = (summary_mat \ mat`x')
}
matrix summary_mat = (summary_mat \ nhomes, nhomes01, nhomes05, nhomes10, nhomes25)

matrix colnames summary_mat = "Full Sample" "Top 99% Homes" "Top 95% Homes" "Top 90% Homes" "Top 75% Homes" 
matrix rownames summary_mat = "" "Total Costs (million $)" "Total Benefits (million $)" "Net Benefits (million $)" "Number of Homes"

esttab matrix(summary_mat, fmt("%9.2f %9.2f %9.2f %9.2f %9.0fc")) ///
		using "./Regression Outputs/Heterogeneity/programwideCE_`y'.tex", replace
}


** cutoffs for program-wide cost-effectiveness, added manually to results table
sort obsid
local pct_base = pctile[`=scalar(obs_base_ce)']
di `pct_base'

sort obsid10
local pct_year10 = pctile_year10[`=scalar(obs_year10_ce)']
di `pct_year10'

sort obsid30
local pct_year30 = pctile_year30[`=scalar(obs_year30_ce)']
di `pct_year30'

sort obsid6
local pct_disc6 = pctile_disc6[`=scalar(obs_disc6_ce)']
di `pct_disc6'


/*
*** ranking with bootstrapped standard errors
forval i = 1/150 {
replace baseline_npv`i' = baseline_npv`i'/1000
rename baseline_npv`i' boot_baseline_npv`i'
}

egen baseline_npv_sd = rowsd(boot_baseline_npv*)
gen baseline_npv_min95 = baseline_npv-1.96*baseline_npv_sd
gen baseline_npv_max95 = baseline_npv+1.96*baseline_npv_sd

sort obsid
eclplot baseline_npv baseline_npv_min95 baseline_npv_max95 obsid, ///
	estopts(lcolor(black) lwidth(thick))  ///
	legend(on order(1  "95% CI" 2 "Home NPB")) ///
	eplottype(line) rplottype(rarea) ///
	ciopts(fcolor(gray%75) lwidth(none)) ///
	xtitle("Home Percentile Rank") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("WAP Net Present Benefits ($1000)") ///
	xlabel(0(1190)7140, labsize(small) angle(45) valuelabel) ///
	ylabel(-40(10)35, labsize(small)) ///
	xline(`=scalar(obs_top25)', lcolor(green) lpattern(shortdash_dot)) ///
	text(-30 `=scalar(obs_top25)+0.07' "Top 25% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_top10)', lcolor(green) lpattern(shortdash_dot)) ///
	text(-30 `=scalar(obs_top10)+0.07' "Top 10% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_top5)', lcolor(green) lpattern(shortdash_dot)) ///
	text(-30 `=scalar(obs_top5)+0.07' "Top 5% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_top1)', lcolor(green) lpattern(shortdash_dot)) ///
	text(-30 `=scalar(obs_top1)+0.07' "Top 1% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_bot25)', lcolor(green) lpattern(shortdash_dot)) ///
	text(20 `=scalar(obs_bot25)-0.07' "Bottom 25% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_bot10)', lcolor(green) lpattern(shortdash_dot)) ///
	text(20 `=scalar(obs_bot10)-0.07' "Bottom 10% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_bot5)', lcolor(green) lpattern(shortdash_dot)) ///
	text(20 `=scalar(obs_bot5)-0.07' "Bottom 5% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_bot1)', lcolor(green) lpattern(shortdash_dot)) ///
	text(20 `=scalar(obs_bot1)-0.07' "Bottom 1% homes", placement(e) color(green) size(small) just(left) orient(vertical)) ///
	xline(`=scalar(obs_maxce)', lcolor(red) lpattern(dash)) ///
	text(20 `=scalar(obs_maxce)+0.07' "Max. Net Present Benefits", placement(e) color(red) size(small) just(left) orient(vertical)) ///
	addplot((pcarrowi 5 `=scalar(obs_bot25-50)' 5 `=scalar(obs_bot25)-350', color(green)) ///
	(pcarrowi -7 `=scalar(obs_top25+50)' -7 `=scalar(obs_top25)+350', color(green)) ///
	(pcarrowi -10 `=scalar(obs_maxce+50)' -10 `=scalar(obs_maxce)+350', color(red)))	
graph export "./Regression Outputs/Heterogeneity/programwideCE_tophomes_sd.png", replace
*/

********************************************************************************
*** ttests comparing low, high and interquartile CE homes
clear
cd "P:\IHWAP"
use "./Temp Data/Regression Outputs/Heterogeneity/contractor_rank.dta"

merge 1:m ContractorID using "./Temp Data/Contractors_by_jobnumber_long.dta"
keep if _merge==3
drop _merge

sort jobnumber
by jobnumber : egen contractor_quality = max(cont_quality)
*by jobnumber : egen contractor_quality_bayes = max(cont_quality_bayes)

keep jobnumber contractor_quality
*keep jobnumber contractor_quality contractor_quality_bayes
duplicates drop

xtile cont_decile = contractor_quality, nq(10)
xtile cont_quintile = contractor_quality, nq(5)
*xtile cont_decile_bayes = contractor_quality_bayes, nq(10)
*xtile cont_quintile_bayes = contractor_quality_bayes, nq(5)

merge 1:m jobnumber using "./Temp Data/tempdata.dta"
keep if _merge==3
drop _merge

replace sqfeet = . if sqfeet==0
winsor2 sqfeet, cuts(0.5 99.5) trim replace


*** generating average energy consumption variables
* winter (oct-mar)
foreach x in total gas electric {
bysort Household : egen `x'_w_pre = mean(`x'_mmbtu) if (end_month>9 | end_month <4) & end_month!=. & treated==0 & twoyears_prepost==1
bysort Household : egen `x'_winter_pre = max(`x'_w_pre)
drop `x'_w_pre
gen `x'bysqft_winter_pre = `x'_winter_pre*1000000/sqfeet
winsor2 `x'bysqft_winter_pre, cuts(0.5 99.5) trim replace
bysort Household : egen `x'_w_post = mean(`x'_mmbtu) if (end_month>9 | end_month <4) & end_month!=. & treated==1 & twoyears_prepost==1
bysort Household : egen `x'_winter_post = max(`x'_w_post)
drop `x'_w_post
gen `x'bysqft_winter_post = `x'_winter_post*1000000/sqfeet
winsor2 `x'bysqft_winter_post, cuts(0.5 99.5) trim replace
}

* summer (apr-sep)
foreach x in total gas electric {
bysort Household : egen `x'_s_pre = mean(`x'_mmbtu) if end_month>=4 & end_month<=9 & treated==0 & twoyears_prepost==1
bysort Household : egen `x'_summer_pre = max(`x'_s_pre)
gen `x'bysqft_summer_pre = `x'_summer_pre*1000000/sqfeet
winsor2 `x'bysqft_summer_pre, cuts(0.5 99.5) trim replace
drop `x'_s_pre
bysort Household : egen `x'_s_post = mean(`x'_mmbtu) if end_month>=4 & end_month<=9 & treated==1 & twoyears_prepost==1
bysort Household : egen `x'_summer_post = max(`x'_s_post)
gen `x'bysqft_summer_post = `x'_summer_post*1000000/sqfeet
drop `x'_s_post
winsor2 `x'bysqft_summer_post, cuts(0.5 99.5) trim replace
}

* number of observations per home pre and post
sort Household meterend
gen aux = 1
by Household : gen nobs_pre = sum(aux) if treated==0 & twoyears_prepost==1
by Household : egen totobs_pre = max(nobs_pre)
by Household : gen nobs_post = sum(aux) if treated==1 & twoyears_prepost==1
by Household : egen totobs_post = max(nobs_post)


* graphs comparing pre and post energy (high performance versus other homes)
sort baseline_npv
gen cumul_npv = sum(baseline_npv)
order cumul_npv, after(baseline_npv)
cumul baseline_npv, gen(pctile_npv)
order pctile_npv, after(cumul_npv)
gsort -baseline_npv
gen cumul_npv_neg = sum(baseline_npv)
order cumul_npv_neg, after(pctile_npv)
cumul baseline_npv, gen(pctile_npv_neg)
order pctile_npv_neg, after(cumul_npv_neg)

gen cutoff_pctile = pctile_npv_neg if cumul_npv_neg>0 & cumul_npv_neg[_n+1]<0
order cutoff_pctile, after(pctile_npv_neg)
sum cutoff_pctile
scalar cut = `r(mean)'
di scalar(cut)

bysort Household : egen npv_inv_pctile = max(pctile_npv_neg)

gen neg_save = 1 if npv_inv_pctile>scalar(cut) & npv_inv_pctile!=.
replace neg_save = 0 if npv_inv_pctile<=scalar(cut)

label define months 1 "jan" 2 "feb" 3 "mar" 4 "apr" 5 "may" 6 "jun" 7 "jul" 8 "aug" 9 "sep" 10 "oct" 11 "nov" 12 "dec"
label values end_month months

label var total_mmbtu "Combined Energy"
label var electric_mmbtu "Electricity"
label var gas_mmbtu "Gas"

foreach x in total electric gas {
gen `x'_kbtu_sqft = `x'_mmbtu*1000/sqfeet
winsor2 `x'_kbtu_sqft, cuts(0.5 99.5) trim replace
}


****************** t-tests comparing low-ce homes with rest of sample
***
** engineering PRISM related variables
gen maxpret = max if treated==0
bysort Household : egen max_pretreat = max(maxpret)
drop maxpret
gen maxpost = max if treated==1
bysort Household : egen max_posttreat = max(maxpost)
drop maxpost

gen HDDbestpret = HDDbest if treated==0
bysort Household : egen HDDbest_pretreat = max(HDDbestpret)
drop HDDbestpret
gen HDDbestpost = HDDbest if treated==1
bysort Household : egen HDDbest_posttreat = max(HDDbestpost)
drop HDDbestpost

** reduce to home level dataset
keep if ww_treat==1
duplicates drop
keep if baseline_npv!=.

* indicators for low versus high-performing homes
drop cumul_npv* pctile_npv*
sort baseline_npv
gen cumul_npv = sum(baseline_npv)
order cumul_npv, after(baseline_npv)
cumul baseline_npv, gen(pctile_npv)
order pctile_npv, after(cumul_npv)
gen lowperform = 0 if pctile_npv>0.25 & pctile_npv<=0.75
replace lowperform = 1 if pctile_npv<=0.25
gen highperform = 0 if pctile_npv>0.25 & pctile_npv<=0.75
replace highperform = 1 if pctile_npv>0.75 & pctile_npv!=.

* rescaling the NPV variable
gen npv1000 = baseline_npv/1000

* indicators for low PRIMS r-squared
gen lowrsq_pre = max_pretreat<0.95 if max_pretreat!=.
gen lowrsq_post = max_posttreat<0.95 if max_posttreat!=.

* ranking homes by NPV
sort baseline_npv
cumul baseline_npv, gen(pctile)
replace pctile = round(pctile, 0.01)
tostring pctile, replace force format(%9.2f)

sort baseline_npv
gen obsid = _n
*findit labmask
labmask obsid, values(pctile)

***** sample restrictions based on PRISM
gen prismrest = 0 if max_pretreat!=. & max_posttreat!=. & HDDbest_pretreat!=. & HDDbest_posttreat!=.
replace prismrest = 1 if HDDbest_pretreat>=43 & HDDbest_pretreat<73 & HDDbest_posttreat>=43 & HDDbest_posttreat<73 & ///
		max_pretreat>=0.85 & max_pretreat!=. & max_posttreat>=0.85 & max_posttreat!=.


*** correlate NPV and PRISM r-squared
twoway (scatter npv1000 max_pretreat if highperform==0, msize(vsmall) mcolor(black)) ///
	(scatter npv1000 max_pretreat if highperform==1, msize(vsmall) mcolor(green)) ///
	(scatter npv1000 max_pretreat if lowperform==1, msize(vsmall) mcolor(red)), ///
	ytitle("Baseline Model NPV (US$ 1000)") ///
	xtitle("PRISM R-Squared (Pre-Treatment)") ///
	graphregion(color(white)) bgcolor(white) ///
	legend(order(1 "Interquartile" 2 "Top 25%" 3 "Bottom 25%"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/corr_npv_rsq_pre.png", replace

twoway (scatter npv1000 max_posttreat if highperform==0, msize(vsmall) mcolor(black)) ///
	(scatter npv1000 max_posttreat if highperform==1, msize(vsmall) mcolor(green)) ///
	(scatter npv1000 max_posttreat if lowperform==1, msize(vsmall) mcolor(red)), ///
	ytitle("Baseline Model NPV (US$ 1000)") ///
	xtitle("PRISM R-Squared (Post-Treatment)") ///
	graphregion(color(white)) bgcolor(white) ///
	legend(order(1 "Interquartile" 2 "Top 25%" 3 "Bottom 25%"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/corr_npv_rsq_post.png", replace

 
*** correlate PRISM HDD pre and HDD post
twoway (scatter HDDbest_posttreat HDDbest_pretreat if highperform==0, msize(vsmall) mcolor(black)) ///
	(scatter HDDbest_posttreat HDDbest_pretreat if highperform==1, msize(vsmall) mcolor(green)) ///
	(scatter HDDbest_posttreat HDDbest_pretreat if lowperform==1, msize(vsmall) mcolor(red)) ///
	(lfit HDDbest_posttreat HDDbest_pretreat, lcolor(black)), ///
	ytitle("PRISM HDD* (Post-Treatment)") ///
	xtitle("PRISM HDD* (Pre-Treatment)") ///
	graphregion(color(white)) bgcolor(white) ///
	legend(order(1 "Interquartile" 2 "Top 25%" 3 "Bottom 25%"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/corr_hdd.png", replace

/*
twoway (sunflower HDDbest_posttreat HDDbest_pretreat if highperform==0, msize(vsmall) mcolor(black)) ///
	(sunflower HDDbest_posttreat HDDbest_pretreat if highperform==1, msize(vsmall) mcolor(green)) ///
	(sunflower HDDbest_posttreat HDDbest_pretreat if lowperform==1, msize(vsmall) mcolor(red)) ///
	(lfit HDDbest_posttreat HDDbest_pretreat, lcolor(black)), ///
	ytitle("PRISM HDD* (Post-Treatment)") ///
	xtitle("PRISM HDD* (Pre-Treatment)") ///
	graphregion(color(white)) bgcolor(white) ///
	legend(order(1 "Interquartile" 2 "Top 25%" 3 "Bottom 25%"))
*/

*** correlate PRISM r-squared pre and post
twoway (scatter max_posttreat max_pretreat if highperform==0, msize(vsmall) mcolor(black)) ///
	(scatter max_posttreat max_pretreat if highperform==1, msize(vsmall) mcolor(green)) ///
	(scatter max_posttreat max_pretreat if lowperform==1, msize(vsmall) mcolor(red)), ///
	ytitle("PRISM R-Squared (Post-Treatment)") ///
	xtitle("PRISM R-Squared (Pre-Treatment)") ///
	graphregion(color(white)) bgcolor(white) ///
	legend(order(1 "Interquartile" 2 "Top 25%" 3 "Bottom 25%"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/corr_rsq_prepost.png", replace


*** correlate PRISM HDD and r-squared
twoway (scatter HDDbest_pretreat max_pretreat if highperform==0, msize(vsmall) mcolor(black)) ///
	(scatter HDDbest_pretreat max_pretreat if highperform==1, msize(vsmall) mcolor(green)) ///
	(scatter HDDbest_pretreat max_pretreat if lowperform==1, msize(vsmall) mcolor(red)), ///
	ytitle("PRISM HDD* (Pre-Treatment)") ///
	xtitle("PRISM R-Squared (Pre-Treatment)") ///
	graphregion(color(white)) bgcolor(white) ///
	xline(0.85, lpattern(dash))	yline(43, lpattern(dash)) yline(72, lpattern(dash)) ///
	legend(order(1 "Interquartile" 2 "Top 25%" 3 "Bottom 25%"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/corr_hdd_rsq_pre.png", replace

twoway (scatter HDDbest_posttreat max_posttreat if highperform==0, msize(vsmall) mcolor(black)) ///
	(scatter HDDbest_posttreat max_posttreat if highperform==1, msize(vsmall) mcolor(green)) ///
	(scatter HDDbest_posttreat max_posttreat if lowperform==1, msize(vsmall) mcolor(red)), ///
	ytitle("PRISM HDD* (Post-Treatment)") ///
	xtitle("PRISM R-Squared (Post-Treatment)") ///
	graphregion(color(white)) bgcolor(white) ///
	xline(0.85, lpattern(dash))	yline(43, lpattern(dash)) yline(72, lpattern(dash)) ///
	legend(order(1 "Interquartile" 2 "Top 25%" 3 "Bottom 25%"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/corr_hdd_rsq_post.png", replace


*** correlate NPV and PRISM HDD
twoway (scatter npv1000 HDDbest_pretreat if highperform==0, msize(vsmall) mcolor(black)) ///
	(scatter npv1000 HDDbest_pretreat if highperform==1, msize(vsmall) mcolor(green)) ///
	(scatter npv1000 HDDbest_pretreat if lowperform==1, msize(vsmall) mcolor(red)), ///
	ytitle("Baseline Model NPV (US$ 1000)") ///
	xtitle("PRISM HDD* (Pre-Treatment)") ///
	graphregion(color(white)) bgcolor(white) ///
	legend(order(1 "Interquartile" 2 "Top 25%" 3 "Bottom 25%"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/corr_npv_hdd_pre.png", replace

twoway (scatter npv1000 HDDbest_posttreat if highperform==0, msize(vsmall) mcolor(black)) ///
	(scatter npv1000 HDDbest_posttreat if highperform==1, msize(vsmall) mcolor(green)) ///
	(scatter npv1000 HDDbest_posttreat if lowperform==1, msize(vsmall) mcolor(red)), ///
	ytitle("Baseline Model NPV (US$ 1000)") ///
	xtitle("PRISM HDD* (Post-Treatment)") ///
	graphregion(color(white)) bgcolor(white) ///
	legend(order(1 "Interquartile" 2 "Top 25%" 3 "Bottom 25%"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/corr_npv_hdd_post.png", replace


*** correlate NPV and number of observations available
twoway (scatter npv1000 totobs_pre if highperform==0, msize(vsmall) mcolor(black)) ///
	(scatter npv1000 totobs_pre if highperform==1, msize(vsmall) mcolor(green)) ///
	(scatter npv1000 totobs_pre if lowperform==1, msize(vsmall) mcolor(red)), ///
	ytitle("Baseline Model NPV (US$ 1000)") ///
	xtitle("Number of Monthly Observations (Pre-Treatment)") ///
	graphregion(color(white)) bgcolor(white) ///
	legend(order(1 "Interquartile" 2 "Top 25%" 3 "Bottom 25%"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/corr_npv_obs_pre.png", replace

twoway (scatter npv1000 totobs_post if highperform==0 & totobs_post<26, msize(vsmall) mcolor(black)) ///
	(scatter npv1000 totobs_post if highperform==1 & totobs_post<26, msize(vsmall) mcolor(green)) ///
	(scatter npv1000 totobs_post if lowperform==1 & totobs_post<26, msize(vsmall) mcolor(red)), ///
	ytitle("Baseline Model NPV (US$ 1000)") ///
	xtitle("Number of Monthly Observations (Post-Treatment)") ///
	graphregion(color(white)) bgcolor(white) ///
	legend(order(1 "Interquartile" 2 "Top 25%" 3 "Bottom 25%"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/corr_npv_obs_post.png", replace

**** relate NPV with an indicator for low r-squared 
twoway (scatter npv1000 obsid if lowrsq_pre==1, msize(vsmall) mcolor(red)) ///
	, xtitle("Home Percentile Rank" "Restricted to PRISM R-Squared <0.95 (Pre-Treatment)") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("WAP Net Present Benefits ($1000)") ///
	xlabel(0(1190)7140, labsize(small) angle(45) valuelabel) ///
	ylabel(-40(10)35, labsize(small))
graph export "./Regression Outputs/Heterogeneity/npv_bylowrsq_pre.png", replace width(5000)

twoway (scatter npv1000 obsid if lowrsq_pre==1, msize(vsmall) mcolor(red)) ///
	, xtitle("Home Percentile Rank" "Restricted to PRISM R-Squared <0.95 (Post-Treatment)") ///
	graphregion(color(white)) bgcolor(white) ///
	ytitle("WAP Net Present Benefits ($1000)") ///
	xlabel(0(1190)7140, labsize(small) angle(45) valuelabel) ///
	ylabel(-40(10)35, labsize(small))
graph export "./Regression Outputs/Heterogeneity/npv_bylowrsq_post.png", replace width(5000)


******************
* bootstrapping indicators for low versus high performing homes
forval i = 1/150 {
sort baseline_npv`i'
cumul baseline_npv`i' [fweight=weight`i'], gen(pctile_npv`i')
gen lowperform`i' = 0 if pctile_npv`i'>0.25 & pctile_npv`i'<=0.75
replace lowperform`i' = 1 if pctile_npv`i'<=0.25
gen highperform`i' = 0 if pctile_npv`i'>0.25 & pctile_npv`i'<=0.75
replace highperform`i' = 1 if pctile_npv`i'>0.75 & pctile_npv`i'!=.
}

* cleaning some auxiliary variables
tabulate cont_quintile, gen(tab_contquint)

gen tab_builddateX = 0 if builddate!=.
replace tab_builddateX = 1 if tab_builddate11==1 | tab_builddate12==1 | tab_builddate13==1

gen liheap = 0 if liheapemergency=="False"
replace liheap = 1 if liheapemergency=="True"

gen pct_BlowerReduc = BlowerReduc/Blower_Pre

gen atticR = Attic_RValue
replace atticR = . if atticR==0

gen hasattic = 0 if Attic_RValue==0
replace hasattic = 1 if Attic_RValue>0 & Attic_RValue!=.

** merge with contractor CFM performance
merge 1:1 jobnumber using "./Temp Data/Regression Outputs/Heterogeneity/contractor_rank_cfm.dta", nogen keep(3)
tab contractor_quintile_bayes, gen(tab_CFMquint)

********************************
* regressions to test if high and low performing homes are significantly different in covariates
** program that produces nice estimation table of bootstrap results - matrix labels already predefined
prog def bootstraplabelled, eclass
	args b V N /* input coef matrix, variance matrix, and number of obs */
	ereturn post `b' `V', obs(`=scalar(`N')')
	ereturn local cmd "bootstrapres"
	ereturn local properties "b V"
	ereturn display
end

**** loop over covariates related to housing structure
* defining variable labels to be outputed in tables
label var Blower_Pre "Blower Door Pre (CFM50)"
label var Blower_Post "Blower Door Post (CFM50)"
label var BlowerReduc "Blower Door Reduced (CFM50)"
label var pct_BlowerReduc "Percent Blower Door Reduced (%)"
label var hasattic "Has an Attic (%)"
label var atticR "Attic R-Value"
label var sqfeet "Floor Area (sqft)"
label var bedrooms "N Bedrooms"
label var nstories "Has Multiple Stories (%)"
label var tab_builddate1 "Built Pre-1900 (%)"
label var tab_builddate2 "Built 1900-1909 (%)"
label var tab_builddate3 "Built 1910-1919 (%)"
label var tab_builddate4 "Built 1920-1929 (%)"
label var tab_builddate5 "Built 1930-1939 (%)"
label var tab_builddate6 "Built 1940-1949 (%)"
label var tab_builddate7 "Built 1950-1959 (%)"
label var tab_builddate8 "Built 1960-1969 (%)"
label var tab_builddate9 "Built 1970-1977 (%)"
label var tab_builddate10 "Built 1978-1989 (%)"
label var tab_builddateX "Built 1990-Present (%)"
		  

foreach x in Blower_Pre Blower_Post BlowerReduc pct_BlowerReduc hasattic atticR sqfeet bedrooms nstories ///
		tab_builddate1 tab_builddate2 tab_builddate3 tab_builddate4 tab_builddate5 ///
		tab_builddate6 tab_builddate7 tab_builddate8 tab_builddate9 tab_builddate10 tab_builddateX {
	sum `x' if highperform==0
	sca coef1 = `r(mean)'
	sum `x' if lowperform==1
	sca coef2 = `r(mean)'
	sum `x' if highperform==1
	sca coef3 = `r(mean)'
	reg `x' i.lowperform
	matrix mat1 = e(b)
	sca coef4 = mat1[1,2]
	reg `x' i.highperform
	sca nobs = e(N)
	matrix mat2 = e(b)
	sca coef5 = mat2[1,2]
	matrix ttestcoefs = (coef1, coef2, coef3, coef4, coef5)
	mat colnames ttestcoefs="A" "B" "C" "D" "E"

	matrix input btstcoefs = (., ., ., ., .)
	forval i = 1/150 {
	qui : sum `x' [fweight=weight`i'] if highperform`i'==0
	sca coef1 = `r(mean)'
	qui : sum `x' [fweight=weight`i'] if lowperform`i'==1
	sca coef2 = `r(mean)'
	qui : sum `x' [fweight=weight`i'] if highperform`i'==1
	sca coef3 = `r(mean)'
	qui : reg `x' i.lowperform`i' [fweight=weight`i']
	matrix mat1 = e(b)
	sca coef4 = mat1[1,2]
	qui : reg `x' i.highperform`i' [fweight=weight`i']
	matrix mat2 = e(b)
	sca coef5 = mat2[1,2]
	matrix tempmat = (0, 0, 0, coef4, coef5)
	matrix btstcoefs = (btstcoefs \ tempmat)
	}
	mata : st_matrix("ttestsvar", mm_colvar(st_matrix("btstcoefs")))
	matrix ttestsvar = diag(ttestsvar)
	mat colnames ttestsvar="A" "B" "C" "D" "E"
	mat rownames ttestsvar="A" "B" "C" "D" "E"

	bootstraplabelled ttestcoefs ttestsvar nobs
	est store `x'
}

esttab Blower_Pre Blower_Post BlowerReduc pct_BlowerReduc hasattic atticR sqfeet bedrooms nstories ///
		tab_builddate1 tab_builddate2 tab_builddate3 tab_builddate4 tab_builddate5 ///
		tab_builddate6 tab_builddate7 tab_builddate8 tab_builddate9 tab_builddate10 ///
		tab_builddateX , se nostar
matrix C = r(coefs)
eststo clear
local rnames : rownames C
local models : coleq C
local models : list uniq models
local i 0
foreach name of local rnames {
    local ++i
    local j 0
    capture matrix drop b
    capture matrix drop se
    foreach model of local models {
        local ++j
        matrix tmp = C[`i', 2*`j'-1]
        if tmp[1,1]<. {
			local varlab : variable label `model'
            matrix colnames tmp = "`varlab'"
            matrix b = nullmat(b), tmp
            matrix tmp[1,1] = C[`i', 2*`j']
            matrix se = nullmat(se), tmp
        }
    }
    ereturn post b
    quietly estadd matrix se
    eststo `name'
}
esttab B A C D E using "./Regression Outputs/Heterogeneity/ttests_housing_topbot.tex", ///
	replace se b(3) se(3) noobs nonotes ///
	mlabels("Bottom 25% Homes" "Interquartile Homes" "Top 25% Homes" "Diff. (1)-(2)" "Diff. (3)-(2)")


**** loop over covariates related to the program administration
label var py09 "PY 2009 (%)"
label var py10 "PY 2010 (%)"
label var py11 "PY 2011 (%)"
label var py12 "PY 2012 (%)"
label var py13 "PY 2013 (%)"
label var py14 "PY 2014 (%)"
label var py15 "PY 2015 (%)"
label var py16 "PY 2016 (%)"
label var tab_contquint1 "Top Contractor Quintile (%)"
label var tab_contquint2 "2nd Contractor Quintile (%)" 
label var tab_contquint3 "3rd Contractor Quintile (%)" 
label var tab_contquint4 "4th Contractor Quintile (%)"
label var tab_contquint5 "Bottom Contractor Quintile (%)"
label var tab_CFMquint1 "Top CFM50 Cont. Quintile (%)"
label var tab_CFMquint2 "2nd CFM50 Cont. Quintile (%)" 
label var tab_CFMquint3 "3rd CFM50 Cont. Quintile (%)" 
label var tab_CFMquint4 "4th CFM50 Cont. Quintile (%)"
label var tab_CFMquint5 "Bottom CFM50 Cont. Quintile (%)"
label var max_pretreat "Pre-WAP PRISM R-sq"
label var max_posttreat "Post-WAP PRISM R-sq"
label var HDDbest_pretreat "Pre-WAP PRISM HDD"
label var HDDbest_posttreat "Post-WAP PRISM HDD" 

foreach x in py09 py10 py11 py12 py13 py14 py15 py16 ///
		tab_contquint1 tab_contquint2 tab_contquint3 tab_contquint4 tab_contquint5 ///
		tab_CFMquint1 tab_CFMquint2 tab_CFMquint3 tab_CFMquint4 tab_CFMquint5 ///
		max_pretreat max_posttreat HDDbest_pretreat HDDbest_posttreat {
	sum `x' if highperform==0
	sca coef1 = `r(mean)'
	sum `x' if lowperform==1
	sca coef2 = `r(mean)'
	sum `x' if highperform==1
	sca coef3 = `r(mean)'
	reg `x' i.lowperform
	matrix mat1 = e(b)
	sca coef4 = mat1[1,2]
	reg `x' i.highperform
	sca nobs = e(N)
	matrix mat2 = e(b)
	sca coef5 = mat2[1,2]
	matrix ttestcoefs = (coef1, coef2, coef3, coef4, coef5)
	mat colnames ttestcoefs="A" "B" "C" "D" "E"

	matrix input btstcoefs = (., ., ., ., .)
	forval i = 1/150 {
	qui : sum `x' [fweight=weight`i'] if highperform`i'==0
	sca coef1 = `r(mean)'
	qui : sum `x' [fweight=weight`i'] if lowperform`i'==1
	sca coef2 = `r(mean)'
	qui : sum `x' [fweight=weight`i'] if highperform`i'==1
	sca coef3 = `r(mean)'
	qui : reg `x' i.lowperform`i' [fweight=weight`i']
	matrix mat1 = e(b)
	sca coef4 = mat1[1,2]
	qui : reg `x' i.highperform`i' [fweight=weight`i']
	matrix mat2 = e(b)
	sca coef5 = mat2[1,2]
	matrix tempmat = (0, 0, 0, coef4, coef5)
	matrix btstcoefs = (btstcoefs \ tempmat)
	}
	mata : st_matrix("ttestsvar", mm_colvar(st_matrix("btstcoefs")))
	matrix ttestsvar = diag(ttestsvar)
	mat colnames ttestsvar="A" "B" "C" "D" "E"
	mat rownames ttestsvar="A" "B" "C" "D" "E"

	bootstraplabelled ttestcoefs ttestsvar nobs
	est store `x'
}

esttab py09 py10 py11 py12 py13 py14 py15 py16 ///
		tab_contquint1 tab_contquint2 tab_contquint3 tab_contquint4 tab_contquint5 ///
		tab_CFMquint1 tab_CFMquint2 tab_CFMquint3 tab_CFMquint4 tab_CFMquint5 ///
		max_pretreat max_posttreat HDDbest_pretreat HDDbest_posttreat, se nostar
matrix C = r(coefs)
eststo clear
local rnames : rownames C
local models : coleq C
local models : list uniq models
local i 0
foreach name of local rnames {
    local ++i
    local j 0
    capture matrix drop b
    capture matrix drop se
    foreach model of local models {
        local ++j
        matrix tmp = C[`i', 2*`j'-1]
        if tmp[1,1]<. {
			local varlab : variable label `model'
            matrix colnames tmp = "`varlab'"
            matrix b = nullmat(b), tmp
            matrix tmp[1,1] = C[`i', 2*`j']
            matrix se = nullmat(se), tmp
        }
    }
    ereturn post b
    quietly estadd matrix se
    eststo `name'
}
esttab B A C D E using "./Regression Outputs/Heterogeneity/ttests_program_topbot.tex", ///
	replace se b(3) se(3) noobs nonotes ///
	mlabels("Bottom 25% Homes" "Interquartile Homes" "Top 25% Homes" "Diff. (1)-(2)" "Diff. (3)-(2)")



**** loop over covariates related to the program administration
label var Real_income1000 "Income ($/1000)"
label var occupants "N Occupants"
label var Age "Householder Age"
label var female "Female Householder (%)"
label var renter "Renter (%)"
label var white "White (%)"
label var black "Black (%)"
label var hispanic "Hispanic (%)"
label var nativeamerican "Native American (%)"
label var otherrace "Other Race (%)"
label var haselderly "Seniors 65+ (%)"
label var hasminor "Children Under 18 (%)"
label var priority "Priority"
label var liheap "LIHEAP (%)"

foreach x in Real_income1000 occupants Age female renter white black hispanic nativeamerican ///
	otherrace haselderly hasminor priority liheap {
	sum `x' if highperform==0
	sca coef1 = `r(mean)'
	sum `x' if lowperform==1
	sca coef2 = `r(mean)'
	sum `x' if highperform==1
	sca coef3 = `r(mean)'
	reg `x' i.lowperform
	matrix mat1 = e(b)
	sca coef4 = mat1[1,2]
	reg `x' i.highperform
	sca nobs = e(N)
	matrix mat2 = e(b)
	sca coef5 = mat2[1,2]
	matrix ttestcoefs = (coef1, coef2, coef3, coef4, coef5)
	mat colnames ttestcoefs="A" "B" "C" "D" "E"

	matrix input btstcoefs = (., ., ., ., .)
	forval i = 1/150 {
	qui : sum `x' [fweight=weight`i'] if highperform`i'==0
	sca coef1 = `r(mean)'
	qui : sum `x' [fweight=weight`i'] if lowperform`i'==1
	sca coef2 = `r(mean)'
	qui : sum `x' [fweight=weight`i'] if highperform`i'==1
	sca coef3 = `r(mean)'
	qui : reg `x' i.lowperform`i' [fweight=weight`i']
	matrix mat1 = e(b)
	sca coef4 = mat1[1,2]
	qui : reg `x' i.highperform`i' [fweight=weight`i']
	matrix mat2 = e(b)
	sca coef5 = mat2[1,2]
	matrix tempmat = (0, 0, 0, coef4, coef5)
	matrix btstcoefs = (btstcoefs \ tempmat)
	}
	mata : st_matrix("ttestsvar", mm_colvar(st_matrix("btstcoefs")))
	matrix ttestsvar = diag(ttestsvar)
	mat colnames ttestsvar="A" "B" "C" "D" "E"
	mat rownames ttestsvar="A" "B" "C" "D" "E"

	bootstraplabelled ttestcoefs ttestsvar nobs
	est store `x'
}

esttab Real_income1000 occupants Age female renter white black hispanic nativeamerican ///
	otherrace haselderly hasminor priority liheap, se nostar
matrix C = r(coefs)
eststo clear
local rnames : rownames C
local models : coleq C
local models : list uniq models
local i 0
foreach name of local rnames {
    local ++i
    local j 0
    capture matrix drop b
    capture matrix drop se
    foreach model of local models {
        local ++j
        matrix tmp = C[`i', 2*`j'-1]
        if tmp[1,1]<. {
			local varlab : variable label `model'
            matrix colnames tmp = "`varlab'"
            matrix b = nullmat(b), tmp
            matrix tmp[1,1] = C[`i', 2*`j']
            matrix se = nullmat(se), tmp
        }
    }
    ereturn post b
    quietly estadd matrix se
    eststo `name'
}
esttab B A C D E using "./Regression Outputs/Heterogeneity/ttests_demog_topbot.tex", ///
	replace se b(3) se(3) noobs nonotes ///
	mlabels("Bottom 25% Homes" "Interquartile Homes" "Top 25% Homes" "Diff. (1)-(2)" "Diff. (3)-(2)")


**** loop over covariates related to the program administration
label var Real_total_cost "Total"
label var Real_nonHS_cost "Total (without H&S)"
label var Real_tot_actAirCon "Air Conditioning"
label var Real_tot_actAirSeal "Air Sealing"
label var Real_tot_actAttic "Attic"
label var Real_tot_actBaseload "Baseload"
label var Real_tot_actDoor "Door"
label var Real_tot_actFoundation "Foundation"
label var Real_tot_actFurnace "Furnace"
label var Real_tot_actGeneral "General"
label var Real_tot_actHealSfty "Health and Safety"
label var Real_tot_actWallIns "Wall Insulation"
label var Real_tot_actWindow "Window"
label var Real_tot_actWtHtr "Water Heater"

foreach x in Real_total_cost Real_nonHS_cost Real_tot_actAirCon	Real_tot_actAirSeal Real_tot_actAttic Real_tot_actBaseload ///
			Real_tot_actDoor Real_tot_actFoundation Real_tot_actFurnace ///
			Real_tot_actGeneral Real_tot_actHealSfty Real_tot_actWallIns ///
			Real_tot_actWindow Real_tot_actWtHtr {
	sum `x' if highperform==0
	sca coef1 = `r(mean)'
	sum `x' if lowperform==1
	sca coef2 = `r(mean)'
	sum `x' if highperform==1
	sca coef3 = `r(mean)'
	reg `x' i.lowperform
	matrix mat1 = e(b)
	sca coef4 = mat1[1,2]
	reg `x' i.highperform
	sca nobs = e(N)
	matrix mat2 = e(b)
	sca coef5 = mat2[1,2]
	matrix ttestcoefs = (coef1, coef2, coef3, coef4, coef5)
	mat colnames ttestcoefs="A" "B" "C" "D" "E"

	matrix input btstcoefs = (., ., ., ., .)
	forval i = 1/150 {
	qui : sum `x' [fweight=weight`i'] if highperform`i'==0
	sca coef1 = `r(mean)'
	qui : sum `x' [fweight=weight`i'] if lowperform`i'==1
	sca coef2 = `r(mean)'
	qui : sum `x' [fweight=weight`i'] if highperform`i'==1
	sca coef3 = `r(mean)'
	qui : reg `x' i.lowperform`i' [fweight=weight`i']
	matrix mat1 = e(b)
	sca coef4 = mat1[1,2]
	qui : reg `x' i.highperform`i' [fweight=weight`i']
	matrix mat2 = e(b)
	sca coef5 = mat2[1,2]
	matrix tempmat = (0, 0, 0, coef4, coef5)
	matrix btstcoefs = (btstcoefs \ tempmat)
	}
	mata : st_matrix("ttestsvar", mm_colvar(st_matrix("btstcoefs")))
	matrix ttestsvar = diag(ttestsvar)
	mat colnames ttestsvar="A" "B" "C" "D" "E"
	mat rownames ttestsvar="A" "B" "C" "D" "E"

	bootstraplabelled ttestcoefs ttestsvar nobs
	est store `x'
}

esttab Real_total_cost Real_nonHS_cost Real_tot_actAirCon	Real_tot_actAirSeal Real_tot_actAttic Real_tot_actBaseload ///
			Real_tot_actDoor Real_tot_actFoundation Real_tot_actFurnace ///
			Real_tot_actGeneral Real_tot_actHealSfty Real_tot_actWallIns ///
			Real_tot_actWindow Real_tot_actWtHtr, se nostar
matrix C = r(coefs)
eststo clear
local rnames : rownames C
local models : coleq C
local models : list uniq models
local i 0
foreach name of local rnames {
    local ++i
    local j 0
    capture matrix drop b
    capture matrix drop se
    foreach model of local models {
        local ++j
        matrix tmp = C[`i', 2*`j'-1]
        if tmp[1,1]<. {
			local varlab : variable label `model'
            matrix colnames tmp = "`varlab'"
            matrix b = nullmat(b), tmp
            matrix tmp[1,1] = C[`i', 2*`j']
            matrix se = nullmat(se), tmp
        }
    }
    ereturn post b
    quietly estadd matrix se
    eststo `name'
}
esttab B A C D E using "./Regression Outputs/Heterogeneity/ttests_spending_topbot.tex", ///
	replace se b(3) se(3) noobs nonotes ///
	mlabels("Bottom 25% Homes" "Interquartile Homes" "Top 25% Homes" "Diff. (1)-(2)" "Diff. (3)-(2)")

	
**** loop over covariates related to energy savings and the gap
forval i = 1/150 {
gen projected_savings`i' = projected_savings
}

label var realized_savings "Realized Savings (%)"
label var projected_savings "Projected Savings (%)"
label var savings_gap "Savings Gap (%)"

foreach x in realized_savings projected_savings savings_gap {
	sum `x' if highperform==0
	sca coef1 = `r(mean)'
	sum `x' if lowperform==1
	sca coef2 = `r(mean)'
	sum `x' if highperform==1
	sca coef3 = `r(mean)'
	reg `x' i.lowperform
	matrix mat1 = e(b)
	sca coef4 = mat1[1,2]
	reg `x' i.highperform
	sca nobs = e(N)
	matrix mat2 = e(b)
	sca coef5 = mat2[1,2]
	matrix ttestcoefs = (coef1, coef2, coef3, coef4, coef5)
	mat colnames ttestcoefs="A" "B" "C" "D" "E"

	matrix input btstcoefs = (., ., ., ., .)
	forval i = 1/150 {
	qui : sum `x'`i' [fweight=weight`i'] if highperform`i'==0
	sca coef1 = `r(mean)'
	qui : sum `x'`i' [fweight=weight`i'] if lowperform`i'==1
	sca coef2 = `r(mean)'
	qui : sum `x'`i' [fweight=weight`i'] if highperform`i'==1
	sca coef3 = `r(mean)'
	qui : reg `x'`i' i.lowperform`i' [fweight=weight`i']
	matrix mat1 = e(b)
	sca coef4 = mat1[1,2]
	qui : reg `x'`i' i.highperform`i' [fweight=weight`i']
	matrix mat2 = e(b)
	sca coef5 = mat2[1,2]
	matrix tempmat = (0, 0, 0, coef4, coef5)
	matrix btstcoefs = (btstcoefs \ tempmat)
	}
	mata : st_matrix("ttestsvar", mm_colvar(st_matrix("btstcoefs")))
	matrix ttestsvar = diag(ttestsvar)
	mat colnames ttestsvar="A" "B" "C" "D" "E"
	mat rownames ttestsvar="A" "B" "C" "D" "E"

	bootstraplabelled ttestcoefs ttestsvar nobs
	est store `x'
}

esttab realized_savings projected_savings savings_gap, se nostar
matrix C = r(coefs)
eststo clear
local rnames : rownames C
local models : coleq C
local models : list uniq models
local i 0
foreach name of local rnames {
    local ++i
    local j 0
    capture matrix drop b
    capture matrix drop se
    foreach model of local models {
        local ++j
        matrix tmp = C[`i', 2*`j'-1]
        if tmp[1,1]<. {
			local varlab : variable label `model'
            matrix colnames tmp = "`varlab'"
            matrix b = nullmat(b), tmp
            matrix tmp[1,1] = C[`i', 2*`j']
            matrix se = nullmat(se), tmp
        }
    }
    ereturn post b
    quietly estadd matrix se
    eststo `name'
}
esttab B A C D E using "./Regression Outputs/Heterogeneity/ttests_gap_topbot.tex", ///
	replace se b(3) se(3) noobs nonotes ///
	mlabels("Bottom 25% Homes" "Interquartile Homes" "Top 25% Homes" "Diff. (1)-(2)" "Diff. (3)-(2)")


************************ graphical comparisons of low versus high perfomrning homes
** graphs for electric
local varlab : variable label electric_mmbtu
reg electric_mmbtu ibn.end_month if realized_savings_mmbtu!=. & lowperform==1 & treated==0, nocons vce(cluster Household)
est sto use_bad_pre
reg electric_mmbtu ibn.end_month if realized_savings_mmbtu!=. & lowperform==1 & treated==1, nocons vce(cluster Household)
est sto use_bad_post
coefplot (use_bad_pre, msymbol(Th)) use_bad_post , vertical xtitle("Month of Year") ///
			graphregion(color(white)) bgcolor(white) ///
			ytitle("`varlab' Usage (MMBtu)") legend(order(2 "Pre-WAP" 4 "Post-WAP")) ///
			yscale(range(1 7)) ylabel(1(1)7)
graph export "./Regression Outputs/Heterogeneity/CE_electricpermonth_bot.png", replace width(10000)

local varlab : variable label electric_mmbtu
reg electric_mmbtu ibn.end_month if realized_savings_mmbtu!=. & highperform==1 & treated==0, nocons vce(cluster Household)
est sto use_bad_pre
reg electric_mmbtu ibn.end_month if realized_savings_mmbtu!=. & highperform==1 & treated==1, nocons vce(cluster Household)
est sto use_bad_post
coefplot (use_bad_pre, msymbol(Th)) use_bad_post , vertical xtitle("Month of Year") ///
			graphregion(color(white)) bgcolor(white) ///
			ytitle("`varlab' Usage (MMBtu)") legend(order(2 "Pre-WAP" 4 "Post-WAP")) ///
			yscale(range(1 7)) ylabel(1(1)7)
graph export "./Regression Outputs/Heterogeneity/CE_electricpermonth_top.png", replace width(10000)

local varlab : variable label electric_mmbtu
reg electric_kbtu_sqft ibn.end_month if realized_savings_mmbtu!=. & lowperform==1 & treated==0, nocons vce(cluster Household)
est sto use_bad_pre
reg electric_kbtu_sqft ibn.end_month if realized_savings_mmbtu!=. & lowperform==1 & treated==1, nocons vce(cluster Household)
est sto use_bad_post
coefplot (use_bad_pre, msymbol(Th)) use_bad_post , vertical xtitle("Month of Year") ///
			graphregion(color(white)) bgcolor(white) ///
			ytitle("`varlab' Usage" "per Floor Area (kBtu/sqft)") legend(order(2 "Pre-WAP" 4 "Post-WAP")) ///
			yscale(range(0.5 5.5)) ylabel(0.5(1)5.5)
graph export "./Regression Outputs/Heterogeneity/CE_electricpermonthsqft_bot.png", replace width(10000)

local varlab : variable label electric_mmbtu
reg electric_kbtu_sqft ibn.end_month if realized_savings_mmbtu!=. & highperform==1 & treated==0, nocons vce(cluster Household)
est sto use_bad_pre
reg electric_kbtu_sqft ibn.end_month if realized_savings_mmbtu!=. & highperform==1 & treated==1, nocons vce(cluster Household)
est sto use_bad_post
coefplot (use_bad_pre, msymbol(Th)) use_bad_post , vertical xtitle("Month of Year") ///
			graphregion(color(white)) bgcolor(white) ///
			ytitle("`varlab' Usage" "per Floor Area (kBtu/sqft)") legend(order(2 "Pre-WAP" 4 "Post-WAP")) ///
			yscale(range(0.5 5.5)) ylabel(0.5(1)5.5)
graph export "./Regression Outputs/Heterogeneity/CE_electricpermonthsqft_top.png", replace width(10000)


** graphs for gas
local varlab : variable label gas_mmbtu
reg gas_mmbtu ibn.end_month if realized_savings_mmbtu!=. & lowperform==1 & treated==0, nocons vce(cluster Household)
est sto use_bad_pre
reg gas_mmbtu ibn.end_month if realized_savings_mmbtu!=. & lowperform==1 & treated==1, nocons vce(cluster Household)
est sto use_bad_post
coefplot (use_bad_pre, msymbol(Th)) use_bad_post , vertical xtitle("Month of Year") ///
			graphregion(color(white)) bgcolor(white) ///
			ytitle("`varlab' Usage (MMBtu)") legend(order(2 "Pre-WAP" 4 "Post-WAP")) ///
			yscale(range(0 25)) ylabel(0(5)25)
graph export "./Regression Outputs/Heterogeneity/CE_gaspermonth_bot.png", replace width(10000)

local varlab : variable label gas_mmbtu
reg gas_mmbtu ibn.end_month if realized_savings_mmbtu!=. & highperform==1 & treated==0, nocons vce(cluster Household)
est sto use_bad_pre
reg gas_mmbtu ibn.end_month if realized_savings_mmbtu!=. & highperform==1 & treated==1, nocons vce(cluster Household)
est sto use_bad_post
coefplot (use_bad_pre, msymbol(Th)) use_bad_post , vertical xtitle("Month of Year") ///
			graphregion(color(white)) bgcolor(white) ///
			ytitle("`varlab' Usage (MMBtu)") legend(order(2 "Pre-WAP" 4 "Post-WAP")) ///
			yscale(range(0 25)) ylabel(0(5)25)
graph export "./Regression Outputs/Heterogeneity/CE_gaspermonth_top.png", replace width(10000)

local varlab : variable label gas_mmbtu
reg gas_kbtu_sqft ibn.end_month if realized_savings_mmbtu!=. & lowperform==1 & treated==0, nocons vce(cluster Household)
est sto use_bad_pre
reg gas_kbtu_sqft ibn.end_month if realized_savings_mmbtu!=. & lowperform==1 & treated==1, nocons vce(cluster Household)
est sto use_bad_post
coefplot (use_bad_pre, msymbol(Th)) use_bad_post , vertical xtitle("Month of Year") ///
			graphregion(color(white)) bgcolor(white) ///
			ytitle("`varlab' Usage" "per Floor Area (kBtu/sqft)") legend(order(2 "Pre-WAP" 4 "Post-WAP")) ///
			yscale(range(0 17)) ylabel(0(5)17)
graph export "./Regression Outputs/Heterogeneity/CE_gaspermonthsqft_bot.png", replace width(10000)

local varlab : variable label gas_mmbtu
reg gas_kbtu_sqft ibn.end_month if realized_savings_mmbtu!=. & highperform==1 & treated==0, nocons vce(cluster Household)
est sto use_bad_pre
reg gas_kbtu_sqft ibn.end_month if realized_savings_mmbtu!=. & highperform==1 & treated==1, nocons vce(cluster Household)
est sto use_bad_post
coefplot (use_bad_pre, msymbol(Th)) use_bad_post , vertical xtitle("Month of Year") ///
			graphregion(color(white)) bgcolor(white) ///
			ytitle("`varlab' Usage" "per Floor Area (kBtu/sqft)") legend(order(2 "Pre-WAP" 4 "Post-WAP")) ///
			yscale(range(0 17)) ylabel(0(5)17)
graph export "./Regression Outputs/Heterogeneity/CE_gaspermonthsqft_top.png", replace width(10000)


** graphs for total usage
local varlab : variable label total_mmbtu
reg total_mmbtu ibn.end_month if realized_savings_mmbtu!=. & lowperform==1 & treated==0, nocons vce(cluster Household)
est sto use_bad_pre
reg total_mmbtu ibn.end_month if realized_savings_mmbtu!=. & lowperform==1 & treated==1, nocons vce(cluster Household)
est sto use_bad_post
coefplot (use_bad_pre, msymbol(Th)) use_bad_post , vertical xtitle("Month of Year") ///
			graphregion(color(white)) bgcolor(white) ///
			ytitle("`varlab' Usage (MMBtu)") legend(order(2 "Pre-WAP" 4 "Post-WAP")) ///
			yscale(range(0 26)) ylabel(0(5)25)
graph export "./Regression Outputs/Heterogeneity/CE_totalpermonth_bot.png", replace width(10000)

local varlab : variable label total_mmbtu
reg total_mmbtu ibn.end_month if realized_savings_mmbtu!=. & highperform==1 & treated==0, nocons vce(cluster Household)
est sto use_bad_pre
reg total_mmbtu ibn.end_month if realized_savings_mmbtu!=. & highperform==1 & treated==1, nocons vce(cluster Household)
est sto use_bad_post
coefplot (use_bad_pre, msymbol(Th)) use_bad_post , vertical xtitle("Month of Year") ///
			graphregion(color(white)) bgcolor(white) ///
			ytitle("`varlab' Usage (MMBtu)") legend(order(2 "Pre-WAP" 4 "Post-WAP")) ///
			yscale(range(0 26)) ylabel(0(5)25)
graph export "./Regression Outputs/Heterogeneity/CE_totalpermonth_top.png", replace width(10000)

local varlab : variable label total_mmbtu
reg total_kbtu_sqft ibn.end_month if realized_savings_mmbtu!=. & lowperform==1 & treated==0, nocons vce(cluster Household)
est sto use_bad_pre
reg total_kbtu_sqft ibn.end_month if realized_savings_mmbtu!=. & lowperform==1 & treated==1, nocons vce(cluster Household)
est sto use_bad_post
coefplot (use_bad_pre, msymbol(Th)) use_bad_post , vertical xtitle("Month of Year") ///
			graphregion(color(white)) bgcolor(white) ///
			ytitle("`varlab' Usage" "per Floor Area (kBtu/sqft)") legend(order(2 "Pre-WAP" 4 "Post-WAP")) ///
			yscale(range(0 20)) ylabel(0(5)20)
graph export "./Regression Outputs/Heterogeneity/CE_totalpermonthsqft_bot.png", replace width(10000)

local varlab : variable label total_mmbtu
reg total_kbtu_sqft ibn.end_month if realized_savings_mmbtu!=. & highperform==1 & treated==0, nocons vce(cluster Household)
est sto use_bad_pre
reg total_kbtu_sqft ibn.end_month if realized_savings_mmbtu!=. & highperform==1 & treated==1, nocons vce(cluster Household)
est sto use_bad_post
coefplot (use_bad_pre, msymbol(Th)) use_bad_post , vertical xtitle("Month of Year") ///
			graphregion(color(white)) bgcolor(white) ///
			ytitle("`varlab' Usage" "per Floor Area (kBtu/sqft)") legend(order(2 "Pre-WAP" 4 "Post-WAP")) ///
			yscale(range(0 20)) ylabel(0(5)20)
graph export "./Regression Outputs/Heterogeneity/CE_totalpermonthsqft_top.png", replace width(10000)


**** HISTOGRAMS FOR DOLLARS SPENT IN EACH BIN - comparing low-performing homes versus rest of sample
gen newbinFurnace = binFurnace-300
gen newbinAirSeal = binAirSeal-100
gen newbinAttic = binAttic-200
gen newbinWallIns = binWallIns-300
gen newbinWindow = binWindow-200
gen newbinTotalCost = bin_TotalCost


foreach x in binFurnace binAirSeal binAttic binWallIns binWindow binTotalCost {
bysort new`x' : gen count`x'_good = sum(aux) if ProgramYear>=2009 & neg_save==1 & ww_treat==1
bysort new`x' : egen freq_`x'_good = max(count`x'_good) if ProgramYear>=2009 & neg_save==1 & ww_treat==1
bysort new`x' : gen count`x'_bad = sum(aux) if ProgramYear>=2009 & neg_save==0 & ww_treat==1
bysort new`x' : egen freq_`x'_bad = max(count`x'_bad) if ProgramYear>=2009 & neg_save==0 & ww_treat==1
drop count`x'_good count`x'_bad
}						

label var Real_tot_actAirCon "Air Conditioning"
label var Real_tot_actAirSeal "Air Sealing"
label var Real_tot_actAttic "Attic"
label var Real_tot_actBaseload "Baseload"
label var Real_tot_actDoor "Door"
label var Real_tot_actFoundation "Foundation"
label var Real_tot_actFurnace "Furnace"
label var Real_tot_actGeneral "General"
label var Real_tot_actHealSfty "Health and Safety"
label var Real_tot_actWallIns "Wall Insulation"
label var Real_tot_actWindow "Window"
label var Real_tot_actWtHtr "Water Heater"

label define furnacelab2 -299 "0" 0 "1-300" 300 "301-600" 600 "601-900" 900 "901-1200" 1200 "1201-1500" ///
		1500 "1501-1800" 1800 "1801-2100" 2100 "2101-2400" 2400 "2401-2700" 2700 "2701-3000" 3000 ">3000", replace
label values newbinFurnace furnacelab2
label values newbinWallIns furnacelab2

label define airseallab -99 "0" 0 "1-100" 100 "101-200" 200 "201-300" 300 "301-400" 400 "401-500" ///
		500 "501-600" 600 "601-700" 700 "701-800" 800 "801-900" 900 ">900", replace
label values newbinAirSeal airseallab
		
label define atticlab2 -199 "0" 0 "1-200" 200 "201-400" 400 "401-600" 600 "601-800" 800 "801-1000" ///
		1000 "1001-1200" 1200 "1201-1400" 1400 "1401-1600" 1600 "1601-1800" 1800 "1801-2000" 2000 ">2000", replace
label values newbinAttic atticlab2	
label values newbinWindow atticlab2

label define totallab 500 "<500" 1000 "501-1000" 1500 "1001-1500" 2000 "1501-2000" ///
	2500 "2001-2500" 3000 "2501-3000" 3500 "3001-3500" 4000 "3501-4000" ///
	4500 "4001-4500" 5000 "4501-5000" 5500 "5001-5500" 6000 "5501-6000" ///
	6500 "6001-6500" 7000 "6501-7000" 7500 "7001-7500" 8000 "7501-8000" ///
	8500 "8001-8500" 9000 "8501-9000" 9500 ">9000", replace
label values newbinTotalCost totallab	


* histograms comparing good and bad homes
foreach x in Furnace WallIns AirSeal Attic Window {
local varlab : variable label Real_tot_act`x'
graph bar (percent) freq_bin`x'_bad freq_bin`x'_good if ww_treat==1 ///
		, over(newbin`x', label(angle(45) labsize(small))) graphregion(color(white)) bgcolor(white) ///
		legend(style(column) ring(0) position(1) bmargin(large) ///
		order(1 "Bottom 14.8% Homes" 2 "Rest of Sample") title("Home Cost-Effectiveness:", size(small)) ///
		symysize(5)  symxsize(5)) bar(1, col(maroon)) bar(2, col(navy)) ///
		ytitle("Percent of Sample") ///
		b1title("Amount Spent on `varlab' ($)")		
graph export "./Regression Outputs/Heterogeneity/CEhist_topVSbot_`x'.png", replace width(10000)
}
foreach x in Furnace {
local varlab : variable label Real_tot_act`x'
graph bar (percent) freq_bin`x'_bad freq_bin`x'_good if ww_treat==1 ///
		, over(newbin`x', label(angle(45) labsize(small))) graphregion(color(white)) bgcolor(white) ///
		legend(style(column) ring(0) position(12) bmargin(large) ///
		order(1 "Bottom 14.8% Homes" 2 "Rest of Sample") title("Home Cost-Effectiveness:", size(small)) ///
		symysize(5)  symxsize(5)) bar(1, col(maroon)) bar(2, col(navy)) ///
		ytitle("Percent of Sample") ///
		b1title("Amount Spent on `varlab' ($)")		
graph export "./Regression Outputs/Heterogeneity/CEhist_topVSbot_`x'.png", replace width(10000)
}

graph bar (percent) freq_binTotalCost_bad freq_binTotalCost_good if ww_treat==1 ///
		, over(newbinTotalCost, label(angle(45) labsize(small))) graphregion(color(white)) bgcolor(white) ///
		legend(style(column) ring(0) position(10) bmargin(large) ///
		order(1 "Bottom 14.8% Homes" 2 "Rest of Sample") title("Home Cost-Effectiveness:", size(small)) ///
		symysize(5)  symxsize(5)) bar(1, col(maroon)) bar(2, col(navy)) ///
		ytitle("Percent of Sample") ///
		b1title("Total Spent on Home ($)")	
graph export "./Regression Outputs/Heterogeneity/CEhist_topVSbot_TotalCost.png", replace width(10000)




****************************************************************************************************
****************************************************************************************************
******************************* ML MODEL DIAGNOSTICS ***********************************************
clear
cd "P:\IHWAP\"

** importing ML results from aForge
import delimited "./Machine Learning/Model Outputs/CV_predcitpre_best", varnames(1) case(preserve)

gen meterend = mdy(end_month, end_day, end_year)
format meterend %d
sort rowid

*foreach x in pred1_1 pred1_2 pred1_3 pred1_4 pred1_5 pred_full {
*winsor2 `x', cuts(0.5 99.5) trim replace /* do NOT winsorize - makes predictions worse */
*}

**** prediction errors
** cross validated errors for
foreach i in 1 {
	gen cvpreds`i' = .
	forval j = 1/5 {
		replace cvpreds`i' = pred`i'_`j' if kfold2==`j'
	}
	gen cvresids`i' = total_mmbtu - cvpreds`i'
	gen cverrors`i' = 100*cvresids`i'/total_mmbtu
}

** in sample errors
foreach i in 1  {
	forval j = 1/5 {
		replace pred`i'_`j' = . if kfold2==`j'
	}
	egen inpreds`i' = rowmean(pred`i'_1 - pred`i'_5)
	gen inresids`i' = total_mmbtu - inpreds`i'
	gen inerrors`i' = 100*inresids`i'/total_mmbtu
}

gen inresids_full = total_mmbtu - pred_full
gen inerrors_full = 100*inresids_full/total_mmbtu

** binning the mmbtu outcome variable
gen bin_mmbtu = .
forval i = 0(2)34 {
replace bin_mmbtu = `i' if total_mmbtu>`i' & total_mmbtu<=`i'+2
label define mmbtulabs `i' "(`i' `=scalar(`i')+2']", add
}
replace bin_mmbtu = 36 if total_mmbtu>36 & total_mmbtu!=.
label define mmbtulabs 36 ">36", add
label values bin_mmbtu mmbtulabs


*** regressions correlating model performance with the outcome
* cross validated errors
reg cverrors1 ibn.bin_mmbtu if treated==0 & bin_mmbtu>0, nocons vce(cluster Household)
est sto cverrors1

reg inerrors_full ibn.bin_mmbtu if treated==0 & bin_mmbtu>0, nocons vce(cluster Household)
est sto inerrorsfull

reg inerrors1 ibn.bin_mmbtu if treated==0 & bin_mmbtu>0, nocons vce(cluster Household)
est sto inerrors1

coefplot inerrorsfull  inerrors1 cverrors1, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Monthly Energy Usage (MMBtu)") ///
		graphregion(color(white)) bgcolor(white) ///
		legend(title("Number of iterations:", size(small)) order(2 "Full Model In Sample Errors" 4 "CV In Sample Errors" 6 "CV Out of Sample Errors"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/CV_vs_full_errors.png", replace width(5000)



clear
cd "P:\IHWAP\"

** importing cross-validation results from aForge
* cross validation randomly splitting OBSERVATIONS into 5 folds
*import delimited "./Machine Learning/Model Outputs/newCV_predcitpre_results", varnames(1) case(preserve)
import delimited "./Machine Learning/Model Outputs/CV_predcitpre_gbtTune2", varnames(1) case(preserve)
merge m:1 Household using "./Temp Data/IHWAP_State.dta", keepusing(AuditDate FinalDate) keep(3)

gen meterend = mdy(end_month, end_day, end_year)
format meterend %d
sort rowid

* normalizing months since treatment
** some auxiliary variables
/*
gen month_year = mofd(meterend)
format month_year %tm
gen finalmonth = mofd(FinalDate)
format finalmonth %tm

sort Household meterend
by Household : gen treated = 0 if meterend < AuditDate
by Household : replace treated = 1 if meterstart > FinalDate+5 & meterstart!=.
*/

gen aux1 = AuditDate - meterend
gen aux2 = meterend - FinalDate
gen twoyears_prepost=1 if aux1>0 & aux1<=730
replace twoyears_prepost=1 if aux2>0 & aux2<=730

gen mst = ""
forval i = 30(30)360 {
local ival = `i'/30
replace mst = "`ival'" if aux2>=`i' & aux2<`i' + 30
}
forval i = 0(30)360 {
local ival = `i'/30 + 1
replace mst = "-`ival'" if aux1>=`i' & aux1<`i' + 30
}
drop aux1 aux2

gen counter = 1
gen months_since_treat = .
forval i = -12(1)-1 {
replace months_since_treat = counter if mst=="`i'"
replace counter = counter+1
}
forval i = 1(1)12 {
replace months_since_treat = counter if mst=="`i'"
replace counter = counter+1
}
labmask months_since_treat, values(mst)

** cross validated errors for each model
*forval i = 1/8 {
forval i = 1(2)7 {
	gen cvpreds`i' = .
	forval j = 1/5 {
		replace cvpreds`i' = pred`i'_`j' if kfold2==`j'
	}
	gen cvresids`i' = total_mmbtu - cvpreds`i'
	winsor2 cvresids`i', cuts(0.5 99.5) trim replace
	gen cverrors`i' = 100*cvresids`i'/total_mmbtu
	winsor2 cverrors`i', cuts(0.5 99.5) trim replace
}

** in sample errors for each model
*forval i = 1/8 {
forval i = 1(2)7 {
	forval j = 1/5 {
		replace pred`i'_`j' = . if kfold2==`j'
	}
	egen inpreds`i' = rowmean(pred`i'_1 - pred`i'_5)
	gen inresids`i' = total_mmbtu - inpreds`i'
	winsor2 inresids`i', cuts(0.5 99.5) trim replace
	gen inerrors`i' = 100*inresids`i'/total_mmbtu
	winsor2 inerrors`i', cuts(0.5 99.5) trim replace
}


** binning the mmbtu outcome variable
gen bin_mmbtu = .
forval i = 0(2)38 {
replace bin_mmbtu = `i' if total_mmbtu>`i' & total_mmbtu<=`i'+2
label define mmbtulabs `i' "(`i' `=scalar(`i')+2']", add
}
replace bin_mmbtu = 40 if total_mmbtu>40 & total_mmbtu!=.
label define mmbtulabs 40 ">40", add
label values bin_mmbtu mmbtulabs

** number of observations pre and post
sort Household meterend
gen aux = 1
by Household : gen nobs_pre = sum(aux) if treated==0 & twoyears_prepost==1 & inpreds7!=.
by Household : egen totobs_pre = max(nobs_pre)
by Household : gen nobs_post = sum(aux) if treated==1 & twoyears_prepost==1 & inpreds7!=.
by Household : egen totobs_post = max(nobs_post)
drop nobs_pre nobs_post

*** regressions correlating model performance with the outcome
* cross validated errors
*forval i = 1/8 {
forval i = 1(2)7 {
reg cverrors`i' ibn.bin_mmbtu if treated==0 & bin_mmbtu>0, nocons vce(cluster Household)
est sto cverrors`i'
}

coefplot cverrors1 cverrors3 cverrors5 cverrors7 , vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Monthly Energy Usage (MMBtu)") ///
		graphregion(color(white)) bgcolor(white) ///
		legend(title("Number of iterations:", size(small)) order(2 "50" 4 "60" 6 "70" 8 "80"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/CVerrors_gbt_iters.png", replace width(5000)

/*
coefplot cverrors1 cverrors2 cverrors3 cverrors4 cverrors5 cverrors6 cverrors7 cverrors8, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Monthly Energy Usage (MMBtu)") ///
		graphregion(color(white)) bgcolor(white) ///
		legend(order(2 "Model 1" 4 "Model 2" 6 "Model 3" 8 "Model 4" ///
		10 "Model 5" 12 "Model 6" 14 "Model 7" 16 "Model 8"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/CVerrors_gbt.png", replace width(5000)
*/
/*
* cross validated errors
forval i = 1/8 {
reg cverrors`i' ibn.bin_mmbtu if treated==0 & bin_mmbtu>0 & totobs_pre>12 & totobs_post>12, nocons vce(cluster Household)
est sto cverrors`i'
}
*/



* in sample errors
*forval i = 1/8 {
forval i = 1(2)7 {
reg inerrors`i' ibn.bin_mmbtu if treated==0 & bin_mmbtu>0, nocons vce(cluster Household)
est sto inerrors`i'
}

coefplot inerrors1 inerrors3 inerrors5 inerrors7 , vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Monthly Energy Usage (MMBtu)") ///
		graphregion(color(white)) bgcolor(white) ///
		legend(title("Number of iterations:", size(small)) order(2 "50" 4 "60" 6 "70" 8 "80"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/INerrors_gbt_iters.png", replace width(5000)


/*
coefplot inerrors1 inerrors2 inerrors3 inerrors4 inerrors5 inerrors6 inerrors7 inerrors8, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Monthly Energy Usage (MMBtu)") ///
		graphregion(color(white)) bgcolor(white) ///
		legend(order(2 "Model 1" 4 "Model 2" 6 "Model 3" 8 "Model 4" ///
		10 "Model 5" 12 "Model 6" 14 "Model 7" 16 "Model 8"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/INerrors_gbt.png", replace width(5000)
*/

/*
* in sample errors
forval i = 1/8 {
reg inerrors`i' ibn.bin_mmbtu if treated==0 & bin_mmbtu>0 & totobs_pre>12 & totobs_post>12, nocons vce(cluster Household)
est sto inerrors`i'
}
*/


**** now find the model with best out of sample performance and plot errors for that one
*forval i = 1/8 {
forval i = 1(2)7 {
gen sqcvresids`i' = cvresids`i'*cvresids`i'
gen sqinresids`i' = inresids`i'*inresids`i'
}

*forval i = 1/8 {
forval i = 1(2)7 {
qui : sum sqcvresids`i' if treated==0
sca mean`i' = r(mean)
sca rmse`i' = sqrt(mean`i'^2)
qui : sum sqinresids`i' if treated==0
sca mean`i'2 = r(mean)
sca rmse`i'2 = sqrt(mean`i'2^2)
dis "Model `i' RMSE; In sample = "rmse`i'2 "; Cross validated (out of sample) = "rmse`i'
}


*** visual inspection of best model
coefplot inerrors1 cverrors1, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Monthly Energy Usage (MMBtu)") ///
		graphregion(color(white)) bgcolor(white) ///
		legend(order(2 "In Sample Errors" 4 "Cross Validated (Out of Sample) Errors"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/BESTerrors_gbt.png", replace width(5000)


coefplot inerrors2 cverrors2, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Monthly Energy Usage (MMBtu)") ///
		graphregion(color(white)) bgcolor(white) ///
		legend(order(2 "In Sample Errors" 4 "Cross Validated (Out of Sample) Errors"))
		

**** "Event study" graphs for ML method
** program that produces nice estimation table of bootstrap results - matrix labels already predefined
prog def bootstraplabelled, eclass
	args b V N /* input coef matrix, variance matrix, and number of obs */
	ereturn post `b' `V', obs(`=scalar(`N')')
	ereturn local cmd "bootstrapres"
	ereturn local properties "b V"
	ereturn display
end

*** difference between true usage and prediction for each home
sort Household meterend
by Household : gen firstobs = 1 if _n==1

foreach x in 1 7 {
	foreach y in resids errors {
		by Household : egen in`y'`x'_pre = mean(in`y'`x') if treated==0 & twoyears_prepost==1
		by Household : egen avg_in`y'`x'_pre = max(in`y'`x'_pre)
		drop in`y'`x'_pre

		by Household : egen in`y'`x'_post = mean(in`y'`x') if treated==1 & twoyears_prepost==1
		by Household : egen avg_in`y'`x'_post = max(in`y'`x'_post)
		drop in`y'`x'_post
		}
}

foreach x in 1 7 {
	foreach y in resids {
		gen sq_in`y'`x' = in`y'`x'*in`y'`x'
		
		by Household : egen rm_in`y'_pre = total(sq_in`y'`x') if treated==0 & twoyears_prepost==1
		replace rm_in`y'_pre = rm_in`y'_pre/totobs_pre
		by Household : egen rmse_in`y'`x'_pre = max(rm_in`y'_pre)
		drop rm_in`y'_pre
		
		by Household : egen rm_in`y'_post = total(sq_in`y'`x') if treated==1 & twoyears_prepost==1
		replace rm_in`y'_post = rm_in`y'_post/totobs_post
		by Household : egen rmse_in`y'`x'_post = max(rm_in`y'_post)
		drop rm_in`y'_post
		}
}


sum avg_inresids7_pre if firstobs==1, detail
sum avg_inerrors7_pre if firstobs==1, detail
sum avg_inerrors7_pre if firstobs==1, detail
sum rmse_inresids7_pre if firstobs==1, detail


** average errors by household, weight by size of outcome


reg inresids7 ibn.months_since_treat if twoyears_prepost==1 & Household==28796, nocons

matrix ml_ate_mst = e(b)
scalar nobs = e(N)
*matrix input ml_mst = (., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., .)
matrix input ml_mst = (., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., .)
*matrix input ml_mst = (., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., .)
*matrix input ml_mst = (., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., ., .)

forval i = 2/5 {
capture reg inresids`i' ibn.months_since_treat if twoyears_prepost==1 & Household==28796, nocons
capture matrix tempmat = e(b)
capture matrix ml_mst = (ml_mst \ tempmat)
}
mata : st_matrix("ml_mst_var", mm_colvar(st_matrix("ml_mst")))
matrix ml_mst_var = diag(ml_mst_var)
local colnames : colnames ml_ate_mst
mat colnames ml_mst_var=`colnames'
mat rownames ml_mst_var=`colnames'

bootstraplabelled ml_ate_mst ml_mst_var nobs
est store predictuse

coefplot predictuse, vertical xtitle("Months From Treatment (Weatherization)") ytitle("Energy Usage (MMBtu)") ///
						graphregion(color(white)) bgcolor(white) ///
						xlabel(, labsize(small)) legend(order(2 "Realized" 4 "Predicted (ML)"))		
		
		
		
set matsize 10000

reg inresids7 ibn.meterend if end_year>=2007 & end_year<=2011 & Household!=28796 & treated==0, nocons
est sto contsave


reg inresids7 ibn.meterend if twoyears_prepost==1 & Household==28796, nocons
est sto treatsave
local varnames : colnames e(b)


coefplot treatsave contsave, vertical xtitle("Month of Sample") ytitle("ML Model Residuals (MMBtu)") ///
						graphregion(color(white)) bgcolor(white) keep(`varnames') ///
						xlabel(, labsize(small)) legend(order(2 "Household=28796" 4 "Non-Treated Overlapping Households")) ///
						xline(23.5) xlabel(none)
	
				

reg cvresids7 ibn.meterend if end_year>=2007 & end_year<=2011 & Household!=28796 & treated==0, nocons
est sto contsave

reg cvresids7 ibn.meterend if twoyears_prepost==1 & Household==28796, nocons
est sto treatsave
local varnames : colnames e(b)


coefplot treatsave contsave, vertical xtitle("Month of Sample") ytitle("ML Model Cross-Validated Residuals (MMBtu)") ///
						graphregion(color(white)) bgcolor(white) keep(`varnames') ///
						xlabel(, labsize(small)) legend(order(2 "Household=28796" 4 "Non-Treated Overlapping Households")) ///
						xline(23.5) xlabel(none)
	
				


reg total_mmbtu ibn.meterend if twoyears_prepost==1 & Household==28796, nocons
est sto realuse

reg inpreds7 ibn.meterend if twoyears_prepost==1 & Household==28796, nocons
est sto preduse
local varnames : colnames e(b)

coefplot realuse preduse, vertical xtitle("Month of Sample") ytitle("ML Model Cross-Validated Residuals (MMBtu)") ///
						graphregion(color(white)) bgcolor(white) keep(`varnames') ///
						xlabel(, labsize(small)) legend(order(2 "Household=28796" 4 "Non-Treated Overlapping Households")) ///
						xline(23.5) xlabel(none)
	

							
******************** cross validation with folds split by home instead of split by observations
* cross validation randomly splitting HOMES into 5 folds
clear
cd "P:\IHWAP\"

** importing cross-validation results from aForge
import delimited "./Machine Learning/Model Outputs/homeCV_predcitpre_results2", varnames(1) case(preserve)

gen meterend = mdy(end_month, end_day, end_year)
format meterend %d
sort rowid

** cross validated errors for each model
forval i = 1/8 {
	gen cvpreds`i' = .
	forval j = 1/5 {
		replace cvpreds`i' = pred`i'_`j' if kfold==`j'
	}
	gen cvresids`i' = total_mmbtu - cvpreds`i'
	gen cverrors`i' = 100*cvresids`i'/total_mmbtu
}

** in sample errors for each model
forval i = 1/8 {
	forval j = 1/5 {
		replace pred`i'_`j' = . if kfold==`j'
	}
	egen inpreds`i' = rowmean(pred`i'_1 - pred`i'_5)
	gen inresids`i' = total_mmbtu - inpreds`i'
	gen inerrors`i' = 100*inresids`i'/total_mmbtu
}


** binning the mmbtu outcome variable
gen bin_mmbtu = .
forval i = 0(2)38 {
replace bin_mmbtu = `i' if total_mmbtu>`i' & total_mmbtu<=`i'+2
label define mmbtulabs `i' "(`i' `=scalar(`i')+2']", add
}
replace bin_mmbtu = 40 if total_mmbtu>40 & total_mmbtu!=.
label define mmbtulabs 40 ">40", add
label values bin_mmbtu mmbtulabs


*** regressions correlating model performance with the outcome
* cross validated errors
forval i = 1/8 {
reg cverrors`i' ibn.bin_mmbtu if treated==0 & bin_mmbtu>0, nocons vce(cluster Household)
est sto cverrors`i'
}

coefplot cverrors1 cverrors2 cverrors3 cverrors4 cverrors5 cverrors6 cverrors7 cverrors8, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Monthly Energy Usage (MMBtu)") ///
		graphregion(color(white)) bgcolor(white) ///
		legend(order(2 "Model 1" 4 "Model 2" 6 "Model 3" 8 "Model 4" ///
		10 "Model 5" 12 "Model 6" 14 "Model 7" 16 "Model 8"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/homeCVerrors_gbt.png", replace width(5000)

* in sample errors
forval i = 1/8 {
reg inerrors`i' ibn.bin_mmbtu if treated==0 & bin_mmbtu>0, nocons vce(cluster Household)
est sto inerrors`i'
}

coefplot inerrors1 inerrors2 inerrors3 inerrors4 inerrors5 inerrors6 inerrors7 inerrors8, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Monthly Energy Usage (MMBtu)") ///
		graphregion(color(white)) bgcolor(white) ///
		legend(order(2 "Model 1" 4 "Model 2" 6 "Model 3" 8 "Model 4" ///
		10 "Model 5" 12 "Model 6" 14 "Model 7" 16 "Model 8"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/homeINerrors_gbt.png", replace width(5000)


**** now find the model with best out of sample performance and plot errors for that one
forval i = 1/8 {
gen sqcvresids`i' = cvresids`i'*cvresids`i'
gen sqinresids`i' = inresids`i'*inresids`i'
}

forval i = 1/8 {
qui : sum sqcvresids`i' if treated==0
sca mean`i' = r(mean)
sca rmse`i' = sqrt(mean`i'^2)
qui : sum sqinresids`i' if treated==0
sca mean`i'2 = r(mean)
sca rmse`i'2 = sqrt(mean`i'2^2)
dis "Model `i' RMSE; In sample = "rmse`i'2 "; Cross validated (out of sample) = "rmse`i'
}

*** visual inspection of best model
coefplot inerrors4 cverrors4, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Monthly Energy Usage (MMBtu)") ///
		graphregion(color(white)) bgcolor(white) ///
		legend(order(2 "In Sample Errors" 4 "Cross Validated (Out of Sample) Errors"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/homeBESTerrors_gbt.png", replace width(5000)











****************************************************************************************************
****************************************************************************************************
**************************************** TARGETING *************************************************
clear
cd "P:\IHWAP\"

import delimited "./Machine Learning/Model Outputs/predict_train70_treat", varnames(1) case(preserve)

gen meterend = mdy(end_month, end_day, end_year)
format meterend %d

drop end_day end_month end_year
rename predict_train70_treatprediction predict_train70_treat
tempfile mergepredict
save `mergepredict'

clear
import delimited "./Machine Learning/Model Outputs/predict_train30_treat", varnames(1) case(preserve)

gen meterend = mdy(end_month, end_day, end_year)
format meterend %d

drop end_day end_month end_year
rename predict_train30_treatprediction predict_train30_treat
append using `mergepredict'
tempfile mergepredict
save `mergepredict'

clear
import delimited "./Machine Learning/Model Outputs/predict_train70_notreat", varnames(1) case(preserve)

gen meterend = mdy(end_month, end_day, end_year)
format meterend %d

drop end_day end_month end_year
rename predict_train70_notreatpredictio predict_train70_notreat
merge 1:1 Household meterend using `mergepredict', nogen
tempfile mergepredict
save `mergepredict'

clear
import delimited "./Machine Learning/Model Outputs/predict_train30_notreat", varnames(1) case(preserve)

gen meterend = mdy(end_month, end_day, end_year)
format meterend %d

drop end_day end_month end_year
rename predict_train30_notreatpredictio predict_train30_notreat
merge 1:1 Household meterend using `mergepredict', nogen

merge 1:1 Household meterend using "./Temp Data/tempdata.dta", nogen


**** assessing targeting ml model performance
** binning the mmbtu outcome variable
gen bin_mmbtu = .
forval i = 0(2)38 {
replace bin_mmbtu = `i' if total_mmbtu>`i' & total_mmbtu<=`i'+2
label define mmbtulabs `i' "(`i' `=scalar(`i')+2']", add
}
replace bin_mmbtu = 40 if total_mmbtu>40 & total_mmbtu!=.
label define mmbtulabs 40 ">40", add
label values bin_mmbtu mmbtulabs

* generate residuals and percent errors
gen resids = total_mmbtu - ml_predictions
gen pcterror = 100*(total_mmbtu - ml_predictions)/total_mmbtu

gen resids_train70_notreat = total_mmbtu - predict_train70_notreat
gen pcterror_train70_notreat = 100*(total_mmbtu - predict_train70_notreat)/total_mmbtu

gen resids_train30_notreat = total_mmbtu - predict_train30_notreat
gen pcterror_train30_notreat = 100*(total_mmbtu - predict_train30_notreat)/total_mmbtu

gen resids_train70_treat = total_mmbtu - predict_train70_treat
gen pcterror_train70_treat = 100*(total_mmbtu - predict_train70_treat)/total_mmbtu

gen resids_train30_treat = total_mmbtu - predict_train30_treat
gen pcterror_train30_treat = 100*(total_mmbtu - predict_train30_treat)/total_mmbtu

gen testdata = 0 if resids_train70_notreat!=.
replace testdata = 1 if resids_train30_notreat!=.

* regressions correlating model performance with the outcome - pre-treatment
reg pcterror ibn.bin_mmbtu if treated==0 & twoyears_prepost==1 & meterdays_monthly!=. & bin_mmbtu>0, nocons vce(cluster Household)
est sto errorsfull_notreat

reg pcterror_train70_notreat ibn.bin_mmbtu if treated==0 & twoyears_prepost==1 & meterdays_monthly!=. & bin_mmbtu>0 & testdata==0, nocons vce(cluster Household)
est sto errorstrain70_notreat

reg pcterror_train30_notreat ibn.bin_mmbtu if treated==0 & twoyears_prepost==1 & meterdays_monthly!=. & bin_mmbtu>0 & testdata==1, nocons vce(cluster Household)
est sto errorstrain30_notreat

** plotting performance
coefplot errorsfull_notreat , vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Pre-Treatment Monthly Energy Usage (MMBtu)") ///
		legend(order(2 "Heterogeneity (pre-WAP) Model")) ///
		graphregion(color(white)) bgcolor(white)
graph export "./Temp Data/Regression Outputs/Heterogeneity/mlerror_notreat.png", replace width(5000)

/*
coefplot errorsfull_notreat errorstrain70_notreat errorstrain30_notreat, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Pre-Treatment Monthly Energy Usage (MMBtu)") ///
		legend(order(2 "Heterogeneity Model" 4 "Targeting Model (train)" 6 "Targeting Model (test)")) ///
		graphregion(color(white)) bgcolor(white)
graph export "./Temp Data/Regression Outputs/Heterogeneity/mlerror_notreat.png", replace width(5000)
*/

* regressions correlating model performance with the outcome - post-treatment
reg pcterror ibn.bin_mmbtu if treated==1 & twoyears_prepost==1 & meterdays_monthly!=. & bin_mmbtu>0 , nocons vce(cluster Household)
est sto errorsfull_treat

reg pcterror_train70_treat ibn.bin_mmbtu if treated==1 & twoyears_prepost==1 & meterdays_monthly!=. & bin_mmbtu>0 & testdata==0, nocons vce(cluster Household)
est sto errorstrain70_treat

reg pcterror_train30_treat ibn.bin_mmbtu if treated==1 & twoyears_prepost==1 & meterdays_monthly!=. & bin_mmbtu>0 & testdata==1, nocons vce(cluster Household)
est sto errorstrain30_treat

** plotting performance
coefplot errorstrain70_treat errorstrain30_treat, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Post-Treatment Monthly Energy Usage (MMBtu)") ///
		legend(order(2 "Targeting (post-WAP) Model (train)" 4 "Targeting (post-WAP) Model (test)")) ///
		graphregion(color(white)) bgcolor(white)
graph export "./Temp Data/Regression Outputs/Heterogeneity/mlerror_treat.png", replace width(5000)

/*
** plotting performance
coefplot errorsfull_treat errorstrain70_treat errorstrain30_treat, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Post-Treatment Monthly Energy Usage (MMBtu)") ///
		legend(order(2 "Heterogeneity Model" 4 "Targeting Model (train)" 6 "Targeting Model (test)")) ///
		graphregion(color(white)) bgcolor(white)
graph export "./Temp Data/Regression Outputs/Heterogeneity/mlerror_treat.png", replace width(5000)
*/

************** household-specific energy USAGE 
sort Household end_month
by Household end_month : egen monthly_real_usage = mean(total_mmbtu) if treated==0 & twoyears_prepost==1 & meterdays_monthly!=.
forval i = 1/12 {
by Household : egen use_month`i' = max(monthly_real_usage) if end_month==`i'
by Household : egen real_use_month`i' = max(use_month`i')
drop use_month`i'
}
egen num_months = rownonmiss(real_use_month*)
egen realized_usage_pre = rowtotal(real_use_month*) if num_months==12
drop monthly_real_usage num_months real_use_month*

sort Household end_month
by Household end_month : egen monthly_real_usage = mean(total_mmbtu) if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
forval i = 1/12 {
by Household : egen use_month`i' = max(monthly_real_usage) if end_month==`i'
by Household : egen real_use_month`i' = max(use_month`i')
drop use_month`i'
}
egen num_months = rownonmiss(real_use_month*)
egen realized_usage_post = rowtotal(real_use_month*) if num_months==12
drop monthly_real_usage num_months real_use_month*

sum realized_usage_pre if ww_treat==1, detail
sum realized_usage_post if ww_treat==1, detail

* predicted usage
sort Household end_month
by Household end_month : egen monthly_real_usage = mean(ml_predictions) if treated==0 & twoyears_prepost==1 & meterdays_monthly!=.
forval i = 1/12 {
by Household : egen use_month`i' = max(monthly_real_usage) if end_month==`i'
by Household : egen real_use_month`i' = max(use_month`i')
drop use_month`i'
}
egen num_months = rownonmiss(real_use_month*)
egen predicted_usage_pre = rowtotal(real_use_month*) if num_months==12
drop monthly_real_usage num_months real_use_month*

sort Household end_month
by Household end_month : egen monthly_real_usage = mean(ml_predictions) if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
forval i = 1/12 {
by Household : egen use_month`i' = max(monthly_real_usage) if end_month==`i'
by Household : egen real_use_month`i' = max(use_month`i')
drop use_month`i'
}
egen num_months = rownonmiss(real_use_month*)
egen predicted_usage_counterpre = rowtotal(real_use_month*) if num_months==12
drop monthly_real_usage num_months real_use_month*

sort Household end_month
by Household end_month : egen monthly_real_usage = mean(predict_train70_treat) if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
forval i = 1/12 {
by Household : egen use_month`i' = max(monthly_real_usage) if end_month==`i'
by Household : egen real_use_month`i' = max(use_month`i')
drop use_month`i'
}
egen num_months = rownonmiss(real_use_month*)
egen predicted_usage_posttrain = rowtotal(real_use_month*) if num_months==12
drop monthly_real_usage num_months real_use_month*

sort Household end_month
by Household end_month : egen monthly_real_usage = mean(predict_train30_treat) if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
forval i = 1/12 {
by Household : egen use_month`i' = max(monthly_real_usage) if end_month==`i'
by Household : egen real_use_month`i' = max(use_month`i')
drop use_month`i'
}
egen num_months = rownonmiss(real_use_month*)
egen predicted_usage_posttest = rowtotal(real_use_month*) if num_months==12
drop monthly_real_usage num_months real_use_month*


* household-specific histogram of usage
gen hist_homemmbtu_pre = realized_usage_pre
replace hist_homemmbtu_pre=261 if realized_usage_pre>260 & realized_usage_pre!=.

gen hist_homemmbtu_post = realized_usage_post
replace hist_homemmbtu_post=261 if realized_usage_post>260 & realized_usage_post!=.

histogram hist_homemmbtu_pre if ww_treat==1, ///
	start(0) width(20) percent graphregion(color(white)) ///
	bgcolor(white) bcolor(blue%30) xlabel(0(20)280) ///
	xtitle("Household Energy Usage (MMBtu)") ytitle("Percent of Sample")
graph export "./Temp Data/Regression Outputs/Heterogeneity/Homeusage_histogram.png", replace width(5000)


twoway (histogram hist_homemmbtu_pre if ww_treat==1, ///
	start(0) width(20) percent graphregion(color(white)) ///
	bgcolor(white) bcolor(blue%30) xlabel(0(20)280) ///
	xtitle("Monthly Energy Usage (MMBtu)") ytitle("Percent of Sample")) ///
	(histogram hist_homemmbtu_post if ww_treat==1, ///
	start(0) width(20) percent graphregion(color(white)) ///
	bgcolor(white) bcolor(red%30) xlabel(0(20)280) ///
	xtitle("Monthly Energy Usage (MMBtu)") ytitle("Percent of Sample")), ///
	legend(order(1 "Pre-WAP" 2 "Post-WAP"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/Homeusage_histogram_septreat.png", replace width(5000)


* household-specific residuals and percent errors
gen home_resids = realized_usage_pre - predicted_usage_pre
gen home_pcterror = 100*(realized_usage_pre - predicted_usage_pre)/realized_usage_pre

gen home_resids_train_treat = realized_usage_post - predicted_usage_posttrain
gen home_pcterror_train_treat = 100*(realized_usage_post - predicted_usage_posttrain)/realized_usage_post

gen home_resids_test_treat = realized_usage_post - predicted_usage_posttest
gen home_pcterror_test_treat = 100*(realized_usage_post - predicted_usage_posttest)/realized_usage_post


* regressions correlating model home-specific performance with the outcome - pre-treatment
gen bin_homemmbtu_pre = .
replace bin_homemmbtu_pre = 260 if realized_usage_pre>260 & realized_usage_pre!=.
label define homemmbtulabs 260 ">260"
forval i = 0(20)240 {
replace bin_homemmbtu_pre = `i' if realized_usage_pre>`i' & realized_usage_pre<=`i'+20
label define homemmbtulabs `i' "(`i' `=scalar(`i')+20']", add
}
label values bin_homemmbtu_pre homemmbtulabs

gen bin_homemmbtu_post = .
forval i = 0(20)240 {
replace bin_homemmbtu_post = `i' if realized_usage_post>`i' & realized_usage_post<=`i'+20
}
replace bin_homemmbtu_post = 260 if realized_usage_post>260 & realized_usage_post!=.
label values bin_homemmbtu_post homemmbtulabs

* regressions to assess performance
reg home_pcterror ibn.bin_homemmbtu_pre if ww_treat==1, nocons vce(cluster Household)
est sto errorsfull_notreat

reg home_pcterror_train_treat ibn.bin_homemmbtu_post if ww_treat==1, nocons vce(cluster Household)
est sto errorstrain70_treat

reg home_pcterror_test_treat ibn.bin_homemmbtu_post if ww_treat==1 & bin_homemmbtu_post>20, nocons vce(cluster Household)
est sto errorstrain30_treat

** plotting performance - pre-treatment
coefplot errorsfull_notreat , vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Pre-Treatment Household Energy Usage (MMBtu)") ///
		legend(order(2 "Heterogeneity (pre-WAP) Model")) ///
		graphregion(color(white)) bgcolor(white)
graph export "./Temp Data/Regression Outputs/Heterogeneity/home_mlerror_notreat.png", replace width(5000)


** plotting performance - post-treatment
coefplot errorstrain70_treat errorstrain30_treat, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Percent Errors (%)") ///
		xtitle("Post-Treatment Household Energy Usage (MMBtu)") ///
		legend(order(2 "Targeting (post-WAP) Model (train)" 4 "Targeting (post-WAP) Model (test)")) ///
		graphregion(color(white)) bgcolor(white)
graph export "./Temp Data/Regression Outputs/Heterogeneity/home_mlerror_treat.png", replace width(5000)

************** household-specific energy SAVINGS from different methods
gen home_save = 100*(realized_usage_post - predicted_usage_counterpre)/predicted_usage_counterpre
gen home_save_train = 100*(predicted_usage_posttrain - predicted_usage_counterpre)/predicted_usage_counterpre
gen home_save_test = 100*(predicted_usage_posttest - predicted_usage_counterpre)/predicted_usage_counterpre


***** plotting savings according to different models
reg home_save ibn.bin_homemmbtu_pre if ww_treat==1 , nocons vce(cluster Household)
est sto savefull

reg home_save_train ibn.bin_homemmbtu_pre if ww_treat==1 , nocons vce(cluster Household)
est sto savetrain

reg home_save_test ibn.bin_homemmbtu_pre if ww_treat==1 , nocons vce(cluster Household)
est sto savetest

coefplot savefull savetrain savetest, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Energy Savings (%)") ///
		xtitle("Pre-Treatment Household Energy Usage (MMBtu)") ///
		legend(order(2 "Heterogeneity Model" 4 "Targeting Model (train)" 6 "Targeting Model (test)")) ///
		graphregion(color(white)) bgcolor(white)
graph export "./Temp Data/Regression Outputs/Heterogeneity/home_mlsave_vspre.png", replace width(5000)


reg home_save ibn.bin_homemmbtu_post if ww_treat==1 , nocons vce(cluster Household)
est sto savefull

reg home_save_train ibn.bin_homemmbtu_post if ww_treat==1 , nocons vce(cluster Household)
est sto savetrain

reg home_save_test ibn.bin_homemmbtu_post if ww_treat==1 , nocons vce(cluster Household)
est sto savetest

coefplot savefull savetrain savetest, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Energy Savings (%)") ///
		xtitle("Post-Treatment Household Energy Usage (MMBtu)") ///
		legend(order(2 "Heterogeneity Model" 4 "Targeting Model (train)" 6 "Targeting Model (test)")) ///
		graphregion(color(white)) bgcolor(white)
graph export "./Temp Data/Regression Outputs/Heterogeneity/home_mlsave_vspost.png", replace width(5000)



************************** MONTHLY ENERGY SAVINGS
* realized savings from the heterogeneity model
sort Household end_month
by Household end_month : egen monthly_real_save = mean(pct_tau_ml) if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
forval i = 1/12 {
by Household : egen save_month`i' = max(monthly_real_save) if end_month==`i'
by Household : egen real_save_month`i' = max(save_month`i')
drop save_month`i'
}
egen num_months = rownonmiss(real_save_month*)
egen realized_savings_het = rowmean(real_save_month*) if num_months==12
drop monthly_real_save num_months real_save_month*

* realized savings from the targeting model - training sample
gen pct_save_train = (predict_train70_treat - ml_predictions)/ml_predictions
winsor2 pct_save_train, cuts(0.5 99.5) trim replace

sort Household end_month
by Household end_month : egen monthly_real_save = mean(pct_save_train) if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
forval i = 1/12 {
by Household : egen save_month`i' = max(monthly_real_save) if end_month==`i'
by Household : egen real_save_month`i' = max(save_month`i')
drop save_month`i'
}
egen num_months = rownonmiss(real_save_month*)
egen realized_savings_train = rowmean(real_save_month*) if num_months==12
drop monthly_real_save num_months real_save_month*


* realized savings from the targeting model - testing sample
gen pct_save_test = (predict_train30_treat - ml_predictions)/ml_predictions
winsor2 pct_save_test, cuts(0.5 99.5) trim replace

sort Household end_month
by Household end_month : egen monthly_real_save = mean(pct_save_test) if treated==1 & twoyears_prepost==1 & meterdays_monthly!=.
forval i = 1/12 {
by Household : egen save_month`i' = max(monthly_real_save) if end_month==`i'
by Household : egen real_save_month`i' = max(save_month`i')
drop save_month`i'
}
egen num_months = rownonmiss(real_save_month*)
egen realized_savings_test = rowmean(real_save_month*) if num_months==12
drop monthly_real_save num_months real_save_month*


***** plotting savings according to different models
gen counterbin_mmbtu = .
forval i = 0(2)38 {
replace counterbin_mmbtu = `i' if ml_predictions>`i' & ml_predictions<=`i'+2
}
replace counterbin_mmbtu = 40 if ml_predictions>40 & ml_predictions!=.
label values counterbin_mmbtu mmbtulabs

** pre treat in x-axis
reg pct_tau_ml ibn.counterbin_mmbtu if treated==1 & twoyears_prepost==1 & meterdays_monthly!=. & counterbin_mmbtu>0 , nocons vce(cluster Household)
est sto savefull

reg pct_save_train ibn.counterbin_mmbtu if treated==1 & twoyears_prepost==1 & meterdays_monthly!=. & counterbin_mmbtu>0 & testdata==0, nocons vce(cluster Household)
est sto savetrain

reg pct_save_test ibn.counterbin_mmbtu if treated==1 & twoyears_prepost==1 & meterdays_monthly!=. & counterbin_mmbtu>0 & testdata==1, nocons vce(cluster Household)
est sto savetest

coefplot savefull savetrain savetest, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Energy Savings (%)") ///
		xtitle("(Counterfactual) Pre-Treatment Monthly Energy Usage (MMBtu)") ///
		legend(order(2 "Heterogeneity Model" 4 "Targeting Model (train)" 6 "Targeting Model (test)")) ///
		graphregion(color(white)) bgcolor(white)
graph export "./Temp Data/Regression Outputs/Heterogeneity/mlsave_vspre.png", replace width(5000)

** post treat in x-axis
reg pct_tau_ml ibn.bin_mmbtu if treated==1 & twoyears_prepost==1 & meterdays_monthly!=. & bin_mmbtu>0 , nocons vce(cluster Household)
est sto savefull

reg pct_save_train ibn.bin_mmbtu if treated==1 & twoyears_prepost==1 & meterdays_monthly!=. & bin_mmbtu>0 & testdata==0, nocons vce(cluster Household)
est sto savetrain

reg pct_save_test ibn.bin_mmbtu if treated==1 & twoyears_prepost==1 & meterdays_monthly!=. & bin_mmbtu>0 & testdata==1, nocons vce(cluster Household)
est sto savetest

coefplot savefull savetrain savetest, vertical ///
		xlabel(,labsize(small) angle(45)) ytitle("ML Model Energy Savings (%)") ///
		xtitle("Post-Treatment Monthly Energy Usage (MMBtu)") ///
		legend(order(2 "Heterogeneity Model" 4 "Targeting Model (train)" 6 "Targeting Model (test)")) ///
		graphregion(color(white)) bgcolor(white)
graph export "./Temp Data/Regression Outputs/Heterogeneity/mlsave_vspost.png", replace width(5000)


** finally comparing savings from different methods
keep if ww_treat==1

gsort -realized_savings_het
gen negsave_het = -realized_savings_het
cumul negsave_het, gen(pctile_het)
replace pctile_het = round(pctile_het, 0.01)
tostring pctile_het, replace force format(%9.2f)
gen obsid_het = _n if realized_savings_het!=.
labmask obsid_het, values(pctile_het)
destring pctile_het, replace

gsort -realized_savings_train
gen negsave_train = -realized_savings_train
cumul negsave_train, gen(pctile_train)
replace pctile_train = round(pctile_train, 0.01)
tostring pctile_train, replace force format(%9.2f)
gen obsid_train = _n if realized_savings_train!=.
labmask obsid_train, values(pctile_train)
destring pctile_train, replace

gsort -realized_savings_test
gen negsave_test = -realized_savings_test
cumul negsave_test, gen(pctile_test)
replace pctile_test = round(pctile_test, 0.01)
tostring pctile_test, replace force format(%9.2f)
gen obsid_test = _n if realized_savings_test!=.
labmask obsid_test, values(pctile_test)
destring pctile_test, replace

************ graphs ranking energy savings
sum realized_savings_het
sca mean_save = round(r(mean),0.01)
twoway (scatter realized_savings_het obsid_het, msize(vsmall) mcolor(black)), ///
	ytitle("WAP Energy Savings (%)") ///
	xlabel(30(600)4800, labsize(small) angle(45) valuelabel) ///
	ylabel(-0.8(0.1)1.2, labsize(small)) ///
	xtitle("Home Percentile Rank: more savings {&rarr}") ///
	graphregion(color(white)) bgcolor(white) ///
	yline(0, lcolor(black)) yline(`=scalar(mean_save)', lcolor(red)) ///
	text(`=scalar(mean_save)-0.05' 1000 "Avg. savings = `=scalar(mean_save)'", placement(w) color(red) size(small) just(left))
graph export "./Temp Data/Regression Outputs/Heterogeneity/saverank_het.png", replace

sum realized_savings_train
sca mean_save = round(r(mean),0.01)
twoway (scatter realized_savings_train obsid_train, msize(vsmall) mcolor(black)), ///
	ytitle("WAP Energy Savings (%)") ///
	xlabel(30(300)3000, labsize(small) angle(45) valuelabel) ///
	ylabel(-0.8(0.1)1.2, labsize(small)) ///
	xtitle("Home Percentile Rank: more savings {&rarr}") ///
	graphregion(color(white)) bgcolor(white) ///
	yline(0, lcolor(black)) yline(`=scalar(mean_save)', lcolor(red)) ///
	text(`=scalar(mean_save)-0.05' 1000 "Avg. savings = `=scalar(mean_save)'", placement(w) color(red) size(small) just(left))
graph export "./Temp Data/Regression Outputs/Heterogeneity/saverank_train.png", replace

sum realized_savings_test
sca mean_save = round(r(mean),0.01)
twoway (scatter realized_savings_test obsid_test, msize(vsmall) mcolor(black)), ///
	ytitle("WAP Energy Savings (%)") ///
	xlabel(30(200)1200, labsize(small) angle(45) valuelabel) ///
	ylabel(-0.8(0.1)1.2, labsize(small)) ///
	xtitle("Home Percentile Rank: more savings {&rarr}") ///
	graphregion(color(white)) bgcolor(white) ///
	yline(0, lcolor(black)) yline(`=scalar(mean_save)', lcolor(red)) ///
	text(`=scalar(mean_save)-0.05' 500 "Avg. savings = `=scalar(mean_save)'", placement(w) color(red) size(small) just(left))
graph export "./Temp Data/Regression Outputs/Heterogeneity/saverank_test.png", replace


twoway (scatter realized_savings_het realized_savings_train, msize(vsmall) mcolor(black)) ///
	(lfit realized_savings_het realized_savings_train), ///
	ytitle("Targeting Model Savings (%)" "Train Data") ///
	xtitle("Heterogeneity Model Savings (%)") ///
	graphregion(color(white)) bgcolor(white) ///
	legend(order(2 "Linear Fit"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/corr_save_train.png", replace

twoway (scatter realized_savings_het realized_savings_test, msize(vsmall) mcolor(black)) ///
	(lfit realized_savings_het realized_savings_test), ///
	ytitle("Targeting Model Savings (%)" "Test Data") ///
	xtitle("Heterogeneity Model Savings (%)") ///
	graphregion(color(white)) bgcolor(white) ///
	legend(order(2 "Linear Fit"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/corr_save_test.png", replace



*** assess accuracy of percentiles
forval i = 1/9 {
gen perc`i'_het = pctile_het>=0.`i' - 0.1 & pctile_het<0.`i' & pctile_het!=.
replace perc`i'_het = . if pctile_het==.
}
gen perc10_het = pctile_het>=0.9 & pctile_het!=.
replace perc10_het = . if pctile_het==.

forval i = 1/9 {
gen perc`i'_train = pctile_train>=0.`i' - 0.1 & pctile_train<0.`i' & pctile_train!=.
replace perc`i'_train = . if pctile_train==.
}
gen perc10_train = pctile_train>=0.9 & pctile_train!=.
replace perc10_train = . if pctile_train==.

forval i = 1/9 {
gen perc`i'_test = pctile_test>=0.`i' - 0.1 & pctile_test<0.`i' & pctile_test!=.
replace perc`i'_test = . if pctile_test==.
}
gen perc10_test = pctile_test>=0.9 & pctile_test!=.
replace perc10_test = . if pctile_test==.

forval i = 1/10 {
gen acc`i'_train = perc`i'_het==perc`i'_train if perc`i'_het==1
replace acc`i'_train = . if perc`i'_train==.
}

forval i = 1/10 {
gen acc`i'_test = perc`i'_het==perc`i'_test if perc`i'_het==1
replace acc`i'_test = . if perc`i'_test==.
}

forval i = 1/10 {
reg acc`i'_train
est sto corr`i'_train
}

forval i = 1/10 {
reg acc`i'_test
est sto corr`i'_test
}

coefplot corr1_train || corr2_train || corr3_train || corr4_train || corr5_train || ///
	corr6_train || corr7_train || corr8_train || corr9_train || corr10_train ///
	, bycoefs vertical ///
	ytitle("Percent Correctly Identified Within Bin (%)" "Train Data") ///
	xtitle("Bins of Percentile Savings from Heterogeneity Model") ///
	graphregion(color(white)) bgcolor(white) ///
	xlabel(1 "[0% 10%)" 2 "[10% 20%)" 3 "[20% 30%)" 4 "[30% 40%)" 5 "[40% 50%)" ///
	6 "[50% 60%)" 7 "[60% 70%)" 8 "[70% 80%)" 9 "[80% 90%)" 10 "[90% 100%]", labsize(small) angle(45))
graph export "./Temp Data/Regression Outputs/Heterogeneity/accuracy_bin10_train.png", replace
	
	
coefplot corr1_test || corr2_test || corr3_test || corr4_test || corr5_test || ///
	corr6_test || corr7_test || corr8_test || corr9_test || corr10_test ///
	, bycoefs vertical ///
	ytitle("Percent Correctly Identified Within Bin (%)" "Test Data") ///
	xtitle("Bins of Percentile Rank from Heterogeneity Model") ///
	graphregion(color(white)) bgcolor(white) ///
	xlabel(1 "[0% 10%)" 2 "[10% 20%)" 3 "[20% 30%)" 4 "[30% 40%)" 5 "[40% 50%)" ///
	6 "[50% 60%)" 7 "[60% 70%)" 8 "[70% 80%)" 9 "[80% 90%)" 10 "[90% 100%]", labsize(small) angle(45))
graph export "./Temp Data/Regression Outputs/Heterogeneity/accuracy_bin10_test.png", replace
	
coefplot corr1_train corr1_test || corr2_train corr2_test || corr3_train corr3_test || corr4_train corr4_test || corr5_train corr5_test || ///
	corr6_train corr6_test || corr7_train corr7_test || corr8_train corr8_test || corr9_train corr9_test || corr10_train corr10_test ///
	, bycoefs vertical ///
	ytitle("Percent Correctly Identified Within Bin (%)") ///
	xtitle("Bins of Percentile Rank from Heterogeneity Model") ///
	graphregion(color(white)) bgcolor(white) ///
	xlabel(1 "[0% 10%)" 2 "[10% 20%)" 3 "[20% 30%)" 4 "[30% 40%)" 5 "[40% 50%)" ///
	6 "[50% 60%)" 7 "[60% 70%)" 8 "[70% 80%)" 9 "[80% 90%)" 10 "[90% 100%]", labsize(small) angle(45)) ///
	legen(order(2 "Train Data" 4 "Test Data"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/accuracy_bin10_full.png", replace

	
** top 85% homes
gen perc85_het = pctile_het>=0.15 & pctile_het!=.
replace perc85_het = . if pctile_het==.
gen perc15_het = pctile_het<0.15
replace perc15_het = . if pctile_het==.

gen perc85_train = pctile_train>=0.15 & pctile_train!=.
replace perc85_train = . if pctile_train==.
gen perc15_train = pctile_train<0.15
replace perc15_train = . if pctile_train==.

gen perc85_test = pctile_test>=0.15 & pctile_test!=.
replace perc85_test = . if pctile_test==.
gen perc15_test = pctile_test<0.15
replace perc15_test = . if pctile_test==.

gen acc85_train = perc85_het==perc85_train if perc85_het==1
replace acc85_train = . if perc85_train==.
gen acc85_test = perc85_het==perc85_test if perc85_het==1
replace acc85_test = . if perc85_test==.

gen acc15_train = perc15_het==perc15_train if perc15_het==1
replace acc15_train = . if perc15_train==.
gen acc15_test = perc15_het==perc15_test if perc15_het==1
replace acc15_test = . if perc15_test==.

reg acc85_train
est sto corr85_train
reg acc85_test
est sto corr85_test
reg acc15_train
est sto corr15_train
reg acc15_test
est sto corr15_test

coefplot corr85_train corr85_test ///
	, bycoefs vertical ///
	ytitle("Percent Correctly Identified Within Bin (%)") ///
	xtitle("Bins of Percentile Rank from Heterogeneity Model") ///
	graphregion(color(white)) bgcolor(white) ///
	xlabel(1 "[15% 100%]", labsize(small) angle(45)) ///
	legen(order(2 "Train Data" 4 "Test Data"))
graph export "./Temp Data/Regression Outputs/Heterogeneity/accuracy_bin85_full.png", replace




