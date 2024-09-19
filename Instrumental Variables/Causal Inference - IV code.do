************************************************************************
***************SETUP CODE HEADER FOR ALL PROGRAMS***********************
***************									 ***********************
************************************************************************
clear
clear matrix
clear mata
cap log close

cd "C:\ECON485\Assignments\Assignment3\datasets"
************************************************************************
************************************************************************
log using "Assignment3.txt", text replace

************ Part I. Data Generation***************
set seed 1203
set obs 1000

gen X_i = invnorm(uniform())
gen A_i = invnorm(uniform())
gen epsilon_i = invnorm(uniform())
gen epsilon3 = invnorm(uniform())*3
gen epsilont = invnorm(uniform())*2

gen Z_1 = runiformint(0,1)
gen Z_2 = runiformint(0,1)
gen Z_3 = (epsilon3 + A_i > 0)
gen T_i = (5*Z_1 + 0.01*Z_2 + Z_3 + X_i + 10*A_i + epsilont > 0)


* population parameters of tau = beta0 = beta = gamma = 1
gen logY_i = 1*T_i + 1 + 1*X_i + 1*A_i + epsilon_i


* Summary statistics for n = 1000
eststo clear
estpost summarize logY_i T_i X_i A_i epsilon_i Z_1 Z_2 Z_3 epsilon3 epsilont
esttab using 1k_DGP_summary.tex, replace cells("mean sd min max") nonumber title("Summary Table of Select Variables")

eststo clear
qui{
eststo: reg logY_i T_i X_i, vce(robust)
eststo: ivreg2 logY_i X_i (T_i = Z_1), robust
	local A1 = round(e(cdf),0.01)
eststo: ivreg2 logY_i X_i (T_i = Z_2), robust
eststo: ivreg2 logY_i X_i (T_i = Z_3), robust
eststo: ivreg2 logY_i X_i (T_i = Z_1 Z_2), robust
}

esttab _all using 1k_DGP_table.tex, style(tex) replace se star(* 0.1 ** 0.05 *** 0.01) b(%9.3f) nogaps nomtitles mlabels("OLS" "IV w/ Z_1" "IV w/ Z_2" "IV w/ Z_3" "IV w/ Z_1 and Z_2")



* Relationship between Xi and Z1
eststo clear
eststo: reg X_i Z_1, vce(robust)
esttab using instrumentrelevance.tex, replace se star(* 0.1 ** 0.05 *** 0.01) b(%9.3f) nogaps nomtitles title("Relationship Between X and Z_1") coeflabels(_cons "Constant") mlabels("Dep. var = X" )


**** Data simulation with n = 100,000*************************************************
clear
set seed 120302
set obs 100000

gen X_i = invnorm(uniform())
gen A_i = invnorm(uniform())
gen epsilon_i = invnorm(uniform())
gen epsilon3 = invnorm(uniform())*3
gen epsilont = invnorm(uniform())*2
gen Z_1 = runiformint(0,1)
gen Z_2 = runiformint(0,1)
gen Z_3 = (epsilon3 + A_i > 0)
gen T_i = (5*Z_1 + 0.01*Z_2 + Z_3 + 10*A_i + epsilont > 0)
gen logY_i = 1*T_i + 1 + 1*X_i + 1*A_i + epsilon_i
* Summary statistics for n = 100000
eststo clear
estpost summarize logY_i T_i X_i A_i epsilon_i Z_1 Z_2 Z_3 epsilon3 epsilont
esttab using 100k_DGP_summary.tex, replace cells("mean sd min max") nonumber title("Summary Table of Select Variables")

* Wald estimator Subsample Means 
*First-stage
summarize T_i if Z_1 == 0, meanonly
local T0 = `r(mean)'
summarize T_i if Z_1 == 1, meanonly
local T1 = `r(mean)'
*Reduced
summarize logY_i if Z_1 == 0, meanonly
local Y0 = `r(mean)'
summarize logY_i if Z_1 == 1, meanonly
local Y1 = `r(mean)'

* Displaying means
eststo clear
estpost tabstat logY_i T_i, by(Z_1) statistics(mean) columns(statistics) listwise nototal 
esttab using subsample_means.tex, replace main(mean) unstack nonote nonum eqlabels(`e(labels)') title("Subsample Means Conditional on Z_1") b(a5)


display "Wald Estimate: " (`Y1' - `Y0') /(`T1' - `T0')
 

eststo clear
eststo: ivreg2 logY_i (T_i = Z_1), robust
esttab using ivreg2results.tex, replace se star(* 0.1 ** 0.05 *** 0.01) b(%9.5f) nogaps nomtitles title("IV w/ Z_1") coeflabels(_cons "Constant") 

************ Part II. Angrist and Lavy Replication***************
use "final4.dta", clear
eststo clear
qui{
eststo: reg avgverb classize, vce(robust)
eststo: reg avgverb classize tipuach, vce(robust) 
eststo: reg avgverb classize tipuach c_size, vce(robust) 
eststo: reg avgmath classize, vce(robust)
eststo: reg avgmath classize tipuach, vce(robust) 
eststo: reg avgmath classize tipuach c_size, vce(robust) 
}

esttab _all using AngristLavy.tex, style(tex) replace se star(* 0.1 ** 0.05 *** 0.01) b(%9.3f) nogaps nomtitles order(classize tipuach c_size) mlabels("(7)" "(8)" "(9)" "(10)" "(11)" "(12)") nonumb  coeflabel(classize "Class Size" tipuach "Percent disadvantaged" c_size "Enrollment") title("OLS Estimates for 1991") drop(_cons) stats(rmse r2 N, labels("Root MSE" "R$^2$" "N" )) collabels(,none)

**Partialling Out Approach**
use "final4.dta", clear
* Goal is to obtain classize coefficient in this model:
reg avgmath classize tipuach c_size, vce(robust)
* Regress classize on c_size and tipuach
regress classize c_size tipuach if e(sample), vce(robust) 
* Obtain predicted residuals
predict classize_hat, resid
* Regress avgmath on predicted residuals
regress avgmath classize_hat if e(sample), vce(robust)

eststo clear
qui{ 
eststo: reg avgmath classize tipuach c_size, vce(robust) 
eststo: regress avgmath classize_hat if e(sample), vce(robust)
}
esttab _all using 6dAngristLavy.tex, style(tex) replace se star(* 0.1 ** 0.05 *** 0.01) b(%9.3f) nogaps nomtitles order(classize classize_hat tipuach c_size)  mlabels("(12)" "Partialled-Out") nonumb coeflabel(classize "Class Size" tipuach "Percent disadvantaged" c_size "Enrollment" _cons "Constant") title("OLS and Partialled-Out Estimates for 1991")


** Scatterplots **
twoway (scatter avgmath classize) (lfit avgmath classize)
twoway (scatter avgmath classize_hat) (lfit avgmath classize_hat)

log close