************************************************************************
* Author: Isaac Shon
* About: RDD Simulation Exercise
************************************************************************

/* Package install for RDD: rdrobust

ssc install rdrobust, replace

*/

clear
clear matrix
clear mata
cap log close

* SET PATH
macro drop _all
cd "C:\Users\isaac\OneDrive\Desktop\example_codes\RDD"
global rddexport "C:\Users\isaac\OneDrive\Desktop\example_codes\RDD\writeup"

************************************************************************
************************************************************************
log using "Assignment4.txt", text replace

***********************Part I. Sharp RD Data Generation**************************
set seed 1203
set obs 1000

* Specify population parameters:
global alpha = 25
global tau = 40
global beta = 1.5

* Create Running Variable
gen X = rnormal(50, 25)
replace X=0 if X < 0
drop if X > 100
	*sum X, det

* Create error term
gen u = rnormal(0, 20)

* Create treatment variable with rule: D = 1 if X > c (let c = 50).
gen c = 50
gen D = (X > c)


* Generate outcome variable for each unit:
gen Y = $alpha + ($tau *D) + ($beta *X) + u

* Summary statistics for simulated data
eststo clear
estpost summarize Y D X u
esttab using "$rddexport\1k_sharpDGP_summary.tex", replace cells("mean sd min max") b(%9.3f) nonumber title("Summary Table of Select Variables")

************ Data Visualization & Falsification Test ************
* Create a scatterplot of outcome variable and running variable:

twoway (scatter Y X [fweight=N] if D==0, msize(0.25) mcolor("red") xline(50)) ///
(scatter Y X [fweight=N] if D==1, msize(0.25) mcolor("green")), legend(label(1 "Untreated") label(2 "Treated"))
graph export "$rddexport\sharpRDDscatter.pdf" , as(pdf) replace

* Create a histogram to see if there is bunching around cutoff: 
* Is the number of observations surprisingly different from either end of cutoff?
twoway histogram X, xline(50)
graph export "$rddexport\sharpRDDdensity.pdf", as(pdf) replace
*****************************************************************

* Running RD local polynomial regressions using rdplot:
rdrobust Y X, c(50) all	 
outreg2 using "$rddexport\table2.tex", replace ctitle(Model 1) ///
    tex(fragment) noobs nonote ///
    addtext(Covariates, NO) ///
    addstat(Mean dep. var., r(mean), Observations, e(N), Bandwidth, e(h_l), Order polyn., e(p))
rdrobust Y X, c(50) h(10 10) all	 
outreg2 using "$rddexport\table2.tex", append ctitle(Model 2) ///
    tex(fragment) noobs nonote ///
    addtext(Covariates, NO) ///
    addstat(Mean dep. var., r(mean), Observations, e(N), Bandwidth, e(h_l), Order polyn., e(p))
rdrobust Y X, c(50) p(2) all	
outreg2 using "$rddexport\table2.tex", append ctitle(Model 3) ///
    tex(fragment) noobs nonote ///
    addtext(Covariates, NO) ///
    addstat(Mean dep. var., r(mean), Observations, e(N), Bandwidth, e(h_l), Order polyn., e(p))
rdrobust Y X, c(50) p(2) h(10 10) all	
outreg2 using "$rddexport\table2.tex", append ctitle(Model 4) ///
    tex(fragment) noobs nonote ///
    addtext(Covariates, NO) ///
    addstat(Mean dep. var., r(mean), Observations, e(N), Bandwidth, e(h_l), Order polyn., e(p))
rdrobust Y X, c(50) p(3) all	
outreg2 using "$rddexport\table2.tex", append ctitle(Model 5) ///
    tex(fragment) noobs nonote ///
    addtext(Covariates, NO) ///
    addstat(Mean dep. var., r(mean), Observations, e(N), Bandwidth, e(h_l), Order polyn., e(p))
rdrobust Y X, c(50) p(3) h(10 10) all	
outreg2 using "$rddexport\table2.tex", append ctitle(Model 6) ///
    tex(fragment) noobs nonote ///
    addtext(Covariates, NO) ///
    addstat(Mean dep. var., r(mean), Observations, e(N), Bandwidth, e(h_l), Order polyn., e(p))
rdrobust Y X, c(50) p(4) all	
outreg2 using "$rddexport\table2.tex", append ctitle(Model 7) ///
    tex(fragment) noobs nonote ///
    addtext(Covariates, NO) ///
    addstat(Mean dep. var., r(mean), Observations, e(N), Bandwidth, e(h_l), Order polyn., e(p))
rdrobust Y X, c(50) p(4) h(10 10) all	
outreg2 using "$rddexport\table2.tex", append ctitle(Model 8) ///
    tex(fragment) noobs nonote ///
    addtext(Covariates, NO) ///
    addstat(Mean dep. var., r(mean), Observations, e(N), Bandwidth, e(h_l), Order polyn., e(p)) ///
			
	
* Generate Some Plots of Different Fits (with Confidence Intervals)
rdplot Y X, p(1) c(50) ci(95) graph_options(title(Sharp RD Plot) ytitle(Y) xtitle(X))
graph export "$rddexport\sharpRDDplot_order1.pdf", as(pdf) replace

rdplot Y X, p(2) c(50) ci(95) graph_options(title(Sharp RD Plot) ytitle(Y) xtitle(X))
graph export "$rddexport\sharpRDDplot_order2.pdf", as(pdf) replace

rdplot Y X, p(3) c(50) ci(95) graph_options(title(Sharp RD Plot) ytitle(Y) xtitle(X))
graph export "$rddexport\sharpRDDplot_order3.pdf", as(pdf) replace

rdplot Y X, p(4) c(50) ci(95) graph_options(title(Sharp RD Plot) ytitle(Y) xtitle(X))
graph export "$rddexport\sharpRDDplot_order4.pdf", as(pdf) replace



log close
