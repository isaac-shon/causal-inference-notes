************************************************************************
* Author: Isaac Shon
* About: DID STATA Exercise - 2x2 DID from Card & Kreuger (1994)
************************************************************************

clear
clear matrix
clear mata
cap log close

* SET PATH
macro drop _all
cd "C:\Users\isaac\OneDrive\Desktop\example_codes\DID"
global didexport "C:\Users\isaac\OneDrive\Desktop\example_codes\DID\writeup"

set seed 1203
************************************************************************
log using "DID.txt", text replace
************************************************************************

* Load in dataset and declare panel data:
use "data\CK1994.dta", replace
xtset store time

* As stated in original paper, FTE employment counts each part-time worker as half a full-time worker.
gen fte = empft + nmgrs + (0.5*emppt)

* Let us first visualize the distribution of wages in each survey wave, split by state:
twoway histogram wage_st if time==0, by(state) xtitle("Starting Wage (dollars per hour)")
	graph export "$didexport\Figure1a.pdf" , as(pdf) replace
twoway histogram wage_st if time==1, by(state) xtitle("Starting Wage (dollars per hour)")
	graph export "$didexport\Figure1b.pdf" , as(pdf) replace
	
	
* Let us now manually compute DID estimates. We can do this by obtaining conditional means for our groups.
* NJ
summarize fte if state == 1 & time == 1, meanonly
	local NJ_1 = `r(mean)'
summarize fte if state == 1 & time == 0, meanonly
	local NJ_0 = `r(mean)'
*PA
summarize fte if state == 0 & time == 1, meanonly
	local PA_1 = `r(mean)'
summarize fte if state == 0 & time == 0, meanonly
	local PA_0 = `r(mean)'

display "DID Estimate: " (`NJ_1' - `NJ_0') - (`PA_1' - `PA_0')

* We can summarize these means into a small table:
eststo clear
eststo mean1: estpost summarize fte if state == 1 & time == 1, meanonly
eststo mean2: estpost summarize fte if state == 1 & time == 0, meanonly
eststo mean3: estpost summarize fte if state == 0 & time == 1, meanonly
eststo mean4: estpost summarize fte if state == 0 & time == 0, meanonly

esttab mean1 mean2 mean3 mean4 using "$didexport\Table1.tex", replace main(mean) unstack nonote nonum eqlabels(`e(labels)') b(a5) mtitles("Post-NJ" "Pre-NJ" "Post-PA" "Pre-PA")

* Instead of manually computing the DID estimate, we can also run regressions:

*create interaction term
gen postxtreat = time * state
eststo clear
eststo m1: reg fte time state postxtreat
eststo m2: reg fte time state postxtreat, robust
eststo m3: reg fte time state postxtreat, vce(cluster store)

esttab m1 m2 m3 using "$didexport\Table2.tex", replace se star(* 0.1 ** 0.05 *** 0.01) b(%9.3f)


log close	