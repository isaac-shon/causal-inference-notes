* Author: Isaac Shon
* Date: 9/12/2024
* About: This do-file creates a simulated panel dataset for staggered DID.
*********************************************************************************

clear all
clear matrix
clear mata


macro drop _all
set seed 120302

* PATH
cd "C:\Users\isaac\OneDrive\Desktop\example_codes\DID"
global did_data_export "C:\Users\isaac\OneDrive\Desktop\example_codes\DID\data"

* We will observe 1000 units in a given cross-section:
set obs 1000
gen i = _n

* Unit FE: alpha_i ~ N(0.025, 0.5)
gen alpha_i = rnormal(0.025, 0.5)

* Randomly assign some of these units as never-treated or eventually-treated.
* Assign random numbers to the observations and rank them from the smallest to the largest
    gen random_number = uniform()   
    egen ordering = rank(random_number) 
    * Assign observations to control & treatment group based on their ranks 
    gen TreatedAtSomePoint = .  
    replace TreatedAtSomePoint = 1 if ordering <= _N/2
    replace TreatedAtSomePoint = 0 if ordering > _N/2
	drop random_number ordering
 
*We will create a dummy indicating if a unit will be treated at some point:
	* If never-treated, designate cohort as 0
	gen g = 0 if TreatedAtSomePoint == 0
	* If eventually-treated, randomly designate some treatment cohort
	replace g = runiformint(2003,2006) if TreatedAtSomePoint == 1


* Expand to 10 periods, create time indicator (2000-2009):
expand 10
bysort i: gen t = 1999 + _n

* Create variable indicating no. of periods unit was exposed to treatment
gen e = t - g if g > 0
replace e = 0 if e == .

* time FE: gamma_t ~ N(0.05,0.32)
* Generate a value for each unique value in t:
bysort t: gen gamma_t = runiform(0.05,0.32) if _n == 1
* Assign the same value to all rows with the same t:
bysort t: replace gamma_t = gamma_t[_n-1] if missing(gamma_t)

* Generate treatment status dummy based on cohort
gen D_it = (t >= g) if g !=0
replace D_it = 0 if g == 0

* Create error term:
gen e_it = rnormal(0,5)

* Generate outcome variable, introduce treatment effect dynamics:
gen eff = .
levelsof g, local(lvls) 
foreach x of local lvls{
	local b = runiformint(3,7)
		replace eff = `b' if g==`x'
}
gen y_it = .
replace y_it = alpha_i + gamma_t + cond(D_it==1, eff * e, 0) + e_it




* Declare our panel
xtset i t



save "$did_data_export\staggered_did_simulated.dta", replace
