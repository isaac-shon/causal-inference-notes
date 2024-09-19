************************************************************************
* Author: Isaac Shon
* About: RCT Simulation Exercise
************************************************************************
clear
clear matrix
clear mata
cap log close

cd "C:\Users\isaac\OneDrive\Desktop\example_codes\RCT"
global rctexport "C:\Users\isaac\OneDrive\Desktop\example_codes\RCT\writeup"
************************************************************************
************************************************************************
log using "Assignment2.txt", text replace

************ Part I. Data Generation***************
set seed 1203
set obs 1000

gen X_1 = rnormal(7,1)
gen X_2 = rnormal(2,0.5)
gen X_3 = rnormal(3,0.5)
gen epsilon_i = rnormal(0,1)

* Random Treatment Assignment
gen T_i = uniform()

* Set population parameters: b1 = 1, b2 = 2, b3 = 2.5. 
gen Y_i = 1*T_i + 2*X_1 + 2.5*X_2 + 3*X_3 + epsilon_i


* Summary statistics for n = 1000
eststo clear
estpost summarize Y_i T_i X_1 X_2 X_3 epsilon_i
esttab using "C:\Users\isaac\OneDrive\Desktop\example_codes\RCT\writeup\1k_DGP_summary.tex", replace cells("mean sd min max") nonumber title("Summary Table of Select Variables")

eststo clear
qui{
eststo: reg Y_i T_i, vce(robust)
eststo: reg Y_i T_i X_1, vce(robust)
eststo: reg Y_i T_i X_1 X_2, vce(robust)
eststo: reg Y_i T_i X_1 X_2 X_3, vce(robust)
}

esttab _all using "C:\Users\isaac\OneDrive\Desktop\example_codes\RCT\writeup\1k_DGP_table.tex", style(tex) replace se star(* 0.1 ** 0.05 *** 0.01) b(%9.3f) nogaps nomtitles mlabels("OLS" "OLS \& 1 Covariate" "OLS \& 2 Covariates" "OLS \& 3 Covariates") title("Regression Results Using Simulated Data")



log close