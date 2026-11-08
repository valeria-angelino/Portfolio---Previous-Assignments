ssc install palettes, replace
ssc install spmap, replace
ssc install geo2xy, replace
ssc install shp2dta, replace
ssc install schemepack, replace
ssc install scheme-burd, replace
ssc install colrspace, replace
ssc install egenmore, replace
ssc install outreg2, replace


cd "C:\Thesis Stata"

import excel "C:\for_Stata.xlsx", sheet("A1") firstrow clear
save "A1.dta", replace

import excel "C:\for_Stata.xlsx", sheet("A2") firstrow clear
save "A2.dta", replace

//////////////// Programs and Applications

use "A1.dta", clear

gen percent = (applied_female/applied_total)*100
tabstat percent, by(year) statistics(mean sum count)

tabstat percent if program_id == "Game Development" | program_id == "Computer Game Development", by(year) statistics(mean sum count)

tabstat applied_female if program_id == "Game Design and Programming" | program_id == "Programming in Game Development" | program_id == "Innovative Programming"| program_id == "Game Programming", by(year) statistics(mean sum min max count)

tabstat applied_male if program_id == "Game Design and Programming" | program_id == "Programming in Game Development" | program_id == "Innovative Programming"| program_id == "Game Programming", by(year) statistics(mean sum min max count)

tabstat applied_total if program_id == "Game Design and Programming" | program_id == "Programming in Game Development" | program_id == "Innovative Programming"| program_id == "Game Programming", by(year) statistics(mean sum min max count)
	 
egen avg_percent = mean(percent), by(year)
bysort year: keep if _n == 1

gen program_indicator = 1
collapse (sum) applied_female (count) num_programs=program_indicator, by(year)

/////////////Accepted and Employees
	
	
	use "A2.dta", clear
	
	 gen total_accepted = ( accepted_male + accepted_female)
	gen percent = (accepted_female/(accepted_male))*100
	
	tabstat percent, by(year) statistics(mean sum count)

	 bysort year: gen num_programs = _N
	 collapse (sum) accepted_female , by(year)
	 gen female_employees = .
replace female_employees = 135 if year == 2008
replace female_employees = 109 if year == 2009
replace female_employees = 121 if year == 2010
replace female_employees = 212 if year == 2011
replace female_employees = 293 if year == 2012
replace female_employees = 405 if year == 2013
replace female_employees = 563 if year == 2014
replace female_employees = 651 if year == 2015
replace female_employees = 800 if year == 2016
replace female_employees = 1041 if year == 2017
replace female_employees = 1699 if year == 2018
replace female_employees = 2079 if year == 2019

	list
	
	regress female_employees accepted_female year

	
	
