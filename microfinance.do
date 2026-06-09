*Creation of new HH variables 

local varlist bizrev bizexpense bizinvestment bizemployees hours_weekbiz 
foreach vars in `varlist'{
gen `vars'_all = `vars'
replace `vars'_all = 0 if `vars'_all == .
}

drop if area_dropped == 1

* Table1A

iebaltab hh_size adults children male_head head_age head_noeduc spandana othermfi bank informal anyloan spandana_amt othermfi_amt bank_amt informal_amt anyloan_amt total_biz female_biz female_biz_pct bizrev bizexpense bizinvestment bizemployees hours_weekbiz total_exp_mo nondurable_exp_mo durables_exp_mo home_durable_index bizrev_all bizexpense_all bizinvestment_all bizemployees_all hours_weekbiz_all, grpvar(treatment) vce(cluster areaid) rowvarlabels stats(pair(p)) savexlsx("Table2_A.xlsx")


*TABLE 3 
keep if sample1 == 1 & sample2 == 1

*Creating Index of Dependent Variables 
global varlist spandana_1 othermfi_1 anymfi_1 anybank_1 everlate_1 mfi_loan_cycles_1 anyloan_1

*Creating Area based controls for the regression 
global controls area_pop_base area_debt_total_base area_business_total_base area_exp_pc_mean_base area_literate_head_base area_literate_base

foreach var of varlist $varlist {
egen `var'_score = std(`var')
}

est clear 
foreach var in spandana_1 othermfi_1 anymfi_1 anybank_1	anyinformal_1  anyloan_1 everlate_1 mfi_loan_cycles_1 credit_index_1 {
			   reg `var' treatment $controls [pweight=w1], vce(cluster areaid)
			   estadd scalar pval = 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
			   sum `var' if treatment ==  0  & e(sample)
			   estadd scalar control_mn = r(mean)
			   est store `var' 
			   }
			   
			   esttab * using Table2.txt ,drop($controls) title("Table2") prehead("Panel A") cells(b(fmt(3) s) se(fmt(3) par)) replace s(N r2 sd2 mn2 pval) label postfoot("Robust Standard errors clustered at area id ") collabels(none)
			   
est clear 			   
			   
	foreach var in spandana_amt_1 othermfi_amt_1 anymfi_amt_1 bank_amt_1 informal_amt_1 anyloan_amt_1 	{
	         reg `var' treatment $controls [pweight=w1],vce(cluster areaid)
			 estadd scalar pval= 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
			 sum  `var' if treatment == 0 & e(sample)
			 estadd scalar control_mn  = r(mean)
			 est store `var'
			 }
			 
			 esttab * using Table2.txt,drop($controls) append prehead("") s(N r2 sd3 mn3 pval) label collabels(none)
est clear 			 
			 
	foreach var in  spandana_2 othermfi_2 anymfi_2 anybank_2 anyinformal_2  anyloan_2 everlate_2 mfi_loan_cycles_2 credit_index_2{
	          reg `var' treatment $controls [pweight=w2],vce(cluster areaid)
			  sum `var' if treatment == 0 & e(sample)
			  estadd scalar control_mn = r(mean)
			  est store `var'
			  }
			  
			  esttab * using Table2.txt , drop($controls) append prehead("Panel B") s(N r2 sd4 mn4 pval) cells(b(fmt(3) s) se(fmt(3) par)) label collabels(none) 
est clear 
     foreach var in spandana_amt_2 othermfi_amt_2 anymfi_amt_2 bank_amt_2 informal_amt_2 anyloan_amt_2{	
	         reg `var' treatment $controls [pweight=w2],vce(cluster areaid)
			 estadd scalar pval = 2*ttail(e(df_r), abs(_b[treatment]/_se[treatment]))
			 sum `var' if treatment ==0 & e(sample)
			 estadd scalar control_mn = r(mean)
			 est store `var'
			 }
	esttab * using Table2.txt, drop($controls) append prehead("loan amount") s(N r2 sd5 mn5 pval) cells(b(fmt(3) s) se(fmt(3) par)) nolabel collabels(none) nolegend 
	
	
	
	*Table3
	
	est clear
	
	foreach var in bizassets_1 bizinvestment_1  bizexpense_1   bizprofit_1    biz_index_all_1 any_biz_1 total_biz_1 any_new_biz_1 biz_stop_1 {
	reg `var' treatment $controls [pweight=w1],vce(cluster areaid)
	estadd scalar pval =2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
	sum `var' if treatment ==0 & e(sample)
	estadd scalar mn6 = r(mean)
	estadd scalar sd6= r(sd)
	est store `var'
	}
	esttab * using Table3.txt , drop($controls) replace title("Table 3") prehead("Panel A") cells(b(fmt(3) s) se(fmt(3) par)) s(N r2 sd6 mn6 pval) label  collabels(none)
	
	est clear 
	
	foreach var in bizassets_2 bizinvestment_2   bizexpense_2   bizprofit_2    biz_index_all_2 any_biz_2 total_biz_2 any_new_biz_2 biz_stop_2 {
	reg `var' treatment $controls [pweight=w2],vce(cluster areaid)
	estadd scalar pval =2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
	sum `var' if treatment ==0 & e(sample)
	estadd scalar mn7 = r(mean)
	estadd scalar sd7= r(sd)
	est store `var'
	}
	esttab * using Table3.txt , drop($controls) append title("Table 3") prehead("Panel B:Endline 2 ") cells(b(fmt(3) s) se(fmt(3) par)) s(N r2 sd6 mn6 pval) label  collabels(none)
	
	
	*Table3B Old Businesses
	
	keep if any_old_biz ==1
	
	est clear
	
	foreach var in bizassets_1 bizinvestment_1  bizrev_1 bizexpense_1   bizprofit_1  bizemployees_1 biz_index_old_1 {
	reg `var' treatment $controls [pweight=w1],vce(cluster areaid)
	estadd scalar pval =2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
	sum `var' if treatment ==0 & e(sample)
	estadd scalar mn6 = r(mean)
	estadd scalar sd6= r(sd)
	est store `var'
	}
	esttab * using Table3B.txt , replace title("Self-Employment Activities: Revenues, Assets and Profits") drop($controls) prehead("Panel A") cells(b(fmt(3) s) se(fmt(3) par)) s(N r2 sd6 mn6 pval) label  collabels(none) 
	
	est clear
	
	foreach var in bizassets_2 bizinvestment_2 bizrev_2 bizexpense_2   bizprofit_2  bizemployees_2 biz_index_old_2 {
	reg `var' treatment $controls [pweight=w2],vce(cluster areaid)
	estadd scalar pval =2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
	sum `var' if treatment ==0 & e(sample)
	estadd scalar mn6 = r(mean)
	estadd scalar sd6= r(sd)
	est store `var'
	}
	esttab * using Table3B.txt , drop($controls) append prehead("Panel B:Endline 2") cells(b(fmt(3) s) se(fmt(3) par)) s(N r2 sd6 mn6 pval) label  collabels(none)
	
*Table 3C New Businesses
keep if any_new_biz_1 ==1
	
	est clear
	
	foreach var in bizassets_1 bizinvestment_1  bizrev_1 bizexpense_1   bizprofit_1  bizemployees_1 biz_index_new_1  {
	reg `var' treatment $controls [pweight=w1],vce(cluster areaid)
	estadd scalar pval =2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
	sum `var' if treatment ==0 & e(sample)
	estadd scalar mn6 = r(mean)
	estadd scalar sd6= r(sd)
	est store `var'
	}
	esttab * using Table3C.txt , replace title("Self-Employment Activities: Revenues, Assets and Profits") drop($controls) prehead("Panel A") cells(b(fmt(3) s) se(fmt(3) par)) s(N r2 sd6 mn6 pval) label  collabels(none)

*Table 4

est clear 

foreach var in   wages_nonbiz_1 income_index_1 bizprofit_1 {
 reg `var' treatment $controls [pweight=w1], vce(cluster areaid)
 estadd scalar pval =2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
 sum `var' if treatment ==0 & e(sample)
 estadd scalar mn7 = r(mean)
estadd scalar sd7 = r(sd)
est store `var'

}
esttab * using Table4.txt , replace title("Table 4—Income") drop($controls) prehead("Panel A:Endline 1") cells(b(fmt(3) s) se(fmt(3) par)) s(N r2 sd7 mn7 pval) label  collabels(none)

est clear
foreach var in   wages_nonbiz_2 income_index_2 bizprofit_2 {
 reg `var' treatment $controls [pweight=w2], vce(cluster areaid)
 estadd scalar pval =2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
 sum `var' if treatment ==0 & e(sample)
 estadd scalar mn7 = r(mean)
estadd scalar sd7 = r(sd)
est store `var'
}
esttab * using Table4.txt , append drop($controls) prehead("Panel B: Endline 2") cells(b(fmt(3) s) se(fmt(3) par)) s(N r2 sd7 mn7 pval) label  collabels(none) postfoot("Notes: The table presents the coefficient of a treatment dummy in a regression of each variable
on treatment (with control variables listed in the text). Cluster-robust standard errors in parentheses.
Results are weighted to account for oversampling of Spandana borrowers. Self-employment
income equals profit of a self-employment activity (summed across activities if multiple in the
household). Equal to zero for households with no self-employment activity. Daily labor/salaried
income is income from employment other than self employment, summed across working
household members.")

*Table 6
est clear 
foreach var in home_durable_index_1 temptation_exp_mo_pc_1 festival_exp_mo_pc_1 health_exp_mo_pc_1 educ_exp_mo_pc_1 durables_exp_mo_pc_1 nondurable_exp_mo_pc_1 food_exp_mo_pc_1 total_exp_mo_1{
reg `var' treatment $controls [pweight=w1],vce(cluster areaid)
estadd scalar pval=2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
sum `var' if treatment ==0 & e(sample)
estadd scalar mn4= r(mean)
estadd scalar sd4 =r(sd)
est store `var' 
}

esttab * using TAble6.txt,replace drop($controls)  title("Table 6—Consumption ") prehead("Panel A:Endline 1") cells(b(fmt(3) s) se(fmt(3) par)) s(N r2 pval sd4 mn4) label collabels(none)

est clear 
foreach var in home_durable_index_2 temptation_exp_mo_pc_2 festival_exp_mo_pc_2 health_exp_mo_pc_2 educ_exp_mo_pc_2 durables_exp_mo_pc_2 nondurable_exp_mo_pc_2 food_exp_mo_pc_2 total_exp_mo_2{

reg `var' treatment $controls [pweight=w2],vce(cluster areaid)
estadd scalar pval= 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
sum `var' if treatment ==0 & e(sample)
estadd scalar mn4= r(mean)
estadd scalar sd4 =r(sd)
est store `var' 
}

esttab * using TAble6.txt,append  drop($controls)  prehead("Panel B:Endline 2") cells(b(fmt(3) s) se(fmt(3) par)) s(N r2 pval sd4 mn4) label collabels(none)

*Table 7 
est clear 
foreach var in girl515_school_1 boy515_school_1  girl515_workhrs_pc_1 boy515_workhrs_pc_1 girl1620_school_1 boy1620_school_1 women_emp_index_1 female_biz_new_1 social_index_1{

reg `var' treatment $controls [pweight=w1],vce(cluster areaid) 
estadd scalar pval= 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
sum `var' if treatment ==0 & e(sample)
estadd scalar mn4= r(mean)
estadd scalar sd4 = r(sd)
est store `var' 
}

esttab * using Table7.tex, drop($controls) replace title("Table 7—Social Effects") mgroups("Share of children aged 5–15 in school" "Hours workedper child aged 5–15over the past 7 days"  "Share of teenagers in school", pattern(1 0 1 0 1 0 )) mtitles("Girls" "Boys" "Girls" "Boys" "Girls" "Boys" "Index of women independence"  "Number new self-employment activities managed by women"  "Index of dependent variables")  prehead("Panel A:Endline 1") cells(b(fmt(3) s) se(fmt(3) par)) s(N r2 pval sd4 mn4) label collabels(none)

est clear
foreach var in girl515_school_2 boy515_school_2  girl515_workhrs_pc_2 boy515_workhrs_pc_2 girl1620_school_2 boy1620_school_2 women_emp_index_2 female_biz_new_2 social_index_2{
reg `var' treatment $controls [pweight=w2],vce(cluster areaid) 
estadd scalar pval= 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
sum `var' if treatment ==0 & e(sample)
estadd scalar mn4= r(mean)
estadd scalar sd4 = r(sd)
est store `var'
}
esttab * using Table7.txt, append drop($controls)   prehead("Panel B:Endline 2") cells(b(fmt(3) s) se(fmt(3) par)) s(N r2 pval sd4 mn4) label collabels(none)

*Table 5 
est clear 
foreach var in hours_week_1 hours_week_biz_1 hours_week_outside_1 hours_boy1620_week_1 hours_girl1620_week_1 hours_headspouse_week_1 hours_headspouse_biz_1 hours_headspouse_outside_1  labor_index_1{
reg `var' treatment $controls [pweight=w1],vce(cluster areaid)
estadd scalar pval= 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
sum `var' if treatment ==0 & e(sample)
estadd scalar mn4= r(mean)
estadd scalar sd4 = r(sd)
est store `var'
}
esttab * using Table5.txt,replace drop($controls) title("Table 5—Time Worked by Household Members") mgroups("All Adults and Teens" "Teens" "Household head and spouse",pattern(1 0 0 1 0 0 1 0 0)) mtitle("Total" "Self-Employment" "Outside Activities" "Girls" "Boys" "Total" "Self Employment" "Outside Activities" "Index of Dependent Variables") prehead("Panel A:Endline 1") cells(b(fmt(3) s) se(fmt(3) par)) s(N r2 pval sd4 mn4) label collabels(none)

est clear 
foreach var in hours_week_2 hours_week_biz_2 hours_week_outside_2 hours_boy1620_week_2 hours_girl1620_week_2 hours_headspouse_week_2 hours_headspouse_biz_2 hours_headspouse_outside_2  labor_index_2{
reg `var' treatment $controls [pweight=w2],vce(cluster areaid)
estadd scalar pval= 2*ttail(e(df_r),abs(_b[treatment]/_se[treatment]))
sum `var' if treatment ==0 & e(sample)
estadd scalar mn4= r(mean)
estadd scalar sd4 = r(sd)
est store `var'
}
esttab * using Table5.txt,append drop($controls) prehead("Panel B:Endline 2") cells(b(fmt(3) s) se(fmt(3) par)) s(N r2 pval sd4 mn4) label collabels(none)

*Table 1B



ds *_1
di "`r(varlist)'"
local varlist `r(varlist)'
di "`varlist'"
local stubs : subinstr local varlist "_1" "", all
di "`stubs'"

 local businesses "biz_rev biz_expense biz_investment biz_employees hours_week_biz"

reshape long * ,i(hhid) j(control group)

* 1. Define macro & loop in ONE block
local businesses "bizrev bizexpense bizinvestment bizemployees hours_week_biz"

* 2. Verify macro loaded
di ">>> Macro contains: `businesses'"

* 3. Create variables with built-in feedback
forval i = 1/2 {
    foreach var in `businesses' {
        capture confirm variable `var'_`i'
        if _rc != 0 {
            di "⚠️ SKIPPED: `var'_`i' does not exist in your data"
            continue
        }
        
        capture drop `var'_allHH_`i'
        gen `var'_allHH_`i' = `var'_`i' if total_biz_`i' > 0
        di "✅ Created: `var'_allHH_`i'"
    }
}

* 4. Final verification
describe *_allHH_*

keep if sample1==1 & sample2 ==1


reshape long  hhsize_ adults_ children_ male_head_ head_age_ head_noeduc_ spandana_ othermfi_ anymfi_ anybank_ anyinformal_ anyloan_ spandana_amt_ othermfi_amt_ anymfi_amt_ bank_amt_ informal_amt_ anyloan_amt_ bizassets_ bizinvestment_ bizrev_ bizexpense_ bizprofit_ bizemployees_ hours_week_biz_ total_biz_ female_biz_ female_biz_pct_ total_exp_mo_ nondurable_exp_mo_ durables_exp_mo_ home_durable_index_ bizrev_allHH_ bizexpense_allHH_ bizinvestment_allHH_ bizemployees_allHH_ hours_week_biz_allHH_ ,i(hhid) j(Control_Group)

iebaltab hhsize_ adults_ children_ male_head_ head_age_ head_noeduc_ spandana_ othermfi_ anymfi_ anybank_ anyinformal_ anyloan_ spandana_amt_ othermfi_amt_ anymfi_amt_ bank_amt_ informal_amt_ anyloan_amt_ bizassets_ bizinvestment_ bizrev_ bizexpense_ bizprofit_ bizemployees_ hours_week_biz_ total_biz_ female_biz_ female_biz_pct_ total_exp_mo_ nondurable_exp_mo_ durables_exp_mo_ home_durable_index_ bizrev_allHH_ bizexpense_allHH_ bizinvestment_allHH_ bizemployees_allHH_ hours_week_biz_allHH_ ,grpvar(Control_Group) vce(cluster areaid )  stats(pair(p beta)) savexlsx("Table1bbb.xlsx")
 


	
			 
	    
	 
			 