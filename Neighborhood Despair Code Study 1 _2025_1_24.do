*====================================================================================;
*Code for Manuscript: Longer residence in disadvantaged neighborhoods predicts both a syndrome of despair and deaths of despair: Complementary evidence from nationwide register and birth-cohort studies
*AUTHOR: B Milne 
*IDI Refresh: IDI_Clean_202406

*PURPOSE: Study 1 Analysis Code

*UPDATES: 
*28 Aug 2024 - Analyses run on full cohort (with total income to be updated).
*7 Sep 2024 - Analyses rerun on full cohort (with total income updated).
*10 Nob 2024 - Analyses run with linear predictors.
*====================================================================================;


log using "I:\MAA2019-101\Deprivation\cohdod_dep_analyses_240828.smcl", replace


**************************
***FINALISING VARIABLES***
**************************
use "I:\MAA2019-101\Deprivation\cohdod_dep2.dta", clear

*sex
encode snz_sex_gender_code, gen(sex)
recode sex 3=.
label define mf 1 Male 2 Female
label value sex mf
tab sex

*cohort
tab snz_birth_year_nbr
recode snz_birth_year_nbr (1955/1974=1) (1975/1994=2), gen(cohort)
label define coh 1 "1955/1974" 2 "1975/1994"
label value cohort coh
tab cohort

*dod
label define pc 0 "None" 1 "DoD" 2 "Other"
label value dod_2 pc
tab dod_2

*education
tab max_nqflevel_sofar 
recode max_nqflevel_sofar (0=0) (1/3=1) (4/6=2) (7/10=3) (.=9), gen(hieduc)
label define hied 0 "None" 1 "School" 2 "Post-school" 3 "Degree" 9 "Missing"
label value hieduc hied
tab hieduc

*income
sum totinc
recode totinc (.=0)
sum totinc
egen totincq5 = cut(totinc), group(5)
bysort totincq5: sum totinc
replace totincq5=totincq5+1
label define q 1 "Q1-Lowest" 2 "Q2" 3 "Q3" 4 "Q4" 5 "Q5-Highest" 9 "Missing"
label value totincq5 q
tab totincq5

*bornNZ
tab bornNZ
label define imm 0 "born overseas" 1 "born in NZ"
label values bornNZ imm
tab bornNZ


*mean dep quintiles
sum meandep
egen meandepq5 = cut(meandep), group(5)
bysort meandepq5: sum meandep
replace meandepq5=meandepq5+1
recode meandepq5 (.=9)
label value meandepq5 q
tab meandepq5

*dep1cat
recode dep1cat (.=9)
label value dep1cat q
tab dep1cat

*lastdepcat
recode lastdepcat (.=9)
label value lastdepcat q
tab lastdepcat


*******************
******TABLE 1******DESCRIPTIVES
*******************
tab sex cohort
bysort sex cohort: tab1 dod_2 hieduc totincq5 bornNZ meandepq5 if sex~=.
bysort sex cohort: tab meandepq5 dod_2 if sex~=., row 
tab meandepq5 dod_2 if sex~=. & snz_ethnicity_grp1_nbr==1, row 
tab meandepq5 dod_2 if sex~=. & snz_ethnicity_grp2_nbr==1, row 
tab meandepq5 dod_2 if sex~=. & snz_ethnicity_grp3_nbr==1, row 
tab meandepq5 dod_2 if sex~=. & snz_ethnicity_grp4_nbr==1, row 
tab dep1cat dod_2, row
tab lastdepcat dod_2, row
bysort sex cohort: tab1 Alcohol Suicide Overdose
tab dod_2 if dod_2~=1 | Alcohol==1
tab dod_2 if dod_2~=1 | Suicide==1
tab dod_2 if dod_2~=1 | Overdose==1
 

*******************
******TABLE 2******ASSOCIATIONS
*******************


***overall - mean dep quintiles, controlling for sex, age, ethnicity, NZborn
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ, rrr vce(robust)
*strong, dose response

***overall - mean dep quintiles, controlling for sex, age, ethnicity, NZborn, education, income
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5, rrr vce(robust)
*weaker, but still there, mostly dose response

***sensitivity - 1st dep quintiles, controlling for sex, age, ethnicity, NZborn
mlogit dod_2 i.dep1cat sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ, rrr vce(robust)
*strong, dose response

***sensitivity - 1st dep quintiles, controlling for sex, age, ethnicity, NZborn, education, income
mlogit dod_2 i.dep1cat sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5, rrr vce(robust)
*weaker, but still there, mostly dose response

***sensitivity - last dep quintiles, controlling for sex, age, ethnicity, NZborn
mlogit dod_2 i.lastdepcat sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ, rrr vce(robust)
*strong, dose response

***sensitivity - last dep quintiles, controlling for sex, age, ethnicity, NZborn, education, income
mlogit dod_2 i.lastdepcat sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5, rrr vce(robust)
*weaker, but still there, mostly dose response

***sensitivity - ALCOHOL, mean dep quintiles, controlling for sex, age, ethnicity, NZborn
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if dod_2~=1 | Alcohol==1, rrr vce(robust)
*strong, dose response

***sensitivity - ALCOHOL, mean dep quintiles, controlling for sex, age, ethnicity, NZborn, education, income
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if dod_2~=1 | Alcohol==1, rrr vce(robust)
*weaker, but still there, mostly dose response

***sensitivity - SUICIDE, mean dep quintiles, controlling for sex, age, ethnicity, NZborn
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if dod_2~=1 | Suicide==1, rrr vce(robust)
*strong, dose response

***sensitivity - SUICIDE, mean dep quintiles, controlling for sex, age, ethnicity, NZborn, education, income
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if dod_2~=1 | Suicide==1, rrr vce(robust)
*weaker, but still there, mostly dose response

***sensitivity - OVERDOSE, mean dep quintiles, controlling for sex, age, ethnicity, NZborn
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if dod_2~=1 | Overdose==1, rrr vce(robust)
*strong, dose response

***sensitivity - OVERDOSE, mean dep quintiles, controlling for sex, age, ethnicity, NZborn, education, income
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if dod_2~=1 | Overdose==1, rrr vce(robust)
*weaker, but still there, mostly dose response


***by sex and cohort
*controlling for ethnicity, NZborn
bysort sex cohort: mlogit dod_2 i.meandepq5 snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if sex~=., rrr vce(robust)
*old males - strong, dose-response
*young males - weaker, does response, Q2 not significant
*old females - v strong, dose-response
*young females - weaker, does response, Q2 not significant

*controlling for ethnicity, NZborn, education, income
bysort sex cohort: mlogit dod_2 i.meandepq5 snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if sex~=., rrr vce(robust)
*old males - weaker, does response, Q2 not significant
*young males - weaker, does response, Q2, Q3 not significant
*old females - strong, not dose-response
*young females - weaker, not dose response, Q2, Q5 not significant



*European
tab snz_ethnicity_grp1_nbr dod_2
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr bornNZ if snz_ethnicity_grp1_nbr==1, rrr vce(robust)
*strong, dose-response
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr bornNZ i.hieduc i.totincq5 if snz_ethnicity_grp1_nbr==1, rrr vce(robust)
*weaker dose-response


*Maori
tab snz_ethnicity_grp2_nbr dod_2
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr bornNZ if snz_ethnicity_grp2_nbr==1, rrr vce(robust)
*dose-response, Q2 not significant
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr bornNZ i.hieduc i.totincq5 if snz_ethnicity_grp2_nbr==1, rrr vce(robust)
*q2 & q3 no=t significant


*Pacific
tab snz_ethnicity_grp3_nbr dod_2
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr bornNZ if snz_ethnicity_grp3_nbr==1, rrr vce(robust)
*no effect
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr bornNZ i.hieduc i.totincq5 if snz_ethnicity_grp3_nbr==1, rrr vce(robust)
*no effect

*Asian
tab snz_ethnicity_grp4_nbr dod_2
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr bornNZ if snz_ethnicity_grp4_nbr==1, rrr vce(robust)
*no effect
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr bornNZ i.hieduc i.totincq5 if snz_ethnicity_grp4_nbr==1, rrr vce(robust)
*no effect


*Immigrants
tab bornNZ dod_2
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr if bornNZ==0, rrr vce(robust)
*dose response, Q2 & Q3 not significant
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr i.hieduc i.totincq5 if bornNZ==0, rrr vce(robust)
*dose response, Q2 & Q3 not significant

*NZ born
mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr if bornNZ==1, rrr vce(robust)
*strong, dose-response

mlogit dod_2 i.meandepq5 sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr i.hieduc i.totincq5 if bornNZ==1, rrr vce(robust)
*strong, dose-response

log close



log using "I:\MAA2019-101\Deprivation\cohdod_dep_analyses_241110.smcl", replace
****ADDITIONAL ANALYSES - linear effect of deprivation****

**Linear effect of deprivation - meandep
tab meandepq5
recode meandepq5 (9=.), gen(meandepq5_nm)
tab meandepq5_nm

mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ, rrr vce(robust)
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5, rrr vce(robust)

**Linear effect of deprivation - 1st dep
tab dep1cat
recode dep1cat (9=.), gen(dep1cat_nm)
tab dep1cat_nm

mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5, rrr vce(robust)



**Linear effect of deprivation - meandep, ALCOHOL
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if dod_2~=1 | Alcohol==1, rrr vce(robust)
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if dod_2~=1 | Alcohol==1, rrr vce(robust)

**Linear effect of deprivation - 1st dep, ALCOHOL
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if dod_2~=1 | Alcohol==1, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if dod_2~=1 | Alcohol==1, rrr vce(robust)


**Linear effect of deprivation - meandep, SUICIDE
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if dod_2~=1 | Suicide==1, rrr vce(robust)
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if dod_2~=1 | Suicide==1, rrr vce(robust)

**Linear effect of deprivation - 1st dep, SUICIDE
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if dod_2~=1 | Suicide==1, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if dod_2~=1 | Suicide==1, rrr vce(robust)


**Linear effect of deprivation - meandep, OVERDOSE
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if dod_2~=1 | Overdose==1, rrr vce(robust)
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if dod_2~=1 | Overdose==1, rrr vce(robust)

**Linear effect of deprivation - 1st dep, OVERDOSE
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if dod_2~=1 | Overdose==1, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if dod_2~=1 | Overdose==1, rrr vce(robust)


***by sex and cohort, controlling for ethnicity, NZborn
bysort sex cohort: mlogit dod_2 meandepq5_nm snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if sex~=., rrr vce(robust) 
bysort sex cohort: mlogit dod_2 dep1cat_nm snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if sex~=., rrr vce(robust) 

***by sex and cohort, controlling for ethnicity, NZbornn, education, income
bysort sex cohort: mlogit dod_2 meandepq5_nm snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if sex~=., rrr vce(robust)
bysort sex cohort: mlogit dod_2 dep1cat_nm snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if sex~=., rrr vce(robust)


*European
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr bornNZ if snz_ethnicity_grp1_nbr==1, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr bornNZ if snz_ethnicity_grp1_nbr==1, rrr vce(robust)
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr bornNZ i.hieduc i.totincq5 if snz_ethnicity_grp1_nbr==1, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr bornNZ i.hieduc i.totincq5 if snz_ethnicity_grp1_nbr==1, rrr vce(robust)


*Maori
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr bornNZ if snz_ethnicity_grp2_nbr==1, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr bornNZ if snz_ethnicity_grp2_nbr==1, rrr vce(robust)
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr bornNZ i.hieduc i.totincq5 if snz_ethnicity_grp2_nbr==1, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr bornNZ i.hieduc i.totincq5 if snz_ethnicity_grp2_nbr==1, rrr vce(robust)


*Pacific
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr bornNZ if snz_ethnicity_grp3_nbr==1, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr bornNZ if snz_ethnicity_grp3_nbr==1, rrr vce(robust)
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr bornNZ i.hieduc i.totincq5 if snz_ethnicity_grp3_nbr==1, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr bornNZ i.hieduc i.totincq5 if snz_ethnicity_grp3_nbr==1, rrr vce(robust)


*Asian
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr bornNZ if snz_ethnicity_grp4_nbr==1, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr bornNZ if snz_ethnicity_grp4_nbr==1, rrr vce(robust)
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr bornNZ i.hieduc i.totincq5 if snz_ethnicity_grp4_nbr==1, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr bornNZ i.hieduc i.totincq5 if snz_ethnicity_grp4_nbr==1, rrr vce(robust)


*Immigrants
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if bornNZ==0, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if bornNZ==0, rrr vce(robust)
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if bornNZ==0, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if bornNZ==0, rrr vce(robust)


*NZ born
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if bornNZ==1, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ if bornNZ==1, rrr vce(robust)
mlogit dod_2 meandepq5_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if bornNZ==1, rrr vce(robust)
mlogit dod_2 dep1cat_nm sex snz_birth_year_nbr snz_ethnicity_grp1_nbr snz_ethnicity_grp2_nbr snz_ethnicity_grp3_nbr snz_ethnicity_grp4_nbr snz_ethnicity_grp5_nbr snz_ethnicity_grp6_nbr bornNZ i.hieduc i.totincq5 if bornNZ==1, rrr vce(robust)


log close
