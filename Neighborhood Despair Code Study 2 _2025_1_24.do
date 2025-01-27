
*====================================================================================;
*Code for Manuscript: Longer residence in disadvantaged neighborhoods predicts both a syndrome of despair and deaths of despair: Complementary evidence from nationwide register and birth-cohort studies

*AUTHOR: A. Reuben and G. Brennan 

*PURPOSE: Study 2 Analysis Code

*Last Updated: 10/15/2024
*====================================================================================;


use "/Users/asr28/Library/CloudStorage/Box-Box/Reuben, Aaron/2022_Spatial_NZ/Despair/Stat check March 2024/Aaron Despair Master Data_2024_4_6.dta"



**********
// Attrition tests
**********

sum ZDESPAIR NeighDep2645_factorSD
sum ZDESPAIR if NeighDep2645_factorSD!=.
tab Seen if NeighDep2645_factorSD!=.
tab sex if NeighDep2645_factorSD!=. & Seen==1


**********
// Primary study tests / Table 1
**********

zscore NeighDep2645_factorSD 

global despair ZDESPAIR ZSUIC ZSUBSTANC ZSLEEP ZPAIN

foreach var of global despair{
	regress `var' z_NeighDep2645_factorSD sex if SESall45!=., robust level(95)
	regress `var' z_NeighDep2645_factorSD sex SESall45, robust level(95)
	regress `var' z_NeighDep2645_factorSD sex URI2645avgc5, robust level(95)
}

**********
// Longitunidal trajectory tests
**********


// Conducted in MPLUS and SPSS. Files provided separately



**********
// Sensitivty tests of childhood factors (selection effects)
**********

global child prsev1 SESchildhd ChildhdIQ zChildPoorHlth SumDx1115 NewEduc45

foreach var of global child{
	regress ZDESPAIR z_NeighDep2645_factorSD sex SESall45 `var' , robust level(95)
}

regress ZDESPAIR z_NeighDep2645_factorSD sex SESall45 prsev1 SESchildhd ChildhdIQ zChildPoorHlth SumDx1115 NewEduc45, robust level(95)


**********
// Sensitivity tests of adult factors
**********

global adult PoorPhyHlth45 PYstress45 InfIsol45 SocSup45 MthUnem3845 govben45

foreach var of global adult{
	regress ZDESPAIR z_NeighDep2645_factorSD sex SESall45 `var' , robust level(95)
}

regress ZDESPAIR z_NeighDep2645_factorSD sex SESall45 PoorPhyHlth45 PYstress45 InfIsol45 SocSup45 MthUnem3845 govben45, robust level(95)


**********
// Figure 1
**********

// Generated in ArcGIS


**********
// Figure 2
**********

// Generated in Excel using Insert Chart > Bar Chart command


**********
// Figure 3
**********

sort NeighDep2645_factorq5
by NeighDep2645_factorq5: egen med = median(ZDESPAIR)
by NeighDep2645_factorq5: egen lqt = pctile(ZDESPAIR), p(25)
by NeighDep2645_factorq5: egen uqt = pctile(ZDESPAIR), p(75)
by NeighDep2645_factorq5: egen iqr = iqr(ZDESPAIR)
by NeighDep2645_factorq5: egen mean = mean(ZDESPAIR)
by NeighDep2645_factorq5: egen ls = min(max(ZDESPAIR, lqt-1.5*iqr))
by NeighDep2645_factorq5: egen us = max(min(ZDESPAIR, uqt+1.5*iqr))
gen outliers = ZDESPAIR if (ZDESPAIR<=lqt-1.5*iqr | ZDESPAIR>=lqt+1.5*iqr)
	
	twoway  ///
		scatter ZDESPAIR NeighDep2645_factorq5,  pstyle(p1) mcolor(ltblue%50)  msize(med) || ///
		rbar lqt med NeighDep2645_factorq5, pstyle(p1) barw(.75) msize(vsmall) fcolor(gs13%2) lcolor(ebblue%50)|| ///
		rbar  med uqt NeighDep2645_factorq5, pstyle(p1) barw(.75) msize(vsmall) fcolor(gs13%2) mcolor(%50) lcolor(ebblue%50)|| ///
		lfit ZDESPAIR NeighDep2645_factorq5, pstyle(p1) lcolor(blue) lwidth(medthick)  ///
		legend(off) ytitle(Despair Syndrome Score) xtitle(Neighborhood Disadvantage Quintiles)
	

**********
// Figure 4
**********

// Generated manually in Microsoft Powerpoint



************************************************************
********** Supplemental Figures and Tables *************
************************************************************

**********
// eTable 3 Associations of sex, age-45 social class, and urbanicity with the midlife despair syndrome in the Dunedin birth cohort.
**********

// Column 1 sex
zscore sex

global outcomes ZDESPAIR ZSUIC ZSUBSTANC ZSLEEP ZPAIN
foreach var of global outcomes{
reg `var' z_sex, robust level(95)
}

// Column 2 SESall45
zscore SESall45

global outcomes ZDESPAIR ZSUIC ZSUBSTANC ZSLEEP ZPAIN
foreach var of global outcomes{
reg `var' z_SESall45, robust level(95)
}

// Column 3 urbanicity
zscore URI2645avgc5

global outcomes ZDESPAIR ZSUIC ZSUBSTANC ZSLEEP ZPAIN
foreach var of global outcomes{
reg `var' z_URI2645avgc5, robust level(95)
}


**********
// eTable 4 Associations of pre-existing developmental risk factors and adult difficulties with neighborhood disadvantage and despair in the Dunedin birth cohort.
**********

// childhood factors

global child prsev1 SESchildhd ChildhdIQ zChildPoorHlth SumDx1115 NewEduc45

foreach var of global child{
	regress `var' NeighDep2645_factorSD sex SESall45  , robust beta
	regress `var' ZDESPAIR  sex SESall45 , robust beta
}

//  adult factors

global adult PoorPhyHlth45 PYstress45 InfIsol45 SocSup45 MthUnem3845 govben45

foreach var of global adult{
	regress `var' NeighDep2645_factorSD sex SESall45  , robust beta
	regress `var' ZDESPAIR  sex SESall45 , robust beta
}

**********
// eFigure 2. Cumulative neighborhood disadvantage across adulthood (ages 26 to 45 years) in the Dunedin birth cohort (N=907).  
**********

hist NeighDep2645_factor if Seen==1, percent


