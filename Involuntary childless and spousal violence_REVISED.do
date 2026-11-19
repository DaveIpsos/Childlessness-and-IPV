
cd "" // set directory
set maxvar 6000
set more off

***NDHS 2018
use "NGIR7AFL.DTA", clear

set more off

***Main Eplanatory variable
****Childlessness & contraception
fre v201
fre v302a
fre v228
gen childlessness=v201==0
ta childlessness

****Ever used anything to avoid pregnancy
gen everuse = 0
replace everuse=1 if v302a > 0
ta everuse


***Generate variable for childlessness
gen type_childlessness=.
replace type_childlessness=1 if (childlessness==1) & (v302a==0) & (v228==0)
replace type_childlessness=0 if (childlessness==0) 
ta type_childlessness

la def type_childlessness 1 "Involuntary" 0 "Non-childless", modify
la val type_childlessness type_childlessness
ta type_childlessness

****Target population: Married women, never had multiple unions, never used contraception, either involuntariliy childless or non-childless, and answered questions on violence under a private condition
********Selected for interview in violence module
fre v044
****Currently married woman
fre v501
***Number of unions

gen pop=0
replace pop=1 if (v044==1 & v501==1 & type_childlessness !=. & v503==1)
ta pop

***Outcome variables: Emotional violence (d103a-103c), Sexual violence (d105h, d105i & d105k), Physical violence (d105a-d105g, d105j, d105k)
foreach var in d103a d103b d103c d105a d105b d105c d105d d105e d105f d105h d105i d105j d105k {
	replace `var'=. if `var'==8
}
gen exp_violence = 0
replace exp_violence=1 if (d103a>0 | d103b>0 | d103c>0 | d105a>0 | d105b>0 | d105c>0 | d105d>0 | d105e>0 | d105f>0 | d105h>0 | d105i>0 | d105j>0  | d105k>0)
ta exp_violence

****Duration of first marriage
fre v512

///Control variables
fre d101a d101b d101c d101d d101e 
foreach var in d101a d101b d101c d101d d101e {
replace `var'=. if `var'==8 // don't know or No
}
egen marital_cont = rowtotal(d101a d101b d101c d101d d101e)
gen marital_control = marital_cont
replace marital_control=. if (marital_cont==0) & (d101a==. & d101b==. & d101c==. & d101d==. & d101e==.)
ta marital_control if pop==1

alpha d101a d101b d101c d101d d101e if (pop==1), item   

**Age
ta v012
recode v012 (15/24=1 "15-24years")(25/34=2 "25-34years")(35/39=3 "35-39years") ///
(40/44=4 "40-44 years")(45/49=5 "45-49 years"), gen(age)
ta age if pop==1

//Education
fre v106
recode v106 (0=1 "No Edu")(1=2 "Primary")(2/3=3 "At least secondary") ///
,gen(edu)
ta edu if pop==1

***Index of support for wife-beating
fre v744a v744b v744c v744d v744e
foreach var in v744a v744b v744c v744d v744e {
replace `var'=0 if `var'==8 // no or don't know
}
gen attitude = v744a + v744b + v744c + v744d + v744e
ta attitude if pop==1

alpha v744a v744b v744c v744d v744e if (pop==1), item 

****Partner consumes alcohol
fre d113

***Index of husband's dominance in household decisions
fre v743a v743b v743d
foreach var in v743a v743b v743d {
gen decision_`var'=0 if (`var' !=4 & `var' !=.)
replace decision_`var'=1 if `var' ==4 
}

fre decision_v743a decision_v743b decision_v743d
egen male_deci = rowtotal(decision_v743a decision_v743b decision_v743d)
ta male_deci
gen male_decision= male_deci
replace male_decision=. if (male_deci==0) & (decision_v743a==. & decision_v743b==. & decision_v743d==.)
ta male_decision

alpha decision_v743a decision_v743b decision_v743d if (pop==1), item   // 0.8504

****Husband's desire for more children
fre v621

****Type of marriage: Polygynous vers monogamous
fre v505
gen type_marr=v505
gen polygynous=v505 > 0 & (v505 !=98 & v505 !=.)
ta polygynous if pop==1

***Experience of inter-parental violence
fre d121 
recode d121 (0 8=0 "No")(1=1 "Yes"), gen(beat_mother)
ta beat_mother if pop==1

**Wealth quintiles
fre v190
 
** Type of Place of Residence
ta v025
 
**Ethnicity
fre v131 
gen tribe=.
replace tribe=1 if inrange(v131,2,3) | (v131==8)
replace tribe=2 if v131==6
replace tribe=3 if v131==10
replace tribe=4 if (v131==1) | (v131==4) | (v131==7) | (v131==9) | (v131==5) | (v131>=96)

la def tribe 1 "Fulani/Hausa/Kanuri or Beri Beri" ///
2 "Igbo" 3 "Yoruba" 4 "Other minority"
la val tribe tribe
ta tribe if pop==1,m
ta tribe if pop==1

**Region of residence
fre v024 


****Generating weight*****
gen strata=v023
gen psu=v021
gen wt=d005/1000000
svyset psu [pweight=wt], strata(strata) singleunit(centered)

***Generating non-missing data
egen nmiss = rowmiss(exp_violence type_childlessness v513 marital_control age edu attitude d113 male_decision v621 polygynous beat_mother v190 v025 tribe v024)
ta nmiss if pop==1

missings report exp_violence type_childlessness v513 marital_control age edu attitude d113 male_decision v621 polygynous beat_mother v190 v025 tribe v024 if (pop==1), percent 

****Target population: Complete cases
gen targetpop=0
replace targetpop=1 if (pop==1 & nmiss==0)
ta targetpop


****Model 1: Childlessness
qui svy,subpop(targetpop): logistic exp_violence i.type_childlessness c.v512 i.v621 marital_control i.age i.edu attitude i.d113 male_decision i.polygynous i.beat_mother i.v190 i.v025 i.tribe i.v024

****Plot 1 (childlessness)
qui margins, over (type_childlessness)
set scheme cleanplots
marginsplot, recast(bar) ytitle("Probability of spousal violence", size(small)) xtitle("Childlessness", size(small)) title("") ylabel(0(.25)0.5)

local colors yellow black 
forval j= 1/`=wordcount("`colors'")'{
    gr_edit .plotregion1.plot1.EditCustomStyle , j(`j') style(area(shadestyle(color(`=word("`colors'", `j')'%100))))
}

***Afterwards click to edit graph and click to record editing, 
***Change the first bar to yellow and the second to black
***Change the color of each confidence interval to gray
***Stop editing and save as "graph2"
***Save chart
***Rerun line 171 to 178 and run the command below
graph play "graph2"  // this code plays the editing procedure using the graph editor
***Save graph
graph save violence1, replace

****Plot 2 (Years of marriage)
qui margins, at(v512 =(0(5)40))
set scheme cleanplots
marginsplot, recast(line) ciopt(color(black%10)) recastci(rarea) ytitle("Probability of spousal violence", size(small)) xtitle("Years of marriage", size(small)) title("") plot1opts(lcolor(black)) ylabel(0(.25)0.5)
graph save violence2, replace


****Interaction model
qui svy,subpop(targetpop): logistic exp_violence i.type_childlessness##c.v512 i.v621 marital_control i.age i.edu attitude i.d113 male_decision i.polygynous i.beat_mother i.v190 i.v025 i.tribe i.v024

****Plot 3 
qui margins type_childlessness, at(v512 =(0(5)40))
set scheme cleanplots
marginsplot, recast(line) ciopt(color(%10)) recastci(rarea) ytitle("Probability of spousal violence", size(small)) xtitle("Years of marriage", size(small)) plot1opts(lcolor(red)) plot2opts(lcolor(blue)) title("") legend(position(11) cols(1) ring(0)) ylabel(0(.25)1) 
graph save violence3, replace
graph export "violence3.png", as(png) name("Graph") replace


****Plot 4
qui margins, dydx(type_childlessness) at(v512 =(0(5)40))
set scheme cleanplots
marginsplot, recast(line) ciopt(color(black%10)) recastci(rarea) ytitle("Effect of childlessness: Involuntary - Non-childless", size(small)) xtitle("Years of marriage", size(small)) title("") legend(position(11) cols(1) ring(0)) plot1opts(lcolor(black)) yline(0) ylabel(-0.25(.25)0.65)
graph save violence4, replace

***Combine plots
gr combine violence1.gph violence2.gph violence4.gph, rows(1) altshrink iscale(1.6) 
graph export "Combinedviolence.png", as(png) name("Graph") replace

