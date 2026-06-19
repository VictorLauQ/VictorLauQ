/*==============================================================================
   Project:     Effect of parental death on education
   Do-file:     Sample construction and variable creation

   Description:
   This do-file builds the analysis sample by round, appends all rounds into
   a panel, corrects inconsistencies in the orphanhood variable and constructs
   the main outcome and treatment variables
   
	* Note 1: Comments in original dofile were made in spanish. This is the translated 
	version
 
   * Note 2: This do-file is a coding sample illustrating workflow. Some
   limitations reflect survey design issues, and a few are coding imperfections I
   identified after the analysis was completed
==============================================================================*/


/*
	Contents:
	
	I. Sample by round 
	II. Main panel
	III. Chanel datasets
*/


// Set working directory
cd "C:\Users\VICTOR\Desktop\IE\Bases_ie"


*===================================================
* PART I: BUILD THE CLEANED SAMPLE FOR EACH ROUND  
*===================================================

//----- Round 1 -----//
{
    * ------------------------------------------------------------------
    * Step 1: Load raw data
    * ------------------------------------------------------------------
    use "R1\pechildlevel1yrold.dta", clear

    * ------------------------------------------------------------------
    * Step 2: Identify children whose parents had already died at baseline and drop them (missing values are not dropped yet, only confirmed deaths)
    * ------------------------------------------------------------------
    ** mother already dead
    codebook momlive
    drop if momlive == 3

    ** father already dead
    drop if daddead == 3

    * ------------------------------------------------------------------
    * Step 3: Keep only the identifier (this round is only used as a filter) and save
    * ------------------------------------------------------------------
    keep childid
    save muestra_r1.dta, replace
}



//----- Round 2 -----//
{
    * ------------------------------------------------------------------
    * Step 1: Load raw data and restrict to children who passed the round-1 filter (parents alive)
    * ------------------------------------------------------------------
    use "R2\pechildlevel5yrold.dta", clear

    merge 1:1 childid using muestra_r1.dta
    drop if _merge != 3
    drop _merge

    * Drop children who dropped out of the study (attrition)
    drop if situac_r2 != 1

    * Drop those with a dead parent or a stepparent (baseline sample requires both biological parents alive)
    drop if dadal == 0 | mumal == 0

    * Drop those with both parents alive but at least one is a stepparent
    drop if (dadal == 1 & mumal == 1) & (biodad == 0 | biomum == 0)

    * Recover a few observations (dadal missing, but biodad reported)
    replace dadal = 1 if dadal == . & biodad == 1

    * ------------------------------------------------------------------
    * Step 2: Build the X and Y variables
    * ------------------------------------------------------------------
    ** X variables
    gen ronda = 2
    gen orphanhood = 0
    gen orphan_dad = 0
    gen orphan_mom = 0

    ** Y variables
    egen gasto_educ = rowtotal(spyr11a spyr12a spyr13a spyr15 spyr16) // education expenditure
    //keep spnam11a spnam12a spnam13a spname15 spname16 // shares (not used)
    merge 1:1 childid using "R2\horas_actividades_r2.dta" // time-use variables
    drop if _merge == 2
    drop _merge

    * ------------------------------------------------------------------
    * Step 3: Keep only the variables needed for the analysis and save
    * ------------------------------------------------------------------
    keep childid ronda orphanhood orphan_dad orphan_mom gasto_educ horas_sleep horas_care  horas_chores horas_paywork horas_npaywork horas_study horas_school estudia
    save muestra_r2.dta, replace
}



//----- Round 3 -----//
{
    * ------------------------------------------------------------------
    * Step 1: Load raw data, drop children with dead parents (rounds 1-2) and round-2 attrition, and bring in household data
    * ------------------------------------------------------------------
    use "R3\pe_yc_childlevel.dta", clear

    merge 1:1 childid using muestra_r2.dta
    drop if _merge != 3 // using-only --> round 2-3 attrition; master-only --> deaths from r1/r2
    drop _merge

    * Drop round-2 Y/X variables (will be regenerated for round 3)
    drop ronda orphanhood orphan_dad orphan_mom gasto_educ horas_sleep horas_care  horas_chores horas_paywork horas_npaywork horas_study horas_school estudia

    * Merge with the household-level file (unlike r1/r2, parent info is separate)
    merge 1:1 childid using "R3\pe_yc_householdlevel.dta"
    drop if _merge != 3
    drop _merge

    * ------------------------------------------------------------------
    * Step 2: Build the X and Y variables
    * ------------------------------------------------------------------
    ** X variables
    gen ronda = 3
    gen orphanhood = 0
    replace orphanhood = 1 if dadalr3 == 0 | mumalr3 == 0
    gen orphan_dad = 0
    replace orphan_dad = 1 if dadalr3 == 0
    gen orphan_mom = 0
    replace orphan_mom = 1 if mumalr3 == 0

    ** Y variables
    egen gasto_educ = rowtotal(spyrr311 spyrr312 spyrr313 spyrr315 spyrr316)
    gen estudia = .
    replace estudia = 1 if enrschr3 == 1
    replace estudia = 0 if enrschr3 == 0
    drop enrschr3
    merge 1:1 childid using "R3\horas_actividades_r3.dta"
    drop if _merge == 2
    drop _merge

    * ------------------------------------------------------------------
    * Step 3: Keep only the variables needed for the analysis and save
    * ------------------------------------------------------------------
    keep childid ronda orphanhood orphan_dad orphan_mom gasto_educ horas_sleep horas_care  horas_chores horas_paywork horas_npaywork horas_study horas_school estudia
    save muestra_r3.dta, replace
}



//------ Round 4 -----//
{
    * ------------------------------------------------------------------
    * Step 1: Load raw data and standardize the childid identifier
    * ------------------------------------------------------------------
    use "R4\pe_r4_ycch_youngerchild.dta", clear

    tostring CHILDCODE, gen (childid_aux)
    replace childid_aux = "0" + childid_aux if strlen(childid_aux) == 5
    gen childid = "PE" + childid_aux
    drop childid_aux CHILDCODE

    * Drop deaths from r1/r2 and round-3 attrition (round-3 dropouts are kept here because they still have valid data for r2/r3, even though they will have no data for r4)
    merge 1:1 childid using muestra_r3.dta
    drop if _merge != 3
    drop _merge

    * Drop round-3 Y/X variables (will be regenerated for round 4)
    drop ronda orphanhood orphan_dad orphan_mom gasto_educ horas_sleep horas_care  horas_chores horas_paywork horas_npaywork horas_study horas_school estudia

    * Merge with the household-level file (parent info is separate)
    merge 1:1 childid using "R4\R4_ychousehold.dta"
    drop if _merge != 3
    drop _merge

    * ------------------------------------------------------------------
    * Step 2: Build the X and Y variables
    * ------------------------------------------------------------------
    ** X variables
    gen ronda = 4
    gen orphanhood = 0
    replace orphanhood = 1 if DADALR4 == 0 | MUMALR4 == 0
    gen orphan_dad = 0
    replace orphan_dad = 1 if DADALR4 == 0
    gen orphan_mom = 0
    replace orphan_mom = 1 if MUMALR4 == 0

    ** Y variables
    merge 1:1 childid using "R4\R4_ycnonfoodconsumption.dta" // gasto_educ comes from here
    keep if _merge == 3
    drop _merge

    rename SLEEPR4  horas_sleep
    rename CROTHR4  horas_care
    rename DMTSKR4  horas_chores
    rename TSFARMR4 horas_npaywork
    rename ACTPAYR4 horas_paywork
    rename ATSCHR4  horas_school
    rename STUDYGR4 horas_study

    gen estudia = .
    replace estudia = 1 if ENRSCHR4 == 1
    replace estudia = 0 if ENRSCHR4 == 0
    drop ENRSCHR4

    * ------------------------------------------------------------------
    * Step 3: Keep only the variables needed for the analysis and save
    * ------------------------------------------------------------------
    keep childid ronda orphanhood orphan_dad orphan_mom gasto_educ horas_sleep horas_care  horas_chores horas_paywork horas_npaywork horas_study horas_school estudia
    save muestra_r4.dta, replace
}



//------ Round 5 -----//
{
    * ------------------------------------------------------------------
    * Step 1: Load raw data and standardize the childid identifier
    * ------------------------------------------------------------------
    use "R5\pe_r5_ycch_youngerchild.dta", clear

    tostring CHILDCODE, gen (childid_aux)
    replace childid_aux = "0" + childid_aux if strlen(childid_aux) == 5
    gen childid = "PE" + childid_aux
    drop childid_aux CHILDCODE

    * Drop deaths from r1/r2 and round-4 attrition (round-4 dropouts are kept here because they still have valid data for r2/r3/r4)
    merge 1:1 childid using muestra_r4.dta
    drop if _merge != 3
    drop _merge

    * Drop round-4 Y/X variables (will be regenerated for round 5)
    drop ronda orphanhood orphan_dad orphan_mom gasto_educ horas_sleep horas_care  horas_chores horas_paywork horas_npaywork horas_study horas_school estudia

    * Merge with the household-level file (parent info is separate)
    merge 1:1 childid using "R5\R5_ychousehold.dta"
    drop if _merge != 3
    drop _merge

    * ------------------------------------------------------------------
    * Step 2: Build the X and Y variables
    * ------------------------------------------------------------------
    ** X variables
    gen ronda = 5
    gen orphanhood = 0
    replace orphanhood = 1 if DADALR5 == 0 | MUMALR5 == 0
    gen orphan_dad = 0
    replace orphan_dad = 1 if DADALR5 == 0
    gen orphan_mom = 0
    replace orphan_mom = 1 if MUMALR5 == 0

    ** Y variables
    merge 1:1 childid using "R5\R5_ycnonfoodconsumption.dta" // gasto_educ comes from here
    keep if _merge == 3
    drop _merge

    rename SLEEPR5  horas_sleep
    rename CROTHR5  horas_care
    rename DMTSKR5  horas_chores
    rename TSFARMR5 horas_npaywork
    rename ACTPAYR5 horas_paywork
    rename ATSCHR5  horas_school
    rename STUDYGR5 horas_study

    gen estudia = .
    replace estudia = 1 if ENRSCHR5 == 1
    replace estudia = 0 if ENRSCHR5 == 0
    drop ENRSCHR5

    * ------------------------------------------------------------------
    * Step 3: Keep only the variables needed for the analysis and save
    * ------------------------------------------------------------------
    keep childid ronda orphanhood orphan_dad orphan_mom gasto_educ horas_sleep horas_care  horas_chores horas_paywork horas_npaywork horas_study horas_school estudia
    save muestra_r5.dta, replace
}



*====================================================================
*   PART II: BUILD THE MAIN ANALYTIC PANEL (TIME-VARYING CONTROLS)
*====================================================================

// Set working directory
cd "C:\Users\VICTOR\Desktop\IE\Bases_ie"

* ----------------------------------------------------------------------
* Step 1: Append all rounds (2 to 5) into a single long panel
* ----------------------------------------------------------------------
use muestra_r2.dta, clear
append using muestra_r3.dta
append using muestra_r4.dta
append using muestra_r5.dta

* ----------------------------------------------------------------------
* Step 2: Check and correct "dies then revives" inconsistencies in the orphanhood flags, which can happen due to questionnaire / recording errors across rounds
* ----------------------------------------------------------------------
{
    * Flag potential inconsistencies using auxiliary "total" variables: if a child is marked as an orphan in some rounds and not others inconsistently, these sums help spot it for manual review (see the hardcoded fixes below)
    sort childid ronda
    bysort childid: egen suma = total(orphanhood)
    bysort childid: egen suma_dad = total(orphan_dad)
    bysort childid: egen suma_mom = total(orphan_mom)

    /* The following observations need manual correction due to
       questionnaire errors (identified through manual review of the data):

    PE031055
    PE071024
    PE091056 (mother)
    PE111052
    PE121029
    PE121051 (additional)
    PE131091 (mother) -- does not appear in the data
    PE161050
    PE171092
    PE181093
    */

    ** Fathers first
    replace orphanhood = 1 if childid == "PE031055" & ronda == 4
    replace orphan_dad = 1 if childid == "PE031055" & ronda == 4
    replace orphanhood = 1 if childid == "PE071024" & ronda == 4
    replace orphan_dad = 1 if childid == "PE071024" & ronda == 4
    replace orphanhood = 1 if childid == "PE111052" & ronda == 4
    replace orphan_dad = 1 if childid == "PE111052" & ronda == 4
    replace orphanhood = 1 if childid == "PE121029" & ronda == 4
    replace orphan_dad = 1 if childid == "PE121029" & ronda == 4
    replace orphanhood = 1 if childid == "PE161050" & ronda == 4
    replace orphan_dad = 1 if childid == "PE161050" & ronda == 4
    replace orphanhood = 1 if childid == "PE171092" & ronda == 4
    replace orphan_dad = 1 if childid == "PE171092" & ronda == 4
    replace orphanhood = 1 if childid == "PE181093" & ronda == 4
    replace orphan_dad = 1 if childid == "PE181093" & ronda == 4

    ** Then mothers
    replace orphanhood = 1 if childid == "PE091056" & ronda == 4
    replace orphan_mom = 1 if childid == "PE091056" & ronda == 4

    ** Finally, one additional observation
    replace orphanhood = 1 if childid == "PE121051" & ronda == 5
    replace orphan_dad = 1 if childid == "PE121051" & ronda == 5

    * Drop the auxiliary variables
    drop suma suma_dad suma_mom
}

* ----------------------------------------------------------------------
* Step 3: Create the treatment-timing variable G (first round in which a parent dies)
* ----------------------------------------------------------------------
bysort childid (ronda): egen G = min(cond(orphanhood==1, ronda, .))
replace G = 0 if missing(G) // children who are never orphaned get G = 0 (never-treated)

* ----------------------------------------------------------------------
* Step 4: Merge cluster identifiers into the panel
* ----------------------------------------------------------------------
/*
A cluster-only file must be created first:
use "R2\pechildlevel5yrold.dta", clear
keep childid clustid
save "base_clusters.dta", replace
*/
merge m:1 childid using "base_clusters.dta"
drop if _merge == 2
drop _merge

* ----------------------------------------------------------------------
* Step 5: Add controls
* ----------------------------------------------------------------------
/*
use base_controles_final.dta, clear
drop Ronda
save base_controles_final.dta, replace
*/
merge 1:1 childid ronda using base_controles_final.dta
drop if _merge == 2
drop _merge

* ----------------------------------------------------------------------
* Step 6: Clean up implausible values in the time-use variables (88 was used in the raw questionnaire as a "don't know"/non-response code)
* ----------------------------------------------------------------------
{
    tab horas_sleep
    replace horas_sleep = . if horas_sleep > 24

    tab horas_care
    replace horas_care = . if horas_care > 24

    tab horas_chores
    replace horas_chores = . if horas_chores > 24

    tab horas_npaywork
    replace horas_npaywork = . if horas_npaywork > 24

    tab horas_paywork
    replace horas_paywork = . if horas_paywork > 24

    tab horas_school
    replace horas_school = . if horas_school > 24

    tab horas_study
    replace horas_study = . if horas_study > 24
}

* ----------------------------------------------------------------------
* Step 7: Convert nominal expenditure to real expenditure using the CPI (BCRP)
* ----------------------------------------------------------------------
/*
CPI (BCRP)
2006 : 62.65011092
2009 : 69.43510712
2013 : 77.6556915
2016 : 86.00411314
*/
gen gasto_real = 0
replace gasto_real = gasto_educ / 62.65011092 * 100 if ronda == 2
replace gasto_real = gasto_educ / 69.43510712 * 100 if ronda == 3
replace gasto_real = gasto_educ / 77.6556915 * 100 if ronda == 4
replace gasto_real = gasto_educ / 86.00411314 * 100 if ronda == 5

* ----------------------------------------------------------------------
* Step 8: Save the main analytic sample
* ----------------------------------------------------------------------
save muestra_FINAL.dta, replace



*===================================================
*   PART III: BUILD THE CHANNEL DATASETS
*===================================================


* III.1 Wealth channel  -->  "muestra_FINAL2.dta"
*       Split variable: canal_riqueza_50 (median split)

* ----------------------------------------------------------------------
* Step 1: Load the main analytic sample
* ----------------------------------------------------------------------
use muestra_FINAL.dta, clear

* ----------------------------------------------------------------------
* Step 2: Compute the pre-treatment wealth index (one period before the current round)
* ----------------------------------------------------------------------
bysort childid (ronda): gen wi_previo = wi[_n-1]

* ----------------------------------------------------------------------
* Step 3: Compute the median of pre-treatment wealth, measured at the treatment round (G == ronda), among eventually-treated children
* ----------------------------------------------------------------------
preserve
    keep if G != 0
    keep if G == ronda
    sum wi_previo, detail
    scalar percentil50 = r(p50)
restore

* ----------------------------------------------------------------------
* Step 4: Split eventually-treated children into above/below median wealth
* ----------------------------------------------------------------------
gen canal_riqueza_50 = 0
by childid: replace canal_riqueza_50 = 1 if wi_previo > percentil50 & G != 0
by childid: replace canal_riqueza_50 = 2 if wi_previo <= percentil50 & G != 0

preserve
    keep if G != 0
    keep if G == ronda
    tab canal_riqueza_50
restore

tab canal_riqueza_50

* ----------------------------------------------------------------------
* Step 5: Save the wealth-channel dataset
* ----------------------------------------------------------------------
save muestra_FINAL2.dta, replace



* III.2 Returns / cognitive-ability channel  -->  "muestra_FINAL3.dta"
*       Split variable: canal_retornos_75 (75th-percentile split)


* ----------------------------------------------------------------------
* Step 1: Load the main analytic sample
* ----------------------------------------------------------------------
use muestra_FINAL.dta, clear

rename porcentaje_ppvt ppvt

* ----------------------------------------------------------------------
* Step 2: Compute the pre-treatment PPVT score (proxy for cognitive ability / returns to schooling), one period before the current round
* ----------------------------------------------------------------------
bysort childid (ronda): gen ppvt_previo = ppvt[_n-1]

* ----------------------------------------------------------------------
* Step 3: Compute the 75th percentile of pre-treatment PPVT, measured at the treatment round (G == ronda), among eventually-treated children
* ----------------------------------------------------------------------
preserve
    keep if G != 0
    keep if G == ronda
    sum ppvt_previo, detail
    scalar percentil75 = r(p75)
restore

* ----------------------------------------------------------------------
* Step 4: Split eventually-treated children above/below the 75th percentile of pre-treatment PPVT
* ----------------------------------------------------------------------
gen canal_retornos_50 = 0
by childid: replace canal_retornos_75 = 1 if ppvt_previo > percentil75 & G != 0
by childid: replace canal_retornos_75 = 2 if ppvt_previo <= percentil75 & G != 0

preserve
    keep if G != 0
    keep if G == ronda
    tab canal_retornos_75
restore

tab canal_retornos_75

* ----------------------------------------------------------------------
* Step 5: Save the returns-channel dataset
* ----------------------------------------------------------------------
save muestra_FINAL3.dta, replace
