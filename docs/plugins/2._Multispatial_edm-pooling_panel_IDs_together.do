* CITE AS: Zyphur, M. J., & Li, J. (2019). Multispatial EDM (pooling panel IDs together). Stata .do file retrieved online.
* CONTACT: mzyphur@unimelb.edu.au

* NOTES ON THE .DO FILE AND RESULTS:
* This estimates multisptatial EDM for all panel IDs (xtset) as follows:
* 1) Initial simplex projections (SP), which is diagnostic and determines the E used in subsequent analyses
* 2) Predictability at different prediction steps (TP) into the future using simplex projections
* 3) S-mapping (SMAP) to test for nonlinearity at different theta values
* 4) Coprediction to evaluate predictability for all unique variable pairs using simplex projections
* 5) Convergent cross mapping (CCM) or xmap to evaluate causal effects vs. common drivers, etc.
* Note 1: Requires these "scc install" packages: lgraph, labutil, heatplot, palettes, colrspace, findname, gtools
* Note 2: If computations fail for any analysis, no record is created in saved results, so check output for errors
* Note 3: User must first address each local macro below following the ALL CAPS HEADINGS
* Note 4: The length of variable names must be 8 characters or less 
* Note 5: Similarly, panel ID codings must be 7 characters or less (i.e., less than 10,000,000)


* EDM COMMAND OPTIONS AND SAVE RESULTS
local options = ""   // Options for the EDM command can be entered here with spaces between them; ensure options apply to both 'explore' and 'xmap' subcommands
* Note 1: For example this can be left bank to use the parallel processing plugin or enter "mata" to run using original mata code
* Note 2: If the default plugin is used, users may enter "nthreads(#)" to set the number of threads # for the plugin (overriding the default thread count)
* Note 3: To create equivalent runs set a seed() option as per usual such as seed(12345)
local saveresults = "yes"   // Will save all results into local Excel file EDMresults.xlsx
local useresults = "rho"   // Set "rho" or "mae" to choose outputting rho or MAE results; consider matching to 'testval' and 'Ecriterion' below
* Note 1: this option will also be used for any plots selected below


* SET VARIABLE AND PANEL ID / TIME INFO
* Note 1: This added precision will simplify various equality checks below
unab variables:   // Create varlist by entering variable names after colon
* Note 1: Coprediction produces: (a) results in a 'wide' format as square matrices of these variables; and
* (b) results in a 'long' format with all variable pairs stacked and coded numerically (and as strings) for plotting
local panelIDvar = ""   // Name of xtset panel ID variable entered in quotes
local timevar = ""   // Name of xtset time variable entered in quotes
local dropzeros = "yes"   // Set "yes" or "no" to drop E-length runs of zero for analyses AFTER the initial simplex projections
* Note 1: Deyle et al., 2016a (Global... influenza): "long stretches of zero-values... will contain little or no information"
* "we exclude all stretches of zeros at least... maximum tested E... from the prediction set to improve the sensitivity of the analysis."
* Note 2: Dropping E-length runs of zero can speed up estimation, often with only small differences in results
local prestd = "z."   // Set "z." to standardize variables as z-scores for analysis, or leave blank "" for no standardization
* Note 1: Standardization can help coprediction/CCM in small panel-specific (within) sample sizes with very different scales/variances
* Note 2: Standardization helps interpretation of MAE (on a scale 0-1); for rho analogue this allows constructing 1 - MAE
* Note 3: Standardization done before differencing, so to retain this MAE interpretation difference the variable before analysis with "z."
* Note 4: Standardization done for whole variable, not on a per-panel ID basis; may want to pre-standardize per panel before analysis
* Note 4: Standardization only essential when doing multivariate EDM embeddings, which is not programmed here
local forcing = "force"   // Set "force" to adjust analyses for overlapping points/values on manifold; otherwise set blank ""
* Note 1: If forcing left blank "" then many overlapping points/values will cause the analysis to be skipped


* SET MISSING DATA FEATURES AND EXTRA VARIABLES TO INCLUDE IN MANIFOLD
local time_gap = ""   // Set "dt" to include information about varying time gaps (i.e., unequal intervals of observation) or leave blank as ""
* Note 1: This will include E - 1 variables that measure time gaps as dt (e.g., t1 - t2), with weighting equal to measured variable as SDx/SDdt
* Note 2: If time gaps are equal but data are missing, then setting "dt" will incorporate info about missingness as time gap data
* Note 3: In this case missing data will still be dropped unless the "missing" option below is used
* Note 4: If dt is set then all CCM analyses are contemporaneous and the lag/leads options below are automatically reset to 0
local tgap_weight = ""   // Set to override automatic weight (not recommended without prior exploratory analyses) or leave blank "" for default
* Note 1: For optimal weight, first explore prediction quality in simplex projection with automatic weight and then vary this weight +/- increments manually
* Note 2: This is generally not recommended in combination with the "extra" option below, as different variables may have different optimal weights
local missings = ""   // Set "allowmissing" to keep missing data in the manifold, with distance set to SDx where x is measured variable
* Note 1: This is analogous to an imputation strategy with missing values replaced by whatever results in nearest-neighbor distances = SD*2/sqrt(pi)
* Note 2: If data are missing and this is not set, then those data will be dropped to produce a manifold with up to E missing data vectors
* Note 3: With unequal time gaps with "dt" and missing data, both options can be used to keep all data in for example CCM where x is missing but y is not
local miss_distance = ""   //   Set to override automatic missing datance SD*2/sqrt(pi) (not recommended without exploratory analysis) or leave blank "" for default
* Note 1: For optimal distance, exploratory analyses can be conducted using simplex projection but default may be advised to reflect uncertainty
* Note 2: As with tgap_weight this is not recommended with the "extra" option below, as different variables may have different optimal distances
local extras = ""   // Set to include any additional susbtantive variables in the manifold (i.e., a multivariate embedding)
* Note 1: To ensure equal weighting of variables set prestd = "z." above and standardization is automatically applied to variables listed as extras here
* Note 2: When a difference operator is applied to main variables it is also automatically applied to the variables listed as extras
local extras_raw = ""   // Set to include any extra variables that should not be differenced during analyses, such as time-invariant variables
* Note 1: With missing data using missing = "allowmissing", extras_raw can include missing data info as binary indicator of missingness
* Note 2: The prestd = "z." prefix is not applied to extras_raw so if standardization is desired it should be done manuall using the z. prefix (e.g., "z.x")
* Note 3: With multiple panels, info about panels can be included as N - 1 dummy variables listed as separate extras_raw variables


* SET INITIAL SIMPLEX PROJECTION (SP) METHOD
* This analysis uses the default 50/50 random split of the sample for analysis
local SPrun = "yes"   // Set "yes" or "no" to run initial simplex projections; results required, so only set "no" if previously run
* Note 1: That results from this analysis will be used to set E for all subsequent analyses
* Note 2: Plots and results saved as variables are named SP_'par'_'var'
* 'par' is parameter name (e.g., rho, mae, etc.) and 'var' is variable name
local eminimum = 2   // User sets minimum E used in exploratory simplex projections, 2 by default
local emaximum = 20   // User sets maximum E used in exploratory simplex projections, 20 by default
* Note 1: Set eminimum=emaximum overrides 'Ecriterion' to fix E at one value (not recommended as it doesn't choose E based on results)
local drange = "0/1"   // Set difference operators for initial evaluations, such as 0/1 for raw and first-differenced
* Note 1: Should be presented as a range, so if no differencing required set "0/0" or if first only set "1/1"
* Note 2: This value is automatically set to 0 if 'dt' used for time gaps, as differencing is not straightforward
local dropzeros_SP = "yes"   // Set "yes" or "no" to drop spells of zeros of length maximum E -- same notes as 'dropzeros'  
* Note 1: If "yes" then zeros will also be excluded from the permutations used for hypothesis tests of SP results
* Note 1: If "yes" then options below are rendered irrelevant and are overridden
local SPsampler = "rep"   // Set "rep", "crossfold", or "full"
* Note 1: Set "rep" for 50/50 random library/manifold splits, which is default
* Note 2: Set "crossfold" for k-fold cross-validation
* Note 3: With T < ~50 set "full" for leave-one-out cross-validation using full sample for training/manifold reconstruction and testing/prediction
local SPreps = 50   // Sets number of replications for 50/50 random splits or the number of groups k for k-fold cross-validation; must be > 1
* Note 1: When SPsampler = "full" the SPreps option is overridden so is irrelevant


* CHOOSE METHOD FOR SELECTING E FOR ALL SUBSEQUENT ANALYSES, AND SET THE DESIRED DIFFERENCE OPERATOR
local Ecriterion = "rho"   // Set "rho", "mae", "dmae", "R2", or "dR2" for E settings per panel ID
* Note 1: "rho" and "mae" options choose max(rho) or min(MAE) across the range of E analyzed for each panel ID
* Note 2: Selecting "rho" or "mae" offers enhanced descriptive and diagnostic plots of max(rho)-min(MAE) relationship
* Note 3: Use MAE over rho for short time series where rho-E relationship noisy (MAE less sensitive to outliers; Deyle et al., 2013, online appendix)
* Note 4: "dmae" uses proportion mae change (mae(E) - mae(E+1))/mae(E), starting at minimum E and choosing E+1 if difference > 'dmaeval'
* Note 5: "R2" uses R2 change rho^2(E+1) - rho^2(E), starting at minimum E and choosing E+1 if difference > 'R2val'
* Note 6: "dR2" uses proportion R2 change (rho^2(E+1) - rho^2(E))/rho^2(E), starting at minimum E and choosing E+1 if difference > 'dR2val'
* Note 7: Consider "dmae", "R2", or "dR2" if prediction improves as E-->infinity, meaning there is no true max(rho) or min(MAE), merely max E analyzed
* Note 8: In the unlikely case of negative rho, "dmae", "R2", and "dR2" account for the sign of rho, measuring improvements as rho moves towards 1
* Note 9: "dmae", "R2", and "dR2" methods compare E and E+1 starting at E=2, so if E=3 does not improve R2, then E=2 chosen even if big improvements at E>3
local dmaeval = ".05"   // If Ecriterion = "dmae", dmaeval is the cutoff that proportion change in mae must exceed to reject E for E+1 (5% default)
local R2val = ".01"   // If Ecriterion = "R2", R2val is the cutoff that change in R2 must exceed to reject E for E+1 (.01 default)
local dR2val = ".05"   // If Ecriterion = "dR2", dR2val is the cutoff that proportion change in R2 must exceed to reject E for E+1 (5% default)
local diff = 0   // Set difference operator for all subsequent analyses;
* Note 1: Setting to 1 can eliminate any fixed between-group differences and linear trends


* SET PREDICTION STEPS INTO THE FUTURE (TP) USING SIMPLEX PROJECTION METHOD
* This analysis uses the default 50/50 random split of the sample for analysis
local TPrun = "yes"   // Set "yes" or "no" to run step-ahead prediction analysis
* Note 1: Plots and results saved as variables are named TP_'par'_'var'
* 'par' is parameter name (e.g., rho, mae, etc.) and 'var' is variable name
local TPmax = 5   // Set max value for step-ahead prediction (tp; treated as numlist), set as 1/'TPmax'
local TPsampler = "rep"   // Set "rep", "crossfold", or "full"
* Note 1: Set "rep" for 50/50 random library/manifold splits, which is default
* Note 2: Set "crossfold" for k-fold cross-validation
* Note 3: With T < ~50 set "full" for leave-one-out cross-validation using full sample for training/manifold reconstruction and testing/prediction
local TPreps = 50   // Sets number of replications for 50/50 random splits or the number of groups k for k-fold cross-validation; must be > 1
* Note 1: When SPsampler = "full" the SPreps option is overridden so is irrelevant


* SET S-MAPPING (SMAP) METHOD
* This analysis uses the default 50/50 random split of the sample for analysis
local SMAPrun = "yes"   // Set "yes" or "no" to run SMAPs
* Note 1: Plots and results saved as variables are named SMAP_'par'_'var'
* 'par' is parameter name (e.g., rho, mae, etc.) and 'var' is variable name
local theta = "0 .01 .1 .5 1 2 3 5"   // Theta values to test nonlinearity (as numlist), default "0 .01 .1 .5 1 1.5 2 5"
local kval = -1   // Number of neighbors to use, for all possible set to -1 but here set to 70
local SMAPsampler = "rep"   // Set "rep", "crossfold", or "full"
* Note 1: Set "rep" for 50/50 random library/manifold splits, which is default
* Note 2: Set "crossfold" for k-fold cross-validation
* Note 3: With T < ~50 set "full" for leave-one-out cross-validation using full sample for training/manifold reconstruction and testing/prediction
local SMAPreps = 50   // Sets number of replications for 50/50 random splits or the number of groups k for k-fold cross-validation; must be > 1
* Note 1: When SPsampler = "full" the SPreps option is overridden so is irrelevant


* SET CO-PREDICTION (COPRED)
* This analysis automatically uses the "full" sample option to form the manifolds used for predictions
local COPREDrun = "yes"   // Set "yes" or "no" to run Copredictions


* SET CONVERGENT CROSS MAPPING (CCM) OR XMAP METHOD
local CCMrun = "yes"   // Set "yes" or "no" to run CCM
local lib1 = 2   // First library size E+'lib1' to test convergence; lib1 must be > 1 for minimum of E+1 neighbors and 1 target vector on manifold
* Note 1: These libraries smaller than the maximum library size formed by randomly sampling E-length vectors of observations
local CCMreps1 = 100   // Number of replications for the smallest library size lib(E+2) often used to find 95% CI, default 200; must be > 0
* Note 1: Many replications should not take very long with a small library size L, but compute times will grow exponentially as L increases
local lib2 = 3   // Next library size beyond lib1, takes the form E+'lib2' so should be > lib1, numlist default 5
local lib3 = 10   // Next library size beyond lib2, takes the form E+'lib3' so should be > lib2, numlist default 10
local lib4 = 20   // Next library size beyond lib3, takes the form E+'lib4' so should be > lib3, numlist default 20
local CCMreps2 = 50   // Number of replications for the next set of library sizes lib(E+lib2 E+lib3 E+lib4), default 50; must be > 0
local fraction1 = 3   // Denominator for mid library sizes where N/fraction1, default 3 so library size is rounded 1/3 total possible
local fraction2 = 2   // Denominator for mid library sizes where N/fraction2, default 2 so library size is rounded 1/2 total possible    
local CCMreps3 = 20   // Number of replications for the next set of library sizes lib(N/fraction1 N/fraction2), default 10; must be > 0
local CCMlags = "10"   // Largest lag for CCM as "'CCMlags'(-1)1"; consider default of 'emaximum'+1 but can be any value > 0
* Note 1: For no lagged CCM analyses set to "NA"
local CCMleads = "5"   // largest lead for testing effects vs. common causes as numlist "0/'CCMleads'" 
* Note 1: For no lead analysis set to 0, which in combination with CCMlags = "NA" will run CCM only contemporaneously


* SET SMAP OPTION FOR ESTIMATING LOCAL LINEAR EFFECTS FROM CCM
local CCMSMAP = "yes"   // Set "yes" or "no" to compute coefficients using SMAP method
local CCMSMAPk = "-1"   // Set to number of neighbors to use for deriving CCM SMAP coefficients, -1 is default as this uses all possible neighbors
* Note 1: With large sample sizes compute times can be reduced by setting to some fraction of sample size


* SET HYPOTHESIS TESTING OPTIONS
* Note 1: Unless otherwise noted, tests are one-tailed to correspond with EDM hypotheses (e.g., rho > 0)
* Note 2: Bootstrap uses random 50/50 splits to create a distribution for rho or mae; e.g., tests if two boostrapped rhos are different
* Note 3: Permuting randomizes time index to form NHST-type pivotal null distribution; e.g., tests if observed rho > 95th percentile of null
* Note 4: Permuting uses 'full' samples option whenever possible to ensure uncertainty relates only to randomized the time index
* Note 5: When MAE is used the direction of tests is reversed; e.g., is observed MAE < 5th percentile of permuted null for p < .05?
* Note 6: Replications are done per panel ID, so choosing many reps will be computationally heavy with large N and T
* Note 8: All test results are graphed, but are saved only if the plots for their analyses are also saved below under PLOT OPTIONS
* Note 9: All tests include p < .05 and .01 equivalents, but to interpret p < .01 tests consider many replications (e.g., >= 1,000)
* Note 10: All plots are saved if their respective plotting options further below indicate plots should be saved
local testval = "rho"   // Set "rho" or "mae" to use for all tests; consider matching to 'Ecriterion'
* Note 1: This parameter applies to all tests
local SPtest = "yes"   // Set "yes" or "no" for simplex projection test of prediction quality at chosen E using permutation-based null
* Note 1: This test uses the 'full' sample option rather than the initial 50/50 random split for simplex projections
* Note 2: If dropping zeros = "yes" this test drops zeros in relation to the optimal E found in initial simplex projections rather than 'emaximum'
local SPtestreps = 1000   // Number of permutation replications to generate null distribution for SPtest
* Note 1: With many initial bootstrap replications used for simplex projection, this can save substantial time with large N and T
local SPtestplot = "yes"   // Set "yes" or "no" to finalize and plot the results of the SPtest
local SMAPtest = "yes"   // Set "yes" or "no" for bootstrap test of improved prediction at best theta>0 versus theta=0
* Note 1: The seed for paired bootstrapped S-map tests is seed(12345) unless 'options' list contains a different seed() value
local SMAPtestreps = 1000   // Number of bootstrap replications for SMAPtest
* Note 1: This test reuses any SMAP bootstrap reps 'SMAPreps', so new reps for theta=0 and theta>0 are 2(SMAPtestreps - SMAPreps)
local SMAPtestplot = "yes"   // Set "yes" or "no" to finalize and plot the results of the SMAPtest
local StepwiseCCMtest = "yes"   // Set "yes" or "no" to make CCM testing and SMAP coefficient computations conditional on assumption checks 
* Note 1: This sequentially does the following: 
* 1) tests convergence only if optimal prediction at lag>=0; 
* 2) runs permutation test at max L only if convergence found at 95%; and
* 3) limits SMAP coefficient analysis using CCM method to cases that pass permutation test at max L at 95%
* Note 2: See: Ye et al. (2015). Distinguishing time-delayed causal interactions using convergent cross mapping. Scientific Reports.
* Note 3: See: Wang et al., (2019). Improved CCM for variable causality detection in complex systems. Control Engineering Practice.
local CCMctest = "yes"   // Set "yes" or "no" for bootstrap test of convergence from min library size E+'lib1' versus max
* Note 1: This test constructs a CI around rho/mae at minimum library size E+'lib1' and checks if max library size rho/mae outside the CI
* Note 2: Best test of convergence typically has 'lib1' from above set to minimum value of 2
* Note 3: In some cases manifold reconstruction problems arrise with too many zeros and small libraries; if so consider lib1 > 2
local CCMctestreps = 1000   // Number of bootstrap replications at minimum library size for CCMctest
* Note 1: This test reuses any CCM bootstrap reps 'CCMreps1', so new reps = CCMconvreuse - CCMreps1
local CCMctestplot = "yes"   // Set "yes" or "no" to finalize and plot the results of the CCMctestplot
local CCMLtest = "yes"   // Set "yes" or "no" for permutation test of CCM predictions at max library better than null
local CCMLtestreps = 1000   // Number of permutation replications for CCMLtest
local CCMLtestplot = "yes"   // Set "yes" or "no" to finalize and plot the results of the CCMLtest


* SET GENERAL PLOT OPTIONS
local plots_on = "yes"   // Set "yes" or "no" to enable plotting; this does not affect hypothesis test plots, which are on if tests = "yes"
* Note 1: When useresults="rho" or "mae" with "z." standardization, the y-axis of many plots scaled with range: -.25(.25)1
local exportplot = "png"   // Set as blank "" to not export graphs, or to export specify format "png", "tif", "pdf", "emf", "wmf", "eps" or "ps"
local CIs = 95   // Set 95 or 90 for 90% or 95% CI error bars; enter 0 for no CIs, which is advised with many panel IDs (N > 20)
* Note 1: This option does not affect the way hypothesis tests are plotted, and instead merely pertains to descriptive output
* Note 2: All confidence intervals are symetric as they use SD values, so not used for hypothesis testing merely description
* Note 3: CIs for convergent cross mapping (CCM) not available

* ANALYSIS-SPECIFIC PLOT OPTIONS
* Note 1: Plots of hypothesis tests produced if plots are requested for their analyses (e.g., SPgraph = "yes" will plot 'SPtest' results)
local SPgraph = "yes"   // Set "yes" or "no" for initial simplex projection plots of average rho (y-axis) by E (x-axis)
local SPsave = "yes"   // Set "yes" or "no" to save and replace or not save the initial simplex projection plots
local SPlinefit = ""   // Set "lfit", "qfit", or "fpfit" for linear, quadratic, or fractional polynomial fit; default is nothing
local TPgraph = "yes"   // Set "yes" or "no" for plots of average rho (y-axis) across prediction time tp (x-axis)
local TPsave = "yes"   // Set "yes" or "no" to save and replace or not save the prediction time plots
local TPlinefit = ""   // Set "lfit", "qfit", or "fpfit" for linear, quadratic, or fractional polynomial fit; default is nothing
local SMAPgraph = "yes"   // Set "yes" or "no" for plots of average rho (y-axis) for SMAP theta values (x-axis)
local SMAPsave = "yes"   // Set "yes" or "no" to save and replace or not save the S-map plots
local SMAPlinefit = ""   // Set "lfit", "qfit", or "fpfit" for linear, quadratic, or fractional polynomial fit; default is nothing
* Note 1: For SP, TP, and SMAP the "separate(.001)" option is used to reduce clutter, but can set smaller (larger) for more (fewer) panels
* Note 2: For SP, TP, and SMAP the "nolegend" option used to reduce clutter, but can be set to "legend(size(vsmall))" if of interest
local COPREDgraph = "yes"   // Set "yes" or "no" for plots of every variable pair using the coprediction method
* Note 1: Coprediction plots are: 1) heatmap; and 2) two types of bar graphs (switching predictor-predictee settings)
* Note 2: No CIs are programmed because coprediction methods use the full dataset by default, with leave-one-out predictions
local COPREDsave = "yes"   // Set "yes" or "no" to save and replace or not save the coprediction plots
local COPREDvalues = "yes"   // Set "yes" or "no" to display average rho or MAE values in heatmat plot cells
local COPREDdecimals = 2   // Set number of decimals to display in values of heatmap plot
* Note 1: With more than 10 variables this could be set to 1 for readability
local CCMgraph = "yes"   // Set "yes" or "no" for plots of average rho (y-axis) for CCM library size (x-axis) at lag = 0
local CCMsave = "yes"   // Set "yes" or "no" to save and replace or not save the CCM plots
local CCMlinefit = "fpfit"   // Set "lfit", "qfit", or "fpfit" for linear, quadratic, or fractional polynomial fit; default is nothing
* Note 1: Line fitting helps when different panel IDs have different E or N values, and thus different minimum and maximum library sizes
* Note 2: If library sizes and prediction quality differs across panel IDs, grand-mean lines will be unhelpful without line-fitting
local CCMlaggraph = "yes"   // Set "yes" or "no" for plots of average rho (y-axis) for CCM at different lags (x-axis)
local CCMlagsave = "yes"   // Set "yes" or "no" to save and replace or not save the CCM plots
local CCMlaglinefit = ""   // Set "lfit", "qfit", or "fpfit" for linear, quadratic, or fractional polynomial fit; default is nothing
local CCMSMAPgraph = "yes"   // Set "yes" or "no" for plots of CCM coefficients derived from the SMAP method
local CCMSMAPsave = "yes"   // Set "yes" or "no" to save and replace or not save the CCM SMAP plots
local CCMSMAPlinefit = ""   // Set "lfit", "qfit", or "fpfit" for linear, quadratic, or fractional polynomial fit; default is nothing
local CCMSMAPstat = "median"   // Set statistic from collapse command to plot, default is median but mean for example may be used
local CCMSMAPeffects = "10"   // Set maximum number of effects to plot, where theoretical max is E but plots get cluttered with more than 5 
* Note 1: Each effect is really one of E columns in the E-dimensional vectors used for SMAP predictions
* Note 2: Because each vector is a lagged embedding, this is like the number of 'lagged effects' to plot from L0 to L'CCMSMAPeffects'-1


* "****************************"
* "*                          *"
* "*  Begin Analysis Scripts  *"
* "*                          *"
* "****************************"



findname `variables', any(length("@") > 8)
if "`r(varlist)'"!="" {
*******************************************************************************************
* Note1: Sorry, you have at least one variable with a name longer than 8 characters.      *
* Note2: To solve this perhaps use clonevar and give it a shorter name.                   *
* Note3: Also, please call your elected official to voice concerns about global warming!  *
*******************************************************************************************
exit
}


* Housekeeping
set type double
tempname A A1 A2 B B Blong C C2 C Clong D Dlong E Elong N Nlong Z1 Z2 Z21 Z22 Z3 RhoGM MaeGM ///
Bpart Wpart O Amat CoPred1 CoPred2 NewMat Copredrho Copredmae Predrho Levelslove Test1
tempvar tempstD tempdt1 absvaluE randoM newtimE obsnumbeR obsnO panelvar
if "`tgap_weight'"=="" loc tweight = ""
else loc tweight = "dtweight(`tgap_weight')"
if "`miss_distance'"=="" loc mdistance = ""
else loc mdistance = "missingdistance(`miss_distance')"
loc extraz1 = wordcount("`extras'")
loc extraz2 = wordcount("`extras_raw'")
loc extraz=`extraz1'+`extraz2'
set more off
if "`time_gap'" == "dt" {
loc drange = 0
loc diff = 0
}
if "`CCMlags'" == "NA" & "`CCMleads'"=="0" {
loc CCMlaggraph = "no"
}



* Treat all panels as part of the same dynamic system
local panelrange = "1/1"   // Range of panel IDs to analyze (as a numlist of values on the panel ID variable), here 1-50
gen `panelvar'=1 if `panelIDvar'!=.



* Parse 'extras' list of variables to apply standardization and difference operators
loc exd = `diff'
foreach dval of numlist `drange' {
if `dval' > `diff' loc exd = `dval'
}
tokenize `extras'
forval dn = 0/`exd' {
loc extras`dn' = ""
loc i = 1
while "``i''" != "" {
loc extras`dn' = "`extras`dn'' `prestd'd`dn'.``i''"
loc i = `i' + 1
}
}


* Override default S-map test seed setting if seed appears in 'options' list
tokenize `options'
loc smapseed = "seed(12345)"
loc i = 1
while "``i''" != "" {
loc first=substr("``i''",1,4)
if "`first'" == "seed" loc smapseed = ""
loc i = `i' + 1
}


* Create variables to allow dropping zeros in initial Simplex Projections
tsset
if "`dropzeros_SP'"=="yes" {
foreach var of varlist `variables' {
tempvar `var'_nonzeroSPuse `var'_nonzeroSP
qui gen ``var'_nonzeroSPuse' = .
qui gen ``var'_nonzeroSP' = -1
qui bysort `panelIDvar' (`timevar'): replace ``var'_nonzeroSP' = 0 if `var' == 0
qui bysort `panelIDvar' (`timevar'): replace ``var'_nonzeroSP' = sum(``var'_nonzeroSP')
qui bysort `panelIDvar' (`timevar'): replace ``var'_nonzeroSPuse' = 1 if ``var'_nonzeroSP'[_n] < ``var'_nonzeroSP'[_n-`emaximum'-`diff']
tsset
loc `var'dropzesSP = "& \`\`var'_nonzeroSP'[_n] < \`\`var'_nonzeroSP'[_n-\`emaximum'-\`dval']"
loc `var'dropzesSPtest = "& \`\`var'_nonzeroSPuse' == 1"
}
}
else {
foreach var of varlist `variables' {
loc `var'dropzesSP = ""
loc `var'dropzesSPtest = ""
}
}


* "*************************************"
* "*                                   *"
* "*  Initial Simplex Projection (SP)  *"
* "*                                   *"
* "*************************************"
* "Note 1: Results are used to set E for all subsequent analyses using max(rho) or min(MAE)"
* "Note 2: Plots and results saved as variables are named SP_'par'_'var'
* "The 'par' is parameter name (e.g., rho, mae, etc.) and 'var' is variable name"
if "`SPrun'"=="yes" {
if "`SPsampler'" == "full" {
loc SPreps = 2
loc SPsampler = "rep"
loc fully = "full" 
}
loc enum = `emaximum'-`eminimum'+1   /* Number of E values analyzed, used to create matrices to save results */
loc enumlong = `enum'*`SPreps'
foreach var of varlist `variables' {
tempname SP`var' SPlong`var'
mata: `SP`var''=J(0,9,.)
mata: `SPlong`var''=J(0,7,.)
foreach id of numlist `panelrange' {
mata: `C'=J(1,`enum',`id')   /* Set matrix with panel ID identifiers, linked to saved results below */
mata: `Clong'=J(1,`enumlong',`id')
foreach dval of numlist `drange' {   /* Set difference operators used for analysis */
di "------------------------------------"
di "Below D = `dval', VAR = `var'"
di "------------------------------------"
capture noisily {
edm explore `prestd'd`dval'.`var' if `panelvar'==`id' ``var'dropzesSP', e(`eminimum'/`emaximum') `fully' `SPsampler'(`SPreps') `forcing' `time_gap' `missings' extra(`extras`dval'' `extras_raw') `tweight' `mdistance' reportrawe `options'
}
capture {
mata: `D'=J(1,`enum',`dval')   /* Set matrix with difference operator, linked to saved results below */
mata: `Dlong'=J(1,`enumlong',`dval')
mata: `N'=J(1,`enum',st_numscalar("e(N)"))
mata: `Nlong'=J(1,`enumlong',st_numscalar("e(N)"))
mata: `B'=st_matrix("e(summary)")
mata: `E'=`C'\ `D'\ `B''\ `N'
mata: `SP`var''=`SP`var''\ `E''
mata: `Blong'=st_matrix("e(explore_result)")
mata: `Elong'=`Clong'\ `Dlong'\ `Blong''\ `Nlong'
mata: `SPlong`var''=`SPlong`var''\ `Elong''
}
}
}
capture quietly {
getmata (SP`var'*)=`SP`var'' (SPlong`var'*)=`SPlong`var'', force double
label variable SP`var'1 "Simplex Projection panel ID of 1"
rename SP`var'1 SP_id_`var'
label variable SP`var'2 "Simplex Projection difference operator"
rename SP`var'2 SP_d_`var'
recast byte SP_d_`var'
label variable SP`var'3 "Simplex Projection E value"
rename SP`var'3 SP_e_`var'
recast int SP_e_`var'
drop SP`var'4
label variable SP`var'5 "Simplex Projection average rho (`SPreps' `SPsampler's)"
rename SP`var'5 SP_rho_`var'
label variable SP`var'6 "Simplex Projection rho SD (`SPreps'  `SPsampler's)"
rename SP`var'6 SP_rhoSD_`var'
label variable SP`var'7 "Simplex Projection average MAE (`SPreps'  `SPsampler's)"
rename SP`var'7 SP_mae_`var'
label variable SP`var'8 "Simplex Projection MAE SD (`SPreps'  `SPsampler's)"
rename SP`var'8 SP_maeSD_`var'
label variable SP`var'9 "Simplex Projection sample size as usable observations - max E"
rename SP`var'9 SP_N_`var'
recast long SP_N_`var'

label variable SPlong`var'1 "Simplex Projection long (all `SPsampler's), panel ID of 1"
rename SPlong`var'1 SPlong_id_`var'
label variable SPlong`var'2 "Simplex Projection long (all `SPsampler's), difference operator"
rename SPlong`var'2 SPlong_d_`var'
recast byte SPlong_d_`var'
label variable SPlong`var'3 "Simplex Projection long (all `SPsampler's), E value"
rename SPlong`var'3 SPlong_e_`var'
recast int SPlong_e_`var'
drop SPlong`var'4
label variable SPlong`var'5 "Simplex Projection long (all `SPsampler's), rho"
rename SPlong`var'5 SPlong_rho_`var'
label variable SPlong`var'6 "Simplex Projection long (all `SPsampler's), MAE"
rename SPlong`var'6 SPlong_mae_`var'
label variable SPlong`var'7 "Simplex Projection long (all `SPsampler's), sample size as usable observations - max E"
rename SPlong`var'7 SPlong_N_`var'
}
}
}


clear mata



* Initial simplex projection plots
if "`SPgraph'"=="yes" & "`plots_on'" =="yes" {
preserve
foreach var of varlist `variables' {
* Confidence intervals for full plots
if `CIs'==95 | `CIs'==90 loc a="errortype(ci(`CIs'))"
else loc a=""
* Use rho or MAE labeling scheme
if "`useresults'" == "rho" loc z="&rho"
else if "`useresults'" == "mae" loc z="stSerif:MAE"
* Set scale for rho and standardized MAE results
if "`useresults'" == "rho" loc ylabelscale="ylabel(-.25(.25)1) yscale(range(-.2 1))"
else if "`useresults'" == "mae" & "`prestd'" == "z." loc ylabelscale="ylabel(-.25(.25)1) yscale(range(-.2 1))"
else loc ylabelscale=""
foreach dval of numlist `drange' {
* Saving results
if "`SPsave'"=="yes" loc b="saving(SP_`useresults'_`var'_d`dval', replace)" 
else loc b=""
* Plot command
lgraph SPlong_`useresults'_`var' SPlong_e_`var' SPlong_id_`var' if SPlong_d_`var'==`dval', ///
`a' `b' separate(.001) fit(`SPlinefit') `ylabelscale' ///
title({stSerif:SP: Results for `var'}, color(black)) ///
ytitle({stSerif:Prediction Accuracy} {`z'}) xtitle({stSerif:Dimensionality E}) ///
note({stSerif:Note: Grand-mean line is bolded -- differencing at d`dval'}) ///
loptions(1 lwidth(*6)) foptions(1 lwidth(*6)) graphregion(color(white)) bgcolor(white) ///
name(SP_`useresults'_`var'_d`dval', replace) nolegend

if "`exportplot'" != "" {
graph export SP_`useresults'_`var'_d`dval'.`exportplot', replace
}
serset clear

}
}
restore
}

serset clear
clear mata


* Identify E at max(rho), min(MAE), proportional change in mae, change in R2, or proportional change dR2 for each variable
preserve
* First case is only one E value or "chosen E" used for all analyses
if `eminimum'==`emaximum' {
foreach var of varlist `variables' {
foreach id of numlist `panelrange' {
loc emax`var'`id'=`eminimum'
}
}
}

* Second case is selecting E based on maximum rho or minimum MAE
else if "`Ecriterion'"=="rho" | "`Ecriterion'"=="mae" {
qui gen long `obsnO'=_n
foreach var of varlist `variables' {
foreach id of numlist `panelrange' {
capture {
quietly summarize SP_`Ecriterion'_`var' if SP_id_`var'==`id' & SP_d_`var'==`diff', meanonly
if "`Ecriterion'"=="rho" loc a="max"
else if "`Ecriterion'"=="mae" loc a="min"
sca `Levelslove'=r(`a')
qui levelsof `obsnO' if SP_`Ecriterion'_`var' == `Levelslove' & SP_id_`var'==`id' & SP_d_`var'==`diff'
loc emax`var'`id'=SP_e_`var'[`r(levels)']
}
}
}
}

* Third case is selecting E based on change in R2 or proportion change in R2 (dR2)
else if "`Ecriterion'" == "R2" | "`Ecriterion'"=="dR2" {
qui gen long `obsnO'=_n
foreach var of varlist `variables' {
tempvar R2`var' eR2`var'
qui gen `R2`var''=.
qui gen `eR2`var''=.
replace `eR2`var''=2 if SP_e_`var'==2
qui gen rhosign`var'=1
qui replace rhosign`var'=-1 if SP_rho_`var'<0
gen max_rho_`var'=.
label variable max_rho_`var' "The rho using `Ecriterion' criterion at d`diff'"
gen emax_rho_`var'=.
label variable emax_rho_`var' "The E using `Ecriterion' criterion at d`diff'"
capture {
if "`Ecriterion'"=="R2" {
qui bysort SP_id_`var' SP_d_`var' (SP_e_`var'): ///
replace `R2`var''=SP_rho_`var'^2*rhosign`var'-SP_rho_`var'[_n-1]^2*rhosign`var'[_n-1] ///
if SP_e_`var' > 2 & SP_d_`var'==`diff'
qui bysort SP_id_`var' SP_d_`var' (SP_e_`var'): ///
replace `eR2`var''=SP_e_`var' if `R2`var''>`R2val' & !missing(`eR2`var''[_n-1]) & SP_d_`var'==`diff'
tsset
}
else if "`Ecriterion'"=="dR2" {
qui bysort SP_id_`var' SP_d_`var' (SP_e_`var'): ///
replace `R2`var''=(SP_rho_`var'^2*rhosign`var'-SP_rho_`var'[_n-1]^2*rhosign`var'[_n-1])/SP_rho_`var'[_n-1]^2 ///
if SP_e_`var' > 2 & SP_d_`var'==`diff'
qui bysort SP_id_`var' SP_d_`var' (SP_e_`var'): ///
replace `eR2`var''=SP_e_`var' if `R2`var''>`dR2val' & !missing(`eR2`var''[_n-1]) & SP_d_`var'==`diff'
tsset
}
}
foreach id of numlist `panelrange' {
capture {
qui summarize `eR2`var'' if SP_id_`var'==`id' & SP_d_`var'==`diff', meanonly
sca `Levelslove'=r(max)
qui levelsof `obsnO' if `eR2`var'' == `Levelslove' & SP_id_`var'==`id' & SP_d_`var'==`diff'
loc emax`var'`id'=`eR2`var''[`r(levels)']
qui replace max_rho_`var' = SP_rho_`var' if SP_id_`var'==`id' & SP_d_`var'==`diff' & SP_e_`var'==`eR2`var''[`r(levels)']
qui replace emax_rho_`var' = SP_e_`var' if SP_id_`var'==`id' & SP_d_`var'==`diff' & SP_e_`var'==`eR2`var''[`r(levels)']
}
}
}
}

* Fourth case for selecting E is proportional change in MAE (dmae)
else if "`Ecriterion'" == "dmae" {
qui gen long `obsnO'=_n
foreach var of varlist `variables' {
qui gen dmae`var'=.
qui gen emae`var'=.
replace emae`var'=2 if SP_e_`var'==2
qui gen rhosign`var'=1
qui replace rhosign`var'=-1 if SP_rho_`var'<0
gen max_mae_`var'=.
label variable max_mae_`var' "The mae using `Ecriterion' criterion at d`diff'"
gen emax_mae_`var'=.
label variable emax_mae_`var' "The E using `Ecriterion' criterion at d`diff'"

qui bysort SP_id_`var' SP_d_`var' (SP_e_`var'): ///
replace dmae`var'=(SP_mae_`var'[_n-1]*rhosign`var'[_n-1]-SP_mae_`var'*rhosign`var')/(SP_mae_`var'[_n-1]*rhosign`var'[_n-1]) ///
if SP_e_`var' > 2 & SP_d_`var'==`diff'
qui bysort SP_id_`var' SP_d_`var' (SP_e_`var'): ///
replace emae`var'=SP_e_`var' if dmae`var'>`dmaeval' & !missing(emae`var'[_n-1]) & SP_d_`var'==`diff'
tsset

foreach id of numlist `panelrange' {
qui summarize emae`var' if SP_id_`var'==`id' & SP_d_`var'==`diff', meanonly
sca `Levelslove'=r(max)
qui levelsof `obsnO' if emae`var' == `Levelslove' & SP_id_`var'==`id' & SP_d_`var'==`diff'
loc emax`var'`id'=emae`var'[`r(levels)']
qui replace max_mae_`var' = SP_mae_`var' if SP_id_`var'==`id' & SP_d_`var'==`diff' & SP_e_`var'==emae`var'[`r(levels)']
qui replace emax_mae_`var' = SP_e_`var' if SP_id_`var'==`id' & SP_d_`var'==`diff' & SP_e_`var'==emae`var'[`r(levels)']
}
}
}
restore




* Create variables to allow dropping zeros given the selected E
if "`dropzeros'"=="yes" {
foreach var of varlist `variables' {
tempvar `var'_nonzerouse `var'_nonzero
qui gen ``var'_nonzerouse' = .
qui gen ``var'_nonzero' = -1
qui bysort `panelIDvar' (`timevar'): replace ``var'_nonzero' = 0 if `var' == 0
qui bysort `panelIDvar' (`timevar'): replace ``var'_nonzero' = sum(``var'_nonzero')
sort `panelIDvar' `timevar'
foreach id of numlist `panelrange' {
qui by `panelIDvar' (`timevar'): replace ``var'_nonzerouse' = 1 if ``var'_nonzero'[_n] < ``var'_nonzero'[_n-`emax`var'`id''-`diff']
}
tsset
loc `var'dropzes = "& \`\`var'_nonzerouse' == 1"
loc `var'dropzes1 = "& \`\`var1'_nonzerouse' == 1"
}
}
else {
foreach var of varlist `variables' {
loc `var'dropzes = ""
loc `var'dropzes1 = ""
}
}




* SP Prediction Tests using `emax`var'`id''
if "`SPtest'" == "yes" {
tsset
gen `randoM'=.
gen `newtimE'=.
foreach var of varlist `variables' {
tempname SPtestl`var'
mata: `SPtestl`var''=J(0,6,.)
tempvar d`diff'`var' r`var'
gen `d`diff'`var''=d`diff'.`var'
gen `r`var''=.
foreach i of numlist 1/`SPtestreps' {
bysort `panelIDvar': replace `randoM'=uniform()
if "`dropzeros_SP'"=="yes" {
bysort `panelIDvar' (``var'_nonzeroSPuse' `randoM'): replace `newtimE'=_n if ``var'_nonzeroSPuse'==1
bysort `panelIDvar' (``var'_nonzeroSPuse' `timevar'): replace `r`var''=`d`diff'`var''[`newtimE'] if ``var'_nonzeroSPuse'==1
tsset
}
else {
bysort `panelIDvar' (`randoM'): replace `newtimE'=_n
bysort `panelIDvar' (`timevar'): replace `r`var''=`d`diff'`var''[`newtimE']
tsset
}
foreach id of numlist `panelrange' {
if "`emax`var'`id''"!="" {
if `emax`var'`id''>0 {
di "----------------------------------------------"
di "Below VAR = `var', Rep == `i' of `SPtestreps'"
di "----------------------------------------------"
capture {
edm explore `prestd'`r`var'' if `panelvar'==`id' ``var'dropzes', e(`emax`var'`id'') full `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' reportrawe `options'
}
capture {
mata: `Clong'=`id'
mata: `Nlong'=st_numscalar("e(N)")
mata: `Blong'=st_matrix("e(explore_result)")
mata: `Elong'=`Clong'\ `Blong''\ `Nlong'
mata: `SPtestl`var''=`SPtestl`var''\ `Elong''
}
}
}
}
}
capture quietly {
getmata (SPtestl`var'*)=`SPtestl`var'', force double
label variable SPtestl`var'1 "Simplex Projection test (all reps), panel ID = 1"
rename SPtestl`var'1 SPtestl_id_`var'
label variable SPtestl`var'2 "Simplex Projection test (all reps), E value"
rename SPtestl`var'2 SPtestl_e_`var'
recast int SPtestl_e_`var'
drop SPtestl`var'3
label variable SPtestl`var'4 "Simplex Projection test (all reps), rho"
rename SPtestl`var'4 SPtestl_rho_`var'
label variable SPtestl`var'5 "Simplex Projection test (all reps), MAE"
rename SPtestl`var'5 SPtestl_mae_`var'
label variable SPtestl`var'6 "Simplex Projection test (all reps), sample size as usable observations- max E"
rename SPtestl`var'6 SPtestl_N_`var'
drop `d`diff'`var'' `r`var''
}
}
drop `randoM' `newtimE'
}


* Hypothesis testing calculations and plots
if "`SPtestplot'" == "yes" {
preserve
if "`testval'"=="rho" loc pctiles = "50, 95, 99"
else if "`testval'"=="mae" loc pctiles = "1, 5, 50"
foreach var of varlist `variables' {
tempname SPpct`var'
mata: `SPpct`var'' = J(0,5,.)
loc `var'counter=0
loc `var't95counter=0
loc `var't99counter=0
foreach id of numlist `panelrange' {
if "`emax`var'`id''"!="" {
if `emax`var'`id''>0 {
loc `var'counter=``var'counter'+1
qui edm explore `prestd'd`diff'.`var' if `panelvar'==`id' ``var'dropzes', e(`emax`var'`id'') full `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' reportrawe `options'
mat `Amat' = e(explore_result)
if "`testval'"=="rho" sca `Test1' = `Amat'[1,3]
else if "`testval'"=="mae" sca `Test1' = `Amat'[1,4]
capture {
_pctile SPtestl_`testval'_`var' if SPtestl_id_`var'==`id', percentiles(`pctiles')
if "`testval'"=="rho" & `Test1'-r(r2) > 0 loc `var't95counter=``var't95counter'+1
if "`testval'"=="rho" & `Test1'-r(r3) > 0 loc `var't99counter=``var't99counter'+1
if "`testval'"=="mae" & `Test1'-r(r2) < 0 loc `var't95counter=``var't95counter'+1
if "`testval'"=="mae" & `Test1'-r(r1) < 0 loc `var't99counter=``var't99counter'+1
mata: `Test1'=st_numscalar("`Test1'")
mata: `A' = J(1,4,`id')
mata: `B' = (`pctiles', 110)
mata: `C' = (st_numscalar("r(r1)"), st_numscalar("r(r2)"), st_numscalar("r(r3)"), `Test1')
mata: `C2' = (`Test1'-st_numscalar("r(r1)"), `Test1'-st_numscalar("r(r2)"), `Test1'-st_numscalar("r(r3)"), 0)
mata: `D' = J(1,4,st_numscalar("e(N)"))
mata: `E' = `A'\ `B'\ `C'\ `C2'\ `D'
mata: `SPpct`var''=`SPpct`var''\ `E''
}
}
}
}
capture quietly {
getmata (SPpct`var'*)=`SPpct`var'', force double
label variable SPpct`var'1 "SP Percentile panel ID of 1"
rename SPpct`var'1 SPpct`var'
label variable SPpct`var'2 "SP Percentile"
rename SPpct`var'2 SPpct`var'_pctile
label variable SPpct`var'3 "SP test values"
rename SPpct`var'3 SPpct`var'_vals
label variable SPpct`var'4 "SP test Values Minus Observed"
rename SPpct`var'4 SPpct`var'_valsc
label variable SPpct`var'5 "SP Percentile N"
rename SPpct`var'5 SPpct`var'_N
}
capture {
if "`testval'"=="rho" loc fivepct = 95
else if "`testval'"=="mae" loc fivepct = 5
if "`testval'"=="rho" loc onepct = 99
else if "`testval'"=="mae" loc onepct = 1
if "`testval'"=="rho" loc onesuffix = "th"
else if "`testval'"=="mae" loc onesuffix = "st"
if "`testval'" == "rho" loc z="&rho"
else if "`testval'" == "mae" loc z="stSerif:MAE"

loc pct95rnd = round(100*``var't95counter'/``var'counter', 1)
loc pct95rnd2: di %3.0f `pct95rnd'
loc pct99rnd = round(100*``var't99counter'/``var'counter', 1)
loc pct99rnd2: di %3.0f `pct99rnd'

sum SPpct`var'_vals if SPpct`var'_pctile==110, meanonly
loc testvalGM=r(mean)
loc testvalGMr=round(`testvalGM',.001)
loc testvalGMd: di %8.3f `testvalGMr'

sum SPpct`var'_vals if SPpct`var'_pctile==`onepct', meanonly
loc onepctGM=r(mean)
loc onepctGMr=round(`onepctGM',.001)
loc onepctGMd: di %8.3f `onepctGMr'

sum SPpct`var'_vals if SPpct`var'_pctile==`fivepct' , meanonly
loc fivepctGM=r(mean)
loc fivepctGMr=round(`fivepctGM',.001)
loc fivepctGMd: di %8.3f `fivepctGMr'

sum SPpct`var'_vals if SPpct`var'_pctile==50, meanonly
loc nullmeanGM=r(mean)
loc nullmeanGMr=round(`nullmeanGM',.001)
loc nullmeanGMd: di %6.3f `nullmeanGMr'


if "`testval'" == "rho" loc notess=`"note("{stSerif:Note 1: y=0 is permuted} {&rho}{stSerif:, reject H0 if the 99th and/or 95th percentiles are positive}" "{stSerif:Note 2: Of ``var'counter' panel IDs, ``var't95counter' (i.e., `pct95rnd2'%) show p < .05, and ``var't99counter' (i.e., `pct99rnd2'%) show p < .01}" "{stSerif:Note 3: Raw grand-means are: Observed} {&rho} {stSerif:= `testvalGMd'; `onepct'% = `onepctGMd'; `fivepct'% = `fivepctGMd'; Null mean = `nullmeanGMd'}")"'
else if "`testval'" == "mae" loc notess=`"note("{stSerif:Note 1: y=0 is permuted MAE, reject H0 if the 1st and/or 5th percentiles are negative}" "{stSerif:Note 2: Of ``var'counter' panel IDs, ``var't95counter' (i.e., `pct95rnd2'%) show p < .05, and ``var't99counter' (i.e., `pct99rnd2'%) show p < .01}" "{stSerif:Note 3: Raw grand-means are: Observed MAE = `testvalGMd'; `onepct'% = `onepctGMd'; `fivepct'% = `fivepctGMd'; Null mean = `nullmeanGMd'}")"'

if "`SPsave'" == "yes" loc savers1="saving(SP_test_`var'_`testval'1_d`diff', replace)"
if "`SPsave'" == "yes" loc savers2="saving(SP_test_`var'_`testval'2_d`diff', replace)"

tempvar titled`var'
gen `titled`var'' = ""
label variable `titled`var'' "Percentiles of Permuted Null Distribution"
replace `titled`var''="Permuted Mean Diff" if SPpct`var'_pctile==50
replace `titled`var''="`fivepct'th Pctile Diff" if SPpct`var'_pctile==`fivepct'
replace `titled`var''="`onepct'`onesuffix' Pctile Diff" if SPpct`var'_pctile==`onepct'

qui labmask SPpct`var'_pctile, values(`titled`var'')

twoway (scatter SPpct`var'_valsc SPpct`var' if SPpct`var'_pctile==50) ///
(scatter SPpct`var'_valsc SPpct`var' if SPpct`var'_pctile==`fivepct') ///
(scatter SPpct`var'_valsc SPpct`var' if SPpct`var'_pctile==`onepct'), ///
legend(label(1 {stSerif:Permuted Mean Diff}) label(2 {stSerif:`fivepct'th Pctile Diff}) label(3 {stSerif:`onepct'`onesuffix' Pctile Diff})) ///
title({stSerif:SP: `var' Confidence Intervals Centered on Permuted} {`z'}, color(black)) ytitle({stSerif:Prediction Accuracy} {`z'}) ///
/*xlab(`panelrange')*/ name(SP_test_`var'_`testval'1_d`diff', replace) yline(0) `notess' graphregion(color(white)) `savers1' xline(`panelrange', lwidth(vthin) lpattern(dash))

if "`exportplot'" != "" {
graph export SP_test_`var'_`testval'1_d`diff'.`exportplot', replace
}
serset clear

lgraph SPpct`var'_valsc SPpct`var' SPpct`var'_pctile if SPpct`var'_pctile!=110, graphregion(color(white)) ///
yline(0) ytitle({stSerif:Prediction Accuracy} {`z'}) title({stSerif:SP: `var' Confidence Intervals Centered on Permuted} {`z'}, color(black)) ///
/*xlab(`panelrange')*/  name(SP_test_`var'_`testval'2_d`diff', replace)  `notess' `savers2' xline(`panelrange', lwidth(vthin) lpattern(dot))

if "`exportplot'" != "" {
graph export SP_test_`var'_`testval'2_d`diff'.`exportplot', replace
}
serset clear
}
}
restore
}


clear mata
serset clear

* "**************************************"
* "*                                    *"
* "*  Time of Prediction (TP) Analysis  *"
* "*                                    *"
* "**************************************"
* "Note 1: Predictability at different prediction steps (tp) into the future using simplex projections"
* "Note 2: plots and results saved as variables are named TP_'par'_'var'"
* "'par' is parameter name (e.g., rho, mae, etc.) and 'var' is variable name"
if "`TPrun'"=="yes" {
if "`TPsampler'" == "full" {
loc TPreps = 2
loc TPsampler = "rep"
loc fully = "full" 
}
foreach var of varlist `variables' {
tempname TP`var' TPlong`var'
mata: `TP`var''=J(0,9,.)
mata: `TPlong`var''=J(0,7,.)
foreach id of numlist `panelrange' {
if "`emax`var'`id''"!="" {
if `emax`var'`id''>0 {
mata: `C'=`id'
mata: `Clong'=J(1,`TPreps',`id')
forval i = 1/`TPmax' {
di "--------------------------------"
di "Below VAR = `var', tp==`i'"
di "--------------------------------"
capture noisily {
edm explore `prestd'd`diff'.`var' if `panelvar'==`id' ``var'dropzes', e(`emax`var'`id'') `fully' `TPsampler'(`TPreps') tp(`i') `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' reportrawe `options'
}
capture {
mata: `D'=`i'
mata: `N'=st_numscalar("e(N)")
mata: `B'=st_matrix("e(summary)")
mata: `E'=`C'\ `D'\ `B''\ `N'
mata: `TP`var''=`TP`var''\ `E''
}
capture {
mata: `Dlong'=J(1,`TPreps',`i')
mata: `Nlong'=J(1,`TPreps',st_numscalar("e(N)"))
mata: `Blong'=st_matrix("e(explore_result)")
mata: `Elong'=`Clong'\ `Dlong'\ `Blong''\ `Nlong'
mata: `TPlong`var''=`TPlong`var''\ `Elong''
}
}
}
}
}
capture quietly {
getmata (TP`var'*)=`TP`var'', force double
label variable TP`var'1 "Test of prediction tp steps ahead (TP), ID = 1"
rename TP`var'1 TP_id_`var'
label variable TP`var'2 "tp step-ahead value from 1 to `TPmax'"
rename TP`var'2 TP_tp_`var'
recast int TP_tp_`var'
label variable TP`var'3 "tp E value (max rho from original SP)"
rename TP`var'3 TP_e_`var'
recast int TP_e_`var'
drop TP`var'4
label variable TP`var'5 "tp average rho (`TPreps' `TPsampler's)"
rename TP`var'5 TP_rho_`var'
label variable TP`var'6 "to rho SD (`TPreps' `TPsampler's)"
rename TP`var'6 TP_rhoSD_`var'
label variable TP`var'7 "tp average MAE (`TPreps' `TPsampler's)"
rename TP`var'7 TP_mae_`var'
label variable TP`var'8 "tp MAE SD (`TPreps' `TPsampler's)"
rename TP`var'8 TP_maeSD_`var'
label variable TP`var'9 "tp sample size as usable observations (to form E-dimensional vectors) - E"
rename TP`var'9 TP_N_`var'

getmata (TPlong`var'*)=`TPlong`var'', force double
label variable TPlong`var'1 "Simplex Projection long (all `TPsampler's), panel ID = 1"
rename TPlong`var'1 TPlong_id_`var'
label variable TPlong`var'2 "tp step-ahead value long (all `TPsampler's), from 1 to `TPmax'"
rename TPlong`var'2 TPlong_tp_`var'
recast int TPlong_tp_`var'
label variable TPlong`var'3 "Simplex Projection long (all `TPsampler's), E value"
rename TPlong`var'3 TPlong_e_`var'
recast int TPlong_e_`var'
drop TPlong`var'4
label variable TPlong`var'5 "Simplex Projection long (all `TPsampler's), rho"
rename TPlong`var'5 TPlong_rho_`var'
label variable TPlong`var'6 "Simplex Projection long (all `TPsampler's), MAE"
rename TPlong`var'6 TPlong_mae_`var'
label variable TPlong`var'7 "Simplex Projection long (all `TPsampler's), sample size as usable observations - max E"
rename TPlong`var'7 TPlong_N_`var'
}
}
}


clear mata 


* Prediction step (TP) plots
if "`TPgraph'"=="yes" & "`plots_on'" =="yes" {
* Generate grand-mean as a separate panelvar ID
preserve
foreach var of varlist `variables' {
* Confidence intervals for full plots
if `CIs'==95 | `CIs'==90 loc a="errortype(ci(`CIs'))"
else loc a=""
* Use rho or MAE labeling scheme
if "`useresults'" == "rho" loc z="&rho"
else if "`useresults'" == "mae" loc z="stSerif:MAE"
* Set scale for rho and standardized MAE results
if "`useresults'" == "rho" loc ylabelscale="ylabel(-.25(.25)1) yscale(range(-.2 1))"
else if "`useresults'" == "mae" & "`prestd'" == "z." loc ylabelscale="ylabel(-.25(.25)1) yscale(range(-.2 1))"
else loc ylabelscale=""
* Saving results
if "`TPsave'"=="yes" loc b="saving(TP_`var'_`useresults'_d`diff', replace)" 
else loc b=""

* Plot command
lgraph TPlong_`useresults'_`var' TPlong_tp_`var' TPlong_id_`var', ///
`a' `b' separate(.001) fit(`TPlinefit') `ylabelscale' ///
title({stSerif:TP: Results for `var'}, color(black)) ///
ytitle({stSerif:Prediction Accuracy} {`z'}) xtitle({stSerif:Prediction Step in Future tp}) ///
note({stSerif:Note: Time of prediction (tp) grand-mean line is bolded -- differencing at d`diff'}) ///
loptions(1 lwidth(*6)) foptions(1 lwidth(*6)) graphregion(color(white)) bgcolor(white) ///
name(TP_`var'_`useresults'_d`diff', replace) nolegend xlabel(1(1)`TPmax')

if "`exportplot'" != "" {
graph export TP_`var'_`useresults'_d`diff'.`exportplot', replace
}
serset clear
}
restore
}

serset clear
clear mata


* "*******************************************"
* "*                                         *"
* "*  S-Mapping (SMAP) Test of Nonlinearity  *"
* "*                                         *"
* "*******************************************"
* "Note 1: As theta increases from 0, estimation is more state-dependent (i.e.. nonlinear)"
* "Note 2: Plots and results saved as variables named SMAP_'par'_'var'"
* "The 'par' is parameter name (e.g., rho, mae, etc.) and 'var' is variable name"
if "`SMAPrun'"=="yes" {
if "`SMAPsampler'" == "full" {
loc SMAPreps = 2
loc SMAPsampler = "rep"
loc fully = "full" 
}
local count_theta: word count `theta' 
foreach var of varlist `variables' {
tempname SMAP`var' SMAPlong`var'
mata: `SMAP`var''=J(0,7,.)
mata: `SMAPlong`var''=J(0,5,.)
foreach id of numlist `panelrange' {
if "`emax`var'`id''"!="" {
if `emax`var'`id''>0 {
mata: `C'=J(1,`count_theta',`id')
loc newSMAPreps = `SMAPreps'*`count_theta'
mata: `Clong'=J(1,`newSMAPreps',`id')
di "---------------------------------"
di "Below VAR = `var', SMAP"
di "---------------------------------"
capture noisily {
edm explore `prestd'd`diff'.`var' if `panelvar'==`id' ``var'dropzes', e(`emax`var'`id'') `fully' `SMAPsampler'(`SMAPreps') alg(smap) theta(`theta') k(`kval') `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' reportrawe `options'
}
capture {
mata: `B'=st_matrix("e(summary)")
mata: `Blong'=st_matrix("e(explore_result)")
mata: `E'=`C'\ `B''
mata: `Elong'=`Clong'\ `Blong''
mata: `SMAP`var''=`SMAP`var''\ `E''
mata: `SMAPlong`var''=`SMAPlong`var''\ `Elong''
}
}
}
}
capture quietly {
getmata (SMAP`var'*)=`SMAP`var'', force double
label variable SMAP`var'1 "SMAP results, ID = 1"
rename SMAP`var'1 SMAP_id_`var'
label variable SMAP`var'2 "SMAP E value (max rho from original SP)"
rename SMAP`var'2 SMAP_e_`var'
recast int SMAP_e_`var'
label variable SMAP`var'3 "SMAP theta value"
rename SMAP`var'3 SMAP_theta_`var'
label variable SMAP`var'4 "SMAP average rho (`SMAPreps' `SMAPsampler's)"
rename SMAP`var'4 SMAP_rho_`var'
label variable SMAP`var'5 "SMAP rho SD (`SMAPreps' `SMAPsampler's)"
rename SMAP`var'5 SMAP_rhoSD_`var'
label variable SMAP`var'6 "SMAP average MAE (`SMAPreps' `SMAPsampler's)"
rename SMAP`var'6 SMAP_mae_`var'
label variable SMAP`var'7 "SMAP MAE SD (`SMAPreps' `SMAPsampler's)"
rename SMAP`var'7 SMAP_maeSD_`var'

getmata (SMAPlong`var'*)=`SMAPlong`var'', force double
label variable SMAPlong`var'1 "SMAP long results, ID = 1"
rename SMAPlong`var'1 SMAPlong_id_`var'
label variable SMAPlong`var'2 "SMAP long E value (max rho from original SP)"
rename SMAPlong`var'2 SMAPlong_e_`var'
recast int SMAPlong_e_`var'
label variable SMAPlong`var'3 "SMAP long theta value"
rename SMAPlong`var'3 SMAPlong_theta_`var'
label variable SMAPlong`var'4 "SMAP long rho (`SMAPreps' `SMAPsampler's)"
rename SMAPlong`var'4 SMAPlong_rho_`var'
label variable SMAPlong`var'5 "SMAP long mae (`SMAPreps' `SMAPsampler's)"
rename SMAPlong`var'5 SMAPlong_mae_`var'
}
}
}


clear mata


* S-map analysis plots
if "`SMAPgraph'"=="yes" & "`plots_on'" =="yes" {
* Generate grand-mean as a separate panelvar ID
preserve
foreach var of varlist `variables' {
* Confidence intervals for full plots
if `CIs'==95 | `CIs'==90 loc a="errortype(ci(`CIs'))"
else loc a=""
* Use rho or MAE labeling scheme
if "`useresults'" == "rho" loc z="&rho"
else if "`useresults'" == "mae" loc z="stSerif:MAE"
* Set scale for rho and standardized MAE results
if "`useresults'" == "rho" loc ylabelscale="ylabel(-.25(.25)1) yscale(range(-.2 1))"
else if "`useresults'" == "mae" & "`prestd'" == "z." loc ylabelscale="ylabel(-.25(.25)1) yscale(range(-.2 1))"
else loc ylabelscale=""
* Saving results
if "`SMAPsave'"=="yes" loc b="saving(SMAP_`var'_`useresults'_d`diff', replace)" 
else loc b=""

* Plot command
lgraph SMAPlong_`useresults'_`var' SMAPlong_theta_`var' SMAPlong_id_`var', ///
`a' `b' separate(.001) fit(`SMAPlinefit') `ylabelscale' ///
title({stSerif:SMAP: Results for `var'}, color(black)) ///
ytitle({stSerif:Prediction Accuracy} {`z'}) xtitle({stSerif:Degree of State-Dependence or Nonlinearity {&theta}}) ///
note({stSerif:Note: Grand-mean line is bolded and greater theta means greater nonlinearity -- differencing at d`diff'}) ///
loptions(1 lwidth(*6)) foptions(1 lwidth(*6)) graphregion(color(white)) bgcolor(white) ///
name(SMAP_`var'_`useresults'_d`diff', replace) nolegend

if "`exportplot'" != "" {
graph export SMAP_`var'_`useresults'_d`diff'.`exportplot', replace
}
serset clear
}
restore
}

serset clear
clear mata


* SMAP Prediction Tests using `emax`var'`id''
if "`SMAPtest'"=="yes" {
tsset
* Identify theta > 0 with best predictions
qui gen long `obsnO'=_n
foreach var of varlist `variables' {
foreach id of numlist `panelrange' {
capture {
quietly summarize SMAP_`testval'_`var' if SMAP_id_`var'==`id' & SMAP_theta_`var' > 0, meanonly
if "`testval'"=="rho" loc a="max"
else if "`testval'"=="mae" loc a="min"
sca `Levelslove'=r(`a')
qui levelsof `obsnO' if SMAP_`testval'_`var' == `Levelslove' & SMAP_id_`var'==`id'
tempname theta`var'`id'
sca `theta`var'`id''=SMAP_theta_`var'[`r(levels)']
}
}
}
drop `obsnO'

* Run SMAPs with theta = 0 and theta = `theta`var'`id'' if needed 
foreach var of varlist `variables' {
	tempname SMAPtestl`var'
	mata: `SMAPtestl`var''=J(0,6,.)
	foreach id of numlist `panelrange' {
		capture confirm scalar `theta`var'`id''
		if _rc==0 & "`emax`var'`id''"!="" {
			if `emax`var'`id''>0 {
				foreach i of numlist 0 `=`theta`var'`id''' {
					di "-------------------------------------------------------------------------------"
					di "S-map test theta==`i', Variable == `var', SMAP test"
					di "-------------------------------------------------------------------------------"
					capture {
						qui edm explore `prestd'd`diff'.`var' if `panelvar'==`id' ``var'dropzes', e(`emax`var'`id'') `smapseed' alg(smap) rep(`SMAPtestreps') theta(`i') k(`kval') `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' reportrawe `options'
					}
					capture {
						mata: `Blong'=st_matrix("e(explore_result)")
						mata: `Clong'=J(1,`SMAPtestreps',`id')
						mata: `Dlong'=(1..`SMAPtestreps')
						mata: `Elong'=`Clong'\ `Dlong'\ `Blong''
						mata: `SMAPtestl`var''=`SMAPtestl`var''\ `Elong''
					}
				}
			}
		}
	}
capture quietly {
getmata (SMAPtestl`var'*)=`SMAPtestl`var'', force double
label variable SMAPtestl`var'1 "SMAP long test results, ID = 1"
rename SMAPtestl`var'1 SMAPtestl_id_`var'
recast long SMAPtestl_id_`var'
label variable SMAPtestl`var'2 "SMAP long test results, paired replication number"
rename SMAPtestl`var'2 SMAPtestl_rep_`var'
recast long SMAPtestl_rep_`var'
label variable SMAPtestl`var'3 "SMAP long test E value (optimal E from original SP)"
rename SMAPtestl`var'3 SMAPtestl_e_`var'
recast int SMAPtestl_e_`var'
label variable SMAPtestl`var'4 "SMAP long test theta value (optimal theta > 0 given `testval')"
rename SMAPtestl`var'4 SMAPtestl_theta_`var'
recast float SMAPtestl_theta_`var'
label variable SMAPtestl`var'5 "SMAP long test rho (`SMAPreps' reps)"
rename SMAPtestl`var'5 SMAPtestl_rho_`var'
label variable SMAPtestl`var'6 "SMAP long test mae (`SMAPreps' reps)"
rename SMAPtestl`var'6 SMAPtestl_mae_`var'
}
}
}

* SMAP hypothesis testing calculations and plots
if "`SMAPtestplot'" == "yes" {
* Identify theta > 0 with best predictions
qui gen long `obsnO'=_n
foreach var of varlist `variables' {
foreach id of numlist `panelrange' {
capture {
quietly summarize SMAP_`testval'_`var' if SMAP_id_`var'==`id' & SMAP_theta_`var' > 0, meanonly
if "`testval'"=="rho" loc a="max"
else if "`testval'"=="mae" loc a="min"
sca `Levelslove'=r(`a')
qui levelsof `obsnO' if SMAP_`testval'_`var' == `Levelslove' & SMAP_id_`var'==`id'
tempname theta`var'`id'
loc besttheta`var'=SMAP_theta_`var'[`r(levels)']
}
}
}
drop `obsnO'

preserve
if "`testval'"=="rho" loc pctiles = "1, 5, 50"
else if "`testval'"=="mae" loc pctiles = "50, 95, 99"
foreach var of varlist `variables' {
loc `var'counter=0
loc `var't95counter=0
loc `var't99counter=0
tempname SMAPpct`var'
tempvar SMAPtestval`var'
mata: `SMAPpct`var'' = J(0,3,.)
gen `SMAPtestval`var''=.
capture {
qui bysort SMAPtestl_id_`var' SMAPtestl_rep_`var' (SMAPtestl_theta_`var'): replace `SMAPtestval`var''=SMAPtestl_`testval'_`var'-SMAPtestl_`testval'_`var'[_n-1] if SMAPtestl_theta_`var' > 0 & SMAPtestl_theta_`var'[_n-1]==0 & SMAPtestl_theta_`var'!=.
}
foreach id of numlist `panelrange' {
loc `var'counter=``var'counter'+1
capture {
_pctile `SMAPtestval`var'' if SMAPtestl_id_`var'==`id', percentiles(`pctiles')
if "`testval'"=="rho" & r(r1) > 0 loc `var't99counter=``var't99counter'+1
if "`testval'"=="rho" & r(r2) > 0 loc `var't95counter=``var't95counter'+1
if "`testval'"=="mae" & r(r2) < 0 loc `var't95counter=``var't95counter'+1
if "`testval'"=="mae" & r(r3) < 0 loc `var't99counter=``var't99counter'+1
mata: `A' = J(1,3,`id')
mata: `B' = (`pctiles')
mata: `C' = (st_numscalar("r(r1)"), st_numscalar("r(r2)"), st_numscalar("r(r3)"))
mata: `E' = `A'\ `B'\ `C'
mata: `SMAPpct`var''=`SMAPpct`var''\ `E''
}
}
drop `SMAPtestval`var''
capture quietly {
getmata (SMAPpct`var'*)=`SMAPpct`var'', force double
label variable SMAPpct`var'1 "SMAP Percentile panel ID of 1"
rename SMAPpct`var'1 SMAPpct`var'
label variable SMAPpct`var'2 "SMAP Percentile"
rename SMAPpct`var'2 SMAPpct`var'_pctile
label variable SMAPpct`var'3 "SMAP test values"
rename SMAPpct`var'3 SMAPpct`var'_vals
}
capture {
if "`testval'"=="mae" loc fivepct = 95
else if "`testval'"=="rho" loc fivepct = 5
if "`testval'"=="mae" loc onepct = 99
else if "`testval'"=="rho" loc onepct = 1
if "`testval'"=="mae" loc onesuffix = "th"
else if "`testval'"=="rho" loc onesuffix = "st"
if "`testval'" == "rho" loc z="&rho"
else if "`testval'" == "mae" loc z="stSerif:MAE"

loc pct95rnd = round(100*``var't95counter'/``var'counter', 1)
loc pct95rnd2: di %3.0f `pct95rnd'
loc pct99rnd = round(100*``var't99counter'/``var'counter', 1)
loc pct99rnd2: di %3.0f `pct99rnd'

sum SMAPpct`var'_vals if SMAPpct`var'_pctile==`onepct', meanonly
loc onepctGM=r(mean)
loc onepctGMr=round(`onepctGM',.0001)
loc onepctGMd: di %9.4f `onepctGMr'

sum SMAPpct`var'_vals if SMAPpct`var'_pctile==`fivepct' , meanonly
loc fivepctGM=r(mean)
loc fivepctGMr=round(`fivepctGM',.0001)
loc fivepctGMd: di %9.4f `fivepctGMr'

sum SMAPpct`var'_vals if SMAPpct`var'_pctile==50, meanonly
loc nullmeanGM=r(mean)
loc nullmeanGMr=round(`nullmeanGM',.0001)
loc nullmeanGMd: di %9.4f `nullmeanGMr'

if "`testval'" == "rho" loc notess=`"note("{stSerif:Note 1:} {&rho} {stSerif:at best theta > 0, here `besttheta`var'', minus} {&rho} {stSerif:at theta = 0, positive values support nonlinearity}" "{stSerif:Note 2: Of ``var'counter' panel IDs, ``var't95counter' (i.e., `pct95rnd2'%) show p < .05, and ``var't99counter' (i.e., `pct99rnd2'%) show p < .01}" "{stSerif:Note 3: Grand-means of differences are: `onepct'% = `onepctGMd'; `fivepct'% = `fivepctGMd'; Mean = `nullmeanGMd'}" "{stSerif:Note 4: Missing percentiles implies manifold reconstruction problems (e.g., missing data or many zeros)}")"'
else if "`testval'" == "mae" loc notess=`"note("{stSerif:Note 1: MAE at best theta > 0 minus MAE at theta = 0, negative values support nonlinearity}" "{stSerif:Note 2: Of ``var'counter' panel IDs, ``var't95counter' (i.e., `pct95rnd2'%) show p < .05, and ``var't99counter' (i.e., `pct99rnd2'%) show p < .01}" "{stSerif:Note 3: Grand-means of differences are: `onepct'% = `onepctGMd'; `fivepct'% = `fivepctGMd'; Mean = `nullmeanGMd'}" "{stSerif:Note 4: Missing percentiles implies manifold reconstruction problems (e.g., missing data or many zeros)}")"'

if "`SMAPsave'" == "yes" loc savers1="saving(SMAPtest_`var'_`testval'1_d`diff', replace)"
if "`SMAPsave'" == "yes" loc savers2="saving(SMAPtest_`var'_`testval'2_d`diff', replace)"

tempvar titled`var'
gen `titled`var'' = ""
label variable `titled`var'' "Percentiles of Difference: (theta > 0) - (theta == 0)"
replace `titled`var''="Mean of Difference" if SMAPpct`var'_pctile==50
replace `titled`var''="`fivepct'th Percentile" if SMAPpct`var'_pctile==`fivepct'
replace `titled`var''="`onepct'`onesuffix' Percentile" if SMAPpct`var'_pctile==`onepct'
qui labmask SMAPpct`var'_pctile, values(`titled`var'')
drop `titled`var''

twoway (scatter SMAPpct`var'_vals SMAPpct`var' if SMAPpct`var'_pctile==50) ///
(scatter SMAPpct`var'_vals SMAPpct`var' if SMAPpct`var'_pctile==`fivepct') ///
(scatter SMAPpct`var'_vals SMAPpct`var' if SMAPpct`var'_pctile==`onepct'), ///
legend(label(1 {stSerif:Average Difference}) label(2 {stSerif:`fivepct'th Pctile Diff}) label(3 {stSerif:`onepct'`onesuffix' Pctile Diff})) ///
title({stSerif:SMAP: `var' Confidence Intervals of Difference in} {`z'}, color(black)) ytitle({stSerif:Prediction Accuracy} {`z'}) ///
/*xlab(`panelrange')*/ name(SMAPtest_`testval'1_`var', replace) yline(0) `notess' graphregion(color(white)) `savers1' xline(`panelrange', lwidth(vthin) lpattern(dash))

if "`exportplot'" != "" {
graph export SMAPtest_`var'_`testval'1_d`diff'.`exportplot', replace
}
serset clear

lgraph SMAPpct`var'_vals SMAPpct`var' SMAPpct`var'_pctile, graphregion(color(white)) ///
yline(0) ytitle({stSerif:Prediction Accuracy} {`z'}) title({stSerif:SMAP: `var' Confidence Interval of Difference in} {`z'}, color(black)) ///
/*xlab(`panelrange')*/  name(SMAPtest_`testval'2_`var', replace)  `notess' `savers2' xline(`panelrange', lwidth(vthin) lpattern(dot))

if "`exportplot'" != "" {
graph export SMAPtest_`var'_`testval'2_d`diff'.`exportplot', replace
}
serset clear
}
}
restore
}


clear mata
serset clear




* "*******************************************"
* "*                                         *"
* "*  Coprediciton Using Simplex Projection  *"
* "*                                         *"
* "*******************************************"
* "Note 1: Coprediction to evaluate predictability for all unique variable pairs using simplex projections"
* "Note 2: Results saved in 'Long' format where predictor and outcome coded in two variables with rho and mae in two others"
* "Note 3: Results also saved in a 'Wide' format that is essentially a correlation matrix of results
* "Note 4: Columns of these matrices index the predictor var1 used to construct the E-dimensional manifold"
* "Note 5: Rows index the predictee var2 that is predicted by the predictor var1"
* "Note 6: Thus, diagonal elements of these matrices are typical simplex projection rhos (a variable predicting itself)"
* "Note 7: panelvar ID saved as variable to tie each matrix to its panel ID"
* "Note 8: Wide format plots and results saved as variables named CoPred_'var'"
* "The 'var' is the variable name that serves as the predictor of other variables"
* "Note 9: The 'variables' list determines the Wide format outcome/predictee variable for each row of results"
* "Given the 'variables' list here, there are 'nv' unique rows of results in order of 'variables' list"
if "`COPREDrun'"=="yes" {
loc nv: word count `variables'   /* Number of variables "nv" to form matrices for coprediction results */
loc copredmatsize = `nv'+2   /* Set columns of coprediction matrix, which is nv+1 to include panel IDs as the first column */
mata: `CoPred1'=J(0,`copredmatsize',.)   /* Generate matrix to hold panel IDs and nv*nv square matrix of results */
mata: `CoPred2'=J(0,`copredmatsize',.)   /* Generate matrix to hold panel IDs and nv*nv square matrix of results */
mata: `NewMat'=J(0,5,.)
foreach id of numlist `panelrange' {
loc aa=2   /* Setup counters linked to matrix elements to contain results */
loc bb=0
loc aaa=0
mata: `A1'=J(1,`nv',`id')   /* Set first column to person ID from panelvar */
mata: `A2'=(1..`nv')
mata: `B'=J(`nv',`nv',.)   /* Create nv*nv matrix to contain each person's coprediction matrix */
mata: `C'=`A1'\ `A2'\ `B'
mata: `Copredrho'=`C''
mata: `Copredmae'=`C''
foreach var1 of varlist `variables' {
loc aa=`aa'+1   /* This is column number in coprediction matrix after panel ID, meaning the predictor var1 */
loc aaa=`aaa'+1   /* This codes for predictor in the non-matrix saved results NewMat as varlist variables order */
foreach var2 of varlist `variables' {
loc bb=`bb'+1   /* Predictee var2: 1=ihap; 2=irlx; ...; 8=iNA */
loc cc=`bb'-(`aa'-3)*`nv'   /* This is row number in coprediction matrix for predictee var2 */
di "-------------------------------------------------"
di "Below predictor==`var1', predictee==`var2'"
di "-------------------------------------------------"
capture noisily {
if "`emax`var1'`id''"!="" {
if `emax`var1'`id''>0 {
edm explore `prestd'd`diff'.`var1' if `panelvar'==`id' ``var1'dropzes1', e(`emax`var1'`id'') copredictvar(`prestd'd`diff'.`var2') copredict(`var1'_pred_`var2'_`panelvar') full `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' reportrawe `options'
}
}
}
if "`time_gap'"=="dt" {
preserve
drop if missing(`var2')
}
capture {
gen `tempdt1' = `var2'[_n+1] if `panelvar'==`id' & `panelvar'[_n+1]==`id'   /* Here is a change */
qui cor d`diff'.`tempdt1' `var1'_pred_`var2'_`panelvar' if `panelvar'==`id'   /* Here is a change */
mata: `Predrho'=st_matrix("r(C)")
mata: `Copredrho'[`cc',`aa']=`Predrho'[2,1]
}
capture {
if "`prestd'" == "z." {
qui egen `tempstD' = std(d`diff'.`tempdt1')
qui gen `absvaluE'=abs(`var1'_pred_`var2'_`panelvar'-`tempstD') if `panelvar'==`id'   /* Here is a change */
summarize `absvaluE', meanonly
mata: `Copredmae'[`cc',`aa']=st_numscalar("r(mean)")
mata: `NewMat'=`NewMat'\(`id',`aaa',`cc',`Predrho'[2,1],st_numscalar("r(mean)"))
drop `absvaluE' `tempstD' `var1'_pred_`var2'_`panelvar' `tempdt1'
}
else {
qui gen `absvaluE'=abs(`var1'_pred_`var2'_`panelvar'-d`diff'.`tempdt1') if `panelvar'==`id'   /* Here is a change */
summarize `absvaluE', meanonly
mata: `Copredmae'[`cc',`aa']=st_numscalar("r(mean)")
mata: `NewMat'=`NewMat'\(`id',`aaa',`cc',`Predrho'[2,1],st_numscalar("r(mean)"))
drop `absvaluE' `var1'_pred_`var2'_`panelvar' `tempdt1'
}
}
if "`time_gap'"=="dt" {
restore
}
}
}
mata: `CoPred1'=`CoPred1'\ `Copredrho'
mata: `CoPred2'=`CoPred2'\ `Copredmae'
}
capture quietly {
getmata (CoPred1*)=`CoPred1',force double
getmata (CoPred2*)=`CoPred2',force double
getmata (NewMat*)=`NewMat',force double
label variable CoPred11 "Coprediction, ID from `panelvar'"
label variable CoPred12 "Coprediction, predictee"
label variable NewMat1 "Coprediction ID from `panelvar'"
label variable NewMat2 "Coprediction Predictor"
label variable NewMat3 "Coprediction Predictee"
label variable NewMat4 "Coprediction rho"
label variable NewMat5 "Coprediction MAE"
drop CoPred21 CoPred22
rename CoPred11 CoPredWide
rename CoPred12 CoPredWide_Predictee
recast long CoPredWide_Predictee
rename NewMat1 CoPredLong
rename NewMat2 CoPredLong_Predictor
recast long CoPredLong_Predictor
rename NewMat3 CoPredLong_Predictee
recast long CoPredLong_Predictee
rename NewMat4 CoPredLong_rho
rename NewMat5 CoPredLong_mae
tostring CoPredWide_Predictee, generate(CoPredWide_Predictee_String)
tostring CoPredLong_Predictor, generate(CoPredLong_Predictor_String)
tostring CoPredLong_Predictee, generate(CoPredLong_Predictee_String)
}

* Replace string numeric values with predictee variable names
loc a=0
foreach var of varlist `variables' {
loc a=`a'+1
loc varnum`a' = "`var'"
}
foreach i of numlist 1/`nv' {
qui replace CoPredWide_Predictee_String="`prestd'`varnum`i''" if CoPredWide_Predictee_String=="`i'"
qui replace CoPredLong_Predictor_String="`prestd'`varnum`i''" if CoPredLong_Predictor_String=="`i'"
qui replace CoPredLong_Predictee_String="`prestd'`varnum`i''" if CoPredLong_Predictee_String=="`i'"
}
qui labmask CoPredWide_Predictee, values(CoPredWide_Predictee_String)
qui labmask CoPredLong_Predictor, values(CoPredLong_Predictor_String)
qui labmask CoPredLong_Predictee, values(CoPredLong_Predictee_String)
* Label variables with predictor names
loc a=2
foreach var of varlist `variables' {
loc a=`a'+1
loc varnum`a' = "`var'"
}
foreach i of numlist 3/`copredmatsize' {
label variable CoPred1`i' "Coprediction rho `prestd'`varnum`i'' as predictor, ID `panelvar'"
label variable CoPred2`i' "Coprediction MAE `prestd'`varnum`i'' as predictor, ID `panelvar'"
rename CoPred1`i' CoPredWide_rho_`varnum`i''
rename CoPred2`i' CoPredWide_mae_`varnum`i''
}
}


clear mata 


* Coprediction plots
if "`COPREDgraph'"=="yes" & "`plots_on'" =="yes" {
if "`COPREDvalues'" == "yes" {
loc a="values(format(%9.`COPREDdecimals'f))"
}
else loc a=""
if "`COPREDsave'"=="yes" {
loc b="saving(COPREDheatmap_`useresults'_d`diff', replace)"
loc c="saving(COPREDbar_`useresults'_d`diff'_xpredictee, replace)"
loc d="saving(COPREDbar_`useresults'_d`diff'_xpredictor, replace)"
}
else {
loc b=""
loc c=""
loc d=""
}
* Use rho or MAE labeling scheme
if "`useresults'" == "rho" loc z="&rho"
else if "`useresults'" == "mae" loc z="stSerif:MAE"
* Coprediction heatmap plot
heatplot CoPredLong_`useresults' CoPredLong_Predictee_String CoPredLong_Predictor_String, `a' `b' ///
colors(hcl, bluered reverse) discrete fast graphregion(color(white)) yscale(reverse) ///
title({stSerif:Coprediction Results Heatmap}, color(black)) name(COPREDheatmap_`useresults'_d`diff', replace)

if "`exportplot'" != "" {
graph export COPREDheatmap_`useresults'_d`diff'.`exportplot', replace
}
serset clear

* Coprediction line plots
graph bar CoPredLong_`useresults' if !missing(CoPredLong_`useresults'), `c' ///
over(CoPredLong_Predictor_String) over(CoPredLong_Predictee_String) nofill asyvars ///
title({stSerif:Coprediction Results with Predictee on x-Axis}, color(black)) ytitle({stSerif:Prediction Accuracy} {`z'}) ///
legend(subtitle("Predictor Variable")) name(COPREDbar_`useresults'_d`diff'_xpredictee, replace) ymtick(##4, grid) graphregion(color(white))

if "`exportplot'" != "" {
graph export COPREDbar_`useresults'_d`diff'_xpredictee.`exportplot', replace
}
serset clear

graph bar CoPredLong_`useresults' if !missing(CoPredLong_`useresults'), `d' ///
over(CoPredLong_Predictee_String) over(CoPredLong_Predictor_String) nofill asyvars ///
title({stSerif:Coprediction Results with Predictor on x-Axis}, color(black)) ytitle({stSerif:Prediction Accuracy} {`z'}) ///
legend(subtitle("Predictee Variable")) name(COPREDbar_`useresults'_d`diff'_xpredictor, replace) ymtick(##4, grid) graphregion(color(white))

if "`exportplot'" != "" {
graph export COPREDbar_`useresults'_d`diff'_xpredictor.`exportplot', replace
}
serset clear
}

serset clear
clear mata


* "********************************************"
* "*                                          *"
* "*  Convergent Cross Mapping (CCM) or XMAP  *"
* "*                                          *"
* "********************************************"
* "Note 1: For evaluating causal effects vs. common drivers, etc."
if "`CCMrun'"=="yes" {
foreach var1 of varlist `variables' {
foreach var2 of varlist `variables' {
if "`var1'" != "`var2'" {
tempname ID_`var1'_pred_`var2'_CCM
mata: `ID_`var1'_pred_`var2'_CCM' = J(0,6,.)
foreach id of numlist `panelrange' {
if "`emax`var1'`id''"!="" {
if `emax`var1'`id''>0 {
di "-------------------------------------------------"
di "Below CCM Cause==`var1', Outcome==`var2'"
di "-------------------------------------------------"
if "`CCMlags'"!="NA" & "`time_gap'"!="dt" {
forval z=`CCMlags'(-1)1 {
capture {
di "-------------------------------------------------"
di "Below CCM Cause==`var1', Outcome==`var2'"
di "-------------------------------------------------"
edm xmap `prestd'd`diff'.`var2' `prestd'd`diff'l`z'.`var1' if `panelvar'==`id' ``var1'dropzes1', e(`emax`var1'`id'') direction(oneway) `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' `options'
}
capture {
mata: `Z1'=st_matrix("e(xmap_1)")
mata: `Z2'=(`id')\(-`z')\ `Z1''
mata: `ID_`var1'_pred_`var2'_CCM' = `ID_`var1'_pred_`var2'_CCM'\ `Z2''
}
}
}
else if "`CCMlags'"!="NA" & "`time_gap'"=="dt" {
preserve
drop if missing(`var1') & missing(`var2')
forval z=`CCMlags'(-1)1 {
gen `var1'llag`z' = `var1'[_n-`z'] if `panelvar'==`panelvar'[_n-`z']
capture {
di "-------------------------------------------------"
di "Below CCM Cause==`var1', Outcome==`var2'"
di "-------------------------------------------------"
edm xmap `prestd'd`diff'.`var2' `prestd'd`diff'.`var1'llag`z' if `panelvar'==`id' ``var1'dropzes1', e(`emax`var1'`id'') direction(oneway) `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' `options'
}
capture {
mata: `Z1'=st_matrix("e(xmap_1)")
mata: `Z2'=(`id')\(-`z')\ `Z1''
mata: `ID_`var1'_pred_`var2'_CCM' = `ID_`var1'_pred_`var2'_CCM'\ `Z2''
}
}
restore
}
if "`time_gap'"!="dt" {
forval z=0/`CCMleads' {
capture {
di "-------------------------------------------------"
di "Below CCM Cause==`var1', Outcome==`var2'"
di "-------------------------------------------------"
edm xmap `prestd'd`diff'.`var2' `prestd'd`diff'f`z'.`var1' if `panelvar'==`id' ``var1'dropzes1', e(`emax`var1'`id'') direction(oneway) `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' `options'
}
capture {
if `z'==0 loc idNfull = e(N)
mata: `Z1'=st_matrix("e(xmap_1)")
mata: `Z2'=(`id')\(`z')\ `Z1''
mata: `ID_`var1'_pred_`var2'_CCM' = `ID_`var1'_pred_`var2'_CCM'\ `Z2''
}
}
}
else if "`time_gap'"=="dt" {
preserve
drop if missing(`var1') & missing(`var2')
forval z=0/`CCMleads' {
gen `var1'forwlag`z' = `var1'[_n+`z'] if `panelvar'==`panelvar'[_n+`z']
capture {
di "-------------------------------------------------"
di "Below CCM Cause==`var1', Outcome==`var2'"
di "-------------------------------------------------"
edm xmap `prestd'd`diff'.`var2' `prestd'd`diff'.`var1'forwlag`z' if `panelvar'==`id' ``var1'dropzes1', e(`emax`var1'`id'') direction(oneway) `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' `options'
}
capture {
if `z'==0 loc idNfull = e(N)
mata: `Z1'=st_matrix("e(xmap_1)")
mata: `Z2'=(`id')\(`z')\ `Z1''
mata: `ID_`var1'_pred_`var2'_CCM' = `ID_`var1'_pred_`var2'_CCM'\ `Z2''
}
}
restore
}
capture noisily {
loc lib11 = `emax`var1'`id''+`lib1'
di "-------------------------------------------------"
di "Below CCM Cause==`var1', Outcome==`var2'"
di "-------------------------------------------------"
edm xmap `prestd'd`diff'.`var2' `prestd'd`diff'.`var1' if `panelvar'==`id' ``var1'dropzes1', e(`emax`var1'`id'') direction(oneway) lib(`lib11') rep(`CCMreps1') `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' `options'
}
capture {
mata: `Z21' = J(1,`CCMreps1',`id')
mata: `Z22' = J(1,`CCMreps1',0)
mata: `Z1'=st_matrix("e(xmap_1)")
mata: `Z3'=`Z21'\ `Z22'\ `Z1''
mata: `ID_`var1'_pred_`var2'_CCM' = `ID_`var1'_pred_`var2'_CCM'\ `Z3''
}
capture noisily {
loc lib22 = `emax`var1'`id''+`lib2'
loc lib33 = `emax`var1'`id''+`lib3'
loc lib44 = `emax`var1'`id''+`lib4'
di "-------------------------------------------------"
di "Below CCM Cause==`var1', Outcome==`var2'"
di "-------------------------------------------------"
edm xmap `prestd'd`diff'.`var2' `prestd'd`diff'.`var1' if `panelvar'==`id' ``var1'dropzes1', e(`emax`var1'`id'') direction(oneway) lib(`lib22' `lib33' `lib44') rep(`CCMreps2') `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' `options'
}
capture {
loc matsizetemp2 = 3*`CCMreps2'
mata: `Z21' = J(1,`matsizetemp2',`id')
mata: `Z22' = J(1,`matsizetemp2',0)
mata: `Z1'=st_matrix("e(xmap_1)")
mata: `Z3'=`Z21'\ `Z22'\ `Z1''
mata: `ID_`var1'_pred_`var2'_CCM' = `ID_`var1'_pred_`var2'_CCM'\ `Z3''
}
capture noisily {
loc libthird = round(`idNfull'/`fraction1')
loc libhalf = round(`idNfull'/`fraction2')
di "-------------------------------------------------"
di "Below CCM Cause==`var1', Outcome==`var2'"
di "-------------------------------------------------"
edm xmap `prestd'd`diff'.`var2' `prestd'd`diff'.`var1' if `panelvar'==`id' ``var1'dropzes1', e(`emax`var1'`id'') direction(oneway) lib(`libthird' `libhalf') rep(`CCMreps3') `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' `options'
}
capture {
loc matsizetemp3 = 2*`CCMreps3'
mata: `Z21' = J(1,`matsizetemp3',`id')
mata: `Z22' = J(1,`matsizetemp3',0)
mata: `Z1'=st_matrix("e(xmap_1)")
mata: `Z3'=`Z21'\ `Z22'\ `Z1''
mata: `ID_`var1'_pred_`var2'_CCM' = `ID_`var1'_pred_`var2'_CCM'\ `Z3''
}
}
}
}
* Now save results to variables for plotting etc.
capture quietly {
getmata (ID_`var1'_pred_`var2'_CCM*)=`ID_`var1'_pred_`var2'_CCM', force double
label variable ID_`var1'_pred_`var2'_CCM1 "CCM `var1' causing `var2', ID = 1"
rename ID_`var1'_pred_`var2'_CCM1 CCM_id_`var1'_`var2'
label variable ID_`var1'_pred_`var2'_CCM2 "CCM `var1' causing `var2' time lag, negative values imply a regular lag and positive values imply forward stepping"
rename ID_`var1'_pred_`var2'_CCM2 CCM_lag_`var1'_`var2'
recast int CCM_lag_`var1'_`var2'
drop ID_`var1'_pred_`var2'_CCM3
label variable ID_`var1'_pred_`var2'_CCM4 "CCM `var1' causing `var2', library size"
rename ID_`var1'_pred_`var2'_CCM4 CCM_L_`var1'_`var2'
recast long CCM_L_`var1'_`var2'
label variable ID_`var1'_pred_`var2'_CCM5 "CCM `var1' causing `var2', rho"
rename ID_`var1'_pred_`var2'_CCM5 CCM_rho_`var1'_`var2'
label variable ID_`var1'_pred_`var2'_CCM6 "CCM `var1' causing `var2', MAE"
rename ID_`var1'_pred_`var2'_CCM6 CCM_mae_`var1'_`var2'
}
}
}
}
}


clear mata


* Convergent cross mapping (CCM) or xmap analysis plots
if "`CCMgraph'"=="yes" & "`plots_on'" =="yes" {
* Generate grand-mean as a separate panelvar ID
preserve
* Use rho or MAE labeling scheme
if "`useresults'" == "rho" loc z="&rho"
else if "`useresults'" == "mae" loc z="stSerif:MAE"
* Set scale for rho and standardized MAE results
if "`useresults'" == "rho" loc ylabelscale="ylabel(-.25(.25)1) yscale(range(-.2 1))"
else if "`useresults'" == "mae" & "`prestd'" == "z." loc ylabelscale="ylabel(-.25(.25)1) yscale(range(-.2 1))"
else loc ylabelscale=""
* Generate grand mean for inclusion as a note on the plots
gen long `obsnO'=_n
foreach var1 of varlist `variables' {
foreach var2 of varlist `variables' {
if "`var1'" != "`var2'" {
tempvar GMCCM`var1'`var2'
gen `GMCCM`var1'`var2''=.
foreach id of numlist `panelrange' {
capture {
quietly summarize CCM_L_`var1'_`var2' if CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`useresults'_`var1'_`var2'), meanonly
loc levelz=r(max)
qui levelsof `obsnO' if CCM_L_`var1'_`var2' == `levelz' & CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0
replace `GMCCM`var1'`var2''=CCM_`useresults'_`var1'_`var2'[`r(levels)'] if CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0
}
}
sum  `GMCCM`var1'`var2'', meanonly
loc CCMrhoz=r(mean)
loc CCMrhoz1=round(`CCMrhoz',.001)
loc CCMrhoz11: di %6.3f `CCMrhoz1'
* Saving results
if "`CCMsave'"=="yes" {
loc b="saving(CCM_`var1'_`var2'_`useresults'_l0, replace)" 
else loc b=""
}
* Plot command
lgraph CCM_`useresults'_`var1'_`var2' CCM_L_`var1'_`var2' CCM_id_`var1'_`var2' ///
if CCM_lag_`var1'_`var2'==0 & !missing(CCM_`useresults'_`var1'_`var2'), ///
/// errortype(ci(`CIs'))
`b' separate(.001) fit(`CCMlinefit') `ylabelscale' ///
title({stSerif:CCM: Results for `var1' CCM causing `var2'}, color(black)) ///
ytitle({stSerif:Prediction Accuracy} {`z'}) xtitle({stSerif:Library Size L}) ///
note({stSerif:Note: Grand-mean line is bolded -- differencing at d`diff'}) ///
loptions(1 lwidth(*6)) foptions(1 lwidth(*6)) graphregion(color(white)) bgcolor(white) ///
name(CCM_`var1'_`var2'_`useresults'_l0, replace) nolegend /// 
note({stSerif:Note: Grand Mean of} {&rho} {stSerif:at Maximum Library Size = `CCMrhoz11' -- differencing at d`diff'})


if "`exportplot'" != "" {
graph export CCM_`var1'_`var2'_`useresults'_l0.`exportplot', replace
}
serset clear
}
}
}
restore
}

serset clear
clear mata


* Convergent cross mapping (CCM) or xmap LAGGED analysis plots
if "`CCMlaggraph'"=="yes" & "`plots_on'" =="yes" {
* Generate grand-mean as a separate panelvar ID
preserve
foreach var1 of varlist `variables' {
foreach var2 of varlist `variables' {
if "`var1'" != "`var2'" {
* Find max library size L for each panel ID and each lag, so only max L results are plotted
qui gen CCM_L_`var1'_`var2'max=.
quietly levelsof CCM_lag_`var1'_`var2', local(levels)
foreach lag of local levels {
foreach id of numlist `panelrange' {
quietly summarize CCM_L_`var1'_`var2' if CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==`lag'
qui replace CCM_L_`var1'_`var2'max=r(max) if CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==`lag'
}
}
}
}
}
* Use rho or MAE labeling scheme
if "`useresults'" == "rho" loc z="&rho"
else if "`useresults'" == "mae" loc z="stSerif:MAE"
* Set scale for rho and standardized MAE results
if "`useresults'" == "rho" loc ylabelscale="ylabel(-.25(.25)1) yscale(range(-.2 1))"
else if "`useresults'" == "mae" & "`prestd'" == "z." loc ylabelscale="ylabel(-.25(.25)1) yscale(range(-.2 1))"
else loc ylabelscale=""
foreach var1 of varlist `variables' {
foreach var2 of varlist `variables' {
if "`var1'" != "`var2'" {
* Saving results
if "`CCMlagsave'"=="yes" {
loc b="saving(CCM_`var1'_`var2'_`useresults'_lags, replace)" 
else loc b=""

* Plot command
lgraph CCM_`useresults'_`var1'_`var2' CCM_lag_`var1'_`var2' CCM_id_`var1'_`var2' ///
if !missing(CCM_`useresults'_`var1'_`var2') & CCM_L_`var1'_`var2'max==CCM_L_`var1'_`var2', ///
/// errortype(ci(`CIs'))
`b' separate(.001) fit(`CCMlaglinefit') `ylabelscale' ///
title({stSerif:CCM: Lagged results for `var1' CCM causing `var2'}, color(black)) ///
ytitle({stSerif:Prediction Accuracy} {`z'}) xtitle({stSerif:Time Lag (Negative is Past)}) ///
note({stSerif:Note: Grand-mean line is bolded -- differencing at d`diff'}) ///
loptions(1 lwidth(*6)) foptions(1 lwidth(*6)) graphregion(color(white)) bgcolor(white) ///
name(CCM_`var1'_`var2'_`useresults'_lags, replace) nolegend xlabel(-`CCMlags'(1)`CCMleads')

if "`exportplot'" != "" {
graph export CCM_`var1'_`var2'_`useresults'_lags.`exportplot', replace
}
serset clear
}
}
}
}
restore
}


clear mata
serset clear





* Setup check for max(rho) or min(mae) at lag=0 or greater as assumption check before testing convergence
if "`CCMctest'" == "yes" {
gen long `obsnO'=_n
tempname Xit Lag_best
foreach var1 of varlist `variables' {
foreach var2 of varlist `variables' {
capture confirm variable CCM_id_`var1'_`var2'
if !_rc {
loc count`var1'`var2' = 0
quietly levelsof CCM_id_`var1'_`var2', local(levels)
foreach id of local levels {
loc count`var1'`var2'`id'=0
if "`StepwiseCCMtest'" == "yes" {
capture quietly {
summarize CCM_L_`var1'_`var2' if CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2'), meanonly
loc levelz=r(max)
levelsof `obsnO' if CCM_L_`var1'_`var2' == `levelz' & CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2')
loc levelo = `r(levels)'
}
capture {
putmata `Xit'=(CCM_`testval'_`var1'_`var2' CCM_lag_`var1'_`var2') if CCM_id_`var1'_`var2'==`id' & (CCM_lag_`var1'_`var2'!=0 | _n==`levelo'), replace omitmissing
}
if "`testval'"=="rho" {
mata: _sort(`Xit',-1)
}
else if "`testval'"=="mae" {
mata: _sort(`Xit',1)
}
mata: st_numscalar("`Lag_best'",`Xit'[1,2])
if `Lag_best' < 1 {
loc count`var1'`var2'=`count`var1'`var2''+1
loc count`var1'`var2'`id'=`count`var1'`var2'`id''+1
}
}
else {
loc count`var1'`var2'=`count`var1'`var2''+1
loc count`var1'`var2'`id'=`count`var1'`var2'`id''+1
}
}
}
}
}
drop `obsnO'
}



* CCM Tests for convergence comparing bootstrapped small library size predictions to full results
if "`CCMctest'" == "yes" {
tsset
gen long `obsnO' = _n
loc newCCMctestreps = `CCMctestreps' - `CCMreps1'
* Reuse previously computed bootstraps at 'lib1' library size
foreach var1 of varlist `variables' {
foreach var2 of varlist `variables' {
if "`count`var1'`var2''"!="" {
if `count`var1'`var2''>0 {
tempname reuseCCM reuseCCM`var1'`var2' CCMconvetest
mata: `reuseCCM`var1'`var2''=J(0,3,.)
mata: `CCMconvetest'=J(0,5,.)
quietly levelsof CCM_id_`var1'_`var2', local(levels)
foreach id of local levels {
if "`count`var1'`var2'`id''"!="" {
if `count`var1'`var2'`id''>0 {
tempname CCML2`var1'`var2'`id' Lmin`var1'`var2'`id'
capture quietly {
summarize CCM_L_`var1'_`var2' if CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2'), meanonly
loc levelz=r(max)
loc levelzmin=r(min)
levelsof `obsnO' if CCM_L_`var1'_`var2' == `levelz' & CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2')
sca `CCML2`var1'`var2'`id''=CCM_`testval'_`var1'_`var2'[`r(levels)']
sca `Lmin`var1'`var2'`id''=`levelzmin'
}
loc lib11 = `emax`var1'`id''+`lib1'
if `Lmin`var1'`var2'`id''==`lib11' {
capture {
putmata `reuseCCM'=(CCM_id_`var1'_`var2' CCM_rho_`var1'_`var2' CCM_mae_`var1'_`var2') if CCM_id_`var1'_`var2' == `id' & CCM_L_`var1'_`var2'==`Lmin`var1'`var2'`id'' & CCM_lag_`var1'_`var2' == 0, omitmissing replace
mata: `reuseCCM`var1'`var2''=`reuseCCM`var1'`var2''\ `reuseCCM'
}
* Top up existing bootstraps at 'lib1' if needed
if `newCCMctestreps' > 0 {
di "-------------------------------------------------------------------------------------------"
di "Reps are `newCCMctestreps', CCM Cause==`var1', Outcome==`var2'"
di "-------------------------------------------------------------------------------------------"
capture {
edm xmap `prestd'd`diff'.`var2' `prestd'd`diff'.`var1' if `panelvar'==`id' ``var1'dropzes1', e(`emax`var1'`id'') direction(oneway) lib(`lib11') rep(`newCCMctestreps') `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' `options'
}
capture {
mata: `Z21' = J(1,`newCCMctestreps',`id')
mata: `Z1'=st_matrix("e(xmap_1)")
mata: `Z3'=`Z21'\ `Z1''
mata: `CCMconvetest' = `Z3''
}
capture {
mata: `reuseCCM`var1'`var2''= `reuseCCM`var1'`var2''\ (`CCMconvetest'[|1,1 \ .,1|],`CCMconvetest'[|1,4 \ .,4|],`CCMconvetest'[|1,5 \ .,5|])
}
}
}
}
}
}
capture {
getmata (CCMctest_`var1'_`var2'*)=`reuseCCM`var1'`var2'', force double
label variable CCMctest_`var1'_`var2'1 "CCM convergence test of `testval' at 'lib1' (all reps), panel ID from `panelvar'"
rename CCMctest_`var1'_`var2'1 CCMctest_id_`var1'_`var2'
recast long CCMctest_id_`var1'_`var2'
label variable CCMctest_`var1'_`var2'2 "CCM convergence test of `testval' at 'lib1' (all reps), rho"
rename CCMctest_`var1'_`var2'2 CCMctest_rho_`var1'_`var2'
label variable CCMctest_`var1'_`var2'3 "CCM convergence test of `testval' at 'lib1' (all reps), mae"
rename CCMctest_`var1'_`var2'3 CCMctest_mae_`var1'_`var2'
}
}
}
}
}
drop `obsnO'
}


* Hypothesis testing calculations and plots
if "`CCMctestplot'" == "yes" {
preserve
gen long `obsnO'=_n
if "`testval'"=="rho" loc pctiles = "50, 95, 99"
else if "`testval'"=="mae" loc pctiles = "1, 5, 50"
foreach var1 of varlist `variables' {
foreach var2 of varlist `variables' {
capture confirm variable CCMctest_id_`var1'_`var2'
if !_rc {
tempname CCML2pct`var1'`var2'
mata: `CCML2pct`var1'`var2'' = J(0,5,.)
loc `var1'`var2'tcounter=0
loc `var1'`var2't95counter=0
loc `var1'`var2't99counter=0
quietly levelsof CCMctest_id_`var1'_`var2', local(levels)
foreach id of local levels {
loc `var1'`var2'tcounter=``var1'`var2'tcounter'+1
tempname CCML2`var1'`var2'`id'
capture quietly {
summarize CCM_L_`var1'_`var2' if CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2'), meanonly
loc levelz=r(max)
levelsof `obsnO' if CCM_L_`var1'_`var2' == `levelz' & CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2')
sca `CCML2`var1'`var2'`id''=CCM_`testval'_`var1'_`var2'[`r(levels)']
}
capture {
_pctile CCMctest_`testval'_`var1'_`var2' if CCMctest_id_`var1'_`var2'==`id', percentiles(`pctiles')
if "`testval'"=="rho" & `CCML2`var1'`var2'`id''-r(r2) > 0 loc `var1'`var2't95counter=``var1'`var2't95counter'+1
if "`testval'"=="rho" & `CCML2`var1'`var2'`id''-r(r3) > 0 loc `var1'`var2't99counter=``var1'`var2't99counter'+1
if "`testval'"=="mae" & `CCML2`var1'`var2'`id''-r(r2) < 0 loc `var1'`var2't95counter=``var1'`var2't95counter'+1
if "`testval'"=="mae" & `CCML2`var1'`var2'`id''-r(r1) < 0 loc `var1'`var2't99counter=``var1'`var2't99counter'+1
mata: `CCML2`var1'`var2'`id''=st_numscalar("`CCML2`var1'`var2'`id''")
mata: `A' = J(1,4,`id')
mata: `B' = (`pctiles', 110)
mata: `C' = (st_numscalar("r(r1)"), st_numscalar("r(r2)"), st_numscalar("r(r3)"), `CCML2`var1'`var2'`id'')
mata: `C2' = (`CCML2`var1'`var2'`id''-st_numscalar("r(r1)"), `CCML2`var1'`var2'`id''-st_numscalar("r(r2)"), `CCML2`var1'`var2'`id''-st_numscalar("r(r3)"), 0)
capture confirm scalar e(N)
if !_rc {
mata: `D' = J(1,4,st_numscalar("e(N)"))
}
else {
mata: `D' = J(1,4,.)
}
mata: `E' = `A'\ `B'\ `C'\ `C2'\ `D'
mata: `CCML2pct`var1'`var2''=`CCML2pct`var1'`var2''\ `E''
}
}
capture {
getmata (CCML2pct`var1'`var2'*)=`CCML2pct`var1'`var2'', force double
label variable CCML2pct`var1'`var2'1 "CCM convergence test Percentile panel ID"
rename CCML2pct`var1'`var2'1 CCML2pct`var1'`var2'
label variable CCML2pct`var1'`var2'2 "CCM convergence test Percentile"
rename CCML2pct`var1'`var2'2 CCML2pct`var1'`var2'_pctile
label variable CCML2pct`var1'`var2'3 "CCM convergence test values"
rename CCML2pct`var1'`var2'3 CCML2pct`var1'`var2'_vals
label variable CCML2pct`var1'`var2'4 "CCM convergence test Values Minus Observed"
rename CCML2pct`var1'`var2'4 CCML2pct`var1'`var2'_valsc
label variable CCML2pct`var1'`var2'5 "CCM convergence test Percentile N"
rename CCML2pct`var1'`var2'5 CCML2pct`var1'`var2'_N
}
capture {
if "`testval'"=="rho" loc fivepct = 95
else if "`testval'"=="mae" loc fivepct = 5
if "`testval'"=="rho" loc onepct = 99
else if "`testval'"=="mae" loc onepct = 1
if "`testval'"=="rho" loc onesuffix = "th"
else if "`testval'"=="mae" loc onesuffix = "st"
if "`testval'" == "rho" loc z="&rho"
else if "`testval'" == "mae" loc z="stSerif:MAE"

loc pct95rnd = round(100*``var1'`var2't95counter'/``var1'`var2'tcounter', 1)
loc pct95rnd2: di %3.0f `pct95rnd'
loc pct99rnd = round(100*``var1'`var2't99counter'/``var1'`var2'tcounter', 1)
loc pct99rnd2: di %3.0f `pct99rnd'

sum CCML2pct`var1'`var2'_vals if CCML2pct`var1'`var2'_pctile==110, meanonly
loc testvalGM=r(mean)
loc testvalGMr=round(`testvalGM',.001)
loc testvalGMd: di %8.3f `testvalGMr'

sum CCML2pct`var1'`var2'_vals if CCML2pct`var1'`var2'_pctile==`onepct', meanonly
loc onepctGM=r(mean)
loc onepctGMr=round(`onepctGM',.001)
loc onepctGMd: di %8.3f `onepctGMr'

sum CCML2pct`var1'`var2'_vals if CCML2pct`var1'`var2'_pctile==`fivepct' , meanonly
loc fivepctGM=r(mean)
loc fivepctGMr=round(`fivepctGM',.001)
loc fivepctGMd: di %8.3f `fivepctGMr'

sum CCML2pct`var1'`var2'_vals if CCML2pct`var1'`var2'_pctile==50, meanonly
loc nullmeanGM=r(mean)
loc nullmeanGMr=round(`nullmeanGM',.001)
loc nullmeanGMd: di %6.3f `nullmeanGMr'


if "`testval'" == "rho" loc notess=`"note("{stSerif:Note 1: y=0 is} {&rho}{stSerif: at Min L, reject H0 if 99th and/or 95th percentiles are positive}" "{stSerif:Note 2: Of ``var1'`var2'tcounter' panel IDs, ``var1'`var2't95counter' (i.e., `pct95rnd2'%) show p < .05, and ``var1'`var2't99counter' (i.e., `pct99rnd2'%) show p < .01}" "{stSerif:Note 3: Raw grand-means: Observed} {&rho} {stSerif:= `testvalGMd'; `onepct'% = `onepctGMd'; `fivepct'% = `fivepctGMd'; Min L mean = `nullmeanGMd'}")"'
else if "`testval'" == "mae" loc notess=`"note("{stSerif:Note 1: y=0 is MAE at Min L, reject H0 if 1st and/or 5th percentiles are negative}" "{stSerif:Note 2: Of ``var1'`var2'tcounter' panel IDs, ``var1'`var2't95counter' (i.e., `pct95rnd2'%) show p < .05, and ``var1'`var2't99counter' (i.e., `pct99rnd2'%) show p < .01}" "{stSerif:Note 3: Raw grand-means: Observed MAE = `testvalGMd'; `onepct'% = `onepctGMd'; `fivepct'% = `fivepctGMd';  Min L mean = `nullmeanGMd'}")"'

if "`CCMsave'" == "yes" loc savers1="saving(CCMctest_`var1'`var2'_`testval'1, replace)"
if "`CCMsave'" == "yes" loc savers2="saving(CCMctest_`var1'`var2'_`testval'2, replace)"

tempvar titled2`var1'`var2'
gen `titled2`var1'`var2'' = ""
label variable `titled2`var1'`var2'' "Percentiles of Bootstrapped Minimum Library Size"
replace `titled2`var1'`var2''="Mean Diff at Min L" if CCML2pct`var1'`var2'_pctile==50
replace `titled2`var1'`var2''="`fivepct'th Pctile Diff" if CCML2pct`var1'`var2'_pctile==`fivepct'
replace `titled2`var1'`var2''="`onepct'`onesuffix' Pctile Diff" if CCML2pct`var1'`var2'_pctile==`onepct'

qui labmask CCML2pct`var1'`var2'_pctile, values(`titled2`var1'`var2'')

twoway (scatter CCML2pct`var1'`var2'_valsc CCML2pct`var1'`var2' if CCML2pct`var1'`var2'_pctile==50) ///
(scatter CCML2pct`var1'`var2'_valsc CCML2pct`var1'`var2' if CCML2pct`var1'`var2'_pctile==`fivepct') ///
(scatter CCML2pct`var1'`var2'_valsc CCML2pct`var1'`var2' if CCML2pct`var1'`var2'_pctile==`onepct'), ///
legend(label(1 {stSerif:Mean Diff at Min L}) label(2 {stSerif:`fivepct'th Pctile Diff}) label(3 {stSerif:`onepct'`onesuffix' Pctile Diff})) ///
title({stSerif:CCM: Convergence test of `var1' CCM causing `var2'}, color(black)) ytitle({stSerif:Prediction Accuracy} {`z'}) ///
/*xlab(`panelrange')*/ name(CCMctest_`var1'_`var2'_`testval'1, replace) yline(0) `notess' graphregion(color(white)) `savers1' xline(`panelrange', lwidth(vthin) lpattern(dash))

if "`exportplot'" != "" {
graph export CCMctest_`var1'_`var2'_`testval'1.`exportplot', replace
}
serset clear

lgraph CCML2pct`var1'`var2'_valsc CCML2pct`var1'`var2' CCML2pct`var1'`var2'_pctile if CCML2pct`var1'`var2'_pctile!=110, graphregion(color(white)) ///
yline(0) ytitle({stSerif:Prediction Accuracy} {`z'}) title({stSerif:CCM: Convergence test of `var1' CCM causing `var2'}, color(black)) ///
/*xlab(`panelrange')*/  name(CCMctest_`var1'_`var2'_`testval'2, replace)  `notess' `savers2' xline(`panelrange', lwidth(vthin) lpattern(dot))

if "`exportplot'" != "" {
graph export CCMctest_`var1'_`var2'_`testval'2.`exportplot', replace
}
serset clear
}
}
}
}
restore
}


serset clear
clear mata



* Setup check for convergence before testing effect as max L using permutation-based null (i.e., surrogate data)
if "`CCMLtest'" == "yes" {
gen long `obsnO'=_n
if "`testval'"=="rho" loc pctiles = "50, 95, 99"
else if "`testval'"=="mae" loc pctiles = "1, 5, 50"
foreach var1 of varlist `variables' {
foreach var2 of varlist `variables' {
capture confirm variable CCMctest_id_`var1'_`var2'
if !_rc {
loc count`var1'`var2' = 0
quietly levelsof CCMctest_id_`var1'_`var2', local(levels)
foreach id of local levels {
loc count`var1'`var2'`id'=0
if "`StepwiseCCMtest'" == "yes" {
tempname CCML2`var1'`var2'`id'
capture quietly {
summarize CCM_L_`var1'_`var2' if CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2'), meanonly
loc levelz=r(max)
levelsof `obsnO' if CCM_L_`var1'_`var2' == `levelz' & CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2')
sca `CCML2`var1'`var2'`id''=CCM_`testval'_`var1'_`var2'[`r(levels)']
}
capture {
_pctile CCMctest_`testval'_`var1'_`var2' if CCMctest_id_`var1'_`var2'==`id', percentiles(`pctiles')
if "`testval'"=="rho" & r(r2)-`CCML2`var1'`var2'`id'' < 0 loc count`var1'`var2'=`count`var1'`var2''+1
if "`testval'"=="rho" & r(r2)-`CCML2`var1'`var2'`id'' < 0 loc count`var1'`var2'`id'=`count`var1'`var2'`id''+1
if "`testval'"=="mae" & r(r2)-`CCML2`var1'`var2'`id'' > 0 loc count`var1'`var2'=`count`var1'`var2''+1
if "`testval'"=="mae" & r(r2)-`CCML2`var1'`var2'`id'' > 0 loc count`var1'`var2'`id'=`count`var1'`var2'`id''+1
}
}
else {
loc count`var1'`var2'=`count`var1'`var2''+1
loc count`var1'`var2'`id'=`count`var1'`var2'`id''+1
}
}
}
}
}
drop `obsnO'
}





* CCM Tests for prediction at maximum library size using permutation
if "`CCMLtest'" == "yes" {
tsset
* First permute for 'var1', run CCM analyses, and obtain the 'testval' at lag==0 and max L
gen `randoM'=.
gen long `newtimE'=.
gen long `obsnO'=_n
foreach var1 of varlist `variables' {
tempvar d`diff'`var1' r`var1'
gen `d`diff'`var1''=d`diff'.`var1'
gen `r`var1''=.
foreach var2 of varlist `variables' {
if "`count`var1'`var2''"!="" {
if `count`var1'`var2''>0 {
tempname CCMLtest`var1'`var2'
mata: `CCMLtest`var1'`var2''=J(0,5,.)
foreach i of numlist 1/`CCMLtestreps' {
bysort `panelIDvar': replace `randoM'=uniform()
if "`dropzeros'"=="yes" {
bysort `panelIDvar' (``var1'_nonzerouse' `randoM'): replace `newtimE'=_n if ``var1'_nonzerouse'==1
bysort `panelIDvar' (``var1'_nonzerouse' `timevar'): replace `r`var1''=`d`diff'`var1''[`newtimE'] if ``var1'_nonzerouse'==1
tsset
}
else {
bysort `panelIDvar' (`randoM'): replace `newtimE'=_n
bysort `panelIDvar' (`timevar'): replace `r`var1''=`d`diff'`var1''[`newtimE']
tsset
}
capture quietly levelsof CCMctest_id_`var1'_`var2', local(levels)
foreach id of local levels {
if "`count`var1'`var2'`id''"!="" {
if `count`var1'`var2'`id''>0 {
capture quietly {
summarize CCM_L_`var1'_`var2' if CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2'), meanonly
loc levelz=r(max)
levelsof `obsnO' if CCM_L_`var1'_`var2' == `levelz' & CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0
tempname CCML`var1'`var2'`id'
sca `CCML`var1'`var2'`id''=CCM_`testval'_`var1'_`var2'[`r(levels)']
}
di "--------------------------------------------------------------------------------"
di "Rep == `i' of `CCMLtestreps', CCM Cause==`var1', Outcome==`var2'"
di "--------------------------------------------------------------------------------"
capture {
qui edm xmap `prestd'd`diff'.`var2' `prestd'`r`var1'' if `panelvar'==`id' ``var1'dropzes1', e(`emax`var1'`id'') direction(oneway) `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' `options'
}
capture {
mata: `Z21' = (`id')
mata: `Z1'=st_matrix("e(xmap_1)")
tempname ehere
sca `ehere'=e(e_actual)
mata: `Z1'[1,1]=st_numscalar("`ehere'")
mata: `Z3'=`Z21'\ `Z1''
mata: `CCMLtest`var1'`var2'' = `CCMLtest`var1'`var2''\ `Z3''
}
}
}
}
}
capture quietly {
getmata (CCMLtest_`var1'_`var2'*)=`CCMLtest`var1'`var2'', force double
label variable CCMLtest_`var1'_`var2'1 "CCM test of `testval' at max library (all reps), panel ID from `panelvar'"
rename CCMLtest_`var1'_`var2'1 CCMLtest_id_`var1'_`var2'
label variable CCMLtest_`var1'_`var2'2 "CCM test of `testval' at max library (all reps), E value for CCM cause `var1'"
rename CCMLtest_`var1'_`var2'2 CCMLtest_e_`var1'_`var2'
recast int CCMLtest_e_`var1'_`var2'
drop CCMLtest_`var1'_`var2'3
label variable CCMLtest_`var1'_`var2'4 "CCM test of `testval' at max library (all reps), rho"
rename CCMLtest_`var1'_`var2'4 CCMLtest_rho_`var1'_`var2'
label variable CCMLtest_`var1'_`var2'5 "CCM test of `testval' at max library (all reps), , MAE"
rename CCMLtest_`var1'_`var2'5 CCMLtest_mae_`var1'_`var2'
}
}
}
}
drop `d`diff'`var1'' `r`var1''
}
drop `randoM' `newtimE' `obsnO'
}



* Hypothesis testing calculations and plots
if "`CCMLtestplot'" == "yes" {
preserve
gen long `obsnO' = _n
if "`testval'"=="rho" loc pctiles = "50, 95, 99"
else if "`testval'"=="mae" loc pctiles = "1, 5, 50"
foreach var1 of varlist `variables' {
foreach var2 of varlist `variables' {
capture confirm variable CCMLtest_id_`var1'_`var2'
if !_rc {
tempname CCMLpct`var1'`var2'
mata: `CCMLpct`var1'`var2'' = J(0,5,.)
loc `var1'`var2'tcounter=0
loc `var1'`var2't95counter=0
loc `var1'`var2't99counter=0
quietly levelsof CCMLtest_id_`var1'_`var2', local(levels)
foreach id of local levels {
* capture quietly {
summarize CCM_L_`var1'_`var2' if CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2'), meanonly
loc levelz=r(max)
levelsof `obsnO' if CCM_L_`var1'_`var2' == `levelz' & CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0
tempname CCML`var1'`var2'`id'
sca `CCML`var1'`var2'`id''=CCM_`testval'_`var1'_`var2'[`r(levels)']
* }
loc `var1'`var2'tcounter=``var1'`var2'tcounter'+1
* capture {
_pctile CCMLtest_`testval'_`var1'_`var2' if CCMLtest_id_`var1'_`var2'==`id', percentiles(`pctiles')
if "`testval'"=="rho" & `CCML`var1'`var2'`id''-r(r2) > 0 loc `var1'`var2't95counter=``var1'`var2't95counter'+1
if "`testval'"=="rho" & `CCML`var1'`var2'`id''-r(r3) > 0 loc `var1'`var2't99counter=``var1'`var2't99counter'+1
if "`testval'"=="mae" & `CCML`var1'`var2'`id''-r(r2) < 0 loc `var1'`var2't95counter=``var1'`var2't95counter'+1
if "`testval'"=="mae" & `CCML`var1'`var2'`id''-r(r1) < 0 loc `var1'`var2't99counter=``var1'`var2't99counter'+1
mata: `CCML`var1'`var2'`id''=st_numscalar("`CCML`var1'`var2'`id''")
mata: `A' = J(1,4,`id')
mata: `B' = (`pctiles', 110)
mata: `C' = (st_numscalar("r(r1)"), st_numscalar("r(r2)"), st_numscalar("r(r3)"), `CCML`var1'`var2'`id'')
mata: `C2' = (`CCML`var1'`var2'`id''-st_numscalar("r(r1)"), `CCML`var1'`var2'`id''-st_numscalar("r(r2)"), `CCML`var1'`var2'`id''-st_numscalar("r(r3)"), 0)
capture confirm scalar e(N)
if !_rc {
mata: `D' = J(1,4,st_numscalar("e(N)"))
}
else {
mata: `D' = J(1,4,.)
}
mata: `E' = `A'\ `B'\ `C'\ `C2'\ `D'
mata: `CCMLpct`var1'`var2''=`CCMLpct`var1'`var2''\ `E''
* }
}
capture quietly {
getmata (CCMLpct`var1'`var2'*)=`CCMLpct`var1'`var2'', force double
label variable CCMLpct`var1'`var2'1 "CCM max L test Percentile panel ID"
rename CCMLpct`var1'`var2'1 CCMLpct`var1'`var2'
label variable CCMLpct`var1'`var2'2 "CCM max L test Percentile"
rename CCMLpct`var1'`var2'2 CCMLpct`var1'`var2'_pctile
label variable CCMLpct`var1'`var2'3 "CCM max L test values"
rename CCMLpct`var1'`var2'3 CCMLpct`var1'`var2'_vals
label variable CCMLpct`var1'`var2'4 "CCM max L test Values Minus Observed"
rename CCMLpct`var1'`var2'4 CCMLpct`var1'`var2'_valsc
label variable CCMLpct`var1'`var2'5 "CCM max L Percentile N"
rename CCMLpct`var1'`var2'5 CCMLpct`var1'`var2'_N
}
capture {
if "`testval'"=="rho" loc fivepct = 95
else if "`testval'"=="mae" loc fivepct = 5
if "`testval'"=="rho" loc onepct = 99
else if "`testval'"=="mae" loc onepct = 1
if "`testval'"=="rho" loc onesuffix = "th"
else if "`testval'"=="mae" loc onesuffix = "st"
if "`testval'" == "rho" loc z="&rho"
else if "`testval'" == "mae" loc z="stSerif:MAE"

loc pct95rnd = round(100*``var1'`var2't95counter'/``var1'`var2'tcounter', 1)
loc pct95rnd2: di %3.0f `pct95rnd'
loc pct99rnd = round(100*``var1'`var2't99counter'/``var1'`var2'tcounter', 1)
loc pct99rnd2: di %3.0f `pct99rnd'

sum CCMLpct`var1'`var2'_vals if CCMLpct`var1'`var2'_pctile==110, meanonly
loc testvalGM=r(mean)
loc testvalGMr=round(`testvalGM',.001)
loc testvalGMd: di %8.3f `testvalGMr'

sum CCMLpct`var1'`var2'_vals if CCMLpct`var1'`var2'_pctile==`onepct', meanonly
loc onepctGM=r(mean)
loc onepctGMr=round(`onepctGM',.001)
loc onepctGMd: di %8.3f `onepctGMr'

sum CCMLpct`var1'`var2'_vals if CCMLpct`var1'`var2'_pctile==`fivepct' , meanonly
loc fivepctGM=r(mean)
loc fivepctGMr=round(`fivepctGM',.001)
loc fivepctGMd: di %8.3f `fivepctGMr'

sum CCMLpct`var1'`var2'_vals if CCMLpct`var1'`var2'_pctile==50, meanonly
loc nullmeanGM=r(mean)
loc nullmeanGMr=round(`nullmeanGM',.001)
loc nullmeanGMd: di %6.3f `nullmeanGMr'


if "`testval'" == "rho" loc notess=`"note("{stSerif:Note 1: y=0 is permuted} {&rho}{stSerif:, reject H0 if 99th and/or 95th percentiles are positive}" "{stSerif:Note 2: Of ``var1'`var2'tcounter' panel IDs, ``var1'`var2't95counter' (i.e., `pct95rnd2'%) show p < .05, and ``var1'`var2't99counter' (i.e., `pct99rnd2'%) show p < .01}" "{stSerif:Note 3: Raw grand-means are: Observed} {&rho} {stSerif:= `testvalGMd'; `onepct'% = `onepctGMd'; `fivepct'% = `fivepctGMd'; Null mean = `nullmeanGMd'}")"'
else if "`testval'" == "mae" loc notess=`"note("{stSerif:Note 1: y=0 is permuted MAE, reject H0 if 1st and/or 5th percentiles are negative}" "{stSerif:Note 2: Of ``var1'`var2'tcounter' panel IDs, ``var1'`var2't95counter' (i.e., `pct95rnd2'%) show p < .05, and ``var1'`var2't99counter' (i.e., `pct99rnd2'%) show p < .01}" "{stSerif:Note 3: Raw grand-means are: Observed MAE = `testvalGMd'; `onepct'% = `onepctGMd'; `fivepct'% = `fivepctGMd'; Null mean = `nullmeanGMd'}")"'

if "`CCMsave'" == "yes" loc savers1="saving(CCMLtest_`var1'_`var2'_`testval'1, replace)"
if "`CCMsave'" == "yes" loc savers2="saving(CCMLtest_`var1'_`var2'_`testval'2, replace)"

tempvar titled`var1'`var2'
gen `titled`var1'`var2'' = ""
label variable `titled`var1'`var2'' "Percentiles of Permuted Null Distribution"
replace `titled`var1'`var2''="Permuted Mean Diff" if CCMLpct`var1'`var2'_pctile==50
replace `titled`var1'`var2''="`fivepct'th Pctile Diff" if CCMLpct`var1'`var2'_pctile==`fivepct'
replace `titled`var1'`var2''="`onepct'`onesuffix' Pctile Diff" if CCMLpct`var1'`var2'_pctile==`onepct'

qui labmask CCMLpct`var1'`var2'_pctile, values(`titled`var1'`var2'')

twoway (scatter CCMLpct`var1'`var2'_valsc CCMLpct`var1'`var2' if CCMLpct`var1'`var2'_pctile==50) ///
(scatter CCMLpct`var1'`var2'_valsc CCMLpct`var1'`var2' if CCMLpct`var1'`var2'_pctile==`fivepct') ///
(scatter CCMLpct`var1'`var2'_valsc CCMLpct`var1'`var2' if CCMLpct`var1'`var2'_pctile==`onepct'), ///
legend(label(1 {stSerif:Permuted Mean Diff}) label(2 {stSerif:`fivepct'th Pctile Diff}) label(3 {stSerif:`onepct'`onesuffix' Pctile Diff})) ///
title({stSerif:CCM: Test of `var1' CCM causing `var2' at max L}, color(black)) ytitle({stSerif:Prediction Accuracy} {`z'}) ///
/*xlab(`panelrange')*/ name(CCMLtest_`var1'_`var2'_`testval'1, replace) yline(0) `notess' graphregion(color(white)) `savers1' xline(`panelrange', lwidth(vthin) lpattern(dash))

if "`exportplot'" != "" {
graph export CCMLtest_`var1'_`var2'_`testval'1.`exportplot', replace
}
serset clear

lgraph CCMLpct`var1'`var2'_valsc CCMLpct`var1'`var2' CCMLpct`var1'`var2'_pctile if CCMLpct`var1'`var2'_pctile!=110, graphregion(color(white)) ///
yline(0) ytitle({stSerif:Prediction Accuracy} {`z'}) title({stSerif:CCM: Test of `var1' CCM causing `var2' at max L}, color(black)) ///
/*xlab(`panelrange')*/  name(CCMLtest_`var1'_`var2'_`testval'2, replace)  `notess' `savers2' xline(`panelrange', lwidth(vthin) lpattern(dot))

if "`exportplot'" != "" {
graph export CCMLtest_`var1'_`var2'_`testval'2.`exportplot', replace
}
serset clear
}
}
}
}
restore
}


clear mata
serset clear



* Setup before computing SMAP coefficients check for significant prediction at max L using permutation-based null (i.e., surrogate data) 
if "`CCMSMAP'" == "yes" {
gen long `obsnO'=_n
if "`testval'"=="rho" loc pctiles = "50, 95, 99"
else if "`testval'"=="mae" loc pctiles = "1, 5, 50"
foreach var1 of varlist `variables' {
foreach var2 of varlist `variables' {
capture confirm variable CCMLtest_id_`var1'_`var2'
if !_rc {
loc ncount`var1'`var2' = 0
quietly levelsof CCMLtest_id_`var1'_`var2', local(levels)
foreach id of local levels {
loc ncount`var1'`var2'`id'=0
if "`StepwiseCCMtest'" == "yes" {
tempname CCML2`var1'`var2'`id'
capture quietly {
summarize CCM_L_`var1'_`var2' if CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2'), meanonly
loc levelz=r(max)
levelsof `obsnO' if CCM_L_`var1'_`var2' == `levelz' & CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2')
sca `CCML2`var1'`var2'`id''=CCM_`testval'_`var1'_`var2'[`r(levels)']
}
capture {
_pctile CCMLtest_`testval'_`var1'_`var2' if CCMLtest_id_`var1'_`var2'==`id', percentiles(`pctiles')
if "`testval'"=="rho" & r(r2)-`CCML2`var1'`var2'`id'' < 0 loc ncount`var1'`var2'=`ncount`var1'`var2''+1
if "`testval'"=="rho" & r(r2)-`CCML2`var1'`var2'`id'' < 0 loc ncount`var1'`var2'`id'=`ncount`var1'`var2'`id''+1
if "`testval'"=="mae" & r(r2)-`CCML2`var1'`var2'`id'' > 0 loc ncount`var1'`var2'=`ncount`var1'`var2''+1
if "`testval'"=="mae" & r(r2)-`CCML2`var1'`var2'`id'' > 0 loc ncount`var1'`var2'`id'=`ncount`var1'`var2'`id''+1
}
}
else {
loc ncount`var1'`var2'=`ncount`var1'`var2''+1
loc ncount`var1'`var2'`id'=`ncount`var1'`var2'`id''+1
}
}
}
}
}
drop `obsnO'
}



* Compute CCM coefficients using SMAP methods
if "`CCMSMAP'" == "yes" {
tsset
foreach var1 of varlist `variables' {
foreach var2 of varlist `variables' {
if "`ncount`var1'`var2''"!="" {
if `ncount`var1'`var2''>0 {
summarize CCMLtest_e_`var1'_`var2', meanonly
loc levelz=r(max)
forval i=0/`levelz' {
gen CCM_SMAP_`var1'_`var2'_b`i'=.
}
quietly levelsof CCMLtest_id_`var1'_`var2', local(levels)
foreach id of local levels {
if "`ncount`var1'`var2'`id''"!="" {
if `ncount`var1'`var2'`id''>0 {
di "--------------------------------------------------------------------------------"
di "CCM SMAP  Cause==`var1', Outcome==`var2'"
di "--------------------------------------------------------------------------------"
loc a = 0
capture noisily {
edm xmap `prestd'd`diff'.`var1' `prestd'd`diff'.`var2' if `panelvar'==`id' ``var1'dropzes1', e(`emax`var1'`id'') direction(oneway) `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' alg(smap) k(`CCMSMAPk') savesmap(z`var1'`var2') `options'
}
if "`time_gap'"=="dt" loc dtextras = `emax`var1'`id''-1
else loc dtextras = 0
loc a = `emax`var1'`id''+`extraz'+`dtextras'
forval i=0/`levelz' {
capture confirm variable z`var1'`var2'1_b`i'_rep1
if !_rc {
replace CCM_SMAP_`var1'_`var2'_b`i'=z`var1'`var2'1_b`i'_rep1 if z`var1'`var2'1_b`i'_rep1 != .
if `a'==`levelz' {
local labelz : variable label z`var1'`var2'1_b`i'_rep1
label variable CCM_SMAP_`var1'_`var2'_b`i' "`labelz'"
}
drop z`var1'`var2'1_b`i'_rep1
}
}
}
}
}
}
}
}
}
}



serset clear 
clear mata



* CCM SMAP plots
if "`CCMSMAPgraph'"=="yes" & "`plots_on'" =="yes" {
preserve
foreach var1 of varlist `variables' {
foreach var2 of varlist `variables' {
capture confirm variable CCM_SMAP_`var1'_`var2'_b0
if !_rc {
summarize CCMLtest_e_`var1'_`var2', meanonly
loc levelz=r(max)
loc a = ""
loc `var1'`var2'b = ""
loc c = `levelz'-`extraz'
loc d = ""
loc d`var1'`var2' = ""
loc zz`var1'`var2' = ""
loc nope=""
loc h=0
capture forval i=1/`levelz' {
loc labeldt : variable label CCM_SMAP_`var1'_`var2'_b`i'
loc first=substr("`labeldt'",1,2)
if `i' == 1 | (`i' > `extraz'+1 & "`first'"!="dt") {
loc h=`h'+1
replace CCM_SMAP_`var1'_`var2'_b`i'=. if CCM_SMAP_`var1'_`var2'_b`i'==0
sum CCM_SMAP_`var1'_`var2'_b`i', meanonly 
if r(N) != 0 & `h' <= `CCMSMAPeffects' {
loc a = "`a'(kdensity CCM_SMAP_`var1'_`var2'_b`i', lwidth(medthick))"
loc d = "`d' CCM_SMAP_`var1'_`var2'_b`i'"
loc d`var1'`var2' = "`d`var1'`var2'' CCM_SMAP_`var1'_`var2'_b`i'"
loc zz`var1'`var2' = "`zz`var1'`var2'' `timevar' CCM_SMAP_`var1'_`var2'_b`i'"
loc nope="`nope' no"
loc e = `h'-1
loc `var1'`var2'b = `"``var1'`var2'b' `h' "Lag `e'""'
}
}
}
if "`nope'"!="" {
if "`CCMSMAPsave'" == "yes" loc savers1="saving(CCM_SMAP_`var1'_`var2'_Marg, replace)"
if "`CCMSMAPsave'" == "yes" loc savers2="saving(CCM_SMAP_`var1'_`var2'_Tim1, replace)"
if "`CCMSMAPsave'" == "yes" loc savers3="saving(CCM_SMAP_`var1'_`var2'_Tim2, replace)"

twoway `a', xline(0) legend(order(``var1'`var2'b') textfirst col(1) position(3)) xtitle({stSerif:Local Linear Slope}) ///
name(CCM_SMAP_`var1'_`var2'_Marg, replace) `savers1' title({stSerif:Marginal CCM SMAP Effect of `var1' on `var2'}, color(black)) ///
ytitle({stSerif:Density}) graphregion(color(white)) note({stSerif:Note: Differencing at d`diff' and missing coefficient variables reflect problems with SMAP estimates})  ///

if "`exportplot'" != "" {
graph export CCM_SMAP_`var1'_`var2'_Marg.`exportplot', replace
}
serset clear
}
}
}
}
restore
}

/*foreach var1 in `variables' {
foreach var2 in `variables' {
capture confirm variable CCM_SMAP_`var1'_`var2'_b0
if !_rc {
if "`d`var1'`var2''"!="" {
preserve
capture forval i=1/`levelz' {
loc labeldt : variable label CCM_SMAP_`var1'_`var2'_b`i'
loc first=substr("`labeldt'",1,2)
if `i' == 1 | (`i' > `extraz'+1 & "`first'"!="dt") {
replace CCM_SMAP_`var1'_`var2'_b`i'=. if CCM_SMAP_`var1'_`var2'_b`i'==0
}
}
collapse (`CCMSMAPstat') CCM_SMAP*, by(`timevar')
tempvar meanZ
egen `meanZ'= rowmean(`d`var1'`var2'')
drop if `meanZ'==.
tsset `timevar'
tsfill
tsline `d`var1'`var2'', xtitle({stSerif:Time Variable}) ytitle({stSerif:Local Linear Slopes}) ///
graphregion(color(white)) name(CCM_SMAP_`var1'_`var2'_Tim1, replace) cmissing(`nope') lwidth(medthick ..)  ///
title({stSerif:CCM SMAP Effect Averages of `var1' on `var2'}, color(black)) `savers2' ///
legend(order(``var1'`var2'b') textfirst col(1) position(3)) note({stSerif:Note: Differencing at d`diff' and missing coefficient variables reflect problems with SMAP estimates}) 

if "`exportplot'" != "" {
graph export CCM_SMAP_`var1'_`var2'_Tim1.`exportplot', replace
}
restore
}
}
}
}
*/
/*
foreach var1 of varlist `variables' {
foreach var2 of varlist `variables' {
capture confirm variable CCMLtest_id_`var1'_`var2'
if !_rc {
preserve
stack `zz`var1'`var2'', into(`timevar' CCM_SMAP_`var1'_`var2') clear
gen Lag = _stack-1
drop if CCM_SMAP_`var1'_`var2' ==. | (CCM_SMAP_`var1'_`var2'==0 & Lag==0)
lgraph CCM_SMAP_`var1'_`var2' `timevar' Lag, statistic(`CCMSMAPstat') ///
separate(.005) fit(`CCMSMAPlinefit')  /* errortype(`CCMSMAPerrortype') */  ///
title({stSerif:CCM SMAP Effect Averages of `var1' on `var2'}, color(black)) ///
ytitle({stSerif:Local Linear Slopes}) xtitle({stSerif:Time Variable}) ///
note({stSerif:Note: Differencing at d`diff'}) `savers3' ///
graphregion(color(white)) bgcolor(white) nomarker ///
name(CCM_SMAP_`var1'_`var2'_Tim2, replace) legend(order(`b') textfirst col(1) position(3))

if "`exportplot'" != "" {
graph export CCM_SMAP_`var1'_`var2'_Tim2.`exportplot', replace
}
restore
}
}
}
*/
* local CCMSMAPerrortype = "ci(95)"   // Set errortype from lgraph command, here default is 95% ci but can set minmax for example or quantiles
* Note 1: For no CI enter 0



* Save all results
if "`saveresults'"=="yes" {
putexcel set EDMresults.xlsx, replace


* Save SP results 
capture qui {
foreach dval of numlist `drange' {
	putexcel set EDMresults.xlsx, sheet("SP_d`dval'") modify
	putexcel A1 = "Simplex projection results using `useresults' and differencing at d`dval' (d0 = raw data, no differencing; d1 = first-differencing; etc.)"
	putexcel A3 = "No results exist, check data and/or EDM input"
	mata: spresults = `eminimum'::`emaximum'
	loc list = "E"
	foreach var of varlist `variables' {
		capture confirm variable SPlong_`useresults'_`var'
		if !_rc {
			mata: mm = J(0,1,.)
			foreach e of numlist `eminimum'/`emaximum' {
				qui sum SPlong_`useresults'_`var' if SPlong_d_`var' == `dval' & SPlong_e_`var' == `e', meanonly
				loc mn = r(mean)
				mata: mm = mm \ `mn'
			}
			mata: spresults = spresults , mm
			loc list = "`list' `var'"
		}
	}
}
mata: st_matrix("spresults", spresults)
matrix colnames spresults = `list'
putexcel A2 = matrix(spresults), names  
loc num = `emaximum'+2
loc num2 = `num'+1
putexcel A`num' = "Note: results are averaged across panels if running 'multiple EDM' with N>1"
putexcel A`num2' = "for additional information such as sample sizes or SDs see the SP variables saved to the data file"
}


* Save SP hypothesis test results
capture qui {
putexcel set EDMresults.xlsx, sheet("SP_test") modify
putexcel A1 = "SP hypothesis test for prediction accuracy based on `testval' against permuted/surrogate data replications as the null distribution"
putexcel A3 = "No results exist, check data and/or EDM input"
mata: SPtest = J(7,0,.)
loc list = ""
preserve
if "`testval'"=="rho" loc pctiles = "50, 95, 99"
else if "`testval'"=="mae" loc pctiles = "1, 5, 50"
foreach var of varlist `variables' {
	tempname SPpct`var'
	mata: `SPpct`var'' = J(0,5,.)
	loc `var'counter=0
	loc `var't95counter=0
	loc `var't99counter=0
	foreach id of numlist `panelrange' {
		if "`emax`var'`id''"!="" {
			if `emax`var'`id''>0 {
				loc `var'counter=``var'counter'+1
				capture {
					edm explore `prestd'd`diff'.`var' if `panelvar'==`id' ``var'dropzes', e(`emax`var'`id'') full `forcing' `time_gap' `missings' extra(`extras`diff'' `extras_raw') `tweight' `mdistance' reportrawe `options'
					mat `Amat' = e(explore_result)
					if "`testval'"=="rho" sca `Test1' = `Amat'[1,3]
					else if "`testval'"=="mae" sca `Test1' = `Amat'[1,4]
					_pctile SPtestl_`testval'_`var' if SPtestl_id_`var'==`id', percentiles(`pctiles')
					if "`testval'"=="rho" & `Test1'-r(r2) > 0 loc `var't95counter=``var't95counter'+1
					if "`testval'"=="rho" & `Test1'-r(r3) > 0 loc `var't99counter=``var't99counter'+1
					if "`testval'"=="mae" & `Test1'-r(r2) < 0 loc `var't95counter=``var't95counter'+1
					if "`testval'"=="mae" & `Test1'-r(r1) < 0 loc `var't99counter=``var't99counter'+1
					mata: `Test1'=st_numscalar("`Test1'")
					mata: `A' = J(1,4,`id')
					mata: `B' = (`pctiles', 110)
					mata: `C' = (st_numscalar("r(r1)"), st_numscalar("r(r2)"), st_numscalar("r(r3)"), `Test1')
					mata: `C2' = (`Test1'-st_numscalar("r(r1)"), `Test1'-st_numscalar("r(r2)"), `Test1'-st_numscalar("r(r3)"), 0)
					mata: `D' = J(1,4,st_numscalar("e(N)"))
					mata: `E' = `A'\ `B'\ `C'\ `C2'\ `D'
					mata: `SPpct`var''=`SPpct`var''\ `E''
				}
			}
		}
	}
	capture quietly {
		getmata (SPpct`var'*)=`SPpct`var'', force double
		label variable SPpct`var'1 "SP Percentile panel ID"
		rename SPpct`var'1 SPpct`var'
		label variable SPpct`var'2 "SP Percentile"
		rename SPpct`var'2 SPpct`var'_pctile
		label variable SPpct`var'3 "SP test values"
		rename SPpct`var'3 SPpct`var'_vals
		label variable SPpct`var'4 "SP test Values Minus Observed"
		rename SPpct`var'4 SPpct`var'_valsc
		label variable SPpct`var'5 "SP Percentile N"
		rename SPpct`var'5 SPpct`var'_N
	}
	capture {
		if "`testval'"=="rho" loc fivepct = 95
		else if "`testval'"=="mae" loc fivepct = 5
		if "`testval'"=="rho" loc onepct = 99
		else if "`testval'"=="mae" loc onepct = 1
		if "`testval'"=="rho" loc onesuffix = "th"
		else if "`testval'"=="mae" loc onesuffix = "st"
		if "`testval'" == "rho" loc z="&rho"
		else if "`testval'" == "mae" loc z="stSerif:MAE"

		loc pct95rnd = round(100*``var't95counter'/``var'counter', 1)
		loc pct95rnd2: di %3.0f `pct95rnd'
		loc pct99rnd = round(100*``var't99counter'/``var'counter', 1)
		loc pct99rnd2: di %3.0f `pct99rnd'

		sum SPpct`var'_vals if SPpct`var'_pctile==110, meanonly
		loc testvalGM=r(mean)
		loc `var'SPfullresult = r(mean)
		loc testvalGMr=round(`testvalGM',.001)
		loc testvalGMd: di %8.3f `testvalGMr'

		sum SPpct`var'_vals if SPpct`var'_pctile==`onepct', meanonly
		loc onepctGM=r(mean)
		loc onepctGMr=round(`onepctGM',.001)
		loc onepctGMd: di %8.3f `onepctGMr'

		sum SPpct`var'_vals if SPpct`var'_pctile==`fivepct' , meanonly
		loc fivepctGM=r(mean)
		loc fivepctGMr=round(`fivepctGM',.001)
		loc fivepctGMd: di %8.3f `fivepctGMr'

		sum SPpct`var'_vals if SPpct`var'_pctile==50, meanonly
		loc nullmeanGM=r(mean)
		loc nullmeanGMr=round(`nullmeanGM',.001)
		loc nullmeanGMd: di %6.3f `nullmeanGMr'
		}
		mata: mm = `testvalGMd' \ `nullmeanGMd' \ `fivepctGMd' \ `onepctGMd' \ `pct95rnd2' \ `pct99rnd2' \ ``var'counter'
		mata: SPtest = SPtest , mm
		loc list = "`list' `var'"
	}
mata: st_matrix("SPtest", SPtest)
matrix colnames SPtest = `list'
matrix rownames SPtest = "Observed `testval'" "Permuted mean" "Permuted `fivepct'%" "Permuted `onepct'%" "p<.05 percentage" "p<.01 percentage" "Number of panels"
putexcel A2 = matrix(SPtest), names  
putexcel A10 = "Note: the 'observed' and permuted SP values are for the entire sample using the 'full' option with leave-one-out cross-validation"
putexcel A11 = "The p<.05 and p<.01 percentages show what percentage of panel IDs reject this null H0" 
putexcel A12 = "So for N=1 or 'multispatial EDM' with N>1 cases this is either non-significant and 0 or significant and 100"
putexcel A13 = "Note also that for 'multiple EDM' with N>1 the coefficient percentiles are computed for each panel ID and are then averaged"
putexcel A14 = "For additional information see the permutation-based replications in SPtest variables"
restore
}


* Save tp results
capture qui {
putexcel set EDMresults.xlsx, sheet("TP") modify
putexcel A1 = "Time of prediction or tp results using `useresults'"
putexcel A3 = "No results exist, check data and/or EDM input"
mata: tpresults = 1::`TPmax'
loc list = "tp"
foreach var of varlist `variables' {
	capture confirm variable TPlong_`useresults'_`var'
	if !_rc {
		mata: mm = J(0,1,.)
		forval i=1/`TPmax' {
			qui sum TPlong_`useresults'_`var' if TPlong_tp_`var' == `i', meanonly
			loc mn = r(mean)
			mata: mm = mm \ `mn'
		}
		mata: tpresults
		mata: mm
		mata: tpresults = tpresults , mm
		loc list = "`list' `var'"
	}
}
mata: st_matrix("tpresults", tpresults)
matrix colnames tpresults = `list'
putexcel A2 = matrix(tpresults), names  
loc num = `TPmax'+3
putexcel A`num' = "Note: results are averaged across panels if running 'multiple EDM' with N>1, for additional information see the saved TP variables"
}


* Save S-map results
capture qui {
putexcel set EDMresults.xlsx, sheet("Smap") modify
putexcel A1 = "S-map results using `useresults' at different theta values"
putexcel A3 = "No results exist, check data and/or EDM input"
local numtemp: word count `theta'
mata: SMAPresults = J(0,1,.)
foreach i of numlist `theta' {
	mata: SMAPresults = SMAPresults \ `i'
}
loc list = "theta"
foreach var of varlist `variables' {
	capture confirm variable SMAPlong_`useresults'_`var'
	if !_rc {
		mata: mm = J(0,1,.)
		foreach i of numlist `theta' {
			qui sum SMAPlong_`useresults'_`var' if SMAPlong_theta_`var' == `i', meanonly
			loc mn = r(mean)
			mata: mm = mm \ `mn'
		}
		mata: SMAPresults = SMAPresults , mm
		loc list = "`list' `var'"
	}
}
mata: st_matrix("SMAPresults", SMAPresults)
matrix colnames SMAPresults = `list'
putexcel A2 = matrix(SMAPresults), names  
loc num = `numtemp'+3
loc num2 = `num'+1
putexcel A`num' = "Note: results are averaged across panels if running 'multiple EDM' with N>1, for additional information see the saved SMAP variables"
}


* Save S-map hypothesis test results
capture qui {
putexcel set EDMresults.xlsx, sheet("Smap_test") modify
putexcel A1 = "S-map hypothesis test for nonlinearity comparing theta=0 to optimal theta>0 based on `testval'"
putexcel A3 = "No results exist, check data and/or EDM input"
mata: SMAPtest = J(6,0,.)
loc list = ""
preserve
if "`testval'"=="rho" loc pctiles = "1, 5, 50"
else if "`testval'"=="mae" loc pctiles = "50, 95, 99"
foreach var of varlist `variables' {
	loc `var'counter=0
	loc `var't95counter=0
	loc `var't99counter=0
	tempname SMAPpct`var'
	tempvar SMAPtestval`var'
	mata: `SMAPpct`var'' = J(0,3,.)
	gen `SMAPtestval`var''=.
	capture {
		qui bysort SMAPtestl_id_`var' SMAPtestl_rep_`var' (SMAPtestl_theta_`var'): replace `SMAPtestval`var''=SMAPtestl_`testval'_`var'-SMAPtestl_`testval'_`var'[_n-1] if SMAPtestl_theta_`var' > 0 & SMAPtestl_theta_`var'[_n-1]==0 & SMAPtestl_theta_`var'!=.
	}
	foreach id of numlist `panelrange' {
		loc `var'counter=``var'counter'+1
		capture {
			_pctile `SMAPtestval`var'' if SMAPtestl_id_`var'==`id', percentiles(`pctiles')
			if "`testval'"=="rho" & r(r1) > 0 loc `var't99counter=``var't99counter'+1
			if "`testval'"=="rho" & r(r2) > 0 loc `var't95counter=``var't95counter'+1
			if "`testval'"=="mae" & r(r2) < 0 loc `var't95counter=``var't95counter'+1
			if "`testval'"=="mae" & r(r3) < 0 loc `var't99counter=``var't99counter'+1
			mata: `A' = J(1,3,`id')
			mata: `B' = (`pctiles')
			mata: `C' = (st_numscalar("r(r1)"), st_numscalar("r(r2)"), st_numscalar("r(r3)"))
			mata: `E' = `A'\ `B'\ `C'
			mata: `SMAPpct`var''=`SMAPpct`var''\ `E''
		}
	}
	drop `SMAPtestval`var''
	capture quietly {
		getmata (SMAPpct`var'*)=`SMAPpct`var'', force double
		label variable SMAPpct`var'1 "SMAP Percentile panel ID"
		rename SMAPpct`var'1 SMAPpct`var'
		label variable SMAPpct`var'2 "SMAP Percentile"
		rename SMAPpct`var'2 SMAPpct`var'_pctile
		label variable SMAPpct`var'3 "SMAP test values"
		rename SMAPpct`var'3 SMAPpct`var'_vals
	}
	capture {
		if "`testval'"=="mae" loc fivepct = 95
		else if "`testval'"=="rho" loc fivepct = 5
		if "`testval'"=="mae" loc onepct = 99
		else if "`testval'"=="rho" loc onepct = 1
		if "`testval'"=="mae" loc onesuffix = "th"
		else if "`testval'"=="rho" loc onesuffix = "st"
		if "`testval'" == "rho" loc z="&rho"
		else if "`testval'" == "mae" loc z="stSerif:MAE"

		loc pct95rnd = round(100*``var't95counter'/``var'counter', 1)
		loc pct95rnd2: di %3.0f `pct95rnd'
		loc pct99rnd = round(100*``var't99counter'/``var'counter', 1)
		loc pct99rnd2: di %3.0f `pct99rnd'

		sum SMAPpct`var'_vals if SMAPpct`var'_pctile==`onepct', meanonly
		loc onepctGM=r(mean)
		loc onepctGMr=round(`onepctGM',.0001)
		loc onepctGMd: di %9.4f `onepctGMr'

		sum SMAPpct`var'_vals if SMAPpct`var'_pctile==`fivepct' , meanonly
		loc fivepctGM=r(mean)
		loc fivepctGMr=round(`fivepctGM',.0001)
		loc fivepctGMd: di %9.4f `fivepctGMr'

		sum SMAPpct`var'_vals if SMAPpct`var'_pctile==50, meanonly
		loc nullmeanGM=r(mean)
		loc nullmeanGMr=round(`nullmeanGM',.0001)
		loc nullmeanGMd: di %9.4f `nullmeanGMr'
	}
	mata: mm = `nullmeanGMd' \ `fivepctGMd' \ `onepctGMd' \ `pct95rnd2' \ `pct99rnd2' \ ``var'counter'
	mata: SMAPtest = SMAPtest , mm
	loc list = "`list' `var'"
}
mata: st_matrix("SMAPtest", SMAPtest)
matrix colnames SMAPtest = `list'
matrix rownames SMAPtest = "Mean difference" "`fivepct'% difference" "`onepct'% difference" "p<.05 percentage" "p<.01 percentage" "Number of panels"
putexcel A2 = matrix(SMAPtest), names  
putexcel A9 = "Note: values are `testval' at best theta > 0 minus `testval' at theta = 0; for rho positive values support nonlinearity but for MAE negative values support nonlinearity"
putexcel A10 = "Note: the p<.05 and p<.01 percentages show what percentage of panel IDs reject this null H0" 
putexcel A11 = "For N=1 or 'multispatial EDM' cases this is either non-significant and 0 or significant and 100"
putexcel A12 = "For additional information see replications in SMAPtestl variables"
restore
}


* Save coprediction results as a square but non-symmetric correlation matrix
capture qui {
putexcel set EDMresults.xlsx, sheet("Copredict") modify
putexcel A1 = "Coprediction results as a square but non-symmetric matrix of `useresults' values"
putexcel A3 = "No results exist, check data and/or EDM input"
local numtemp: word count `variables'
mata: Copred = J(`numtemp',`numtemp',.)
forval n=1/`numtemp' {
	forval m=1/`numtemp' {
		qui sum CoPredLong_`useresults' if CoPredLong_Predictor == `n' & CoPredLong_Predictee == `m', meanonly
		loc mn = r(mean)
		mata: Copred[`m',`n']=`mn'
	}
}
mata: st_matrix("Copred", Copred)
matrix colnames Copred = `variables'
matrix rownames Copred = `variables'
putexcel A2 = matrix(Copred), names
loc num = `numtemp'+3
loc num2 = `num'+1
putexcel A`num' = "Note: results are averaged across panels if running 'multiple EDM' with N>1, for additional information see saved CoPred variables"
putexcel A`num2' = "Predictor is columns and predictees are row"
}


* Save CCM results as a matrix (predictors are columns, outcomes are rows) with Simplex Projection results on the diagonal
capture qui {
preserve
gen long `obsnO'=_n
putexcel set EDMresults.xlsx, sheet("CMM_results_matrix") modify
putexcel A1 = "CCM coprediction results as a square but non-symmetric matrix of `useresults' values, with SP results on diagonal"
putexcel A3 = "No results exist, check data and/or EDM input"
local numtemp: word count `variables'
mata: CCM_matrix = J(`numtemp',`numtemp',.)
loc n = 0
foreach var1 of varlist `variables' {
	loc n = `n'+1
	loc m = 0
	foreach var2 of varlist `variables' {
		loc m = `m'+1
		capture confirm variable CCM_`useresults'_`var1'_`var2'
		if !_rc {
* Find max library size L for each panel ID and each lag, so only max L results are plotted
			gen temp12`var1'`var2'lagz00 = .
			foreach id of numlist `panelrange' {
				summarize CCM_L_`var1'_`var2' if CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2'), meanonly
				loc levelz=r(max)
				levelsof `obsnO' if CCM_L_`var1'_`var2' == `levelz' & CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0
				replace temp12`var1'`var2'lagz00 = CCM_`testval'_`var1'_`var2' if _n ==`r(levels)'
			}
			sum temp12`var1'`var2'lagz00
			loc mn = r(mean)
			mata: CCM_matrix[`m',`n']=`mn'
		}
		else if `m' == `n' {
			loc mn = ``var2'SPfullresult'
			mata: CCM_matrix[`m',`n']=`mn'
		}
	}
}
mata: st_matrix("CCM_matrix", CCM_matrix)
matrix colnames CCM_matrix = `variables'
matrix rownames CCM_matrix = `variables'
putexcel A2 = matrix(CCM_matrix), names  
loc num = `numtemp'+3
loc num2 = `num'+1
loc num3 = `num'+2
putexcel A`num' = "Cause is columns and outcomes are rows; for comparison the on-diagonal elements are SP results for full sample (variable predicts its own future)"
putexcel A`num2' = "Note: results are averaged across panels if running 'multiple EDM' with N>1"
putexcel A`num3' = "Note: For additional information see the CCM variables saved to the data file including CCM_lag variables"
restore
}


* Save CCM results at the requested lags and leads
capture qui {
preserve
gen long `obsnO'=_n
loc numtemp = `CCMlags'+`CCMleads'+1
loc list = "Lag"
mata: CCMlags = -`CCMlags'::`CCMleads'
putexcel set EDMresults.xlsx, sheet("CCM_lags") modify
putexcel A1 = "CCM prediction accuracy using `useresults' at the requested lag levels (negative lags imply an impossible backwards in time cause-effect relationship)"
putexcel A3 = "No results exist, check data and/or EDM input"
foreach var1 of varlist `variables' {
	foreach var2 of varlist `variables' {
		capture confirm variable CCM_`useresults'_`var1'_`var2'
		if !_rc {
* Find max library size L for each panel ID and each lag, so only max L results are plotted
			gen temp12`var1'`var2'lagz00 = .
			mata: mm = J(0,1,.)
			foreach id of numlist `panelrange' {
				summarize CCM_L_`var1'_`var2' if CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2'), meanonly
				loc levelz=r(max)
				levelsof `obsnO' if CCM_L_`var1'_`var2' == `levelz' & CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0
				replace temp12`var1'`var2'lagz00 = CCM_`testval'_`var1'_`var2' if _n ==`r(levels)'
			}
			forval i = -`CCMlags'(1)`CCMleads' {
				if `i' == 0 {
					sum temp12`var1'`var2'lagz00
					loc mn = r(mean)
					mata: mm = mm \ `mn'
				}
				else if `i'!=0 {
					capture quietly sum CCM_`testval'_`var1'_`var2' if CCM_lag_`var1'_`var2'==`i'
					loc mn = r(mean)
					mata: mm = mm \ `mn'
				}
			}
			mata: CCMlags
			mata: mm
			mata: CCMlags = CCMlags , mm
			loc list = "`list' `var1'_cause_`var2'"
		}
	}					
}
mata: st_matrix("CCMlags", CCMlags)
matrix colnames CCMlags = `list'
putexcel A2 = matrix(CCMlags), names  
loc num = `numtemp'+3
loc num2 = `num'+1
loc num3 = `num'+2
loc num4 = `num'+3
putexcel A`num' = "Note: For additional information see the CCM variables saved to the data file including CCM_lag variables"
putexcel A`num2' = "Positive lags are forward in time, so should make worse predictions, but if prediction improves this implies non-causal relationships"
putexcel A`num3' = "See Ye et al. (2015) Distinguishing time-delayed causal interactions using convergent cross mapping"
restore
}


* Save CCM hypothesis test of convergence using replications option
capture qui {
putexcel set EDMresults.xlsx, sheet("CCM_ctest") modify
putexcel A1 = "CCM hypothesis test for convergence from minimum library size L using `testval' with `CCMctestreps' replications"
putexcel A3 = "No results exist, perhaps due to optimal predictions occuring at a forward lag with StepwiseCCMtest=yes"
mata: ctest = J(7,0,.)
loc list = ""
preserve
gen long `obsnO'=_n
if "`testval'"=="rho" loc pctiles = "50, 95, 99"
else if "`testval'"=="mae" loc pctiles = "1, 5, 50"
foreach var1 of varlist `variables' {
	foreach var2 of varlist `variables' {
		capture confirm variable CCMctest_id_`var1'_`var2'
		if !_rc {
			tempname CCML2pct`var1'`var2'
			mata: `CCML2pct`var1'`var2'' = J(0,5,.)
			loc `var1'`var2'tcounter=0
			loc `var1'`var2't95counter=0
			loc `var1'`var2't99counter=0
			quietly levelsof CCMctest_id_`var1'_`var2', local(levels)
			foreach id of local levels {
				loc `var1'`var2'tcounter=``var1'`var2'tcounter'+1
				tempname CCML2`var1'`var2'`id'
				capture quietly {
					summarize CCM_L_`var1'_`var2' if CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2'), meanonly
					loc levelz=r(max)
					levelsof `obsnO' if CCM_L_`var1'_`var2' == `levelz' & CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2')
					sca `CCML2`var1'`var2'`id''=CCM_`testval'_`var1'_`var2'[`r(levels)']
				}
				capture {
					_pctile CCMctest_`testval'_`var1'_`var2' if CCMctest_id_`var1'_`var2'==`id', percentiles(`pctiles')
					if "`testval'"=="rho" & `CCML2`var1'`var2'`id''-r(r2) > 0 loc `var1'`var2't95counter=``var1'`var2't95counter'+1
					if "`testval'"=="rho" & `CCML2`var1'`var2'`id''-r(r3) > 0 loc `var1'`var2't99counter=``var1'`var2't99counter'+1
					if "`testval'"=="mae" & `CCML2`var1'`var2'`id''-r(r2) < 0 loc `var1'`var2't95counter=``var1'`var2't95counter'+1
					if "`testval'"=="mae" & `CCML2`var1'`var2'`id''-r(r1) < 0 loc `var1'`var2't99counter=``var1'`var2't99counter'+1
					mata: `CCML2`var1'`var2'`id''=st_numscalar("`CCML2`var1'`var2'`id''")
					mata: `A' = J(1,4,`id')
					mata: `B' = (`pctiles', 110)
					mata: `C' = (st_numscalar("r(r1)"), st_numscalar("r(r2)"), st_numscalar("r(r3)"), `CCML2`var1'`var2'`id'')
					mata: `C2' = (`CCML2`var1'`var2'`id''-st_numscalar("r(r1)"), `CCML2`var1'`var2'`id''-st_numscalar("r(r2)"), `CCML2`var1'`var2'`id''-st_numscalar("r(r3)"), 0)
					capture confirm scalar e(N)
					if !_rc {
						mata: `D' = J(1,4,st_numscalar("e(N)"))
					}
					else {
						mata: `D' = J(1,4,.)
					}
					mata: `E' = `A'\ `B'\ `C'\ `C2'\ `D'
					mata: `CCML2pct`var1'`var2''=`CCML2pct`var1'`var2''\ `E''
				}
			}
			capture {
				getmata (CCML2pct`var1'`var2'*)=`CCML2pct`var1'`var2'', force double
				label variable CCML2pct`var1'`var2'1 "CCM convergence test Percentile panel ID"
				rename CCML2pct`var1'`var2'1 CCML2pct`var1'`var2'
				label variable CCML2pct`var1'`var2'2 "CCM convergence test Percentile"
				rename CCML2pct`var1'`var2'2 CCML2pct`var1'`var2'_pctile
				label variable CCML2pct`var1'`var2'3 "CCM convergence test values"
				rename CCML2pct`var1'`var2'3 CCML2pct`var1'`var2'_vals
				label variable CCML2pct`var1'`var2'4 "CCM convergence test Values Minus Observed"
				rename CCML2pct`var1'`var2'4 CCML2pct`var1'`var2'_valsc
				label variable CCML2pct`var1'`var2'5 "CCM convergence test Percentile N"
				rename CCML2pct`var1'`var2'5 CCML2pct`var1'`var2'_N
			}
			capture {
				if "`testval'"=="rho" loc fivepct = 95
				else if "`testval'"=="mae" loc fivepct = 5
				if "`testval'"=="rho" loc onepct = 99
				else if "`testval'"=="mae" loc onepct = 1
				if "`testval'"=="rho" loc onesuffix = "th"
				else if "`testval'"=="mae" loc onesuffix = "st"
				if "`testval'" == "rho" loc z="&rho"
				else if "`testval'" == "mae" loc z="stSerif:MAE"

				loc pct95rnd = round(100*``var1'`var2't95counter'/``var1'`var2'tcounter', 1)
				loc pct95rnd2: di %3.0f `pct95rnd'
				loc pct99rnd = round(100*``var1'`var2't99counter'/``var1'`var2'tcounter', 1)
				loc pct99rnd2: di %3.0f `pct99rnd'

				sum CCML2pct`var1'`var2'_vals if CCML2pct`var1'`var2'_pctile==110, meanonly
				loc testvalGM=r(mean)
				loc testvalGMr=round(`testvalGM',.001)
				loc testvalGMd: di %8.3f `testvalGMr'

				sum CCML2pct`var1'`var2'_vals if CCML2pct`var1'`var2'_pctile==`onepct', meanonly
				loc onepctGM=r(mean)
				loc onepctGMr=round(`onepctGM',.001)
				loc onepctGMd: di %8.3f `onepctGMr'

				sum CCML2pct`var1'`var2'_vals if CCML2pct`var1'`var2'_pctile==`fivepct' , meanonly
				loc fivepctGM=r(mean)
				loc fivepctGMr=round(`fivepctGM',.001)
				loc fivepctGMd: di %8.3f `fivepctGMr'

				sum CCML2pct`var1'`var2'_vals if CCML2pct`var1'`var2'_pctile==50, meanonly
				loc nullmeanGM=r(mean)
				loc nullmeanGMr=round(`nullmeanGM',.001)
				loc nullmeanGMd: di %6.3f `nullmeanGMr'
			}
		mata: mm = `testvalGMd' \ `nullmeanGMd' \ `fivepctGMd' \ `onepctGMd' \ `pct95rnd2' \ `pct99rnd2' \ ``var1'`var2'tcounter'
		mata: ctest = ctest , mm
		loc list = "`list' `var1'_cause_`var2'"
		}
	}
}
mata: st_matrix("ctest", ctest)
matrix colnames ctest = `list'
matrix rownames ctest = "Observed `testval'" "Replicated mean" "Replicated `fivepct'%" "Replicated `onepct'%" "p<.05 percentage" "p<.01 percentage" "Number of panels"
putexcel A2 = matrix(ctest), names  
putexcel A10 = "Note: the p<.05 and p<.01 percentages show what percentage of panel IDs reject this null H0" 
putexcel A11 = "For N=1 or 'multispatial EDM' cases this is either non-significant and 0 or significant and 100"
putexcel A12 = "Note also that for 'multiple EDM' with N>1 the coefficient percentiles are computed for each panel ID and are then averaged"
putexcel A13 = "For additional information see replications in CCMctest variables"
restore
}


* Save hypothesis test of rho or mae (`testval') at max L
capture qui {
putexcel set EDMresults.xlsx, sheet("CCM_Ltest") modify
putexcel A1 = "CCM hypothesis test for `testval' at maximum library size L test using `CCMLtestreps' permuted/surrogate data replications"
putexcel A3 = "No results exist, perhaps due to previous hypothesis tests being non-significant with StepwiseCCMtest=yes"
mata: Ltest = J(7,0,.)
loc list = ""
preserve
gen long `obsnO' = _n
if "`testval'"=="rho" loc pctiles = "50, 95, 99"
else if "`testval'"=="mae" loc pctiles = "1, 5, 50"
foreach var1 of varlist `variables' {
	foreach var2 of varlist `variables' {
		capture confirm variable CCMLtest_id_`var1'_`var2'
		if !_rc {
			mata: mm = J(0,1,.)
			tempname CCMLpct`var1'`var2'
			mata: `CCMLpct`var1'`var2'' = J(0,5,.)
			loc `var1'`var2'tcounter=0
			loc `var1'`var2't95counter=0
			loc `var1'`var2't99counter=0
			quietly levelsof CCMLtest_id_`var1'_`var2', local(levels)
			foreach id of local levels {
				quietly summarize CCM_L_`var1'_`var2' if CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0 & !missing(CCM_`testval'_`var1'_`var2'), meanonly
				loc levelz=r(max)
				levelsof `obsnO' if CCM_L_`var1'_`var2' == `levelz' & CCM_id_`var1'_`var2'==`id' & CCM_lag_`var1'_`var2'==0
				tempname CCML`var1'`var2'`id'
				sca `CCML`var1'`var2'`id''=CCM_`testval'_`var1'_`var2'[`r(levels)']
				loc `var1'`var2'tcounter=``var1'`var2'tcounter'+1
				_pctile CCMLtest_`testval'_`var1'_`var2' if CCMLtest_id_`var1'_`var2'==`id', percentiles(`pctiles')							
				if "`testval'"=="rho" & `CCML`var1'`var2'`id''-r(r2) > 0 loc `var1'`var2't95counter=``var1'`var2't95counter'+1
				if "`testval'"=="rho" & `CCML`var1'`var2'`id''-r(r3) > 0 loc `var1'`var2't99counter=``var1'`var2't99counter'+1
				if "`testval'"=="mae" & `CCML`var1'`var2'`id''-r(r2) < 0 loc `var1'`var2't95counter=``var1'`var2't95counter'+1
				if "`testval'"=="mae" & `CCML`var1'`var2'`id''-r(r1) < 0 loc `var1'`var2't99counter=``var1'`var2't99counter'+1
				mata: `CCML`var1'`var2'`id''=st_numscalar("`CCML`var1'`var2'`id''")
				mata: `A' = J(1,4,`id')
				mata: `B' = (`pctiles', 110)
				mata: `C' = (st_numscalar("r(r1)"), st_numscalar("r(r2)"), st_numscalar("r(r3)"), `CCML`var1'`var2'`id'')
				mata: `C2' = (`CCML`var1'`var2'`id''-st_numscalar("r(r1)"), `CCML`var1'`var2'`id''-st_numscalar("r(r2)"), `CCML`var1'`var2'`id''-st_numscalar("r(r3)"), 0)
				capture confirm scalar e(N)
				if !_rc {
					mata: `D' = J(1,4,st_numscalar("e(N)"))
				}
				else {
					mata: `D' = J(1,4,.)
				}
				mata: `E' = `A'\ `B'\ `C'\ `C2'\ `D'
				mata: `CCMLpct`var1'`var2''=`CCMLpct`var1'`var2''\ `E''
			}
			capture quietly {
				getmata (CCMLpct`var1'`var2'*)=`CCMLpct`var1'`var2'', force double
				label variable CCMLpct`var1'`var2'1 "CCM max L test Percentile panel ID"
				rename CCMLpct`var1'`var2'1 CCMLpct`var1'`var2'
				label variable CCMLpct`var1'`var2'2 "CCM max L test Percentile"
				rename CCMLpct`var1'`var2'2 CCMLpct`var1'`var2'_pctile
				label variable CCMLpct`var1'`var2'3 "CCM max L test values"
				rename CCMLpct`var1'`var2'3 CCMLpct`var1'`var2'_vals
				label variable CCMLpct`var1'`var2'4 "CCM max L test Values Minus Observed"
				rename CCMLpct`var1'`var2'4 CCMLpct`var1'`var2'_valsc
				label variable CCMLpct`var1'`var2'5 "CCM max L Percentile N"
				rename CCMLpct`var1'`var2'5 CCMLpct`var1'`var2'_N
			}
			capture {
				if "`testval'"=="rho" loc fivepct = 95
				else if "`testval'"=="mae" loc fivepct = 5
				if "`testval'"=="rho" loc onepct = 99
				else if "`testval'"=="mae" loc onepct = 1
				if "`testval'"=="rho" loc onesuffix = "th"
				else if "`testval'"=="mae" loc onesuffix = "st"
				if "`testval'" == "rho" loc z="&rho"
				else if "`testval'" == "mae" loc z="stSerif:MAE"

				loc pct95rnd = round(100*``var1'`var2't95counter'/``var1'`var2'tcounter', 1)
				loc pct95rnd2: di %3.0f `pct95rnd'
				loc pct99rnd = round(100*``var1'`var2't99counter'/``var1'`var2'tcounter', 1)
				loc pct99rnd2: di %3.0f `pct99rnd'

				sum CCMLpct`var1'`var2'_vals if CCMLpct`var1'`var2'_pctile==110, meanonly
				loc testvalGM=r(mean)
				loc testvalGMr=round(`testvalGM',.001)
				loc testvalGMd: di %8.3f `testvalGMr'

				sum CCMLpct`var1'`var2'_vals if CCMLpct`var1'`var2'_pctile==`onepct', meanonly
				loc onepctGM=r(mean)
				loc onepctGMr=round(`onepctGM',.001)
				loc onepctGMd: di %8.3f `onepctGMr'

				sum CCMLpct`var1'`var2'_vals if CCMLpct`var1'`var2'_pctile==`fivepct' , meanonly
				loc fivepctGM=r(mean)
				loc fivepctGMr=round(`fivepctGM',.001)
				loc fivepctGMd: di %8.3f `fivepctGMr'

				sum CCMLpct`var1'`var2'_vals if CCMLpct`var1'`var2'_pctile==50, meanonly
				loc nullmeanGM=r(mean)
				loc nullmeanGMr=round(`nullmeanGM',.001)
				loc nullmeanGMd: di %6.3f `nullmeanGMr'
			}
			mata: mm = `testvalGMd' \ `nullmeanGMd' \ `fivepctGMd' \ `onepctGMd' \ `pct95rnd2' \ `pct99rnd2' \ ``var1'`var2'tcounter'
			mata: Ltest = Ltest , mm
			loc list = "`list' `var1'_cause_`var2'"
		}
	}
}
mata: st_matrix("Ltest", Ltest)
matrix colnames Ltest = `list'
matrix rownames Ltest = "Observed `testval'" "Permuted mean" "Permuted `fivepct'%" "Permuted `onepct'%" "p<.05 percentage" "p<.01 percentage" "Number of panels"
putexcel A2 = matrix(Ltest), names  
putexcel A10 = "Note: the p<.05 and p<.01 percentages show what percentage of panel IDs reject this null H0" 
putexcel A11 = "For N=1 or 'multispatial EDM' cases this is either non-significant and 0 or significant and 100"
putexcel A12 = "Note also that for 'multiple EDM' with N>1 the coefficient percentiles are computed for each panel ID and are then averaged"
putexcel A13 = "For additional information see the permutation-based replications in CCMLtest variables"
putexcel A14 = "If StepwiseCCMtest=yes then sample sizes can be smaller compared to convergence test, thus average `testval' reported may differ"
restore
}


* Save average CCM SMAP Results
capture qui {
putexcel set EDMresults.xlsx, sheet("CCM_SMAP") modify
putexcel A1 = "CCM S-map coefficients averaged across time with 5th and 95th percentiles, mean, and SD values"
putexcel A3 = "No results exist, perhaps due to previous hypothesis tests being non-significant with StepwiseCCMtest=yes"
loc list = "Lag"
loc j = 0
loc jj = 0
foreach var1 of varlist `variables' {
	quietly summarize SMAP_e_`var1', meanonly
	if `jj' < r(max) loc jj = r(max)
	foreach var2 of varlist `variables' {
		capture confirm variable CCM_SMAP_`var1'_`var2'_b0
		if !_rc {
			quietly summarize CCMLtest_e_`var1'_`var2', meanonly
			loc CCMLeval`var1'`var2' = r(max)
			if `j' < r(max) loc j = r(max)
		}
	}
}
mata: CCMSMAPresults = 0::`jj'-1
mata: CCMSMAPresults[1,1]=0
foreach var1 of varlist `variables' {
	foreach var2 of varlist `variables' {
		loc h = 0
		mata: mm = J(0,4,.)
		capture confirm variable CCM_SMAP_`var1'_`var2'_b0
		if !_rc {
			forval i=1/`j' {
				if `CCMLeval`var1'`var2'' >= `i' {
					loc labeldt : variable label CCM_SMAP_`var1'_`var2'_b`i'
					loc first=substr("`labeldt'",1,2)
					if `i' == 1 | (`i' > `extraz'+1 & "`first'"!="dt") {
						loc h = `h'+1
						qui sum CCM_SMAP_`var1'_`var2'_b`i', detail
						loc fpct = r(p5)
						loc mn = r(mean)
						loc npct = r(p95)
						loc sd = r(sd)
						mata: mm
						mata: mm = mm \ `fpct' , `mn' , `npct' , `sd'
						mata: mm
					}
				}
			}
			if `h' < `jj' { 
				loc bb = `jj' - `h'
				forval i= 1/`bb' {
					mata: mm = mm \ .,.,.,.
				}
			}
			mata: CCMSMAPresults = CCMSMAPresults , mm
			loc list = "`list' `var1'_cause_`var2'_5% `var1'_cause_`var2'_mean `var1'_cause_`var2'_95% `var1'_cause_`var2'_SD"
		}
	}
}
mata: st_matrix("CCMSMAPresults", CCMSMAPresults)
matrix colnames CCMSMAPresults = `list'
matrix colnames CCMSMAPresults = `list'
putexcel A2 = matrix(CCMSMAPresults), names  
loc num = `jj'+3
putexcel A`num' = "Note: For additional information see the CCM_SMAP variables saved to the data file"
}
}
/*
di "************************************************************************************"
di "*   RESULTS ARE SAVED TO FILE EDMresults.xlsx                                      *"
di "*   FOR ADDITIONAL INFO/RESULTS SEE NEW SAVED VARIABLES                            *"
di "*   SORRY FOR ALL THE PLOTS -- I GOT A BIT CARRIED AWAY                            *"
di "*   ALSO PLEASE CALL YOUR ELECTED OFFICIALS TO VOICE CONCERNS ABOUT GLOBAL WARMING *"
di "************************************************************************************"
*/
