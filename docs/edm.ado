*! version 1.5.0, 01Jun2021, Jinjing Li, Michael Zyphur, George Sugihara, Edoardo Tescari, Patrick J. Laub
*! conact: <jinjing.li@canberra.edu.au>

global EDM_VERSION = "1.5.0"
/* Empirical dynamic modelling

Version history:
* 29/10/2020: suppress some debug information and fix a bug on copredict with dt
* 13/10/2020: speed improvement on matrix copying
* 15/9/2020: add plugin support
* 15/8/2020: fix a bug in the default number of k in smap and labelling
* 29/5/2020: robustness improvement
* 26/5/2020: update ci() return format and add pc tile values
* 26/5/2020: return e(ub_rho1) e(lb_rho1) etc for the ci() estimates
* 19/5/2020: Update s-map / llr algorithm
* 13/5/2020: bug fix in coprediction
* 22/4/2020: allow copredict() together with dt
* 26/3/2020: remove omitted coefficients instead of reporting as 0
* 17/2/2020: robustness improvement when dt and allowmissing are specified at the same time
* 15/2/2020: improve robustness for estimations using data with missing identifier
* 4/2/2020: update smap coefficient label
* 27/1/2020: report actual E used, add "reportrawe" option
* 23/1/2020: bug fix to disallow constant in extras
* 16/1/2020: update default distance
* 16/1/2020: allow error recovery
* 9/1/2020: allow missing data to be included in the calculation
* 9/1/2020: allow automatic time difference in the embedding via dt option
* 26/11/2019: extra embedding option for both explore and xmap
* 17/11/2019: fix a bug where the missing values of the second variables are not detected in the data cleaning process
* 2/11/2019: add -force- option, improve error message for xmap
* 24/10/2019: speed improvement for duplicate values
* 24/10/2019: bug fix on develop update
* 28/9/2019: bug fixes in explore mode
* 10/7/2019: coprediction and many others including an improvement of nearest non-overlap neighbour search
* 15/6/2019: Many new features
* 12/3/2019: labelling of the savesmap

 */

/* global EDM_DEBUG = 0 */

program define edm, eclass sortpreserve
	version 14

	if replay() {
		if (`"`e(cmd)'"' != "edm") {
			noi di as error "results for edm not found"
			exit 301
		}
		edmDisplay `0'
		exit `rc'
	}
	else edmParser `0'
end

program define edmDisplayCI, rclass
	syntax ,  mat(name) ci(integer) [maxr(integer 2)]
	quietly {
		noi di as result %18s "Est. mean `ci'% CI" _c

		if `maxr' == 1 {
			noi di as result %17s " " _c
		}
		local datasize = r(N)
		tempname varbuffer
		/* noi mat list `mat' */
		svmat `mat',names(`varbuffer'_ci`ci'_)

		local type1 "rho"
		local type2 "mae"

		forvalues j=1/`maxr' {
			cap ci `varbuffer'_ci`ci'_`j', level(`ci')
			if _rc !=0 {
				cap ci means `varbuffer'_ci`ci'_`j', level(`ci')
			}
			return scalar lb_mean_`type`j'' = `=r(lb)'
			return scalar ub_mean_`type`j'' = `=r(ub)'

			noi di as result "   [" _c

			/* noi di as result  %9.5g  `=`matmean'[1,`j']-invnormal(1-(100-`ci')/200)*`matsd'[1,`j']/sqrt(`rep')' _c */
			noi di as result  %9.5g  `=r(lb)' _c
			noi di as result ", " _c

			/* noi di as result  %9.5g  `=`matmean'[1,`j']+invnormal(1-(100-`ci')/200)*`matsd'[1,`j']/sqrt(`rep')' _c */
			noi di as result  %9.5g  `=r(ub)' _c

			noi di as result " ]" _c


		}
		noi qui count
		local datasize = r(N)
		/* mat list `buffer' */
		/* set trace on */
		noi di ""
		noi di as result %18s "`=(100-`ci')/2'/`=100 - (100-`ci')/2' Pc (Est.)" _c
		forvalues j=1/`maxr' {
			if `maxr' == 1 {
				noi di as result %17s " " _c
			}
			qui sum `varbuffer'_ci`ci'_`j'
			noi di as result "   [" _c
			noi di as result %9.5g `=r(mean)-invnormal(1-(100-`ci')/200)*r(sd)' _c
			return scalar lb_pce_`type`j'' = `=r(mean)-invnormal(1-(100-`ci')/200)*r(sd)'
			noi di as result ", " _c
			noi di as result %9.5g `=r(mean)+invnormal(1-(100-`ci')/200)*r(sd)' _c
			return scalar ub_pce_`type`j'' = `=r(mean)+invnormal(1-(100-`ci')/200)*r(sd)'
			noi di as result " ]" _c
		}

		noi di ""
		noi di as result %18s "`=(100-`ci')/2'/`=100 - (100-`ci')/2' Pc (Obs.)" _c
		forvalues j=1/`maxr' {

			if `maxr' == 1 {
				noi di as result %17s " " _c
			}
			_pctile `varbuffer'_ci`ci'_`j', percentile(`=(100-`ci')/2' `=100 - (100-`ci')/2' )
			noi di as result "   [" _c
			noi di as result %9.5g `=r(r1)' _c
			return scalar lb_pco_`type`j'' = `=r(r1)'
			noi di as result ", " _c
			noi di as result %9.5g `=r(r2)' _c
			return scalar ub_pco_`type`j'' = `=r(r2)'
			noi di as result " ]" _c
			drop `varbuffer'_ci`ci'_`j'
		}

		cap drop `varbuffer'_ci`ci'_*
		qui keep if _n<=`datasize'
		noi di ""
	}

end

program define edmParser, eclass

	/* syntax anything(id="subcommand or variables" min=2 max=3)  [if], [e(numlist ascending)] [theta(numlist ascending)] [manifold(string)] [converge] */

	* identify subcommand
	local subcommand "`1'"
	if strpos("`subcommand'",",") !=0 {
		local subcommand =substr("`1'",1,strpos("`subcommand'",",")-1)
	}
	local subargs = substr("`0'", strlen("`subcommand'")+1,.)

	if "`subcommand'" == "update" {
		edmUpdate `subargs'
	}
	else if "`subcommand'" == "version" {
		edmVersion `subargs'
	}
	else {
		qui xtset
		local original_t = r(timevar)
		if "`=r(panelvar)'" == "." {
			local original_id = ""
		}
		else {
			local original_id = r(panelvar)
		}

		ereturn clear
		if inlist("`subcommand'","explore","xmap") {
			cap noi {
				nobreak {
					if "`subcommand'" == "explore" {
						edmExplore `subargs'
					}
					else if "`subcommand'" == "xmap" {
						edmXmap `subargs'
					}
				}
			}
			if _rc !=0 {
				local error_code = _rc
				cap xtset
				if _rc !=0 {
					if "`original_id'" != ""{
						qui xtset `original_id' `original_t'
					}
					else {
						qui tsset `original_t'
					}
				}
				cap error `error_code'
				/* di as text "r(`error_code');" */
				exit(`error_code')
			}

		}
		else {
			di as error `"Invalid subcommand. Use "explore" or "xmap""'
			error 1
		}
		ereturn local cmd "edm"
		ereturn local cmdline `"edm `0'"'
	}
end

program define edmUpdate
	syntax , [DEVELOPment] [replace]
	if "`development'" == "development" {
		di "Updating edm from the development channel"
		net install edm, from("https://raw.githubusercontent.com/EDM-Developers/edm-releases/master/") `replace'
	}
	else {
		di "Updating edm from SSC"
		ssc install edm, `replace'
	}
	discard

end

program define edmVersion
	syntax , [test]
	dis "${EDM_VERSION}"
end


program define edmPluginCheck, rclass
	syntax , [mata]
	if "${EDM_MATA}" == "1" | "`mata'" =="mata" {
		return scalar mata_mode = 1

	}
	else {
		cap edm_plugin
		if _rc == 199 {
			di as text "Warning: Using slow (mata) edm implementation (failed to load the compiled plugin)"
		}
		return scalar mata_mode = _rc==199
	}
end

/*
program define edmCoremap, eclass
	syntax anything  [if], [e(integer 2)] [theta(real 1)] [k(integer 0)] [library(integer 0)] [seed(integer 0)] [ALGorithm(string)] [tau(integer 1)] [DETails] [Predict(name)] [tp(integer 1)] [COPredict(name)] [copredictvar(string)] [full] [force] [EXTRAembed(string)] [ALLOWMISSing] [MISSINGdistance(real 0)] trainset(string) predictset(string)
 */

program define hasMissingValues
	syntax [anything] , out(name)
	qui gen byte `out' = 0

	if "`anything'" != "" {
		tsunab varlist : `anything'

		foreach var in `varlist' {
			qui replace `out' = 1 if `var' == .
		}
	}
end

program define edmPreprocessVariable
	syntax anything , touse(name) out(name)

	if substr("`1'", 1, 2) == "z." {
		local unnormalized = substr("`1'", 3, .)
		qui egen `out' = std(`unnormalized') if `touse'
	}
	else {
		qui gen double `out' = `1'
	}
end

program define edmManifoldSize, rclass
	syntax , e(int) dt(int) dt0(int) num_extras(int) [num_eextras(int 0)]
	local num_xs = 1 + `e'-1
	local num_dts = `dt' * (`dt0' + `e'-1)
	local total_num_extras = `num_extras' + `num_eextras'*(`e'-1)
	return local total_num_extras = `total_num_extras'
	return local manifold_size = `num_xs' + `num_dts' + `total_num_extras'
end

program define edmConstructManifolds, rclass
	syntax anything , x(name) touse(name) [dt_value(name)] ///
			[z_vars(string)] [z_e_varying(string)] ///
			max_e(int) tau(int) dt(int) dt0(int) [dtw(real 0)]

	// Generate lags for 'x' data
	local manifold_index = 0
	forvalues i=0/`=`max_e'-1' {
		local x_`i' = "``++manifold_index''"
		qui gen double `x_`i'' = l`=`i'*`tau''.`x' if `touse'
	}

	// Generate lags for the 'dt' if requested
	if `dt' {
		forvalues i=`=(1 - `dt0')'/`=`max_e'-1' {
			local t_`i' = "``++manifold_index''"
			qui gen double `t_`i'' = `=cond(`i'==0, "f", "l`=`i'-1'")'.`dt_value' * `dtw' if `touse'
		}
	}

	// Generate extra variables & their lags if requested
	local z_count = wordcount("`z_vars'")
	forvalues k=1/`z_count' {
		local z_k : word `k' of `z_vars'
		local z_k_varying : word `k' of `z_e_varying'

		forvalues i=0/`=`z_k_varying'*(`max_e'-1)' {
			local z_`k'_`i' = "``++manifold_index''"
			qui gen double `z_`k'_`i'' = l`=`i'*`tau''.`z_k' if `touse'
		}
	}

	// Put the manifolds together
	forvalues i=1/`=`max_e'-1' {
		forvalues j=0/`i' {
			local mapping_`i' = "`mapping_`i'' `x_`j''"
		}
		forvalues j=0/`i' {
			local mapping_`i' = "`mapping_`i'' `t_`j''"
		}

		forvalues k=1/`z_count' {
			forvalues j=0/`i' {
				local mapping_`i' = "`mapping_`i'' `z_`k'_`j''"
			}
		}
		return local mapping_`i' = "`mapping_`i''"
	}

	local max_e_manifold = strtrim("`mapping_`=`max_e'-1''")
	return local max_e_manifold = "`max_e_manifold'"
end

program define edmCountExtras, rclass
	syntax [anything]

	local extravars = strtrim("`anything'")
	local z_names = ""
	local z_count = 0
	local z_e_varying = ""
	local z_e_varying_count = 0

	foreach v of local extravars {
		local z_prefix = strpos("`v'", "z.")
		if `z_prefix' {
			if `z_prefix' > 1 {
				noi di as error "Extra '`v'' must have 'z.' prefix come first"
				error 198
			}
			local v = substr("`v'",3,.)
		}

		local e_varying = strpos("`v'", "(e)")
		if `e_varying' {
			local suffix_ind = strlen("`v'")-3+1
			if `e_varying' != `suffix_ind' {
				noi di as error "Extra '`v'' must have '(e)' suffix come last"
				error 198
			}
			local v = substr("`v'", 1, `suffix_ind'-1)
			local ++z_e_varying_count 
			local z_e_varying = "`z_e_varying' 1"
		}

		tsunab v_list : `v'
		local v_count = wordcount("`v_list'")
		if `v_count' > 1 & `e_varying' {
			noi di as error "Extra '`v'' can't combine '(e)' suffix to a time-series varlist"
			error 198
		}
		tokenize `v_list'
		forvalues i = 1/`v_count' {
			local z_names = "`z_names' `=cond(`z_prefix', "z.", "")'``i''`=cond(`e_varying', "(e)", "")'"
			local ++z_count
			if !`e_varying' {
					local z_e_varying = "`z_e_varying' 0"
			}
		}
	}

	return local z_names = strtrim("`z_names'")
	return local z_count = `z_count'
	return local z_e_varying_count = `z_e_varying_count'
	return local z_e_varying = strtrim("`z_e_varying'")
end

program define edmPreprocessExtras, rclass
	syntax [anything] , touse(name) [z_vars(string)]

	local z_names = strtrim("`anything'")
	local z_count = wordcount("`z_names'")

	forvalues i = 1/`z_count' {
		local z_name : word `i' of `z_names'
		local z_var : word `i' of `z_vars'

		local e_varying = strpos("`z_name'", "(e)")
		if `e_varying' {
			local suffix_ind = strlen("`z_name'")-3+1
			local z_name = substr("`z_name'", 1, `suffix_ind'-1)
		}

		edmPreprocessVariable `z_name' , touse(`touse') out(`z_var')
	}
end 

program define edmPrintPluginProgress
	plugin call edm_plugin , "report_progress"

	local breakJustHit = 0
	local breakEverHit = 0
	local sleepTime = 0.001
	local numSleeps = 0

	while !plugin_finished {
		local ++numSleeps

		// This is the only time when the plugin will accept the 'break' button
		capture break sleep `sleepTime'

		if _rc {
			local breakJustHit = 1
			local breakEverHit = 1
		}
		plugin call edm_plugin , "report_progress" "`breakJustHit'"

		local breakJustHit = 0

		if `numSleeps' == 1000 {
			local sleepTime = 1
		}
	}

	if `breakEverHit' {
		exit 1
	}
end

program define edmExplore, eclass
	syntax anything  [if], [e(numlist ascending >=2)] [theta(numlist ascending)] [k(integer 0)] ///
			[REPlicate(integer 1)] [seed(integer 0)] [ALGorithm(string)] [tau(integer 1)] [DETails] ///
			[Predict(name)] [CROSSfold(integer 0)] [CI(integer 0)] [tp(integer 1)] ///
			[COPredict(name)] [copredictvar(string)] [full] [force] [EXTRAembed(string)] ///
			[ALLOWMISSing] [MISSINGdistance(real 0)] [dt] [DTWeight(real 0)] [DTSave(name)] ///
			[reportrawe] [CODTWeight(real 0)] [dot(integer 1)] [mata] [nthreads(integer 0)] ///
			[saveinputs(string)] [verbosity(integer 1)] [newdt] [parmode(integer 0)]
	* set seed
	if `seed' != 0 {
		set seed `seed'
	}
	if `tp' < 1 {
		di as error "tp must be greater than or equal to 1"
		error 9
	}
	* check predict
	if "`predict'" !="" {
		confirm new variable `predict'
	}

	if `crossfold' > 0 {
		if `replicate' > 1 {
			di as error "Replication must be not set if crossfold validation is used."
			error 119
		}
		if "`full'" == "full" {
			di as error "option full cannot be specified in combination with crossfold."
			error 119
		}
	}

	if "`copredictvar'" != "" & "`copredict'" == "" {
		di as error "The copredict() option is not specified"
		error 111
	}

	* default values
	if "`theta'" == ""{
		local theta = 1
	}

	* identify data structure
	qui xtset
	if "`=r(panelvar)'" != "." {
		local ispanel =1
		local panel_id = r(panelvar)
	}
	else {
		local ispanel =0
	}
	if !inlist("`algorithm'","smap","simplex","llr","") {
		dis as error "Not valid algorithm specification"
		error 121
	}
	if "`algorithm'" == "" {
		local algorithm "simplex"
	}

	qui xtset
	local timevar "`=r(timevar)'"

	edmPluginCheck, `mata'

	local mata_mode = r(mata_mode)
	if "${EDM_VERBOSITY}"!="" {
		local verbosity=${EDM_VERBOSITY}
	}
	if "${EDM_NTHREADS}"!="" {
		local nthreads=${EDM_NTHREADS}
	}

	local allow_missing_mode = `missingdistance' !=0 | "`allowmissing'"=="allowmissing"

	* create manifold as variables
	tokenize "`anything'"
	local ori_x "`1'"
	if "`2'" != "" {
		error 103
	}
	if "`e'" == "" {
		local e = 2
	}
	local report_actuale = "`reportrawe'" ==""
	marksample touse
	markout `touse' `timevar' `panel_id'

	tempvar x
	edmPreprocessVariable "`1'", touse(`touse') out(`x')

	local parsed_dt = ("`dt'" == "dt") | ("`newdt'" == "newdt")
	local parsed_dt0 = ("`newdt'" == "newdt")
	local parsed_dtw = "`dtweight'"
	if "`dtsave'" != ""{
		confirm new variable `dtsave'
	}
	local parsed_dtsave = "`dtsave'"

	if `parsed_dt' {
		/* general algorithm for generating t patterns
		1. keep only touse
		2. drop all missings
		3. regenerate t
		4. generate oldt_pattern
		 */
		qui {
			preserve
			keep if `touse'
			if !`allow_missing_mode' {
				keep if `x' != .
			}
			xtset
			local original_t = r(timevar)

			if "`=r(panelvar)'" == "." {
				local original_id = ""
				local byori =""
			}
			else {
				local original_id = r(panelvar)
				local byori ="by `original_id': "
				/* keep if `original_id' !=. */
			}
			tempvar newt
			sort `original_id' `original_t'
			`byori' gen `newt' = _n
			if "`original_id'" != ""{
				xtset `original_id' `newt'
			}
			else {
				tsset `newt'
			}

			tempvar dt_value
			qui gen double `dt_value' = d.`original_t'
			keep `original_id' `original_t' `newt' `dt_value'
			/* assert `original_id' !=. & `original_t' !=. */
			tempfile updatedt_main
			save `updatedt_main'
			restore

			// update copredict mainfold
			if "`copredictvar'" != "" {
				preserve
				keep if `touse'
				// this part is for filtering only
				if !`allow_missing_mode' {
					tokenize "`copredictvar'"
					local co_x "`1'"
					tempvar co_x_new
					if substr("`co_x'", 1, 2) == "z." {
						gen `co_x_new' = `=substr("`co_x'", 3, .)' if `touse'
					}
					else {
						gen `co_x_new' = `co_x' if `touse'
					}
					keep if `co_x_new' !=.
				}

				tempvar newt_co
				sort `original_id' `original_t'
				`byori' gen `newt_co' = _n
				if "`original_id'" != ""{
					qui xtset `original_id' `newt_co'
				}
				else {
					qui tsset `newt_co'
				}

				tempvar dt_value_co
				gen double `dt_value_co' = d.`original_t'

				keep `original_id' `original_t' `newt_co' `dt_value_co'
				tempfile updatedt_co
				save `updatedt_co'
				restore
			}

			merge m:1 `original_id' `original_t' using `updatedt_main', assert(master match) nogen
			if "`copredictvar'" != "" {
				merge m:1 `original_id' `original_t' using `updatedt_co', assert(master match) nogen
			}

			sort `original_id' `newt'
			if "`original_id'" != ""{
				qui xtset `original_id' `newt'
			}
			else {
				qui tsset `newt'
			}
			if !inlist("`parsed_dtsave'","",".") {
				clonevar `parsed_dtsave' = `dt_value'
				qui label variable `parsed_dtsave' "Time delta (`original_t')"
			}
		}
	}

	// Get the vector of future values which we'll be trying to predict
	tempvar x_f
	local future_step = `tp'-1 + `tau' //predict the future value with an offset defined by tp
	qui gen double `x_f' = f`future_step'.`x' if `touse'

	// Calculate the default value for 'dtweight'
	if `parsed_dt' {
		if `parsed_dtw' == 0 {
			qui sum `x' if `touse'
			local xsd = r(sd)
			qui sum `dt_value' if `touse'
			local tsd = r(sd)
			local parsed_dtw = `xsd'/`tsd'
			if `tsd' == 0 {
				// if there is no variance, no sampling required
				local parsed_dtw = 0
				local parsed_dt = 0
				local parsed_dt0 = 0
			}
		}
	}

	if !`parsed_dt' {
		tempvar before_tsfill
		qui tsset
		qui gen `before_tsfill' = 1
		qui tsfill
		qui replace `touse' = 0 if !`before_tsfill'
	}

	edmCountExtras `extraembed'
	local z_count = `r(z_count)'
	local z_names = "`r(z_names)'"
	local z_e_varying_count = `r(z_e_varying_count)'
	local z_e_varying = "`r(z_e_varying)'"
	local z_vars = ""
	forvalues i = 1/`z_count' {
		tempvar z
		local z_vars = "`z_vars' `z'"
	}
	edmPreprocessExtras `z_names' , touse(`touse') z_vars(`z_vars')

	numlist "`e'"
	local e_size = wordcount("`=r(numlist)'")
	local max_e : word `e_size' of `e'

	numlist "`theta'"
	local theta_size = wordcount("`=r(numlist)'")
	local round = max(`crossfold', `replicate')
	
	local task_num = 1
	local num_tasks = `round'*`theta_size'*`e_size'
	mat r = J(`num_tasks', 4, .)	

	edmManifoldSize, e(`max_e') dt(`parsed_dt') dt0(`parsed_dt0') ///
		num_extras(`z_count') num_eextras(`z_e_varying_count')
	local manifold_size = `r(manifold_size)'
	local total_num_extras = `r(total_num_extras)'

	if `mata_mode' {
		local manifold_vars = ""
		forvalues i = 1/`manifold_size' {
			tempvar manifold_var
			local manifold_vars = "`manifold_vars' `manifold_var'"
		}
		edmConstructManifolds `manifold_vars' , x(`x') touse(`touse') dt_value(`dt_value') ///
			z_vars("`z_vars'") z_e_varying("`z_e_varying'") ///
			max_e(`max_e') tau(`tau') dt(`parsed_dt') dt0(`parsed_dt0') dtw(`parsed_dtw')

		forvalues i=1/`=`max_e'-1' {
			local mapping_`i' = "`r(mapping_`i')'"
		}
		local max_e_manifold = "`r(max_e_manifold)'"
	}

	// Choose which rows of the manifold we will use for the analysis
	// (this mainly depends on whether we're keeping or discarding rows 
	// with some missing values).
	tempvar usable

	if `mata_mode' {	
		if `allow_missing_mode' {
			// Work on any row of the manifold with >= 1 non-missing value
			qui {
				qui gen byte `usable' = 0
				foreach v of local max_e_manifold {
					replace `usable' = 1 if `v' != . & `touse'
				}
				qui replace `usable' = 0 if `x_f' ==.
			}
		}
		else {
			// Find which rows of the manifold have any values which are missing
			tempvar any_missing_in_manifold
			hasMissingValues `max_e_manifold', out(`any_missing_in_manifold')
			gen byte `usable' = `touse' & !`any_missing_in_manifold' & `x_f' != .
		}
	}
	else {
		// PJL: Check that `savesmap' is not needed in explore mode.
		// Setup variables which the plugin will modify
		scalar plugin_finished = 0
		qui gen double `usable' = .
		local missing_dist_used = ""

		local explore_mode = 1
		local full_mode = ("`full'" == "full")
		if `parsed_dt' {
			local time = "`original_t'"
		}

		plugin call edm_plugin `x' `x_f' `z_vars' `time' `usable' `touse', "transfer_manifold_data" ///
				"`z_count'" "`parsed_dt'" "`parsed_dt0'" "`parsed_dtw'" "`algorithm'" "`force'" "`missingdistance'" "`nthreads'" "`verbosity'" "`num_tasks'" ///
				"`explore_mode'" "`full_mode'" "`crossfold'" "`tau'" "`parmode'" "`max_e'" "`allow_missing_mode'"

		local missingdistance = `missing_dist_used'
		qui compress `usable'
	}

	// Default value for 'missingdistance'
	if `mata_mode' & `allow_missing_mode' & `missingdistance' <= 0 {
		qui sum `x' if `usable'
		local missingdistance = 2/sqrt(c(pi))*r(sd)
	}

	if "`copredictvar'" != "" {
		if "`copredict'" == "" {
			di as error "The copredict() option is not specified"
			error 111
		}
		// temporary move to newt_co
		if `parsed_dt' {
			if "`original_id'" != ""{
				qui xtset `original_id' `newt_co'
			}
			else {
				qui tsset `newt_co'
			}
		}

		confirm new variable `copredict'
		tempvar co_train_set co_predict_set
		gen byte `co_train_set' = `usable'

		* build prediction manifold
		tokenize "`copredictvar'"
		tempvar co_x
		edmPreprocessVariable "`1'", touse(`touse') out(`co_x')

		* z list
		local co_z_vars = "`z_vars'"
		tempvar any_co_extras_missing
		hasMissingValues `co_z_vars', out(`any_co_extras_missing')

		tempvar co_usable
		gen byte `co_usable' = `touse' & `co_x' != . & !`any_co_extras_missing'

		local codtweight = cond(`parsed_dt' & `codtweight' == 0, `parsed_dtw', 0)

		local co_manifold_vars = ""
		forvalues i = 1/`manifold_size' {
			tempvar co_manifold_var
			local co_manifold_vars = "`co_manifold_vars' `co_manifold_var'"
		}
		edmConstructManifolds `co_manifold_vars' , x(`co_x') touse(`touse') dt_value(`dt_value_co') z_vars("`co_z_vars'") ///
			max_e(`max_e') tau(`tau') dt(`parsed_dt') dt0(`parsed_dt0') dtw(`codtweight')

		local co_mapping = "`r(max_e_manifold)'"

		forvalues i=0/`=`max_e'-1' {
			local co_x_`i' : word `=`i'+1' of `co_mapping'
			qui replace `co_usable' = 0 if `co_x_`i'' ==.
		}

		gen byte `co_predict_set' = `co_usable'

		//restore t
		if `parsed_dt' {
			if "`original_id'" != ""{
				qui xtset `original_id' `newt'
			}
			else {
				qui tsset `newt'
			}
		}
	}

	if `mata_mode' {
		tempvar train_set predict_set
		tempvar x_p
		qui gen double `x_p' = .
	}

	qui count if `usable'
	local num_usable = r(N)

	if `crossfold' > 0 {
		if `crossfold' > `num_usable' / `max_e' {
			di as error "Not enough observations for cross-validations"
			error 149
		}
		tempvar crossfoldu crossfoldunum
		qui gen double `crossfoldu' = runiform() if `usable'
		qui egen `crossfoldunum'= rank(`crossfoldu'), unique
	}
	
	if `num_usable' == 0 | (`num_usable' == 1 & "`full'" != "full") | (`crossfold' > 0 & `num_usable' < `crossfold') {
		noi display as error "Invalid dimension or library specifications"
		error 9
	}

	tempvar overlap
	if `round' > 1 & `dot' > 0 {
		if `replicate' > 1 {
			di "Replication progress (`replicate' in total)"
		}
		else if `crossfold' > 1 {
			di "`crossfold'-fold cross-validation progress (`crossfold' in total)"
		}
		local finished_rep = 0
	}

	if "`predict'" != "" {
		cap gen double `predict' = .
		qui label variable `predict' "edm prediction result"
	}

	if (`crossfold' == 0 & "`full'" != "full") {
		tempvar u
	}

	forvalues t=1/`round' {
		
		// Generate some random numbers (if we're in a mode which needs them
		// to separate the testing and prediction sets.)
		if `crossfold' == 0 & "`full'" != "full" {
			if `t' == 1 {
				qui gen double `u' = runiform() if `usable'
			}
			else {
				qui replace `u' = runiform() if `usable'
			}
		}

		// Split the data into training and prediction sets.
		// (The plugin will do this itself.)
		if `mata_mode' {
			cap drop `train_set' `predict_set' `overlap'
			if `crossfold' > 0 {
				qui gen byte `train_set' = mod(`crossfoldunum',`crossfold') != (`t' - 1) & `usable'
				qui gen byte `predict_set' = mod(`crossfoldunum',`crossfold') == (`t' - 1) & `usable'
			}
			else if "`full'" == "full"  {
				gen byte `train_set' = `usable'
				gen byte `predict_set' = `train_set'
			}
			else {
				qui sum `u', d
				qui gen byte `train_set' = `u' < r(p50) & `u' !=.
				qui gen byte `predict_set' = `u' >= r(p50) & `u' !=.
			}
			
			qui gen byte `overlap' = (`train_set' == `predict_set') & `predict_set'
			if "`full'" != "full" {
				assert `overlap' == 0 if `predict_set'
			}
		}

		if `crossfold' > 0 {
			// PJL: Try to clean up this part a bit.
			tempvar counting_up not_in_crossfold_t
			qui gen `counting_up' = _n if _n <= `num_usable'
			qui gen `not_in_crossfold_t' = mod(`counting_up',`crossfold') != (`t' - 1) 
			qui count if `not_in_crossfold_t'
			local train_size = r(N)
		}
		else if "`full'" == "full"  {
			local train_size = `num_usable'
		}
		else {
			local train_size = floor(`num_usable'/2)
		}

		// Set the maximum library size to be the size of the training set
		local max_lib_size = `train_size'

		foreach i of numlist `e' {
			if `mata_mode' {
				local manifold "mapping_`=`i'-1'"
			}
			edmManifoldSize, e(`i') dt(`parsed_dt') dt0(`parsed_dt0') ///
				num_extras(`z_count') num_eextras(`z_e_varying_count')
			local total_num_extras = `r(total_num_extras)'
			local e_offset = `r(manifold_size)' - `i'
			local current_e =`i' + cond(`report_actuale'==1,`e_offset',0)

			foreach j of numlist `theta' {

				if `k' > 0 {
					local lib_size = min(`k',`train_size')
				}
				else if `k' == 0 {
					local lib_size = `i' + `total_num_extras' + `parsed_dt' + cond("`algorithm'" == "smap", 2, 1)
				}
				else {
					local lib_size = `max_lib_size'
				}
				if `lib_size' > `max_lib_size' {
					local lib_size = `max_lib_size'
				}
				if `k' != 0 {
					local cmdfootnote = "Note: Number of neighbours (k) is adjusted to `lib_size'" + char(10)
				}
				else if `k' != `lib_size' & `k' == 0 {
					local plus_amt = `total_num_extras' + `parsed_dt' + cond("`algorithm'" =="smap",2,1)
					local cmdfootnote = "Note: Number of neighbours (k) is set to E+`plus_amt'" + char(10)
				}

				mat r[`task_num',1] = `current_e'
				mat r[`task_num',2] = `j'

				local save_prediction = ("`predict'" != "") & ((`crossfold' > 0)  | (`task_num' == `num_tasks'))

				if `mata_mode' {
					local savesmap_vars ""
					break mata: smap_block("``manifold''", "", "`x_f'", "`x_p'","`train_set'","`predict_set'",`j',`lib_size',"`overlap'", "`algorithm'", "`savesmap_vars'","`force'", `missingdistance')

					qui corr `x_f' `x_p' if `predict_set'
					mat r[`task_num',3] = r(rho)

					tempvar mae
					qui gen double `mae' = abs(`x_p' - `x_f') if `predict_set'
					qui sum `mae'
					drop `mae'
					mat r[`task_num',4] = r(mean)
					
					if `save_prediction' {
						cap replace `predict' = `x_p' if `x_p' !=.
					}
				}
				else {
					// PJL: Check we never save SMAP coeffs in explore mode.
					local save_smap_coeffs = 0
					local k_adj = `lib_size'
					plugin call edm_plugin `u' `crossfoldu', "launch_edm_task" ///
							"`t'" "`i'" "`j'" "`k_adj'" "`lib_size'" "`save_prediction'" "`save_smap_coeffs'" "`saveinputs'"
				}
				local ++task_num
			}
		}

		if `mata_mode' & `round' > 1 & `dot' > 0 {
			local ++finished_rep
			if mod(`finished_rep', 50*`dot') == 0 {
				di as text ". `finished_rep'"
			}
			else if mod(`finished_rep', `dot') == 0{
				di as text "." _c
			}
		}
	}
	if `mata_mode' & `round' > 1 & `dot' > 0 {
		if mod(`finished_rep', 50*`dot') != 0 {
			di ""
		}
	}

	// Collect all the asynchronous predictions from the plugin
	if `mata_mode' == 0 {
		edmPrintPluginProgress
		local result_matrix = "r"
		plugin call edm_plugin `predict', "collect_results" "`result_matrix'"
	}

	if "`copredictvar'" != ""  {
		if `num_tasks' == 1 {
			if `mata_mode' {
				qui replace `overlap' = 0
			}
			qui replace `co_train_set' = 0 if `usable' == 0

			tempvar co_x_p
			qui gen double `co_x_p'=.

			if `mata_mode' {
				break mata: smap_block("``manifold''", "`co_mapping'", "`x_f'", "`co_x_p'","`co_train_set'","`co_predict_set'",`theta',`lib_size',"`overlap'", "`algorithm'", "","`force'",`missingdistance')
			}
			else {
				scalar plugin_finished = 0

				plugin call edm_plugin `co_x' `co_train_set' `co_predict_set', "launch_coprediction_task" ///
						"`max_e'" "`theta'" "`lib_size'" "`saveinputs'"
 
				edmPrintPluginProgress
				plugin call edm_plugin `co_x_p', "collect_results"
			}

			qui gen double `copredict' = `co_x_p'
			qui label variable `copredict' "edm copredicted  `copredictvar' using manifold `ori_x' `ori_y'"
		}
		else {
			di as error "Error: coprediction can only run with one specified manifold construct (no repetition etc.)" _c
			di as result ""
		}
	}

	/* mat r = r[2...,.] */
	if !`parsed_dt' {
		qui keep if `before_tsfill' != .
		drop `before_tsfill'
	}

	mat cfull = r[1,3]
	local cfullname= subinstr("`ori_x'",".","/",.)
	matrix colnames cfull = `cfullname'
	matrix rownames cfull = rho
	/* mat list cfull */
	ereturn post cfull, esample(`usable')
	ereturn scalar N = `num_usable'
	/* ereturn post r, esample(`usable') dep("`y'") properties("r") */
	ereturn local subcommand = "explore"
	ereturn local direction = "oneway"
	ereturn scalar e_offset = `e_offset'
	ereturn scalar report_actuale = `report_actuale'
	ereturn local x "`ori_x'"
	ereturn local y "`ori_y'"
	if `crossfold' > 0 {
		ereturn local cmdfootnote "`cmdfootnote'Note: `crossfold'-fold cross validation results reported"
	}
	else {
		if "`full'" == "full" {
			ereturn local cmdfootnote "`cmdfootnote'Note: Full sample used for the computation"
		}
		else {
			ereturn local cmdfootnote "`cmdfootnote'Note: Random 50/50 split for training and validation data"
		}

	}

	ereturn matrix explore_result  = r
	ereturn local algorithm "`algorithm'"
	ereturn scalar tau = `tau'
	ereturn scalar replicate = `replicate'
	ereturn scalar crossfold = `crossfold'
	ereturn scalar rep_details = "`details'" == "details"
	ereturn scalar ci = `ci'
	ereturn local copredict ="`copredict'"
	ereturn local copredictvar ="`copredictvar'"
	ereturn scalar force_compute = "`force'" =="force"
	ereturn scalar panel =`ispanel'
	ereturn scalar dt =`parsed_dt'
	if `allow_missing_mode' {
		ereturn scalar missingdistance = `missingdistance'
	}
	if `parsed_dt' {
		ereturn scalar dtw =`parsed_dtw'
		ereturn local dtsave "`parsed_dtsave'"
		if "`z_names'" != "" {
			ereturn local extraembed = "`z_names' (+ time delta)"
		}
		else {
			ereturn local extraembed = "(time delta)"
		}

	}
	else {
		ereturn local extraembed = "`z_names'"
	}
	if ("`dt'" == "dt") | ("`newdt'" == "newdt") {
		sort `original_id' `original_t'
		qui xtset `original_id' `original_t'
		if "`original_id'" != ""{
			qui xtset `original_id' `original_t'
		}
		else {
			qui tsset `original_t'
		}
		if `parsed_dt' ==0 {
			ereturn local cmdfootnote "`cmdfootnote'Note: dt option is ignored due to lack of variations in time delta"
		}
	}
	ereturn local mode = cond(`mata_mode', "mata","plugin")
	edmDisplay
end


program define edmXmap, eclass
	syntax anything  [if], [e(integer 2)] [theta(real 1)] [Library(numlist)] [seed(integer 0)] ///
			[k(integer 0)] [ALGorithm(string)] [tau(integer 1)] [REPlicate(integer 1)] ///
			[SAVEsmap(string)] [DETails] [DIrection(string)] [Predict(name)] [CI(integer 0)] ///
			[tp(integer 0)] [COPredict(name)] [copredictvar(string)] [force] [EXTRAembed(string)] ///
			[ALLOWMISSing] [MISSINGdistance(real 0)] [dt] [DTWeight(real 0)] [DTSave(name)] ///
			[oneway] [savemanifold(name)] [CODTWeight(real 0)] [dot(integer 1)] [mata] ///
			[nthreads(integer 0)] [saveinputs(string)] [verbosity(integer 1)] [newdt] [parmode(integer 0)]
	* set seed
	if `seed' != 0 {
		set seed `seed'
	}
	if `tp' < 0 {
		di as error "tp must be greater than or equal to 0"
		error 9
	}

	if "`oneway'" =="oneway" {
		if !inlist("`direction'","oneway","") {
			di as error "option oneway does not match direction() option"
			error 9
		}
		else {
			local direction "oneway"
		}
	}
	if "`direction'" != "oneway" & "`dtsave'" !="" {
		di as error "dtsave() option can only be used together with oneway"
		error 9
	}
	* check prediction save
	if "`predict'" !="" {
		confirm new variable `predict'
		if "`direction'" != "oneway" {
			dis as error "direction() option must be set to oneway if predicted values are to be saved."
			error 197
		}
	}

	* default values
	// PJL: If these are varlists then it is fine. If they are real values, we can delete these defaults
	if "`e'" =="" {
		local e = "2"
	}
	if "`theta'" == ""{
		local theta = 1
	}
	local l_ori "`library'"
	if !inlist("`algorithm'","smap","simplex","llr","") {
		dis as error "Not valid algorithm specification"
		error 121
	}
	if "`algorithm'" == "" {
		local algorithm "simplex"
	}
	else if ("`algorithm'" == "smap"|"`algorithm'" == "llr") {
		if "`savesmap'" != "" {
			cap sum `savesmap'*
			if _rc != 111 {
				dis as error "There should be no variable with existing prefix when savesmap() option is used"
				error 110
			}
		}
	}
	if "`savesmap'" != "" & !("`algorithm'" =="smap" | "`algorithm'" =="llr") {
		dis as error "savesmap() option should only be specified with S-map"
		error 119
	}
	if "`direction'" == ""  {
		local direction "both"
	}
	if !inlist("`direction'","both","oneway") {
		dis as error "direction() option should be either both or oneway"
		error 197
	}
	* identify data structure
	qui xtset
	if "`=r(panelvar)'" != "." {
		local ispanel =1
		local panel_id = r(panelvar)
	}
	else {
		local ispanel =0
	}
	qui xtset
	local timevar "`=r(timevar)'"

	marksample touse
	markout `touse' `timevar' `panel_id'
	sort `panel_id' `timevar'


	edmPluginCheck, `mata'
	local mata_mode = r(mata_mode)
	if "${EDM_VERBOSITY}"!="" {
		local verbosity=${EDM_VERBOSITY}
	}
	if "${EDM_NTHREADS}"!="" {
		local nthreads=${EDM_NTHREADS}
	}

	local allow_missing_mode = `missingdistance' !=0 | "`allowmissing'"=="allowmissing"

	* create manifold as variables
	tokenize "`anything'"

	local ori_x "`1'"
	local ori_y "`2'"
	if "`3'" != "" {
		error 103
	}

	if "`1'" =="" | "`2'" == "" {
		error 102
	}

	tempvar x y
	edmPreprocessVariable "`1'", touse(`touse') out(`x')
	edmPreprocessVariable "`2'", touse(`touse') out(`y')

	if (`e' < 1) {
		dis as error "Some of the proposed number of dimensions for embedding is too small."
		error 121
	}
	local comap_constructed = 0

	mat r1 = J(1,4,.)
	mat r2 = J(1,4,.)
	local num_directions = 1 + ("`direction'" == "both")

	forvalues direction_num = 1/`num_directions' {

		if `direction_num' == 2 {
			local swap "`x'"
			local x "`y'"
			local y "`swap'"
		}

		/* return list */
		local parsed_dt = ("`dt'" == "dt") | ("`newdt'" == "newdt")
		local parsed_dt0 = ("`newdt'" == "newdt")
		local parsed_dtw = "`dtweight'"
		if "`dtsave'" != ""{
			confirm new variable `dtsave'
		}
		local parsed_dtsave = "`dtsave'"

		if `parsed_dt' {
			/* general algorithm for generating t patterns
			1. keep only touse
			2. drop all missings
			3. regenerate t
			4. generate oldt_pattern
			*/
			qui {
				// update main mainfold
				preserve
				keep if `touse'
				if !`allow_missing_mode' {
					keep if `x' != .
				}
				qui xtset
				local original_t = r(timevar)
				if "`=r(panelvar)'" == "." {
					local original_id = ""
					local byori =""
				}
				else {
					local original_id = r(panelvar)
					local byori ="by `original_id': "
				}
				tempvar newt
				sort `original_id' `original_t'
				`byori' gen `newt' = _n
				if "`original_id'" != ""{
					qui xtset `original_id' `newt'
				}
				else {
					qui tsset `newt'
				}

				tempvar dt_value
				gen double `dt_value' = d.`original_t'
				keep `original_id' `original_t' `newt' `dt_value'
				tempfile updatedt_main
				save `updatedt_main'
				restore

				// update copredict mainfold
				if "`copredictvar'" != "" {
					preserve
					keep if `touse'
					if !`allow_missing_mode' {
						tokenize "`copredictvar'"
						local co_x "`1'"
						local co_y "`2'"
						foreach v in "x" "y" {
							tempvar co_`v'_new
							if substr("`co_`v''",1,2) =="z." {
								gen `co_`v'_new' = `=substr("`co_`v''",3,.)' if `touse'
							}
							else {
								if "`co_`v''" !="" {
									gen `co_`v'_new' = `co_`v'' if `touse'
								}
								else {
									continue
								}
							}
							keep if `co_`v'_new' !=.
						}
					}
					tempvar newt_co
					sort `original_id' `original_t'
					`byori' gen `newt_co' = _n
					if "`original_id'" != ""{
						qui xtset `original_id' `newt_co'
					}
					else {
						qui tsset `newt_co'
					}

					tempvar dt_value_co
					gen double `dt_value_co' = d.`original_t'
					/* sum `dt_value_co' */
					keep `original_id' `original_t' `newt_co' `dt_value_co'
					tempfile updatedt_co
					save `updatedt_co'
					restore
				}

				merge m:1 `original_id' `original_t' using `updatedt_main', assert(master match) nogen
				if "`copredictvar'" != "" {
					merge m:1 `original_id' `original_t' using `updatedt_co', assert(master match) nogen
				}
				/* tempvar mergevar
				merge m:1 `original_id' `original_t' using `updatedt_main', assert(master match) gen(`mergevar')

				noi {

					tab `mergevar'

					if "`original_id'" !="" {
						assert `mergevar' ==3 if  `original_t'!=. & `original_id'!=. & `touse' & `x' !=.
					}
					else {
						assert `mergevar' ==3 if  `original_t'!=. & `touse'  & `x' !=.
					}

				}

				drop `mergevar'
				*/
				/* merge m:1 `original_id' `original_t' using `updatedt_main', assert(master match)  */
				/* tab _merge */


				sum `original_t' `newt'
				sort `original_id' `newt'
				if "`original_id'" != ""{
					qui xtset `original_id' `newt'
				}
				else {
					qui tsset `newt'
				}
				if !inlist("`parsed_dtsave'","",".") {
					clonevar `parsed_dtsave' = `dt_value'
					qui label variable `parsed_dtsave' "Time delta (`original_t')"
				}
			}

		}

		// Calculate the default value for `dtweight'
		if `parsed_dt' {
			if `parsed_dtw' == 0 {
				qui sum `x' if `touse'
				local xsd = r(sd)
				qui sum `dt_value' if `touse'
				local tsd = r(sd)
				local parsed_dtw = `xsd'/`tsd'
				if `tsd' == 0 {
					// if there is no variance, no sampling required
					local parsed_dtw = 0
					local parsed_dt = 0
					local parsed_dt0 = 0
				}
			}
			local parsed_dtw`direction_num' = `parsed_dtw'
		}

		if !`parsed_dt' & `direction_num' == 1 {
			tempvar before_tsfill
			qui tsset
			qui gen `before_tsfill' = 1
			qui tsfill
			qui replace `touse' = 0 if !`before_tsfill'
		}

		edmCountExtras `extraembed'
		local z_count = `r(z_count)'
		local z_names = "`r(z_names)'"
		local z_e_varying_count = `r(z_e_varying_count)'
		local z_e_varying = "`r(z_e_varying)'"
		local z_vars = ""
		forvalues i = 1/`z_count' {
			tempvar z
			local z_vars = "`z_vars' `z'"
		}
		edmPreprocessExtras `z_names' , touse(`touse') z_vars(`z_vars')

		numlist "`e'"
		local e_size = wordcount("`=r(numlist)'")
		local max_e : word `e_size' of `e'

		numlist "`theta'"
		local theta_size = wordcount("`=r(numlist)'")

		if "`l_ori'" == "" | "`l_ori'" == "0" {
			local l_size = 1
		}
		else {
			numlist "`library'"
			local l_size = wordcount("`=r(numlist)'")
		}

		local num_tasks = `replicate'*`theta_size'*`e_size'*`l_size'
		mat r`direction_num' = J(`num_tasks', 4, .)

		edmManifoldSize, e(`max_e') dt(`parsed_dt') dt0(`parsed_dt0') ///
			num_extras(`z_count') num_eextras(`z_e_varying_count')
		local manifold_size = `r(manifold_size)'
		local total_num_extras = `r(total_num_extras)'

		if `mata_mode' {
			local manifold_vars = ""
			forvalues i = 1/`manifold_size' {
				tempvar manifold_var
				local manifold_vars = "`manifold_vars' `manifold_var'"
			}
			edmConstructManifolds `manifold_vars' , x(`x') touse(`touse') dt_value(`dt_value') ///
				z_vars("`z_vars'") z_e_varying("`z_e_varying'") ///
				max_e(`max_e') tau(`tau') dt(`parsed_dt') dt0(`parsed_dt0') dtw(`parsed_dtw')

			forvalues i=1/`=`max_e'-1' {
				local mapping_`i' = "`r(mapping_`i')'"
			}
			local max_e_manifold = "`r(max_e_manifold)'"
		}

		// Get the vector of values which we'll try to predict
		tempvar x_f
		qui gen double `x_f' = f`tp'.`y' if `touse'

		// Select the rows which we'll use in the analysis
		tempvar usable

		local missingdistance`direction_num' = `missingdistance'
		if `mata_mode' {
			if `allow_missing_mode' {
				qui gen byte `usable' = 0
				foreach v of local max_e_manifold {
					qui replace `usable' = 1 if `v' !=. & `touse'
				}
				
				qui replace `usable' = 0 if `x_f' == .

				if `missingdistance' <= 0 {
					qui sum `x' if `usable'
					local defaultmissingdist = 2/sqrt(c(pi))*r(sd)
					local missingdistance`direction_num' = `defaultmissingdist'
				}
			}
			else {
				tempvar any_missing_in_manifold
				hasMissingValues `max_e_manifold', out(`any_missing_in_manifold')
				gen byte `usable' = `touse' & !`any_missing_in_manifold' & `x_f' != .
			}
		}
		else {
			// Setup variables which the plugin will modify
			scalar plugin_finished = 0
			qui gen double `usable' = .
			local missing_dist_used = ""

			local explore_mode = 0
			local full_mode = 0
			local crossfold = 0
			if `parsed_dt' {
				local time = "`original_t'"
			}

			plugin call edm_plugin `x' `x_f' `z_vars' `time' `usable' `touse', "transfer_manifold_data" ///
					"`z_count'" "`parsed_dt'" "`parsed_dt0'" "`parsed_dtw'" "`algorithm'" "`force'" "`missingdistance'" "`nthreads'" "`verbosity'" "`num_tasks'" ///
					"`explore_mode'" "`full_mode'" "`crossfold'" "`tau'" "`parmode'"  "`max_e'" "`allow_missing_mode'"

			local missingdistance`direction_num' = `missing_dist_used'
			// Collect a list of all the variables created to store the SMAP coefficients
			// across all the 'replicate's for this xmap direction.
			local all_savesmap_vars = ""
		}

		if ("`copredictvar'" != "") & (`comap_constructed' == 0) {
			// temporary move to newt_co
			if `parsed_dt' {
				qui {
					if "`original_id'" != ""{
						qui xtset `original_id' `newt_co'
					}
					else {
						qui tsset `newt_co'
					}
				}
			}
			confirm new variable `copredict'
			tempvar co_train_set co_predict_set
			gen byte `co_train_set' = `usable'
			* build prediction manifold
			tokenize "`copredictvar'"

			if ("`1'" == "") |  ("`2'" == "") {
				di as error "Coprediction does not match the main manifold construct"
				error 111
			}

			tempvar co_x co_y
			edmPreprocessVariable "`1'", touse(`touse') out(`co_x')
			edmPreprocessVariable "`2'", touse(`touse') out(`co_y')

			* z list
			local co_z_vars = "`z_vars'"
			local co_z_e_varying = "`z_e_varying'"
			tempvar any_co_extras_missing
			hasMissingValues `co_z_vars', out(`any_co_extras_missing')

			tempvar co_usable
			gen byte `co_usable' = `touse' & !`any_co_extras_missing'

			// note: there are issues in recalculating the codtweight as the variable usable are not generated in the same way as cousable
			local codtweight = cond(`parsed_dt' & `codtweight' == 0, `parsed_dtw', 0)

			local co_manifold_vars = ""
			forvalues i = 1/`manifold_size' {
				tempvar co_manifold_var
				local co_manifold_vars = "`co_manifold_vars' `co_manifold_var'"
			}
			edmConstructManifolds `co_manifold_vars' , x(`co_x') touse(`touse') dt_value(`dt_value_co') ///
				z_vars("`co_z_vars'") z_e_varying("`co_z_e_varying'") ///
				max_e(`max_e') tau(`tau') dt(`parsed_dt') dt0(`parsed_dt0') dtw(`codtweight')

			local co_mapping = "`r(max_e_manifold)'"

			forvalues i=0/`=`max_e'-1' {
				local co_x_`i' : word `=`i'+1' of `co_mapping'
				qui replace `co_usable' = 0 if `co_x_`i'' ==.
			}

			gen byte `co_predict_set' = `co_usable'
			local comap_constructed = 1

			//restore t
			if `parsed_dt' {
				qui {
					if "`original_id'" != ""{
						xtset `original_id' `newt'
					}
					else {
						tsset `newt'
					}
				}
			}
		}

		tempvar train_set predict_set

		if "`predict'" != "" {
			cap gen double `predict' = .
			qui label variable `predict' "edm prediction result"
		}

		if `mata_mode' {
			tempvar x_p
			qui gen double `x_p' = .
		}

		qui gen byte `predict_set' = `usable'
		qui gen byte `train_set' = . // to be decided by library length

		tempvar u urank
		tempvar overlap

		local task_num = 1
		if `replicate' > 1 & `direction_num' == 1 & `dot' > 0 {
			di "Replication progress (`=`replicate'*`num_directions'' in total)"
			local finished_rep = 0
		}

		// Now that `usable' is defined, we can set the default library size to be sum(usable).
		// N.B. For each direction of the xmap, we probably have a different sum(usable) value. 
		qui count if `usable'
		local num_usable = r(N)

		if "`l_ori'" == "" | "`l_ori'" == "0" {
			local library = `num_usable'
		}

		qui gen double `u' = .
	
		forvalues rep =1/`replicate' {

			qui replace `u' = runiform() if `usable'
			
			if `mata_mode' {
				cap drop `urank'
				qui egen double `urank' =rank(`u') if `usable', unique
			}

			foreach i of numlist `e' {
				local manifold "mapping_`=`i'-1'"
				foreach j of numlist `theta' {
					foreach lib_size of numlist `library' {
						if `lib_size' > `num_usable' {
							di as error "Library size exceeds the limit."
							error 1
							// PJL: Does the next line ever get reached?
							// PJL: Can easily check these lib_size constraints earlier in the function.
							continue, break
						}
						else if `lib_size' <= `i' + 1 {
							di as error "Cannot estimate under the current library specification"
							error 1
						}

						if `mata_mode' {
							qui replace `train_set' = `urank' <= `lib_size' & `usable'
						}

						local train_size = min(`lib_size',`num_usable')

						// detect k size
						if `k' > 0 {
							local k_size = min(`k',`train_size' -1)
						}
						else if `k' == 0{
							local k_size = `i' + `total_num_extras' + `parsed_dt' + cond("`algorithm'" == "smap", 2, 1)
						}
						else if `k' < 0  {
							local k_size = `train_size' - 1
							/* di "full lib" */
						}

						if `k' != 0 {
							local cmdfootnote = "Note: Number of neighbours (k) is adjusted to `k_size'" + char(10)
						}
						else if `k' != `k_size' & `k' == 0 {
							/* local cmdfootnote = "Note: Number of neighbours (k) is set to E+1" + char(10) */
						}

						if "`savesmap'" != "" {
							local xx = "`=cond(`direction_num'==1,"`ori_x'","`ori_y'")'"
							local yy = "`=cond(`direction_num'==1,"`ori_y'","`ori_x'")'"

							qui gen double `savesmap'`direction_num'_b0_rep`rep' = .
							qui label variable `savesmap'`direction_num'_b0_rep`rep' "constant in `xx' predicting `yy' S-map equation (rep `rep')"
							local savesmap_vars "`savesmap'`direction_num'_b0_rep`rep'"

							local mapping_name "`xx'" 

							forvalues ii=1/`=`e'-1' {
								local mapping_name "`mapping_name' l`=`ii'*`tau''.`xx'"
							}
							if `parsed_dt' {
								forvalues ii=`=(1 - `parsed_dt0')'/`=`e'-1' {
									local mapping_name "`mapping_name' dt`ii'"
								}
							}

							forvalues kk=1/`z_count' {
								local z_name : word `kk' of `z_names'

								local e_varying = strpos("`z_name'", "(e)")
								if `e_varying' {
									local suffix_ind = strlen("`z_name'")-3+1
									local z_name = substr("`z_name'", 1, `suffix_ind'-1)
								}

								local mapping_name = "`mapping_name' `z_name'"
								forvalues ii=1/`=(`e_varying'>0)*(`e'-1)' {
									local lagged_z_name = "l`=`ii'*`tau''.`z_name'"
									local mapping_name = "`mapping_name' `lagged_z_name'"
								}
							}

							local ii = 1
							local label "predicting `yy' or `yy'|M(`xx') S-map coefficient (rep `rep')"
							foreach name of local mapping_name {
								qui gen double `savesmap'`direction_num'_b`ii'_rep`rep' = .
								qui label variable `savesmap'`direction_num'_b`ii'_rep`rep' "`name' `label'"
								local savesmap_vars "`savesmap_vars' `savesmap'`direction_num'_b`ii'_rep`rep'"
								local ++ii
							}
							local all_savesmap_vars`direction_num' "`all_savesmap_vars`direction_num'' `savesmap_vars'" 
						}

						qui gen byte `overlap' = `train_set' ==`predict_set' if `predict_set'
						local last_theta =  `j'

						// PJL: currently `savemanifold' does nothing in the plugin. 
						if `mata_mode' & "`savemanifold'" !="" {
							local counter = 1
							foreach v of varlist ``manifold'' {
								cap gen double `savemanifold'`direction_num'_`counter' = `v'
								if _rc!=0 {
									di as error "Cannot save the manifold using variable `savemanifold'`direction_num'_`counter' - is the prefix used already?"
									exit(100)
								}
								local ++counter
							}
						}

						mat r`direction_num'[`task_num',1] = `direction_num'
						mat r`direction_num'[`task_num',2] = `lib_size'

						local save_prediction = (`task_num' == `num_tasks' & "`predict'" != "")

						if `mata_mode' {
							break mata: smap_block("``manifold''","", "`x_f'", "`x_p'","`train_set'","`predict_set'",`j',`k_size', "`overlap'", "`algorithm'","`savesmap_vars'","`force'",`missingdistance`direction_num'')

							qui corr `x_f' `x_p' if `predict_set'
							mat r`direction_num'[`task_num',3] = r(rho)

							tempvar mae
							qui gen double `mae' = abs(`x_p' - `x_f') if `predict_set'
							qui sum `mae'
							drop `mae'
							mat r`direction_num'[`task_num',4] = r(mean)

							if `save_prediction' {
								cap replace `predict' = `x_p' if `x_p' != .
							}
						}
						else {
							local save_smap_coeffs = ("`savesmap'" != "")
							plugin call edm_plugin `u', "launch_edm_task" ///
									"`rep'" "`i'" "`j'" "`k_size'" "`lib_size'" "`save_prediction'" "`save_smap_coeffs'" "`saveinputs'"
						}
						drop `overlap'
						local ++task_num
					}
				}
			}

			if `mata_mode' & `replicate' > 1 & `dot' >0 {
				local ++finished_rep
				if mod(`finished_rep',50*`dot') == 0 {
					di as text ". `finished_rep'"
				}
				else if mod(`finished_rep',`dot') == 0{
					di as text "." _c
				}
			}
		}

		// Collect all the asynchronous predictions from the plugin 
		if `mata_mode' == 0 {
			edmPrintPluginProgress
			local result_matrix = "r`direction_num'"
			plugin call edm_plugin `predict' `all_savesmap_vars`direction_num'', "collect_results" "`result_matrix'"
		}

		* reset the panel structure
		if ("`dt'" == "dt") | ("`newdt'" == "newdt") {
			sort `original_id' `original_t'
			qui xtset `original_id' `original_t'
			if "`original_id'" != ""{
				qui xtset `original_id' `original_t'
			}
			else {
				qui tsset `original_t'
			}
			if `parsed_dt' == 0 {
				if "`direction'" == "oneway" {
					local cmdfootnote "`cmdfootnote'Note: dt option is ignored due to lack of variations in time delta"
				}
				else {
					local cmdfootnote "Note: dt option is ignored in at least one direction"
				}

			}
		}

	}
	if `mata_mode' & `replicate' > 1 & `dot' > 0 {
		if mod(`finished_rep', 50*`dot') != 0 {
			di ""
		}
	}

	if "`copredictvar'" != "" {
		if `num_tasks' == 1 {
			qui gen byte `overlap' = 0
			qui replace `co_train_set' = 0 if `usable' == 0

			tempvar co_x_p
			qui gen double `co_x_p' = .

			//check whether dt transformation is required for copredict?
			// extract t for copredict variables -> add to copredict extras
			// set to new id t for mainfold construction
			if `mata_mode' {
				break mata: smap_block("``manifold''","`co_mapping'", "`x_f'", "`co_x_p'","`co_train_set'","`co_predict_set'",`last_theta',`k_size', "`overlap'", "`algorithm'","","`force'",`missingdistance')
			}
			else {
				scalar plugin_finished = 0
				plugin call edm_plugin `co_x' `co_train_set' `co_predict_set', "launch_coprediction_task" ///
						"`max_e'" "`theta'" "`k_size'" "`saveinputs'"
				edmPrintPluginProgress
				plugin call edm_plugin `co_x_p', "collect_results"
			}

			qui gen double `copredict' = `co_x_p'
			qui label variable `copredict' "edm copredicted `copredictvar' using manifold `ori_x' `ori_y'"
		}
		else {
			di as error "Error: coprediction can only run with one specified manifold construct (no repetition etc.)" _c
			di as result ""
		}
	}

	if !`parsed_dt' {
		qui keep if `before_tsfill' != .
		drop `before_tsfill'
	}

	mat cfull = (r1[1,3],r2[1,3])

	/* local cfullname = subinstr("`ori_y'|M(`ori_x') `ori_x'|M(`ori_y')",".","/",.) */
	local name1 = subinstr("`ori_y'|M(`ori_x')",".","/",.)
	local name2 = subinstr("`ori_x'|M(`ori_y')",".","/",.)
	local shortened = 1
	forvalues n =1/2 {
		if strlen("`name`n''") > 32 {
			local name`n' = substr("`name`n''",1,29) + "~`shortened'"
			local ++shortened
		}
	}
	matrix colnames cfull = `name1' `name2'
	matrix rownames cfull = rho
	/* mat list cfull */
	if "`direction'" == "oneway" {
		mat cfull = cfull[1...,1]
	}
	/* mat list cfull */
	ereturn post cfull, esample(`usable')
	ereturn scalar N = `num_usable'
	ereturn local subcommand = "xmap"
	ereturn matrix xmap_1  = r1
	if "`direction'" == "both" {
		ereturn matrix xmap_2  = r2
	}
	// the actual size of e should be main e + dt + extras
	ereturn scalar e_main = `e'
	edmManifoldSize, e(`e') dt(`parsed_dt') dt0(`parsed_dt0') ///
		num_extras(`z_count') num_eextras(`z_e_varying_count')
	ereturn scalar e_actual = `r(manifold_size)'
	ereturn scalar e_offset = `r(manifold_size)' - `e'
	ereturn scalar theta = `theta'
	ereturn local x "`ori_x'"
	ereturn local y "`ori_y'"
	ereturn local algorithm "`algorithm'"
	ereturn local cmdfootnote "`cmdfootnote'"
	ereturn scalar tau = `tau'
	ereturn scalar replicate = `replicate'
	ereturn scalar rep_details = "`details'" == "details"
	ereturn local direction = "`direction'"
	ereturn scalar ci = `ci'
	ereturn local copredict ="`copredict'"
	ereturn local copredictvar ="`copredictvar'"
	ereturn scalar force_compute = "`force'" =="force"
	ereturn local extraembed = "`extraembed'"
	ereturn scalar panel =`ispanel'
	ereturn scalar dt =`parsed_dt'
	if `allow_missing_mode' {
		ereturn scalar missingdistance = `missingdistance1'
		ereturn scalar missingdistance1 = `missingdistance1'
		if "`direction'" == "both" {
			ereturn scalar missingdistance2 = `missingdistance2'
		}
	}
	if `parsed_dt' {
		ereturn scalar dtw =`parsed_dtw1'
		ereturn scalar dtw1 =`parsed_dtw1'
		if "`direction'" == "both" {
			ereturn scalar dtw2 =`parsed_dtw2'
		}
		ereturn local dtsave "`parsed_dtsave'"
		if "`z_names'" != "" {
			ereturn local extraembed = "`z_names' (+ time delta)"
		}
		else {
			ereturn local extraembed = "(time delta)"
		}
	}
	else {
		ereturn local extraembed = "`z_names'"
	}
	ereturn local mode = cond(`mata_mode', "mata","plugin")
	edmDisplay
end


program define edmDisplay, eclass
/*
Empirical Dynamic Modelling
Univariate simplex projection with manifold construct x and its lag values
------------------------------------
| E | theta | rho |
| 2 | 0.1 | 0.993 |
------------------------------------
*/
	display _n "Empirical Dynamic Modelling"
	local diopts "`options'"
	local fmt "%12.5g"
	local fmtprop "%8.3f"
	local ci_counter = 1
	if e(subcommand) =="explore" {
		if !inlist("`=e(extraembed)'","",".") {
			di as text "Multivariate mapping with `=e(x)' and its lag values"
		}
		else {
			di as text "Univariate mapping with `=e(x)' and its lag values"
		}

		if !inlist("`=e(extraembed)'","",".") {
			di as text "Additional variable" _c
			di cond(wordcount("`=e(extraembed)'")>1,"s","") _c
			di " in the embedding: `=e(extraembed)'"
		}
		if e(missingdistance)>0 & e(missingdistance)!= .{
			di as text "Missing values are assumed to have a distance of " _c
			di `:di %8.2g `=e(missingdistance)'' _c
			di " with all values."
		}
		if ((e(replicate) == 1 & e(crossfold) <=0) | e(rep_details) == 1) {
			di as txt "{hline 68}"
			display as text %18s cond(e(report_actuale)==1,"Actual E","E")  _c
			display as text %16s "theta"  _c
			display as text %16s "rho"  _c
			display as text %16s "MAE"
			di as txt "{hline 68}"

			mat r = e(explore_result)
			local nr = rowsof(r)
			local kr = colsof(r)
			forvalues i = 1/ `nr' {
				forvalues j=1/`kr' {
					if `j'==1 {
						local dformat "%18s"
					}
					else {
						local dformat "%16s"
					}
					display as result `dformat' `"`:display `fmt' r[`i',`j'] '"' _c
				}
				display " "
			}
			di as txt "{hline 68}"
		}
		else {
			di as txt "{hline 70}"
			di as text %22s " " _c
			di as txt "{hline 9} rho {hline 9}  " _c
			di as txt "{hline 9} MAE {hline 9}"
			/* di as txt "{hline 70}" */
			display as text %9s cond(e(report_actuale)==1,"Actual E","E")  _c
			display as text %9s "theta"  _c
			display as text %13s "Mean"  _c
			display as text %13s "Std. Dev."  _c
			display as text %13s "Mean"  _c
			display as text %13s "Std. Dev."
			di as txt "{hline 70}"
			local dformat "%13s"

			// process the return matrix
			tempname reported_r r buffer summary_r
			mat `r' = e(explore_result)
			local nr = rowsof(`r')
			local kr = colsof(`r')
			mat `reported_r' = J(`nr',1,0)
			mat `summary_r' = J(1,6,.)
			/* mat list `r'
			mat list `reported_r' */
			forvalues i = 1/ `nr' {
				mat `buffer' = J(1,2,.)
				if `reported_r'[`i',1] == 1 {
					continue
				}
				local base_E = `r'[`i',1]
				local base_theta = `r'[`i',2]
				forvalues j=1/`nr' {
					if `reported_r'[`j',1] ==0 {
						if `r'[`j',1] == `base_E' & `r'[`j',2] == `base_theta' {
							mat `buffer' = (`buffer'\ `=`r'[`j',3]',`=`r'[`j',4]')
							mat `reported_r'[`j',1] =1
							/* mat list `buffer' */
						}
					}
				}
				/* noi mat list `buffer' */
				// now get the mean and st
				tempname mat_mean mat_sd
				mata: st_matrix("`mat_sd'", diagonal(sqrt(variance(st_matrix("`buffer'"))))')
				/* if changes to standard error */
				/* mata: st_matrix("`mat_sd'", diagonal(sqrt(variance(st_matrix("`buffer'"))))/sqrt(`nr')') */
				mata: st_matrix("`mat_mean'", mean(st_matrix("`buffer'")))

				di as result %9s  `"`: display %9.0g `r'[`i',1] '"' _c
				di as result %9s  `"`: display %9.5g `r'[`i',2] '"' _c
				forvalues j=1/2{
					display as result `dformat' `"`:display `fmt' `mat_mean'[1,`j'] '"' _c
					display as result `dformat' `"`:display `fmt' `mat_sd'[1,`j'] '"' _c
				}
				mat `summary_r' = (`summary_r'\ `=`r'[`i',1]',`=`r'[`i',2]', `=`mat_mean'[1,1]',`=`mat_sd'[1,1]', `=`mat_mean'[1,2]',`=`mat_sd'[1,2]')

				di ""
				if `=e(ci)'>0 & `=e(ci)'<100 {
					/* set trace on */
					edmDisplayCI , mat(`buffer') ci(`=e(ci)')
					local type1 "rho"
					local type2 "mae"
					forvalues j=1/2 {
						foreach t_type in "lb_mean" "ub_mean" "lb_pco" "ub_pco" "lb_pce" "ub_pce" {
							ereturn scalar `t_type'_`type`j''`ci_counter' =r(`t_type'_`type`j'')
						}
					}
					local ++ci_counter
				}
			}
			mat `summary_r'=`summary_r'[2...,.]
			ereturn matrix summary = `summary_r'
			di as txt "{hline 70}"
			di as text "Note: Results from `=max(`=e(replicate)',`=e(crossfold)')' runs"
		}

		if e(e_offset) != 0 {
			di as text "Note: Actual E is higher than the specified E due to extras"
		}
		di as text ustrtrim(e(cmdfootnote))
		/* di as txt "Note: E is the embedding dimension" */
	}
	else if e(subcommand) == "xmap" {
		di as txt "Convergent Cross-mapping result for variables {bf:`=e(x)'} and {bf:`=e(y)'}"
		if !inlist("`=e(extraembed)'","",".") {
			di as text "Additional variable" _c
			di cond(wordcount("`=e(extraembed)'")>1,"s","") _c
			di " in the embedding: `=e(extraembed)'"
		}
		if e(missingdistance)>0 & e(missingdistance)!= . {
			di as text "Missing values are assumed to have a distance of " _c
			/* di `:di %8.2g `=e(missingdistance)'' _c */

			if `=e(missingdistance1)' != `=e(missingdistance2)' & `=e(missingdistance1)' !=. & e(direction) != "oneway" {
				di `:di %8.2g `=e(missingdistance1)'' _c
				di " and " _c
				di `:di %8.2g `=e(missingdistance2)''
			}
			else {
				di `:di %8.2g `=e(missingdistance)'' _c
				di " with all values."
			}
		}
		local direction1 = "`=e(y)' ~ `=e(y)'|M(`=e(x)')"
		local direction2 = "`=e(x)' ~ `=e(x)'|M(`=e(y)')"
		forvalues i=1/2{
			if strlen("`direction`i''")>26 {
				local direction`i' = substr("`direction`i''",1,24) + ".."
			}
		}
		local mapp_col_length = min(28, max(strlen("`direction1'"), strlen("`direction2'")) +3)
		local line_length = 50 + `mapp_col_length'
		if (e(replicate) == 1 | e(rep_details) == 1) {
			* the case of no replication
			di as txt "{hline `line_length'}"
			display as text %`mapp_col_length's "Mapping"  _c
			display as text %16s "Library size"  _c
			display as text %16s "rho"  _c
			display as text %16s "MAE"
			di as txt "{hline `line_length'}"
			local num_directions = 1 + (e(direction) =="both")
			forvalues direction_num = 1/`num_directions' {
				if `direction_num' == 1 {
					mat r = e(xmap_1)
				}
				else {
					mat r = e(xmap_2)
				}

				local nr = rowsof(r)
				local kr = colsof(r)

				forvalues i = 1/ `nr' {

					forvalues j=1/`kr' {
						if `j' == 1 {
							display as result %`mapp_col_length's "`direction`=r[`i',`j']''" _c
						}
						else {
							display as result %16s `"`:display `fmt' r[`i',`j'] '"' _c
						}
					}
					display " "
				}
			}

			di as txt "{hline `line_length'}"
		}
		else {
			* the case of replication
			di as txt "{hline `line_length'}"
			display as text %`mapp_col_length's "Mapping"  _c
			display as text %16s "Lib size"  _c
			display as text %16s "Mean rho"  _c
			display as text %16s "Std. Dev."
			di as txt "{hline `line_length'}"
			local dformat "%16s"

			// process the return matrix
			tempname reported_r r buffer summary_r

			forvalues direction_num = 1/2 {
				if `direction_num' == 1 {
					mat `r' = e(xmap_1)
				}
				else {
					mat `r' = e(xmap_2)
					if e(direction) =="oneway" {
						continue, break
					}
				}

				local nr = rowsof(`r')
				local kr = colsof(`r')
				mat `reported_r' = J(`nr',1,0)

				mat `summary_r' = J(1,6,.)
				/* mat list `r'
				mat list `reported_r' */
				forvalues i = 1/ `nr' {
					mat `buffer' = J(1,2,.)
					if `reported_r'[`i',1] == 1 {
						continue
					}
					local base_direction = `r'[`i',1]
					local base_L = `r'[`i',2]
					forvalues j=1/`nr' {
						if `reported_r'[`j',1] ==0 {
							if `r'[`j',1] == `base_direction' & `r'[`j',2] == `base_L' {
								mat `buffer' = (`buffer'\ `=`r'[`j',3]',`=`r'[`j',4]')
								mat `reported_r'[`j',1] =1
								/* mat list `buffer' */
							}
						}
					}
					// now get the mean and st
					tempname mat_mean mat_sd
					/* mat list `buffer' */
					mata: st_matrix("`mat_sd'", diagonal(sqrt(variance(st_matrix("`buffer'"))))')
					mata: st_matrix("`mat_mean'", mean(st_matrix("`buffer'")))

					di as result %`mapp_col_length's "`direction`base_direction''" _c
					di as result `dformat' `"`: display `fmt' `r'[`i',2] '"' _c
					forvalues j=1/1{
						display as result `dformat' `"`:display `fmt' `mat_mean'[1,`j'] '"' _c
						display as result `dformat' `"`:display `fmt' `mat_sd'[1,`j'] '"' _c
					}
					mat `summary_r' = (`summary_r'\ `=`r'[`i',1]',`=`r'[`i',2]', `=`mat_mean'[1,1]',`=`mat_sd'[1,1]', `=`mat_mean'[1,2]',`=`mat_sd'[1,2]')
					/* mat list `summary_r' */
					display ""
					if `=e(ci)'>0 & `=e(ci)'<100 {
						edmDisplayCI , mat(`buffer') ci(`=e(ci)') maxr(1)
						local type1 "rho"
						local type2 "mae"
						forvalues j=1/1 {
							foreach t_type in "lb_mean" "ub_mean" "lb_pco" "ub_pco" "lb_pce" "ub_pce" {
								ereturn scalar `t_type'_`type`j''`ci_counter' =r(`t_type'_`type`j'')
							}
						}
						local ++ci_counter
					}
				}
			}
			mat `summary_r'=`summary_r'[2...,.]
			ereturn matrix summary = `summary_r'
			di as txt "{hline `line_length'}"
			di as text "Note: Results from `=e(replicate)' replications"
		}



		if "`=e(cmdfootnote)'" != "." {
			di as text ustrtrim(e(cmdfootnote))
		}
		di as txt "Note: The embedding dimension E is `=e(e_actual)'" _c
		/* set trace on */
		if e(e_main) != e(e_actual) {
			di " (including `=e(e_offset)' extra`=cond(e(e_offset)>1,"s","")')"
		}
		else {
			di ""
		}
		/* di as txt "Note: The embedding dimension E is `=e(e)', theta (distance weight) is `=e(theta)'" */
	}
	/* if `=e(ci)'>0 & `=e(ci)'<100 {
		di as text "Note: CI is estimated based on mean +/- " _c
		local sdm:display %10.2f `=invnormal(1-(100-`=e(ci)')/200)'
		di trim("`sdm'") _c
		di "*std / sqrt(`=e(replicate)')"
	} */
	if `=e(force_compute)' == 1 {
		di as txt "Note: -force- option is specified. The estimate may not be derived from the specified k."
	}
	if `=e(dt)' == 1 {
		di as txt "Note: Embedding includes the delta of the time variable with a weight of " _c
		if `=e(dtw1)' != `=e(dtw2)' & `=e(dtw2)' !=. & e(direction) !="oneway" {
			di `:di %8.2g `=e(dtw1)'' _c
			di " and " _c
			di `:di %8.2g `=e(dtw2)''
		}
		else {
			di `:di %8.2g `=e(dtw)''
		}
	}

	/* di as txt "For more information, please refer to {help edm:help file} and the article." */
end


capture mata mata drop smap_block()
mata:
mata set matastrict on
void smap_block(string scalar manifold, string scalar p_manifold, string scalar prediction, string scalar result, string scalar train_use, string scalar predict_use, real scalar theta, real scalar l, string scalar skip_obs, string scalar algorithm, string scalar savesmap_vars, string scalar force, real scalar missingdistance)
{
	real scalar force_compute, k, i
	force_compute = force == "force" // check whether we need to force the computation if k is too high
	real matrix M, Mp, y, ystar,S
	st_view(M, ., tokens(manifold), train_use) //base manifold
	st_view(y, ., prediction, train_use) // known prediction of the base manifold
	st_view(ystar, ., result, predict_use)
	if (p_manifold != "") {
		//data used for prediction is different than the source manifold
		st_view(Mp, ., tokens(p_manifold), predict_use)
	}
	else {
		st_view(Mp, ., tokens(manifold), predict_use)
	}

	st_view(S, ., skip_obs, predict_use)

	if (l <= 0) { //default value of local library size
		k = cols(M)
		l = k + 1 // local library size (E+1) + itself
	}

	real matrix B
	real scalar save_mode
	if (savesmap_vars != "") {
		st_view(B, ., tokens(savesmap_vars), predict_use)
		save_mode = 1
	}
	else {
		save_mode = 0
	}
	real scalar n
	n = rows(Mp)

	real rowvector b

	for(i=1;i<=n;i++) {
		b= Mp[i,.]
		ystar[i] = mf_smap_single(M,b,y,l,theta,S[i],algorithm, save_mode*i, B, force_compute,missingdistance)
	}
}
end


capture mata mata drop mf_smap_single()
mata:
mata set matastrict on
real scalar mf_smap_single(real matrix M, real rowvector b, real colvector y, real scalar l, real scalar theta, real scalar skip_obs, string scalar algorithm, real scalar save_index, real matrix Beta_smap, real scalar force_compute, real scalar missingdistance)
{
	/* real scalar mf_smap_single(real matrix M, real rowvector b, real colvector y, real scalar l, real scalar theta, real scalar skip_obs, string scalar algorithm, real scalar save_index, real matrix Beta_smap, transmorphic scalar Acache) */

	/* M : manifold matrix
	b : the vector used for prediction
	y: existing predicted value for M (same number of rows with M)
	l : library size
	theta: exponential weighting parameter
	skip_obs: number of closest neighbours to skip (to exclude itself sometimes) */

	/* sprintf("begin") */
	real colvector d, w, a
	real colvector ind, v
	real scalar i,j,n,r,n_ls
	n = rows(M)
	d = J(n, 1, 0)

	for(i=1;i<=n;i++) {
		a= M[i,.] - b

		if (missingdistance !=0) {
			a=editvalue(a,., missingdistance)
		}
		// d is squared distance
		d[i] = a*a'
	}

	minindex(d, l+skip_obs, ind, v)
	// create weights for each point in the library

	// find the smallest non-zero distance
	real scalar d_base
	real scalar pre_adj_skip_obs
	pre_adj_skip_obs = skip_obs
	for(j=1;j<=l;j++) {
		if (d[ind[j+skip_obs]] == 0) {
			skip_obs++
		}
		else {
			break
		}
	}
	if (pre_adj_skip_obs!=skip_obs) {
		minindex(d, l+skip_obs, ind, v)
	}
	if (d[ind[1+skip_obs]] == 0) {
		d= editvalue(d, 0,.)
		/* sprintf("search failed") */
		/* skip_obs++ */
		skip_obs = 0
		minindex(d, l+skip_obs, ind, v)
	}
	d_base = d[ind[1+skip_obs]]
	/* if (d_base ==0) {
		sprintf("error")
	} */
	/* sprintf("dbase %g with %g",d_base, skip_obs) */
	w = J(l+skip_obs, 1, .)
	if (rows(ind)<l+skip_obs) {
		if (force_compute==1) {
			l=rows(ind)-skip_obs // change l to match neighbor size
			/* sprintf("library size has been reduced for some observations")	 */
			if (l<=0) {
				sprintf("Insufficient number of unique observations in the dataset even with -force- option.")
				exit(error(503))
			}
		}
		else {
			sprintf("Insufficient number of unique observations, consider tweaking the values of E, k or use -force- option")
			exit(error(503))
		}
	}
	// note the w, X_ls, y_ls matrix are larger than necessary, the first skip_obs rows are not used
	r = 0

	if (algorithm == "" | algorithm == "simplex") {
		for(j=1+skip_obs;j<=l+skip_obs;j++) {
			w[j] = exp(-theta*(d[ind[j]] / d_base)^(1/2))
		}
		w = w/sum(w)
		for(j=1+skip_obs;j<=l+skip_obs;j++) {
			r = r +  y[ind[j]] * w[j]
		}

		return(r)
	}
	else if (algorithm =="smap" | algorithm =="llr") {

		real colvector y_ls, b_ls, w_ls
		real matrix X_ls, XpXi
		real rowvector x_pred
		real scalar mean_w

		for(j=1+skip_obs;j<=l+skip_obs;j++) {
			w[j] = d[ind[j]] ^ (1/2)
		}
		mean_w = mean(w)
		for(j=1+skip_obs;j<=l+skip_obs;j++) {
			w[j] = exp(-theta*(w[j] / mean_w))
		}

		y_ls = J(l, 1, .)
		X_ls = J(l, cols(M), .)
		w_ls = J(l, 1, .)

		real scalar rowc
		rowc = 0
		/* sprintf("start") */
		for(j=1+skip_obs;j<=l+skip_obs;j++) {
			if (hasmissing(y[ind[j]]) | hasmissing(M[ind[j],.])) {
				continue
			}
			rowc++
			if (algorithm == "llr") {
				y_ls[rowc]    = y[ind[j]]
				/* matlist(X_ls[j,.]) */
				X_ls[rowc,.]    = M[ind[j],.]
				/* matlist(X_ls[j,.]) */
				w_ls[rowc] = w[j]
			}
			else if (algorithm =="smap") {
				y_ls[rowc]    = y[ind[j]] * w[j]
				X_ls[rowc,.]    = M[ind[j],.] * w[j]
				w_ls[rowc] = w[j]
			}
		}
		if (rowc ==0) {
			return(.)
		}

		y_ls =y_ls[1..rowc]
		X_ls =X_ls[1..rowc,.]
		w_ls = w_ls[1..rowc]

		n_ls   = rows(X_ls)
		// add constant
		X_ls    = w_ls,X_ls

		if (algorithm == "llr") {
			XpXi = quadcross(X_ls, w_ls, X_ls)
			XpXi = invsym(XpXi)
			b_ls    = XpXi*quadcross(X_ls, w_ls, y_ls)
		}
		else {
			b_ls = svsolve(X_ls, y_ls)
		}

		if (save_index>0) {
			Beta_smap[save_index,.] = editvalue(b_ls',0,.)
			/* Beta_smap[save_index,.] = b_ls' */
		}

		x_pred = 1,editvalue(b,.,0)

		r = x_pred * b_ls

		return(r)
	}

}
end

// Load the C++ implementation
cap program edm_plugin, plugin using(edm.plugin)

// The developers of this plugin often have the file by a different
// name locally, so this will load the plugin based on the OS-specific
// filenames.
cap program edm_plugin, plugin using(edm_`=c(os)'_x64.plugin)
cap program edm_plugin, plugin using("`=strlower("edm_`=c(os)'_x64.plugin")'")
cap program edm_plugin, plugin using(edm_`=c(os)'_arm.plugin)
cap program edm_plugin, plugin using("`=strlower("edm_`=c(os)'_arm.plugin")'")
cap program edm_plugin, plugin using(edm_plugin.plugin)
