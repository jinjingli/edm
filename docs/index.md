## edm Description

`edm` implements a series of tools that can be used for empirical dynamic modeling in Stata. The command keyword is `edm`, and should be immediately followed by the subcommand `explore` or `xmap`. A dataset must be declared as time-series or panel data by the tsset or xtset command prior to using the edm command, and time-series operators including `l.`, `f.`, `d.`, and `s.` can be used (the last for seasonal differencing).

## Installation

To install the stable version directly through Stata:

~~~
ssc install edm, replace
~~~

To install the latest development version directly through Stata:

~~~
net install edm, from("https://raw.githubusercontent.com/EDM-Developers/edm-releases/master/") replace
~~~

The source code for the package is available [here](https://github.com/EDM-Developers/EDM).

## Example Data and Do File

[This paper](https://jinjingli.github.io/edm/edm-wp.pdf) describes the use of edm in Stata. The slides for the talk given at the QMNET seminar series is available [here](https://jinjingli.github.io/edm/EDM-talk-QMNET.pdf).

The Chicago crime dataset used in the edm paper can be downloaded [here](https://jinjingli.github.io/edm/chicago.dta).

This [do file](https://jinjingli.github.io/edm/sj-edm.do) contains the a series of examples of the edm command using both a synthetic dataset and the Chicago crime dataset.

## Plugins

The `edm` command will support a plugin system which facilities some automated common analyses. This feature is under development and the files below provide some selected early stage prototypes allowing automated Simplex Projections, S-maps, Coprediction, and Convergent Cross Mapping as well as relevant hypothesis tests:

- [edm Plugin - Automated time-series analysis (N=1 case)](plugins/1._EDM_for_N_of_1-traditional_time-series_case.do)
- [edm Plugin - Automated multi-spatial analysis (N>1 panel data case)](plugins/2._Multispatial_edm-pooling_panel_IDs_together.do)
- [edm Plugin - Automated multiple-edm analysis (N>1 panel data case)](plugins/3._Multiple_EDM-analyze_each_panel_ID_separately.do)
