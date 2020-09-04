## edm Description

`edm` implements a series of tools that can be used for empirical dynamic modeling in Stata. The command keyword is `edm`, and should be immediately followed by the subcommand `explore` or `xmap`. A dataset must be declared as time-series or panel data by the tsset or xtset command prior to using the edm command, and time-series operators including `l.`, `f.`, `d.`, and `s.` can be used (the last for seasonal differencing).

## Installation

To install the stable version directly through Stata:

~~~
ssc install edm, replace
~~~

To install the latest development version directly through Stata:

~~~
net install edm, from("https://jinjingli.github.io/edm/") replace
~~~

## Example Data and Do File

[This paper](https://jinjingli.github.io/edm/edm-wp.pdf) describes the use of edm in Stata. The slides for the talk given at the QMNET seminar series is available [here](https://jinjingli.github.io/edm/EDM-talk-QMNET.pdf).

The Chicago crime dataset used in the edm paper can be downloaded [here](https://jinjingli.github.io/edm/chicago.dta).

This [do file](https://jinjingli.github.io/edm/sj-edm.do) contains the a series of examples of the edm command using both a synthetic dataset and the Chicago crime dataset.

