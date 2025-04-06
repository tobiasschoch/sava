# sava: Small Area Variation Analysis

## Summary

The `sava` package is a contributed package for the R language and environment for statistical computing ([R Core Team, 2025](#references)). It implements the following estimators and tools:

- **Systematic Component of Variation** (SCV, [McPherson et al., 1982)](#references). In addition, a modified SCV estimator according to [Diehr et al. (1990)](#reference) is also implemented.
- **Poisson-Gamma Empirical Bayes** approach: [Marutzzi and Hills (1995)](#references) suggested an Empirical Bayes approach based on a Poisson-Gamma hierarchical model. The model is the same as the one in [Clayton and Kaldor (1987)](#references) except that the mean of the Gamma a-priori distribution is fixed at unity. 
- Statistical inference for the rates is based on the estimated **mean square (prediction) error** (MSE). The package implements the jackknife MSE estimators of [Jiang et al. (2002)](#references) and [Rao (2003)](#references).
- **Extremal Quotient** (EQ, [Coory and Gibberd, 1998](#references)). EQ is the ratio of high versus low rates.
- Functions for **direct standardization of rates** (incl. confidence intervals; see [Fay and Feuer, 1997](#Reference)).

<b style="color:red;"> Learn more:</b> Have a look at the [vignette](https://tobiasschoch.github.io/sava/vignettes/sava.html) for more information.   



## Installation

Make sure that the R package `devtools` is installed. Then, the `sava` package can be pulled from this GitHub repository and installed by:
```
devtools::install_github("tobiasschoch/sava")
```



## Community guidelines

#### Submitting an issue

If you have any suggestions for feature additions or any problems with the software that you would like addressed with the development community, please submit an issue on the Issues tab of the project GitHub repository. You may want to search the existing issues before submitting, to avoid asking a question or requesting a feature that has already been discussed.

#### How to contribute

In order to contribute, please contact the developer: Tobias Schoch at fhnw dot ch (the names are separated by a dot).

#### Asking for help

If you have questions about how to use the software, or would like to seek out collaborations related to this project, you may contact Tobias Schoch (see contact details above).



## References

CLAYTON, D. and KALDOR, J. (1987). Empirical Bayes estimates of age-standardized relative risks for use in disease mapping. *Biometrics*, **43**, 671–681. [DOI: 10.2307/2532003](https://doi.org/10.2307/2532003)

COORY, M. and GIBBERD, R. (1998). New measures for reporting the magnitude of small-area variation in rates. *Statistics in Medicine*, **17**, 2625–2634. [DOI: 10.1002/(SICI)1097-0258(19981130)17:22<2625::AID-SIM957>3.0.CO;2-4](https://doi.org/10.1002/(SICI)1097-0258(19981130)17:22<2625::AID-SIM957>3.0.CO;2-4)

DIEHR, P., CAIN, K., CONNELL, F. and VOLINN, E. (1990). What is too much variation? The null hypothesis in small-area analysis. *Health Service Research*, **24**, 741–771.

FAY, M. P. and FEUER, E. J. (1997). Confidence intervals for directly standardized rates: A method based on the gamma distribution. *Statistics in Medicine*, **16**, 791–801. [DOI: 10.1002/(SICI)1097-0258(19970415)16:7<791::AID-SIM500>3.0.CO;2-%23](https://doi.org/10.1002/(SICI)1097-0258(19970415)16:7<791::AID-SIM500>3.0.CO;2-%23)

JIANG, J., LAHIRI, P. and WAN, S.-M. (2002). A unified jackknife theory for empirical best prediction with M-estimation. *The Annals of Statistics*, **30**, 1782–1810. [DOI: 10.1214/aos/1043351257](https://doi.org/10.1214/aos/1043351257)

MARTUZZI, M. and HILLS, M. (1995). Estimating the Degree of Heterogeneity between Event Rates Using Likelihood. *American Journal of Epidemiology*, **141**, 369–374. [DOI: 10.1093/aje/141.4.369](https://doi.org/10.1093/aje/141.4.369)

MCPHERSON, K., WENNBERG, J. E., HOVIND, O. B. and CLIFFORD, P. (1982). Small-Area Variations in the Use of Common Surgical Procedures: An International Comparison of New England, England, and Norway. *New England Journal of Medicine*, **307**, 1310–1314. [DOI: 10.1056/NEJM198211183072104](https://doi.org/10.1056/NEJM198211183072104)

R CORE TEAM (2025). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

RAO, J. (2003). *Small Area Estimation*, Hoboken (NJ): John Wiley & Sons.
