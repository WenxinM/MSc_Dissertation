# MSc_Dissertation
## Data description: 
- IPUMS USA: ACS 1-year estimates
- Variable descriptions can be found via this search page: https://usa.ipums.org/usa-action/variables/search
- Start with all the variables that may be relevant. Manually drop some due to missing values etc. and use ML algorithm for further feature selection 
- 3 potential responses:
	- ``FERTYR``: "Women ages 15 to 50, regardless of marital status, were asked whether they had given birth to any children in the past 12 months. FERTYR reports their "yes" or "no" answer to this question."
	- ``NCHILD``: "NCHILD counts the number of own children (of any age or marital status) residing with each individual. NCHILD includes step-children and adopted children as well as biological children. Persons with no children present are coded "0." "
	- ``NCHLT5``: "NCHLT5 counts the number of own children age 4 and under residing with each individual. NCHLT5 includes step-children and adopted children as well as biological children. Persons with no children under 5 present are coded "0." "
	- ``ELDCH`` and ``YNGCH`` have more than 50% missing values and are not used in the analysis.

**Data source**: _IPUMS USA, University of Minnesota, www.ipums.org._

## Codes:
- Data cleaning and basic analysis in ``basic.R``. 

	- The ``ipumsr`` package is required to analyze the IPUMS data. Descriptions of this package can be found here: https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums.html
	- tbc
	

- Further analysis in ``analysis.py``
