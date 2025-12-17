* NOTE: You need to set the Stata working directory to the path
* where the data file is located.

set more off

clear
quietly infix                  ///
  int     year        1-4      ///
  long    sample      5-10     ///
  double  serial      11-18    ///
  double  cbserial    19-31    ///
  double  hhwt        32-41    ///
  double  cluster     42-54    ///
  double  cpi99       55-59    ///
  byte    statefip    60-61    ///
  int     countyfip   62-64    ///
  byte    metro       65-65    ///
  double  strata      66-77    ///
  byte    gq          78-78    ///
  long    hhincome    79-85    ///
  byte    qhhincome   86-86    ///
  int     pernum      87-90    ///
  double  perwt       91-100   ///
  byte    nchild      101-101  ///
  byte    nchlt5      102-102  ///
  byte    sex         103-103  ///
  int     age         104-106  ///
  byte    marst       107-107  ///
  byte    school      108-108  ///
  byte    educ        109-110  ///
  int     educd       111-113  ///
  byte    empstat     114-114  ///
  byte    empstatd    115-116  ///
  long    inctot      117-123  ///
  long    ftotinc     124-130  ///
  long    incwage     131-136  ///
  byte    migrate1    137-137  ///
  byte    migrate1d   138-139  ///
  int     migplac1    140-142  ///
  int     migcounty1  143-145  ///
  long    migmet131   146-150  ///
  byte    pwstate2    151-152  ///
  int     pwcounty    153-155  ///
  byte    qage        156-156  ///
  byte    qmarst      157-157  ///
  byte    qsex        158-158  ///
  byte    qempstat    159-159  ///
  byte    qincbus     160-160  ///
  byte    qincinvs    161-161  ///
  byte    qincothe    162-162  ///
  byte    qincreti    163-163  ///
  byte    qincss      164-164  ///
  byte    qinctot     165-165  ///
  byte    qincwage    166-166  ///
  byte    qincwelf    167-167  ///
  byte    qmigplc1    168-168  ///
  byte    qmigrat1    169-169  ///
  byte    qpwstat2    170-170  ///
  using `"usa_00168.dat"'

replace hhwt       = hhwt       / 100
replace cpi99      = cpi99      / 1000
replace perwt      = perwt      / 100

format serial     %8.0f
format cbserial   %13.0f
format hhwt       %10.2f
format cluster    %13.0f
format cpi99      %5.3f
format strata     %12.0f
format perwt      %10.2f

label var year       `"Census year"'
label var sample     `"IPUMS sample identifier"'
label var serial     `"Household serial number"'
label var cbserial   `"Original Census Bureau household serial number"'
label var hhwt       `"Household weight"'
label var cluster    `"Household cluster for variance estimation"'
label var cpi99      `"CPI-U adjustment factor to 1999 dollars"'
label var statefip   `"State (FIPS code)"'
label var countyfip  `"County (FIPS code, identifiable counties only)"'
label var metro      `"Metropolitan status (where determinable)"'
label var strata     `"Household strata for variance estimation"'
label var gq         `"Group quarters status"'
label var hhincome   `"Total household income "'
label var qhhincome  `"Flag for HHINCOME"'
label var pernum     `"Person number in sample unit"'
label var perwt      `"Person weight"'
label var nchild     `"Number of own children in the household"'
label var nchlt5     `"Number of own children under age 5 in household"'
label var sex        `"Sex"'
label var age        `"Age"'
label var marst      `"Marital status"'
label var school     `"School attendance"'
label var educ       `"Educational attainment [general version]"'
label var educd      `"Educational attainment [detailed version]"'
label var empstat    `"Employment status [general version]"'
label var empstatd   `"Employment status [detailed version]"'
label var inctot     `"Total personal income"'
label var ftotinc    `"Total family income"'
label var incwage    `"Wage and salary income"'
label var migrate1   `"Migration status 1 year ago [general version]"'
label var migrate1d  `"Migration status 1 year ago [detailed version]"'
label var migplac1   `"State or country of residence 1 year ago"'
label var migcounty1 `"County of residence 1 year ago (FIPS code)"'
label var migmet131  `"Metropolitan area of residence 1 year ago (2013 delineations)"'
label var pwstate2   `"Place of work: state"'
label var pwcounty   `"Place of work: county"'
label var qage       `"Flag for Age"'
label var qmarst     `"Flag for Marst"'
label var qsex       `"Flag for Sex"'
label var qempstat   `"Flag for Empstat, Labforce"'
label var qincbus    `"Flag for Incbus, Inctot, Incearn"'
label var qincinvs   `"Flag for Incinvst, Inctot"'
label var qincothe   `"Flag for Incother, Inctot"'
label var qincreti   `"Flag for Incretir, Inctot"'
label var qincss     `"Flag for Incss, Inctot"'
label var qinctot    `"Flag for Inctot"'
label var qincwage   `"Flag for Incwage, Inctot, Incearn"'
label var qincwelf   `"Flag for Incwelfr, Inctot"'
label var qmigplc1   `"Flag for Migplac1"'
label var qmigrat1   `"Flag for Migrate1"'
label var qpwstat2   `"Flag for Pwstate2 "'

label define year_lbl 1850 `"1850"'
label define year_lbl 1860 `"1860"', add
label define year_lbl 1870 `"1870"', add
label define year_lbl 1880 `"1880"', add
label define year_lbl 1900 `"1900"', add
label define year_lbl 1910 `"1910"', add
label define year_lbl 1920 `"1920"', add
label define year_lbl 1930 `"1930"', add
label define year_lbl 1940 `"1940"', add
label define year_lbl 1950 `"1950"', add
label define year_lbl 1960 `"1960"', add
label define year_lbl 1970 `"1970"', add
label define year_lbl 1980 `"1980"', add
label define year_lbl 1990 `"1990"', add
label define year_lbl 2000 `"2000"', add
label define year_lbl 2001 `"2001"', add
label define year_lbl 2002 `"2002"', add
label define year_lbl 2003 `"2003"', add
label define year_lbl 2004 `"2004"', add
label define year_lbl 2005 `"2005"', add
label define year_lbl 2006 `"2006"', add
label define year_lbl 2007 `"2007"', add
label define year_lbl 2008 `"2008"', add
label define year_lbl 2009 `"2009"', add
label define year_lbl 2010 `"2010"', add
label define year_lbl 2011 `"2011"', add
label define year_lbl 2012 `"2012"', add
label define year_lbl 2013 `"2013"', add
label define year_lbl 2014 `"2014"', add
label define year_lbl 2015 `"2015"', add
label define year_lbl 2016 `"2016"', add
label define year_lbl 2017 `"2017"', add
label define year_lbl 2018 `"2018"', add
label define year_lbl 2019 `"2019"', add
label define year_lbl 2020 `"2020"', add
label define year_lbl 2021 `"2021"', add
label define year_lbl 2022 `"2022"', add
label define year_lbl 2023 `"2023"', add
label values year year_lbl

label define sample_lbl 202304 `"2019-2023, PRCS 5-year"'
label define sample_lbl 202303 `"2019-2023, ACS 5-year"', add
label define sample_lbl 202302 `"2023 PRCS"', add
label define sample_lbl 202301 `"2023 ACS"', add
label define sample_lbl 202204 `"2018-2022, PRCS 5-year"', add
label define sample_lbl 202203 `"2018-2022, ACS 5-year"', add
label define sample_lbl 202202 `"2022 PRCS"', add
label define sample_lbl 202201 `"2022 ACS"', add
label define sample_lbl 202104 `"2017-2021, PRCS 5-year"', add
label define sample_lbl 202103 `"2017-2021, ACS 5-year"', add
label define sample_lbl 202102 `"2021 PRCS"', add
label define sample_lbl 202101 `"2021 ACS"', add
label define sample_lbl 202004 `"2016-2020, PRCS 5-year"', add
label define sample_lbl 202003 `"2016-2020, ACS 5-year"', add
label define sample_lbl 202001 `"2020 ACS"', add
label define sample_lbl 201904 `"2015-2019, PRCS 5-year"', add
label define sample_lbl 201903 `"2015-2019, ACS 5-year"', add
label define sample_lbl 201902 `"2019 PRCS"', add
label define sample_lbl 201901 `"2019 ACS"', add
label define sample_lbl 201804 `"2014-2018, PRCS 5-year"', add
label define sample_lbl 201803 `"2014-2018, ACS 5-year"', add
label define sample_lbl 201802 `"2018 PRCS"', add
label define sample_lbl 201801 `"2018 ACS"', add
label define sample_lbl 201704 `"2013-2017, PRCS 5-year"', add
label define sample_lbl 201703 `"2013-2017, ACS 5-year"', add
label define sample_lbl 201702 `"2017 PRCS"', add
label define sample_lbl 201701 `"2017 ACS"', add
label define sample_lbl 201604 `"2012-2016, PRCS 5-year"', add
label define sample_lbl 201603 `"2012-2016, ACS 5-year"', add
label define sample_lbl 201602 `"2016 PRCS"', add
label define sample_lbl 201601 `"2016 ACS"', add
label define sample_lbl 201504 `"2011-2015, PRCS 5-year"', add
label define sample_lbl 201503 `"2011-2015, ACS 5-year"', add
label define sample_lbl 201502 `"2015 PRCS"', add
label define sample_lbl 201501 `"2015 ACS"', add
label define sample_lbl 201404 `"2010-2014, PRCS 5-year"', add
label define sample_lbl 201403 `"2010-2014, ACS 5-year"', add
label define sample_lbl 201402 `"2014 PRCS"', add
label define sample_lbl 201401 `"2014 ACS"', add
label define sample_lbl 201306 `"2009-2013, PRCS 5-year"', add
label define sample_lbl 201305 `"2009-2013, ACS 5-year"', add
label define sample_lbl 201304 `"2011-2013, PRCS 3-year"', add
label define sample_lbl 201303 `"2011-2013, ACS 3-year"', add
label define sample_lbl 201302 `"2013 PRCS"', add
label define sample_lbl 201301 `"2013 ACS"', add
label define sample_lbl 201206 `"2008-2012, PRCS 5-year"', add
label define sample_lbl 201205 `"2008-2012, ACS 5-year"', add
label define sample_lbl 201204 `"2010-2012, PRCS 3-year"', add
label define sample_lbl 201203 `"2010-2012, ACS 3-year"', add
label define sample_lbl 201202 `"2012 PRCS"', add
label define sample_lbl 201201 `"2012 ACS"', add
label define sample_lbl 201106 `"2007-2011, PRCS 5-year"', add
label define sample_lbl 201105 `"2007-2011, ACS 5-year"', add
label define sample_lbl 201104 `"2009-2011, PRCS 3-year"', add
label define sample_lbl 201103 `"2009-2011, ACS 3-year"', add
label define sample_lbl 201102 `"2011 PRCS"', add
label define sample_lbl 201101 `"2011 ACS"', add
label define sample_lbl 201008 `"2010 Puerto Rico 10%"', add
label define sample_lbl 201007 `"2010 10%"', add
label define sample_lbl 201006 `"2006-2010, PRCS 5-year"', add
label define sample_lbl 201005 `"2006-2010, ACS 5-year"', add
label define sample_lbl 201004 `"2008-2010, PRCS 3-year"', add
label define sample_lbl 201003 `"2008-2010, ACS 3-year"', add
label define sample_lbl 201002 `"2010 PRCS"', add
label define sample_lbl 201001 `"2010 ACS"', add
label define sample_lbl 200906 `"2005-2009, PRCS 5-year"', add
label define sample_lbl 200905 `"2005-2009, ACS 5-year"', add
label define sample_lbl 200904 `"2007-2009, PRCS 3-year"', add
label define sample_lbl 200903 `"2007-2009, ACS 3-year"', add
label define sample_lbl 200902 `"2009 PRCS"', add
label define sample_lbl 200901 `"2009 ACS"', add
label define sample_lbl 200804 `"2006-2008, PRCS 3-year"', add
label define sample_lbl 200803 `"2006-2008, ACS 3-year"', add
label define sample_lbl 200802 `"2008 PRCS"', add
label define sample_lbl 200801 `"2008 ACS"', add
label define sample_lbl 200704 `"2005-2007, PRCS 3-year"', add
label define sample_lbl 200703 `"2005-2007, ACS 3-year"', add
label define sample_lbl 200702 `"2007 PRCS"', add
label define sample_lbl 200701 `"2007 ACS"', add
label define sample_lbl 200602 `"2006 PRCS"', add
label define sample_lbl 200601 `"2006 ACS"', add
label define sample_lbl 200502 `"2005 PRCS"', add
label define sample_lbl 200501 `"2005 ACS"', add
label define sample_lbl 200401 `"2004 ACS"', add
label define sample_lbl 200301 `"2003 ACS"', add
label define sample_lbl 200201 `"2002 ACS"', add
label define sample_lbl 200101 `"2001 ACS"', add
label define sample_lbl 200008 `"2000 Puerto Rico 1%"', add
label define sample_lbl 200007 `"2000 1%"', add
label define sample_lbl 200006 `"2000 Puerto Rico 1% sample (old version)"', add
label define sample_lbl 200005 `"2000 Puerto Rico 5%"', add
label define sample_lbl 200004 `"2000 ACS"', add
label define sample_lbl 200003 `"2000 Unweighted 1%"', add
label define sample_lbl 200002 `"2000 1% sample (old version)"', add
label define sample_lbl 200001 `"2000 5%"', add
label define sample_lbl 199007 `"1990 Puerto Rico 1%"', add
label define sample_lbl 199006 `"1990 Puerto Rico 5%"', add
label define sample_lbl 199005 `"1990 Labor Market Area"', add
label define sample_lbl 199004 `"1990 Elderly"', add
label define sample_lbl 199003 `"1990 Unweighted 1%"', add
label define sample_lbl 199002 `"1990 1%"', add
label define sample_lbl 199001 `"1990 5%"', add
label define sample_lbl 198007 `"1980 Puerto Rico 1%"', add
label define sample_lbl 198006 `"1980 Puerto Rico 5%"', add
label define sample_lbl 198005 `"1980 Detailed metro/non-metro"', add
label define sample_lbl 198004 `"1980 Labor Market Area"', add
label define sample_lbl 198003 `"1980 Urban/Rural"', add
label define sample_lbl 198002 `"1980 1%"', add
label define sample_lbl 198001 `"1980 5%"', add
label define sample_lbl 197009 `"1970 Puerto Rico Neighborhood"', add
label define sample_lbl 197008 `"1970 Puerto Rico Municipio"', add
label define sample_lbl 197007 `"1970 Puerto Rico State"', add
label define sample_lbl 197006 `"1970 Form 2 Neighborhood"', add
label define sample_lbl 197005 `"1970 Form 1 Neighborhood"', add
label define sample_lbl 197004 `"1970 Form 2 Metro"', add
label define sample_lbl 197003 `"1970 Form 1 Metro"', add
label define sample_lbl 197002 `"1970 Form 2 State"', add
label define sample_lbl 197001 `"1970 Form 1 State"', add
label define sample_lbl 196002 `"1960 5%"', add
label define sample_lbl 196001 `"1960 1%"', add
label define sample_lbl 195002 `"1950 100% database"', add
label define sample_lbl 195001 `"1950 1%"', add
label define sample_lbl 194002 `"1940 100% database"', add
label define sample_lbl 194001 `"1940 1%"', add
label define sample_lbl 193004 `"1930 100% database"', add
label define sample_lbl 193003 `"1930 Puerto Rico"', add
label define sample_lbl 193002 `"1930 5%"', add
label define sample_lbl 193001 `"1930 1%"', add
label define sample_lbl 192003 `"1920 100% database"', add
label define sample_lbl 192002 `"1920 Puerto Rico sample"', add
label define sample_lbl 192001 `"1920 1%"', add
label define sample_lbl 191004 `"1910 100% database"', add
label define sample_lbl 191003 `"1910 1.4% sample with oversamples"', add
label define sample_lbl 191002 `"1910 1%"', add
label define sample_lbl 191001 `"1910 Puerto Rico"', add
label define sample_lbl 190004 `"1900 100% database"', add
label define sample_lbl 190003 `"1900 1% sample with oversamples"', add
label define sample_lbl 190002 `"1900 1%"', add
label define sample_lbl 190001 `"1900 5%"', add
label define sample_lbl 188003 `"1880 100% database"', add
label define sample_lbl 188002 `"1880 10%"', add
label define sample_lbl 188001 `"1880 1%"', add
label define sample_lbl 187003 `"1870 100% database"', add
label define sample_lbl 187002 `"1870 1% sample with black oversample"', add
label define sample_lbl 187001 `"1870 1%"', add
label define sample_lbl 186003 `"1860 100% database"', add
label define sample_lbl 186002 `"1860 1% sample with black oversample"', add
label define sample_lbl 186001 `"1860 1%"', add
label define sample_lbl 185002 `"1850 100% database"', add
label define sample_lbl 185001 `"1850 1%"', add
label values sample sample_lbl

label define statefip_lbl 01 `"Alabama"'
label define statefip_lbl 02 `"Alaska"', add
label define statefip_lbl 04 `"Arizona"', add
label define statefip_lbl 05 `"Arkansas"', add
label define statefip_lbl 06 `"California"', add
label define statefip_lbl 08 `"Colorado"', add
label define statefip_lbl 09 `"Connecticut"', add
label define statefip_lbl 10 `"Delaware"', add
label define statefip_lbl 11 `"District of Columbia"', add
label define statefip_lbl 12 `"Florida"', add
label define statefip_lbl 13 `"Georgia"', add
label define statefip_lbl 15 `"Hawaii"', add
label define statefip_lbl 16 `"Idaho"', add
label define statefip_lbl 17 `"Illinois"', add
label define statefip_lbl 18 `"Indiana"', add
label define statefip_lbl 19 `"Iowa"', add
label define statefip_lbl 20 `"Kansas"', add
label define statefip_lbl 21 `"Kentucky"', add
label define statefip_lbl 22 `"Louisiana"', add
label define statefip_lbl 23 `"Maine"', add
label define statefip_lbl 24 `"Maryland"', add
label define statefip_lbl 25 `"Massachusetts"', add
label define statefip_lbl 26 `"Michigan"', add
label define statefip_lbl 27 `"Minnesota"', add
label define statefip_lbl 28 `"Mississippi"', add
label define statefip_lbl 29 `"Missouri"', add
label define statefip_lbl 30 `"Montana"', add
label define statefip_lbl 31 `"Nebraska"', add
label define statefip_lbl 32 `"Nevada"', add
label define statefip_lbl 33 `"New Hampshire"', add
label define statefip_lbl 34 `"New Jersey"', add
label define statefip_lbl 35 `"New Mexico"', add
label define statefip_lbl 36 `"New York"', add
label define statefip_lbl 37 `"North Carolina"', add
label define statefip_lbl 38 `"North Dakota"', add
label define statefip_lbl 39 `"Ohio"', add
label define statefip_lbl 40 `"Oklahoma"', add
label define statefip_lbl 41 `"Oregon"', add
label define statefip_lbl 42 `"Pennsylvania"', add
label define statefip_lbl 44 `"Rhode Island"', add
label define statefip_lbl 45 `"South Carolina"', add
label define statefip_lbl 46 `"South Dakota"', add
label define statefip_lbl 47 `"Tennessee"', add
label define statefip_lbl 48 `"Texas"', add
label define statefip_lbl 49 `"Utah"', add
label define statefip_lbl 50 `"Vermont"', add
label define statefip_lbl 51 `"Virginia"', add
label define statefip_lbl 53 `"Washington"', add
label define statefip_lbl 54 `"West Virginia"', add
label define statefip_lbl 55 `"Wisconsin"', add
label define statefip_lbl 56 `"Wyoming"', add
label define statefip_lbl 61 `"Maine-New Hampshire-Vermont"', add
label define statefip_lbl 62 `"Massachusetts-Rhode Island"', add
label define statefip_lbl 63 `"Minnesota-Iowa-Missouri-Kansas-Nebraska-S.Dakota-N.Dakota"', add
label define statefip_lbl 64 `"Maryland-Delaware"', add
label define statefip_lbl 65 `"Montana-Idaho-Wyoming"', add
label define statefip_lbl 66 `"Utah-Nevada"', add
label define statefip_lbl 67 `"Arizona-New Mexico"', add
label define statefip_lbl 68 `"Alaska-Hawaii"', add
label define statefip_lbl 72 `"Puerto Rico"', add
label define statefip_lbl 97 `"Military/Mil. Reservation"', add
label define statefip_lbl 99 `"State not identified"', add
label values statefip statefip_lbl

label define metro_lbl 0 `"Metropolitan status indeterminable (mixed)"'
label define metro_lbl 1 `"Not in metropolitan area"', add
label define metro_lbl 2 `"In metropolitan area: In central/principal city"', add
label define metro_lbl 3 `"In metropolitan area: Not in central/principal city"', add
label define metro_lbl 4 `"In metropolitan area: Central/principal city status indeterminable (mixed)"', add
label values metro metro_lbl

label define gq_lbl 0 `"Vacant unit"'
label define gq_lbl 1 `"Households under 1970 definition"', add
label define gq_lbl 2 `"Additional households under 1990 definition"', add
label define gq_lbl 3 `"Group quarters--Institutions"', add
label define gq_lbl 4 `"Other group quarters"', add
label define gq_lbl 5 `"Additional households under 2000 definition"', add
label define gq_lbl 6 `"Fragment"', add
label values gq gq_lbl

label define hhincome_lbl 9999998 `"9999998"'
label define hhincome_lbl 9999999 `"9999999"', add
label values hhincome hhincome_lbl

label define qhhincome_lbl 0 `"Original entry or Inapplicable (not in universe)"'
label define qhhincome_lbl 1 `"Failed edit"', add
label define qhhincome_lbl 2 `"Entry illegible"', add
label define qhhincome_lbl 3 `"Missing"', add
label define qhhincome_lbl 4 `"Allocated"', add
label define qhhincome_lbl 5 `"Illegible"', add
label define qhhincome_lbl 6 `"Missing"', add
label define qhhincome_lbl 7 `"Illegible"', add
label define qhhincome_lbl 8 `"Missing or failed edit"', add
label values qhhincome qhhincome_lbl

label define nchild_lbl 0 `"0 children present"'
label define nchild_lbl 1 `"1 child present"', add
label define nchild_lbl 2 `"2"', add
label define nchild_lbl 3 `"3"', add
label define nchild_lbl 4 `"4"', add
label define nchild_lbl 5 `"5"', add
label define nchild_lbl 6 `"6"', add
label define nchild_lbl 7 `"7"', add
label define nchild_lbl 8 `"8"', add
label define nchild_lbl 9 `"9+"', add
label values nchild nchild_lbl

label define nchlt5_lbl 0 `"No children under age 5"'
label define nchlt5_lbl 1 `"1 child under age 5"', add
label define nchlt5_lbl 2 `"2"', add
label define nchlt5_lbl 3 `"3"', add
label define nchlt5_lbl 4 `"4"', add
label define nchlt5_lbl 5 `"5"', add
label define nchlt5_lbl 6 `"6"', add
label define nchlt5_lbl 7 `"7"', add
label define nchlt5_lbl 8 `"8"', add
label define nchlt5_lbl 9 `"9+"', add
label values nchlt5 nchlt5_lbl

label define sex_lbl 1 `"Male"'
label define sex_lbl 2 `"Female"', add
label define sex_lbl 9 `"Missing/blank"', add
label values sex sex_lbl

label define age_lbl 000 `"Less than 1 year old"'
label define age_lbl 001 `"1"', add
label define age_lbl 002 `"2"', add
label define age_lbl 003 `"3"', add
label define age_lbl 004 `"4"', add
label define age_lbl 005 `"5"', add
label define age_lbl 006 `"6"', add
label define age_lbl 007 `"7"', add
label define age_lbl 008 `"8"', add
label define age_lbl 009 `"9"', add
label define age_lbl 010 `"10"', add
label define age_lbl 011 `"11"', add
label define age_lbl 012 `"12"', add
label define age_lbl 013 `"13"', add
label define age_lbl 014 `"14"', add
label define age_lbl 015 `"15"', add
label define age_lbl 016 `"16"', add
label define age_lbl 017 `"17"', add
label define age_lbl 018 `"18"', add
label define age_lbl 019 `"19"', add
label define age_lbl 020 `"20"', add
label define age_lbl 021 `"21"', add
label define age_lbl 022 `"22"', add
label define age_lbl 023 `"23"', add
label define age_lbl 024 `"24"', add
label define age_lbl 025 `"25"', add
label define age_lbl 026 `"26"', add
label define age_lbl 027 `"27"', add
label define age_lbl 028 `"28"', add
label define age_lbl 029 `"29"', add
label define age_lbl 030 `"30"', add
label define age_lbl 031 `"31"', add
label define age_lbl 032 `"32"', add
label define age_lbl 033 `"33"', add
label define age_lbl 034 `"34"', add
label define age_lbl 035 `"35"', add
label define age_lbl 036 `"36"', add
label define age_lbl 037 `"37"', add
label define age_lbl 038 `"38"', add
label define age_lbl 039 `"39"', add
label define age_lbl 040 `"40"', add
label define age_lbl 041 `"41"', add
label define age_lbl 042 `"42"', add
label define age_lbl 043 `"43"', add
label define age_lbl 044 `"44"', add
label define age_lbl 045 `"45"', add
label define age_lbl 046 `"46"', add
label define age_lbl 047 `"47"', add
label define age_lbl 048 `"48"', add
label define age_lbl 049 `"49"', add
label define age_lbl 050 `"50"', add
label define age_lbl 051 `"51"', add
label define age_lbl 052 `"52"', add
label define age_lbl 053 `"53"', add
label define age_lbl 054 `"54"', add
label define age_lbl 055 `"55"', add
label define age_lbl 056 `"56"', add
label define age_lbl 057 `"57"', add
label define age_lbl 058 `"58"', add
label define age_lbl 059 `"59"', add
label define age_lbl 060 `"60"', add
label define age_lbl 061 `"61"', add
label define age_lbl 062 `"62"', add
label define age_lbl 063 `"63"', add
label define age_lbl 064 `"64"', add
label define age_lbl 065 `"65"', add
label define age_lbl 066 `"66"', add
label define age_lbl 067 `"67"', add
label define age_lbl 068 `"68"', add
label define age_lbl 069 `"69"', add
label define age_lbl 070 `"70"', add
label define age_lbl 071 `"71"', add
label define age_lbl 072 `"72"', add
label define age_lbl 073 `"73"', add
label define age_lbl 074 `"74"', add
label define age_lbl 075 `"75"', add
label define age_lbl 076 `"76"', add
label define age_lbl 077 `"77"', add
label define age_lbl 078 `"78"', add
label define age_lbl 079 `"79"', add
label define age_lbl 080 `"80"', add
label define age_lbl 081 `"81"', add
label define age_lbl 082 `"82"', add
label define age_lbl 083 `"83"', add
label define age_lbl 084 `"84"', add
label define age_lbl 085 `"85"', add
label define age_lbl 086 `"86"', add
label define age_lbl 087 `"87"', add
label define age_lbl 088 `"88"', add
label define age_lbl 089 `"89"', add
label define age_lbl 090 `"90 (90+ in 1980 and 1990)"', add
label define age_lbl 091 `"91"', add
label define age_lbl 092 `"92"', add
label define age_lbl 093 `"93"', add
label define age_lbl 094 `"94"', add
label define age_lbl 095 `"95"', add
label define age_lbl 096 `"96"', add
label define age_lbl 097 `"97"', add
label define age_lbl 098 `"98"', add
label define age_lbl 099 `"99"', add
label define age_lbl 100 `"100 (100+ in 1960-1970)"', add
label define age_lbl 101 `"101"', add
label define age_lbl 102 `"102"', add
label define age_lbl 103 `"103"', add
label define age_lbl 104 `"104"', add
label define age_lbl 105 `"105"', add
label define age_lbl 106 `"106"', add
label define age_lbl 107 `"107"', add
label define age_lbl 108 `"108"', add
label define age_lbl 109 `"109"', add
label define age_lbl 110 `"110"', add
label define age_lbl 111 `"111"', add
label define age_lbl 112 `"112 (112+ in the 1980 internal data)"', add
label define age_lbl 113 `"113"', add
label define age_lbl 114 `"114"', add
label define age_lbl 115 `"115 (115+ in the 1990 internal data)"', add
label define age_lbl 116 `"116"', add
label define age_lbl 117 `"117"', add
label define age_lbl 118 `"118"', add
label define age_lbl 119 `"119"', add
label define age_lbl 120 `"120"', add
label define age_lbl 121 `"121"', add
label define age_lbl 122 `"122"', add
label define age_lbl 123 `"123"', add
label define age_lbl 124 `"124"', add
label define age_lbl 125 `"125"', add
label define age_lbl 126 `"126"', add
label define age_lbl 127 `"127"', add
label define age_lbl 128 `"128"', add
label define age_lbl 129 `"129"', add
label define age_lbl 130 `"130"', add
label define age_lbl 131 `"131"', add
label define age_lbl 132 `"132"', add
label define age_lbl 133 `"133"', add
label define age_lbl 134 `"134"', add
label define age_lbl 135 `"135"', add
label define age_lbl 140 `"140"', add
label define age_lbl 999 `"Missing"', add
label values age age_lbl

label define marst_lbl 1 `"Married, spouse present"'
label define marst_lbl 2 `"Married, spouse absent"', add
label define marst_lbl 3 `"Separated"', add
label define marst_lbl 4 `"Divorced"', add
label define marst_lbl 5 `"Widowed"', add
label define marst_lbl 6 `"Never married/single"', add
label define marst_lbl 9 `"Blank, missing"', add
label values marst marst_lbl

label define school_lbl 0 `"N/A"'
label define school_lbl 1 `"No, not in school"', add
label define school_lbl 2 `"Yes, in school"', add
label define school_lbl 8 `"Unknown"', add
label define school_lbl 9 `"Missing"', add
label values school school_lbl

label define educ_lbl 00 `"N/A or no schooling"'
label define educ_lbl 01 `"Nursery school to grade 4"', add
label define educ_lbl 02 `"Grade 5, 6, 7, or 8"', add
label define educ_lbl 03 `"Grade 9"', add
label define educ_lbl 04 `"Grade 10"', add
label define educ_lbl 05 `"Grade 11"', add
label define educ_lbl 06 `"Grade 12"', add
label define educ_lbl 07 `"1 year of college"', add
label define educ_lbl 08 `"2 years of college"', add
label define educ_lbl 09 `"3 years of college"', add
label define educ_lbl 10 `"4 years of college"', add
label define educ_lbl 11 `"5+ years of college"', add
label define educ_lbl 99 `"Missing"', add
label values educ educ_lbl

label define educd_lbl 000 `"N/A or no schooling"'
label define educd_lbl 001 `"N/A"', add
label define educd_lbl 002 `"No schooling completed"', add
label define educd_lbl 010 `"Nursery school to grade 4"', add
label define educd_lbl 011 `"Nursery school, preschool"', add
label define educd_lbl 012 `"Kindergarten"', add
label define educd_lbl 013 `"Grade 1, 2, 3, or 4"', add
label define educd_lbl 014 `"Grade 1"', add
label define educd_lbl 015 `"Grade 2"', add
label define educd_lbl 016 `"Grade 3"', add
label define educd_lbl 017 `"Grade 4"', add
label define educd_lbl 020 `"Grade 5, 6, 7, or 8"', add
label define educd_lbl 021 `"Grade 5 or 6"', add
label define educd_lbl 022 `"Grade 5"', add
label define educd_lbl 023 `"Grade 6"', add
label define educd_lbl 024 `"Grade 7 or 8"', add
label define educd_lbl 025 `"Grade 7"', add
label define educd_lbl 026 `"Grade 8"', add
label define educd_lbl 030 `"Grade 9"', add
label define educd_lbl 040 `"Grade 10"', add
label define educd_lbl 050 `"Grade 11"', add
label define educd_lbl 060 `"Grade 12"', add
label define educd_lbl 061 `"12th grade, no diploma"', add
label define educd_lbl 062 `"High school graduate or GED"', add
label define educd_lbl 063 `"Regular high school diploma"', add
label define educd_lbl 064 `"GED or alternative credential"', add
label define educd_lbl 065 `"Some college, but less than 1 year"', add
label define educd_lbl 070 `"1 year of college"', add
label define educd_lbl 071 `"1 or more years of college credit, no degree"', add
label define educd_lbl 080 `"2 years of college"', add
label define educd_lbl 081 `"Associate's degree, type not specified"', add
label define educd_lbl 082 `"Associate's degree, occupational program"', add
label define educd_lbl 083 `"Associate's degree, academic program"', add
label define educd_lbl 090 `"3 years of college"', add
label define educd_lbl 100 `"4 years of college"', add
label define educd_lbl 101 `"Bachelor's degree"', add
label define educd_lbl 110 `"5+ years of college"', add
label define educd_lbl 111 `"6 years of college (6+ in 1960-1970)"', add
label define educd_lbl 112 `"7 years of college"', add
label define educd_lbl 113 `"8+ years of college"', add
label define educd_lbl 114 `"Master's degree"', add
label define educd_lbl 115 `"Professional degree beyond a bachelor's degree"', add
label define educd_lbl 116 `"Doctoral degree"', add
label define educd_lbl 999 `"Missing"', add
label values educd educd_lbl

label define empstat_lbl 0 `"N/A"'
label define empstat_lbl 1 `"Employed"', add
label define empstat_lbl 2 `"Unemployed"', add
label define empstat_lbl 3 `"Not in labor force"', add
label define empstat_lbl 9 `"Unknown/Illegible"', add
label values empstat empstat_lbl

label define empstatd_lbl 00 `"N/A"'
label define empstatd_lbl 10 `"At work"', add
label define empstatd_lbl 11 `"At work, public emerg"', add
label define empstatd_lbl 12 `"Has job, not working"', add
label define empstatd_lbl 13 `"Armed forces"', add
label define empstatd_lbl 14 `"Armed forces--at work"', add
label define empstatd_lbl 15 `"Armed forces--not at work but with job"', add
label define empstatd_lbl 20 `"Unemployed"', add
label define empstatd_lbl 21 `"Unemp, exper worker"', add
label define empstatd_lbl 22 `"Unemp, new worker"', add
label define empstatd_lbl 30 `"Not in Labor Force"', add
label define empstatd_lbl 31 `"NILF, housework"', add
label define empstatd_lbl 32 `"NILF, unable to work"', add
label define empstatd_lbl 33 `"NILF, school"', add
label define empstatd_lbl 34 `"NILF, other"', add
label define empstatd_lbl 99 `"Unknown/Illegible"', add
label values empstatd empstatd_lbl

label define incwage_lbl 000000 `"000000"'
label define incwage_lbl 000100 `"000100"', add
label define incwage_lbl 000200 `"000200"', add
label define incwage_lbl 000300 `"000300"', add
label define incwage_lbl 000400 `"000400"', add
label define incwage_lbl 000500 `"000500"', add
label define incwage_lbl 000600 `"000600"', add
label define incwage_lbl 000700 `"000700"', add
label define incwage_lbl 000800 `"000800"', add
label define incwage_lbl 000900 `"000900"', add
label define incwage_lbl 001000 `"001000"', add
label define incwage_lbl 001100 `"001100"', add
label define incwage_lbl 001200 `"001200"', add
label define incwage_lbl 001300 `"001300"', add
label define incwage_lbl 001400 `"001400"', add
label define incwage_lbl 001500 `"001500"', add
label define incwage_lbl 001600 `"001600"', add
label define incwage_lbl 001700 `"001700"', add
label define incwage_lbl 001800 `"001800"', add
label define incwage_lbl 001900 `"001900"', add
label define incwage_lbl 002000 `"002000"', add
label define incwage_lbl 002100 `"002100"', add
label define incwage_lbl 002200 `"002200"', add
label define incwage_lbl 002300 `"002300"', add
label define incwage_lbl 002400 `"002400"', add
label define incwage_lbl 002500 `"002500"', add
label define incwage_lbl 002600 `"002600"', add
label define incwage_lbl 002700 `"002700"', add
label define incwage_lbl 002800 `"002800"', add
label define incwage_lbl 002900 `"002900"', add
label define incwage_lbl 003000 `"003000"', add
label define incwage_lbl 003100 `"003100"', add
label define incwage_lbl 003200 `"003200"', add
label define incwage_lbl 003300 `"003300"', add
label define incwage_lbl 003400 `"003400"', add
label define incwage_lbl 003500 `"003500"', add
label define incwage_lbl 003600 `"003600"', add
label define incwage_lbl 003700 `"003700"', add
label define incwage_lbl 003800 `"003800"', add
label define incwage_lbl 003900 `"003900"', add
label define incwage_lbl 004000 `"004000"', add
label define incwage_lbl 004100 `"004100"', add
label define incwage_lbl 004200 `"004200"', add
label define incwage_lbl 004300 `"004300"', add
label define incwage_lbl 004400 `"004400"', add
label define incwage_lbl 004500 `"004500"', add
label define incwage_lbl 004600 `"004600"', add
label define incwage_lbl 004700 `"004700"', add
label define incwage_lbl 004800 `"004800"', add
label define incwage_lbl 004900 `"004900"', add
label define incwage_lbl 005000 `"005000"', add
label define incwage_lbl 005100 `"005100"', add
label define incwage_lbl 005200 `"005200"', add
label define incwage_lbl 005300 `"005300"', add
label define incwage_lbl 005400 `"005400"', add
label define incwage_lbl 005500 `"005500"', add
label define incwage_lbl 005600 `"005600"', add
label define incwage_lbl 005700 `"005700"', add
label define incwage_lbl 005800 `"005800"', add
label define incwage_lbl 005900 `"005900"', add
label define incwage_lbl 006000 `"006000"', add
label define incwage_lbl 006100 `"006100"', add
label define incwage_lbl 006200 `"006200"', add
label define incwage_lbl 006300 `"006300"', add
label define incwage_lbl 006400 `"006400"', add
label define incwage_lbl 006500 `"006500"', add
label define incwage_lbl 006600 `"006600"', add
label define incwage_lbl 006700 `"006700"', add
label define incwage_lbl 006800 `"006800"', add
label define incwage_lbl 006900 `"006900"', add
label define incwage_lbl 007000 `"007000"', add
label define incwage_lbl 007100 `"007100"', add
label define incwage_lbl 007200 `"007200"', add
label define incwage_lbl 007300 `"007300"', add
label define incwage_lbl 007400 `"007400"', add
label define incwage_lbl 007500 `"007500"', add
label define incwage_lbl 007600 `"007600"', add
label define incwage_lbl 007700 `"007700"', add
label define incwage_lbl 007800 `"007800"', add
label define incwage_lbl 007900 `"007900"', add
label define incwage_lbl 008000 `"008000"', add
label define incwage_lbl 008100 `"008100"', add
label define incwage_lbl 008200 `"008200"', add
label define incwage_lbl 008300 `"008300"', add
label define incwage_lbl 008400 `"008400"', add
label define incwage_lbl 008500 `"008500"', add
label define incwage_lbl 008600 `"008600"', add
label define incwage_lbl 008700 `"008700"', add
label define incwage_lbl 008800 `"008800"', add
label define incwage_lbl 008900 `"008900"', add
label define incwage_lbl 009000 `"009000"', add
label define incwage_lbl 009100 `"009100"', add
label define incwage_lbl 009200 `"009200"', add
label define incwage_lbl 009300 `"009300"', add
label define incwage_lbl 009400 `"009400"', add
label define incwage_lbl 009500 `"009500"', add
label define incwage_lbl 009600 `"009600"', add
label define incwage_lbl 009700 `"009700"', add
label define incwage_lbl 009800 `"009800"', add
label define incwage_lbl 009900 `"009900"', add
label define incwage_lbl 010000 `"010000"', add
label define incwage_lbl 999998 `"Missing"', add
label define incwage_lbl 999999 `"N/A"', add
label values incwage incwage_lbl

label define migrate1_lbl 0 `"N/A"'
label define migrate1_lbl 1 `"Same house"', add
label define migrate1_lbl 2 `"Moved within state"', add
label define migrate1_lbl 3 `"Moved between states"', add
label define migrate1_lbl 4 `"Abroad one year ago"', add
label define migrate1_lbl 9 `"Unknown"', add
label values migrate1 migrate1_lbl

label define migrate1d_lbl 00 `"N/A"'
label define migrate1d_lbl 10 `"Same house"', add
label define migrate1d_lbl 20 `"Same state (migration status within state unknown)"', add
label define migrate1d_lbl 21 `"Different house, moved within county"', add
label define migrate1d_lbl 22 `"Different house, moved within state, between counties"', add
label define migrate1d_lbl 23 `"Different house, moved within state, within PUMA"', add
label define migrate1d_lbl 24 `"Different house, moved within state, between PUMAs"', add
label define migrate1d_lbl 25 `"Different house, unknown within state"', add
label define migrate1d_lbl 30 `"Different state (general)"', add
label define migrate1d_lbl 31 `"Moved between contigious states"', add
label define migrate1d_lbl 32 `"Moved between non-contiguous states"', add
label define migrate1d_lbl 40 `"Abroad one year ago"', add
label define migrate1d_lbl 90 `"Unknown"', add
label values migrate1d migrate1d_lbl

label define migplac1_lbl 000 `"N/A"'
label define migplac1_lbl 001 `"Alabama"', add
label define migplac1_lbl 002 `"Alaska"', add
label define migplac1_lbl 004 `"Arizona"', add
label define migplac1_lbl 005 `"Arkansas"', add
label define migplac1_lbl 006 `"California"', add
label define migplac1_lbl 008 `"Colorado"', add
label define migplac1_lbl 009 `"Connecticut"', add
label define migplac1_lbl 010 `"Delaware"', add
label define migplac1_lbl 011 `"District of Columbia"', add
label define migplac1_lbl 012 `"Florida"', add
label define migplac1_lbl 013 `"Georgia"', add
label define migplac1_lbl 015 `"Hawaii"', add
label define migplac1_lbl 016 `"Idaho"', add
label define migplac1_lbl 017 `"Illinois"', add
label define migplac1_lbl 018 `"Indiana"', add
label define migplac1_lbl 019 `"Iowa"', add
label define migplac1_lbl 020 `"Kansas"', add
label define migplac1_lbl 021 `"Kentucky"', add
label define migplac1_lbl 022 `"Louisiana"', add
label define migplac1_lbl 023 `"Maine"', add
label define migplac1_lbl 024 `"Maryland"', add
label define migplac1_lbl 025 `"Massachusetts"', add
label define migplac1_lbl 026 `"Michigan"', add
label define migplac1_lbl 027 `"Minnesota"', add
label define migplac1_lbl 028 `"Mississippi"', add
label define migplac1_lbl 029 `"Missouri"', add
label define migplac1_lbl 030 `"Montana"', add
label define migplac1_lbl 031 `"Nebraska"', add
label define migplac1_lbl 032 `"Nevada"', add
label define migplac1_lbl 033 `"New Hampshire"', add
label define migplac1_lbl 034 `"New Jersey"', add
label define migplac1_lbl 035 `"New Mexico"', add
label define migplac1_lbl 036 `"New York"', add
label define migplac1_lbl 037 `"North Carolina"', add
label define migplac1_lbl 038 `"North Dakota"', add
label define migplac1_lbl 039 `"Ohio"', add
label define migplac1_lbl 040 `"Oklahoma"', add
label define migplac1_lbl 041 `"Oregon"', add
label define migplac1_lbl 042 `"Pennsylvania"', add
label define migplac1_lbl 044 `"Rhode Island"', add
label define migplac1_lbl 045 `"South Carolina"', add
label define migplac1_lbl 046 `"South Dakota"', add
label define migplac1_lbl 047 `"Tennessee"', add
label define migplac1_lbl 048 `"Texas"', add
label define migplac1_lbl 049 `"Utah"', add
label define migplac1_lbl 050 `"Vermont"', add
label define migplac1_lbl 051 `"Virginia"', add
label define migplac1_lbl 053 `"Washington"', add
label define migplac1_lbl 054 `"West Virginia"', add
label define migplac1_lbl 055 `"Wisconsin"', add
label define migplac1_lbl 056 `"Wyoming"', add
label define migplac1_lbl 099 `"United States, ns"', add
label define migplac1_lbl 100 `"Samoa, 1950"', add
label define migplac1_lbl 105 `"Guam"', add
label define migplac1_lbl 110 `"Puerto Rico"', add
label define migplac1_lbl 115 `"Virgin Islands"', add
label define migplac1_lbl 120 `"Other US Possessions"', add
label define migplac1_lbl 150 `"Canada"', add
label define migplac1_lbl 151 `"English Canada"', add
label define migplac1_lbl 152 `"French Canada"', add
label define migplac1_lbl 160 `"Atlantic Islands"', add
label define migplac1_lbl 200 `"Mexico"', add
label define migplac1_lbl 211 `"Belize/British Honduras"', add
label define migplac1_lbl 212 `"Costa Rica"', add
label define migplac1_lbl 213 `"El Salvador"', add
label define migplac1_lbl 214 `"Guatemala"', add
label define migplac1_lbl 215 `"Honduras"', add
label define migplac1_lbl 216 `"Nicaragua"', add
label define migplac1_lbl 217 `"Panama"', add
label define migplac1_lbl 218 `"Canal Zone"', add
label define migplac1_lbl 219 `"Central America, nec"', add
label define migplac1_lbl 250 `"Cuba"', add
label define migplac1_lbl 261 `"Dominican Republic"', add
label define migplac1_lbl 262 `"Haiti"', add
label define migplac1_lbl 263 `"Jamaica"', add
label define migplac1_lbl 264 `"British West Indies"', add
label define migplac1_lbl 267 `"Other West Indies"', add
label define migplac1_lbl 290 `"Other Caribbean and North America"', add
label define migplac1_lbl 305 `"Argentina"', add
label define migplac1_lbl 310 `"Bolivia"', add
label define migplac1_lbl 315 `"Brazil"', add
label define migplac1_lbl 320 `"Chile"', add
label define migplac1_lbl 325 `"Colombia"', add
label define migplac1_lbl 330 `"Ecuador"', add
label define migplac1_lbl 345 `"Paraguay"', add
label define migplac1_lbl 350 `"Peru"', add
label define migplac1_lbl 360 `"Uruguay"', add
label define migplac1_lbl 365 `"Venezuela"', add
label define migplac1_lbl 390 `"South America, nec"', add
label define migplac1_lbl 400 `"Denmark"', add
label define migplac1_lbl 401 `"Finland"', add
label define migplac1_lbl 402 `"Iceland"', add
label define migplac1_lbl 404 `"Norway"', add
label define migplac1_lbl 405 `"Sweden"', add
label define migplac1_lbl 410 `"England"', add
label define migplac1_lbl 411 `"Scotland"', add
label define migplac1_lbl 412 `"Wales"', add
label define migplac1_lbl 413 `"United Kingdom (excluding England: 2005ACS)"', add
label define migplac1_lbl 414 `"Ireland"', add
label define migplac1_lbl 415 `"Northern Ireland"', add
label define migplac1_lbl 419 `"Other Northern Europe"', add
label define migplac1_lbl 420 `"Belgium"', add
label define migplac1_lbl 421 `"France"', add
label define migplac1_lbl 422 `"Luxembourg"', add
label define migplac1_lbl 425 `"Netherlands"', add
label define migplac1_lbl 426 `"Switzerland"', add
label define migplac1_lbl 429 `"Other Western Europe"', add
label define migplac1_lbl 430 `"Albania"', add
label define migplac1_lbl 433 `"Greece"', add
label define migplac1_lbl 434 `"Dodecanese Islands"', add
label define migplac1_lbl 435 `"Italy"', add
label define migplac1_lbl 436 `"Portugal"', add
label define migplac1_lbl 437 `"Azores"', add
label define migplac1_lbl 438 `"Spain"', add
label define migplac1_lbl 450 `"Austria"', add
label define migplac1_lbl 451 `"Bulgaria"', add
label define migplac1_lbl 452 `"Czechoslovakia"', add
label define migplac1_lbl 453 `"Germany"', add
label define migplac1_lbl 454 `"Hungary"', add
label define migplac1_lbl 455 `"Poland"', add
label define migplac1_lbl 456 `"Romania"', add
label define migplac1_lbl 457 `"Yugoslavia"', add
label define migplac1_lbl 458 `"Bosnia and Herzegovinia"', add
label define migplac1_lbl 459 `"Other Eastern Europe"', add
label define migplac1_lbl 460 `"Estonia"', add
label define migplac1_lbl 461 `"Latvia"', add
label define migplac1_lbl 462 `"Lithuania"', add
label define migplac1_lbl 463 `"Other Northern or Eastern Europe"', add
label define migplac1_lbl 465 `"USSR"', add
label define migplac1_lbl 498 `"Ukraine"', add
label define migplac1_lbl 499 `"Europe, ns"', add
label define migplac1_lbl 500 `"China"', add
label define migplac1_lbl 501 `"Japan"', add
label define migplac1_lbl 502 `"Korea"', add
label define migplac1_lbl 503 `"Taiwan"', add
label define migplac1_lbl 515 `"Philippines"', add
label define migplac1_lbl 517 `"Thailand"', add
label define migplac1_lbl 518 `"Vietnam"', add
label define migplac1_lbl 519 `"Other South East Asia"', add
label define migplac1_lbl 520 `"Nepal"', add
label define migplac1_lbl 521 `"India"', add
label define migplac1_lbl 522 `"Iran"', add
label define migplac1_lbl 523 `"Iraq"', add
label define migplac1_lbl 525 `"Pakistan"', add
label define migplac1_lbl 534 `"Israel/Palestine"', add
label define migplac1_lbl 535 `"Jordan"', add
label define migplac1_lbl 537 `"Lebanon"', add
label define migplac1_lbl 539 `"United Arab Emirates"', add
label define migplac1_lbl 540 `"Saudi Arabia"', add
label define migplac1_lbl 541 `"Syria"', add
label define migplac1_lbl 542 `"Turkey"', add
label define migplac1_lbl 543 `"Afghanistan"', add
label define migplac1_lbl 551 `"Other Western Asia"', add
label define migplac1_lbl 599 `"Asia, nec"', add
label define migplac1_lbl 600 `"Africa"', add
label define migplac1_lbl 610 `"Northern Africa"', add
label define migplac1_lbl 611 `"Egypt"', add
label define migplac1_lbl 619 `"Nigeria"', add
label define migplac1_lbl 620 `"Western Africa"', add
label define migplac1_lbl 621 `"Eastern Africa"', add
label define migplac1_lbl 622 `"Ethiopia"', add
label define migplac1_lbl 623 `"Kenya"', add
label define migplac1_lbl 694 `"South Africa (Union of)"', add
label define migplac1_lbl 699 `"Africa, nec"', add
label define migplac1_lbl 701 `"Australia"', add
label define migplac1_lbl 702 `"New Zealand"', add
label define migplac1_lbl 710 `"Pacific Islands (Australia and New Zealand Subregions, not specified, Oceania and at Sea: ACS)"', add
label define migplac1_lbl 900 `"Abroad (unknown) or at sea"', add
label define migplac1_lbl 988 `"Suppressed for data year 2022 for select cases"', add
label define migplac1_lbl 997 `"Unknown value"', add
label define migplac1_lbl 999 `"Missing"', add
label values migplac1 migplac1_lbl

label define migmet131_lbl 00000 `"Not in universe or not in identifiable area"'
label define migmet131_lbl 10420 `"Akron, OH"', add
label define migmet131_lbl 10580 `"Albany-Schenectady-Troy, NY"', add
label define migmet131_lbl 10740 `"Albuquerque, NM"', add
label define migmet131_lbl 10780 `"Alexandria, LA"', add
label define migmet131_lbl 10900 `"Allentown-Bethlehem-Easton, PA-NJ"', add
label define migmet131_lbl 11020 `"Altoona, PA"', add
label define migmet131_lbl 11100 `"Amarillo, TX"', add
label define migmet131_lbl 11260 `"Anchorage, AK"', add
label define migmet131_lbl 11460 `"Ann Arbor, MI"', add
label define migmet131_lbl 11500 `"Anniston-Oxford-Jacksonville, AL"', add
label define migmet131_lbl 11700 `"Asheville, NC"', add
label define migmet131_lbl 12020 `"Athens-Clarke County, GA"', add
label define migmet131_lbl 12060 `"Atlanta-Sandy Springs-Roswell, GA"', add
label define migmet131_lbl 12100 `"Atlantic City-Hammonton, NJ"', add
label define migmet131_lbl 12220 `"Auburn-Opelika, AL"', add
label define migmet131_lbl 12260 `"Augusta-Richmond County, GA-SC"', add
label define migmet131_lbl 12420 `"Austin-Round Rock, TX"', add
label define migmet131_lbl 12540 `"Bakersfield, CA"', add
label define migmet131_lbl 12580 `"Baltimore-Columbia-Towson, MD"', add
label define migmet131_lbl 12620 `"Bangor, ME"', add
label define migmet131_lbl 12700 `"Barnstable Town, MA"', add
label define migmet131_lbl 12940 `"Baton Rouge, LA"', add
label define migmet131_lbl 12980 `"Battle Creek, MI"', add
label define migmet131_lbl 13140 `"Beaumont-Port Arthur, TX"', add
label define migmet131_lbl 13220 `"Beckley, WV"', add
label define migmet131_lbl 13380 `"Bellingham, WA"', add
label define migmet131_lbl 13460 `"Bend-Redmond, OR"', add
label define migmet131_lbl 13740 `"Billings, MT"', add
label define migmet131_lbl 13780 `"Binghamton, NY"', add
label define migmet131_lbl 13820 `"Birmingham-Hoover, AL"', add
label define migmet131_lbl 13900 `"Bismarck, ND"', add
label define migmet131_lbl 13980 `"Blacksburg-Christiansburg-Radford, VA"', add
label define migmet131_lbl 14010 `"Bloomington, IL"', add
label define migmet131_lbl 14020 `"Bloomington, IN"', add
label define migmet131_lbl 14260 `"Boise City, ID"', add
label define migmet131_lbl 14460 `"Boston-Cambridge-Newton, MA-NH"', add
label define migmet131_lbl 14740 `"Bremerton-Silverdale, WA"', add
label define migmet131_lbl 14860 `"Bridgeport-Stamford-Norwalk, CT"', add
label define migmet131_lbl 15180 `"Brownsville-Harlingen, TX"', add
label define migmet131_lbl 15380 `"Buffalo-Cheektowaga-Niagara Falls, NY"', add
label define migmet131_lbl 15500 `"Burlington, NC"', add
label define migmet131_lbl 15540 `"Burlington-South Burlington, VT"', add
label define migmet131_lbl 15940 `"Canton-Massillon, OH"', add
label define migmet131_lbl 15980 `"Cape Coral-Fort Myers, FL"', add
label define migmet131_lbl 16580 `"Champaign-Urbana, IL"', add
label define migmet131_lbl 16620 `"Charleston, WV"', add
label define migmet131_lbl 16700 `"Charleston-North Charleston, SC"', add
label define migmet131_lbl 16740 `"Charlotte-Concord-Gastonia, NC-SC"', add
label define migmet131_lbl 16820 `"Charlottesville, VA"', add
label define migmet131_lbl 16860 `"Chattanooga, TN-GA"', add
label define migmet131_lbl 16940 `"Cheyenne, WY"', add
label define migmet131_lbl 16980 `"Chicago-Naperville-Elgin, IL-IN-WI"', add
label define migmet131_lbl 17020 `"Chico, CA"', add
label define migmet131_lbl 17140 `"Cincinnati, OH-KY-IN"', add
label define migmet131_lbl 17300 `"Clarksville, TN-KY"', add
label define migmet131_lbl 17420 `"Cleveland, TN"', add
label define migmet131_lbl 17460 `"Cleveland-Elyria, OH"', add
label define migmet131_lbl 17660 `"Coeur d'Alene, ID"', add
label define migmet131_lbl 17780 `"College Station-Bryan, TX"', add
label define migmet131_lbl 17820 `"Colorado Springs, CO"', add
label define migmet131_lbl 17860 `"Columbia, MO"', add
label define migmet131_lbl 17900 `"Columbia, SC"', add
label define migmet131_lbl 18140 `"Columbus, OH"', add
label define migmet131_lbl 18580 `"Corpus Christi, TX"', add
label define migmet131_lbl 18880 `"Crestview-Fort Walton Beach-Destin, FL"', add
label define migmet131_lbl 19100 `"Dallas-Fort Worth-Arlington, TX"', add
label define migmet131_lbl 19300 `"Daphne-Fairhope-Foley, AL"', add
label define migmet131_lbl 19340 `"Davenport-Moline-Rock Island, IA-IL"', add
label define migmet131_lbl 19380 `"Dayton, OH"', add
label define migmet131_lbl 19460 `"Decatur, AL"', add
label define migmet131_lbl 19500 `"Decatur, IL"', add
label define migmet131_lbl 19660 `"Deltona-Daytona Beach-Ormond Beach, FL"', add
label define migmet131_lbl 19740 `"Denver-Aurora-Lakewood, CO"', add
label define migmet131_lbl 19780 `"Des Moines-West Des Moines, IA"', add
label define migmet131_lbl 19820 `"Detroit-Warren-Dearborn, MI"', add
label define migmet131_lbl 20020 `"Dothan, AL"', add
label define migmet131_lbl 20100 `"Dover, DE"', add
label define migmet131_lbl 20500 `"Durham-Chapel Hill, NC"', add
label define migmet131_lbl 20700 `"East Stroudsburg, PA"', add
label define migmet131_lbl 20740 `"Eau Claire, WI"', add
label define migmet131_lbl 20940 `"El Centro, CA"', add
label define migmet131_lbl 21060 `"Elizabethtown-Fort Knox, KY"', add
label define migmet131_lbl 21140 `"Elkhart-Goshen, IN"', add
label define migmet131_lbl 21340 `"El Paso, TX"', add
label define migmet131_lbl 21500 `"Erie, PA"', add
label define migmet131_lbl 21660 `"Eugene, OR"', add
label define migmet131_lbl 21780 `"Evansville, IN-KY"', add
label define migmet131_lbl 22140 `"Farmington, NM"', add
label define migmet131_lbl 22180 `"Fayetteville, NC"', add
label define migmet131_lbl 22220 `"Fayetteville-Springdale-Rogers, AR-MO"', add
label define migmet131_lbl 22380 `"Flagstaff, AZ"', add
label define migmet131_lbl 22420 `"Flint, MI"', add
label define migmet131_lbl 22500 `"Florence, SC"', add
label define migmet131_lbl 22520 `"Florence-Muscle Shoals, AL"', add
label define migmet131_lbl 22660 `"Fort Collins, CO"', add
label define migmet131_lbl 23060 `"Fort Wayne, IN"', add
label define migmet131_lbl 23420 `"Fresno, CA"', add
label define migmet131_lbl 23460 `"Gadsden, AL"', add
label define migmet131_lbl 23540 `"Gainesville, FL"', add
label define migmet131_lbl 23580 `"Gainesville, GA"', add
label define migmet131_lbl 24020 `"Glens Falls, NY"', add
label define migmet131_lbl 24140 `"Goldsboro, NC"', add
label define migmet131_lbl 24300 `"Grand Junction, CO"', add
label define migmet131_lbl 24340 `"Grand Rapids-Wyoming, MI"', add
label define migmet131_lbl 24540 `"Greeley, CO"', add
label define migmet131_lbl 24660 `"Greensboro-High Point, NC"', add
label define migmet131_lbl 24780 `"Greenville, NC"', add
label define migmet131_lbl 24860 `"Greenville-Anderson-Mauldin, SC"', add
label define migmet131_lbl 25060 `"Gulfport-Biloxi-Pascagoula, MS"', add
label define migmet131_lbl 25220 `"Hammond, LA"', add
label define migmet131_lbl 25260 `"Hanford-Corcoran, CA"', add
label define migmet131_lbl 25420 `"Harrisburg-Carlisle, PA"', add
label define migmet131_lbl 25500 `"Harrisonburg, VA"', add
label define migmet131_lbl 25540 `"Hartford-West Hartford-East Hartford, CT"', add
label define migmet131_lbl 25620 `"Hattiesburg, MS"', add
label define migmet131_lbl 25860 `"Hickory-Lenoir-Morganton, NC"', add
label define migmet131_lbl 25940 `"Hilton Head Island-Bluffton-Beaufort, SC"', add
label define migmet131_lbl 26140 `"Homosassa Springs, FL"', add
label define migmet131_lbl 26380 `"Houma-Thibodaux, LA"', add
label define migmet131_lbl 26420 `"Houston-The Woodlands-Sugar Land, TX"', add
label define migmet131_lbl 26620 `"Huntsville, AL"', add
label define migmet131_lbl 26900 `"Indianapolis-Carmel-Anderson, IN"', add
label define migmet131_lbl 26980 `"Iowa City, IA"', add
label define migmet131_lbl 27060 `"Ithaca, NY"', add
label define migmet131_lbl 27100 `"Jackson, MI"', add
label define migmet131_lbl 27140 `"Jackson, MS"', add
label define migmet131_lbl 27180 `"Jackson, TN"', add
label define migmet131_lbl 27260 `"Jacksonville, FL"', add
label define migmet131_lbl 27340 `"Jacksonville, NC"', add
label define migmet131_lbl 27500 `"Janesville-Beloit, WI"', add
label define migmet131_lbl 27620 `"Jefferson City, MO"', add
label define migmet131_lbl 27740 `"Johnson City, TN"', add
label define migmet131_lbl 27780 `"Johnstown, PA"', add
label define migmet131_lbl 27860 `"Jonesboro, AR"', add
label define migmet131_lbl 27900 `"Joplin, MO"', add
label define migmet131_lbl 28020 `"Kalamazoo-Portage, MI"', add
label define migmet131_lbl 28100 `"Kankakee, IL"', add
label define migmet131_lbl 28140 `"Kansas City, MO-KS"', add
label define migmet131_lbl 28420 `"Kennewick-Richland, WA"', add
label define migmet131_lbl 28660 `"Killeen-Temple, TX"', add
label define migmet131_lbl 28700 `"Kingsport-Bristol-Bristol, TN-VA"', add
label define migmet131_lbl 28940 `"Knoxville, TN"', add
label define migmet131_lbl 29100 `"La Crosse-Onalaska, WI-MN"', add
label define migmet131_lbl 29180 `"Lafayette, LA"', add
label define migmet131_lbl 29200 `"Lafayette-West Lafayette, IN"', add
label define migmet131_lbl 29340 `"Lake Charles, LA"', add
label define migmet131_lbl 29420 `"Lake Havasu City-Kingman, AZ"', add
label define migmet131_lbl 29460 `"Lakeland-Winter Haven, FL"', add
label define migmet131_lbl 29540 `"Lancaster, PA"', add
label define migmet131_lbl 29620 `"Lansing-East Lansing, MI"', add
label define migmet131_lbl 29700 `"Laredo, TX"', add
label define migmet131_lbl 29740 `"Las Cruces, NM"', add
label define migmet131_lbl 29820 `"Las Vegas-Henderson-Paradise, NV"', add
label define migmet131_lbl 29940 `"Lawrence, KS"', add
label define migmet131_lbl 30020 `"Lawton, OK"', add
label define migmet131_lbl 30140 `"Lebanon, PA"', add
label define migmet131_lbl 30340 `"Lewiston-Auburn, ME"', add
label define migmet131_lbl 30620 `"Lima, OH"', add
label define migmet131_lbl 30700 `"Lincoln, NE"', add
label define migmet131_lbl 30780 `"Little Rock-North Little Rock-Conway, AR"', add
label define migmet131_lbl 31080 `"Los Angeles-Long Beach-Anaheim, CA"', add
label define migmet131_lbl 31140 `"Louisville/Jefferson County, KY-IN"', add
label define migmet131_lbl 31180 `"Lubbock, TX"', add
label define migmet131_lbl 31340 `"Lynchburg, VA"', add
label define migmet131_lbl 31460 `"Madera, CA"', add
label define migmet131_lbl 31700 `"Manchester-Nashua, NH"', add
label define migmet131_lbl 31860 `"Mankato-North Mankato, MN"', add
label define migmet131_lbl 31900 `"Mansfield, OH"', add
label define migmet131_lbl 32420 `"Mayagüez, PR"', add
label define migmet131_lbl 32580 `"McAllen-Edinburg-Mission, TX"', add
label define migmet131_lbl 32780 `"Medford, OR"', add
label define migmet131_lbl 32820 `"Memphis, TN-MS-AR"', add
label define migmet131_lbl 32900 `"Merced, CA"', add
label define migmet131_lbl 33100 `"Miami-Fort Lauderdale-West Palm Beach, FL"', add
label define migmet131_lbl 33140 `"Michigan City-La Porte, IN"', add
label define migmet131_lbl 33260 `"Midland, TX"', add
label define migmet131_lbl 33340 `"Milwaukee-Waukesha-West Allis, WI"', add
label define migmet131_lbl 33460 `"Minneapolis-St. Paul-Bloomington, MN-WI"', add
label define migmet131_lbl 33660 `"Mobile, AL"', add
label define migmet131_lbl 33700 `"Modesto, CA"', add
label define migmet131_lbl 33740 `"Monroe, LA"', add
label define migmet131_lbl 33780 `"Monroe, MI"', add
label define migmet131_lbl 33860 `"Montgomery, AL"', add
label define migmet131_lbl 34060 `"Morgantown, WV"', add
label define migmet131_lbl 34580 `"Mount Vernon-Anacortes, WA"', add
label define migmet131_lbl 34620 `"Muncie, IN"', add
label define migmet131_lbl 34740 `"Muskegon, MI"', add
label define migmet131_lbl 34820 `"Myrtle Beach-Conway-North Myrtle Beach, SC-NC"', add
label define migmet131_lbl 34900 `"Napa, CA"', add
label define migmet131_lbl 34940 `"Naples-Immokalee-Marco Island, FL"', add
label define migmet131_lbl 34980 `"Nashville-Davidson--Murfreesboro--Franklin, TN"', add
label define migmet131_lbl 35300 `"New Haven-Milford, CT"', add
label define migmet131_lbl 35380 `"New Orleans-Metairie, LA"', add
label define migmet131_lbl 35620 `"New York-Newark-Jersey City, NY-NJ-PA"', add
label define migmet131_lbl 35660 `"Niles-Benton Harbor, MI"', add
label define migmet131_lbl 35840 `"North Port-Sarasota-Bradenton, FL"', add
label define migmet131_lbl 35980 `"Norwich-New London, CT"', add
label define migmet131_lbl 36100 `"Ocala, FL"', add
label define migmet131_lbl 36140 `"Ocean City, NJ"', add
label define migmet131_lbl 36220 `"Odessa, TX"', add
label define migmet131_lbl 36260 `"Ogden-Clearfield, UT"', add
label define migmet131_lbl 36420 `"Oklahoma City, OK"', add
label define migmet131_lbl 36500 `"Olympia-Tumwater, WA"', add
label define migmet131_lbl 36540 `"Omaha-Council Bluffs, NE-IA"', add
label define migmet131_lbl 36740 `"Orlando-Kissimmee-Sanford, FL"', add
label define migmet131_lbl 36780 `"Oshkosh-Neenah, WI"', add
label define migmet131_lbl 36980 `"Owensboro, KY"', add
label define migmet131_lbl 37100 `"Oxnard-Thousand Oaks-Ventura, CA"', add
label define migmet131_lbl 37340 `"Palm Bay-Melbourne-Titusville, FL"', add
label define migmet131_lbl 37460 `"Panama City, FL"', add
label define migmet131_lbl 37620 `"Parkersburg-Vienna, WV"', add
label define migmet131_lbl 37860 `"Pensacola-Ferry Pass-Brent, FL"', add
label define migmet131_lbl 37900 `"Peoria, IL"', add
label define migmet131_lbl 37980 `"Philadelphia-Camden-Wilmington, PA-NJ-DE-MD"', add
label define migmet131_lbl 38060 `"Phoenix-Mesa-Scottsdale, AZ"', add
label define migmet131_lbl 38300 `"Pittsburgh, PA"', add
label define migmet131_lbl 38340 `"Pittsfield, MA"', add
label define migmet131_lbl 38660 `"Ponce, PR"', add
label define migmet131_lbl 38860 `"Portland-South Portland, ME"', add
label define migmet131_lbl 38900 `"Portland-Vancouver-Hillsboro, OR-WA"', add
label define migmet131_lbl 38940 `"Port St. Lucie, FL"', add
label define migmet131_lbl 39140 `"Prescott, AZ"', add
label define migmet131_lbl 39300 `"Providence-Warwick, RI-MA"', add
label define migmet131_lbl 39340 `"Provo-Orem, UT"', add
label define migmet131_lbl 39380 `"Pueblo, CO"', add
label define migmet131_lbl 39460 `"Punta Gorda, FL"', add
label define migmet131_lbl 39540 `"Racine, WI"', add
label define migmet131_lbl 39580 `"Raleigh, NC"', add
label define migmet131_lbl 39740 `"Reading, PA"', add
label define migmet131_lbl 39820 `"Redding, CA"', add
label define migmet131_lbl 39900 `"Reno, NV"', add
label define migmet131_lbl 40060 `"Richmond, VA"', add
label define migmet131_lbl 40140 `"Riverside-San Bernardino-Ontario, CA"', add
label define migmet131_lbl 40220 `"Roanoke, VA"', add
label define migmet131_lbl 40380 `"Rochester, NY"', add
label define migmet131_lbl 40420 `"Rockford, IL"', add
label define migmet131_lbl 40580 `"Rocky Mount, NC"', add
label define migmet131_lbl 40900 `"Sacramento--Roseville--Arden-Arcade, CA"', add
label define migmet131_lbl 40980 `"Saginaw, MI"', add
label define migmet131_lbl 41060 `"St. Cloud, MN"', add
label define migmet131_lbl 41100 `"St. George, UT"', add
label define migmet131_lbl 41140 `"St. Joseph, MO-KS"', add
label define migmet131_lbl 41180 `"St. Louis, MO-IL"', add
label define migmet131_lbl 41420 `"Salem, OR"', add
label define migmet131_lbl 41500 `"Salinas, CA"', add
label define migmet131_lbl 41540 `"Salisbury, MD-DE"', add
label define migmet131_lbl 41620 `"Salt Lake City, UT"', add
label define migmet131_lbl 41660 `"San Angelo, TX"', add
label define migmet131_lbl 41700 `"San Antonio-New Braunfels, TX"', add
label define migmet131_lbl 41740 `"San Diego-Carlsbad, CA"', add
label define migmet131_lbl 41860 `"San Francisco-Oakland-Hayward, CA"', add
label define migmet131_lbl 41900 `"San Germán, PR"', add
label define migmet131_lbl 41940 `"San Jose-Sunnyvale-Santa Clara, CA"', add
label define migmet131_lbl 41980 `"San Juan-Carolina-Caguas, PR"', add
label define migmet131_lbl 42020 `"San Luis Obispo-Paso Robles-Arroyo Grande, CA"', add
label define migmet131_lbl 42100 `"Santa Cruz-Watsonville, CA"', add
label define migmet131_lbl 42140 `"Santa Fe, NM"', add
label define migmet131_lbl 42200 `"Santa Maria-Santa Barbara, CA"', add
label define migmet131_lbl 42220 `"Santa Rosa, CA"', add
label define migmet131_lbl 42540 `"Scranton--Wilkes-Barre--Hazleton, PA"', add
label define migmet131_lbl 42660 `"Seattle-Tacoma-Bellevue, WA"', add
label define migmet131_lbl 42680 `"Sebastian-Vero Beach, FL"', add
label define migmet131_lbl 43100 `"Sheboygan, WI"', add
label define migmet131_lbl 43340 `"Shreveport-Bossier City, LA"', add
label define migmet131_lbl 43900 `"Spartanburg, SC"', add
label define migmet131_lbl 44060 `"Spokane-Spokane Valley, WA"', add
label define migmet131_lbl 44100 `"Springfield, IL"', add
label define migmet131_lbl 44140 `"Springfield, MA"', add
label define migmet131_lbl 44180 `"Springfield, MO"', add
label define migmet131_lbl 44220 `"Springfield, OH"', add
label define migmet131_lbl 44300 `"State College, PA"', add
label define migmet131_lbl 44700 `"Stockton-Lodi, CA"', add
label define migmet131_lbl 44940 `"Sumter, SC"', add
label define migmet131_lbl 45060 `"Syracuse, NY"', add
label define migmet131_lbl 45220 `"Tallahassee, FL"', add
label define migmet131_lbl 45300 `"Tampa-St. Petersburg-Clearwater, FL"', add
label define migmet131_lbl 45460 `"Terre Haute, IN"', add
label define migmet131_lbl 45540 `"The Villages, FL"', add
label define migmet131_lbl 45780 `"Toledo, OH"', add
label define migmet131_lbl 45820 `"Topeka, KS"', add
label define migmet131_lbl 45940 `"Trenton, NJ"', add
label define migmet131_lbl 46060 `"Tucson, AZ"', add
label define migmet131_lbl 46140 `"Tulsa, OK"', add
label define migmet131_lbl 46220 `"Tuscaloosa, AL"', add
label define migmet131_lbl 46340 `"Tyler, TX"', add
label define migmet131_lbl 46520 `"Urban Honolulu, HI"', add
label define migmet131_lbl 46540 `"Utica-Rome, NY"', add
label define migmet131_lbl 46660 `"Valdosta, GA"', add
label define migmet131_lbl 46700 `"Vallejo-Fairfield, CA"', add
label define migmet131_lbl 47220 `"Vineland-Bridgeton, NJ"', add
label define migmet131_lbl 47260 `"Virginia Beach-Norfolk-Newport News, VA-NC"', add
label define migmet131_lbl 47300 `"Visalia-Porterville, CA"', add
label define migmet131_lbl 47380 `"Waco, TX"', add
label define migmet131_lbl 47580 `"Warner Robins, GA"', add
label define migmet131_lbl 47900 `"Washington-Arlington-Alexandria, DC-VA-MD-WV"', add
label define migmet131_lbl 48140 `"Wausau, WI"', add
label define migmet131_lbl 48300 `"Wenatchee, WA"', add
label define migmet131_lbl 48620 `"Wichita, KS"', add
label define migmet131_lbl 48660 `"Wichita Falls, TX"', add
label define migmet131_lbl 48700 `"Williamsport, PA"', add
label define migmet131_lbl 48900 `"Wilmington, NC"', add
label define migmet131_lbl 49180 `"Winston-Salem, NC"', add
label define migmet131_lbl 49340 `"Worcester, MA-CT"', add
label define migmet131_lbl 49420 `"Yakima, WA"', add
label define migmet131_lbl 49620 `"York-Hanover, PA"', add
label define migmet131_lbl 49660 `"Youngstown-Warren-Boardman, OH-PA"', add
label define migmet131_lbl 49700 `"Yuba City, CA"', add
label define migmet131_lbl 49740 `"Yuma, AZ"', add
label values migmet131 migmet131_lbl

label define pwstate2_lbl 00 `"N/A"'
label define pwstate2_lbl 01 `"Alabama"', add
label define pwstate2_lbl 02 `"Alaska"', add
label define pwstate2_lbl 04 `"Arizona"', add
label define pwstate2_lbl 05 `"Arkansas"', add
label define pwstate2_lbl 06 `"California"', add
label define pwstate2_lbl 08 `"Colorado"', add
label define pwstate2_lbl 09 `"Connecticut"', add
label define pwstate2_lbl 10 `"Delaware"', add
label define pwstate2_lbl 11 `"District of Columbia"', add
label define pwstate2_lbl 12 `"Florida"', add
label define pwstate2_lbl 13 `"Georgia"', add
label define pwstate2_lbl 15 `"Hawaii"', add
label define pwstate2_lbl 16 `"Idaho"', add
label define pwstate2_lbl 17 `"Illinois"', add
label define pwstate2_lbl 18 `"Indiana"', add
label define pwstate2_lbl 19 `"Iowa"', add
label define pwstate2_lbl 20 `"Kansas"', add
label define pwstate2_lbl 21 `"Kentucky"', add
label define pwstate2_lbl 22 `"Louisiana"', add
label define pwstate2_lbl 23 `"Maine"', add
label define pwstate2_lbl 24 `"Maryland"', add
label define pwstate2_lbl 25 `"Massachusetts"', add
label define pwstate2_lbl 26 `"Michigan"', add
label define pwstate2_lbl 27 `"Minnesota"', add
label define pwstate2_lbl 28 `"Mississippi"', add
label define pwstate2_lbl 29 `"Missouri"', add
label define pwstate2_lbl 30 `"Montana"', add
label define pwstate2_lbl 31 `"Nebraska"', add
label define pwstate2_lbl 32 `"Nevada"', add
label define pwstate2_lbl 33 `"New Hampshire"', add
label define pwstate2_lbl 34 `"New Jersey"', add
label define pwstate2_lbl 35 `"New Mexico"', add
label define pwstate2_lbl 36 `"New York"', add
label define pwstate2_lbl 37 `"North Carolina"', add
label define pwstate2_lbl 38 `"North Dakota"', add
label define pwstate2_lbl 39 `"Ohio"', add
label define pwstate2_lbl 40 `"Oklahoma"', add
label define pwstate2_lbl 41 `"Oregon"', add
label define pwstate2_lbl 42 `"Pennsylvania"', add
label define pwstate2_lbl 44 `"Rhode Island"', add
label define pwstate2_lbl 45 `"South Carolina"', add
label define pwstate2_lbl 46 `"South Dakota"', add
label define pwstate2_lbl 47 `"Tennessee"', add
label define pwstate2_lbl 48 `"Texas"', add
label define pwstate2_lbl 49 `"Utah"', add
label define pwstate2_lbl 50 `"Vermont"', add
label define pwstate2_lbl 51 `"Virginia"', add
label define pwstate2_lbl 53 `"Washington"', add
label define pwstate2_lbl 54 `"West Virginia"', add
label define pwstate2_lbl 55 `"Wisconsin"', add
label define pwstate2_lbl 56 `"Wyoming"', add
label define pwstate2_lbl 61 `"Maine-New Hamp-Vermont"', add
label define pwstate2_lbl 62 `"Massachusetts-Rhode Island"', add
label define pwstate2_lbl 63 `"Minn-Iowa-Missouri-Kansas-S Dakota-N Dakota"', add
label define pwstate2_lbl 64 `"Mayrland-Delaware"', add
label define pwstate2_lbl 65 `"Montana-Idaho-Wyoming"', add
label define pwstate2_lbl 66 `"Utah-Nevada"', add
label define pwstate2_lbl 67 `"Arizona-New Mexico"', add
label define pwstate2_lbl 68 `"Alaska-Hawaii"', add
label define pwstate2_lbl 72 `"Puerto Rico"', add
label define pwstate2_lbl 73 `"U.S. outlying area"', add
label define pwstate2_lbl 74 `"United States (1980 Puerto Rico samples)"', add
label define pwstate2_lbl 80 `"Abroad"', add
label define pwstate2_lbl 81 `"Europe"', add
label define pwstate2_lbl 82 `"Eastern Asia"', add
label define pwstate2_lbl 83 `"South Central, South East, and Western Asia"', add
label define pwstate2_lbl 84 `"Mexico"', add
label define pwstate2_lbl 85 `"Other Americas"', add
label define pwstate2_lbl 86 `"Other, nec"', add
label define pwstate2_lbl 87 `"Iraq"', add
label define pwstate2_lbl 88 `"Canada"', add
label define pwstate2_lbl 90 `"Confidential"', add
label define pwstate2_lbl 99 `"Not reported"', add
label values pwstate2 pwstate2_lbl

label define qage_lbl 0 `"Entered as written"'
label define qage_lbl 1 `"Failed edit"', add
label define qage_lbl 2 `"Illegible"', add
label define qage_lbl 3 `"Missing"', add
label define qage_lbl 4 `"Allocated"', add
label define qage_lbl 5 `"Illegible"', add
label define qage_lbl 6 `"Missing"', add
label define qage_lbl 7 `"Original entry illegible"', add
label define qage_lbl 8 `"Original entry missing or failed edit"', add
label values qage qage_lbl

label define qmarst_lbl 0 `"Entered as written"'
label define qmarst_lbl 1 `"Failed edit"', add
label define qmarst_lbl 2 `"Illegible"', add
label define qmarst_lbl 3 `"Missing"', add
label define qmarst_lbl 4 `"Allocated"', add
label define qmarst_lbl 5 `"Illegible"', add
label define qmarst_lbl 6 `"Missing"', add
label define qmarst_lbl 7 `"Original entry illegible"', add
label define qmarst_lbl 8 `"Original entry missing or failed edit"', add
label values qmarst qmarst_lbl

label define qsex_lbl 0 `"Entered as written"'
label define qsex_lbl 1 `"Failed edit"', add
label define qsex_lbl 2 `"Illegible"', add
label define qsex_lbl 3 `"Missing"', add
label define qsex_lbl 4 `"Allocated"', add
label define qsex_lbl 5 `"Illegible"', add
label define qsex_lbl 6 `"Missing"', add
label define qsex_lbl 7 `"Original entry illegible"', add
label define qsex_lbl 8 `"Original entry missing or failed edit"', add
label values qsex qsex_lbl

label define qempstat_lbl 0 `"Original entry or Inapplicable (not in universe)"'
label define qempstat_lbl 1 `"Failed edit"', add
label define qempstat_lbl 2 `"Illegible"', add
label define qempstat_lbl 3 `"Missing"', add
label define qempstat_lbl 4 `"Allocated"', add
label define qempstat_lbl 5 `"Illegible"', add
label define qempstat_lbl 6 `"Missing"', add
label define qempstat_lbl 7 `"Original entry illegible"', add
label define qempstat_lbl 8 `"Original entry missing or failed edit"', add
label values qempstat qempstat_lbl

label define qincbus_lbl 0 `"Not allocated"'
label define qincbus_lbl 1 `"Allocated, pre-edit"', add
label define qincbus_lbl 3 `"Allocated, consistency edit"', add
label define qincbus_lbl 4 `"Allocated"', add
label values qincbus qincbus_lbl

label define qincinvs_lbl 0 `"Not allocated or N/A"'
label define qincinvs_lbl 1 `"Allocated, pre-edit"', add
label define qincinvs_lbl 3 `"Allocated, consistency edit"', add
label define qincinvs_lbl 4 `"Allocated"', add
label values qincinvs qincinvs_lbl

label define qincothe_lbl 0 `"Original entry or Inapplicable (not in universe)"'
label define qincothe_lbl 1 `"Failed edit"', add
label define qincothe_lbl 2 `"Illegible"', add
label define qincothe_lbl 3 `"Missing"', add
label define qincothe_lbl 4 `"Allocated"', add
label define qincothe_lbl 5 `"Illegible"', add
label define qincothe_lbl 6 `"Missing"', add
label define qincothe_lbl 7 `"Original entry illegible"', add
label define qincothe_lbl 8 `"Original entry missing or failed edit"', add
label values qincothe qincothe_lbl

label define qincreti_lbl 0 `"Not allocated"'
label define qincreti_lbl 3 `"Not allocated, derived"', add
label define qincreti_lbl 4 `"Allocated"', add
label values qincreti qincreti_lbl

label define qincss_lbl 0 `"Not allocated"'
label define qincss_lbl 1 `"Allocated, pre-edit"', add
label define qincss_lbl 3 `"Allocated, consistency edit"', add
label define qincss_lbl 4 `"Allocated"', add
label values qincss qincss_lbl

label define qinctot_lbl 0 `"Original entry or Inapplicable (not in universe)"'
label define qinctot_lbl 1 `"Failed edit"', add
label define qinctot_lbl 2 `"Illegible"', add
label define qinctot_lbl 3 `"Missing"', add
label define qinctot_lbl 4 `"Allocated"', add
label define qinctot_lbl 5 `"Illegible"', add
label define qinctot_lbl 6 `"Missing"', add
label define qinctot_lbl 7 `"Original entry illegible"', add
label define qinctot_lbl 8 `"Original entry missing or failed edit"', add
label values qinctot qinctot_lbl

label define qincwage_lbl 0 `"Original entry or Inapplicable (not in universe)"'
label define qincwage_lbl 1 `"Failed edit"', add
label define qincwage_lbl 2 `"Illegible"', add
label define qincwage_lbl 3 `"Missing"', add
label define qincwage_lbl 4 `"Allocated"', add
label define qincwage_lbl 5 `"Illegible"', add
label define qincwage_lbl 6 `"Missing"', add
label define qincwage_lbl 7 `"Original entry illegible"', add
label define qincwage_lbl 8 `"Original entry missing or failed edit"', add
label values qincwage qincwage_lbl

label define qincwelf_lbl 0 `"Not allocated"'
label define qincwelf_lbl 1 `"Allocated, pre-edit"', add
label define qincwelf_lbl 3 `"Allocated, consistency edit"', add
label define qincwelf_lbl 4 `"Allocated"', add
label values qincwelf qincwelf_lbl

label define qmigplc1_lbl 0 `"Not allocated"'
label define qmigplc1_lbl 1 `"Failed edit"', add
label define qmigplc1_lbl 2 `"Failed edit/illegible"', add
label define qmigplc1_lbl 3 `"Failed edit/missing"', add
label define qmigplc1_lbl 4 `"Failed edit"', add
label define qmigplc1_lbl 5 `"Illegible"', add
label define qmigplc1_lbl 6 `"Failed edit/missing"', add
label define qmigplc1_lbl 7 `"Illegible"', add
label define qmigplc1_lbl 8 `"Illegible/missing or failed edit"', add
label values qmigplc1 qmigplc1_lbl

label define qmigrat1_lbl 0 `"Not allocated"'
label define qmigrat1_lbl 1 `"Failed edit"', add
label define qmigrat1_lbl 2 `"Illegible"', add
label define qmigrat1_lbl 3 `"Missing"', add
label define qmigrat1_lbl 4 `"Allocated"', add
label define qmigrat1_lbl 5 `"Illegible"', add
label define qmigrat1_lbl 6 `"Missing"', add
label define qmigrat1_lbl 7 `"Original entry illegible"', add
label define qmigrat1_lbl 8 `"Original entry missing or failed edit"', add
label values qmigrat1 qmigrat1_lbl

label define qpwstat2_lbl 0 `"Not allocated"'
label define qpwstat2_lbl 4 `"Allocated"', add
label values qpwstat2 qpwstat2_lbl


