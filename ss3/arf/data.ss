#V3.30.20.00;_safe;_compile_date:_Sep 30 2022;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.0
#_Stock_Synthesis_is_a_work_of_the_U.S._Government_and_is_not_subject_to_copyright_protection_in_the_United_States.
#_Foreign_copyrights_may_apply._See_copyright.txt_for_more_information.
#_User_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_User_info_available_at:https://vlab.noaa.gov/group/stock-synthesis
#_Source_code_at:_https://github.com/nmfs-stock-synthesis/stock-synthesis

#_Start_time: Fri Sep 30 15:20:34 2022
#_echo_input_data
#C data file for simple example
#V3.30.20.00;_safe;_compile_date:_Sep 30 2022;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_13.0
1996 #_StartYr
2021 #_EndYr
1 #_Nseas
 12 #_months/season
2 #_Nsubseasons (even number, minimum is 2)
1 #_spawn_month
2 #_Ngenders: 1, 2, -1  (use -1 for 1 sex setup with SSB multiplied by female_frac parameter)
20 #_Nages=accumulator age, first age is always age 0
1 #_Nareas
7 #_Nfleets (including surveys)
#_fleet_type: 1=catch fleet; 2=bycatch only fleet; 3=survey; 4=predator(M2) 
#_sample_timing: -1 for fishing fleet to use season-long catch-at-age for observations, or 1 to use observation month;  (always 1 for surveys)
#_fleet_area:  area the fleet/survey operates in 
#_units of catch:  1=bio; 2=num (ignored for surveys; their units read later)
#_catch_mult: 0=no; 1=yes
#_rows are fleets
#_fleet_type fishery_timing area catch_units need_catch_mult fleetname
 1 -1 1 1 0 FREEZER  # 1
 1 -1 1 1 0 SHORESIDE  # 2
 3 1 1 1 0 SYNQCS  # 3
 3 1 1 1 0 HSMULT  # 4
 3 1 1 1 0 SYNHS  # 5
 3 1 1 1 0 SYNWCVI  # 6
 3 1 1 1 0 DCPUE  # 7
#Bycatch_fleet_input_goes_next
#a:  fleet index
#b:  1=include dead bycatch in total dead catch for F0.1 and MSY optimizations and forecast ABC; 2=omit from total catch for these purposes (but still include the mortality)
#c:  1=Fmult scales with other fleets; 2=bycatch F constant at input value; 3=bycatch F from range of years
#d:  F or first year of range
#e:  last year of range
#f:  not used
# a   b   c   d   e   f 
#_Catch data: yr, seas, fleet, catch, catch_se
#_catch_se:  standard error of log(catch)
#_NOTE:  catch data is ignored for survey fleets
-999 1 1 0 0.01
1996 1 1 0.0007 0.01
1997 1 1 0 0.01
1998 1 1 0 0.01
1999 1 1 0 0.01
2000 1 1 0.1131 0.01
2001 1 1 0.0314 0.01
2002 1 1 0.0504 0.01
2003 1 1 0.0161 0.01
2004 1 1 0.0004 0.01
2005 1 1 1.5986 0.01
2006 1 1 3.416 0.01
2007 1 1 1.1653 0.01
2008 1 1 2.1458 0.01
2009 1 1 0.0021 0.01
2010 1 1 0.175 0.01
2011 1 1 3.1771 0.01
2012 1 1 3.4118 0.01
2013 1 1 7.7678 0.01
2014 1 1 11.5878 0.01
2015 1 1 9.4926 0.01
2016 1 1 9.6724 0.01
2017 1 1 8.5798 0.01
2018 1 1 7.7848 0.01
2019 1 1 6.1485 0.01
2020 1 1 0.9737 0.01
2021 1 1 2.5491 0.01
1996 1 2 8.1704 0.01
1997 1 2 5.2383 0.01
1998 1 2 7.4182 0.01
1999 1 2 7.9478 0.01
2000 1 2 7.3779 0.01
2001 1 2 10.5984 0.01
2002 1 2 7.9387 0.01
2003 1 2 7.0972 0.01
2004 1 2 9.4431 0.01
2005 1 2 17.2151 0.01
2006 1 2 4.7853 0.01
2007 1 2 3.4018 0.01
2008 1 2 3.2929 0.01
2009 1 2 3.8761 0.01
2010 1 2 3.186 0.01
2011 1 2 5.1028 0.01
2012 1 2 3.8279 0.01
2013 1 2 3.4031 0.01
2014 1 2 2.7123 0.01
2015 1 2 2.3203 0.01
2016 1 2 2.8255 0.01
2017 1 2 2.8238 0.01
2018 1 2 1.4789 0.01
2019 1 2 1.4942 0.01
2020 1 2 0.9662 0.01
2021 1 2 1.4267 0.01
-9999 0 0 0 0
#
 #_CPUE_and_surveyabundance_observations
#_Units:  0=numbers; 1=biomass; 2=F; 30=spawnbio; 31=recdev; 32=spawnbio*recdev; 33=recruitment; 34=depletion(&see Qsetup); 35=parm_dev(&see Qsetup)
#_Errtype:  -1=normal; 0=lognormal; >0=T
#_SD_Report: 0=no sdreport; 1=enable sdreport
#_Fleet Units Errtype SD_Report
1 1 0 0 # FREEZER
2 1 0 0 # SHORESIDE
3 1 0 0 # SYNQCS
4 1 0 0 # HSMULT
5 1 0 0 # SYNHS
6 1 0 0 # SYNWCVI
7 1 0 0 # DCPUE
#_yr month fleet obs stderr
2003 7 3 5.75 0.11 #_ SYN QCS
2004 7 3 11.86 0.19 #_ SYN QCS
2005 7 3 13.63 0.17 #_ SYN QCS
2007 7 3 7.41 0.14 #_ SYN QCS
2009 7 3 9.32 0.13 #_ SYN QCS
2011 7 3 13.37 0.19 #_ SYN QCS
2013 7 3 11.3 0.17 #_ SYN QCS
2015 7 3 13.79 0.15 #_ SYN QCS
2017 7 3 12.22 0.19 #_ SYN QCS
2019 7 3 12.17 0.15 #_ SYN QCS
2021 7 3 10.43 0.14 #_ SYN QCS
1984 7 4 5.66 0.2 #_ OTHER HS MSA
1987 7 4 7.11 0.22 #_ OTHER HS MSA
1989 7 4 12.85 0.19 #_ OTHER HS MSA
1991 7 4 11.06 0.16 #_ OTHER HS MSA
1993 7 4 5.12 0.28 #_ OTHER HS MSA
1995 7 4 6.42 0.29 #_ OTHER HS MSA
1996 7 4 6.48 0.26 #_ OTHER HS MSA
1998 7 4 7.73 0.27 #_ OTHER HS MSA
2000 7 4 12.58 0.23 #_ OTHER HS MSA
2002 7 4 10.38 0.17 #_ OTHER HS MSA
2003 7 4 11.09 0.22 #_ OTHER HS MSA
2005 7 5 14.53 0.22 #_ SYN HS
2007 7 5 6.57 0.19 #_ SYN HS
2009 7 5 12.61 0.17 #_ SYN HS
2011 7 5 15.24 0.14 #_ SYN HS
2013 7 5 14.03 0.17 #_ SYN HS
2015 7 5 8.23 0.17 #_ SYN HS
2017 7 5 10.67 0.26 #_ SYN HS
2019 7 5 4.23 0.1 #_ SYN HS
2021 7 5 3.89 0.12 #_ SYN HS
2004 7 6 8.53 0.25 #_ SYN WCVI
2006 7 6 7.98 0.18 #_ SYN WCVI
2008 7 6 6.44 0.28 #_ SYN WCVI
2010 7 6 14.71 0.17 #_ SYN WCVI
2012 7 6 5.48 0.14 #_ SYN WCVI
2014 7 6 13.82 0.11 #_ SYN WCVI
2016 7 6 10.2 0.23 #_ SYN WCVI
2018 7 6 2.75 0.1 #_ SYN WCVI
2021 7 6 3.39 0.11 #_ SYN WCVI
1996 7 7 10.18 0.21 #_ DCPUE
1997 7 7 9.85 0.21 #_ DCPUE
1998 7 7 10.73 0.21 #_ DCPUE
1999 7 7 10.58 0.21 #_ DCPUE
2000 7 7 9.28 0.21 #_ DCPUE
2001 7 7 8.84 0.21 #_ DCPUE
2002 7 7 10.18 0.21 #_ DCPUE
2003 7 7 10.53 0.21 #_ DCPUE
2004 7 7 10.74 0.21 #_ DCPUE
2005 7 7 11.38 0.21 #_ DCPUE
2006 7 7 6.08 0.21 #_ DCPUE
2007 7 7 7.57 0.21 #_ DCPUE
2008 7 7 7.36 0.21 #_ DCPUE
2009 7 7 11.53 0.21 #_ DCPUE
2010 7 7 11.84 0.21 #_ DCPUE
2011 7 7 9.29 0.21 #_ DCPUE
2012 7 7 8.42 0.21 #_ DCPUE
2013 7 7 10.7 0.21 #_ DCPUE
2014 7 7 8.95 0.21 #_ DCPUE
2015 7 7 7.58 0.21 #_ DCPUE
2016 7 7 6.89 0.21 #_ DCPUE
2017 7 7 6.42 0.22 #_ DCPUE
2018 7 7 5.46 0.22 #_ DCPUE
2019 7 7 4.9 0.22 #_ DCPUE
2020 7 7 4.12 0.22 #_ DCPUE
2021 7 7 4.07 0.23 #_ DCPUE
-9999 1 1 1 1 # terminator for survey observations 
#
0 #_N_fleets_with_discard
#_discard_units (1=same_as_catchunits(bio/num); 2=fraction; 3=numbers)
#_discard_errtype:  >0 for DF of T-dist(read CV below); 0 for normal with CV; -1 for normal with se; -2 for lognormal; -3 for trunc normal with CV
# note: only enter units and errtype for fleets with discard 
# note: discard data is the total for an entire season, so input of month here must be to a month in that season
#_Fleet units errtype
# -9999 0 0 0.0 0.0 # terminator for discard data 
#
0 #_use meanbodysize_data (0/1)
#_COND_0 #_DF_for_meanbodysize_T-distribution_like
# note:  type=1 for mean length; type=2 for mean body weight 
#_yr month fleet part type obs stderr
#  -9999 0 0 0 0 0 0 # terminator for mean body size data 
#
# set up population length bin structure (note - irrelevant if not using size data and using empirical wtatage
2 # length bin method: 1=use databins; 2=generate from binwidth,min,max below; 3=read vector
2 # binwidth for population size comp 
10 # minimum size in the population (lower edge of first bin and size at age 0.00) 
80 # maximum size in the population (lower edge of last bin) 
0 # use length composition data (0/1)
#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.
#_addtocomp:  after accumulation of tails; this value added to all bins
#_combM+F: males and females treated as combined gender below this bin number 
#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation
#_Comp_Error:  0=multinomial, 1=dirichlet using Theta*n, 2=dirichlet using beta, 3=MV_Tweedie
#_ParmSelect:  consecutive index for dirichlet or MV_Tweedie
#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001
#
#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
#_N_LengthBins; then enter lower edge of each length bin
#_yr month fleet sex part Nsamp datavector(female-male)
#
25 #_N_age_bins
 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
0 #_N_ageerror_definitions
#_mintailcomp: upper and lower distribution for females and males separately are accumulated until exceeding this level.
#_addtocomp:  after accumulation of tails; this value added to all bins
#_combM+F: males and females treated as combined gender below this bin number 
#_compressbins: accumulate upper tail by this number of bins; acts simultaneous with mintailcomp; set=0 for no forced accumulation
#_Comp_Error:  0=multinomial, 1=dirichlet using Theta*n, 2=dirichlet using beta, 3=MV_Tweedie
1
#_ParmSelect:  consecutive index for dirichlet or MV_Tweedie
0
#_minsamplesize: minimum sample size; set to 1 to match 3.24, minimum value is 0.001
#
#_mintailcomp addtocomp combM+F CompressBins CompError ParmSelect minsamplesize
0 1e-07 1 0 0 0 1 #_fleet:1_FISHERY
0 1e-07 1 0 0 0 1 #_fleet:2_Fish2
0 1e-07 1 0 0 0 1 #_fleet:3_SURVEY2
0 1e-07 1 0 0 0 1 #_fleet:4_SURVEY2
0 1e-07 1 0 0 0 1 #_fleet:5_SURVEY2
0 1e-07 1 0 0 0 1 #_fleet:6_SURVEY2
0 1e-07 1 0 0 0 1 #_fleet:7_SURVEY2
1 #_Lbin_method_for_Age_Data: 1=poplenbins; 2=datalenbins; 3=lengths
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
#_yr month fleet sex part ageerr Lbin_lo Lbin_hi Nsamp datavector(female-male)
2013 7 1 3 0 0 1 -1 210 0 0 0 0 1 6 12 9 20 17 18 18 6 7 11 6 5 1 0 0 0 0 1 0 1 0 0 0 1 5 11 13 10 7 5 8 4 2 5 0 0 0 0 0 0 0 0 0 0 0
2014 7 1 3 0 0 1 -1 307 0 0 0 2 5 11 18 30 16 18 19 16 14 17 19 4 3 3 1 2 0 1 0 0 0 0 0 1 2 3 6 21 17 12 7 8 4 6 10 4 4 1 0 0 0 1 0 1 0 0
2015 7 1 3 0 0 1 -1 269 0 0 1 4 3 8 12 22 19 35 14 18 11 6 11 4 5 3 0 0 0 0 0 1 1 0 0 0 12 4 6 10 21 7 8 6 4 4 2 2 3 2 0 0 0 0 0 0 0 0
2016 7 1 3 0 0 1 -1 333 0 0 0 4 13 21 18 26 32 20 21 13 12 8 9 6 2 1 0 0 0 0 0 0 0 0 0 1 5 12 16 20 16 17 12 8 4 2 8 2 1 3 0 0 0 0 0 0 0 0
2017 7 1 3 0 0 1 -1 159 0 0 0 0 1 8 12 10 10 12 15 8 3 4 2 3 5 1 1 0 0 0 0 0 0 0 0 0 3 2 8 4 8 8 6 4 7 5 2 3 1 0 1 0 0 1 1 0 0 0
2018 7 1 3 0 0 1 -1 220 0 0 0 1 3 8 11 8 23 16 21 10 14 6 10 11 4 3 2 1 0 1 1 0 0 0 0 0 1 3 10 6 7 12 9 5 3 3 2 0 3 1 0 0 1 0 0 0 0 0
2019 7 1 3 0 0 1 -1 135 0 0 0 0 1 6 3 10 5 7 13 11 14 3 7 3 2 5 2 1 0 0 0 0 0 0 0 0 0 0 4 7 5 3 4 2 3 6 2 2 1 0 2 0 1 0 0 0 0 0
1996 7 1 3 0 0 1 -1 48 0 0 0 0 0 0 0 2 9 10 4 2 4 1 1 0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 1 1 3 5 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0
1997 7 1 3 0 0 1 -1 126 0 0 0 0 0 0 4 9 11 13 11 14 7 9 4 3 5 1 1 1 1 0 0 1 0 0 0 1 0 0 4 5 7 3 7 1 1 2 0 0 0 0 0 0 0 0 0 0 0 0
1998 7 1 3 0 0 1 -1 236 0 0 1 1 4 5 11 15 20 26 42 21 9 13 2 1 1 3 2 0 1 1 1 0 0 0 0 0 3 3 4 6 5 8 4 8 7 3 2 2 0 1 0 0 0 0 0 0 0 0
1999 7 1 3 0 0 1 -1 194 0 0 0 0 1 2 11 21 28 24 24 12 9 8 4 2 1 1 1 0 1 0 0 0 0 0 0 0 0 0 1 7 4 8 5 5 7 3 0 2 2 0 0 0 0 0 0 0 0 0
2000 7 1 3 0 0 1 -1 238 0 0 0 2 4 5 6 17 23 21 30 20 13 17 7 7 3 1 3 2 1 0 0 0 0 0 1 1 2 5 9 6 10 4 4 3 4 4 1 1 1 0 0 0 0 0 0 0 0 0
2001 7 1 3 0 0 1 -1 199 0 0 0 0 0 1 9 12 18 18 25 17 10 11 8 7 6 2 0 1 1 0 0 0 0 0 0 0 0 2 4 5 3 5 5 5 6 3 5 6 2 2 0 0 0 0 0 0 0 0
2002 7 1 3 0 0 1 -1 176 0 1 6 3 2 5 8 11 13 23 21 13 3 8 5 4 4 3 0 1 0 0 0 0 0 0 0 13 4 3 3 2 7 3 0 3 3 0 1 0 0 0 0 0 0 0 0 0 0 0
2003 7 1 3 0 0 1 -1 163 0 0 0 0 1 3 8 4 13 15 5 12 11 7 6 5 0 2 2 0 0 1 0 0 0 0 0 1 2 3 5 10 6 5 11 5 4 3 2 4 4 2 0 0 0 0 0 1 0 0
2004 7 1 3 0 0 1 -1 278 0 0 0 0 1 1 7 21 21 23 13 9 9 10 7 4 1 2 2 1 1 0 0 0 0 0 0 0 0 3 4 21 40 23 20 11 6 4 5 2 2 1 1 1 0 0 0 1 0 0
2005 7 1 3 0 0 1 -1 298 0 0 4 5 15 15 26 21 25 22 15 13 4 4 3 3 0 1 2 1 1 0 0 0 0 0 0 5 7 5 11 16 11 18 13 13 2 4 5 4 3 0 0 1 0 0 0 0 0 0
2006 7 1 3 0 0 1 -1 246 0 0 0 2 1 8 9 19 29 20 11 19 18 4 13 9 9 6 2 3 1 0 3 0 1 0 0 0 0 6 9 13 11 7 2 3 2 1 2 1 1 1 0 0 0 0 0 0 0 0
2007 7 1 3 0 0 1 -1 283 0 0 0 2 4 10 10 33 32 35 31 21 14 15 3 6 3 0 0 0 0 1 0 0 0 0 0 0 1 4 5 9 8 13 8 3 4 2 4 0 1 1 0 0 0 0 0 0 0 0
2008 7 1 3 0 0 1 -1 59 0 0 0 0 0 0 3 4 10 6 2 6 4 3 7 0 2 1 3 0 1 0 1 0 0 0 0 0 0 0 0 0 1 3 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0
2009 7 1 3 0 0 1 -1 103 0 0 0 0 0 1 5 13 14 15 11 3 6 8 3 0 1 0 1 0 0 1 1 0 0 0 0 0 0 1 3 3 4 1 2 2 1 0 1 1 0 0 0 0 0 0 0 1 0 0
2010 7 1 3 0 0 1 -1 178 0 0 0 2 6 1 3 7 9 13 14 7 3 5 2 1 0 0 0 1 1 0 0 1 1 0 0 7 13 7 7 10 13 17 15 7 4 0 0 0 1 0 0 0 0 0 0 0 0 0
2011 7 1 3 0 0 1 -1 239 0 0 0 0 0 1 8 18 25 34 39 26 19 9 7 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 1 4 9 8 7 8 8 5 2 0 0 0 0 0 0 0 0 0 0 0
2012 7 1 3 0 0 1 -1 263 0 0 2 1 5 12 4 14 18 32 28 24 15 6 6 0 2 0 1 0 1 0 0 0 0 0 0 0 1 4 7 11 12 10 14 16 10 3 2 1 1 0 0 0 0 0 0 0 0 0
2013 7 1 3 0 0 1 -1 84 0 0 0 0 1 3 2 5 11 18 10 11 2 4 5 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 2 2 3 0 1 0 0 0 0 0 0 0 0 0 0 0
2015 7 1 3 0 0 1 -1 25 0 0 0 1 1 0 1 4 2 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 2 2 2 3 1 1 0 1 0 0 0 1 0 0 0 0 0 0 0
2016 7 1 3 0 0 1 -1 22 0 0 0 0 0 0 0 0 5 1 4 1 1 0 0 2 1 0 1 0 0 0 0 0 0 0 0 1 0 2 1 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
2019 7 1 3 0 0 1 -1 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
2003 7 1 3 0 0 1 -1 110 3 9 12 10 11 4 6 9 2 3 1 1 0 1 1 0 0 0 0 0 0 0 1 0 0 1 9 5 4 9 1 2 2 2 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0
2005 7 1 3 0 0 1 -1 89 0 4 5 9 3 0 5 5 3 4 5 3 0 0 1 0 1 3 1 0 0 1 0 0 0 0 0 1 8 8 7 5 3 2 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0
2011 7 1 3 0 0 1 -1 90 4 6 7 3 2 4 4 3 9 4 4 2 3 1 0 0 0 0 0 0 0 0 0 0 0 4 9 3 1 2 1 6 3 0 2 1 2 0 0 0 0 0 0 0 0 0 0 0 0 0
2013 7 1 3 0 0 1 -1 100 1 10 2 3 1 2 3 4 12 1 14 4 2 2 0 0 0 0 0 0 0 0 0 0 0 1 8 3 4 3 6 3 0 5 2 1 0 1 0 2 0 0 0 0 0 0 0 0 0 0
2015 7 1 3 0 0 1 -1 121 3 7 3 9 8 5 5 7 10 6 2 6 5 2 0 0 0 0 0 0 0 0 0 0 0 0 6 2 3 3 7 4 5 2 3 2 1 2 1 2 0 0 0 0 0 0 0 0 0 0
2017 7 1 3 0 0 1 -1 111 2 4 5 15 6 7 1 9 4 2 2 3 4 0 0 0 0 2 0 0 0 0 0 0 0 3 0 2 7 9 8 2 2 5 4 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0
2019 7 1 3 0 0 1 -1 127 2 3 7 4 6 11 13 12 10 5 7 2 2 1 3 1 1 0 1 0 0 0 0 0 0 0 2 2 4 6 4 4 4 6 0 0 1 0 2 0 1 0 0 0 0 0 0 0 0 0
2005 7 1 3 0 0 1 -1 576 6 55 48 28 18 15 23 35 21 23 14 22 11 12 15 17 7 11 5 5 3 0 1 0 0 6 21 26 23 17 27 14 16 7 7 7 2 5 0 0 1 1 0 1 0 0 0 0 0 0
2007 7 1 3 0 0 1 -1 463 2 6 10 9 14 21 27 40 33 47 23 14 10 12 7 6 5 2 3 8 3 1 0 0 0 0 5 6 9 14 23 33 20 17 9 10 4 3 1 2 2 0 0 1 0 0 0 1 0 0
2009 7 1 3 0 0 1 -1 650 1 20 16 16 10 14 24 44 58 72 39 34 10 12 7 6 0 2 0 1 0 1 0 0 0 0 21 21 10 7 7 28 36 38 40 30 7 10 6 1 1 0 0 0 0 0 0 0 0 0
2011 7 1 3 0 0 1 -1 253 0 8 6 7 7 12 13 23 28 16 13 13 10 7 1 0 0 2 0 0 0 0 0 0 0 2 12 13 4 8 8 9 7 10 2 6 3 2 1 0 0 0 0 0 0 0 0 0 0 0
2013 7 1 3 0 0 1 -1 588 1 41 8 12 11 16 30 47 34 29 30 30 32 23 11 1 2 0 0 0 0 0 0 0 0 2 22 9 13 14 21 26 22 35 18 9 9 15 7 4 1 2 0 0 1 0 0 0 0 0
2015 7 1 3 0 0 1 -1 103 0 5 0 4 10 11 3 4 5 4 0 1 3 3 1 3 1 2 0 1 0 0 0 0 0 0 0 2 5 5 4 5 6 6 1 0 1 2 1 2 1 0 0 1 0 0 0 0 0 0
2017 7 1 3 0 0 1 -1 116 2 6 3 7 1 1 3 10 10 4 8 7 2 3 5 0 1 0 0 0 0 0 0 0 0 0 1 1 8 6 3 8 2 3 1 2 4 0 2 0 0 0 1 1 0 0 0 0 0 0
2019 7 1 3 0 0 1 -1 124 3 4 5 5 6 5 12 9 1 4 7 7 3 2 1 2 1 0 1 0 0 0 0 0 0 1 6 3 0 6 4 3 5 4 1 3 4 1 2 1 0 1 0 0 0 1 0 0 0 0
2004 7 1 3 0 0 1 -1 138 0 0 0 4 11 7 6 13 10 10 7 4 4 4 2 3 1 2 0 1 0 0 0 0 0 0 0 1 3 15 4 8 8 2 2 1 0 0 2 0 0 1 1 0 1 0 0 0 0 0
2006 7 1 3 0 0 1 -1 128 0 0 1 2 2 4 10 9 8 18 10 7 6 11 5 4 0 3 4 0 1 0 0 0 0 0 0 0 2 3 2 4 1 1 2 1 4 2 0 0 0 0 0 1 0 0 0 0 0 0
2008 7 1 3 0 0 1 -1 224 0 1 2 0 1 8 7 23 36 20 18 8 8 7 2 7 1 1 3 0 0 2 2 0 0 0 0 0 4 5 5 15 11 17 5 1 3 0 1 0 0 0 0 0 0 0 0 0 0 0
2010 7 1 3 0 0 1 -1 586 4 20 3 13 12 9 20 53 67 68 59 21 13 12 5 9 4 4 1 1 0 0 1 1 0 4 9 5 7 6 17 31 35 31 22 11 3 4 0 1 0 0 0 0 0 0 0 0 0 0
2012 7 1 3 0 0 1 -1 249 2 4 1 4 3 11 9 18 17 24 12 16 9 5 2 2 0 1 1 1 0 0 0 0 0 0 0 1 0 15 12 15 16 18 11 9 6 0 4 0 0 0 0 0 0 0 0 0 0 0
2014 7 1 3 0 0 1 -1 100 5 1 5 1 3 5 7 6 12 9 5 1 1 0 1 0 0 0 0 0 0 0 0 0 0 1 2 4 3 4 5 10 4 2 1 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0
2016 7 1 3 0 0 1 -1 88 0 0 1 0 3 1 3 5 4 8 5 9 6 8 4 2 0 0 0 0 0 0 0 0 0 0 0 0 0 6 1 4 2 2 2 4 3 1 0 0 3 0 1 0 0 0 0 0 0 0
2018 7 1 3 0 0 1 -1 101 0 9 2 5 6 2 4 2 3 1 7 3 1 2 1 5 0 0 0 1 0 0 0 0 0 0 3 1 0 3 3 9 5 3 2 5 6 2 2 0 1 0 0 2 0 0 0 0 0 0
-9999  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#
0 #_Use_MeanSize-at-Age_obs (0/1)
# sex codes:  0=combined; 1=use female only; 2=use male only; 3=use both as joint sexxlength distribution
# partition codes:  (0=combined; 1=discard; 2=retained
# ageerr codes:  positive means mean length-at-age; negative means mean bodywt_at_age
#_yr month fleet sex part ageerr ignore datavector(female-male)
#                                          samplesize(female-male)
#
0 #_N_environ_variables
# -2 in yr will subtract mean for that env_var; -1 will subtract mean and divide by stddev (e.g. Z-score)
#Yr Variable Value
#
# Sizefreq data. Defined by method because a fleet can use multiple methods
0 # N sizefreq methods to read (or -1 for expanded options)
# 
0 # do tags (0/1/2); where 2 allows entry of TG_min_recap
#
0 #    morphcomp data(0/1) 
#  Nobs, Nmorphs, mincomp
#  yr, seas, type, partition, Nsamp, datavector_by_Nmorphs
#
0  #  Do dataread for selectivity priors(0/1)
# Yr, Seas, Fleet,  Age/Size,  Bin,  selex_prior,  prior_sd
# feature not yet implemented
#
999

