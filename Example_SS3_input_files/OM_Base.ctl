#V3.30.04.02-trans
#CONTROL FILE
#_data_and_control_files: sandbar.dat // sandbar.ctl
#_SS-V3.30.04.02-trans;_2017_05_31;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_11.6
#_SS-V3.30.04.02-trans;user_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_SS-V3.30.04.02-trans;user_info_available_at:https://vlab.ncep.noaa.gov/group/stock-synthesis
0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
1  #_N_Growth_Patterns
1 #_N_platoons_Within_GrowthPattern 
#_Cond 1 #_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond  1 #vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
4 # recr_dist_method for parameters:  2=main effects for GP, Settle timing, Area; 3=each Settle entity; 4=none when N_GP*Nsettle*pop==1
1 # not yet implemented; Future usage: Spawner-Recruitment: 1=global; 2=by area
1 #  number of recruitment settlement assignments 
0 # unused option
#GPattern month  area  age (for each settlement assignment)
 1 1 1 0
#
#_Cond 0 # N_movement_definitions goes here if Nareas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
0 #1 #_Nblock_Patterns
# 1 #_blocks_per_pattern 
# begin and end years of blocks
# 1959 1959
#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
#  autogen
1 1 1 1 1 # autogen
# where: 0 = autogen all time-varying parms; 1 = read each time-varying parm line; 2 = read then autogen if min=-12345
# 	1st element for biology, 2nd for SR, 3rd for Q, 5th for selex, 4th reserved
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement 
#
3 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
 #_Age_natmort_by sex x growthpattern
 0.160419 0.160419 0.160419 0.160419 0.160419 0.160419 0.157755 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805
 0.160419 0.160419 0.160419 0.160419 0.160419 0.160419 0.157755 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805 0.116805
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K; 4=not implemented
0 #_Growth_Age_for_L1
999 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (fixed at 0.2 in 3.24; value should approx initial Z; -999 replicates 3.24)
0  #_placeholder for future growth feature
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
4 #3 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
#_Age_Maturity by growth pattern
# 0.000182241 0.000352538 0.000681863 0.00131842 0.00254773 0.00491762 0.00947104 0.0181637 0.0345562 0.064767 0.118157 0.20587 0.334033 0.492501 0.652489 0.784147 0.875447 0.931502 0.963385 0.980735 0.989949 0.99478 0.997295 0.9986 0.999276 0.999626 0.999806 0.9999 0.999948 0.999973 0.999986 0.999993
 0 0 0 0 0 0 0.03304641 0.06788599 0.10420506 0.21260806 0.43263289 0.76857325 1.22367426 1.83784806 2.46242623 2.98084523 3.38885132 3.60552922 3.74383974 3.84167846 3.8985862 3.91422815 3.96772316 3.98005281 3.99094457 4.00056614 4.00906563 4.01657391 4.02320658 4.02906574 4.0342416 4.03881386
13 #_First_Mature_Age
4 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
2 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms
#_LO	HI	INIT	PRIOR	PR_SD PR_type PHASE env_var&link dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn		
#_LO	HI	INIT	PRIOR	PR_SD 	PR_type PHASE 	env dev_link dev_minyr dev_maxyr dev_PH Block Block_Fxn		
10	120	58.4	58.4	10	6	3	0	0	0	0	0	0	0	#	L_at_Amin_Fem_GP_1	#OBS	Err	Phase	3																																							
40	410	183.322	183	5	6	3	0	0	0	0	0	0	0	#	L_at_Amax_Fem_GP_1	#OBS	Err	Phase	3																																							
0.1	0.25	0.124	0.12	0.01	6	3	0	0	0	0	0	0	0	#	VonBert_K_Fem_GP_1	#OBS	Err	Phase	3																																							
0.05	0.3	0.22	0.123153 99	0	-3	0	0	0	0	0	0	0	#	CV_young_Fem_GP_1
0.05	0.3	0.1197	0.1	99	0	-3	0	0	0	0	0	0	0	#	CV_old_Fem_GP_1
-3	3	1.09E-05 1.09E-05 0.8	0	-3	0	0	0	0	0.5	0	0	#	Wtlen_1_Fem
0	3.5	3.0124	3.0124	0.01	6	3	0	0	0	0	0	0	0	#	Wtlen_2_Fem	#OBS	Err	Phase	3																																							
-3	300	154.9	55	0.8	0	-3	0	0	0	0	0.5	0	0	#	Mat50%_Fem
-3	3	-0.138	-0.138	0.8	0	-3	0	0	0	0	0.5	0	0	#	Mat_slope_Fem
-3	36	1.69908	0	0.8	0	-3	0	0	0	0	0.5	0	0	#	Eggs_intercept_Fem
-3	30	0.01296	0	0.8	0	-3	0	0	0	0	0.5	0	0	#	Eggs_slope_len_Fem
-3	3	-0.14393 0	0.8	0	-3	0	0	0	0	0.5	0	0	#	L_at_Amin_Mal_GP_1
-3	3	-0.0434285 0	0.8	0	-2	0	0	0	0	0.5	0	0	#	L_at_Amax_Mal_GP_1
-3	3	0.142563 0	0.8	0	-3	0	0	0	0	0.5	0	0	#	VonBert_K_Mal_GP_1
-3	3	0	0	99	0	-3	0	0	0	0	0	0	0	#	CV_young_Mal_GP_1
-3	3	0	0.56	99	0	-3	0	0	0	0	0	0	0	#	CV_old_Mal_GP_1
-3	3	1.09E-05 1.09E-05 0.8	0	-3	0	0	0	0	0.5	0	0	#	Wtlen_1_Mal
-3	3.5	3.0124	3.0124	0.8	0	-3	0	0	0	0	0.5	0	0	#	Wtlen_2_Mal
#0	0	0	0	0	0	-4	0	0	0	0	0	0	0	#	RecrDist_GP_1
#0	0	0	0	0	0	-4	0	0	0	0	0	0	0	#	RecrDist_Area_1
#0	0	0	0	0	0	-4	0	0	0	0	0	0	0	#	RecrDist_timing_1
1	1	1	1	1	0	-1	0	0	0	0	0	0	0	#	CohortGrowDev
0.000001 0.999999 0.5	0.5	0.5	0	-99	0	0	0	0	0	0	0	#	FracFemale_GP_1
#
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
#_Spawner-Recruitment
7 #_SR_function: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepard_3Parm
0  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_ LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn	#  parm_name
3	10	6.152510  6.152510 1	0	1	0	0	0	0	0	0	0	#	SR_LN(R0)																																											
0.5	1	0.85226	0.85226	0.05	6	2	0	0	0	0	0	0	0	#	SR_surv_Sfrac	#OBS	uncertainty	phase	2																																							
0.01	1	0.36	0.36	0.05	6	2	0	0	0	0	0	0	0	#	SR_surv_Beta	#OBS	uncertainty	phase	2																																							
0	2	0.18	0.2	0.8	0	-1	0	0	0	0	0	0	0	# SR_sigmaR
-2	2	0	0	0	0	-1	0	0	0	0	0	0	0	# SR_regime
-1	1	0.879134 0	0.13	0	-2	0	0	0	0	0	0	0	# SR_autocorr 0.671116, SD=0.130654
#
#3 #_SR_function: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepard_3Parm
#0  # 0/1 to use steepness in initial equ recruitment calculation
#0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
2 #do_recdev:  0=none; 1=devvector; 2=simple deviations
1980 # first year of main recr_devs; early devs can preceed this era
2015 # last year of main recr_devs; forecast devs start in following year
3 #_recdev phase 
1 # (0/1) to read 13 advanced options
 -10 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
 1 #_recdev_early_phase
 0 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
# 1983.5177 #_last_early_yr_nobias_adj_in_MPD                   
# 1984.9394 #_first_yr_fullbias_adj_in_MPD                      
# 2001.6001 #_last_yr_fullbias_adj_in_MPD                       
# 2001.7231 #_first_recent_yr_nobias_adj_in_MPD                 
#    0.0563 #_max_bias_adj_in_MPD (1.0 to mimic pre-2009 models)
# 1980.4 #_last_early_yr_nobias_adj_in_MPD
# 2015 #_first_yr_fullbias_adj_in_MPD
# 2015.9 #_last_yr_fullbias_adj_in_MPD
# 2016 #_first_recent_yr_nobias_adj_in_MPD
# 0.2543 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
 1970.044 #_last_early_yr_nobias_adj_in_MPD                   
 1990.614 #_first_yr_fullbias_adj_in_MPD                      
 2011.279 #_last_yr_fullbias_adj_in_MPD                       
 2015.814 #_first_recent_yr_nobias_adj_in_MPD                 
-1 #    0.000 #_max_bias_adj_in_MPD (1.0 to mimic pre-2009 models) (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
 0 #_period of cycles in recruitment (N parms read below)
 -15 #min rec_dev
 15 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#  1970E 1971E 1972E 1973E 1974E 1975E 1976E 1977E 1978E 1979E 1980R 1981R 1982R 1983R 1984R 1985R 1986R 1987R 1988R 1989R 1990R 1991R 1992R 1993R 1994R 1995R 1996R 1997R 1998R 1999R 2000R 2001R 2002R 2003R 2004R 2005R 2006R 2007R 2008R 2009R 2010R 2011R 2012R 2013R 2014R 2015R 2016F
#  -0.0115821 -0.0147935 -0.0155881 -0.0141344 -0.0158498 -0.00863294 0.000489352 0.0111848 0.035004 0.0476974 0.0399609 0.0442132 0.053377 0.0679423 0.045267 0.00760698 -0.00628016 -0.0276716 -0.0530526 -0.0949305 -0.121556 -0.123273 -0.126929 -0.13944 -0.122728 -0.0888777 -0.0163761 0.0521985 0.058488 0.0574458 0.0255265 0.0417787 -0.0248079 0.0863372 0.130889 0.505432 0.297043 0.108097 0.0300089 -0.267142 0.0422813 -0.0901449 0.0633888 0.0932142 0.0170739 -0.167816 0
# implementation error by year in forecast:  0
#
#Fishing Mortality info 
0.0005 # F ballpark
-2009 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
5 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
# if Fmethod=3; read N iterations for tuning for Fmethod 3
4  # N iterations for tuning F in hybrid method (recommend 3 to 7)
#
#_initial_F_parms; count = 1
#_ LO HI INIT PRIOR PR_SD  PR_type  PHASE
 1e-007 5 1e-007 -1 99 6 -1 # InitF_seas_1_flt_4F4_MEN_DSC
#2016 2016
# F rates by fleet
# Yr:  1960 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# F1_COM_GOM 3.04024e-005 6.08138e-005 9.12354e-005 0.000121668 0.000152112 0.000182569 0.000213038 0.00024352 0.000274016 0.000304524 0.000335047 0.000365586 0.000396154 0.0004268 0.000457591 0.000488575 0.000694204 0.000986626 0.00140248 0.00199403 0.00283789 0.00406128 0.00412696 0.0045632 0.00644236 0.00620848 0.0192685 0.058463 0.0909993 0.133487 0.121734 0.133573 0.111929 0.0775226 0.171734 0.101766 0.061041 0.0431171 0.053356 0.0432636 0.0540167 0.0791504 0.101722 0.0871448 0.0656148 0.0585868 0.0842762 0.0247202 0.00289626 0.0112332 0.00713783 0.00874064 0.00424797 0.00318942 0.00291166 0.00412479 0.00318005
# F2_COM_SA 3.68087e-006 7.36282e-006 1.1046e-005 1.47304e-005 1.84162e-005 2.21034e-005 2.57922e-005 2.94826e-005 3.31746e-005 3.68685e-005 4.05655e-005 4.42694e-005 4.79845e-005 5.17129e-005 5.54546e-005 5.92074e-005 8.40995e-005 0.00011945 0.000169641 0.000240931 0.000342664 0.000491328 0.000501178 0.00055567 0.000784259 0.000750832 0.00197138 0.00591139 0.0146502 0.0190106 0.0128607 0.00307229 0.0131493 0.0120797 0.020948 0.0193571 0.0168244 0.0109771 0.0133697 0.0223147 0.0173926 0.0173229 0.0274875 0.0205449 0.0191361 0.0210945 0.0203307 0.0118945 0.00176991 0.00150475 0.000986966 0.00124054 0.000611728 0.000987415 0.00190486 0.00260225 0.00200623
# F3_RecMEX 1.79978e-006 3.59997e-006 5.40054e-006 7.2015e-006 9.00287e-006 1.08047e-005 1.2607e-005 1.44099e-005 1.62134e-005 1.80175e-005 1.98655e-005 2.17446e-005 2.36328e-005 2.55083e-005 2.73799e-005 2.92053e-005 0.000130585 0.000582579 0.00258897 0.0115061 0.051803 0.244604 0.205061 0.381269 0.263764 0.218037 0.295705 0.169483 0.26313 0.194545 0.28267 0.242068 0.268354 0.262401 0.258542 0.321479 0.423189 0.335248 0.308819 0.309496 0.123664 0.139425 0.0868458 0.0748595 0.0723364 0.0644917 0.0655382 0.0711731 0.0548267 0.0549892 0.0649626 0.0427235 0.0547741 0.0528508 0.0469972 0.0487017 0.037547
# F4_MEN_DSC 0.000129117 0.000129134 0.000129151 0.000129169 0.000129187 0.000129206 0.000129225 0.000129246 0.000129268 0.00012929 0.000129492 0.000129733 0.000129962 0.000130142 0.000130326 0.000130378 0.00013028 0.000130038 0.000129499 0.000129009 0.000129527 0.000198389 0.000212248 0.000220446 0.000231867 0.000215568 0.000220394 0.000237774 0.000242151 0.000272008 0.00028416 0.000235045 0.000220083 0.000239298 0.00027058 0.000275971 0.000291555 0.000313346 0.000312274 0.000357402 0.000312378 0.000293814 0.000294118 0.000289292 0.000296514 0.000279203 0.000265178 0.000273595 0.000282318 0.000298067 0.00030359 0.000311723 0.000281474 0.000263949 0.00023315 0.000237298 0.000182947
#
#_Q_setup
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
         5         1         0         0         0         0  #  S1_LPS
         6         1         0         0         0         0  #  S5_NMFS_LLSE
-9999 0 0 0 0 0
#
#_Q_parms(if_any);Qunits_are_ln(q)
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn	#	parm_name																																											
-15	15	-7.36048 -7.36048 0.1	6	1	0	4	1960	2115	2	0	0	#	LnQ_base_S1_LPS(5)																																											
-15	15	-6.65551 -6.65551 0.1	6	1	0	4	1960	2115	2	0	0	#	LnQ_base_S5_NMFS_LLSE(9)																																											
#_timevary	q	parameters																																																								
#_	LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	#	parm_name																																																	
0.0001	2	0.5	0.5	0.5	6	-5	#	LnQ_base_S1(5)_dev_se																																																		
-0.99	0.99	0	0	0.5	6	-6	#	LnQ_base_S1(5)_dev_autocorr																																																		
0.0001	2	0.5	0.5	0.5	6	-5	#	LnQ_base_S2(6)_dev_se																																																		
-0.99	0.99	0	0	0.5	6	-6	#	LnQ_base_S2(6)_dev_autocorr																																																		
#_size_selex_types
#discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead
#_Pattern Discard Male Special
 24 0 3 0 # 1 F1_COM_GOM
  1 0 0 0 # 2 F2_COM_SA
 24 0 0 0 # 3 F3_RecMEX
  1 0 0 0 # 4 F4_MEN_DSC
 24 0 0 0 # 5 S1_LPS
 24 0 3 0 # 9 S5_NMFS_LLSE
#
#_age_selex_types
#_Pattern Discard Male Special
 0 0 0 0 # 1 F1_COM_GOM
 0 0 0 0 # 2 F2_COM_SA
 0 0 0 0 # 3 F3_RecMEX
 0 0 0 0 # 4 F4_MEN_DSC
 0 0 0 0 # 5 S1_LPS
 0 0 0 0 # 9 S5_NMFS_LLSE
#
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn	#parm_name
# 1   F1_COM_GOM LenSelex														
100	200	149.786	149.362	5	6	3	0	4	1960	2115	6	0	0	#SizeSel_P1_F1_COM_GOM(1)
-15	15	-10	-10	0	0	-3	0	0	0	0	0	0	0	#SizeSel_P2_F1_COM_GOM(1)
0	10	5.44459	5.44811	0.2	6	3	0	4	1960	2115	6	0	0	#SizeSel_P3_F1_COM_GOM(1)
0	10	5.59817	5.61113	0.2	6	3	0	4	1960	2115	6	0	0	#SizeSel_P4_F1_COM_GOM(1)
-999	-999	-999	-999	0	0	-3	0	0	0	0	0	0	0	#SizeSel_P5_F1_COM_GOM(1)
-999	-999	-999	-999	0	0	-3	0	0	0	0	0	0	0	#SizeSel_P6_F1_COM_GOM(1)
0	100	4	4	50	0	-4	0	0	0	0	0	0	0	#SzSel_Male_Peak_F1_COM_GOM(1)
-1	1	0.732645	0.747596	50	0	4	0	0	0	0	0	0	0	#SzSel_Male_Ascend_F1_COM_GOM(1)
-1	1	-0.6	-0.6	50	0	-4	0	0	0	0	0	0	0	#SzSel_Male_Descend_F1_COM_GOM(1)
-15	15	0	0	50	0	-4	0	0	0	0	0	0	0	#SzSel_Male_Final_F1_COM_GOM(1)
0	1	0.672053	0.669357	0.05	6	5	0	0	0	0	0	0	0	#SzSel_Male_Scale_F1_COM_GOM(1)
# 2   F2_COM_SA LenSelex														
50	150	92.4207	90.8082	0.5	6	2	0	4	1960	2115	6	0	0	#SizeSel_P1_F2_COM_SA(2)
1	100	27.3304	26.9618	1	6	3	0	4	1960	2115	6	0	0	#SizeSel_P2_F2_COM_SA(2)
# 3   F3_RecMEX LenSelex														
1	100	55.1711	55	5	6	2	0	4	1960	2115	6	0	0	#SizeSel_P1_F3_RecMEX(3)
-15	-5	-9.99662	-10	1	6	-3	0	0	0	0	0	0	0	#SizeSel_P2_F3_RecMEX(3)
-15	15	0	0	0	0	-3	0	0	0	0	0	0	0	#SizeSel_P3_F3_RecMEX(3)
0	10	7.24959	7.24959	0.2	6	3	0	4	1960	2115	6	0	0	#SizeSel_P4_F3_RecMEX(3)
-15	15	-15	-15	0	0	-3	0	0	0	0	0	0	0	#SizeSel_P5_F3_RecMEX(3)
-999	-999	-999	-999	0	0	-3	0	0	0	0	0	0	0	#SizeSel_P6_F3_RecMEX(3)
# 4   F4_MEN_DSC LenSelex														
1	100	45.6654	45.6654	1	6	2	0	4	1960	2115	6	0	0	#SizeSel_P1_F4_MEN_DSC(4)
0.01	5	1	1	0.25	6	3	0	4	1960	2115	6	0	0	#SizeSel_P2_F4_MEN_DSC(4)
# 5   S1_LPS LenSelex														
100	200	154.925	155.595	5	6	3	0	4	1960	2115	6	0	0	#SizeSel_P1_S1_LPS(5)
-15	15	-10	-10	0	0	-3	0	0	0	0	0	0	0	#SizeSel_P2_S1_LPS(5)
0	10	7.31615	7.30133	0.2	6	3	0	4	1960	2115	6	0	0	#SizeSel_P3_S1_LPS(5)
0	30	14.7717	14.6324	1	6	3	0	4	1960	2115	6	0	0	#SizeSel_P4_S1_LPS(5)
-999	-999	-999	-999	0	0	-3	0	0	0	0	0	0	0	#SizeSel_P5_S1_LPS(5)
-999	-999	-999	-999	0	0	-3	0	0	0	0	0	0	0	#SizeSel_P6_S1_LPS(5)
# 6   S5_NMFS_LLSE LenSelex														
100	200	162.45	161.844	5	6	2	0	4	1960	2115	6	0	0	#SizeSel_P1_S5_NMFS_LLSE(9)
-15	15	-10	-10	0	0	-3	0	0	0	0	0	0	0	#SizeSel_P2_S5_NMFS_LLSE(9)
0	10	7.12809	7.15082	0.2	6	3	0	4	1960	2115	6	0	0	#SizeSel_P3_S5_NMFS_LLSE(9)
0	10	5.56867	5.61039	0.3	6	3	0	4	1960	2115	6	0	0	#SizeSel_P4_S5_NMFS_LLSE(9)
-999	-999	-999	-999	0	0	-3	0	0	0	0	0	0	0	#SizeSel_P5_S5_NMFS_LLSE(9)
-999	-999	-999	-999	0	0	-3	0	0	0	0	0	0	0	#SizeSel_P6_S5_NMFS_LLSE(9)
-10	10	-6.60294	-6.14903	1	6	4	0	0	0	0	0	0	0	#SzSel_Male_Peak_S5_NMFS_LLSE(9)
-5	5	-0.659874	-0.656065	0.1	6	4	0	0	0	0	0	0	0	#SzSel_Male_Ascend_S5_NMFS_LLSE(9)
-5	5	-0.777465	-0.805247	0.1	6	4	0	0	0	0	0	0	0	#SzSel_Male_Descend_S5_NMFS_LLSE(9)
-15	15	0	0	0	0	-4	0	0	0	0	0	0	0	#SzSel_Male_Final_S5_NMFS_LLSE(9)
0	1	0.729221	0.734501	0.05	6	5	0	0	0	0	0	0	0	#SzSel_Male_Scale_S5_NMFS_LLSE(9)
#_timevary	selex	parameters																																																								
#_LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	#	parm_name																																																		
0.0001	2	0.5	0.5	0.5	6	-6	#SizeSel_P1_F1_COM_GOM(1)_dev_se																																																			
-0.99	0.99	0	0	0.5	6	-7	#SizeSel_P1_F1_COM_GOM(1)_dev_autocorr																																																			
0.0001	2	0.5	0.5	0.5	6	-6	#SizeSel_P1_F1_COM_GOM(3)_dev_se																																																			
-0.99	0.99	0	0	0.5	6	-7	#SizeSel_P1_F1_COM_GOM(3)_dev_autocorr																																																			
0.0001	2	0.5	0.5	0.5	6	-6	#SizeSel_P1_F1_COM_GOM(4)_dev_se																																																			
-0.99	0.99	0	0	0.5	6	-7	#SizeSel_P1_F1_COM_GOM(4)_dev_autocorr																																																			
0.0001	2	0.5	0.5	0.5	6	-6	#SizeSel_P1_F2_COM_SA(2)																																																			
-0.99	0.99	0	0	0.5	6	-7	#SizeSel_P1_F2_COM_SA(2)																																																			
0.0001	2	0.5	0.5	0.5	6	-6	#SizeSel_P2_F2_COM_SA(2)																																																			
-0.99	0.99	0	0	0.5	6	-7	#SizeSel_P2_F2_COM_SA(2)																																																			
0.0001	2	0.5	0.5	0.5	6	-6	#SizeSel_P1_F3_RecMEX(3)_dev_se																																																			
-0.99	0.99	0	0	0.5	6	-7	#SizeSel_P1_F3_RecMEX(3)_dev_autocorr																																																			
0.0001	2	0.5	0.5	0.5	6	-6	#SizeSel_P4_F3_RecMEX(3)_dev_se																																																			
-0.99	0.99	0	0	0.5	6	-7	#SizeSel_P4_F3_RecMEX(3)_dev_autocorr																																																			
0.0001	2	0.5	0.5	0.5	6	-6	#SizeSel_P1_F4_MEN_DSC(4)_dev_se																																																			
-0.99	0.99	0	0	0.5	6	-7	#SizeSel_P1_F4_MEN_DSC(4)_dev_autocorr																																																			
0.0001	2	0.5	0.5	0.5	6	-6	#SizeSel_P2_F4_MEN_DSC(4)_dev_se																																																			
-0.99	0.99	0	0	0.5	6	-7	#SizeSel_P2_F4_MEN_DSC(4)_dev_autocorr																																																			
0.0001	2	0.5	0.5	0.5	6	-6	#SizeSel_P1_S1_LPS(5)_dev_se																																																			
-0.99	0.99	0	0	0.5	6	-7	#SizeSel_P1_S1_LPS(5)_dev_autocorr																																																			
0.0001	2	0.5	0.5	0.5	6	-6	#SizeSel_P3_S1_LPS(5)_dev_se																																																			
-0.99	0.99	0	0	0.5	6	-7	#SizeSel_P3_S1_LPS(5)_dev_autocorr																																																			
0.0001	2	0.5	0.5	0.5	6	-6	#SizeSel_P4_S1_LPS(5)_dev_se																																																			
-0.99	0.99	0	0	0.5	6	-7	#SizeSel_P4_S1_LPS(5)_dev_autocorr																																																			
0.0001	2	0.5	0.5	0.5	6	-6	#SizeSel_P1_S5_NMFS_LLSE(9)_dev_se																																																			
-0.99	0.99	0	0	0.5	6	-7	#SizeSel_P1_S5_NMFS_LLSE(9)_dev_autocorr																																																			
0.0001	2	0.5	0.5	0.5	6	-6	#SizeSel_P3_S5_NMFS_LLSE(9)_dev_se																																																			
-0.99	0.99	0	0	0.5	6	-7	#SizeSel_P3_S5_NMFS_LLSE(9)_dev_autocorr																																																			
0.0001	2	0.5	0.5	0.5	6	-6	#SizeSel_P4_S5_NMFS_LLSE(9)_dev_se																																																			
-0.99	0.99	0	0	0.5	6	-7	#SizeSel_P4_S5_NMFS_LLSE(9)_dev_autocorr																																																			
#
0   #  use 2D_AR1 selectivity(0/1):  experimental feature
#_no 2D_AR1 selex offset used
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# no timevary parameters
#
#
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp
# #Factor Fleet New_Var_adj hash Old_Var_adj New_Francis   New_MI Francis_mult Francis_lo Francis_hi  MI_mult
#       4     1    0.954323    #           1    0.954323  2.18530     0.954323   0.567269   2.891951  2.18530
#       4     2    1.221688    #           1    1.221688 11.49890     1.221688   0.658024   4.551748 11.49890
#       4     3    1.003188    #           1    1.003188  1.25207     1.003188   0.603999   5.118390  1.25207
#       4     5    0.980506    #           1    0.980506  0.46142     0.980506   0.950946        Inf  0.46142
#       4     6    1.047855    #           1    1.047855  3.34823     1.047855   0.673844   3.235377  3.34823
#Factor Fleet New_Var_adj 
       4     1    1 # F1_COM_GOM     
       4     2    1 # F2_COM_SA     
       4     3    1 # F3_RecMEX     
       4     5    1 # S1_LPS      
       4     6    1 # S5_NMFS_LLSE     
#Factor Fleet New_Var_adj hash Old_Var_adj New_Francis    New_MI Francis_mult Francis_lo Francis_hi   MI_mult Type               Name Note
#       4     1    0.328801    #      0.2921    0.328801  0.699766     1.125644   0.698506   2.917212  2.395638  len         F1_COM_GOM     
#       4     2    0.046092    #      0.0294    0.046092  0.514138     1.567739   0.851587   4.829831 17.487687  len          F2_COM_SA     
#       4     3    2.392529    #      0.9092    2.392529  3.001230     2.631466   1.506294  15.188400  3.300957  len          F3_RecMEX     
#       4     5    2.170880    #      1.1398    2.170880  1.000930     1.904615   1.818879        Inf  0.878163  len             S1_LPS     
#       4     6    0.028464    #      0.0716    0.028464  0.593435     0.397548   0.225126   1.124115  8.288198  len         S2_BLLOP_1     
#       4     7    8.411934    #      9.8483    8.411934 15.003800     0.854151   0.496222   4.993204  1.523491  len         S3_BLLOP_2     
#       4     8    0.158126    #      0.1317    0.158126  2.550370     1.200656   0.604691   4.498119 19.364996  len           S4_VA_LL     
#       4     9    0.214959    #      0.2936    0.214959  0.714757     0.732149   0.487065   2.342410  2.434458  len       S5_NMFS_LLSE     
#       4    10    0.524174    #      1.5812    0.524174  1.575920     0.331504   0.225727   0.806482  0.996661  len       S6_CST_NE_LL     
#       4    11    0.446504    #      0.1447    0.446504  1.054100     3.085721   1.615676 239.012360  7.284727  len         S7_NMFS_NE     
#       4    12    0.547055    #      1.0707    0.547055  3.904440     0.510932   0.351455  60.633220  3.646624  len           S8_PLLOP     
#       4    13    1.554705    #      2.0907    1.554705  1.020210     0.743629   0.486727   2.108953  0.487975  len S9_COASTSPAN_SE_LL     
#       4    14    0.161348    #      0.1649    0.161348  1.283450     0.978459   0.540257  18.522996  7.783202  len    S10_SCDNR_RedDr     
#       4    15    0.529259    #      0.4257    0.529259  0.960780     1.243267   0.698749   8.953763  2.256942  len   S11_SEAMAP_LL_SE   
#_Factor  Fleet  Value
#      4      1    0.2921
#      4      2    0.0294
#      4      3    0.9092
#      4      5    1.1398
#      4      6    0.0716
#      4      7    9.8483
#      4      8    0.1317
#     4      9    0.2936
#     4     10    1.5812
#     4     11    0.1447
#     4     12    1.0707
#     4     13    2.0907
#     4     14    0.1649
#     4     15    0.4257
 -9999   1    0  # terminator
#
1 #_maxlambdaphase
1 #_sd_offset
# read 31 changes to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark
#like_comp fleet  phase  value  sizefreq_method
 1 1 1 0 1
 1 2 1 0 1
 1 3 1 0 1
 1 4 1 0 1
 1 5 1 1 1
 1 6 1 1 1
 4 1 1 1 0
 4 2 1 1 0
 4 3 1 1 0
 4 4 1 0 0
 4 5 1 1 0
 4 6 1 1 0
 9 1 1 0 0
-9999  1  1  1  1  #  terminator
#
# lambdas (for info only; columns are phases)
#  0 #_CPUE/survey:_1
#  0 #_CPUE/survey:_2
#  0 #_CPUE/survey:_3
#  0 #_CPUE/survey:_4
#  1 #_CPUE/survey:_5
#  1 #_CPUE/survey:_6
#  1 #_CPUE/survey:_7
#  1 #_CPUE/survey:_8
#  1 #_CPUE/survey:_9
#  1 #_CPUE/survey:_10
#  1 #_CPUE/survey:_11
#  1 #_CPUE/survey:_12
#  1 #_CPUE/survey:_13
#  1 #_CPUE/survey:_14
#  1 #_CPUE/survey:_15
#  1 #_lencomp:_1
#  1 #_lencomp:_2
#  1 #_lencomp:_3
#  0 #_lencomp:_4
#  1 #_lencomp:_5
#  0 #_lencomp:_6
#  0 #_lencomp:_7
#  1 #_lencomp:_8
#  1 #_lencomp:_9
#  1 #_lencomp:_10
#  1 #_lencomp:_11
#  1 #_lencomp:_12
#  1 #_lencomp:_13
#  1 #_lencomp:_14
#  1 #_lencomp:_15
#  0 #_init_equ_catch
#  1 #_recruitments
#  1 #_parameter-priors
#  1 #_parameter-dev-vectors
#  1 #_crashPenLambda
#  0 # F_ballpark_lambda
0 # (0/1) read specs for more stddev reporting 
 # 0 1 -1 5 1 5 1 -1 5 # placeholder for selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages
 # placeholder for vector of selex bins to be reported
 # placeholder for vector of growth ages to be reported
 # placeholder for vector of NatAges ages to be reported
999

