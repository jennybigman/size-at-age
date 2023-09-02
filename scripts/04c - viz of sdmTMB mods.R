# 04c - visualizations of sdmTMB mods

	library(visreg)
	
	pol_plot_df <- visreg(presurvey_btemp_int_pol, "presurvey_btemp", by = "age_f")
		