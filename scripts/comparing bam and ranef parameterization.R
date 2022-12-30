	mod1 <- bam(log_sc_weight ~  te(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  		   s(cohort, bs="re"),
                			   random=~(1|YEAR/HAUL), data=lagdat) 

	
	mod2 <- bam(log_sc_weight ~  te(LONGITUDE, LATITUDE) + s(julian, k = 4) + #te is better for bam
              s(cohort, bs="re") + s(HAUL, by = YEAR, bs = "re"),
              data=lagdat)  
	
	mod3 <- bam(log_sc_weight ~  te(LONGITUDE, LATITUDE) + s(julian, k = 4) + #te is better for bam
             s(cohort, bs="re") + s(HAUL, YEAR, bs = "re"),
             data=lagdat)  

	
	summary(mod1)
	summary(mod2)
	summary(mod3)
	
	coef(mod1)
	coef(mod2)
	
	# extract random effect from mod1 to see if it mactches mod2
	
	variance_comp(mod1) # doesn't come up
	variance_comp(mod2)
	variance_comp(mod3)

	
# try plotting them
	
	v <- visreg(fit, "Time", by="Cow", re.form=~(Time|Cow), plot=FALSE)
subCow <- sample(levels(Milk$Cow), 10)
vv <- subset(v, Cow %in% subCow)
plot(vv, ylab="Protein", layout=c(5,2))

	# mod1

	mod1_vr <- visreg(mod1, "julian", re.form = ~ (1|YEAR/HAUL),
										plot = FALSE)





	# gamm from mgcv also fits random effects
	
	mod3 <- gamm(log_sc_weight ~  t2(LONGITUDE, LATITUDE) + s(julian, k = 4),
               random = list(cohort = ~1), data=lagdat, REML=TRUE) 
	
	mod4 <- gamm(log_sc_weight ~  t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
					s(cohort, bs="re"),
          data=lagdat, REML=TRUE) 

 summary(mod3$gam)
 summary(mod4$gam)

 mod5 <- gamm(log_sc_weight ~  t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
 						 	s(cohort, bs = "re") + s(HAUL, by = YEAR, bs = "re"),
              data=lagdat, REML=TRUE)  
 
 summary(mod5$gam)
 summary(mod2)
 
 
 

 
 
	# Krista's original model
	base_mod_KO <- readRDS("~/Dropbox/NOAA AFSC Postdoc/temp, growth, size project/size-at-age/output/model output/pollock/base_mod_KO.rds")

	base_mod_KO_gamm <- gamm4(log_sc_weight ~  t2(LONGITUDE, LATITUDE) + s(julian, k = 4) +
                  		   s(cohort, bs="re"),
                			   random=~(1|YEAR/HAUL), data=lagdat, REML=TRUE) 
	
	summary(base_mod_KO_bam)
	
	ranef_df <- ranef(base_mod_KO$mer)

	variance_comp(base_mod_KO$gam)	
	