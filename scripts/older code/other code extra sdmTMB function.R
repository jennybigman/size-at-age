 # if no error and sanity() does not look good aside from a few non-issues (see above), rerun model with extra optimization
		 						
						} else if sanity(mod)$all_ok != TRUE
		 						
		 							print('running extra optimization')
								
									mod_eo <- try(run_extra_optimization(mod, nlminb_loops = 3, newton_loops = 1)) 
							
										# if model with extra optimization (eo) threw an error, rerun with no newton loops
 										if (class(mod_eo) == "try-error"){
		 						
											print(paste("newton loops threw error, running with no newtown loops"))

											mod_eo <- try(run_extra_optimization(mod, nlminb_loops = 3, newton_loops = 0)) 
			
												} else if sanity(mod_eo)$all_ok == "TRUE") {
		 				 	
		 												write_rds(mod_eo, 
															file = paste0(here(), file_path_all, y, "_no_int_mod_", sp, ".rds"))
		 						
		 												print(paste("no int model for", sp, "with", y, "complete")) 
		 										
		 										{ else if }				
		 										
												} else {
													
													print('extra optimization did not work')
												}
									
		 				
									} else if sanity(mod)$gradients_ok != "TRUE" | sanity(mod)$se_magnitude_ok != "TRUE" { 
					
		 						
							
 												####### FIX STEP HERE #########
											
															print(paste0('boo - check sanity for no int model for', sp, "with", y))
 													 	
 													 } else {
 													 	
 													 		write_rds(mod_eo, 
 													 							file = paste0(here(), 
 													 							file_path_all, y, "_no_int_mod_", sp, ".rds"))
		 						
 												   }
							
								# if model with extra optimization (eo) looks all good, save model
									
		 						# if model with extra optimization (eo) does not look all good aside from a few non-issues, tell me
										} else if (any(ind_eo %!in% not_probs)) { 
											
												print(paste0('boo - check sanity for no int model for', sp, "with", y))

												write_rds(mod_eo, 
													file = paste0(here(), file_path_all, y, "_no_int_mod_", sp, ".rds"))
		 									
											
		 					 # if model with extra optimization (eo) looks all good aside from a few non-issues, save model
										} else {
								
												write_rds(mod_eo, 
													file = paste0(here(), file_path_all, y, "_no_int_mod_", sp, ".rds"))
		 									
										}
								
						