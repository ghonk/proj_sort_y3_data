# # # # HELPER FUNCTIONS FOR PROJECT SORT YEAR 3 ANALYSIS # # # #
require(dplyr)
require(stringr)
require(mi)


# # # # separate analysis output; prints clean divisions and spaces
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
printspace <- function(numspaces,printline){
	if (printline==1)
		{
		print(rep("___", 24))
		}
	cat(rep("\n",numspaces))
	}
	
# # # # iterable imputation func
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
iterimp <- function(datatoimpute,numberimps,expname){
	# # get/set imputation info
	info = mi.info(datatoimpute)	
	info = update(info, "type", list("period" = "unordered-categorical"))
	print(info)
	# # impute dat data once
	imp = mi(datatoimpute, info, n.imp = numberimps, n.iter = 60, R.hat = 1.1, 
		max.minutes = 90, rand.imp.method = "bootstrap", 
		run.past.convergence = FALSE, seed = NA, 
		check.coef.convergence = TRUE, add.noise = noise.control())
	# # impute dat data til *data* convergence
	looptoconverge = FALSE
	if ((converged(imp, check = "data")) == TRUE){
		looptoconverge = TRUE
		}
	while(looptoconverge == FALSE){
		imp = mi(imp)
		looptoconverge = converged(imp, check = "data")
		print(paste0("data converged: ",converged(imp, check = "data")))
		print(paste0("coefs converged: ",converged(imp, check = "coefs")))
		}
	# # save imputed datasets
	curdir = getwd()
	subdir = expname
	if (file.exists(subdir)){
		setwd(file.path(curdir, subdir))
		write.mi(imp)
	} else {
		dir.create(file.path(curdir, subdir))
		setwd(file.path(curdir, subdir))
		write.mi(imp)
	}
	setwd(curdir)	
	return(imp)
	}


# # # # convert ordinal vars to proportion correct scores for averaging
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
scale_for_avg <- function(variables){
	var_df <- NULL
	colnames(variables)
	var_names <- paste0(colnames(variables), '_p')
	for (target_var in variables){
		max_val <- max(target_var, na.rm = TRUE)
		scaled_var <- target_var/max_val 
		var_df <- cbind(var_df, scaled_var)
	}
	colnames(var_df) <- var_names
	return(data.frame(var_df))
}


# # # # convert all items to proportion scores
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
convert_all_vars <- function(all_imputed_data){
  for(exp_data in 1:length(all_imputed_data)){
    for(dataset in 1:length(all_imputed_data[[exp_data]])){
			
	  temp_df <- all_imputed_data[[exp_data]][[dataset]][,-(1:8)]
			
	  proportion_vars <- scale_for_avg(temp_df)
	  tot_prop_correct <- apply(proportion_vars, 1, mean)
	
	  mc_correct <-
	    all_imputed_data[[exp_data]][[dataset]][substr(colnames(all_imputed_data[[exp_data]][[dataset]]),5,6) == 'mc']
	  mc_pts_correct <- apply(mc_correct, 1, sum)

	  fr_correct <- 
		all_imputed_data[[exp_data]][[dataset]][substr(colnames(all_imputed_data[[exp_data]][[dataset]]),5,6) == 'fr']
	  fr_correct <- scale_for_avg(fr_correct)
	  fr_prop_correct <- apply(fr_correct, 1, mean)
	  	
	  all_imputed_data[[exp_data]][[dataset]] <-
	    cbind(all_imputed_data[[exp_data]][[dataset]], 
	  	  proportion_vars, tot_prop_correct, mc_pts_correct, fr_prop_correct)
	}
  }
  return(all_imputed_data)
}


# # # this function iterates through the datasets of each experiment, 
# # # runs the appropriate models and collapses the results into one
# # # coefficient table for the target variable
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
iter_lmer_multi <- function(experiment_data,target_var){
  require(lmerTest)
  require(mi)

  # # key for conditions variable:
  exp_key <- list(c(1),c(1),c(2),c(2),c(3),c(3))

  # # iterate thru mastery and transfer experiments (1-6)
  for(experiment in 1:length(experiment_data)){
	
	get_conditions_command <- 
	  paste0('conditions <- attributes(experiment_data[[experiment]][[1]]$cond_e',
	  exp_key[[experiment]],')$levels')
	eval(parse(text = paste0(get_conditions_command)))	

	# # create lists for storing regression models
	for(condition in 2:length(conditions)){
		
	  model_list_label <- paste0(conditions[[condition]],experiment,'models')  
	  eval(parse(text = paste0(model_list_label,' <- list()')))
	
	  # # iterate thru imputed datasets 
      for(dataset in 1:length(experiment_data[[experiment]])){
			
	    # # set the intercept factors for the regression
		relevel_command <- paste0('experiment_data[[experiment]][[dataset]] <- within(experiment_data[[experiment]][[dataset]], cond_e',
		  exp_key[[experiment]],' <- relevel(cond_e', exp_key[[experiment]],', ref = \"', conditions[[condition]],'\"))')
		eval(parse(text = relevel_command))
		
		# # construct the model
		model_command <- 
		  paste0('model <- lmer(',target_var,' ~ as.factor(cond_e', exp_key[[experiment]],
			') + (1|z_cellstest) + (1|teacher/tp), data = experiment_data[[experiment]][[dataset]])')
		eval(parse(text = model_command))

		eval(parse(text = paste0(model_list_label,'[[',dataset,']] <- model')))
		# print(summary(model))
		
		print(paste0(' --- model ', dataset,' for experiment ', experiment,
		  ' - ', conditions[[condition]], ' as intercept --- '))
	  }
	}
  }
  # # TODO: get rid of this hard coding
  lists_of_models <- 
    list(sgl1models, spc1models, wsh1models, #E1 mast
	     sgl2models, spc2models, wsh2models, #E1 tran
		 bsd3models, psd3models, wsh3models, #E2 mast
		 bsd4models, psd4models, wsh4models, #E2 tran
		 ind5models, nof5models, wsh5models, #E3 mast
		 ind6models, nof6models, wsh6models) #E3 tran

  final_results_tables <- list()

  for(model_set in 1:length(lists_of_models)){

	coefficient_table_list <- list(list(),list()) # [[1]] for coefs, [[2]] for std error
	model_row_names <- rownames(coefficients(summary(lists_of_models[[model_set]][[1]])))
	model_row_length <- length(model_row_names)
	
	for(model in 1:length(lists_of_models[[model_set]])){
		
	  # # construct each model's coefficient table
	  current_coef_table <- 
		t(coefficients(summary(lists_of_models[[model_set]][[model]])))
	
		coefficient_table_list[[1]][[model]] <- current_coef_table[1,] # regression coefficients
		coefficient_table_list[[2]][[model]] <- current_coef_table[2,] # standard error
	}
	# get pooled coefficients and error via mi package
	pooled_coefs_and_error <- 
	  mi.pooled(coefficient_table_list[[1]],coefficient_table_list[[2]])

	# tidy it up and get the p estimate
	coef_est  <- pooled_coefs_and_error$coefficients
	std_error <- pooled_coefs_and_error$se
	z_vals    <- coef_est / std_error
	p_vals    <- 2 * (1 - pnorm(abs(z_vals)))

	final_results_tables[[model_set]] <- 
	  data.frame(cbind(coef_est, std_error, z_vals, p_vals))	
  }
  
  return(final_results_tables)

}


# # # this function takes data from one experiment, a target variable 
# # # and the experiment number, runs regression models and collapses
# # # the results into one coefficient table for the target variable
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
iter_lmer <- function(experiment_data,target_var,experiment){
  require(lmerTest)
  require(mi)

  # # key for conditions variable:
  exp_key <- list(c(1),c(1),c(2),c(2),c(3),c(3))
	
  get_conditions_command <- 
    paste0('conditions <- attributes(experiment_data[[1]]$cond_e',
    exp_key[[experiment]],')$levels')
  eval(parse(text = paste0(get_conditions_command)))	

  # # create lists for storing regression models
  lists_of_models <- list()

  # # iterate thru possible intercepts for regression
  for(condition in 2:length(conditions)){
	
    model_list_label <- paste0(conditions[[condition]],experiment,'models')  
    eval(parse(text = paste0(model_list_label,' <- list()')))
    
    # # iterate thru imputed datasets 
    for(dataset in 1:length(experiment_data)){
		
      # # set the intercept factors for the regression
	  relevel_command <- paste0('experiment_data[[dataset]] <- within(experiment_data[[dataset]], cond_e',
	    exp_key[[experiment]],' <- relevel(cond_e', exp_key[[experiment]],', ref = \"', conditions[[condition]],'\"))')
	  eval(parse(text = relevel_command))
	
	  # # construct the model
	  model_command <- 
	    paste0('model <- lmer(',target_var,' ~ as.factor(cond_e', exp_key[[experiment]],
		  ') + (1|z_cellstest) + (1|teacher/tp), data = experiment_data[[dataset]])')
	  eval(parse(text = model_command))

	  eval(parse(text = paste0(model_list_label,'[[',dataset,']] <- model')))
	  # print(summary(model))
	
	  print(paste0(' --- model ', dataset,' for experiment ', experiment,
	    ' - ', conditions[[condition]], ' as intercept --- '))
    }

    # # save list of regression models 
    reg_list_command <- 
  	  paste0('lists_of_models[[(condition) - 1]] <- ', model_list_label)  
    eval(parse(text = reg_list_command))
  }

  # # now extract regsression summaries, pool them
  final_results_tables <- list()
  for(model_set in 1:length(lists_of_models)){

	coefficient_table_list <- list(list(),list()) # [[1]] for coefs, [[2]] for std error
	model_row_names <- rownames(coefficients(summary(lists_of_models[[model_set]][[1]])))
	model_row_length <- length(model_row_names)
	
	for(model in 1:length(lists_of_models[[model_set]])){
		
	  # # construct putative coefficient table
	  current_coef_table <- 
		t(coefficients(summary(lists_of_models[[model_set]][[model]])))
	
	  coefficient_table_list[[1]][[model]] <- current_coef_table[1,] # regression coefficients
	  coefficient_table_list[[2]][[model]] <- current_coef_table[2,] # standard error
	}

	pooled_coefs_and_error <- 
	  mi.pooled(coefficient_table_list[[1]],coefficient_table_list[[2]])

	coef_est  <- pooled_coefs_and_error$coefficients
	std_error <- pooled_coefs_and_error$se
	z_vals    <- coef_est / std_error
	p_vals    <- 2 * (1 - pnorm(abs(z_vals)))

	final_results_tables[[model_set]] <- 
	  data.frame(cbind(coef_est, std_error, z_vals, p_vals))	
  }
  
  return(final_results_tables)

}


# # # this function takes data from one experiment, a target variable 
# # # and the experiment number, runs regression models and collapses
# # # the results into one coefficient table for the target variable
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
iter_clmm <- function(experiment_data,target_var,experiment){
  require(ordinal)
  require(mi)

  # # key for conditions variable:
  exp_key <- list(c(1),c(1),c(2),c(2),c(3),c(3))
	
  get_conditions_command <- 
    paste0('conditions <- attributes(experiment_data[[1]]$cond_e',
    exp_key[[experiment]],')$levels')
  eval(parse(text = paste0(get_conditions_command)))	

  # # create lists for storing regression models
  lists_of_models <- list()

  # # iterate thru possible intercepts for regression
  for(condition in 2:length(conditions)){
	
    model_list_label <- paste0(conditions[[condition]],experiment,'models')  
    eval(parse(text = paste0(model_list_label,' <- list()')))
    
    # # iterate thru imputed datasets 
    for(dataset in 1:length(experiment_data)){
		
      # # set the intercept factors for the regression
	  relevel_command <- paste0('experiment_data[[dataset]] <- within(experiment_data[[dataset]], cond_e',
	    exp_key[[experiment]],' <- relevel(cond_e', exp_key[[experiment]],', ref = \"', conditions[[condition]],'\"))')
	  eval(parse(text = relevel_command))
	
	  # # construct the model
	  model_command <- 
	    paste0('model <- clmm(as.factor(',target_var,') ~ as.factor(cond_e', exp_key[[experiment]],
		  ') + (1|z_cellstest) + (1|teacher/tp), data = experiment_data[[dataset]])')
	  eval(parse(text = model_command))

	  eval(parse(text = paste0(model_list_label,'[[',dataset,']] <- model')))
	  # print(summary(model))
	
	  print(paste0(' --- model ', dataset,' for experiment ', experiment,
	    ' - ', conditions[[condition]], ' as intercept --- '))
    }

    # # save list of regression models 
    reg_list_command <- 
  	  paste0('lists_of_models[[(condition) - 1]] <- ', model_list_label)  
    eval(parse(text = reg_list_command))
  }

  # # now extract regsression summaries, pool them
  final_results_tables <- list()
  for(model_set in 1:length(lists_of_models)){

	coefficient_table_list <- list(list(),list()) # [[1]] for coefs, [[2]] for std error
	model_row_names <- rownames(coefficients(summary(lists_of_models[[model_set]][[1]])))
	model_row_length <- length(model_row_names)
	
	for(model in 1:length(lists_of_models[[model_set]])){
		
	  # # construct putative coefficient table
	  current_coef_table <- 
		t(coefficients(summary(lists_of_models[[model_set]][[model]])))
	
	  coefficient_table_list[[1]][[model]] <- current_coef_table[1,] # regression coefficients
	  coefficient_table_list[[2]][[model]] <- current_coef_table[2,] # standard error
	}
    
	pooled_coefs_and_error <- 
	  mi.pooled(coefficient_table_list[[1]],coefficient_table_list[[2]])

	coef_est  <- pooled_coefs_and_error$coefficients
	std_error <- pooled_coefs_and_error$se
	z_vals    <- coef_est / std_error
	p_vals    <- 2 * (1 - pnorm(abs(z_vals)))

	final_results_tables[[model_set]] <- 
	  data.frame(cbind(coef_est, std_error, z_vals, p_vals))	
  }
  
  return(final_results_tables)

}

# # # takes a specific dataset and returns adjusted condition means
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
calc_adjusted_means <- function(complete_datalist, target_var, exp, dataset){
  require(lmerTest)
  require(tables)

  exp_key <- list(c(1),c(1),c(2),c(2),c(3),c(3))

  model_specs <- paste0('model <- lmer(',target_var,' ~ cond_e', exp_key[[exp]],
    ' + (1|z_cellstest) + (1|teacher/tp), data = complete_datalist[[', 
    exp, ']][[', dataset, ']])')
  eval(parse(text = model_specs))

  lsm_tab <- lsmeans(model, test.effs = NULL, method.grad = 'simple')
  lsm_tab <- data.frame(lsm_tab$lsmeans.table)

  return(lsm_tab)

}


# # # add a new variable to all data sets 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
add_variable <- function(complete_datalist){
  require(dplyr)
  dir <- 'C://Users//garre//Dropbox//aa projects//materials//SORT//year 3//year 3 data'

  # make new df with pid and new variable
  datafull <- read.csv(paste0(dir, '//psy3master.csv'))
  pid <- datafull$pid
  num_sorts <- datafull$num_sorts
  sort_data <- data.frame(cbind(pid, num_sorts))

  # add data to relevant imputed datasets
  for(exp in 5:6){
    for(ds in 1:length(complete_datalist[[exp]])){
      complete_datalist[[exp]][[ds]] <- inner_join(complete_datalist[[exp]][[ds]], sort_data)
    }
  }

  return(complete_datalist)

}




