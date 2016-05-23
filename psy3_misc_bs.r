setwd('C://Users//garre//Dropbox//aa projects//materials//SORT//year 3//year 3 data')
load('finaldata_allps.rdata')
source('psy3helperfuncs.r')
complete_datalist <- list(impdata_e1, impdata_e2, impdata_e3,
  impdata_e4, impdata_e5, impdata_e6)
complete_datalist <- convert_all_vars(complete_datalist)


Standard.Error <- aggregate(Standard.Error ~ cond_e1, mean, data = dataset)
Estimate <- aggregate(Estimate ~ cond_e1, mean, data = dataset)
cbind(Estimate, Standard.Error)

Standard.Error <- aggregate(Standard.Error ~ cond_e3, mean, data = adjusted_means[[6]])
Estimate <- aggregate(Estimate ~ cond_e3, mean, data = adjusted_means[[6]])
lsm_tab <- cbind(Estimate, Standard.Error)

aggregate(Upper.CI ~ cond_e1, mean, data = adjusted_means[[1]])




graph <- ggplot(data = lsm_tab, aes(x = cond_e2, y = Estimate)) + geom_bar(stat = 'identity') +
	theme(plot.title = element_text(size=32, face="bold", vjust=2),
		axis.title.y = element_text(size=26,vjust = 1.5),
		axis.text.y = element_text(size=18,color='black'),
		axis.title.x = element_text(size=26,vjust = -.5),
		axis.text.x = element_text(size = 18,color='black'),
		panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())+
	ggtitle('Transfer - Assessment Accuracy') +
	scale_y_continuous(limits=c(0,1.00),breaks=seq(0, 1, 0.1), name = 'Proportion Correct') + 
	scale_x_discrete(name = 'Condition', 
		breaks = c('bsd','psd','nsd','wsh'),
		limits = c('bsd','psd','nsd','wsh'),
		labels = c('Both Cards','Positive Card','No Cards','Worksheet')) +
	geom_errorbar(aes(ymin = lsm_tab$Estimate - lsm_tab$Standard.Error,
		ymax = lsm_tab$Estimate + lsm_tab$Standard.Error), 
		width=.2,position=position_dodge(.9)) +
	coord_cartesian(ylim=c(0,.75)) 


cla <- ggplot(cla_only, aes(x = e3t_mc_1)) + geom_histogram(binwidth = 1, aes(y=..count../sum(..count..))) + 
	scale_x_continuous(limits=c(0,3), name = '') + ggtitle ('Classroom Feedback') + scale_y_continuous(limits=c(0,1),breaks=seq(0, 1, 0.2), name = '')
ind <- ggplot(ind_only, aes(x = e3t_mc_1)) + geom_histogram(binwidth = 1, aes(y=..count../sum(..count..))) + 
	scale_x_continuous(limits=c(0,3), name = '') + ggtitle ('Individual Feedback') + scale_y_continuous(limits=c(0,1),breaks=seq(0, 1, 0.2),name = '')
nof <- ggplot(nof_only, aes(x = e3t_mc_1)) + geom_histogram(binwidth = 1, aes(y=..count../sum(..count..))) + 
	scale_x_continuous(limits=c(0,3), name = '') + ggtitle ('No Feedback') + scale_y_continuous(limits=c(0,1),breaks=seq(0, 1, 0.2),name = '')
wsh <- ggplot(wsh_only, aes(x = e3t_mc_1)) + geom_histogram(binwidth = 1, aes(y=..count../sum(..count..))) + 
	scale_x_continuous(limits=c(0,3), name = '') + ggtitle ('Worksheet') + scale_y_continuous(limits=c(0,1),breaks=seq(0, 1, 0.2),name = '')


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
  for(experiment in 5:length(experiment_data)){
	
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
		  paste0('model <- lmer(',target_var,' ~ as.numeric(num_sorts) + as.factor(cond_e', exp_key[[experiment]], ') + (1|z_cellstest) + (1|teacher/tp), data = experiment_data[[experiment]][[dataset]])')
		eval(parse(text = model_command))

		eval(parse(text = paste0(model_list_label,'[[',dataset,']] <- model')))
		# print(summary(model))
		
		print(paste0(' --- model ', dataset,' for experiment ', experiment,
		  ' - ', conditions[[condition]], ' as intercept --- '))
	  }
	}
  }
  
  lists_of_models <- 
    list(ind5models, nof5models, wsh5models, #E3 mast
		 ind6models, nof6models, wsh6models) #E3 tran

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
	    paste0('model <- lmer(',target_var,' ~ as.numeric(num_sorts) + (1|z_cellstest) + (1|teacher/tp), data = experiment_data[[dataset]])')
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











# # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
graph_adjusted_means <- function(dataset, experiment){
  require(ggplot2)

  if (experiment == 1){

  	Standard.Error <- aggregate(Standard.Error ~ cond_e1, mean, data = dataset)
    Estimate <- aggregate(Estimate ~ cond_e1, mean, data = dataset)
    lsm_tab <- cbind(Estimate, Standard.Error)

    graph <- ggplot(data = lsm_tab, aes(x = cond_e1, y = Estimate)) + geom_bar(stat = 'identity') +
	theme(plot.title = element_text(size=32, face="bold", vjust=2),
		axis.title.y = element_text(size=26,vjust = 1.5),
		axis.text.y = element_text(size=18,color='black'),
		axis.title.x = element_text(size=26,vjust = -.5),
		axis.text.x = element_text(size = 18,color='black'),
		panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())+
	ggtitle('Mastery - Assessment Accuracy') +
	scale_y_continuous(limits=c(0,1.00),breaks=seq(0, 1, 0.1), name = 'Proportion Correct') + 
	scale_x_discrete(name = 'Condition', 
		breaks = c('msd','spc','sgl','wsh'),
		limits = c('msd','spc','sgl','wsh'),
		labels = c('Massed\nSort','Spaced\nSort','Single\nSort','Worksheet')) +
	geom_errorbar(aes(ymin = lsm_tab$Estimate - lsm_tab$Standard.Error,
		ymax = lsm_tab$Estimate + lsm_tab$Standard.Error), 
		width=.2,position=position_dodge(.9)) +
	coord_cartesian(ylim=c(.0,.75)) 

  } else if (experiment == 2) {

  	Standard.Error <- aggregate(Standard.Error ~ cond_e1, mean, data = dataset)
    Estimate <- aggregate(Estimate ~ cond_e1, mean, data = dataset)
    lsm_tab <- cbind(Estimate, Standard.Error)

  	graph <- ggplot(data = lsm_tab, aes(x = cond_e1, y = Estimate)) + geom_bar(stat = 'identity') +
	  theme(plot.title = element_text(size=32, face="bold", vjust=2),
		axis.title.y = element_text(size=26,vjust = 1.5),
		axis.text.y = element_text(size=18,color='black'),
		axis.title.x = element_text(size=26,vjust = -.5),
		axis.text.x = element_text(size = 18,color='black'),
		panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())+
	  ggtitle('Transfer - RMTS Accuracy') +
	  scale_y_continuous(limits=c(0,1.00),breaks=seq(0, 1, 0.1), name = 'Proportion Correct') + 
	  scale_x_discrete(name = 'Condition', 
		breaks = c('msd','spc','sgl','wsh'),
		limits = c('msd','spc','sgl','wsh'),
		labels = c('Massed\nSort','Spaced\nSort','Single\nSort','Worksheet')) +
	  geom_errorbar(aes(ymin = lsm_tab$Estimate - lsm_tab$Standard.Error,
		ymax = lsm_tab$Estimate + lsm_tab$Standard.Error), 
		width=.2,position=position_dodge(.9)) +
	  coord_cartesian(ylim=c(0,.75))
  
  } else if (experiment == 3) {

  graph <- ggplot(data = lsm_tab, aes(x = cond_e2, y = Estimate)) + geom_bar(stat = 'identity') +
	theme(plot.title = element_text(size=32, face="bold", vjust=2),
		axis.title.y = element_text(size=26,vjust = 1.5),
		axis.text.y = element_text(size=18,color='black'),
		axis.title.x = element_text(size=26,vjust = -.5),
		axis.text.x = element_text(size = 18,color='black'),
		panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())+
	ggtitle('Mastery - Free Response Accuracy') +
	scale_y_continuous(limits=c(0,1.00),breaks=seq(0, 1, 0.05), name = 'Proportion Correct') + 
	scale_x_discrete(name = 'Condition', 
		breaks = c('bsd','psd','nsd','wsh'),
		limits = c('bsd','psd','nsd','wsh'),
		labels = c('Both Cards','Positive Card','No Cards','Worksheet')) +
	geom_errorbar(aes(ymin = lsm_tab$Estimate - lsm_tab$Standard.Error,
		ymax = lsm_tab$Estimate + lsm_tab$Standard.Error), 
		width=.2,position=position_dodge(.9)) +
	coord_cartesian(ylim=c(.0,.25)) 

  } else if (experiment == 4) {

  	graph <- ggplot(data = lsm_tab, aes(x = cond_e2, y = Estimate)) + geom_bar(stat = 'identity') +
	theme(plot.title = element_text(size=32, face="bold", vjust=2),
		axis.title.y = element_text(size=26,vjust = 1.5),
		axis.text.y = element_text(size=18,color='black'),
		axis.title.x = element_text(size=26,vjust = -.5),
		axis.text.x = element_text(size = 18,color='black'),
		panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())+
	ggtitle('Transfer - Free Response Accuracy') +
	scale_y_continuous(limits=c(0,1.00),breaks=seq(0, 1, 0.05), name = 'Proportion Correct') + 
	scale_x_discrete(name = 'Condition', 
		breaks = c('bsd','psd','nsd','wsh'),
		limits = c('bsd','psd','nsd','wsh'),
		labels = c('Both Cards','Positive Card','No Cards','Worksheet')) +
	geom_errorbar(aes(ymin = lsm_tab$Estimate - lsm_tab$Standard.Error,
		ymax = lsm_tab$Estimate + lsm_tab$Standard.Error), 
		width=.2,position=position_dodge(.9)) +
	coord_cartesian(ylim=c(0,.25)) 

  } else if (experiment == 5) {

  	graph <- ggplot(data = lsm_tab, aes(x = cond_e3, y = Estimate)) + geom_bar(stat = 'identity') +
	theme(plot.title = element_text(size=32, face="bold", vjust=2),
		axis.title.y = element_text(size=26,vjust = 1.5),
		axis.text.y = element_text(size=18,color='black'),
		axis.title.x = element_text(size=26,vjust = -.5),
		axis.text.x = element_text(size = 18,color='black'),
		panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())+
	ggtitle('Mastery - Free Response Accuracy') +
	scale_y_continuous(limits=c(0,1.00),breaks=seq(0, 1, 0.1), name = 'Proportion Correct') + 
	scale_x_discrete(name = 'Condition', 
		breaks = c('cla','ind','nof','wsh'),
		limits = c('cla','ind','nof','wsh'),
		labels = c('Classroom\nFeedback','Individual\nFeedback','No Feedback\nProvided','Worksheet')) +
	geom_errorbar(aes(ymin = lsm_tab$Estimate - lsm_tab$Standard.Error,
		ymax = lsm_tab$Estimate + lsm_tab$Standard.Error), 
		width=.2,position=position_dodge(.9)) +
	coord_cartesian(ylim=c(.0,.50)) 

  } else if (experiment == 6) {

  	graph <- ggplot(data = lsm_tab, aes(x = cond_e3, y = Estimate)) + geom_bar(stat = 'identity') +
	theme(plot.title = element_text(size=32, face="bold", vjust=2),
		axis.title.y = element_text(size=26,vjust = 1.5),
		axis.text.y = element_text(size=18,color='black'),
		axis.title.x = element_text(size=26,vjust = -.5),
		axis.text.x = element_text(size = 18,color='black'),
		panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())+
	ggtitle('Transfer - Free Response Accuracy') +
	scale_y_continuous(limits=c(0,1.00),breaks=seq(0, 1, 0.1), name = 'Proportion Correct') + 
	scale_x_discrete(name = 'Condition', 
		breaks = c('cla','ind','nof','wsh'),
		limits = c('cla','ind','nof','wsh'),
		labels = c('Classroom\nFeedback','Individual\nFeedback','No Feedback\nProvided','Worksheet')) +
	geom_errorbar(aes(ymin = lsm_tab$Estimate - lsm_tab$Standard.Error,
		ymax = lsm_tab$Estimate + lsm_tab$Standard.Error), 
		width=.2,position=position_dodge(.9)) +
	coord_cartesian(ylim=c(0,.25)) 
  
  }

  return(graph)

}
