# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # PROJECT SORT YEAR THREE IMPUTATION AND ANALYSIS SCRIPT  # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # setwd('C://Users//garre//Dropbox//aa projects//materials//SORT//year 3//year 3 data')


# # load the helper functions
source('psy3helperfuncs.r')


# # # # record time, set text file output for history
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
sink(file = "projectSortY3Analysis.txt", append = FALSE, 
  type = c("output", "message"),split = TRUE)
rundate = Sys.time()
print(rundate)
printspace(1,0)


# # # # load data
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
datafull <- read.csv('psy3master.csv')
datafull <- subset(datafull, subset = (sped == 0))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # THIS SECTION BREAKS THE IMPUTATION; SERIOUS WTF MOMENT HERE
# there was a data skew issue with the unit test scores
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# this is what I did to fix it:
datafull$z_cellstest <- scale(log10((max(datafull$raw_cells_test, na.rm = TRUE) + 1) 
  - datafull$raw_cells_test), center = TRUE, scale = TRUE)
datafull$z_evotest <- scale(log10((max(datafull$raw_evo_test, na.rm = TRUE) + 1) 
  - datafull$raw_evo_test), center = TRUE, scale = TRUE)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # make dataframe for each experiment
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # E1
e1_mast_data <- subset(dplyr::select(datafull, pid, teacher, period, tp, z_cellstest, 
  z_evotest, cond_e1, contains('e1m')), subset = (e1mast == 1))
e1_tran_data <- subset(dplyr::select(datafull, pid, teacher, period, tp, z_cellstest, 
  z_evotest, cond_e1, contains('e1t')), subset = (e1trans == 1))

# # E2
e2_mast_data <- subset(dplyr::select(datafull, pid, teacher, period, tp, z_cellstest, 
  z_evotest, cond_e2, contains('e2m')), subset = (e2mast == 1))
e2_tran_data <- subset(dplyr::select(datafull, pid, teacher, period, tp, z_cellstest, 
  z_evotest, cond_e2, contains('e2t')), subset = (e2trans == 1))

# # E3
e3_mast_data <- subset(dplyr::select(datafull, pid, teacher, period, tp, z_cellstest, 
  z_evotest, cond_e3, contains('e3m')), subset = (e3mast == 1))
e3_tran_data <- subset(dplyr::select(datafull, pid, teacher, period, tp, z_cellstest, 
  z_evotest, cond_e3, contains('e3t')), subset = (e3trans == 1))

# # full list of datasets to be imputed
imp_data_list <- list(e1_mast_data, e1_tran_data, e2_mast_data, e2_tran_data,
  e3_mast_data, e3_tran_data)


# # # # save image for imputation on skynet
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
save.image('data_for_imp.rdata')


# # # # iterate through datasets and impute values 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
for(dataset in 1:length(imp_data_list)){
  # # create labels
  objlabel = paste0("impobj_e",dataset)
  datasetlabel = paste0("impdata_e",dataset)
  # # run imp and save imputation objects
  tempcom = paste0("impobj_e",dataset," <- iterimp(imp_data_list[[dataset]],
					60,datasetlabel)")
  eval(parse(text=tempcom))
  # # save datasets as active dataframes
  tempcom = paste0(datasetlabel, " <- mi.completed(", objlabel, ")")
  eval(parse(text=tempcom))
}

save.image("y3_imputed_data.rdata")


# # # # load imputed data and create important variables
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
load("y3_imputed_data.rdata")


complete_datalist <- list(impdata_e1, impdata_e2, impdata_e3,
  impdata_e4, impdata_e5, impdata_e6)

# # incorporate sort data into the main datasets




# # # # create proportion correct scores from ordinal vars
complete_datalist <- convert_all_vars(complete_datalist)

# # # # run lmers on total proportion correct
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
complete_proportion_correct <- 
  iter_lmer_multi(complete_datalist,'tot_prop_correct')

# # # # run lmers on mc pts correct <<<WE MIGHT WANT THIS TO BE ORDINAL
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
mc_correct <- iter_lmer_multi(complete_datalist,'mc_pts_correct')

# # # # run lmers on fr proportion correct
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
fr_correct <- iter_lmer_multi(complete_datalist,'fr_prop_correct')


# # # # run lmers on appropriate individual variables
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # e1
# mastery
e1_mast_fr1 <- iter_lmer(complete_datalist[[1]],'e1m_fr_1',1)
e1_mast_fr2 <- iter_lmer(complete_datalist[[1]],'e1m_fr_2',1)
# # e1
# transfer

# # e2 
# mastery 
e2_mast_fr1 <- iter_lmer(complete_datalist[[3]],'e2m_fr_1',3)
e2_mast_fr2 <- iter_lmer(complete_datalist[[3]],'e2m_fr_2',3)
# # e2
# transfer

# # e3
# mastery
e3_mast_fr1 <- iter_lmer(complete_datalist[[5]],'e3m_fr_1',5)
e3_mast_fr2 <- iter_lmer(complete_datalist[[5]],'e3m_fr_2',5)
# # e3
# transfer

# # # # run clmms on appropriate individual variables
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # e1
# mastery
e1_mast_mc1 <- iter_clmm(complete_datalist[[1]],'e1m_mc_1',1)
e1_mast_mc2 <- iter_clmm(complete_datalist[[1]],'e1m_mc_2',1)
# # e1
# transfer
e1_tran_fr1 <- iter_lmer(complete_datalist[[2]],'e1t_fr_1',2) # Needs to be logistic
e1_tran_fr2 <- iter_clmm(complete_datalist[[2]],'e1t_fr_2',2)
e1_tran_mc1 <- iter_clmm(complete_datalist[[2]],'e1t_mc_1',2)
e1_tran_mc2 <- iter_clmm(complete_datalist[[2]],'e1t_mc_2',2)

# # e2 
# mastery
e2_mast_mc1 <- iter_clmm(complete_datalist[[3]],'e2m_mc_1',3)
e2_mast_mc2 <- iter_clmm(complete_datalist[[3]],'e2m_mc_2',3)
# # e2
# transfer
e2_tran_fr1  <- iter_clmm(complete_datalist[[4]],'e2t_fr_1',4)
e2_tran_fr2a <- iter_clmm(complete_datalist[[4]],'e2t_fr_2a',4)
e2_tran_fr2b <- iter_clmm(complete_datalist[[4]],'e2t_fr_2b',4)

e2_tran_mc1 <- iter_clmm(complete_datalist[[4]],'e2t_mc_1',4)
e2_tran_mc2 <- iter_clmm(complete_datalist[[4]],'e2t_mc_2',4)

# # e3
# mastery
e3_mast_mc1 <- iter_lmer(complete_datalist[[5]],'e3m_mc_1',5) # Needs to be logistic
e3_mast_mc2 <- iter_clmm(complete_datalist[[5]],'e3m_mc_2',5) 

# # e3
# transfer
e3_tran_fr1 <- iter_clmm(complete_datalist[[6]],'e3t_fr_1',6)
e3_tran_fr2 <- iter_clmm(complete_datalist[[6]],'e3t_fr_2',6)

e3_tran_mc1 <- iter_clmm(complete_datalist[[6]],'e3t_mc_1',6)

# # # ref for loading the final data
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
load('finaldata_allps.rdata')


# # # code to split participants based on some var
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# for(exp in 1:length(complete_datalist)){
#   for(df in 1:length(exp)){
#     complete_datalist[[exp]][[df]] <- subset(complete_datalist[[exp]][[df]],
#                                              complete_datalist[[exp]][[df]]$z_cellstest >= -.30)
#   }
# }

# for(exp in 1:length(complete_datalist)){
#   for(df in 1:length(exp)){
#     complete_datalist[[exp]][[df]] <- subset(complete_datalist[[exp]][[df]],
#                                              complete_datalist[[exp]][[df]]$z_cellstest <= .885323)
#   }
# }


# # # add number of sorts (or anything else) back into all datasets
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
complete_datalist <- add_variable(complete_datalist)


# # # get adjusted means
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
adjusted_means <- list(list(),list(),list(),list(),list(),list())

for (exp in 1:length(complete_datalist)){  
  for (dataset in 1:length(complete_datalist[[1]])){
    if (dataset == 1){
      adjusted_means[[exp]] <- 
        calc_adjusted_means(complete_datalist, 'fr_prop_correct', exp, dataset)
    } else {
      adjusted_means[[exp]] <-
        rbind(adjusted_means[[exp]],
          calc_adjusted_means(complete_datalist, 'fr_prop_correct', exp, dataset))
    }   
  }   
  row.names(adjusted_means[[exp]]) <- NULL
}


# # # stop (if) recording output
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
sink()	
closeAllConnections()