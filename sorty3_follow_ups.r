
library(dplyr)
library(lavaan)
library(Hmisc)

setwd('C://Users//garre//Dropbox//aa projects//materials//SORT//year 3//year 3 data')
load('finaldata_allps.rdata')

# # # ISSUES
# what package to use for SEM
# problem of excluding those without sort
# 

# *) look at outliers a little closer

# *) add sort accuracy and other vars to total data
# # # add sort accuracy variable to all data sets 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
add_task_variables <- function(complete_datalist){
  require(dplyr)
  dir <- 'C://Users//garre//Dropbox//aa projects//materials//SORT//year 3//year 3 data//raw data'

  # e1
  df_e1_task <- read.csv(paste0(dir, '//y3_e1_task.csv'))
  df_e1_task <- df_e1_task[,c(1,7:19)] 
  
  for(exp in 1:2){
    for(ds in 1:length(complete_datalist[[exp]])){
      complete_datalist[[exp]][[ds]] <- 
        inner_join(complete_datalist[[exp]][[ds]], df_e1_task, by = 'pid')
    }
  }

  # e2
  df_e2_task <- read.csv(paste0(dir, '//y3_e2_task.csv'))
  df_e2_task <- df_e2_task[,c(1,7:12)] 

  for(exp in 3:4){
    for(ds in 1:length(complete_datalist[[exp]])){
      complete_datalist[[exp]][[ds]] <- 
        inner_join(complete_datalist[[exp]][[ds]], df_e2_task, by = 'pid')
    }
  }

  # e3
  df_e3_task <- read.csv(paste0(dir, '//y3_e3_task.csv'))
  df_e3_task <- df_e3_task[,c(1,7:12)] 
  
  for(exp in 5:6){
    for(ds in 1:length(complete_datalist[[exp]])){
      complete_datalist[[exp]][[ds]] <- 
        inner_join(complete_datalist[[exp]][[ds]], df_e3_task, by = 'pid')
    }
  }

  return(complete_datalist)

}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# 1) The E2 principle specifies a benefit to the group from variation which in 
# the cards is shown as the continuation of the group. This could result from 
# survival and/or reproduction but students might not be conceptualizing benefit
#  as reproduction (this is an evolution content specific definition), but just 
#  as survival. So by scoring for reproduction (not explicitly part of the 
#  	principle) we actually might not be targeting student use of the principle 
#  and instead outside information. Given this, I would recommend examining student 
#  scores without the reproduction criteria. These include removing E2 M FR1 
#  categories 10 and 11 and E2 M FR2 categories 7, 8, 13, and 14. 

# 2) If we want to attempt to prune a bit of potential noise from the mastery data
#  there is a category in E2 M FR 2 (category c9) which is student response to a 
#  dichotomous choice item. Due to random student choice, this could be adding noise
#   and can be removed and still retain their reasoning for their choice in C9 
#   (which is C10). In this way we would just score the more definitive category.

# 3) Can we look at what students did understand in mastery by condition?

# 4) Subsets of students with wsh/sort scores; mastery performance?


		
# 5) Mediation analyses?
sem_test_model <- 'E2sort_acc ~ a*cond_e2
           tot_prop_correct ~ b*E2sort_acc + c*cond_e2

           indirect := a*b
           direct   := c
           total    := c*(a*b)'  

fit <- sem(sem_test_model, data = df_test)

summary(fit, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)


# 6) how does worksheet accuracy correlate to performance?

task_acc_corr <- list(
  # worksheet / proportion correct correlation
  ws_corr = list( 
    
    e1m = mean(unlist(lapply(complete_datalist[[1]], function(x) {
      rcorr(x$tot_prop_correct, rowMeans(x[,28:32]))$r[1,2]
    }))),
    
    e1t = mean(unlist(lapply(complete_datalist[[2]], function(x) {
      rcorr(x$tot_prop_correct, rowMeans(x[,28:32]))$r[1,2]
    }))),
    
    e2m = mean(unlist(lapply(complete_datalist[[3]], function(x) {
      rcorr(x$tot_prop_correct, rowMeans(x[,22:25]))$r[1,2]
    }))),

    e2t = mean(unlist(lapply(complete_datalist[[4]], function(x) {
      rcorr(x$tot_prop_correct, rowMeans(x[,24:27]))$r[1,2]
    }))),

    e3m = mean(unlist(lapply(complete_datalist[[5]], function(x) {
      rcorr(x$tot_prop_correct, rowMeans(x[,22:25]))$r[1,2]
    }))),

    e3t = mean(unlist(lapply(complete_datalist[[6]], function(x) {
      rcorr(x$tot_prop_correct, rowMeans(x[,20:23]))$r[1,2]
    })))),
  
  # sort / proportion correct correlation
  so_corr = list( 
    
    e1m = mean(unlist(lapply(complete_datalist[[1]], function(x) {
      rcorr(x$tot_prop_correct, x$e1_sorts_corr)$r[1,2]
    }))),
    
    e1t = mean(unlist(lapply(complete_datalist[[2]], function(x) {
      rcorr(x$tot_prop_correct, x$e1_sorts_corr)$r[1,2]
    }))),
    
    e2m = mean(unlist(lapply(complete_datalist[[3]], function(x) {
      rcorr(x$tot_prop_correct, x$E2sort_acc)$r[1,2]
    }))),

    e2t = mean(unlist(lapply(complete_datalist[[4]], function(x) {
      rcorr(x$tot_prop_correct, x$E2sort_acc)$r[1,2]
    }))),

    e3m = mean(unlist(lapply(complete_datalist[[5]], function(x) {
      rcorr(x$tot_prop_correct, x$E3sort_acc)$r[1,2]
    }))),

    e3t = mean(unlist(lapply(complete_datalist[[6]], function(x) {
      rcorr(x$tot_prop_correct, x$E3sort_acc)$r[1,2]
    })))))


# 7) Redo contrasts in the analysis

