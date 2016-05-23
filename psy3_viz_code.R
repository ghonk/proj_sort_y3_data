library(ggplot2)
library(tables)
library(lmerTest)

setwd('C://Users//garre//Dropbox//aa projects//materials//SORT//year 3//year 3 data')

datafull <- read.csv('psy3 master.csv')
datafull <- subset(datafull, subset = (sped == 0))

e1_trandata <- subset(datafull, subset = (e1trans == 1))
e1_mastdata <- subset(datafull, subset = (e1mast == 1))

e2_trandata <- subset(datafull, subset = (e2trans == 1))
e2_mastdata <- subset(datafull, subset = (e2mast == 1))

e3_trandata <- subset(datafull, subset = (e3trans == 1))
e3_mastdata <- subset(datafull, subset = (e3mast == 1))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # E1 condition referents
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
e1_trandata.wsh <- within(e1_trandata, cond_e1 <- relevel(cond_e1, ref = 'wsh'))
e1_trandata.spc <- within(e1_trandata, cond_e1 <- relevel(cond_e1, ref = 'spc'))
e1_trandata.sgl <- within(e1_trandata, cond_e1 <- relevel(cond_e1, ref = 'sgl'))
e1_trandata.msd <- within(e1_trandata, cond_e1 <- relevel(cond_e1, ref = 'msd'))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
e1_mastdata.wsh <- within(e1_mastdata, cond_e1 <- relevel(cond_e1, ref = 'wsh'))
e1_mastdata.spc <- within(e1_mastdata, cond_e1 <- relevel(cond_e1, ref = 'spc'))
e1_mastdata.sgl <- within(e1_mastdata, cond_e1 <- relevel(cond_e1, ref = 'sgl'))
e1_mastdata.msd <- within(e1_mastdata, cond_e1 <- relevel(cond_e1, ref = 'msd'))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # E2 condition referents 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
e2_trandata.wsh <- within(e2_trandata, cond_e2 <- relevel(cond_e2, ref = 'wsh'))
e2_trandata.nsd <- within(e2_trandata, cond_e2 <- relevel(cond_e2, ref = 'nsd'))
e2_trandata.psd <- within(e2_trandata, cond_e2 <- relevel(cond_e2, ref = 'psd'))
e2_trandata.bsd <- within(e2_trandata, cond_e2 <- relevel(cond_e2, ref = 'bsd'))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
e2_mastdata.wsh <- within(e2_mastdata, cond_e2 <- relevel(cond_e2, ref = 'wsh'))
e2_mastdata.nsd <- within(e2_mastdata, cond_e2 <- relevel(cond_e2, ref = 'nsd'))
e2_mastdata.psd <- within(e2_mastdata, cond_e2 <- relevel(cond_e2, ref = 'psd'))
e2_mastdata.bsd <- within(e2_mastdata, cond_e2 <- relevel(cond_e2, ref = 'bsd'))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # E3 condition referents 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
e3_trandata.wsh <- within(e3_trandata, cond_e3 <- relevel(cond_e3, ref = 'wsh'))
e3_trandata.cla <- within(e3_trandata, cond_e3 <- relevel(cond_e3, ref = 'cla'))
e3_trandata.nof <- within(e3_trandata, cond_e3 <- relevel(cond_e3, ref = 'nof'))
e3_trandata.ind <- within(e3_trandata, cond_e3 <- relevel(cond_e3, ref = 'ind'))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
e3_mastdata.wsh <- within(e3_mastdata, cond_e3 <- relevel(cond_e3, ref = 'wsh'))
e3_mastdata.cla <- within(e3_mastdata, cond_e3 <- relevel(cond_e3, ref = 'cla'))
e3_mastdata.nof <- within(e3_mastdata, cond_e3 <- relevel(cond_e3, ref = 'nof'))
e3_mastdata.ind <- within(e3_mastdata, cond_e3 <- relevel(cond_e3, ref = 'ind'))
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # MASTERY GRAPHS
perf.model <- lmer(e1_m_corr ~ cond_e1 + (1|z_cellstest) + (1|teacher/tp), data = e1_mastdata.wsh, REML = FALSE)
lsm_tab <- lsmeans(perf.model, test.effs=NULL, method.grad='simple')
lsm_tab <- data.frame(lsm_tab$lsmeans.table)

e1mast <- ggplot(data = lsm_tab, aes(x = cond_e1, y = Estimate)) + geom_bar(stat = 'identity') +
	theme(plot.title = element_text(size=32, face="bold", vjust=4),
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
		labels = c('Massed Sort','Spaced Sort','Single Sort','Worksheet')) +
	geom_errorbar(aes(ymin = lsm_tab$Estimate - lsm_tab$Standard.Error,
		ymax = lsm_tab$Estimate + lsm_tab$Standard.Error), 
		width=.2,position=position_dodge(.9)) +
	coord_cartesian(ylim=c(.0,.75)) 


perf.model <- lmer(e2_m_corr ~ cond_e2 + (1|z_cellstest) + (1|teacher/tp), data = e2_mastdata.wsh, REML = FALSE)
lsm_tab <- lsmeans(perf.model, test.effs=NULL, method.grad='simple')
lsm_tab <- data.frame(lsm_tab$lsmeans.table)

e2mast <- ggplot(data = lsm_tab, aes(x = cond_e2, y = Estimate)) + geom_bar(stat = 'identity') +
	theme(plot.title = element_text(size=32, face="bold", vjust=2),
		axis.title.y = element_text(size=26,vjust = 1.5),
		axis.text.y = element_text(size=18,color='black'),
		axis.title.x = element_text(size=26,vjust = -.5),
		axis.text.x = element_text(size = 18,color='black'),
		panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())+
	ggtitle('Mastery - Assessment Accuracy') +
	scale_y_continuous(limits=c(0,1.00),breaks=seq(0, 1, 0.1), name = 'Proportion Correct') + 
	scale_x_discrete(name = 'Condition', 
		breaks = c('bsd','psd','nsd','wsh'),
		limits = c('bsd','psd','nsd','wsh'),
		labels = c('Both Cards','Positive Card','No Cards','Worksheet')) +
	geom_errorbar(aes(ymin = lsm_tab$Estimate - lsm_tab$Standard.Error,
		ymax = lsm_tab$Estimate + lsm_tab$Standard.Error), 
		width=.2,position=position_dodge(.9)) +
	coord_cartesian(ylim=c(.0,.75)) 
	


perf.model <- lmer(e3_m_corr ~ cond_e3 + (1|z_cellstest) + (1|teacher/tp), data = e3_mastdata.wsh, REML = FALSE)
lsm_tab <- lsmeans(perf.model, test.effs=NULL, method.grad='simple')
lsm_tab <- data.frame(lsm_tab$lsmeans.table)

e3mast <- ggplot(data = lsm_tab, aes(x = cond_e3, y = Estimate)) + geom_bar(stat = 'identity') +
	theme(plot.title = element_text(size=32, face="bold", vjust=2),
		axis.title.y = element_text(size=26,vjust = 1.5),
		axis.text.y = element_text(size=18,color='black'),
		axis.title.x = element_text(size=26,vjust = -.5),
		axis.text.x = element_text(size = 18,color='black'),
		panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())+
	ggtitle('Mastery - Assessment Accuracy') +
	scale_y_continuous(limits=c(0,1.00),breaks=seq(0, 1, 0.1), name = 'Proportion Correct') + 
	scale_x_discrete(name = 'Condition', 
		breaks = c('cla','ind','nof','wsh'),
		limits = c('cla','ind','nof','wsh'),
		labels = c('Classroom','Individual','No Feedback','Worksheet')) +
	geom_errorbar(aes(ymin = lsm_tab$Estimate - lsm_tab$Standard.Error,
		ymax = lsm_tab$Estimate + lsm_tab$Standard.Error), 
		width=.2,position=position_dodge(.9)) +
	coord_cartesian(ylim=c(.0,.75)) 





# # # # TRANSFER GRAPHS 

perf.model <- lmer(e1_t_corr ~ cond_e1 + (1|z_cellstest) + (1|teacher/tp), data = e1_trandata.wsh, REML = FALSE)
lsm_tab <- lsmeans(perf.model, test.effs=NULL, method.grad='simple')
lsm_tab <- data.frame(lsm_tab$lsmeans.table)

e1tran <- ggplot(data = lsm_tab, aes(x = cond_e1, y = Estimate)) + geom_bar(stat = 'identity') +
	theme(plot.title = element_text(size=32, face="bold", vjust=2),
		axis.title.y = element_text(size=26,vjust = 1.5),
		axis.text.y = element_text(size=18,color='black'),
		axis.title.x = element_text(size=26,vjust = -.5),
		axis.text.x = element_text(size = 18,color='black'),
		panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())+
	ggtitle('Transfer - Assessment Accuracy') +
	scale_y_continuous(limits=c(0,1.00),breaks=seq(0, 1, 0.1), name = 'Proportion Correct') + 
	scale_x_discrete(name = 'Condition', 
		breaks = c('msd','spc','sgl','wsh'),
		limits = c('msd','spc','sgl','wsh'),
		labels = c('Massed Sort','Spaced Sort','Single Sort','Worksheet')) +
	geom_errorbar(aes(ymin = lsm_tab$Estimate - lsm_tab$Standard.Error,
		ymax = lsm_tab$Estimate + lsm_tab$Standard.Error), 
		width=.2,position=position_dodge(.9)) +
	coord_cartesian(ylim=c(0,.75)) 


perf.model <- lmer(e2_t_corr ~ cond_e2 + (1|z_cellstest) + (1|teacher/tp), data = e2_trandata.wsh, REML = FALSE)
lsm_tab <- lsmeans(perf.model, test.effs=NULL, method.grad='simple')
lsm_tab <- data.frame(lsm_tab$lsmeans.table)

e2tran <- ggplot(data = lsm_tab, aes(x = cond_e2, y = Estimate)) + geom_bar(stat = 'identity') +
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
	


perf.model <- lmer(e3t_mc_1_p ~ cond_e3 + (1|z_cellstest) + (1|teacher/tp), data = e3_mastdata.wsh, REML = FALSE)
lsm_tab <- lsmeans(perf.model, test.effs=NULL, method.grad='simple')
lsm_tab <- data.frame(lsm_tab$lsmeans.table)

e3tran <- ggplot(data = lsm_tab, aes(x = cond_e3, y = Estimate)) + geom_bar(stat = 'identity') +
	theme(plot.title = element_text(size=32, face="bold", vjust=2),
		axis.title.y = element_text(size=26,vjust = 1.5),
		axis.text.y = element_text(size=18,color='black'),
		axis.title.x = element_text(size=26,vjust = -.5),
		axis.text.x = element_text(size = 18,color='black'),
		panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())+
	ggtitle('Transfer - Assessment Accuracy') +
	scale_y_continuous(limits=c(0,1.00),breaks=seq(0, 1, 0.1), name = 'Proportion Correct') + 
	scale_x_discrete(name = 'Condition', 
		breaks = c('cla','ind','nof','wsh'),
		limits = c('cla','ind','nof','wsh'),
		labels = c('Classroom','Individual','No Feedback','Worksheet')) +
	geom_errorbar(aes(ymin = lsm_tab$Estimate - lsm_tab$Standard.Error,
		ymax = lsm_tab$Estimate + lsm_tab$Standard.Error), 
		width=.2,position=position_dodge(.9)) +
	coord_cartesian(ylim=c(0,.75)) 


