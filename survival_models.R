library(ggplot2)
library(scales)
library(dplyr)
library(survival)
library(spduration)
options(scipen=999)
library(simPH)
library(texreg)
library(xtable)

blm <- read.csv("~/Downloads/blm_weekly_first_gf-2.csv")

blm <- blm[,c("Total.population_x","Protests.under.50km.past.week_x","Protests.same.state.past.week...50km_x","Protests.past.week.not.same.state_x","Black.or.African.American_x","White_x","Median.age..years._x","HBCU_x","Total.housing.units_x","Weeks.since.GF","State_x","NAME10_x","Protest._x","Not.immune","Time.since.first.event","Temp.spatial.first.event..all.","Temp.spatial.last.event..all.","Temp.spatial.first.event...50km.","Temp.spatial.last.event...50km.","Temp.spatial.first.event..state..50km.","Temp.spatial.last.event..state..50km.","Temp.spatial.first.event..state..50km.","Temp.spatial.first.event..non.state.","Temp.spatial.last.event..state..50km.","Temp.spatial.last.event..non.state.","Time.to.first.BLM.protest.since.GF")]


blm$Total.population_x.log <- log(blm$Total.population_x)

blm$start.date <- blm$Weeks.since.GF-1
blm$end.date <- blm$Weeks.since.GF


surv_model.1.all <- coxph(Surv(start.date,end.date,Protest._x) ~ Temp.spatial.first.event..all.*Total.population_x.log+
													Black.or.African.American_x+ 
													White_x+Median.age..years._x, data = blm[blm$Not.immune==1,])
summary(surv_model.1.all)


surv_model.1.cat <- coxph(Surv(start.date,end.date,Protest._x) ~ Temp.spatial.first.event...50km.+
													Temp.spatial.first.event..state..50km.+
													Temp.spatial.first.event..non.state.+
													Total.population_x.log+
													Black.or.African.American_x+ 
													White_x+Median.age..years._x, data = blm[blm$Not.immune==1,])
summary(surv_model.1.cat)

source('~/Documents/git/Protest_Diffusion/surv_model_1_cat_plot.R', chdir = TRUE)

#######################################
#######################################

surv_model.2.cat <- coxph(Surv(start.date,end.date,Protest._x) ~ Temp.spatial.first.event...50km.*Total.population_x.log+
													Temp.spatial.first.event..state..50km.*Total.population_x.log+
													Temp.spatial.first.event..non.state.*Total.population_x.log+
													Black.or.African.American_x+
													White_x+Median.age..years._x, data = blm[blm$Not.immune==1,])
summary(surv_model.2.cat)

source('~/Documents/git/Protest_Diffusion/surv_model_2_cat_plot.R', chdir = TRUE)

surv_model.3.cat <- coxph(Surv(start.date,end.date,Protest._x) ~ Temp.spatial.first.event...50km.*Total.population_x.log+
													Temp.spatial.first.event..state..50km.*Total.population_x.log+
													Temp.spatial.first.event..non.state.*Total.population_x.log+
													Black.or.African.American_x+
													White_x+Median.age..years._x, data = blm)
summary(surv_model.3.cat)

source('~/Documents/git/Protest_Diffusion/surv_model_3_cat_plot.R', chdir = TRUE)


###################
#split-models
###################


blm$year_synth <- base::as.Date(paste("01-01-",as.character(2000+blm$Weeks.since.GF),sep=""),"%m-%d-%Y")

blm <- add_duration(blm, y="Protest._x", unitID = "NAME10_x", tID = "year_synth", freq = "year", ongoing = FALSE)

blm <- blm[is.na(blm$White_x)==FALSE,]

sp_weib_model <- spdur(duration ~  Temp.spatial.first.event...50km.*Total.population_x.log+
													Temp.spatial.first.event..state..50km.*Total.population_x.log+
													Temp.spatial.first.event..non.state.*Total.population_x.log+
													Black.or.African.American_x+ 
													White_x+Median.age..years._x, atrisk ~ Temp.spatial.first.event...50km.*Total.population_x.log+
													Temp.spatial.first.event..state..50km.*Total.population_x.log+
													Temp.spatial.first.event..non.state.*Total.population_x.log+
													Black.or.African.American_x+ 
													White_x+Median.age..years._x ,data = blm, distr = "weibull")

													


source('~/Documents/git/Protest_Diffusion/sp_weib_model_plot.R', chdir = TRUE)


 
sp_loglog_model <- spdur(duration ~  Temp.spatial.first.event...50km.*Total.population_x.log+
													Temp.spatial.first.event..state..50km.*Total.population_x.log+
													Temp.spatial.first.event..non.state.*Total.population_x.log+
													Black.or.African.American_x+ 
													White_x+Median.age..years._x, atrisk ~ Temp.spatial.first.event...50km.*Total.population_x.log+
													Temp.spatial.first.event..state..50km.*Total.population_x.log+
													Temp.spatial.first.event..non.state.*Total.population_x.log+
													Black.or.African.American_x+ 
													White_x+Median.age..years._x ,data = blm, distr = "loglog")





source('~/Documents/git/Protest_Diffusion/sp_loglog_model_plot.R', chdir = TRUE)






# ONSET GRAPH

protests <- blm[blm$Protest._x == 1,]

colors <- c("> 75k" = "#FF0000", "50k - 75k" = "#e31298", "25k - 50k" = "#660099", "< 25k" = "#0000FF")
days <- 30

ggplot() + 
  geom_freqpoly(data=protests[protests$Total.population_x > 75000,], aes(x=Time.to.first.BLM.protest.since.GF, color = '> 75k'), bins=days+1) +
  geom_freqpoly(data=protests[protests$Total.population_x < 75000 & protests$Total.population_x > 50000,], aes(x=Time.to.first.BLM.protest.since.GF, color = '50k - 75k'), bins=days+1) +
  geom_freqpoly(data=protests[protests$Total.population_x < 50000 & protests$Total.population_x > 25000,], aes(x=Time.to.first.BLM.protest.since.GF, color = '25k - 50k'), bins=days+1) +
  geom_freqpoly(data=protests[protests$Total.population_x < 25000,], aes(x=Time.to.first.BLM.protest.since.GF, color = '< 25k'), bins=days+1) +
  scale_x_continuous(limits=c(0,days)) +
  scale_color_manual(values = colors) +
  labs(x = 'Time to first protest (days)', y = 'Number of protests', title = paste('George Floyd protests US (first', as.character(days), 'days)', sep=' '), color = 'Population')

ggplot() + 
  geom_density(data=protests[protests$Total.population_x > 75000,], aes(x=Time.to.first.BLM.protest.since.GF, color = '> 75k')) +
  geom_density(data=protests[protests$Total.population_x < 75000 & protests$Total.population_x > 50000,], aes(x=Time.to.first.BLM.protest.since.GF, color = '50k - 75k')) +
  geom_density(data=protests[protests$Total.population_x < 50000 & protests$Total.population_x > 25000,], aes(x=Time.to.first.BLM.protest.since.GF, color = '25k - 50k')) +
  geom_density(data=protests[protests$Total.population_x < 25000,], aes(x=Time.to.first.BLM.protest.since.GF, color = '< 25k')) +
  scale_x_continuous(limits=c(0,days)) +
  scale_color_manual(values = colors) +
  labs(x = 'Time to first protest (days)', y = 'Density', title = paste('George Floyd protests US (first', as.character(days), 'days)', sep=' '), color = 'Population')



# RESULTS
sink('./Final model results/protest_results.txt')
summary(sp_eff_blm)
summary(sp_eff_right)
summary(sp_int_3)
summary(sp_int_4)
summary(sp_int_5)
summary(charac)
summary(charac_states)
summary(sp_eff_ch)
summary(sp_int_ch)
