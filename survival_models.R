library(ggplot2)
library(scales)
library(dplyr)
library(survival)
library(spduration)
options(scipen=999)

blm <- read.csv("~/Downloads/blm_weekly_first_gf-2.csv")

blm <- blm[,c("Total.population_x","Protests.under.50km.past.week_x","Protests.same.state.past.week...50km_x","Protests.past.week.not.same.state_x","Black.or.African.American_x","White_x","Median.age..years._x","HBCU_x","Total.housing.units_x","Weeks.since.GF","State_x","NAME10_x","Protest._x","Not.immune","Time.since.first.event","Temp.spatial.first.event..all.","Temp.spatial.last.event..all.","Temp.spatial.first.event...50km.","Temp.spatial.last.event...50km.","Temp.spatial.first.event..state..50km.","Temp.spatial.last.event..state..50km.","Temp.spatial.first.event..state..50km.","Temp.spatial.first.event..non.state.","Temp.spatial.last.event..state..50km.","Temp.spatial.last.event..non.state.","Time.to.first.BLM.protest.since.GF")]


blm$start.date <- blm$Weeks.since.GF-1
blm$end.date <- blm$Weeks.since.GF
surv_model <- coxph(Surv(start.date,end.date,Protest._x) ~ Total.population_x + Protests.under.50km.past.week_x + Protests.same.state.past.week...50km_x + Protests.past.week.not.same.state_x + 
               Black.or.African.American_x + White_x +
               Median.age..years._x + HBCU_x + Total.housing.units_x, data = blm[blm$Not.immune==1,])
summary(surv_model)


surv_model <-  coxph(Surv(start.date,end.date,Protest._x) ~ Total.population_x*Protests.under.50km.past.week_x + Total.population_x*Protests.same.state.past.week...50km_x +
               Total.population_x*Protests.past.week.not.same.state_x + 
               Black.or.African.American_x + White_x +
               Median.age..years._x + HBCU_x + Total.housing.units_x, data = blm)
summary(surv_model)


surv_model <- coxph(Surv(start.date,end.date,Protest._x) ~ Total.population_x + Protests.under.50km.past.week_x + Protests.same.state.past.week...50km_x + Protests.past.week.not.same.state_x + 
               Black.or.African.American_x + White_x +
               Median.age..years._x + HBCU_x + Total.housing.units_x, data = blm)
summary(surv_model)


surv_model <-  coxph(Surv(start.date,end.date,Protest._x) ~ Total.population_x*Protests.under.50km.past.week_x + Total.population_x*Protests.same.state.past.week...50km_x +
               Total.population_x*Protests.past.week.not.same.state_x + 
               Black.or.African.American_x + White_x +
               Median.age..years._x + HBCU_x + Total.housing.units_x, data = blm)
summary(surv_model)

surv_model <- coxph(Surv(start.date,end.date,Protest._x) ~ Temp.spatial.last.event...50km.+Temp.spatial.last.event..state..50km.+Temp.spatial.last.event..non.state.+log(Total.population_x)+Black.or.African.American_x + White_x+Median.age..years._x, data = blm[blm$Not.immune==1,])
summary(surv_model)

surv_model <- coxph(Surv(start.date,end.date,Protest._x) ~ Temp.spatial.first.event...50km.+Temp.spatial.first.event..state..50km.+Temp.spatial.first.event..non.state.+log(Total.population_x)+Black.or.African.American_x + White_x+Median.age..years._x, data = blm[blm$Not.immune==1,])
summary(surv_model)



surv_model <- coxph(Surv(start.date,end.date,Protest._x) ~ Temp.spatial.first.event..all.+
													log(Total.population_x)+
													I(Temp.spatial.first.event..all.*log(Total.population_x))+
													Black.or.African.American_x+ 
													White_x+Median.age..years._x, data = blm[blm$Not.immune==1,])
summary(surv_model)


surv_model <- coxph(Surv(start.date,end.date,Protest._x) ~ Temp.spatial.last.event..all.+
													log(Total.population_x)+
													I(Temp.spatial.last.event..all.*log(Total.population_x))+
													Black.or.African.American_x+ 
													White_x+Median.age..years._x, data = blm[blm$Not.immune==1,])
summary(surv_model)



surv_model <- coxph(Surv(start.date,end.date,Protest._x) ~ Temp.spatial.first.event...50km.+
													Temp.spatial.first.event..state..50km.+
													Temp.spatial.first.event..non.state.+
													log(Total.population_x)+
													Black.or.African.American_x+ 
													White_x+Median.age..years._x, data = blm[blm$Not.immune==1,])
summary(surv_model)

surv_model <- coxph(Surv(start.date,end.date,Protest._x) ~ Temp.spatial.first.event...50km.+
													Temp.spatial.first.event..state..50km.+
													Temp.spatial.first.event..non.state.+
													log(Total.population_x)+
													I(Temp.spatial.first.event...50km.*log(Total.population_x))+
													I(Temp.spatial.first.event..state..50km.*log(Total.population_x))+
													I(Temp.spatial.first.event..non.state.*log(Total.population_x))+
													Black.or.African.American_x+
													White_x+Median.age..years._x, data = blm[blm$Not.immune==1,])
summary(surv_model)


surv_model <- coxph(Surv(start.date,end.date,Protest._x) ~ Temp.spatial.first.event...50km.+
													Temp.spatial.first.event..state..50km.+
													Temp.spatial.first.event..non.state.+
													log(Total.population_x)+
													I(Temp.spatial.first.event...50km.*log(Total.population_x))+
													I(Temp.spatial.first.event..state..50km.*log(Total.population_x))+
													I(Temp.spatial.first.event..non.state.*log(Total.population_x))+
													Black.or.African.American_x+
													White_x+Median.age..years._x, data = blm)
summary(surv_model)




surv_model <- coxph(Surv(start.date,end.date,Protest._x) ~ Temp.spatial.last.event...50km.+
													Temp.spatial.last.event..state..50km.+
													Temp.spatial.last.event..non.state.+
													log(Total.population_x)+
													Black.or.African.American_x+ 
													White_x+Median.age..years._x, data = blm[blm$Not.immune==1,])
summary(surv_model)



surv_model <- coxph(Surv(start.date,end.date,Protest._x) ~ Temp.spatial.last.event...50km.+
													Temp.spatial.last.event..state..50km.+
													Temp.spatial.last.event..non.state.+
													log(Total.population_x)+
													I(Temp.spatial.last.event...50km.*log(Total.population_x))+
													I(Temp.spatial.last.event..state..50km.*log(Total.population_x))+
													I(Temp.spatial.last.event..non.state.*log(Total.population_x))+
													Black.or.African.American_x+ 
													White_x+Median.age..years._x, data = blm[blm$Not.immune==1,])
summary(surv_model)






surv_model <- coxph(Surv(start.date,end.date,Protest._x) ~ Temp.spatial.first.event...50km.+Temp.spatial.first.event..state..50km.+Temp.spatial.first.event..non.state.+Temp.spatial.last.event...50km.+Temp.spatial.last.event..state..50km.+Temp.spatial.last.event..non.state.+log(Total.population_x)+Black.or.African.American_x + White_x+Median.age..years._x, data = blm[blm$Not.immune==1,])
summary(surv_model)


blm$year_synth <- base::as.Date(paste("01-01-",as.character(2000+blm$Weeks.since.GF),sep=""),"%m-%d-%Y")

blm <- add_duration(blm, y="Protest._x", unitID = "NAME10_x", tID = "year_synth", freq = "year", ongoing = FALSE)

blm <- blm[is.na(blm$White_x)==FALSE,]

weib_model <- spdur(duration ~  Temp.spatial.first.event...50km.+
													Temp.spatial.first.event..state..50km.+
													Temp.spatial.first.event..non.state.+
													log(Total.population_x)+
													I(Temp.spatial.first.event...50km.*log(Total.population_x))+
													I(Temp.spatial.first.event..state..50km.*log(Total.population_x))+
													I(Temp.spatial.first.event..non.state.*log(Total.population_x))+Black.or.African.American_x+ 
													White_x+Median.age..years._x, atrisk ~ 1 ,data = blm, distr = "weibull")
		 
loglog_model <- spdur(duration ~  Temp.spatial.first.event...50km.+
													Temp.spatial.first.event..state..50km.+
													Temp.spatial.first.event..non.state.+
													log(Total.population_x)+
													I(Temp.spatial.first.event...50km.*log(Total.population_x))+
													I(Temp.spatial.first.event..state..50km.*log(Total.population_x))+
													I(Temp.spatial.first.event..non.state.*log(Total.population_x)), atrisk ~ 1 ,data = blm, distr = "loglog")
													
													
weib_model <- spdur(duration ~  Black.or.African.American_x+Protests.under.50km.past.week_x+Protests.same.state.past.week...50km_x+log(Total.population_x), atrisk ~ 1 ,data = blm, distr = "weibull")

weib_model <- spdur(duration ~  Black.or.African.American_x+Protests.under.50km.past.week_x+Protests.same.state.past.week...50km_x+Protests.past.week.not.same.state_x+log(Total.population_x)+Median.age..years._x, atrisk ~ 1 + log(Total.population_x) +White_x,data = blm, distr = "weibull")

summary(weib_model)

weib_model <- spdur(duration ~  Black.or.African.American_x+Protests.under.50km.past.week_x+Protests.same.state.past.week...50km_x+Protests.past.week.not.same.state_x+log(Total.population_x)+Median.age..years._x, atrisk ~ 1 + log(Total.population_x) +White_x,data = blm, distr = "loglog")

summary(weib_model)



weib_model <- spdur(duration ~  Black.or.African.American_x+Protests.under.50km.past.week_x*log(Total.population_x)+Protests.same.state.past.week...50km_x*log(Total.population_x)+Protests.past.week.not.same.state_x*log(Total.population_x)+Median.age..years._x, atrisk ~ 1 + log(Total.population_x) +White_x,data = blm, distr = "weibull")

summary(weib_model)

weib_model <- spdur(duration ~  Black.or.African.American_x+Protests.under.50km.past.week_x*log(Total.population_x)+Protests.same.state.past.week...50km_x*log(Total.population_x)+Protests.past.week.not.same.state_x*log(Total.population_x)+Median.age..years._x, atrisk ~ 1 + log(Total.population_x) +White_x,data = blm, distr = "loglog")

summary(weib_model)




# SURVIVAL MODEL

surv_model = coxph(Surv(Weeks.since.GF, Protest._x) - Total.population_x*Protests.under.50km.past.week_x + Total.population_x*Protests.same.state.past.week...50km_x +
               Total.population_x*Protests.past.week.not.same.state_x + 
               Black.or.African.American_x + White_x +
               Median.age..years._x + HBCU_x + Total.housing.units_x, data = blm)
summary(surv_model)

surv_model = coxph(Surv(Weeks.since.GF, Protest._x) - 1, blm)
summary(surv_model)







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
