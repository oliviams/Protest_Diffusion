

model.data <- model.frame(Surv(start.date,end.date,Protest._x) ~ Temp.spatial.first.event...50km.*Total.population_x.log+
													Temp.spatial.first.event..state..50km.*Total.population_x.log+
													Temp.spatial.first.event..non.state.*Total.population_x.log+
													Black.or.African.American_x+
													White_x+Median.age..years._x, data = blm[blm$Not.immune==1,])

plot.data <- model.data %>%
				summarise(Temp.spatial.first.event...50km.=mean(Temp.spatial.first.event...50km.),
													Temp.spatial.first.event..state..50km.= mean(Temp.spatial.first.event..state..50km.),
													Temp.spatial.first.event..non.state. = mean(Temp.spatial.first.event..non.state.),
													Total.population_x.log = quantile(Total.population_x.log,probs=0.15),
													Black.or.African.American_x = mean(Black.or.African.American_x),
													White_x = mean(White_x),
													Median.age..years._x = mean(Median.age..years._x))
					
			plot.list <- list()
				for(i in 1:100){
					plot.list[[i]] <- plot.data
				}
			plot.data <- bind_rows(plot.list)		
					plot.data$Temp.spatial.first.event...50km. <- seq(min(model.data$Temp.spatial.first.event...50km.),max(model.data$Temp.spatial.first.event...50km.),length=100)
					
			plot.data.1 <- plot.data


plot.data <- model.data %>%
				summarise(Temp.spatial.first.event...50km.=mean(Temp.spatial.first.event...50km.),
													Temp.spatial.first.event..state..50km.= mean(Temp.spatial.first.event..state..50km.),
													Temp.spatial.first.event..non.state. = mean(Temp.spatial.first.event..non.state.),
													Total.population_x.log = quantile(Total.population_x.log,probs=0.85),
													Black.or.African.American_x = mean(Black.or.African.American_x),
													White_x = mean(White_x),
													Median.age..years._x = mean(Median.age..years._x))
					
			plot.list <- list()
				for(i in 1:100){
					plot.list[[i]] <- plot.data
				}
			plot.data <- bind_rows(plot.list)		
					plot.data$Temp.spatial.first.event...50km. <- seq(min(model.data$Temp.spatial.first.event...50km.),max(model.data$Temp.spatial.first.event...50km.),length=100)
					
			plot.data.2 <- plot.data

			plot.data <- bind_rows(plot.data.1,plot.data.2)	

pred.model <- predict(surv_model.2.cat,type="risk",se.fit=TRUE,newdata=plot.data)

plot.data$fit <- pred.model$fit
plot.data$se.fit <- pred.model$se.fit
plot.data$hi95 <- plot.data$fit+1.96*plot.data$se.fit
plot.data$lo95 <- plot.data$fit-1.96*plot.data$se.fit


plot.data$Population <- as.factor(plot.data$Total.population_x.log)

p1 <- ggplot(data=plot.data,aes(y=fit,x=Temp.spatial.first.event...50km.,group=Population,colour=Population,fill=Population))+
		#geom_point()+
			geom_line()+
				geom_ribbon(aes(ymin=lo95,ymax=hi95),alpha=0.3,linetype=0)+
					scale_fill_discrete(labels = c("Low", "High"))+
					scale_colour_discrete(labels = c("Low", "High"))+
					 ylab("Risk")+ xlab("Distance to first event within 50km")+theme_minimal()

ggsave("~/Dropbox/Apps/Overleaf/Protest Diffusion (BLM & Alt-Right)/figures/surv_model_2_cat_1.pdf",p1, width = 9, height = 9, units = c("cm"))

####################################
####################################

plot.data <- model.data %>%
				summarise(Temp.spatial.first.event...50km.=mean(Temp.spatial.first.event...50km.),
													Temp.spatial.first.event..state..50km.= mean(Temp.spatial.first.event..state..50km.),
													Temp.spatial.first.event..non.state. = mean(Temp.spatial.first.event..non.state.),
													Total.population_x.log = quantile(Total.population_x.log,probs=0.15),
													Black.or.African.American_x = mean(Black.or.African.American_x),
													White_x = mean(White_x),
													Median.age..years._x = mean(Median.age..years._x))
					
			plot.list <- list()
				for(i in 1:100){
					plot.list[[i]] <- plot.data
				}
			plot.data <- bind_rows(plot.list)		
					plot.data$Temp.spatial.first.event..state..50km. <- seq(min(model.data$Temp.spatial.first.event..state..50km.),max(model.data$Temp.spatial.first.event..state..50km.),length=100)
					
			plot.data.1 <- plot.data


plot.data <- model.data %>%
				summarise(Temp.spatial.first.event...50km.=mean(Temp.spatial.first.event...50km.),
													Temp.spatial.first.event..state..50km.= mean(Temp.spatial.first.event..state..50km.),
													Temp.spatial.first.event..non.state. = mean(Temp.spatial.first.event..non.state.),
													Total.population_x.log = quantile(Total.population_x.log,probs=0.85),
													Black.or.African.American_x = mean(Black.or.African.American_x),
													White_x = mean(White_x),
													Median.age..years._x = mean(Median.age..years._x))
					
			plot.list <- list()
				for(i in 1:100){
					plot.list[[i]] <- plot.data
				}
			plot.data <- bind_rows(plot.list)		
					plot.data$Temp.spatial.first.event..state..50km. <- seq(min(model.data$Temp.spatial.first.event..state..50km.),max(model.data$Temp.spatial.first.event..state..50km.),length=100)
					
			plot.data.2 <- plot.data

			plot.data <- bind_rows(plot.data.1,plot.data.2)	

pred.model <- predict(surv_model.2.cat,type="risk",se.fit=TRUE,newdata=plot.data)

plot.data$fit <- pred.model$fit
plot.data$se.fit <- pred.model$se.fit
plot.data$hi95 <- plot.data$fit+1.96*plot.data$se.fit
plot.data$lo95 <- plot.data$fit-1.96*plot.data$se.fit




plot.data$Population <- as.factor(plot.data$Total.population_x.log)

p1 <- ggplot(data=plot.data,aes(y=fit,x=Temp.spatial.first.event..state..50km.,group=Population,colour=Population,fill=Population))+
		#geom_point()+
			geom_line()+
				geom_ribbon(aes(ymin=lo95,ymax=hi95),alpha=0.3,linetype=0)+
					scale_fill_discrete(labels = c("Low", "High"))+
					scale_colour_discrete(labels = c("Low", "High"))+
					 ylab("Risk")+ xlab("Distance to first event within state")+theme_minimal()

ggsave("~/Dropbox/Apps/Overleaf/Protest Diffusion (BLM & Alt-Right)/figures/surv_model_2_cat_2.pdf",p1, width = 9, height = 9, units = c("cm"))

####################################
####################################
plot.data <- model.data %>%
				summarise(Temp.spatial.first.event...50km.=mean(Temp.spatial.first.event...50km.),
													Temp.spatial.first.event..state..50km.= mean(Temp.spatial.first.event..state..50km.),
													Temp.spatial.first.event..non.state. = mean(Temp.spatial.first.event..non.state.),
													Total.population_x.log = quantile(Total.population_x.log,probs=0.15),
													Black.or.African.American_x = mean(Black.or.African.American_x),
													White_x = mean(White_x),
													Median.age..years._x = mean(Median.age..years._x))
					
			plot.list <- list()
				for(i in 1:100){
					plot.list[[i]] <- plot.data
				}
			plot.data <- bind_rows(plot.list)		
					plot.data$Temp.spatial.first.event..non.state. <- seq(min(model.data$Temp.spatial.first.event..non.state.),max(model.data$Temp.spatial.first.event..non.state.),length=100)
					
			plot.data.1 <- plot.data


plot.data <- model.data %>%
				summarise(Temp.spatial.first.event...50km.=mean(Temp.spatial.first.event...50km.),
													Temp.spatial.first.event..state..50km.= mean(Temp.spatial.first.event..state..50km.),
													Temp.spatial.first.event..non.state. = mean(Temp.spatial.first.event..non.state.),
													Total.population_x.log = quantile(Total.population_x.log,probs=0.85),
													Black.or.African.American_x = mean(Black.or.African.American_x),
													White_x = mean(White_x),
													Median.age..years._x = mean(Median.age..years._x))
					
			plot.list <- list()
				for(i in 1:100){
					plot.list[[i]] <- plot.data
				}
			plot.data <- bind_rows(plot.list)		
					plot.data$Temp.spatial.first.event..non.state. <- seq(min(model.data$Temp.spatial.first.event..non.state.),max(model.data$Temp.spatial.first.event..non.state.),length=100)
					
			plot.data.2 <- plot.data

			plot.data <- bind_rows(plot.data.1,plot.data.2)	

pred.model <- predict(surv_model.2.cat,type="risk",se.fit=TRUE,newdata=plot.data)

plot.data$fit <- pred.model$fit
plot.data$se.fit <- pred.model$se.fit
plot.data$hi95 <- plot.data$fit+1.96*plot.data$se.fit
plot.data$lo95 <- plot.data$fit-1.96*plot.data$se.fit


plot.data$Population <- as.factor(plot.data$Total.population_x.log)

p1 <- ggplot(data=plot.data,aes(y=fit,x=Temp.spatial.first.event..non.state.,group=Population,colour=Population,fill=Population))+
		#geom_point()+
			geom_line()+
				geom_ribbon(aes(ymin=lo95,ymax=hi95),alpha=0.3,linetype=0)+
					scale_fill_discrete(labels = c("Low", "High"))+
					scale_colour_discrete(labels = c("Low", "High"))+
					 ylab("Risk")+ xlab("Distance to first event outside state")+theme_minimal()

ggsave("~/Dropbox/Apps/Overleaf/Protest Diffusion (BLM & Alt-Right)/figures/surv_model_2_cat_3.pdf",p1, width = 9, height = 9, units = c("cm"))
####################################
####################################
