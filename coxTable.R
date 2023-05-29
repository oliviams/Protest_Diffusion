



#coef.Names.after <- c("DV$_{lag}$","Number of troops in country$_{100,000}$","Years after peacekeepers left grid","Casualties in grid$_{1,000}$","Spatial Lag number of troops in grid")


model.Names <- c("Base Model","Interaction Model","Full Sample")

texreg(list(surv_model.1.cat,surv_model.2.cat,surv_model.3.cat),use.packages=FALSE,dcolumn=TRUE,custom.model.names=model.Names,stars = c(0.01, 0.05, 0.1),digits=4,scalebox=0.7,file="~/Dropbox/Apps/Overleaf/Protest Diffusion (BLM & Alt-Right)/tables/coxModels.tex",caption="Cox Models",label="table:coefficientsCox",caption.above=TRUE)


#custom.coef.names=coef.Names.cox