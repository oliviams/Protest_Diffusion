

temp.table <- xtable(sp_weib_model, caption = "Protest model with Weibull hazard")

print(temp.table, caption.placement = "top", comment = FALSE, include.rownames = FALSE, file="~/Dropbox/Apps/Overleaf/Protest Diffusion (BLM & Alt-Right)/tables/splitWeibModels.tex")


temp.table <- xtable(sp_loglog_model, caption = "Protest model with Loglog hazard")

print(temp.table, caption.placement = "top", comment = FALSE, include.rownames = FALSE, file="~/Dropbox/Apps/Overleaf/Protest Diffusion (BLM & Alt-Right)/tables/splitLoglogModels.tex")




#custom.coef.names=coef.Names.cox