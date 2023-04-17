library(ggplot2)
library(scales)
library(dplyr)
options(scipen=999)

blm <- read.csv("./data/blm_weekly_first_gf.csv")

# SPATIAL EFFECTS

sp_eff_blm = glm(Protest._x ~ Protests.under.50km.past.week_x + Protests.same.state.past.week...50km_x +
                      Protests.past.week.not.same.state_x + 
                      poly(Weeks.since.GF, 3), family=binomial(logit), data=blm)
summary(sp_eff_blm)

sp_eff_right = glm(Protest._x ~ Protests.under.50km.past.week_y + Protests.same.state.past.week...50km_y +
                        Protests.past.week.not.same.state_y + poly(Weeks.since.GF, 3), family=binomial(logit), data=blm)
summary(sp_eff_right)


# SPATIAL INTERACTION

sp_int_3 = glm(Protest._x ~ Distance.to.closest.top.3.city.in.state.protest.past.week..inc.same._x + 
                      poly(Weeks.since.GF, 3), family=binomial(logit), data=blm)
summary(sp_int_3)

sp_int_4 = glm(Protest._x ~ Distance.to.closest.top.4.city.in.state.protest.past.week..inc.same._x + 
                 poly(Weeks.since.GF, 3), family=binomial(logit), data=blm)
summary(sp_int_4)

sp_int_5 = glm(Protest._x ~ Distance.to.closest.top.5.city.in.state.protest.past.week..inc.same._x + 
                 poly(Weeks.since.GF, 3), family=binomial(logit), data=blm)
summary(sp_int_5)

# CHARACTERISTICS

charac = glm(Protest._x ~ Total.population_x + Black.or.African.American_x + White_x +
             Median.age..years._x + HBCU_x + Total.housing.units_x + poly(Weeks.since.GF, 3), data=blm)
summary(charac)

charac_states = glm(Protest._x ~ Total.population_x + Black.or.African.American_x + White_x +
                     Median.age..years._x + HBCU_x + Total.housing.units_x + poly(Weeks.since.GF, 3) +
                    MN + CA + MI + TX + IL + OK + NC + FL + CO + TN + OR + AL + AZ + IA + NM + KY + NV + 
                    NY + MA + WA + AR + NE + OH + PA + ME + GA + MS + IN + CT + WI + ID + KS + HI + MO + 
                    MT + VA + UT + LA + AK + WY + DC + MD + WV + SC + NJ + ND + VT + NH + RI + SD + DE + 
                    PR + GU + MP + VI + AS, data=blm)
summary(charac_states)


# SPATIAL EFFECTS + CHARACTERISTICS

sp_eff_ch = glm(Protest._x ~ Protests.under.50km.past.week_x + Protests.same.state.past.week...50km_x +
                      Protests.past.week.not.same.state_x + 
                      poly(Weeks.since.GF, 3) + Total.population_x + Black.or.African.American_x + White_x +
                  Median.age..years._x + HBCU_x + Total.housing.units_x +
                  MN + CA + MI + TX + IL + OK + NC + FL + CO + TN + OR + AL + AZ + IA + NM + KY + NV + 
                  NY + MA + WA + AR + NE + OH + PA + ME + GA + MS + IN + CT + WI + ID + KS + HI + MO + 
                  MT + VA + UT + LA + AK + WY + DC + MD + WV + SC + NJ + ND + VT + NH + RI + SD + DE + 
                  PR + GU + MP + VI + AS, family=binomial(logit), data=blm)
summary(sp_eff_ch)


# SPATIAL INTERACTION + CHARACTERISTICS

sp_int_ch = glm(Protest._x ~ Distance.to.closest.top.3.city.in.state.protest.past.week..inc.same._x + 
               poly(Weeks.since.GF, 3) + Total.population_x + Black.or.African.American_x + White_x +
                 Median.age..years._x + HBCU_x + Total.housing.units_x +
                 MN + CA + MI + TX + IL + OK + NC + FL + CO + TN + OR + AL + AZ + IA + NM + KY + NV + 
                 NY + MA + WA + AR + NE + OH + PA + ME + GA + MS + IN + CT + WI + ID + KS + HI + MO + 
                 MT + VA + UT + LA + AK + WY + DC + MD + WV + SC + NJ + ND + VT + NH + RI + SD + DE + 
                 PR + GU + MP + VI + AS, family=binomial(logit), data=blm)

summary(sp_int_ch)

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
