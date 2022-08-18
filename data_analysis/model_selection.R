
mod_lm <- lm(violence ~ spatial_lag + pgrowth + vprior + time, data = data.frame(mer))
summary(mod_lm)
AIC(mod_lm)

mod_poi <- glm(violence ~ spatial_lag + pgrowth + vprior + time, data = data.frame(mer), family = "poisson")
summary(mod_poi)

mod_nb <- glm.nb(violence ~ spatial_lag + pgrowth + vprior + time, data = data.frame(mer))
summary(mod_nb)

stargazer(mod_lm, mod_poi, mod_nb, type = "html", out = "model_comparison.doc", style = "ajps", star.cutoffs = c(0.05, 0.01, 0.001), add.lines=list(c("BIC", round(BIC(mod_lm),3), round(BIC(mod_poi),3), round(BIC(mod_nb),3))))
