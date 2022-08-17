library(DescTools)
# ALL STATES
## winsorized data
wind <- mer
wind$violence <- Winsorize(mer$violence)

mod_win <- glm.nb(violence ~ spatial_lag + pgrowth + vprior + time, data = data.frame(wind))
summary(mod0)

# stargazer(mod_win, type = "html", out = "model_win.doc", style = "ajps", star.cutoffs = c(0.05, 0.01, 0.001), add.lines=list(c("BIC", round(BIC(mod_win),3))))

mix.int_win <- glmer.nb(violence ~ scale(spatial_lag) + scale(pgrowth) + scale(vprior) + time + (1 | state), data = data.frame(wind))
summary(mix.int)

mix.slope_win <- glmer.nb(violence ~ scale(spatial_lag) + scale(pgrowth) + scale(vprior) + time + (scale(pgrowth) | state), data = data.frame(wind))
summary(mix.slope)
ranef(mix.slope)

## stargazer(mix.int_win, mix.slope_win, type = "html", out = "model_win_random.doc", style = "ajps", star.cutoffs = c(0.05, 0.01, 0.001), add.lines=list(c("BIC", round(BIC(mix.int_win),3), round(BIC(mix.slope_win),3))))

## add offset 

mod_off <- glm.nb(violence ~ spatial_lag + pgrowth + vprior + time + offset(log(pop)), data = data.frame(mer))
summary(mod_off)

## stargazer(mod_off, type = "html", out = "model_off.doc", style = "ajps", star.cutoffs = c(0.05, 0.01, 0.001), add.lines=list(c("BIC", round(BIC(mod_off),3))))

mix.int_off <- glmer.nb(violence ~ scale(spatial_lag) + scale(pgrowth) + scale(vprior) + time + offset(log(pop)) + (1 | state), data = data.frame(mer))
summary(mix.int)

mix.slope_off <- glmer.nb(violence ~ scale(spatial_lag) + scale(pgrowth) + scale(vprior) + time + offset(log(pop)) + (scale(pgrowth) | state), data = data.frame(mer))
summary(mix.slope)

## stargazer(mix.int_off, mix.slope_off, type = "html", out = "model_off_random.doc", style = "ajps", star.cutoffs = c(0.05, 0.01, 0.001), add.lines=list(c("BIC", round(BIC(mix.int_off),3), round(BIC(mix.slope_off),3))))

# NAGALAND

## winsorized data

win_naga <- mer_naga
win_naga$violence <- round(Winsorize(mer_naga$violence))

mod1_w <- glm.nb(violence ~ spatial_lag + pgrowth + vprior + time, data = win_naga, control=glm.control(maxit=50))
summary(mod1_w)

mod2_w <- glm.nb(violence ~ spatial_lag + pgrowth + forest_recovery + urbanization + forest_contrast + urban_contrast + vprior + time, data = win_naga, control=glm.control(maxit=50))
summary(mod2_w)

mod3_w <- glm.nb(violence ~ spatial_lag + pgrowth + forest_recovery + urbanization + forest_contrast + urban_contrast + vprior + time + alignemnt, data = win_naga, control=glm.control(maxit=50))
summary(mod3_w)

mod4_w <- glm.nb(violence ~ spatial_lag + forest_recovery + urbanization + forest_contrast + urban_contrast + vprior + time + alignemnt, data = win_naga, control=glm.control(maxit=50))
summary(mod4_w)

stargazer(mod1_w, mod2_w, mod3_w, mod4_w, type = "html", out = "model_w.doc", style = "ajps", star.cutoffs = c(0.05, 0.01, 0.001), add.lines=list(c("BIC", round(BIC(mod1_w),3),round(BIC(mod2_w),3),round(BIC(mod3_w),3)), round(BIC(mod4_w),3)))

## binomial logistic regression

mer_naga$alignemnt <- as.numeric(mer_naga$alignemnt)

mod_bin <- glm(alignemnt ~ pgrowth + time, data = mer_naga, family = "binomial")
summary(mod_bin)

## stargazer(mod_bin, type = "html", out = "model_binary.doc", style = "ajps", star.cutoffs = c(0.05, 0.01, 0.001), add.lines=list(c("BIC", round(BIC(mod),3)))

## add offset

mod1_off <- glm.nb(violence ~ spatial_lag + pgrowth + vprior + time + offset(log(pop)), data = mer_naga, control=glm.control(maxit=50))
summary(mod1_off)

mod2_off <- glm.nb(violence ~ spatial_lag + pgrowth + forest_recovery + urbanization + forest_contrast + urban_contrast + vprior + time + offset(log(pop)), data = mer_naga, control=glm.control(maxit=50))
summary(mod2_off)

mod3_off <- glm.nb(violence ~ spatial_lag + pgrowth + forest_recovery + urbanization + forest_contrast + urban_contrast + vprior + time + alignemnt + offset(log(pop)), data = mer_naga, control=glm.control(maxit=50))
summary(mod3_off)

mod4_off <- glm.nb(violence ~ spatial_lag + forest_recovery + urbanization + forest_contrast + urban_contrast + vprior + time + alignemnt + offset(log(pop)), data = mer_naga, control=glm.control(maxit=50))
summary(mod4_off)

## stargazer(mod1_off, mod2_off, mod3_off, mod4_off, type = "html", out = "model_off_naga.doc", style = "ajps", star.cutoffs = c(0.05, 0.01, 0.001), add.lines=list(c("BIC", round(BIC(mod1_off),3),round(BIC(mod2_off),3),round(BIC(mod3_off),3)), round(BIC(mod4_off),3)))
