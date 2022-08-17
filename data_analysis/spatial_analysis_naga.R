library(lmtest)

# subset Nagaland data
mer_naga <- mer[mer$state == "Nagaland",]

# subset Nagaland data by time
mer_naga_1 <- mer_naga[mer_naga$time == "1",]
mer_naga_2 <- mer_naga[mer_naga$time == "2",]
mer_naga_3 <- mer_naga[mer_naga$time == "3",]
mer_naga_4 <- mer_naga[mer_naga$time == "4",]
mer_naga_5 <- mer_naga[mer_naga$time == "5",]

# spatial weights

nb_naga1 <- poly2nb(mer_naga_1, queen = T)
nb_naga2 <- poly2nb(mer_naga_2, queen = T)
nb_naga3 <- poly2nb(mer_naga_3, queen = T)
nb_naga4 <- poly2nb(mer_naga_4, queen = T)
nb_naga5 <- poly2nb(mer_naga_5, queen = T)

lw_naga <- nb2listw(nb_naga1)
lw_naga <- nb2listw(nb_naga2)
lw_naga <- nb2listw(nb_naga3)
lw_naga <- nb2listw(nb_naga4)
lw_naga <- nb2listw(nb_naga5)

# spatial lag 
mer_naga_1$spatial_lag <- lag.listw(x=lw_naga, var=mer_naga_1$violence)
mer_naga_2$spatial_lag <- lag.listw(x=lw_naga, var=mer_naga_2$violence)
mer_naga_3$spatial_lag <- lag.listw(x=lw_naga, var=mer_naga_3$violence)
mer_naga_4$spatial_lag <- lag.listw(x=lw_naga, var=mer_naga_4$violence)
mer_naga_5$spatial_lag <- lag.listw(x=lw_naga, var=mer_naga_5$violence)

mer_naga <- rbind(mer_naga_1, mer_naga_2, mer_naga_3, mer_naga_4, mer_naga_5)

# political alignment
for (i in 1:nrow(mer_naga)){
  mer_naga$alignemnt[i] <- data_nagaland$alignment[(data_nagaland$name == mer_naga$name[i]) & (data_nagaland$year == mer_naga$time[i])]
}

# add PCs
dat <- cbind(data_nagaland[,1:2], data_pcs1)
names(dat)[2] <- "time"
mer_naga <- merge(data.frame(mer_naga), dat, by = c("name", "time"))

# name PCS
names(mer_naga)[52] <- "forest_recovery"
names(mer_naga)[53] <- "urbanization"
names(mer_naga)[54] <- "forest_contrast"
names(mer_naga)[55] <- "urban_contrast"

# analysis
mod1 <- glm.nb(violence ~ spatial_lag + pgrowth + vprior + time, data = mer_naga, control=glm.control(maxit=50))
summary(mod1)

confint(mod1)

mod2 <- glm.nb(violence ~ spatial_lag + pgrowth + forest_recovery + urbanization + forest_contrast + urban_contrast + vprior + time, data = mer_naga, control=glm.control(maxit=50))
summary(mod2)

confint(mod2)

mod3 <- glm.nb(violence ~ spatial_lag + pgrowth + forest_recovery + urbanization + forest_contrast + urban_contrast + alignemnt + vprior + time, data = mer_naga, control=glm.control(maxit=50))
summary(mod3)

confint(mod3)

mod4 <- glm.nb(violence ~ spatial_lag + forest_recovery + urbanization + forest_contrast + urban_contrast + alignemnt + vprior + time, data = mer_naga, control=glm.control(maxit=50))
summary(mod4)

confint(mod4)

#stargazer(mod1, mod2, mod3, mod4, type = "html", out = "model_3.doc", style = "ajps", star.cutoffs = c(0.05, 0.01, 0.001), add.lines=list(c("BIC", round(BIC(mod1),3),round(BIC(mod2),3),round(BIC(mod3),3)), round(BIC(mod4),3)))

# likelihood ratio tests
lrtest(mod1,mod2)
lrtest(mod1,mod3)



