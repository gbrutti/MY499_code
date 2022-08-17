mer_arunachal <- mer[mer$state == "Arunachal Pradesh",]
mer_arunachal_1 <- mer_arunachal[mer_arunachal$time == "1",]
mer_arunachal_2 <- mer_arunachal[mer_arunachal$time == "2",]
mer_arunachal_3 <- mer_arunachal[mer_arunachal$time == "3",]
mer_arunachal_4 <- mer_arunachal[mer_arunachal$time == "4",]
mer_arunachal_5 <- mer_arunachal[mer_arunachal$time == "5",]

nb_arunachal1 <- poly2nb(mer_arunachal_1, queen = T)
nb_arunachal2 <- poly2nb(mer_arunachal_2, queen = T)
nb_arunachal3 <- poly2nb(mer_arunachal_3, queen = T)
nb_arunachal4 <- poly2nb(mer_arunachal_4, queen = T)
nb_arunachal5 <- poly2nb(mer_arunachal_5, queen = T)

lw_arunachal <- nb2listw(nb_arunachal1)
lw_arunachal <- nb2listw(nb_arunachal2)
lw_arunachal <- nb2listw(nb_arunachal3)
lw_arunachal <- nb2listw(nb_arunachal4)
lw_arunachal <- nb2listw(nb_arunachal5)

mer_arunachal_1$spatial_lag <- lag.listw(x=lw_arunachal, var=mer_arunachal_1$violence)
mer_arunachal_2$spatial_lag <- lag.listw(x=lw_arunachal, var=mer_arunachal_2$violence)
mer_arunachal_3$spatial_lag <- lag.listw(x=lw_arunachal, var=mer_arunachal_3$violence)
mer_arunachal_4$spatial_lag <- lag.listw(x=lw_arunachal, var=mer_arunachal_4$violence)
mer_arunachal_5$spatial_lag <- lag.listw(x=lw_arunachal, var=mer_arunachal_5$violence)

mer_arunachal <- rbind(mer_arunachal_1, mer_arunachal_2, mer_arunachal_3, mer_arunachal_4, mer_arunachal_5)

mod_arunachal <- glm.nb(violence ~ spatial_lag + pgrowth + vprior + time, data = mer_arunachal, control=glm.control(maxit=50))
summary(mod_arunachal)

mer_assam <- mer[mer$state == "Assam",]
mer_assam_1 <- mer_assam[mer_assam$time == "1",]
mer_assam_2 <- mer_assam[mer_assam$time == "2",]
mer_assam_3 <- mer_assam[mer_assam$time == "3",]
mer_assam_4 <- mer_assam[mer_assam$time == "4",]
mer_assam_5 <- mer_assam[mer_assam$time == "5",]

nb_assam1 <- poly2nb(mer_assam_1, queen = T)
nb_assam2 <- poly2nb(mer_assam_2, queen = T)
nb_assam3 <- poly2nb(mer_assam_3, queen = T)
nb_assam4 <- poly2nb(mer_assam_4, queen = T)
nb_assam5 <- poly2nb(mer_assam_5, queen = T)

lw_assam <- nb2listw(nb_assam1)
lw_assam <- nb2listw(nb_assam2)
lw_assam <- nb2listw(nb_assam3)
lw_assam <- nb2listw(nb_assam4)
lw_assam <- nb2listw(nb_assam5)

mer_assam_1$spatial_lag <- lag.listw(x=lw_assam, var=mer_assam_1$violence)
mer_assam_2$spatial_lag <- lag.listw(x=lw_assam, var=mer_assam_2$violence)
mer_assam_3$spatial_lag <- lag.listw(x=lw_assam, var=mer_assam_3$violence)
mer_assam_4$spatial_lag <- lag.listw(x=lw_assam, var=mer_assam_4$violence)
mer_assam_5$spatial_lag <- lag.listw(x=lw_assam, var=mer_assam_5$violence)

mer_assam <- rbind(mer_assam_1, mer_assam_2, mer_assam_3, mer_assam_4, mer_assam_5)

mod_assam <- glm.nb(violence ~ spatial_lag + pgrowth + vprior + time, data = mer_assam, control=glm.control(maxit=50))
summary(mod_assam)

mer_manipur <- mer[mer$state == "Manipur",]
mer_manipur_1 <- mer_manipur[mer_manipur$time == "1",]
mer_manipur_2 <- mer_manipur[mer_manipur$time == "2",]
mer_manipur_3 <- mer_manipur[mer_manipur$time == "3",]
mer_manipur_4 <- mer_manipur[mer_manipur$time == "4",]
mer_manipur_5 <- mer_manipur[mer_manipur$time == "5",]

nb_manipur1 <- poly2nb(mer_manipur_1, queen = T)
nb_manipur2 <- poly2nb(mer_manipur_2, queen = T)
nb_manipur3 <- poly2nb(mer_manipur_3, queen = T)
nb_manipur4 <- poly2nb(mer_manipur_4, queen = T)
nb_manipur5 <- poly2nb(mer_manipur_5, queen = T)

lw_manipur <- nb2listw(nb_manipur1)
lw_manipur <- nb2listw(nb_manipur2)
lw_manipur <- nb2listw(nb_manipur3)
lw_manipur <- nb2listw(nb_manipur4)
lw_manipur <- nb2listw(nb_manipur5)

mer_manipur_1$spatial_lag <- lag.listw(x=lw_manipur, var=mer_manipur_1$violence)
mer_manipur_2$spatial_lag <- lag.listw(x=lw_manipur, var=mer_manipur_2$violence)
mer_manipur_3$spatial_lag <- lag.listw(x=lw_manipur, var=mer_manipur_3$violence)
mer_manipur_4$spatial_lag <- lag.listw(x=lw_manipur, var=mer_manipur_4$violence)
mer_manipur_5$spatial_lag <- lag.listw(x=lw_manipur, var=mer_manipur_5$violence)

mer_manipur <- rbind(mer_manipur_1, mer_manipur_2, mer_manipur_3, mer_manipur_4, mer_manipur_5)

mod_manipur <- glm.nb(violence ~ spatial_lag + pgrowth + vprior + time, data = mer_manipur, control=glm.control(maxit=50))
summary(mod_manipur)

mer_meghalaya <- mer[mer$state == "Meghalaya",]
mer_meghalaya_1 <- mer_meghalaya[mer_meghalaya$time == "1",]
mer_meghalaya_2 <- mer_meghalaya[mer_meghalaya$time == "2",]
mer_meghalaya_3 <- mer_meghalaya[mer_meghalaya$time == "3",]
mer_meghalaya_4 <- mer_meghalaya[mer_meghalaya$time == "4",]
mer_meghalaya_5 <- mer_meghalaya[mer_meghalaya$time == "5",]

nb_meghalaya1 <- poly2nb(mer_meghalaya_1, queen = T)
nb_meghalaya2 <- poly2nb(mer_meghalaya_2, queen = T)
nb_meghalaya3 <- poly2nb(mer_meghalaya_3, queen = T)
nb_meghalaya4 <- poly2nb(mer_meghalaya_4, queen = T)
nb_meghalaya5 <- poly2nb(mer_meghalaya_5, queen = T)

lw_meghalaya <- nb2listw(nb_meghalaya1)
lw_meghalaya <- nb2listw(nb_meghalaya2)
lw_meghalaya <- nb2listw(nb_meghalaya3)
lw_meghalaya <- nb2listw(nb_meghalaya4)
lw_meghalaya <- nb2listw(nb_meghalaya5)

mer_meghalaya_1$spatial_lag <- lag.listw(x=lw_meghalaya, var=mer_meghalaya_1$violence)
mer_meghalaya_2$spatial_lag <- lag.listw(x=lw_meghalaya, var=mer_meghalaya_2$violence)
mer_meghalaya_3$spatial_lag <- lag.listw(x=lw_meghalaya, var=mer_meghalaya_3$violence)
mer_meghalaya_4$spatial_lag <- lag.listw(x=lw_meghalaya, var=mer_meghalaya_4$violence)
mer_meghalaya_5$spatial_lag <- lag.listw(x=lw_meghalaya, var=mer_meghalaya_5$violence)

mer_meghalaya <- rbind(mer_meghalaya_1, mer_meghalaya_2, mer_meghalaya_3, mer_meghalaya_4, mer_meghalaya_5)

mod_meghalaya <- glm.nb(violence ~ spatial_lag + pgrowth + vprior + time, data = mer_meghalaya, control=glm.control(maxit=50))
summary(mod_meghalaya)

mer_tripura <- mer[mer$state == "Tripura",]
mer_tripura_1 <- mer_tripura[mer_tripura$time == "1",]
mer_tripura_2 <- mer_tripura[mer_tripura$time == "2",]
mer_tripura_3 <- mer_tripura[mer_tripura$time == "3",]
mer_tripura_4 <- mer_tripura[mer_tripura$time == "4",]
mer_tripura_5 <- mer_tripura[mer_tripura$time == "5",]

nb_tripura1 <- poly2nb(mer_tripura_1, queen = T)
nb_tripura2 <- poly2nb(mer_tripura_2, queen = T)
nb_tripura3 <- poly2nb(mer_tripura_3, queen = T)
nb_tripura4 <- poly2nb(mer_tripura_4, queen = T)
nb_tripura5 <- poly2nb(mer_tripura_5, queen = T)

lw_tripura <- nb2listw(nb_tripura1)
lw_tripura <- nb2listw(nb_tripura2)
lw_tripura <- nb2listw(nb_tripura3)
lw_tripura <- nb2listw(nb_tripura4)
lw_tripura <- nb2listw(nb_tripura5)

mer_tripura_1$spatial_lag <- lag.listw(x=lw_tripura, var=mer_tripura_1$violence)
mer_tripura_2$spatial_lag <- lag.listw(x=lw_tripura, var=mer_tripura_2$violence)
mer_tripura_3$spatial_lag <- lag.listw(x=lw_tripura, var=mer_tripura_3$violence)
mer_tripura_4$spatial_lag <- lag.listw(x=lw_tripura, var=mer_tripura_4$violence)
mer_tripura_5$spatial_lag <- lag.listw(x=lw_tripura, var=mer_tripura_5$violence)

mer_tripura <- rbind(mer_tripura_1, mer_tripura_2, mer_tripura_3, mer_tripura_4, mer_tripura_5)

mod_tripura <- glm.nb(violence ~ spatial_lag + pgrowth + vprior + time, data = mer_tripura, control=glm.control(maxit=50))
summary(mod_tripura)

# stargazer(mod_arunachal, mod_assam, mod_manipur, mod_meghalaya, mod_tripura, type = "html", out = "model_states.doc", style = "ajps", star.cutoffs = c(0.05, 0.01, 0.001), add.lines=list(c("BIC", round(BIC(mod_arunachal),3),round(BIC(mod_assam),3),round(BIC(mod_manipur),3),round(BIC(mod_meghalaya),3),round(BIC(mod_tripura),3))))


