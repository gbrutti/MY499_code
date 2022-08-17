library(raster)
library(spdep)
library(MASS)
library(lme4)

# map NE districts and aggregate by state
s <- shapefile("data/NE_districts.shp")
ss <- aggregate(s, "state")

shape <- s[,1]

# merge with LULC data
mer1 <- merge(shape, data_LULC_1, by = "name")
mer2 <- merge(shape, data_LULC_2, by = "name")
mer3 <- merge(shape, data_LULC_3, by = "name")
mer4 <- merge(shape, data_LULC_4, by = "name")
mer5 <- merge(shape, data_LULC_5, by = "name")

# spatial weights
nb1 <- poly2nb(mer1, queen = T)
nb2 <- poly2nb(mer2, queen = T)
nb3 <- poly2nb(mer3, queen = T)
nb4 <- poly2nb(mer4, queen = T)
nb5 <- poly2nb(mer5, queen = T)

lw1 <- nb2listw(nb1)
lw2 <- nb2listw(nb2)
lw3 <- nb2listw(nb3)
lw4 <- nb2listw(nb4)
lw5 <- nb2listw(nb5)

mer1$spatial_lag <- lag.listw(x=lw1, var=mer1$incidents)
mer2$spatial_lag <- lag.listw(x=lw2, var=mer2$incidents)
mer3$spatial_lag <- lag.listw(x=lw3, var=mer3$incidents)
mer4$spatial_lag <- lag.listw(x=lw1, var=mer4$incidents)
mer5$spatial_lag <- lag.listw(x=lw1, var=mer5$incidents)

mer <- rbind(mer1, mer2, mer3, mer4, mer5)

# Moran I tests

moran.test(mer1$incidents, lw1)
moran.test(mer2$incidents, lw2)
moran.test(mer3$incidents, lw3)
moran.test(mer4$incidents, lw4)
moran.test(mer5$incidents, lw5)

# rename variables
names(mer)[2] <- "time"
names(mer)[4] <- "violence"
names(mer)[49] <- "pgrowth"
names(mer)[48] <- "vprior"
names(mer)[10] <- "GSS_MNEF"    
names(mer)[11] <- "GSS_MDF"             
names(mer)[12] <- "GSS_ASUA"        
names(mer)[13] <- "MC_ASUA"                
names(mer)[14] <- "MDF_GSS"               
names(mer)[15] <- "MDF_MC"                        
names(mer)[16] <- "MNEF_GSS"   
names(mer)[17] <- "SW_ASUA"                   
names(mer)[18] <- "MNEF_ASUA"
names(mer)[19] <- "MNEF_MC"             
names(mer)[20] <- "MDF_ASUA"          
names(mer)[21] <- "SW_BA"                                       
names(mer)[22] <- "SW_GSS"                        
names(mer)[23] <- "BA_GSS"                             
names(mer)[24] <- "GSS_SW"                        
names(mer)[25] <- "MC_MNEF"            
names(mer)[26] <- "MNEF_BA"                 
names(mer)[27] <- "GSS_SUFV"     
names(mer)[28] <- "SW_SOFV"                
names(mer)[29] <- "BA_MC"                                   
names(mer)[30] <- "BA_SW"                                        
names(mer)[31] <- "MC_BA"                                   
names(mer)[32] <- "MC_SW"                                
names(mer)[33] <- "SW_MC"                                 
names(mer)[34] <- "MC_SOFV"                
names(mer)[35] <- "MC_MDF"                        
names(mer)[36] <- "SOFV_ASUA" 
names(mer)[37] <- "SW_SV"                              
names(mer)[38] <- "GSS_BA"                            
names(mer)[39] <- "MNEF_SW"              
names(mer)[40] <- "SW_MNEF"              
names(mer)[41] <- "SOFV_MC"               
names(mer)[42] <- "SOFV_SW"                 
names(mer)[43] <- "MC_SV"                             
names(mer)[44] <- "SOFV_GSS"
names(mer)[45] <- "BA_SV"                                  
names(mer)[46] <- "SV_SW"                             
names(mer)[47] <- "MDF_SW"

# analysis

mod0 <- glm.nb(violence ~ spatial_lag + pgrowth + vprior + time, data = data.frame(mer))
summary(mod0)

# stargazer(mod0, type = "html", out = "model_1.doc", style = "ajps", star.cutoffs = c(0.05, 0.01, 0.001), add.lines=list(c("BIC", round(BIC(mod0),3))))

mix.int <- glmer.nb(violence ~ scale(spatial_lag) + scale(pgrowth) + scale(vprior) + time + (1 | state), data = data.frame(mer))
summary(mix.int)

mix.slope <- glmer.nb(violence ~ scale(spatial_lag) + scale(pgrowth) + scale(vprior) + time + (scale(pgrowth) | state), data = data.frame(mer))
summary(mix.slope)
ranef(mix.slope)

# stargazer(mix.int,mix.slope, type = "html", out = "model_2.doc", style = "ajps", star.cutoffs = c(0.05, 0.01, 0.001), add.lines=list(c("BIC", round(BIC(mix.int),3),round(BIC(mix.slope),3))))

anova(mix.int, mix.slope)

