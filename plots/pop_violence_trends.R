library(ggplot2)

# data preparation
d <- aggregate(data_LULC$pop_change, list(data_LULC$year, data_LULC$state), FUN = sum)
d$pop <- aggregate(data_LULC$pop, list(data_LULC$year, data_LULC$state), FUN = sum)[,3]
d$pop_growth <- log((d$x + d$pop)/d$pop)
d$prop_pop <- (d$x + d$pop)/d$pop

names(d)[1] <- "time"
names(d)[2] <- "state"
names(d)[3] <- "pop_change"
names(d)[4] <- "pop"
names(d)[5] <- "pop_growth"

d$period <- d$time

d$violence <- aggregate(data_LULC$incidents, list(data_LULC$year, data_LULC$state), FUN = sum)[,3]

d$time[d$time == 1] <- "1990"
d$time[d$time == 2] <- "1995"
d$time[d$time == 3] <- "2000"
d$time[d$time == 4] <- "2005"
d$time[d$time == 5] <- "2010"

d$time <- as.numeric(d$time)
d$period <- as.numeric(d$period)

# plots
ggplot(data=d, aes(x=time, y=log(pop), group = state, color = state))+geom_point() + geom_line(size = 1, alpha = 0.4)+ labs(title = "",
                                                                                                                            x = "",
                                                                                                                            y = "log(population)") + xlim(c(1990,2010)) + theme(plot.margin = margin(t = 0, 
                                                                                                                                                                                                       r = 0, 
                                                                                                                                                                                                       b = 0,  
                                                                                                                                                                                                       l = 0))


ggplot(data=d, aes(x=period, y=pop_growth, group = state, color = state))+geom_point() + geom_line(size = 1, alpha = 0.4)+ labs(title = "",
                                                                                                                            x = "time-period",
                                                                                                                            y = "log(pop_t/pop_(t-1))") + theme(plot.margin = margin(t = 0, 
                                                                                                                                                                                     r = 0, 
                                                                                                                                                                                     b = 0,  
                                                                                                                                                                                     l = 0)) + xlim(c(1,5))

ggplot(data=d, aes(x=period, y=violence, group = state, color = state))+geom_point() + geom_line(size = 1, alpha = 0.4)+ labs(title = "",
                                                                                                                                x = "time-period",
                                                                                                                                y = "number of incidents") + theme(plot.margin = margin(t = 0, 
                                                                                                                                                                     r = 0, 
                                                                                                                                                                     b = 0,  
                                                                                                                                                                     l = 0)) + xlim(c(1,5))
                                                                                                                                
