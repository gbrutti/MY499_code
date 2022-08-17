library(latticeExtra)
library(RColorBrewer)
detach("package:ggplot2")

p1 <- spplot(mer1, "incidents", at=c(1, 10, 30, 50, 100, 150), col.regions=brewer.pal(6, "Reds"), col="transparent")
p1 + layer(sp.polygons(mer1))

p2 <- spplot(mer2, "incidents", at=c(1, 10, 30, 50, 100, 150), col.regions=brewer.pal(6, "Reds"), col="transparent")
p2 + layer(sp.polygons(mer2))

p3 <- spplot(mer3, "incidents", at=c(1, 10, 30, 50, 100, 150), col.regions=brewer.pal(6, "Reds"), col="transparent")
p3 + layer(sp.polygons(mer3))

p4 <- spplot(mer4, "incidents", at=c(1, 10, 30, 50, 100, 150), col.regions=brewer.pal(6, "Reds"), col="transparent")
p4 + layer(sp.polygons(mer4))

p5 <- spplot(mer5, "incidents", at=c(1, 10, 30, 50, 100, 150), col.regions=brewer.pal(6, "Reds"), col="transparent")
p5 + layer(sp.polygons(mer5))
