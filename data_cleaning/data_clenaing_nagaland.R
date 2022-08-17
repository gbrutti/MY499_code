library(stargazer)

# Nagaland subset
data_nagaland <- data_LULC[(data$state == "Nagaland"),]

# politicla laignment data
p98 <- read.csv("data/political_alignment98.csv")
p03 <- read.csv("data/political_alignment03.csv")
p08 <- read.csv("data/political_alignment08.csv")
p13 <- read.csv("data/political_alignment13.csv")
p18 <- read.csv("data/political_alignment18.csv")

p98 <- p98[, c(1,2,5)]
names(p98)[3] <- "party98"
names(p98)[1] <- "AC.Name"
names(p98)[2] <- "AC.No."
p03 <- p03[, c(1,2,5)]
names(p03)[3] <- "party03"
p08 <- p08[, c(1,2,5)]
names(p08)[3] <- "party08"
p13 <- p13[, c(1,2,6)]
names(p13)[3] <- "party13"
p18 <- p18[, c(1,2,4,6)]
names(p18)[4] <- "party18"

politics <- merge(p18, p13, by = c("AC.Name", "AC.No."), all = T)
politics <- merge(politics, p08, by = c("AC.Name", "AC.No."), all = T)
politics <- merge(politics, p03, by = c("AC.Name", "AC.No."), all = T)
politics <- merge(politics, p98, by = c("AC.Name", "AC.No."), all = T)

politics <- melt(setDT(politics), id = c(1,2,3), variable.name = "year")

politics$year <- as.character(politics$year)
politics[politics == "party98"] <- "1"
politics[politics == "party03"] <- "2"
politics[politics == "party08"] <- "3"
politics[politics == "party13"] <- "4"
politics[politics == "party18"] <- "5"

names(politics)[3] <- "name"

unique(data_nagaland$name)[which(!(unique(data_nagaland$name) %in% unique(politics$name)))]

politics$name[politics$AC.Name == "Noklak"] <- "Noklak"
politics$name[politics$AC.Name == "Shamator"] <- "Noklak"

unique(politics$value[politics$year == "4"])

politics$alignment[politics$year == "1"] <- 0

politics$alignment[politics$year == "2" & politics$value == "Indian National Congress"] <- 0
politics$alignment[politics$year == "2" & politics$value == "Independent"] <- 0
politics$alignment[politics$year == "2" & politics$value == "Nagaland Peoples Front"] <- 0
politics$alignment[politics$year == "2" & politics$value == "Bharatiya Janta Party"] <- 1
politics$alignment[politics$year == "2" & politics$value == "Janata Dal (United)"] <- 1
politics$alignment[politics$year == "2" & politics$value == "Samata Party"] <- 1
politics$alignment[politics$year == "2" & politics$value == "Nationalist Democratic Movement"] <- 0

politics$alignment[politics$year == "3" & politics$value == "Indian National Congress"] <- 1
politics$alignment[politics$year == "3" & politics$value == "Bharatiya Janta Party"] <- 0
politics$alignment[politics$year == "3" & politics$value == "Independent"] <- 0
politics$alignment[politics$year == "3" & politics$value == "Nationalist Congress Party"] <- 1
politics$alignment[politics$year == "3" & politics$value == "Nagaland Peoples Front"] <- 0

politics$alignment[politics$year == "4" & politics$value == "Indian National Congress"] <- 1
politics$alignment[politics$year == "4" & politics$value == "Bharatiya Janta Party"] <- 0
politics$alignment[politics$year == "4" & politics$value == "Independent"] <- 0
politics$alignment[politics$year == "4" & politics$value == "Nationalist Congress Party"] <- 1
politics$alignment[politics$year == "4" & politics$value == "Naga Peoples Front"] <- 0
politics$alignment[politics$year == "4" & politics$value == "Janata Dal (United)"] <- 0

politics$alignment[politics$year == "5" & politics$value == "Indian National Congress"] <- 0
politics$alignment[politics$year == "5" & politics$value == "Bharatiya Janta Party"] <- 1
politics$alignment[politics$year == "5" & politics$value == "Independent"] <- 0
politics$alignment[politics$year == "5" & politics$value == "Nationalist Democratic Progressive Party"] <- 1
politics$alignment[politics$year == "5" & politics$value == "Naga Peoples Front"] <- 1
politics$alignment[politics$year == "5" & politics$value == "Janata Dal (United)"] <- 0
politics$alignment[politics$year == "5" & politics$value == "National People's Party"] <- 1

t <- data.frame(table(politics$alignment, politics$name, politics$year))
group <- as.data.table(t)

group$Var1 <- as.character(group$Var1)

group[duplicated(group[,-1])]

group$Var1[(group$Var2 == "Kiphire") & (group$Var3 == 2)] <- 1
group$Var1[(group$Var2 == "Longleng") & (group$Var3 == 2)] <- 1
group$Var1[(group$Var2 == "Longleng") & (group$Var3 == 3)] <- 1
group$Var1[(group$Var2 == "Mokokchung") & (group$Var3 == 3)] <- 1
group$Var1[(group$Var2 == "Peren") & (group$Var3 == 3)] <- 1
group$Var1[(group$Var2 == "Wokha") & (group$Var3 == 4)] <- 1

t <- group[group[, .I[which.max(Freq)], by= c("Var2", "Var3")]$V1]

alignment <- t[,1:3]
names(alignment)[1] <- "alignment"
names(alignment)[2] <- "name"
names(alignment)[3] <- "year"

data_nagaland <- merge(data_nagaland, alignment, by = c("name", "year"))

# PCA
data_nagalandx <- data_nagaland[,(colnames(data_nagaland) %like% 'Shape_Area')]
data_nagalandx <- data_nagalandx[,which(colSums(data_nagalandx) != 0)]
data_nagalandx <- data_nagalandx[, -c(1:2)]

names(data_nagalandx)[1] <- "GSS_MNEF"
names(data_nagalandx)[2] <- "GSS_MDF"
names(data_nagalandx)[3] <- "GSS_ASUA"
names(data_nagalandx)[4] <- "MC_ASUA"
names(data_nagalandx)[5] <- "MDF_GSS"
names(data_nagalandx)[6] <- "MNEF_GSS"
names(data_nagalandx)[7] <- "MNEF_AUSA"
names(data_nagalandx)[8] <- "MNEF_MC"
names(data_nagalandx)[9] <- "MC_MNEF"
names(data_nagalandx)[10] <- "MC_MDF"

pca1 <- prcomp(data_nagalandx, scale = T)
comps1 <- pca1$x

# choosing number of PCs

#1
screeplot(pca1, type='l', main="", npcs = 9)

#2
pca_var1 <- pca1$sdev^2
which(pca_var1 >= .8)

#3
total_var1 <- sum(pca_var1)
sum(pca_var1[1:4]/total_var1)
pca_var1[4]

# save PCs
data_pcs1 <- comps1[,1:4]

# PCs weights
pca1$rotation[,1][order(pca1$rotation[,1],decreasing = T)]

pca1$rotation[,2][order(pca1$rotation[,2],decreasing = T)]

pca1$rotation[,3][order(pca1$rotation[,3],decreasing = T)]

pca1$rotation[,4][order(pca1$rotation[,4],decreasing = T)]

#stargazer(pca1$rotation[,1],type = "html", out = "PC1.doc")
#stargazer(pca1$rotation[,2],type = "html", out = "PC2.doc")
#stargazer(pca1$rotation[,3],type = "html", out = "PC3.doc")
#stargazer(pca1$rotation[,4],type = "html", out = "PC4.doc")




