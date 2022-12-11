library(maptools)
library(rgeos)
library(reshape2)

#  Global (e.g. ensemble de l’espace d’étude)
#  Intermédiaire (e.g niveau administratif supérieur)
#  Local (régions contiguës ou proches à vol d’oiseau)

fdc <- readShapeSpatial("Tunisie_snuts4.shp")
donnees <- read.csv("data_immig_2008.csv", header = TRUE, sep = ";", dec = ",", encoding = "latin1", )

names(fdc@data)[1] <- "id"
names(donnees)[1] <- "id"
fdc@data <- data.frame(fdc@data, donnees[match(fdc@data[, "id"], donnees[, "id"]), ])

# ===================================
# 2. Variables
# ===================================

# Indicateurs & labels
stock1 <- fdc@data$mig_out
stock2 <- fdc@data$area
#****** employe feminin /employe total
stockemp1 <- fdc@data$pop_urb
stockemp2 <- fdc@data$pop_t
#****** les personnes qui ont finis leur etudes superieur
stockedusup1 <- fdc@data$mig_in
stockedusup2 <- fdc@data$area

ratio <- stock1 / stock2
# stock1_label<-"Population totale, 2004 (unit)"
# stock2_label<-"Superficie (unit)"
ratio_label <- "Taux d'immigration vers l'extérieure (2002)"

# Deviation globale (valeur)
global <- sum(stock1) / sum(stock2)
stock2
# Deviation intermediaire (champ aggregatif)
medium <- fdc@data$id_snuts2

# Deviation locale (0 = contiguite, sinon distance euclidienne dans l'unité de la carte)
mydist <- 0

# Critere et seuil
critere <- "sup" # "sup" = plus grand que, "inf" = plus petit que
seuil <- 125

# ===================================
# 3. Calculs
# ===================================

# On ne garde que ce qui est utile
fdc@data <- data.frame(fdc@data$id, medium, stock1, stock2, ratio)
colnames(fdc@data) <- c("id", "id2", "stock1", "stock2", "ratio")

# 3.1 Calcul deviation global
fdc@data$global <- (fdc@data$ratio / global) * 100

# 3.2 Calcul deviation intermédiare
stock1_med <- aggregate(fdc@data$stock1, by = list(id2 = fdc@data$id2), sum, simplify = TRUE)
stock2_med <- aggregate(fdc@data$stock2, by = list(id2 = fdc@data$id2), sum, simplify = TRUE)
med <- data.frame(c(stock1_med, stock2_med))
med$tmp <- (med$x / med$x.1)
med <- data.frame(med$id2, med$tmp)
colnames(med) <- c("id2", "medium")
fdc@data <- data.frame(fdc@data, med[match(fdc@data[, "id2"], med[, "id2"]), 2])
colnames(fdc@data) <- c("id", "id2", "stock1", "stock2", "ratio", "global", "medium")
fdc@data$medium <- (fdc@data$ratio / fdc@data$medium) * 100

# 3.3 Calcul déviation locale
if (mydist == 0) {
  dist <- gIntersects(fdc, byid = TRUE, prepared = TRUE)
  row.names(dist) <- fdc@data$id
  colnames(dist) <- fdc@data$id
  dist <- melt(dist, variable.name = 1, value.name = "fij", na.rm = TRUE)
  colnames(dist) <- c("i", "j", "cij")
  dist <- dist[dist$cij == TRUE, ]
} else {
  centres <- gCentroid(fdc, byid = TRUE, id = NULL)
  dist <- gWithinDistance(centres, byid = TRUE, dist = mydist)
  row.names(dist) <- fdc@data$id
  colnames(dist) <- fdc@data$id
  dist <- melt(dist, variable.name = 1, na.rm = TRUE)
  colnames(dist) <- c("i", "j", "cij")
  dist <- dist[dist$cij == TRUE, ]
}

dist <- data.frame(dist, fdc@data[match(dist[, "j"], fdc@data[, "id"]), c("stock1", "stock2")])
local_stock1 <- aggregate(dist$stock1, by = list(i = dist$i), sum, simplify = TRUE)
local_stock2 <- aggregate(dist$stock2, by = list(i = dist$i), sum, simplify = TRUE)
local <- data.frame(local_stock1, local_stock2$x)
colnames(local) <- c("id", "stock1", "stock2")
local$ratio <- local$stock1 / local$stock2
local <- data.frame(local$id, local$ratio)
colnames(local) <- c("id", "local")

fdc@data <- data.frame(fdc@data, local[match(fdc@data[, "id"], local[, "id"]), 2])
colnames(fdc@data) <- c("id", "id2", "stock1", "stock2", "ratio", "global", "medium", "local")
fdc@data$local <- (fdc@data$ratio / fdc@data$local) * 100


# ===================================
# 4. Typologie multiscalaire
# ===================================

# Types
fdc@data$tmp1 <- 0
fdc@data$tmp2 <- 0
fdc@data$tmp3 <- 0
fdc@data$typo <- 0

if (critere == "sup") {
  fdc@data$tmp1[fdc@data$global >= seuil] <- 1
  fdc@data$tmp2[fdc@data$medium >= seuil] <- 2
  fdc@data$tmp3[fdc@data$local >= seuil] <- 4
}

if (critere == "inf") {
  fdc@data$tmp1[fdc@data$global <= seuil] <- 1
  fdc@data$tmp2[fdc@data$medium <= seuil] <- 2
  fdc@data$tmp3[fdc@data$local <= seuil] <- 4
}

fdc@data$typo <- fdc@data$tmp1 + fdc@data$tmp2 + fdc@data$tmp3
fdc@data$typo <- as.factor(fdc@data$typo)

# couleurs
colours <- c("#efd9d9", "#864c04", "#9c9c28", "#af333d", "#184612", "#927a4f", "#ddd55e", "#e67786")
fdc@data$col <- colours[1]
fdc@data$col[fdc@data$typo == 1] <- colours[2]
fdc@data$col[fdc@data$typo == 2] <- colours[3]
fdc@data$col[fdc@data$typo == 3] <- colours[4]
fdc@data$col[fdc@data$typo == 4] <- colours[5]
fdc@data$col[fdc@data$typo == 5] <- colours[6]
fdc@data$col[fdc@data$typo == 6] <- colours[7]
fdc@data$col[fdc@data$typo == 7] <- colours[8]

# ===================================
# 5. Cartographie
# ===================================

cols <- as.character(fdc@data$col)
plot(fdc, col = cols)

rVal <- c("+1000", "+2000", "+3000", "+4000", "+5000", "+6000", "+7000", "+8000")
legend("bottomleft", legend = rVal, fill = colours, bty = "n", pt.cex = 1, cex = 0.7, title = paste(paste(critere, seuil), "\nDensite par Habitant"))

if (mydist == 0) {
  soustitre <- "NB : déviation locale basée sur la contiguité"
}
if (mydist > 0) {
  soustitre <- paste("NB : déviation locale basée sur la distance : ", mydist)
}
title(main = paste("Typologie Des Immigrants Externe\n", ratio_label), cex.sub = 0.7, sub = soustitre)







fdc <- readShapeSpatial("Tunisie_snuts4.shp")
donnees <- read.csv("data_immig_2008.csv", header = TRUE, sep = ";", dec = ",", encoding = "latin1", )

names(fdc@data)[1] <- "id"
names(donnees)[1] <- "id"
fdc@data <- data.frame(fdc@data, donnees[match(fdc@data[, "id"], donnees[, "id"]), ])



# Indicateurs & labels

ratioemp <- stockemp1 / stockemp2
# stockemp1_label<-"Population totale, 2004 (unit)"
# stockemp2_label<-"Superficie (unit)"
ratioemp_label <- "Taux de Population Urbaine (2002)"

# Deviation globalempe (valeur)
globalemp <- sum(stockemp1) / sum(stockemp2)

# Deviation intermediaire (champ aggregatif)
mediumemp <- fdc@data$id_snuts2

# Deviation locale (0 = contiguite, sinon distance euclidienne dans l'unité de la carte)
mydist <- 0

# Critere et seuil
critere <- "sup" # "sup" = plus grand que, "inf" = plus petit que
seuil <- 125

# ===================================
# 3. Calculs
# ===================================

# On ne garde que ce qui est utile
fdc@data <- data.frame(fdc@data$id, mediumemp, stockemp1, stockemp2, ratioemp)
colnames(fdc@data) <- c("id", "id2", "stockemp1", "stockemp2", "ratioemp")

# 3.1 Calcul deviation globalemp
fdc@data$globalemp <- (fdc@data$ratioemp / globalemp) * 100

# 3.2 Calcul deviation intermédiare
stockemp1_med <- aggregate(fdc@data$stockemp1, by = list(id2 = fdc@data$id2), sum, simplify = TRUE)
stockemp2_med <- aggregate(fdc@data$stockemp2, by = list(id2 = fdc@data$id2), sum, simplify = TRUE)
med <- data.frame(c(stockemp1_med, stockemp2_med))
med$tmp <- (med$x / med$x.1)
med <- data.frame(med$id2, med$tmp)
colnames(med) <- c("id2", "mediumemp")
fdc@data <- data.frame(fdc@data, med[match(fdc@data[, "id2"], med[, "id2"]), 2])
colnames(fdc@data) <- c("id", "id2", "stockemp1", "stockemp2", "ratioemp", "globalemp", "mediumemp")
fdc@data$mediumemp <- (fdc@data$ratioemp / fdc@data$mediumemp) * 100

# 3.3 Calcul déviation locale
if (mydist == 0) {
  dist <- gIntersects(fdc, byid = TRUE, prepared = TRUE)
  row.names(dist) <- fdc@data$id
  colnames(dist) <- fdc@data$id
  dist <- melt(dist, variable.name = 1, value.name = "fij", na.rm = TRUE)
  colnames(dist) <- c("i", "j", "cij")
  dist <- dist[dist$cij == TRUE, ]
} else {
  centres <- gCentroid(fdc, byid = TRUE, id = NULL)
  dist <- gWithinDistance(centres, byid = TRUE, dist = mydist)
  row.names(dist) <- fdc@data$id
  colnames(dist) <- fdc@data$id
  dist <- melt(dist, variable.name = 1, na.rm = TRUE)
  colnames(dist) <- c("i", "j", "cij")
  dist <- dist[dist$cij == TRUE, ]
}

dist <- data.frame(dist, fdc@data[match(dist[, "j"], fdc@data[, "id"]), c("stockemp1", "stockemp2")])
local_stockemp1 <- aggregate(dist$stockemp1, by = list(i = dist$i), sum, simplify = TRUE)
local_stockemp2 <- aggregate(dist$stockemp2, by = list(i = dist$i), sum, simplify = TRUE)
local <- data.frame(local_stockemp1, local_stockemp2$x)
colnames(local) <- c("id", "stockemp1", "stockemp2")
local$ratioemp <- local$stockemp1 / local$stockemp2
local <- data.frame(local$id, local$ratioemp)
colnames(local) <- c("id", "local")

fdc@data <- data.frame(fdc@data, local[match(fdc@data[, "id"], local[, "id"]), 2])
colnames(fdc@data) <- c("id", "id2", "stockemp1", "stockemp2", "ratioemp", "globalemp", "mediumemp", "local")
fdc@data$local <- (fdc@data$ratioemp / fdc@data$local) * 100


# ===================================
# 4. Typologie multiscalaire
# ===================================

# Types
fdc@data$tmp1 <- 0
fdc@data$tmp2 <- 0
fdc@data$tmp3 <- 0
fdc@data$typo <- 0

if (critere == "sup") {
  fdc@data$tmp1[fdc@data$globalemp >= seuil] <- 1
  fdc@data$tmp2[fdc@data$mediumemp >= seuil] <- 2
  fdc@data$tmp3[fdc@data$local >= seuil] <- 4
}

if (critere == "inf") {
  fdc@data$tmp1[fdc@data$globalemp <= seuil] <- 1
  fdc@data$tmp2[fdc@data$mediumemp <= seuil] <- 2
  fdc@data$tmp3[fdc@data$local <= seuil] <- 4
}

fdc@data$typo <- fdc@data$tmp1 + fdc@data$tmp2 + fdc@data$tmp3
fdc@data$typo <- as.factor(fdc@data$typo)

# couleurs
colours <- c("#c5efeb", "#a84099", "#e0a5d3", "#52158b", "#0c75bb", "#a4a721", "#95c906", "#659c9d")
fdc@data$col <- colours[1]
fdc@data$col[fdc@data$typo == 1] <- colours[2]
fdc@data$col[fdc@data$typo == 2] <- colours[3]
fdc@data$col[fdc@data$typo == 3] <- colours[4]
fdc@data$col[fdc@data$typo == 4] <- colours[5]
fdc@data$col[fdc@data$typo == 5] <- colours[6]
fdc@data$col[fdc@data$typo == 6] <- colours[7]
fdc@data$col[fdc@data$typo == 7] <- colours[8]

# ===================================
# 5. Cartographie
# ===================================

cols <- as.character(fdc@data$col)
plot(fdc, col = cols)

rVal <- c("+10.000", "+20.000", "+30.000", "+40.000", "+50.000", "+60.000", "+70.000", "+80.000")
legend("bottomleft", legend = rVal, fill = colours, bty = "n", pt.cex = 1, cex = 0.7, title = paste(paste(critere, seuil), "\nDensite par Habitant"))

if (mydist == 0) {
  soustitre <- "NB : Déviation locale basée sur la Corrélation"
}
if (mydist > 0) {
  soustitre <- paste("NB : déviation locale basée sur la distance : ", mydist)
}
title(main = paste("Typologie multiscalaire des Immigrants \n", ratioemp_label), cex.sub = 0.7, sub = soustitre)



# =====Education superieur



fdc <- readShapeSpatial("Tunisie_snuts4.shp")
donnees <- read.csv("data_immig_2008.csv", header = TRUE, sep = ";", dec = ",", encoding = "latin1", )

names(fdc@data)[1] <- "id"
names(donnees)[1] <- "id"
fdc@data <- data.frame(fdc@data, donnees[match(fdc@data[, "id"], donnees[, "id"]), ])

# ===================================
# 2. Variables
# ===================================

# Indicateurs & labels

ratioedusup <- stockedusup1 / stockedusup2
# stockedusup1_label<-"Population totale, 2004 (unit)"
# stockedusup2_label<-"Superficie (unit)"
ratioedusup_label <- "Taux d'immigration Internes (2002)"

# Deviation globaledusupe (valeur)
globaledusup <- sum(stockedusup1) / sum(stockedusup2)

# Deviation intermediaire (champ aggregatif)
mediumedusup <- fdc@data$id_snuts2

# Deviation locale (0 = contiguite, sinon distance euclidienne dans l'unité de la carte)
mydist <- 0

# Critere et seuil
critere <- "sup" # "sup" = plus grand que, "inf" = plus petit que
seuil <- 125

# ===================================
# 3. Calculs
# ===================================

# On ne garde que ce qui est utile
fdc@data <- data.frame(fdc@data$id, mediumedusup, stockedusup1, stockedusup2, ratioedusup)
colnames(fdc@data) <- c("id", "id2", "stockedusup1", "stockedusup2", "ratioedusup")

# 3.1 Calcul deviation globaledusup
fdc@data$globaledusup <- (fdc@data$ratioedusup / globaledusup) * 100

# 3.2 Calcul deviation intermédiare
stockedusup1_med <- aggregate(fdc@data$stockedusup1, by = list(id2 = fdc@data$id2), sum, simplify = TRUE)
stockedusup2_med <- aggregate(fdc@data$stockedusup2, by = list(id2 = fdc@data$id2), sum, simplify = TRUE)
med <- data.frame(c(stockedusup1_med, stockedusup2_med))
med$tmp <- (med$x / med$x.1)
med <- data.frame(med$id2, med$tmp)
colnames(med) <- c("id2", "mediumedusup")
fdc@data <- data.frame(fdc@data, med[match(fdc@data[, "id2"], med[, "id2"]), 2])
colnames(fdc@data) <- c("id", "id2", "stockedusup1", "stockedusup2", "ratioedusup", "globaledusup", "mediumedusup")
fdc@data$mediumedusup <- (fdc@data$ratioedusup / fdc@data$mediumedusup) * 100

# 3.3 Calcul déviation locale
if (mydist == 0) {
  dist <- gIntersects(fdc, byid = TRUE, prepared = TRUE)
  row.names(dist) <- fdc@data$id
  colnames(dist) <- fdc@data$id
  dist <- melt(dist, variable.name = 1, value.name = "fij", na.rm = TRUE)
  colnames(dist) <- c("i", "j", "cij")
  dist <- dist[dist$cij == TRUE, ]
} else {
  centres <- gCentroid(fdc, byid = TRUE, id = NULL)
  dist <- gWithinDistance(centres, byid = TRUE, dist = mydist)
  row.names(dist) <- fdc@data$id
  colnames(dist) <- fdc@data$id
  dist <- melt(dist, variable.name = 1, na.rm = TRUE)
  colnames(dist) <- c("i", "j", "cij")
  dist <- dist[dist$cij == TRUE, ]
}

dist <- data.frame(dist, fdc@data[match(dist[, "j"], fdc@data[, "id"]), c("stockedusup1", "stockedusup2")])
local_stockedusup1 <- aggregate(dist$stockedusup1, by = list(i = dist$i), sum, simplify = TRUE)
local_stockedusup2 <- aggregate(dist$stockedusup2, by = list(i = dist$i), sum, simplify = TRUE)
local <- data.frame(local_stockedusup1, local_stockedusup2$x)
colnames(local) <- c("id", "stockedusup1", "stockedusup2")
local$ratioedusup <- local$stockedusup1 / local$stockedusup2
local <- data.frame(local$id, local$ratioedusup)
colnames(local) <- c("id", "local")

fdc@data <- data.frame(fdc@data, local[match(fdc@data[, "id"], local[, "id"]), 2])
colnames(fdc@data) <- c("id", "id2", "stockedusup1", "stockedusup2", "ratioedusup", "globaledusup", "mediumedusup", "local")
fdc@data$local <- (fdc@data$ratioedusup / fdc@data$local) * 100


# ===================================
# 4. Typologie multiscalaire
# ===================================

# Types
fdc@data$tmp1 <- 0
fdc@data$tmp2 <- 0
fdc@data$tmp3 <- 0
fdc@data$typo <- 0

if (critere == "sup") {
  fdc@data$tmp1[fdc@data$globaledusup >= seuil] <- 1
  fdc@data$tmp2[fdc@data$mediumedusup >= seuil] <- 2
  fdc@data$tmp3[fdc@data$local >= seuil] <- 4
}

if (critere == "inf") {
  fdc@data$tmp1[fdc@data$globaledusup <= seuil] <- 1
  fdc@data$tmp2[fdc@data$mediumedusup <= seuil] <- 2
  fdc@data$tmp3[fdc@data$local <= seuil] <- 4
}

fdc@data$typo <- fdc@data$tmp1 + fdc@data$tmp2 + fdc@data$tmp3
fdc@data$typo <- as.factor(fdc@data$typo)

# couleurs
colours <- c("#e1edd4", "#9bd5e0", "#72a9c3", "#40a5b3", "#8c9e67", "#3cbc6b", "#276547", "#5d7bcc")
fdc@data$col <- colours[1]
fdc@data$col[fdc@data$typo == 1] <- colours[2]
fdc@data$col[fdc@data$typo == 2] <- colours[3]
fdc@data$col[fdc@data$typo == 3] <- colours[4]
fdc@data$col[fdc@data$typo == 4] <- colours[5]
fdc@data$col[fdc@data$typo == 5] <- colours[6]
fdc@data$col[fdc@data$typo == 6] <- colours[7]
fdc@data$col[fdc@data$typo == 7] <- colours[8]

# ===================================
# 5. Cartographie
# ===================================

cols <- as.character(fdc@data$col)
plot(fdc, col = cols)

rVal <- c("+7000", "+6000", "+5000", "+4000", "+3000", "+2000", "+1000", "+500")
legend("bottomleft", legend = rVal, fill = colours, bty = "n", pt.cex = 1, cex = 0.7, title = paste(paste(critere, seuil), "\n[glo] [med] [loc]"))

if (mydist == 0) {
  soustitre <- "NB : déviation locale basée sur la contiguité"
}
if (mydist > 0) {
  soustitre <- paste("NB : déviation locale basée sur la distance : ", mydist)
}
title(main = paste("Typologie multiscalaire Education \n", ratioedusup_label), cex.sub = 0.7, sub = soustitre)
