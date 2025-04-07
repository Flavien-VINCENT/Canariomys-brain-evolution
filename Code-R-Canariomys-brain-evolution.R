# loads packages
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggpubr)
library(quantmod)
library(forecast)
library(SIBER)
library(ggfortify)


mesur.pca <- read.delim("mesur.pca.txt")
mesur.pca <- read.delim("ratioM.txt")
mesur.pca <- read.delim("article.C.bravoi")

## calculation of the PCA with the mesuerments:
res_pca <- PCA(mesur.pca, graph = FALSE, quali.sup = 1:3)
  
fviz_screeplot(res_pca, ncp=10)
eig.val <- get_eigenvalue(res_pca)
eig.val

# Contributions of variables to PC1
fviz_contrib(res_pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res_pca, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC3
fviz_contrib(res_pca, choice = "var", axes = 3, top = 10)

## Plot of the PCA :
fviz_pca_ind(res_pca, habillage = "taxa", geom = "point",mean.point = FALSE, addEllipses = TRUE, axes = c(1, 2)) +
    stat_chull(aes(color = taxa))

plot(res_pca, choix = "var", axes = c(1, 2))

##############################################################################

## calculation of the PCA with the ratios mesuerments:
res_pca <- PCA(ratioM, graph = FALSE, quali.sup = 1:3)

fviz_screeplot(res_pca, ncp=10)
eig.val <- get_eigenvalue(res_pca)
eig.val


# Contributions of variables to PC1
fviz_contrib(res_pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res_pca, choice = "var", axes = 2, top = 10)
# Contributions of variables to PC3
fviz_contrib(res_pca, choice = "var", axes = 3, top = 10)

## Plot of the PCA with ratios mesuerments
fviz_pca_ind(res_pca, habillage = "taxa", geom = "point", axes = c(1, 2)) +
  stat_chull(aes(color = taxa),
             addEllipses = TRUE, # Concentration ellipses
legend.title = "Groups"
)

plot(res_pca, choix = "var", axes = c(1, 2))




##############################################################################

## Endocranial volume
ggplot(article.C.bravoi, aes(x=taxon, y=vol.endocast)) + 
  geom_boxplot() +
  geom_point(aes( shape = taxa), size = 4) +
  scale_shape_manual(values = c(1, 2, 0, 4, 5, 6, 7, 8, 9, 10, 11))+
  ylab("Endocranial volume") +
  theme(axis.text=element_text(size=12),
       axis.title=element_text(size=16,face="bold"))

## Olfactory bulbs volume poucentage
ggplot(article.C.bravoi, aes(x=taxon, y=X..olfactory.bulbs)) + 
  geom_boxplot() +
  geom_point(aes( shape = taxa), size = 4) +
  scale_shape_manual(values = c(1, 2, 0, 4, 5, 6, 7, 8, 9, 10, 11))+
  ylab("olfactory bulbs volume pourcentage (%)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))


## Paraflocculi volume pourcentage
ggplot(article.C.bravoi, aes(x=taxon, y=X.petrosol.lobule)) + 
  geom_boxplot() +
  geom_point(aes( shape = taxa), size = 4) +
  scale_shape_manual(values = c(1, 2, 0, 4, 5, 6, 7, 8, 9, 10, 11))+
  ylab("Petrosal lobules volume pourcentage (%)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))


## cerebrum volume pourcentage
ggplot(article.C.bravoi, aes(x=taxon, y=X..cerebrum)) + 
  geom_boxplot() +
  geom_point(aes( shape = taxa), size = 4) +
  scale_shape_manual(values = c(1, 2, 0, 4, 5, 6, 7, 8, 9, 10, 11))+
  ylab("Cerebrum volume pourcentage (%)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))

## area neocortex %
ggplot(article.C.bravoi, aes(x=taxon, y=pourcentage.area.neocortex)) + 
  geom_boxplot() +
  geom_point(aes( shape = taxa), size = 4) +
  scale_shape_manual(values = c(1, 2, 0, 4, 5, 6, 7, 8, 9, 10, 11))+
  ylab("Neocortical surface area (%)") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))


## BM
ggplot(article.C.bravoi, aes(x=taxon, y=ornella.skull)) + 
  geom_boxplot() +
  geom_point(aes(color=taxa), size = 4) +
  ylab("BM") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))


## EQ

ggplot(article.C.bravoi, aes(x=taxon, y=eq.pilleri.skull)) + 
  geom_boxplot() +
   #scale_fill_brewer(palette="Set3")+
   scale_color_brewer(palette="Dark2")+
  geom_point(aes( shape = taxa), size = 4) +
  scale_shape_manual(values = c(1, 2, 0, 4, 5, 6, 7, 8, 9, 10, 11))+
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)+
  #stat_chull(aes(color = taxa))+
  ylab("EQ") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))


#box plots supplementary figures____________________________________________________________________

# loads packages
library(dplyr)
library(tidyr)
library(readr)

#Create dataframe in tibble format
data <- tibble(
  Measurements = c("TL", 
                   "CRMW", 
                   "CRML", 
                   "CRMH", 
                   "CLML", 
                   "CLW", 
                   "OW", 
                   "OH", 
                   "OL"),
  `Canariomys_1` = c(34.09, 21.82, 22.04, 14.49, 5.72, 24.01, 9.99, 7.72, 6.15),
  `Canariomys_2` = c(33.18, 21.13, 21.13, 13.8, 6.43, 20.78, 7.98, 6.58, 5.6),
  `Canariomys_3`= c(31.46,	21.19,	19.77,	14.3,	05.04,	18.63,	7.32,	8.27,	6.56),
  `Arvicanthis_1` = c(19.71, 12.84, 11.5, 8.76, 3.93, 11.01, 4.51, 4.73, 4.11),
  `Arvicanthis_2` = c(19.5, 12.7, 11.12, 8.85, 3.59, 10.99, 4.83, 4.96, 4.72),
  `Arvicanthis_3` = c(22.95,	13.5,	12.9,	9.14,	3.79,	11.53,	4.76,	4.79,	5.96),
  `Arvicanthis_4`= c(20.25,	13.09,	11.3,	8.85,	3.98,	11.5,	4.71,	4.74,	4.94),
)

#Turn the table format with the pipeline operator
data_long <- data %>%
  pivot_longer(cols = -Measurements, 
               names_to = "Species_Individual", 
               values_to = "Value") %>%
  separate(Species_Individual, into = c("Genus", "Individual"), sep = "_")


#plot
ggplot(data_long, aes(x = Genus, y = Value, fill = Measurements, color = Measurements)) +
  geom_boxplot( fill = "white", outlier.color = "black") +
  labs(y = "Mesuerments (mm)")


#____________________________________________________________________

# Create dataframe in tibble format
data <- tibble(
  Measurements = c("TL", 
                   "CRMW", 
                   "CRML", 
                   "CRMH", 
                   "CLML", 
                   "CLW", 
                   "OW", 
                   "OH", 
                   "OL"),
  `C. bravoi_1` = c(34.09, 21.82, 22.04, 14.49, 5.72, 24.01, 9.99, 7.72, 6.15),
  `C. bravoi_2` = c(33.18, 21.13, 21.13, 13.8, 6.43, 20.78, 7.98, 6.58, 5.6),
  `A. niloticus_1` = c(19.71, 12.84, 11.5, 8.76, 3.93, 11.01, 4.51, 4.73, 4.11),
  `A. niloticus_2` = c(19.5, 12.7, 11.12, 8.85, 3.59, 10.99, 4.83, 4.96, 4.72),
)

#Turn the table format with the pipeline operator
data_long <- data %>%
  pivot_longer(cols = -Measurements, 
               names_to = "Species_Individual", 
               values_to = "Value") %>%
  separate(Species_Individual, into = c("Species", "Individual"), sep = "_")

# Plot
ggplot(data_long, aes(x = Species, y = Value, fill = Measurements, color = Measurements)) +
  geom_boxplot( fill = "white", outlier.color = "black") +
  labs(y = "Mesuerments (mm)")

#____________________________________________________________________

#Create dataframe in tibble format
data <- tibble(
  Measurements = c("Endocranial volume", 
                   "Olfactory bulbs volume", 
                   "Petrosol lobule volume", 
                   "Cerebrum volume"),
  `C. bravoi_1` = c(6186.56,	217.415, NA,4099.51),
  `C. bravoi_2` = c(5456.54,	196.207, NA,3716.77),
  `A. niloticus_1` = c(1059.44,	37.5649,	10.75,	723.863),
  `A. niloticus_2` = c(1104.42,	46.808,	9.71,	748.059),
)

#Turn the table format with the pipeline operator
data_long <- data %>%
  pivot_longer(cols = -Measurements, 
               names_to = "Species_Individual", 
               values_to = "Value") %>%
  separate(Species_Individual, into = c("Species", "Individual"), sep = "_")

# Plot
ggplot(data_long, aes(x = Species, y = Value, fill = Measurements, color = Measurements)) +
  geom_boxplot( fill = "white", outlier.color = "black") +
  labs(y = "Volumetric measurement (mm^3)")

#____________________________________________________________________

#Create dataframe in tibble format
data <- tibble(
  Measurements = c("Endocranial volume", 
                   "Olfactory bulbs volume", 
                   "Petrosol lobule volume", 
                   "Cerebrum volume"),
  `Canariomys_1` = c(6186.56,	217.415, NA,4099.51),
  `Canariomys_2` = c(5456.54,	196.207, NA,3716.77),
  `Canariomys_3`= c(5279.46,	192.73,	37.7,	3759.69),
  `Arvicanthis_1` = c(1059.44,	37.5649,	10.75,	723.863),
  `Arvicanthis_2` = c(1104.42,	46.808,	9.71,	748.059),
  `Arvicanthis_3` = c(1415.73, 72.8371,	12.26,	934.981),
  `Arvicanthis_4`= c(1181.43,	57.5692,	10.95,	802.148),
)

#Turn the table format with the pipeline operator
data_long <- data %>%
  pivot_longer(cols = -Measurements, 
               names_to = "Species_Individual", 
               values_to = "Value") %>%
  separate(Species_Individual, into = c("Species", "Individual"), sep = "_")

# Plot
ggplot(data_long, aes(x = Species, y = Value, fill = Measurements, color = Measurements)) +
  geom_boxplot( fill = "white", outlier.color = "black") +
  labs(y = "Volumetric measurement (mm^3)")

#____________________________________________________________________

#Create dataframe in tibble format
data <- tibble(
  Measurements = c("CRML/TL", 
                   "CLML/TL", 
                   "CLW/CRMW", 
                   "OW/CRMW",
                   "OH/CRMH",
                   "OL/TL"),
  `Canariomys_1` = c(64.65, 16.78, 110.04, 45.78, 53.28, 18.04),
  `Canariomys_2` = c(63.68, 19.38, 98.34, 37.77, 47.68, 16.88),
  `Canariomys_3`= c(62.84, 16.02, 87.92, 34.54, 57.83, 20.85),
  `Arvicanthis_1` = c(58.35, 19.94, 85.75, 35.12, 54, 20.85),
  `Arvicanthis_2` = c(57.03, 18.41, 86.54, 38.03, 56.05, 24.21),
  `Arvicanthis_3` = c(56.21, 16.51, 85.41, 35.26, 52.41, 25.97),
  `Arvicanthis_4`= c(55.8, 19.65, 87.85, 35.98, 53.56, 24.4),
)

#Turn the table format with the pipeline operator
data_long <- data %>%
  pivot_longer(cols = -Measurements, 
               names_to = "Species_Individual", 
               values_to = "Value") %>%
  separate(Species_Individual, into = c("Species", "Individual"), sep = "_")

# Plot
ggplot(data_long, aes(x = Species, y = Value, fill = Measurements, color = Measurements)) +
  geom_boxplot( fill = "white", outlier.color = "black") +
  labs(y = "Volumetric ratio measurement (mm^3)")


#____________________________________________________________________

#Create dataframe in tibble format
data <- tibble(
  Measurements = c("CRML/TL", 
                   "CLML/TL", 
                   "CLW/CRMW", 
                   "OW/CRMW",
                   "OH/CRMH",
                   "OL/TL"),
  `C. bravoi_1` = c(64.65, 16.78, 110.04, 45.78, 53.28, 18.04),
  `C. bravoi_2` = c(63.68, 19.38, 98.34, 37.77, 47.68, 16.88),
  `A. niloticus_1` = c(58.35, 19.94, 85.75, 35.12, 54, 20.85),
  `A. niloticus_2` = c(57.03, 18.41, 86.54, 38.03, 56.05, 24.21),
)

#Turn the table format with the pipeline operator
data_long <- data %>%
  pivot_longer(cols = -Measurements, 
               names_to = "Species_Individual", 
               values_to = "Value") %>%
  separate(Species_Individual, into = c("Species", "Individual"), sep = "_")

# Plot
ggplot(data_long, aes(x = Species, y = Value, fill = Measurements, color = Measurements)) +
  geom_boxplot( fill = "white", outlier.color = "black") +
  labs(y = "Volumetric measurement (mm^3)")