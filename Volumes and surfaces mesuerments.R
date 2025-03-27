library(ggplot2)
library(hrbrthemes)
library(viridis)

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


## EQ
ggplot(article.C.bravoi, aes(x=taxon, y=eq.pilleri.skull)) + 
  geom_boxplot() +
  geom_point(aes( shape = taxa), size = 4) +
  scale_shape_manual(values = c(1, 2, 0, 4, 5, 6, 7, 8, 9, 10, 11))+
  ylab("EQ") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=16,face="bold"))
