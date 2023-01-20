install.packages(c("dplyr","tidyverse","ggplot2","ggfortify", 
                   "FactoMineR", "Factoshiny", "factoextra", "ggpubr",
                   "viridis", "countrycode", 
                   "remotes", "pheatmap" ))
library("dplyr")
library("tidyverse")
library("ggplot2")
library("ggfortify")
library("FactoMineR")
library("Factoshiny")
library("factoextra")
library("ggpubr")
library("viridis")
library("countrycode")
library("remotes")
library("pheatmap")

        
setwd("C:/Users/lout0/OneDrive/Images/PROJETS OPENCLASSROOMS/P9_TALBI_Loutfi/DAN-P9-data/DAN-P9-data")
#chemin d'accès pour l'importation des CSV

dispo_alimentaire <- read.table(file = 'DisponibiliteAlimentaire_2017.csv', header = TRUE, sep = ',',encoding = "UTF-8")

code_distance <- read.table(file = 'code_distance.csv', header = TRUE, fill =TRUE, sep = ';', quote = "" ,encoding = "UTF-8")
colnames(code_distance)[colnames(code_distance) == "X.U.FEFF.Column1"] <- "Zone"
colnames(code_distance)[colnames(code_distance) == "distance..2..Column73"] <- "Distance(KM) depuis France"

import_poulet <- read.table(file = 'import_poulet.csv', header = TRUE, quote="", sep = ';',encoding = "UTF-8")
import_poulet$Importations..Unités. <- import_poulet$Importations..Unités. * 1000
colnames(import_poulet)[colnames(import_poulet) == "X.U.FEFF.Code"] <- "Code"

colnames(import_poulet)[colnames(import_poulet) == "Importations..Unités."] <- "Quantité importée (Unité)"

dispo_alimentaire$Quantité <- dispo_alimentaire$Quantité * 1000
dispo_alim_volaille <-  dispo_alimentaire[ grep ( "Viande de Volailles" , dispo_alimentaire$Produit ) , ] 
dispo_alim_volaille <- dispo_alim_volaille %>% select(-one_of('Produit', 'Année', 'Unité', 'X.U.FEFF.Code.Domaine', 'Domaine', 'Code.zone', 'Code.Élément', 'Code.Produit', 'Code.année','Symbole','Description.du.Symbole'))
production_animale <- read.table(file = 'production_animale.csv', header = TRUE, sep = ';', quote="" ,encoding="UTF-8")
production_animale$Quantité..Unité. <- production_animale$Quantité..Unité. * 1000
colnames(production_animale)[colnames(production_animale) == "X.U.FEFF.Code"] <- "Code"

stabilitepol2017 <- read.table(file = 'stabilite2017.csv', header = TRUE, quote="", sep = ';',encoding = "UTF-8")
colnames(stabilitepol2017)[colnames(stabilitepol2017) == "X.U.FEFF.Country.Territory"] <- "Zone"

options(scipen=999)
PIB_hab <- read.table(file = 'PIB_hab.csv', header = TRUE, sep = ';',encoding = "UTF-8", quote="")
colnames(PIB_hab)[colnames(PIB_hab) == "X.U.FEFF.Code"] <- "Code"




prod_poulet_stab = merge(x=production_animale,y=stabilitepol2017,by="Code", ALL = TRUE) 


code_distance <- read.table(file = "code_distance.csv", header = TRUE, quote="", sep = ";" ,fileEncoding="UTF-8-BOM")


Population_2017 <- read.table(file = "Population2017_2.csv", header = TRUE ,fill= TRUE, quote="" ,sep = ";",fileEncoding="UTF-8-BOM")
Population_2017$Valeur <- Population_2017$Valeur * 1000

pop_distance = merge(Population_2017,code_distance, by = "Code", ALL =TRUE)
pop_distance <-  pop_distance %>% select(-one_of('Zone.y'))
colnames(pop_distance)[colnames(pop_distance) == "Zone.x"] <- "Zone"
colnames(pop_distance)[colnames(pop_distance) == "Valeur"] <- "Population"


prod_poulet_stab_pop <- merge(x=prod_poulet_stab,y=pop_distance,by="Code", ALL = TRUE) 
prod_poulet_stab_pop <-  prod_poulet_stab_pop %>% select(-one_of('Zone.x'))

colnames(prod_poulet_stab_pop)[colnames(prod_poulet_stab_pop) == "Valeur"] <- "Quantité produite (Unité)"



prod_poulet_stab_pop_pib <- merge(x=prod_poulet_stab_pop,y=PIB_hab,by="Code", All = TRUE)
colnames(prod_poulet_stab_pop_pib)[colnames(prod_poulet_stab_pop_pib) == "Code.x"] <- "Code"
prod_poulet_stab_pop_pib <-  prod_poulet_stab_pop_pib %>% select(-one_of('Zone.y'))

df_final <- merge(x=prod_poulet_stab_pop_pib,y=import_poulet,by="Code", ALL = FALSE) 
colnames(df_final)[colnames(df_final) == "Quantité..Unité."] <- "Quantité produite (Unité)"
colnames(df_final)[colnames(df_final) == "Importations..Unités."] <- "Quantité importées (Unité)"
df_final <-  df_final %>% select(-one_of('Zone.x'))

df_final$tete_par_personne = (df_final$`Quantité produite (Unité)`) / (df_final$Population)

df_final2 <- df_final
df_final2 <- df_final2 [-c(29,69,70, 159, 115), ] #retirer Chine, Inde, Indonésie, USA, Pays-Bas


rownames(df_final2) <- df_final2[,8]

df_final2 <- df_final2%>%select(-Code, -Zone, -Region)
df_final_scaled<-as.data.frame(scale(df_final2))



# outliers <- function(x) {
#   
#   Q1 <- quantile(x, probs=.10)
#   Q3 <- quantile(x, probs=.90)
#   iqr = Q3-Q1
#   
#   upper_limit = Q3 + (iqr*1.5)
#   lower_limit = Q1 - (iqr*1.5)
#   
#   x > upper_limit | x < lower_limit
# }
# 
# remove_outliers <- function(df_final2, cols = names(df_final2)) {
#   for (col in cols) {
#     df_final2 <- df_final2[!outliers(df_final2[[col]]),]
#   }
#   df_final2
# }
# 
# df_final_out6 <-remove_outliers(df_final2)
# df_final_out7 <- remove_outliers(df_final_scaled)  

# Outliers : Belgique, Brésil, Chine, Allemagne, Guyana, Indonésie, Inde, Pays Bas, Pakistan, Trinité et Tobago, USA

res.PCA7<-PCA(df_final_scaled[,-8],graph=FALSE, ncp=7, scale.unit=FALSE)
variance <- data.frame(round(res.PCA7$eig, digits = 4)) #variance

colnames(variance)[colnames(variance) == "eigenvalue"] <- "Variance expliquée"

summary(res.PCA7)

res.PCA7$var

eig.val1 <- res.PCA7$eig
barplot(eig.val1[, 2], 
        names.arg = 1:nrow(eig.val1), 
        main = "Eboulis des valeurs propres",
        xlab = "Composantes principales",
        ylab = "Pourcentage variance",
        col ="steelblue",
        ylim = c(0,105))
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val1), eig.val1[, 3], 
      type = "b", pch = 19, col = "red")
lines(x = 1:nrow(eig.val1), eig.val1[, 2], 
      type = "b", pch = 19, col = "green")

plot.PCA(res.PCA7,choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA7, choix = "var", axes = c(1,3))
plot.PCA(res.PCA7, choix = "var", axes = c(2,3))

plot.PCA(res.PCA7,title="Graphe des individus de l'ACP",cex=0.9,cex.main=0.9,cex.axis=0.9, axes = c(1,2))
plot.PCA(res.PCA7,title="Graphe des individus de l'ACP",cex=0.9,cex.main=0.9,cex.axis=0.9, axes = c(1,3))
plot.PCA(res.PCA7,title="Graphe des individus de l'ACP",cex=0.9,cex.main=0.9,cex.axis=0.9, axes = c(2,3))

res.HCPC7<-HCPC(res.PCA7,nb.clust=4,consol=TRUE,graph=FALSE)
data_clust <- data.frame(res.HCPC7$data.clust)


heat <-data.frame( data_clust %>% group_by(clust)  %>%
  summarise(Quantité.produite..Unité. = mean(Quantité.produite..Unité.),
          Population = mean(Population),
          DistanceenKM = mean(DistanceenKM),
          PIB.par.habitant.... = mean(PIB.par.habitant....),
          Quantité.importée..Unité. = mean(Quantité.importée..Unité.),
          Indice_stabilite = mean(Indice_stabilite),
          tete_par_personne = mean(tete_par_personne),
            .groups = 'drop'))

pheatmap(heat[,-1], display_numbers = T)

plot.HCPC(res.HCPC7,choice='tree',title='Classification ascendante hiérarchique')


cluster1<-filter(data_clust, clust==1)
cluster2<-filter(data_clust, clust==2)
cluster3<-filter(data_clust, clust==3)
cluster4<-filter(data_clust, clust==4)

df_final_scaled$Zone <- rownames(df_final_scaled)


# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = df_final_scaled[,-8], centers = k)
  model$tot.withinss
})
# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)
# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = seq(1,10,1)) + labs(title ="Méthode du coude", x = "nombre de clusters (k)", y = "total somme carrés par cluster" ) + theme_classic()

# Clustering K-means montrant le groupe de chaque individu

df_cluster2 <- data.frame(res.km2$cluster)

df_cluster2$Zone <- rownames(df_cluster2)

fviz_cluster(res.km2, data = df_final_scaled[,-8],
             palette = c("#2E9FDF", "green", "#E7B800", "#D12E34", "pink", "grey"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main= "Projection des individus sur F1 F2")

fviz_cluster(res.km2, data = df_final_scaled[,-8],
             palette = c("#2E9FDF", "green", "#E7B800", "#D12E34", "pink", "grey"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main= "Projection des individus sur F1 F3",
             axes = c(1,3))

fviz_cluster(res.km2, data = df_final_scaled[,-8],
             palette = c("#2E9FDF", "green", "#E7B800", "#D12E34", "pink", "grey"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main= "Projection des individus sur F2 F3",
             axes= c(2,3))
             
             
ggplot(data=data_clust, aes(x=clust, y=Indice_stabilite))+
  geom_boxplot(fill = c("#2E9FDF", "green", "#E7B800", "#D12E34"), alpha = 0.6)

ggplot(data=data_clust, aes(x=clust, y=Quantité.produite..Unité.))+
  geom_boxplot(fill = c("#2E9FDF", "green", "#E7B800", "#D12E34"), alpha = 0.6)

ggplot(data=data_clust, aes(x=clust, y=(Population)))+
    geom_boxplot(fill = c("#2E9FDF", "green", "#E7B800", "#D12E34"), alpha = 0.6)

ggplot(data=data_clust, aes(x=clust, y=(DistanceenKM)))+
  geom_boxplot(fill = c("#2E9FDF", "green", "#E7B800", "#D12E34"), alpha = 0.6)

ggplot(data=data_clust, aes(x=clust, y=(PIB.par.habitant....)))+
  geom_boxplot(fill = c("#2E9FDF", "green", "#E7B800", "#D12E34"), alpha = 0.6)

ggplot(data=data_clust, aes(x=clust, y=Quantité.importée..Unité.))+
  geom_boxplot(fill = c("#2E9FDF", "green", "#E7B800", "#D12E34"), alpha = 0.6)

ggplot(data=data_clust, aes(x=clust, y=(tete_par_personne)))+
  geom_boxplot(fill = c("#2E9FDF", "green", "#E7B800", "#D12E34") , alpha = 0.6)  


pheatmap(res.km2$centers, display_numbers = T)
             
require(maps)
require(viridis)
theme_set(
  theme_void()
)

world_map <- map_data("world")

world_map1 <- map_data("world")

install_github('vincentarelbundock/countrycode')

world_map$region   <- countrycode(world_map$region, origin = "country.name.en", destination = "iso3c")
df_cluster2$region <- countrycode(df_cluster2[,2], origin = "country.name.fr", destination = "iso3c")
df_cluster2 <- df_cluster2[,-2]
df_cluster2['region'][df_cluster2['region'] == 'COG'] <- 'COD'
df_cluster2[is.na(df_cluster2)] <- c("GBR", "HKG", "IRQ", "MAC", "RUS")

clustermap1 <- left_join(df_cluster2, world_map, by = 'region')

ggplot(clustermap1, aes(long, lat, group = group))+
  geom_polygon(aes(fill = factor(res.km2.cluster) ), color = "white")+
  scale_fill_manual(values = c("#2E9FDF", "green", "#E7B800", "#D12E34", "black"))      



#------------------------------------


clust1_2 <- rbind(cluster1, cluster2)
clust1_2_scaled <- scale(clust1_2[,-8])             

# -----------------------------------------------            



res.PCA8<-PCA(clust1_2_scaled,graph=FALSE, ncp=7, scale.unit=TRUE)
variance2 <- data.frame(round(res.PCA8$eig, digits = 4)) #variance

colnames(variance2)[colnames(variance2) == "eigenvalue"] <- "Variance expliquée"

summary(res.PCA8)

res.PCA8$var

eig.val1 <- res.PCA8$eig
barplot(eig.val1[, 2], 
        names.arg = 1:nrow(eig.val1), 
        main = "Eboulis des valeurs propres",
        xlab = "Composantes principales",
        ylab = "Pourcentage variance",
        col ="steelblue",
        ylim = c(0,105))
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val1), eig.val1[, 3], 
      type = "b", pch = 19, col = "red")
lines(x = 1:nrow(eig.val1), eig.val1[, 2], 
      type = "b", pch = 19, col = "green")

plot.PCA(res.PCA8,choix='var',title="Graphe des variables de l'ACP")
plot.PCA(res.PCA8, choix = "var", axes = c(1,3))
plot.PCA(res.PCA8, choix = "var", axes = c(2,3))

plot.PCA(res.PCA8,title="Graphe des individus de l'ACP",cex=0.9,cex.main=0.9,cex.axis=0.9, axes = c(1,2))
plot.PCA(res.PCA8,title="Graphe des individus de l'ACP",cex=0.9,cex.main=0.9,cex.axis=0.9, axes = c(1,3))
plot.PCA(res.PCA8,title="Graphe des individus de l'ACP",cex=0.9,cex.main=0.9,cex.axis=0.9, axes = c(2,3))

res.HCPC8<-HCPC(res.PCA8,nb.clust=3,consol=TRUE,graph=FALSE)
res.HCPC8
data_clust2 <- data.frame(res.HCPC8$data.clust)


heat2 <-data.frame( data_clust2 %>% group_by(clust)  %>%
                     summarise(Quantité.produite..Unité. = mean(Quantité.produite..Unité.),
                               Population = mean(Population),
                               DistanceenKM = mean(DistanceenKM),
                               PIB.par.habitant.... = mean(PIB.par.habitant....),
                               Quantité.importée..Unité. = mean(Quantité.importée..Unité.),
                               Indice_stabilite = mean(Indice_stabilite),
                               tete_par_personne = mean(tete_par_personne),
                               .groups = 'drop'))





pheatmap(heat2[,-1], display_numbers = T)

plot.HCPC(res.HCPC8,choice='tree', title='Classification ascendante hiérarchique')

cluster1_2<-filter(data_clust2, clust==1)
cluster2_2<-filter(data_clust2, clust==2)
cluster3_2<-filter(data_clust2, clust==3)




df_final_out8 <- data.frame(clust1_2_scaled)
df_final_out8$Zone <- rownames(df_final_out8)



# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = clust1_2_scaled , centers = k)
  model$tot.withinss
})
# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)
# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = seq(1,10,1)) + labs(title ="Méthode du coude", x = "nombre de clusters (k)", y = "total somme carrés par cluster" ) + theme_classic()

# Clustering des K-means montrant le groupe de chaque individu

set.seed(123)
res.km3 <- kmeans((df_final_out8[,-8]), 3, nstart = 25)


ggplot(data=data.frame(res.km3), aes(x= rownames_to_column(centrek), y=Indice_stabilite))+
  geom_boxplot(fill = c("#2E9FDF", "green", "#E0D901"), alpha = 0.6) + theme_classic()


df_cluster3 <- data.frame(res.km3$cluster)


df_cluster3$Zone <- rownames(df_cluster3)

data_clust3 <- data.frame(data_clust2$clust)

data_clust3$Zone <- rownames(data_clust2)

cluster1k<-filter(df_cluster3, res.km3.cluster==1)
cluster2k<-filter(df_cluster3, res.km3.cluster==2)
cluster3k<-filter(df_cluster3, res.km3.cluster==3)

df_cluster4 <- merge(df_cluster3, df_final_out8, by = 'Zone')

heat_k <-data.frame( df_cluster4 %>% group_by(res.km3.cluster)  %>%
                      summarise(Quantité.produite..Unité. = mean(Quantité.produite..Unité.),
                                Population = mean(Population),
                                DistanceenKM = mean(DistanceenKM),
                                PIB.par.habitant.... = mean(PIB.par.habitant....),
                                Quantité.importée..Unité. = mean(Quantité.importée..Unité.),
                                Indice_stabilite = mean(Indice_stabilite),
                                tete_par_personne = mean(tete_par_personne),
                                .groups = 'drop'))
pheatmap(heat_k[,-1], display_numbers = T)

fviz_cluster(res.km3, data = df_final_out8[,-8],
             palette = c("green", "#F0BC00", "#2E9FDF"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main= "Projection des individus sur F1 F2")

fviz_cluster(res.km3, data = df_final_out8[,-8],
             palette = c("green", "#E0D901", "#2E9FDF"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main= "Projection des individus sur F1 F3",
             axes = c(1,3))

fviz_cluster(res.km3, data = df_final_out8[,-8],
             palette = c("green", "#E0D901", "#2E9FDF"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main= "Projection des individus sur F2 F3",
             axes= c(2,3))

library('ggplot2')
ggplot(data=data_clust2, aes(x=clust, y=Indice_stabilite))+
  geom_boxplot(fill = c("green", "#F0BC00", "#2E9FDF"), alpha = 0.6) + theme_classic()

ggplot(data=data_clust2, aes(x=clust, y=Quantité.produite..Unité.))+
  geom_boxplot(fill = c("green", "#F0BC00", "#2E9FDF" ), alpha = 0.6) + theme_classic()

ggplot(data=data_clust2, aes(x=clust, y=(Population)))+
  geom_boxplot(fill = c("green", "#F0BC00", "#2E9FDF"), alpha = 0.6) + theme_classic()

ggplot(data=data_clust2, aes(x=clust, y=(DistanceenKM)))+
  geom_boxplot(fill = c("green", "#F0BC00", "#2E9FDF"), alpha = 0.6) + theme_classic()

ggplot(data=data_clust2, aes(x=clust, y=(PIB.par.habitant....)))+
  geom_boxplot(fill = c("green", "#F0BC00", "#2E9FDF"), alpha = 0.6) + theme_classic()

ggplot(data=data_clust2, aes(x=clust, y=Quantité.importée..Unité.))+
  geom_boxplot(fill = c("green", "#F0BC00", "#2E9FDF"), alpha = 0.6) + theme_classic()

ggplot(data=data_clust2, aes(x=clust, y=(tete_par_personne)))+
  geom_boxplot(fill = c("green", "#F0BC00", "#2E9FDF") , alpha = 0.6) + theme_classic()

world_map1 <- map_data("world")
world_map1$region   <- countrycode(world_map1$region, origin = "country.name.en", destination = "iso3c")
df_cluster3$region <- countrycode(df_cluster3[,2], origin = "country.name.fr", destination = "iso3c")
df_cluster3 <- df_cluster3[,-2]
df_cluster3[is.na(df_cluster3)] <- c("GBR","MAC", "HKG")


clustermap2 <- left_join(df_cluster3, world_map1, by = 'region')


#worldmap des clusters avec la méthode K-means

ggplot(clustermap2, aes(long, lat, group = group))+
  geom_polygon(aes(fill = factor(res.km3.cluster) ), color = "white")+
  scale_fill_manual(values = c( "green","#E0D901", "#2E9FDF"))



