setwd("C:/Users/Alex/Documents/R/DM_T3/AprendizajeNoSupervisado/datos")



library("rgl")

#-------------------------------------------------------------------------#
#----------------------- FUNCIONES ---------------------------------#
#--------------------------------------------------------------------#


ds.clust = function(ds,distan,metodo,nclass,altura){
  
  print(paste("el metodo es:",metodo,"usando distancia:",distan))
  # Encontramos modelo
  ds.matrix <- as.matrix(ds)
  
  distancia <- dist(x = ds.matrix , method = distan)
  
  cluster <- hclust(d = distancia,method = metodo)
  plot(cluster)
  
  rect.hclust(tree = cluster, k = nclass, border = 2)
  
  
  corte = cutree(tree = cluster, k= nclass)
  plot(x = ds$V1,
       y = ds$V2,
       col = corte,
       xlab = "X",
       ylab = "Y",
       main = paste("Plot del corte con",nclass,"clases"))
  
  
  #segundo método
  
  #cluster = hclust(distancia, method = metodo)
  #dendrogram = as.dendrogram(cluster)
  #plot(dendrogram)
  
  #cortes = cut(dendrogram, h = 26)$upper
  #plot(cortes)
  
  
  # Comparamos datos$class con la salida del método
  #print(table(predecir, corte))
}



codo.jambu= function(d){
  mydata <- d
  wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
  plot(1:15,
       wss,
       type="b",
       xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}





distancias <- c("euclidean","maximum","manhattan","binary")


hclust.metodos <- c("ward.D", "single", "complete", "average", "mcquitty",
                    "median", "centroid", "ward.D2")

# Method "centroid" is typically meant to be used with squared Euclidean distances.


#-------------------------------------------------------------------------#
#----------------------------- a.csv -------------------------------------#
#-------------------------------------------------------------------------#

a <- read.csv(file = "a.csv",header= F)
a$V3 <- a$V3 +1 
table(a$V3)

plot(x = a$V1,
     y = a$V2,
     col = a$V3,
     xlim = c(min(a$V1-0.5), max(a$V1+0.5)),
     ylim = c(min(a$V2-0.5), max(a$V2+0.5)),
     xlab = "X",
     ylab = "Y",
     main = "Clustering Rectangular del dataset a")

a.kmedias = kmeans(x = a[, c("V1", "V2")],
                         centers = 3)


plot(x = a$V1,
     y = a$V2,
     col = a.kmedias$cluster)

points(x = a.kmedias$centers[, c("V1", "V2")],
       col = 1:4, pch = 19, cex = 3)

table(a.kmedias$cluster, a$V3)



for(i in hclust.metodos){
  for(j in distancias){
    ds.clust(ds = a[,c(1,2)],distan = j, metodo = i,
             nclass = 3, altur = 26)
  }
}

#-------------------------------------------------------------------------#
#-----------------------------  FIN a.csv --------------------------------#
#-------------------------------------------------------------------------#







#-------------------------------------------------------------------------#
#------------------------------  a._big.csv ------------------------------#
#-------------------------------------------------------------------------#

a.big <- read.csv(file = "a_big.csv",header=F)

object.size(a.big)

head(a.big)
#para que tenga color
a.big$V3 <- a.big$V3 + 1
table(a.big$V3)






#-------------------------------------------------------------------------#
#---------------------------- FIN a._big.csv -----------------------------#
#-------------------------------------------------------------------------#





#-------------------------------------------------------------------------#
#------------------------------  good._luck.csv ------------------------------#
#-------------------------------------------------------------------------#

good.luck <- read.csv(file = "good_luck.csv",header = F)


head(good.luck)
#para que tenga color

table(good.luck$V11)
#2 clases , 0  y 1





#-------------------------------------------------------------------------#
#---------------------------- FIN good._luck.csv -----------------------------#
#-------------------------------------------------------------------------#






#-------------------------------------------------------------------------#
#------------------------------  guess.csv ------------------------------#
#-------------------------------------------------------------------------#

guess <- read.csv(file = "guess.csv",header = F)

head(guess)
# no tiene columna con la clase

codo.jambu(guess)


plot(x = guess$V1,
     y = guess$V2,
     xlim = c(min(guess$V1-0.5), max(guess$V1+0.5)),
     ylim = c(min(guess$V2-0.5), max(guess$V2+0.5)),
     xlab = "X",
     ylab = "Y",
     main = "Clustering Rectangular del dataset guess")


# evaluando ambos gráficos , lo mejor es formar 3 clusters

guess.kmedias = kmeans(x = guess,
                   centers = 3)


plot(x = guess$V1,
     y = guess$V2,
     col = guess.kmedias$cluster)

points(x = guess.kmedias$centers[, c("V1", "V2")],
       col = 1:4, pch = 19, cex = 3)





for(i in hclust.metodos){
  for(j in distancias){
    ds.clust(ds = guess,distan = j, metodo = i,
             nclass = 3, altur = 26)
  }
}








#-------------------------------------------------------------------------#
#---------------------------- FIN guess.csv -----------------------------#
#-------------------------------------------------------------------------#





#-------------------------------------------------------------------------#
#------------------------------  h.csv ------------------------------#
#-------------------------------------------------------------------------#

a.big <- read.csv(file = "a_big.csv",header=F)



head(a.big)
#para que tenga color
a.big$V3 <- a.big$V3 + 1
table(a.big$V3)



#-------------------------------------------------------------------------#
#---------------------------- FIN h.csv -----------------------------#
#-------------------------------------------------------------------------#




#-------------------------------------------------------------------------#
#------------------------------  help.csv ------------------------------#
#-------------------------------------------------------------------------#

help <- read.csv(file = "help.csv",header = F)

head(help)
#para que tenga color

table(help$V4)


plot3d(x = help$V1,
       y = help$V2,
       z = help$V3,
       main = "Plot del dataset help.csv")  



codo.jambu(help[ ,c(1,2,3)])

# segun el codo de jambu , 3 o 4 cluster



help.kmedias = kmeans(x = help[ ,c(1,2,3)],
                      centers = 3)


plot3d(x = help$V1,
       y = help$V2,
       z = help$V3,
       col = help.kmedias$cluster,
       main = "Plot del dataset help.csv")



help.kmedias = kmeans(x = help[ ,c(1,2,3)],
                       centers = 4)


plot3d(x = help$V1,
       y = help$V2,
       z = help$V3,
       col = help.kmedias$cluster,
       main = "Plot del dataset help.csv") 

# viendo la gráfica con k = 3 y con k =4 y viend el codo de jambu , es mejor 3





#-------------------------------------------------------------------------#
#---------------------------- FIN help.csv -----------------------------#
#-------------------------------------------------------------------------#




#-------------------------------------------------------------------------#
#------------------------------  moon.csv ------------------------------#
#-------------------------------------------------------------------------#

moon <- read.csv(file = "moon.csv",header = F)


#para que tenga color
moon$V3 <- moon$V3 + 1
table(moon$V3)


plot(x = moon$V1,
     y = moon$V2,
     col = moon$V3,
     xlim = c(min(moon$V1-0.5), max(moon$V1+0.5)),
     ylim = c(min(moon$V2-0.5), max(moon$V2+0.5)),
     xlab = "X",
     ylab = "Y",
     main = "Clustering Rectangular del dataset moon")

moon.kmedias = kmeans(x = moon[, c("V1", "V2")],
                   centers = 2)



plot(x = moon$V1,
     y = moon$V2,
     col = moon.kmedias$cluster,
     main = "Plot del k-media del dataset moon")

points(x = moon.kmedias$centers[, c("V1", "V2")],
       col = 1:4, pch = 19, cex = 3)

table(moon.kmedias$cluster, moon$V3)

# En este dataset k-media no funciona bien


for(i in hclust.metodos){
  for(j in distancias){
    ds.clust(ds = moon[ ,c(1,2)],distan = j, metodo = i,
             nclass = 2, altur = 26)
  }
}


#-------------------------------------------------------------------------#
#---------------------------- FIN moon.csv -----------------------------#
#-------------------------------------------------------------------------#





#-------------------------------------------------------------------------#
#----------------------------------  s.csv -------------------------------#
#-------------------------------------------------------------------------#

s <- read.csv(file = "s.csv",header = F)



head(a.big)
#para que tenga color
a.big$V3 <- a.big$V3 + 1
table(a.big$V3)



#-------------------------------------------------------------------------#
#---------------------------- FIN s.csv -----------------------------#
#-------------------------------------------------------------------------#


