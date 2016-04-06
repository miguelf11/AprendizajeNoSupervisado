setwd("C:/Users/Alex/Documents/R/DM_T3/AprendizajeNoSupervisado/datos")

a = read.csv(file = "a.csv",header= F)
a.big = read.csv(file = "a_big.csv",header=F)
good.luck = read.csv(file = "good_luck.csv",header = F)
guess = read.csv(file = "guess.csv",header = F)
h = read.csv(file = "h.csv", header = F)
help = read.csv(file = "help.csv",header = F)
moon = read.csv(file = "moon.csv",header = F)
s = read.csv(file = "s.csv",header = F)



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

modelo.k.medias = kmeans(x = a[, c("V1", "V2")],
                         centers = 3)


plot(x = a$V1,
     y = a$V2,
     col = modelo.k.medias$cluster)


points(x = modelo.k.medias$centers[, c("V1", "V2")],
       col = 1:4, pch = 19, cex = 3)



table(modelo.k.medias$cluster, a$V3)



# Copiamos el dataset en una variable nueva
entrada.num <- a

# Eliminamos la columna clase para obtener la matriz de distancia adecuada
entrada.num$V3 <- NULL

# De DataFrame a Matrix
entrada.num = as.matrix(entrada.num)

# Matriz de distancia, ?dist para otras opciones distinta a norma 2
distancia = dist(entrada.num)


# Una vez calculada la matriz de distancia podemos hacer el
# clustering llamando al método hclust.
metodo = "complete"
cluster = hclust(distancia, method = metodo)

plot(cluster)


# Seleccionamos el número de clases que queremos y cortamos el árbol en esa altural.
nclases = 3
# Cortamos
corte = cutree(cluster, k=nclases)
# Qué hay en corte?
head(corte)

plot(x = a$V1,
     y = a$V2,
     col = corte)








# Cortamos por altura ahora
corte = cutree(cluster, h=9,k=3)
# Verificamos cuántos clúster tenemos
unique(corte)


# Graficamos nuevamente
plot(x = a$V1, y = a$V1, col = corte)





# Comparamos datos$class con la salida del método
table(a$V3, corte)


head(a.big)
#para que tenga color
a.big$V3 <- a.big$V3 + 1
table(a.big$V3)





















