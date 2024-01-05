library(TSP) 
library(geosphere)
library(ggplot2)

#-------Simular 60 ciudades-------------------
set.seed(1)
cords <- data.frame(lon = runif(60),lat = runif(60))

#-------Calclar la matriz de distancia--------
distance <- distm(cords, fun = distHaversine)

#Obtener mehor ruta
res <- solve_TSP(TSP(distance),method = "two_opt", start=1L)


orden <- as.integer(res) #Orden de la ruta 

#Graficar los resultados
ruta <- data.frame(
  X = cords$lon[orden],
  Y = cords$lat[orden]
)

ggplot(data = ruta) +
  geom_path(aes(x = X, y = Y), color = "blue") +
  geom_point(aes(x = X, y = Y), color = "red", size = 3) +
  theme_minimal() + 
  geom_label(aes(x = ruta[1,1], y = ruta[1,2], label = "Inicio")) + 
  labs(title = "Mejor ruta encontrada",
       x = "Lon", y = "Lat") + 
  theme(plot.title=element_text(hjust=0.5,size=17)) 



