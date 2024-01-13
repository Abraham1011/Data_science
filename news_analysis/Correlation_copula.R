#-------Extracción de caracteristicas---------------
library(tidyverse)
library(ggplot2)
library(reticulate)
library(tm)
library(stringi)
library(patchwork)
library(GRIDCOPULA)

clean <- function(txt){
  txt_new <- tolower(txt)
  txt_new <- gsub("[0-9]", "", txt_new)
  txt_new <- gsub("[[:punct:]]", "", txt_new)
  #txt_new <- iconv(txt_new, "ASCII//TRANSLIT")
  # Eliminar acentos
  txt_new <- stringi::stri_trans_general(txt_new, "Latin-ASCII")
  txt_new <- gsub("\\s+", " ", txt_new)
  return(txt_new)
}

notas <- read.csv('Economic_news.csv',encoding = "UTF-8")
notas$txt <- clean(notas$Text)

stop_words <- data.frame(word = c("la","los","de","a",
                                  "que","del","las","con",
                                  "para","por","una","más",
                                  "como","desde","este",
                                  "sobre","año","entre"))




sklearn_f <- import("sklearn.feature_extraction.text")
vectorizador <- sklearn_f$TfidfVectorizer#TF-IDF
vectorizador <- vectorizador()
vectorizador <- vectorizador$fit(notas$txt)

features <- as.matrix(vectorizador$transform(notas$txt))
colnames(features) <- vectorizador$get_feature_names_out()
features <- as.data.frame(features)

#--------------------------------------------
#----------Distribuciones marginales---------

#-----------------Densidad-------------------
x <- features$mexico
y <- features$crecimiento


d1 <- ggplot(data.frame(x),aes(x = x)) +
  geom_density(color = "darkblue", fill = "lightblue") + 
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5,size=17)) + 
  labs(x = "Valor TF-IDF",
      title = "México")

d2 <- ggplot(data.frame(y),aes(x = y)) +
  geom_density(color = "darkblue", fill="lightgreen") + 
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5,size=17)) + 
  labs(x = "Valor TF-IDF",
       title = "Crecimiento")


d1 + d2 +
  plot_layout(ncol = 2, nrow = 1)


#-------Función de distribución empírica-------
Fx <- ecdf(x)
plot(Fx, main = "Distribución empírica para México",
     xlab = "Valor TF-IDF", col = "lightblue")


Fy <- ecdf(y)
plot(Fy, main = "Distribución empírica para crecimiento",
     xlab = "Valor TF-IDF", col = "lightgreen")



#---------------------------------------------
#------Relación entre ambas palabras------

X <- data.frame(x,y)


#Corelación utilizando la muestra
r1 <- ggplot(data = X) + 
  geom_point(aes(x = x, y = y),
             color = "red") + 
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5,size=11)) + 
  labs(x = "México", y= "Crecimiento",
       title = "Diagrama de dispersión (Dominio original)",
       caption = "*Correlación calculada con la muestra") + 
  geom_text(aes(x = 0.2,y = 0.17,
                label = paste("Rho: ",
                              round(cor(x,y,method = "s"),3))),
            color = "black") + 
  theme(plot.caption = element_text(color = "black",
                                     size = 8,hjust=0))

#--------------------------------------------
#------Correlación con copulas---------------
u <- Fx(x)
v <- Fy(y)

U <- cbind(u,v)

#Ajustar copulas no parametricas

#Grid copula
gc <- estimate.gridCopula(U, method = "ml", m = 5)
mosaic.grid(gc)

cor_grid <- rho.grid(gc)


#Kernel copula

library(kdecopula)

cop <- kdecop(U, method = "MR")
contour(cop, margins = "unif")
cor_kernel <- dep_measures(cop,measures = "spearman")


#--------------------------------------
r2 <- ggplot(data = data.frame(U)) + 
  geom_point(aes(x = u, y = v),
             color = "blue")  +
  theme_bw() + 
  labs(x = "U", y= "V",
       title = "Diagrama de dispersión (Dominio de las cópulas)",
       caption = "*Correlación calculada con copulas") + 
  geom_text(aes(x = 0.7,y = 0.97,
                label = paste("Rho 1: ",
                              round(cor_grid,3))),
            color = "black") + 
  geom_text(aes(x = 0.7,y = 0.93,
                label = paste("Rho 2: ",
                              round(cor_kernel,3))),
            color = "black") +
  theme(plot.title = element_text(color = "black",
                                  size = 11,hjust=0.5)) +
  theme(plot.caption = element_text(color = "black",
                                  size = 8,hjust=0)) 


r1 + r2 +
  plot_layout(ncol = 2, nrow = 1)












