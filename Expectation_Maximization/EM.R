library(mvtnorm)    
library(ellipse) 
library(ggplot2)
library(gganimate)



#------------------EM----------------------
#------------------------------------------
x <- faithful
n <- dim(x)[1]
k <- 2
d <- 2

set.seed(1011)
mu <-  rmvnorm(k, apply(x,2,mean), var(x)) 

sigma <- list()
sigma[[1]] <- var(x)/k
sigma[[2]] <- var(x)/k

w <- rep(1,k)/k

histt <- list()
for (i in 1:30) {
  v <- matrix(ncol = k,nrow = n)
  for(kk in 1:k){
    v[,kk] <- log(w[kk]) + 
      dmvnorm(x, mean = mu[kk,], 
              sigma = sigma[[kk]], log = TRUE)
  }
  for(nn in 1:n){
    v[nn,] = exp(v[nn,] - max(v[nn,]))/
      sum(exp(v[nn,] - max(v[nn,])))  #Go from logs to actual weights in a numerically stable manner
  }
  #v <- t(apply(v, 1, function(x) x/sum(x)))
  w <- colMeans(v)
  
  mu <- matrix(ncol = d, nrow = k)
  for(kk in 1:k){
    mu[kk,] <- colSums(v[,kk] * x)/sum(v[,kk])
  }
  print(w)
  sigma <- list()
  for (kk in 1:k) {
    sigma[[kk]] <- matrix(0,ncol = d,nrow = d)
    for(nn in 1:n){
      diff <- as.matrix(x[nn,] - mu[kk,])
      sigma[[kk]] <- sigma[[kk]] + v[nn, kk] * (t(diff) %*% diff)
    }
    sigma[[kk]] <- sigma[[kk]] / sum(v[,kk])
  }
  
  histt[[i]] <- list(sigma = sigma, w = w, mu = mu)
}

#------------------------------------------
#------------------------------------------

df1_95 <- data.frame(matrix(ncol = 3))[-1,]
colnames(df1_95) <- c(colnames(x),"iter")

df1_60 <- data.frame(matrix(ncol = 2))[-1,]
colnames(df1_60) <- c("x1_60","y1_60")

df2_60 <- data.frame(matrix(ncol = 2))[-1,]
colnames(df2_60) <- c("x2_60","y2_60")

df2_95 <- data.frame(matrix(ncol = 3))[-1,]
colnames(df2_95) <- c(colnames(x),"iter")

for(i in 1:length(histt)){
  ellipse1_95 <- ellipse(x = histt[[i]]$sigma[[1]], 
                      centre = histt[[i]]$mu[1,], level = 0.95)
  e1_95 <- as.data.frame(ellipse1_95)
  df_aux <- cbind(e1_95, iter = rep(i,dim(e1_95)[1]))
  df1_95 <- rbind(df1_95,df_aux)
  
  ellipse1_60 <- ellipse(x = histt[[i]]$sigma[[1]], 
                         centre = histt[[i]]$mu[1,], level = .60)
  e1_60 <- as.data.frame(ellipse1_60)
  colnames(e1_60) <- colnames(df1_60)
  df1_60 <- rbind(df1_60,e1_60)
  
  
  ellipse2_60 <- ellipse(x = histt[[i]]$sigma[[2]], 
                         centre = histt[[i]]$mu[2,], level = .60)
  e2_60 <- as.data.frame(ellipse2_60)
  colnames(e2_60) <- colnames(df2_60)
  df2_60 <- rbind(df2_60,e2_60)
  
  ellipse2_95 <- ellipse(x = histt[[i]]$sigma[[2]], 
                         centre = histt[[i]]$mu[2,], level = 0.95)
  e2_95 <- as.data.frame(ellipse2_95)
  df_aux2 <- cbind(e2_95, iter = rep(i,dim(e2_95)[1]))
  df2_95 <- rbind(df2_95,df_aux2)
  
}


df <- cbind(df1_95,df2_95,df1_60,df2_60)
colnames(df) <- c("x","y","iter1","x2","y2",
                  "iter2","x1_60","y1_60","x2_60","y2_60")

g <- ggplot(df) +
  annotate(geom = "point", x = x$eruptions, y = x$waiting,
           color = "#9DFF8E", size = 3) +
  geom_path(aes(x = x, y = y),
            color = "blue", linetype = "dashed",
            size = 1) +
  geom_path(aes(x = x1_60, y = y1_60), linetype = "dashed",
            color = "blue",
            size = 1) +
  geom_path(aes(x = x2, y = y2),
            color = "red", linetype = "dashed", size = 1) +
  geom_path(aes(x = x2_60, y = y2_60),
            color = "red", linetype = "dashed", size = 1)  + 
  theme_bw() + 
  theme(plot.title = element_text(color = "black",
                                  size = 35,hjust=0.5),
        axis.title.x = element_text(size = 30),  # Tamaño del nombre del eje x
        axis.title.y = element_text(size = 30),
        panel.border = element_blank(),  # Eliminar bordes del panel
        panel.spacing = unit(0, "lines")) +
  transition_manual(iter1) + 
  labs(title = "EM iteración {current_frame}", y = "Waiting",
       x = "Eruptions")

animate(g,fps = 15,renderer = av_renderer(),
        width = 600,
        height = 400)

anim_save("animacion.mp4", g,
          renderer = av_renderer(),width = 700,
          height = 700,fps = 15)



ggplot(x, aes(x =eruptions,fill = "A")) +
  geom_density(fill = "#9DFF8E", color = "#9DFF8E",
               alpha = 0.5) + 
  theme_void(
    base_size = 10,
    base_line_size =2,
    base_rect_size =2)

ggplot(x, aes(x =waiting,fill = "A")) +
  geom_density(fill = "#9DFF8E", color = "#9DFF8E",
               alpha = 0.5) + 
  theme_void(
    base_size = 10,
    base_line_size =2,
    base_rect_size =2)

