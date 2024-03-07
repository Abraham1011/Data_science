library(mvtnorm)
library(ggplot2)

k <- 3
d <- 2
#w.true = rep(1/k,k) 
w.true = c(.35,.35,0.3)
mu.true     = matrix(ncol = d, nrow = k)
mu.true[1,] = c(1.5,3.5)   
mu.true[2,] = c(0.5,6.6)   
mu.true[3,] = c(3,6)   
Sigma.true  = list()
Sigma.true[[1]] = matrix(c(1,-0.9,-0.9,1),ncol = d, nrow = d)   
Sigma.true[[2]] = matrix(c(1,-.9,-.9,1),ncol = d, nrow = d)   
Sigma.true[[3]] = matrix(c(1,-.9,-.9,1),ncol = d, nrow = d)

n  = 500
cc = sample(1:3, n, replace=T, prob=w.true)
x <- matrix(ncol = d, nrow = n)
for(i in 1:n){
  set.seed(i)
  x[i,] = rmvnorm(1, mu.true[cc[i],], Sigma.true[[cc[i]]])
}


df_X <- data.frame(x1 = x[,1], x2 = x[,2], class = cc)

ggplot(data = df_X) + 
  geom_point(aes(x = x1, y = x2, color = as.factor(class))) + 
  labs(color = "Class", title = "True classes") + 
  theme_bw() + 
  theme(plot.title = element_text(color = "black",
                                  size = 20,hjust=0.5),
        axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15),
        panel.border = element_blank(),  
        panel.spacing = unit(0, "lines"))

#-------------K-means---------------
k_means <- kmeans(x,centers = k)
class_k <- k_means$cluster
df_X$class_kmeans <- class_k

#-------------EM-----------------
res_em <- list()
QQ <- c()
for (em_n in 1:10) {
  mu <-  rmvnorm(k, apply(x,2,mean), var(x)) 
  
  sigma <- list()
  sigma[[1]] <- var(x)
  sigma[[2]] <- var(x)
  sigma[[3]] <- var(x)
  
  w <- rep(1,k)/k
  
  histt <- list()
  Q <- c()
  for (i in 1:20) {
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
    sigma <- list()
    for (kk in 1:k) {
      sigma[[kk]] <- matrix(0,ncol = d,nrow = d)
      for(nn in 1:n){
        diff <- as.matrix(x[nn,] - mu[kk,])
        sigma[[kk]] <- sigma[[kk]] + v[nn, kk] * (diff %*% t(diff))
      }
      sigma[[kk]] <- sigma[[kk]] / sum(v[,kk])
    }
    
    
    QQn = 0
    for(nn in 1:n){
      for(kk in 1:k){
        QQn = QQn + v[nn,kk]*(log(w[kk]) + 
                                dmvnorm(x[nn,],mu[kk,],sigma[[kk]],log=TRUE))
      }
    }
    
    Q[i] <- QQn
    histt[[i]] <- list(sigma = sigma, w = w, mu = mu,v = v)
  }
  res_em[[em_n]] <- histt
  QQ[em_n] <- QQn
}



v <- res_em[[which.max(QQ)]][[20]]$v
class_em <- apply(v, 1, which.max) 
df_X$class_em <- class_em
#--------------------------------------------


ggplot(data = df_X) + 
  geom_point(aes(x = x1, y = x2, color = as.factor(class))) + 
  labs(color = "Class", title = "True classes",
       x = "x", y = "y") + 
  theme_bw() + 
  theme(plot.title = element_text(color = "black",
                                  size = 20,hjust=0.5),
        axis.title.x = element_text(size = 15),  
        axis.title.y = element_text(size = 15),
        panel.border = element_blank(),  
        panel.spacing = unit(0, "lines"),
        legend.text=element_text(size = 15)) + 
  scale_color_manual(values = c("blue2","green2","red2"))


ggplot(data = df_X) + 
  geom_point(aes(x = x1, y = x2,
                 color = as.factor(class_kmeans )),
             size = 4) + 
  labs(color = "Class", title = "K-means",
       x = "", y = "") + 
  theme_bw() + 
  theme(plot.title = element_text(color = "black",
                                  size = 30,hjust=0.5),
        axis.title.x = element_text(size = 20),  
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_blank(),  
        panel.spacing = unit(0, "lines"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.position = "none",
        panel.grid.major = element_blank(),  # Eliminar rejillas principales
        panel.grid.minor = element_blank())+ 
  scale_color_manual(values = c("#18E2AC","#B9DEFF","#FF396E"))


ggplot(data = df_X) + 
  geom_point(aes(x = x1, y = x2, color = as.factor(class_em)),
             size = 4) + 
  labs(color = "Class", title = "Gaussian mixture model",
       x = "", y = "") +
  theme_bw() + 
  theme(plot.title = element_text(color = "black",
                                  size = 30,hjust=0.5),
        axis.title.x = element_text(size = 20),  
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.border = element_blank(),  
        panel.spacing = unit(0, "lines"),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.position = "none",
        panel.grid.major = element_blank(),  # Eliminar rejillas principales
        panel.grid.minor = element_blank())+ 
  scale_color_manual(values = c("#B9DEFF","#FF396E","#18E2AC"))

