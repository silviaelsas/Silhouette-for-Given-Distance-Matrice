library(dplyr)
datafix$Rating<-as.numeric(datafix$Rating)
kat <- c('Genre')
ord <- c('Rating')

#Fungsi
cluster <- function (data, kat, ord){
  data2 = data %>% select(kat)
  data3 = data %>% select(ord)
  dataa = data %>% select_if(is.numeric)
  data1 = dataa[,!(names(dataa)%in%ord)]
  n = nrow(data1)
  #numerical
  L1 = list()
  for (b1 in seq_along(data1)){
    L1[[b1]] = matrix(, nrow=n, ncol=n)
    G1 = data1[[b1]]
    for (i in 1:n){
      for (j in 1:n){
        L1[[b1]][i,j] <- 1-abs(G1[i]-G1[j])/(max(G1)-min(G1))
        diag(L1[[b1]]) = 1
      }}}
  #Categorical
  L2 = list()
  for (b2 in seq_along(data2)){
    L2[[b2]] = matrix(, nrow=n, ncol=n)
    G2 = data2[[b2]]
    for (i in 1:n){
      for (j in 1:n){
        if (G2[i] == G2[j]) {
          L2[[b2]][i,j] <- 1
        }
        else{
          L2[[b2]][i,j] <- 0
        } }}} 
  #Ordinal
  L3 = list()
  for (b3 in seq_along(data3)){
    L3[[b3]] = matrix(, nrow=n, ncol=n)
    G3 = data3[[b3]]
    for (i in 1:n){
      for (j in 1:n){
        L3[[b3]][i,j] <- 1- abs(G3[i]-G3[j])/(max(G3)-min(G3))
        diag(L3[[b3]]) = 1
      }}}
  R1 = Reduce("+",L1)
  R2 = Reduce("+",L2)
  R3 = Reduce("+",L3)
  R = R1 + R2 + R3
  print(R)
}

jarak = cluster(datafix, kat, ord)
jarak


###### CLUSTERING ########
library(cluster)
library(ggdendro)
library(ggplot2)
hierarki<-hclust(as.dist(jarak),method="complete")
hierarki
dhc <- as.dendrogram(hierarki)
pic <- dendro_data(dhc, type="rectangle")
p <- ggplot(segment(pic))+geom_segment(aes(x=x,y=y,xend=xend,yend=yend))
p

fit <- cutree(hierarki, k=3)
fit


a<-list()
for (p in 1:length(fit)){
  a[[p]] = 0
  n = 0
  for (q in 1:length(fit)){
    if (fit[p] == fit[q]){
      a[[p]] = a[[p]]+jarak[[p,q]]
      n = n + 1
    }
  }
  a[[p]]=a[[p]]/n
}
a

### UNTUK 3 CLUSTER ###
b<-c()
for (i in 1:length(fit)){
  b[[i]]<-list()
  b[[i]][[1]]<-0
  b[[i]][[2]]<-0
  n1=0
  n2=0
  for (j in 1:length(fit)){
    if(fit[i]==1 & fit[j]==2){
      b[[i]][[1]] <- b[[i]][[1]] + jarak[[i,j]]
      n1=n1+1
    }
    if(fit[i]==1 & fit[j]==3){
      b[[i]][[2]] <- b[[i]][[2]] + jarak[[i,j]]
      n2=n2+1
    }
    if(fit[i]==2 & fit[j]==1){
      b[[i]][[1]] <- b[[i]][[1]] + jarak[[i,j]]
      n1=n1+1
    }
    if(fit[i]==2 & fit[j]==3){
      b[[i]][[2]] <- b[[i]][[2]] + jarak[[i,j]]
      n2=n2+1
    }
    if(fit[i]==3 & fit[j]==1){
      b[[i]][[1]] <- b[[i]][[1]] + jarak[[i,j]]
      n1=n1+1
    }
    if(fit[i]==3 & fit[j]==2){
      b[[i]][[2]] <- b[[i]][[2]] + jarak[[i,j]]
      n2=n2+1
    }
  }
  b[[i]][[1]]=b[[i]][[1]]/n1
  b[[i]][[2]]=b[[i]][[2]]/n2
}
bc <- lapply(b, function(x) x[which.min(x)])
bc

### UNTUK 2 CLUSTER ### 
b<-c()
for (i in 1:length(fit)){
  b[[i]]<-list()
  b[[i]][[1]]<-0
  n1=0
  for (j in 1:length(fit)){
    if(fit[i]==1 & fit[j]==2){
      b[[i]][[1]] <- b[[i]][[1]] + jarak[[i,j]]
      n1=n1+1
    }
    if(fit[i]==2 & fit[j]==1){
      b[[i]][[1]] <- b[[i]][[1]] + jarak[[i,j]]
      n1=n1+1
    }
  }
  b[[i]][[1]]=b[[i]][[1]]/n1
}
bc <- lapply(b, function(x) x[which.min(x)])

### Silhouette Setiap Objek ###
s <- list()
for (i in 1:length(fit)){
  s[[i]] = 1-(a[[i]]/bc[[i]][[1]])
}
s

### Silhouette Setiap Cluster ###
k=3
c1<-list()
for (i in 1:k){
  c1[[i]] = 0
  n = 0
  for(j in 1:length(fit)){
    if (fit[j]==i){
      c1[[i]]<-c1[[i]]+s[[j]]
      n = n+1
    }
  }
  c1[[i]]<-c1[[i]]/n
}
c1

### Silhouette Total ###
t = 0
for (i in 1:length(c1)){
  t = t + c1[[i]]
}
t = t/length(c1)
t