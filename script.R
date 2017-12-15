rm(list=ls())

library(e1071)
library(MASS)
library(AtmRay)
library(varhandle)
library(rgl)

splitDataTrainAndTest<-function(xc1,xc2,proportion)
{
  
  #Definindo os limites
  c1l1<-1:(proportion[1]*nrow(xc1))
  c1l2<-(c1l1[length(c1l1)]+1):(sum(proportion[1:2])*nrow(xc1))
  #c1l3<-(c1l2[length(c1l2)]+1):(nrow(xc1))
  
  c2l1<-1:(proportion[1]*nrow(xc2))
  c2l2<-(c2l1[length(c2l1)]+1):(sum(proportion[1:2])*nrow(xc2))

  #Separando treino e teste
  x1train <- xc1[c1l1,]
  x1validation <- xc1[c1l2,]
  
  x2train <- xc2[c2l1,]
  x2validation <- xc2[c2l2,]
  
  trainY<-matrix(-1,nrow=(nrow(x1train)+nrow(x2train)),ncol=1)
  trainY[1:nrow(x1train),]<-1
  trainX <- rbind(x1train,x2train)
  
  valY<-matrix(-1,nrow=(nrow(x1validation)+nrow(x2validation)),ncol=1)
  valY[1:nrow(x1validation),]<-1
  valX<-rbind(x1validation,x2validation)
  
  output<-list(trainX=trainX,trainY=trainY,valX=valX,valY=valY)
  return(output)
}


KNN <- function(xt,X,D,K)
{
  #Cálculo das distâncias entre xt e X
  distancias<-as.matrix(dist(rbind(xt,X),method="euclidean",
                             diag=TRUE,upper=TRUE)) [,-1]
  vizinhos <- order(distancias[1,])[1:K]
  
  if (sum(D[vizinhos,1]==1)>sum(D[vizinhos,1]==-1))
  {
    D_amostra <- 1
  }else{
    D_amostra <- -1
  }
  
  return(D_amostra)
}
  
sigma1<-diag(2)*2
media1<-c(2,2)
classe1<-mvrnorm(n=50,media1,sigma1)

sigma2<-diag(2)*2
media2<-c(5,5)
classe2<-mvrnorm(n=50,media2,sigma2)

plot(classe1[,1],classe1[,2],xlim=c(-3,8),ylim=c(-3,8),col='red',
     xlab='',ylab='')
par(new=T)
plot(classe2[,1],classe2[,2],xlim=c(-3,8),ylim=c(-3,8),col='blue',
     xlab='x1',ylab='x2',main='Distribuição espacial dos dados')


acc.knn.mean<-c()
acc.knn.sd<-c()
for(j in 1:70)
{
  K<-j
  yhat<-c()
  acc<-c()
  for(it in 1:30)
  {
    #Separando dados em treino e teste
    classe1<-classe1[sample(nrow(classe1)),]
    classe2<-classe2[sample(nrow(classe2)),]
    split_data<-splitDataTrainAndTest(classe1,classe2,c(0.8,0.2))
    trainX<-split_data[[1]]
    trainY<-split_data[[2]]
    testX<-split_data[[3]]
    testY<-split_data[[4]]
    
    for(i in 1:nrow(testX))
    {
      xt<-testX[i,]
      yhat[i]<-KNN(xt,trainX,trainY,K)
    }
    acc[it]<-sum(diag(table(yhat,testY)))/(sum(table(yhat,testY)))
  }
  acc.knn.mean[j]<-mean(acc)
  acc.knn.sd[j]<-sd(acc)
  print(j)
}

plot(acc.knn.mean,type='b',xlim=c(1,70),ylab='Acurácia Média',xlab='K',
     main='Acurácia Média x Ordem do modelo')

plot(acc.knn.sd,type='b',xlim=c(1,70),ylab='Variância da Acurácia',xlab='K',
     main='Variância da Acurácia x Ordem do modelo')

plot(seq(from=1,to=60,by=5),acc.knn.mean[seq(from=1,to=60,by=5)],
     type='b',xlim=c(1,56),ylab='Acurácia Média',xlab='K',
     main='Acurácia Média x Ordem do modelo')

plot(seq(from=1,to=60,by=5),acc.knn.sd[seq(from=1,to=60,by=5)],
     type='b',xlim=c(1,56),ylab='Acurácia Média',xlab='K',
     main='Variância da acurácia x Ordem do modelo')


K<-which.max(acc.knn.mean)
#K<-1
#Plotando separação
intervalo<-seq(from=-3,to=8,by=0.2)
grid<-meshgrid(intervalo,intervalo)
grid$z<-matrix(nrow=nrow(grid$x),ncol=ncol(grid$x))
amostra<-matrix(nrow=1,ncol=2)
for(i in c(1:ncol(grid$x)))
{
  for(j in c(1:ncol(grid$y)))
  {
    amostra[1,1]<-grid$x[i,j]
    amostra[1,2]<-grid$y[i,j]
    grid$z[i,j]<-KNN(amostra,trainX,trainY,K)
  }
}

plot(classe1[,1],classe1[,2],xlim=c(-3,8),ylim=c(-3,8),col='red',
     xlab='',ylab='')
par(new=T)
plot(classe2[,1],classe2[,2],xlim=c(-3,8),ylim=c(-3,8),col='blue',
     xlab='x1',ylab='x2',main='Distribuição espacial dos dados')
par(new=T)
contour(intervalo,intervalo,grid$z,xlim=c(-3,8),ylim=c(-3,8),
        nlevels=1,drawlabels = FALSE)

persp3d(grid$x,grid$y,grid$z,alpha=0.5,col='lightblue',
       xlab='X',ylab='Y',zlab='Sinc(r)')
points3d(classe1[,1],classe1[,2],0.3,col='red',size=6)
points3d(classe2[,1],classe2[,2],0.3,col='blue',size=6)
