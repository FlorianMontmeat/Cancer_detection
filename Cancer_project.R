install.packages("glmnet")
install.packages("ISLR")
install.packages("foreach")
install.packages("readr")
install.packages("curl")
install.packages("TIR")
install.packages("quantmod")

install.packages("DMwR")

library(glmnet)
library(ISLR)
library(class)
library(curl)
library(TIR)
library(quantmod)
library(DMwR)

path<- file.choose()
base <- read.csv(file=path, header=TRUE, sep=",", encoding="UTF-8")
head(base)

#création de la colonne test: sommes des 4 types de tests
base<-data.frame(base, test=base$Hinselmann+base$Schiller+base$Citology+ base$Biopsy)
head(base)

#pour faire une colonne avec le test>=2
base2<-data.frame(base, test2=base$test>1)
head(base2)
base2<-base2[-c(15,22,33,34,35,36,37)] #on a enlevé les 4 colonnes des 4 tests et celle de la somme + 2 colonne égale tout le temps à 0
base2[base2=='?'] <- NA 




#w<-(is.na(base2[,2])+is.na(base2[,3])+is.na(base2[,4])+is.na(base2[,5])+is.na(base2[,6])+is.na(base2[,7])+is.na(base2[,8])+is.na(base2[,9]))<1
#c<-c(1:length(w))
#c<-c*w
#c1<-which(c==0)
#c<-c[-c1]
#base2<-base2[c,]
#base2<-base2[-c(10:30)]  #on a enlevé 20 colonnes qui semblait poluer et enlevé le peu de NA qu'il y avait dans les 8 1ères colonnes

#v<-c(1:dim(base2)[1])[base2$test2<1]
#m<-length(v)
#m1 = sample.int(m, round(0.08*m))
#v<-v[-m1]

#base2<-base2[-v,]   ### on enleve des données "non" pour en avoir autant que le nombre de données "oui"

#z<-c(1:dim(base2)[1])[base2$test>0]
#z1<-length(z)
#add<-base2[z,]

#newbase<-rbind(base2, add, add, add, add, add, add, add, add)

#base2<-newbase


base2$test2<-as.numeric(base2$test2)

sapply(base2, class) # résumé des colonnes

for (j in (1:(dim(base2)[2]-1))){ # on transforme tout en chiffres
  base2[,j]<-as.numeric(as.character(base2[,j]))
}

#remplacement de NA par la moyenne
for (j in (1:(dim(base2)[2]-1))){
  for (i in (1:dim(base2)[1])){
    
    m<-mean(base2[,j][!is.na(base2[,j])])
    
    if (is.na(base2[i,j])){
      base2[i,j]<-m
    }
  }
}


base2$test2 = as.factor(base2$test2)

newData <- SMOTE(test2 ~ ., base2, perc.over = 600, perc.under=100)
newData2 <- SMOTE(test2 ~ ., base2, perc.over = 200, perc.under=100)
newData3 <- SMOTE(test2 ~ ., base2, perc.over = 400, perc.under=100)

base2<-newData
base2$test2<-as.numeric(as.character(base2$test2))


x = model.matrix(test2~., base2)[,-1] #recr?er en quelque sorte la vasse de donn?es
y = base2$test2
head(cbind(x,y)) # redonne un aper?u de la base de donn?es des x et y

#construction echantillon app et valid
nbind = nrow(x)
ind.train = sample.int(nbind, round(0.7*nbind))
x.train = x[ind.train,]
y.train = y[ind.train]
x.test = x[-ind.train,]
y.test = y[-ind.train]

#M?thode des plus proches voisins

#########################
###r�gression

#ridge
cv.out = cv.glmnet(x[ind.train, ], y[ind.train],alpha=0, family="binomial")
plot(cv.out)
lambda_opt_ridge = cv.out$lambda.min
ridge.mod.opt = glmnet(x[ind.train, ], y[ind.train],alpha=0, family="binomial", lambda = lambda_opt_ridge)
ridge.pred.opt = predict(ridge.mod.opt,newx = x[-ind.train, ], type="response")

compare1 <- data.frame(ridge.pred.opt>0.5, y[-ind.train])
compare1
#Lasso
cv.out = cv.glmnet(x[ind.train, ], y[ind.train],alpha=1, family="binomial")
plot(cv.out)
lambda_opt_lasso = cv.out$lambda.min
lasso.mod.opt = glmnet(x[ind.train, ], y[ind.train],alpha=1, family="binomial", lambda = lambda_opt_lasso)
lasso.pred.opt = predict(lasso.mod.opt,newx = x[-ind.train, ], type="response")
compare2 <- data.frame(lasso.pred.opt, y[-ind.train])
compare2
#Comparaison des coefficients
cbind(coef(ridge.mod.opt), coef(lasso.mod.opt))

#Comparaison des pr?dictions de test2
predall <- cbind.data.frame(obs=y[-ind.train], lasso=as.numeric(lasso.pred.opt), ridge=as.numeric(ridge.pred.opt))
head(predall)



#premier essai pour 5 voisins
knn1 = knn(x.train,x.test,y.train, k=5)
perreur = mean(knn1!=y.test)

#choix de k par validation crois?e
knnCV <- function(xtrain,ytrain,kvect){
  ntrain = nrow(xtrain)
  nbk = length(kvect)
  risque = rep(NA,nbk)
  for (j in 1:nbk){
    preds = rep(NA,ntrain)
    for (i in 1:ntrain){
      preds[i] = as.numeric(as.character(knn(xtrain[-i,],xtrain[i,],ytrain[-i],k=kvect[j])))
    }
    risque[j] = mean(preds!=ytrain)
  }
  khat <- kvect[which.min(risque)]
  print(cbind(k=kvect, risk=risque))
  plot(kvect, risque, type="b", pch=16)
  points(khat, min(risque), col="red")
  print(khat)
}

#on applique pour K voisins
K <- 10
kvect1 <- 1:K
khat1 <- knnCV(x.train, y.train, kvect1)
res1 <- knn(x.train, x.test , y.train ,khat1)
# Erreur de classification 
mean(res1!=y.test)
comp<-data.frame(y.test[1:length(y.test)], res1[1:length(y.test)])
comp
res1
#on estime sur Ivalid

knn11 = knn(x.train,x.test,y.train, k=1)
perreur1 = mean(knn11!=y.test)
knn12=as.integer(as.character(knn11))   #on va faire un graphique pour prouver que le kppv ne convient pas
y.test2=as.integer(as.character(y.test))#on va visualiser le couple (r?sultat,pr?diction)
plot(1:length(knn12),2*y.test2-knn12)  
#pour voir que si le patient a le cancer, on arrive (presque) jamais ? le pr?dire
#axe x --> les individus
#axe y -> le couple (res,pred) avec (1,1)->1, (1,0)->2, (0,0)->0, (0,1)->-1
knn3 = knn(x.train,x.test,y.train, k=3)
perreur3 = mean(knn3!=y.test)
knn10 = knn(x.train,x.test,y.train, k=10)
perreur10 = mean(knn10!=y.test)
erreurknn=1:K
for (i in 1:K){
  tempknn=knn(x.train,x.test,y.train, k=i)
  erreurknn[i]=mean(tempknn!=y.test)
}
plot(1:K,erreurknn) #

#######################################################
##ARBRE##
######################################################
base3 <-base2
f<-numeric(nbind)
for (i in(1:nbind)){
  if (base2$test2[i]==0){
    f[i] <- 'No'
  }
  else {f[i] <- 'Yes'}
}

base3<-data.frame(base3[,-31], cancer=(f)) #on met Yes ou No pour un 1 ou 0

sum(base3$cancer=='No')/nbind

Xapp = base3[ind.train,-31]
Yapp = base3[ind.train, 31]
Xtest = base3[-ind.train,-31]
Ytest = base3[-ind.train,31]

#install.packages("tree")
library(tree)

tree.fit <-tree(cancer~.,base3, subset=ind.train)
tree.fit
summary(tree.fit)
plot(tree.fit)
text(tree.fit)
#on a l'impression que il y a bcp de profondeur d'arbre pour rien, puisque on pr?dit de la meme fa?on
## ---- fig.width=12-------------------------------------------------------
Itrain2 <- sample(nbind,round(0.7*nbind))
tree.fit2 <- tree(cancer~.,base3, subset=Itrain2)

#par(mfrow=1:2, mar=c(4,4,2,1))
plot(tree.fit)
text(tree.fit)
plot(tree.fit2)
text(tree.fit2)

## ------------------------------------------------------------------------
install.packages("party")
library("party")
RF.fit <- cforest(cancer~.,base3, subset=ind.train, controls=cforest_control(ntree=100))
RF.fit.vimp<- varimp(RF.fit, mincriterion=1e-5)
barplot(sort(RF.fit.vimp[abs(RF.fit.vimp)>4e-5]))
Ytestpred.RF = predict(RF.fit,newdata=Xtest)
risqueclassif.RF =mean(Ytestpred.RF!=Ytest)


###############################
##Réseau de neuronne
##############################
install.packages("neuralnet")
library(neuralnet)
donnees_RN =data.frame(Xapp,cancer=Yapp)
donnees_RN$cancer = rep(0,round(0.7*nbind))
donnees_RN$cancer[Yapp=="Yes"] =1

s<- as.formula(paste("cancer~", paste(names(base3)[1:30], collapse="+")))
RN.fit1 = neuralnet(s, donnees_RN, linear.output = FALSE)
plot(RN.fit1)

prob.RN.fit1=compute(RN.fit1,Xtest)$net.result
Ytestpred.RN.fit1 =rep('No',nbind-round(0.7*nbind))
Ytestpred.RN.fit1[prob.RN.fit1>=0.5]='Yes'

risqueclassif.RN.fit1=mean(Ytestpred.RN.fit1!=Ytest)
compare2<-data.frame(Ytestpred.RN.fit1,Ytest)

#avec plusieurs couches cachées
RN.fit3=neuralnet(s,donnees_RN,linear.output = FALSE,hidden=3)
prob.RN.fit3=compute(RN.fit3,Xtest)$net.result
Ytestpred.RN.fit3=rep('No',nbind-round(0.7*nbind))
Ytestpred.RN.fit3[prob.RN.fit3>=0.5]='Yes'
plot(RN.fit3)

risqueclassif.RN.fit3=mean(Ytestpred.RN.fit3!=Ytest)

compare3<-data.frame(Ytestpred.RN.fit3, Ytest)

