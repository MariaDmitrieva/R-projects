library(dplyr)
library(readxl)
library(bindr)
library(rgl)
library(proxy)
library(MASS)
library(ConsRank)
###### 
countcombination=function(N){
  
  indici_1=matrix(0,1,1)
  indici_2=matrix(0,N-1,1)
  i=1
  
  while (i < N) {
    m = matrix(0,(N-i),1)
    for(j in 1:(N-i)){
      m[j] = i
    }
    indici_1=rbind(indici_1,m)
    i=i+1
  }
  
  indici_1=as.matrix(indici_1[-(1)])
  
  for(i in 1:(N-1)){
    indici_2[i]=i+1
  }
  for (k in 2:(N-1)){
    m = matrix(0,(N-k),1)
    m = tail.matrix(indici_2, (N-k))
    indici_2=rbind(indici_2,m)
    k=k+1
  }
  ynew =cbind(indici_1, indici_2)
  
}
####
findconsensusBBupd = function(cij) {
  
  X=mat.or.vec(1,ncol(cij)) + 1
  N=ncol(X)
  indici=countcombination(N)
  for (j in 1:nrow(indici)) {
    if ( sign(cij[indici[j,1],indici[j,2]]) == 1 & sign(cij[indici[j,2],indici[j,1]]) == -1 ) {
      X[indici[j,1]]=X[indici[j,1]]+1
    } else if ( sign(cij[indici[j,1],indici[j,2]]) == -1 & sign(cij[indici[j,2],indici[j,1]]) == 1 ) {
      X[indici[j,2]]=X[indici[j,2]] + 1
    } else if (sign(cij[indici[j,1],indici[j,2]]) == -1 & sign(cij[indici[j,2],indici[j,1]]) == -1 ) {
      X[indici[j,1]]= NA
    } else if (sign(cij[indici[j,1],indici[j,2]]) == 1 & sign(cij[indici[j,2],indici[j,1]]) == 1 ){
      X[indici[j,1]]=X[indici[j,1]]+1
      X[indici[j,2]]=X[indici[j,2]] + 1
    }
    
  }
  
  X=(N+1)-X;
  return(X)
}
######
# NP - численность особей в популяции
# L - Предел поколений: максимальное количество последовательных поколений без улучшения
# FF - коэффициен мутации
# CR - Диапазон кроссовера
# FULL = False - матрица неполная
# Wk = NULL отсутсвуют веса судий
# Матрица  Solution содержит в себе номер компании, дату рейтинга и значение консенсуса
# Tau - усредненный коэффициент корреляции ранга TauX
tic = proc.time()[3]
NP=15 
L=100
FF=0.4
CR=0.9
FULL=FALSE
Wk=NULL
# Необходимо дать ссылку на исходную таблицу 
#file.choose()
X <- read_excel("/Users/maria/Desktop/rankings/7 агенств, уникальные, больше одного рейтинга, загрубленные/7agencies_unique_more_than_one_rougth.xlsx")
data_name=cbind(X[8],X[,9])
X= X[,-c(8:9)]
l <- nrow(X)
X <- mapply(X, FUN=as.numeric)
X <- matrix(data=X, ncol=7, nrow=l)


X=t(X)
M = nrow(X)
N=ncol(X)

cij = combinpmatr(X)
NJ=nrow(X)

N=nrow(cij)        # колличество обьектов ранжирования                    
costs       = matrix(0,1,NP) 
# популяция (случайная выборка)
population = matrix(0,(NP-1),N)
for (k in 1:(NP-1)){ population[k,] = sample(N)}  
population=rbind(population,findconsensusBBupd(cij))
costs=matrix(0,NP,1)
taos=costs 

for (i in 1:NP){
  
  COTA=combincost(population[i,],cij,NJ)
  costs[i]=COTA$cp
  taos[i]=COTA$tp
}  

# Сохраняем наиболее подходящих представителей выборки
bestc = min(costs)
bestind = which(costs==min(costs))
bestT = max(taos)
besti=population[bestind,]


# число поколений
g = 2

no_gain = 0


while (no_gain < L){
  
  
  # Мутации в популяции
  for (i in 1:NP){
    
    
    # применяем мутацию 
    evolution = mutaterand1(population,FF,i);
    
    # применяем кроссовер
    evolution = crossover(population[i,],evolution,CR)
    
    
    # применить дискретизацию и преобразовать в рейтинг
    
    if (FULL==TRUE){
      
      evolution=order(evolution)}
    
    else{
      
      evolution = childtie(evolution)
    }
    
    # Отбираем лучших 
    COTAN = combincost(evolution,cij,NJ)
    cost_new=COTAN$cp
    ta_new=COTAN$tp
    
    
    if (cost_new < costs[i]){
      population[i,] = evolution
      costs[i] = cost_new
      taos[i]=ta_new
    }
    
  }
  
  # Сохраняем лучших представителей данного поколения 
  
  bestco = min(costs)
  bestc=rbind(bestc,bestco)
  bestind = which.min(costs)
  bestTa = max(taos)
  bestT=rbind(bestT,bestTa)
  bestin=population[bestind,]
  besti=rbind(besti,bestin)
  
  
  # Проверяем, нужно ли считать дальше 
  if (bestc[g] == bestc[(g-1)]){
    
    no_gain = no_gain + 1}
  
  else{
    
    no_gain = 0
  }
  
  # следущее поколение 
  g = g + 1
  
} 

indexes = which(bestc==min(bestc))
if (FULL==TRUE){ #if1
  
  if (length(indexes)==1){ #if2
    bests=childclosint(matrix(besti[indexes,],1,N))}
  else{
    bests=matrix(0,length(indexes),N)
    for (j in 1:length(indexes)){
      bests[j,]=childclosint(besti[indexes[j],])
    } 
  } 
  
} else { #if FULL = FALSE
  
  if(length(indexes)==1){
    
    bests = reordering(matrix(besti[indexes,],1,N))
    
  } else {
    
    bests = reordering(besti[indexes,])}
  
} 

avgTau = bestT[indexes]

ConsR=unique(bests)
Tau=matrix(rep(avgTau,nrow(ConsR)),nrow(ConsR),1)

toc=proc.time()[3]
eltime1=toc-tic   # Время, затраченное на выполнение рассчетов
ConsR <- t(ConsR)
Solution <- cbind(data_name, ConsR) # Матрица решений 
X<- t(X)
# Сохнаняем все в таблицу, нужно прописать путь 
write.csv(Solution,"/Users/maria/Desktop/rankings/solution.csv")