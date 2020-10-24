### Inferencia Ecologica para las elecciones a Presidente 2020 en Salta
#### Author: Franco Galeano
#### Init: 25 Oct 2020


## Script Original de Ernesto Calvo
## Link: https://www.dropbox.com/s/05wlfh88mxtl5jo/Codigo%20y%20Data%20Transferencias%20PASO-General%20para%20R-2019.zip?dl=0&file_subpath=%2FCodigo+Transferencias+PASO-General+para+R-2019.txt
## Posee pequeñas adaptaciones para adaptarlo a esta eleccion


## LAS FUNCIONES Y APLICACIONES NO ESTAN REALIZADAS DE FORMA "TIDY"

# Ecological Inference in the RxC case
# CALL.DIFP
# Calculates penalty for given parameters
# p     - parameter vector R x (C-1)
# mx    - Column marginals
# my    - row marginals
# nR    - number of rows
# nC    - number of columns
# nP    - number of precincts
# const - weight for penalty

wallTime <- function(expr) system.time(expr)[3]

library(boot);

call.difp <- function(p,mx,my,covar,nR,nC,nP,const)
{
  pen = 0;
  d <- seq(from=0,to=0, length=nR*(nC-1));
  g <- p[1:(nR*(nC-1))];
  if(is.numeric(covar)) {  
    d <- p[(nR*(nC-1)+1):(2*nR*(nC-1))];
    gamma <- array(0,c(nR,nC-1,nP));
    diff <- 0;
    for(i in 1:nP) {
      temp = 0;
      if(is.numeric(covar)) temp = covar[i];
      gamma[,,i] <- matrix(data=g+temp*d,nrow=nR,ncol=nC-1,byrow=T);
      expo <- exp(gamma[,,i]);
      if(nC!=2) {
        ebeta <- exp(gamma[,,i])/(1 + apply(exp(gamma[,,i]),1,sum));
      }
      else {
        ebeta <- exp(gamma[,,i])/(1 + exp(gamma[,,i]));
      }
      yhat  <- mx[i,] %*% ebeta;
      diff <- diff + sum((yhat-my[i,-C])^2);
      # diff <- diff + sum((yhat-my[i,-nC])^2) + (const*sum(gamma[,,i]^2));
      # diff <- diff + sum((yhat-my[i,-C])^2) + (10000*sum(gamma[,,i]^2));
    }
  }
  else {
    gamma <- matrix(data=g,nrow=nR,ncol=nC-1,byrow=T);
    expo <- exp(gamma);
    ebeta <- exp(gamma)/(1 + apply(exp(gamma),1,sum));
    yhat  <- mx %*% ebeta;
    diff <- sum((yhat-my[,-nC])^2) + (const*sum(gamma^2));
  }
  return(diff);
}


# Penalized Least Square Minimizer
# PARAMS.ESTIM
# Estimates parameters minimizing the penalized least squares criterion
# x       - index (optional, for bootstrapping)
# data    - marginals (optionally with covariates)
# nR      - number of rows
# nC      - number of columns
# const   - weight for penalty
# parSeed - Seed for parameters (optional)
"params.estim" <- function(data,x=-1,nR,nC,const=0.001,parSeed=-1)
{
  
  if(x[1]==-1) x = 1:nrow(data);
  
  mx <- data[x,1:nR];
  my <- data[x,(nR+1):(nR+nC)];
  nP <- nrow(data);
  covar = F;
  if(ncol(data)>nR+nC){
    covar <- data[x,nR+nC+1];
    if(parSeed[1]==-1) parSeed = rnorm(2*nR*(nC-1));
  }
  else {
    if(parSeed[1]==-1) parSeed = rnorm(nR*(nC-1));
  }
  fit <- optim(parSeed, fn = call.difp, method="L-BFGS-B", covar = covar, nR = nR, nC = nC, nP = nP, mx = mx, my = my, const = const);
  #, method="L-BFGS-B", method="SANN"
  return(fit$par);
}

# Calculate Fractions
# CALC.FRACTIONS
# Calculate fractions from the parameters
# p     - parameters
# nR    - number of rows
# nC    - number of columns
# covar - (Optional) Vector of covariates
"calc.fractions" <- function(p,nR,nC,covar=F)
{
  d <- seq(from=0,to=0, length=nR*(nC-1));
  g <- p[1:(nR*(nC-1))];
  if(is.numeric(covar)) {
    nP = length(covar);
    ests <- array(0,c(nR,nC,nP));
    d <- p[(nR*(nC-1)+1):(2*nR*(nC-1))];
    for(i in 1:nP) {
      p.exp <- exp(g + d*covar[i]);
      p.matrix <- matrix(p.exp,nrow=nR,byrow=T);
      p.sums <- apply(p.matrix,1,sum);
      p.sums <- p.sums + 1;
      p.less <- p.matrix/p.sums;
      ests[,,i] <- cbind(p.less, 1 - apply(p.less,1,sum));
    }
  }
  else {
    p.exp <- exp(g);
    p.matrix <- matrix(p.exp,nrow=nR,byrow=T);
    p.sums <- apply(p.matrix,1,sum);
    p.sums <- p.sums + 1;
    p.less <- p.matrix/p.sums;
    ests <- cbind(p.less, 1 - apply(p.less,1,sum));
  }
  return(ests);
}

# Bootstrapping
# PARAMS.BOOT
# data        - marginals (optionally, with covariates)
# nR          - number of rows
# nC          - number of columns
# bootSamples - number of bootstrap samples
"params.boot" <- function(data,nR,nC,bootSamples)
{
  output = boot(data=data,statistic=params.estim,R=bootSamples,nR=nR,nC=nC);
  return(output);
}


# Calculate Fractions
# CALC.FRACTIONS
# Calculate fractions from the parameters
# p     - parameters
# nR    - number of rows
# nC    - number of columns
# covar - (Optional) Vector of covariates
"calc.fractions" <- function(p,nR,nC,covar=F)
{
  d <- seq(from=0,to=0, length=nR*(nC-1));
  g <- p[1:(nR*(nC-1))];
  if(is.numeric(covar)) {
    nP = length(covar);
    ests <- array(0,c(nR,nC,nP));
    d <- p[(nR*(nC-1)+1):(2*nR*(nC-1))];
    for(i in 1:nP) {
      p.exp <- exp(g + d*covar[i]);
      p.matrix <- matrix(p.exp,nrow=nR,byrow=T);
      p.sums <- apply(p.matrix,1,sum);
      p.sums <- p.sums + 1;
      p.less <- p.matrix/p.sums;
      ests[,,i] <- cbind(p.less, 1 - apply(p.less,1,sum));
    }
  }
  else {
    p.exp <- exp(g);
    p.matrix <- matrix(p.exp,nrow=nR,byrow=T);
    p.sums <- apply(p.matrix,1,sum);
    p.sums <- p.sums + 1;
    p.less <- p.matrix/p.sums;
    ests <- cbind(p.less, 1 - apply(p.less,1,sum));
  }
  return(ests);
}

# Bootstrapping
# PARAMS.BOOT
# data        - marginals (optionally, with covariates)
# nR          - number of rows
# nC          - number of columns
# bootSamples - number of bootstrap samples
"params.boot" <- function(data,nR,nC,bootSamples)
{
  output = boot(data=data,statistic=params.estim,R=bootSamples,nR=nR,nC=nC);
  return(output);
}

######################
##
## Cerrar Funcion 
##
######################

## Cargo Base de datos
# Procurar que no tenga NA's 
data1<-read.csv("data/base_inferencia_ecologica_Bolivia201920.csv")


dsPpre <- data1[,9:17] #De que col a que col van los rtados de las elecciones 2019
dsPpre <- dsPpre/data1[,7] #n de col de electores
dsPpre <- cbind(dsPpre,1-rowSums(dsPpre))
dsPpre <- as.matrix(dsPpre)

dsGpre <- data1[,18:22] #de que col a que col van los rtados de las elecciones 2020
dsGpre <- dsGpre/data1[,7] #n de col de electores
dsGpre <- cbind(dsGpre,1-rowSums(dsGpre))
dsGpre<- as.matrix(dsGpre)

#Paso a General
newdata<-as.matrix(cbind(dsPpre,dsGpre))
estsPG<-params.estim(newdata,nR=10,nC=6) #9 filas (1 para blancos/ausentes), 6 cols (1 para blancos/ausentes)
fracsPG<-calc.fractions(estsPG,nR=10,nC=6)
colnames(fracsPG)<-colnames(dsGpre)
rownames(fracsPG)<-colnames(dsPpre)

##Datos Summary
VotosPaso <- data1[,9:17]
VotosPaso <- cbind(VotosPaso,data1[,7]-rowSums(VotosPaso))
VotosGeneral <- data1[,18:22]
VotosGeneral <- cbind(VotosGeneral ,data1[,7]-rowSums(VotosGeneral))
colnames(VotosPaso)<-colnames(dsPpre)
colnames(VotosGeneral)<-colnames(dsGpre)


tabla <- round(fracsPG*colSums(VotosPaso),0)

tabla # No esta en formato tidy

# ahora hay un poco de hardcodeo para que quede en tidy y poder guardarla en un csv 
library(janitor)
library(tidyverse)

indice <- tibble(indice = c("Mesa (CC)", "Rodriguez (FPV)",
                            "Patsi (MTS)", "Cárdenas (UCS)",
                            "Morales (MAS-IPSP)","Ortiz (BDN)",
                            "Chi (PDC)", "Trigo (MNR)", "Juchani (PAN)", "No votó/Blanco"))

tabla %>%
  as_tibble() %>% 
  bind_cols(indice) %>%
  select(indice, everything()) %>%
  rename(NoVoto_Blanco = `1 - rowSums(dsGpre)`) %>% 
  write_csv("data/inferenciaEcoResultados.csv")


