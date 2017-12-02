##
##  R code to generate Figure 1 of
##  "Accelerate iterated filtering"
##  by D. Nguyen.
##

require(is2)

pompExample(ou2)
p.truth <- coef(ou2)

binary.file <- "ou2toy1000.rda"
if (file.exists(binary.file)) {
  load(binary.file)
} else {
  CORES <- 10     ## number of parallel processes
  JOBS <- 10     ## number of estimation runs
  NMIF <- 25      ## number of IF iterations per estimation run
  NP <- 1000       ## number of particles in pfilter operations

  require(doParallel)
  registerDoParallel(CORES)
  
  guess1 <- p.truth
  th2.range <- c(-1,1)
  th3.range <- c(-1,1)
  X1.range <- c(-5,5)
  X2.range <- c(-5,5)
  
  ## RUN IF1 TO ESTIMATE PARAMETERS
  
  tic <- Sys.time()
  mpar1 <- foreach(i=1:JOBS,
                   .packages='pomp',
                   .inorder=FALSE) %dopar% 
  {
    jobno<-123+i
    set.seed(jobno)
    th.draw <-c(alpha.2=runif(1,min=th2.range[1],max=th2.range[2]), 
                alpha.3=runif(1,min=th3.range[1],max=th3.range[2]),
                X1.0=runif(1,min=X1.range[1],max=X1.range[2]),
                X2.0=runif(1,min=X2.range[1],max=X2.range[2]))
    guess1[c('alpha.2','alpha.3','x1.0','x2.0')] <-th.draw
    
    oo<-try(mif(ou2,Nmif=NMIF,start=guess1,
                pars=c('alpha.2','alpha.3'),ivps=c('x1.0','x2.0'),
                rw.sd=c(x1.0=0.2,x2.0=0.2,
                        alpha.2=0.02,alpha.3=0.02
                      ),
                Np=NP,
                var.factor=1,
                ic.lag=10,
                method="mif",
                lag=0,
                cooling.type="geometric",
                cooling.fraction=0.3,
                max.fail=100
              )
            )
    if(class(oo)[1]=='try-error'){
      c(alpha.2=NA,alpha.3=NA)
    } else{
      coef(oo)[c('alpha.2','alpha.3')]
    }
  }
  toc <- Sys.time()
  etime1 <- toc-tic
  
  m1.out <- do.call(rbind,mpar1)
  m1.out<-m1.out[is.finite(m1.out[,1]),]
  m1.out<-t(m1.out)
  
  ## RUN IF2 TO ESTIMATE PARAMETERS
  
  tic <- Sys.time()
  mpar2 <- foreach(i=1:JOBS,
                   .packages='pomp',
                   .inorder=FALSE) %dopar% {
    jobno<-123+i
    set.seed(jobno)
    th.draw <-c(alpha.2=runif(1,min=th2.range[1],max=th2.range[2]), 
                alpha.3=runif(1,min=th3.range[1],max=th3.range[2]),
                X1.0=runif(1,min=X1.range[1],max=X1.range[2]),
                X2.0=runif(1,min=X2.range[1],max=X2.range[2]))
    guess1[c('alpha.2','alpha.3','x1.0','x2.0')] <-th.draw
    
    oo<-try(mif(ou2,Nmif=NMIF,start=guess1,
                pars=c('alpha.2','alpha.3'),ivps=c('x1.0','x2.0'),
                rw.sd=c(x1.0=0.2,x2.0=0.2,
                        alpha.2=0.02,alpha.3=0.02
                      ),
                Np=NP,
                var.factor=1,
                ic.lag=10,
                method="mif2",
                lag=0,
                cooling.type="geometric",
                cooling.fraction=0.05,
                max.fail=100
              )
            )
    if(class(oo)[1]=='try-error'){
      c(alpha.2=NA,alpha.3=NA)
    } else{
      coef(oo)[c('alpha.2','alpha.3')]
    }
  }
  toc <- Sys.time()
  etime2 <- toc-tic
  
  m2.out <- do.call(rbind,mpar2)
  m2.out<-m2.out[is.finite(m2.out[,1]),]
  m2.out<-t(m2.out)
  
  ## RUN AIF TO ESTIMATE PARAMETERS
  
  tic <- Sys.time()
  mpar3 <- foreach(i=1:JOBS,
                   .packages='is2',
                   .inorder=FALSE) %dopar% {
    jobno<-123+i
    set.seed(jobno)
    th.draw <-c(alpha.2=runif(1,min=th2.range[1],max=th2.range[2]), 
                alpha.3=runif(1,min=th3.range[1],max=th3.range[2]),
                X1.0=runif(1,min=X1.range[1],max=X1.range[2]),
                X2.0=runif(1,min=X2.range[1],max=X2.range[2]))
    guess1[c('alpha.2','alpha.3','x1.0','x2.0')] <-th.draw
    
    oo<-try(is2(ou2,Nis=NMIF,start=guess1,
                pars=c('alpha.2','alpha.3'),ivps=c('x1.0','x2.0'),
                rw.sd=c(x1.0=0.2,x2.0=0.2,
                        alpha.2=0.02,alpha.3=0.02
                      ),
                Np=NP,
                var.factor=1,
                ic.lag=10,
                method="aif",
                lag=1,
                cooling.type="geometric",
                cooling.fraction=0.3,
                max.fail=100
              )
            )
    if(class(oo)[1]=='try-error'){
      c(alpha.2=NA,alpha.3=NA)
    } else{
      coef(oo)[c('alpha.2','alpha.3')]
    }
  }
  toc <- Sys.time()
  etime3 <- toc-tic
  
  m3.out <- do.call(rbind,mpar3)
  m3.out<-m3.out[is.finite(m3.out[,1]),]
  m3.out<-t(m3.out)
  
  ## RUN IS2 TO ESTIMATE PARAMETERS
  
  tic <- Sys.time()
  mpar4 <- foreach(i=1:JOBS,
                   .packages='is2',
                   .inorder=FALSE) %dopar% {
    jobno<-123+i
    set.seed(jobno)
    th.draw <-c(alpha.2=runif(1,min=th2.range[1],max=th2.range[2]), 
                alpha.3=runif(1,min=th3.range[1],max=th3.range[2]),
                X1.0=runif(1,min=X1.range[1],max=X1.range[2]),
                X2.0=runif(1,min=X2.range[1],max=X2.range[2]))
    guess1[c('alpha.2','alpha.3','x1.0','x2.0')] <-th.draw
    
    oo<-try(is2(ou2,Nis=NMIF,start=guess1,
                pars=c('alpha.2','alpha.3'),ivps=c('x1.0','x2.0'),
                rw.sd=c(x1.0=2,x2.0=2,
                        alpha.2=.25,alpha.3=0.25
                      ),
                Np=NP,
                var.factor=1,
                ic.lag=10,
                method="is2",
                lag=1,
                cooling.type="geometric",
                cooling.fraction=0.3,
                max.fail=100
              )
            )
    if(class(oo)[1]=='try-error'){
      c(alpha.2=NA,alpha.3=NA)
    } else{
      coef(oo)[c('alpha.2','alpha.3')]
    }
  }
  toc <- Sys.time()
  etime4 <- toc-tic
  
  m4.out <- do.call(rbind,mpar4)
  m4.out<-m4.out[is.finite(m4.out[,1]),]
  m4.out<-t(m4.out)
    
  save(m1.out,etime1,
       m2.out,etime2,
       m3.out,etime3,
       m4.out,etime4,
       file="ou2toy1000.rda",compress='xz')
}

load("ou2toy1000.rda")
set.seed(577639485L)

kalman.filter <- function (y, x0, a, b, sigma, tau) {
  n <- nrow(y)
  ntimes <- ncol(y)
  sigma.sq <- sigma%*%t(sigma)
  tau.sq <- tau%*%t(tau)
  inv.tau.sq <- solve(tau.sq)
  cond.dev <- numeric(ntimes)
  filter.mean <- matrix(0,n,ntimes)
  pred.mean <- matrix(0,n,ntimes)
  pred.var <- array(0,dim=c(n,n,ntimes))
  dev <- 0
  m <- x0
  v <- diag(0,n)
  for (k in seq(length=ntimes)) {
    pred.mean[,k] <- M <- a%*%m
    pred.var[,,k] <- V <- a%*%v%*%t(a)+sigma.sq
    q <- b%*%V%*%t(b)+tau.sq
    r <- y[,k]-b%*%M
    cond.dev[k] <- n*log(2*pi)+log(det(q))+t(r)%*%solve(q,r)
    dev <- dev+cond.dev[k]
    q <- t(b)%*%inv.tau.sq%*%b+solve(V)
    v <- solve(q)
    filter.mean[,k] <- m <- v%*%(t(b)%*%inv.tau.sq%*%y[,k]+solve(V,M))
  }
  list(
    pred.mean=pred.mean,
    pred.var=pred.var,
    filter.mean=filter.mean,
    cond.loglik=-0.5*cond.dev,
    loglik=-0.5*dev
  )
}

kalman <- function (x, object, params) {
  y <- data.array(object)
  p <- params
  p[names(x)] <- x
  x0 <- init.state(object,params=p)
  a <- matrix(p[c('alpha.1','alpha.2','alpha.3','alpha.4')],2,2)
  b <- diag(1,2)
  sigma <- matrix(p[c('sigma.1','sigma.2','sigma.2','sigma.3')],2,2)
  sigma[1,2] <- 0
  tau <- diag(p['tau'],2,2)
  -kalman.filter(y,x0,a,b,sigma,tau)$loglik
}

# true coefficients
p.truth <- coef(ou2)
cat("coefficients at `truth'\n")
print(p.truth[c('alpha.2','alpha.3','x1.0','x2.0')],digits=4)

res1<- apply(m1.out, MARGIN=c(2), FUN=kalman, object=ou2, params=p.truth)

res2<- apply(m2.out, MARGIN=c(2), FUN=kalman, object=ou2, params=p.truth)

res3<- apply(m3.out, MARGIN=c(2), FUN=kalman, object=ou2, params=p.truth)

res4<- apply(m4.out, MARGIN=c(2), FUN=kalman, object=ou2, params=p.truth)


pdf("ou2-compareAIF.pdf")

k1<- density(-res1, bw=0.4)
plot(k1, main="",lty=2, col=2, axes=T,xlab='',ylab='', ylim=c(0,1), xlim=c(-486,-477.7),lwd=2)

k2<- density(-res2, bw=0.4)
lines(k2, col=1, lty=1,lwd=2)

k3<- density(-res3, bw=0.4)
lines(k3, col=4, lty=4,lwd=2)

k4<- density(-res4, bw=0.4)
lines(k4, col=5, lty=5,lwd=2)


abline(v = -477.6, col = "darkblue", lwd=2, lty=5)


XLIM <- c(-0.75,-.25)
YLIM <- c(-486,-477.7)
LINE.YAXIS <- 2
LINE.XAXIS <- 2.5
X.LABEL <- 0.87
Y.LABEL <- 0.87  

CEX.TRIANGLE <- CEX.SQUARE <- 1.5
CEX.POINTS <- 1.5
CEX.LAB <- 1.5
CEX.AXIS <- 1.2

CEX.TRIANGLE <- CEX.SQUARE <- 1
CEX.POINTS <- 1
CEX.LAB <- 1.2
CEX.AXIS <- 1.5
CEX.AXIS.NUMBERS <- 1


box()
axis(side=1,cex=CEX.AXIS.NUMBERS)
mtext(side=1,bquote("log likelihood"),line=LINE.XAXIS,cex=CEX.AXIS)

abline(h=0)
legend(-486,1, c("IF1", "IF2", "AIF","IS2"), col = c(2, 1, 4, 5),
       lty = c(2, 1, 4, 5),
       merge = TRUE, bg = "gray90")
plot.window(c(0,1),c(0,1))

dev.off()
