#######################################################################################################
##### R function for doing stepwise/backward/forward model selection using F-statistics criterion #####
#######################################################################################################


######### Stepwise regression variable selection using F-stat for lm model
## object: the initial linear model fitting 
## scope: a model formula specifying all variables considered in the model selection
## fput: a model formula specifying which variables must be included
## direction: stepwise/forward/backward regression
## Fin,Fout: the cutoff values for the F-statistics criteria
## trace: indicator of whether printing the detailed output
## steps: hard-coded maximum number of iterations, 1e3 is enough for most dataset
stepF <- function(object, scope, fput=NULL, direction = c("both", "backward", "forward"),
                  Fin=4.0, Fout=3.9, trace = 1, steps = 1000, ...){
  cut.string <- function(string){
    if(length(string) > 1)
      string[-1] <- paste("\n", string[-1], sep = "")
    string
  }

  step.results <- function(models, fit, object){
    change <- sapply(models, "[[", "change")
    rdf <- sapply(models, "[[", "df.resid")
    ddf <- c(NA, diff(rdf))
    SSE <- sapply(models, "[[", "SSE")
    heading <- c("Stepwise Model Path \nAnalysis of Variance Table",
                 "\nInitial Model:", deparse(as.vector(formula(object))),
                 "\nFinal Model:", deparse(as.vector(formula(fit))),
                 "\n")
    aod <- data.frame(Step = I(change), Df = ddf, "Resid. Df" = rdf, 
                      SSE = SSE, check.names = FALSE)
    attr(aod, "heading") <- heading
    fit$anova <- aod
    fit
  }

  Terms <- terms(object)
  object$call$formula <- object$formula <- Terms
  md <- missing(direction)
  direction <- match.arg(direction)
  backward <- direction == "both" | direction == "backward"
  forward  <- direction == "both" | direction == "forward"
  if(missing(scope)){
    fdrop <- numeric(0)
    fadd <- attr(Terms, "factors")
    if(md) forward <- FALSE
  } else{
    if(is.list(scope)){
      fdrop <-
        if(!is.null(fdrop <- scope$lower))
          attr(terms(update.formula(object, fdrop)), "factors")
        else numeric(0)
      fadd <-
        if(!is.null(fadd <- scope$upper))
          attr(terms(update.formula(object, fadd)), "factors")
    } else{
      fadd <-
        if(!is.null(fadd <- scope))
          attr(terms(update.formula(object, scope)), "factors")
      fdrop <- numeric(0)
    }
  }
  if( !missing(fput) ){
    fput = attr(terms(update.formula(object, fput)), "term.labels")
  }
  models <- vector("list", steps)
  n <- length(object$residuals)
  fit <- object
  bSSE = deviance(fit)
  edf <- fit$df.resid
  nm <- 1
  Terms <- fit$terms
  if(trace)
    cat("Start:  SSE=", format(round(bSSE, 2)), "\n",
        cut.string(deparse(as.vector(formula(fit)))), "\n\n")
  
  models[[nm]] <- list(df.resid = edf, change = "", SSE = bSSE)
  while(steps > 0){
    steps <- steps - 1
    SSE <- bSSE
    ffac <- attr(Terms, "factors")
    scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
    scope$add = setdiff(scope$add, fput)
    scope$drop = setdiff(scope$drop, fput)
    aod <- NULL
    change <- NULL
    if(backward && length(scope$drop)){
      aod <- drop1(fit, scope$drop, trace = trace, test="F", ...)
      attr(aod, "heading") <- NULL
      rn <- row.names(aod)
      row.names(aod) <- c(rn[1], paste("-", rn[-1], sep=" "))
      nc <- match("F value", names(aod))
      nc <- nc[!is.na(nc)][1]
      o <- order(aod[, nc])
      if(trace) print(aod[o, ])
      if(aod[o[1],nc] > Fout){
        change = NULL
      } else{
        change <- rownames(aod)[o[1]]
      }
    }
    if( is.null(change) ){
      if(forward && length(scope$add)){
        aod <- add1(fit, scope$add, trace = trace, test="F", ...)
        attr(aod, "heading") <- NULL
        rn <- row.names(aod)
        row.names(aod) <- c(rn[1], paste("+", rn[-1], sep=" "))
        nc <- match("F value", names(aod))
        nc <- nc[!is.na(nc)][1]
        o <- order(-aod[, nc])
        if(trace) print(aod[o, ])
        if(aod[o[1],nc] < Fin){
          break
        } else{
          change <- rownames(aod)[o[1]]
        }
      }
    }
    if( is.null(change) ){
      break
    }
    fit <- update(fit, paste("~ .", change), evaluate = FALSE)
    fit <- eval.parent(fit)
    if(length(fit$residuals) != n)
      stop("number of rows in use has changed: remove missing values?")
    Terms <- terms(fit)
    bSSE <- deviance(fit)
    edf <- fit$df.resid
    if(trace)
      cat("\nStep:  SSE=", format(round(bSSE, 2)), "\n",
          cut.string(deparse(as.vector(formula(fit)))), "\n\n")
    ## add a tolerance as dropping 0-df terms
    if(bSSE >= SSE + 1e-7) break
    nm <- nm + 1
    models[[nm]] <- list(df.resid = edf, change = change, SSE = bSSE)
  }
  step.results(models = models[seq(nm)], fit, object)
}

### Example
surg = read.table("SurgUnit.txt", head=F)
names(surg) = c("Blood", "Prog", "Enzyme", "Liver", "Age", "Gender", "Alc.Mod", "Alc.Heavy", "Survival", "LogTime")
attach(surg)
namx = names(surg)[1:4]; modx = namx[1]
for(i in namx[-1])  modx = paste(modx, i, sep="+")
### stepwise
aS = stepF(lm(LogTime ~ 1), scope=paste("~", modx), direction="both", Fin=4,Fout=3.9)
### backward
mody = as.formula( paste("LogTime", modx, sep="~") )
aB = stepF(lm(mody), scope=paste("~", modx), direction="backward", Fout=3.9)
### forward
aF = stepF(lm(LogTime ~ 1), scope=paste("~", modx), direction="forward", Fin=4)
### Force some variable, e.g. Prog, in the model
stepF(lm(LogTime ~ Prog), scope="~Liver+Prog+Enzyme+Blood", 
      fput="~Prog", direction="forward", Fin=4)

### final model fitting summmary
summary(aS)
summary(aB)
summary(aF)



#######################################################################################################
######################################## Some example R codes #########################################
#######################################################################################################


pairs(surg[,-6])
pairs(surg[,-5])

reg1 = lm(Time ~ Blood + Prog + Enzyme + Liver, data=surg)
plot(reg1)
reg = lm(LogTime ~ Blood + Prog + Enzyme + Liver, data=surg)
plot(reg)
  
y = surg$LogTime; xmat = as.matrix(surg[,1:4])
n = length(y)
s2 = sum(reg$res^2)/(n-5)

tmp = vector("list", 4)
for(i in 1:4) tmp[[i]] = 0:1
id = expand.grid(tmp)
R2 = Ra = Cp = Per = rep(0,16)
Vid = rep("", 16)
Vid[1] = "0"
R2[1] = var(y)*(n-1)
Ra[1] = R2[1]/(n-1)
Cp[1] = R2[1]/s2 - (n-2)
Per[1] = R2[1]/(1-1/n)^2
Pm = rowSums(id)+1

for(k in 2:16){
  ik = which(id[k,]>0)
  a = ""
  for(i in ik) a = paste(a, i, sep=",")
  Vid[k] = substring(a,2)
  pk = length(ik)
  regk = lm(y ~ xmat[,ik])
  R2[k] = sum(regk$res^2)
  Ra[k] = R2[k]/(n-pk-1)
  Cp[k] = R2[k]/s2 - (n-2*pk-2)
  xs = cbind(1,xmat[,ik])
  H = xs%*%solve(t(xs)%*%xs)%*%t(xs)
  Per[k] = sum(regk$res^2/(1-diag(H))^2)
}

i = which(Vid=="1,3")
cr = 1-R2/R2[1]
cr = 1-Ra/R2[1]*(n-1)
cr = Ra
cr = Per
plot(Pm[-i], cr[-i], pch=20, xlim=c(0,5), xlab="p", ylab="criterion")
text(Pm[-i], cr[-i], label=Vid[-i], pos=2, col=2)
points(Pm[i], cr[i], pch=3, col=4)
text(Pm[i], cr[i], label=Vid[i], pos=4, col=4)
lines(unique(Pm), tapply(cr,Pm,min), lty=2, col=3)

## par(mfrow=c(1,3))
cr = Cp
plot(Pm[-i], cr[-i], pch=20, xlim=c(0,5),
     xlab="p", ylab="Cp")
text(Pm[-i], cr[-i], label=Vid[-i], pos=2, col=2)
points(Pm[i], cr[i], pch=3, col=4)
text(Pm[i], cr[i], label=Vid[i], pos=4, col=4)
## abline(0,1, col=3)

plot(Pm, cr, pch=20, xlim=c(0,5), ylim=c(0,5),
     xlab="p", ylab="Cp")
text(Pm, cr, label=Vid, pos=2, col=2)
abline(0,1, col=3)

plot(Pm[-i], cr[-i], pch=20, xlim=c(0,5), ylim=c(100,max(Cp)),
     xlab="p", ylab="Cp", log="y")
text(Pm[-i], cr[-i], label=Vid[-i], pos=2, col=2)
points(Pm[i], cr[i], pch=3, col=4)
text(Pm[i], cr[i], label=Vid[i], pos=4, col=4)

