library(pscl)

alpha <- 3.55
beta <- 27.5
summary(rigamma(n=1000,alpha,beta)*1000)

xseq <- seq(.1,30,by=.1)
fx <- densigamma(xseq,alpha,beta)
plot(xseq,fx,type="n",
     xlab="x",
     ylab="f(x)",
     ylim=c(0,1.01*max(fx)),
     yaxs="i",
     axes=FALSE)
axis(1)
title(substitute(list(alpha==a,beta==b),list(a=alpha,b=beta)))
q <- igammaHDR(alpha,beta,debug=TRUE)
xlo <- which.min(abs(q[1]-xseq))
xup <- which.min(abs(q[2]-xseq))
plotZero <- par()$usr[3]
polygon(x=xseq[c(xlo,xlo:xup,xup:xlo)],
        y=c(plotZero,
            fx[xlo:xup],
            rep(plotZero,length(xlo:xup))),
        border=FALSE,
        col=gray(.45))
lines(xseq,fx,lwd=1.25)

