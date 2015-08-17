#' Generating a vector of Present Values from a vector of Cash Flows
#' 
#' This function creates a vector 'net' containing the present value of the
#' components of vector 'x' at rate 'rate'.
#' @param x a vector of length n containing cash flows 
#' @param rate the discount or interest rate
#' @return net a vector of length n containing the present values of each component of vector 'x' at rate 'rate'
#' @author Luis Martin Alonso

fullnpv<-function(x=c(-1000,1000,1000,1000),rate=0.0){
     n<-length(x)
     net<-vector(mode="numeric",length=n)
     for (i in 1:n){
          net[i]<-x[i]*(1+rate)^(-i+1)
     }
     net
}

#' Calculate the Net Present Value
#' 
#' This function calculates the Net Present Value (NPV) 'net' of a series of cash flows in vector
#' 'x' at discount/interest rate 'rate'.
#' 
#' @param x a vector of length n containing cash flows 
#' @param rate the discount or interest rate
#' @return net a numeric value representing the Net Present Value of Cash Flows in 'x'
#' @author Luis Martin Alonso
#' @export

npv<-function(x=c(-1000,1000,1000,1000),rate=0.0){
     n<-length(x)
     full<-vector(mode="numeric",length=n)
     full<-fullnpv(x,rate)
     net<-sum(full)
     net
}

#' Plot the Present Values
#' 
#' This function plots the Cash Flows in their corresponding periods.
#' 
#' @param x a vector of length n containing cash flows 
#' @param xl X label 
#' @param yl Y label
#' @return a barplot of the Cash Flows
#' @author Luis Martin Alonso
#' @export
#' @seealso \code{barplot}
#' @importFrom graphics barplot

plotCF<-function(x=c(-1000,1000,1000,1000),xl="Period",yl="Dollars"){
     n<-length(x)
     net<-x
     per<-seq(from=0,to=n-1,by=1)
     barplot(height=net,names.arg=per,main="Cash Flows",xlab=xl,ylab=yl)
}

#' Calculate the Net Future Value
#' 
#' This functions calculates the Net Future Value (NFV) 'tot' of a series of cash flows in vector
#' 'x' at discount/interest rate 'rate'.
#' 
#' @param x a vector of length n containing cash flows 
#' @param rate the discount or interest rate
#' @return net a numeric value representing the Net Future Value of Cash Flows in 'x' at rate 'rate'
#' @author Luis Martin Alonso
#' @export

nfv<-function(x=c(-1000,1000,1000,1000),rate=0.0){
     n<-length(x)
     net<-vector(mode="numeric",length=n)
     net<-npv(x,disc)
     net<-net*(1+disc)^(n-1)
     net
}

#' Calculate the Internal Rate of Return
#' 
#' This function calculates the Internal Rate of Return (IRR) 'rate' of a series of cash flows in vector
#' 'x'.
#' 
#' @param x a vector of length n containing cash flows 
#' @return rate a numeric value representing the Internal Rate of Return (IRR) of Cash Flows in 'x'.
#' @author Luis Martin Alonso
#' @export

irr<-function(x=c(-1000,1000,1000,1000)){
     roots<-polyroot(x)
     n<-length(x)-1
     for(i in 1:n){
          if(round(Im(roots[i]),5)!=0){
               roots[i]<-NA
          }
     }
     rate<-Re(roots)
     rate<-1/rate-1
     rate<-rate[!is.na(rate)]
     rate<-round(rate[rate>-1],4)
     rate
}

#' Plot the NPV profile
#' 
#' This function plot the Net Present Value (NPV) of a series of cash flows in vector 'x' to a series of
#' interest/discount rates.
#' 
#' @param val a vector of length n containing cash flows 
#' @return a plot of NPV vs. Interest Rate.
#' @author Luis Martin Alonso
#' @export
#' @seealso \code{plot,abline}
#' @importFrom graphics plot
#' @importFrom graphics abline

plotRate<-function(val=c(-1000,1000,1000,1000)){
     base<-irr(val)
     inrate<-seq(from=(base-0.5),to=(base+0.5),by=0.0001)
     values<-vector(mode="numeric",length=length(inrate))
     for(i in 1:length(inrate)){
          values[i]<-npv(x=val,rate=inrate[i])
     }
     plot(x=inrate,y=values,main="NPV vs Rate",pch=20,xlab="Interest Rate",ylab="Net Present Value")
     abline(h=c(0),v=c(0),lty=2,col="red")
}