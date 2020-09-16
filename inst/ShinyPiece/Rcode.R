

myf = function(x,xk,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
}

rsq = function(xk,data){ # data=spruce.df
  df=within(data, X<-(BHDiameter-xk)*(BHDiameter>xk))
  lmp=lm(Height ~ BHDiameter + X, data=df)
  tmp = summary(lmp)
  tmp$r.squared
}

rsqdash = function(xk,h,data) {
 (rsq((xk+h/2),data)-rsq((xk-h/2),data))/h
}


myf2 = function(x,xk,xk2,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)+ coef[4]*(x-xk2)*(x-xk2>0)
}

coeff = function(xk,xk2,data){ # data=spruce.df
  df=within(data, {
            X<-(BHDiameter-xk)*(BHDiameter>xk)
            X2<-(BHDiameter-xk2)*(BHDiameter>xk2)
  }
            )
  lmp=lm(Height ~ BHDiameter + X + X2, data=df)
  coef(lmp)
}
