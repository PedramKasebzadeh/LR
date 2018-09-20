

linreg$new(formula = Petal.Length~Sepal.Width+Sepal.Length, data=iris)
linreg$new(formula = Petal.Length~Sepal.Width+Sepal.Length, data=iris)

formula = Petal.Length~Sepal.Width+Sepal.Length
data=iris

linreg <- setRefClass(Class = "linreg",
                      
                      fields = list(formula="formula", data="data.frame", Beta="matrix", yf="matrix", e="matrix", df="numeric" 
                                    , Var_Beta="matrix", t_Beta="matrix", pvalue="matrix",
                                    parse="character", stand_res="matrix",variance="numeric"),
                      
                      methods = list(
                        
                        initialize =function (formula,data)
                        {
                          c<-colnames(data)
                          d<-all.vars(formula)
                          stopifnot(d %in% c)
                          stopifnot (is.data.frame(data))
                          formula <<- formula
                          data <<- data
                          X <- model.matrix(formula,data)
                          dep_y <- all.vars(formula)[1]
                          y <- (data[,dep_y])
                          #parse <<- deparse(substitute(data)) not sure we need it
                          #Regressions coefficients
                          
                          
                          
                          
                          #1 regressions coefficients
                          Beta <<- solve(  t(X) %*% X)   %*%   t(X)   %*% y
                          
                          
                          #2 fitted values
                          yf <<- X %*% Beta
                          
                          #Residuals
                          e <<- y-yf
                          
                          #Degrees of freedom
                          
                           adf<<- nrow(X)-ncol(X)
                          
                          #Residual variance needs a solve. 
                          Sigma_square <<- solve(t(e)%*%e) / df 
                          
                          #THROWS error, commented until fixed
                          
                          #Variance of regression coefficients
                          #Var_Beta <<- Sigma_square * solve((t(X)%*%X))
                          #t-values for each coefficient
                         # t_Beta <<- Beta / sqrt(diag(Var_Beta))
                          #p values for reg coefficients
                          #pvalue <<- pt(abs(t_Beta),df)
                          #variance value
                          #variance <<- round(sqrt(Sigma_square),2)
                          #standardised residual for plot2
                          #stand_res <<- sqrt(abs((e-mean(e)) / sqrt(Sigma_square)))
                        },
                        
                        
                        print = function(){
                          
                          
                        },
                        
                        
                        plot = function(){
                          library(ggplot2)
                          #plotting yf and e
                          data_frame1 <- data.frame(Fitted_values=yf,Residuals=e)
                          p1 <- ggplot(data_frame1,aes(Fitted_values,Residuals))+
                            geom_point()+geom_abline()+
                            ggtitle("Residuals vs Fitted")+
                            xlab("Fitted values lm(Petal.Length ~ Species)")+
                            ylab("Residuals")
                          
                          data_frame2 <- data.frame(Fitted_values=yf,Residuals=stand_res)
                          p2 <- ggplot(data_frame2,aes(Fitted_values,Residuals))+
                            geom_point()+geom_abline()+
                            ggtitle("Scale-Location")+
                            xlab("Fitted values lm(Petal.Length ~ Species)")+
                            ylab(expression(bold(sqrt(bold("Standardized Residuals")))) )
                          return(list(p1, p2))
                        },
                        
                        resid = function(){
                          cat("Returning vector of residuals e:", "\n")
                          return(as.vector(round(e,2)))
                        },
                        
                        pred = function(){
                          cat("Returning predicted values yf:", "\n")
                          return(as.vector(round(yf,2)))
                        },
                        
                        coef = function(){
                          cat("Returning coefficients as a vector:", "\n")
                          return(as.vector(round(Beta,2)))
                        },
                        
                        summary = function(){
                          
                          
                          
                        }
                        
                        
                        
                      ))
