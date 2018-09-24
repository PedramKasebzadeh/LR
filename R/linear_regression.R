#' A Reference Class for cumputating Linear Regression
#'
#'This class has a vary of functions.
#'
#' @field computing a liner regression 
#' library(ggplot2)
#' 
linreg_mod$yf
linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
linreg_mod$summary()
linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
#0.5  0.1  0.0


expect_output(linreg_mod$summary(), "\\(Intercept\\)( )*-2.5[0-9]*( )*0.5[0-9]*( )*-4.4[0-9]*( )*.*( )*\\*\\*\\*")  
expect_output(linreg_mod$summary(), "Sepal.Width( )*-1.3[0-9]*( )*0.1[0-9]*( )*-10.9[0-9]*( )*.*( )*\\*\\*\\*")
expect_output(linreg_mod$summary(), "Sepal.Length( )*1.7[0-9]*( )*0.0[0-9]*( )*27.5[0-9]*( )*.*( )*\\*\\*\\*")
expect_output(linreg_mod$summary(), "Residual standard error: 0.6[0-9]* on 147 degrees of freedom")

linreg <- setRefClass(Class = "linreg",
                      
                    
                      fields = list(formula="formula", data="data.frame", regco="matrix", yf="matrix", e="matrix", dfreedom="numeric", 
                                    Sigma_square="numeric", Var_Beta="matrix", t_Beta="matrix", pvalue="matrix",
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
                          parse <<- deparse(substitute(data))
                          #Regressions coefficients
                          regco <<- solve((t(X)%*%X))%*%t(X)%*%y
                          #X <- QR
                          #Beta <- solve(R)%*%t(Q)%*%y
                          #Fitted values
                          yf <<- X%*%regco
                          #Residuals
                          e <<- y-yf
                          #Degrees of freedom
                          dfreedom <<- nrow(X)-ncol(X)
                          #Residual variance
                          Sigma_square <<- as.numeric((t(e)%*%e) / dfreedom)
                          #Variance of regression coefficients
                          Var_Beta <<- Sigma_square * solve((t(X)%*%X))
                          #t-values for each coefficient
                          t_Beta <<- regco / sqrt(diag(Var_Beta))
                          #p values for reg coefficients
                          pvalue <<- pt(abs(t_Beta),dfreedom)
                          #variance value
                          variance <<- round(sqrt(Sigma_square),2)
                          #standardised residual for plot2
                          stand_res <<- sqrt(abs((e-mean(e)) / sqrt(Sigma_square)))
                        
                        },
                        #Methods
                        #print(),plot(), resid(),pred(),coef(),summary()
                        print = function(){
                          cat("Coefficients:","\n",
                              paste(row.names(regco)),"\n",
                              format(round(regco,2),width = 12)
                           )   
                           },
                            
                        
                        plot = function(){
                          library(ggplot2)
                          #plotting yf and e
                          data_frame1 <- data.frame(Fitted_values=yf,Residuals=e)
                          p1 <- ggplot(data_frame1,aes(Fitted_values,Residuals))+
                            geom_point(shape = 21, colour = "black", fill = "white", size = 2.8, stroke = 1.3)+
                            geom_smooth(method = "loess",color = "red", se = FALSE)+
                            ggtitle("Residuals vs Fitted")+
                            xlab("Fitted values linreg(Petal.Length ~ Species)")+
                            ylab("Residuals")+
                            xlim(1,6)+
                            ylim(-1.5,1.5)+
                            theme(panel.background = element_blank(),
                                  panel.border = element_rect(colour = "black", fill=NA, size=1))
                          
                          data_frame2 <- data.frame(Fitted_values=yf,Residuals=stand_res)
                          p2 <- ggplot(data_frame2,aes(Fitted_values,Residuals))+
                            geom_point(shape = 21, colour = "black", fill = "white", size = 2.8, stroke = 1.3)+
                            geom_smooth(method = "loess",color = "red", se = FALSE)+
                            ggtitle("Scale-Location")+
                            xlab("Fitted values linreg(Petal.Length ~ Species)")+
                            ylab(expression(sqrt("Standardized Residuals")))+
                            xlim(1,6)+
                            ylim(0.0,1.5)+
                            theme(panel.background = element_blank(),
                              panel.border = element_rect(colour = "black", fill=NA, size=1))
                          return(list(p1,p2))
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
                          
                        return(list(regco,t_Beta,stand_res,dfreedom))
                          
                          

                          
                        }
                        
                        
                        
                      ))
