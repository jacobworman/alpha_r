library('R6');
library('tseries');
library('caret')
library(Quandl)
library('dplyr')

QuandlClass <- R6Class("QuandlClass",
                         private = list(
                           
                           keys = NULL,
                           names1 = NULL
                           
                           
                         ),
                         public = list(
                           
                           timeSeries = list(),
                           
                           initialize = function(keys, names1){
                             
                             private$keys = keys;
                             private$names1 = names1
                             
                             
                           },
                           
                           ## Check if the timeSeries are cointegrated
                           fetchData = function(startdate1, enddate1, match){
                             
     
                            for(t in seq_along(private$keys)){
                              #print(private$keys[[t]])
                              index = Quandl(private$keys[[t]], api_key="5s7-4eZFGjPKSdFwTrys")
                              #print(index)
                              if(length(index[['Date']]) > 0){
                                index[['Date']] <- as.Date(index[['Date']], format= "%Y-%m-%d");
                                index <- subset(index, Date >= startdate1 & Date <= enddate1); 
                                colnames(index)[1] <- "date"
                                colnames(index)[2] <- "close"
                              }else if(length(index[['Trade Date']]) > 0){
                                index[['Trade Date']] <- as.Date(index[['Trade Date']], format= "%Y-%m-%d");
                                colnames(index)[1] <- "date"
                                index <- subset(index, date >= startdate1 & date <= enddate1); 
                                colnames(index)[2] <- "close"
                              }else{
                                index[['DATE']] <- as.Date(index[['DATE']], format= "%Y-%m-%d");
                                colnames(index)[1] <- "date"
                                index <- subset(index, date >= startdate1 & date <= enddate1); 
                                colnames(index)[2] <- "close"
                              }
                              index[['close']] <- rev(index[['close']])
                              index[['date']] <- rev(index[['date']])
                              
                              dplr <- NULL
                              dplr <- left_join(match, index, by=c("date"))
                              timeSeries = na.locf(dplr[['close.y']] , fromLast = TRUE)
                              
                              self$timeSeries[[ private$names1[[t]] ]] = timeSeries
                              
                            }
                             
                             return(self);
                           }
                           
                           
                         )
);

Cointegration <- R6Class("Cointegration",
                         private = list(
                           
                           y = c(),
                           timeSeries = data.frame(),
                           
                           
                           cointegrationTermEvery = function(array_1, array_2, days_1){
                             g <- c(); 
                             
                             
                             n <- length(array_1)
                             for(i in seq_along(array_1)){
                               if(n <= 0 || n-days_1 <= 0){
                                 break;
                               }
                               
                               tidsSerie_Y = array_1[(n-days_1):n];
                               first_day_y = array_1[[1]]
                               tidsSerie_X = array_2[(n-days_1):n];
                               first_day_x = array_2[[1]]
                               cointegrated_relationship = Regression$new(data.frame(
                                 y = (tidsSerie_Y)/first_day_y,
                                 x = (tidsSerie_X)/first_day_x
                               ), TRUE);
                               
                               g <- c(g, get_last_item(residuals(cointegrated_relationship$get() )) );
                               
                               n <- n - days_1
                             }
                             return(rev(g));
                           }
                           
                         ),
                         public = list(
                           
                           
                           initialize = function(y = NULL, timeSeries = NULL){
                             
                             if(is.null(y)){
                             private$y = y;
                             private$timeSeries = timeSeries;
                             }
                             
                           },
                           
                           ## Check if the timeSeries are cointegrated
                           isCointegrated = function(){
                             
                             Cointegrated = FALSE;
                             
                             if(!TimeSerie$new(private$y)$isStationary()){
                               
                               
                               hittadEnMedStatinar = FALSE;
                               for (i in colnames(private$timeSeries)){
                                 
                                 if(TimeSerie$new( private$timeSeries[[i]] )$isStationary()){
                                   hittadEnMedStatinar = TRUE;
                                   break;
                                 }
                                 
                                 
                               }
                               
                               if(!hittadEnMedStatinar){
                                 
                                 
                                 data1 = data.frame(
                                   y = log(private$y)
                                 );
                                 
                                 for (i in colnames(private$timeSeries)){
                                   data1 = cbind(data1, log(private$timeSeries[[i]]))
                                 }
                                 
                                 residualerna = Regression$new(data1, TRUE)$residualerna();
                                 
                                 if(adf.test(residualerna)$p.value < 0.05){
                                   Cointegrated = TRUE;
                                 }
                                 
                                 
                                 
                               }
                               
                             }
                             
                             return(Cointegrated);
                             
                           },
                           
                           makeCointegrationTerms = function(x, x1, y, y1){
                             aList = list();
                             
                             tidsSerie_Y = x;
                             tidsSerie_X = x1;
                             cointegrated_relationship = Regression$new(data.frame(
                               y = log(tidsSerie_Y),
                               x = log(tidsSerie_X)
                             ), TRUE);
                             
                             aList[[1]] = cointegrated_relationship$residualerna();
                             
                             tidsSerie_Y = y;
                             tidsSerie_X = y1;
                             cointegrated_relationship1 = Regression$new(data.frame(
                               y = log(tidsSerie_Y),
                               x = log(tidsSerie_X)
                             ), TRUE);
                             
                             aList[[2]] = cointegrated_relationship1$residualerna();
                             
                             return(aList);
                             
                           },
                           
                           makeCointegrationTerm = function(x, x1){
                             aList = list();
                             
                             tidsSerie_Y = x;
                             tidsSerie_X = x1;
                             cointegrated_relationship = Regression$new(data.frame(
                               y = log(tidsSerie_Y),
                               x = log(tidsSerie_X)
                             ), TRUE);
                             
                             aList[[1]] = cointegrated_relationship$residualerna();
                             
                             return(aList);
                             
                           },
                           
                           makeCointegrationTermsForEvery = function(x,x1,y,y1,interv){
                             aList = list()
                             aList[[1]] = private$cointegrationTermEvery(log(x),log(x1),interv)
                             aList[[2]] = private$cointegrationTermEvery(log(y),log(y1),interv)
                             return(aList)
                           },
                           
                           makeCointegrationTermForEvery = function(x,x1interv){
                             aList = list()
                             aList[[1]] = private$cointegrationTermEvery(x,x1,interv)
                             return(aList)
                           },
                           
                           makeCointegrationTermsMutiple = function(df1, df2){
                             aList = list();

                             cointegrated_relationship = Regression$new(df1, TRUE);
                             
                             aList[[1]] = cointegrated_relationship$residualerna();
                             

                             cointegrated_relationship1 = Regression$new(df2, TRUE);
                             
                             aList[[2]] = cointegrated_relationship1$residualerna();
                             
                             return(aList);
                           }
                           
                           
                         )
                         );

TimeSerie <- R6Class("TimeSerie",
                     
                     private = list(
                       
                       timeSerie = c(),
                       tidTillbaka = NULL,
                       
                       get_return_smooth = function(array_1, days_1){
                         g <- c()
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           g <- c(g, mean((array_1[[n]] - array_1[(n-days_1):n])/array_1[(n-days_1):n]));
                           
                           n <- n - days_1
                         }
                         return(rev(g)); 
                       },
                       get_return_smooth_every_day = function(array_1, days_1){
                         g <- c()
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           g <- c(g, mean((array_1[[n]] - array_1[(n-days_1):n])/array_1[(n-days_1):n]));
                           
                           n <- n - 1
                         }
                         return(rev(g)); 
                       },
                       
                       is_above = function(array_1, days_1, above){
                         
                         g <- c(); 
                         
                         
                         n <- length(array_1)
                         t <- length(serie)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           
                           
                           
                           avkastning = ((array_1[(n-days_1+1):n] - array_1[[n-days_1]])/array_1[[n-days_1]])
                           
                           days_until = length(avkastning)
                           for(d in seq(1:length(avkastning))){
                             if(avkastning[[d]] > above){
                               days_until = d
                               break
                             }
                           }
                           
                           g <- c(g, days_until );
                           
                           
                           n <- n - days_1
                           t <- t - 1
                         }
                         return(rev(g)); 
                         
                       },
                       
                       is_below = function(array_1, days_1, below, value, serie){
                         
                         g <- c(); 
                         
                         
                         n <- length(array_1)
                         t <- length(serie)
                         for(i in seq_along(serie)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           
                           if(serie[[t]] >= value ){
                             
                           avkastning = get_return(array_1[(n-days_1):n], 21 )
                             #((array_1[(n-days_1+1):n] - array_1[[n-days_1]])/array_1[[n-days_1]])
                           
                           "days_until = length(avkastning)
                           for(d in seq(1:length(avkastning))){

                             if(avkastning[[d]] < below){
                               days_until = d
                               break
                             }
                           }
                           "
                           g <- c(g, sd(avkastning) );
                           }
                           
                           n <- n - days_1
                           t <- t - 1
                         }
                         return(rev(g)); 
                         
                       },
                       
                       is_change_below = function(array_1, days_1, below){
                         
                         g <- c(); 
                         
                         
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           
                           avkastning = ((array_1[(n-days_1+1):n] - array_1[[n-days_1]])/array_1[[n-days_1]])
                           
                           g <- c(g, any(avkastning < below) );
                           
                           n <- n - days_1
                         }
                         return(rev(g)); 
                         
                       },
                       
                       change = function(array_1, days_1, above){
                         
                         g <- c(); 
                         
                         times = 0;
                         count = 0;
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           
                           avkastning = ((array_1[(n-days_1+1):n] - array_1[[n-days_1]])/array_1[[n-days_1]])
                           
                           if(any(avkastning > above)){
                             if(avkastning[[1]] > 0){
                               count = count + 1
                             }
                             times = times+ 1
                           }
                           
                           
                           
                           n <- n - days_1
                         }
                         return(count/times); 
                         
                       },
                       
                       is_change_above = function(array_1, days_1, above){
                         
                         g <- c(); 
                        dd = days_1
                         days_1 = days_1
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-dd <= 0){
                             break;
                           }
                           
                           avkastning = ((array_1[(n-days_1+1):n] - array_1[[n-days_1]])/array_1[[n-days_1]])
                           
                           
                           g <- c(g, as.integer(as.logical( any(avkastning > above))) );
                           
                           n <- n - dd
                         }
                         return(rev(g)); 
                         
                       },
                       
                       expected = function(array_1, days_1, above, change){
                         
                         g <- c(); 
                         
                         
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           
                           avkastning = ((array_1[(n-days_1+1):n] - array_1[[n-days_1]])/array_1[[n-days_1]])
                           
                           first = (array_1[[n-days_1+1]] - array_1[[n-days_1]])/array_1[[n-days_1]]
                           
                           first = as.integer(as.logical( first > 0))
                           
                           expected = NULL
                           for(t in seq_along(avkastning)){
                             if(avkastning[[t]]/above > change){
                               expected = max(avkastning[t:length(avkastning)])
                               break;
                             }
                           }
                           
                           if(is.null(expected)){
                           
                           }else{
                             g <- c(g, expected );
                           }
                           
                           n <- n - days_1
                         }
                         return(rev(g)); 
                         
                       },
                       
                       expected2 = function(array_1, days_1, above, change){
                         
                         g <- c(); 
                         
                         
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           
                           avkastning = ((array_1[(n-days_1+1):n] - array_1[[n-days_1]])/array_1[[n-days_1]])
                           
                           first = (array_1[[n-days_1+1]] - array_1[[n-days_1]])/array_1[[n-days_1]]
                           
                           first = as.integer(as.logical( first > 0))
                           
                           expected = NULL
                           for(t in seq_along(avkastning)){
                             if(avkastning[[t]] > above){
                               expected = max(avkastning[t:length(avkastning)])
                               break;
                             }
                           }
                           
                           if(is.null(expected)){
                             
                           }else{
                             g <- c(g, expected );
                           }
                           
                           n <- n - days_1
                         }
                         return(rev(g)); 
                         
                       },
                       
                       is_change_above_everyday = function(array_1, days_1, above){
                         
                         g <- c(); 
                         
                         
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           
                           avkastning = ((array_1[(n-days_1+1):n] - array_1[[n-days_1]])/array_1[[n-days_1]])
                           
                           g <- c(g, any(avkastning > above) );
                           
                           n <- n - 1
                         }
                         return(rev(g)); 
                         
                       },
                       
                       get_linear = function(array_1, days_1){
                         
                         g <- c(); 
                         
                         
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           
                           timeSerieHere = array_1[(n-days_1):n]
                           
                           minValue = min(timeSerieHere)
                           index = which.min(timeSerieHere)
                           index_max = which.max(timeSerieHere)
                           focusedTimeSeries = timeSerieHere[1:(length(timeSerieHere)/2)]
                           
                           aRegression = Regression$new(data.frame(y = focusedTimeSeries, x=seq_this(focusedTimeSeries)), TRUE)
                           s1 = aRegression$getSummary()
                           
                           g <- c(g, s1$coefficients[,1][[2]] );
                           
                           n <- n - days_1
                         }
                         return(rev(g));
                         
                       },
                       
                       get_slope_and_effect = function(array_1, days_1, above){
                         g <- c(); 
                         av <- 0; 
                         if(is.numeric(avgift) == TRUE ){ 
                           av <- avgift; 
                         };
                         
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           
                           array1 = array_1[((n-(days_1/2))-days_1):(n-(days_1/2))]
                           
                           
                           linearmod1 = Regression$new(data.frame(
                             y = array1,
                             x = seq_this(array1)
                           ), TRUE)
                           
                           s1 = linearmod1$getSummary()
                           
                           avkastning = ((array_1[(n-days_1+1):n] - array_1[[n-days_1]])/array_1[[n-days_1]])
                           
                           first_days = avkastning[1:5]
                           
                          condition1 = as.integer(as.logical( any(avkastning > above) ))
                          condition2 = as.integer(as.logical( s1$coefficients[,1][[2]] > 0 ))
                           g <- c(g, condition1*condition2 );
                           
                           n <- n - days_1
                         }
                         return(rev(g)); 
                       },
                       
                       get_slope = function(array_1, days_1){
                         g <- c(); 
                         av <- 0; 
                         if(is.numeric(avgift) == TRUE ){ 
                           av <- avgift; 
                         };
                         
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           
                           array1 = array_1[(n-days_1):n]
                           
                           
                           linearmod1 = Regression$new(data.frame(
                             y = array1,
                             x = seq_this(array1)
                           ), TRUE)
                           
                           s1 = linearmod1$getSummary()
                           
         
                           
                           g <- c(g, s1$coefficients[,1][[2]] > 0);
                           
                           n <- n - days_1
                         }
                         return(rev(g)); 
                       },
                       
                       get_beta = function(array_1, marketTimeSerie, days_1, lags = 1, avgift = NULL){ 
                         g <- c(); 
                         av <- 0; 
                         if(is.numeric(avgift) == TRUE ){ 
                           av <- avgift; 
                         };
                         
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           
                           array1 = array_1[(n-days_1):n]
                           array2 = marketTimeSerie[(n-days_1):n]
                           
                           array1_return = get_return(array1, lags)
                           array2_return = get_return(array2, lags)
                           
                           
                           linearmod1 = Regression$new(data.frame(
                             y = array1_return,
                             x = array2_return
                           ), TRUE)
                           
                           s1 = linearmod1$getSummary()
                           
                           
                           g <- c(g, s1$coefficients[,1][[2]]);
                           
                           n <- n - days_1
                         }
                         return(rev(g)); 
                       },
                       
                       get_return = function(array_1, days_1, avgift = NULL){ 
                         g <- c(); 
                         av <- 0; 
                         if(is.numeric(avgift) == TRUE ){ 
                           av <- avgift; 
                         };
                         
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           
                           g <- c(g, ((array_1[[n]] - array_1[[n-days_1]])/array_1[[n-days_1]]) - av);
                           
                           n <- n - days_1
                         }
                         return(rev(g)); 
                       },
                       get_sharpe= function(array_1, days_1, avgift = NULL){ 
                         g <- c(); 
                         av <- 0; 
                         if(is.numeric(avgift) == TRUE ){ 
                           av <- avgift; 
                         };
                         
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           
                           serie = array_1[(n-days_1):n]
                           
                           g <- c(g, sharpe(serie));
                           
                           n <- n - days_1
                         }
                         return(rev(g)); 
                       },
                       get_return_every_day = function(array_1, days_1, avgift = NULL){ 
                         g <- c(); 
                         av <- 0; 
                         if(is.numeric(avgift) == TRUE ){ 
                           av <- avgift; 
                         };
                         
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           
                           g <- c(g, ((array_1[[n]] - array_1[[n-days_1]])/array_1[[n-days_1]]) - av);
                           
                           n <- n - 1
                         }
                         return(rev(g)); 
                       }
                       
                     ),
                     public = list(
                       
                    
                       
                       initialize = function(y, tidTillbaka1){
                         
                         private$timeSerie = (y);
                         private$tidTillbaka = tidTillbaka1;
                         
                         return(self);
                         
                       },
                       
                       addNew = function(newTimeSeries){
                         
                         private$timeSerie = (newTimeSeries);
                         
                       },
                       
                       setLength = function(z, time_span){
                         
                         private$timeSerie = private$timeSerie[(z-(time_span-1)):(z)];
                         return(self);
                         
                       },
                       
                       setLength2 = function(z, time_span){
                         
                         private$timeSerie = private$timeSerie[(z-(time_span-1)-private$tidTillbaka):(z-private$tidTillbaka)]; 
                         
                         return(self);
                       },
                       
                       get = function(){
                         return(private$timeSerie);
                       },
                       
                       andraTidTillbaka = function(tidTillbaka1){
                         private$tidTillbaka =tidTillbaka1;
                       },
                       
                       isStationary = function(){
                         Stationary = FALSE;
                         if(adf.test(private$timeSerie)$p.value < 0.05){
                           Stationary = TRUE;
                         }
                         return(Stationary);
                       },
                       
                       test = function(){
                         
                         print(private$timeSerie);
                         
                       },
                       
                       getLinear = function(tidTillbaka = NULL){
                         
                         days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         if(!is.null(tidTillbaka)){
                           days_1 = tidTillbaka
                         }
                         return(private$get_linear(private$timeSerie,days_1));
                         
                       },
                       
                       getEffectEveryDay = function(above,tidTillbaka = NULL){
                         #return(self$getEffect(above, tidTillbaka))
                         days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         if(!is.null(tidTillbaka)){
                           days_1 = tidTillbaka
                         }
                         return(private$is_change_above_everyday(private$timeSerie,days_1,above));
                       },
                       
                       getEffect = function(above,tidTillbaka = NULL){
                         days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         if(!is.null(tidTillbaka)){
                           days_1 = tidTillbaka
                         }
                         return(private$is_change_above(private$timeSerie,days_1,above));
                       },
                       
                       
                       getChange = function(above,tidTillbaka = NULL){
                         days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         if(!is.null(tidTillbaka)){
                           days_1 = tidTillbaka
                         }
                         return(private$change(private$timeSerie,days_1,above));
                       },
                       
                       getEffectBelow = function(below,tidTillbaka = NULL){
                         days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         if(!is.null(tidTillbaka)){
                           days_1 = tidTillbaka
                         }
                         return(private$is_change_below(private$timeSerie,days_1,below));
                       },
                          
                       getExpectedEffect = function(above, change, tidTillbaka = NULL){
                         days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         if(!is.null(tidTillbaka)){
                           days_1 = tidTillbaka
                         }
                         return(private$expected(private$timeSerie,days_1,above, change));
                       },
                       
                       getExpectedEffectAbove = function(above,tidTillbaka = NULL){
                         days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         if(!is.null(tidTillbaka)){
                           days_1 = tidTillbaka
                         }
                         return(private$expected2(private$timeSerie,days_1,above));
                       },
                       
                       getAverageDaysEffect = function(above, tidTillbaka = NULL){
                         days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         if(!is.null(tidTillbaka)){
                           days_1 = tidTillbaka
                         }
                         return(private$is_above(private$timeSerie,days_1,above));
                       },
                       getAverageDaysEffectBelow = function(below, value, serie, tidTillbaka = NULL){
                         days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         if(!is.null(tidTillbaka)){
                           days_1 = tidTillbaka
                         }
                         return(private$is_below(private$timeSerie,days_1,below, value, serie));
                       },
                       ## BETAS
                       getBeta = function(marketTimeSerie, lags = 1, tidTillbaka = NULL){
                         days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         if(!is.null(tidTillbaka)){
                           days_1 = tidTillbaka
                         }
                         return(private$get_beta(private$timeSerie,marketTimeSerie,days_1, lags));
                       },
                       
                       getSlope = function(tidTillbaka = NULL){
                         days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         if(!is.null(tidTillbaka)){
                           days_1 = tidTillbaka
                         }
                         return(private$get_slope(private$timeSerie,days_1));
                       },
                       
                       getSlopeEffect = function(above, tidTillbaka = NULL){
                         days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         if(!is.null(tidTillbaka)){
                           days_1 = tidTillbaka
                         }
                         return(private$get_slope_and_effect(private$timeSerie, days_1, above));
                       },

                       
                       ## Avkastningar
                       getReturn = function(tidTillbaka = NULL){
                         days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         if(!is.null(tidTillbaka)){
                           days_1 = tidTillbaka
                         }
                         return(private$get_return(private$timeSerie,days_1));
                       },
                       
                       getSharpeRatioEvery = function(tidTillbaka = NULL){
                         days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         if(!is.null(tidTillbaka)){
                           days_1 = tidTillbaka
                         }
                         return(private$get_sharpe(private$timeSerie,days_1));
                       },
                       
                       getReturnEveryDay = function(){
                         return(self$getReturn());
                       "days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         return(private$get_return_every_day(private$timeSerie,days_1));"
                         
                       },
                       
                       smoothOut = function(){
                         as1 = private$timeSerie
                         tryCatch({
                           
                           
                           "private$timeSerie = SMA(private$timeSerie, n=5)
                           private$timeSerie = na.locf(private$timeSerie, fromLast = FALSE)
                           private$timeSerie = na.approx(private$timeSerie)"
                           
                         }, 
                         error = function(e){
                           private$timeSerie = as1
                         })
           
                         
                         return(self);
                       },
                       
                       getReturnSmooth = function(){
                         days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         return(private$get_return_smooth(private$timeSerie,days_1));
                       },
                       
                       getReturnSmoothEveryDay = function(){
                       #return(self$getReturnSmooth());
                        days_1 = 21;
                         if(!is.null(private$tidTillbaka)){
                           days_1 = private$tidTillbaka;
                         }
                         return(private$get_return_smooth_every_day(private$timeSerie,days_1));
                         
                       },
                       
                       ## Standardavvkelese av avkastningar
                       getSD = function(){
                         return(sd(self$getReturn()));
                       },
                       
                       ##Sharpe ratio
                       getSharpeRatio = function(tidTillbaka = NULL){
                         return(sharpe(self$getReturn(tidTillbaka)));
                       }
                       
                     )
                     )

TimeSeries <- R6Class("TimeSeries",
                     private = list(
                       
                       timeSeries = list(),
                       tidTillbaka = NULL,
                       
                       get_return = function(array_1, days_1, avgift = NULL){ 
                         g <- c(); 
                         av <- 0; 
                         if(is.numeric(avgift) == TRUE ){ 
                           av <- avgift; 
                         };
                         
                         n <- length(array_1)
                         for(i in seq_along(array_1)){
                           if(n <= 0 || n-days_1 <= 0){
                             break;
                           }
                           
                           g <- c(g, ((array_1[[n]] - array_1[[n-days_1]])/array_1[[n-days_1]]) - av);
                           
                           n <- n - days_1
                         }
                         return(rev(g)); 
                       }
                       
                     ),
                     public = list(
                       
                       
                       
                       initialize = function(listaMedTidsSerier,tidTillbaka){
                         
                         private$tidTillbaka = tidTillbaka;
                         
                         for(i in seq_along(listaMedTidsSerier)){
                           private$timeSeries[[i]] = TimeSerie$new(listaMedTidsSerier[[i]], tidTillbaka);
                         }
                        
                         return(self);
                         
                       },
                       
                       length = function(){
                         return(length(private$timeSeries))
                       },
                       
                       get = function(){
                         
                         return(private$timeSeries);
                         
                       },
                       
                       getIndex = function(index){
                         return(private$timeSeries[[index]]);
                       },
                       
                       setLength = function(z, time_span){
                         
                         for(i in seq_along(private$timeSeries)){
                           private$timeSeries[[i]] = private$timeSeries[[i]]$setLength(z, time_span);
                         }
                         
                         return(self);
                         
                       },
                       
                       setLength2 = function(z, time_span){
                         
                         for(i in seq_along(private$timeSeries)){
                           private$timeSeries[[i]] = private$timeSeries[[i]]$setLength2(z, time_span);
                         }
                         
                         return(self);
                         
                         
                       },
                       
                       andraTidTillbaka = function(tidTillbaka){
                         private$tidTillbaka =tidTillbaka;
                         
                         for(i in seq_along(private$timeSeries)){
                           private$timeSeries[[i]] = TimeSerie$new(private$timeSeries[[i]], tidTillbaka);
                         }
                         
                       },
                       
                       smoothOut = function(){
                         
                         for(i in seq_along(private$timeSeries)){
                           private$timeSeries[[i]] = private$timeSeries[[i]]$smoothOut();
                         }
                         
                         return(self);
                         
                       },
                       
                       ## Avkastningar
                       getAverageMean = function(){
                         avg_mean <- c()
                         for(i in seq_along(private$timeSeries)){
                           avg_mean <- c(avg_mean, mean(private$timeSeries[[i]]$getReturn() ) );
                         }
                         return(mean(avg_mean));
                       },
                       
                       getMonthlyReturns = function(){
                         
                         lista = list();
                         
                         for(i in seq_along(private$timeSeries)){
                           
                           lista[[i]] = private$timeSeries[[i]]$getReturn();
                           
                         }
                         
                         return(lista);
                         
                       },
                       
                       ## Standardavvkelese av avkastningar
                       getAverageSD = function(){
                         avg_sd <- c()
                         for(i in seq_along(private$timeSeries)){
                           avg_sd <- c(avg_sd, mean(private$timeSeries[[i]]$getSD() ) );
                         }
                         return(avg_sd);
                       },
                       
                       getAverageSDSD = function(){
                          
                         avg_sd <- c()
                         for(i in seq_along(private$timeSeries)){
                           avg_sd <- c(avg_sd, mean(private$timeSeries[[i]]$getSD() ) );
                         }
                         return(sd(avg_sd));
                          
                       },
                       
                       ##Sharpe ratio
                       getAverageSharpeRatio = function(){
                         avg_sharpe <- c()
                         for(i in seq_along(private$timeSeries)){
                           avg_sharpe <- c(avg_sharpe, mean(private$timeSeries[[i]]$getSharpeRatio() ) );
                         }
                         return(median(avg_sharpe));
                       },
                       ##Sharpe ratio
                       getAverageSharpeRatioSD = function(){
                         avg_sharpe <- c()
                         for(i in seq_along(private$timeSeries)){
                           avg_sharpe <- c(avg_sharpe, mean(private$timeSeries[[i]]$getSharpeRatio() ) );
                         }
                         return(sd(avg_sharpe));
                       }
                       
                     )
)

Regression <- R6Class("Regression",
                      private = list(
                        data = data.frame(),
                        
                        regressionen = NULL,
                        
                        alpha = 0.05
                      ),
                     public = list(
                  
                       
                       initialize = function(data, medKonstant, alpha = NULL, ownRegression = NULL){
                         
                         if(is.null(ownRegression)){
                         
                         private$data = data;
                         
                         if(medKonstant){
                           linearmod <- lm(y~., private$data);
                         }else{
                           linearmod <- lm(y~.+0, private$data);
                         }
                         
                         private$regressionen = linearmod;
                         
                         if(!is.null(alpha)){
                           private$alpha = alpha;
                         }
                         
                         }else{
                           private$regressionen = ownRegression
                         }
                         
                         return(self);
                         
                       },
                       
                       uppdateraData = function(data){
                         private$data = data;
                       },
                       
                       chooseBestRSquared = function(medKonstant){
                         names1 = names(private$data)
                         
                         highestRightNow = 0;
                         regen = NULL
                         for(t in seq_along(private$data)){
                           if(names1[[t]] == 'y'){
                             next
                           }
                           data = data.frame(
                             y = private$data[['y']]
                           )
                           data[[ names1[[t]] ]] = private$data[[ names1[[t]] ]]
                           reg = Regression$new(data, medKonstant)
                           if(is.na(reg$getSummary()$adj.r.squared) || is.na(reg$getSummary()$r.squared) 
                              || is.na(reg$getSummary()$adj.r.squared/reg$getSummary()$r.squared)){
                             next;
                           }
                           if(reg$getSummary()$adj.r.squared/reg$getSummary()$r.squared > highestRightNow || highestRightNow == 0){
                             highestRightNow = reg$getSummary()$adj.r.squared/reg$getSummary()$r.squared
                             regen = reg;
                           }
                           
                         }
                         
                         return(regen)
                         
                       },
                       
                       orderHighestRSquared = function(medKonstant, topp){
                         names1 = names(private$data)
                         regen = list()
                         a = c()
                         for(t in seq_along(private$data)){
                           if(names1[[t]] == "y"){
                             next;
                           }
                           data = data.frame(
                             y = private$data[['y']]
                           )
                           data[[ names1[[t]] ]] = private$data[[ names1[[t]] ]]
                           reg = Regression$new(data, medKonstant)
                           
                           regen[[ names1[[t]] ]] = reg$getSummary()$r.squared
                           a = c(a,reg$getSummary()$r.squared)
                         }
                         b = topp;
                         if(length(a) < topp){
                           b = length(a)
                         }
                         if(all(is.na(a))){
                           return(c())
                         }
                         top1 = sort(a, decreasing = TRUE)[[b]]
                          v = c()
                         for(t in seq_along(names1)){
                           if(names1[[t]] == "y"){
                             next;
                           }
                           if(is.na(regen[[ names1[[t]] ]])){
                             next;
                           }
                           if(regen[[ names1[[t]] ]] < top1){
                             v = c(v, names1[[t]])
                           }
                         }
                         
                         return(v)
                         
                       },
                       
                       checkIfRightSign = function(){
                         names1 = names(private$data)
                         regen = list()
                         a = c()
                         for(t in seq_along(private$data)){
                           if(names1[[t]] == "y"){
                             next;
                           }
                           data = data.frame(
                             y = private$data[['y']]
                           )
                           data[[ names1[[t]] ]] = private$data[[ names1[[t]] ]]
                           reg = Regression$new(data, medKonstant)
                           
                           regen[[ names1[[t]] ]] = reg$getSummary()$coefficients[,1][[2]]
                         }
                         s = self$getSummary()
                         remove = c()
                         for(t in seq_along(regen)){
                           if(names1[[t]] == 'y'){
                             next
                           }
                           if(sign(s$coefficients[,1][[t]]) != sign(regen[[ t ]])){
                             remove = c(remove, names1[[t]])
                           }
                         }
                         return(remove)
                       },
                       
                       checkPValue = function(alpha = NULL){
                         if(is.null(alpha)){
                           alpha = private$alpha;
                         }
                         
                         pvalue = self$getPValue();
                         hittad = FALSE;
                         v = c();
                         names1 = names(pvalue);
                         for(i in seq_along(pvalue)){
                           if(is.na(pvalue[[i]])){
                             break;
                           }
                           if(pvalue[[i]] > alpha){
                             v = c(v, names1[[i]]);
                             hittad = TRUE;
                           }
                         }
                         rLista = list();
                         rLista[[1]] = hittad;
                         rLista[[2]] = v;
                         return(rLista);
                       },
                       
                       predict = function(newdata, h, interval){
                         
                         return(predict(private$regressionen, n.ahead=h, newdata = newdata, interval = interval));
                         
                       },
                       
                       get = function(){
                         
                         return(private$regressionen);
                         
                       },
                       
                       reg = function(data, medKonstant){
                         self$uppdateraData(data);
                         
                         if(medKonstant){
                           linearmod <- lm(y~., private$data);
                         }else{
                           linearmod <- lm(y~.+0, private$data);
                         }
                         
                         private$regressionen = linearmod;
                         
                         return(self);
                       },
                       
                       getSummary = function(){
                         
                         return(summary(private$regressionen));
                         
                       },
                       
                       residualerna = function(){
                         s <- summary(private$regressionen);
                         
                        res <- residuals(s);
                        
                        return(res);
                         
                       },
                       
                       getSigma = function(){
                         if(!is.null(private$regressionen)){
                           s <- summary(private$regressionen);
                           return(s$sigma);
                         }
                       },
                       
                       getRSquared = function(){
                         if(!is.null(private$regressionen)){
                           s <- summary(private$regressionen);
                           return(s$r.squared[[1]]);
                         }
                       },
                       
                       getPValue = function(hamtaAlla = TRUE){
                         if(!is.null(private$regressionen)){
                           s <- summary(private$regressionen);
                           pvalues = s$coefficients[,4]
                           if(!hamtaAlla){
                           pvalues = pvalues[-c(1)]
                           }
                           return(pvalues);
                         }
                       },
                       
                       getCoefficients = function(){
                         if(!is.null(private$regressionen)){
                           s <- summary(private$regressionen);
                           return(s$coefficients[,1]);
                         }
                       },
                       
                       getTValue = function(){
                         if(!is.null(private$regressionen)){
                           s <- summary(private$regressionen);
                           return(s$coefficients[,3]);
                         }
                       },
                       
                       getResiduals = function(){
                         if(!is.null(private$regressionen)){
                           s <- summary(private$regressionen);
                           return(residuals(s));
                         }
                       }
                       
                     )
)

ListaMedDataFrames <- R6Class("ListaMedDataFrames",
                      private = list(
                        obj = list()
                      ),
                      public = list(
                        
                        initialize = function(listan){
                          
                          private$obj = listan;
                          
                          
                        },
                        
                        getCuttedObservation = function(index, tidBakat){
                          
                          ## CLONE OBJ
                          obj = private$obj;
                          
                          ## CUT OBSERVATIONS
                          for(h in seq_along(obj)){
                            obj[[h]] <- obj[[h]][(index-(tidBakat-1)):(index),];
                          }
                          
                          return(obj);
                          
                        },
                        
                        konverteraKolumnTillLista = function(lista, kolumnNamn){
                          nyLista = list();
                          
                          for(i in seq_along(lista)){
                            nyLista[[i]] = lista[[i]][[kolumnNamn]];
                          }
                          
                          return(nyLista);
                          
                        },
                        
                        getList = function(){
                          return(self$obj);
                        }
                        
                        
                      )
                      

)

DataFrame <- R6Class("DataFrame",
                              private = list(
                                DataFrame = data.frame()
                              ),
                              public = list(
                                
                                initialize = function(dataframe){
                                  
                                  private$DataFrame = dataframe;
                                  
                                  
                                },
                                
                                taBortKolumner = function(array){
                                  
                                  private$DataFrame = private$DataFrame[ , !(names(private$DataFrame) %in% c(array))];
                                  
                                },
                                
                                get = function(){
                                  return(private$DataFrame);
                                }
                                
                                
                              )
                              
                              
)

Test <- R6Class("Test",
                     private = list(
                       
                     ),
                     public = list(
                       
                       listaMedData = list(),
                       
                       initialize = function(){
                         
                         return(self);
                         
                       },
                       
                       addObservation = function(namn, data){
                         
                         if(length(self$listaMedData[[namn]]) == 0){
                           self$listaMedData[[namn]] = c(data)
                         }else{
                           self$listaMedData[[namn]] = c(self$listaMedData[[namn]], data)
                         }
                         
                       },
                       
                       plot = function(namn){
                         plot(self$listaMedData[[namn]], self$listaMedData[['y']])
                       },
                       
                       findSignificance = function(alpha, withLog = NULL){
                         names1 = names(self$listaMedData)
                         names2 = c()
                         for(t in seq_along(names1)){
                           if(names1[[t]] == 'y'){
                             next;
                           }
                           if(is.null(withLog)){
                           a = Regression$new(data.frame(
                             y = self$listaMedData[['y']],
                             x = as.numeric(self$listaMedData[[names1[[t]]]])
                           ), TRUE)
                           }else{
                             a = Regression$new(data.frame(
                               y = self$listaMedData[['y']],
                               x = log(as.numeric(self$listaMedData[[names1[[t]]]]) )
                             ), TRUE)
                           }
                           s = a$getSummary()
                           if(s$coefficients[,4][[2]] < alpha){
                             names2 = c(names2, names1[[t]])
                           }
                         }
                         return(names2)
                       },
                       
                       regress = function(name1 = NULL, withLog = NULL){
                         if(is.null(name1)){
                         dataFrame = as.data.frame(self$listaMedData)
                         }else{
                           dataFrame = data.frame(
                             y = self$listaMedData[['y']]
                           )
                           for(t in seq_along(name1)){
                             aList = list()
                             if(is.null(withLog)){
                             aList[[ name1[[t]] ]] = as.numeric(self$listaMedData[[ name1[[t]] ]])
                             }else{
                               aList[[ name1[[t]] ]] = log(as.numeric(self$listaMedData[[ name1[[t]] ]]))
                             }
                             dataFrame = cbind(dataFrame, as.data.frame(aList))
                             
                           }
                         }
                         regression = Regression$new(dataFrame, TRUE)
                         return(regression);
                         
                       },
                       
                       addDummy = function(names1, names2){
                         
                         for(t in seq_along(names2)){
                           a = 0;
                           namn = names2[[t]]
                           if(length(which(names1 == namn)) == 0){
                             a = 1
                           }
                           
                           if(length(self$listaMedData[[namn]]) == 0){
                             self$listaMedData[[namn]] = c(a)
                           }else{
                             self$listaMedData[[namn]] = c(self$listaMedData[[namn]], a)
                           }
                           
                         }
                         
                       },
                       
                       addDataframe = function(df){
                         
                         names1 = names(df)
                         
                         for(t in seq_along(names1)){
                           namn = names1[[t]]
                           a = as.numeric(df[[ namn ]]) 
                           if(is.na(a)){
                             a = 0
                           }
                           if(length(self$listaMedData[[namn]]) == 0){
                             self$listaMedData[[namn]] = c(as.numeric(df[[ namn ]]) )
                           }else{
                             self$listaMedData[[namn]] = c(self$listaMedData[[namn]], as.numeric(df[[ namn ]]) )
                           }
                         }
                         
                       },
                       
                       test = function(name1){
                         return(summary(lm(y~., data.frame(y = self$listaMedData[['y']],  x= self$listaMedData[['x']]))) )
                       }
                       
                       
                     )
                     
                     
)


kollaOmSammaTecken <- function(names1, lista_av_vilket_tecken, s){
  remove <- c()
  for(t in seq_along(names1)){
    if(names1[[t]] == '(Intercept)'){
      next
    }
    tecken <- lista_av_vilket_tecken[[ names1[[t]] ]]
    if(s$coefficients[,1][[ names1[[t]] ]] > 0 && tecken > 0){
      next;
    }else if(s$coefficients[,1][[ names1[[t]] ]] < 0 && tecken < 0){
      next;
    }
    
    #if(grepl(names1[[t]], '2')){
    remove <- c(remove, names1[[t]] )
    #}
    
  }
  return(remove);
}

taBortOgitligaVariablerOchRetuneraNyRegression <- function(remove, data1){
  if(!is.null(remove)){
    
    data1 <- data1[ , !(names(data1) %in% c(remove))]
    
    if(!is.data.frame(data1) || length(data1) == 1 ){
      return(NULL)
    }
    
    linearmod <- lm(y~., data1)
    
    s <- summary(linearmod)
    
    #names1 <- names( s$coefficients[,1])
    
  }
  aList <- list()
  aList[['reg']] <- s
  aList[['data1']] <- data1
  return(aList);
}

checkIfWrongSign <- function(s123, names1123){
  wrong <- FALSE
  remove <- c()
  for(t in seq_along(names1123)){
    
    if(names1123[[t]] == 'x9'){
      if(s123$coefficients[,1][['x9']] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'x_m'){
      if(s123$coefficients[,1][['x_m']] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'x8'){
      if(s123$coefficients[,1][['x8']] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'x4'){
      if(s123$coefficients[,1][['x4']] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'x'){
      if(s123$coefficients[,1][['x']] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'x100'){
      if(s123$coefficients[,1][['x100']] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    
    if(names1123[[t]] == 'x5'){
      if(s123$coefficients[,1][['x5']] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'x6'){
      if(s123$coefficients[,1][['x6']] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'policy'){
      if(s123$coefficients[,1][['policy']] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'volume'){
      if(s123$coefficients[,1][['volume']] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'market_volume'){
      if(s123$coefficients[,1][['market_volume']] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'asset_gold'){
      if(s123$coefficients[,1][['asset_gold']] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'asset_oil'){
      if(s123$coefficients[,1][['asset_oil']] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'asset_energy'){
      if(s123$coefficients[,1][['asset_energy']] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
  }
  aList <- list()
  aList[['wrong']] <- wrong
  aList[['remove']] <- remove
  return(aList)
}


checkIfWrongSignPositive <- function(s123, names1123){
  wrong <- FALSE
  remove <- c()
  for(t in seq_along(names1123)){
    
    if(names1123[[t]] == 'FinancialStessIndex'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Fastigheter'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'FinancialStessIndex1'){
      if(s123$coefficients[,1][[t]] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Fordon1'){
      if(s123$coefficients[,1][[t]] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'x9'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Guld'){
      if(s123$coefficients[,1][[t]] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Olja'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Policy'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Industri'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Material'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Teknologi1'){
      if(s123$coefficients[,1][[t]] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Teknologi'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Halsovard'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Telekom1'){
      if(s123$coefficients[,1][[t]] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Fastigheter1'){
      if(s123$coefficients[,1][[t]] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'MaterialPrisIndex'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'MaterialPrisIndex1'){
      if(s123$coefficients[,1][[t]] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'market_volume'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'x'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Guld1'){
      if(s123$coefficients[,1][[t]] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'asset_energy'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Telekom'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Fordon'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Tourism1'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'Tourism'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'beta'){
      if(s123$coefficients[,1][[t]] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'RSI'){
      if(s123$coefficients[,1][[t]] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'RSI_500'){
      if(s123$coefficients[,1][[t]] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'volume'){
      if(s123$coefficients[,1][[t]] > 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
    if(names1123[[t]] == 'x_volume'){
      if(s123$coefficients[,1][[t]] < 0){
        wrong <- TRUE
        remove <- c(remove, names1123[[t]])
      }
    }
    
  }
  aList <- list()
  aList[['wrong']] <- wrong
  aList[['remove']] <- remove
  return(aList)
}

checkValid <- function(s, names1){
  wrong <- FALSE
  remove <- c()
  for(t in seq_along(names1)){
    
    if(names1[[t]] == 'x2'){
      if(length(which(names1 == 'x')) == 0){
        wrong <- TRUE
        remove <- c(remove, names1[[t]])
      }
    }
    
    if(names1[[t]] == 'x42'){
      if(length(which(names1 == 'x4')) == 0){
        wrong <- TRUE
        remove <- c(remove, names1[[t]])
      }
    }
    
    if(names1[[t]] == 'x52'){
      if(length(which(names1 == 'x5')) == 0){
        wrong <- TRUE
        remove <- c(remove, names1[[t]])
      }
    }
    
    if(names1[[t]] == 'x62'){
      if(length(which(names1 == 'x6')) == 0){
        wrong <- TRUE
        remove <- c(remove, names1[[t]])
      }
    }
    
    if(names1[[t]] == 'x82'){
      if(length(which(names1 == 'x8')) == 0){
        wrong <- TRUE
        remove <- c(remove, names1[[t]])
      }
    }
    
    if(names1[[t]] == 'x92'){
      if(length(which(names1 == 'x9')) == 0){
        wrong <- TRUE
        remove <- c(remove, names1[[t]])
      }
    }
    
    if(names1[[t]] == 'x_m2'){
      if(length(which(names1 == 'x_m')) == 0){
        wrong <- TRUE
        remove <- c(remove, names1[[t]])
      }
    }
    
  }
  aList <- list()
  aList[['wrong']] <- wrong
  aList[['remove']] <- remove
  return(aList)
}

checkPValue <- function(s, names1,alpha){
  found <- FALSE
  total <- length(names1)
  andel <- 0
  remove <- c()
  atLeastOneNeedsToHaveATValueAbove3 <- 0
  for(r in seq_along(s$coefficients[,4])){
    if(is.na(s$coefficients[,4][[r]])){
      found <- TRUE
      break;
    }
    if(s$coefficients[,4][[r]] > alpha){
      remove <- c(remove, names1[[r]])
      found <- TRUE
    }
    
    
  }
  aList <- list()
  aList[['found']] <- found
  aList[['remove']] <- remove
  return(aList)
}

removeInf = function(array){
  vol1 = array
  vol1[!is.finite(vol1)] <- 0
  return(vol1);
}

removeHighCorrelated = function(data, limit = NULL){
  if(is.null(limit)){
    limit = 0.3
  }
  
  df2 = cor(data)
  hc = findCorrelation(df2, cutoff=limit) # putt any value as a "cutoff" 
  hc = sort(hc)
  
  aList = list();
  aList[[1]] = hc;
  
 # reduced_Data = data[,-c(hc)]
  
  return(aList);
}

removeHC = function(explanatory_variables, y, multi = 0.4){
  break1 = FALSE
  if(length(explanatory_variables) > 2){
    explanatory_variables = explanatory_variables[ , !(names(explanatory_variables) %in% c('y'))]
    a = removeHighCorrelated(explanatory_variables, multi)
    
    if(!is.null(a[[1]]) && length(a[[1]]) > 0){
      plus = a[[1]]+1
      names_with = names1[-c(1,plus)];
      
      explanatory_variables = explanatory_variables[,-c(a[[1]] )]
      
      if(length(explanatory_variables) <= 1 || !is.data.frame(explanatory_variables)){
        a = list()
        if(length(names_with) == 0 || is.null(names_with) || is.na(names_with)){
          return(NULL)
        }
        a[[ names_with[[1]] ]] = explanatory_variables
        explanatory_variables = a
      }
      
      explanatory_variables = cbind(explanatory_variables, data.frame(
        y = y
      ))
      
      if(length(explanatory_variables) <= 1 || !is.data.frame(explanatory_variables)){
        break1 = TRUE;
      }
      
      "linearmod <- Regression$new(explanatory_variables, TRUE);
      
      s <- linearmod$getSummary()
      
      names1 <- names( s$coefficients[,1])"
    }
  }
  if(break1){
   return(NULL) 
  }else{
  return(explanatory_variables)
  }
}

removeHighPValues = function(linearmod, explanatory_variables, alpha, intercept1 = TRUE){
  if(is.array(alpha)){
  array <- alpha
  }else{
    array = c(alpha)
  }
  break1 = FALSE
  for(d in seq(1:length(array))){
    
    
    check = linearmod$checkPValue(array[[d]]);
    remove = NULL;
    
    b = FALSE
    tryCatch({
      if(check[[1]]){
        remove = check[[2]];
        
        explanatory_variables <- explanatory_variables[ , !(names(explanatory_variables) %in% c(remove))]
        
        if(!is.data.frame(explanatory_variables)){
          break1 = TRUE;
          break;
        }
        
        linearmod <- linearmod$reg(explanatory_variables, intercept1);
        s <- linearmod$getSummary();
        names1 <- names(s$coefficients[,1])
        
      }
    }, 
    error = function(e){
      b = TRUE
    })
    if(b){
      break
    }
  }
  
  if(break1 || b){
    return(NULL)
  }
  a = list()
  a[[1]] = linearmod
  a[[2]] = explanatory_variables
  return(a)
}

checkIfBuySignal = function(explanatory_variables, y, alpha = NULL, multi = 0.3){
  
  if(is.null(alpha)){
    alpha = c(0.4, 0.1, 0.08, 0.05, 0.05, 0.05)
  }
  
  linearmod <- Regression$new(explanatory_variables, TRUE);
  
  s <- linearmod$getSummary()
  
  names1 <- names( s$coefficients[,1])
  
  
  if(length(explanatory_variables) > 2){
    explanatory_variables = explanatory_variables[ , !(names(explanatory_variables) %in% c('y'))]
    a = removeHighCorrelated(explanatory_variables, 0.6)
    
    if(!is.null(a[[1]]) && length(a[[1]]) > 0){
      plus = a[[1]]+1
      names_with = names1[-c(1,plus)];
      
      explanatory_variables = explanatory_variables[,-c(a[[1]] )]
      
      if(length(explanatory_variables) <= 1 || !is.data.frame(explanatory_variables)){
        a = list()
        if(is.null(names_with) || is.na(names_with)){
          return(NULL)
        }
        a[[ names_with[[1]] ]] = explanatory_variables
        explanatory_variables = a
      }
      
      explanatory_variables = cbind(explanatory_variables, data.frame(
        y = y
      ))
      
      if(length(explanatory_variables) <= 1 || !is.data.frame(explanatory_variables)){
        break1 = TRUE;
      }
      
      linearmod <- Regression$new(explanatory_variables, TRUE);
      
      s <- linearmod$getSummary()
      
      names1 <- names( s$coefficients[,1])
    }
  }
  
  if(break1){
    return(NULL)
  }
  
  if(length(explanatory_variables[['y']]) == 0){
    explanatory_variables = cbind(explanatory_variables, data.frame(
      y = y
    ))
    
  }
  
  linearmod <- Regression$new(explanatory_variables, TRUE);
  
  s <- linearmod$getSummary()
  
  names1 <- names( s$coefficients[,1])
  
  if(length(s$coefficients[,1]) <= 1 || length(names1) <= 1 
     || is.na(s$adj.r.squared)
  ){
    return(NULL)
  }
  
  break1 = FALSE
  for(d in seq(1:13)){
    check = checkIfWrongSignPositive(s, names1);
    if(check[[1]]){
      remove = check[[2]]
      explanatory_variables <- explanatory_variables[ , !(names(explanatory_variables) %in% c(remove))]
      
      if(!is.data.frame(explanatory_variables)){
        break1 = TRUE;
        break;
      }
      
      linearmod <- linearmod$reg(explanatory_variables, TRUE);
      s <- linearmod$getSummary();
      names1 <- names(s$coefficients[,1])
      
    }
  }
  
  
  
  if(break1){
    return(NULL)
  }
  
  ## P VALUE
  break1 = FALSE;
  
  if(length(explanatory_variables[['y']]) == 0){
    explanatory_variables = cbind(explanatory_variables, data.frame(
      y = y
    ))
    
  }
  
  array <- alpha
  for(d in seq(1:6)){
    
    
    check = linearmod$checkPValue(array[[d]]);
    remove = NULL;
    if(check[[1]]){
      remove = check[[2]];
      
      explanatory_variables <- explanatory_variables[ , !(names(explanatory_variables) %in% c(remove))]
      
      if(!is.data.frame(explanatory_variables)){
        break1 = TRUE;
        break;
      }
      
      linearmod <- linearmod$reg(explanatory_variables, TRUE);
      s <- linearmod$getSummary();
      names1 <- names(s$coefficients[,1])
      
    }
  }
  
  if(break1){
    return(NULL)
  }
  
  if(length(explanatory_variables[['y']]) == 0){
    explanatory_variables = cbind(explanatory_variables, data.frame(
      y = y
    ))
    
  }
  
  if(length(explanatory_variables) > 2){
    explanatory_variables = explanatory_variables[ , !(names(explanatory_variables) %in% c('y'))]
    a = removeHighCorrelated(explanatory_variables, multi)
    
    if(!is.null(a[[1]]) && length(a[[1]]) > 0){
      plus = a[[1]]+1
      names_with = names1[-c(1,plus)];
      
      explanatory_variables = explanatory_variables[,-c(a[[1]] )]
      
      if(length(explanatory_variables) <= 1 || !is.data.frame(explanatory_variables)){
        a = list()
        a[[ names_with[[1]] ]] = explanatory_variables
        explanatory_variables = a
      }
      
      explanatory_variables = cbind(explanatory_variables, data.frame(
        y = y
      ))
      
      if(length(explanatory_variables) <= 1 || !is.data.frame(explanatory_variables)){
        break1 = TRUE;
      }
      
      linearmod <- Regression$new(explanatory_variables, TRUE);
      
      s <- linearmod$getSummary()
      
      names1 <- names( s$coefficients[,1])
    }
  }
  
  if(break1){
    return(NULL)
  }
  
  if(length(explanatory_variables[['y']]) == 0){
    explanatory_variables = cbind(explanatory_variables, data.frame(
      y = y
    ))
    
  }
  
  break1 = FALSE
  for(d in seq(1:3)){
    check = checkIfWrongSign(s, names1);
    if(check[[1]]){
      remove = check[[2]]
      explanatory_variables <- explanatory_variables[ , !(names(explanatory_variables) %in% c(remove))]
      
      if(!is.data.frame(explanatory_variables)){
        break1 = TRUE;
        break;
      }
      
      linearmod <- linearmod$reg(explanatory_variables, TRUE);
      s <- linearmod$getSummary();
      names1 <- names(s$coefficients[,1])
      
    }
  }
  
  if(break1){
    return(NULL)
  }
  
  
  for(d in seq(1:6)){
    
    
    check = linearmod$checkPValue(array[[d]]);
    remove = NULL;
    if(check[[1]]){
      remove = check[[2]];
      
      explanatory_variables <- explanatory_variables[ , !(names(explanatory_variables) %in% c(remove))]
      
      if(!is.data.frame(explanatory_variables)){
        break1 = TRUE;
        break;
      }
      
      linearmod <- linearmod$reg(explanatory_variables, TRUE);
      s <- linearmod$getSummary();
      names1 <- names(s$coefficients[,1])
      
    }
  }
  
  if(break1){
    return(NULL)
  }
  
  if(length(explanatory_variables[['y']]) == 0){
    explanatory_variables = cbind(explanatory_variables, data.frame(
      y = y
    ))
    
  }
  
  break1 = FALSE
  for(d in seq(1:13)){
    check = checkIfWrongSignPositive(s, names1);
    if(check[[1]]){
      remove = check[[2]]
      explanatory_variables <- explanatory_variables[ , !(names(explanatory_variables) %in% c(remove))]
      
      if(!is.data.frame(explanatory_variables)){
        break1 = TRUE;
        break;
      }
      
      linearmod <- linearmod$reg(explanatory_variables, TRUE);
      s <- linearmod$getSummary();
      names1 <- names(s$coefficients[,1])
      
    }
  }
  
  
  
  if(break1){
    return(NULL)
  }
  
  
  if(length(s$coefficients[,1]) <= 1 || length(names1) <= 1){
    return(NULL)
  }
  
  
  if(is.na(s$adj.r.squared)){
    return(NULL)
  }
  
  if( s$coefficients[,4][['(Intercept)']] > 0.05){
    return(NULL)
  }
  
  ##
  
  "if(length(names1) > 3){
    
    lowest_p_value = sort(linearmod$getPValue())[[3]]
    pvalues = linearmod$getPValue()
    names_pvalue = names(pvalues)
    remove = c()
    for(t in seq_along(pvalues)){
      if(pvalues[[t]] > lowest_p_value){
        remove = c(remove, names_pvalue[[t]])
      }
    }
    
    if(!is.null(remove)){
      explanatory_variables <- explanatory_variables[ , !(names(explanatory_variables) %in% c(remove))]
      
      if(!is.data.frame(explanatory_variables)){
        break1 = TRUE;
      }
      
      linearmod <- linearmod$reg(explanatory_variables, TRUE);
      s <- linearmod$getSummary();
      names1 <- names(s$coefficients[,1])
    }
    
  }"
  
  if(break1){
    return(NULL)
  }
  
  
  return(linearmod);
  
}
get_every1 <- function(array_1, days_1){ 
  g <- c(); 
  
  n <- length(array_1)
  for(i in seq_along(array_1)){
    if(n <= 0 || n-days_1 <= 0){
      break;
    }
    
    g <- c(g, array_1[[n]]);
    
    n <- n - days_1
  }
  return(rev(g)); 
  #return(get_every_every_day(array_1, days_1));
}
get_every <- function(array_1, days_1){ 
g <- c(); 
  
  n <- length(array_1)
  for(i in seq_along(array_1)){
    if(n <= 0 || n-days_1 <= 0){
      break;
    }
    
    g <- c(g, array_1[[n]]);
    
    n <- n - days_1
  }
  return(rev(g));
  #return(get_every_every_day(array_1, days_1));
}
cor_for_every = function(array_1, array_2, days_1){
  g <- c(); 
  
  n <- length(array_1)
  for(i in seq_along(array_1)){
    if(n <= 0 || n-days_1 <= 0){
      break;
    }
    d = cor(
      get_return(array_1[(n-days_1):n], 42),
      get_return(array_2[(n-days_1):n],42)
                 )
      
    g <- c(g, d[[1]]);
    
    n <- n - days_1
  }
  return(rev(g));
}
get_every_every_day <- function(array_1, days_1){ 
  g <- c(); 
  
  n <- length(array_1)
  for(i in seq_along(array_1)){
    if(n <= 0 || n-days_1 <= 0){
      break;
    }
    
    g <- c(g, array_1[[n]]);
    
    n <- n - 1
  }
  return(rev(g)); 
}

checkIfStationary = function(df){
  remove = c()
  names1 = names(df)
  a = c()
  for(t in seq_along(names1)){
    if(adf.test( df[[ names1[[t]] ]] )$p.value > 0.05){
      remove = c(remove, names1[[t]])
    }else{
      a = c(a, names1[[t]])
    }
  }
  if(is.null(remove)){
    return(df)
  }else{
    df = df[ , !(names(df) %in% c(remove))]
    if(!is.null(a) && length(a) <= 1){
      b = list()
      b[[ a[[1]] ]] = df;
      df = as.data.frame(b)
    }
    return(df)
  }
}

atLeastThreeSignificance = function(s){
  count = 0;
  for(t in seq_along(s$coefficients[,4])){
    if(s$coefficients[,4][[t]] < 0.1){
      count = count + 1
    }
  }
  if(count >= 3 && length(s$coefficients[,4]) >= 3){
    return(TRUE)
  }else if(count >= 2 && length(s$coefficients[,4]) == 2){
    return(TRUE)
  }else{
    return(FALSE)
  }
}


raknaSignCoef = function(var, explanatory_variables){
  
  names1 = names(explanatory_variables)
  
  for(t in seq_along(names1)){
    if(names1[[t]] == 'y'){
      next;
    }
    data = list()
    data[['y']] = explanatory_variables[['y']]
    data[[ names1[[t]] ]] = explanatory_variables[[ names1[[t]] ]]
    linearmod = lm(y~., as.data.frame(data))
    s = summary(linearmod)
    if(length(var[[ names1[[t]] ]]) == 0 && sign(s$coefficients[,1][[2]]) == 1){
      var[[ names1[[t]] ]] = 1
    }else if(sign(s$coefficients[,1][[2]]) == 1){
      var[[ names1[[t]] ]] = 1 + var[[ names1[[t]] ]] 
    }
  }
  
  return(var);
  
}

testa_medelvarde = function(x, y){
  t.test(x, y)
}

transform_boolean = function(array){
  return(as.integer(as.logical(array)))
}

allocation_weights_excel_companies <- function(new_co, z, time_span, inte, avgift){
  x_12 <- list()
  for(i in seq_along(new_co)){
    x_12[[new_co[[i]] ]] <- get_return(
      obj1[[ new_co[[i]] ]][['Kurs']][1:z] - avgift[[i]]
      ,inte)
  }
  if(length(x_12) > 1){
    the_portfolio <- as.timeSeries(
      as.data.frame(x_12)
    )
    dd <- tangencyPortfolio(the_portfolio, spec = portfolioSpec(), constraints = 'LongOnly')
    dd_w <- getWeights(dd)
  }else if(length(x_12) == 1){
    dd_w <- list()
    dd_w[[ 
      gsub(paste0("-"),"." ,as.character(names(x_12)[[1]]))
      ]] <- 1
  }
  print(dd)
  return(dd_w)
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
