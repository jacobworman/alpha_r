## aggregate(utveckling ~ format(report$datum, "%Y"), report, mean)

library('lmtest')
library('gvlma')
library('car')
utveckling <- c()
utveckling2 <- c()

kopta <- data.frame(list(1,2))
colnames(kopta) <- c('company','date')
money <- 1000
a <- 0

history <- data.frame(list(0, 0, 'company', 0))
colnames(history) <- c('company', 'brought_date', 'sold_date', 'utveckling')
history <- history[0,]

avg_k <- c()
me <- list()

track_global <- list()
track_common <- list()

a <- c()
b1 <- c()
c1 <- c()
c12 <- c()
c13 <- c()
c14 <- c()
c15 <- c()
forecast_error <- c()

new_companies <- c();
new_companies2 <- c();
count <- 0
library('PerformanceAnalytics')
library("fPortfolio")
library("TTR")

#runif(1, 400, 850)
time_span <- 252*10
time_period <- time_span
time_span2 <- 252*10
#time_span2 <- 406
index_copy <- NULL
selection_done <- FALSE
interv <- 21*6
interv2 <- (21*3)*2
stop <- FALSE
loop_start_date <- NULL
count <- 0
count_1 <- 0
count_2 <- 0
last_time_ <- NULL
hold_date <- NULL
pass <- FALSE
pop <- FALSE
pop2 <- FALSE
oT
#used1[seq(1, length(used1), 10)][1:10]
#used1[seq(1, length(used1), 21*3)]
#sample(used1, 10)
#seq(1:20)+468+20
#sample(used1, 20)
#seq(1:20)+532+20
#seq(1:40)+1627+20*6
#sample(used1, 20)
global_i <- 1

mal <- 0.02

test = Test$new()
r1 = list()
r2 = list()
r3 = list()
track_sd <- list()
track_1 <- c()
used1 <- seq_along(obj_static[[1]][['date']])
# 2090
used1 <- used1[used1 > (time_span+120*9+1) & used1 < (get_last_item(used1)-interv) ]
#used1 <- used1[used1 > 3000 & used1 < 3250]
test1 = sample(used1, 1)
random_sample_z <- c()
#get_every(used1, 21*3)

random_sample_z = c(random_sample_z, length(obj_static[[1]][['date']]))

#sample(used1, 15)
#sample(used1, 20)
#c(test1, test1+2,test1+2*2,test1+2*3,test1+2*4,test1+2*5)
#used1[seq(1, length(used1), 10)]
#used1[seq(1, length(used1), 21*3)]
#c(1205+21*3, 1205+21*4, 1205+21*5) <-- TES
r_total = 0;
r2_total = 0;
r3_total = 0;
testUtveckling = c()
time_obs = c()
total = 0
total_pos = 0
alpha = 0.01
lista_med_modeller = list()
new_companies <- c()
best_forecast = c()
constant_true = c()

utveckling_a = c()
utveckling_b = c()

investera_i_bolagen = c()


mal = 0.10
dagar = 0

plot1 = function(name){
  plot(explanatory_variables[[name]], explanatory_variables[['y']])
}

## all_companies (variabkle)
#companies = sample(all_companies, 100)

listaMedAllaTillgangar = ListaMedDataFrames$new(obj_static);
for(z in seq_along(obj_static[[1]][['close']])){
  if(z-time_span <= 0 || z+interv+1 > length(obj_static[[1]][['close']]) ){
    next;
  }
  
  #companies = sample(all_companies, 50)
  z <- random_sample_z[[global_i]]
  global_i <- global_i + 1
  
  print(obj_static[[1]][['date']][[z]])
  
  
  ## CUT OBSERVATIONS
  #obj = listaMedAllaTillgangar$getCuttedObservation(z, time_span);
  listaAvTillgangarsPriser = listaMedAllaTillgangar$konverteraKolumnTillLista(obj_static, 'close');
  vol = listaMedAllaTillgangar$konverteraKolumnTillLista(obj_static, 'volume');
  
  ## List sources
  tidsSerie = TimeSeries$new(listaAvTillgangarsPriser, interv)$setLength2(z, time_span)$smoothOut()
  sp500Index = TimeSerie$new(sp500, interv)$setLength2(z, time_span)$smoothOut();
  
  framtidaTidsSerie = TimeSeries$new(listaAvTillgangarsPriser, interv)$setLength(z, time_span)$smoothOut()
  sp500IndexFramtida = TimeSerie$new(sp500, interv)$setLength(z, time_span)$smoothOut()

  
  best_sharpe_ratio = c()
  if(TRUE){
    
    
    
    if(TRUE){
      
      
      
      ## MONTHLY
      monthly_returns <- tidsSerie$getMonthlyReturns();
      
      
      ## SD
      avg_sd <- tidsSerie$getAverageSD();
      sd_sd <- tidsSerie$getAverageSDSD();
      
      ## mean
      avg_mean <- tidsSerie$getAverageMean();
      
      avg_sharpe <- tidsSerie$getAverageSharpeRatio();
      
      sd_sharpe <- tidsSerie$getAverageSharpeRatioSD();
      
      a_co <- c()
      
      all_martin_ratio = c()
      all_sharpe_ratio = c()
      
      "serie2 = RSI(sp500IndexFramtida$get(), n = 21*2)
      serie2 = serie2[!is.na(serie2)]
      serie2 = na.locf(serie2, fromLast = TRUE)
      serie2 = get_every(serie2, 21*2)
      print(get_last_item(serie2))
      if(get_last_item(serie2) > quantile(serie2, 0.8)){
        next
      }"
      
      "if(get_last_item(get_return(sp500[(z-250):z],6)) < 0.02){
        next
      
      }"
      
      
      
      topp_beta = c()
      betas = c()
      betas2 = c()
      topp_beta2 = c()
      
      #for(i in seq_along(companies)){
      
      "rsi_n = ceiling(21)
        rsi2 = RSI(framtidaTidsSerie$getIndex(i)$get() , n = rsi_n)"
      
      "the_b = framtidaTidsSerie$getIndex(i)$getBeta(sp500IndexFramtida$get(), 1, 21*6)
        betas2 = c(betas2, get_last_item(the_b))
        topp_beta2 = c(topp_beta2,get_last_item( the_b))"
      
      #}
      
      #toppBeta2 = sort(topp_beta2, decreasing = TRUE)[[10]]
      
      "for(i in seq_along(companies)){
        
        if(betas2[[i]] < toppBeta2){
          betas = c(betas, 0)
          next
        }
        
        
  
        the_b = framtidaTidsSerie$getIndex(i)$getBeta(sp500IndexFramtida$get(), 1, interv)
        b = mean(the_b)
        betas = c(betas, b)
        topp_beta = c(topp_beta, b)
        
        
      }
      
      toppBeta = sort(topp_beta, decreasing = TRUE)[[20]]"
      
      topp_beta2 = c()
      for(i in seq_along(companies)){
        
        topp_beta2 = c(topp_beta2, obj_static[[ companies[[i]] ]][['close']][[z]])
        
      }
      
      #toppBeta2 = sort(topp_beta2, decreasing = TRUE)[[100]]
      
      
      
      
      a_co = companies
      if(TRUE){
        new_names = c()
        new_sigma = c()
        counter_positive = 0
        total = 0;
        array_with_future_returns = c()
        new_companies2 = c()
        counter = c()
        sell_at = c()
        
        count = 0
        
        "for(i in seq_along(companies)){
          if(get_last_item(framtidaTidsSerie$getIndex(i)$getBeta(sp500IndexFramtida$get(), intv, interv)) > 1){
            count = count + 1
          }
          print(count)
        }
        
        test$addObservation('x', count/length(companies))
        avkastning = (sp500[[z+interv]]- sp500[[z]])/sp500[[z]]
        test$addObservation('y', avkastning)
        
        
        print(count/length(companies))
        if(count/length(companies) < 0.55){
          
          next
        }
        "
        for(i in seq_along(companies)){
          
          slump = runif(1, min=0, max=100)
          
          print("yes")

          if(
            TRUE
          ){
            
          }else{
            next
          }
          
          
          "serie = obj_static[[ companies[[i]] ]][['close']][(z-interv):z]
          
          linearmod = Regression$new(data.frame(
            y = serie,
            x= seq_this(serie)
          ), TRUE)
          s = linearmod$getSummary()
          
          if(s$coefficients[,1][[2]] < 0
             || s$coefficients[,1][[2]] > 0.02
          ){
            next
          }"
          
          
          
          explanatory_variables12 = data.frame(
            y = framtidaTidsSerie$getIndex(i)$getBeta(sp500IndexFramtida$get(), intv, interv),
            x= tidsSerie$getIndex(i)$getBeta(sp500Index$get(), intv, interv)
          )
          
          linearmod = Regression$new(explanatory_variables12, TRUE)
          
          s = linearmod$getSummary()
          
          
          
          b = FALSE
          tryCatch({
            res = linearmod$predict(data.frame(
              x = get_last_item(framtidaTidsSerie$getIndex(i)$getBeta(sp500IndexFramtida$get(), intv, interv))
            ), 1, 'confidence');
          }, 
          error = function(e){
            b = TRUE
          })
          if(b){
            next;
          }
          
          if(is.na(res[1,1]) ){
            next;
          }
          
          if(res[1,1]-s$sigma*1/3 < 1.15 || s$r.squared < 0.3){
            next
          }
          
          print(s)
          print(companies[[i]])
          
          investera_i_bolagen = c(investera_i_bolagen, companies[[i]])
          
        }
        
        
        
        
      }
      
      #buy_signal =  s$coefficients[,1][[1]] +s$coefficients[,1][[2]]*counter_positive/total
      
      
      "test$addObservation('x', counter_positive/total )
      test$addObservation('y', mean(array_with_future_returns))
      
      linearmod = Regression$new(data.frame(
        y= test$listaMedData[['y']],
        x =test$listaMedData[['x']]
      ), TRUE)
      s = linearmod$getSummary()
      print(linearmod$getSummary())"
      
      
      #new_companies = co_sharpe
      if(FALSE){
        "limit = 15
        
        if(length(new_companies) >= 15){
        best_forecast_1 = sort(best_forecast)[[limit]]
        a = c()
        for(t in seq_along(new_companies)){
          if(best_forecast[[t]] <= best_forecast_1){
            a = c(a, new_companies[[t]])
          }
        }
        new_companies = a
        }"
        
        print("YES ALLOCATION")
        print("length")
        print(length(new_companies))
        
        dd_w <- allocation_weights(new_companies, z , time_span, interv)
        
        ut = c()
        for(i in seq_along(new_companies)){
          series = dj[z:(z+interv)]
          series2 = obj_static[[ new_companies[[i]] ]][['close']][z:(z+interv)]
          buy = obj_static[[ new_companies[[i]] ]][['close']][[z]]
          sell = obj_static[[ new_companies[[i]] ]][['close']][[z+interv]]
          "
          if(sell/buy < 0.99){
            sell = obj_static[[ new_companies[[i]] ]][['close']][[z+interv*2]]
          }"
          
          buys = c(buy)
          for(t in seq_along(series)){
            if(t == 1){
              break;
            }
            if(FALSE){
              if( (series2[[t]] - series2[[1]])/series2[[1]] >= sell_at[[i]]){
                sell = obj_static[[ new_companies[[i]] ]][['close']][[z+t]]
                break;
              }
            }
          }
          if(FALSE){
            #dd_w[[ gsub(paste0('-'),'.' ,new_companies[[i]]) ]] 
            ut = c(ut, sell/mean(buys)*dd_w[[ gsub(paste0('-'),'.' ,new_companies[[i]]) ]]  )
          }else{
            ut = c(ut, sell_at[[i]]*dd_w[[ gsub(paste0('-'),'.' ,new_companies[[i]]) ]] )
          }
        }
        
        ut = sum(ut)
        money = money*sum(ut)
        
        print("AVKASTNING")
        print(sum(ut))
        testUtveckling = c(testUtveckling, sum(ut))
        global_test = c(global_test, sum(ut))
        new_companies = c()
        best_forecast = c()
        constant_true = c()
        sell_at = c()
        
        #test$addObservation('y', sum(ut))
        
        #linearmod = Regression$new(data.frame(
        #  y= test$listaMedData[['y']],
        #  x =test$listaMedData[['x']]
        #), TRUE)
        #s = linearmod$getSummary()
        #print(linearmod$getSummary())
        
      }else{
        new_companies = c()
        constant_true = c()
        best_forecast = c()
        sell_at = c()
      }
      
      
    }
    
  }
  
}


print(investera_i_bolagen)
