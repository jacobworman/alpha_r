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
r3 = list()n_inv <- FALSE
track_sd <- list()
track_1 <- c()
used1 <- seq_along(obj_static[[1]][['date']])
# 2090
used1 <- used1[used1 > (time_span+120*9+1) & used1 < (get_last_item(used1)-interv) ]
#used1 <- used1[used1 > 3000 & used1 < 3250]
test1 = sample(used1, 1)
random_sample_z <- c()
  #get_every(used1, 21*3)

for(t in seq_along(used1)){
  if(t %% 21 == 0){
    random_sample_z = c(random_sample_z, used1[[t]])
  }
}

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
  #oil_index = TimeSerie$new(oil_index_static, interv)$setLength2(z, time_span);
  #gold_index = TimeSerie$new(gold_index_static, interv)$setLength2(z, time_span);
  energy_indeks = TimeSerie$new(energy_index, interv)$setLength2(z, time_span)$smoothOut()
  market = TimeSerie$new(index_market, interv)$setLength2(z, time_span)$smoothOut()
  #policy_index = TimeSerie$new(policy_index_static, interv)$setLength2(z, time_span);
  Volumer = TimeSeries$new(vol, interv)$setLength2(z, time_span)$smoothOut()
  sp500_volume_timeserie = TimeSerie$new(sp500_volume, interv)$setLength2(z, time_span)$smoothOut()
  sp500Index = TimeSerie$new(sp500, interv)$setLength2(z, time_span)$smoothOut();
  
  
  bransch_serier = TimeSeries$new(qwert$timeSeries, interv)$setLength2(z, time_span)$smoothOut()
  bransch_serier2 = TimeSeries$new(qwert2$timeSeries, interv)$setLength2(z, time_span)$smoothOut()
  
  framtidaTidsSerie = TimeSeries$new(listaAvTillgangarsPriser, interv)$setLength(z, time_span)$smoothOut()
  #oil_index_framtida = TimeSerie$new(oil_index_static, interv)$setLength(z, time_span);
  #gold_index_framtida = TimeSerie$new(gold_index_static, interv)$setLength(z, time_span);
  energy_indeks_framtida = TimeSerie$new(energy_index, interv)$setLength(z, time_span)$smoothOut()
  market_framtida = TimeSerie$new(index_market, interv)$setLength(z, time_span)$smoothOut()
  #policy_index_framtida = TimeSerie$new(policy_index_static, interv)$setLength(z, time_span);
  framtidaVolumer = TimeSeries$new(vol, interv)$setLength(z, time_span)$smoothOut()
  sp500_volume_timeserie_framtida = TimeSerie$new(sp500_volume, interv)$setLength(z, time_span)$smoothOut()
  sp500IndexFramtida = TimeSerie$new(sp500, interv)$setLength(z, time_span)$smoothOut()
  
  bransch_serier_framtida = TimeSeries$new(qwert$timeSeries, interv)$setLength(z, time_span)$smoothOut()
  bransch_serier_framtida2 = TimeSeries$new(qwert2$timeSeries, interv)$setLength(z, time_span)$smoothOut()
  ## List sources 2
  "tidsSerie2 = TimeSeries$new(listaAvTillgangarsPriser, interv)$setLength2(z, time_span2)$smoothOut()
  #oil_index = TimeSerie$new(oil_index_static, interv)$setLength2(z, time_span);
  #gold_index = TimeSerie$new(gold_index_static, interv)$setLength2(z, time_span);
  energy_indeks2 = TimeSerie$new(energy_index, interv)$setLength2(z, time_span2)$smoothOut()
  market2 = TimeSerie$new(index_market, interv)$setLength2(z, time_span2)$smoothOut()
  #policy_index = TimeSerie$new(policy_index_static, interv)$setLength2(z, time_span);
  Volumer2 = TimeSeries$new(vol, interv)$setLength2(z, time_span2)$smoothOut()
  sp500_volume_timeserie2 = TimeSerie$new(sp500_volume, interv)$setLength2(z, time_span2)$smoothOut()
  sp500Index2 = TimeSerie$new(sp500, interv)$setLength2(z, time_span2)$smoothOut();
  
  
  bransch_serier2 = TimeSeries$new(qwert$timeSeries, interv)$setLength2(z, time_span2)$smoothOut()
  bransch_serier22 = TimeSeries$new(qwert2$timeSeries, interv)$setLength2(z, time_span2)$smoothOut()
  
  framtidaTidsSerie2 = TimeSeries$new(listaAvTillgangarsPriser, interv)$setLength(z, time_span2)$smoothOut()
  #oil_index_framtida = TimeSerie$new(oil_index_static, interv)$setLength(z, time_span);
  #gold_index_framtida = TimeSerie$new(gold_index_static, interv)$setLength(z, time_span);
  energy_indeks_framtida2 = TimeSerie$new(energy_index, interv)$setLength(z, time_span2)$smoothOut()
  market_framtida2 = TimeSerie$new(index_market, interv)$setLength(z, time_span2)$smoothOut()
  #policy_index_framtida = TimeSerie$new(policy_index_static, interv)$setLength(z, time_span);
  framtidaVolumer2 = TimeSeries$new(vol, interv)$setLength(z, time_span2)$smoothOut()
  sp500_volume_timeserie_framtida2 = TimeSerie$new(sp500_volume, interv)$setLength(z, time_span2)$smoothOut()
  sp500IndexFramtida2 = TimeSerie$new(sp500, interv)$setLength(z, time_span2)$smoothOut()
  
  bransch_serier_framtida2 = TimeSeries$new(qwert$timeSeries, interv)$setLength(z, time_span2)$smoothOut()
  bransch_serier_framtida22 = TimeSeries$new(qwert2$timeSeries, interv)$setLength(z, time_span2)$smoothOut()"
  
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
      
      
      
      "count = 0
      for(i in seq_along(companies)){
        avkastning_1 = obj_static[[ companies[[i]] ]][['close']][(z-(250*5)):z]
        avkastning_2 = sp500[(z-(250*5)):z]
        
        returns1 = get_return(avkastning_1, 21)
        returns2 = get_return(avkastning_2, 21)
        
        linearmod = Regression$new(data.frame(
          y = returns1,
          x= returns2
        ), TRUE)
        
        s = linearmod$getSummary()
        
        if(s$coefficients[,1][[2]] > 1){
          count = count + 1
        }
      }
      
      test$addObservation('x', count/length(companies))
      avkastning = (sp500[[z+interv]]- sp500[[z]])/sp500[[z]]
      test$addObservation('y', avkastning)
      
      
      next"
      a_co = c()
      for(i in seq_along(companies)){
        avkastning_1 = obj_static[[ companies[[i]] ]][['close']][(z-(250*5)):z]
        avkastning_2 = sp500[(z-(250*5)):z]
        
        returns1 = get_return(avkastning_1, 21)
        returns2 = get_return(avkastning_2, 21)
        
        linearmod = Regression$new(data.frame(
          y = returns1,
          x= returns2
        ), TRUE)
        
        s = linearmod$getSummary()
        
        if(s$coefficients[,1][[2]] < 1
          && s$coefficients[,1][[2]] > 0.8 ){
          a_co = c(a_co, companies[[i]])
        }
      }
      print("length")
      print(length(a_co))
      #for(p in seq_along(obj_static[[ 1 ]][['close']][z:(z+126)]  )){
        
        dd_w <- allocation_weights(a_co, z, 126, 1)
        avkastning = 0
        for(i in seq_along(a_co)){
          buy = obj_static[[ a_co[[i]] ]][['close']][[z]]
          sell = obj_static[[ a_co[[i]] ]][['close']][[z+1]]
          avkastning =  avkastning + sell/buy*dd_w[[ gsub(paste0('-'),'.' ,a_co[[i]]) ]] 
          
        }
        utveckling = c(utveckling, avkastning)
      #}
      
      
      print("avkastning")
      print(utveckling)
      
      next
      
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
      
      "toppBeta2 = sort(topp_beta2, decreasing = TRUE)[[10]]
      
      for(i in seq_along(companies)){
        
        if(betas2[[i]] < toppBeta2){
          betas = c(betas, 0)
          next
        }
        
        
  
        the_b = framtidaTidsSerie$getIndex(i)$getBeta(sp500IndexFramtida$get(), 1, interv)
        b = mean(the_b)
        betas = c(betas, b)
        topp_beta = c(topp_beta, b)
        
        
      }"
      
      #toppBeta = sort(topp_beta, decreasing = TRUE)[[20]]
      
      "topp_beta2 = c()
      for(i in seq_along(companies)){
        
        topp_beta2 = c(topp_beta2, framtidaTidsSerie$getIndex(i)$getSharpeRatio() )
        
      }
      
      toppBeta2 = sort(topp_beta2, decreasing = TRUE)[[20]]
      
      a_co = c()
      for(i in seq_along(companies)){
        
        if(framtidaTidsSerie$getIndex(i)$getSharpeRatio() > toppBeta2){
          a_co = c(a_co, companies[[i]])
        }
        
      }
      
      topp_beta2 = c()
      for(i in seq_along(companies)){
        
        if(length(which(a_co == companies[[i]])) == 0){
          next
        }
        
        topp_beta2 = c(topp_beta2, cor(
          framtidaTidsSerie$getIndex(i)$getReturn(),
          market_framtida$getReturn()
        )[[1]] )
        
      }
      
      toppBeta2 = sort(topp_beta2, decreasing = TRUE)[[10]]
      
      a_co2 = c()
      for(i in seq_along(companies)){
        
       if(length(which(a_co == companies[[i]])) == 0){
          next
        }

          a_co2 = c(a_co2, companies[[i]])
        }
        
      }"
      
      
      "n = length(obj_static[[1]][['close']][(z-time_span):z])
      p = length(obj_static[[ 1 ]][['close']][((z+interv)-time_span):(z+interv)])
      array1 = obj_static[[1]][['close']][(z-time_span):z]
      array2 = sp500[(z-time_span):z]
      lags = intv
      g = c()
      g2 = c()
      days_1 = interv
      for(w in seq_along(obj_static[[1]][['close']][(z-time_span):z] )){
        
        if(n <= 0 || n-days_1 <= 0){
          break;
        }
        print(n)
        
        sum1 = c()
        sum2 = c()
        for(t in seq_along(a_co2)){
          
        array_1 = obj_static[[ a_co2[[t]] ]][['close']][(z-time_span):z]
          
        array1 = array_1[(n-days_1):n]
        #array2 = marketTimeSerie[(n-days_1):n]
        
        array1_return = get_return(array1, lags)
        array2_return = get_return(array2, lags)
        
        
        linearmod1 = Regression$new(data.frame(
          y = array1_return,
          x = array2_return
        ), TRUE)
        
        s1 = linearmod1$getSummary()
        
        ##
        array_1 = obj_static[[ a_co2[[t]] ]][['close']][((z+interv)-time_span):(z+interv)]
       
          
        array1 = array_1[(p-days_1):p]
        #array2 = marketTimeSerie[(p-days_1):n]
        
        array1_return = get_return(array1, lags)
        array2_return = get_return(array2, lags)
        
        
        linearmod1 = Regression$new(data.frame(
          y = array1_return,
          x = array2_return
        ), TRUE)
        
        s2 = linearmod1$getSummary()
        
        sum1 <- c(sum1, s1$coefficients[,1][[2]]/length(a_co2) );
        sum2 = c(sum2, s2$coefficients[,1][[2]]/length(a_co2) );
        
        }
        
        p = p - days_1
        n = n - days_1
        
        g = c(g, sum(sum1))
        g2 = c(g2, sum(sum2))
        
      }
      
      explanatory_variables12 = data.frame(
        y = g2,
        x= g
      )"
      
      "serie1 = c()
      for(w in seq_along(obj_static[[1]][['close']])){
        sum1 = c()
        for(p in seq_along(a_co2)){
         sum1 = c(sum1, obj_static[[ a_co2[[p]] ]][['close']][[w]]/length(a_co2)) 
        }
        serie1 = c(serie1, sum(sum1))
      }
      
      
      tidsserien = TimeSerie$new(serie1, interv)$setLength2(z, time_span)$smoothOut()
      tidsserienFramtida = TimeSerie$new(serie1, interv)$setLength(z, time_span)$smoothOut()
      
      explanatory_variables12 = data.frame(
        y = tidsserienFramtida$getBeta(sp500IndexFramtida$get(), intv, interv),
        x= tidsserien$getBeta(sp500Index$get(), intv, interv)
      )
      
      linearmod = Regression$new(explanatory_variables12, TRUE)
      
      s = linearmod$getSummary()
      
      print(s)
      
      
      
      b = FALSE
      tryCatch({
        res = linearmod$predict(data.frame(
          x = get_last_item(tidsserienFramtida$getBeta(sp500IndexFramtida$get(), intv, interv))
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
      print(res[1,1])
      if(res[1,1]-s$sigma < 1.1 || s$r.squared < 0.5){
        next
      }
      
      print(s)
      print( (serie1[[ z+interv ]] - serie1[[z]])/serie1[[z]] )
      utveckling = c(utveckling, (serie1[[ z+interv ]] - serie1[[z]])/serie1[[z]] )
      "
      
      
      ### HÄR
      
      topp_beta2 = c()
      betas = c()
     " for(i in seq_along(companies)){
        
        avkastning_1 = obj_static[[ companies[[i]] ]][['close']][(z-(250*5)):z]
        avkastning_2 = sp500[(z-(250*5)):z]
        
        returns1 = get_return(avkastning_1, 21)
        returns2 = get_return(avkastning_2, 21)
        
        linearmod = Regression$new(data.frame(
          y = returns1,
          x= returns2
        ), TRUE)
        
        s = linearmod$getSummary()
        
        if(s$coefficients[,1][[2]] < 1){
          next
        }
        
        if(
          
          cor(
            get_return(obj_static[[ companies[[i]] ]][['close']][(z-126):z],10),
            get_return(sp500[(z-126):z],10)
          )[[1]] < 0.6
        ){
          next
        }
        
        if(
          mean(get_return(obj_static[[ companies[[i]] ]][['close']][(z-126):z],10)) < 0.008 ||
          get_last_item(get_return(obj_static[[ companies[[i]] ]][['close']][(z-126):z],10)) > 0
          || get_last_item(get_return(sp500IndexFramtida$get(), 10)) < 0
        ){
          next
        }
        
        utveckling = c(utveckling, ((obj_static[[ companies[[i]] ]][['close']][[z+10]])/
                         obj_static[[ companies[[i]] ]][['close']][[z]])
                       )
        
       print(get_last_item(utveckling))
      }"
      
    
      
      "topp_beta2 = c()
      for(i in seq_along(companies)){
        
        topp_beta2 = c(topp_beta2, get_last_item(framtidaTidsSerie$getIndex(i)$getReturn()) )
        
      }
      
      toppBeta2 = sort(topp_beta2, decreasing = TRUE)[[20]]"
      
      
      
      
      a_co = companies
      if(FALSE){
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
          
          
          
         " if(betas2[[i]] < toppBeta2){
            next
          }
          "
     
          
          "
          get_last_item(get_return(obj_static[[ companies[[i]] ]][['close']][(z-time_span):(z)],21))
            + sd(get_return(obj_static[[ companies[[i]] ]][['close']][(z-time_span):(z)],21))
            < get_last_item(get_return(obj_static[[ companies[[i]] ]][['close']][(z-time_span-21):(z-21)],21))
            && framtidaTidsSerie$getIndex(i)$getSharpeRatio() > avg_sharpe
          "
          "cor(
              framtidaTidsSerie$getIndex(i)$getReturn(),
              sp500IndexFramtida$getReturn()
            ) > 0.4
            && get_last_item(get_return(sp500[(z-250):z],interv)) >
             get_last_item(get_return(obj_static[[companies[[i]]]][['close']][(z-250):z],interv))"
          
          "
          cor(
              framtidaTidsSerie$getIndex(i)$getReturn(),
              sp500IndexFramtida$getReturn()
            ) > 0.4
            && get_last_item(get_return(sp500[(z-250):z],interv)) >
            get_last_item(get_return(obj_static[[companies[[i]]]][['close']][(z-250):z],interv))
          "
          if(
            #s2$coefficients[,1][[2]] > s$coefficients[,1][[2]]
            #sd(get_return(serie,21*6)) > 0.25
            
            #slump < 10
            
            #obj_static[[ companies[[i]] ]][['close']][[z]] > 100
            #&&
            
            #max(obj_static[[ companies[[i]] ]][['close']][(z-time_span):z]) > 80
            
            #sd(get_return(obj_static[[ companies[[i]] ]][['close']][(z-time_span):z], 21*6)) < 0.2
            #&&
            get_last_item(framtidaTidsSerie$getIndex(i)$getReturn()) > toppBeta2
            #framtidaTidsSerie$getIndex(i)$getSharpeRatio() > avg_sharpe
            
            #&& 
            #get_last_item(fordelning) < mean(fordelning) - sd(fordelning)
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
          
          
         "if(get_last_item(get_return(serie, 1)) < 0.03){
            next
          }"
          
          "linearmod = Regression$new(data.frame(
            y = framtidaTidsSerie$getIndex(i)$getEffect(0.02)
          ), TRUE)
          s = linearmod$getSummary()
          
          
          if(
            s$coefficients[,1][[1]] < 0.6
          ){
            next
          }"
          
          "rsi_n = ceiling(interv)
          rsi3 = RSI(framtidaTidsSerie$getIndex(i)$get() , n = rsi_n)
          rsi3 = rsi3[!is.na(rsi3)]
          
          if(get_last_item(get_every(rsi3, interv)) >  quantile(get_every(rsi3, interv), 0.5)){
            
          }else{
            next
          }"
          
          
         "serie = framtidaTidsSerie$getIndex(i)$getBeta(sp500IndexFramtida$get(), 1, 21)
          median_1 = mean(serie)
          #quantile(serie, 0.98)[[1]]
          #get_last_item(serie) < quantile(serie, 0.95)[[1]] || 
          if(  get_last_item(serie) > quantile(serie, 0.95)[[1]] && get_last_item(serie) > 1){
            pass = TRUE
          }else{
            next
          }"
          
          "if(get_last_item(serie) < quantile(serie, 0.95)[[1]] || get_last_item(serie)*0.9 < 1 ){
            next
          }else{
            print(get_last_item(serie)*0.9)
          }"
          
          
         "rsi_n = ceiling(21*3)
          rsi2 = RSI(tidsSerie$getIndex(i)$get() , n = rsi_n)
          rsi2 = rsi2[!is.na(rsi2)]"
          
          
          "explanatory_variables12 = data.frame(
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
          
          print(s)"
          
          avkastning_1 = obj_static[[ companies[[i]] ]][['close']][(z-(250*5)):z]
          avkastning_2 = sp500[(z-(250*5)):z]
          
          returns1 = get_return(avkastning_1, 21)
          returns2 = get_return(avkastning_2, 21)
          
          linearmod = Regression$new(data.frame(
            y = returns1,
            x= returns2
          ), TRUE)
          
          s = linearmod$getSummary()
          
          ##
          avkastning_1 = obj_static[[ companies[[i]] ]][['close']][((z-126)-(250*5+126)):(z-126)]
          avkastning_2 = sp500[((z-126)-(250*5+126)):(z-126)]
          
          returns1 = get_return(avkastning_1, 21)
          returns2 = get_return(avkastning_2, 21)
          
          linearmod = Regression$new(data.frame(
            y = returns1,
            x= returns2
          ), TRUE)
          
          s2 = linearmod$getSummary()
          
          if(
            s$coefficients[,1][[2]] < 1.5
             ){
            next
          }
          
          print()
          
          serie = obj_static[[ companies[[i]] ]][['close']][(z):(z+interv)]
          
          a = get_return(serie, 5)
          for(t in seq_along(get_return(serie, 5))){
            if(a[[t]] < -0.05){
              utveckling = c( utveckling, obj_static[[ companies[[i]] ]][['close']][[z+interv]]
                              /obj_static[[ companies[[i]] ]][['close']][[z+(t*5)]]
                              )
              break;
            }
          }
          
          
          
          if(
            FALSE
          ){
            #print(res)
            if(
              !is.na(max(obj_static[[ a_co[[i]] ]][['close']]))
            ){
              if(
                TRUE
                #get_last_item(obj[[ a_co[[i]] ]][['close']])/min(obj[[ a_co[[i]] ]][['close']]) < 3
                #max(obj_static[[ a_co[[i]] ]][['close']][ (z-(time_span-1)):z ])/min(obj_static[[a_co[[i]] ]][['close']][ (z-(time_span-1)):z ]) < 15
              ){
                print("First")
                if(length(which(new_companies == a_co[[i]])) == 0){
                  new_companies <- c(new_companies, a_co[[i]])
                  new_sigma = c(new_sigma, s$sigma)
                  constant_true = c(constant_true, intercept1)
                  best_forecast = c(best_forecast,  mean(linearmod$getPValue())  )
                  #sell_at = c(sell_at, mean_1)
                }
                
                #new_names = c(new_names, names1[[2]])
                print(a_co[[i]])
                b = obj_static[[ a_co[[i]] ]][['close']][[(z)]]
                buy = c(b )
                print(obj_static[[ a_co[[i]] ]][['close']][[(z)]])
                a = obj_static[[ a_co[[i]] ]][['close']][(z+1):(z+interv)]
                stop = TRUE
                claim = mean_1
                prevent = FALSE
                prevent2 = FALSE
                prevent3 = TRUE
                brought = 1
                x = 1
                y = 1
                for(r in seq(1:126)){
                  x = r
                  a1 = obj_static[[ a_co[[i]] ]][['close']][[(z+x)]]
                  a2 = obj_static[[ a_co[[i]] ]][['close']][(z+x-10):(z+x)]
                  #a1 = a1[c(TRUE, FALSE)]
                  
                  "if(r > 11){
                  claim = mal
                }

                
                if(mean( (a1-buy)/buy)/claim > 2 && prevent == FALSE && r < 11){
                  if(length(expected_rise2) == 0){
                    claim = mean( (a1-buy)/buy)*3
                  }else{
                    claim = expected_rise2
                  }
                  #claim = mean( (a1-buy)/buy)*3
                  prevent = TRUE
                }"
                  
                  "if(mean( (a1-buy)/buy) >= claim && prevent2 == FALSE && prevent == FALSE){
 
                  if(length(expected_rise) == 0){
                    claim = mean( (a1-buy)/buy)*1.5
                  }else{
                    claim = expected_rise
                  }
                  #claim = mean( (a1-buy)/buy)*3
                  prevent2 = TRUE
                }"
                  
                  "if(mean( (a1-buy)/buy)/claim > 1.5 && prevent2 == FALSE){

                  claim = mean( (a1-buy)/buy)*4
                  prevent2 = TRUE
                }"
                  
                  
                  "if(mean( (obj_static[[ a_co[[i]] ]][['open']][[(z+x)]]-buy)/buy) < 0 && prevent3 == TRUE){
                  if(stop){
                    utveckling <- c(utveckling,  mean(obj_static[[ a_co[[i]] ]][['open']][[(z+x)]]/buy) )
                  }else{
                    utveckling = c(utveckling, ((mean(obj_static[[ a_co[[i]] ]][['open']][[(z+x)]]/buy)-1)*0.8+1) )
                  }
                  sell_at = c(sell_at,  mean(obj_static[[ a_co[[i]] ]][['open']][[(z+x)]]/buy))
                  time_obs = c(time_obs, x)
                  break;
                }
                "
                  "if(mean( (obj_static[[ a_co[[i]] ]][['close']][[(z+x)]]-buy)/buy) < 0){
                  if(stop){
                    utveckling <- c(utveckling,  mean(a1/buy) )
                  }else{
                    utveckling = c(utveckling, ((mean(a1/buy)-1)*0.8+1) )
                  }
                  sell_at = c(sell_at,  mean(a[[x]]/buy))
                  time_obs = c(time_obs, x)
                  break;
                }"
                  
                  
                  if( mean( (a1-buy)/buy)  >= 0.01 && prevent3 == TRUE){
                  print(mean(a1/buy))
                  if(stop){
                    utveckling <- c(utveckling,  mean(a1/buy) )
                  }else{
                    utveckling = c(utveckling, ((mean(a1/buy)-1)*0.8+1) )
                  }
                  #sell_at = c(sell_at,  mean(a[[x]]/buy))
                  time_obs = c(time_obs, x)
                  break;
                }
                  
                  
                  
                  if(y >= length(a) || r == 126){
                    print(mean(a1/buy))
                    time_obs = c(time_obs, x)
                    #sell_at = c(sell_at,  mean(a[[x]]/buy))
                    if(stop){
                      utveckling <- c(utveckling, mean(a1/buy) )
                    }else{
                      utveckling <- c(utveckling, ((mean(a1/buy)-1)*0.8+1) )
                    }
                    break;
                  }
                  
                  y = y + 1
                }
                
                "if(length(utveckling[utveckling < 1]) > 0 && length(utveckling[utveckling >= 1]) > 0){
              andel = length(utveckling[utveckling < 1])/length(utveckling)
              
               mal = 1.0125/(1-andel) - mean(utveckling[utveckling < 1])*andel/(1-andel)
               mal = mal - 1
              }"
                
                "dagar = mean(time_obs)
              per_ar = 252/dagar
              andel = length(utveckling[utveckling < 1])/length(utveckling)
              if((mean(utveckling[utveckling < 1])*andel + ((mean(utveckling[utveckling >= 1]))*(1-andel)))^(252/mean(time_obs)) < 1.25 
                 && length(utveckling[utveckling < 1]) > 0 && length(utveckling[utveckling >= 1]) > 0 && length(utveckling) > 5){
                
                 x = 1.25^(1/per_ar)/(1-andel) - mean(utveckling[utveckling < 1])*andel/(1-andel) - mal - 1
                 
                 mal = mal + x
                
                
              }"
                
                "dagar = mean(time_obs)
              per_ar = 252/dagar
              resultatet = (1.25)^(1/per_ar)
              mal = resultatet - 1
              if(length(utveckling[utveckling < 1]) > 0 && length(utveckling[utveckling >= 1]) > 0 && length(utveckling) > 14){
                andel = length(utveckling[utveckling < 1])/length(utveckling)
                mal =  resultatet/(1-andel) - (andel*mean(utveckling[utveckling < 1]))/(1-andel) - 1
              }
              
              dagar = mean(time_obs)
              per_ar = 252/dagar
              
              
              mean(utveckling[utveckling < 1])*andel + (mean(utveckling[utveckling >= 1])*(1-andel)) = resultatet
              
              (mean(utveckling[utveckling < 1])*andel + ((mean(utveckling[utveckling >= 1]))*(1-andel)))^(252/mean(time_obs)) > 1.25"
                
                print(s)
                print(res)
                #a <- obj_static[[ a_co[[i]] ]][['close']][(z):(z+interv)]
                a = obj_static[[ a_co[[i]] ]][['close']][(z+interv):(z+interv)]
                b = obj_static[[ a_co[[i]] ]][['close']][(z):(z)]
                last = mean(a[c(TRUE, FALSE)]/b[c(TRUE, FALSE)] )
                #utveckling <- c(utveckling, 
                #                mean(a[c(TRUE, FALSE)]/b[c(TRUE, FALSE)] ) )
                utveckling_a = c(utveckling_a, get_last_item(utveckling))
                utveckling_b = c(utveckling_b, last)
                #get_last_item(utveckling)*0.2+
                "if(get_last_item(utveckling) < 0.9){
                utveckling[[length(utveckling)]] = get_last_item(utveckling)
              }else{
                utveckling[[length(utveckling)]] = last
              }"
                if(TRUE){
                  utveckling[[length(utveckling)]] = last
                  
                  print(get_last_item(utveckling))
                }
                d = data.frame(
                  datum = obj_static[[ companies[[i]] ]][['date']][[z]],
                  utveckling = get_last_item(utveckling),
                  #time_obs = get_last_item(time_obs),
                  company = companies[[i]],
                  forecast = res[1,1]
                  # coefficients = get_last_item(names1)
                )
                if(length(report) == 0){
                  report = d
                }else{
                  report = rbind(report, d)
                }
                
                
              }
            }
            
          }
          
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



sample_1 = sample(utveckling, 5)
sample_1[[5]] = (sample_1[[5]] - 1)*0.5 + 1
money = 1000
money1 = 1000
for(t in seq_along(sample_1)){
  money1 = money1*sample_1[[t]]
  print(money1)
}
print(money1)

print(length(utveckling[utveckling < 1])/length(utveckling))
print(length(utveckling_a[utveckling_a < 1])/length(utveckling_a))
