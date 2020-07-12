  ## aggregate(utveckling ~ format(report$datum, "%Y"), report, mean)

seq(1:(length(report[['datum']])- length(utveckling))) + length(report[['datum']])- length(utveckling)
  
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
  
  test2 = Test$new()
  r1 = list()
  r2 = list()
  r3 = list()n_inv <- FALSE
  track_sd <- list()
  track_1 <- c()
  used1 <- seq_along(obj_static[[1]][['date']])
  # 2090
  used1 <- used1[used1 > (time_span+126+1) & used1 < (get_last_item(used1)-interv) ]
  #used1 <- used1[used1 > 3000 & used1 < 3250]
  test1 = sample(used1, 1)
  random_sample_z <- get_every(used1, 18)
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
  time_obs2 = c()
  total = 0
  total_pos = 0
  alpha = 0.01
  lista_med_modeller = list()
  new_companies <- c()
  best_forecast = c()
  constant_true = c()
  
  utveckling_a = c()
  utveckling_b = c()
  utveckling_c = c()
  
  graf_over_kop_salj = list()
  kop_salj = list()
  reg1 = list()
  
  per_intervall_utveckling = list()
  
  
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
        
        "serie2 = RSI(sp500IndexFramtida$get(), n = 21*3)
        serie2 = serie2[!is.na(serie2)]
        serie2 = na.locf(serie2, fromLast = TRUE)
        serie2 = get_every(serie2, 21*3)
        print(get_last_item(serie2))
        if(get_last_item(serie2) > quantile(serie2, 0.25)){
          next
        }"
        
        
        
        ## MONTHLY
        monthly_returns <- tidsSerie$getMonthlyReturns();
        
        
        ## SD
        avg_sd <- framtidaTidsSerie$getAverageSD();
        sd_sd <- framtidaTidsSerie$getAverageSDSD();
        
        ## mean
        avg_mean <- tidsSerie$getAverageMean();
        
        avg_sharpe <- framtidaTidsSerie$getAverageSharpeRatio();
        
        sd_sharpe <- framtidaTidsSerie$getAverageSharpeRatioSD();
        
        a_co <- c()
        
        all_martin_ratio = c()
        all_sharpe_ratio = c()
        
        #toppBorsVarde = sort(toppBorsVarde)[[30]]
        
        
        for(i in seq_along(companies)){
          
          
          #best_sharpe_ratio = c(best_sharpe_ratio, tidsSerie$getIndex(i)$getSharpeRatio())
          
          if(
            TRUE
          ){
            
            a_co <- c(a_co, companies[[i]])
          }
          
        }
        
        #toppSharpe = sort(best_sharpe_ratio, decreasing = TRUE)[[15]]
        
        co_sharpe = c()
        "for(i in seq_along(companies)){
          
          
          if(
            tidsSerie$getIndex(i)$getSharpeRatio() > toppSharpe
          ){
            
            co_sharpe <- c(co_sharpe, companies[[i]])
          }
          
        }"
        
        topp_beta2 = c()
        for(i in seq_along(companies)){

        topp_beta2 = c(topp_beta2, obj_static[[ companies[[i]] ]][['close']][[z]])
        
        }
        
        toppBeta2 = sort(topp_beta2, decreasing = TRUE)[[150]]
        
      
        if(TRUE){
        new_names = c()
        new_sigma = c()
        counter_positive = 0
        total = 0;
        array_with_future_returns = c()
        new_companies2 = c()
        counter = c()
        sell_at = c()
        new_found = TRUE
        for(i in seq_along(companies)){
          
          slump = runif(1, min=0, max=100)
          serie = framtidaTidsSerie$getIndex(i)$get()
          n = length(serie)
          fordelning = c()
          for(t in seq_along(serie)){
            if(n <= 1 || n-14 < 1){
              break;
            }
            avkastning = (serie[[n]]-serie[[n-14]])/serie[[n-14]]
            fordelning = c(fordelning, mean(avkastning))
            n = n - 14
          }
          fordelning = rev(fordelning)
          
          
          
          avkastning_1 = obj_static[[ companies[[i]] ]][['close']][(z-(250*5)):z]
          avkastning_2 = sp500[(z-(250*5)):z]
          
          returns1 = get_return(avkastning_1, 126)
          returns2 = get_return(avkastning_2, 126)
          
          linearmod = Regression$new(data.frame(
            y = returns1,
            x= returns2
          ), TRUE)
          
          s = linearmod$getSummary()
          
          
          row1 <- list_of_companies_with_cointegrated_relationship[
            which(list_of_companies_with_cointegrated_relationship$company1 == as.character(companies[[i]])), ];
          row2 <- list_of_companies_with_cointegrated_relationship[
            which(list_of_companies_with_cointegrated_relationship$company2 == as.character(companies[[i]])), ];
          
          co2_name = NULL
          sec = 0
          if(nrow(row1) > 0){
            sec = 1
            if(nrow(row1) > 1){
              a = length(row1) - 1
              co2_name = as.character(row1[2,2])
            }else{
              co2_name = row1$company2
            }
          }else if(nrow(row2) > 0){
            if(nrow(row2) > 1){
              co2_name = as.character(row2[2,1])
            }else{
              co2_name = row2$company1
            }
            sec = 2
          }else{
            next
          }
          
          
          
          co = obj_static[[ companies[[i]] ]][['close']][(z-(250*2)):z]
          co2 = obj_static[[ co2_name ]][['close']][(z-(250*2)):z]
          
          if(sec == 1){
          linearmod <- lm(y ~., data.frame(
            y =  (co),
            x= (co2)
          ))
          }else{
            linearmod <- lm(y ~., data.frame(
              y =  (co2),
              x= (co)
            ))
          }
          
          spread <- linearmod$residuals
          #print("Spread")
          "print(get_last_item(spread))
          print(sd(spread)*2*-1)"
          if(get_last_item(spread) < sd(spread)*2*-1 && sec == 1
             || get_last_item(spread) > sd(spread)*2 && sec == 2){
            print("YEs")
            
          }else{
            next
          }
          
          
          n = length(obj_static[[ companies[[i]] ]][['close']][(z-(time_span)):z])
          aList = list(y = c(), x=c())
          for(t in seq_along(obj_static[[ companies[[i]] ]][['close']][(z-(time_span)):z])){
            
            if((z-(250*2)-t) <= 0){
              break
            }
            
            if(t %% 18 != 0){
              next
            }
            
            co = obj_static[[ companies[[i]] ]][['close']][(z-(250*2)-t):(z-t)]
            co2 = obj_static[[ co2_name ]][['close']][(z-(250*2)-t):(z-t)]
            
            if(sec == 1){
              linearmod <- lm(y ~., data.frame(
                y =  (co),
                x= (co2)
              ))
            }else{
              linearmod <- lm(y ~., data.frame(
                y =  (co2),
                x= (co)
              ))
            }
            
            spread <- linearmod$residuals
            
            if(get_last_item(spread) < sd(spread)*2*-1 && sec == 1
               || get_last_item(spread) > sd(spread)*2 && sec == 2){
              
              aList[['x']] = c(aList[['x']], 1)
              
            }else{
              aList[['x']] = c(aList[['x']], 0)
            }
            
            avkastning = obj_static[[ companies[[i]] ]][['close']][(z-t+1):(z-t+1+interv)]/obj_static[[ companies[[i]] ]][['close']][[(z-t)]]
            
            if(any(avkastning > 1.02)){
              aList[['y']] = c(aList[['y']], 1)
            }else{
              aList[['y']] = c(aList[['y']], 0)
            }
          }
          
          df = as.data.frame(aList)
          
          linearmod = Regression$new(df,FALSE)
          s= linearmod$getSummary()
          
          
          if(length(s$coefficients[,1]) == 0){
            next
          }
          print(s)
          print(s$coefficients[,1][[1]])
          if(s$coefficients[,1][[1]] < 0.95 || length(df[['x']][ df[['x']] > 0 ]) > 10){
            next
          }
          
          if(
            #s2$coefficients[,1][[2]] > s$coefficients[,1][[2]]
            #sd(get_return(serie,21*6)) > 0.25
            
            #s$coefficients[,1][[2]] < 1.1
            #&& s$coefficients[,1][[2]] > 0.7
            framtidaTidsSerie$getIndex(i)$getSharpeRatio() > (avg_sharpe)
            #&&
            
            #sd(get_return(obj_static[[ companies[[i]] ]][['close']][(z-time_span):z], 21*6)) > 0.3
            #&& mean(framtidaTidsSerie$getIndex(i)$getReturn()) > 0.005
            #&& 
            #get_last_item(fordelning) < mean(fordelning) - sd(fordelning)
          ){
            
          }else{
            next
          }
          
          np = i
          i = which(companies == co2_name)
          
          print(companies[[i]])
          
          if(FALSE){
          
          "if(
            get_last_item(get_return(obj_static[[ companies[[i]] ]][['close']][(z-time_span):z], 21))
            < 
            get_last_item(get_return(obj_static[[ companies[[i]] ]][['close']][((z-21)-time_span):(z-21)], 21))
          ){
            
          }else{
            next
          }"
          
          
          "serie = framtidaTidsSerie$getIndex(i)$getBeta(sp500IndexFramtida$get(), 1, 21*6)
          median_1 = mean(serie)
          if(get_last_item(serie) > 1.1 ){
            
          }else{
            next
          }"
          
          "linearmod = Regression$new(data.frame(
            y = framtidaTidsSerie$getIndex(i)$getEffect(0.08)
          ), TRUE);
          s = linearmod$getSummary()
          
          print(s$coefficients[,1][[1]])
          
          if(s$coefficients[,1][[1]] < 0.60){
            next
          }"
          
          "rsi_n = ceiling(21*6)
          rsi3 = RSI(framtidaTidsSerie$getIndex(i)$get() , n = rsi_n)
          rsi3 = rsi3[!is.na(rsi3)]
          rsi3 = get_every(rsi3, 21*6)
          if(get_last_item(rsi3) >  quantile(rsi3, 0.80)){
            
          }else{
            next
          }"
          
          
          
          
          "if(res[1,1] > 0.95 &&  s$coefficients[,4][[2]] < 0.05){
              
            }else{
              next
            }"
          
          "serie = framtidaTidsSerie$getIndex(i)$getBeta(sp500IndexFramtida$get(), 1, 21*6)
          median_1 = mean(serie)
          #quantile(serie, 0.98)[[1]]
          if( get_last_item(serie) < quantile(serie, 0.70)[[1]]
              ||
              get_last_item(serie) > quantile(serie, 0.95)[[1]] ){
            next
          }"
          
          
  
    
  
          
          ## HERE
          
          
          b1 = FALSE
          tryCatch({
            "cointegrationTermAssetMarket = Cointegration$new()$makeCointegrationTerms(
              tidsSerie$getIndex(i)$get(),
              sp500Index$get(),
              framtidaTidsSerie$getIndex(i)$get(),
              sp500Index$get()
            )
          "
            cointegrationTermAssetMarket = Cointegration$new()$makeCointegrationTermsForEvery(
              tidsSerie$getIndex(i)$get(),
              sp500Index$get(),
              framtidaTidsSerie$getIndex(i)$get(),
              sp500IndexFramtida$get(),
              interv
            )
            
            #cointegrationTermAssetMarket[[1]] = get_every(cointegrationTermAssetMarket[[1]], interv)
            
            
            "cointegrationTermAssetEnergy = Cointegration$new()$makeCointegrationTermsForEvery(
              tidsSerie$getIndex(i)$get(),
              energy_indeks$get(),
              framtidaTidsSerie$getIndex(i)$get(),
              energy_indeks_framtida$get(),
              interv
            )"
          }, 
          error = function(e){
            b1 = TRUE
          })
          
          if(b1){
            next;
          }
          
  
          
          
          rsi_n = ceiling(21*3)
         rsi2 = RSI(tidsSerie$getIndex(i)$get() , n = rsi_n)
          len = length(rsi2)
         # rsi2 = rsi2[(252+interv):len]
          rsix = rsi2[!is.na(rsi2)]
          rsi2 = na.locf(rsi2, fromLast = TRUE)
          focused = 51.3
          rsi1 = get_every(rsi2,interv)
          #rsi1 = rsi1[(2520-2394+1):length(rsi1)]
          
          rsi_500 = RSI(sp500Index$get(), n = rsi_n)
          rsix = rsi_500[!is.na(rsi_500)]
          rsi_500 = na.locf(rsi_500, fromLast = TRUE)
          rsi_500_r = get_every(rsi_500, interv)
          
          
          "if(all(rsi1 == FALSE)){
            next;
          }"
          
          ## FDIRST
          ## DEFINE EXPLANATORY VARIABLES
          explanatory_variables = data.frame(
            
            #beta = tidsSerie$getIndex(i)$getReturnEveryDay() - market$getReturnEveryDay(),
            #x100 = tidsSerie$getIndex(i)$getReturnSmoothEveryDay()
            #x4 = oil_index$getReturnEveryDay(),
            #x5 = gold_index$getReturnEveryDay(),
            
            ## interactions terms
            #x_x9 = tidsSerie$getIndex(i)$getReturnEveryDay()*cointegrationTermAssetMarket[[1]],
            #x_x8 = tidsSerie$getIndex(i)$getReturnEveryDay()*market$getReturnEveryDay(),
            #x_volume = tidsSerie$getIndex(i)$getReturnEveryDay()*removeInf(Volumer$getIndex(i)$getReturnEveryDay()),
            #x_x5 = tidsSerie$getIndex(i)$getReturnEveryDay()*gold_index$getReturnEveryDay(),
            
            #x8_x9 = cointegrationTermAssetMarket[[1]]*market$getReturnEveryDay(),
            #x_x8_x9 = cointegrationTermAssetMarket[[1]]*market$getReturnEveryDay()*tidsSerie$getIndex(i)$getReturnEveryDay()
            #x8_x9_x5_volume = get_every_every_day(relationsTermMarknadBakat, interv)*market$getReturnEveryDay()*tidsSerie$getIndex(i)$getReturnEveryDay()*removeInf(Volumer$getIndex(i)$getReturnEveryDay())*
            #  gold_index$getReturnEveryDay(),
            #x = tidsSerie$getIndex(i)$getReturnEveryDay(),
            #policy = get_every(policy_index$get(), interv),
            #volume = removeInf(Volumer$getIndex(i)$getReturnEveryDay()),
            RSI = rsi1,
            RSI_500 = rsi_500_r,
            #market_volume = removeInf(sp500_volume_timeserie$getReturnEveryDay()),
            #x8 = market$getReturnEveryDay(),
            beta = tidsSerie$getIndex(i)$getBeta(sp500Index$get(), intv, interv),
            x9 = cointegrationTermAssetMarket[[1]]
          );
          
          framtida_cointegration_term = list()
          lista_med_data = list()
          lista_med_avkastningar = list()
          lista_med_avkastningar_framtida = list()
          beta_bransch = list()
          if(TRUE){
          for(t in seq(1:bransch_serier$length())){
            if(any(is.na(bransch_serier$getIndex(t)$get()) == TRUE) 
               || any(is.na(bransch_serier_framtida$getIndex(t)$get()) == TRUE)){
              next;
            }
            if(bransch_namn[[t]] == 'Policy'  ||
               bransch_namn[[t]] == 'MaterialPrisIndex'){
              #sd_1 = mean(bransch_serier$getIndex(t)$get()) + sd(bransch_serier$getIndex(t)$get())
              #sd_2 = mean(bransch_serier_framtida$getIndex(t)$get()) + sd(bransch_serier_framtida$getIndex(t)$get())
              lista_med_avkastningar[[ paste0(bransch_namn[[t]], '1')  ]] = get_every(log(bransch_serier$getIndex(t)$get()),interv)
              lista_med_avkastningar_framtida[[ paste0(bransch_namn[[t]], '1')  ]] = get_last_item(get_every(log(bransch_serier_framtida$getIndex(t)$get()), interv))
              next;
            }else if(bransch_namn[[t]] == 'FinancialStessIndex'){
              next
            }
            
          
            
            if(length(bransch_serier$getIndex(t)$get()) != length(tidsSerie$getIndex(i)$get()) ||
               length(framtidaTidsSerie$getIndex(i)$get()) != length(bransch_serier_framtida$getIndex(t)$get())
            ){
              if( (length(bransch_serier$getIndex(t)$get()) > length(tidsSerie$getIndex(i)$get())) ){
                shortest <- min(length(bransch_serier$getIndex(t)$get()), length(tidsSerie$getIndex(i)$get()))
                bransch_serier$getIndex(t)$addNew(bransch_serier$getIndex(t)$get()[1:shortest] )
                shortest <- min(length(bransch_serier_framtida$getIndex(t)$get()), length(framtidaTidsSerie$getIndex(i)$get()))
                bransch_serier_framtida$getIndex(t)$addNew(bransch_serier_framtida$getIndex(t)$get()[1:shortest])
                
                #x = na.locf(bransch_serier$getIndex(t)$get(), fromLast = TRUE)
                #x1 = na.locf(bransch_serier_framtida$getIndex(t)$get(), fromLast = TRUE)
                
                #bransch_serier$getIndex(t)$addNew(x)
                #bransch_serier_framtida$getIndex(t)$addNew(x1)
                
              }else{
                next
              }
            }
            ##
            if(TRUE){
            
              
            #lista_med_avkastningar[[ paste0(bransch_namn[[t]], '1')  ]] = bransch_serier$getIndex(t)$getReturn()
            #lista_med_avkastningar_framtida[[ paste0(bransch_namn[[t]], '1')  ]] = get_last_item( bransch_serier_framtida$getIndex(t)$getReturn() )
            
            lista_med_avkastningar[[ paste0(bransch_namn[[t]], '12')  ]] = tidsSerie$getIndex(i)$getBeta(bransch_serier$getIndex(t)$get(), intv, interv)
            lista_med_avkastningar_framtida[[ paste0(bransch_namn[[t]], '12')  ]] = get_last_item( framtidaTidsSerie$getIndex(i)$getBeta(bransch_serier_framtida$getIndex(t)$get(), intv, interv) )
            
            
            
            #x = RSI(bransch_serier$getIndex(t)$get(), n = 252)
            ##x = x[!is.na(x)]
            #x = na.locf(x, fromLast = TRUE)
            #x = get_every(x, interv)
            #lista_med_avkastningar[[ paste0(bransch_namn[[t]], '13')  ]] = x
            #lista_med_avkastningar_framtida[[ paste0(bransch_namn[[t]], '13') ]] = get_last_item(RSI(bransch_serier_framtida$getIndex(t)$get() , n = 252))
            
              #bransch_serier_framtida$getIndex(t)$getReturnEveryDay())
            ##
           # if(length(regressors[[companies[[i]]]]) > 0 && length(which(bransch_namn == regressors[[companies[[i]]]] )) > 0
            #   || length(regressors[[companies[[i]]]]) == 0){
           b1 = FALSE
            tryCatch({
              cointegrationTerm = Cointegration$new()$makeCointegrationTermsForEvery(
                tidsSerie$getIndex(i)$get(),
                bransch_serier$getIndex(t)$get(),
                framtidaTidsSerie$getIndex(i)$get(),
                bransch_serier_framtida$getIndex(t)$get(),
                interv
              )
              
              #cointegrationTerm[[1]] = get_every(cointegrationTerm[[1]], interv)
              #cointegrationTerm[[2]] = get_every(cointegrationTerm[[2]], interv)
  
            }, 
            error = function(e){
              b1 = TRUE
            })
            
            if(b1){
              next;
            }
            if( length(cointegrationTerm[[1]]) == length(framtidaTidsSerie$getIndex(i)$getReturnEveryDay()) ){
              lista_med_data[[ bransch_namn[[t]] ]] = cointegrationTerm[[1]]
              framtida_cointegration_term[[ bransch_namn[[t]] ]] = get_last_item(cointegrationTerm[[2]])
            }
              #
          }
            #}
            ##
          }
        }
          
          if(length(lista_med_data) != 0){
            explanatory_variables = cbind(explanatory_variables, as.data.frame(lista_med_data))
          }
          if(length(lista_med_avkastningar) != 0){
            lista_med_avkastningar = lista_med_avkastningar[lengths(lista_med_avkastningar) == length(framtidaTidsSerie$getIndex(i)$getReturn() )]
            if(length(lista_med_avkastningar) != 0){
              beta_bransch = as.data.frame(beta_bransch)
              explanatory_variables = cbind(explanatory_variables, as.data.frame(lista_med_avkastningar))
            }
          }
          
          
          data2_pred <- data.frame(
            x = get_last_item(framtidaTidsSerie$getIndex(i)$getReturnEveryDay()),
            #x4 = get_last_item(oil_index_framtida$getReturnEveryDay()),
            #x5 = get_last_item(gold_index_framtida$getReturnEveryDay()),
            x8 = get_last_item(market_framtida$getReturnEveryDay()),
            x9 = get_last_item(cointegrationTermAssetMarket[[2]]),
            
            
            x_x9 = get_last_item(framtidaTidsSerie$getIndex(i)$getReturnEveryDay())*get_last_item(cointegrationTermAssetMarket[[2]]),
            x_x8 = get_last_item(framtidaTidsSerie$getIndex(i)$getReturnEveryDay())*get_last_item(market_framtida$getReturnEveryDay()),
            x_x5 = get_last_item(framtidaTidsSerie$getIndex(i)$getReturnEveryDay())*get_last_item(gold_index_framtida$getReturnEveryDay()),
            x_volume = get_last_item(framtidaTidsSerie$getIndex(i)$getReturnEveryDay())*removeInf( get_last_item(framtidaVolumer$getIndex(i)$getReturnEveryDay()) ),
            
            x8_x9 = get_last_item(market_framtida$getReturnEveryDay())*get_last_item(cointegrationTermAssetMarket[[2]]),
            x_x8_x9 = get_last_item(market_framtida$getReturnEveryDay())*get_last_item(cointegrationTermAssetMarket[[2]])*get_last_item(framtidaTidsSerie$getIndex(i)$getReturnEveryDay()),
            x8_x9_x5_volume = get_last_item(market_framtida$getReturnEveryDay())*get_last_item(cointegrationTermAssetMarket[[2]])*get_last_item(gold_index_framtida$getReturnEveryDay())*removeInf( get_last_item(framtidaVolumer$getIndex(i)$getReturnEveryDay()) ),
            
            
            x100_x9 = get_last_item(framtidaTidsSerie$getIndex(i)$getReturnSmoothEveryDay())*get_last_item(cointegrationTermAssetMarket[[2]]),
            x100_x8 = get_last_item(framtidaTidsSerie$getIndex(i)$getReturnSmoothEveryDay())*get_last_item(market_framtida$getReturnEveryDay()),
            x100_x8_x9 = get_last_item(market_framtida$getReturnEveryDay())*get_last_item(cointegrationTermAssetMarket[[2]])*get_last_item(framtidaTidsSerie$getIndex(i)$getReturnSmoothEveryDay()),
            
            
            x100 = get_last_item(framtidaTidsSerie$getIndex(i)$getReturnSmoothEveryDay()),
            policy = get_last_item(policy_index_framtida$get()),
            volume = get_last_item(removeInf(framtidaVolumer$getIndex(i)$getReturnEveryDay())),
            market_volume = get_last_item(removeInf(sp500_volume_timeserie_framtida$getReturnEveryDay())),
            
            ##
            asset_gold = get_last_item(get_every(cointegrationTermAssetGold[[2]], interv) ),
            asset_energy = get_last_item(cointegrationTermAssetEnergy[[2]]),
            asset_oil = get_last_item(get_every(cointegrationTermAssetOil[[2]], interv) ),
            ##
           beta = get_last_item(framtidaTidsSerie$getIndex(i)$getBeta(sp500IndexFramtida$get(), intv, interv)),
            RSI = get_last_item(RSI(framtidaTidsSerie$getIndex(i)$get(), n = rsi_n)),
            RSI_500 = get_last_item(RSI(sp500IndexFramtida$get(), n = rsi_n))
          )
          
         
          "RSI = as.integer(as.logical(get_last_item(RSI(framtidaTidsSerie$getIndex(i)$get(), n = 140)) < focused))
              RSI_500 = as.integer(as.logical(get_last_item(RSI(sp500IndexFramtida$get(), n = 140)) < focused))"
          if(length(framtida_cointegration_term) != 0){
            data2_pred = cbind(data2_pred, as.data.frame(framtida_cointegration_term))
          }
          if(length(lista_med_avkastningar_framtida) != 0){
            data2_pred = cbind(data2_pred, as.data.frame(lista_med_avkastningar_framtida))
          }
  
  
          #length(get_every(cointegrationTermAssetEnergy[[1]], interv)) != length(framtidaTidsSerie$getIndex(i)$getReturnEveryDay()) 
          #length(cointegrationTermAssetEnergy[[1]]) != length(framtidaTidsSerie$getIndex(i)$getReturnEveryDay())  
          copy_explanatory_variables2 = explanatory_variables
          if( TRUE    ){
            returns = get_return(obj_static[[ companies[[i]] ]][['close']][(z-time_span/3):z],interv)
            
            mean_1 = abs(mean(returns))
            
            
            if(is.na(mean_1)){
              next
            }
          
            if(TRUE){
              print("Mål:")
              print(mal)
              mean_1 = 0.02
            }
            
            
            
            
            
            "if(mean_1 < 0.003){
              mean_1 = 0.003
            }"
            
            "if(mean_1 < 0.06){
              mean_1 = 0.06
            }
            
            mean_1 = 0.08"
            
            #mean_1 = 0.13
            #days_1 = framtidaTidsSerie$getIndex(i)$getAverageDaysEffect(mean_1)
            #days_1_log = log(framtidaTidsSerie$getIndex(i)$getAverageDaysEffect(mean_1)+1)
            #days_1_log_mean = mean(days_1_log)
            #ean_days = exp(days_1_log_mean)-1
            #sd_days = exp(sd(days_1_log))-1
            how_long_until_it_reaches = mean(framtidaTidsSerie$getIndex(i)$getAverageDaysEffect(mean_1))
            #standard_derivation_reaches = sd(framtidaTidsSerie$getIndex(i)$getAverageDaysEffect(mean_1))
        
            
            "expected_rise = framtidaTidsSerie$getIndex(i)$getExpectedEffectAbove(mean_1)
            expected_rise = quantile(expected_rise, 0.25)[[1]]"
            
            "expected_rise2 = framtidaTidsSerie$getIndex(i)$getExpectedEffect(mean_1, 2)
            expected_rise2 = mean(expected_rise2)"
            #expected_rise = mean_1
            
            
            "ev = cbind(explanatory_variables, data.frame(
              y = as.integer(as.logical( framtidaTidsSerie$getIndex(i)$getEffectBelow(-0.10) ))
            ))"
            
            as = framtidaTidsSerie$getIndex(i)$getEffect(mean_1)
            
           explanatory_variables = cbind(explanatory_variables, data.frame(
              y = as
              #y = framtidaTidsSerie$getIndex(i)$getSlope()
              #x9 = cointegrationTermAssetMarket[[1]],
              #asset_energy = cointegrationTermAssetEnergy[[1]]
            ))
            
           a = explanatory_variables[ , !(names(explanatory_variables) %in% c( 'y' ))]
            returns2 = get_return(obj_static[[ companies[[i]] ]][['close']][(z-time_span/8):z],1)
            explanatory_variables12 = cbind(a,data.frame(
              y = framtidaTidsSerie$getIndex(i)$getBeta(sp500IndexFramtida$get(), intv, interv)
            )
            )
            
            a = explanatory_variables[ , !(names(explanatory_variables) %in% c( 'y' ))]
            explanatory_variables13 = cbind(a,
                                            data.frame(
                                              y = framtidaTidsSerie$getIndex(i)$getReturn()
                                            ))
            
            a = explanatory_variables[ , !(names(explanatory_variables) %in% c( 'y' ))]
            rsi_500 = RSI(framtidaTidsSerie$getIndex(i)$get(), n = rsi_n)
            rsix = rsi_500[!is.na(rsi_500)]
            rsi_500 = na.locf(rsi_500, fromLast = TRUE)
            rsi_500_r = get_every(rsi_500, interv)
            explanatory_variables14 = cbind(a,
                                            data.frame(
                                              y = rsi_500_r
                                            ))
            
            "explanatory_variables12 = data.frame(
              y=framtidaTidsSerie$getIndex(i)$getReturn(),
              x9 = cointegrationTermAssetMarket[[1]]
            )"
            
            # how long until it reaches -8%
            #how_long_until_it_reaches_minus_8 = median(framtidaTidsSerie$getIndex(i)$getAverageDaysEffectBelow(-0.08))
            
           # mean_1 = 0.05
            
            
            
            
          }else{
            explanatory_variables = cbind(explanatory_variables, data.frame(
              y = framtidaTidsSerie$getIndex(i)$getReturnEveryDay(),
              x9 = cointegrationTermAssetMarket[[1]]
            ))
          }
          
          
          "if(all(explanatory_variables[['y']] == 0) || length(explanatory_variables) <= 1){
            next;
          }
          
          explanatory_variables[is.na(explanatory_variables)] <- 0
          is.na(explanatory_variables)<-sapply(explanatory_variables, is.infinite)
          explanatory_variables[is.na(explanatory_variables)]<-0
          
          explanatory_variables = explanatory_variables[,apply(explanatory_variables,2,function(explanatory_variables) !all(explanatory_variables==0))]
          
          
          copy_explanatory_variables = explanatory_variables"
          
          
          ##
          
         "b = FALSE
          pass = FALSE
          tryCatch({
            
            series = framtidaTidsSerie$getIndex(i)$get()
            #market = market_framtida$get()
            betas = c()
  
            h = length(series)
            for(r in seq_along(series)){
              if(h %% interv == 0 && h >= 252*5){
                
                series = framtidaTidsSerie$getIndex(i)$get()[(h-252*5):h ]
                market = sp500IndexFramtida$get()[(h-252*5):h ]
                vol = Volumer$getIndex(i)$get()[(h-252*5):h ]
                
                lm1 = Regression$new(data.frame(
                  y = get_return(series,interv),
                  x = get_return(market,interv)
                ), TRUE)
                s = lm1$getSummary()
                betas = c(betas, s$coefficients[,1][[2]])
  
              }
              h = h - interv
            }
            betas = rev(betas)
            
            a <- arima(betas, order=c(1,0,0))
            p <-predict(a, n.ahead = 1)
            b = TRUE
          }, 
          error = function(e){
            b = FALSE
          })
          if(!b){
            next;
          }
          d = data.frame(
            datum = obj_static[[ companies[[i]] ]][['date']][[z]],
            utveckling = get_last_item(obj_static[[ companies[[i]] ]][['close']][[z+interv]]/obj_static[[ companies[[i]] ]][['close']][[z]]),
            company = companies[[i]],
            forecast = p$pred[[1]]
          )
          if(length(report) == 0){
            report = d
          }else{
            report = rbind(report, d)
          }
          print(a$aic)
          if(p$pred[[1]] >= 1.1  && a$aic < -200) {
            print(p$pred[[1]])
            print(a$aic)
            print(a)
            
            pass = TRUE
            
          }else{
            next
          }"
          ##
          
          
          
          
          
          "reg = Regression$new(explanatory_variables12, TRUE)$chooseBestRSquared(TRUE)
          if(is.null(reg)){
            next
          }
          s = reg$getSummary()
          
          b = FALSE
          tryCatch({
            res = reg$predict(data2_pred, 1, 'confidence');
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
          if(res[1,1]-s$sigma*1/3 > 0.02 && s$r.squared > 0.1){
            print(s)
          }else{
            next
          }"
          
          
          
          pass = FALSE
          indikator = 0
          times = 0
          if( FALSE  ){
            
            "explanatory_variables = cbind(explanatory_variables, data.frame(
              beta = tidsSerie$getIndex(i)$getBeta(market$get())
            ))"
            
            ## best regression
            d = cor(explanatory_variables)
            d = as.data.frame(d)  %>% select(c('y'))
            names1_1 = row.names(d)
            #d <- d[!rownames(d) %in% c('beta'), ]
            d[is.na(d)] <- 0 
            d = abs(d)
            t_x = order(d, decreasing = TRUE)
            #t_x[[1]] = t_x[[2]]
            #names1_1 = names1_1[names1_1 != "y"]
            
            "lm1 = Regression$new(explanatory_variables %>% select(c('y', 'beta')), FALSE)
            lm2 = Regression$new(explanatory_variables %>% select(c('y', 'Policy1')), FALSE)
            s1 = lm1$getSummary()
            s2 = lm2$getSummary()
            
            if(s1$adj.r.squared > s2$adj.r.squared){
              e = explanatory_variables %>% select(c('y', 'beta'
              ) )
              e2 = copy_explanatory_variables %>% select(c('y'
              ) )
              aList = list()
              aList[[ 'beta' ]] = explanatory_variables[[ 'beta' ]]
              aList[[ 'y' ]] = explanatory_variables[[ 'y' ]]
            }else{
              e = explanatory_variables %>% select(c('y', 'Policy1'
              ) )
              aList = list()
              aList[[ 'Policy1' ]] = explanatory_variables[[ 'Policy1' ]]
              aList[[ 'y' ]] = explanatory_variables[[ 'y' ]]
            }"
            
            static_explanatory_variables = explanatory_variables
            copy_explanatory_variables = explanatory_variables
            for(x in seq(1:3)){
              if(!is.data.frame(copy_explanatory_variables)){
                next
              }
              
              if(is.null(copy_explanatory_variables[['y']])){
                copy_explanatory_variables[['y']] = static_explanatory_variables[['y']]
              }
  
            ## Choose best r squared
            reg = Regression$new(copy_explanatory_variables, TRUE)$chooseBestRSquared(TRUE)
            if(is.null(reg)){
              next
            }
            s = reg$getSummary()
            best_r_squared = names(s$coefficients[,1])
            
            if(is.null(best_r_squared)){
              best_r_squared = row.names(s$coefficients)
            }
            
            ##REMOVE
            #best_r_squared[[2]] = best_r_squared[[1]]
            
            explanatory_variables2 = static_explanatory_variables[ , !(names(static_explanatory_variables) %in% c( best_r_squared[[2]] ))]
            
            ## ---
            
            coef = c(best_r_squared[[2]], 'y')
              #c(best_r_squared[[1]], 'y')
            
            coef = unique(unlist(strsplit(coef, " ")))
            
            
            #e = static_explanatory_variables %>% select(c(coef, 'y'
            #) )
            #e2 = copy_explanatory_variables %>% select(c('y'
            #          ) )
            aList = list()
            for(g in seq_along(coef)){
              aList[[ coef[[g]] ]] = static_explanatory_variables[[ coef[[g]] ]]
            }
            
            important = as.data.frame(aList)
            explanatory_variables = important
            linearmod = reg
              #Regression$new(explanatory_variables, FALSE)
            
            "linearmod = Regression$new(explanatory_variables, FALSE)$chooseBestRSquared(FALSE);
            s = linearmod$getSummary();
            names1 = names(s$coefficients[,1])
            if(is.null(names1)){
              names1 = row.names(s$coefficients)
            }
            aList = list()
            aList[[ names1[[1]] ]] = explanatory_variables[[ names1[[1]]  ]]
            aList[[ 'y' ]] = explanatory_variables[[ 'y' ]]
            important = as.data.frame(aList)"
            "linearmod_with_out_regressor = Regression$new(e2, TRUE);
            s2 = linearmod_with_out_regressor$getSummary()
            if(s2$coefficients[,1][[1]] < 0.8 || s2$coefficients[,4][[1]] > 0.005){
              next
            }"
            #add1 = explanatory_variables %>% select(c( names1_1[[ t_x[[2]] ]] ,names1_1[[ t_x[[3]] ]], names1_1[[ t_x[[4]] ]] ))
            
            "if(is.null(linearmod) || length(linearmod$getSummary()$coefficients[,1]) <= 0 ){
              next;
            }"
            
            
            #linearmod = Regression$new(important, FALSE);
            
            s = linearmod$getSummary();
            names1 = names(s$coefficients[,1])
            
            if(is.null(names1)){
              names1 = row.names(s$coefficients)
            }
            
            ## Mutiple regression
            "names1_1 = names(add1)
            s = linearmod$getSummary();
            b1 = FALSE
            for(p in seq_along(explanatory_variables)){
              if(b1){
                break;
              }
              for(t in seq_along(add1)){
                if(!is.data.frame(add1)){
                  b1 = TRUE
                  break;
                }
                aList = list()
                aList[[ names1_1[[t]] ]] = add1[[t]]
                test1 = cbind(e, as.data.frame(aList))
                reg_new = Regression$new(test1, FALSE);
                s2 = reg_new$getSummary()
                len = length(s2$coefficients[,1])
                if(is.na(s2$adj.r.squared)){
                  b1 = TRUE
                  break;
                }
                if(s2$adj.r.squared > s$adj.r.squared && all(reg_new$getPValue() < 0.05)
                    ){
                  linearmod = reg_new
                  s = s2
                  e = test1
                  aList = list()
                  aList[[  names1_1[[t]]]] = explanatory_variables[[ names1_1[[t]] ]]
                  important = cbind(important, as.data.frame(aList))
                  add1 =  add1[ , !(names(add1) %in% c( names1_1[[t]] ))]
                  names1_1 = names(add1)
                  ##
                  #b1 = TRUE
                  ##
                  break;
                  
                }
              }
            }"
            
           
            "s = linearmod$getSummary();
            names1 = names(s$coefficients[,1])
            
            if(is.null(names1)){
              names1 = row.names(s$coefficients)
            }"
            
            ## check if stationary
            "remove = c()
            for(t in seq_along(names1)){
              if(names1[[t]] == '(Intercept)' ||names1[[t]] == 'y'){
                next;
              }
              if(adf.test(explanatory_variables[[ names1[[t]] ]])$p.value >= 0.05){
                remove = c(remove, names1[[t]])
              }
            }
            if(!is.null(remove)){
              explanatory_variables <- explanatory_variables[ , !(names(explanatory_variables) %in% c(remove))]
              if(!is.data.frame(explanatory_variables) || length(explanatory_variables) == 0){
                next;
              }
              if(length(explanatory_variables[['y']]) == 0){
                explanatory_variables[['y']] = framtidaTidsSerie$getIndex(i)$getReturnEveryDay()
              }
              linearmod = Regression$new(explanatory_variables, FALSE)
            }"
            "}else if(adf.test(explanatory_variables[[ names1[[1]] ]])$p.value >= 0.05){
           next;
            }"
            
            "if(any(sign(s$coefficients[,1]) == 1)){
              next
            }"
            
            
            
            
            
            ## remove low p values
            "objekt = removeHighPValues(linearmod, explanatory_variables, c(0.6,0.5,0.4, 0.3), TRUE)
            if(is.null(objekt)){
              break;
            }
            linearmod = objekt[[1]]
            if(is.null(linearmod)){
              break
            }
            explanatory_variables = objekt[[2]]
            
            s = linearmod$getSummary();
            names1 = names(s$coefficients[,1])
            
            if(is.null(names1)){
              names1 = row.names(s$coefficients)
            }
            
            if(is.null(explanatory_variables) || is.null(linearmod)){
              next
            }"
            
            
            "tmp <- cor(explanatory_variables)
            tmp[upper.tri(tmp)] <- 0
            diag(tmp) <- 0
            # Above two commands can be replaced with 
            # tmp[!lower.tri(tmp)] <- 0
            #
             
            explanatory_variables <- explanatory_variables[,!apply(tmp,2,function(x) any(x > 0.99))]"
            
            "y = explanatory_variables[['y']]
            explanatory_variables = removeHC(explanatory_variables, y, 0.75)
            explanatory_variables = removeHC(explanatory_variables, y, 0.55)
            
            if(is.null(explanatory_variables)){
              next
            }
            
            if(is.null(explanatory_variables[['y']])){
              explanatory_variables[['y']] = y
            }
            
            linearmod = Regression$new(explanatory_variables, TRUE)
            
            s = linearmod$getSummary()
            names1 = names(s$coefficients[,1])
            if(is.null(names1)){
              names1 = row.names(s$coefficients)
            }"
            
            ## remove low p values
            "objekt = removeHighPValues(linearmod, explanatory_variables, c(0.6,0.5,0.4, 0.3, 0.1, 0.05, 0.05, 0.05, 0.01), TRUE)
            if(is.null(objekt)){
              break;
            }
            linearmod = objekt[[1]]
            if(is.null(linearmod)){
              break
            }
            explanatory_variables = objekt[[2]]
            
            s = linearmod$getSummary();
            names1 = names(s$coefficients[,1])
            
            if(is.null(names1)){
              names1 = row.names(s$coefficients)
            }
            
            if(is.null(explanatory_variables) || is.null(linearmod)){
              next
            }"
            
            ##
            
            
            
            "explanatory_variables[[ paste0(names1[[1]], '2') ]] = explanatory_variables[[names1[[1]]]]^2
            
            linearmod1 = Regression$new(explanatory_variables, FALSE)
            s2 = linearmod1$getSummary()
            s = linearmod$getSummary()
            
            if(s2$adj.r.squared > s$adj.r.squared){
              linearmod = linearmod1
              s = s2
            }"
            
            ## CHECK VALIDATE
            "if(any(linearmod$getPValue() < 0.05) ){
              
            }else{
              next
            }"
            
            ## interception? and ^2
            "intercept1 = FALSE
            explanatory_variables[[ paste0(names1[[1]], '2') ]] = explanatory_variables[[names1[[1]]]]^2
            extra_coef = explanatory_variables[[names1[[1]]]]^2
            extra_coef_name = paste0(names1[[1]], '2')
            linearmod1 = Regression$new(explanatory_variables, FALSE)
            s2 = linearmod1$getSummary()
            s = linearmod$getSummary()
            
            
            if(is.na(s2$adj.r.squared)){
              
            }else if( s2$adj.r.squared > s$adj.r.squared && s2$coefficients[,4][[2]] < alpha ){
              intercept1 = TRUE
              linearmod = linearmod1
              s = s2
            }"
            
            "names1 = names(s$coefficients[,1])
            if(is.null(names1)){
              names1 = row.names(s$coefficients)
            }"
            
            b = c()
            cn = 1
            "for(g in seq(1:3)){
              if(length(explanatory_variables2) <= 0 || !is.data.frame(explanatory_variables2)){
                next
              }
              reg = Regression$new(explanatory_variables2, FALSE)$chooseBestRSquared(FALSE)
              if(is.null(reg)){
                next
              }
              s = reg$getSummary();
              names1 = names(s$coefficients[,1])
              
              if(is.null(names1)){
                names1 = row.names(s$coefficients)
              }
              
              if( s$r.squared < 0.3){
                explanatory_variables2 =  explanatory_variables2[ , !(names(explanatory_variables2) %in% c( names1[[cn]] ))]
                next
              }
              b1 = FALSE
              for(t in seq_along(coef)){
                if(abs(cor(explanatory_variables2[[ names1[[cn]] ]],
                           static_explanatory_variables[[ coef[[ t ]] ]]
                )[[1]]) > 0.18 ){
                  b1 = TRUE
                  break
                }
              }
              
              if(b1){
                explanatory_variables2 =  explanatory_variables2[ , !(names(explanatory_variables2) %in% c( names1[[cn]] ))]
                next
              }
              
              if(abs(cor(explanatory_variables2[[ names1[[cn]] ]],
                         static_explanatory_variables[[ best_r_squared[[1]] ]]
                     )[[1]]) < 0.18 && s$r.squared > 0.3){
              
                b = c(b, names1[[cn]])
              explanatory_variables2 =  explanatory_variables2[ , !(names(explanatory_variables2) %in% c( names1[[cn]] ))]
              
                break;
              }
            }"
          
            
            coef = c(coef, b)
            ##linearmod = Regression$new(static_explanatory_variables %>% select(c(coef)), FALSE);
            "a=static_explanatory_variables %>% select(c(coef))
            if(intercept1 == TRUE){
            aList = list(
            )
            aList[[ extra_coef_name ]] = extra_coef
            a = cbind(a, as.data.frame(aList))
            }
            linearmod = Regression$new(a, FALSE)
            s = linearmod$getSummary()
            
            ## ^2
            ## CHECK VALIDATE
            if(any(linearmod$getPValue() < alpha) ){
              
            }else{
              next
            }"
            
            ## right sign?
            "break1 = FALSE
         for(d in seq(1:2)){
          remove = checkIfWrongSignPositive(s, names1)
           if(remove[['wrong']]){
             remove_this = remove[['remove']]
             explanatory_variables <- explanatory_variables[ , !(names(explanatory_variables) %in% c(remove_this))]
             
             if(!is.data.frame(explanatory_variables)){
               break1 = TRUE
               break;
             }
             
             linearmod <- linearmod$reg(explanatory_variables, TRUE);
             s <- linearmod$getSummary();
             names1 <- names(s$coefficients[,1])
             if(is.null(names1)){
               names1 = row.names(s$coefficients)
             }
             
           }
  
         }
         if(break1){
           next;
         }"
            
            
            ## check if residuals is approximatly zero
            "residual = mean(residuals(linearmod$getSummary()))
         if(residual > 0.00001 || residual < -0.00001){
           next;
         }"
            
            ## check for auto correlation
            "autoCorrelationTest = dwtest(linearmod$get())
         if(autoCorrelationTest$p.value < 0.05){
           next;
         }"
            
            ## check variance
            "fail = FALSE
         for(t in seq_along(explanatory_variables)){
           if(var(explanatory_variables[[t]]) <= 0){
             fail = TRUE
             break;
           }
         }
          if(fail){
            next;
          }"
            
            ## check noramlity in residual
            "pvalue = shapiro.test(residuals(linearmod$get()))$p.value
         if(pvalue < 0.05){
           next;
         }"
            
            ## check everything
            "g = gvlma::gvlma(linearmod$get())
         fail = FALSE
         if(g$GlobalTest$GlobalStat4$pvalue < 0.05 ||
            g$GlobalTest$DirectionalStat1$pvalue < 0.05 ||
            g$GlobalTest$DirectionalStat2$pvalue < 0.05 ||
            g$GlobalTest$DirectionalStat3$pvalue < 0.05 ||
            g$GlobalTest$DirectionalStat4$pvalue < 0.05
         ){
           fail = TRUE
         }
         if(fail){
           next
         }"
            
            
            
            
          
          
          
          "data2_pred = cbind(data2_pred, data.frame(
              cointe = get_last_item(get_every(cointe[[2]], interv))
            ))"
          #data2_pred = cbind(data2_pred, add_data_pred)
            
            ## CHECK IF risk is below -10%
           " e = data.frame(
              RSI = static_explanatory_variables[['RSI']],
              y = ev[['y']]
            )
            linearmod1 = Regression$new(e, intercept1)
            b = FALSE
            tryCatch({
              res = linearmod1$predict(data2_pred, 1, 'confidence');
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
            
            if(res[1,1] > 0.20){
              next
            }"
            
            "lowest_value = mean(static_explanatory_variables[static_explanatory_variables[, 'y'] == FALSE,][[ names1[[1]] ]])
            
            a = static_explanatory_variables %>% select(c('y', names1[[1]]))
            
            a = static_explanatory_variables[static_explanatory_variables[, names1[[1]]] >= lowest_value,]
            
            linearmod = Regression$new(static_explanatory_variables, FALSE)"
            
            "objekt = removeHighPValues(linearmod, explanatory_variables, c(0.4,0.2,0.1,0.05,alpha), FALSE)
            linearmod = objekt[[1]]
            explanatory_variables = objekt[[2]]
            
            if(is.null(explanatory_variables) || is.null(linearmod)){
              next
            }"
            
            s = linearmod$getSummary();
            names1 = names(s$coefficients[,1])
            
            if(is.null(names1)){
              names1 = row.names(s$coefficients)
            }
            
   
            
            
            
            explanatory_variables = static_explanatory_variables %>% select(c( coef ))
            "linearmod = Regression$new(static_explanatory_variables %>% select(c( coef )), FALSE)
            s= linearmod$getSummary()
            
            s = linearmod$getSummary()
            names1 = names(s$coefficients[,1])
            if(is.null(names1)){
              names1 = row.names(s$coefficients)
            }"
            
           "intercept1 = FALSE
            explanatory_variables[[ paste0(names1[[2]], '2') ]] = explanatory_variables[[names1[[2]]]]^2
            #explanatory_variables[[ paste0(names1[[1]], '3') ]] = explanatory_variables[[names1[[1]]]]^3
            linearmod1 = Regression$new(explanatory_variables, TRUE)
            s2 = linearmod1$getSummary()
            s = linearmod$getSummary()
            
            
            if(is.na(s2$adj.r.squared)){
              next
            }else if( TRUE ){
    
              linearmod = linearmod1
              s = s2
              aList = list()
              aList[[ paste0(names1[[2]], '2') ]] = data2_pred[[ paste0(names1[[2]]) ]]^2
              #aList[[ paste0(names1[[1]], '3') ]] = data2_pred[[ paste0(names1[[1]]) ]]^3
              data2_pred = cbind(data2_pred, as.data.frame(aList))
            }else{
              #next
            }"
            
            "objekt = removeHighPValues(linearmod, static_explanatory_variables, c(0.05, alpha), FALSE)
            linearmod = objekt[[1]]
            explanatory_variables = objekt[[2]]
            
            if(is.null(explanatory_variables) || is.null(linearmod)){
              next
            }
            
            intercept1 = FALSE
            linearmod1 = Regression$new(explanatory_variables %>% select(c('y','RSI')), FALSE)
            s2 = linearmod1$getSummary()
            s = linearmod$getSummary()
            
            if(is.na(s2$adj.r.squared)){
              
            }else if( s2$adj.r.squared > s$adj.r.squared && s2$coefficients[,4][[1]] < alpha ){
              intercept1 = TRUE
              linearmod = linearmod1
              s = s2
            }"
            
          
            ## FORECAST
          b = FALSE
          tryCatch({
            res = linearmod$predict(data2_pred, 1, 'confidence');
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
          
            "if(s$r.squared < 0.40 || res[1,1] < 0.11 || s$sigma > 0.15){
              copy_explanatory_variables =  copy_explanatory_variables[ , !(names(copy_explanatory_variables) %in% c( best_r_squared[[1]] ))]
              next
            }
            
            if(res[1,1]-s$sigma*1/4 >= 0.06){
              pass = TRUE
              break;
            }else{
              copy_explanatory_variables =  copy_explanatory_variables[ , !(names(copy_explanatory_variables) %in% c( best_r_squared[[1]] ))]
              next
            }"
            
            
  
            
            
            ## SAVE REGRESSIONS
            if(length(regressioner[[ a_co[[i]] ]]) == 0){
              regressioner[[ a_co[[i]] ]] = list()
            }
            if(length(regressioner[[ a_co[[i]] ]]) < 5){
              regressioner[[ a_co[[i]] ]][[ length(regressioner[[ a_co[[i]] ]])+1 ]] = linearmod
            }
  
            
          if(FALSE){
            copy_explanatory_variables =  copy_explanatory_variables[ , !(names(copy_explanatory_variables) %in% c( best_r_squared[[2]] ))]
            next
          }
            if(x == 1){
              print(companies[[i]])
            print(res[1,1])
            }
            
            
            
          if(  res[1,1] >= 0.95 && s$r.squared > 0.2 && all(s$coefficients[,4] < 0.05)) {
            
           print(s)
            ## probability that is will rise
            
            
            pass= TRUE
            break;
            indikator = indikator + 1
            copy_explanatory_variables =  copy_explanatory_variables[ , !(names(copy_explanatory_variables) %in% c( best_r_squared[[2]] ))]
            next
            
            
          }else{
            copy_explanatory_variables =  copy_explanatory_variables[ , !(names(copy_explanatory_variables) %in% c( best_r_squared[[2]] ))]
            next
          }
          
            }
          
          }else if( FALSE ){
            
           
            
          }
          
          
          "linearmod = Regression$new(explanatory_variables, TRUE)
          s = linearmod$getSummary()
          pass = FALSE
          print(s$coefficients[,1][[1]])
          if(s$coefficients[,1][[1]] >= 0.95){
            pass = TRUE
          }"
          
          #aList[[ names1[[1]] ]] =explanatory_variables[[ names1[[1]] ]]
          
          #explanatory_variables12 = cbind(explanatory_variables12, 
           #                               as.data.frame(aList))
          
         
          
         " linearmod = Regression$new(explanatory_variables13 %>% select(c('y')), TRUE)
          s = linearmod$getSummary()
          pass = FALSE"
          
          "b = FALSE
          tryCatch({
            res = linearmod$predict(data2_pred, 1, 'confidence');
          }, 
          error = function(e){
            b = TRUE
          })
          if(b){
            next;
          }
          
          if(is.na(res[1,1]) ){
            next;
          }"
          
          "if(s$coefficients[,1][[1]] < 0.1){
            pass = TRUE
          }"
          
          "if(pass == FALSE){
            next
          }
          "
         
          
          "linearmod = Regression$new(data.frame(
            y = explanatory_variables[['y']]
          ), TRUE);
          s = linearmod$getSummary()
          
          print(s$coefficients[,1][[1]])
          
          if(s$coefficients[,1][[1]] < 0.88){
            next
          }"
          
          
          
          
          "if(pass == FALSE){
            next
          }"
          
         
          
          
          static_explanatory_variables = explanatory_variables12
          if(FALSE){
            indikator = 0;
            times = 0;
            count = 0;
            pass = FALSE
            variable_name = NULL
            the_regression = NULL
          for(l in seq(1:5)){
             
            if(!is.data.frame(static_explanatory_variables) || length(static_explanatory_variables) == 1){
              next
            }
          
          
          ##
          linearmod = Regression$new(static_explanatory_variables, TRUE)$chooseBestRSquared(TRUE)
          s = linearmod$getSummary();
          
          names1 = names(s$coefficients[,1])
          
          if(is.null(names1)){
            names1 = row.names(s$coefficients)
          }
          
          "aList = list()
          variable_1 = c(names1[[2]], variable_name, 'y')
          for(t in seq_along(variable_1)){
            aList[[variable_1[[t]]]] = static_explanatory_variables[[ variable_1[[t]] ]]
          }
          explanatory_variables12 = as.data.frame(aList)
          linearmod = Regression$new(as.data.frame(aList), TRUE)
          s = linearmod$getSummary();
          
          names1 = names(s$coefficients[,1])
          
          if(is.null(names1)){
            names1 = row.names(s$coefficients)
          }"
          
          ##
          explanatory_variables12 = static_explanatory_variables
          ##
          "objekt = removeHighPValues(linearmod, explanatory_variables12, c(0.4, 0.3,0.2, 0.1, 0.05, 0.05, 0.05), TRUE)
          if(is.null(objekt)){
            break
          }
          linearmod = objekt[[1]]
          if(is.null(linearmod)){
            break
          }
          explanatory_variables12 = objekt[[2]]
          
          s = linearmod$getSummary();
          
          names1 = names(s$coefficients[,1])
          
          if(is.null(names1)){
            names1 = row.names(s$coefficients)
          }"
          ##
          ##
          
          #y = explanatory_variables12[['y']]
          #explanatory_variables12 = removeHC(explanatory_variables12, y, 0.4)
          
          "if(is.null(explanatory_variables12) || !is.data.frame(explanatory_variables12)){
            aList = list()
            variable_1 = c(variable_name, 'y')
            for(t in seq_along(variable_1)){
              aList[[variable_1[[t]]]] = static_explanatory_variables[[ variable_1[[t]] ]]
            }
            explanatory_variables12 = as.data.frame(aList)
          }
          
          if(is.null(explanatory_variables12[['y']])){
            explanatory_variables12[['y']] = y
          }"
          "if(is.null(explanatory_variables12[['y']])){
            explanatory_variables12[['y']] = y
          }
          linearmod = Regression$new(explanatory_variables12, TRUE)
          s = linearmod$getSummary();"
          ##
          
          "linearmod1 = Regression$new(explanatory_variables12, TRUE)$chooseBestRSquared(TRUE);
          s1 = linearmod1$getSummary();
          
          names1 = names(s1$coefficients[,1])
          
          if(is.null(names1)){
            names1 = row.names(s1$coefficients)
          }"
          #explanatory_variables12 = static_explanatory_variables[ , !(names(static_explanatory_variables) %in% c( names1[[2]] ))]
          variable_name = names1[[2]]
          
          ##
          "bol = FALSE
          aList = list();
          aList[[names1[[2]] ]] = explanatory_variables12[[ names1[[2]] ]]
          aList[['y']] = explanatory_variables12[['y']]
          linearmod = Regression$new(as.data.frame(aList), TRUE)"
          
          
          #names1[[2]] = names1[[1]] 
          
            found = FALSE
            for(t in seq_along(s$coefficients[,4])){
              if(s$coefficients[,4][[t]] > 0.05){
                found = TRUE
                break;
              }
            }
            if(found){
              
              static_explanatory_variables = static_explanatory_variables[ , !(names(static_explanatory_variables) %in% c( variable_name ))]
              next;
            }else{
            }
          
          if(s$r.squared < 0.3){
            break;
          }
          
          
          
          b = FALSE
          tryCatch({
            res = linearmod$predict(data2_pred, 1, 'confidence');
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
          
          if(s$r.squared > 0.2){
          times = times + 1
          
          if(res[1,1] > 1){
            count = count + 1
          }
          }
          
          print("Beta forecast")
          print(res[1,1])
          if( 
            res[1,1] > 0.3
            &&
            res[1,1] < 0.8 && s$r.squared >= 0.3){
            indikator = indikator + 1
            variable_name = names1[[2]]
            the_regression = linearmod
            static_explanatory_variables = static_explanatory_variables[ , !(names(static_explanatory_variables) %in% c( variable_name ))]
            break;
          }else{
            static_explanatory_variables = static_explanatory_variables[ , !(names(static_explanatory_variables) %in% c( variable_name ))]
            next;
          }
          
          }
          
          if(indikator >= 1){
            pass = TRUE
          }
            
          }
          
          "if(pass == FALSE){
            next
          }"
          
          ## ARIMA
          print("ARIMA")
          
          if(TRUE){
            indikator = 0;
            times = 0;
            count = 0;
            pass = FALSE
            variable_name = NULL
            the_regression = NULL
            explanatory_variables13 = explanatory_variables
            static_explanatory_variables = explanatory_variables13
            for(l in seq(1:5)){
              
              if(!is.data.frame(static_explanatory_variables) || length(static_explanatory_variables) == 1){
                next
              }
              
              linearmod = Regression$new(static_explanatory_variables, TRUE)$chooseBestRSquared(TRUE);
              s = linearmod$getSummary();
              
              names1 = names(s$coefficients[,1])
              
              if(is.null(names1)){
                names1 = row.names(s$coefficients)
              }
              variable_name = names1[[2]]
              
              aList = list(
              
              )
              
              "aList[[ variable_name ]]  = static_explanatory_variables[[ variable_name ]]
              aList[[ paste0(variable_name, '2') ]]  = static_explanatory_variables[[ variable_name ]]^2 
              
              aList2 = list()
              aList2[[ paste0(variable_name, '2') ]] = data2_pred[[ variable_name ]]^2
              data2_pred = cbind(data2_pred,
                                 as.data.frame(aList2)
                                 )
              
              linearmod = Regression$new(cbind(
                data.frame(
                  y = static_explanatory_variables[['y']]
                ),
                as.data.frame(aList)
              ), TRUE);
              s2 = linearmod$getSummary();
              
              if(s2$adj.r.squared > s$adj.r.squared){
                s = s2
              }"
              
              ##
              
              "names1 = names(explanatory_variables13)
              for(t in seq_along(names1)){
                if(names1[[t]] == variable_name || names1[[t]] == 'y'){
                  next
                }
                
                aList = list()
                aList[[names1[[t]]]] = explanatory_variables13[[ names1[[t]] ]]
                
                names2 = names(s$coefficients[,1])
                for(p in seq_along(names2)){
                  if(names2[[p]] == '(Intercept)'){
                    next
                  }
                  
                  aList[[ names2[[p]] ]] = explanatory_variables13[[ names2[[p]] ]]
                  
                }
                
                d = cbind(data.frame(
                  y = explanatory_variables13[['y']]
                ), as.data.frame(aList))
                
                linearmod = Regression$new(d, TRUE)
                
                s2 = linearmod$getSummary();
                
                
                if(s2$adj.r.squared > s$adj.r.squared
                   && s2$coefficients[,4][[ length(aList)+1 ]] < 0.05){
                  s = s2
                }
                
              }"

              
              
              #names1[[2]] = names1[[1]] 
              
              found = FALSE
              for(t in seq_along(s$coefficients[,4])){
                if(s$coefficients[,4][[t]] > 0.05){
                  found = TRUE
                  break;
                }
              }
              if(found){
                
                static_explanatory_variables = static_explanatory_variables[ , !(names(static_explanatory_variables) %in% c( variable_name ))]
                next;
              }else{
              }
              
              
              
              b = FALSE
              tryCatch({
                res = linearmod$predict(data2_pred, 1, 'confidence');
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
              
              if(s$r.squared > 0.2){
                times = times + 1
                
                if(res[1,1] > 0){
                  count = count + 1
                }
              }
              
              print(res[1,1])
              if( res[1,1] > 0.95 && s$r.squared > 0.2){
                indikator = indikator + 1
                variable_name = names1[[2]]
                the_regression = linearmod
                static_explanatory_variables = static_explanatory_variables[ , !(names(static_explanatory_variables) %in% c( variable_name ))]
                break;
              }else{
                static_explanatory_variables = static_explanatory_variables[ , !(names(static_explanatory_variables) %in% c( variable_name ))]
                next;
              }
              
            }
            
            if(indikator >= 1){
              pass = TRUE
            }
            
          }
          
          if(pass == FALSE){
            next
          }
          
          "print(indikator)
          if(indikator < 1){
            next
          }
          "
          
          "linearmod1 = Regression$new(data.frame(
            y = framtidaTidsSerie$getIndex(i)$getEffect(0.08)
          ), TRUE)
          s1 = linearmod1$getSummary()
          
          
          if(
            s1$coefficients[,1][[1]] < 0.6
          ){
            next
          }"
          
          if(pass == FALSE){
            next
          }
          
          }
          
          i = np
          
          
          if(
            TRUE
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
                #best_forecast = c(best_forecast,  mean(linearmod$getPValue())  )
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
                  
     
                  if( mean( (a1-buy)/buy)  > 0.02 && prevent3 == TRUE){
                    #buy = c(buy, obj_static[[ a_co[[i]] ]][['close']][[(z+r)]])
                    #prevent3 = FALSE
                    
                    if(stop){
                      utveckling <- c(utveckling,  mean(a1/buy) )
                    }else{
                      utveckling = c(utveckling, ((mean(a1/buy)-1)*0.8+1) )
                    }
                    sell_at = c(sell_at,  mean(a[[x]]/buy))
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

                  
                  
                  if( mean( (a1-buy)/buy)  > 0.08 && prevent3 == TRUE){
                    #buy = c(buy, obj_static[[ a_co[[i]] ]][['close']][[(z+r)]])
                    #prevent3 = FALSE
                    
                    if(stop){
                      utveckling_c <- c(utveckling_c,  mean(a1/buy) )
                    }else{
                      utveckling_c = c(utveckling_c, ((mean(a1/buy)-1)*0.8+1) )
                    }
                    sell_at = c(sell_at,  mean(a[[x]]/buy))
                    time_obs2 = c(time_obs2, x)
                    break;
                  }
                  
                  
                  
                  if(y >= length(a) || r == 126){
                    print(mean(a1/buy))
                    time_obs2 = c(time_obs2, x)
                    #sell_at = c(sell_at,  mean(a[[x]]/buy))
                    if(stop){
                      utveckling_c <- c(utveckling_c, mean(a1/buy) )
                    }else{
                      utveckling_c <- c(utveckling_c, ((mean(a1/buy)-1)*0.8+1) )
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
                graf_over_kop_salj[[  ]]
                b = obj_static[[ a_co[[i]] ]][['close']][(z):(z)]
                last = a/b
                #utveckling <- c(utveckling, 
                #                mean(a[c(TRUE, FALSE)]/b[c(TRUE, FALSE)] ) )
                utveckling_a = c(utveckling_a, get_last_item(utveckling))
                utveckling_b = c(utveckling_b, last)
                
                "if(length(graf_over_kop_salj) == 0){
                  graf_over_kop_salj[[1]] = obj_static[[ a_co[[i]] ]][['close']][(z-30):(z+ get_last_item(time_obs) )]
                  kop_salj[[1]] = c(rep(c(0), times = 30), obj_static[[ a_co[[i]] ]][['close']][(z):(z+get_last_item(time_obs))])
                  reg1[[1]] = data.frame(
                    y = static_explanatory_variables[['y']],
                    x = static_explanatory_variables[['beta']]
                  )
                }else{
                  graf_over_kop_salj[[length(graf_over_kop_salj)+1]] = obj_static[[ a_co[[i]] ]][['close']][(z-30):(z+ get_last_item(time_obs) )]
                  kop_salj[[length(kop_salj)+1]] = c(rep(c(0), times = 30),obj_static[[ a_co[[i]] ]][['close']][(z):(z+get_last_item(time_obs))])
                  reg1[[length(reg1)+1]] = data.frame(
                    y = static_explanatory_variables[['y']],
                    x = static_explanatory_variables[['beta']]
                  )
                }"
                
                if(new_found){
                  per_intervall_utveckling[[ length(per_intervall_utveckling)+1 ]] = c(
                                                                                       get_last_item(utveckling) )
                  
                }else{
                per_intervall_utveckling[[ length(per_intervall_utveckling) ]] = c(per_intervall_utveckling[[ length(per_intervall_utveckling) ]],
                                                                                   get_last_item(utveckling) )
                }
                new_found= FALSE
                #get_last_item(utveckling)*0.2+
                "if(get_last_item(utveckling) < 0.9){
                  utveckling[[length(utveckling)]] = get_last_item(utveckling)
                }else{
                  utveckling[[length(utveckling)]] = last
                }"
                "if(prevent3 == FALSE){
                  avkastning = obj_static[[ a_co[[i]] ]][['close']][[(z+interv)]]/buy[[1]]*0.7 + 
                    obj_static[[ a_co[[i]] ]][['close']][[(z+interv)]]/buy[[2]]*0.3
                  
                utveckling[[length(utveckling)]] = avkastning
                
                print(get_last_item(utveckling))
                }else{
                  avkastning = (obj_static[[ a_co[[i]] ]][['close']][[(z+interv)]]/buy[[1]] - 1)*0.7 + 1
                  utveckling[[length(utveckling)]] = avkastning
                  print(get_last_item(utveckling))
                }"
                utveckling[[length(utveckling)]] = get_last_item(utveckling)
                print("utveckling")
                print(get_last_item(utveckling))
                print("utveckling")
                print(last)
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
        if(length(new_companies) > 0){
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
            
            ut = c(ut, sell/buy*dd_w[[ gsub(paste0('-'),'.' ,new_companies[[i]]) ]] )
            
            "buys = c(buy)
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
            }"
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
    
  
  
  "print(utveckling)
  print(sd(utveckling[utveckling > 0]))
  print(mean(utveckling[utveckling > 0.7]))
  hist(utveckling)
  
  a_list <- list(
    '1' = utveckling[1:12],
    '2' = utveckling[13:24],
    '3' = utveckling[25:36]
  )
  mean_per_year <- c()
  for( i in seq(1:3)){
    money_dev <- 100000
    money_dev1 <- 0
    ev <- a_list[[i]]
    prev <- FALSE
    for(i in seq_along(ev)){
      money_dev <- money_dev*ev[[i]]
    }
    print(money_dev-100000)
    mean_per_year <- c(mean_per_year, (money_dev - 100000))
  }
  avg <- c()
  ev <- utveckling
  for(i in seq_along(ev)){
    money_dev <- 100000
    if(i %% 12 == 0 || i == 1){
      ev1 <- ev[(i):(i+12-1)]
      prev <- FALSE
      for(p in seq_along(ev1)){
        money_dev <- money_dev*ev1[[p]]
      }
      print(money_dev - 100000)
      if(!is.na(money_dev)){
        avg <- c(avg, money_dev - 100000)
      }
    }
  }
  print(mean(avg))"
  money_dev <- 100000
  ev <- utveckling
  m1 <- money_dev*0.8
  m2 <- money_dev*0.2
  for(t in seq(1:4)){
    ev <- sample(utveckling, 24)
    ev2 <- sample(utveckling, 6)
    m1 <- m1*mean(ev)
    a = m2
    m2 <- m2*mean(ev2)
    if(t > 1){
      a <- m1 + m2
      m1 = a*0.8
      m2 = a*0.2
    }
  }
  print(m1+m2)
  
  
  print(mean(utveckling)-1)
  print(sd(utveckling))
  print(min(utveckling)-1)
  print(min(c12)-1)
  print(quantile(c12,0.02)-1)
  longtermUtveckling <- c(longtermUtveckling, utveckling)
  longtermc12 <- c(longtermc12, c12)
  length(utveckling)
  
  money_dev <-  0
  df = data.frame(
    utveckling = utveckling,
    time = time_obs,
    m = 1000/10
  )
  sample_1 <- df[sample(nrow(df), 10), ]
  
  for(i in seq(1:(nrow(sample_1)))){
    
      prev = (interv)
      for(t in seq(1:100)){
      money_dev = money_dev + (sample_1[i,][['m']]*sample_1[i,][['utveckling']] - sample_1[i,][['m']])
      sample_1[i,][['utveckling']] = sample(utveckling,1)
      prev = (prev-sample_1[i,][['time']]) 
      sample_1[i,][['time']] = sample(time_obs,1)
      hw = floor( (prev) /sample_1[i,][['time']])
      
      if(hw <= 0){
        break
      }
      
      }
      
      money_dev = money_dev + sample_1[i,][['m']]
    
  }
  print(money_dev)
  
  
  for(t in seq(1:4)){
    a <- runif(10, min=1, max=length(utveckling))
    a <- ceiling(a)
    avkastning <- 0
    ev <- utveckling[c(a)]
    money_dev = money_dev*mean(ev)
  }
  print(money_dev)
  
  hist(c12)
  print(mean(c12))
  print(sd(forecast_error))
  print(mean(forecast_error))
  shapiro.test(c12)
  mean(utveckling)
  mean(testUtveckling)
  mean(time_obs)
  hist(utveckling)
  print(length(utveckling[utveckling < 1])/length(utveckling))
  print(length(utveckling))
  
  
  money = 200000
  currentInvest = 50000
  buy = FALSE;
  i = 0;
  loop_i = 1
  for(t in seq(1:252)){
  
    i = i + 1
    buy = FALSE;
    
    if(loop_i >= 252){
      break;
    }
    
    print("Current investing")
    print(currentInvest)
      
      if((currentInvest*utveckling[[i]] - currentInvest) < 0){
        x = (10000-4000)/0.08
        print((currentInvest*utveckling[[i]] - currentInvest))
        print(utveckling[[i]])
        print(x)
        currentInvest = currentInvest + x
        print("Updated invest")
        print(currentInvest)
        next;
      }else{
       
        money = (currentInvest*utveckling[[i]] - currentInvest) + money
        
        loop_i = loop_i + time_obs[[i]]
        
        currentInvest = 50000
         
      }
    
  }
  
  money/200000
  
  
  sample_1 = sample(utveckling, 83)
  money = 1000
  money1 = 1000
  for(t in seq_along(sample_1)){
    money1 = money1*sample_1[[t]]
    print(money1)
  }
  print(money1)
  m = money/40
  money = 0
  for(t in seq_along(sample_1)){
    print(m*sample_1[[t]])
    money = money + m*sample_1[[t]]
  }
  print(money)
  
  genomsnitt_per_gang = c()
  chansen_for_under_ett = c()
  ett_kop = c()
  m = 500
  m2 = 500
  money = 1000
  for(k in seq(1:1)){
  period = sample(seq(1:length(per_intervall_utveckling)), 4)
  for(t in seq(1:length(period))){
    money = money*sample(per_intervall_utveckling[[period[[t]]]],1)
    print(money)
  }
  }
  print(sum(money))
  for(t in seq_along(per_intervall_utveckling)){
    genomsnitt_per_gang = c(genomsnitt_per_gang, mean(per_intervall_utveckling[[t]]))
    chansen_for_under_ett = c(chansen_for_under_ett, length(per_intervall_utveckling[[t]][ per_intervall_utveckling[[t]] < 1 ])/length(per_intervall_utveckling[[t]]))
  }
  mean(genomsnitt_per_gang)
  mean(chansen_for_under_ett)
  
  nr = 1
  plot(seq_this(graf_over_kop_salj[[nr]]), graf_over_kop_salj[[nr]], type="l")
  lines(seq_this(kop_salj[[nr]]), kop_salj[[nr]], col="red", type="l")
  
  nr = 8
  plot(reg1[[nr]][['x']], reg1[[nr]][['y']])
  
  length(utveckling[utveckling < 1])/length(utveckling)
  
  
  plot(explanatory_variables13[['x9']], explanatory_variables13[['y']])
  
  linearmod = Regression$new(data.frame(y= a[['y']], 
                                        olja12=a[['Olja12']]), TRUE)
s = linearmod$getSummary()  
s

a = sample(utveckling, 55)
length(a[a < 1])/length(a)
