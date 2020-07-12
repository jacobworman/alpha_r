library('tseries')
library('PerformanceAnalytics')

#z <- length(obj_static[[1]][['close']])-252
z = (252*10+interv+1+120)
time_period <- (250*2)
obj <- obj_static

list_of_companies_with_cointegrated_relationship <- data.frame(list("1", "2"))
colnames(list_of_companies_with_cointegrated_relationship) <- c('company1','company2')

list_of_companies_with_cointegrated_relationship <- list_of_companies_with_cointegrated_relationship[-c(1),]

## CUT OBSERVATIONS
for(h in seq_along(obj)){
  obj[[h]] <- obj[[h]][(z-(time_period-1)):(z),];
}



find_cointegrated_relationship <- function(z12 = NULL, co = NULL){
  list_of_companies_with_cointegrated_relationship <- list_of_companies_with_cointegrated_relationship[0,]
  
  if(is.null(z12) == FALSE){
    z <- z12
  }
  
  if(is.null(co) == FALSE){
    companies <- co
  }
  
  print("Z:")
  print(z)
  
  obj <- obj_static
  
  ## CUT OBSERVATIONS
  for(h in seq_along(obj)){
    obj[[h]] <- obj[[h]][(z-(time_period-1)):(z),];
    obj[[h]][['close']] <- log(obj[[h]][['close']] )
  }
  
  
  
  
## FIND COINTEGRATED RELATIONSHIP
found <- FALSE
for(i in seq_along(companies)){
  

  "if(adf.test( obj[[ companies[[i]] ]][['close']] )$p.value > 0.4){
    next;
  }"
  
  sample_companies = sample(companies)
  for(k in seq_along(sample_companies)){
    if(companies[[i]] == sample_companies[[k]]){
      next;
    }
    
    co <- obj[[ companies[[i]] ]][['close']]
    co2 <- obj[[ sample_companies[[k]] ]][['close']]
    
    row1 <- list_of_companies_with_cointegrated_relationship[
      which(list_of_companies_with_cointegrated_relationship$company1 == as.character(sample_companies[[k]])
            & list_of_companies_with_cointegrated_relationship$company2 == as.character(companies[[i]])), ];
    row2 <- list_of_companies_with_cointegrated_relationship[
      which(list_of_companies_with_cointegrated_relationship$company2 == as.character(sample_companies[[k]])
            & list_of_companies_with_cointegrated_relationship$company1 == as.character(companies[[i]])), ];
    
    if(nrow(row1) > 0 || nrow(row2) > 0){
      print("Combination already exists")
      next;
    }
    
    if(any(is.na(co2))){
      print("na")
      next
    }
    
    "if(adf.test( co2 )$p.value > 0.4){
      next; 
    }"
    linearmod <- lm(y ~., data.frame(
      y =  (co),
      x= (co2)
    ))
    
    residuals_var <- linearmod$residuals
    
    #adf.test(residuals_var)$p.value <= 0.01
    if(adf.test(residuals_var)$p.value <= 0.05){
      print("found")
      ## there is a cointegrated relationship
      
      the_spread <- co -linearmod$coefficients[[1]]- linearmod$coefficients[[2]]*co2
        
      found <- TRUE;
      
      #if(adf.test(the_spread)$p.value < 0.05){
      list_of_companies_with_cointegrated_relationship <-
        rbind(list_of_companies_with_cointegrated_relationship, data.frame(company1 = companies[[i]], company2=sample_companies[[k]]))
      #}
      break;  
    }
    
  }

}

return(list_of_companies_with_cointegrated_relationship)

}


list_of_companies_with_cointegrated_relationship <- find_cointegrated_relationship(NULL, companies)
list_of_companies_with_cointegrated_relationship1 <- list_of_companies_with_cointegrated_relationship

co_1 = "ZION"
co_2 = "HBAN"
plot(seq_this(obj_static[[ co_1 ]][['close']][z:(z-(time_span))]/obj_static[[ co_1 ]][['close']][[z]]),  
     obj_static[[ co_1 ]][['close']][z:(z-(time_span))]/obj_static[[ co_1 ]][['close']][[z]], type="l")
lines(seq_this(obj_static[[ co_2 ]][['close']][z:(z-(time_span))]/obj_static[[ co_2 ]][['close']][[z]]),  
      obj_static[[ co_2 ]][['close']][z:(z-(time_span))]/obj_static[[ co_2 ]][['close']][[z]], type="l")
