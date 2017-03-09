woe_function <- function(matrix , variable) {
  
  for(i in 1 : nrow(matrix)){
    
    k <- matrix
    
    s <- k[i,1]
    
    if(s=="NA"){
      
      replace_by = k[i,2]
      
      variable[which(is.na(variable))] = replace_by
      
    } else {
      
      s <- str_replace_all(s, fixed(" "), "")
      
      s_list <- strsplit(gsub("\\[|\\]", "", s), split=",")
      
      n = as.integer(s_list[[1]][[1]])
      m = as.integer(s_list[[1]][[2]])
      
      range <- n:m
      
      replace_by = k[i,2]
      
      
      variable[which(variable %in% range)] = replace_by
      Credit_Demogs_merge_4[,14]
      
    }
    
  }
  
  return(variable)
}

empty_marix <- matrix(0,nrow(Credit_Demogs_merge_4),ncol(Credit_Demogs_merge_4))

for(i in 1:ncol(Credit_Demogs_merge_4))
{
  
  empty_marix[,i] = woe_function(matrix_1[[i]],Credit_Demogs_merge_4[,i])  
  
}


woe_data <- data.frame(empty_marix)



for(i in 1:ncol(woe_data)){
  
  colnames(woe_data)[i] <- colnames(Credit_Demogs_merge_4)[i]
  
}


