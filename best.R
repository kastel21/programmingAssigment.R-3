best <- function(state, outcome) { ## Read outcome data
  ## Check that state and outcome are valid
  csv<-read.csv('outcome-of-care-measures.csv',colClasses = "character")
  
  outs<-c('heart attack','heart failure','pneumonia')
  ## Return hospital name in that state with lowest 30-day death ## rate

  if(sum(csv$State == state) != 0 & !is.na(state)){
    
    if(!is.na(outcome) && sum(outs == outcome) != 0){
      
      
      csv <- csv[(csv[, 7] == state), ]
      
    
        
  
        
        
        
        if(outcome == 'heart attack'){
          
          csv[, 11] <- as.numeric(csv[,11])
          
          
          
          csv <- csv[!is.na(csv[, 11]), ]
          csv <- csv[order(csv[, 11]), ]
          
          
          sn <- csv[csv[, 11] ==min(csv[, 11]), 2]
          sort(sn)[1]
          
          
          
        }else if(outcome == 'heart failure'){
          
          
          
          
          
          
          
      
          
          csv[, 17] <- as.numeric(csv[,17])
          
          
          
          csv <- csv[!is.na(csv[, 17]), ]
          csv <- csv[order(csv[, 17]), ]
          

          sn <- csv[csv[, 17] ==min(csv[, 17]), 2]
          #sn <- csv[csv[, 17] ==min(csv[, 17]), 2]
          sort(sn)[1]

        }else if(outcome == 'pneumonia'){
          
          csv[, 23] <- as.numeric(csv[,23])
          
          
          
          csv <- csv[!is.na(csv[, 23]), ]
          csv <- csv[order(csv[, 23]), ]
          
          
          sn <- csv[csv[, 23] ==min(csv[, 23]), 2]
          #sn <- csv[csv[, 17] ==min(csv[, 17]), 2]
          sort(sn)[1]
          
        }
        
        
        
        
        
        
        
      
    }else{
      stop('invalid outcome')
    }
    
  }else{
    
    stop('invalid state')
    
    
    
  }
  
  
  
}

best("SC", "heart attack")

