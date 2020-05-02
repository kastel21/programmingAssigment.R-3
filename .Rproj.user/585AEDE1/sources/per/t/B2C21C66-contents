rankall <- function(outcome, num = "best") { ## Read outcome data

      csv<-read.csv('outcome-of-care-measures.csv',colClasses = "character")

      st<-levels(as.factor( csv$State))

  print(length(st))
  outs<-c('heart attack','heart failure','pneumonia')
  ## Return hospital name in that state with lowest 30-day death ## rate
  
  
    
    if(!is.na(outcome) && sum(outs == outcome) != 0){
      
      sts<-c()
      hops<-c()
      
      df<-data.frame()
      
      for (state in st) {
        
        
        sts<-c(sts,c(state))
        #print(state)
      
        dd <- csv[(csv[, 7] == state), ]
        
          
          
          if(outcome == 'heart attack'){
            
            csv[, 11] <- as.numeric(csv[,11])
            
            
            
            csv <- csv[!is.na(csv[, 11]), ]
            #csv <- csv[order(csv[, 11]), ]
            
            csv <- csv[order(csv[, 11], csv[, 2]), ]
            
            #sn <- csv[csv[, 11] ==min(csv[, 11]), 2]
            
            
            
            if(num == 'best'){
              
              sn <- dd[1, 2]
              df <- rbind(df, data.frame(hospital = sn,state = state),stringsAsFactors = FALSE)
              
              
            }else if(num=='worst'){
              

              sn <- dd[nrow(dd), 2]
              df <- rbind(df, data.frame(hospital = sn,state = state),stringsAsFactors = FALSE)
              
              
            }else if(is.numeric(num) && num>0){
              
              sn <- dd[num, 2]
              df <- rbind(df, data.frame(hospital = sn,state = state),stringsAsFactors = FALSE)
              
              
              
            }else{
              stop("Invalid rank")
            }
            
            
            
          }else if(outcome == 'heart failure'){
            
            csv[, 17] <- as.numeric(csv[,17])
            
            
            
            csv <- csv[!is.na(csv[, 17]), ]
            csv <- csv[order(csv[, 17], csv[, 2]), ]
            
            
            #sn <- csv[csv[, 17] ==min(csv[, 17]), 2]
            
            
            if(num == 'best'){
              
              sn <- dd[1, 2]
              df <- rbind(df, data.frame(hospital = sn,state = state),stringsAsFactors = FALSE)
              
            }else if(num=='worst'){
              
              sn <- dd[nrow(dd), 2]
              df <- rbind(df, data.frame(hospital = sn,state = state),stringsAsFactors = FALSE)
              
              
            }else if(is.numeric(num) && num>0){
              sn <- dd[num, 2]
              df <- rbind(df, data.frame(hospital = sn,state = state),stringsAsFactors = FALSE)
              
              
              
            }else{
              stop("Invalid rank")
            }
            
            
            
          }else if(outcome == 'pneumonia'){
            
            csv[, 23] <- as.numeric(csv[,23])
            
            
            
            csv <- csv[!is.na(csv[, 23]), ]
            csv <- csv[order(csv[, 23], csv[, 2]), ]
            
            
            #sn <- csv[csv[, 23] ==min(csv[, 23]), 2]
            
            
            
            if(num == 'best'){
              
              
              sn <- dd[1, 2]
              df <- rbind(df, data.frame(hospital = sn,state = state),stringsAsFactors = FALSE)
              
            }else if(num=='worst'){
              
              sn <- dd[nrow(dd), 2]
              df <- rbind(df, data.frame(hospital = sn,state = state),stringsAsFactors = FALSE)
              
              
            }else if(is.numeric(num) && num>0){
              
              sn <- dd[num, 2]
              df <- rbind(df, data.frame(hospital = sn,state = state),stringsAsFactors = FALSE)
              
              
              
            }else{
              stop("Invalid rank")
            }
            
          }
          
          
          
          
          
          
          }
      
      
df
      
      
    }else{
      stop('invalid outcome')
    }
    

  
  
  
  
  
  
  
}

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)

