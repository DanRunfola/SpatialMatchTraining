#SciClone Helper Functions
rename_header <- function(x,sub)
{
  t <- paste(substr(x, 1, 0), sub, substr(x, 1, nchar(x)), sep = "")
  substr(t, 1, nchar(t)-1)
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}



timeRangeTrend <- function(dta,prefix,startyr,endyr,IDfield, formatT, ...)
{
  grep_str = paste(IDfield,prefix,sep="|")
  tDF <- dta@data[grepl(grep_str,names(dta@data))]
  
  dta_IDs <- unique(grep(paste(prefix,collapse="|"), 
                         names(tDF)))
  
  names(tDF)[dta_IDs]<- 
    sapply(
      names(tDF)[dta_IDs], 
      function(x) {
        rename_header(x, substrRight(x,1))
      }
    )
  
  
  analysisDF <- melt(tDF,id=c(IDfield))
  
  
  #cleaned GREP
  new_pre <- gsub("[0-9]","",prefix,fixed=TRUE)
  new_pre<- 
    sapply(
      new_pre, 
      function(x) {
        rename_header(x, substrRight(x,1))
      }
    )
  if(formatT == "y")
  {
    analysisDF["Year"] <- lapply(analysisDF["variable"],FUN=function(x) as.numeric(gsub(new_pre,"",x)))
    
    analysisDF <- analysisDF[analysisDF["Year"] >= startyr ,]
    analysisDF <- analysisDF[analysisDF["Year"] <= endyr ,]
  }
  
  if(formatT == "ym")
  {
    analysisDF["Year"] <- lapply(analysisDF["variable"],FUN=function(x) as.numeric(gsub(new_pre,"",substr(as.character(x), 1, nchar(as.character(x))-2))))
    analysisDF <- analysisDF[analysisDF["Year"] >= startyr ,]
    analysisDF <- analysisDF[analysisDF["Year"] <= endyr ,]
    first_year <- min(analysisDF["Year"])
    analysisDF["Month"] <- lapply(analysisDF["variable"],FUN=function(x) as.numeric(gsub(new_pre,"",substr(as.character(x), nchar(as.character(x))-1, nchar(as.character(x))))))
    
    analysisDF["OldYear"] <- analysisDF["Year"]
    analysisDF["Year"] <- analysisDF["Year"] + 1
    
    analysisDF["ID_lm"] <- lapply(analysisDF["Year"], FUN=function(x) x - first_year) 
    analysisDF["ID_lmb"] <- (analysisDF["ID_lm"] * 12) +(analysisDF["Month"][[1]])
    analysisDF["Year"] <- analysisDF["ID_lmb"]
    
    
  }
  #pc41_198001e
  
  dta@data["newfieldID"] <- 0
  for (i in 1:length(dta))
  {
    ID <- as.character(dta@data[IDfield][i,])
    #Fit trend model
    ID_dat <- analysisDF[analysisDF[IDfield] == ID,]
    trend_mod <- lm(value ~ Year,data=ID_dat)
    
    dta@data["newfieldID"][i,] <- summary(trend_mod)$coefficients[2]
    #print(summary(trend_mod))
    #plot(ID_dat$Year, ID_dat$value)
    #abline(trend_mod)
    
  }
  return(dta@data$newfieldID)
  
}

timeRangeAvg <- function(dta,prefix,affix,startyr,endyr, dateF)
{
  if(dateF == "ym"){
    searchS = paste("^",prefix,startyr,"..",affix,sep="")
    searchE = paste("^",prefix,endyr,"..",affix,sep="")
  }
  if(dateF == "y")
  {
    searchS = paste("^",prefix,startyr,affix,sep="")
    searchE = paste("^",prefix,endyr,affix,sep="")
  }
  
  strt_id <- grep(searchS,colnames(dta@data))
  end_id <- grep(searchE,colnames(dta@data))
  
  rmean <- rowMeans(dta@data[strt_id[[1]]:end_id[[length(end_id)]]], 
                    na.rm=FALSE)
  return(rmean)
}

#--------------------------------------------------
#--------------------------------------------------
#--------------------------------------------------