SimEvent = function(n,p) {
  
  # Settings
  only.flood = 0
  only.slide = 0
  both = 0
  
  # Simulate n years
  for (i in 1:n) {
    
    # Step1: Simulating simple event under a certain probability -------------------------------
    ## flood event under 0.3 probability
    f.samp = sample(x = 1:10, size = 1, replace = T)
    if (f.samp <= 3) {
      flood = 1
    } else {
      flood = 0
    }
    
    ## slide event under 0.4 probability
    s.samp = sample(x = 1:10, size = 1, replace = T)
    if (s.samp <= 4) {
      slide = 1
    } else {
      slide = 0
    }
    
    # Step2: Count compound events ---------------------------------
    ## (1) Only flood: 
    if (flood == 1 & slide == 0) {
      only.flood = only.flood + 1
    }
    
    ## (2) Only slide:
    if (flood == 0 & slide == 1) {
      only.slide = only.slide + 1
    }
    
    ## (3) Both: 
    if (flood == 1 & slide == 1) {
      both = both + 1
    }
    ## (4) flood but no slide: 
    if (flood == 1 & slide == 1) {
      both = both + 1
    }
    
  }
  
  # Step3: Preparing the output message --------------------------
  
  #as.character()
  txt = paste("Count of years with only flood event:", only.flood, "|", 
              "Count of years with only slide event:", only.slide, "|", 
              "Count of years with both event:", both)
  print(txt)
  
  result = list(only.flood = only.flood, 
                only.slide = only.slide, 
                both = both)
  return(result)
}

#1有沒有在cearl中
1%in%cearls 
#直接抽八包
sample(x=1:4,size=8,replace=T) 