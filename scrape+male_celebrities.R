library(rvest)
library(tidyverse)

    male_celebs <- read_html("https://www.realmenrealstyle.com/best-dressed-stylish-men/")
    
    # use selectorgadget
    
    males <- male_celebs %>%
        xml_nodes(".size-full") %>% 
        xml_attr("src")
    
    for(k in seq_along(males)){
        download.file(url = males[k], destfile = paste("www/male_celebs/maleceleb",k,".jpg", sep = ""))    
    }
  