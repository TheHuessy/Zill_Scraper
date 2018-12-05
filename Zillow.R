library(httr)
library(RCurl)
library(rvest)
library(XML)
library(xml2)
library(curl)
library(civis)

options(timeout= 4000000)
zips <- {c("02118",
           "02119",
           "02120",
           "02130",
           "02134",
           "02135",
           "02445",
           "02446",
           "02447",
           "02467",
           "02108",
           "02114",
           "02115",
           "02116",
           "02215",
           "02128",
           "02129",
           "02150",
           "02151",
           "02152",
           "02124",
           "02126",
           "02131",
           "02132",
           "02136",
           "02109",
           "02110",
           "02111",
           "02113",
           "02121",
           "02122",
           "02124",
           "02125",
           "02127",
           "02210"
)}


zips <- sample(zips)

base.z1 <- "https://www.zillow.com/boston-ma-"
base.z2 <- "/rentals/"


Zillow <- data.frame("Address" = as.character(),
                     "Price" = as.character(),
                     "Beds" = as.character(),
                     "Baths" = as.character(),
                     "SQFT" = as.character(),
                     "Type" = as.character(),
                     "Laundry" = as.character(),
                     "Heating" = as.character(),
                     "Cooling" = as.character(),
                     "Pets" = as.character(),
                     "Parking" = as.character(),
                     "Desc" = as.character(),
                     "Scrape_Date" = as.character(),
                     "Scrape_Zip" = as.character(),
                     "Link" = as.character()
)
#for (i in 1:length(zips)){
for (i in 1){
  print(paste("Starting zip", zips[i], i, "of", length(zips)))
  
  if ((i %% 20) == 0){
    slp <- sample(30:60, 1)
    print(paste("Super Sleeping for", slp, "seconds at", Sys.time()))
    Sys.sleep(slp)  
    
  } else {
  slp <- sample(1:4, 1)
  print(paste("Sleeping for", slp, "seconds at", Sys.time()))
  Sys.sleep(slp)
  }
  
  add.links <- as.character()
  
  #Determine number of pages per landing page
  print(paste("Building URL..."))
  base.t <- paste(base.z1, zips[i], base.z2, sep = "")
  
  slp <- sample(1:4, 1)
  print(paste("Sleeping for", slp, "seconds at", Sys.time()))
  Sys.sleep(slp)
  
  print(paste("URL TRY CATCH INITIATING"))
  
  chka <-  tryCatch({
    read_html(base.t)
  },
    error = function(e){e}
  )
  if(inherits(chka, "error")) {
    print(paste("URL", base.t, "Broken...Trying Again"))
    print(paste("Sleeping for", 10, "seconds at", Sys.time()))
    Sys.sleep(10)
    chka <-  tryCatch({
      read_html(base.t)
    },
      error = function(e){e}
    )
    if(inherits(chka, "error")) {
      print("---")
      print(paste("URL", base.t, "Broken For Real"))
      next
    }
    }
  slp <- sample(1:4, 1)
  print(paste("Sleeping for", slp, "seconds at", Sys.time()))
  Sys.sleep(slp)
  
 
  plim <- read_html(base.t)
  ps <- plim %>%
    html_nodes(".main-wrapper") %>% 
    html_nodes(".active-view") %>% 
    html_nodes("#c-column") %>% 
    html_nodes("#inner-c-column") %>% 
    html_nodes("#list-container-column") %>% 
    html_nodes(".active-view") %>% 
    html_nodes("#list-results-container") %>% 
    html_nodes(".list-container") %>% 
    html_nodes("#inner-list-container") %>% 
    html_nodes("#list-core-content-container") %>% 
    html_nodes("#search-pagination-wrapper") %>% 
    html_nodes(".zsg-pagination") %>% 
    html_children() %>% 
    .[(grep("next", .)-1)] %>% 
    html_text() %>% 
    as.numeric()
    if (length(ps) == 0){
      print(paste("No additional pages found for url", base.t))
      ps <- 1
      #next
    }
  
  
  
  
  for (t in 1:ps){
    
    base.z <- paste(base.z1, zips[i], base.z2, t, "_p/", sep = "")
    
    slp <- sample(1:4, 1)
    print(paste("Sleeping for", slp, "seconds at", Sys.time()))
    Sys.sleep(slp)
    
    print(paste("Link Try Catch Initiating..."))
    
    chka <-  tryCatch({
      read_html(base.z)
    },
    error = function(e){e}
    )
    if(inherits(chka, "error")) {
      print(paste("Try Catch Failed!"))
      print(paste("URL", base.z, "Broken...Trying Again"))
      print(paste("Sleeping for", 10, "seconds at", Sys.time()))
      Sys.sleep(10)
      
      chka <-  tryCatch({
        read_html(base.z)
      },
      error = function(e){e}
      )
      if(inherits(chka, "error")) {
        print("---")
        print(paste("URL", base.z, "Broken For Real"))
        next
      }
    }
    print(paste("Try Catch Success!"))
    slp <- sample(1:4, 1)
    print(paste("Sleeping for", slp, "seconds at", Sys.time()))
    Sys.sleep(slp)
    
    print(paste("Reading Link",t, "of", ps))
    
    z.inf <- read_html(base.z)
    

    #grab hrefs
    z.list <- z.inf %>%
      html_nodes(".photo-cards") %>%
      html_nodes("li") %>%
      html_nodes("a") %>%
      html_attr("href")
    
    #id the links that are just favorite buttons
    print(paste("IDing links that are just favorite buttons"))
    
    z.lk <- grep("myzillow", z.list)
    
    #remove favorite button links from list of links
    z.list <- z.list[-z.lk]
    
    #the vorwahl is www.zillow.com/....
    z.links <- paste("https://www.zillow.com", z.list, "?fullpage=true", sep = "")
    #now you have one page's worth of links!
    
    #Seperate out the building links and grab the individual listings from them
    bs <- z.links[grep("/b/", z.links)]
    
    #remove buildings from z.links
    z.links <- z.links[-grep("/b/", z.links)]
    
    #grab listings
    if (length(bs) == 0){
      print("No building pages...moving on")
      next
      }
    for (z in 1:length(bs)){
      print(paste("Beginning Building Page Link Pull",z, "of", length(bs)))
      
      slp <- sample(1:4, 1)
      print(paste("Sleeping for", slp, "seconds at", Sys.time()))
      Sys.sleep(slp)
      
      print(paste("Building Page Link Try Catch Initiated"))
      
      chka <-  tryCatch({
        read_html(bs[z])
      },
      error = function(e){e}
      )
      if(inherits(chka, "error")) {
        print(paste("Try Catch Failed!"))
        print(paste("URL", bs[z], "Broken...Trying Again"))
        print(paste("Sleeping for", 10, "seconds at", Sys.time()))
        Sys.sleep(10)
        chka <-  tryCatch({
          read_html(bs[z])
        },
        error = function(e){e}
        )
        if(inherits(chka, "error")) {
          print("---")
          print(paste("URL", bs[z], "Broken For Real"))
          next
        }
      }
      
      print(paste("Try Catch Success!"))
      
      slp <- sample(1:4, 1)
      print(paste("Sleeping for", slp, "seconds at", Sys.time()))
      Sys.sleep(slp)
      
      blista <-  read_html(bs[z])
      
      #Check type of page: either going to have a facts section or a rich data section
      
      pinf.rd <-  blista %>%
        html_nodes("#wrapper") %>% 
        html_nodes("#search-detail-lightbox") %>%
        html_nodes("#search-detail-lightbox_content") %>%
        html_children() %>% 
        html_nodes("#search-detail-lightbox-content") %>% 
        html_nodes("#detail-container-column") %>% 
        html_nodes(".active-view.preload-lightbox") %>% 
        html_nodes("main") %>%
        html_nodes("#bdp-content") %>%
        html_nodes(".bdp-rich-data.bdp-module.zsg-content-component") %>% 
        length()
      
      pinf.f <-  blista %>%
        html_nodes("#wrapper") %>% 
        html_nodes("#search-detail-lightbox") %>%
        html_nodes("#search-detail-lightbox_content") %>%
        html_children() %>% 
        html_nodes("#search-detail-lightbox-content") %>% 
        html_nodes("#detail-container-column") %>% 
        html_nodes(".active-view.preload-lightbox") %>% 
        html_nodes("main") %>%
        html_nodes("#bdp-content") %>%
        html_nodes(".bdp-module.building-facts.zsg-content-section") %>% 
        length()

        
      
      if (length(pinf.rd) > 0){
        #pull description data from rich-data nodes
        #Laundry
        lndy <- blista %>%
          html_nodes("#wrapper") %>% 
          html_nodes("#search-detail-lightbox") %>%
          html_nodes("#search-detail-lightbox_content") %>%
          html_children() %>% 
          html_nodes("#search-detail-lightbox-content") %>% 
          html_nodes("#detail-container-column") %>% 
          html_nodes(".active-view.preload-lightbox") %>% 
          html_nodes("main") %>%
          html_nodes("#bdp-content") %>% 
          html_nodes(".bdp-rich-data.bdp-module.zsg-content-component") %>% 
          html_children() %>% 
          html_children() %>% 
          html_children() %>% 
          .[grep("laundry", ., ignore.case = TRUE)] %>% 
          html_text() %>% 
          .[which(nchar(.) > 0)]
        if (length(lndy) == 0){
          lndy <- NA
        }
        
        #Heating
        heat <- blista %>%
          html_nodes("#wrapper") %>% 
          html_nodes("#search-detail-lightbox") %>%
          html_nodes("#search-detail-lightbox_content") %>%
          html_children() %>% 
          html_nodes("#search-detail-lightbox-content") %>% 
          html_nodes("#detail-container-column") %>% 
          html_nodes(".active-view.preload-lightbox") %>% 
          html_nodes("main") %>%
          html_nodes("#bdp-content") %>% 
          html_nodes(".bdp-rich-data.bdp-module.zsg-content-component") %>% 
          html_children() %>% 
          html_children() %>% 
          html_children() %>% 
          .[grep("heating", ., ignore.case = TRUE)] %>% 
          html_text()
        if (length(heat) == 0){
          heat <- NA
        }
        
        #Cooling
        cool <- blista %>%
          html_nodes("#wrapper") %>% 
          html_nodes("#search-detail-lightbox") %>%
          html_nodes("#search-detail-lightbox_content") %>%
          html_children() %>% 
          html_nodes("#search-detail-lightbox-content") %>% 
          html_nodes("#detail-container-column") %>% 
          html_nodes(".active-view.preload-lightbox") %>% 
          html_nodes("main") %>%
          html_nodes("#bdp-content") %>% 
          html_nodes(".bdp-rich-data.bdp-module.zsg-content-component") %>% 
          html_children() %>% 
          html_children() %>% 
          html_children() %>% 
          .[grep("cooling", ., ignore.case = TRUE)] %>% 
          html_text()
        if (length(cool) == 0){
          cool <- NA
        }   
        
        
        #Pets
        pwrd <- c("cat", "dog", "pet")
        pet <- blista %>%
          html_nodes("#wrapper") %>% 
          html_nodes("#search-detail-lightbox") %>%
          html_nodes("#search-detail-lightbox_content") %>%
          html_children() %>% 
          html_nodes("#search-detail-lightbox-content") %>% 
          html_nodes("#detail-container-column") %>% 
          html_nodes(".active-view.preload-lightbox") %>% 
          html_nodes("main") %>%
          html_nodes("#bdp-content") %>% 
          html_nodes(".bdp-rich-data.bdp-module.zsg-content-component") %>% 
          html_children() %>% 
          html_children() %>% 
          html_children() %>% 
          .[grep(paste(pwrd, collapse = "|"), ., ignore.case = TRUE)] %>% 
          html_text() %>% 
          .[which(nchar(.) > 0)]
        
        if (length(pet) == 0){
          pet <- NA
        }
        
        #Parking
        prkwrd <- c("garage", "park")
        prk <- blista %>%
          html_nodes("#wrapper") %>% 
          html_nodes("#search-detail-lightbox") %>%
          html_nodes("#search-detail-lightbox_content") %>%
          html_children() %>% 
          html_nodes("#search-detail-lightbox-content") %>% 
          html_nodes("#detail-container-column") %>% 
          html_nodes(".active-view.preload-lightbox") %>% 
          html_nodes("main") %>%
          html_nodes("#bdp-content") %>% 
          html_nodes(".bdp-rich-data.bdp-module.zsg-content-component") %>% 
          html_children() %>% 
          html_children() %>% 
          html_children() %>% 
          .[grep(paste(prkwrd, collapse = "|"), ., ignore.case = TRUE)] %>% 
          html_text() %>% 
          .[which(nchar(.) > 0)]
        
        if (length(prk) == 0){
          prk <- NA
        }
        
        #Description
        descr <- blista %>%
          html_nodes("#wrapper") %>% 
          html_nodes("#search-detail-lightbox") %>%
          html_nodes("#search-detail-lightbox_content") %>%
          html_children() %>% 
          html_nodes("#search-detail-lightbox-content") %>% 
          html_nodes("#detail-container-column") %>% 
          html_nodes(".active-view.preload-lightbox") %>% 
          html_nodes("main") %>%
          html_nodes("#bdp-content") %>% 
          html_nodes(".bdp-rich-data.bdp-module.zsg-content-component") %>% 
          html_text()
        if (length(descr) == 0){
          descr <- NA
        }

      } else if (length(pinf.f) > 0){
        #pull description data from facts nodes
        
        #Laundry
        lndy <- blista %>%
          html_nodes("#wrapper") %>% 
          html_nodes("#search-detail-lightbox") %>%
          html_nodes("#search-detail-lightbox_content") %>%
          html_children() %>% 
          html_nodes("#search-detail-lightbox-content") %>% 
          html_nodes("#detail-container-column") %>% 
          html_nodes(".active-view.preload-lightbox") %>% 
          html_nodes("main") %>%
          html_nodes("#bdp-content") %>% 
          html_nodes(".bdp-module.building-facts.zsg-content-section") %>% 
          html_children() %>% 
          html_children() %>% 
          html_children() %>% 
          .[grep("laundry", ., ignore.case = TRUE)] %>% 
          html_text()
        if (length(lndy) == 0){
          lndy <- NA
        }
        
        #Heating
        heat <- blista %>%
          html_nodes("#wrapper") %>% 
          html_nodes("#search-detail-lightbox") %>%
          html_nodes("#search-detail-lightbox_content") %>%
          html_children() %>% 
          html_nodes("#search-detail-lightbox-content") %>% 
          html_nodes("#detail-container-column") %>% 
          html_nodes(".active-view.preload-lightbox") %>% 
          html_nodes("main") %>%
          html_nodes("#bdp-content") %>% 
          html_nodes(".bdp-module.building-facts.zsg-content-section") %>% 
          html_children() %>% 
          html_children() %>% 
          html_children() %>% 
          .[grep("heating type", ., ignore.case = TRUE)] %>% 
          html_text()
        if (length(heat) == 0){
          heat <- NA
        }
        
        #Cooling
        cool <- blista %>%
          html_nodes("#wrapper") %>% 
          html_nodes("#search-detail-lightbox") %>%
          html_nodes("#search-detail-lightbox_content") %>%
          html_children() %>% 
          html_nodes("#search-detail-lightbox-content") %>% 
          html_nodes("#detail-container-column") %>% 
          html_nodes(".active-view.preload-lightbox") %>% 
          html_nodes("main") %>%
          html_nodes("#bdp-content") %>% 
          html_nodes(".bdp-module.building-facts.zsg-content-section") %>% 
          html_children() %>% 
          html_children() %>% 
          html_children() %>% 
          .[grep("cooling", ., ignore.case = TRUE)] %>% 
          html_text()
        if (length(cool) == 0){
          cool <- NA
        }
        
        #Pets
        pet <- blista %>%
          html_nodes("#wrapper") %>% 
          html_nodes("#search-detail-lightbox") %>%
          html_nodes("#search-detail-lightbox_content") %>%
          html_children() %>% 
          html_nodes("#search-detail-lightbox-content") %>% 
          html_nodes("#detail-container-column") %>% 
          html_nodes(".active-view.preload-lightbox") %>% 
          html_nodes("main") %>%
          html_nodes("#bdp-content") %>% 
          html_nodes(".bdp-module.building-facts.zsg-content-section") %>% 
          html_children() %>% 
          html_children() %>% 
          html_children() %>% 
          .[grep("pets", ., ignore.case = TRUE)] %>% 
          html_text()
        if (length(pet) == 0){
          pet <- NA
        }
        
        #Parking
        prk <- blista %>%
          html_nodes("#wrapper") %>% 
          html_nodes("#search-detail-lightbox") %>%
          html_nodes("#search-detail-lightbox_content") %>%
          html_children() %>% 
          html_nodes("#search-detail-lightbox-content") %>% 
          html_nodes("#detail-container-column") %>% 
          html_nodes(".active-view.preload-lightbox") %>% 
          html_nodes("main") %>%
          html_nodes("#bdp-content") %>% 
          html_nodes(".bdp-module.building-facts.zsg-content-section") %>% 
          html_children() %>% 
          html_children() %>% 
          html_children() %>% 
          .[grep("parking", ., ignore.case = TRUE)] %>% 
          html_text()
        if (length(prk) == 0){
          prk <- NA
        }
        
        #Description
        descr <- blista %>%
          html_nodes("#wrapper") %>% 
          html_nodes("#search-detail-lightbox") %>%
          html_nodes("#search-detail-lightbox_content") %>%
          html_children() %>% 
          html_nodes("#search-detail-lightbox-content") %>% 
          html_nodes("#detail-container-column") %>% 
          html_nodes(".active-view.preload-lightbox") %>% 
          html_nodes("main") %>%
          html_nodes("#bdp-content") %>% 
          html_nodes(".bdp-module.building-facts.zsg-content-section") %>% 
          html_text()
        if (length(descr) == 0){
          descr <- NA
        }
      } # END if pinf.f is more than 0
      
      #Type
      typ <- "Apartment"
      
      #Address
      ad1 <- blista %>%
        html_nodes("#wrapper") %>% 
        html_nodes("#search-detail-lightbox") %>%
        html_nodes("#search-detail-lightbox_content") %>%
        html_children() %>% 
        html_nodes("#search-detail-lightbox-content") %>% 
        html_nodes("#detail-container-column") %>% 
        html_nodes(".active-view.preload-lightbox") %>% 
        html_nodes("main") %>%
        html_nodes("#bdp-content") %>%
        html_nodes(".zsg-media-bd") %>% 
        .[grep("h1",.)] %>% 
        html_nodes("h1") %>% 
        html_text()
      if (length(ad1) == 0){
        ad1 <- NA
      }
      
      ad2 <- blista %>%
        html_nodes("#wrapper") %>% 
        html_nodes("#search-detail-lightbox") %>%
        html_nodes("#search-detail-lightbox_content") %>%
        html_children() %>% 
        html_nodes("#search-detail-lightbox-content") %>% 
        html_nodes("#detail-container-column") %>% 
        html_nodes(".active-view.preload-lightbox") %>% 
        html_nodes("main") %>%
        html_nodes("#bdp-content") %>%
        html_nodes(".zsg-media-bd") %>% 
        .[grep("h2",.)] %>% 
        html_nodes("h2") %>% 
        html_text()
      if (length(ad2) == 0){
        ad2 <- NA
      }
      ad <- paste(ad1, ad2)
      
    #style 1, links in rental amounts to individual listing pages
      
      blist <-  blista %>%   
        html_nodes("#units-panel-for-rent") %>%
        html_nodes(".individual-unit-price") %>%
        html_nodes(".routable") %>%
        html_attr("href")
      if (length(blist) == 0){
      
      blist <-  blista %>%   
        html_nodes(".bedroom-group-content") %>%
        html_nodes(".individual-unit-price") %>%
        html_nodes(".routable") %>%
        html_attr("href")
      }

      if (length(blist) == 0){
        #Grabbing the raw listing data
        #Checking to make sure that this sort of thing is here and this isn't just a dead link of some variaety
        
        #Check to see how many units are listed as "for rent"
        blistn <- blista %>%
          html_nodes("#wrapper") %>% 
          html_nodes("#search-detail-lightbox") %>%
          html_nodes("#search-detail-lightbox_content") %>%
          html_children() %>% 
          html_nodes("#search-detail-lightbox-content") %>% 
          html_nodes("#detail-container-column") %>% 
          html_nodes(".active-view.preload-lightbox") %>% 
          html_nodes("main") %>%
          html_nodes("#bdp-content") %>% 
          html_nodes(".zsg-badge.zsg-badge_green") %>%
          html_text() %>% 
          gsub("\\+","",.) %>% 
          as.numeric()
        
        #If there is more than 0 listings in "for rent" then continue and grab the data
        if (length(blistn) > 0){
          #Find all uls
          ndrops <-  blista %>%
            html_nodes("#wrapper") %>% 
            html_nodes("#search-detail-lightbox") %>%
            html_nodes("#search-detail-lightbox_content") %>%
            html_children() %>% 
            html_nodes("#search-detail-lightbox-content") %>% 
            html_nodes("#detail-container-column") %>% 
            html_nodes(".active-view.preload-lightbox") %>% 
            html_nodes("main") %>%
            html_nodes("#bdp-content") %>%
            html_nodes(".zsg-content-section.bdp-units") %>% 
            html_nodes(".zsg-tabview.units-section_tabbed") %>%
            html_nodes("#units-panel-for-rent") %>% 
            html_nodes(".bedroom-group-content") %>% 
            length() #gets you number of dropdown sections
         
         #Check to see how many subsections exist in each dropdown
         ndinf <- data.frame("Drodown" = as.numeric(), #number assigned to different types of units
                             "Subsection" = as.numeric(), #number assigned to different listed bed/bath/sqft listings
                             "SS.Listings" = as.numeric() #number of differently priced listings per b/b/s combo
                             )
         for (rr in 1:ndrops){
           #Get number of subsections
           SS <-  blista %>%
             html_nodes("#wrapper") %>% 
             html_nodes("#search-detail-lightbox") %>%
             html_nodes("#search-detail-lightbox_content") %>%
             html_children() %>% 
             html_nodes("#search-detail-lightbox-content") %>% 
             html_nodes("#detail-container-column") %>% 
             html_nodes(".active-view.preload-lightbox") %>% 
             html_nodes("main") %>%
             html_nodes("#bdp-content") %>%
             html_nodes(".zsg-content-section.bdp-units") %>% 
             html_nodes(".zsg-tabview.units-section_tabbed") %>%
             html_nodes("#units-panel-for-rent") %>% 
             html_nodes(".bedroom-group-content") %>% 
             .[rr] %>% 
             html_nodes(".zsg-media-bd") %>% 
             length()
          
          for (dd in 1:SS){
            #get number of listings per subsection
            SS.L <- blista %>%
              html_nodes("#wrapper") %>% 
              html_nodes("#search-detail-lightbox") %>%
              html_nodes("#search-detail-lightbox_content") %>%
              html_children() %>% 
              html_nodes("#search-detail-lightbox-content") %>% 
              html_nodes("#detail-container-column") %>% 
              html_nodes(".active-view.preload-lightbox") %>% 
              html_nodes("main") %>%
              html_nodes("#bdp-content") %>%
              html_nodes(".zsg-content-section.bdp-units") %>% 
              html_nodes(".zsg-tabview.units-section_tabbed") %>%
              html_nodes("#units-panel-for-rent") %>% 
              html_nodes(".bedroom-group-content") %>% 
              .[rr] %>% 
              html_nodes(".zsg-media-bd") %>% 
              .[dd] %>% 
              html_nodes(".individual-unit") %>% 
              length()
            
            adf <- data.frame("Drodown" = rr, #number assigned to different types of units
                              "Subsection" = dd, #number assigned to different listed bed/bath/sqft listings
                              "SS.Listings" = SS.L #number of differently priced listings per b/b/s combo
                              )
            ndinf <- rbind(ndinf, adf)
            } #END number of different price lisitngs
             
         }# End number of different subsections
         
#This gives you a table of which drop downs have how many sub sections and how many listings are in
#each subsection. You should be able to feed these parameters into another for loop to get all the listings
#data you need from a building page         
         
         for (q in 1:nrow(ndinf)){
           #Bedrooms
           bd <-  blista %>%
             html_nodes("#wrapper") %>% 
             html_nodes("#search-detail-lightbox") %>%
             html_nodes("#search-detail-lightbox_content") %>%
             html_children() %>% 
             html_nodes("#search-detail-lightbox-content") %>% 
             html_nodes("#detail-container-column") %>% 
             html_nodes(".active-view.preload-lightbox") %>% 
             html_nodes("main") %>%
             html_nodes("#bdp-content") %>%
             html_nodes(".zsg-content-section.bdp-units") %>% 
             html_nodes(".zsg-tabview.units-section_tabbed") %>%
             html_nodes("#units-panel-for-rent") %>% 
             html_nodes(".bedroom-group-content") %>% 
             .[ndinf[q,1]] %>% 
             html_nodes(".zsg-media-bd") %>% 
             .[ndinf[q,2]] %>% 
             html_nodes(".hide-xs") %>% 
             html_text() %>% 
             gsub(" bed . ", "", .) %>% 
             trimws()
           if (length(bd) == 0){
             bd <- NA
             }
           
           #Baths
           bth <- blista %>%
             html_nodes("#wrapper") %>% 
             html_nodes("#search-detail-lightbox") %>%
             html_nodes("#search-detail-lightbox_content") %>%
             html_children() %>% 
             html_nodes("#search-detail-lightbox-content") %>% 
             html_nodes("#detail-container-column") %>% 
             html_nodes(".active-view.preload-lightbox") %>% 
             html_nodes("main") %>%
             html_nodes("#bdp-content") %>%
             html_nodes(".zsg-content-section.bdp-units") %>% 
             html_nodes(".zsg-tabview.units-section_tabbed") %>%
             html_nodes("#units-panel-for-rent") %>% 
             html_nodes(".bedroom-group-content") %>% 
             .[ndinf[q,1]] %>% 
             html_nodes(".zsg-media-bd") %>% 
             .[ndinf[q,2]] %>%
             html_nodes(".short-ba") %>% 
             html_text() %>% 
             gsub(" bath", "", .) %>% 
             trimws()
           if (length(bth) == 0){
             bth <- NA
           }
           
          #SQFT
           sqf <-  blista %>%
             html_nodes("#wrapper") %>% 
             html_nodes("#search-detail-lightbox") %>%
             html_nodes("#search-detail-lightbox_content") %>%
             html_children() %>% 
             html_nodes("#search-detail-lightbox-content") %>% 
             html_nodes("#detail-container-column") %>% 
             html_nodes(".active-view.preload-lightbox") %>% 
             html_nodes("main") %>%
             html_nodes("#bdp-content") %>%
             html_nodes(".zsg-content-section.bdp-units") %>% 
             html_nodes(".zsg-tabview.units-section_tabbed") %>%
             html_nodes("#units-panel-for-rent") %>% 
             html_nodes(".bedroom-group-content") %>% 
             .[ndinf[q,1]] %>% 
             html_nodes(".zsg-media-bd") %>% 
             .[ndinf[q,2]]  %>% 
             html_nodes(".floorplan-title") %>% 
             html_text() %>% 
             gsub("[0-9] bed . [0-9] bath . ", "", .) %>% 
             trimws()
           if (length(sqf) == 0){
             sqf <- NA
           }
           for (wr in 1:ndinf[q,3]){
             pr <- blista %>%
               html_nodes("#wrapper") %>% 
               html_nodes("#search-detail-lightbox") %>%
               html_nodes("#search-detail-lightbox_content") %>%
               html_children() %>% 
               html_nodes("#search-detail-lightbox-content") %>% 
               html_nodes("#detail-container-column") %>% 
               html_nodes(".active-view.preload-lightbox") %>% 
               html_nodes("main") %>%
               html_nodes("#bdp-content") %>%
               html_nodes(".zsg-content-section.bdp-units") %>% 
               html_nodes(".zsg-tabview.units-section_tabbed") %>%
               html_nodes("#units-panel-for-rent") %>% 
               html_nodes(".bedroom-group-content") %>% 
               .[ndinf[q,1]] %>% 
               html_nodes(".zsg-media-bd") %>% 
               .[ndinf[q,2]] %>%
               html_nodes(".floorplan-units") %>% 
               html_nodes(".floorplan-unit-price") %>% 
               .[wr] %>% 
               html_text()
             if (length(pr) == 0){
               pr <- NA
               }
             #Create row here once price is in and rbind
             grb <- data.frame("Address" = ad,
                               "Price" = pr,
                               "Beds" = bd,
                               "Baths" = bth,
                               "SQFT" = sqf,
                               "Type" = typ,
                               "Laundry" = lndy,
                               "Heating" = heat,
                               "Cooling" = cool,
                               "Pets" = pet,
                               "Parking" = prk,
                               "Desc" = descr,
                               "Scrape_Date" = format(Sys.time(), "%m-%d-%Y"),
                               "Scrape_Zip" = i,
                               "Link" = bs[z]
                               )
             Zillow <- rbind(Zillow, grb)
             print("++++")
             print(paste("Finished Direct Building Listing", wr , "of", ndinf[q,3], "for subsection", ndinf[q,2], "of", max(ndinf[,2]), "in dropdown", ndinf[q,1], "of", max(ndinf[,1])))
             print("++++")
             } #END listings per subsection for loop
           } #END loop for number of discovered for rent units on building page (blistn results)
         } else {                #END IF there are listing present in style 2 on the page
           print(paste("There were no actual listing for building page:", bs[z]))
           print(paste("Finished Building Page", z, "of", length(bs)))
           next
           }
        } #END if building page brings up 0 "for rent" links -- should just skip to the next building page as the else parameter on this
      add.links <- c(add.links, blist)
      print(paste("++++"))
      print(paste("Finished Building Page", z, "of", length(bs)))
      } # END if the listing is not style 1
    
    z.links <- c(z.links, add.links)
    z.links <- unique(z.links)
    print(paste("Finished Getting Links From Page", t, "of 20 for Zip", i, "of", length(zips)))
    } # END page listing scrape
  for (a in 1:length(z.links)){
    slp <- sample(1:4, 1)
    print(paste("Sleeping for", slp, "seconds at", Sys.time()))
    Sys.sleep(slp)
    
    print(paste("Try Catch for Listing Links Initiated"))
    chka <-  tryCatch({
      read_html(z.links[a])
    },
      error = function(e){e}
    )
    if(inherits(chka, "error")) {
      print(paste("Try Catch Failed!"))
      print(paste("URL", z.links[a],  "Broken... Trying again"))
      print(paste("Sleeping for", 10, "seconds at", Sys.time()))
      Sys.sleep(10)
      chka <-  tryCatch({
        read_html(z.links[a])
      },
        error = function(e){e}
      )
      if(inherits(chka, "error")) {
        print("----")
        print(paste("URL", z.links[a], "Broken"))
        next
      }
      }
    print(paste("Try Catch Success!"))
    slp <- sample(1:3, 1)
    print(paste("Sleeping for", slp, "seconds at", Sys.time()))
    Sys.sleep(slp)  
    
    ls <- read_html(z.links[a])
    #Rental Check
    cls <- ls %>%
      html_nodes(".hdp-summary") %>%
      html_nodes("#listing-icon") %>%
      html_attrs() 
    
    rc <- grep("for-rent", cls)
    
    if (length(rc) < 1){
      print(paste("Listing", a,  "Not For Rent:", z.links[a])) 
      next
    }
    
    #Address
    ad <- ls %>%
      html_nodes(".zsg-content-header.addr") %>%
      html_nodes(".notranslate") %>%
      html_text()
    
    
    #Price
    pr <- ls %>%
      html_nodes(".main-row.home-summary-row") %>%
      html_text() %>%
      gsub("/mo", "", .) %>%
      gsub(" ", "", .)
    
    #Beds
    bd <- ls %>%
      html_nodes(".zsg-content-header.addr") %>%
      html_nodes(".addr_bbs") %>%
      html_text() %>%
      .[1]
    
    #Baths
    bth <- ls %>%
      html_nodes(".zsg-content-header.addr") %>%
      html_nodes(".addr_bbs") %>%
      html_text() %>%
      .[2]
    
    #SQFT
    sqf <- ls %>%
      html_nodes(".zsg-content-header.addr") %>%
      html_nodes(".addr_bbs") %>%
      html_text() %>%
      .[3]
    
    #Type
    typ <- ls %>% 
      html_nodes(".hdp-facts.zsg-content-component.z-moreless") %>%
      html_nodes(".zsg-g.zsg-g_gutterless") %>%
      html_children() %>%
      .[grep("Type", .)] %>%
      html_nodes(".hdp-fact-ataglance-value") %>%
      html_text()
    
    
    
    #Laundry
    lndy <- ls %>% 
      html_nodes(".hdp-facts.zsg-content-component.z-moreless") %>%
      html_nodes(".zsg-g.zsg-g_gutterless") %>%
      html_children() %>%
      .[grep("Laundry", .)] %>%
      html_nodes(".hdp-fact-ataglance-value") %>%
      html_text()
    
    
    #Heating
    heat <- ls %>% 
      html_nodes(".hdp-facts.zsg-content-component.z-moreless") %>%
      html_nodes(".zsg-g.zsg-g_gutterless") %>%
      html_children() %>%
      .[grep("Heating", .)] %>%
      html_nodes(".hdp-fact-ataglance-value") %>%
      html_text()
    
    #Cooling
    cool <- ls %>% 
      html_nodes(".hdp-facts.zsg-content-component.z-moreless") %>%
      html_nodes(".zsg-g.zsg-g_gutterless") %>%
      html_children() %>%
      .[grep("Cooling", .)] %>%
      html_nodes(".hdp-fact-ataglance-value") %>%
      html_text()
    
    #Pets
    pet <- ls %>% 
      html_nodes(".hdp-facts.zsg-content-component.z-moreless") %>%
      html_nodes(".zsg-g.zsg-g_gutterless") %>%
      html_children() %>%
      .[grep("Pets", .)] %>%
      html_nodes(".hdp-fact-ataglance-value") %>%
      html_text()
    
    #Parking
    prk <- ls %>% 
      html_nodes(".hdp-facts.zsg-content-component.z-moreless") %>%
      html_nodes(".zsg-g.zsg-g_gutterless") %>%
      html_children() %>%
      .[grep("Parking", .)] %>%
      html_nodes(".hdp-fact-ataglance-value") %>%
      html_text()
    
    #Description
    descr <- ls %>% 
      html_nodes(".main-wrapper") %>% 
      html_nodes(".zsg-layout-bc.zsg-layout-width") %>% 
      html_nodes(".zsg-layout-bc-b") %>% 
      html_nodes(".zsg-layout-content") %>% 
      html_children() %>% 
      .[grep("data-zmm",.)] %>% 
      html_nodes("#hdp-content") %>% 
      html_nodes(".hdp-content-wrapper.zsg-content-section") %>%
      #html_nodes("section") %>% 
      html_nodes(".zsg-content-section") %>% 
      html_nodes(".zsg-content-component") %>% 
      html_nodes(".notranslate.zsg-content-item") %>% 
      html_text()
    grb <- data.frame("Address" = ad,
                      "Price" = pr,
                      "Beds" = bd,
                      "Baths" = bth,
                      "SQFT" = sqf,
                      "Type" = typ,
                      "Laundry" = lndy,
                      "Heating" = heat,
                      "Cooling" = cool,
                      "Pets" = pet,
                      "Parking" = prk,
                      "Desc" = descr,
                      "Scrape_Date" = format(Sys.time(), "%m-%d-%Y"),
                      "Scrape_Zip" = i,
                      "Link" = z.links[a]
                      )
    Zillow <- rbind(Zillow, grb)
    print("++++")
    print(paste("Finished Listing", a, "of", length(z.links)))
    print("++++")
  }
  slp <- sample(1:8, 1)
  print(paste("Sleeping for", slp, "seconds at", Sys.time()))
  Sys.sleep(slp)
  print(paste("Finished Zip", i, "of", length(zips)))  
}

### WRITE TO POSTGRES SANDBOX ###

Zillow <- unique(Zillow)

pls <- nrow(Zillow)
if (pls < 1){
  print("Something went wrong, pls shows no new listings...")
  print("ending script....")
  print("......")
} else {

  #write to zillow master table
  write_civis(tablename = "sandbox.zillow_master", if_exists = "append")
  
  #write to zillow "daily" table, it's supposed to 
  write_civis(tablename = "sandbox.zillow_daily", if_exists = "drop")


  print(paste("Finished Scraping", pls, "new listings. Total listings at", nrow(Zillow)))
}



