## Run JavaSetup.exe // Set JAVA_HOME to C:\Program Files (x86)\Java\jre1.8.0_251
## PATH to JAVA not found. Please check JAVA is installed. 

## Make sure to download Chrome


library(RSelenium)
library(raster)
library(magrittr)
library(rvest)
library(stringi)
library(stringr)
library(jsonlite)
library(dplyr)

init_scraper <- function(baseUrl, endUrl, windows = FALSE){
  Sys.setenv(TZ = "Asia/Hong_Kong") # Setting Time Zone to HK
  baseUrl <<- baseUrl
  endUrl <<- endUrl
  all_df_pg <<- c()
  all_df_posts <<- c()
  failed <<- c()
}

print_debug <- function(message){
  print(paste0("[DEBUG] ", message))
}

print_error <- function(message){
  print(paste0("[ERROR] !!!!!! ", message))
}


launch_browser <- function(browser="chrome", chromever = "84.0.4147.30", port = sample(1000:60000, 1)){
  driver <<- rsDriver(browser=browser, chromever = chromever, port = port, phantomver = NULL)
  remote_driver <<- driver[["client"]]
  remote_driver$navigate(baseUrl)
}

crack_it <- function(url){
  remote_driver$navigate(url)
  Sys.sleep(sample(seq(3, 5, by=0.001), 1))
  html <- remote_driver$getPageSource()
  if(grepl("recaptcha_widget", html[[1]])){
    readline(prompt="Captcha Present.. Press [Enter] after done solving")
  }
  pg <-  read_html(html[[1]])
  return(pg)
}

lay_low <- function(){
  Sys.sleep(sample(seq(1, 2, by=0.001), 1))
}


scrape_page <- function(page, url){
  
  thread_df.posts <- c()
  attempt <- 1
  notdone <- TRUE
  
  print_debug(paste0("Scraping Page: ",url))
  
  
  while( notdone && attempt <= 2 ) { # Auto restart when fails
    print(paste0("Attempt: ", attempt))
    attempt <- attempt + 1
    try({
      
      
      html <- crack_it(url) 
      
      urlList <- html %>% html_nodes(".post-header") %>% html_nodes("h2 > a") %>% html_attr('href')
      
      #### Scrape topic content ####
      
        for (i in 1:length(urlList)) {
          
          thread_df.posts <- tryCatch({
            post_url_html <- crack_it(urlList[i])
            title <- post_url_html %>% html_nodes("title") %>% html_text()
            
            contentList <- post_url_html %>% html_nodes(".post-entry") %>% html_nodes("p") %>% html_text()
            contentList <- contentList[contentList != ""]
            
            for (j in 1:length(contentList)) {
              
            }
            
            explanation <- stri_split_fixed(post_url_content[1], ": ", 2)[[1]][2]
            originated_from <- stri_split_fixed(post_url_content[2], ": ", 2)[[1]][2]
            full_article_url <- post_url_html %>% html_nodes("article > div > div > div > a")  %>% html_attr('href')
            
            outdf <- as.data.frame(cbind(author, date, country, rating, title, explanation, originated_from, post_url, full_article_url))
            outdf$post_id <- postId[i]
            outdf$page <- page
            outdf$ts <- Sys.time()  # Timestamp
            thread_df.posts <- rbind(thread_df.posts, outdf)
            
          }, error = function(e) {
            print_error(e)
            print(postId[i])
            print(url)
            return(rbind(thread_df.posts, as.data.frame(cbind(author = "ERROR", date = "ERROR",  country = "ERROR", rating = "ERROR", title = "ERROR", 
                                                              explanation = "ERROR", originated_from = "ERROR",
                                                              post_url = "ERROR", full_article_url= "ERROR",
                                                              post_id = postId[i],
                                                              page = page,
                                                              ts = Sys.time()))))
          })
        }
        
      
      # Done
      notdone <- FALSE
      lay_low()
    })
  } # End of While Loop
  
  
  
  if( notdone && attempt > 1 ){
    # Error page, Stopping
    print_error(paste0("Error page.", url))
    break
  }
  
  
  # Parse main thread info Rda
  save(thread_df.posts,file=paste0("indv-page-",page,".Rda"))
  
  
  return(thread_df.posts)
}


start_scraping_posts <- function(){ 
  
  while (pageidn <= stoppagen){
    print_debug(paste0("Scrape page no: ", pageidn))
    indvidual_df_posts <- scrape_page(pageidn, paste0(baseUrl, endUrl, pageidn))
    all_df_posts <<- bind_rows(all_df_posts, indvidual_df_posts)
  
    pageidn <<- pageidn + 1
  }
  
  # Save topic links
  save(all_df_posts,file=paste0("all-pages-",stoppagen,".Rda"))
  print_debug("DONE SCRAPING. Pls check for [ERROR] logs.")
  
}


################ Step 1: Set Up (Only do once) ################
# Set the working directory - create [out folder here]
outfilePath <<- "outfc"

# Start page
pageidn <<- 1 
# Stop page
stoppagen <<- 2


setwd(outfilePath)
# Remove all the data.. run only once for the entire set

init_scraper(baseUrl="https://eatbook.sg/", 
             endUrl = "category/food-reviews/restaurant-reviews/page/", windows = FALSE)

################ Step 2: Launch Browser and Solve Captcha ################
launch_browser()
# If browser crashed, run this again

start_scraping_posts()
# If scraping stopped, run this again