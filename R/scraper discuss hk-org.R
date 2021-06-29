library(RSelenium)
library(raster)
library(magrittr)
library(rvest)
library(stringi)
library(stringr)
library(jsonlite)
library(dplyr)

init_scraper <- function(categoryId, baseUrl, windows = FALSE){
  Sys.setenv(TZ = "Asia/Hong_Kong") # Setting Time Zone to HK
  if(windows){
    Sys.setlocale("LC_CTYPE", locale="Chinese (Traditional)")
  }
  baseUrl <<- baseUrl
  categoryId <<- categoryId
  pagen <<- 1
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

launch_browser <- function(browser="chrome", chromever = "83.0.4103.39", port = sample(1000:60000, 1)){
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

scrape_category <- function(){
  html <- crack_it(paste0(baseUrl, "forumdisplay.php?fid=", categoryId)) 
  
  lastpg <- strtoi(html %>% html_node(".threadlist__top-controls .last")  %>% html_attr("data-pn"))
  
  print_debug(paste0("Scraping link: ",paste0(baseUrl, "forumdisplay.php?fid=", categoryId)))
  print_debug(paste0("From Page 1 to Page ",lastpg))
  
  return(lastpg)
}

scrape_page <- function(page, url){
  
  thread_df.main <- c()
  attempt <- 1
  notdone <- TRUE
  
  print_debug(paste0("Scraping Page: ",url))
  
    
    while( notdone && attempt <= 2 ) { # Auto restart when fails
      print(paste0("Attempt: ", attempt))
      attempt <- attempt + 1
      try({
        

        html <- crack_it(url) 
        
        topicTitle <- gsub("\t","",html %>% html_nodes(".tsubject") %>% html_text())
        topicTitle <- gsub("\n","",topicTitle)
        
        topicUrl <- html %>% html_nodes(".tsubject") %>% html_children() %>% html_attr('href')
        topicUrl <- topicUrl[!is.na(topicUrl)]
        
        if (length(topicTitle) == length(topicUrl)){
          
          thread_df.main <- as.data.frame(cbind(page,topicTitle, topicUrl))
          
        } else {
          print_error("Hot topics links and titles mismatch!")
          print(topicTitle)
          print(topicUrl)
          next
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
    #save(thread_df.main,file=paste0("cat-",categoryId,"-page-",page,".Rda"))
      
    
  return(thread_df.main)
}

scrape_posts <- function(topic, url){
  
  thread_df.main <- c()
  attempt <- 1
  notdone <- TRUE
  
  print_debug(paste0("Scraping Topic Posts: ",url))
  
  
  while( notdone && attempt <= 2 ) { # Auto restart when fails
    print(paste0("Attempt: ", attempt))
    attempt <- attempt + 1
    postpg <- 1
    
    try({
      
      html <- crack_it(url) 
      
      # Read topic_id
      topic_id <- as.numeric(gsub("([0-9]+).*$", "\\1", sapply(strsplit(url, "tid="), "[[", 2)))
      
      
      # Read number of views + replies + following
      view_no <-0
      reply_no <-0
      following_no <-0
      headers <- strsplit(gsub(" ","", html %>% html_nodes(".viewthread__head .viewthread-number") %>% html_text()), "\n")[[1]]
      for (i in 1:length(headers)) {
        if (grepl("瀏覽", headers[i], fixed = TRUE)) {view_no <- strtoi(gsub(",","", gsub("瀏覽:","", headers[i])))}
        else if (grepl("回覆", headers[i], fixed = TRUE)) {reply_no <- strtoi(gsub(",","", gsub("回覆:","", headers[i])))}
        else if (grepl("追帖", headers[i], fixed = TRUE)) {following_no <- strtoi(gsub(",","", gsub("追帖:","", headers[i])))}
      }
      
      # Read title
      topic_title <- html %>% html_node(".topbar_tid")  %>% html_text()
      
      # Read number of pages within topic
      lastpg <- strtoi(html %>% html_node(".pagination-control .pagination-buttons .last")  %>% html_attr("data-pn"))
      if (is.na(lastpg)) {
        c <- html %>% html_node(".pagination-control .pagination-buttons") %>% html_text()
        lastpg <- strtoi(str_sub(paste(regmatches(c, gregexpr("[[:digit:]]+", c))[[1]], collapse = ''),-1,-1))
      } 
      if (is.na(lastpg)) {lastpg <- 1} 
      
      #### Scrape topic content ####
      
      thread_df.posts <- c()
      
      for (postpg in 1:lastpg){
        attempt_postpg <- 1
        notdone_postpg <- TRUE
        
        while (notdone_postpg && attempt_postpg <= 2) {
          
          print_debug(paste0("Scraping Page: ", postpg, " / ", lastpg))
          print(paste0("Attempt Post Page: ", attempt_postpg))
          attempt_postpg <- attempt_postpg + 1
          
          try({
            
            url_postpg <- paste0(url, "&page=", postpg)
            
            if (postpg > 1) {html <- crack_it(url_postpg)}
            
            # Post Id
            post_id <- html %>% html_nodes(".post-date .post-number") %>% html_attr('id')
            post_id <- as.numeric(gsub("([0-9]+).*$", "\\1", sapply(strsplit(post_id, "_"), "[[", 2)))
            
            # User name
            user_name <- html %>% html_nodes(".author-detail .autor-name-row .name") %>% html_text()
            user_url <- html %>% html_nodes(".author-detail .autor-name-row .name") %>% html_attr('href')
            
            # Post date
            post_date <- strsplit(gsub("\n","", html %>% html_nodes(".postinfo .post-date") %>% html_text()), "發表於")
            post_no <- str_trim(html %>% html_nodes(".postinfo .post-date .post-number") %>% html_text())
            post_date <- str_trim(sapply(post_date, "[[", 2))
            
            # Post content
            post_obj_ls <- html %>% html_nodes(".postmessage-content > span") 
            post_body <- c()
            for (post_obj in post_obj_ls) {
              tempdf <- c()
              tempdf$post_id <- as.numeric(gsub("([0-9]+).*$", "\\1", sapply(strsplit(post_obj %>% html_attr('id'), "_"), "[[", 2)))
              tempdf$post_content <- post_obj %>% html_text()
              tempdf$post_content_raw <- toString(post_obj)
              
              post_imgs <- post_obj %>% html_nodes("img") %>% html_attr('src')
              tempdf$post_imgs <- list(tibble(post_imgs))
              
              quote_post <- post_obj %>% html_node("blockquote > a") %>% html_attr('href')
              quote_post_source <- NA
              quote_thread_source <- NA
              quote_url_source <- NA
              if (!is.na(quote_post)) {
                quote_post_source <- as.numeric(gsub("([0-9]+).*$", "\\1", sapply(strsplit(quote_post,"pid="), "[[", 2)))
                quote_thread_source <- as.numeric(gsub("([0-9]+).*$", "\\1", sapply(strsplit(quote_post,"ptid="), "[[", 2)))
                quote_url_source <- paste0(baseUrl,"viewthread.php?tid=",quote_thread_source,"&pid=",quote_post_source,"#pid",quote_post_source)
              } 
              
              tempdf$quote <- list(tibble(as.data.frame(cbind(quote_post_source,quote_thread_source,quote_url_source))))
              
              post_body <- bind_rows(post_body, tempdf)
            }
            
            
            # Timestamp
            ts <- Sys.time()
            
            thread_df.posts <- tryCatch({
              outdf <- as.data.frame(cbind(post_no, post_id, user_name, user_url, post_date))
              outdf <- merge(outdf, post_body, by.x="post_id", by.y="post_id", all = TRUE)
              outdf$ts <- ts
              thread_df.posts <- rbind(thread_df.posts, outdf)
              
            }, warning = function(w) {
              print_error(w)
              print(post_no)
              print(post_id)
              print(post_date)
              
              return(rbind(thread_df.posts, as.data.frame(cbind(post_no = "ERROR", post_no = "ERROR",  post_id = "ERROR", user_name = "ERROR", user_url = "ERROR", 
                                                                            post_date = "ERROR", post_content = "ERROR", post_imgs = "ERROR", 
                                                                            ts = "ERROR"))))
            }, error = function(e) {
              print_error(e)
              return(rbind(thread_df.posts, as.data.frame(cbind(post_no = "ERROR", post_no = "ERROR",  post_id = "ERROR", user_name = "ERROR", user_url = "ERROR", 
                                                                post_date = "ERROR", post_content = "ERROR", post_imgs = "ERROR", 
                                                                ts = "ERROR"))))
            })
            
            
            # Done
            notdone_postpg <- FALSE
            lay_low()
          })
          
        }
        
        if( notdone_postpg && attempt_postpg > 1 ){
          # Error page, Stopping
          print_error(paste0("Error Scraping page. ", url_postpg))
        }
        
      }

      #############################
      
      thread_df.main <- as.data.frame(cbind(topic, topic_id, topic_title, view_no, reply_no, following_no))
      thread_df.main$no_pg <- lastpg
      thread_df.main$topic_url <- url
      thread_df.main$posts_content <- list(tibble(thread_df.posts))
      
      # Done
      notdone <- FALSE
      lay_low()
    })
  } # End of While Loop
  
  if( notdone && attempt > 1 ){
    # Error page, Stopping
    print_error(paste0("Error page. ", url))
    break
  }
  
  
  # Parse individual topic JSON + Rda
  
  #load("C:/Users/dinhk_1phzc8w/Desktop/py/LIHKGr-master/LIHKGr-master/R/out/topic-content-1230.Rda")
  #all_df_posts <- toJSON(all_df_posts, dataframe = "rows")
  #con <- file(paste0("topic-content-1230.json"), open = "w+", encoding = "native.enc")
  #writeLines(all_df_posts, con = con, useBytes = TRUE)
  #close(con)
  
  # Parse main thread info Rda
  save(thread_df.main,file=paste0("indv-topic-",topic,".Rda"))
  
  
  return(thread_df.main)
}

start_scraping_pg <- function(){  
  
  stopn <- scrape_category()
  
  stopn <- 1 #FIXX
  
  while (pagen <= stopn){
    indvidual_df_pg <- scrape_page(pagen, paste0(baseUrl, "forumdisplay.php?fid=", categoryId, "&page=", pagen))
    all_df_pg <<- bind_rows(all_df_pg, indvidual_df_pg)
    
    pagen <<- pagen + 1
  }
  
  # Save topic links
  save(all_df_pg,file=paste0("page-links-",categoryId,".Rda"))

}

start_scraping_posts <- function(){  
  
  while (topicidn <= stoppostn){
    print_debug(paste0("Scrape topic no: ", topicidn))
    indvidual_df_posts <- scrape_posts(topicidn, paste0(baseUrl, all_df_pg[topicidn,3]))
    all_df_posts <<- bind_rows(all_df_posts, indvidual_df_posts)
    
    topicidn <<- topicidn + 1
  }
  
  # Save topic links
  save(all_df_posts,file=paste0("topic-content-",categoryId,".Rda"))
  print_debug("DONE SCRAPING. Pls check for [ERROR] logs.")
  
}

init_scraper_posts <- function(start=start, stop=stop) {
  
  tryCatch({
    # Load remote driver
    remote_driver$open()
    remote_driver$navigate(baseUrl)
    
  }, error = function(e) {
    outfilePath <<- "C:\\Users\\dinhk_1phzc8w\\Desktop\\py\\LIHKGr-master\\LIHKGr-master\\R\\out"
    setwd(outfilePath)
    init_scraper(categoryId = 62, baseUrl="https://news.discuss.com.hk/", windows = FALSE)
    launch_browser()
  })
  
  
  filenm <- paste0(gsub("\\\\", "/", outfilePath), "/page-links-",categoryId,".Rda")
  
  tryCatch({
    # Load topic links
    print_debug(paste0("Loading ", filenm))
    load(file = filenm)
    
    all_df_pg <<- all_df_pg
    
    topicidn <<- start
    if (stop==-1) {stoppostn <<- nrow(all_df_pg)}
    else {stoppostn <<- stop}
    
  }, error = function(e) {
    print_error(e)
    print_error(paste0("Error in loading file: ", filenm, ". Make sure file exists by running step 3."))
  })
  


}



#debugSource('C:/Users/dinhk_1phzc8w/Desktop/py/LIHKGr-master/LIHKGr-master/R/scraper discuss hk.R', encoding = 'UTF-8')


################ Step 1: Set Up (Only do once) ################
# Set the working directory - create [out folder here]
outfilePath <<- "C:\\Users\\dinhk_1phzc8w\\Desktop\\py\\LIHKGr-master\\LIHKGr-master\\R\\out"
setwd(outfilePath)
# Remove all the data.. run only once for the entire set

init_scraper(categoryId = 62, baseUrl="https://news.discuss.com.hk/", windows = FALSE)
# categoryId = <category-id-of-forum>


################ Step 2: Launch Browser and Solve Captcha ################
launch_browser()
# If browser crashed, run this again


################ Step 3: Start Scraping pages links in forum ################
start_scraping_pg()
# If scraping stopped, run this again
# Only need to run this once
# This will save the results in page-links-XX.Rda for step 4 usage


################ Step 4: Start Scraping topic posts ################
init_scraper_posts(start = 5, stop = 6)
# init_scraper_posts(start=1,stop=-1) (Start from topic #1 to the end of category) ==> -1 default end

start_scraping_posts()
# If scraping stopped, run this again