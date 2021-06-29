library(RSelenium)
library(raster)
library(magrittr)
library(rvest)
library(stringi)
library(stringr)
library(jsonlite)
library(dplyr)

init_scraper <- function(startn, stopn, windows = FALSE){
  Sys.setenv(TZ = "Asia/Hong_Kong") # Setting Time Zone to HK
  if(windows){
    Sys.setlocale("LC_CTYPE", locale="Chinese (Traditional)")
  }
  nostep <<- startn
  stopn <<- stopn
  all_df <<- c()
  postid <<- c()
  failed <<- c()
}

launch_browser <- function(browser="chrome", chromever = "81.0.4044.69", port = sample(1000:60000, 1)){
  driver <<- rsDriver(browser=browser, chromever = chromever, port = port, phantomver = NULL)
  remote_driver <<- driver[["client"]]
  remote_driver$navigate("https://lihkg.com")
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

content_raw_formatter <- function(content){
  tst <- gsub("<div class=\"_2cNsJna0_hV8tdMj3X6_gJ\" data-ast-root=\"true\">","",toString(content))
  tst <- gsub("<div class=\"_3pnUeDm8-bRxLH84eNhuV_ _2a77EyqZUCKZHeOe3NmCFV\">","",tst)
  tst <- gsub("<div class=\"oStuGCWyiP2IrBDndu1cY\">","",tst)
  tst <- gsub("style=\"padding: 0px 1px;\"","",tst)
  tst <- gsub("<br>","",tst)
  tst <- gsub("</br>","",tst)
  tst <- gsub("</div>","",tst)
  
  tst <- gsub("<blockquote class=\"_31B9lsqlMMdzv-FSYUkXeV\">","<QUOTATION>",tst)
  tst <- gsub("</blockquote>","</QUOTATION>",tst)
  
  return(tst)
}


scrape_page <- function(html){
  post_no <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node("div div small ._3SqN3KZ8m8vCsD9FNcxcki") %>%
    html_text()

  post_date <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node("div div small .Ahi80YgykKo22njTSCzs_") %>%
    html_attr("data-tip")

  user_name <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node("div div small .ZZtOrmcIRcvdpnW09DzFk a") %>%
    html_attr('href')

  post_content_element <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node("div div .GAagiRXJU88Nul1M7Ai0H ._2cNsJna0_hV8tdMj3X6_gJ")
  
  post_content_element_2 <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
     html_node("div div ._1d3Z5jQRq3WnuIm0hnMh0c .oStuGCWyiP2IrBDndu1cY")
  
  if(length(post_content_element)!=length(post_content_element_2)) { stop("Error post_content_element_1: ",post_content_element," != post_content_element_2: ",post_content_element_2)}
  
  post_content_raw <- vector(mode = "list", length = length(post_no))
  for (j in 1:length(post_no)) {
    if(is.na(post_content_element[j])) {
      if(!is.na(post_content_element_2[j])) {post_content_element[j] <- post_content_element_2[j] }
      else { print("!!!!! WARNING - empty content, username: ", user_name[j],", post no:", post_no[j])}
    }
    
    post_content_raw[j] <- content_raw_formatter(post_content_element[j])
  }
  
  post_content <- post_content_element %>% html_text()
  
  post_obj <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node(".GAagiRXJU88Nul1M7Ai0H div")
  imgs_list <- vector(mode="character", length=length(post_obj))
  
  for (j in 1:length(post_obj)) {
    item <- post_obj[j] %>% html_children() %>% html_attr("src")
    if (length(item) == 0) {imgs_list[j] = NA} else {
      item_extr <- item[!is.na(item)]
      if (length(item_extr) == 0) {imgs_list[j] <- str_c(post_obj[j] %>% html_nodes("img")  %>% html_attr("src"),collapse=' \n ')} else  {imgs_list[j] <- str_c(item_extr,collapse=' \n ')}
    }
  }
  
  embedded_urls_list <- stri_extract_all(post_content,regex="http.*")
  post_urls <- post_content_element %>% html_node("a")  %>% html_attr("href")
  
  post_likes <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node("._1jvTHwVJobs9nsM0JDYqKB+ ._1drI9FJC8tyquOpz5QRaqf") %>% html_text()

  post_dislikes <- html %>% html_nodes("._36ZEkSvpdj_igmog0nluzh") %>%
    html_node("._2_VFV1QOZok8YhOTGa_3h9+ ._1drI9FJC8tyquOpz5QRaqf") %>% html_text()

  ts <- Sys.time()

  outdf <- as.data.frame(cbind(post_no, user_name, post_date, post_content, post_content_raw, post_likes, post_dislikes, post_urls))
  
  outdf$post_imgs <- imgs_list
  outdf$post_embedded_urls <- embedded_urls_list
  outdf$ts <- ts
  return(outdf)
}

scrape_thread <- function(html,postid,posts){
  outdf <- c()
  
  thread_title <- (html %>% html_nodes("._2k_IfadJWjcLJlSKkz_R2- span") %>% html_text())[2]
  
  thread_likes <- (html %>% html_nodes("._1gUiNP0DXim6f5CvBTx7T7 ._8_NT40G-QNQzcSSTrRXAD") %>%html_attr('data-score'))[1]
  thread_dislikes <- (html %>% html_nodes("._1gUiNP0DXim6f5CvBTx7T7 ._8_NT40G-QNQzcSSTrRXAD") %>%html_attr('data-score'))[2]
  
  outdf$postid <- postid # This bit might fail if date etc is NULL
  outdf$thread_title <- thread_title
  outdf$thread_likes <- thread_likes
  outdf$thread_dislikes <- thread_dislikes
  outdf$posts <- list(tibble(posts))
  return(outdf)
}

scrape_data <- function(postid){
  thread_df.posts <- c()
  for(counter in 1:999){
    attempt <- 1
    notdone <- TRUE
    nextpage <- FALSE
    first_flag <- TRUE
    
    while( notdone && attempt <= 4 ) { # Auto restart when fails
      print(paste0("Attempt: ", attempt))
      attempt <- attempt + 1
      try({
        html <- crack_it(paste0("https://lihkg.com/thread/", postid, "/page/", counter))
        print(paste0("xxxxxxxx https://lihkg.com/thread/", postid, "/page/", counter))
        next.page <- html %>% html_node("._3omJTNzI7U7MErH1Cfr3gE a") %>% html_text()
        #next.page <- html %>% html_node("._3omJTNzI7U7MErH1Cfr3gE+ ._3omJTNzI7U7MErH1Cfr3gE a") %>% html_text()
        titlewords <- html %>% html_nodes("._2k_IfadJWjcLJlSKkz_R2- span") %>% html_text() %>% length()
        if (is.na(next.page)){
          print(paste0("page ", counter, " (last page)"))
          post <- scrape_page(html)
          thread_df.posts <- rbind(thread_df.posts, post)
          notdone <- FALSE
        } elif (titlewords == 1){
          notdone <- FALSE
          thread_df.posts <- data.frame(number = "ERROR", date = "ERROR", uid = "ERROR", text = "ERROR", upvote = "ERROR", downvote = "ERROR", postid = postid, title = "Deleted Post", board = "ERROR", collection_time = Sys.time())
          print("Empty Post, Skipping")
        } else {
          print(paste0("page ", counter, " (to be continued)"))
          post <- scrape_page(html)
          thread_df.posts <- rbind(thread_df.posts, post)
          nextpage <- TRUE
          notdone <- FALSE
        }
        if (first_flag) {
          thread_df.main <- scrape_thread(html, postid, thread_df.posts)
          first_flag <- FALSE
        }
        lay_low()
      })
    } # End of While Loop
    if( notdone && attempt > 2 ){
      if (titlewords == 2 && nrow(thread_df.posts) > 1){
        warning <- data.frame(number = "EMPTY LAST PAGE", date = "EMPTY LAST PAGE", uid = "ERROR", text = "ERROR", upvote = "ERROR", downvote = "ERROR", postid = postid, title = "Deleted Last Page", board = "ERROR", collection_time = Sys.time())
        print("Empty Last Page Detected. Ending scraping..")
        
        notdone <- FALSE
      } else {
        # Error page, Stopping
        thread_df.main <- c()
        break
      }
    }
    if(nextpage){
      next
    }else if(!notdone){
      # Parse individual post JSON + Rda
      #save(thread_df.posts,file=paste0("thread-content-",postid,".Rda"))
      
      #thread_df.posts <- toJSON(thread_df.posts, dataframe = "rows")
      #con <- file(paste0("thread-content-",postid,".json"), open = "w+", encoding = "native.enc")
      #writeLines(thread_df.posts, con = con, useBytes = TRUE)
      #close(con)
      
      # Parse main thread info Rda
      save(thread_df.main,file=paste0("thread-info-",postid,".Rda"))
      
      break
    }
    
  } # End of For Loop
  return(thread_df.main)
}

lay_low <- function(){
  Sys.sleep(sample(seq(1, 2, by=0.001), 1))
}

start_scraping <- function(){
  for (i in nostep:stopn){
    print(paste0("Scrape post: ", i))
    postid <<- i
    indvidual_df <- scrape_data(i)
    all_df <<- bind_rows(all_df, indvidual_df)
    
    print(paste0("Total posts: ", nrow(all_df)))
    print(paste0("Finished scraping thread id: ", i))
  }
  nostep <<- i-1
  save(all_df,file=paste0("all-threads-",stopn,".Rda"))
  print(paste0("DONE saving Rda .. Results file: all-threads-",stopn,".Rda at ",getwd()))
  
  out_json <- toJSON(all_df, dataframe = "rows")
  con <- file(paste0("all-threads-",stopn,".json"), open = "w+", encoding = "native.enc")
  writeLines(out_json, con = con, useBytes = TRUE)
  close(con)
  
  print(paste0("DONE saving JSON .. Results file: all-threads-",stopn,".json at ",getwd()))
  
}

#source('C:/Users/dinhk_1phzc8w/Desktop/py/LIHKGr-master/LIHKGr-master/R/scraper discuss hk.R', encoding = 'native.enc')

################ Step 1: Set Up (Only do once) ################
# Set the working directory - create [out folder here]
setwd("C:\\Users\\dinhk_1phzc8w\\Desktop\\py\\LIHKGr-master\\LIHKGr-master\\R\\out")
# Remove all the data.. run only once for the entire set
# startn = <first thread-id to scrape>, stopn =  <last thread-id to scrape>
init_scraper(startn = 401, stopn = 500, windows = FALSE)

################ Step 2: Launch Browser and Solve Captcha ################
launch_browser()
# If browser crashed, run this again

################ Step 3: Start Scraping ################
start_scraping()
# If scraping stopped, run this again

