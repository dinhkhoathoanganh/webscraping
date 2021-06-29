library(readxl)

setwd("C:\\Users\\dinhk_1phzc8w\\Documents")


ARISE_Fellowsn <- read_excel("ARISE Fellowsnew.xlsx")


photo_end_ntu <- "http://research.ntu.edu.sg/expertise/AcademicProfile/Pages/StaffPhoto.aspx?ST_EMAILID="
photo_end_lkc <- "http://www.lkcmedicine.ntu.edu.sg/aboutus/Faculty-and-Staff/PublishingImages/"


text_templ_1 <- "
<tr style=\"font-size: 12px;\">
<th rowspan=\"1\" colspan=\"1\" style=\"width: 130px; height: 120px; text-align: center;\">

<img src=\""

# #/aboutus/People/Documents/Fabian%20Lim.jpg\
text_templ_1b <- "\" alt=\"\" style=\"width: 110px; height: auto;\"/><br/><p class=\"MsoNormal\" style=\"line-height: 1.5; font-size: 12px;\">
<span>"

# # Since 2017
text_templ_2 <- "</span></p></th>

<th rowspan=\"1\" colspan=\"1\" style=\"height: 120px; text-align: left; font-size: 12px;\"><br/><br/>
<p style=\"line-height: normal; font-size: 14px; font-weight: bold;\"><span>

<a href=\""

# # http://www.lkcmedicine.ntu.edu.sg/aboutus/Faculty-and-Staff/Pages/Fabian-Lim.aspx
text_templ_3 <- "\">"

# # Associate Professor Fabian Lim
text_templ_4 <- "</a></span></p><span>

</span>
<p class=\"MsoNormal\" style=\"line-height: normal; font-size: 12px;\">
<span>"

# # BSc, MSc, MBA, PhD<br/>
#Assistant Dean, Research<br/>
#Associate Professor of Exercise Physiology<br/>
#Programme Director for Graduate Diploma in Sports Medicine
text_templ_5 <- "<br/>
Email: <a href=\"mailto:"

# # fabianlim@ntu.edu.sg

text_templ_6 <- "\">"
# #fabianlim@ntu.edu.sg
text_templ_7 <- "</a>
<br/></span></p>
</th></tr>


"

finalText <- ""

for (row in 1:29) { # nrow(ARISE_Fellowsn)
  yearContent <- ARISE_Fellowsn[row, "year"]
  textContent  <- ARISE_Fellowsn[row, "text"]
  linkContent  <- ARISE_Fellowsn[row, "link"]
  
  componentsText <- strsplit(paste0(ARISE_Fellowsn[row, "text"]), "\r\n\r\n")[[1]]
  
  if (length(componentsText)!=3) {print(paste0("ERROR - row ", row))}

  # Email source
  emailText <- strsplit(componentsText[3], ":")
  
  if (length(emailText[[1]])!= 2) {print(paste0("ERROR - email row ", row))} else {emailText <- emailText[[1]][2]}
  
  photoLink <- "/aboutus/People/Documents/Fabian%20Lim.jpg"
  
  # Photo source
  if (grepl("research.ntu.edu.sg", linkContent, fixed = TRUE)) {
    photoLink <- strsplit(paste0(linkContent), "=")
    if (length(photoLink[[1]])!= 2) {print(paste0("ERROR - photo row ", row))} 
    else {photoLink <- paste0(photo_end_ntu, photoLink[[1]][2])}
  } 
  if (grepl("lkcmedicine.ntu.edu.sg", linkContent, fixed = TRUE)) {
    photoLink <- strsplit(paste0(linkContent), "/")
    
    if (!grepl(".aspx", photoLink[[1]][length(photoLink[[1]])], fixed = TRUE)) {print(paste0("ERROR - photo row ", row))} 
    else {photoLink <- paste0(photo_end_lkc, gsub("-", "%20", gsub(".aspx", ".jpg", paste0(photoLink[[1]][length(photoLink[[1]])])))) }
  
    
  }
  
  finalText <- paste0(finalText, text_templ_1, photoLink ,text_templ_1b, yearContent, text_templ_2, linkContent, text_templ_3, 
                      componentsText[1], text_templ_4, gsub("\r\n","<br/>", componentsText[2]),text_templ_5, 
                      emailText, text_templ_6, emailText, text_templ_7)
  
  print(row)
  print(photoLink)
  
#  finalText <- paste0(finalText, text_templ_1, yearContent, text_templ_2, linkContent, text_templ_3, 
#                      componentsText[1], text_templ_4, gsub("\r\n","<br/>", componentsText[2]),text_templ_5, 
#                      text_templ_7)
  
}


fileConn<-file("output6.txt")
writeLines(c(finalText), fileConn)
close(fileConn)