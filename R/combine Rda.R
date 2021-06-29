
join_Rda <- function(originalDir, outputDir){
  setwd(originalDir)
  folderList <- list.files(getwd())
  all_df <<- c()

  for (i in 1:length(folderList)) {
    if (!grepl(".Rda",folderList[i])) {
      setwd(normalizePath(paste0("./",folderList[i])))
      rDaList <- list.files(getwd())
      
      # print(rDaList)
      
      
      for (i in 1:length(rDaList)) {
        # Load Rda
        load(list.files(getwd())[i])
        
        indvidual_df <- thread_df.main
        all_df <<- bind_rows(all_df, indvidual_df)
      }
      
      setwd(originalDir)
    } else {
      print(paste0("!!!!!!Please put ",folderList[i], " into thread folder!!!!!!!" ))
    }
  }

  # Save joined Rda
  setwd(outputDir)
  save(all_df,file=paste0("all-threads-",folderList[1],".Rda"))
  print(paste0("DONE saving Rda .. Results file: all-threads-",folderList[1],".Rda at ",getwd()))
  
  return(all_df)
}

convert_Rda_toJson <- function(inRda, outputDir) {
  
  setwd(outputDir)
  
  out_json <- toJSON(inRda, dataframe = "rows")
  con <- file(paste0(gsub(".Rda", "",list.files(getwd())[1]),".json"), open = "w+", encoding = "native.enc")
  writeLines(out_json, con = con, useBytes = TRUE)
  close(con)
  
  print(paste0("DONE saving JSON .. Results file: all-threads-",stopn,".json at ",getwd()))
}


################ Step 1: Set Up (Only do once) ################
# Set the working directory - create [out folder here]
inputDir <- "C:\\Users\\dinhk_1phzc8w\\Desktop\\py\\LIHKGr-master\\LIHKGr-master\\R\\out"
outputDir <- "C:\\Users\\dinhk_1phzc8w\\Desktop\\py\\LIHKGr-master\\LIHKGr-master\\R\\joined"


################ Step 2: Join small Rda files ################
joinedRda <- join_Rda(inputDir, outputDir)
# join RdA files


################ Step 3: Convert to Rda to JSON ################
convert_Rda_toJson(joinedRda, outputDir)
# convert to JSON