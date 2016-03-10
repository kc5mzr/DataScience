corr <- function(directory, threshold = 0) {
  filenames <- list.files(directory, full.names = TRUE)
 
   df_list <- lapply(seq_along(filenames), 
                    function(x, z) {
                      read.csv(file = x[z], header = T, stringsAsFactors = FALSE)
                      }, x=filenames)
  rt_df <- do.call(rbind, df_list)
  completed_list <- lapply(seq_along(df_list), 
                    function(x, z) {
                      completed <- sum(complete.cases(df_list[[z]]$sulfate,df_list[[z]]$nitrate))
                      data.frame(ID = z, nobs = completed)
                    }, x=df_list)
  completed_df <- do.call(rbind,completed_list)
  names(completed_df) <- c("ID", "nobs")
  over <- completed_df$nobs>threshold
  over_df <- rt_df[rt_df$ID %in% completed_df$ID[over],]
  over_df <- over_df[complete.cases(over_df$sulfate,over_df$nitrate),]
  corr_results <- numeric()
  j <- 0
  for (i in which(over)) {
    j <- j + 1;
    corr_results[j] <- round(cor(over_df$sulfate[over_df$ID==i],over_df$nitrate[over_df$ID==i]),5)
  } 
  return(corr_results)
  }
  