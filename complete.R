complete <- function(directory, id = 1:332) {
  filenames = Sys.glob(file.path(directory, sprintf("*%03i.csv", id)))
  df_list <- lapply(seq_along(id), 
                    function(x, y, z) {
                      obs <- read.csv(file = y[[z]], header = T)
                      completed <- sum(complete.cases(obs))
                      data.frame(ID = id[z], nobs = completed)
                    }
                    , x = id, y = filenames)
  rt_df <- do.call(rbind, df_list)
  names(rt_df) <- c("ID", "nobs")
  return(rt_df)
}