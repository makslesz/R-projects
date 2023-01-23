sizeReport <- function(path, patt = ".*", dironly = FALSE, level = Inf) {
  files <- list.files(path = path, pattern = patt, full.names = TRUE, recursive = ! dironly, include.dirs = ! dironly)
  size <- lapply(files, file.size)
  df <- as.data.frame(cbind(files, size))
  colnames(df) <- c('path', 'size')
#  if (level > 1) {
#    files[ lengths(strsplit(files, "/")) < level * 2 + 1 ]
#  } else if (level == 0) {
#    df <- as.data.frame(cbind("../", Reduce(`+`, size)))
#    colnames(df) <- c('path', 'size')
#    return(df)
#  } else{
    return(df)
#  }
}


sizeReport(path = "~/Documents", dironly = TRUE)


files <- list.files(path = "../", full.names = TRUE, recursive = TRUE)
size <- lapply(files, file.size)

Reduce(`+`, size)

sizeReport(path = "../", level = 0)



list.files("~/Documents", recursive = TRUE, include.dirs = TRUE)



depthSearch <- function(path, patt = ".*", level = Inf) {
  files <- list.files(path, recursive = FALSE, full.names = TRUE)
  if (level > 1) {
    add <- depthSearch(files, level-1)
    c(files, add)
  } else {
    files
  }
}




