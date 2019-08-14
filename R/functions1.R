#' Get the name of the latest version of a file or directory
#'
#' @param path path to files, defaults to wd
#' @param pattern Add the pattern that the file or directory should start with.
#'
#' @export
#' @return latest_file_name
#'
get_latest_file  = function(path = NA, pattern){

  if(is.na(path)){path = getwd()}

  #get a list of those files
  file_names = list.files(path, pattern = pattern)

  #check there are soem files
  if(length(file_names) == 0){
    stop("No files of that pattern in that directory")
  }

  file_dates = file.info(paste0(path, "/", file_names))$mtime

  ind = which.max(file_dates)

  #print(paste0(path, "/", file_names[ind]))

  return(paste0(path, "/", file_names[ind]))
}




#' Generate a sequel name
#'
#' @param x numeric eg. 3
#' @param n number of outputs
#'
#' @export
#' @return sequel with rhyming end
#'
generate_sequel = function(x, n){

  numberword = numbers2words(x)

  rhymeword = rhymer::get_rhyme(numberword, limit = 20)$word[sample(1:20, n)]

  otherword = unlist(strsplit(ids::adjective_animal(n), "_"))[c(TRUE, FALSE)]

  out = paste0(otherword, "_", rhymeword)

  out = gsub(" ","_",out)

  return(out)
}


#' Generate a sequel name
#'
#' @param x numeric eg. 3
#'
#' @export
#' @return number word
#'
numbers2words <- function(x){
  ## Function by John Fox found here:
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html

  helper <- function(x){

    digits <- rev(strsplit(as.character(x), "")[[1]])
    nDigits <- length(digits)
    if (nDigits == 1) as.vector(ones[digits])
    else if (nDigits == 2)
      if (x <= 19) as.vector(teens[digits[1]])
    else trim(paste(tens[digits[2]],
                    Recall(as.numeric(digits[1]))))
    else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred",
                                      Recall(makeNumber(digits[2:1]))))
    else {
      nSuffix <- ((nDigits + 2) %/% 3) - 1
      if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
      trim(paste(Recall(makeNumber(digits[
        nDigits:(3*nSuffix + 1)])),
        suffixes[nSuffix],
        Recall(makeNumber(digits[(3*nSuffix):1]))))
    }
  }
  trim <- function(text){
    gsub("^\ ", "", gsub("\ *$", "", text))
  }
  makeNumber <- function(...) as.numeric(paste(..., collapse=""))
  opts <- options(scipen=100)
  on.exit(options(opts))
  ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
            "eight", "nine")
  names(ones) <- 0:9
  teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
             "sixteen", " seventeen", "eighteen", "nineteen")
  names(teens) <- 0:9
  tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
            "ninety")
  names(tens) <- 2:9
  x <- round(x)
  suffixes <- c("thousand", "million", "billion", "trillion")
  if (length(x) > 1) return(sapply(x, helper))
  helper(x)
}

#----------------------------------------------#
#' check utf-8
#'
#' @param df data frame
#'
#' @return df with utf 8  cnversion
#' @export
check_utf8 = function(df){

  for(i in 1:ncol(df)){
    df[, i] =  stringi::stri_trans_general(str = df[, i], id = "Latin-ASCII")
  }


  #TEST
  for(i in 1:ncol(df)){
    xfun::write_utf8(df[,i], "temp")
  }

  file.remove("temp")

  return(df)
}
