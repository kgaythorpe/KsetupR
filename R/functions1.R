#' Get the name of the latest version of a file or directory
#'
#' @param pattern Add the pattern that the file or directory should start with.
#'                If the file is in another directory this should also have the file path
#'
#' @export
#' @return latest_file_name
#'
get_latest_file  = function(pattern){


  #get a list of those files
  file_names = list.files(pattern = pattern)

  #check there are soem files
  if(length(file_names) == 0){
    stop("No files of that pattern in that directory")
  }

  file_dates = file.info(file_names)$mtime

  ind = which.max(file_dates)

  print(file_names[ind])

  return(file_names[ind])
}
