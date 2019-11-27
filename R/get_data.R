#' get_data
#' @param file character, name of file to download
#' @param path character, output path. Defaults to working directory
#' @details 
#' logic
# !file exists - download
# file exists & !matching hashes - download

#' @importFrom glue glue
#' @importFrom fs file_exists
#' @importFrom digest digest
#' @export

get_file <- function(file, path  = "."){
  filepath <- file.path(path, file)

  #get osf id of file
  #magic
  
  #get new meta_data
  new_meta <- osf_retrieve_file("xxx")
  
    
  #check local file exists
  if(file_exists(filepath)){
    
    #get hash of local file
    hash <- digest(file = filepath)
    
    #if hash matches & !force
    if(identical(hash, newhash)){
      message(glue("{file} already up to date."))
      return()
    }
  }  
  #download
  osf_download(new_meta, path = pathfile)
  
}


