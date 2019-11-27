#' @importFrom glue glue
#' @importFrom fs file_exists
#' @export

get_file <- function(
  file, path  = ".", force = FALSE,
  osf_metadata_file = ".osf_metadata.RDS"){
  
  #make osf_metadata path/file
  osf_metadata_file <- file.path(path, osf_metadata_file) 
  #read existing osf_metadata if any
  osf_metadata <- if(file_exists(osf_metadata_file)){
    readRDS(osf_metadata_file)
  } else {
    message(glue("File {osf_metadata_file} not found. Will attempt to make it."))
    NULL
  }

  #get osf id of file
  #magic
  
  #get new meta_data
  new_meta <- osf_retrieve_file(xxx)
  
    
  #check local file exists
  if(file_exists(file.path(data, file))){
    
  #check hash matches  
  #if hash matches & !force
  message(glue("{file} already up to date. \nUse force = TRUE to force new download"))
    
  #else
  }  

  #create metadata
  #write metadata
}

#logic
#!file exists - download
#file exists & !matching hashes - download
#file exists & matching hashes & force = TRUE - download
