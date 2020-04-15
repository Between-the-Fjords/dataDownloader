#' get_data
#' @param node character, osf node
#' @param remote_path character, remote path to file
#' @param file character, name of file to download
#' @param path character, output path. Defaults to working directory
#' @param conflicts What to do if a file already exists. Default is "overwrite" see \code{\link[osfr]{osf_download}} for other options.
#' @details 
#' logic
#' !file exists - download
#' file exists & !matching hashes - download
#' @examples 
#' get_file(node = "p7ayb", file = "Analysis notes.txt", path = "data")



#' @importFrom glue glue
#' @importFrom fs file_exists dir_exists dir_create
#' @importFrom digest digest
#' @importFrom osfr osf_download  osf_retrieve_file osf_retrieve_node osf_ls_files
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @export

get_file <- function(node, remote_path = NULL, file, path  = ".", conflicts = "overwrite"){
  
  #make path if required
  if(!dir_exists(path)){
    if(file_exists(path)){
      stop("Cannot create directory '{path}' as file with this name already exists.")
    }
    message(glue("Creating missing path '{path}'"))
    dir_create(path)
  }
  filepath <- file.path(path, file)

  #get osf id of file
  meta_node <- osf_retrieve_node(node) %>% 
    osf_ls_files(path = remote_path)
  
  #magic
  file_id <- filter(meta_node, .data$name == file) 
  
  #check file found
  if(nrow(file_id) == 0){
    stop(glue("'{file}' not found in node '{node}'. Check you are looking in the correct folder."))
  }
  
  #get new meta_data
  meta_file <- osf_retrieve_file(file_id$meta[[1]]$attributes$guid)
    
  #check local file exists
  if(file_exists(filepath)){
    
    #get hash of local file
    hash <- digest(file = filepath)
    hash_new <- meta_file$meta[[1]]$attributes$extra$hashes$md5
    
    #if hash matches
    if(identical(hash, hash_new)){
      message(glue("{file} already up to date."))
      return(invisible(filepath))
    }
  }  
  
  #download
  osf_download(meta_file, path = path, conflicts = conflicts)
  
  #check success
  message(glue("'{file}' downloaded succesfully"))
  
  return(invisible(filepath))
}


