#' Need Update
#' @param node character, osf node
#' @param remote_path character, remote path to file
#' @param file character, name of file to download
#' @param path character, output path. Defaults to working directory
#' @details 
#' logic
#' !file exists - download
#' file exists & !matching hashes - download
#' @examples 
#' need_update(node = "p7ayb", file = "Analysis notes.txt", path = "data")

#' @importFrom glue glue
#' @importFrom fs file_exists
#' @importFrom digest digest
#' @importFrom osfr osf_retrieve_file osf_retrieve_node osf_ls_files
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @export

need_update <- function(node, remote_path = NULL, file, path  = "."){
  
  filepath <- file.path(path, file)
  
  #check if file is missing
  needs_updating <- if(!file_exists(filepath)){
    TRUE # file is missing
  } else {
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
    
    #get hash of local file
    hash <- digest(file = filepath)
    hash_new <- meta_file$meta[[1]]$attributes$extra$hashes$md5
    
    #Check hash matches
    !identical(hash, hash_new) # needs updating if not identical
  }
  else{
    FALSE
  }
  needs_updating
  }  
  

}


