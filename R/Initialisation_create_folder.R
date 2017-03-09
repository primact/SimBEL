# GK 06/03/2017
setGeneric(name = "init_create_folder", def = function(x){standardGeneric("init_create_folder")})
setMethod(
    f = "init_create_folder",
    signature = "Initialisation",
    definition = function(x){
        # On ecrase tous les dossiers sauf le init qui contient la photo originale
        # Si les dossiers existent, on les ecrasent et on les recreent
        # Sinon on les creent
        lapply(names(x@address[["save_folder"]][names(x@address[["save_folder"]]) != "init"]), 
               function(y){ 
                    path <- x@address[["save_folder"]][[y]]                           
                    if (dir.exists(path) == F) { dir.create(path) } else { unlink(paste(path, "/*", sep = "")) }})
        return(TRUE)
    }
    
)
     
      