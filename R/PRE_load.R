# Script permettant de charger la valeur initiale de PRE dans un objet de type PRE

setGeneric(name = "pre_load", def = function(file_PRE_address){standardGeneric("pre_load")})
setMethod(
    f = "pre_load",
    signature = "character",
    definition = function(file_PRE_address){
        temp <- read.csv2(file_PRE_address)
        if(temp[,"ryth_dot"] != round(temp[,"ryth_dot"])) {stop("[PRE : load] : L'input de rythme de dotation doit etre un entier. \n")}
        pre <- new("PRE", 
                   val_debut    = temp[,"pre_init"],
                   val_courante = temp[,"pre_init"],
                   ryth_dot     = as.integer(temp[,"ryth_dot"]))
        return(pre)
    }
)