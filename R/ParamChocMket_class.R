#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe ParamChocMket
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe \code{ParamChocMket}.
##'
##' Une classe contenant les parametres des chocs de marche de la formule standard.
##' @name ParamChocMket
##' @slot table_choc_action_type1 un \code{data.frame} contenant les parametres du choc action type 1.
##' @slot table_choc_action_type2 un \code{data.frame} contenant les parametres du choc action type 2.
##' @slot table_choc_immo un \code{data.frame} contenant les parametres du choc immobilier.
##' @slot table_choc_spread un \code{data.frame} contenant les parametres du choc de spread.
##' @docType class
##' @author Prim'Act
##' @keywords classes
##' @export
setClass(
    Class = "ParamChocMket",
    representation = representation(
        table_choc_action_type1  = "data.frame",
        table_choc_action_type2  = "data.frame",
        table_choc_immo          = "data.frame",
        table_choc_spread        = "data.frame"),
    validity = function(object){
        retval <- NULL

        # Extraction des tables
        table_choc_action_type1 <- object@table_choc_action_type1
        table_choc_action_type2 <- object@table_choc_action_type2
        table_choc_immo         <- object@table_choc_immo
        table_choc_spread       <- object@table_choc_spread
        
        # Tests sur les classes
        if(!is.data.frame(table_choc_action_type1)) retval <- c("[ParamChocMket] : Attribut table_choc_action_type1 non valide")
        if(!is.data.frame(table_choc_action_type2)) retval <- c("[ParamChocMket] : Attribut table_choc_action_type2 non valide")
        if(!is.data.frame(table_choc_immo))   retval <- c("[ParamChocMket] : Attribut table_choc_immo non valide")
        if(!is.data.frame(table_choc_spread)) retval <- c("[ParamChocMket] : Attribut table_choc_spread non valide")

        # Nom des data frame
        nom_action <- c("num_index", "choc_action")
        nom_immo   <- c("num_index", "choc_immo")
        nom_spread <- c("rating", "duration","param_A","param_B")

        # Tests sur les noms
        if(sum(names(table_choc_action_type1) != nom_action) > 0) retval <- c("[ParamChocMket] : Le fichier d'input des parametres de choc action doit etre compose de deux colonnes")
        if(sum(names(table_choc_action_type2) != nom_action) > 0) retval <- c("[ParamChocMket] : Le fichier d'input des parametres de choc action doit etre compose de deux colonnes")
        if(sum(names(table_choc_immo)   != nom_immo)   > 0) retval <- c("[ParamChocMket] : Le fichier d'input des parametres de choc immo doit etre compose de deux colonnes")
        if(sum(names(table_choc_spread) != nom_spread) > 0) retval <- c("[ParamChocMket] : Le fichier d'input des parametres de choc spread doit etre compose de deux colonnes")

        # Tests sur les types
        if(!is.integer(table_choc_action_type1$num_index))   retval <- c("[ParamChocMket] : Attribut table_choc_action_type1 numeros d'indec non entiers")
        if(!is.numeric(table_choc_action_type1$choc_action)) retval <- c("[ParamChocMket] : Attribut table_choc_action_type1 choc_action non reels")
        if(!is.integer(table_choc_action_type2$num_index))   retval <- c("[ParamChocMket] : Attribut table_choc_action_type2 numeros d'indec non entiers")
        if(!is.numeric(table_choc_action_type2$choc_action)) retval <- c("[ParamChocMket] : Attribut table_choc_action_type2 choc_action non reels")
        if(!is.integer(table_choc_immo$num_index))     retval <- c("[ParamChocMket] : Attribut table_choc_immo numeros d'index non entiers")
        if(!is.numeric(table_choc_immo$choc_immo))     retval <- c("[ParamChocMket] : Attribut table_choc_immo choc_immo non reels")
        if(!is.integer(table_choc_spread$rating))      retval <- c("[ParamChocMket] : Attribut table_choc_spreads rating non entier")
        if(!is.character(table_choc_spread$duration))  retval <- c("[ParamChocMket] : Attribut table_choc_spreads duration non character")
        if(!is.numeric(table_choc_spread$param_A))     retval <- c("[ParamChocMket] : Attribut table_choc_spreads param_A non character")
        if(!is.numeric(table_choc_spread$param_B))     retval <- c("[ParamChocMket] : Attribut table_choc_spreads param_B non character")

        if (is.null(retval)) return (TRUE)
        else return (retval)
    }
)
