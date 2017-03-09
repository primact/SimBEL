#--------------------------------------------------------------------------------------------------------------------
# Ce script comprend les declarateurs, constructeurs et verificateurs de la classe ESG
#--------------------------------------------------------------------------------------------------------------------
# Suivi version
# Version 1.0 du 24/01/2017. Fait par GK : initialisation
#--------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Declarateur
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' La classe ESG
##'
##' Classe pour les donnees globales d'actif
##'
##' @name ParamChocMket
##' @slot table_choc_action
##' @slot table_choc_immo
##' @slot table_choc_spread
##' @docType class
##' @section Lien a creer
##' @author Prim'Act
##' @seealso Mettre le lien vers les methodes de la classe
##' @keywords classes
##' @export
object <- 
setClass(
    Class = "ParamChocMket",
    representation = representation(
        table_choc_action  = "data.frame",
        table_choc_immo    = "data.frame",
        table_choc_spread  = "data.frame"),
    validity = function(object){
        retval <- NULL
        
        if(!is.data.frame(object@table_choc_action)) {retval <- c("[ParamChocMket] : Attribut table_choc_action non valide")}
        if(!is.data.frame(object@table_choc_immo))   {retval <- c("[ParamChocMket] : Attribut table_choc_immo non valide")}
        if(!is.data.frame(object@table_choc_spread)) {retval <- c("[ParamChocMket] : Attribut table_choc_spread non valide")}
        
        # Nom des data frame
        nom_action <- c("num_index", "choc_action")
        nom_immo   <- c("num_index", "choc_immo")
        nom_spread <- c("rating", "duration","param_A","param_B")
    
        if(sum(names(object@table_choc_action) != nom_action) > 0) {retval <- c("[ParamChocMket] : Le fichier d'input des parametres de choc action doit etre compose de deux colonnes")}
        if(sum(names(object@table_choc_immo)   != nom_immo)   > 0) {retval <- c("[ParamChocMket] : Le fichier d'input des parametres de choc immo doit etre compose de deux colonnes")}
        if(sum(names(object@table_choc_spread) != nom_spread) > 0) {retval <- c("[ParamChocMket] : Le fichier d'input des parametres de choc spread doit etre compose de deux colonnes")}
        
        if(!is.integer(object@table_choc_action$num_index))   {retval <- c("[ParamChocMket] : Attribut table_choc_action numeros d'indec non entiers")}
        if(!is.numeric(object@table_choc_action$choc_action)) {retval <- c("[ParamChocMket] : Attribut table_choc_action choc_action non reels")}
        if(!is.integer(object@table_choc_immo$num_index))     {retval <- c("[ParamChocMket] : Attribut table_choc_immo numeros d'index non entiers")}
        if(!is.numeric(object@table_choc_immo$choc_immo))     {retval <- c("[ParamChocMket] : Attribut table_choc_immo choc_immo non reels")}
        if(!is.integer(object@table_choc_spread$rating))      {retval <- c("[ParamChocMket] : Attribut table_choc_spreads rating non entier")}
        if(!is.character(object@table_choc_spread$duration))  {retval <- c("[ParamChocMket] : Attribut table_choc_spreads duration non character")}
        if(!is.numeric(object@table_choc_spread$param_A))     {retval <- c("[ParamChocMket] : Attribut table_choc_spreads param_A non character")}
        if(!is.numeric(object@table_choc_spread$param_B))     {retval <- c("[ParamChocMket] : Attribut table_choc_spreads param_B non character")}
        
        if (is.null(retval)) return (TRUE)
        else return (retval)
    }
)
