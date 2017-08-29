#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe DataBase
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' La classe \code{DataBase}.
##'
##' Une classe permettant de stocker les donnes d'output de la fonction \code{\link{proj_an}}.
##'
##' @name DataBase
##' @slot database une base de donnees \code{SQLite}
##' @slot ecriture_base un \code{logical} qui indique si les output doivent etre ecrits dans la base \code{SQLite}.
##' @docType class
##' @author Prim'Act
##' @keywords classes
##' @export

setClass(
    Class = "DataBase",

    representation = representation(
        database = "SQLiteConnection",
        ecriture_base = "logical"),

    validity = function (object){}
)
