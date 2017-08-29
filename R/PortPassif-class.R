#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Definition de la classe PortPassif
#----------------------------------------------------------------------------------------------------------------------------------------------------

##' La classe \code{PortPassif}.
##'
##' Une classe regroupant l'ensemble des donnees de passifs et les hypotheses correspondantes.
##'
##' @name PortPassif
##' @slot annee une valeur entiere correspondant a l'annee de projection.
##' @slot eei une liste d'objets de la classe \code{\link{EpEuroInd}} contenant l'ensemble
##' des produits de type epargne en euros.
##' @slot rer une liste d'objets de la classe \code{\link{RetraiteEuroRest}} contenant l'ensemble
##' des produits de retraite en phase de restitution.
##' @slot names_class_prod un vecteur \code{character} indiquant les noms de classes de produits.
##' @slot ht un objet de classe \code{\link{HypTech}} contenant les hypotheses techniques.
##' @slot fp un objet de classe \code{\link{FraisPassif}} contenant les hypotheses de frais de passif
##' par produit.
##' @slot tx_pb un objet de classe \code{\link{TauxPB}} contenant les taux contractuel de participation
##' aux benefices par produit.
##' @slot autres_passifs un objet de classe \code{\link{AutresPassifs}}.
##' @slot autres_reserves un objet de classe \code{\link{AutresReserves}}.
##' @slot calc_proba un \code{logical} indiquant si les probabilites ont ete calculees.
##' @docType class
##' @author Prim'Act
##' @seealso La projection des produits sur l'annee avant attributiuon de participation
##' aux benefices : \code{\link{proj_annee_av_pb}}.
##' Le vieillissement des model points de passifs avant et apres attributiuon de participation
##' aux benefices : \code{\link{viellissement_av_pb}}, \code{\link{vieillissment_ap_pb}}.
##' @keywords classes
##' @export
##' @include HypTech-class.R FraisPassif-class.R TauxPB-class.R AutresPassifs-class.R AutresReserves-class.R EpEuroInd-class.R
setClass(
    Class = "PortPassif",

    representation = representation(
        annee = "integer",
        eei = "list",
        rer = "list",
        names_class_prod = "character",
        ht = "HypTech",
        fp = "FraisPassif",
        tx_pb = "TauxPB",
        autres_passifs = "AutresPassifs",
        autres_reserves = "AutresReserves",
        calc_proba = "logical"),

    validity = function (object){

        # liste permettant de stocker les erreurs de chargement
        retval <- NULL

        # Nombre de produits
        nb_eei <- length(object@eei)
        nb_rer <- length(object@rer)

        # Tests de non absence de noms
        if(nb_eei != 0L & is.null(names(object@eei))) retval <- c(retval, "[PortPassif] : 'eei' doit etre une liste avec des noms")
        if(nb_rer != 0L & is.null(names(object@rer))) retval <- c(retval, "[PortPassif] : 'rer' doit etre une liste avec des noms")

        # Test si un nom n a pas ete oublie
        if(length(names(object@eei) != c("")) != nb_eei)
            retval <- c(retval, "[PortPassif] : tous les elements de 'eei' n'ont pas ete nommes")
        if(length(names(object@rer) != c("")) != nb_rer)
            retval <- c(retval, "[PortPassif] : tous les elements de 'rer' n'ont pas ete nommes")

        # Test sur le type des objets mis en parametre
        if(! validObject(object@ht)) retval <- c(retval, "[PortPassif] : Objet 'ht' non valide")
        if(! validObject(object@fp)) retval <- c(retval, "[PortPassif] : Objet 'fp' non valide")
        if(! validObject(object@tx_pb)) retval <- c(retval, "[PortPassif] : Objet 'tx_pb' non valide")
        if(! validObject(object@autres_passifs)) retval <- c(retval, "[PortPassif] : Objet 'autres_passifs' non valide")
        if(! validObject(object@autres_reserves)) retval <- c(retval, "[PortPassif] : Objet 'autres_reserves' non valide")
        if(length(object@annee) > 1L) retval <- c("[PortPassif] : 'annee' doit etre entier, pas de composante vectorielle autorisee.")
        if(! is.integer(object@annee)) retval <- c("[PortPassif] : 'annee' doit etre entier.")

        # Test que les elements des listes sont bien des objets du bon type
        if(nb_eei != 0L){
            for(i in 1L:nb_eei){
                if(! validObject(object@eei[[i]]))
                    retval <- c(retval, paste("[PortPassif] : l element", i, "de 'eei' est non valide \n"))
            }
        }
        if(nb_rer != 0L) {
            for(i in 1L:nb_rer){
                if(! validObject(object@rer[[i]]))
                    retval <- c(retval, paste("[PortPassif] : l element", i, "de 'rer' est non valide \n"))
            }
        }

        # Resultats du controle
        if (is.null(retval)){
            return (TRUE)
        }else{
            return (retval)
        }

    }
)
