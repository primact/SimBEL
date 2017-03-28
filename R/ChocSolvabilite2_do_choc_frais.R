#----------------------------------------------------------------------------------------------------------------------------------------------------
#           do_choc_frais
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Permet a partir d'un canton initial de creer un canton choque frais.
##'
##' \code{do_choc_frais} est une methode permettant d'appliquer le choc frais de la formule standard Solvabilite 2
##'  a un canton.
##' @name do_choc_frais
##' @docType methods
##' @param x objet de la classe \code{\link{ChocSolvabilite2}}.
##' @param canton est un objet de la classe \code{\link{Canton}}. Il correspond au canton non choque (i.e. central)
##'  de l'assureur.
##' @param autres_passifs_choc est un objet de la classe \code{\link{AutresPassifs}}, il correspond au chargement
##'  des autres passifs choques.
##' Ces derniers ont ete renseignes par l'utilisateur en donnees.
##' @return \code{canton} l'objet  de la classe \code{\link{Canton}} correspondant au scenario choque frais
##'  au sens de la formule standard Solvabilite 2.
##' @note La parametrisation des chocs de frais est effectuee dans les fichiers d'inputs utilisateurs.
##' @author Prim'Act
##' @export
##' @include ChocSolvabilite2_class.R Canton_class.R AutresPassifs-class.R

setGeneric(name = "do_choc_frais", def = function(x, canton, autres_passifs_choc){standardGeneric("do_choc_frais")})
setMethod(
    f = "do_choc_frais",
    signature = c("ChocSolvabilite2", "Canton", "AutresPassifs"),
    definition = function(x, canton, autres_passifs_choc){

        # SOUS ENTENDU L OBJET Choc_Solvabilite2 contient un attribut param_choc_sousc qui est de la forme $mp$choc_frais_assiette
        tx_relatif_choc_frais <- as.numeric(x@param_choc_sousc@mp$choc_frais_assiette)
        ptf_passif <- canton@ptf_passif

        #prime
        ptf_passif["fp"]["mp"]["frais_fixe_prime"] <- ptf_passif["fp"]["mp"]["frais_fixe_prime"] * (1 + tx_relatif_choc_frais)
        ptf_passif["fp"]["mp"]["frais_var_prime"]  <- ptf_passif["fp"]["mp"]["frais_var_prime"]  * (1 + tx_relatif_choc_frais)
        #prest
        ptf_passif["fp"]["mp"]["frais_fixe_prest"] <- ptf_passif["fp"]["mp"]["frais_fixe_prest"] * (1 + tx_relatif_choc_frais)
        ptf_passif["fp"]["mp"]["frais_var_prest"]  <- ptf_passif["fp"]["mp"]["frais_var_prest"]  * (1 + tx_relatif_choc_frais)
        #enc
        ptf_passif["fp"]["mp"]["frais_fixe_enc"]   <- ptf_passif["fp"]["mp"]["frais_fixe_enc"]   * (1 + tx_relatif_choc_frais)
        ptf_passif["fp"]["mp"]["frais_var_enc"]    <- ptf_passif["fp"]["mp"]["frais_var_enc"]    * (1 + tx_relatif_choc_frais)

        # Chargement des autres passifs
        ptf_passif@autres_passifs <- autres_passifs_choc

        canton@ptf_passif <- ptf_passif

        return(canton)
    }
)
