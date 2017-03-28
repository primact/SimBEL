SEUIL_DEL_IMMO <- 0.001

#----------------------------------------------------------------------------------------------------------------------------------------------------
#           sell_immo
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Mise a jour de chaque composante du portefeuille immobilier suite a la vente de tout ou partie de ce portefeuille.
##'
##' \code{sell_immo} est une methode permettant de mettre a jour chaque composante d'un portefeuille immobilier suite a la vente
##' de tout ou partie de ce portefeuille.
##' @name sell_immo
##' @docType methods
##' @param x objet de la classe \code{\link{Immo}} (decrivant le portefeuille immobilier en detention).
##' @param num_sold vecteur de type \code{numeric} contenant le numero de model point immobilier du portefeuille que l'on souhaite vendre.
##' @param nb_sold vecteur de type \code{numeric} contenant le nombre d'unite que l'on souhaite vendre (a autant de ligne que le vecteur num_sold).
##' @return \code{immo} l'objet \code{x} mis a jour de l'operation de vente (suppression des lignes vendues).
##' @return \code{pmvr} le montant des plus ou moins-values realisees.
##' @author Prim'Act
##' @export
##' @include Immo_class.R

setGeneric(name = "sell_immo", def = function(x, num_sold, nb_sold){standardGeneric("sell_immo")})
setMethod(
  f = "sell_immo",
  signature = c(x = "Immo", num_sold = "numeric", nb_sold = "numeric"),
  definition = function(x, num_sold, nb_sold){

    nom_table  <- names(x@ptf_immo)
    num_mp     <- which(nom_table == "num_mp")
    nb_unit    <- which(nom_table == "nb_unit")
    val_achat  <- which(nom_table == "val_achat")
    val_marche <- which(nom_table == "val_marche")
    val_nc     <- which(nom_table == "val_nc")
    cessible   <- which(nom_table == "cessible")
    presence   <- which(nom_table == "presence")

    # Verification des inputs
    if(length(num_sold)!=length(nb_sold)) {stop("[Immo : sell] : Les vecteurs num_sold et nb_sold doivent etre de même longueur.")}
    if(sum(num_sold %in% .subset2(x@ptf_immo, num_mp))!= length(num_sold)) {stop("[Immo : sell] : Tentative de vente d'une immo non existante \n")}

    pmvr_part <- 0
    pmvr_tot  <- 0

    # Selection des lignes soumises a une operation de vente
    temp <- x@ptf_immo[which(.subset2(x@ptf_immo, num_mp) == num_sold),]

    # Verification qu'on ne vende pas plus d'unite qu'on n'en possede ou qu on ne vende pas une ligne non cessible
    if(sum(nb_sold > .subset2(temp, nb_unit)) > 0)   {stop("[Immo : sell] : Vente d'immo entraine une position negative \n")}
    if(sum(.subset2(temp, cessible) == F) > 0)       {stop("[Immo : sell] : Tentative de vente d'une immo non cessible \n")}
    if(sum(.subset2(temp,presence) == F) > 0)       {stop("[Immo : sell] : Tentative de vente d'une immo non presente \n")}

    # Premier cas : vente partielle d'une ligne immo
    temp_part               <- new("Immo", temp[which(.subset2(temp, nb_unit) > nb_sold),])
    num_sold_part           <- .subset2(temp_part@ptf_immo, num_mp)
    nb_sold_part            <- nb_sold[which(num_sold %in% num_sold_part)]
    # Operations
    if(length(num_sold_part)>0){
      pmvr_part <-   sum((.subset2(temp_part@ptf_immo, val_marche) - .subset2(temp_part@ptf_immo, val_nc))  * nb_sold_part / .subset2(temp_part@ptf_immo, nb_unit))
      temp_part@ptf_immo$val_achat  <- .subset2(temp_part@ptf_immo, val_achat)  * (1 - nb_sold_part / .subset2(temp_part@ptf_immo, nb_unit))
      temp_part@ptf_immo$val_marche <- .subset2(temp_part@ptf_immo, val_marche) * (1 - nb_sold_part / .subset2(temp_part@ptf_immo, nb_unit))
      temp_part@ptf_immo$val_nc     <- .subset2(temp_part@ptf_immo, val_nc)     * (1 - nb_sold_part / .subset2(temp_part@ptf_immo, nb_unit))
      temp_part@ptf_immo$nb_unit    <- .subset2(temp_part@ptf_immo, nb_unit) - nb_sold_part
    }

    # Deuxieme cas : vente totale d'une ligne immo
    temp_tot     <- new("Immo", temp[which(.subset2(temp, nb_unit) == nb_sold),])
    num_sold_tot <- .subset2(temp_tot@ptf_immo, num_mp)

    if(length(num_sold_tot) > 0) {
      pmvr_tot     <- sum(.subset2(temp_tot@ptf_immo, val_marche) - .subset2(temp_tot@ptf_immo, val_nc))}
    # Mise a jour de l'objet immo
    # 1 : Suppression des lignes immo integralement vendues
    if(length(num_sold_tot) > 0) {
      x@ptf_immo <- x@ptf_immo[-which(.subset2(x@ptf_immo,num_mp) %in% num_sold_tot),]}
    # 2 : Ajustement des lignes immo partiellement vendues
    if(length(num_sold_part) > 0) {
      x@ptf_immo[which(.subset2(x@ptf_immo, num_mp) %in% num_sold_part),] <- temp_part@ptf_immo}
    # 3 : Prevention en cas de portefeuille vide
    if(nrow(x@ptf_immo) == 0) {warning(" [Immo : sell] : Attention : portefeuille immo vide")}

    # 4 : vider les immo dont la valeur nette comptable est inferieure a 0.001
    x@ptf_immo <- x@ptf_immo[which(.subset2(x@ptf_immo, val_nc) > SEUIL_DEL_IMMO),]

    pmvr <- pmvr_part + pmvr_tot
    return(list(immo = x,pmvr = pmvr))
  }
)
