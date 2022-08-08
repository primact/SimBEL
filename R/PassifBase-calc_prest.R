
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des flux de prestations d un model point
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les flux de prestations pour des contrats epargne en euros ou retraite euros en phases de restitution.
##'
##' \code{calc_prest} est une methode permettant de calculer les flux de prestations,
##' les chargements sur encours relatifs a ces prestations et les nombres de sorties sur une periode.
##' @name calc_prest
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} ou de la classe \code{\link{RetraiteEuroRest}} contenant les model points epargne euros,
##' ou les model points retraite euro en phase de restitution.
##' @param an une valeur \code{integer} correspondant a l'annee du calcul des prestations.
##' @param method un \code{character} prenant pour valeur \code{normal} pour le calcul des flux avec application de la revalorisation au titre de
##' la participation aux benefices, et la valeur \code{gar} pour le calcul avec uniquement les flux garanti (calcul de la FDB).
##' @param y une liste a remplir uniquement si \code{x} est de type \code{\link{EpEuroInd}} contenant les parametres :
##' \describe{
##' \item{\code{proba_dyn} : }{une liste contenant le taux de rachats dynamiques (totaux et partiels) par model points.}
##' \item{\code{tx_min} : }{une liste contenant le taux de revalorisation minimum associes a chaque ligne de model points.
##' Le format de cette liste correspond a la sortie de la methode \code{\link{calc_tx_min}}.}
##' \item{\code{tx_soc} : }{est une valeur \code{numeric} correspondant au taux de prelevements sociaux.}
##' \item{\code{choc_lapse_mass} : }{est une valeur \code{numeric} correspondant au choc de rachat massif.}
##' }
##' @details En epargne, cette methode permet de calculer les flux de sortie en echeance, les flux de rachat totaux et partiels et
##' les flux de deces d'un contrat epargne en euros. Ces prestations font l'objet d'une relavorisation
##' au taux minimum contractuel. Les nombres de sortie sont egalement produits. Il est possible de realiser un choc de rachat
##' massif si \code{an}. Dans ce cas, les prestations de rachats massifs sortent en debut d'annee et ne sont pas revalorisees.
##' Des chargements sont appliques sur flux de rachats. Des prelevements sur encours sont appliques sur les
##' prestations revalorises au taux minimum contractuel. Cette methode permet de gerer les contrats a taux de
##' revalorisation net negatif.
##' Pour un contrat de retraite, elle permet de sortir les flux de rente et
##'  les nombres de sorties.
##' @return Une liste contenant :
##' \describe{
##' \item{\code{method} : }{la valeur de l'argument \code{method}}
##' \item{\code{flux} : }{une liste comprenant les flux de l'annee}
##' \item{\code{stock} : }{une liste comprenant les nombres de sorties}
##' }
##' @return Le format de la liste \code{flux} est :
##' \describe{
##' \item{\code{ech} : }{un vecteur contenant les flux de sortie en echeance de l'annee : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{rach_tot} : }{un vecteur contenant les flux de rachat totaux de l'annee : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{dc} : }{un vecteur contenant les flux de deces de l'annee : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{rach_part} : }{un vecteur contenant les flux de rachat partiel de l'annee : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{rente} : }{le flux annuel de rente par model point : nul si l'objet est de type \code{\link{EpEuroInd}}.}
##' \item{\code{prest} : }{un vecteur contenant les flux prestations de l'annee (renseigne que l'objet x soit de type \code{\link{RetraiteEuroRest}} ou \code{\link{EpEuroInd}}).}
##' \item{\code{rev_ech} : }{un vecteur contenant la revalorisation des echeances de l'annee : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{rev_rach_tot} : }{un vecteur contenant la revalorisation des rachats totaux de l'annee : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{rev_dc} : }{un vecteur contenant la revalorisation des deces de l'annee : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{rev_rach_part} : }{un vecteur contenant la revalorisation des rachats partiels de l'annee : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{rev_prest} : }{un vecteur contenant la revalorisation brute des prestations de l'annee : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{rev_prest_nette} : }{un vecteur contenant la revalorisation des prestations nette de l'annee : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{enc_charg} : }{un vecteur contenant les chargements sur l'encours de l'annee : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{rach_charg} : }{un vecteur contenant les chargements sur les rachats de l'annee : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{soc_prest} : }{un vecteur contenant les prelevements sociaux sur prestations de l'annee : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{it_tech_prest} : }{un vecteur contenant les interets techniques sur prestations de l'annee. : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{arr_charg} : }{un vecteur contenant les chargements sur arrerages. : nul si l'objet est de type \code{\link{EpEuroInd}}.}
##' }
##' @return Le format de la liste \code{stock} est :
##' \describe{
##' \item{\code{nb_ech : }}{un vecteur contenant le nombre de sorties en echeance de l'annee : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{nb_rach_tot : }}{un vecteur contenant le nombre de rachats totaux de l'annee : nul si l'objet est de type \code{\link{RetraiteEuroRest}}.}
##' \item{\code{nb_dc : }}{un vecteur contenant le nombre de deces de l'annee}
##' \item{\code{nb_sortie : }}{un vecteur contenant le nombre de sorties de l'annee}
##' \item{\code{nb_contr_fin : }}{un vecteur contenant le nombre de contrats en cours en fin d'annee}
##' \item{\code{nb_contr_moy : }}{un vecteur contenant la moyenne du nombre de contrats sur l'annee.}
##' }
##' @author Prim'Act
##' @seealso \code{\link{calc_proba_flux}}, \code{\link{calc_tx_min}}.
##' @export
##' @include EpEuroInd-class.R RetraiteEuroRest_class.R
##'

#--------------------------------------------------------
setGeneric(name = "calc_prest", def = function(x, method, an, y){standardGeneric("calc_prest")})


#--------------------------------------------------------
setMethod(
    f = "calc_prest",
    signature = c(x = "EpEuroInd", method = "character", an = "integer", y = "list"),
    def = function(x, method, an, y){
        # Verification inputs
        if (length(y) != 4L)  stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 4. \n")
        # Verification des noms des elements de la liste
        if (sum(names(y) == c("proba_dyn", "tx_min", "tx_soc", "choc_lapse_mass")) != length(y))
          stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 4 de nom : proba_dyn, tx_min, tx_soc, choc_lapse_mass. \n")

        # affectation pour eviter la modification des fonctions
        proba_dyn       <- .subset2(y, 1L)
        tx_min          <- .subset2(y, 2L)
        tx_soc          <- .subset2(y, 3L)
        choc_lapse_mass <- .subset2(y, 4L)

        # Verification des types des elements de la liste
        if (! is.list(proba_dyn))   stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 4, de nom : proba_dyn, tx_min, an, tx_soc, choc_lapse_mass, dont le type est : list, list, numeric. \n")
        if (! is.list(tx_min))      stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 4, de nom : proba_dyn, tx_min, tx_soc, choc_lapse_mass, dont le type est : list, list, numeric \n")
        if (! is.numeric(tx_soc))   stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 4, de nom : proba_dyn, tx_min, tx_soc, choc_lapse_mass, dont le type est : list, list, numeric \n")
        if (! is.numeric(choc_lapse_mass))   stop("[EpEuroInd : calc_prest] : L'input y doit correspondre a une liste de longueur 4, de nom : proba_dyn, tx_min, tx_soc, choc_lapse_mass, dont le type est : list, list, numeric \n")

        # Extraction des donnees du tableau de probas
        annee <- paste("Annee", an , sep = "_")

        # Table de proba
        tab_proba <- x@tab_proba

        # Extraction des probabilites de rachat total
        name_rachat_tot <- names(tab_proba@qx_rach_tot)
        num_rachat_tot  <- which(name_rachat_tot ==  annee)
        qx_rach_tot     <- .subset2(tab_proba@qx_rach_tot, num_rachat_tot)

        # Extraction des probabilites de rachats partiels
        name_rachat_part <- names(tab_proba@qx_rach_part)
        num_rachat_part  <- which(name_rachat_part ==  annee)
        qx_rach_part     <- .subset2(tab_proba@qx_rach_part, num_rachat_part)

        # Extraction des probabilites de DC
        name_proba_dc <- names(tab_proba@qx_dc)
        num_proba_dc  <- which(name_proba_dc ==  annee)
        qx_dc         <- .subset2(tab_proba@qx_dc, num_proba_dc)

        # Model Point
        mp      <- x@mp # Extraction
        nb_mp   <- nrow(mp) # Nombre de model point
        nom_epeuro      <- names(mp)
        num_pm          <- which(nom_epeuro == "pm")
        num_pm_gar      <- which(nom_epeuro == "pm_gar")
        num_terme       <- which(nom_epeuro == "terme")
        num_nb_contr    <- which(nom_epeuro == "nb_contr")
        num_chgt_enc    <- which(nom_epeuro == "chgt_enc")
        num_chgt_rach   <- which(nom_epeuro == "chgt_rach")
        num_ind_chgt_enc_pos    <- which(nom_epeuro == "ind_chgt_enc_pos")

        # Extraction de donnees du MP
        nb_contr         <- .subset2(mp, num_nb_contr)
        ind_chgt_enc_pos <- .subset2(mp, num_ind_chgt_enc_pos)
        chgt_enc         <- .subset2(mp, num_chgt_enc)


        # Applique la methode de calcul
        if(method == "normal") # calcul des flux normaux
            pm_deb <- .subset2(mp, num_pm)
        else if (method == "gar") # calcul des flux avec revalorisation garantie uniquement
            pm_deb <- .subset2(mp, num_pm_gar)
        else
            stop("[EpEuroInd : calc_prest] : L'input method dont etre egal a 'normal' ou 'gar' \n")


        # Indicatrice de premiere annee pour application du choc de rachat massif et application du choc de rachat
        # Ce choc en applique en debut d'annee. Il n'y a donc pas de revalorisation
        choc_lapse_mass <- choc_lapse_mass *  (an == 1L)
        rach_mass <- pm_deb * choc_lapse_mass
        nb_rach_mass <- nb_contr * choc_lapse_mass

        # Indicatrice de sortie en echeance
        ind_ech <- (an <= .subset2(mp, num_terme))


        # Extraction des taux de revalorisation minimum et des taux technique
        tx_min_an  <- tx_min[["tx_an"]]
        tx_min_se  <- tx_min[["tx_se"]]


        # Calcul des echeances et de la revalorisation
        ech <- pm_deb * (1 - ind_ech) * (1 - choc_lapse_mass) # echeance
        rev_ech <- ech * tx_min_se # revalorisation au taux minimum
        nb_ech <- nb_contr * (1 - ind_ech) * (1 - choc_lapse_mass)# nombre de contrats en echeance


        # Calcul des flux  rachats totaux
        # Taux de rachat incluant les rachats structurels et conjoncturels
        qx_rach_tot_glob <- pmax(0, pmin(1, qx_rach_tot + proba_dyn[["qx_rach_tot_dyn"]]))
        rach_tot <- pm_deb * qx_rach_tot_glob * ind_ech * (1 - choc_lapse_mass) # Flux de rachats totaux
        rev_rach_tot <- rach_tot * tx_min_se # revalorisation au taux minimum
        nb_rach_tot <- nb_contr * qx_rach_tot_glob * ind_ech * (1 - choc_lapse_mass) # nombre de contrats en rachat total

        # Calcul des flux de deces
        # Taux de deces sur la population des non rachetes
        qx_dc_rach <- qx_dc * (1 - qx_rach_tot_glob)
        dc <- pm_deb * qx_dc_rach * ind_ech * (1 - choc_lapse_mass) # Flux de rachats totaux
        rev_dc <- dc * tx_min_se # revalorisation au taux minimum
        nb_dc <- nb_contr * qx_dc_rach * ind_ech * (1 - choc_lapse_mass) # nombre de contrats en deces

        # Calcul des flux rachats partiels
        # Taux de rachat incluant les rachats structurels et conjoncturels sur la population des non rachetes et vivants
        qx_rach_part_glob <- (1 - qx_rach_tot_glob) * (1 - qx_dc) *
            pmax(0, pmin(1, qx_rach_part + proba_dyn[["qx_rach_part_dyn"]]))
        rach_part <- pm_deb * qx_rach_part_glob * ind_ech * (1 - choc_lapse_mass) # Flux de rachats partiels
        rev_rach_part <- rach_part * tx_min_se # revalorisation au taux minimum

        # Total des prestations
        prest <- rach_mass + ech + rach_tot + dc + rach_part # total prestations
        rev_prest <- rev_ech + rev_rach_tot + rev_dc + rev_rach_part # total revalorisation des prestations
        nb_sortie <- nb_rach_mass + nb_ech + nb_dc + nb_rach_tot # nombre de sorties
        nb_contr_debut <- nb_contr
        nb_contr_fin <- nb_contr_debut - nb_sortie # nombre de contrats en cours en fin d'annee
        nb_contr_moy <- (nb_contr_debut + nb_contr_fin) / 2  # nombre de contrats moyen

        # Calcul du taux de chargement sur encours
        # Applique une limite sur le chargement sur encours selon la valeur de l'indicatrice
        # permettant les taux negatifs.
        chgt_enc <- pmin(chgt_enc, tx_min_an /(1 + tx_min_an)) * ind_chgt_enc_pos + chgt_enc * (1 - ind_chgt_enc_pos)

        # Calcul des chargements sur encours
        enc_charg <- (prest - rach_mass + rev_prest) * chgt_period(chgt_enc, period = "se")

        # Calcul de la revalorisation nette des prestations avec capitalisation sur un semestre
        rev_prest_nette <- rev_prest - enc_charg

        # Calcul des autres chargements et des prelevements sociaux
        rach_charg_mass <- rach_mass * .subset2(mp, num_chgt_rach)
        rach_charg <- (rach_mass + rach_tot + rach_part + rev_rach_tot + rev_rach_part) * .subset2(mp, num_chgt_rach)
        soc_prest <- pmax(0, rev_prest_nette) * tx_soc # prelevements sociaux

        # Calcul des interets techniques sur prestations
        it_tech_prest <- (prest - rach_mass) * tx_min[["tx_tech_se"]]

        # Output zero
        out_zero <- rep(0, nb_mp)

        # output
        return(list(method = method,
                    flux = list(
                        ech             = ech,
                        rach_mass       = rach_mass,
                        rach_tot        = rach_tot,
                        dc              = dc,
                        rach_part       = rach_part,
                        rente           = out_zero,
                        prest           = prest,
                        rev_ech         = rev_ech,
                        rev_rach_tot    = rev_rach_tot,
                        rev_dc          = rev_dc,
                        rev_rach_part   = rev_rach_part,
                        rev_prest       = rev_prest,
                        rev_prest_nette = rev_prest_nette,
                        enc_charg_prest = enc_charg,
                        rach_charg      = rach_charg,
                        rach_charg_mass = rach_charg_mass,
                        soc_prest       = soc_prest,
                        it_tech_prest   = it_tech_prest,
                        arr_charg       = out_zero),
                    stock = list(
                        nb_ech       = nb_ech,
                        nb_rach_mass = nb_rach_mass,
                        nb_rach_tot  = nb_rach_tot,
                        nb_dc        = nb_dc,
                        nb_debut     = nb_contr_debut,
                        nb_sortie    = nb_sortie,
                        nb_contr_fin = nb_contr_fin,
                        nb_contr_moy = nb_contr_moy
                    )
        ))
    }
)

#--------------------------------------------------------
setMethod(
    f = "calc_prest",
    signature = c(x = "RetraiteEuroRest", method = "character", an = "integer"),
    def = function(x, method, an){

        # Annee en cours
        annee <- paste("Annee", an , sep = "_")

        # Table de proba
        tab_proba <- x@tab_proba

        # Extraction des probabilites de sortie
        num_sortie_retraite     <- which(names(tab_proba@sortie_retraite) ==  annee)
        proba_sortie_retraite   <- .subset2(tab_proba@sortie_retraite, num_sortie_retraite)

        # Extraction des probabilites de survie un an
        num_survie_un_an    <- which(names(tab_proba@survie_un_an) ==  annee)
        proba_survie_un_an  <- .subset2(tab_proba@survie_un_an, num_survie_un_an)

        # Extraction de donnees du data.frame de ModelPoint
        mp <- x@mp
        nb_mp          <- nrow(mp)
        nom_retraite   <- names(mp)
        num_rente      <- which(nom_retraite == "rente")
        num_rente_gar  <- which(nom_retraite == "rente_gar")
        num_nb_contr   <- which(nom_retraite == "nb_contr")
        num_ch_arr     <- which(nom_retraite == "ch_arr")
        nom_diff       <- which(nom_retraite == "diff")

        ## Calcul du nombre de contrats
        # Nombre de contrat en debut de periode
        nb_contr_debut <- .subset2(mp, num_nb_contr)

        # Nombre de contrats
        nb_sortie    <-  proba_sortie_retraite * nb_contr_debut          # Nombre de contrats sortis
        nb_contr_fin <- nb_contr_debut - nb_sortie           # nombre de contrats en cours en fin d'annee
        nb_contr_moy <- (nb_contr_debut + nb_contr_fin) / 2  # nombre de contrats moyen


        ## Prestations
        # Taux de chargement sur arrerage
        ch_arr <- .subset2(mp, num_ch_arr)
        # Duree du differe
        dur_diff <- .subset2(mp, nom_diff)
        # Indicatrice differe
        ind_diff <- dur_diff < an

        # Rentes
        if(method == "normal")
            rente <- .subset2(mp, num_rente) * ind_diff
        else if (method == "gar")
            rente <- .subset2(mp, num_rente_gar) * ind_diff
        else
            stop("[RetraiteEuroRest : calc_prest] : L'input method doit etre egal a 'normal' ou 'gar' \n")

        # Calcul des prestations
        prestations <- nb_contr_debut * (rente / (1 + ch_arr))

        # Calcul des rentes
        rente_flux <- proba_survie_un_an * prestations


        # Calcul des chargements
        arr_charg <- rente * (ch_arr / (1 + ch_arr))

        # output zero
        out_zero <- rep(0, nb_mp)

        # output
        return(list(method = method,
                    flux = list(
                        ech             = out_zero,
                        rach_mass       = out_zero,
                        rach_tot        = out_zero,
                        dc              = out_zero,
                        rach_part       = out_zero,
                        rente           = rente_flux,
                        prest           = rente_flux,
                        rev_ech         = out_zero,
                        rev_rach_tot    = out_zero,
                        rev_dc          = out_zero,
                        rev_rach_part   = out_zero,
                        rev_prest       = out_zero,
                        rev_prest_nette = out_zero,
                        enc_charg_prest = out_zero,
                        rach_charg      = out_zero,
                        rach_charg_mass = out_zero,
                        soc_prest       = out_zero,
                        it_tech_prest   = out_zero,
                        arr_charg       = arr_charg),
                    stock = list(
                        nb_ech          = out_zero,
                        nb_rach_mass    = out_zero,
                        nb_rach_tot     = out_zero,
                        nb_dc           = nb_sortie,
                        nb_debut        = nb_contr_debut,
                        nb_sortie       = nb_sortie,
                        nb_contr_fin    = nb_contr_fin,
                        nb_contr_moy    = nb_contr_moy
                    )
        ))
    }
)
