
#----------------------------------------------------------------------------------------------------------------------------------------------------
#           Fonction de calcul des flux de PM d un model point Epargne euro ou Retraite euro en phase de restitution
#----------------------------------------------------------------------------------------------------------------------------------------------------
##' Calcul les PM pour des contrats epargne en euros et les contrats de retraite euro en phases de restitution.
##'
##' \code{calc_pm} est une methode permettant de calculer les provisions mathematiques (PM)
##'  de fin de periode avant application de la revalorisation au titre de la participation aux benefices.
##' @name calc_pm
##' @docType methods
##' @param x un objet de la classe \code{\link{EpEuroInd}} ou de la classe \code{\link{RetraiteEuroRest}} contenant les model points epargne euros ou retraite euro en phases de restitution.
##' @param an une valeur \code{integer} correspondant a l'annee du calcul des prestations.
##' @param method un \code{character} prenant pour valeur \code{normal} pour le calcul des flux avec application de la revalorisation au titre de
##' la participation aux benefices, et la valeur \code{gar} pour le calcul avec uniquement les flux garanti (calcul de la FDB).
##' @param tx_cible une liste conteant les taux cibles annuel et semestriel par model points.
##' Le format de cette liste correspond a la sortie de la methode \code{\link{calc_tx_cible}}
##' @param y une liste a remplir uniquement si \code{x} est de type \code{\link{EpEuroInd}} contenant les parametres :
##' \describe{
##' \item{\code{tab_prime} : }{une liste contenant les flux de primes pour chaque ligne de model points.
##' Le format de cette liste correspond a la sortie \code{flux} de la methode \code{\link{calc_primes}}.
##' A remplir uniquement si l'objet \code{x} est de type \code{\link{EpEuroInd}}.}
##' \item{\code{tab_prest} : }{une liste contenant le taux de revalorisation minimum associes a chaque ligne de model points.
##' Le format de cette liste correspond a la sortie \code{flux} de la methode \code{\link{calc_prest}}.
##' A remplir uniquement si \code{x} est de type \code{\link{EpEuroInd}}.}
##' \item{\code{tx_min} : }{une liste contenant le taux de revalorisation minimum associes a chaque ligne de model points.
##'  Le format de cette liste correspond a la sortie de la methode \code{\link{calc_tx_min}}.
##'  A remplir uniquement si \code{x} est de type \code{\link{EpEuroInd}}.}
##' \item{\code{tx_soc} : }{est une valeur \code{numeric} correspondant au taux de prelevements sociaux \code{\link{EpEuroInd}}.}
##' }
##' @details En epargne, cette methode permet de calculer les montants de PM de fin d'annee, avec une revalorisation
##' minimale pour les inputs. Les chargements sur encours sont egalement preleves. Cette methode permet de gerer les contrats a taux de
##' revalorisation net negatif. Cette methode permet egalement de calculer le besoin de financement necessaire
##' pour atteindre les exigences de revalorisation des assures.
##' Pour la  retraite, cette methode renvoie les elements de PM ainsi que le besoin de financement afferent.
##' @return Une liste contenant :
##' \describe{
##' \item{\code{method} : }{la valeur de l'argument \code{method}.}
##' \item{\code{flux} : }{une liste comprenant les flux de l'annee.}
##' \item{\code{stock} : }{une liste comprenant les nombres de sorties.}
##' }
##' @return Le format de la liste \code{flux} est :
##' \describe{
##' \item{\code{rev_stock_brut} : }{un vecteur contenant la revalorisation minimale
##' brute de l'annee appliquee au PM (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
##' \item{\code{rev_stock_nette} : }{un vecteur contenant la revalorisation minimale
##' nette de l'annee appliquee au PM (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
##' \item{\code{enc_charg_stock} : }{un vecteur contenant les chargement sur encours de l'annee,
##' calcules en prenant en compte la revalorisation minimale (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
##' \item{\code{enc_charg_base_th} : }{un vecteur contenant les chargements sur encours theoriques
##' de l'annee, evalues sur la base de la PM non revalorisees (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
##' \item{\code{enc_charg_rmin_th} : }{un vecteur contenant les chargements sur encours theoriques
##'  de l'annee, evalues sur la seule base de la revalorisation minimale des PM (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
##' \item{\code{base_enc_th} : }{un vecteur contenant l'assiette de calcul des chargements
##'  sur encours de l'annee (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
##' \item{\code{soc_stock} : }{un vecteur contenant le prelevements sociaux de l'annee
##'  (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
##' \item{\code{it_tech_stock} : }{un vecteur contenant les interets
##' techniques sur stock de l'annee (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
##' \item{\code{it_tech} : }{un vecteur contenant les interets techniques sur stock et
##' sur prestations de l'annee (nul en cas de typage \code{\link{RetraiteEuroRest}}).}
##' \item{\code{bes_tx_cible} : }{un vecteur contenant le besoin de financement de l'annee pour
##' atteindre le taux cible de chaque assure.}
##' }
##' @return Le format de la liste \code{stock} est :
##' \describe{
##' \item{\code{pm_deb : }}{un vecteur contenant le montant de PM en debut d'annee}
##' \item{\code{pm_fin : }}{un vecteur contenant le montant de PM en fin d'annee, avec
##' revalorisation au taux minimum}
##' \item{\code{pm_moy : }}{un vecteur contenant le montant de PM moyenne sur l'annee.}
##' }
##' @author Prim'Act
##' @seealso \code{\link{calc_primes}}, \code{\link{calc_prest}}, \code{\link{calc_tx_cible}},
##' \code{\link{calc_tx_min}}.
##' @export
##' @include EpEuroInd-class.R RetraiteEuroRest_class.R HypTech-class.R

#--------------------------------------------------------
setGeneric(name = "calc_pm", def = function(x, method, an, tx_cible, y){standardGeneric("calc_pm")})
# Epargne
# y = list(tab_prime, tab_prest, tx_cible, tx_min, an, method, tx_soc)
# Retraite
# y = list(ht, tx_cible, method)
#--------------------------------------------------------

setMethod(
    f = "calc_pm",
    signature = c(x = "EpEuroInd", method = "character", an = "integer", tx_cible = "list", y = "list"),
    def = function(x, method, an, tx_cible, y){

        # Verification inputs
        if (length(y) != 4L) stop("[EpEuroInd : calc_pm] : L'input y doit correspondre a une liste de longueur 4. \n")

        # Verification des noms des elements de la liste
        if (sum(names(y) == c("tab_prime", "tab_prest", "tx_min", "tx_soc")) != 4L)
            stop("[EpEuroInd : calc_pm] : L'input y doit correspondre a une liste de longueur 4 de nom : tab_prime, tab_prest, tx_min, tx_soc. \n")

        # Affectation pour eviter la modification des fonctions
        tab_prime <- .subset2(y, 1L)
        tab_prest <- .subset2(y, 2L)
        tx_min    <- .subset2(y, 3L)
        tx_soc    <- .subset2(y, 4L)

        # Verification des types des elements de la liste
        if (! is.list(tab_prime))      stop("[EpEuroInd : calc_pm] : L'input y doit correspondre a une liste de longueur 4, de nom : tab_prime, tab_prest, tx_min, tx_soc, dont le type est : list, list, list, numeric. \n")
        if (! is.list(tab_prest))      stop("[EpEuroInd : calc_pm] : L'input y doit correspondre a une liste de longueur 4, de nom : tab_prime, tab_prest, tx_min, tx_soc, dont le type est : list, list, list, numeric. \n")
        if (! is.list(tx_min))         stop("[EpEuroInd : calc_pm] : L'input y doit correspondre a une liste de longueur 4, de nom : tab_prime, tab_prest, tx_min, tx_soc, dont le type est : list, list, list, numeric. \n")
        if (! is.numeric(tx_soc))      stop("[EpEuroInd : calc_pm] : L'input y doit correspondre a une liste de longueur 4, de nom : tab_prime, tab_prest, tx_min, tx_soc, dont le type est : list, list, list, numeric. \n")


        # Seuil pour gerer les problemes d'arrondi
        SEUIL_ARRONDI <- 0.00001

        # Table ModelPoint
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

        # Extraction de donnees
        tx_min_an  <- tx_min[["tx_an"]]
        tx_min_se  <- tx_min[["tx_se"]]
        tx_tech_an <- tx_min[["tx_tech_an"]]
        tx_tech_se <- tx_min[["tx_tech_se"]]
        pri_net     <- tab_prime[["pri_net"]]
        prest     <- tab_prest[["prest"]]


        # Applique la method de calcul
        if (method == "normal") # calcul des flux normaux
            pm_deb <- .subset2(mp, num_pm)
        else if (method == "gar") # calcul des flux avec revalorisation garantie uniquement
            pm_deb <- .subset2(mp, num_pm_gar)
        else
            stop("[EpEuroInd : calc_pm] : L'input method dont etre egal a 'normal' ou 'gar' \n")


        # Calculs effectues plusieurs fois
        diff_pm_prest <- pm_deb - prest
        tx_min_an_1   <- (1 + tx_min_an)

        # Calcul du taux de chargement sur encours
        # Applique une limite sur le chargement sur encours selon la valeur de l'indicatrice permettant les taux negatifs.
        chgt_enc_th <- .subset2(mp, num_chgt_enc) # Chargements sur encours theoriques
        ind_chgt_enc_pos <- .subset2(mp, num_ind_chgt_enc_pos) # Indicatrice chargements sur encours
        chgt_enc <- pmin(chgt_enc_th, tx_min_an / tx_min_an_1) * ind_chgt_enc_pos + chgt_enc_th * (1 - ind_chgt_enc_pos)

        # Calcul de la revalorisation brute
        rev_stock_brut <- diff_pm_prest * tx_min_an + pri_net * tx_min_se

        # Chargements : sur encours
        enc_charg_stock <- diff_pm_prest * tx_min_an_1 * chgt_enc +
            pri_net * (1 + tx_min_se) * chgt_period(chgt_enc, period = "se")

        # Chargement sur encours theorique en decomposant la part revalative au passif non revalorises et a la revalorisation
        chgt_se <- chgt_period(chgt_enc_th, period = "se")
        tmp1 <- diff_pm_prest * chgt_enc_th
        tmp2 <- pri_net * chgt_se
        enc_charg_base_th <- tmp1 + tmp2
        enc_charg_rmin_th <- tmp1 * tx_min_an + tmp2 * tx_min_se

        # Base utilise pour appliques le calcul du taux de chargement sur encours
        base_enc_th <- diff_pm_prest * tx_min_an_1 + pri_net * (1 + tx_min_se)

        # Calcul de la revalorisation sur stock
        rev_stock_nette <- rev_stock_brut - enc_charg_stock

        # Revalorisation nette totale
        rev_total_nette <- rev_stock_nette + tab_prest[["rev_prest_nette"]]

        # Prelevement sociaux
        soc_stock <- pmax(0, rev_stock_nette) * tx_soc

        # Evaluation des provisions mathematiques avant PB
        pm_fin <- diff_pm_prest + pri_net + rev_stock_nette - soc_stock

        # Application d'un seuil pour eviter les problemes d'arrondi
        pm_fin[which(abs(pm_fin) < SEUIL_ARRONDI)] <- 0

        # Message d'erreur si PM negative
        if(! all(pm_fin >= 0))
            stop("[EpEuro : calc_pm] : la valeur des PM ne peut pas etre negative.")

        # PM moyenne et taux de chargement
        pm_moy <- (pm_deb + pm_fin) / 2

        # Evaluation des interets techniques
        it_tech_stock   <- diff_pm_prest * tx_tech_an + pri_net * tx_tech_se
        it_tech         <- it_tech_stock + tab_prest[["it_tech_prest"]]

        # EValuation du besoin de taux cible
        bes_tx_cible <- pmax(0,
                             tx_cible[["tx_cible_an"]] * diff_pm_prest +
                                 tx_cible[["tx_cible_se"]] * pri_net)

        # output
        return(list(method = method,
                    stock = list(
                        pm_deb = pm_deb,
                        pm_fin = pm_fin,
                        pm_moy = pm_moy
                    ),
                    flux = list(
                        rev_stock_brut = rev_stock_brut,
                        rev_stock_nette = rev_stock_nette,
                        enc_charg_stock = enc_charg_stock,
                        enc_charg_base_th = enc_charg_base_th,
                        enc_charg_rmin_th = enc_charg_rmin_th,
                        base_enc_th = base_enc_th,
                        soc_stock = soc_stock,
                        it_tech_stock = it_tech_stock,
                        it_tech = it_tech,
                        bes_tx_cible = bes_tx_cible
                    )
        ))
    }
)

setMethod(
    f = "calc_pm",
    signature = c(x = "RetraiteEuroRest", method = "character", an = "integer", tx_cible = "list"),
    def = function(x, method, an, tx_cible){

        # Seuil pour gerer les problemes d'arrondi
        SEUIL_ARRONDI <- 0.00001

        # Annee en cours
        annee <- paste("Annee", an , sep = "_")

        # Extraction des coeeficients actuariels
        tab_proba <- x@tab_proba
        name_ax <- names(tab_proba@ax)
        num_ax  <- which(name_ax ==  annee)
        coef_rente      <- .subset2(tab_proba@ax, num_ax)

        # Table ModelPoint
        mp <- x@mp

        # Recuperer les numeros de colonnes des parametres utilises
        nb_mp          <- nrow(mp)
        nom_retraite   <- names(mp)
        num_rente      <- which(nom_retraite == "rente")
        num_rente_gar  <- which(nom_retraite == "rente_gar")
        num_nb_contr   <- which(nom_retraite == "nb_contr")
        num_pm         <- which(nom_retraite == "pm")
        num_ch_arr     <- which(nom_retraite == "ch_arr")

        # Liste TxCible
        nom_tx_cib <- names(tx_cible)
        num_cib_an <- which(nom_tx_cib == "tx_cible_an")

        # Recuperer la rente
        if(method == "normal") # calcul de la rente normal
            rente <- .subset2(mp, num_nb_contr) * .subset2(mp, num_rente)
        else if (method == "gar") # calcul de la rente avec revalorisation garantie uniquement
            rente <- .subset2(mp, num_nb_contr) * .subset2(mp, num_rente_gar)
        else
            stop("[RetEuroRest : calc_pm] : L'input method dont etre egal a 'normal' ou 'gar' \n")


        # Calcul de la PM (ax * rente)
        pm_fin <- coef_rente * rente

        # Evaluation des provisions mathematiques avant PB
        pm_fin <- pm_fin / (1 + .subset2(mp, num_ch_arr))
        pm_deb <- .subset2(mp, num_pm)

        # Application d'un seuil pour eviter les problemes d'arrondi
        pm_fin[which(abs(pm_fin) < SEUIL_ARRONDI)] <- 0


        # Message d'erreur si PM negative
        if(! all(pm_fin >= 0))
            stop("[RetraiteEuroRest : calc_pm] : la valeur des PM retraite ne peut pas etre negative.")

        # PM moyenne et taux de chargement
        pm_moy <- (pm_deb + pm_fin) / 2

        # EValuation du besoin de taux cible
        bes_tx_cible <- .subset2(tx_cible, num_cib_an) * pm_fin

        # output
        out_zero <- rep(0, nb_mp) # output zero
        return(list(method = method,
                    stock = list(
                        pm_deb = pm_deb,
                        pm_fin = pm_fin,
                        pm_moy = pm_moy
                    ),
                    flux = list(
                        rev_stock_brut = out_zero,
                        rev_stock_nette = out_zero,
                        enc_charg_stock = out_zero,
                        enc_charg_base_th = out_zero,
                        enc_charg_rmin_th = out_zero,
                        base_enc_th = out_zero,
                        soc_stock = out_zero,
                        it_tech_stock = out_zero,
                        it_tech = out_zero,
                        bes_tx_cible = bes_tx_cible
                    )
        ))
    }
)

