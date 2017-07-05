
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
##' @param y une liste contenant des parametres distincts selon l'objet initialement renseigne :
##' \describe{
##' \item{\code{tab_prime} : }{une liste contenant les flux de primes pour chaque ligne de model points.
##' Le format de cette liste correspond a la sortie \code{flux} de la methode \code{\link{calc_primes}}.
##' A remplir uniquement si l'objet \code{x} est de type \code{\link{EpEuroInd}}.}
##' \item{\code{tab_prest} : }{une liste contenant le taux de revalorisation minimum associes a chaque ligne de model points.
##' Le format de cette liste correspond a la sortie \code{flux} de la methode \code{\link{calc_prest}}.
##' A remplir uniquement si \code{x} est de type \link{\code{EpEuroInd}}.}
##' \item{\code{tx_cible} : }{une liste conteant les taux cible annuel et semestriel par model points.
##' Le format de cette liste correspond a la sortie de la methode \code{\link{calc_tx_cible}}.}
##' \item{\code{tx_min} : }{une liste contenant le taux de revalorisation minimum associes a chaque ligne de model points.
##'  Le format de cette liste correspond a la sortie de la methode \code{\link{calc_tx_min}}.
##'  A remplir uniquement si \code{x} est de type \link{\code{EpEuroInd}}.}
##' \item{\code{an} : } {une valeur \code{numeric} represantant l'annee de projection courante.
##' A remplir uniquement si \code{x} est de type \link{\code{EpEuroInd}}.}
##' \item{\code{method} : }{un \code{character} prenant pour valeur \code{normal} pour le calcul :
##' des flux avec application de la revalorisation au titre de la participation aux benefices, et
##' la valeur \code{gar} pour le calcul avec uniquement les flux garanti (calcul de la FDB).}
##' \item{\code{tx_soc} : }{est une valeur \code{numeric} correspondant au taux de prelevements sociaux \link{\code{EpEuroInd}}.}
##' \item{\code{ht} : }{un objet de type \code{\link{HypTech}} caracterisant les parametres de mortalite necessaire
##'  a la determination du montant de PM. A remplir uniquement si \code{x} est de type \link{\code{RetraiteEuroRest}}.}
##' \item{\code{coef_rente} : }{un vecteur de type \code{numeric} correspondant aux coefficients actuariels.
##' A remplir uniquement si \code{x} est de type \link{\code{RetraiteEuroRest}}.}
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
##' @include EpEuroInd-class.R RetraiteEuroRest_class.R

#--------------------------------------------------------
setGeneric(name = "calc_pm", def = function(x, y){standardGeneric("calc_pm")})
# Epargne
# y = list(tab_prime, tab_prest, tx_cible, tx_min, an, method, tx_soc)
# Retraite
# y = list(ht, tx_cible, method)
#--------------------------------------------------------

setMethod(
    f = "calc_pm",
    signature = c(x = "EpEuroInd", y = "list"),
    def = function(x, y){
        # Verification inputs
        if (length(y) != 7)                         {stop("[EpEuroInd : calc_pm] : L'input y doit correspondre a une liste de longueur 7. \n")}
        # Verification des noms des elements de la liste
        if (sum(names(y) == c("tab_prime", "tab_prest", "tx_cible", "tx_min", "an", "method", "tx_soc")) != length(y)) {stop("[EpEuroInd : calc_pm] : L'input y doit correspondre a une liste de longueur 7 de nom : tab_prime, tab_prest, tx_cible, tx_min, an, method, tx_soc. \n")}
        # Verification des types des elements de la liste
        if (! is.list(y[["tab_prime"]]))      {stop("[EpEuroInd : calc_pm] : L'input y doit correspondre a une liste de longueur 7, de nom : tab_prime, tab_prest, tx_cible, tx_min, an, method, tx_soc, dont le type est : list, list, list, list, numeric, character, numeric. \n")}
        if (! is.list(y[["tab_prest"]]))      {stop("[EpEuroInd : calc_pm] : L'input y doit correspondre a une liste de longueur 7, de nom : tab_prime, tab_prest, tx_cible, tx_min, an, method, tx_soc, dont le type est : list, list, list, list, numeric, character, numeric. \n")}
        if (! is.list(y[["tx_cible"]]))       {stop("[EpEuroInd : calc_pm] : L'input y doit correspondre a une liste de longueur 7, de nom : tab_prime, tab_prest, tx_cible, tx_min, an, method, tx_soc, dont le type est : list, list, list, list, numeric, character, numeric. \n")}
        if (! is.list(y[["tx_min"]]))         {stop("[EpEuroInd : calc_pm] : L'input y doit correspondre a une liste de longueur 7, de nom : tab_prime, tab_prest, tx_cible, tx_min, an, method, tx_soc, dont le type est : list, list, list, list, numeric, character, numeric. \n")}
        if (! is.numeric(y[["an"]]))          {stop("[EpEuroInd : calc_pm] : L'input y doit correspondre a une liste de longueur 7, de nom : tab_prime, tab_prest, tx_cible, tx_min, an, method, tx_soc, dont le type est : list, list, list, list, numeric, character, numeric. \n")}
        if (! is.character(y[["method"]]))    {stop("[EpEuroInd : calc_pm] : L'input y doit correspondre a une liste de longueur 7, de nom : tab_prime, tab_prest, tx_cible, tx_min, an, method, tx_soc, dont le type est : list, list, list, list, numeric, character, numeric. \n")}
        if (! is.numeric(y[["tx_soc"]]))      {stop("[EpEuroInd : calc_pm] : L'input y doit correspondre a une liste de longueur 7, de nom : tab_prime, tab_prest, tx_cible, tx_min, an, method, tx_soc, dont le type est : list, list, list, list, numeric, character, numeric. \n")}
        # Affectation pour eviter la modification des fonctions
        tab_prime = y[["tab_prime"]]
        tab_prest = y[["tab_prest"]]
        tx_cible  = y[["tx_cible"]]
        tx_min    = y[["tx_min"]]
        an        = y[["an"]]
        method    = y[["method"]]
        tx_soc    = y[["tx_soc"]]


        # Seuil pour gerer les problemes d'arrondi
        SEUIL_ARRONDI <- 0.00001

        # Applique la method de calcul
        if(method == "normal"){ # calcul des flux normaux
            pm_deb <- x@mp$pm
        }else if (method == "gar"){ # calcul des flux avec revalorisation garantie uniquement
            pm_deb <- x@mp$pm_gar
        }

        nb_mp <- nrow(x@mp)
        # Extraction des taux de revalorisation minimum. La table tx_rev peut etre une table de taux de revalorisation apres pb ou avant pb
        # tx_min <-  calc_tx_min(x, an)
        tx_min_an <- tx_min[["tx_an"]]
        tx_min_se <- tx_min[["tx_se"]]
        tx_tech_an <- tx_min[["tx_tech_an"]]
        tx_tech_se <- tx_min[["tx_tech_se"]]

        # Calcul du taux de chargement sur encours
        # Applique une limite sur le chargement sur encours selon la valeur de l'indicatrice
        # permettant les taux negatifs.
        chgt_enc_th <- x@mp$chgt_enc # Chargements sur encours theoriques
        chgt_enc <- pmin(chgt_enc_th, tx_min_an /(1 + tx_min_an)) * x@mp$ind_chgt_enc_pos +
            chgt_enc_th * (1 - x@mp$ind_chgt_enc_pos)

        # Calcul de la revalorisation brute
        rev_stock_brut <- (pm_deb - tab_prest[["prest"]]) * (tx_min_an) +
            tab_prime[["pri_net"]] * tx_min_se

        # Chargements : sur encours
        enc_charg_stock <- (pm_deb - tab_prest[["prest"]]) * (1 + tx_min_an) * chgt_enc +
            tab_prime[["pri_net"]] * (1 + tx_min_se) * chgt_period(chgt_enc, period = "se")

        # Chargement sur encours theorique en decomposant la part revalative au passif non revalorises
        # et a la revalorisation
        enc_charg_base_th <- (pm_deb - tab_prest[["prest"]]) * chgt_enc_th +
            tab_prime[["pri_net"]] * chgt_period(chgt_enc_th, period = "se")
        enc_charg_rmin_th <- (pm_deb -tab_prest[["prest"]]) * (tx_min_an)  * chgt_enc_th +
            tab_prime[["pri_net"]] * tx_min_se * chgt_period(chgt_enc_th, period = "se")

        # Base utilise pour appliques le calcul du taux de chargement sur encours
        base_enc_th <- (pm_deb -tab_prest[["prest"]]) * (1 + tx_min_an) +
            tab_prime[["pri_net"]] * (1 + tx_min_se)


        # enc_charg <- enc_charg_stock + tab_prest[["enc_charg_prest"]]

        # Calcul de la revalorisation sur stock
        rev_stock_nette <- rev_stock_brut - enc_charg_stock

        # Revalorisation nette totale
        rev_total_nette <- rev_stock_nette + tab_prest[["rev_prest_nette"]]

        # Prelevement sociaux
        soc_stock <- pmax(0, rev_stock_nette) * tx_soc
        # soc <- soc_stock + tab_prest[["soc_prest"]]

        # Evaluation des provisions mathematiques avant PB
        pm_fin <- pm_deb - tab_prest[["prest"]] + tab_prime[["pri_net"]] + rev_stock_nette -  soc_stock

        # Application d'un seuil pour eviter les problemes d'arrondi
        pm_fin[which(abs(pm_fin) < SEUIL_ARRONDI)] <- 0

        # Message d'erreur si PM negative
        if( ! prod(pm_fin >= 0)){
            stop("[EpEuro-calc_pm] : la valeur des PM ne peut pas etre negative.")
        }

        # PM moyenne et taux de chargement
        pm_moy <- (pm_deb + pm_fin) / 2


        # Evaluation des interets techniques
        it_tech_stock <- (pm_deb - tab_prest[["prest"]]) * tx_tech_an + tab_prime[["pri_net"]] * tx_tech_se
        it_tech <- it_tech_stock + tab_prest[["it_tech_prest"]]

        # EValuation du besoin de taux cible
        bes_tx_cible <- pmax(0,
                             tx_cible[["tx_cible_an"]]*(pm_deb - tab_prest[["prest"]]) +
                                 tx_cible[["tx_cible_se"]] * tab_prime[["pri_net"]])

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
                        # rev_total_nette = rev_total_nette,
                        enc_charg_stock = enc_charg_stock,
                        enc_charg_base_th = enc_charg_base_th,
                        enc_charg_rmin_th = enc_charg_rmin_th,
                        base_enc_th = base_enc_th,
                        soc_stock = soc_stock,
                        # enc_charg = enc_charg,
                        # rach_charg = rach_charg,
                        # pri_chgt = pri_chgt,
                        # soc_stock = soc_stock,
                        it_tech_stock = it_tech_stock,
                        it_tech = it_tech,
                        bes_tx_cible = bes_tx_cible
                    )
        ))
    }
)

setMethod(
    f = "calc_pm",
    signature = c(x = "RetraiteEuroRest", y = "list"),
    def = function(x, y){
        # Verification inputs
        if (length(y) != 4){
          stop("[RetraiteEuroRest : calc_pm] : L'input y doit correspondre a une liste de longueur 4. \n")}
        # Verification des noms des elements de la liste
        if (sum(names(y) == c("ht","tx_cible", "coef_rente", "method")) != length(y)) {
          stop("[RetraiteEuroRest : calc_pm] : L'input y doit correspondre a une liste de longueur 4 de nom
               : ht, tx_cible, method. \n")}
        # Verification des types des elements de la liste
        if (! class(y[["ht"]]) == "HypTech")  {
          stop("[RetraiteEuroRest : calc_pm] : L'input y doit correspondre a une liste de longueur 4, de nom : ht, tx_cible, method, dont le type est : HypTech, list, matrix, character. \n")}
        if (! is.list(y[["tx_cible"]]))       {
          stop("[RetraiteEuroRest : calc_pm] : L'input y doit correspondre a une liste de longueur 4, de nom : ht, tx_cible, method, dont le type est : HypTech, list, matrix, character. \n")}
        if (! is.matrix(y[["coef_rente"]]))       {
          stop("[RetraiteEuroRest : calc_pm] : L'input y doit correspondre a une liste de longueur 4, de nom : ht, tx_cible, method, dont le type est : HypTech, list, matrix, character. \n")}
        if (! is.character(y[["method"]]) |  ! y[["method"]] %in% c("normal", "gar"))    {
          stop("[EpEuroInd : calc_pm] : L'input y['method'] doit ?tre une variable de type character et prenant pour valeur 'normal' ou 'gar'.")}

        # Affectation pour eviter la modification des fonctions
        ht        = y[["ht"]]
        tx_cible  = y[["tx_cible"]]
        coef_rente  = y[["coef_rente"]]
        method    = y[["method"]]

        # Seuil pour gerer les problemes d'arrondi
        SEUIL_ARRONDI <- 0.00001


        # Recuperer les numeros de colonnes des parametres utilises
        nb_mp          <- nrow(x@mp)
        nom_retraite   <- names(x@mp)
        num_mp         <- which(nom_retraite == "num_mp")
        num_rente      <- which(nom_retraite == "rente")
        num_rente_gar  <- which(nom_retraite == "rente_gar")
        num_age_1      <- which(nom_retraite == "age")
        num_gen_1      <- which(nom_retraite == "gen")
        num_statut     <- which(nom_retraite == "statut_rvs")
        num_tx_rvs     <- which(nom_retraite == "tx_rvs")
        num_tab_mort_1 <- which(nom_retraite == "num_tab_mort")
        num_age_2      <- which(nom_retraite == "age_rvs")
        num_gen_2      <- which(nom_retraite == "gen_rvs")
        num_tab_mort_2 <- which(nom_retraite == "num_tab_mort_rvs")
        num_nb_contr   <- which(nom_retraite == "nb_contr")
        num_pm         <- which(nom_retraite == "pm")
        num_tx_tech    <- which(nom_retraite == "tx_tech")
        num_freq_rente <- which(nom_retraite == "freq_rente")
        num_echu       <- which(nom_retraite == "echu")

        # Initialisation de la PM finale a 0
        pm_fin <- rep(0,nb_mp)

        # Recuperer la rente
        if(method == "normal"){ # calcul de la rente normal
            rente <- .subset2(x@mp, num_nb_contr) * .subset2(x@mp, num_rente)
        }else if (method == "gar"){ # calcul de la rente avec revalorisation garantie uniquement
            rente <- .subset2(x@mp, num_nb_contr) * .subset2(x@mp, num_rente_gar)
        }

        # Applique la method de calcul
        pm_fin <- coef_rente * rente

        # Evaluation des provisions mathematiques avant PB
        # Application du chargement sur arrerage
        pm_fin <- pm_fin / (1 + x@mp[["ch_arr"]])
        #calcul ax * rente
        pm_deb <- .subset2(x@mp, num_pm)
        # Application d'un seuil pour eviter les problemes d'arrondi
        pm_fin[which(abs(pm_fin) < SEUIL_ARRONDI)] <- 0


        # Message d'erreur si PM negative
        if( ! prod(pm_fin >= 0)) {
            stop("[PassifBase-calc_pm] : la valeur des PM retraite ne peut pas etre negative.")
        }

        # PM moyenne et taux de chargement
        pm_moy <- (pm_deb + pm_fin) / 2

        # EValuation du besoin de taux cible
        bes_tx_cible <- tx_cible[["tx_cible_an"]] * pm_fin

        # output
        return(list(method = method,
                    stock = list(
                        pm_deb = pm_deb,
                        pm_fin = pm_fin,
                        pm_moy = pm_moy
                    ),
                    flux = list(
                        rev_stock_brut = rep(0, nb_mp),
                        rev_stock_nette = rep(0, nb_mp),
                        # rev_total_nette = rev_total_nette,
                        enc_charg_stock = rep(0, nb_mp),
                        enc_charg_base_th = rep(0, nb_mp),
                        enc_charg_rmin_th = rep(0, nb_mp),
                        base_enc_th = rep(0, nb_mp),
                        soc_stock = rep(0, nb_mp),
                        # enc_charg = enc_charg,
                        # rach_charg = rach_charg,
                        # pri_chgt = pri_chgt,
                        # soc_stock = soc_stock,
                        it_tech_stock = rep(0, nb_mp),
                        it_tech = rep(0, nb_mp),
                        bes_tx_cible = bes_tx_cible
                    )
        ))
    }
)

