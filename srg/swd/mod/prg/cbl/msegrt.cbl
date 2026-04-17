       Identification Division.
       Program-Id.                                 msegrt             .
      *================================================================*
      *                                                                *
      *                    Modulo residente di segreteria              *
      *                                                                *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 27/06/91    *
      *                       Ultima revisione:    NdK del 29/07/24    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * A LINEA '6470' EVENTUALE VISUALIZZAZIONE !!!                   *
      *                                                                *
      *================================================================*
      *                                                                *
      * Tipi operazione previsti :                                     *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - OP :      Dichiarazione inizio utilizzo modulo segreteria    *
      *                                                                *
      *             Input  : s-ope = "OP"                              *
      *                                                                *
      *                      s-fun = Tipo di funzionamento             *
      *                              - F : Foreground                  *
      *                              - B : Background                  *
      *                                                                *
      *                      s-alf = Se foreground : Sigla del driver  *
      *                                              video per l'e-    *
      *                                              secuzione della   *
      *                                              open Video        *
      *                              Se background : pathname del fi-  *
      *                                              le che costitui-  *
      *                                              sce l'immagine    *
      *                                              della segreteria  *
      *                                              proveniente dal   *
      *                                              foreground        *
      *                                                                *
      *                      s-pat = Prefisso o postfisso unico per    *
      *                              files temporanei, max 20 carat-   *
      *                              teri.                             *
      *                                                                *
      *                      s-pmo = Host-Id                           *
      *                                                                *
      *                                                                *
      *             Output : se tipo funzionamento foreground          *
      *                                                                *
      *                      nessuno                                   *
      *                                                                *
      *                      se tipo funzionamento background          *
      *                                                                *
      *                      s-fun = Tipo di funzionamento             *
      *                              - B : Tutto OK                    *
      *                              - # : Errore grave                *
      *                                                                *
      *                      s-pmo = Pathname programma background     *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - CL :      Dichiarazione fine utilizzo modulo segreteria      *
      *                                                                *
      *             Input  : s-ope = "CL"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - U? :      Richiesta prefisso o postfisso unico per files     *
      *             temporanei                                         *
      *                                                                *
      *             Input  : s-ope = "U?"                              *
      *                                                                *
      *             Output : s-pat = Prefisso o postfisso unico        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - H? :      Richiesta Host-Id                                  *
      *                                                                *
      *             Input  : s-ope = "H?"                              *
      *                                                                *
      *             Output : s-pat = Host-Id                           *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - RT :      Dichiarazione tipo di runtime in uso               *
      *                                                                *
      *             Input  : s-ope = "RT"                              *
      *                                                                *
      *                      s-num = Tipo di runtime                   *
      *                              - 00 : Acucobol 85                *
      *                              - 01 : Austec RM Master Cobol 74  *
      *                              - 02 : Ryan-McFarland RM Cobol 85 *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - R? :      Richiesta tipo di runtime in uso                   *
      *                                                                *
      *             Input  : s-ope = "R?"                              *
      *                                                                *
      *             Output : s-num = Tipo di runtime                   *
      *                              - 00 : Acucobol 85                *
      *                              - 01 : Austec RM Master Cobol 74  *
      *                              - 02 : Ryan-McFarland RM Cobol 85 *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - TE :      Dichiarazione codice terminale in uso              *
      *                                                                *
      *             Input  : s-ope = "TE"                              *
      *                                                                *
      *                      s-alf = Indirizzo IP eventuale            *
      *                                                                *
      *                      s-ter = Codice terminale                  *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - UT :      Dichiarazione codice utente in uso                 *
      *                                                                *
      *             Input  : s-ope = "UT"                              *
      *                                                                *
      *                      s-ute = Codice utente                     *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - ST :      Dichiarazione codice stampante e codice stampante  *
      *             locale associate all'utente                        *
      *             tente                                              *
      *                                                                *
      *             Input  : s-ope = "ST"                              *
      *                                                                *
      *                      s-asx = Codice stampante                  *
      *                                                                *
      *                      s-adx = Codice stampante locale           *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - S? :      Richiesta codice stampante e codice stampante lo-  *
      *             cale associate all'utente                          *
      *                                                                *
      *             Input  : s-ope = "S?"                              *
      *                                                                *
      *             Output : s-asx = Codice stampante                  *
      *                                                                *
      *                      s-adx = Codice stampante locale           *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - LU :      Dichiarazione ragione sociale licenziatario        *
      *                                                                *
      *             Input  : s-ope = "LU"                              *
      *                                                                *
      *                      s-alf = Ragione sociale licenziatario     *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - L? :      Richiesta ragione sociale licenziatario            *
      *                                                                *
      *             Input  : s-ope = "L?"                              *
      *                                                                *
      *             Output : s-alf = Ragione sociale licenziatario     *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - AZ :      Dichiarazione codice azienda in uso                *
      *                                                                *
      *             Input  : s-ope = "AZ"                              *
      *                                                                *
      *                      s-azi = Codice azienda                    *
      *                                                                *
      *                      s-alf = Descrizione azienda               *
      *                              s-asx = allineata a sinistra      *
      *                              s-adx = allineata al centro       *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - D> :      Dichiarazione codice dipendenza in uso             *
      *                                                                *
      *             Input  : s-ope = "D>"                              *
      *                                                                *
      *                      s-car = Numero di dipendenze esistenti    *
      *                              per l'azienda                     *
      *                                                                *
      *                      s-num = Codice dipendenza                 *
      *                                                                *
      *                      s-alf = Descrizione dipendenza allineata  *
      *                              a sinistra                        *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - D< :      Richiesta codice dipendenza in uso                 *
      *                                                                *
      *             Input  : s-ope = "D<"                              *
      *                                                                *
      *             Output : s-num = Codice dipendenza                 *
      *                                                                *
      *                      s-alf = Descrizione dipendenza allineata  *
      *                              a sinistra                        *
      *                                                                *
      *             NOTA IMPORTANTE  : presume che si sia entrati      *
      *             ---------------    almeno in un programma che      *
      *             !!!!!!!!!!!!!!!    richieda il codice dipendenza   *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - D? :      Richiesta codici dipendenza relativi al codice a-  *
      *             zienda in uso su cui l'utente in uso e' abilitato  *
      *             ad operare                                         *
      *                                                                *
      *             Input  : s-ope = "D?"                              *
      *                                                                *
      *             Output : s-num = Numero di dipendenze su cui l'u-  *
      *                              tente e' abilitato ad operare     *
      *                              Se zero significa tutte           *
      *                                                                *
      *                      s-alf = Codice delle dipendenze su cui    *
      *                              l'utente e' abilitato ad operare  *
      *                              Ridefinito come una sequenza di   *
      *                              max 20 elementi numerici di due   *
      *                              cifre ciascuno, ognuno indicante  *
      *                              un codice dipendenza su cui l'u-  *
      *                              tente e' abilitato ad operare,    *
      *                              impaccati a sinistra              *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - FP :      Filtro di un pathname di esecuzione per ottenere   *
      *             un pathname di esecuzione sostitutivo              *
      *                                                                *
      *             Input  : s-ope = "FP"                              *
      *                                                                *
      *                      s-pat = Pathname originale                *
      *                                                                *
      *             Output : s-pat = Pathname sostitutivo              *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - P? : Test se programma applicativo attivo o no               *
      *                                                                *
      *        Input  : s-ope = "P?"                                   *
      *                                                                *
      *                 s-pro = Sigla interna del programma            *
      *                                                                *
      *        Output : s-liv = Livello di profondita applicativa      *
      *                                                                *
      *                         Nota : se questo valore e' posto a ze- *
      *                                ro significa che il  programma  *
      *                                non e' attualmente attivo       *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - S0 : Inibizione forzata del salvataggio forzato dell'immagi- *
      *        ne video per la prossima richiesta "P+" "P#" "Q+" "Q#"  *
      *                                                                *
      *        Input  : s-ope = "S0"                                   *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - P+ : Memorizzazione inizio programma applicativo, con modo   *
      *        di funzionamento a 80 colonne                           *
      *                                                                *
      *        Input  : s-ope = "P+"                                   *
      *                                                                *
      *                 s-sap = Sigla sistema applicativo              *
      *                                                                *
      *                 s-arg = Sigla area gestionale                  *
      *                                                                *
      *                 s-set = Sigla settore gestionale               *
      *                                                                *
      *                 s-fas = Sigla fase gestionale                  *
      *                                                                *
      *                 s-pro = Sigla interna del programma            *
      *                                                                *
      *                 s-svv = Flag di Save Video                     *
      *                         - S      : Si                          *
      *                         - Spaces : No                          *
      *                                                                *
      *        Output : s-pst = Program start time del programma       *
      *                                                                *
      *                 s-liv = Livello di profondita applicativa      *
      *                                                                *
      *                 s-sts = Flag di visualizzazione forzata        *
      *                                                                *
      *                         Nota : se questo valore e' posto a ze- *
      *                                ro significa che il  programma  *
      *                                e' stato rifiutato              *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - P# : Memorizzazione inizio programma applicativo, con modo   *
      *        di funzionamento a 132 colonne                          *
      *                                                                *
      *        Nota   : Tutto come funzione "P+", escluso il tipo ope- *
      *                 razione                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - Q+ : Memorizzazione inizio programma applicativo ma senza    *
      *        controllo di password e riservatezza, con modo di       *
      *        funzionamento a 80 colonne                              *
      *                                                                *
      *        Input  : s-ope = "Q+"                                   *
      *                                                                *
      *                 s-sap = Sigla sistema applicativo              *
      *                                                                *
      *                 s-arg = Sigla area gestionale                  *
      *                                                                *
      *                 s-set = Sigla settore gestionale               *
      *                                                                *
      *                 s-fas = Sigla fase gestionale                  *
      *                                                                *
      *                 s-pro = Sigla interna del programma            *
      *                                                                *
      *                 s-svv = Flag di Save Video                     *
      *                         - S      : Si                          *
      *                         - Spaces : No                          *
      *                                                                *
      *        Output : s-pst = Program start time del programma       *
      *                                                                *
      *                 s-liv = Livello di profondita applicativa      *
      *                                                                *
      *                         Nota : se questo valore e' posto a ze- *
      *                                ro significa che il  programma  *
      *                                e' stato rifiutato              *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - Q# : Memorizzazione inizio programma applicativo ma senza    *
      *        controllo di password e riservatezza, con modo di       *
      *        funzionamento a 132 colonne                             *
      *                                                                *
      *        Nota   : Tutto come funzione "Q+", escluso il tipo ope- *
      *                 razione                                        *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - P- : Memorizzazione fine programma applicativo ed esecuzione *
      *        cancellazione programmi di i-o "ioxyz" relativi a files *
      *        movimentati ma non piu' necessari                       *
      *                                                                *
      *        Input  : s-ope = "P-"                                   *
      *                                                                *
      *                 s-pro = Codice programma applicativo           *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - IO : Richiesta informazioni relative a moduli di i-o "ioxyz" *
      *        ancora in essere                                        *
      *                                                                *
      *        Input  : s-ope = "IO"                                   *
      *                                                                *
      *        Output : s-num = Numero moduli di i-o "ioxyz" ancora in *
      *                         essere, con file-open-counter maggiore *
      *                         di zero                                *
      *                                                                *
      *                 s-alf = Sigle dei primi 10 filenames corri-    *
      *                         spondenti ai moduli di i-o ancora      *
      *                         in essere, e dei relativi file-open    *
      *                         counters                               *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - IX : Cancellazione moduli di i-o obsoleti                    *
      *                                                                *
      *        Input  : s-ope = "IX"                                   *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - BF : Richiesta autorizzazione esecuzione in background       *
      *                                                                *
      *        Input  : s-ope = "BF"                                   *
      *                                                                *
      *        Output : s-snb = Tipo esecuzione da seguire             *
      *                         - B : In background                    *
      *                         - F : In foreground                    *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - B+ : Memorizzazione inizio programma in background           *
      *                                                                *
      *        Input  : s-ope = "B+"                                   *
      *                                                                *
      *                 s-npb = Codice programma in background         *
      *                                                                *
      *                 s-pmo = Pathname programma in background       *
      *                                                                *
      *                 s-alf = Descrizione programma in background    *
      *                                                                *
      *        Output : s-sdt = System date and time attuale           *
      *                                                                *
      *                         Nota : se questo valore e' posto a ze- *
      *                                ro significa che il  programma  *
      *                                e' stato rifiutato              *
      *                                                                *
      *                 s-pat = Pathname del file temporaneo su cui    *
      *                         e' stato rediretto lo standard-out-    *
      *                         put                                    *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - B- : Memorizzazione fine programma in background             *
      *                                                                *
      *        Input  : s-ope = "B-"                                   *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - M+ : Memorizzazione inizio modulo transiente                 *
      *                                                                *
      *        Input  : s-ope = "M+"                                   *
      *                                                                *
      *                 s-pmo = Pathname modulo transiente             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - M- : Memorizzazione fine modulo transiente                   *
      *                                                                *
      *        Input  : s-ope = "M-"                                   *
      *                                                                *
      *                 s-pmo = Pathname modulo transiente             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - PN : Inizializzazione pathnames per l'azienda                *
      *                                                                *
      *        Input  : s-ope = "PN"                                   *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - PG : Estrazione pathname memorizzato                         *
      *                                                                *
      *        Input  : s-ope = "PG"                                   *
      *                                                                *
      *                 s-nam = Name                                   *
      *                                                                *
      *        Output : s-pat = Pathname                               *
      *                                                                *
      *                 s-sts = Status di uscita                       *
      *                         - Spaces : Nessuna particolarita'      *
      *                         - KS     : E' stata richiesta una o-   *
      *                                    perazione di sequenzializ-  *
      *                                    zazione del file alla ope-  *
      *                                    razione di Open             *
      *                         - SK     : E' stata richiesta una o-   *
      *                                    perazione di indicizzazio-  *
      *                                    ne senza sostituzione del   *
      *                                    file alla operazione Open   *
      *                         - SJ     : E' stata richiesta una o-   *
      *                                    perazione di indicizzazio-  *
      *                                    ne con sostituzione del     *
      *                                    file alla operazione Open   *
      *                         - KK     : E' stata richiesta una o-   *
      *                                    perazione su se' stesso     *
      *                                    per il file alla operazio-  *
      *                                    ne Open                     *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - PF : Estrazione attributi particolari inerenti la prossima   *
      *        operazione di open del file                             *
      *                                                                *
      *        Input  : s-ope = "PF"                                   *
      *                                                                *
      *        Output : s-num = Tipo di filtro da applicare            *
      *                                                                *
      *                 s-pat = Filtro da applicare                    *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - .F : Estrazione pathname di base per files azienda           *
      *                                                                *
      *        Input  : s-ope = ".F"                                   *
      *                                                                *
      *        Output : s-pat = Pathname                               *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - .P : Estrazione pathname di base per print-files             *
      *                                                                *
      *        Input  : s-ope = ".P"                                   *
      *                                                                *
      *        Output : s-pat = Pathname                               *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - .S : Estrazione pathname di base per spool-files             *
      *                                                                *
      *        Input  : s-ope = ".S"                                   *
      *                                                                *
      *        Output : s-pat = Pathname                               *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - F? : Estrazione contatore open file memorizzato              *
      *                                                                *
      *        Input  : s-ope = "F?"                                   *
      *                                                                *
      *                 s-nam = Name                                   *
      *                                                                *
      *        Output : s-foc = File open counter attuale              *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - FO : Dichiarazione pathname modulo oggetto di i-o            *
      *                                                                *
      *        Input  : s-ope = "FO"                                   *
      *                                                                *
      *                 s-nam = Name                                   *
      *                                                                *
      *                 s-pmo = Pathname modulo oggetto di i-o         *
      *                                                                *
      *        Output : s-foc = File open counter attuale              *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - F+ : Incremento contatore open file memorizzato              *
      *                                                                *
      *        Input  : s-ope = "F+"                                   *
      *                                                                *
      *                 s-nam = Name                                   *
      *                                                                *
      *        Output : s-foc = File open counter attuale              *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - F- : Decremento contatore open file memorizzato              *
      *                                                                *
      *        Input  : s-ope = "F-"                                   *
      *                                                                *
      *                 s-nam = Name                                   *
      *                                                                *
      *        Output : s-foc = File open counter attuale              *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - FE : Messaggio di i-o fatal error                            *
      *                                                                *
      *        Input  : s-ope = "FE"                                   *
      *                                                                *
      *                 s-nam = Name                                   *
      *                                                                *
      *                 s-pat = Pathname                               *
      *                                                                *
      *                 s-sts = I-O status cobol                       *
      *                                                                *
      *        Output : nessuno - stop run                             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - IG : Estrazione informazioni generali                        *
      *                                                                *
      *        Input  : s-ope = "IG"                                   *
      *                                                                *
      *        Output : s-sdt = System date and time attuale           *
      *                                                                *
      *                 s-fun = Tipo funzionamento : F se foreground   *
      *                                              B se background   *
      *                                                                *
      *                 s-snb = Background in esecuzione : S se Si     *
      *                                                    N se No     *
      *                                                                *
      *                 s-npb = Nome programma background in esecuz.   *
      *                                                                *
      *                 s-asx = Descrizione progr. backg. in esecuz.   *
      *                                                                *
      *                 s-psb = Program start time backg. in esecuz.   *
      *                                                                *
      *                 s-ter = Codice terminale attualmente in uso    *
      *                                                                *
      *                 s-ute = Codice utente    attualmente in uso    *
      *                                                                *
      *                 s-azi = Codice azienda   attualmente in uso    *
      *                                                                *
      *                 s-sap = Sigla sis. appl. attualmente in uso    *
      *                                                                *
      *                 s-arg = Sigla area gest. attualmente in uso    *
      *                                                                *
      *                 s-set = Sigla settore    attualmente in uso    *
      *                                                                *
      *                 s-fas = Sigla fase       attualmente in uso    *
      *                                                                *
      *                 s-pro = Codice programma attualmente in uso    *
      *                                                                *
      *                 s-pst = Program start time programma in uso    *
      *                                                                *
      *                 s-liv = Livello di profondita applicativa      *
      *                                                                *
      *                 s-adx = Indirizzo Mail utente (II parte s-alf) *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - IL : Estrazione informazioni su livello di profondita'       *
      *                                                                *
      *        Input  : s-ope = "IL"                                   *
      *                                                                *
      *                 s-liv = Livello di profondita interessato      *
      *                                                                *
      *        Output : s-sap = Sigla sistema applicativo corrispon-   *
      *                         dente al livello di profondita' inte-  *
      *                         ressato                                *
      *                                                                *
      *                 s-arg = Sigla area gestionale corrisponte al   *
      *                         livello di profondita' interessato     *
      *                                                                *
      *                 s-set = Sigla settore gestionale corrisponte   *
      *                         al livello di profondita' interessato  *
      *                                                                *
      *                 s-fas = Sigla fase gestionale corrisponte al   *
      *                         livello di profondita' interessato     *
      *                                                                *
      *                 s-pro = Codice programma corrispondente al     *
      *                         livello di profondita' interessato     *
      *                                                                *
      *                 s-pst = Program start time del programma al    *
      *                         livello di profondita' interessato     *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - IA : Estrazione informazioni su azienda in uso               *
      *                                                                *
      *        Input  : s-ope = "IA"                                   *
      *                                                                *
      *        Output : s-azi = Codice azienda                         *
      *                                                                *
      *                 s-alf = Descrizione azienda                    *
      *                          s-asx = allineata a sinistra          *
      *                          s-adx = allineata al centro           *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - DT : Estrazione system date and time attuale                 *
      *                                                                *
      *        Input  : s-ope = "DT"                                   *
      *                                                                *
      *        Output : s-sdt = System date and time attuale           *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - GS : Estrazione giorno della settimana                       *
      *                                                                *
      *        Input  : s-ope = "GS"                                   *
      *                                                                *
      *                 s-dat = Data s.aa.mm.gg per il calcolo         *
      *                                                                *
      *                                                                *
      *        Output : s-num = numero giorno della settimana          *
      *                         secondo la tabella :                   *
      *                                                                *
      *                          -  1 : Lunedi'                        *
      *                          -  2 : Martedi'                       *
      *                          -  3 : Mercoledi'                     *
      *                          -  4 : Giovedi'                       *
      *                          -  5 : Venerdi'                       *
      *                          -  6 : Sabato                         *
      *                          -  7 : Domenica                       *
      *                                                                *
      *                 s-alf = giorno della settimana alfanumerico    *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - LM : Estrazione literal del mese                             *
      *                                                                *
      *        Input  : s-ope = "LM"                                   *
      *                                                                *
      *                 s-tip = tipo di literal                        *
      *                                                                *
      *                          - 'E' : Esteso                        *
      *                          - 'U' : Esteso, in uppercase          *
      *                          - 'A' : Abbreviato                    *
      *                                 (3 caratteri in uppercase)     *
      *                                                                *
      *                 s-num = indice del mese da 1 a 12              *
      *                                                                *
      *                                                                *
      *        Output : s-alf = Literal del mese                       *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - G+ : Incremento di una data in giorni                        *
      *                                                                *
      *        Input  : s-ope = "G+"                                   *
      *                                                                *
      *                 s-dat = Data s.aa.mm.gg per il calcolo         *
      *                                                                *
      *                 s-num = numero giorni di incremento            *
      *                                                                *
      *        Output : s-dat = Data incrementata                      *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - G- : Decremento di una data in giorni                        *
      *                                                                *
      *        Input  : s-ope = "G-"                                   *
      *                                                                *
      *                 s-dat = Data s.aa.mm.gg per il calcolo         *
      *                                                                *
      *                 s-num = numero giorni di decremento            *
      *                                                                *
      *        Output : s-dat = Data decrementata                      *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - SA : Settimana nell'anno                                     *
      *                                                                *
      *        Input  : s-ope = "SA"                                   *
      *                                                                *
      *                 s-dat = Data s.aa.mm.gg per il calcolo         *
      *                                                                *
      *        Output : s-num = Numero della settimana (1..54)         *
      *                                                                *
      *                 s-dat = I giorno della settimana in cui cade   *
      *                         il giorno in input                     *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - NS : Normalizzazione secolo in data                          *
      *                                                                *
      *        Input  : s-ope = "NS"                                   *
      *                                                                *
      *                 s-dat = Data s.aa.mm.gg da normalizzare        *
      *                                                                *
      *        Output : s-dat = Data s.aa.mm.gg con s. normalizzato    *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - CD : Controllo data                                          *
      *                                                                *
      *        Input  : s-ope = "CD"                                   *
      *                                                                *
      *                 s-dat = Data da controllare s.aa.mm.gg         *
      *                                                                *
      *        Output : s-sts = Spaces : data corretta                 *
      *                         ##     : data non corretta             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - W1 : Wait di un secondo                                      *
      *                                                                *
      *        Input  : s-ope = "W1"                                   *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - WT : Wait di 'n' secondi                                     *
      *                                                                *
      *        Input  : s-ope = "WT"                                   *
      *                                                                *
      *                 s-num = numero di secondi                      *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - S> : Ingresso parametri di selezione stampa                  *
      *                                                                *
      *        Input  : s-ope = "S>"                                   *
      *                                                                *
      *                 s-num = numero segmento da trattare            *
      *                                                                *
      *                 s-alf = segmento di 80 caratteri di parametri  *
      *                                                                *
      *                 s-sts = tipo di stampa selezionato, solo se    *
      *                         numero segmento pari a 1               *
      *                          - I : Immediata                       *
      *                          - D : Su disco                        *
      *                          - V : A video                         *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - S< : Uscita parametri di selezione stampa                    *
      *                                                                *
      *        Input  : s-ope = "S<"                                   *
      *                                                                *
      *                 s-num = numero segmento da trattare            *
      *                                                                *
      *        Output : s-alf = segmento di 80 caratteri di parametri  *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - T> : Ingresso parametri di tipo stampa                       *
      *                                                                *
      *        Input  : s-ope = "T>"                                   *
      *                                                                *
      *                 s-alf = Tipo di stampa                         *
      *                          - I : Immediata                       *
      *                          - D : Su disco                        *
      *                          - V : A video                         *
      *                                                                *
      *                 s-num = numero del print-file se tipo D/V      *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - T< : Uscita parametri di tipo stampa                         *
      *                                                                *
      *        Input  : s-ope = "T<"                                   *
      *                                                                *
      *        Output : s-alf = Tipo di stampa                         *
      *                          - I : Immediata                       *
      *                          - D : Su disco                        *
      *                          - V : A video                         *
      *                                                                *
      *                 s-num = numero del print-file se tipo D/V      *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - UP : Richiesta di un pathname unico per file temporaneo      *
      *                                                                *
      *        Input  : s-ope = "UP"                                   *
      *                                                                *
      *        Output : s-pat = pathname unico                         *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - FF : Duplicazione di un file                                 *
      *                                                                *
      *        Input  : s-ope = "FF"                                   *
      *                                                                *
      *                 s-nam = name del file originale                *
      *                                                                *
      *                 s-alf = name del file duplicato                *
      *                                                                *
      *                 s-sts = tipo di file                           *
      *                         - K : key                              *
      *                         - R : relative                         *
      *                         - S : sequential                       *
      *                                                                *
      *        Output : s-pat = Pathname del file temporaneo su cui    *
      *                         e' stato rediretto lo standard-out-    *
      *                         put                                    *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - FD : Delete di un file                                       *
      *                                                                *
      *        Input  : s-ope = "FD"                                   *
      *                                                                *
      *                 s-nam = name del file da deletare              *
      *                                                                *
      *                 s-sts = tipo di file                           *
      *                         - K : key                              *
      *                         - R : relative                         *
      *                         - S : sequential                       *
      *                                                                *
      *        Output : s-pat = Pathname del file temporaneo su cui    *
      *                         e' stato rediretto lo standard-out-    *
      *                         put                                    *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - PD : Delete di un pathname                                   *
      *                                                                *
      *        Input  : s-ope = "PD"                                   *
      *                                                                *
      *                 s-pat = pathname del file da deletare          *
      *                                                                *
      *                 s-sts = tipo di file                           *
      *                         - K : key                              *
      *                         - R : relative                         *
      *                         - S : sequential                       *
      *                                                                *
      *        Output : s-pat = Pathname del file temporaneo su cui    *
      *                         e' stato rediretto lo standard-out-    *
      *                         put                                    *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - FR : Rename di un file                                       *
      *                                                                *
      *        Input  : s-ope = "FR"                                   *
      *                                                                *
      *                 s-nam = name del file originale                *
      *                                                                *
      *                 s-alf = name del file ridenominato             *
      *                                                                *
      *                 s-sts = tipo di file                           *
      *                         - K : key                              *
      *                         - R : relative                         *
      *                         - S : sequential                       *
      *                                                                *
      *        Output : s-pat = Pathname del file temporaneo su cui    *
      *                         e' stato rediretto lo standard-out-    *
      *                         put                                    *
      *                                                                *
      *                 N.B. : Il file originale prende il nuovo nome  *
      *                        del file ridenominato. Se esisteva un   *
      *                        file gia' con quel nome, esso viene     *
      *                        distrutto.                              *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - RF : Rename di un file, ma in senso inverso alla funzione    *
      *        precedente di Rename file                               *
      *                                                                *
      *        Input  : s-ope = "RF"                                   *
      *                                                                *
      *                 s-nam = name del file originale                *
      *                                                                *
      *                 s-alf = name del file ridenominato             *
      *                                                                *
      *                 s-sts = tipo di file                           *
      *                         - K : key                              *
      *                         - R : relative                         *
      *                         - S : sequential                       *
      *                                                                *
      *        Output : s-pat = Pathname del file temporaneo su cui    *
      *                         e' stato rediretto lo standard-out-    *
      *                         put                                    *
      *                                                                *
      *                 N.B. : Il file ridenominato prende il nuovo    *
      *                        nome del file originale. Se esisteva    *
      *                        un file gia' con quel nome, esso vie-   *
      *                        ne distrutto.                           *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - PV :      Scrittura di una variabile di i.p.c.               *
      *                                                                *
      *             Input  : s-ope = "PV"                              *
      *                                                                *
      *                      s-var = nome della variabile max 20 car.  *
      *                                                                *
      *                      s-dop = destinazione della variabile      *
      *                              G  : globale                      *
      *                              +  : per livello successivo       *
      *                              -  : per livello precedente       *
      *                              =  : per livello attuale          *
      *                                                                *
      *                      s-tip = tipo variabile                    *
      *                              A  : alfanumerica                 *
      *                              N  : numerica                     *
      *                              D  : data                         *
      *                                                                *
      *                      s-car = numero caratteri                  *
      *                              se s-tip = A : 01-80              *
      *                              se s-tip = N : 01-13              *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-dec = numero decimali                   *
      *                              se s-tip = A : non significativo  *
      *                              se s-tip = N : 00-05              *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-sgn = presenza segno algebrico          *
      *                              se s-tip = A : non significativo  *
      *                              se s-tip = N :      S = Si        *
      *                                             Spaces = No        *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-alf = valore della variabile se questa  *
      *                              e' di tipo alfanumerico           *
      *                                                                *
      *                      s-num = valore della variabile se questa  *
      *                              e' di tipo numerico               *
      *                                                                *
      *                      s-dat = valore della variabile se questa  *
      *                              e' di tipo data                   *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - GV        Lettura di una variabile di i.p.c.                 *
      *                                                                *
      *             Input  : s-ope = "GV"                              *
      *                                                                *
      *                      s-var = nome della variabile max 20 car.  *
      *                                                                *
      *                      s-dop = provenienza della variabile       *
      *                              G  : globale                      *
      *                              +  : dal livello successivo       *
      *                              -  : dal livello precedente       *
      *                              =  : dal livello attuale          *
      *                                                                *
      *             Output : s-ves = segnale se variabile esistente    *
      *                              spaces     : si                   *
      *                              altrimenti : no                   *
      *                                                                *
      *                      s-tip = tipo variabile                    *
      *                              A  : alfanumerica                 *
      *                              N  : numerica                     *
      *                              D  : data                         *
      *                                                                *
      *                      s-car = numero caratteri                  *
      *                              se s-tip = A : 01-80              *
      *                              se s-tip = N : 01-13              *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-dec = numero decimali                   *
      *                              se s-tip = A : non significativo  *
      *                              se s-tip = N : 00-05              *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-sgn = presenza segno algebrico          *
      *                              se s-tip = A : non significativo  *
      *                              se s-tip = N :      S = Si        *
      *                                             Spaces = No        *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-alf = valore della variabile se questa  *
      *                              e' di tipo alfanumerico           *
      *                                                                *
      *                      s-num = valore della variabile se questa  *
      *                              e' di tipo numerico               *
      *                                                                *
      *                      s-dat = valore della variabile se questa  *
      *                              e' di tipo data                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - CV        Cancellazione di una variabile di i.p.c.           *
      *                                                                *
      *             Input  : s-ope = "CV"                              *
      *                                                                *
      *                      s-var = nome della variabile max 20 car.  *
      *                                                                *
      *                      s-dop = provenienza della variabile       *
      *                              G  : globale                      *
      *                              +  : dal livello successivo       *
      *                              -  : dal livello precedente       *
      *                              =  : dal livello attuale          *
      *                                                                *
      *             Output : s-ves = segnale se variabile esistente    *
      *                              spaces     : si                   *
      *                              altrimenti : no                   *
      *                                                                *
      *                      s-tip = tipo variabile                    *
      *                              A  : alfanumerica                 *
      *                              N  : numerica                     *
      *                              D  : data                         *
      *                                                                *
      *                      s-car = numero caratteri                  *
      *                              se s-tip = A : 01-80              *
      *                              se s-tip = N : 01-13              *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-dec = numero decimali                   *
      *                              se s-tip = A : non significativo  *
      *                              se s-tip = N : 00-05              *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-sgn = presenza segno algebrico          *
      *                              se s-tip = A : non significativo  *
      *                              se s-tip = N :      S = Si        *
      *                                             Spaces = No        *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-alf = valore della variabile se questa  *
      *                              e' di tipo alfanumerico           *
      *                                                                *
      *                      s-num = valore della variabile se questa  *
      *                              e' di tipo numerico               *
      *                                                                *
      *                      s-dat = valore della variabile se questa  *
      *                              e' di tipo data                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - PS        Lettura di una personalizzazione generale          *
      *                                                                *
      *             Input  : s-ope = "PS"                              *
      *                                                                *
      *                      s-alf = nome della personalizzazione      *
      *                              max 10 caratteri                  *
      *                                                                *
      *                              snx-azi : si/no visualizzazione   *
      *                                        codice azienda          *
      *                                                                *
      *                              tem-spl : template per il lancio  *
      *                                        dello spooler di stam-  *
      *                                        pa                      *
      *                                                                *
      *                              iod-lor : incremento o decremen-  *
      *                                        to per Left/Rght in vi- *
      *                                        sualizzazione archivi   *
      *                                        di stampa               *
      *                                                                *
      *                              tem-bss : template per il blocco  *
      *                                        dello spooler di stam-  *
      *                                        pa                      *
      *                                                                *
      *                              cnt-vpf : parametri per il con-   *
      *                                        trollo della visualiz-  *
      *                                        zazione archivi stampa  *
      *                                                                *
      *                              cnt-pg1 : parametro generale li-  *
      *                                        bero 1                  *
      *                                                                *
      *                              cnt-pg2 : parametro generale li-  *
      *                                        bero 2                  *
      *                                                                *
      *                              cnt-pg3 : parametro generale li-  *
      *                                        bero 3                  *
      *                                                                *
      *                              vpf-24l : Si/No visualizzazione   *
      *                                        archivi stampa su 24    *
      *                                        linee                   *
      *                                         - S : Si               *
      *                                         - N : No               *
      *                                                                *
      *                              scr-132 : Si/No trattamento dello *
      *                                        schermo ammissibile su  *
      *                                        132 colonne             *
      *                                         - S : Si               *
      *                                         - N : No               *
      *                                                                *
      *                              msa-isp : mail server SMTP        *
      *                                                                *
      *                              uid-ssm : user ID server SMTP     *
      *                                                                *
      *                              pwd-ssm : password server SMTP    *
      *                                                                *
      *                              pdf-pde : password documenti PDF  *
      *                                                                *
      *                              asd-ssm : agente invio documenti  *
      *                                                                *
      *                              pwd-sql : password per MySQL      *
      *                                                                *
      *                                                                *
      *             Output : s-num = Valore delle personalizzazione    *
      *                              se di tipo numerico               *
      *                                                                *
      *                      s-alf = Valore delle personalizzazione    *
      *                              se di tipo alfanumerico           *
      *                                                                *
      *                      s-dat = Valore delle personalizzazione    *
      *                              se di tipo data                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - P:          Lettura di una personalizzazione                 *
      *                                                                *
      *             Input  : s-ope = "P:"                              *
      *                                                                *
      *                      s-alf = nome della personalizzazione      *
      *                              max 32 caratteri                  *
      *                                                                *
      *             Output : s-ves = personalizzazione esistente       *
      *                              spaces     : si                   *
      *                              altrimenti : no                   *
      *                                                                *
      *                      s-tip = tipo personalizzazione            *
      *                              A  : alfanumerica                 *
      *                              N  : numerica                     *
      *                              D  : data                         *
      *                                                                *
      *                      s-car = numero caratteri                  *
      *                              se s-tip = A : 01-80              *
      *                              se s-tip = N : 01-13              *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-dec = numero decimali                   *
      *                              se s-tip = A : non significativo  *
      *                              se s-tip = N : 00-05              *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-sgn = presenza segno algebrico          *
      *                              se s-tip = A : non significativo  *
      *                              se s-tip = N :      S = Si        *
      *                                             Spaces = No        *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-alf = valore della personalizzazione se *
      *                              e' di tipo alfanumerico           *
      *                                                                *
      *                      s-num = valore della personalizzazione se *
      *                              e' di tipo numerico               *
      *                                                                *
      *                      s-dat = valore della personalizzazione se *
      *                              e' di tipo data                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - Ps        Start per lettura di una personalizzazione multi-  *
      *             pla                                                *
      *                                                                *
      *             Input  : s-ope = "Ps"                              *
      *                                                                *
      *                      s-alf = nome della personalizzazione      *
      *                              max 32 caratteri                  *
      *                                                                *
      *             Output : s-ves = personalizzazione esistente       *
      *                              spaces     : si                   *
      *                              altrimenti : no : at end          *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - Pn        Lettura sequenziale di una personalizzazione mul-  *
      *             tipla                                              *
      *                                                                *
      *             Input  : s-ope = "Pn"                              *
      *                                                                *
      *             Output : s-ves = personalizzazione esistente       *
      *                              spaces     : si                   *
      *                              altrimenti : no : at end          *
      *                                                                *
      *                      s-tip = tipo personalizzazione            *
      *                              A  : alfanumerica                 *
      *                              N  : numerica                     *
      *                              D  : data                         *
      *                                                                *
      *                      s-car = numero caratteri                  *
      *                              se s-tip = A : 01-80              *
      *                              se s-tip = N : 01-13              *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-dec = numero decimali                   *
      *                              se s-tip = A : non significativo  *
      *                              se s-tip = N : 00-05              *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-sgn = presenza segno algebrico          *
      *                              se s-tip = A : non significativo  *
      *                              se s-tip = N :      S = Si        *
      *                                             Spaces = No        *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-alf = valore della personalizzazione se *
      *                              e' di tipo alfanumerico           *
      *                                                                *
      *                      s-num = valore della personalizzazione se *
      *                              e' di tipo numerico               *
      *                                                                *
      *                      s-dat = valore della personalizzazione se *
      *                              e' di tipo data                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - Px        Richiesta del numero progressivo corrispondente    *
      *             all'ultima personalizzazione multipla letta        *
      *                                                                *
      *             Input  : s-ope = "Px"                              *
      *                                                                *
      *             Output : s-num = numero progressivo della persona- *
      *                              lizzazione                        *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - R:        Lettura di una referenza                           *
      *                                                                *
      *             Input  : s-ope = "R:"                              *
      *                                                                *
      *                      s-alf = nome della referenza              *
      *                              max 32 caratteri                  *
      *                                                                *
      *                      s-num = numero progressivo, solo se re-   *
      *                              ferenza multipla                  *
      *                                                                *
      *             Output : s-ves = referenza esistente               *
      *                              spaces     : si                   *
      *                              altrimenti : no (oppure at end se *
      *                                               refer. multipla) *
      *                                                                *
      *                      s-tip = tipo referenza                    *
      *                              A  : alfanumerica                 *
      *                              N  : numerica                     *
      *                              D  : data                         *
      *                                                                *
      *                      s-car = numero caratteri                  *
      *                              se s-tip = A : 01-80              *
      *                              se s-tip = N : 01-13              *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-dec = numero decimali                   *
      *                              se s-tip = A : non significativo  *
      *                              se s-tip = N : 00-05              *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-sgn = presenza segno algebrico          *
      *                              se s-tip = A : non significativo  *
      *                              se s-tip = N :      S = Si        *
      *                                             Spaces = No        *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-alf = valore della referenza se e' di   *
      *                              tipo alfanumerico                 *
      *                                                                *
      *                      s-num = valore della referenza se e' di   *
      *                              tipo numerico                     *
      *                                                                *
      *                      s-dat = valore della referenza se e' di   *
      *                              tipo data                         *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - Rs        Start per lettura di una referenza multipla        *
      *                                                                *
      *             Input  : s-ope = "Rs"                              *
      *                                                                *
      *                      s-alf = nome della referenza              *
      *                              max 32 caratteri                  *
      *                                                                *
      *             Output : s-ves = referenza esistente               *
      *                              spaces     : si                   *
      *                              altrimenti : no : at end          *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - Rn        Lettura sequenziale di una referenza multipla      *
      *                                                                *
      *             Input  : s-ope = "Rn"                              *
      *                                                                *
      *             Output : s-ves = referenza esistente               *
      *                              spaces     : si                   *
      *                              altrimenti : no : at end          *
      *                                                                *
      *                      s-tip = tipo referenza                    *
      *                              A  : alfanumerica                 *
      *                              N  : numerica                     *
      *                              D  : data                         *
      *                                                                *
      *                      s-car = numero caratteri                  *
      *                              se s-tip = A : 01-80              *
      *                              se s-tip = N : 01-13              *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-dec = numero decimali                   *
      *                              se s-tip = A : non significativo  *
      *                              se s-tip = N : 00-05              *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-sgn = presenza segno algebrico          *
      *                              se s-tip = A : non significativo  *
      *                              se s-tip = N :      S = Si        *
      *                                             Spaces = No        *
      *                              se s-tip = D : non significativo  *
      *                                                                *
      *                      s-alf = valore della referenza se e' di   *
      *                              tipo alfanumerico                 *
      *                                                                *
      *                      s-num = valore della referenza se e' di   *
      *                              tipo numerico                     *
      *                                                                *
      *                      s-dat = valore della referenza se e' di   *
      *                              tipo data                         *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - Rx        Richiesta del numero progressivo corrispondente    *
      *             all'ultima referenza multipla letta                *
      *                                                                *
      *             Input  : s-ope = "Rx"                              *
      *                                                                *
      *             Output : s-num = numero progressivo della referen- *
      *                              za                                *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - Eg        Encoding automatico : get                          *
      *                                                                *
      *             Input  : s-ope = "Eg"                              *
      *                                                                *
      *                      s-nam = Name del file interessato         *
      *                                                                *
      *             Output : s-sts = Status di uscita operazione       *
      *                              - Spaces : record trovato         *
      *                              - #      : record non trovato     *
      *                                                                *
      *                      s-num = Ultimo codice utilizzato          *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - Ep        Encoding automatico : put                          *
      *                                                                *
      *             Input  : s-ope = "Ep"                              *
      *                                                                *
      *                      s-nam = Name del file interessato         *
      *                                                                *
      *                      s-num = Ultimo codice utilizzato          *
      *                                                                *
      *             Output : s-sts = Status di uscita operazione       *
      *                              - Spaces : operazione Ok          *
      *                              - #      : record gia' esistente  *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - Eu        Encoding automatico : update                       *
      *                                                                *
      *             Input  : s-ope = "Eu"                              *
      *                                                                *
      *                      s-nam = Name del file interessato         *
      *                                                                *
      *                      s-num = Ultimo codice utilizzato          *
      *                                                                *
      *             Output : s-sts = Status di uscita operazione       *
      *                              - Spaces : operazione Ok          *
      *                              - #      : record non esistente   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - Er        Encoding automatico : release                      *
      *                                                                *
      *             Input  : s-ope = "Er"                              *
      *                                                                *
      *                      s-nam = Name del file interessato         *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - Yg        Year last page automatico : get                    *
      *                                                                *
      *             Input  : s-ope = "Yg"                              *
      *                                                                *
      *                      s-fas = Codice identificazione stampa     *
      *                                                                *
      *                      s-saa = Secolo anno                       *
      *                                                                *
      *             Output : s-sts = Status di uscita operazione       *
      *                              - Spaces : record trovato         *
      *                              - #      : record non trovato     *
      *                                                                *
      *                      s-num = Ultimo numero di pagina           *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - Yp        Year last page automatico : put                    *
      *                                                                *
      *             Input  : s-ope = "Yp"                              *
      *                                                                *
      *                      s-fas = Codice identificazione stampa     *
      *                                                                *
      *                      s-saa = Secolo anno                       *
      *                                                                *
      *                      s-num = Ultimo numero di pagina           *
      *                                                                *
      *             Output : s-sts = Status di uscita operazione       *
      *                              - Spaces : operazione Ok          *
      *                              - #      : record gia' esistente  *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - Yu        Year last page automatico : update                 *
      *                                                                *
      *             Input  : s-ope = "Yu"                              *
      *                                                                *
      *                      s-fas = Codice identificazione stampa     *
      *                                                                *
      *                      s-saa = Secolo anno                       *
      *                                                                *
      *                      s-num = Ultimo numero di pagina           *
      *                                                                *
      *             Output : s-sts = Status di uscita operazione       *
      *                              - Spaces : operazione Ok          *
      *                              - #      : record non esistente   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - Yr        Year last page automatico : release                *
      *                                                                *
      *                      s-fas = Codice identificazione stampa     *
      *                                                                *
      *                      s-saa = Secolo anno                       *
      *                                                                *
      *             Input  : s-ope = "Er"                              *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - PO        Richiesta di operazione su file alla prossima ope- *
      *             razione di Open del file                           *
      *                                                                *
      *             Input  : s-nam = Name del file interessato         *
      *                                                                *
      *                      s-sts = Tipo operazione richiesta         *
      *                              - KS : Sequenzializzazione        *
      *                              - SK : Indicizzazione senza so-   *
      *                                     stituzione                 *
      *                              - SJ : Indicizzazione con sosti-  *
      *                                     tuzione                    *
      *                              - KK : Operazione su se' stesso   *
      *                                                                *
      *                      s-num = tipo di filtro                    *
      *                              - 0 : nessun filtro               *
      *                              - 1 : filtro standard             *
      *                              - 2 : filtro programma            *
      *                                                                *
      *                      s-pat = nome o pathname del filtro        *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - PB : Estrazione pathname directories di base                 *
      *                                                                *
      *        Input  : s-ope = "PB"                                   *
      *                                                                *
      *                 s-nam = Tipo di pathname di base da estrarre   *
      *                                                                *
      *                         - "asc " : Directory 'asc'             *
      *                         - "azi " : Directory 'azi'             *
      *                         - "azx " : Directory azienda in uso    *
      *                         - "etc " : Directory 'etc'             *
      *                         - "fdb " : Directory 'fdb'             *
      *                         - "ftx " : Directory 'ftx'             *
      *                         - "spl " : Directory 'spl'             *
      *                         - "tmp " : Directory 'tmp'             *
      *                                                                *
      *        Output : s-pat = Pathname della directory di base       *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - EX : Richiesta delle massime dimensioni in caratteri per un  *
      *        un filename e per la sua estensione, ed inoltre del ca- *
      *        rattere da usarsi come separazione per l'estensione     *
      *                                                                *
      *        Input  : s-ope = "EX"                                   *
      *                                                                *
      *        Output : s-alf = Informazioni ottenute, da leggersi in  *
      *                         un'area ridefinita con tre campi come  *
      *                         dalllo schema seguente : XXXYYYZ       *
      *                                                                *
      *                         - XXX : Massima dimensione filename    *
      *                                                                *
      *                         - YYY : Massima dimensione estensione, *
      *                                 se zero significa che non e'   *
      *                                 ammessa estensione             *
      *                                                                *
      *                         - Z   : Carattere che deve essere uti- *
      *                                 lizzato per l'estensione, se a *
      *                                 spaces significa che non e'    *
      *                                 ammessa estensione             *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - AS : Richiesta di concatenamento di un filename con il di-   *
      *        rettorio 'asc'                                          *
      *                                                                *
      *        Input  : s-ope = "AS"                                   *
      *                                                                *
      *                 s-alf = Filename da concatenare                *
      *                                                                *
      *        Output : s-pat = Pathname completo sotto 'asc'          *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - AE : Preparazione filename per l'esportazione di un file nel *
      *        direttorio 'asc' per l'Azienda                          *
      *                                                                *
      *        Input  : s-ope = "AE"                                   *
      *                                                                *
      *                 s-alf = Filename da concatenare                *
      *                                                                *
      *                 s-arg = Area gestionale                        *
      *                                                                *
      *                 s-dat = Data per desumere anno                 *
      *                                                                *
      *                 s-nam = Estensione file da concatenare         *
      *                        (CSV, TXT, XML)                         *
      *                                                                *
      *                                                                *
      *        Output : s-alf = Pathname completo sotto 'asc'          *
      *                        (/abd/asc/{azi}/ARG/ANNO/file.est)      *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - UE : Preparazione filename per l'esportazione Utente nel     *
      *        direttorio 'asc'                                        *
      *                                                                *
      *        Input  : s-ope = "UE"                                   *
      *                                                                *
      *                 s-alf = Filename da concatenare                *
      *                                                                *
      *                 s-tip = Eventuale supplemento nome             *
      *                                                                *
      *                 s-nam = Estensione file da concatenare         *
      *                        (CSV, TXT)                              *
      *                                                                *
      *                                                                *
      *        Output : s-pat = Pathname completo sotto 'asc'          *
      *                        (/abd/asc/{utente}/exp/{azi}_fas.CSV)   *
      *                                                                *
      *                 s-alf = Stringa per messaggio all'utente       *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - XB : Richiesta di esecuzione di una procedura batch conte-   *
      *        nuta in /abd/bat                                        *
      *                                                                *
      *        Input  : s-ope = "XB"                                   *
      *                                                                *
      *                 s-alf = Nome del file, all'interno della di-   *
      *                         rectory /abd/bat, che contiene la      *
      *                         procedura batch                        *
      *                                                                *
      *                 s-svv = Flag di Save Video                     *
      *                         - S      : Si                          *
      *                         - Spaces : No                          *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - VB : Richiesta dati relativi alla valuta base, in funzione   *
      *        del codice azienda                                      *
      *                                                                *
      *        Input  : s-ope = "VB"                                   *
      *                                                                *
      *        Output : s-dec = decimali valuta base                   *
      *                                                                *
      *                 s-alf = sigla valuta base                      *
      *                                                                *
      *                 s-sgn = tipo di cambio valuta base             *
      *                                                                *
      *                 s-num = coefficiente di cambio valuta base     *
      *                                                                *
      *                 s-asx = descrizione valuta base                *
      *                                                                *
      *                 s-adx = descrizione in lingua valuta base      *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - VP : Richiesta di visualizzazione dei promemoria utente      *
      *                                                                *
      *        Input  : s-ope = "VP"                                   *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - AP : Richiesta di aggiornamento dei promemoria utente        *
      *                                                                *
      *        Input  : s-ope = "AP"                                   *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - XP : Richiesta di cancellazione promemoria utente            *
      *                                                                *
      *        Input  : s-ope = "XP"                                   *
      *                                                                *
      *                 s-num = Codice promemoria da cancellare        *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - NP : Nuovo promemoria utente                                 *
      *                                                                *
      *        Input  : s-ope = "AP"                                   *
      *                                                                *
      *                 s-alf = Descrizione promemoria                 *
      *                                                                *
      *                 s-exp = Testo promemoria (max 2 righe)         *
      *                                                                *
      *                 s-dat = Data di riferimento                    *
      *                                                                *
      *        Output : s-num = Codice promemoria attribuito           *
      *                                                                *
      *                 s-ute = Utente per il promemoria               *
      *                                                                *
      *        N.B.   : i valori mancanti vengono posti a default      *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - ?P : Richiesta di esistenza promemoria utente                *
      *                                                                *
      *        Input  : s-ope = "XP"                                   *
      *                                                                *
      *                 s-num = Codice promemoria da cancellare        *
      *                                                                *
      *        Output : s-sts                                          *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - U+ : Memorizzazione evento per User System Log               *
      *                                                                *
      *        Input  : s-ope = "U+"                                   *
      *                                                                *
      *                 s-nam = Name file                              *
      *                                                                *
      *                 s-fun = Tipo funzionamento                     *
      *                         - 'I' : Inserimento                    *
      *                         - 'M' : Modifica                       *
      *                         - 'D' : Cancellazione                  *
      *                                                                *
      *                 s-alf = Chiave identificativa                  *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - Pa : Consegna di un percorso archivio alla segreteria        *
      *                                                                *
      *        Input  : s-ope = "Pa"                                   *
      *                                                                *
      *                 s-pat = Percorso da consegnare                 *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - PA : Recupero di un percorso archivio dalla segreteria       *
      *                                                                *
      *        Input  : s-ope = "PA"                                   *
      *                                                                *
      *                                                                *
      *        Output : s-pat = Percorso precedentemente consegnato    *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - Pl : Consegna di un percorso archivio oltre 40 caratteri     *
      *                                                                *
      *        Input  : s-ope = "Pl"                                   *
      *                                                                *
      *                 s-alf = Percorso da consegnare                 *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - PL : Recupero di un percorso archivio oltre 40 caratteri     *
      *                                                                *
      *        Input  : s-ope = "PL"                                   *
      *                                                                *
      *                                                                *
      *        Output : s-alf = Percorso precedentemente consegnato    *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - IE : Indicatore rotante per programma in esecuzione          *
      *                                                                *
      *        Input  : s-ope = "IE"                                   *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - IC : Cancellazione Indicatore rotante                        *
      *                                                                *
      *        Input  : s-ope = "IC"                                   *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * - MG :  Dichiarazione di modalita' grafica attiva              *
      *                                                                *
      *             Input  : s-ope = "MG"                              *
      *                                                                *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      *             -------------------------------------------------- *
      *                                                                *
      * - UF : Utente e fase per programmi recenti                     *
      *                                                                *
      *        Input  : s-ope = "UF"                                   *
      *                                                                *
      *                 s-foc = Indice progressivo                     *
      *                                                                *
      *                                                                *
      *        Output : s-fas = Sigla fase gestionale                  *
      *                                                                *
      *                 s-ute = Codice utente                          *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.     w-i-p-NdK-PD .
       Object-Computer.     w-i-p-NdK-PD .

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [xse]                                        *
      *    *-----------------------------------------------------------*
           select  optional  xse   assign to disk           f-xse-pat
                             organization is indexed
                             access  mode is dynamic
                             record  key  is xse-rck
                             file  status is f-xse-sts                .

      *    *===========================================================*
      *    * File Control [auc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  auc   assign to disk           f-auc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is auc-key
                             file status  is f-auc-sts                .

      *    *===========================================================*
      *    * File Control [prs]                                        *
      *    *-----------------------------------------------------------*
           select  optional  prs   assign to disk           f-prs-pat
                             organization is indexed
                             access  mode is dynamic
                             record  key  is prs-rck
                             file  status is f-prs-sts                .

      *    *===========================================================*
      *    * File Control [ref]                                        *
      *    *-----------------------------------------------------------*
           select  optional  ref   assign to disk           f-ref-pat
                             organization is indexed
                             access  mode is dynamic
                             record  key  is ref-rck
                             file  status is f-ref-sts                .

      *    *===========================================================*
      *    * File Control [enc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  enc   assign to disk           f-enc-pat
                             organization is indexed
                             access  mode is dynamic
                             record  key  is enc-rck
                             file  status is f-enc-sts                .

      *    *===========================================================*
      *    * File Control [upr]                                        *
      *    *-----------------------------------------------------------*
           select  optional  upr   assign to disk           f-upr-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is upr-k01
                             file status  is                f-upr-sts .

      *    *===========================================================*
      *    * File Control [usl]                                        *
      *    *-----------------------------------------------------------*
           select  optional  usl   assign to disk           f-usl-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is usl-k01
                   alternate record key   is usl-k02
                   alternate record key   is usl-k03
                             file status  is                f-usl-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [xse]                                    *
      *    *-----------------------------------------------------------*
       fd  xse  label record standard.
       01  xse-rec.
           05  xse-rck.
               10  xse-tpk                pic  x(02)                  .
               10  xse-vlk                pic  x(28)                  .
           05  xse-rcd.
               10  filler                 pic  x(90)                  .

      *    *===========================================================*
      *    * File Description [auc]                                    *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/fls/rec/rfauc"                          .

      *    *===========================================================*
      *    * File Description [prs]                                    *
      *    *-----------------------------------------------------------*
       fd  prs  label record standard.
       01  prs-rec.
           05  prs-rck.
               10  prs-tre                pic  x(04)                  .
               10  prs-cod                pic  x(40)                  .
               10  prs-prg                pic  9(03)                  .
           05  prs-rcd.
               10  prs-r00.
                   15  filler occurs 400  pic  x(01)                  .
               10  prs-r10 redefines
                   prs-r00.
                   15  prs-alf            pic  x(80)                  .
                   15  filler occurs 320  pic  x(01)                  .
               10  prs-r20 redefines
                   prs-r00.
                   15  prs-num            pic s9(13)v9(05) trailing
                                                           separate
                                                           character  .
                   15  filler occurs 381  pic  x(01)                  .
               10  prs-r30 redefines
                   prs-r00.
                   15  prs-dat            pic  9(07)                  .
                   15  filler occurs 393  pic  x(01)                  .
               10  prs-r40 redefines
                   prs-r00.
                   15  prs-txt.
                       20  filler
                              occurs 400  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [ref]                                    *
      *    *-----------------------------------------------------------*
       fd  ref  label record standard.
       01  ref-rec.
           05  ref-rck.
               10  ref-tre                pic  x(04)                  .
               10  ref-cod                pic  x(40)                  .
               10  ref-prg                pic  9(03)                  .
           05  ref-rcd.
               10  ref-r00.
                   15  filler occurs 400  pic  x(01)                  .
               10  ref-r10 redefines
                   ref-r00.
                   15  ref-alf            pic  x(80)                  .
                   15  filler occurs 320  pic  x(01)                  .
               10  ref-r20 redefines
                   ref-r00.
                   15  ref-num            pic s9(13)v9(05) trailing
                                                           separate
                                                           character  .
                   15  filler occurs 381  pic  x(01)                  .
               10  ref-r30 redefines
                   ref-r00.
                   15  ref-dat            pic  9(07)                  .
                   15  filler occurs 393  pic  x(01)                  .
               10  ref-r40 redefines
                   ref-r00.
                   15  ref-txt.
                       20  filler
                              occurs 400  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [enc]                                    *
      *    *-----------------------------------------------------------*
       fd  enc  label record standard.
       01  enc-rec.
           05  enc-rck.
               10  enc-tre                pic  x(04)                  .
               10  enc-cod                pic  x(40)                  .
               10  enc-prg                pic  9(03)                  .
           05  enc-rcd.
               10  enc-r00.
                   15  filler occurs 400  pic  x(01)                  .
               10  enc-r10 redefines
                   enc-r00.
                   15  enc-alf            pic  x(80)                  .
                   15  filler occurs 320  pic  x(01)                  .
               10  enc-r20 redefines
                   enc-r00.
                   15  enc-num            pic s9(13)v9(05) trailing
                                                           separate
                                                           character  .
                   15  filler occurs 381  pic  x(01)                  .
               10  enc-r30 redefines
                   enc-r00.
                   15  enc-dat            pic  9(07)                  .
                   15  filler occurs 393  pic  x(01)                  .
               10  enc-r40 redefines
                   enc-r00.
                   15  enc-txt.
                       20  filler
                              occurs 400  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [upr]                                    *
      *    *-----------------------------------------------------------*
       fd  upr       label record standard                            .

      *    *===========================================================*
      *    * Record fisico - Promemoria Utenti                         *
      *    *-----------------------------------------------------------*
       01  upr-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  upr-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : UTECOD                         *
      *            *---------------------------------------------------*
               10  upr-k01.
                   15  upr-cod-ute        pic  x(08)                  .
                   15  upr-cod-upr        pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  upr-dat.
               10  upr-des-upr            pic  x(40)                  .
               10  upr-txt-upr.
                   15  upr-txt-rig occurs 10
                                          pic  x(40)                  .
               10  upr-mdr-upr            pic  9(02)                  .
               10  upr-mda-upr            pic  9(02)                  .
               10  upr-dat-upr            pic  9(07)       comp-3     .
               10  upr-gio-set            pic  9(02)                  .
               10  upr-gio-rif            pic  9(02)                  .
               10  upr-mes-rif            pic  9(02)                  .
               10  upr-ngp-upr            pic  9(03)       comp-3     .
               10  upr-mdp-upr            pic  9(02)                  .
               10  upr-drt-upr            pic  9(02)                  .
               10  upr-dfi-upr            pic  9(07)       comp-3     .
               10  upr-dpv-upr            pic  9(07)       comp-3     .
               10  upr-snx-acp            pic  x(01)                  .
               10  upr-snx-pvs            pic  x(01)                  .
               10  upr-snx-scf            pic  x(01)                  .
               10  upr-alx-exp.
                   15  filler occurs  78  pic  x(01)                  .

      *    *===========================================================*
      *    * File Description [usl]                                    *
      *    *                                                           *
      *    * N.B.: L'attivazione avviene tramite 'xpg120' e puo'       *
      *    *       essere corretta a livello utente con 'xpg220'.      *
      *    *       Il modulo viene richiamato dai singoli programmi    *
      *    *       nelle fasi di conferma.                             *
      *    *-----------------------------------------------------------*
       fd  usl       label record standard                            .

      *    *===========================================================*
      *    * Record fisico                                             *
      *    *-----------------------------------------------------------*
       01  usl-rec.
      *        *-------------------------------------------------------*
      *        * Chiavi                                                *
      *        *-------------------------------------------------------*
           05  usl-key.
      *            *---------------------------------------------------*
      *            * Chiave numero 01 : PPUDAT                         *
      *            *---------------------------------------------------*
               10  usl-k01.
                   15  usl-ide-ppu        pic  x(20)                  .
                   15  usl-ide-dat        pic  9(07)       comp-3     .
                   15  usl-ide-tim        pic  9(08)                  .
                   15  usl-ide-ute        pic  x(08)                  .
                   15  usl-ide-azi        pic  x(04)                  .
                   15  usl-ide-fas        pic  x(06)                  .
                   15  usl-ide-tfu        pic  x(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 02 : UTEDAT                         *
      *            *---------------------------------------------------*
               10  usl-k02.
                   15  usl-ide-ute-2      pic  x(08)                  .
                   15  usl-ide-ppu-2      pic  x(20)                  .
                   15  usl-ide-dat-2      pic  9(07)       comp-3     .
                   15  usl-ide-tim-2      pic  9(08)                  .
                   15  usl-ide-azi-2      pic  x(04)                  .
                   15  usl-ide-fas-2      pic  x(06)                  .
                   15  usl-ide-tfu-2      pic  x(02)                  .
      *            *---------------------------------------------------*
      *            * Chiave numero 03 : DATPPU                         *
      *            *---------------------------------------------------*
               10  usl-k03.
                   15  usl-ide-dat-3      pic  9(07)       comp-3     .
                   15  usl-ide-ppu-3      pic  x(20)                  .
                   15  usl-ide-tim-3      pic  9(08)                  .
                   15  usl-ide-ute-3      pic  x(08)                  .
                   15  usl-ide-azi-3      pic  x(04)                  .
                   15  usl-ide-fas-3      pic  x(06)                  .
                   15  usl-ide-tfu-3      pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  usl-dat.
               10  usl-ide-nam            pic  x(04)                  .
               10  usl-ide-dpz            pic  9(02)                  .
               10  usl-ide-ter            pic  x(08)                  .
               10  usl-ide-ipa            pic  x(25)                  .
               10  usl-ide-key.
                   15  filler  occurs 40  pic  x(01)                  .
               10  usl-alx-exp.
                   15  filler  occurs 40  pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [xse]                *
      *    *-----------------------------------------------------------*
       01  f-xse.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-xse-nam                  pic  x(04) value "xse "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-xse-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-xse-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [auc]                *
      *    *-----------------------------------------------------------*
       01  f-auc.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-auc-nam                  pic  x(04) value "auc "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-auc-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-auc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [prs]                *
      *    *-----------------------------------------------------------*
       01  f-prs.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-prs-nam                  pic  x(04) value "prs "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-prs-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-prs-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [ref]                *
      *    *-----------------------------------------------------------*
       01  f-ref.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-ref-nam                  pic  x(04) value "ref "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-ref-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-ref-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [enc]                *
      *    *-----------------------------------------------------------*
       01  f-enc.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-enc-nam                  pic  x(04) value "enc "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-enc-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-enc-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [upr]                *
      *    *-----------------------------------------------------------*
       01  f-upr.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-upr-nam                  pic  x(04) value "upr "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-upr-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-upr-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [usl]                *
      *    *-----------------------------------------------------------*
       01  f-usl.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-usl-nam                  pic  x(04) value "usl "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-usl-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-usl-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mvideo"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                "mbckgx"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Informazioni gestite dal programma                        *
      *    *-----------------------------------------------------------*
       01  z.
      *        *-------------------------------------------------------*
      *        * Flag di modulo, in foreground, aperto o chiuso        *
      *        *-------------------------------------------------------*
           05  z-uno                      pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Tipo funzionamento foreground/background              *
      *        *-------------------------------------------------------*
           05  z-fun                      pic  x(01) value "F"        .
      *        *-------------------------------------------------------*
      *        * Sub-area per background                               *
      *        *-------------------------------------------------------*
           05  z-bkg.
      *            *---------------------------------------------------*
      *            * Si/No programma background in esecuzione          *
      *            *---------------------------------------------------*
               10  z-bkg-snx              pic  x(01) value "N"        .
      *            *---------------------------------------------------*
      *            * Nome del programma che ha lanciato il background  *
      *            * in esecuzione                                     *
      *            *---------------------------------------------------*
               10  z-bkg-lan              pic  x(10) value spaces     .
      *            *---------------------------------------------------*
      *            * Nome del programma background in esecuzione       *
      *            *---------------------------------------------------*
               10  z-bkg-pro              pic  x(10) value spaces     .
      *            *---------------------------------------------------*
      *            * Pathname programma background in esecuzione       *
      *            *---------------------------------------------------*
               10  z-bkg-pat              pic  x(40) value spaces     .
      *            *---------------------------------------------------*
      *            * Descrizione del programma background in esecuzione*
      *            *---------------------------------------------------*
               10  z-bkg-des              pic  x(40) value spaces     .
      *            *---------------------------------------------------*
      *            * Program start time background in esecuzione       *
      *            *---------------------------------------------------*
               10  z-bkg-pst              pic  9(15) value zero       .
      *        *-------------------------------------------------------*
      *        * Codice Run-Time utilizzato                            *
      *        * - 00  : Acucobol 85                                   *
      *        * - 01  : Austec RM-Master Cobol 74                     *
      *        * - 02  : Ryan McFarland RM-Cobol 85                    *
      *        *-------------------------------------------------------*
           05  z-run                      pic  9(02) value zero       .
      *        *-------------------------------------------------------*
      *        * Ragione sociale licenziatario                         *
      *        *-------------------------------------------------------*
           05  z-lcu                      pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * Segnale di richiesta di visualizzazione del codice a- *
      *        * zienda in uso per il modulo 'mvideo'                  *
      *        *-------------------------------------------------------*
           05  z-vca                      pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Template per lo spooler di stampa                     *
      *        *-------------------------------------------------------*
           05  z-tem                      pic  x(60) value spaces     .
      *        *-------------------------------------------------------*
      *        * Incremento o decremento Left/Rght in visualizzazione  *
      *        * archivi-stampa                                        *
      *        *-------------------------------------------------------*
           05  z-iod                      pic  9(02) value zero       .
      *        *-------------------------------------------------------*
      *        * Template per il blocco dello spooler di stampa        *
      *        *-------------------------------------------------------*
           05  z-tbs                      pic  x(80) value spaces     .
      *        *-------------------------------------------------------*
      *        * Parametri per il controllo della visualizzazione de-  *
      *        * gli archivi stampa                                    *
      *        *-------------------------------------------------------*
           05  z-vpf                      pic  x(10) value spaces     .
      *        *-------------------------------------------------------*
      *        * Parametro generale libero 1                           *
      *        *-------------------------------------------------------*
           05  z-pg1                      pic  x(10) value spaces     .
      *        *-------------------------------------------------------*
      *        * Parametro generale libero 2                           *
      *        *-------------------------------------------------------*
           05  z-pg2                      pic  x(10) value spaces     .
      *        *-------------------------------------------------------*
      *        * Parametro generale libero 3                           *
      *        *-------------------------------------------------------*
           05  z-pg3                      pic  x(10) value spaces     .
      *        *-------------------------------------------------------*
      *        * Correttivi data e ora di sistema                      *
      *        *-------------------------------------------------------*
           05  z-cdt                      pic s9(02) value zero       .
           05  z-chh                      pic s9(02) value zero       .
      *        *-------------------------------------------------------*
      *        * Mail server                                           *
      *        *-------------------------------------------------------*
           05  z-msa                      pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * Postmaster                                            *
      *        *-------------------------------------------------------*
           05  z-psm                      pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * User ID per account SMTP                              *
      *        *-------------------------------------------------------*
           05  z-uid                      pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * Password per account SMTP                             *
      *        *-------------------------------------------------------*
           05  z-ppi                      pic  x(20) value spaces     .
      *        *-------------------------------------------------------*
      *        * Password per la protezione documenti PDF              *
      *        *-------------------------------------------------------*
           05  z-ppe                      pic  x(20) value spaces     .
      *        *-------------------------------------------------------*
      *        * Agente di invio documenti                             *
      *        *-------------------------------------------------------*
           05  z-aid                      pic  x(20) value spaces     .
      *        *-------------------------------------------------------*
      *        * Password MySQL                                        *
      *        *-------------------------------------------------------*
           05  z-pws                      pic  x(20) value spaces     .
      *        *-------------------------------------------------------*
      *        * Indirizzo IP eventuale                                *
      *        *-------------------------------------------------------*
           05  z-ipa                      pic  x(25) value spaces     .
      *        *-------------------------------------------------------*
      *        * Codice terminale attualmente in uso                   *
      *        *-------------------------------------------------------*
           05  z-ter                      pic  x(08) value spaces     .
      *        *-------------------------------------------------------*
      *        * Codice utente attualmente in uso                      *
      *        *-------------------------------------------------------*
           05  z-ute                      pic  x(08) value spaces     .
      *        *-------------------------------------------------------*
      *        * Si/no User System Log per utente attualmente in uso   *
      *        *-------------------------------------------------------*
           05  z-usl                      pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Indirizzo mail utente                                 *
      *        *-------------------------------------------------------*
           05  z-uml                      pic  x(80) value spaces     .
      *        *-------------------------------------------------------*
      *        * Codice stampante associata all'utente                 *
      *        *-------------------------------------------------------*
           05  z-cst                      pic  x(08) value spaces     .
      *        *-------------------------------------------------------*
      *        * Codice stampante locale associata all'utente          *
      *        *-------------------------------------------------------*
           05  z-csl                      pic  x(08) value spaces     .
      *        *-------------------------------------------------------*
      *        * Prefisso o postfisso unico per files temporanei       *
      *        *-------------------------------------------------------*
           05  z-ppu                      pic  x(20) value spaces     .
      *        *-------------------------------------------------------*
      *        * Host-Id                                               *
      *        *-------------------------------------------------------*
           05  z-hoi                      pic  x(20) value spaces     .
      *        *-------------------------------------------------------*
      *        * Codice azienda attualmente in uso                     *
      *        *-------------------------------------------------------*
           05  z-azi                      pic  x(04) value spaces     .
      *        *-------------------------------------------------------*
      *        * Descrizione azienda attualmente in uso                *
      *        *-------------------------------------------------------*
           05  z-azd.
      *            *---------------------------------------------------*
      *            * Descrizione allineata a sinistra                  *
      *            *---------------------------------------------------*
               10  z-azd-sin              pic  x(40) value spaces     .
      *            *---------------------------------------------------*
      *            * Descrizione allineata al centro                   *
      *            *---------------------------------------------------*
               10  z-azd-cen              pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * Numero dipendenze esistenti per l'azienda             *
      *        *-------------------------------------------------------*
           05  z-dpq                      pic  9(02) value zero       .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza attualmente in uso                  *
      *        *-------------------------------------------------------*
           05  z-dpz                      pic  9(02) value zero       .
      *        *-------------------------------------------------------*
      *        * Descrizione dipendenza attualmente in uso             *
      *        *-------------------------------------------------------*
           05  z-dpd                      pic  x(20) value spaces     .
      *        *-------------------------------------------------------*
      *        * Sigla sistema applicativo attualmente in uso          *
      *        *-------------------------------------------------------*
           05  z-sap                      pic  x(03) value spaces     .
      *        *-------------------------------------------------------*
      *        * Sigla area gestionale attualmente in uso              *
      *        *-------------------------------------------------------*
           05  z-arg                      pic  x(03) value spaces     .
      *        *-------------------------------------------------------*
      *        * Sigla settore attualmente in uso                      *
      *        *-------------------------------------------------------*
           05  z-set                      pic  x(03) value spaces     .
      *        *-------------------------------------------------------*
      *        * Sigla fase attualmente in uso                         *
      *        *-------------------------------------------------------*
           05  z-fas                      pic  x(06) value spaces     .
      *        *-------------------------------------------------------*
      *        * Codice programma applicativo attualmente in uso       *
      *        *-------------------------------------------------------*
           05  z-pro                      pic  x(10) value spaces     .
      *        *-------------------------------------------------------*
      *        * Numero progressivo per pathname unico file temporanei *
      *        *-------------------------------------------------------*
           05  z-ptu                      pic  9(04) value zero       .
      *        *-------------------------------------------------------*
      *        * Program start time applicativo  attualm. in uso       *
      *        *-------------------------------------------------------*
           05  z-pst                      pic  9(15) value zero       .
      *        *-------------------------------------------------------*
      *        * Flag di inibizione Save video a inizio prossimo pro-  *
      *        * gramma                                                *
      *        *-------------------------------------------------------*
           05  z-fs0                      pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Percorso archivio consegnato alla segreteria          *
      *        *-------------------------------------------------------*
           05  z-pra                      pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * Percorso archivio lungo consegnato alla segreteria    *
      *        *-------------------------------------------------------*
           05  z-alf                      pic  x(80) value spaces     .
      *        *-------------------------------------------------------*
      *        * Stack  l.i.f.o.  programmi applicativi                *
      *        *-------------------------------------------------------*
           05  z-stk.
      *            *---------------------------------------------------*
      *            * Livello di profondita raggiunto                   *
      *            *---------------------------------------------------*
               10  z-stk-liv              pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tabella programmi memorizzati                     *
      *            *---------------------------------------------------*
               10  z-stk-tab.
      *                *-----------------------------------------------*
      *                * Programmi memorizzati                         *
      *                *-----------------------------------------------*
                   15  z-stk-ele  occurs  20.
      *                    *-------------------------------------------*
      *                    * Sistema applicativo                       *
      *                    *-------------------------------------------*
                       20  z-stk-sap      pic  x(03)                  .
      *                    *-------------------------------------------*
      *                    * Area gestionale                           *
      *                    *-------------------------------------------*
                       20  z-stk-arg      pic  x(03)                  .
      *                    *-------------------------------------------*
      *                    * Settore                                   *
      *                    *-------------------------------------------*
                       20  z-stk-set      pic  x(03)                  .
      *                    *-------------------------------------------*
      *                    * Fase                                      *
      *                    *-------------------------------------------*
                       20  z-stk-fas      pic  x(06)                  .
      *                    *-------------------------------------------*
      *                    * Programma                                 *
      *                    *-------------------------------------------*
                       20  z-stk-pro      pic  x(10)                  .
      *                    *-------------------------------------------*
      *                    * Program start time                        *
      *                    *-------------------------------------------*
                       20  z-stk-pst      pic  9(15)                  .
      *                    *-------------------------------------------*
      *                    * Flag di Save Video                        *
      *                    *-------------------------------------------*
                       20  z-stk-svv      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tabella coppie name-pathname files azienda            *
      *        *-------------------------------------------------------*
           05  z-cnp.
      *            *---------------------------------------------------*
      *            * Numero di coppie memorizzate                      *
      *            *---------------------------------------------------*
               10  z-cnp-ncm              pic  9(03) value zero       .
      *            *---------------------------------------------------*
      *            * Direttorio di base files di carattere generale    *
      *            *---------------------------------------------------*
               10  z-cnp-fdb              pic  x(40) value spaces     .
      *            *---------------------------------------------------*
      *            * Direttorio di base per l'azienda                  *
      *            *---------------------------------------------------*
               10  z-cnp-fls              pic  x(40) value spaces     .
      *            *---------------------------------------------------*
      *            * Direttorio di print-files per l'azienda           *
      *            *---------------------------------------------------*
               10  z-cnp-prf              pic  x(40) value spaces     .
      *            *---------------------------------------------------*
      *            * Direttorio di spool-files per l'azienda           *
      *            *---------------------------------------------------*
               10  z-cnp-spl              pic  x(40) value spaces     .
      *            *---------------------------------------------------*
      *            * Tabella coppie memorizzate                        *
      *            *---------------------------------------------------*
               10  z-cnp-tab.
      *                *-----------------------------------------------*
      *                * Coppie memorizzate                            *
      *                *-----------------------------------------------*
                   15  z-cnp-ele  occurs 256
                                  indexed by z-cnp-inx                .
      *                    *-------------------------------------------*
      *                    * File Name                                 *
      *                    *-------------------------------------------*
                       20  z-cnp-nam      pic  x(04)                  .
      *                    *-------------------------------------------*
      *                    * File open counter                         *
      *                    *-------------------------------------------*
                       20  z-cnp-foc      pic  9(03)                  .
      *                    *-------------------------------------------*
      *                    * Pathname del modulo oggetto di i-o        *
      *                    *-------------------------------------------*
                       20  z-cnp-pmo      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Tabella moduli transienti attivi                      *
      *        *-------------------------------------------------------*
           05  z-tma.
      *            *---------------------------------------------------*
      *            * Numero moduli transienti memorizzati              *
      *            *---------------------------------------------------*
               10  z-tma-max              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Tabella moduli transienti memorizzati             *
      *            *---------------------------------------------------*
               10  z-tma-tab occurs 100.
      *                *-----------------------------------------------*
      *                * Pathname del modulo transiente                *
      *                *-----------------------------------------------*
                   15  z-tma-pmo          pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Numero di utilizzi                            *
      *                *-----------------------------------------------*
                   15  z-tma-nut          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per parametri di selezione stampa                *
      *        *-------------------------------------------------------*
           05  z-pss.
      *            *---------------------------------------------------*
      *            * Ultimo tipo di stampa selezionato, preparato dal  *
      *            * modulo 'mprint' e gestito poi da 'mbckgv'         *
      *            *  - I      : Immediata                             *
      *            *  - D      : Su disco                              *
      *            *  - V      : A video                               *
      *            *---------------------------------------------------*
               10  z-pss-uts              pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Numero di print-file relativo all'ultimo tipo di  *
      *            * stampa selezionato, solo se su disco o a video,   *
      *            * preparato dal modulo 'mprint' e gestito poi dal   *
      *            * modulo 'mbckgv'                                   *
      *            *---------------------------------------------------*
               10  z-pss-upf              pic  9(12)                  .
      *            *---------------------------------------------------*
      *            * Ultimo tipo di stampa selezionato passato con la  *
      *            * funzione 'S<" da 'mpslct', per il controllo di i- *
      *            * nibizione background                              *
      *            *---------------------------------------------------*
               10  z-pss-tds              pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Buffer per parametri di selezione stampa, prepa-  *
      *            * rati dal modulo 'mpslct'                          *
      *            *---------------------------------------------------*
               10  z-pss-buf.
                   15  filler occurs 640  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per pathname unico per la redirezione dello      *
      *        * standard output nelle operazioni su files             *
      *        *-------------------------------------------------------*
           05  z-sto-fls                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Tabella pathnames di esecuzione sostitutivi           *
      *        *-------------------------------------------------------*
           05  z-pnx.
      *            *---------------------------------------------------*
      *            * Numero di coppie memorizzate                      *
      *            *---------------------------------------------------*
               10  z-pnx-ncm              pic  9(03) value zero       .
      *            *---------------------------------------------------*
      *            * Comodo                                            *
      *            *---------------------------------------------------*
               10  z-pnx-ctr              pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Aziende per cui deve essere operata la sostitu-   *
      *            * zione                                             *
      *            * - Spaces : No                                     *
      *            * - S      : Si                                     *
      *            *---------------------------------------------------*
               10  z-pnx-aps.
      *                *-----------------------------------------------*
      *                * Si/No sostituzione limitata solo ad alcune    *
      *                * aziende, quelle elencate nel record avente    *
      *                * chiave : '?azi?'                              *
      *                *-----------------------------------------------*
                   15  z-pnx-aps-snx      pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Lista delle aziende                           *
      *                *-----------------------------------------------*
                   15  z-pnx-aps-laz      pic  x(40)                  .
                   15  z-pnx-aps-la2 redefines
                       z-pnx-aps-laz.
                       20  z-pnx-aps-lel  occurs 10
                                          pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Tabella coppie memorizzate                        *
      *            *---------------------------------------------------*
               10  z-pnx-tab.
      *                *-----------------------------------------------*
      *                * Coppie memorizzate                            *
      *                *-----------------------------------------------*
                   15  z-pnx-ele  occurs 64.
      *                    *-------------------------------------------*
      *                    * Pathname originale                        *
      *                    *-------------------------------------------*
                       20  z-pnx-org      pic  x(40)                  .
      *                    *-------------------------------------------*
      *                    * Pathname sostitutivo                      *
      *                    *-------------------------------------------*
                       20  z-pnx-sos      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Contatori per il programma in esecuzione              *
      *        *-------------------------------------------------------*
           05  z-pec                      pic  9(02) value zero       .
           05  z-pey                      pic  9(02) value zero       .
           05  z-pew                      pic  9(05) value zero       .
      *        *-------------------------------------------------------*
      *        * Stack programmi utilizzati dall'utente                *
      *        *-------------------------------------------------------*
           05  z-sfu.
      *            *---------------------------------------------------*
      *            * Contatore elementi                                *
      *            *---------------------------------------------------*
               10  z-sfu-ctr              pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Tabella programmi memorizzati                     *
      *            *---------------------------------------------------*
               10  z-sfu-tab.
      *                *-----------------------------------------------*
      *                * Programmi memorizzati                         *
      *                *-----------------------------------------------*
                   15  z-sfu-ele  occurs 256
                                  indexed by z-sfu-inx                .
      *                    *-------------------------------------------*
      *                    * Fase                                      *
      *                    *-------------------------------------------*
                       20  z-sfu-fas      pic  x(06)                  .
      *                    *-------------------------------------------*
      *                    * Utente                                    *
      *                    *-------------------------------------------*
                       20  z-sfu-ute      pic  x(08)                  .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w.
      *        *-------------------------------------------------------*
      *        * Comodo per flag di Save Video                         *
      *        *-------------------------------------------------------*
           05  w-svv                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Contatore                                             *
      *        *-------------------------------------------------------*
           05  w-ctr                      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Puntatore                                             *
      *        *-------------------------------------------------------*
           05  w-pnt                      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio data                           *
      *        *-------------------------------------------------------*
           05  w-dat                      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per calcolo settimana dell'anno                *
      *        *-------------------------------------------------------*
           05  w-dia                      pic  9(07)                  .
           05  w-dsa                      pic  9(07)                  .
           05  w-dsw                      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Indici su tabelle                                     *
      *        *-------------------------------------------------------*
           05  w-inx                      pic  9(03)                  .
           05  w-i01                      pic  9(03)                  .
           05  w-i99                      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per puntatore                             *
      *        *-------------------------------------------------------*
           05  w-svp                      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Flag generico                                         *
      *        *-------------------------------------------------------*
           05  w-flg                      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodi generici                                       *
      *        *-------------------------------------------------------*
           05  w-alf                      pic  x(80)                  .
           05  w-arg                      pic  x(03)                  .
           05  w-set                      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per file name                                  *
      *        *-------------------------------------------------------*
           05  w-nam.
               10  filler                 pic  x(01)                  .
               10  w-nam-fdb              pic  x(01)                  .
               10  filler                 pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per manipolazione prefisso o postfisso unico   *
      *        * per files temporanei                                  *
      *        *-------------------------------------------------------*
           05  w-ppu.
               10  w-ppu-nam.
                   15  w-ppu-chr
                               occurs 20  pic  x(01)                  .
               10  w-ppu-ctr              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Comodo di 255  caratteri per string-unstring          *
      *        *-------------------------------------------------------*
           05  w-seu.
               10  filler occurs 255      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per 'Permission denied'                          *
      *        *-------------------------------------------------------*
           05  w-dnd.
               10  w-dnd-cmd              pic  x(06)                  .
               10  w-dnd-ibz              pic  9(02)                  .
               10  w-dnd-sts              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per area gestionale e settore                  *
      *        *-------------------------------------------------------*
           05  w-ags.
               10  w-ags-arg              pic  x(03)                  .
               10  w-ags-set              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per pathname file immagine di segreteria       *
      *        *-------------------------------------------------------*
           05  w-pth-sgr                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per lettura dati utente                        *
      *        *-------------------------------------------------------*
           05  w-usr                      pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Tabella giorni del mese                               *
      *        *-------------------------------------------------------*
           05  w-tgm.
               10  w-tgn.
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 28         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
                   15  filler             pic  9(02) value 30         .
                   15  filler             pic  9(02) value 31         .
               10  w-tgo redefines
                   w-tgn.
                   15  w-tgp occurs 12    pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per variabili di i.p.c.                          *
      *        *-------------------------------------------------------*
           05  w-ipc.
      *            *---------------------------------------------------*
      *            * Chiave record                                     *
      *            *---------------------------------------------------*
               10  w-ipc-key.
      *                *-----------------------------------------------*
      *                * Tipo variabile                                *
      *                * - G : Globale                                 *
      *                * - P : Pipeline                                *
      *                *-----------------------------------------------*
                   15  w-ipc-tva          pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Livello di profondita' applicativa            *
      *                *-----------------------------------------------*
                   15  w-ipc-liv          pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Provenienza                                   *
      *                * - + : dal livello successivo                  *
      *                * - - : dal livello precedente                  *
      *                * = - : dallo stesso livello                    *
      *                *-----------------------------------------------*
                   15  w-ipc-dop          pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Nome della variabile                          *
      *                *-----------------------------------------------*
                   15  w-ipc-var          pic  x(20)                  .
      *            *---------------------------------------------------*
      *            * Dati record                                       *
      *            *---------------------------------------------------*
               10  w-ipc-rcd.
      *                *-----------------------------------------------*
      *                * Tipo di variabile                             *
      *                * - A : Alfanumerica                            *
      *                * - N : Numerica                                *
      *                * - D : Data                                    *
      *                *-----------------------------------------------*
                   15  w-ipc-tip          pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Numero caratteri                              *
      *                *-----------------------------------------------*
                   15  w-ipc-car          pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Numero decimali                               *
      *                *-----------------------------------------------*
                   15  w-ipc-dec          pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Presenza segno algebrico                      *
      *                *-----------------------------------------------*
                   15  w-ipc-sgn          pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Valore della variabile                        *
      *                *-----------------------------------------------*
                   15  w-ipc-val.
      *                    *-------------------------------------------*
      *                    * Se di tipo alfanumerico                   *
      *                    *-------------------------------------------*
                       20  w-ipc-d01.
                           25  w-ipc-alf  pic  x(80)                  .
      *                    *-------------------------------------------*
      *                    * Se di tipo numerico                       *
      *                    *-------------------------------------------*
                       20  w-ipc-d02 redefines
                           w-ipc-d01.
                           25  w-ipc-num  pic s9(13)v9(05) trailing
                                                           separate
                                                           character  .
                           25  filler     pic  x(61)                  .
      *                    *-------------------------------------------*
      *                    * Se di tipo data                           *
      *                    *-------------------------------------------*
                       20  w-ipc-d03 redefines
                           w-ipc-d01.
                           25  w-ipc-dat  pic  9(07)                  .
                           25  filler     pic  x(73)                  .
      *        *-------------------------------------------------------*
      *        * Work per chiave personalizzazione o referenza         *
      *        *-------------------------------------------------------*
           05  w-key-por.
               10  w-key-por-key          pic  x(40)                  .
               10  w-key-por-pre          pic  x(40)                  .
               10  w-key-por-pos          pic  x(40)                  .
               10  w-key-por-pnt          pic  9(02)                  .
               10  w-key-por-lun          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per personalizzazioni                            *
      *        *-------------------------------------------------------*
           05  w-prs.
      *            *---------------------------------------------------*
      *            * Definizione della personalizzazione               *
      *            *---------------------------------------------------*
               10  w-prs-def.
                   15  w-prs-def-cod      pic  x(32)                  .
                   15  w-prs-def-tip      pic  x(01)                  .
                   15  w-prs-def-car      pic  9(02)                  .
                   15  w-prs-def-dec      pic  9(01)                  .
                   15  w-prs-def-sgn      pic  x(01)                  .
                   15  w-prs-def-som      pic  x(01)                  .
                   15  w-prs-def-prg      pic  9(03)                  .
                   15  w-prs-def-nxt      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per referenze                                    *
      *        *-------------------------------------------------------*
           05  w-ref.
      *            *---------------------------------------------------*
      *            * Definizione della referenza                       *
      *            *---------------------------------------------------*
               10  w-ref-def.
                   15  w-ref-def-cod      pic  x(32)                  .
                   15  w-ref-def-tip      pic  x(01)                  .
                   15  w-ref-def-car      pic  9(02)                  .
                   15  w-ref-def-dec      pic  9(01)                  .
                   15  w-ref-def-sgn      pic  x(01)                  .
                   15  w-ref-def-som      pic  x(01)                  .
                   15  w-ref-def-prg      pic  9(03)                  .
                   15  w-ref-def-nxt      pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Nome della variabile di 'Status Background'           *
      *        *-------------------------------------------------------*
           05  w-vsb                      pic  x(20) value
                     "Status Background   "                           .
      *        *-------------------------------------------------------*
      *        * Work per file-to-file e file-delete                   *
      *        *-------------------------------------------------------*
           05  w-ftf.
               10  w-ftf-nam-ori          pic  x(08)                  .
               10  w-ftf-nam-dst          pic  x(08)                  .
               10  w-ftf-tip-fil          pic  x(01)                  .
               10  w-ftf-pth-ori.
                   15  w-ftf-pth-chr occurs 40
                                          pic  x(01)                  .
               10  w-ftf-pth-dst          pic  x(40)                  .
               10  w-ftf-tip-ctr          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per accettazione passwords                       *
      *        *-------------------------------------------------------*
           05  w-acp.
               10  w-acp-exi              pic  x(01)                  .
               10  w-acp-lit              pic  x(13)                  .
               10  w-acp-cod              pic  x(08)                  .
               10  w-acp-psw              pic  x(08)                  .
               10  w-acp-acc              pic  x(08)                  .
               10  w-acp-key              pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Nome del file di cui e' stata richiesta una operazio- *
      *        * ne di sequenzializzazione alla prossima operazione di *
      *        * Open del File                                         *
      *        *-------------------------------------------------------*
           05  w-kse-nam                  pic  x(04) value spaces     .
      *        *-------------------------------------------------------*
      *        * Tipo di operazione da eseguire alla prossima opera-   *
      *        * zione di Open del File                                *
      *        *                                                       *
      *        * - KS : Sequenzializzazione                            *
      *        * - SK : Indicizzazione senza sostituzione              *
      *        * - SJ : Indicizzazione con sostituzione                *
      *        * - KK : Operazione su se' stesso                       *
      *        *-------------------------------------------------------*
           05  w-kse-top                  pic  x(02) value spaces     .
      *        *-------------------------------------------------------*
      *        * Tipo di filtro da applicare su operazione alla pros-  *
      *        * sima operazione di Open del File                      *
      *        *                                                       *
      *        * - 0 : Nessun filtro                                   *
      *        * - 1 : Filtro standard                                 *
      *        * - 1 : Filtro programma                                *
      *        *-------------------------------------------------------*
           05  w-kse-tpf                  pic  9(01) value zero       .
      *        *-------------------------------------------------------*
      *        * Filtro da applicare su operazione alla prossima ope-  *
      *        * razione di Open del File                              *
      *        *                                                       *
      *        * - Se tipo filtro 0 : Spaces                           *
      *        * - Se tipo filtro 1 : Nome filtro                      *
      *        * - Se tipo filtro 2 : Pathname programma filtro        *
      *        *-------------------------------------------------------*
           05  w-kse-nmf                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * Aree per salvataggi temporanei di valori              *
      *        *-------------------------------------------------------*
           05  w-sav.
      *            *---------------------------------------------------*
      *            * Salvataggio di z-azi                              *
      *            *---------------------------------------------------*
               10  w-sav-zet-azi         pic  x(04)                   .

      *    *===========================================================*
      *    * Work area per trattamento promemoria                      *
      *    *-----------------------------------------------------------*
       01  w-upr.
      *        *-------------------------------------------------------*
      *        * Utente                                                *
      *        *-------------------------------------------------------*
           05  w-upr-ute                  pic  x(08)                  .
      *        *-------------------------------------------------------*
      *        * Codice progressivo promemoria                         *
      *        *-------------------------------------------------------*
           05  w-upr-cod                  pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Data promemoria                                       *
      *        *-------------------------------------------------------*
           05  w-upr-dat                  pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data attuale                                          *
      *        *-------------------------------------------------------*
           05  w-upr-daa                  pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Giorno della settimana data attuale                   *
      *        *-------------------------------------------------------*
           05  w-upr-gds                  pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di uscita dalla selezione                        *
      *        *-------------------------------------------------------*
           05  w-upr-flg                  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Titolo promemoria                                     *
      *        *-------------------------------------------------------*
           05  w-upr-des                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Testo promemoria                                      *
      *        *-------------------------------------------------------*
           05  w-upr-txt.
               10  w-upr-rig  occurs 10   pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Castelletto date promemoria                           *
      *        *-------------------------------------------------------*
           05  w-upr-ele occurs 11.
               10  w-upr-dpm              pic  9(07)                  .
           05  w-upr-ctx                  pic  9(03)                  .
           05  w-upr-ctr                  pic  9(03)                  .
           05  w-upr-max                  pic  9(03)      value 011   .
      *        *-------------------------------------------------------*
      *        * Flag di promemoria cancellabile                       *
      *        *-------------------------------------------------------*
           05  w-upr-pcn                  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per data non visualizzata                      *
      *        *-------------------------------------------------------*
           05  w-upr-dnv                  pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per visualizzazione                            *
      *        *-------------------------------------------------------*
           05  w-upr-lin                  pic  9(02)                  .
           05  w-upr-nli                  pic  9(02)      value 05    .
           05  w-upr-dtv                  pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodi per calcoli                                    *
      *        *-------------------------------------------------------*
           05  w-upr-n01                  pic  9(02)                  .
           05  w-upr-n02                  pic  9(03)                  .

      *    *===========================================================*
      *    * Work per records di [auc] 'psg'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucpsg0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'azi'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucazi0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'ute'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucute0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'cmd'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wauccmd0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'pnx'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucpnx0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'dps'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucdps0.cpw"                   .

      *    *===========================================================*
      *    * Work per records di [auc] 'drf'                           *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucdrf0.cpw"                   .

      *    *===========================================================*
      *    * Work area per flags di esistenza records appartenenti ai  *
      *    * file [auc]                                                *
      *    *-----------------------------------------------------------*
       01  w-fes.
           05  w-fes-psg                  pic  x(01)                  .
           05  w-fes-azi                  pic  x(01)                  .
           05  w-fes-ute                  pic  x(01)                  .
           05  w-fes-cmd                  pic  x(01)                  .

      *    *===========================================================*
      *    * Work area per tests di abilitazione                       *
      *    *-----------------------------------------------------------*
       01  w-abl.
      *        *-------------------------------------------------------*
      *        * Per tests di abilitazione su aziende                  *
      *        *-------------------------------------------------------*
           05  w-abl-dpz.
      *            *---------------------------------------------------*
      *            * Numero di dipendenze su cui l'utente in uso e' a- *
      *            * bilitato ad operare sull'azienda in uso           *
      *            *---------------------------------------------------*
               10  w-abl-dpz-num          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tabella codici dipendenze su cui l'utente in uso  *
      *            * e' abilitato ad operare sull'azienda in uso       *
      *            *---------------------------------------------------*
               10  w-abl-dpz-tbl.
                   15  w-abl-dpz-cod occurs 20
                                          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Work per ridefinizione del codice azienda asso-   *
      *            * ciato all'utente, per il controllo sul formato    *
      *            * di codice azienda che indica un codice dipenden-  *
      *            * za di abilitazione : 'd=nn'                       *
      *            *---------------------------------------------------*
               10  w-abl-dpz-rca.
                   15  w-abl-dpz-rca-012
                                          pic  x(02)                  .
                   15  w-abl-dpz-rca-034
                                          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Contatori ed indici di comodo                     *
      *            *---------------------------------------------------*
               10  w-abl-dpz-c01          pic  9(02)                  .

      *    *===========================================================*
      *    * Per determinazione del giorno della settimana             *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wgdslit0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per operazioni sulle date                       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wnrgdat0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using s                      .
      ******************************************************************

      *================================================================*
      *       Main program                                             *
      *----------------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Filtro di un pathname di esecuzione con un so-  *
      *              * stitutivo                                       *
      *              *-------------------------------------------------*
           if        s-ope                =    "FP"
                     perform  flt-pnx-000 thru flt-pnx-999
      *              *-------------------------------------------------*
      *              * Scrittura di una variabile di i.p.c.            *
      *              *-------------------------------------------------*
           else if   s-ope                =    "PV"
                     perform  put-var-000 thru put-var-999
      *              *-------------------------------------------------*
      *              * Lettura di una variabile di i.p.c.              *
      *              *-------------------------------------------------*
           else if   s-ope                =    "GV"
                     perform  get-var-000 thru get-var-999
      *              *-------------------------------------------------*
      *              * Cancellazione di una variabile di i.p.c.        *
      *              *-------------------------------------------------*
           else if   s-ope                =    "CV"
                     perform  clr-var-000 thru clr-var-999
      *              *-------------------------------------------------*
      *              * Lettura di una personalizzazione                *
      *              *-------------------------------------------------*
           else if   s-ope                =    "P:"
                     perform  get-prs-000 thru get-prs-999
      *              *-------------------------------------------------*
      *              * Start su di una personalizzazione               *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Ps"
                     perform  str-prs-000 thru str-prs-999
      *              *-------------------------------------------------*
      *              * Lettura sequenziale di una personalizzazione    *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Pn"
                     perform  nxt-prs-000 thru nxt-prs-999
      *              *-------------------------------------------------*
      *              * Numero progressivo di una personalizzazione     *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Px"
                     perform  num-prs-000 thru num-prs-999
      *              *-------------------------------------------------*
      *              * Lettura di una referenza                        *
      *              *-------------------------------------------------*
           else if   s-ope                =    "R:"
                     perform  get-ref-000 thru get-ref-999
      *              *-------------------------------------------------*
      *              * Start su di una referenza                       *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Rs"
                     perform  str-ref-000 thru str-ref-999
      *              *-------------------------------------------------*
      *              * Lettura sequenziale di una referenza            *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Rn"
                     perform  nxt-ref-000 thru nxt-ref-999
      *              *-------------------------------------------------*
      *              * Numero progressivo di una referenza             *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Rx"
                     perform  num-ref-000 thru num-ref-999
           else      go to main-200.
      *              *-------------------------------------------------*
      *              * INTERRUZIONE 'IF' PER SUPERAMENTO LIMITE DI 100 *
      *              *-------------------------------------------------*
       main-200.
      *              *-------------------------------------------------*
      *              * Estrazione informazioni generali                *
      *              *-------------------------------------------------*
           if        s-ope                =    "IG"
                     perform  inf-gen-000 thru inf-gen-999
      *              *-------------------------------------------------*
      *              * Incremento file open counter                    *
      *              *-------------------------------------------------*
           else if   s-ope                =    "F+"
                     perform  inc-foc-000 thru inc-foc-999
      *              *-------------------------------------------------*
      *              * Decremento file open counter                    *
      *              *-------------------------------------------------*
           else if   s-ope                =    "F-"
                     perform  dec-foc-000 thru dec-foc-999
      *              *-------------------------------------------------*
      *              * Estrazione file open counter                    *
      *              *-------------------------------------------------*
           else if   s-ope                =    "F?"
                     perform  est-foc-000 thru est-foc-999
      *              *-------------------------------------------------*
      *              * Memorizzazione pathname modulo oggetto di i-o   *
      *              *-------------------------------------------------*
           else if   s-ope                =    "FO"
                     perform  pmo-ioc-000 thru pmo-ioc-999
      *              *-------------------------------------------------*
      *              * Estrazione pathname memorizzato                 *
      *              *-------------------------------------------------*
           else if   s-ope                =    "PG"
                     perform  nam-pat-000 thru nam-pat-999
      *              *-------------------------------------------------*
      *              * Estrazione attributi particolari per prossima   *
      *              * operazione di open del file                     *
      *              *-------------------------------------------------*
           else if   s-ope                =    "PF"
                     perform  nam-paf-000 thru nam-paf-999
      *              *-------------------------------------------------*
      *              * Estrazione pathname di base per files azienda   *
      *              *-------------------------------------------------*
           else if   s-ope                =    ".F"
                     perform  pnb-fls-000 thru pnb-fls-999
      *              *-------------------------------------------------*
      *              * Estrazione pathname di base per print-files     *
      *              *-------------------------------------------------*
           else if   s-ope                =    ".P"
                     perform  pnb-prf-000 thru pnb-prf-999
      *              *-------------------------------------------------*
      *              * Estrazione pathname di base per spool-files     *
      *              *-------------------------------------------------*
           else if   s-ope                =    ".S"
                     perform  pnb-spl-000 thru pnb-spl-999
      *              *-------------------------------------------------*
      *              * Estrazione codice stampante e codice stampante  *
      *              * locale associata all'utente in uso              *
      *              *-------------------------------------------------*
           else if   s-ope                =    "S?"
                     perform est-cst-000  thru est-cst-999
      *              *-------------------------------------------------*
      *              * Ingresso parametri di selezione stampa          *
      *              *-------------------------------------------------*
           else if   s-ope                =    "S>"
                     perform ing-pss-000  thru ing-pss-999
      *              *-------------------------------------------------*
      *              * Uscita parametri di selezione stampa            *
      *              *-------------------------------------------------*
           else if   s-ope                =    "S<"
                     perform usc-pss-000  thru usc-pss-999
      *              *-------------------------------------------------*
      *              * Ingresso parametri di tipo stampa               *
      *              *-------------------------------------------------*
           else if   s-ope                =    "T>"
                     perform ing-tds-000  thru ing-tds-999
      *              *-------------------------------------------------*
      *              * Uscita parametri di tipo stampa                 *
      *              *-------------------------------------------------*
           else if   s-ope                =    "T<"
                     perform usc-tds-000  thru usc-tds-999
      *              *-------------------------------------------------*
      *              * Pathname unico per file temporaneo              *
      *              *-------------------------------------------------*
           else if   s-ope                =    "UP"
                     perform pat-uni-000  thru pat-uni-999
      *              *-------------------------------------------------*
      *              * Estrazione informazioni su livello profondita'  *
      *              *-------------------------------------------------*
           else if   s-ope                =    "IL"
                     perform inf-liv-000  thru inf-liv-999
      *              *-------------------------------------------------*
      *              * Estrazione informazioni su azienda in uso       *
      *              *-------------------------------------------------*
           else if   s-ope                =    "IA"
                     perform inf-azi-000  thru inf-azi-999
      *              *-------------------------------------------------*
      *              * Emissione messaggio di i-o fatal error          *
      *              *-------------------------------------------------*
           else if   s-ope                =    "FE"
                     perform fat-err-000  thru fat-err-999
      *              *-------------------------------------------------*
      *              * Memorizzazione inizio modulo transiente         *
      *              *-------------------------------------------------*
           else if   s-ope                =    "M+"
                     perform ini-mod-000  thru ini-mod-999
      *              *-------------------------------------------------*
      *              * Memorizzazione fine modulo transiente           *
      *              *-------------------------------------------------*
           else if   s-ope                =    "M-"
                     perform fin-mod-000  thru fin-mod-999
      *              *-------------------------------------------------*
      *              * Test se programma applicativo attivo o no       *
      *              *-------------------------------------------------*
           else if   s-ope                =    "P?"
                     perform tst-pro-000  thru tst-pro-999
      *              *-------------------------------------------------*
      *              * Memorizzazione inizio programma applicativo     *
      *              *-------------------------------------------------*
           else if   s-ope                =    "P+" or
                     s-ope                =    "Q+"
                     perform ini-pro-000  thru ini-pro-999
      *              *-------------------------------------------------*
      *              * Memorizzazione fine programma applicativo       *
      *              *-------------------------------------------------*
           else if   s-ope                =    "P-"
                     perform fin-pro-000  thru fin-pro-999
      *              *-------------------------------------------------*
      *              * Informazioni relative ai moduli di io 'ioxyz'   *
      *              * ancora in essere                                *
      *              *-------------------------------------------------*
           else if   s-ope                =    "IO"
                     perform inf-mio-000  thru inf-mio-999
      *              *-------------------------------------------------*
      *              * Cancellazione moduli di i-o obsoleti            *
      *              * ancora in essere                                *
      *              *-------------------------------------------------*
           else if   s-ope                =    "IX"
                     perform cnc-mob-000  thru cnc-mob-999
      *              *-------------------------------------------------*
      *              * Autorizzazione esecuzione in background         *
      *              *-------------------------------------------------*
           else if   s-ope                =    "BF"
                     perform aut-bkg-000  thru aut-bkg-999
      *              *-------------------------------------------------*
      *              * Memorizzazione inizio programma in background   *
      *              *-------------------------------------------------*
           else if   s-ope                =    "B+"
                     perform ini-bkg-000  thru ini-bkg-999
      *              *-------------------------------------------------*
      *              * Memorizzazione fine programma in background     *
      *              *-------------------------------------------------*
           else if   s-ope                =    "B-"
                     perform fin-bkg-000  thru fin-bkg-999
      *              *-------------------------------------------------*
      *              * Memorizzazione codice azienda in uso            *
      *              *-------------------------------------------------*
           else if   s-ope                =    "AZ"
                     perform  mem-azi-000 thru mem-azi-999
      *              *-------------------------------------------------*
      *              * Memorizzazione codice dipendenza in uso         *
      *              *-------------------------------------------------*
           else if   s-ope                =    "D>"
                     perform  mem-dpz-000 thru mem-dpz-999
      *              *-------------------------------------------------*
      *              * Richiesta codice dipendenza in uso              *
      *              *-------------------------------------------------*
           else if   s-ope                =    "D<"
                     perform  ric-dpz-000 thru ric-dpz-999
      *              *-------------------------------------------------*
      *              * Richiesta codici dipendenza abilitati           *
      *              *-------------------------------------------------*
           else if   s-ope                =    "D?"
                     perform  abl-dpz-000 thru abl-dpz-999
      *              *-------------------------------------------------*
      *              * Inizializzazione tabella coppie name-pathname   *
      *              *-------------------------------------------------*
           else if   s-ope                =    "PN"
                     perform  cnp-new-000 thru cnp-new-999
      *              *-------------------------------------------------*
      *              * Memorizzazione codice terminale in uso          *
      *              *-------------------------------------------------*
           else if   s-ope                =    "TE"
                     perform  mem-ter-000 thru mem-ter-999
      *              *-------------------------------------------------*
      *              * Memorizzazione codice stampante e codice stam-  *
      *              * pante locale associate all'utente in uso        *
      *              *-------------------------------------------------*
           else if   s-ope                =    "ST"
                     perform  mem-cst-000 thru mem-cst-999
      *              *-------------------------------------------------*
      *              * Memorizzazione codice utente in uso             *
      *              *-------------------------------------------------*
           else if   s-ope                =    "UT"
                     perform  mem-ute-000 thru mem-ute-999
      *              *-------------------------------------------------*
      *              * Estrazione system date and time                 *
      *              *-------------------------------------------------*
           else if   s-ope                =    "DT"
                     perform est-sdt-000  thru est-sdt-999
      *              *-------------------------------------------------*
      *              * Estrazione giorno della settimana               *
      *              *-------------------------------------------------*
           else if   s-ope                =    "GS"
                     perform est-gds-000  thru est-gds-999
      *              *-------------------------------------------------*
      *              * Estrazione literal del mese                     *
      *              *-------------------------------------------------*
           else if   s-ope                =    "LM"
                     perform est-ldm-000  thru est-ldm-999
      *              *-------------------------------------------------*
      *              * Incremento di una data in giorni                *
      *              *-------------------------------------------------*
           else if   s-ope                =    "G+"
                     perform inc-dat-000  thru inc-dat-999
      *              *-------------------------------------------------*
      *              * Decremento di una data in giorni                *
      *              *-------------------------------------------------*
           else if   s-ope                =    "G-"
                     perform dec-dat-000  thru dec-dat-999
      *              *-------------------------------------------------*
      *              * Estrazione settimana dell'anno                  *
      *              *-------------------------------------------------*
           else if   s-ope                =    "SA"
                     perform est-sda-000  thru est-sda-999
      *              *-------------------------------------------------*
      *              * Normalizzazione secolo nella data               *
      *              *-------------------------------------------------*
           else if   s-ope                =    "NS"
                     perform nor-sec-000  thru nor-sec-999
      *              *-------------------------------------------------*
      *              * Controllo data                                  *
      *              *-------------------------------------------------*
           else if   s-ope                =    "CD"
                     perform cnt-dat-000  thru cnt-dat-999
      *              *-------------------------------------------------*
      *              * Open modulo di segreteria                       *
      *              *-------------------------------------------------*
           else if   s-ope                =    "OP"
                     perform  opn-seg-000 thru opn-seg-999
      *              *-------------------------------------------------*
      *              * Close modulo di segreteria                      *
      *              *-------------------------------------------------*
           else if   s-ope                =    "CL"
                     perform  cls-seg-000 thru cls-seg-999
      *              *-------------------------------------------------*
      *              * Richiesta prefisso o postfisso unico per files  *
      *              * temporanei                                      *
      *              *-------------------------------------------------*
           else if   s-ope                =    "U?"
                     perform  ppu-ric-000 thru ppu-ric-999
      *              *-------------------------------------------------*
      *              * Richiesta Host-Id                               *
      *              *-------------------------------------------------*
           else if   s-ope                =    "H?"
                     perform  hoi-ric-000 thru hoi-ric-999
      *              *-------------------------------------------------*
      *              * Dichiarazione tipo di Run-Time in uso           *
      *              *-------------------------------------------------*
           else if   s-ope                =    "RT"
                     perform  run-dic-000 thru run-dic-999
      *              *-------------------------------------------------*
      *              * Richiesta tipo di Run-Time in uso               *
      *              *-------------------------------------------------*
           else if   s-ope                =    "R?"
                     perform  run-ric-000 thru run-ric-999
      *              *-------------------------------------------------*
      *              * Wait di un secondo                              *
      *              *-------------------------------------------------*
           else if   s-ope                =    "W1"
                     perform  wai-t1s-000 thru wai-t1s-999
      *              *-------------------------------------------------*
      *              * Wait di 'n' secondi                             *
      *              *-------------------------------------------------*
           else if   s-ope                =    "WT"
                     perform  wai-tns-000 thru wai-tns-999
      *              *-------------------------------------------------*
      *              * File to file                                    *
      *              *-------------------------------------------------*
           else if   s-ope                =    "FF"
                     perform  fil-tof-000 thru fil-tof-999
      *              *-------------------------------------------------*
      *              * File delete                                     *
      *              *-------------------------------------------------*
           else if   s-ope                =    "FD"
                     perform  fil-del-000 thru fil-del-999
      *              *-------------------------------------------------*
      *              * Pathname delete                                 *
      *              *-------------------------------------------------*
           else if   s-ope                =    "PD"
                     perform  pat-del-000 thru pat-del-999
      *              *-------------------------------------------------*
      *              * File rename                                     *
      *              *-------------------------------------------------*
           else if   s-ope                =    "FR"
                     perform  fil-ren-000 thru fil-ren-999
      *              *-------------------------------------------------*
      *              * File rename in senso inverso                    *
      *              *-------------------------------------------------*
           else if   s-ope                =    "RF"
                     perform  ren-fil-000 thru ren-fil-999
      *              *-------------------------------------------------*
      *              * Encoding automatico : get                       *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Eg"
                     perform  enc-get-000 thru enc-get-999
      *              *-------------------------------------------------*
      *              * Encoding automatico : put                       *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Ep"
                     perform  enc-put-000 thru enc-put-999
      *              *-------------------------------------------------*
      *              * Encoding automatico : update                    *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Eu"
                     perform  enc-upd-000 thru enc-upd-999
      *              *-------------------------------------------------*
      *              * Encoding automatico : release                   *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Er"
                     perform  enc-rel-000 thru enc-rel-999
      *              *-------------------------------------------------*
      *              * Year last page automatico : get                 *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Yg"
                     perform  ylp-get-000 thru ylp-get-999
      *              *-------------------------------------------------*
      *              * Year last page automatico : put                 *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Yp"
                     perform  ylp-put-000 thru ylp-put-999
      *              *-------------------------------------------------*
      *              * Year last page automatico : update              *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Yu"
                     perform  ylp-upd-000 thru ylp-upd-999
      *              *-------------------------------------------------*
      *              * Year last page automatico : release             *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Yr"
                     perform  ylp-rel-000 thru ylp-rel-999
      *              *-------------------------------------------------*
      *              * Richiesta di operazione su file alla prossima   *
      *              * operazione di Open sul File                     *
      *              *-------------------------------------------------*
           else if   s-ope                =    "PO"
                     perform  kse-sqk-000 thru kse-sqk-999
      *              *-------------------------------------------------*
      *              * Lettura di una personalizzazione generale       *
      *              *-------------------------------------------------*
           else if   s-ope                =    "PS"
                     perform  get-pge-000 thru get-pge-999
      *              *-------------------------------------------------*
      *              * Estrazione pathname directory di base           *
      *              *-------------------------------------------------*
           else if   s-ope                =    "PB"
                     perform  ept-ddb-000 thru ept-ddb-999
      *              *-------------------------------------------------*
      *              * Informazioni su filename ed estensione          *
      *              *-------------------------------------------------*
           else if   s-ope                =    "EX"
                     perform  inf-fxt-000 thru inf-fxt-999
      *              *-------------------------------------------------*
      *              * Richiesta di esecuzione di una procedura batch  *
      *              *-------------------------------------------------*
           else if   s-ope                =    "XB"
                     perform  bat-std-000 thru bat-std-999
      *              *-------------------------------------------------*
      *              * Richiesta dati valuta base                      *
      *              *-------------------------------------------------*
           else if   s-ope                =    "VB"
                     perform  val-bas-000 thru val-bas-999
      *              *-------------------------------------------------*
      *              * Richiesta di concatenamento di un filename con  *
      *              * la directory 'asc'                              *
      *              *-------------------------------------------------*
           else if   s-ope                =    "AS"
                     perform  cat-asc-000 thru cat-asc-999
      *              *-------------------------------------------------*
      *              * Preparazione filename per l'esportazione nella  *
      *              * directory 'asc' dell'Azienda                    *
      *              *-------------------------------------------------*
           else if   s-ope                =    "AE"
                     perform  azi-exp-000 thru azi-exp-999
      *              *-------------------------------------------------*
      *              * Preparazione filename per l'esportazione Utente *
      *              * nella directory 'asc'                           *
      *              *-------------------------------------------------*
           else if   s-ope                =    "UE"
                     perform  usr-exp-000 thru usr-exp-999
      *              *-------------------------------------------------*
      *              * Inibizione Save ad inizio programma applicativo *
      *              *-------------------------------------------------*
           else if   s-ope                =    "S0"
                     perform fs0-pro-000  thru fs0-pro-999
      *              *-------------------------------------------------*
      *              * Dichiarazione ragione sociale licenziatario     *
      *              *-------------------------------------------------*
           else if   s-ope                =    "LU"
                     perform  dic-lcu-000 thru dic-lcu-999
      *              *-------------------------------------------------*
      *              * Richiesta ragione sociale licenziatario         *
      *              *-------------------------------------------------*
           else if   s-ope                =    "L?"
                     perform  ric-lcu-000 thru ric-lcu-999
      *              *-------------------------------------------------*
      *              * Richiesta di cancellazione promemoria           *
      *              *-------------------------------------------------*
           else if   s-ope                =    "XP"
                     perform  del-upr-000 thru del-upr-999
      *              *-------------------------------------------------*
      *              * Richiesta visualizzazione promemoria            *
      *              *-------------------------------------------------*
           else if   s-ope                =    "VP" or
                     s-ope                =    "AP"
                     perform  trt-upr-000 thru trt-upr-999
      *              *-------------------------------------------------*
      *              * Nuovo promemoria                                *
      *              *-------------------------------------------------*
           else if   s-ope                =    "NP"
                     perform  new-upr-000 thru new-upr-999
      *              *-------------------------------------------------*
      *              * Richiesta di esistenza promemoria               *
      *              *-------------------------------------------------*
           else if   s-ope                =    "?P"
                     perform  sts-upr-000 thru sts-upr-999
      *              *-------------------------------------------------*
      *              * User System Log                                 *
      *              *-------------------------------------------------*
           else if   s-ope                =    "U+"
                     perform  trt-usl-000 thru trt-usl-999
      *              *-------------------------------------------------*
      *              * Consegna di un percorso archivio                *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Pa"
                     perform  cns-pra-000 thru cns-pra-999
      *              *-------------------------------------------------*
      *              * Richiesta di un percorso archivio               *
      *              *-------------------------------------------------*
           else if   s-ope                =    "PA"
                     perform  rcn-pra-000 thru rcn-pra-999
      *              *-------------------------------------------------*
      *              * Consegna di un percorso archivio lungo          *
      *              *-------------------------------------------------*
           else if   s-ope                =    "Pl"
                     perform  cns-prl-000 thru cns-prl-999
      *              *-------------------------------------------------*
      *              * Richiesta di un percorso archivio lungo         *
      *              *-------------------------------------------------*
           else if   s-ope                =    "PL"
                     perform  rcn-prl-000 thru rcn-prl-999
      *              *-------------------------------------------------*
      *              * Trattamento programma in esecuzione             *
      *              *-------------------------------------------------*
           else if   s-ope                =    "IE"
                     perform  pro-ese-000 thru pro-ese-999
      *              *-------------------------------------------------*
      *              * Trattamento programma in esecuzione - cancel.   *
      *              *-------------------------------------------------*
           else if   s-ope                =    "IC"
                     perform  pro-can-000 thru pro-can-999
      *              *-------------------------------------------------*
      *              * Lettura copia utente programma                  *
      *              *-------------------------------------------------*
           else if   s-ope                =    "UF"
                     perform  ute-fas-000 thru ute-fas-999
      *              *-------------------------------------------------*
      *              * Memorizzazione eventuale modalita' grafica      *
      *              *-------------------------------------------------*
           else if   s-ope                =    "MG"
                     perform  mem-mdg-000 thru mem-mdg-999            .
       main-999.
           exit      program.

      *================================================================*
      *       Routines                                                 *
      *----------------------------------------------------------------*

      *    *===========================================================*
      *    * Open modulo di segreteria                                 *
      *    *-----------------------------------------------------------*
       opn-seg-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore programmi             *
      *              *-------------------------------------------------*
           move      zero                 to   z-sfu-ctr              .
      *              *-------------------------------------------------*
      *              * Se tipo funzionamento foreground                *
      *              *-------------------------------------------------*
           if        s-fun                not  = "B"
                     perform opn-frg-000  thru opn-frg-999
      *              *-------------------------------------------------*
      *              * Se tipo funzionamento background                *
      *              *                                                 *
      *              * N.B.: ATTUALMENTE NON UTILIZZATO                *
      *              *-------------------------------------------------*
           else      perform opn-bkg-000  thru opn-bkg-999            .
       opn-seg-999.
           exit.

      *    *===========================================================*
      *    * Open modulo di segreteria per foreground                  *
      *    *-----------------------------------------------------------*
       opn-frg-000.
      *              *-------------------------------------------------*
      *              * Se modulo in foreground gia' aperto : uscita    *
      *              *-------------------------------------------------*
           if        z-uno                not   = spaces
                     go to opn-frg-999.
           move      "#"                  to    z-uno                 .
      *              *-------------------------------------------------*
      *              * Memorizzazione prefisso o postfisso unico per   *
      *              * files temporanei                                *
      *              *-------------------------------------------------*
           move      s-pat                to   z-ppu                  .
      *              *-------------------------------------------------*
      *              * Memorizzazione Host-Id                          *
      *              *-------------------------------------------------*
           move      s-pmo                to   z-hoi                  .
       opn-frg-050.
      *              *-------------------------------------------------*
      *              * Manipolazione del prefisso o postfisso unico    *
      *              * per files temporanei per eliminare comunque     *
      *              * il suffisso addizionale in caso di esecuzione   *
      *              * di desk-accessory, in quanto il file di appog-  *
      *              * gio per il video dovra' essere comunque in co-  *
      *              * mune per tutti i livelli di profondita' di ese- *
      *              * cuzione desk-accessory                          *
      *              *-------------------------------------------------*
           move      z-ppu                to   w-ppu-nam              .
           move      zero                 to   w-ppu-ctr              .
           inspect   w-ppu-nam        tallying w-ppu-ctr
                                      for all  "."                    .
           if        w-ppu-ctr            not  = 2
                     go to opn-frg-100.
           move      20                   to   w-ppu-ctr              .
       opn-frg-052.
           if        w-ppu-chr
                    (w-ppu-ctr)           not  = "."
                     subtract 1           from w-ppu-ctr
                     go to    opn-frg-052.
       opn-frg-054.
           move      spaces               to   w-ppu-chr
                                              (w-ppu-ctr)             .
           if        w-ppu-ctr            <    20
                     add   1              to   w-ppu-ctr
                     go to opn-frg-054.
       opn-frg-100.
      *              *-------------------------------------------------*
      *              * Open modulo video                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo operazione                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   v-ope                  .
      *                  *---------------------------------------------*
      *                  * Sigla del driver video                      *
      *                  *---------------------------------------------*
           move      s-asx                to   v-alf                  .
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per [xvi], eventual-  *
      *                  * mente depurato dell'eccesso di suffisso per *
      *                  * esecuzione desk-accessory                   *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "tmp"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      spaces               to   o-com                  .
           string    w-ppu-nam  delimited by   spaces
                     ".xvi"     delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   v-edt                  .
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       opn-frg-150.
      *              *-------------------------------------------------*
      *              * Determinazione di un pathname unico per la re-  *
      *              * direzione dello standard output per le opera-   *
      *              * zioni su files in foreground                    *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "tmp"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      spaces               to   o-com                  .
           string    z-ppu      delimited by   spaces
                     ".sto"     delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   z-sto-fls              .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di save video              *
      *              *-------------------------------------------------*
           move      spaces               to   s-svv                  .
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo funzionamento               *
      *              *-------------------------------------------------*
           move      "F"                  to   z-fun                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione livello profondita' programmi  *
      *              *-------------------------------------------------*
           move      spaces               to   z-stk                  .
           move      zero                 to   z-stk-liv              .
      *              *-------------------------------------------------*
      *              * Inizializzazione tabella names-pathnames        *
      *              *-------------------------------------------------*
           move      spaces               to   z-cnp                  .
           move      zero                 to   z-cnp-ncm              .
      *              *-------------------------------------------------*
      *              * Inizializzazione nr moduli transienti attivi    *
      *              *-------------------------------------------------*
           move      spaces               to   z-tma                  .
           move      zero                 to   z-tma-max              .
      *              *-------------------------------------------------*
      *              * Inizializzazione work per parametri stampa      *
      *              *-------------------------------------------------*
           move      spaces               to   z-pss-uts              .
           move      zero                 to   z-pss-upf              .
           move      spaces               to   z-pss-tds              .
           move      spaces               to   z-pss-buf              .
       opn-frg-200.
      *              *-------------------------------------------------*
      *              * Apertura file per variabili e cancellazione di  *
      *              * tutte le variabili di i.p.c. presenti           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per [xse]             *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "tmp"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      spaces               to   o-com                  .
           string    z-ppu      delimited by   spaces
                     ".xse"     delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-xse-pat              .
      *                  *---------------------------------------------*
      *                  * Open [xse]                                  *
      *                  *---------------------------------------------*
           open      i-o    xse                                       .
      *                  *---------------------------------------------*
      *                  * Start su [xse]                              *
      *                  *---------------------------------------------*
           move      spaces               to   xse-rck                .
           start     xse   key not less
                           xse-rck
                           invalid key
                           go to   opn-frg-300.
       opn-frg-220.
      *                  *---------------------------------------------*
      *                  * Next su [xse]                               *
      *                  *---------------------------------------------*
           read      xse   next
                           at end
                           go to opn-frg-300.
      *                  *---------------------------------------------*
      *                  * Delete record da [xse]                      *
      *                  *---------------------------------------------*
           delete    xse   invalid key
                           go to   opn-frg-220.
      *                  *---------------------------------------------*
      *                  * Riciclo su [xse]                            *
      *                  *---------------------------------------------*
           go to     opn-frg-220.
       opn-frg-300.
      *              *-------------------------------------------------*
      *              * Apertura file [auc]                             *
      *              *-------------------------------------------------*
           perform   opn-auc-000          thru opn-auc-999            .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione generali              *
      *              *-------------------------------------------------*
           perform   let-psg-auc-000      thru let-psg-auc-999        .
      *              *-------------------------------------------------*
      *              * Lettura tabella pathnames di esecuzione sosti-  *
      *              * tutivi                                          *
      *              *-------------------------------------------------*
           perform   let-pnx-sos-000      thru let-pnx-sos-999        .
      *              *-------------------------------------------------*
      *              * Apertura file [usl]                             *
      *              *-------------------------------------------------*
           perform   opn-usl-000          thru opn-usl-999            .
      *              *-------------------------------------------------*
      *              * Inizializzazione variabile 'Status Background'  *
      *              *-------------------------------------------------*
           move      w-vsb                to   s-var                  .
           move      "G"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      1                    to   s-car                  .
           move      "N"                  to   s-alf                  .
           perform   put-var-000          thru put-var-999            .
       opn-frg-999.
           exit.

      *    *===========================================================*
      *    * Open modulo di segreteria per background                  *
      *    *                                                           *
      *    * N.B.: ATTUALMENTE NON UTILIZZATO                          *
      *    *-----------------------------------------------------------*
       opn-bkg-000.
      *              *-------------------------------------------------*
      *              * Salvataggio del pathname del file che contiene  *
      *              * l'immagine della segreteria                     *
      *              *-------------------------------------------------*
           move      s-alf                to   w-pth-sgr              .
      *              *-------------------------------------------------*
      *              * Salvataggio del prefisso o postfisso unico per  *
      *              * files temporanei                                *
      *              *-------------------------------------------------*
           move      s-pat                to   z-ppu                  .
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo funzionamento               *
      *              *-------------------------------------------------*
           move      "B"                  to   z-fun                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione aree per tabelle               *
      *              *-------------------------------------------------*
           move      spaces               to   z-stk                  .
           move      spaces               to   z-cnp                  .
           move      spaces               to   z-tma                  .
           move      spaces               to   z-pss-uts              .
           move      zero                 to   z-pss-upf              .
           move      spaces               to   z-pss-tds              .
           move      spaces               to   z-pss-buf              .
      *              *-------------------------------------------------*
      *              * Apertura file [auc]                             *
      *              *-------------------------------------------------*
           perform   opn-auc-000          thru opn-auc-999            .
      *              *-------------------------------------------------*
      *              * Lettura immagine status globale di segreteria   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio lettura record segreteria            *
      *                  *---------------------------------------------*
           move      "OI"                 to   b-ope                  .
           move      z-ppu                to   b-chr                  .
           move      z-run                to   s-num                  .
           call      "swd/mod/prg/obj/mbckgs"
                                         using b
                                               s                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to opn-bkg-300.
      *                  *---------------------------------------------*
      *                  * Estrazione segmenti da 255  bytes da record *
      *                  * segreteria                                  *
      *                  *---------------------------------------------*
           move      1                    to   w-pnt                  .
       opn-bkg-100.
           move      "GT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgs"
                                         using b
                                               s                      .
           if        b-rsc                not  = spaces
                     go to opn-bkg-200.
           move      w-pnt                to   w-svp                  .
           string    b-chr
                     delimited by size    into z
                                  with pointer w-pnt                  .
           if        w-pnt                not  = w-svp
                     go to opn-bkg-100.
       opn-bkg-200.
      *                  *---------------------------------------------*
      *                  * Fine lettura record segreteria              *
      *                  *---------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgs"
                                         using b
                                               s                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                =    spaces
                     go to opn-bkg-400.
       opn-bkg-300.
      *              *-------------------------------------------------*
      *              * Uscita per errore                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Variabile 'Status Background' al valore 'T' *
      *                  *---------------------------------------------*
           move      w-vsb                to   s-var                  .
           move      "G"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      1                    to   s-car                  .
           move      "T"                  to   s-alf                  .
           perform   put-var-000          thru put-var-999            .
      *                  *---------------------------------------------*
      *                  * Emissione messaggio per fatal error         *
      *                  *---------------------------------------------*
           move      "sgr "               to   s-nam                  .
           move      w-pth-sgr            to   s-pat                  .
           move      b-rsc                to   s-sts                  .
           perform   fat-err-000          thru fat-err-999            .
      *                  *---------------------------------------------*
      *                  * Status di uscita ad errore                  *
      *                  *---------------------------------------------*
           move      "#"                  to   s-fun                  .
           go to     opn-bkg-900.
       opn-bkg-400.
      *              *-------------------------------------------------*
      *              * Determinazione di un pathname unico per la re-  *
      *              * direzione dello standard output per le opera-   *
      *              * zioni su files in background                    *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "tmp"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      spaces               to   o-com                  .
           string    z-ppu      delimited by   spaces
                     ".sob"     delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   z-sto-fls              .
      *              *-------------------------------------------------*
      *              * Apertura file per variabili                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per [xse]             *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "tmp"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      spaces               to   o-com                  .
           string    z-ppu      delimited by   spaces
                     ".xse"     delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-xse-pat              .
      *                  *---------------------------------------------*
      *                  * Open [xse]                                  *
      *                  *---------------------------------------------*
           open      i-o    xse                                       .
      *              *-------------------------------------------------*
      *              * Apertura file personalizzazioni                 *
      *              *-------------------------------------------------*
           perform   opn-prs-000          thru opn-prs-999            .
      *              *-------------------------------------------------*
      *              * Apertura file referenze                         *
      *              *-------------------------------------------------*
           perform   opn-ref-000          thru opn-ref-999            .
      *              *-------------------------------------------------*
      *              * Apertura file encoding automatici               *
      *              *-------------------------------------------------*
           perform   opn-enc-000          thru opn-enc-999            .
      *              *-------------------------------------------------*
      *              * Rimemorizzazione tipo funzionamento sovrascrit- *
      *              * to dalla lettura immagine status globale di se- *
      *              * greteria                                        *
      *              *-------------------------------------------------*
           move      "B"                  to   z-fun                  .
      *              *-------------------------------------------------*
      *              * Assestamento area interna segreteria            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Area coppie name-pathname files azienda     *
      *                  *---------------------------------------------*
           move      zero                 to   w-ctr                  .
       opn-bkg-500.
           if        w-ctr                <    z-cnp-ncm
                     add   1              to   w-ctr
                     move  zero           to   z-cnp-foc (w-ctr)
                     move  spaces         to   z-cnp-pmo (w-ctr)
                     go to opn-bkg-500.
      *                  *---------------------------------------------*
      *                  * Area moduli transienti attivi               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ctr                  .
       opn-bkg-600.
           if        w-ctr                <    z-tma-max
                     add   1              to   w-ctr
                     move  spaces         to   z-tma-pmo (w-ctr)
                     move  zero           to   z-tma-nut (w-ctr)
                     go to opn-bkg-600.
           move      zero                 to   z-tma-max              .
      *              *-------------------------------------------------*
      *              * Pathname programma in background da lanciare    *
      *              *-------------------------------------------------*
           move      z-bkg-pat            to   s-pmo                  .
      *              *-------------------------------------------------*
      *              * Status di uscita ad OK                          *
      *              *-------------------------------------------------*
           move      "B"                  to   s-fun                  .
       opn-bkg-900.
      *              *-------------------------------------------------*
      *              * Cancel modulo "mbckgs"                          *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgs"                         .
       opn-bkg-999.
           exit.

      *    *===========================================================*
      *    * Close modulo di segreteria                                *
      *    *-----------------------------------------------------------*
       cls-seg-000.
      *              *-------------------------------------------------*
      *              * Se in foreground e modulo in foreground mai a-  *
      *              * perto : uscita                                  *
      *              *-------------------------------------------------*
           if        z-fun                =    "F" and
                     z-uno                =    spaces
                     go to cls-seg-999.
           move      spaces               to    z-uno                 .
      *              *-------------------------------------------------*
      *              * Se non e' mai stata eseguita la funzione Open,  *
      *              * e quindi non e' mai stato memorizzato il pre-   *
      *              * fisso-postfisso unico per files temporanei, si  *
      *              * esce senza alcuna azione                        *
      *              *-------------------------------------------------*
           if        z-ppu                =    spaces
                     go to cls-seg-999.
      *              *-------------------------------------------------*
      *              * Close file per variabili                        *
      *              *-------------------------------------------------*
           perform   cls-xse-000          thru cls-xse-999            .
      *              *-------------------------------------------------*
      *              * Close file personalizzazioni                    *
      *              *-------------------------------------------------*
           perform   cls-prs-000          thru cls-prs-999            .
      *              *-------------------------------------------------*
      *              * Close file referenze                            *
      *              *-------------------------------------------------*
           perform   cls-ref-000          thru cls-ref-999            .
      *              *-------------------------------------------------*
      *              * Close file encoding automatici                  *
      *              *-------------------------------------------------*
           perform   cls-enc-000          thru cls-enc-999            .
      *              *-------------------------------------------------*
      *              * Chiusura file [auc]                             *
      *              *-------------------------------------------------*
           perform   cls-auc-000          thru cls-auc-999            .
      *              *-------------------------------------------------*
      *              * Chiusura sessione utente                        *
      *              *-------------------------------------------------*
           move      "logout"             to   z-fas                  .
           perform   trt-usl-000          thru trt-usl-999            .
      *              *-------------------------------------------------*
      *              * Chiusura file [usl]                             *
      *              *-------------------------------------------------*
           perform   cls-usl-000          thru cls-usl-999            .
       cls-seg-999.
           exit.

      *    *===========================================================*
      *    * Richiesta prefisso o postfisso unico per files temporanei *
      *    *-----------------------------------------------------------*
       ppu-ric-000.
           move      z-ppu                to   s-pat                  .
       ppu-ric-999.
           exit.

      *    *===========================================================*
      *    * Richiesta Host-Id                                         *
      *    *-----------------------------------------------------------*
       hoi-ric-000.
           move      z-hoi                to   s-pat                  .
       hoi-ric-999.
           exit.

      *    *===========================================================*
      *    * Close file variabili di segreteria                        *
      *    *-----------------------------------------------------------*
       cls-xse-000.
           close      xse                                             .
       cls-xse-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione tipo di Run-time in uso                     *
      *    *-----------------------------------------------------------*
       run-dic-000.
      *              *-------------------------------------------------*
      *              * Sono riconosciuti tre tipi di Run-Time :        *
      *              * - 00 : Acucobol 85                              *
      *              * - 01 : Austec RM-Master Cobol 74                *
      *              * - 02 : Ryan-McFarland RM Cobol 85               *
      *              * Per default si assume Acucobol 85               *
      *              *-------------------------------------------------*
           if        s-num                =    01
                     move  01             to   z-run
           else if   s-num                =    02
                     move  02             to   z-run
           else      move  00             to   z-run                  .
       run-dic-999.
           exit.

      *    *===========================================================*
      *    * Richiesta tipo di Run-time in uso                         *
      *    *-----------------------------------------------------------*
       run-ric-000.
           move      z-run                to   s-num                  .
       run-ric-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione ragione sociale licenziatario               *
      *    *-----------------------------------------------------------*
       dic-lcu-000.
           move      s-alf                to   z-lcu                  .
       dic-lcu-999.
           exit.

      *    *===========================================================*
      *    * Richiesta ragione sociale licenziatario                   *
      *    *-----------------------------------------------------------*
       ric-lcu-000.
           move      z-lcu                to   s-alf                  .
       ric-lcu-999.
           exit.

      *    *===========================================================*
      *    * Open file [auc]                                           *
      *    *-----------------------------------------------------------*
       opn-auc-000.
      *              *-------------------------------------------------*
      *              * Preparazione pathname per [auc]                 *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "fpx"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "auc"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-auc-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di Open                              *
      *              *-------------------------------------------------*
           open      i-o    auc                                       .
       opn-auc-999.
           exit.

      *    *===========================================================*
      *    * Close file [auc]                                          *
      *    *-----------------------------------------------------------*
       cls-auc-000.
           close      auc                                             .
       cls-auc-999.
           exit.

      *    *===========================================================*
      *    * Open file [usl]                                           *
      *    *-----------------------------------------------------------*
       opn-usl-000.
      *              *-------------------------------------------------*
      *              * Test su personalizzazione generale relativa     *
      *              *-------------------------------------------------*
           if        w-psg-rsv-usl        not  = "S"
                     go to opn-usl-999.
      *              *-------------------------------------------------*
      *              * Preparazione pathname per [usl]                 *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "fpx"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "usl"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-usl-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di Open                              *
      *              *-------------------------------------------------*
           open      i-o    usl                                       .
       opn-usl-999.
           exit.

      *    *===========================================================*
      *    * Close file [usl]                                          *
      *    *-----------------------------------------------------------*
       cls-usl-000.
      *              *-------------------------------------------------*
      *              * Test su personalizzazione generale relativa     *
      *              *-------------------------------------------------*
           if        w-psg-rsv-usl        not  = "S"
                     go to cls-usl-999.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           close      usl                                             .
       cls-usl-999.
           exit.

      *    *===========================================================*
      *    * Trattamento User System Log                               *
      *    *-----------------------------------------------------------*
       trt-usl-000.
      *              *-------------------------------------------------*
      *              * Test su personalizzazione generale relativa     *
      *              *-------------------------------------------------*
           if        w-psg-rsv-usl        not  = "S"
                     go to trt-usl-999.
      *              *-------------------------------------------------*
      *              * Test su si/no User System Log utente            *
      *              *-------------------------------------------------*
           if        z-usl                not  = "S"
                     go to trt-usl-999.
       trt-usl-100.
      *              *-------------------------------------------------*
      *              * Raccolta parametri per la scrittura             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prefisso o postfisso unico files temporanei *
      *                  *---------------------------------------------*
           move      z-ppu                to   usl-ide-ppu
                                               usl-ide-ppu-2
                                               usl-ide-ppu-3          .
      *                  *---------------------------------------------*
      *                  * Data di sistema                             *
      *                  *---------------------------------------------*
           perform   est-sdt-000          thru est-sdt-999            .
      *                  *---------------------------------------------*
      *                  * Normalizzazione secolo/anno                 *
      *                  *---------------------------------------------*
           perform   nor-sec-000          thru nor-sec-999            .
      *                  *---------------------------------------------*
      *                  * Data                                        *
      *                  *---------------------------------------------*
           move      s-dat                to   usl-ide-dat
                                               usl-ide-dat-2
                                               usl-ide-dat-3          .
      *                  *---------------------------------------------*
      *                  * Tempo di sistema                            *
      *                  *---------------------------------------------*
           move      s-tim                to   usl-ide-tim
                                               usl-ide-tim-2
                                               usl-ide-tim-3          .
      *                  *---------------------------------------------*
      *                  * Utente                                      *
      *                  *---------------------------------------------*
           move      z-ute                to   usl-ide-ute
                                               usl-ide-ute-2
                                               usl-ide-ute-3          .
      *                  *---------------------------------------------*
      *                  * Azienda                                     *
      *                  *---------------------------------------------*
           move      z-azi                to   usl-ide-azi
                                               usl-ide-azi-2
                                               usl-ide-azi-3          .
      *                  *---------------------------------------------*
      *                  * Fase                                        *
      *                  *---------------------------------------------*
           move      z-fas                to   usl-ide-fas
                                               usl-ide-fas-2
                                               usl-ide-fas-3          .
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento                          *
      *                  *---------------------------------------------*
           move      s-sts                to   usl-ide-tfu
                                               usl-ide-tfu-2
                                               usl-ide-tfu-3          .
      *                  *---------------------------------------------*
      *                  * File name                                   *
      *                  *---------------------------------------------*
           move      s-nam                to   usl-ide-nam            .
      *                  *---------------------------------------------*
      *                  * Dipendenza in uso                           *
      *                  *---------------------------------------------*
           move      z-dpz                to   usl-ide-dpz            .
      *                  *---------------------------------------------*
      *                  * Terminale                                   *
      *                  *---------------------------------------------*
           move      z-ter                to   usl-ide-ter            .
      *                  *---------------------------------------------*
      *                  * IP                                          *
      *                  *---------------------------------------------*
           move      z-ipa                to   usl-ide-ipa            .
      *                  *---------------------------------------------*
      *                  * Chiave                                      *
      *                  *---------------------------------------------*
           move      s-alf                to   usl-ide-key            .
      *                  *---------------------------------------------*
      *                  * Area libera                                 *
      *                  *---------------------------------------------*
           move      spaces               to   usl-alx-exp            .
       trt-usl-300.
      *              *-------------------------------------------------*
      *              * Write                                           *
      *              *-------------------------------------------------*
           write     usl-rec invalid key
                             go to   trt-usl-600.
           go to     trt-usl-999.
       trt-usl-600.
      *              *-------------------------------------------------*
      *              * Rewrite                                         *
      *              *-------------------------------------------------*
           rewrite   usl-rec invalid key
                             go to   trt-usl-300.
       trt-usl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     trt-usl-999.
       trt-usl-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento programmi utilizzati dall'utente            *
      *    *-----------------------------------------------------------*
       agg-sfu-000.
      *              *-------------------------------------------------*
      *              * Ricerca se elemento memorizzato                 *
      *              *-------------------------------------------------*
           set       z-sfu-inx            to   1                      .
           search    z-sfu-ele
                     when   z-sfu-fas
                           (z-sfu-inx)    =    z-fas and
                            z-sfu-ute
                           (z-sfu-inx)    =    z-ute
                            go to agg-sfu-900.
       agg-sfu-200.
      *              *-------------------------------------------------*
      *              * Aggiunta elemento in tabella                    *
      *              *-------------------------------------------------*
           if        z-sfu-ctr            <    256
                     add  1               to   z-sfu-ctr              .
           move      z-fas                to   z-sfu-fas (z-sfu-ctr)  .
           move      z-ute                to   z-sfu-ute (z-sfu-ctr)  .
       agg-sfu-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     agg-sfu-999.
       agg-sfu-999.
           exit.

      *    *===========================================================*
      *    * Richiesta elemento Utente e Fase                          *
      *    *-----------------------------------------------------------*
       ute-fas-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      spaces               to   s-fas                  .   
           move      spaces               to   s-ute                  .   
      *              *-------------------------------------------------*
      *              * Test su utente                                  *
      *              *-------------------------------------------------*
           if        z-ute                not  = z-sfu-ute (s-foc)
                     go to ute-fas-900.
      *              *-------------------------------------------------*
      *              * Test su indice                                  *
      *              *-------------------------------------------------*
           if        s-foc                =    zero or
                     s-foc                >    256
                     go to ute-fas-900.
      *              *-------------------------------------------------*
      *              * Valori in uscita                                *
      *              *-------------------------------------------------*
           move      z-sfu-fas (s-foc)    to   s-fas                  .
           move      z-sfu-ute (s-foc)    to   s-ute                  .
       ute-fas-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ute-fas-999.
       ute-fas-999.
           exit.

      *    *===========================================================*
      *    * Open file personalizzazioni                               *
      *    *-----------------------------------------------------------*
       opn-prs-000.
      *              *-------------------------------------------------*
      *              * Preparazione pathname, concatenando dal pathna- *
      *              * me di base per files normali dell'azienda       *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           perform   pnb-fls-000          thru pnb-fls-999            .
           move      "A5"                 to   o-ope                  .
           move      s-pat                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "prs"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-prs-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di i-o di Open                       *
      *              *-------------------------------------------------*
           open      i-o    prs                                       .
       opn-prs-999.
           exit.

      *    *===========================================================*
      *    * Close file personalizzazioni                              *
      *    *-----------------------------------------------------------*
       cls-prs-000.
      *              *-------------------------------------------------*
      *              * Se pathname a spaces : no chiusura              *
      *              *-------------------------------------------------*
           if        f-prs-pat            =    spaces
                     go to cls-prs-500.
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           close      prs                                             .
       cls-prs-500.
      *              *-------------------------------------------------*
      *              * Pathname : a spaces                             *
      *              *-------------------------------------------------*
           move      spaces               to   f-prs-pat              .
       cls-prs-999.
           exit.

      *    *===========================================================*
      *    * Open file referenze                                       *
      *    *-----------------------------------------------------------*
       opn-ref-000.
      *              *-------------------------------------------------*
      *              * Preparazione pathname, concatenando dal pathna- *
      *              * me di base per files normali dell'azienda       *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           perform   pnb-fls-000          thru pnb-fls-999            .
           move      "A5"                 to   o-ope                  .
           move      s-pat                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "ref"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-ref-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di i-o di Open                       *
      *              *-------------------------------------------------*
           open      i-o    ref                                       .
       opn-ref-999.
           exit.

      *    *===========================================================*
      *    * Close file referenze                                      *
      *    *-----------------------------------------------------------*
       cls-ref-000.
      *              *-------------------------------------------------*
      *              * Se pathname a spaces : no chiusura              *
      *              *-------------------------------------------------*
           if        f-ref-pat            =    spaces
                     go to cls-ref-500.
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           close      ref                                             .
       cls-ref-500.
      *              *-------------------------------------------------*
      *              * Pathname : a spaces                             *
      *              *-------------------------------------------------*
           move      spaces               to   f-ref-pat              .
       cls-ref-999.
           exit.

      *    *===========================================================*
      *    * Open file encoding automatici                             *
      *    *-----------------------------------------------------------*
       opn-enc-000.
      *              *-------------------------------------------------*
      *              * Preparazione pathname, concatenando dal pathna- *
      *              * me di base per files normali dell'azienda       *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           perform   pnb-fls-000          thru pnb-fls-999            .
           move      "A5"                 to   o-ope                  .
           move      s-pat                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "enc"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-enc-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di i-o di Open                       *
      *              *-------------------------------------------------*
           open      i-o    enc                                       .
       opn-enc-999.
           exit.

      *    *===========================================================*
      *    * Close file encoding automatici                            *
      *    *-----------------------------------------------------------*
       cls-enc-000.
      *              *-------------------------------------------------*
      *              * Se pathname a spaces : no chiusura              *
      *              *-------------------------------------------------*
           if        f-enc-pat            =    spaces
                     go to cls-enc-500.
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           close      enc                                             .
       cls-enc-500.
      *              *-------------------------------------------------*
      *              * Pathname : a spaces                             *
      *              *-------------------------------------------------*
           move      spaces               to   f-enc-pat              .
       cls-enc-999.
           exit.

      *    *===========================================================*
      *    * Open file promemoria utente                               *
      *    *-----------------------------------------------------------*
       opn-upr-000.
      *              *-------------------------------------------------*
      *              * Preparazione pathname, concatenando dal pathna- *
      *              * me di base per files normali dell'azienda       *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           perform   pnb-fls-000          thru pnb-fls-999            .
           move      "A5"                 to   o-ope                  .
           move      s-pat                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "upr"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-upr-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di i-o di Open                       *
      *              *-------------------------------------------------*
           open      i-o    upr                                       .
       opn-upr-999.
           exit.

      *    *===========================================================*
      *    * Close file promemoria utente                              *
      *    *-----------------------------------------------------------*
       cls-upr-000.
      *              *-------------------------------------------------*
      *              * Se pathname a spaces : no chiusura              *
      *              *-------------------------------------------------*
           if        f-upr-pat            =    spaces
                     go to cls-upr-500.
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
           close      upr                                             .
       cls-upr-500.
      *              *-------------------------------------------------*
      *              * Pathname : a spaces                             *
      *              *-------------------------------------------------*
           move      spaces               to   f-upr-pat              .
       cls-upr-999.
           exit.

      *    *===========================================================*
      *    * Put Variable                                              *
      *    *-----------------------------------------------------------*
       put-var-000.
           move      "VA"                 to   xse-tpk                .
           if        s-dop                =    "G"
                     move     "G"         to   w-ipc-tva
                     move     zero        to   w-ipc-liv
                     move     spaces      to   w-ipc-dop
                     go to    put-var-100.
           move      "P"                  to   w-ipc-tva              .
           move      z-stk-liv            to   w-ipc-liv              .
           if        s-dop                =    "+"
                     move     "-"         to   w-ipc-dop
                     add      1           to   w-ipc-liv
           else if   s-dop                =    "-"
                     move     "+"         to   w-ipc-dop
                     subtract 1           from w-ipc-liv
           else      move     "="         to   w-ipc-dop              .
       put-var-100.
           move      s-var                to   w-ipc-var              .
           move      w-ipc-key            to   xse-vlk                .
           move      spaces               to   w-ipc-rcd              .
           move      s-tip                to   w-ipc-tip              .
           if        s-tip                =    "A"
                     move  s-car          to   w-ipc-car
                     move  zero           to   w-ipc-dec
                     move  spaces         to   w-ipc-sgn
                     move  s-alf          to   w-ipc-alf
           else if   s-tip                =    "N"   or
                     s-tip                =    "V"   or
                     s-tip                =    "P"
                     move  s-car          to   w-ipc-car
                     move  s-dec          to   w-ipc-dec
                     move  s-sgn          to   w-ipc-sgn
                     move  s-num          to   w-ipc-num
           else if   s-tip                =    "D"
                     move  zero           to   w-ipc-car
                                               w-ipc-dec
                     move  spaces         to   w-ipc-sgn
                     move  s-dat          to   w-ipc-dat              .
           move      w-ipc-rcd            to   xse-rcd                .
       put-var-300.
           write     xse-rec invalid key
                             go to   put-var-600.
           go to     put-var-999.
       put-var-600.
           rewrite   xse-rec invalid key
                             go to   put-var-300.
       put-var-999.
           exit.

      *    *===========================================================*
      *    * Get Variable                                              *
      *    *-----------------------------------------------------------*
       get-var-000.
           move      "VA"                 to   xse-tpk                .
           if        s-dop                =    "G"
                     move  "G"            to   w-ipc-tva
                     move  zero           to   w-ipc-liv
                     move  spaces         to   w-ipc-dop
           else      move  "P"            to   w-ipc-tva
                     move  z-stk-liv      to   w-ipc-liv
                     move  s-dop          to   w-ipc-dop              .
           move      s-var                to   w-ipc-var              .
           move      w-ipc-key            to   xse-vlk                .
           read      xse   with no lock
                           invalid key
                           move    "#"    to   s-ves
                           move    spaces to   s-tip
                           move    zero   to   s-car
                                               s-dec
                           move    spaces to   s-sgn
                                               s-alf
                           move    zero   to   s-num
                                               s-dat
                           go to   get-var-999.
           move      xse-rcd              to   w-ipc-rcd              .
           move      spaces               to   s-ves                  .
           move      w-ipc-tip            to   s-tip                  .
           move      w-ipc-car            to   s-car                  .
           move      w-ipc-dec            to   s-dec                  .
           move      w-ipc-sgn            to   s-sgn                  .
           if        w-ipc-tip            =    "A"
                     move  w-ipc-alf      to   s-alf
           else if   w-ipc-tip            =    "N" or
                     w-ipc-tip            =    "V" or
                     w-ipc-tip            =    "P"
                     move  w-ipc-num      to   s-num
           else if   w-ipc-tip            =    "D"
                     move  w-ipc-dat      to   s-dat                  .
       get-var-999.
           exit.

      *    *===========================================================*
      *    * Clear Variable                                            *
      *    *-----------------------------------------------------------*
       clr-var-000.
           perform   get-var-000          thru get-var-999            .
           if        s-ves                not  = spaces
                     go to clr-var-999.
           delete    xse   invalid key
                           go to   clr-var-000.
       clr-var-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione generale                        *
      *    *-----------------------------------------------------------*
       get-pge-000.
      *              *-------------------------------------------------*
      *              * Sono riconosciute le seguenti personalizzazioni *
      *              * generali :                                      *
      *              *                                                 *
      *              * - snx-azi : Si/No visualizzazione del codice    *
      *              *             azienda                             *
      *              *                                                 *
      *              * - tem-spl : Template per spooler di stampa      *
      *              *                                                 *
      *              * - iod-lor : Incremento o decremento per Left o  *
      *              *             Rght in visualizzazione archivi di  *
      *              *             stampa                              *
      *              *                                                 *
      *              * - tem-bss : Template per il blocco dello spoo-  *
      *              *             ler di stampa                       *
      *              *                                                 *
      *              * - cnt-vpf : Parametri per il controllo della    *
      *              *             visualizzazione archivi stampa      *
      *              *                                                 *
      *              * - cnt-pg1 : Parametro generale libero 1         *
      *              *                                                 *
      *              * - cnt-pg2 : Parametro generale libero 2         *
      *              *                                                 *
      *              * - cnt-pg3 : Parametro generale libero 3         *
      *              *                                                 *
      *              * - msa-isp : Mail server                         *
      *              *                                                 *
      *              * - eml-psm : Postmaster                          *
      *              *                                                 *
      *              * - uid-ssm : User ID per Server SMTP             *
      *              *                                                 *
      *              * - pwd-ssm : Password per Server SMTP            *
      *              *                                                 *
      *              * - pdf-pde : Password documenti PDF              *
      *              *                                                 *
      *              * - ads-ssm : Agente invio documenti              *
      *              *                                                 *
      *              * - pwd-sql : Password MySQL                      *
      *              *                                                 *
      *              * Inoltre sono riconosciute le seguenti richie-   *
      *              * ste di informazione, che vengono ricavate dai   *
      *              * parametri precedenti :                          *
      *              *                                                 *
      *              * - vpf-24l : Si/No visualizzazione archivi stam- *
      *              *             pa su 24 linee, anziche' 18         *
      *              *              - S : Si                           *
      *              *              - N : No                           *
      *              *                                                 *
      *              * - scr-132 : Si/No trattamento dello schermo am- *
      *              *             missibile su 132 colonne anziche'   *
      *              *             su 80 colonne                       *
      *              *              - S : Si                           *
      *              *              - N : No                           *
      *              *                                                 *
      *              *-------------------------------------------------*
           if        s-alf                =    "snx-azi"
                     move  z-vca          to   s-alf
                     go to get-pge-999.
      *
           if        s-alf                =    "tem-spl"
                     move  z-tem          to   s-alf
                     go to get-pge-999.
      *
           if        s-alf                =    "iod-lor"
                     move  z-iod          to   s-num
                     go to get-pge-999.
      *
           if        s-alf                =    "tem-bss"
                     move  z-tbs          to   s-alf
                     go to get-pge-999.
      *
           if        s-alf                =    "cnt-vpf"
                     move  z-vpf          to   s-alf
                     go to get-pge-999.
      *
           if        s-alf                =    "cnt-pg1"
                     move  z-pg1          to   s-alf
                     go to get-pge-999.
      *
           if        s-alf                =    "cnt-pg2"
                     move  z-pg2          to   s-alf
                     go to get-pge-999.
      *
           if        s-alf                =    "cnt-pg3"
                     move  z-pg3          to   s-alf
                     go to get-pge-999.
      *
           if        s-alf                =    "msa-isp"
                     move  z-msa          to   s-alf
                     go to get-pge-999.
      *
           if        s-alf                =    "uid-ssm"
                     move  z-uid          to   s-alf
                     go to get-pge-999.
      *
           if        s-alf                =    "pwd-ssm"
                     move  z-ppi          to   s-alf
                     go to get-pge-999.
      *
           if        s-alf                =    "pdf-pde"
                     move  z-ppe          to   s-alf
                     go to get-pge-999.
      *
           if        s-alf                =    "ads-ssm"
                     move  z-aid          to   s-alf
                     go to get-pge-999.
      *
           if        s-alf                =    "pwd-sql"
                     move  z-pws          to   s-alf
                     go to get-pge-999.
      *
           if        s-alf                =    "eml-psm"
                     move  z-psm          to   s-alf
                     go to get-pge-999.
      *
           if        s-alf                =    "vpf-24l"
                     if    z-vpf (1 : 1)  =    "T"
                           move  "S"      to   s-alf
                           go to get-pge-999
                     else  move  "N"      to   s-alf
                           go to get-pge-999.
      *
           if        s-alf                =    "scr-132"
                     if    z-vpf (2 : 1)  =    "S"
                           move  "S"      to   s-alf
                           go to get-pge-999
                     else  move  "N"      to   s-alf
                           go to get-pge-999.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
           move      spaces               to   s-alf                  .
           move      zero                 to   s-num                  .
           move      zero                 to   s-dat                  .
       get-pge-999.
           exit.

      *    *===========================================================*
      *    * Separazione chiave personalizzazione o referenza in com-  *
      *    * ponenti prima e dopo il carattere chiocciola              *
      *    *-----------------------------------------------------------*
       key-prs-ref-000.
           move      zero                 to   w-key-por-pnt          .
           move      spaces               to   w-key-por-pre          .
           move      spaces               to   w-key-por-pos          .
           inspect   w-key-por-key    tallying w-key-por-pnt
                                for characters
                                before initial "@"                    .
           if        w-key-por-pnt        =    40
                     move  w-key-por-key
                                          to   w-key-por-pre
                     go to key-prs-ref-999.
           move      w-key-por-key
                    (1 : w-key-por-pnt)   to   w-key-por-pre          .
           add       2                    to   w-key-por-pnt          .
           if        w-key-por-pnt        >    40
                     go to key-prs-ref-999.
           move      40                   to   w-key-por-lun          .
           subtract  w-key-por-pnt        from w-key-por-lun          .
           add       1                    to   w-key-por-lun          .
           move      w-key-por-key
                    (w-key-por-pnt :
                     w-key-por-lun)       to   w-key-por-pos          .
       key-prs-ref-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione                                 *
      *    *-----------------------------------------------------------*
       get-prs-000.
      *              *-------------------------------------------------*
      *              * Salvataggio nome personalizzazione completo     *
      *              *-------------------------------------------------*
           move      s-alf                to   w-prs-def-cod          .
      *              *-------------------------------------------------*
      *              * Salvataggio eventuale numero progressivo        *
      *              *-------------------------------------------------*
           move      s-num                to   w-prs-def-prg          .
      *              *-------------------------------------------------*
      *              * Separazione chiave in componenti prima e dopo   *
      *              * il valore chiocciola                            *
      *              *-------------------------------------------------*
           move      w-prs-def-cod        to   w-key-por-key          .
           perform   key-prs-ref-000      thru key-prs-ref-999        .
       get-prs-100.
      *              *-------------------------------------------------*
      *              * Lettura della definizione                       *
      *              *-------------------------------------------------*
       get-prs-125.
      *                  *---------------------------------------------*
      *                  * Operazione di lettura, solo per la compo-   *
      *                  * nente che precede la chiocciola, e devia-   *
      *                  * zione secondo l'esito                       *
      *                  *---------------------------------------------*
           move      "dps "               to   auc-tre                .
           move      w-key-por-pre        to   auc-kre                .
           read      auc   with no lock
                           invalid key
                           go to   get-prs-150.
           go to     get-prs-175.
       get-prs-150.
      *                  *---------------------------------------------*
      *                  * Se definizione non trovata                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di valore non esistente         *
      *                      *-----------------------------------------*
           move      "#"                  to   s-ves                  .
      *                      *-----------------------------------------*
      *                      * Normalizzazione definizione             *
      *                      *-----------------------------------------*
           move      spaces               to   s-tip                  .
           move      zero                 to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
      *                      *-----------------------------------------*
      *                      * Normalizzazione valore                  *
      *                      *-----------------------------------------*
           move      spaces               to   s-alf                  .
           move      zero                 to   s-num                  .
           move      zero                 to   s-dat                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     get-prs-999.
       get-prs-175.
      *                  *---------------------------------------------*
      *                  * Se definizione trovata                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Record letto in area di ridefinizione   *
      *                      *-----------------------------------------*
           move      auc-dat              to   w-dps                  .
      *                      *-----------------------------------------*
      *                      * Salvataggio della definizione           *
      *                      *-----------------------------------------*
           move      w-dps-tip-dps        to   w-prs-def-tip          .
           move      w-dps-car-dps        to   w-prs-def-car          .
           move      w-dps-dec-dps        to   w-prs-def-dec          .
           move      w-dps-sgn-dps        to   w-prs-def-sgn          .
           move      w-dps-som-dps        to   w-prs-def-som          .
      *                      *-----------------------------------------*
      *                      * Test su numero progressivo              *
      *                      *-----------------------------------------*
           if        w-prs-def-som        not  = "M"
                     move  zero           to   w-prs-def-prg          .
       get-prs-300.
      *              *-------------------------------------------------*
      *              * Lettura del valore                              *
      *              *-------------------------------------------------*
       get-prs-325.
      *                  *---------------------------------------------*
      *                  * Operazione di lettura, sul valore completo  *
      *                  *---------------------------------------------*
           move      "prs "               to   prs-tre                .
           move      w-prs-def-cod        to   prs-cod                .
           move      w-prs-def-prg        to   prs-prg                .
           read      prs   with no lock
                           invalid key
                           go to   get-prs-350.
           go to     get-prs-375.
       get-prs-350.
      *                  *---------------------------------------------*
      *                  * Se valore non trovato                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di valore non esistente         *
      *                      *-----------------------------------------*
           move      "#"                  to   s-ves                  .
      *                      *-----------------------------------------*
      *                      * Normalizzazione valore                  *
      *                      *-----------------------------------------*
           move      spaces               to   s-alf                  .
           move      zero                 to   s-num                  .
           move      zero                 to   s-dat                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     get-prs-999.
       get-prs-375.
      *                  *---------------------------------------------*
      *                  * Se valore trovato                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di valore esistente             *
      *                      *-----------------------------------------*
           move      spaces               to   s-ves                  .
      *                      *-----------------------------------------*
      *                      * Preparazione definizione                *
      *                      *-----------------------------------------*
           move      w-prs-def-tip        to   s-tip                  .
           move      w-prs-def-car        to   s-car                  .
           move      w-prs-def-dec        to   s-dec                  .
           move      w-prs-def-sgn        to   s-sgn                  .
      *                      *-----------------------------------------*
      *                      * Preparazione del valore                 *
      *                      *-----------------------------------------*
           if        w-prs-def-tip        =    "A"
                     move  prs-alf        to   s-alf
                     move  zero           to   s-num
                     move  zero           to   s-dat
           else if   w-prs-def-tip        =    "N"
                     move  spaces         to   s-alf
                     move  prs-num        to   s-num
                     move  zero           to   s-dat
           else if   w-prs-def-tip        =    "D"
                     move  spaces         to   s-alf
                     move  zero           to   s-num
                     move  prs-dat        to   s-dat
           else      move  spaces         to   s-alf
                     move  zero           to   s-num
                     move  zero           to   s-dat                  .
       get-prs-999.
           exit.

      *    *===========================================================*
      *    * Start su di una personalizzazione                         *
      *    *-----------------------------------------------------------*
       str-prs-000.
      *              *-------------------------------------------------*
      *              * Salvataggio nome personalizzazione              *
      *              *-------------------------------------------------*
           move      s-alf                to   w-prs-def-cod          .
      *              *-------------------------------------------------*
      *              * Separazione chiave in componenti prima e dopo   *
      *              * il valore chiocciola                            *
      *              *-------------------------------------------------*
           move      w-prs-def-cod        to   w-key-por-key          .
           perform   key-prs-ref-000      thru key-prs-ref-999        .
      *              *-------------------------------------------------*
      *              * Azzeramento numero progressivo                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-prs-def-prg          .
      *              *-------------------------------------------------*
      *              * Azzeramento controllo su next                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-prs-def-nxt          .
       str-prs-100.
      *              *-------------------------------------------------*
      *              * Lettura della definizione                       *
      *              *-------------------------------------------------*
       str-prs-125.
      *                  *---------------------------------------------*
      *                  * Operazione di lettura, solo per la compo-   *
      *                  * nente che precede la chiocciola, e devia-   *
      *                  * zione secondo l'esito                       *
      *                  *---------------------------------------------*
           move      "dps "               to   auc-tre                .
           move      w-key-por-pre        to   auc-kre                .
           read      auc   with no lock
                           invalid key
                           go to   str-prs-150.
           go to     str-prs-175.
       str-prs-150.
      *                  *---------------------------------------------*
      *                  * Se definizione non trovata                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di valore non esistente         *
      *                      *-----------------------------------------*
           move      "#"                  to   s-ves                  .
      *                      *-----------------------------------------*
      *                      * Normalizzazione definizione             *
      *                      *-----------------------------------------*
           move      spaces               to   s-tip                  .
           move      zero                 to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
      *                      *-----------------------------------------*
      *                      * Normalizzazione valore                  *
      *                      *-----------------------------------------*
           move      spaces               to   s-alf                  .
           move      zero                 to   s-num                  .
           move      zero                 to   s-dat                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     str-prs-999.
       str-prs-175.
      *                  *---------------------------------------------*
      *                  * Se definizione trovata                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Record letto in area di ridefinizione   *
      *                      *-----------------------------------------*
           move      auc-dat              to   w-dps                  .
      *                      *-----------------------------------------*
      *                      * Salvataggio della definizione           *
      *                      *-----------------------------------------*
           move      w-dps-tip-dps        to   w-prs-def-tip          .
           move      w-dps-car-dps        to   w-prs-def-car          .
           move      w-dps-dec-dps        to   w-prs-def-dec          .
           move      w-dps-sgn-dps        to   w-prs-def-sgn          .
           move      w-dps-som-dps        to   w-prs-def-som          .
       str-prs-300.
      *              *-------------------------------------------------*
      *              * Start effettiva                                 *
      *              *-------------------------------------------------*
       str-prs-325.
      *                  *---------------------------------------------*
      *                  * Operazione di start, sul valore completo    *
      *                  *---------------------------------------------*
           move      "prs "               to   prs-tre                .
           move      w-prs-def-cod        to   prs-cod                .
           move      w-prs-def-prg        to   prs-prg                .
           start     prs   key not less
                           prs-rck
                           invalid key
                           go to   str-prs-350.
           go to     str-prs-375.
       str-prs-350.
      *                  *---------------------------------------------*
      *                  * Se start errata                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di valore non esistente         *
      *                      *-----------------------------------------*
           move      "#"                  to   s-ves                  .
      *                      *-----------------------------------------*
      *                      * Normalizzazione valore                  *
      *                      *-----------------------------------------*
           move      spaces               to   s-alf                  .
           move      zero                 to   s-num                  .
           move      zero                 to   s-dat                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     str-prs-999.
       str-prs-375.
      *                  *---------------------------------------------*
      *                  * Se valore trovato                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di valore esistente             *
      *                      *-----------------------------------------*
           move      spaces               to   s-ves                  .
      *                      *-----------------------------------------*
      *                      * Preparazione definizione                *
      *                      *-----------------------------------------*
           move      w-prs-def-tip        to   s-tip                  .
           move      w-prs-def-car        to   s-car                  .
           move      w-prs-def-dec        to   s-dec                  .
           move      w-prs-def-sgn        to   s-sgn                  .
       str-prs-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale di una personalizzazione              *
      *    *-----------------------------------------------------------*
       nxt-prs-000.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale e deviazione in funzione    *
      *              * dell'esito                                      *
      *              *-------------------------------------------------*
           read      prs    next
                            with no lock
                            at end
                            go to nxt-prs-100.
           go to     nxt-prs-200.
       nxt-prs-100.
      *              *-------------------------------------------------*
      *              * Se fine file                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di valore non esistente             *
      *                  *---------------------------------------------*
           move      "#"                  to   s-ves                  .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valore                      *
      *                  *---------------------------------------------*
           move      spaces               to   s-alf                  .
           move      zero                 to   s-num                  .
           move      zero                 to   s-dat                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     nxt-prs-999.
       nxt-prs-200.
      *              *-------------------------------------------------*
      *              * Se non fine file                                *
      *              *-------------------------------------------------*
       nxt-prs-225.
      *                  *---------------------------------------------*
      *                  * Test max, sul valore completo, se non supe- *
      *                  * to, come per fine fine                      *
      *                  *---------------------------------------------*
           if        prs-tre              not  = "prs "
                     go to nxt-prs-100.
           if        prs-cod              not  = w-prs-def-cod
                     go to nxt-prs-100.
       nxt-prs-250.
      *                  *---------------------------------------------*
      *                  * Selezione, se non superata : rilettura      *
      *                  *---------------------------------------------*
           if        prs-prg              <    001 or
                     prs-prg              >    999
                     go to nxt-prs-000.
       nxt-prs-275.
      *                  *---------------------------------------------*
      *                  * Memorizzazione ultimo progressivo letto     *
      *                  *---------------------------------------------*
           move      prs-prg              to   w-prs-def-nxt          .
       nxt-prs-300.
      *                  *---------------------------------------------*
      *                  * Segnale di valore esistente                 *
      *                  *---------------------------------------------*
           move      spaces               to   s-ves                  .
       nxt-prs-325.
      *                  *---------------------------------------------*
      *                  * Preparazione del valore                     *
      *                  *---------------------------------------------*
           if        w-prs-def-tip        =    "A"
                     move  prs-alf        to   s-alf
                     move  zero           to   s-num
                     move  zero           to   s-dat
           else if   w-prs-def-tip        =    "N"
                     move  spaces         to   s-alf
                     move  prs-num        to   s-num
                     move  zero           to   s-dat
           else if   w-prs-def-tip        =    "D"
                     move  spaces         to   s-alf
                     move  zero           to   s-num
                     move  prs-dat        to   s-dat
           else      move  spaces         to   s-alf
                     move  zero           to   s-num
                     move  zero           to   s-dat                  .
       nxt-prs-999.
           exit.

      *    *===========================================================*
      *    * Numero progressivo di una personalizzazione               *
      *    *-----------------------------------------------------------*
       num-prs-000.
      *              *-------------------------------------------------*
      *              * Estrazione numero progressivo bufferizzato      *
      *              *-------------------------------------------------*
           move      w-prs-def-nxt        to   s-num                  .
       num-prs-999.
           exit.

      *    *===========================================================*
      *    * Lettura referenza                                         *
      *    *-----------------------------------------------------------*
       get-ref-000.
      *              *-------------------------------------------------*
      *              * Salvataggio nome referenza                      *
      *              *-------------------------------------------------*
           move      s-alf                to   w-ref-def-cod          .
      *              *-------------------------------------------------*
      *              * Salvataggio eventuale numero progressivo        *
      *              *-------------------------------------------------*
           move      s-num                to   w-ref-def-prg          .
      *              *-------------------------------------------------*
      *              * Separazione chiave in componenti prima e dopo   *
      *              * il valore chiocciola                            *
      *              *-------------------------------------------------*
           move      w-ref-def-cod        to   w-key-por-key          .
           perform   key-prs-ref-000      thru key-prs-ref-999        .
       get-ref-100.
      *              *-------------------------------------------------*
      *              * Lettura della definizione                       *
      *              *-------------------------------------------------*
       get-ref-125.
      *                  *---------------------------------------------*
      *                  * Operazione di lettura, solo per la compo-   *
      *                  * nente che precede la chiocciola, e devia-   *
      *                  * zione secondo l'esito                       *
      *                  *---------------------------------------------*
           move      "drf "               to   auc-tre                .
           move      w-key-por-pre        to   auc-kre                .
           read      auc   with no lock
                           invalid key
                           go to   get-ref-150.
           go to     get-ref-175.
       get-ref-150.
      *                  *---------------------------------------------*
      *                  * Se definizione non trovata                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di valore non esistente         *
      *                      *-----------------------------------------*
           move      "#"                  to   s-ves                  .
      *                      *-----------------------------------------*
      *                      * Normalizzazione definizione             *
      *                      *-----------------------------------------*
           move      spaces               to   s-tip                  .
           move      zero                 to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
      *                      *-----------------------------------------*
      *                      * Normalizzazione valore                  *
      *                      *-----------------------------------------*
           move      spaces               to   s-alf                  .
           move      zero                 to   s-num                  .
           move      zero                 to   s-dat                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     get-ref-999.
       get-ref-175.
      *                  *---------------------------------------------*
      *                  * Se definizione trovata                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Record letto in area di ridefinizione   *
      *                      *-----------------------------------------*
           move      auc-dat              to   w-drf                  .
      *                      *-----------------------------------------*
      *                      * Salvataggio della definizione           *
      *                      *-----------------------------------------*
           move      w-drf-tip-drf        to   w-ref-def-tip          .
           move      w-drf-car-drf        to   w-ref-def-car          .
           move      w-drf-dec-drf        to   w-ref-def-dec          .
           move      w-drf-sgn-drf        to   w-ref-def-sgn          .
           move      w-drf-som-drf        to   w-ref-def-som          .
      *                      *-----------------------------------------*
      *                      * Test su numero progressivo              *
      *                      *-----------------------------------------*
           if        w-ref-def-som        not  = "M"
                     move  zero           to   w-ref-def-prg          .
       get-ref-300.
      *              *-------------------------------------------------*
      *              * Lettura del valore                              *
      *              *-------------------------------------------------*
       get-ref-325.
      *                  *---------------------------------------------*
      *                  * Operazione di lettura, sul valore completo  *
      *                  *---------------------------------------------*
           move      "ref "               to   ref-tre                .
           move      w-ref-def-cod        to   ref-cod                .
           move      w-ref-def-prg        to   ref-prg                .
           read      ref   with no lock
                           invalid key
                           go to   get-ref-350.
           go to     get-ref-375.
       get-ref-350.
      *                  *---------------------------------------------*
      *                  * Se valore non trovato                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di valore non esistente         *
      *                      *-----------------------------------------*
           move      "#"                  to   s-ves                  .
      *                      *-----------------------------------------*
      *                      * Normalizzazione valore                  *
      *                      *-----------------------------------------*
           move      spaces               to   s-alf                  .
           move      zero                 to   s-num                  .
           move      zero                 to   s-dat                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     get-ref-999.
       get-ref-375.
      *                  *---------------------------------------------*
      *                  * Se valore trovato                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di valore esistente             *
      *                      *-----------------------------------------*
           move      spaces               to   s-ves                  .
      *                      *-----------------------------------------*
      *                      * Preparazione definizione                *
      *                      *-----------------------------------------*
           move      w-ref-def-tip        to   s-tip                  .
           move      w-ref-def-car        to   s-car                  .
           move      w-ref-def-dec        to   s-dec                  .
           move      w-ref-def-sgn        to   s-sgn                  .
      *                      *-----------------------------------------*
      *                      * Preparazione del valore                 *
      *                      *-----------------------------------------*
           if        w-ref-def-tip        =    "A"
                     move  ref-alf        to   s-alf
                     move  zero           to   s-num
                     move  zero           to   s-dat
           else if   w-ref-def-tip        =    "N"
                     move  spaces         to   s-alf
                     move  ref-num        to   s-num
                     move  zero           to   s-dat
           else if   w-ref-def-tip        =    "D"
                     move  spaces         to   s-alf
                     move  zero           to   s-num
                     move  ref-dat        to   s-dat
           else      move  spaces         to   s-alf
                     move  zero           to   s-num
                     move  zero           to   s-dat                  .
       get-ref-999.
           exit.

      *    *===========================================================*
      *    * Start su di una referenza                                 *
      *    *-----------------------------------------------------------*
       str-ref-000.
      *              *-------------------------------------------------*
      *              * Salvataggio nome referenza                      *
      *              *-------------------------------------------------*
           move      s-alf                to   w-ref-def-cod          .
      *              *-------------------------------------------------*
      *              * Separazione chiave in componenti prima e dopo   *
      *              * il valore chiocciola                            *
      *              *-------------------------------------------------*
           move      w-ref-def-cod        to   w-key-por-key          .
           perform   key-prs-ref-000      thru key-prs-ref-999        .
      *              *-------------------------------------------------*
      *              * Azzeramento numero progressivo                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-ref-def-prg          .
      *              *-------------------------------------------------*
      *              * Azzeramento controllo su next                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-ref-def-nxt          .
       str-ref-100.
      *              *-------------------------------------------------*
      *              * Lettura della definizione                       *
      *              *-------------------------------------------------*
       str-ref-125.
      *                  *---------------------------------------------*
      *                  * Operazione di lettura, solo per la compo-   *
      *                  * nente che precede la chiocciola, e devia-   *
      *                  * zione secondo l'esito                       *
      *                  *---------------------------------------------*
           move      "drf "               to   auc-tre                .
           move      w-key-por-pre        to   auc-kre                .
           read      auc   with no lock
                           invalid key
                           go to   str-ref-150.
           go to     str-ref-175.
       str-ref-150.
      *                  *---------------------------------------------*
      *                  * Se definizione non trovata                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di valore non esistente         *
      *                      *-----------------------------------------*
           move      "#"                  to   s-ves                  .
      *                      *-----------------------------------------*
      *                      * Normalizzazione definizione             *
      *                      *-----------------------------------------*
           move      spaces               to   s-tip                  .
           move      zero                 to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
      *                      *-----------------------------------------*
      *                      * Normalizzazione valore                  *
      *                      *-----------------------------------------*
           move      spaces               to   s-alf                  .
           move      zero                 to   s-num                  .
           move      zero                 to   s-dat                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     str-ref-999.
       str-ref-175.
      *                  *---------------------------------------------*
      *                  * Se definizione trovata                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Record letto in area di ridefinizione   *
      *                      *-----------------------------------------*
           move      auc-dat              to   w-drf                  .
      *                      *-----------------------------------------*
      *                      * Salvataggio della definizione           *
      *                      *-----------------------------------------*
           move      w-drf-tip-drf        to   w-ref-def-tip          .
           move      w-drf-car-drf        to   w-ref-def-car          .
           move      w-drf-dec-drf        to   w-ref-def-dec          .
           move      w-drf-sgn-drf        to   w-ref-def-sgn          .
           move      w-drf-som-drf        to   w-ref-def-som          .
       str-ref-300.
      *              *-------------------------------------------------*
      *              * Start effettiva                                 *
      *              *-------------------------------------------------*
       str-ref-325.
      *                  *---------------------------------------------*
      *                  * Operazione di start, sul valore completo    *
      *                  *---------------------------------------------*
           move      "ref "               to   ref-tre                .
           move      w-ref-def-cod        to   ref-cod                .
           move      w-ref-def-prg        to   ref-prg                .
           start     ref   key not less
                           ref-rck
                           invalid key
                           go to   str-ref-350.
           go to     str-ref-375.
       str-ref-350.
      *                  *---------------------------------------------*
      *                  * Se start errata                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di valore non esistente         *
      *                      *-----------------------------------------*
           move      "#"                  to   s-ves                  .
      *                      *-----------------------------------------*
      *                      * Normalizzazione valore                  *
      *                      *-----------------------------------------*
           move      spaces               to   s-alf                  .
           move      zero                 to   s-num                  .
           move      zero                 to   s-dat                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     str-ref-999.
       str-ref-375.
      *                  *---------------------------------------------*
      *                  * Se valore trovato                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di valore esistente             *
      *                      *-----------------------------------------*
           move      spaces               to   s-ves                  .
      *                      *-----------------------------------------*
      *                      * Preparazione definizione                *
      *                      *-----------------------------------------*
           move      w-ref-def-tip        to   s-tip                  .
           move      w-ref-def-car        to   s-car                  .
           move      w-ref-def-dec        to   s-dec                  .
           move      w-ref-def-sgn        to   s-sgn                  .
       str-ref-999.
           exit.

      *    *===========================================================*
      *    * Lettura sequenziale di una referenza                      *
      *    *-----------------------------------------------------------*
       nxt-ref-000.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale e deviazione in funzione    *
      *              * dell'esito                                      *
      *              *-------------------------------------------------*
           read      ref    next
                            with no lock
                            at end
                            go to nxt-ref-100.
           go to     nxt-ref-200.
       nxt-ref-100.
      *              *-------------------------------------------------*
      *              * Se fine file                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di valore non esistente             *
      *                  *---------------------------------------------*
           move      "#"                  to   s-ves                  .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valore                      *
      *                  *---------------------------------------------*
           move      spaces               to   s-alf                  .
           move      zero                 to   s-num                  .
           move      zero                 to   s-dat                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     nxt-ref-999.
       nxt-ref-200.
      *              *-------------------------------------------------*
      *              * Se non fine file                                *
      *              *-------------------------------------------------*
       nxt-ref-225.
      *                  *---------------------------------------------*
      *                  * Test max, sul valore completo, se non supe- *
      *                  * to, come per fine fine                      *
      *                  *---------------------------------------------*
           if        ref-tre              not  = "ref "
                     go to nxt-ref-100.
           if        ref-cod              not  = w-ref-def-cod
                     go to nxt-ref-100.
       nxt-ref-250.
      *                  *---------------------------------------------*
      *                  * Selezione, se non superata : rilettura      *
      *                  *---------------------------------------------*
           if        ref-prg              <    001 or
                     ref-prg              >    999
                     go to nxt-ref-000.
       nxt-ref-275.
      *                  *---------------------------------------------*
      *                  * Memorizzazione ultimo progressivo letto     *
      *                  *---------------------------------------------*
           move      ref-prg              to   w-ref-def-nxt          .
       nxt-ref-300.
      *                  *---------------------------------------------*
      *                  * Segnale di valore esistente                 *
      *                  *---------------------------------------------*
           move      spaces               to   s-ves                  .
       nxt-ref-325.
      *                  *---------------------------------------------*
      *                  * Preparazione del valore                     *
      *                  *---------------------------------------------*
           if        w-ref-def-tip        =    "A"
                     move  ref-alf        to   s-alf
                     move  zero           to   s-num
                     move  zero           to   s-dat
           else if   w-ref-def-tip        =    "N"
                     move  spaces         to   s-alf
                     move  ref-num        to   s-num
                     move  zero           to   s-dat
           else if   w-ref-def-tip        =    "D"
                     move  spaces         to   s-alf
                     move  zero           to   s-num
                     move  ref-dat        to   s-dat
           else      move  spaces         to   s-alf
                     move  zero           to   s-num
                     move  zero           to   s-dat                  .
       nxt-ref-999.
           exit.

      *    *===========================================================*
      *    * Numero progressivo di una referenza                       *
      *    *-----------------------------------------------------------*
       num-ref-000.
      *              *-------------------------------------------------*
      *              * Estrazione numero progressivo bufferizzato      *
      *              *-------------------------------------------------*
           move      w-ref-def-nxt        to   s-num                  .
       num-ref-999.
           exit.

      *    *===========================================================*
      *    * Encoding automatico : get                                 *
      *    *-----------------------------------------------------------*
       enc-get-000.
      *              *-------------------------------------------------*
      *              * Lettura record con lock, e deviazione secondo   *
      *              * l'esito                                         *
      *              *-------------------------------------------------*
           move      "enc "               to   enc-tre                .
           move      s-nam                to   enc-cod                .
           move      zero                 to   enc-prg                .
           read      enc   invalid key
                           go to   enc-get-100.
           go to     enc-get-200.
       enc-get-100.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di uscita ad errore                  *
      *                  *---------------------------------------------*
           move      "#"                  to   s-sts                  .
      *                  *---------------------------------------------*
      *                  * Rilascio del record                         *
      *                  *---------------------------------------------*
           perform   enc-rel-000          thru enc-rel-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     enc-get-999.
       enc-get-200.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di uscita ad Ok                      *
      *                  *---------------------------------------------*
           move      spaces               to   s-sts                  .
      *                  *---------------------------------------------*
      *                  * Ultimo codice utilizzato in uscita          *
      *                  *---------------------------------------------*
           move      enc-num              to   s-num                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     enc-get-999.
       enc-get-999.
           exit.

      *    *===========================================================*
      *    * Encoding automatico : put                                 *
      *    *-----------------------------------------------------------*
       enc-put-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      spaces               to   enc-rec                .
           move      "enc "               to   enc-tre                .
           move      s-nam                to   enc-cod                .
           move      zero                 to   enc-prg                .
           move      s-num                to   enc-num                .
       enc-put-100.
      *              *-------------------------------------------------*
      *              * Scrittura record, e deviazione secondo l'esito  *
      *              *-------------------------------------------------*
           write     enc-rec invalid key
                             go to   enc-put-200.
           go to     enc-put-300.
       enc-put-200.
      *              *-------------------------------------------------*
      *              * Se record gia' esistente                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di uscita ad errore                  *
      *                  *---------------------------------------------*
           move      "#"                  to   s-sts                  .
      *                  *---------------------------------------------*
      *                  * Rilascio del record                         *
      *                  *---------------------------------------------*
           perform   enc-rel-000          thru enc-rel-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     enc-put-999.
       enc-put-300.
      *              *-------------------------------------------------*
      *              * Se scrittura Ok                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di uscita ad Ok                      *
      *                  *---------------------------------------------*
           move      spaces               to   s-sts                  .
      *                  *---------------------------------------------*
      *                  * Rilascio del record                         *
      *                  *---------------------------------------------*
           perform   enc-rel-000          thru enc-rel-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     enc-put-999.
       enc-put-999.
           exit.

      *    *===========================================================*
      *    * Encoding automatico : update                              *
      *    *-----------------------------------------------------------*
       enc-upd-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      spaces               to   enc-rec                .
           move      "enc "               to   enc-tre                .
           move      s-nam                to   enc-cod                .
           move      zero                 to   enc-prg                .
           move      s-num                to   enc-num                .
       enc-upd-100.
      *              *-------------------------------------------------*
      *              * Riscrittura record, e deviazione secondo l'esi- *
      *              * to                                              *
      *              *-------------------------------------------------*
           rewrite   enc-rec invalid key
                             go to   enc-upd-200.
           go to     enc-upd-300.
       enc-upd-200.
      *              *-------------------------------------------------*
      *              * Se record gia' esistente                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di uscita ad errore                  *
      *                  *---------------------------------------------*
           move      "#"                  to   s-sts                  .
      *                  *---------------------------------------------*
      *                  * Rilascio del record                         *
      *                  *---------------------------------------------*
           perform   enc-rel-000          thru enc-rel-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     enc-upd-999.
       enc-upd-300.
      *              *-------------------------------------------------*
      *              * Se scrittura Ok                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di uscita ad Ok                      *
      *                  *---------------------------------------------*
           move      spaces               to   s-sts                  .
      *                  *---------------------------------------------*
      *                  * Rilascio del record                         *
      *                  *---------------------------------------------*
           perform   enc-rel-000          thru enc-rel-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     enc-upd-999.
       enc-upd-999.
           exit.

      *    *===========================================================*
      *    * Encoding automatico : release                             *
      *    *-----------------------------------------------------------*
       enc-rel-000.
      *              *-------------------------------------------------*
      *              * Unlock                                          *
      *              *-------------------------------------------------*
           unlock    enc    record                                    .
       enc-rel-999.
           exit.

      *    *===========================================================*
      *    * Year last page automatico : get                           *
      *    *-----------------------------------------------------------*
       ylp-get-000.
      *              *-------------------------------------------------*
      *              * Lettura record con lock, e deviazione secondo   *
      *              * l'esito                                         *
      *              *-------------------------------------------------*
           move      "ylp "               to   enc-tre                .
           move      s-fas                to   enc-cod                .
           move      s-saa                to   enc-prg                .
           read      enc   invalid key
                           go to   ylp-get-100.
           go to     ylp-get-200.
       ylp-get-100.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di uscita ad errore                  *
      *                  *---------------------------------------------*
           move      "#"                  to   s-sts                  .
      *                  *---------------------------------------------*
      *                  * Rilascio del record                         *
      *                  *---------------------------------------------*
           perform   ylp-rel-000          thru ylp-rel-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ylp-get-999.
       ylp-get-200.
      *              *-------------------------------------------------*
      *              * Se record esistente                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di uscita ad Ok                      *
      *                  *---------------------------------------------*
           move      spaces               to   s-sts                  .
      *                  *---------------------------------------------*
      *                  * Ultimo codice utilizzato in uscita          *
      *                  *---------------------------------------------*
           move      enc-num              to   s-num                  .
      *                  *---------------------------------------------*
      *                  * Rilascio del record                         *
      *                  *---------------------------------------------*
           perform   ylp-rel-000          thru ylp-rel-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ylp-get-999.
       ylp-get-999.
           exit.

      *    *===========================================================*
      *    * Year last page automatico : put                           *
      *    *-----------------------------------------------------------*
       ylp-put-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      spaces               to   enc-rec                .
           move      "ylp "               to   enc-tre                .
           move      s-fas                to   enc-cod                .
           move      s-saa                to   enc-prg                .
           move      s-num                to   enc-num                .
       ylp-put-100.
      *              *-------------------------------------------------*
      *              * Scrittura record, e deviazione secondo l'esito  *
      *              *-------------------------------------------------*
           write     enc-rec invalid key
                             go to   ylp-put-200.
           go to     ylp-put-300.
       ylp-put-200.
      *              *-------------------------------------------------*
      *              * Se record gia' esistente                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di uscita ad errore                  *
      *                  *---------------------------------------------*
           move      "#"                  to   s-sts                  .
      *                  *---------------------------------------------*
      *                  * Rilascio del record                         *
      *                  *---------------------------------------------*
           perform   ylp-rel-000          thru ylp-rel-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ylp-put-999.
       ylp-put-300.
      *              *-------------------------------------------------*
      *              * Se scrittura Ok                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di uscita ad Ok                      *
      *                  *---------------------------------------------*
           move      spaces               to   s-sts                  .
      *                  *---------------------------------------------*
      *                  * Rilascio del record                         *
      *                  *---------------------------------------------*
           perform   ylp-rel-000          thru ylp-rel-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ylp-put-999.
       ylp-put-999.
           exit.

      *    *===========================================================*
      *    * Year last page automatico : update                        *
      *    *-----------------------------------------------------------*
       ylp-upd-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           move      spaces               to   enc-rec                .
           move      "ylp "               to   enc-tre                .
           move      s-fas                to   enc-cod                .
           move      s-saa                to   enc-prg                .
           move      s-num                to   enc-num                .
       ylp-upd-100.
      *              *-------------------------------------------------*
      *              * Riscrittura record, e deviazione secondo l'esi- *
      *              * to                                              *
      *              *-------------------------------------------------*
           rewrite   enc-rec invalid key
                             go to   ylp-upd-200.
           go to     ylp-upd-300.
       ylp-upd-200.
      *              *-------------------------------------------------*
      *              * Se record gia' esistente                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di uscita ad errore                  *
      *                  *---------------------------------------------*
           move      "#"                  to   s-sts                  .
      *                  *---------------------------------------------*
      *                  * Rilascio del record                         *
      *                  *---------------------------------------------*
           perform   ylp-rel-000          thru ylp-rel-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ylp-upd-999.
       ylp-upd-300.
      *              *-------------------------------------------------*
      *              * Se scrittura Ok                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di uscita ad Ok                      *
      *                  *---------------------------------------------*
           move      spaces               to   s-sts                  .
      *                  *---------------------------------------------*
      *                  * Rilascio del record                         *
      *                  *---------------------------------------------*
           perform   ylp-rel-000          thru ylp-rel-999            .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ylp-upd-999.
       ylp-upd-999.
           exit.

      *    *===========================================================*
      *    * Year last page automatico : release                       *
      *    *-----------------------------------------------------------*
       ylp-rel-000.
      *              *-------------------------------------------------*
      *              * Unlock                                          *
      *              *-------------------------------------------------*
           unlock    enc    record                                    .
       ylp-rel-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di operazione su file alla prossima operazione  *
      *    * di Open sul File                                          *
      *    *-----------------------------------------------------------*
       kse-sqk-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione name file interessato            *
      *              *-------------------------------------------------*
           move      s-nam                to   w-kse-nam              .
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo operazione richiesta        *
      *              *-------------------------------------------------*
           move      s-sts                to   w-kse-top              .
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo filtro richiesto            *
      *              *-------------------------------------------------*
           move      s-num                to   w-kse-tpf              .
      *              *-------------------------------------------------*
      *              * Memorizzazione filtro richiesto                 *
      *              *-------------------------------------------------*
           move      s-pat                to   w-kse-nmf              .
       kse-sqk-999.
           exit.

      *    *===========================================================*
      *    * Filtro di un pathname di esecuzione per ottenere un path- *
      *    * name di esecuzione sostitutivo                            *
      *    *-----------------------------------------------------------*
       flt-pnx-000.
      *              *-------------------------------------------------*
      *              * Se nessun pathname di esecuzione sostitutivo    *
      *              * memorizzato : uscita                            *
      *              *-------------------------------------------------*
           if        z-pnx-ncm            =    zero
                     go to flt-pnx-999.
      *              *-------------------------------------------------*
      *              * Se la sostituzione vale per tutte le aziende :  *
      *              * si continua per il controllo effettivo          *
      *              *-------------------------------------------------*
           if        z-pnx-aps-snx        =    spaces
                     go to flt-pnx-300.
      *              *-------------------------------------------------*
      *              * Se l'azienda in uso e' a spaces : si continua   *
      *              * per il controllo effettivo                      *
      *              *-------------------------------------------------*
           if        z-azi                =    spaces
                     go to flt-pnx-300.
       flt-pnx-100.
      *              *-------------------------------------------------*
      *              * Altrimenti si controlla che l'azienda in uso    *
      *              * sia tra l'elenco delle aziende per cui effet-   *
      *              * tuare l'eventuale sostituzione                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-inx                  .
       flt-pnx-110.
           add       1                    to   w-inx                  .
           if        w-inx                >    10
                     go to flt-pnx-999.
           if        z-pnx-aps-lel (w-inx)
                                          not  = z-azi
                     go to flt-pnx-110.
       flt-pnx-300.
      *              *-------------------------------------------------*
      *              * Se sono memorizzati piu' di 32 pathnames sosti- *
      *              * tutivi si esegue una ricerca dicotomica, altri- *
      *              * menti si esegue una ricerca sequenziale         *
      *              *-------------------------------------------------*
           if        z-pnx-ncm            >    32
                     go to flt-pnx-700.
       flt-pnx-400.
      *              *-------------------------------------------------*
      *              * Ricerca del pathname originale per scansione    *
      *              * sequenziale della tabella                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione indice di scansione        *
      *                  *---------------------------------------------*
           move      zero                 to   w-inx                  .
       flt-pnx-450.
      *                  *---------------------------------------------*
      *                  * Incremento indice di scansione              *
      *                  *---------------------------------------------*
           add       1                    to   w-inx                  .
      *                  *---------------------------------------------*
      *                  * Se oltre numero elementi memorizzati in ta- *
      *                  * bella : uscita                              *
      *                  *---------------------------------------------*
           if        w-inx                >    z-pnx-ncm
                     go to flt-pnx-999.
      *                  *---------------------------------------------*
      *                  * Se elemento diverso da quello cercato : ri- *
      *                  * ciclo su elemento successivo in tabella     *
      *                  *---------------------------------------------*
           if        z-pnx-org
                    (w-inx)               not  = s-pat
                     go to flt-pnx-450.
       flt-pnx-500.
      *                  *---------------------------------------------*
      *                  * Se trovato l'elemento cercato               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sostituzione del pathname originale con *
      *                      * il pathname sostitutivo                 *
      *                      *-----------------------------------------*
           move      z-pnx-sos
                    (w-inx)               to   s-pat                  .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     flt-pnx-999.
       flt-pnx-700.
      *              *-------------------------------------------------*
      *              * Ricerca del pathname originale per scansione    *
      *              * dicotomica della tabella                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione dell'indice di scansione mi-   *
      *                  * nimo e massimo                              *
      *                  *---------------------------------------------*
           move      1                    to   w-i01                  .
           move      z-pnx-ncm            to   w-i99                  .
       flt-pnx-750.
      *                  *---------------------------------------------*
      *                  * Preparazione dell'indice di scansione medio *
      *                  *---------------------------------------------*
           move      w-i01                to   w-inx                  .
           add       w-i99                to   w-inx                  .
           divide    2                    into w-inx                  .
      *                  *---------------------------------------------*
      *                  * Confronto tra l'elemento in esame e quello  *
      *                  * cercato                                     *
      *                  *---------------------------------------------*
           if        z-pnx-org
                    (w-inx)               =    s-pat
                     go to flt-pnx-500
           else if   z-pnx-org
                    (w-inx)               <    s-pat
                     go to flt-pnx-850.
       flt-pnx-800.
      *                  *---------------------------------------------*
      *                  * Se l'elemento in esame e' maggiore dell'e-  *
      *                  * lemento cercato                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se l'indice di scansione massimo e' gia'*
      *                      * pari all'indice di scansione medio : e- *
      *                      * lemento non trovato                     *
      *                      *-----------------------------------------*
           if        w-i99                =    w-inx
                     go to flt-pnx-999.
      *                      *-----------------------------------------*
      *                      * Aggiornamento dell'indice di scansione  *
      *                      * massimo                                 *
      *                      *-----------------------------------------*
           move      w-inx                to   w-i99                  .
      *                      *-----------------------------------------*
      *                      * Riciclo ad esaminare                    *
      *                      *-----------------------------------------*
           go to     flt-pnx-750.
       flt-pnx-850.
      *                  *---------------------------------------------*
      *                  * Se l'elemento in esame e' minore dell'ele-  *
      *                  * mento cercato                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se l'indice di scansione minimo e' gia' *
      *                      * pari all'indice di scansione medio : e- *
      *                      * lemento non trovato                     *
      *                      *-----------------------------------------*
           if        w-i01                =    w-inx
                     go to flt-pnx-999.
      *                      *-----------------------------------------*
      *                      * Aggiornamento dell'indice di scansione  *
      *                      * minimo                                  *
      *                      *-----------------------------------------*
           move      w-inx                to   w-i01                  .
      *                      *-----------------------------------------*
      *                      * Riciclo ad esaminare                    *
      *                      *-----------------------------------------*
           go to     flt-pnx-750.
       flt-pnx-999.
           exit.

      *    *===========================================================*
      *    * Test se programma applicativo attivo o no                 *
      *    *-----------------------------------------------------------*
       tst-pro-000.
           move      zero                 to   s-liv                  .
       tst-pro-100.
           add       1                    to   s-liv                  .
           if        s-liv                >    z-stk-liv
                     move  zero           to   s-liv
                     go to tst-pro-999.
           if        s-pro                not  = z-stk-pro(s-liv)
                     go to tst-pro-100.
       tst-pro-999.
           exit.

      *    *===========================================================*
      *    * Inibizione Save ad inizio programma applicativo           *
      *    *-----------------------------------------------------------*
       fs0-pro-000.
      *              *-------------------------------------------------*
      *              * Flag di inibizione Save video a inizio prossimo *
      *              * programma in On                                 *
      *              *-------------------------------------------------*
           move      "#"                  to   z-fs0                  .
       fs0-pro-999.
           exit.

      *    *===========================================================*
      *    * Inizio programma applicativo                              *
      *    *-----------------------------------------------------------*
       ini-pro-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di status per eventuale    *
      *              * sola visualizzazione                            *
      *              *-------------------------------------------------*
           move      spaces               to   w-dnd-sts              .
      *              *-------------------------------------------------*
      *              * Se il flag di inibizione Save video a inizio    *
      *              * prossimo programma e' in On : si forza in Off   *
      *              * il flag di Save video, ed il flag stesso di i-  *
      *              * nibizione                                       *
      *              *-------------------------------------------------*
           if        z-fs0                not  = spaces
                     move  spaces         to   s-svv
                     move  spaces         to   z-fs0                  .
      *              *-------------------------------------------------*
      *              * Salvataggio e normalizzazione del flag di Save  *
      *              * Video                                           *
      *              *-------------------------------------------------*
           move      s-svv                to   w-svv                  .
           move      spaces               to   s-svv                  .
      *              *-------------------------------------------------*
      *              * Program start time in uscita                    *
      *              *-------------------------------------------------*
           perform   est-sdt-000          thru est-sdt-999            .
           move      s-sdt                to   s-pst                  .
      *              *-------------------------------------------------*
      *              * Test se programma gia' in esecuzione o se supe- *
      *              * ramento di 20 programmi nidificati              *
      *              *-------------------------------------------------*
           if        z-fun                =    "B"
                     go to ini-pro-050.
           if        s-pro                =    z-bkg-lan
                     go to ini-pro-200.
       ini-pro-050.
           move      zero                 to   w-ctr                  .
       ini-pro-100.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    z-stk-liv
                     go to ini-pro-150.
           if        s-pro                =    z-stk-pro(w-ctr)
                     go to ini-pro-200
           else      go to ini-pro-100.
       ini-pro-150.
           if        w-ctr                <    20
                     go to ini-pro-300.
       ini-pro-200.
      *              *-------------------------------------------------*
      *              * Se programma gia' in esecuzione o se superamen- *
      *              * to di 20 programmi nidificati si da' un messag- *
      *              * gio di errore e si esce con livello di profon-  *
      *              * dita' applicativa a zero                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se si e' in background : no messaggio       *
      *                  *---------------------------------------------*
           if        z-fun                =    "B"
                     go to ini-pro-250.
      *                  *---------------------------------------------*
      *                  * Messaggio e presa visione                   *
      *                  *---------------------------------------------*
           perform   gia-ine-000          thru gia-ine-999            .
       ini-pro-250.
      *                  *---------------------------------------------*
      *                  * Livello di profondita' a zero e uscita      *
      *                  *---------------------------------------------*
           move      zero                 to   s-liv                  .
           go to     ini-pro-999.
       ini-pro-300.
      *              *-------------------------------------------------*
      *              * Controlli di riservatezza e password            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiesta ragione sociale licenziatario     *
      *                  * per verificare l'eventuale presenza del     *
      *                  * segnale che indica l'uso in sola visualiz-  *
      *                  * zazione del software (DEMO)                 *
      *                  *                                             *
      *                  * ___ perfezionare con un test di superamento *
      *                  * ___ di 90 gg dalla scadenza                 *
      *                  *                                             *
      *                  * Si tratta di mettere '#' nell'ultimo carat- *
      *                  * tere della ragione sociale                  *
      *                  *---------------------------------------------*

      
      *                  *---------------------------------------------*
      *                  * ___ AREA PER DEMO !!! ___                   *
      *                  *---------------------------------------------*
______*    go to     ini-pro-350.
      
           if        z-azi                not = "ptu " and
                     z-azi                not = "mbv "
                     go to ini-pro-350.
           if        s-dat                not > 1240101
                     go to ini-pro-350.
           move      "V"                  to   w-dnd-sts
                     go to ini-pro-900.
      *                  *---------------------------------------------*
      *                  * ___ AREA PER DEMO - FINE !!! ___            *
      *                  *---------------------------------------------*


       ini-pro-350.
           perform   ric-lcu-000          thru ric-lcu-999            .
           if        s-alf (40 : 01)      =    "#"
                     move  "V"            to   w-dnd-sts
                     go to ini-pro-900.
      *                  *---------------------------------------------*
      *                  * Se in background nessun controllo           *
      *                  *---------------------------------------------*
           if        z-fun                =    "B"
                     go to ini-pro-900.
      *                  *---------------------------------------------*
      *                  * Se richiesta di non controllo : nessun con- *
      *                  * trollo                                      *
      *                  *---------------------------------------------*
           if        s-ope                =    "Q+"
                     go to ini-pro-900.
      *                  *---------------------------------------------*
      *                  * Se l'utente e' di tipo supervisore : nessun *
      *                  * controllo                                   *
      *                  *---------------------------------------------*
           if        w-ute-tip-ute        =    02
                     go to ini-pro-900.
      *                  *---------------------------------------------*
      *                  * Se :                                        *
      *                  *  - non c'e' riservatezza sulle gerarchie di *
      *                  *    comando                                  *
      *                  *  - non c'e' riservatezza sui tipi utente    *
      *                  *  - non c'e' riservatezza sul livello di     *
      *                  *    protezione                               *
      *                  *  - non c'e' controllo passwords sui codici  *
      *                  *    comando                                  *
      *                  * nessun controllo                            *
      *                  *---------------------------------------------*
           if        w-psg-rsv-ide        not  = "S" and
                     w-psg-rsv-tut        not  = "S" and
                     w-psg-rsv-lip        not  = "S" and
                     w-psg-pwd-cmd        not  = "S"
                     go to ini-pro-900.
      *                  *---------------------------------------------*
      *                  * Lettura codice comando                      *
      *                  *---------------------------------------------*
           perform   let-cmd-auc-000      thru let-cmd-auc-999        .
      *                  *---------------------------------------------*
      *                  * Se codice comando non esistente : nessun    *
      *                  * controllo                                   *
      *                  *---------------------------------------------*
           if        w-fes-cmd            not  = spaces
                     go to ini-pro-900.
       ini-pro-400.
      *                  *---------------------------------------------*
      *                  * Controlli di riservatezza                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione nome comando per eventua-  *
      *                      * lita' di controllo non superato         *
      *                      *-----------------------------------------*
           move      w-cmd-cod-cmd        to   w-dnd-cmd              .
       ini-pro-410.
      *                      *-----------------------------------------*
      *                      * Riservatezza su task                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il comando non e' definito come  *
      *                          * task : no controllo                 *
      *                          *-------------------------------------*
           if        w-cmd-tip-cmd        not  = 00
                     go to ini-pro-420.
      *                          *-------------------------------------*
      *                          * Se il comando e' bloccato : il con- *
      *                          * trollo non e' superato              *
      *                          *-------------------------------------*
           if        w-cmd-sts-cmd        not  = 00
                     move  01             to   w-dnd-ibz
                     go to ini-pro-800.
       ini-pro-420.
      *                      *-----------------------------------------*
      *                      * Riservatezza sulle gerarchie di comando *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se non c'e' controllo di riserva-   *
      *                          * tezza sulle gerarchie di comandi :  *
      *                          * no controllo                        *
      *                          *-------------------------------------*
           if        w-psg-rsv-ide        not  = "S"
                     go to ini-pro-430.
      *                          *-------------------------------------*
      *                          * Se l'utente e' abilitato per tutti  *
      *                          * i comandi : no controllo            *
      *                          *-------------------------------------*
           if        w-ute-num-abl        =    00
                     go to ini-pro-430.
      *                          *-------------------------------------*
      *                          * Controllo sulla gerarchia           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Area gestionale e settore ge-   *
      *                              * stionale relativi al comando in *
      *                              * area di lavoro                  *
      *                              *---------------------------------*
           move      w-cmd-are-cmd        to   w-ags-arg              .
           move      w-cmd-set-cmd        to   w-ags-set              .
      *                              *---------------------------------*
      *                              * Inizializzazione indice su abi- *
      *                              * litazioni per l'utente          *
      *                              *---------------------------------*
           move      zero                 to   w-pnt                  .
       ini-pro-421.
      *                              *---------------------------------*
      *                              * Incremento indice su abilita-   *
      *                              * azioni per l'utente             *
      *                              *---------------------------------*
           add       1                    to   w-pnt                  .
      *                              *---------------------------------*
      *                              * Se oltre numero abilitazioni :  *
      *                              * controllo non superato          *
      *                              *---------------------------------*
           if        w-pnt                >    w-ute-num-abl
                     move  02             to   w-dnd-ibz
                     go to ini-pro-800.
      *                              *---------------------------------*
      *                              * Deviazione in funzione del ti-  *
      *                              * po di elemento                  *
      *                              *---------------------------------*
           if        w-ute-tel-abl
                    (w-pnt)               =    "X"
                     go to ini-pro-422
           else if   w-ute-tel-abl
                    (w-pnt)               =    "A"
                     go to ini-pro-423
           else if   w-ute-tel-abl
                    (w-pnt)               =    "S"
                     go to ini-pro-424
           else if   w-ute-tel-abl
                    (w-pnt)               =    "F"
                     go to ini-pro-425
           else      go to ini-pro-421.
       ini-pro-422.
      *                              *---------------------------------*
      *                              * Se sistema applicativo          *
      *                              *---------------------------------*
           if        w-ute-cod-abl
                    (w-pnt)               not  = w-cmd-sis-cmd
                     go to ini-pro-421.
           if        w-ute-tip-abl
                    (w-pnt)               =    "+" 
                     go to ini-pro-430
           else if   w-ute-tip-abl
                    (w-pnt)               =    "v"
                     move  "V"            to   w-dnd-sts
                     go to ini-pro-430
           else      move  02             to   w-dnd-ibz
                     go to ini-pro-800.
       ini-pro-423.
      *                              *---------------------------------*
      *                              * Se area gestionale              *
      *                              *---------------------------------*
           if        w-ute-cod-abl
                    (w-pnt)               not  = w-cmd-are-cmd
                     go to ini-pro-421.
           if        w-ute-tip-abl
                    (w-pnt)               =    "+"
                     go to ini-pro-430
           else if   w-ute-tip-abl
                    (w-pnt)               =    "v"
                     move  "V"            to   w-dnd-sts
                     go to ini-pro-430
           else      move  02             to   w-dnd-ibz
                     go to ini-pro-800.
       ini-pro-424.
      *                              *---------------------------------*
      *                              * Se settore gestionale           *
      *                              *---------------------------------*
           if        w-ute-cod-abl
                    (w-pnt)               not  = w-ags
                     go to ini-pro-421.
           if        w-ute-tip-abl
                    (w-pnt)               =    "+"
                     go to ini-pro-430
           else if   w-ute-tip-abl
                    (w-pnt)               =    "v"
                     move  "V"            to   w-dnd-sts
                     go to ini-pro-430
           else      move  02             to   w-dnd-ibz
                     go to ini-pro-800.
       ini-pro-425.
      *                              *---------------------------------*
      *                              * Se fase gestionale              *
      *                              *---------------------------------*
           if        w-ute-cod-abl
                    (w-pnt)               not  = w-cmd-fas-cmd
                     go to ini-pro-421.
           if        w-ute-tip-abl
                    (w-pnt)               =    "+"
                     go to ini-pro-430
           else if   w-ute-tip-abl
                    (w-pnt)               =    "v"
                     move  "V"            to   w-dnd-sts
                     go to ini-pro-430
           else      move  02             to   w-dnd-ibz
                     go to ini-pro-800.
       ini-pro-430.
      *                      *-----------------------------------------*
      *                      * Riservatezza sui tipi utente            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se non c'e' controllo di riserva-   *
      *                          * tezza sui tipi utente : no control- *
      *                          * lo                                  *
      *                          *-------------------------------------*
           if        w-psg-rsv-tut        not  = "S"
                     go to ini-pro-440.
      *                          *-------------------------------------*
      *                          * Test sul tipo utente                *
      *                          *-------------------------------------*
           if        w-ute-tip-ute        <    w-cmd-tip-ute
                     move  03             to   w-dnd-ibz
                     go to ini-pro-800.
           if        w-ute-tip-ute        =    99 and
                     w-cmd-tip-ute        >    00
                     move  03             to   w-dnd-ibz
                     go to ini-pro-800.
       ini-pro-440.
      *                      *-----------------------------------------*
      *                      * Riservatezza sul livello di protezione  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se non c'e' controllo di riserva-   *
      *                          * tezza sul livello di protezione :   *
      *                          * no controllo                        *
      *                          *-------------------------------------*
           if        w-psg-rsv-lip        not  = "S"
                     go to ini-pro-450.
      *                          *-------------------------------------*
      *                          * Test sul livello di protezione      *
      *                          *-------------------------------------*
           if        w-ute-liv-pro        <    w-cmd-liv-pro
                     move  04             to   w-dnd-ibz
                     go to ini-pro-800.
       ini-pro-450.
      *                  *---------------------------------------------*
      *                  * Controlli di password                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non c'e' controllo di password sui   *
      *                      * comandi : no controllo                  *
      *                      *-----------------------------------------*
           if        w-psg-pwd-cmd        not  = "S"
                     go to ini-pro-900.
      *                      *-----------------------------------------*
      *                      * Se non c'e' password associata al co-   *
      *                      * mando : no password                     *
      *                      *-----------------------------------------*
           if        w-cmd-pwd-cmd        =    spaces
                     go to ini-pro-900.
       ini-pro-500.
      *                      *-----------------------------------------*
      *                      * Accettazione password per il comando    *
      *                      *-----------------------------------------*
           move      "Comando      "      to   w-acp-lit              .
           move      w-cmd-fas-cmd        to   w-acp-cod              .
           move      w-cmd-pwd-cmd        to   w-acp-psw              .
           perform   acc-pwd-000          thru acc-pwd-999            .
      *                      *-----------------------------------------*
      *                      * Se accettazione password non superata : *
      *                      * uscita                                  *
      *                      *-----------------------------------------*
           if        w-acp-exi            =    spaces
                     go to ini-pro-900
           else      move  zero           to   s-liv
                     go to ini-pro-999.
       ini-pro-800.
      *                  *---------------------------------------------*
      *                  * Messaggio per 'permission denied'           *
      *                  *---------------------------------------------*
           perform   prm-dnd-000          thru prm-dnd-999            .
      *                  *---------------------------------------------*
      *                  * Livello di profondita' a zero e uscita      *
      *                  *---------------------------------------------*
           move      zero                 to   s-liv                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ini-pro-999.
       ini-pro-900.
      *              *-------------------------------------------------*
      *              * Aggiornamento livello profondita' applicativa   *
      *              *-------------------------------------------------*
           move      w-ctr                to   z-stk-liv              .
      *              *-------------------------------------------------*
      *              * Livello profondita' applicativa in uscita       *
      *              *-------------------------------------------------*
           move      z-stk-liv            to   s-liv                  .
      *              *-------------------------------------------------*
      *              * Aggiornamento parametri programma attuale       *
      *              *-------------------------------------------------*
           move      s-sap                to   z-sap                  .
           move      s-arg                to   z-arg                  .
           move      s-set                to   z-set                  .
           move      s-fas                to   z-fas                  .
           move      s-pro                to   z-pro                  .
           move      s-pst                to   z-pst                  .
      *              *-------------------------------------------------*
      *              * Aggiornamento parametri su stack programmi      *
      *              *-------------------------------------------------*
           move      z-sap                to   z-stk-sap(z-stk-liv)   .
           move      z-arg                to   z-stk-arg(z-stk-liv)   .
           move      z-set                to   z-stk-set(z-stk-liv)   .
           move      z-fas                to   z-stk-fas(z-stk-liv)   .
           move      z-pro                to   z-stk-pro(z-stk-liv)   .
           move      z-pst                to   z-stk-pst(z-stk-liv)   .
           move      w-svv                to   z-stk-svv(z-stk-liv)   .
      *              *-------------------------------------------------*
      *              * Aggiornamento sessione utente                   *
      *              *-------------------------------------------------*
           move      "A>"                 to   s-sts                  .
           perform   trt-usl-000          thru trt-usl-999            .
      *              *-------------------------------------------------*
      *              * Aggiornamento programmi utilizzati utente       *
      *              *-------------------------------------------------*
           perform   agg-sfu-000          thru agg-sfu-999            .
      *              *-------------------------------------------------*
      *              * Aggiornamento flag di visualizzazione           *
      *              *-------------------------------------------------*
           move      w-dnd-sts            to   s-sts                  .
       ini-pro-950.
      *              *-------------------------------------------------*
      *              * Eventuale salvataggio del video in funzione del *
      *              * flag di Save Video e del Tipo Funzionamento     *
      *              *-------------------------------------------------*
       ini-pro-960.
      *                  *---------------------------------------------*
      *                  * Se non e' stato richiesto il salvataggio    *
      *                  * immagine video, o se si e' in background    *
      *                  * non si esegue il Save                       *
      *                  *---------------------------------------------*
           if        w-svv                not  = "S" or
                     z-fun                not  = "F"
                     go to ini-pro-970.
      *                  *---------------------------------------------*
      *                  * Salvataggio immagine video                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       ini-pro-970.
      *                  *---------------------------------------------*
      *                  * Se si e' in background non si esegue il mo- *
      *                  * do di utilizzo                              *
      *                  *---------------------------------------------*
           if        z-fun                not  = "F"
                     go to ini-pro-999.
      *                  *---------------------------------------------*
      *                  * Forzatura del modo di utilizzo, 80 o 132    *
      *                  * colonne, a seconda del tipo operazione      *
      *                  *---------------------------------------------*
           if        s-ope                =    "P#" or
                     s-ope                =    "Q#"
                     move  "M1"           to   v-ope
           else      move  "M0"           to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       ini-pro-999.
           exit.

      *    *===========================================================*
      *    * Fine programma applicativo                                *
      *    *-----------------------------------------------------------*
       fin-pro-000.
      *              *-------------------------------------------------*
      *              * Eventuale ripristino del video in funzione del  *
      *              * flag di Save Video e del Tipo Funzionamento     *
      *              *-------------------------------------------------*
           if        z-stk-svv
                    (z-stk-liv)           not  = "S" or
                     z-fun                not  = "F"
                     go to fin-pro-100.
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       fin-pro-100.
      *              *-------------------------------------------------*
      *              * Clear delle variabili di pipeline associate al  *
      *              * livello di profondita' applicativa che termina  *
      *              *-------------------------------------------------*
           move      "VA"                 to   xse-tpk                .
           move      "P"                  to   w-ipc-tva              .
           move      z-stk-liv            to   w-ipc-liv
           move      spaces               to   w-ipc-dop
                                               w-ipc-var              .
           move      w-ipc-key            to   xse-vlk                .
           start     xse    key not less
                            xse-rck
                            invalid key
                            go to   fin-pro-500.
       fin-pro-200.
           read      xse    next
                            at end
                            go to   fin-pro-500.
           if        xse-tpk              not  = "VA"
                     go to  fin-pro-300.
           move      xse-vlk              to   w-ipc-key              .
           if        w-ipc-tva            not  = "P"    or
                     w-ipc-liv            not  = z-stk-liv
                     go to  fin-pro-300.
           delete    xse   invalid key
                           go to   fin-pro-100.
           go to     fin-pro-200.
       fin-pro-300.
           unlock    xse    record                                    .
       fin-pro-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento sessione utente                   *
      *              *-------------------------------------------------*
           move      "Z<"                 to   s-sts                  .
           if        z-stk-liv            >    zero
                     move  z-stk-fas
                          (z-stk-liv)     to   z-fas                  .
           perform   trt-usl-000          thru trt-usl-999            .
      *              *-------------------------------------------------*
      *              * Aggiornamento livello di nidificazione          *
      *              *-------------------------------------------------*
           subtract  1                    from z-stk-liv              .
      *              *-------------------------------------------------*
      *              * Aggiornamento parametri programma attuale       *
      *              *-------------------------------------------------*
           if        z-stk-liv            >    zero
                     move   z-stk-sap(z-stk-liv)
                                          to   z-sap
                     move   z-stk-arg(z-stk-liv)
                                          to   z-arg
                     move   z-stk-set(z-stk-liv)
                                          to   z-set
                     move   z-stk-fas(z-stk-liv)
                                          to   z-fas
                     move   z-stk-pro(z-stk-liv)
                                          to   z-pro
                     move   z-stk-pst(z-stk-liv)
                                          to   z-pst
           else      move   spaces        to   z-sap
                                               z-arg
                                               z-set
                                               z-fas
                                               z-pro
                     move   zero          to   z-pst                  .
      *              *-------------------------------------------------*
      *              * Cancellazione moduli di i-o obsoleti            *
      *              *-------------------------------------------------*
           perform   cnc-mob-000          thru cnc-mob-999            .
       fin-pro-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione moduli di i-o obsoleti                      *
      *    *-----------------------------------------------------------*
       cnc-mob-000.
           move      zero                 to   w-ctr                  .
       cnc-mob-100.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    z-cnp-ncm
                     go to cnc-mob-999.
           if        z-cnp-pmo (w-ctr)    =    spaces
                     go to cnc-mob-100.
           if        z-cnp-foc (w-ctr)    not  = zero
                     go to cnc-mob-100.
           cancel    z-cnp-pmo (w-ctr)                                .
           move      spaces               to   z-cnp-pmo (w-ctr)      .
           go to     cnc-mob-100.
       cnc-mob-999.
           exit.

      *    *===========================================================*
      *    * Informazioni relative ai moduli di io 'ioxyz' ancora in   *
      *    * essere, ovvero con file-open-counter maggiore di zero     *
      *    *-----------------------------------------------------------*
       inf-mio-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione numero moduli di i-o ancora in *
      *              * essere                                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione sigle dei primi 10 filenames   *
      *              * corrispondenti ai moduli di i-o ancora in es-   *
      *              * sere, e dei relativi file-open-counters         *
      *              *-------------------------------------------------*
           move      spaces               to   w-seu                  .
       inf-mio-100.
      *              *-------------------------------------------------*
      *              * Inizializzazione indice su tabella file open    *
      *              * counters                                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-inx                  .
       inf-mio-200.
      *              *-------------------------------------------------*
      *              * Incremento indice su tabella file open counters *
      *              *-------------------------------------------------*
           add       1                    to   w-inx                  .
      *              *-------------------------------------------------*
      *              * Se oltre numero elementi memorizzati : ad usci- *
      *              * ta                                              *
      *              *-------------------------------------------------*
           if        w-inx                >    z-cnp-ncm
                     go to inf-mio-900.
      *              *-------------------------------------------------*
      *              * Se il pathname del modulo oggetto in esame e' a *
      *              * spaces, lo si ignora                            *
      *              *-------------------------------------------------*
           if        z-cnp-pmo (w-inx)    =    spaces
                     go to inf-mio-300.
      *              *-------------------------------------------------*
      *              * Se il file-open-counter del modulo oggetto in   *
      *              * esame e' a zero, lo si ignora                   *
      *              *-------------------------------------------------*
           if        z-cnp-foc (w-inx)    =    zero
                     go to inf-mio-300.
      *              *-------------------------------------------------*
      *              * Incremento numero moduli di i-o ancora in esse- *
      *              * re                                              *
      *              *-------------------------------------------------*
           add       1                    to   w-ctr                  .
      *              *-------------------------------------------------*
      *              * Aggiornamento sigle dei primi 10 filenames cor- *
      *              * rispondenti ai moduli di i-o ancora in essere,  *
      *              * e dei relativi file open counters               *
      *              *-------------------------------------------------*
           if        w-ctr                >    10
                     go to inf-mio-300.
           move      w-ctr                to   w-pnt                  .
           subtract  1                    from w-pnt                  .
           multiply  8                    by   w-pnt                  .
           add       1                    to   w-pnt                  .
           string    z-cnp-nam (w-inx)
                                delimited by   size
                     " "        delimited by   size
                     z-cnp-foc (w-inx)
                                delimited by   size
                                          into w-seu
                                  with pointer w-pnt                  .
       inf-mio-300.
      *              *-------------------------------------------------*
      *              * Riciclo ad elemento successivo in tabella       *
      *              *-------------------------------------------------*
           go to     inf-mio-200.
       inf-mio-900.
      *              *-------------------------------------------------*
      *              * Numero moduli di i-o ancora in essere in uscita *
      *              *-------------------------------------------------*
           move      w-ctr                to   s-num                  .
      *              *-------------------------------------------------*
      *              * Sigle dei primi 10 filenames corrispondenti ai  *
      *              * moduli di i-o ancora in essere, e dei relativi  *
      *              * file-open-counters, in uscita                   *
      *              *-------------------------------------------------*
           move      w-seu                to   s-alf                  .
       inf-mio-999.
           exit.

      *    *===========================================================*
      *    * Inizio modulo transiente                                  *
      *    *-----------------------------------------------------------*
       ini-mod-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore e puntatore su primo *
      *              * elemento libero in tabella                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr
                                               w-pnt                  .
       ini-mod-100.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-ctr                  .
      *              *-------------------------------------------------*
      *              * Test se oltre max memorizzato                   *
      *              *-------------------------------------------------*
           if        w-ctr                >    z-tma-max
                     go to ini-mod-300.
      *              *-------------------------------------------------*
      *              * Se non oltre max memorizzato                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se trovato modulo cercato              *
      *                  *---------------------------------------------*
           if        z-tma-pmo (w-ctr)    =    s-pmo
                     go to ini-mod-200.
      *                  *---------------------------------------------*
      *                  * Se non trovato modulo cercato               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se elemento libero aggiornamento punta- *
      *                      * tore a primo elemento libero            *
      *                      *-----------------------------------------*
           if        z-tma-pmo (w-ctr)    =    spaces
                     if    w-pnt          =    zero
                           move w-ctr     to   w-pnt                  .
      *                      *-----------------------------------------*
      *                      * Riciclo su prossimo elemento            *
      *                      *-----------------------------------------*
           go to     ini-mod-100.
       ini-mod-200.
      *                  *---------------------------------------------*
      *                  * Se trovato modulo cercato                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento numero utilizzi           *
      *                      *-----------------------------------------*
           add       1                    to   z-tma-nut (w-ctr)      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ini-mod-999.
       ini-mod-300.
      *              *-------------------------------------------------*
      *              * Se oltre max memorizzato                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se e' stato trovato un elemento libero *
      *                  *---------------------------------------------*
           if        w-pnt                =    zero
                     go to ini-mod-400.
      *                  *---------------------------------------------*
      *                  * Se e' stato trovato un elemento libero      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Memorizzazione pathname modulo          *
      *                      *-----------------------------------------*
           move      s-pmo                to   z-tma-pmo (w-pnt)      .
      *                      *-----------------------------------------*
      *                      * Numero utilizzi a 1                     *
      *                      *-----------------------------------------*
           move      1                    to   z-tma-nut (w-pnt)      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ini-mod-999.
       ini-mod-400.
      *                  *---------------------------------------------*
      *                  * Se non e' stato trovato un elemento libero  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se oltre il massimo assoluto si ignora  *
      *                      * la richiesta di memorizzazione          *
      *                      *-----------------------------------------*
           if        w-ctr                >    100
                     go to ini-mod-999.
      *                      *-----------------------------------------*
      *                      * Aggiornamento del max memorizzato       *
      *                      *-----------------------------------------*
           move      w-ctr                to   z-tma-max              .
      *                      *-----------------------------------------*
      *                      * Memorizzazione pathname modulo          *
      *                      *-----------------------------------------*
           move      s-pmo                to   z-tma-pmo (w-ctr)      .
      *                      *-----------------------------------------*
      *                      * Numero utilizzi a 1                     *
      *                      *-----------------------------------------*
           move      1                    to   z-tma-nut (w-ctr)      .
       ini-mod-999.
           exit.

      *    *===========================================================*
      *    * Fine modulo transiente                                    *
      *    *-----------------------------------------------------------*
       fin-mod-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore e puntatore su ulti- *
      *              * mo elemento libero in tabella                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr
                                               w-pnt                  .
       fin-mod-100.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-ctr                  .
      *              *-------------------------------------------------*
      *              * Test se oltre max memorizzato                   *
      *              *-------------------------------------------------*
           if        w-ctr                >    z-tma-max
                     go to fin-mod-300.
      *              *-------------------------------------------------*
      *              * Se non oltre max memorizzato                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se modulo memorizzato                  *
      *                  *---------------------------------------------*
           if        z-tma-pmo (w-ctr)    =    spaces
                     go to fin-mod-200.
      *                  *---------------------------------------------*
      *                  * Se modulo memorizzato                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se numero utilizzi a zero si cancella   *
      *                      * il modulo, altrimenti si ricicla sul    *
      *                      * prossimo elemento                       *
      *                      *-----------------------------------------*
           if        z-tma-nut (w-ctr)    not  = zero
                     go to fin-mod-100.
      *                          *-------------------------------------*
      *                          * Cancellazione modulo                *
      *                          *-------------------------------------*
           cancel    z-tma-pmo (w-ctr)                                .
      *                          *-------------------------------------*
      *                          * Aggiornamento puntatore all'ultimo  *
      *                          * modulo libero                       *
      *                          *-------------------------------------*
           move      w-ctr                to   w-pnt                  .
      *                          *-------------------------------------*
      *                          * Pathname del modulo a spaces        *
      *                          *-------------------------------------*
           move      spaces               to   z-tma-pmo (w-ctr)      .
      *                          *-------------------------------------*
      *                          * Numero utilizzi a zero              *
      *                          *-------------------------------------*
           move      zero                 to   z-tma-nut (w-ctr)      .
      *                          *-------------------------------------*
      *                          * Riciclo su prossimo elemento        *
      *                          *-------------------------------------*
           go to     fin-mod-100.
       fin-mod-200.
      *                  *---------------------------------------------*
      *                  * Se modulo non memorizzato                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento puntatore all'ultimo mo-  *
      *                      * dulo libero                             *
      *                      *-----------------------------------------*
           move      w-ctr                to   w-pnt                  .
      *                      *-----------------------------------------*
      *                      * Riciclo su prossimo elemento            *
      *                      *-----------------------------------------*
           go to     fin-mod-100.
       fin-mod-300.
      *              *-------------------------------------------------*
      *              * Se oltre max memorizzato                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se puntatore ad ultimo modulo libero a zero *
      *                  * si esce                                     *
      *                  *---------------------------------------------*
           if        w-pnt                =    zero
                     go to fin-mod-999.
      *                  *---------------------------------------------*
      *                  * Se l'ultimo modulo libero e' seguito da mo- *
      *                  * duli non liberi si esce                     *
      *                  *---------------------------------------------*
           move      w-pnt                to   w-ctr                  .
       fin-mod-400.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    z-tma-max
                     go to fin-mod-500.
           if        z-tma-pmo (w-ctr)    =    spaces and
                     z-tma-nut (w-ctr)    =    zero
                     go to fin-mod-400
           else      go to fin-mod-999.
       fin-mod-500.
      *                  *---------------------------------------------*
      *                  * Se l'ultimo modulo libero non e' seguito da *
      *                  * moduli non liberi si aggiorna il max memo-  *
      *                  * rizzato                                     *
      *                  *---------------------------------------------*
           move      w-pnt                to   z-tma-max              .
           subtract  1                    from z-tma-max              .
       fin-mod-999.
           exit.

      *    *===========================================================*
      *    * Autorizzazione esecuzione in background                   *
      *    *-----------------------------------------------------------*
       aut-bkg-000.
      *              *-------------------------------------------------*
      *              * Dal 16/03/10 non esiste più la possibilita' di  *
      *              * eseguire programmi in background                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Forzatura foreground                        *
      *                  *---------------------------------------------*
           move      "F"                  to   s-snb                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     aut-bkg-999.
       aut-bkg-800.
      *              *=================================================*
      *              * ISTRUZIONI OBSOLETE                             *
      *              *-------------------------------------------------*
      *              *-------------------------------------------------*
      *              * Richiesta di ammissibilita' del background al   *
      *              * modulo di interfaccia con il sistema operativo  *
      *              * ospite                                          *
      *              *-------------------------------------------------*
           move      "BF"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Se background non ammissibile si forza il fore- *
      *              * ground                                          *
      *              *-------------------------------------------------*
           if        o-sts                not  = spaces
                     move  "F"            to   s-snb
                     go to aut-bkg-999.
      *              *-------------------------------------------------*
      *              * Se per l'utente non vale il background si forza *
      *              * il foreground                                   *
      *              *-------------------------------------------------*
           if        w-ute-bkg-ute        not  = "S"
                     move  "F"            to   s-snb
                     go to aut-bkg-999.
      *              *-------------------------------------------------*
      *              * Se si e' oltre il primo livello di profondita'  *
      *              * applicativa, si forza il foreground             *
      *              *-------------------------------------------------*
           if        z-stk-liv            >    1
                     move  "F"            to   s-snb
                     go to aut-bkg-999.
      *              *-------------------------------------------------*
      *              * Se l'ultima selezione di stampante eseguita     *
      *              * per mezzo del modulo 'mpslct' ha specifica-     *
      *              * to la stampa a video, si forza il foreground    *
      *              *-------------------------------------------------*
           if        z-pss-tds            =    "V"
                     move  "F"            to   s-snb
                     go to aut-bkg-999.
      *              *-------------------------------------------------*
      *              * L'autorizzazione per il background viene con-   *
      *              * cessa solo se non esiste gia' un background in  *
      *              * esecuzione                                      *
      *              *-------------------------------------------------*
           if        z-bkg-snx            =    "N"
                     move  "B"            to   s-snb
           else      move  "F"            to   s-snb                  .
       aut-bkg-999.
           exit.

      *    *===========================================================*
      *    * Inizio programma background                               *
      *    *-----------------------------------------------------------*
       ini-bkg-000.
      *              *-------------------------------------------------*
      *              * Composizione del pathname del file che contiene *
      *              * l'immagine della segreteria                     *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "tmp"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      spaces               to   o-com                  .
           string    z-ppu      delimited by   spaces
                     ".sgr"     delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   w-pth-sgr              .
      *              *-------------------------------------------------*
      *              * Estrazione system date and time                 *
      *              *-------------------------------------------------*
           perform   est-sdt-000          thru est-sdt-999            .
      *              *-------------------------------------------------*
      *              * 'Si' programma in background                    *
      *              *-------------------------------------------------*
           move      "S"                  to   z-bkg-snx              .
      *              *-------------------------------------------------*
      *              * Nome del programma che ha lanciato il back-     *
      *              * ground                                          *
      *              *-------------------------------------------------*
           move      z-stk-pro
                    (z-stk-liv)           to   z-bkg-lan              .
      *              *-------------------------------------------------*
      *              * Nome programma in background                    *
      *              *-------------------------------------------------*
           move      s-npb                to   z-bkg-pro              .
      *              *-------------------------------------------------*
      *              * Pathname programma in background                *
      *              *-------------------------------------------------*
           move      s-pmo                to   z-bkg-pat              .
      *              *-------------------------------------------------*
      *              * Descrizione programma in background             *
      *              *-------------------------------------------------*
           move      s-alf                to   z-bkg-des              .
      *              *-------------------------------------------------*
      *              * Program-start-time programma in background      *
      *              *-------------------------------------------------*
           move      s-sdt                to   z-bkg-pst              .
      *              *-------------------------------------------------*
      *              * Variabile 'Status Background' al valore "E"     *
      *              *-------------------------------------------------*
           move      w-vsb                to   s-var                  .
           move      "G"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      1                    to   s-car                  .
           move      "E"                  to   s-alf                  .
           perform   put-var-000          thru put-var-999            .
      *              *-------------------------------------------------*
      *              * Scrittura immagine status globale di segreteria *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio scrittura record segreteria          *
      *                  *---------------------------------------------*
           move      "OO"                 to   b-ope                  .
           move      z-ppu                to   b-chr                  .
           move      z-run                to   s-num                  .
           call      "swd/mod/prg/obj/mbckgs"
                                         using b
                                               s                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                not  = spaces
                     go to ini-bkg-600.
      *                  *---------------------------------------------*
      *                  * Estrazione segmenti da 255  bytes da record *
      *                  * segreteria                                  *
      *                  *---------------------------------------------*
           move      1                    to   w-pnt                  .
       ini-bkg-200.
           move      spaces               to   w-seu                  .
           move      w-pnt                to   w-ctr                  .
           unstring  z                    into w-seu
                                  with pointer w-pnt                  .
           move      w-seu                to   b-chr                  .
           if        w-pnt                =    w-ctr
                     go to ini-bkg-400.
           move      "PT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgs"
                                         using b
                                               s                      .
           go to     ini-bkg-200.
       ini-bkg-400.
      *                  *---------------------------------------------*
      *                  * Fine scrittura record segreteria            *
      *                  *---------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgs"
                                         using b
                                               s                      .
      *                      *-----------------------------------------*
      *                      * Se errore : uscita                      *
      *                      *-----------------------------------------*
            if       b-rsc                =    spaces
                     go to ini-bkg-800.
       ini-bkg-600.
      *              *-------------------------------------------------*
      *              * Uscita per errore                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di errore         *
      *                  *---------------------------------------------*
           move      "sgr "               to   s-nam                  .
           move      w-pth-sgr            to   s-pat                  .
           move      b-rsc                to   s-sts                  .
           perform   out-err-000          thru out-err-999            .
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori relativi allo status *
      *                  * del background                              *
      *                  *---------------------------------------------*
           perform   fin-bkg-000          thru fin-bkg-999            .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     ini-bkg-900.
       ini-bkg-800.
      *              *-------------------------------------------------*
      *              * Lancio esecuzione in background                 *
      *              *-------------------------------------------------*
       ini-bkg-820.
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo di interfaccia con il   *
      *                  * sistema operativo ospite                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Tipo operazione                         *
      *                      *-----------------------------------------*
           move      "BK"                 to   o-ope                  .
      *                      *-----------------------------------------*
      *                      * Prefisso unico per files temporanei     *
      *                      *-----------------------------------------*
           move      z-ppu                to   o-pat                  .
      *                      *-----------------------------------------*
      *                      * Richiamo del modulo                     *
      *                      *-----------------------------------------*
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       ini-bkg-840.
      *                  *---------------------------------------------*
      *                  * Se esecuzione fallita : si normalizza lo    *
      *                  * statuis del background                      *
      *                  *---------------------------------------------*
           if        o-sts                =    spaces
                     go to ini-bkg-860.
           perform   fin-bkg-000          thru fin-bkg-999            .
       ini-bkg-860.
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     ini-bkg-900.
       ini-bkg-900.
      *              *-------------------------------------------------*
      *              * Program-start-time in uscita                    *
      *              *-------------------------------------------------*
           move      z-bkg-pst            to   s-sdt                  .
      *              *-------------------------------------------------*
      *              * Cancel modulo "mbckgs"                          *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgs"                         .
       ini-bkg-999.
           exit.

      *    *===========================================================*
      *    * Fine programma background                                 *
      *    *-----------------------------------------------------------*
       fin-bkg-000.
      *              *-------------------------------------------------*
      *              * 'No' programma in background                    *
      *              *-------------------------------------------------*
           move      "N"                  to   z-bkg-snx              .
      *              *-------------------------------------------------*
      *              * Nome programma che ha lanciato il background a  *
      *              * spaces                                          *
      *              *-------------------------------------------------*
           move      spaces               to   z-bkg-lan              .
      *              *-------------------------------------------------*
      *              * Nome programma in background a spaces           *
      *              *-------------------------------------------------*
           move      spaces               to   z-bkg-pro              .
      *              *-------------------------------------------------*
      *              * Pathname programma in background a spaces       *
      *              *-------------------------------------------------*
           move      spaces               to   z-bkg-pat              .
      *              *-------------------------------------------------*
      *              * Descrizione programma in background a spaces    *
      *              *-------------------------------------------------*
           move      spaces               to   z-bkg-des              .
      *              *-------------------------------------------------*
      *              * Program-start-time programma background a zero  *
      *              *-------------------------------------------------*
           move      zero                 to   z-bkg-pst              .
      *              *-------------------------------------------------*
      *              * Variabile 'Status Background' al valore "N"     *
      *              *-------------------------------------------------*
           move      w-vsb                to   s-var                  .
           move      "G"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      1                    to   s-car                  .
           move      "N"                  to   s-alf                  .
           perform   put-var-000          thru put-var-999            .
       fin-bkg-999.
           exit.

      *    *===========================================================*
      *    * Estrazione system date and time                           *
      *    *-----------------------------------------------------------*
       est-sdt-000.
      *              *-------------------------------------------------*
      *              * Accettazione system date and time               *
      *              *-------------------------------------------------*
           move      "DT"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      o-det                to   s-sdt                  .
       est-sdt-999.
           exit.

      *    *===========================================================*
      *    * Estrazione giorno della settimana                         *
      *    *-----------------------------------------------------------*
       est-gds-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        s-dat                =    zero
                     move  spaces         to   s-alf
                     move  zero           to   s-num
                     go to est-gds-999.
      *              *-------------------------------------------------*
      *              * Determinazione literal giorno della settimana   *
      *              *-------------------------------------------------*
           move      s-dat                to   w-det-dow-dat          .
           perform   det-dow-lit-000      thru det-dow-lit-999        .
           move      w-det-dow-lit        to   s-alf                  .
           move      w-det-dow-num        to   s-num                  .
       est-gds-999.
           exit.

      *    *===========================================================*
      *    * Estrazione literal del mese                               *
      *    *-----------------------------------------------------------*
       est-ldm-000.
      *              *-------------------------------------------------*
      *              * Test preliminare su tipo literal                *
      *              *-------------------------------------------------*
           if        s-tip                not  = "E" and
                     s-tip                not  = "U" and
                     s-tip                not  = "A"
                     move  "E"            to   s-tip                  .
      *              *-------------------------------------------------*
      *              * Determinazione literal                          *
      *              *-------------------------------------------------*
           if        s-tip                =    "E"
                     go to est-ldm-100
           else if   s-tip                =    "U"
                     go to est-ldm-200
           else      go to est-ldm-300.
       est-ldm-100.
      *              *-------------------------------------------------*
      *              * Literal esteso normale                          *
      *              *-------------------------------------------------*
           if        s-num                =    01
                     move  "Gennaio  "    to   s-alf
           else if   s-num                =    02
                     move  "Febbraio "    to   s-alf
           else if   s-num                =    03
                     move  "Marzo    "    to   s-alf
           else if   s-num                =    04
                     move  "Aprile   "    to   s-alf
           else if   s-num                =    05
                     move  "Maggio   "    to   s-alf
           else if   s-num                =    06
                     move  "Giugno   "    to   s-alf
           else if   s-num                =    07
                     move  "Luglio   "    to   s-alf
           else if   s-num                =    08
                     move  "Agosto   "    to   s-alf
           else if   s-num                =    09
                     move  "Settembre"    to   s-alf
           else if   s-num                =    10
                     move  "Ottobre  "    to   s-alf
           else if   s-num                =    11
                     move  "Novembre "    to   s-alf
           else if   s-num                =    12
                     move  "Dicembre "    to   s-alf
           else      move  spaces         to   s-alf                  .
      *
           go to     est-ldm-999.
       est-ldm-200.
      *              *-------------------------------------------------*
      *              * Literal esteso Uppercase                        *
      *              *-------------------------------------------------*
           if        s-num                =    01
                     move  "GENNAIO  "    to   s-alf
           else if   s-num                =    02
                     move  "FEBBRAIO "    to   s-alf
           else if   s-num                =    03
                     move  "MARZO    "    to   s-alf
           else if   s-num                =    04
                     move  "APRILE   "    to   s-alf
           else if   s-num                =    05
                     move  "MAGGIO   "    to   s-alf
           else if   s-num                =    06
                     move  "GIUGNO   "    to   s-alf
           else if   s-num                =    07
                     move  "LUGLIO   "    to   s-alf
           else if   s-num                =    08
                     move  "AGOSTO   "    to   s-alf
           else if   s-num                =    09
                     move  "SETTEMBRE"    to   s-alf
           else if   s-num                =    10
                     move  "OTTOBRE  "    to   s-alf
           else if   s-num                =    11
                     move  "NOVEMBRE "    to   s-alf
           else if   s-num                =    12
                     move  "DICEMBRE "    to   s-alf
           else      move  spaces         to   s-alf                  .
      *
           go to     est-ldm-999.
       est-ldm-300.
      *              *-------------------------------------------------*
      *              * Literal abbreviato Uppercase                    *
      *              *-------------------------------------------------*
           if        s-num                =    01
                     move  "GEN"          to   s-alf
           else if   s-num                =    02
                     move  "FEB"          to   s-alf
           else if   s-num                =    03
                     move  "MAR"          to   s-alf
           else if   s-num                =    04
                     move  "APR"          to   s-alf
           else if   s-num                =    05
                     move  "MAG"          to   s-alf
           else if   s-num                =    06
                     move  "GIU"          to   s-alf
           else if   s-num                =    07
                     move  "LUG"          to   s-alf
           else if   s-num                =    08
                     move  "AGO"          to   s-alf
           else if   s-num                =    09
                     move  "SET"          to   s-alf
           else if   s-num                =    10
                     move  "OTT"          to   s-alf
           else if   s-num                =    11
                     move  "NOV"          to   s-alf
           else if   s-num                =    12
                     move  "DIC"          to   s-alf
           else      move  spaces         to   s-alf                  .
      *
           go to     est-ldm-999.
       est-ldm-999.
           exit.

      *    *===========================================================*
      *    * Incremento di una data in giorni                          *
      *    *-----------------------------------------------------------*
       inc-dat-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        s-dat                =    zero or
                     s-num                =    zero
                     go to inc-dat-999.
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           move      s-dat                to   w-det-dat-nrg-dtb      .
           move      s-num                to   w-det-dat-nrg-ngi      .
           perform   det-dat-nrg-000      thru det-dat-nrg-999        .
           move      w-det-dat-nrg-dti    to   s-dat                  .
       inc-dat-999.
           exit.

      *    *===========================================================*
      *    * Decremento di una data in giorni                          *
      *    *-----------------------------------------------------------*
       dec-dat-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        s-dat                =    zero or
                     s-num                =    zero
                     go to dec-dat-999.
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           move      s-dat                to   w-det-nrg-dat-dtb      .
           move      s-num                to   w-det-nrg-dat-ngd      .
           perform   det-nrg-dat-000      thru det-nrg-dat-999        .
           move      w-det-nrg-dat-dtd    to   s-dat                  .
       dec-dat-999.
           exit.

      *    *===========================================================*
      *    * Estrazione settimana dell'anno                            *
      *    *-----------------------------------------------------------*
       est-sda-000.
      *              *-------------------------------------------------*
      *              * Salvataggio della data in input                 *
      *              *-------------------------------------------------*
           move      s-dat                to   w-dat                  .
       est-sda-010.
      *              *-------------------------------------------------*
      *              * Determinazione del giorno della settimana rela- *
      *              * tivo al primo giorno dell'anno richiesto        *
      *              *-------------------------------------------------*
           move      01                   to   s-gio                  .
       est-sda-012.
           move      01                   to   s-mes                  .
           move      s-dat                to   w-dsw                  .
           move      s-dat                to   w-det-dow-dat          .
           perform   det-dow-lit-000      thru det-dow-lit-999        .
       est-sda-020.
      *              *-------------------------------------------------*
      *              * Determinazione del numero di giorni che inter-  *
      *              * corrono tra il primo giorno dell'anno richiesto *
      *              * ed il primo inizio settimana (lunedi'), anche   *
      *              * se appartenente alla fine dell'anno precedente. *
      *              * In caso di giorni festivi, il puntatore viene   *
      *              * portato in avanti (Convenzione 09/11/04)        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Giorni comunque da considerare festivi      *
      *                  * indipendentemente dal fatto che cadano      *
      *                  * in un giorno considerato feriale            *
      *                  *                                             *
      *                  * 31/12 - Ultimo dell'anno                    *
      *                  * 01/01 - Primo dell'anno                     *
      *                  *                                             *
      *                  * In questi casi, si forza il segnale come    *
      *                  * fosse 'domenica' e si fa andare avanti il   *
      *                  * contatore                                   *
      *                  *---------------------------------------------*
           if        s-mes                =    31 and
                     s-gio                =    12
                     move  7              to   w-det-dow-num          .
      *
           if        s-mes                =    01 and
                     s-gio                =    01
                     move  7              to   w-det-dow-num          .
      *                  *---------------------------------------------*
      *                  * Test se giorno 'festivo'                    *
      *                  *---------------------------------------------*
           if        w-det-dow-num        =    6 or
                     w-det-dow-num        =    7
                     add 1                to   s-gio
                     go to est-sda-012.
      *
           move      w-det-dow-num        to   w-ctr                  .
           subtract  1                    from w-ctr
                                        giving w-pnt                  .
       est-sda-040.
      *              *-------------------------------------------------*
      *              * Determinazione del primo giorno della prima     *
      *              * settimana dell'anno                             *
      *              *                                                 *
      *              * Convenzione commerciale :                       *
      *              *                                                 *
      *              * La prima settimana dell'anno inizia dal primo   *
      *              * giorno feriale (lunedi' - venerdi') incontrato. *
      *              * Di conseguenza potrebbe anche cadere nella fine *
      *              * dell'anno precedente                            *
      *              *-------------------------------------------------*
           move      w-dsw                to   w-det-nrg-dat-dtb      .
           move      w-pnt                to   w-det-nrg-dat-ngd      .
           perform   det-nrg-dat-000      thru det-nrg-dat-999        .
           move      w-det-nrg-dat-dtd    to   w-dia                  .
       est-sda-050.
      *              *-------------------------------------------------*
      *              * Test se il giorno determinato e' lo stesso di   *
      *              * quello in input                                 *
      *              *-------------------------------------------------*
           if        w-dia                =    w-dat
                     move  1              to   s-num
                     move  w-dat          to   s-dat
                     go to est-sda-999.
      *              *-------------------------------------------------*
      *              * Test se il giorno determinato cade precedente-  *
      *              * mente alla data in input                        *
      *              *-------------------------------------------------*
           if        w-dat                <    w-dia
                     move  w-dat          to   s-dat
                     subtract 1           from s-saa
                     go to est-sda-010.
       est-sda-100.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione per individuare la settimana *
      *              * di appartenenza del giorno richiesto            *
      *              *-------------------------------------------------*
           move      w-dia                to   w-dsa                  .
           subtract  1                    from w-dsa                  .
           move      zero                 to   w-inx                  .
       est-sda-120.
           add       1                    to   w-inx                  .
           if        w-inx                >    55
                     go to est-sda-100.
           move      w-dsa                to   w-det-dat-nrg-dtb      .
           move      7                    to   w-det-dat-nrg-ngi      .
           perform   det-dat-nrg-000      thru det-dat-nrg-999        .
      *              *-------------------------------------------------*
      *              * Test se il giorno determinato cade precedente-  *
      *              * mente alla data in input                        *
      *              *-------------------------------------------------*
           if        w-det-dat-nrg-dti    <    w-dat
                     move  w-det-dat-nrg-dti
                                          to   w-dsa
                     go to est-sda-120.
       est-sda-200.
      *              *-------------------------------------------------*
      *              * Se la data ottenuta e' uguale o superiore a     *
      *              * quella in input, uscita con valore determinati  *
      *              *-------------------------------------------------*
           move      w-inx                to   s-num                  .
           move      w-dsa                to   s-dat                  .
       est-sda-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione secolo nella data                         *
      *    *-----------------------------------------------------------*
       nor-sec-000.
           move      "NS"                 to   o-ope                  .
           move      s-dat                to   o-dat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      o-dat                to   s-dat                  .
       nor-sec-999.
           exit.

      *    *===========================================================*
      *    * Controllo data                                            *
      *    *-----------------------------------------------------------*
       cnt-dat-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   s-sts                  .
      *              *-------------------------------------------------*
      *              * Se data a zero : uscita                         *
      *              *-------------------------------------------------*
           if        s-dat                =    zero
                     go to cnt-dat-999.
      *              *-------------------------------------------------*
      *              * Selezioni preliminari per errori nella data     *
      *              *-------------------------------------------------*
           if        s-sec                >    1    or
                     s-mes                =    zero or
                     s-mes                >    12   or
                     s-gio                =    zero or
                     s-gio                >    31
                     go to cnt-dat-900.
      *              *-------------------------------------------------*
      *              * Test per mese 2 - febbraio                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Calcolo per l'anno bisestile                *
      *                  *---------------------------------------------*
           if        s-mes                =    02
                     divide 4             into s-ann
                                        giving w-ctr
                                     remainder w-pnt
      *                  *---------------------------------------------*
      *                  * Se la divisione per 4 ha restituito zero    *
      *                  *---------------------------------------------*
                     if   w-pnt           =    zero
      *                      *-----------------------------------------*
      *                      * Se l'anno e' zero                       *
      *                      *-----------------------------------------*
                          if   s-ann      =    zero
      *                          *-------------------------------------*
      *                          * Se il secolo e' zero                *
      *                          *-------------------------------------*
                               if   s-sec =    zero
                                    move 28
                                          to   w-tgp (s-mes)
                               else move 29
                                          to   w-tgp (s-mes)
                          else move 29    to   w-tgp (s-mes)
                     else move 28         to   w-tgp (s-mes)          .
      *              *-------------------------------------------------*
      *              * Test finale                                     *
      *              *-------------------------------------------------*
           if        s-gio                not  > w-tgp (s-mes)
                     go to cnt-dat-999.
       cnt-dat-900.
      *              *-------------------------------------------------*
      *              * Flag di errore in uscita                        *
      *              *-------------------------------------------------*
           move      "##"                 to   s-sts                  .
       cnt-dat-999.
           exit.

      *    *===========================================================*
      *    * Memorizzazione codice azienda in uso                      *
      *    *-----------------------------------------------------------*
       mem-azi-000.
      *              *-------------------------------------------------*
      *              * Salvataggio codice azienda attuale              *
      *              *-------------------------------------------------*
           move      z-azi               to   w-sav-zet-azi         .
      *              *-------------------------------------------------*
      *              * Memorizzazione nuovo codice azienda e relativa  *
      *              * descrizione                                     *
      *              *-------------------------------------------------*
           move      s-azi                to   z-azi                  .
           move      s-alf                to   z-azd                  .
      *              *-------------------------------------------------*
      *              * Lettura del codice azienda                      *
      *              *-------------------------------------------------*
           perform   let-azi-auc-000      thru let-azi-auc-999        .
      *              *-------------------------------------------------*
      *              * Inizializzazione tabella coppie name-pathname   *
      *              *-------------------------------------------------*
           perform   cnp-new-000          thru cnp-new-999            .
      *              *-------------------------------------------------*
      *              * Comunicazione del codice azienda e del segnale  *
      *              * di richiesta visualizzazione codice azienda al  *
      *              * modulo 'mvideo'                                 *
      *              *-------------------------------------------------*
           move      "AZ"                 to   v-ope                  .
           move      z-azi                to   v-alf                  .
           if        z-vca                =    "S"
                     move  1              to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Comunicazione del codice azienda al modulo      *
      *              * 'mopsys'                                        *
      *              *-------------------------------------------------*
           move      "AZ"                 to   o-ope                  .
           move      z-azi                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Chiusura file personalizzazioni                 *
      *              *-------------------------------------------------*
           perform   cls-prs-000          thru cls-prs-999            .
      *              *-------------------------------------------------*
      *              * Chiusura file referenze                         *
      *              *-------------------------------------------------*
           perform   cls-ref-000          thru cls-ref-999            .
      *              *-------------------------------------------------*
      *              * Chiusura file encoding automatico               *
      *              *-------------------------------------------------*
           perform   cls-enc-000          thru cls-enc-999            .
      *              *-------------------------------------------------*
      *              * Chiusura file promemoria utente                 *
      *              *-------------------------------------------------*
           perform   cls-upr-000          thru cls-upr-999            .
      *              *-------------------------------------------------*
      *              * Se codice azienda a spaces : uscita             *
      *              *-------------------------------------------------*
           if        z-azi                =    spaces
                     go to mem-azi-900.
      *              *-------------------------------------------------*
      *              * Apertura file personalizzazioni                 *
      *              *-------------------------------------------------*
           perform   opn-prs-000          thru opn-prs-999            .
      *              *-------------------------------------------------*
      *              * Apertura file referenze                         *
      *              *-------------------------------------------------*
           perform   opn-ref-000          thru opn-ref-999            .
      *              *-------------------------------------------------*
      *              * Apertura file encoding automatico               *
      *              *-------------------------------------------------*
           perform   opn-enc-000          thru opn-enc-999            .
      *              *-------------------------------------------------*
      *              * Apertura file promemoria utente                 *
      *              *-------------------------------------------------*
           perform   opn-upr-000          thru opn-upr-999            .
       mem-azi-900.
      *              *-------------------------------------------------*
      *              * Se codice azienda variato rispetto al codice    *
      *              * azienda precedente si normalizzano il numero di *
      *              * dipendenze esistenti per l'azienda, il codice   *
      *              * dipendenza in uso, e la descrizione della di-   *
      *              * pendenza in uso                                 *
      *              *-------------------------------------------------*
           if        z-azi                =    w-sav-zet-azi
                     go to mem-azi-999.
           move      zero                 to   s-car                  .
           move      zero                 to   s-num                  .
           move      spaces               to   s-alf                  .
           perform   mem-dpz-000          thru mem-dpz-999            .
      *              *-------------------------------------------------*
      *              * Apertura sessione utente                        *
      *              *-------------------------------------------------*
           move      "login "             to   z-fas                  .
           perform   trt-usl-000          thru trt-usl-999            .
       mem-azi-999.
           exit.

      *    *===========================================================*
      *    * Memorizzazione codice dipendenza in uso                   *
      *    *-----------------------------------------------------------*
       mem-dpz-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione numero dipendenze esistenti per  *
      *              * l'azienda                                       *
      *              *-------------------------------------------------*
           move      s-car                to   z-dpq                  .
      *              *-------------------------------------------------*
      *              * Memorizzazione del codice dipendenza            *
      *              *-------------------------------------------------*
           move      s-num                to   z-dpz                  .
      *              *-------------------------------------------------*
      *              * Memorizzazione della relativa descrizione       *
      *              *-------------------------------------------------*
           move      s-alf                to   z-dpd                  .
      *              *-------------------------------------------------*
      *              * Comunicazione del codice dipendenza al modulo   *
      *              * 'mvideo'                                        *
      *              *-------------------------------------------------*
           move      "DP"                 to   v-ope                  .
           move      z-dpq                to   v-car                  .
           move      z-dpz                to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       mem-dpz-999.
           exit.

      *    *===========================================================*
      *    * Richiesta codice dipendenza in uso                        *
      *    *-----------------------------------------------------------*
       ric-dpz-000.
      *              *-------------------------------------------------*
      *              * Codice dipendenza in uscita                     *
      *              *-------------------------------------------------*
           move      z-dpz                to   s-num                  .
      *              *-------------------------------------------------*
      *              * Descrizione dipendenza in uscita                *
      *              *-------------------------------------------------*
           move      z-dpd                to   s-alf                  .
       ric-dpz-999.
           exit.

      *    *===========================================================*
      *    * Richiesta codici dipendenza abilitati                     *
      *    *-----------------------------------------------------------*
       abl-dpz-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare work specifica      *
      *              *-------------------------------------------------*
       abl-dpz-010.
      *                  *---------------------------------------------*
      *                  * Numero di dipendenze su cui l'utente in uso *
      *                  * e' abilitato ad operare sull'azienda in uso *
      *                  *---------------------------------------------*
           move      zero                 to   w-abl-dpz-num          .
       abl-dpz-020.
      *                  *---------------------------------------------*
      *                  * Tabella codici dipendenze su cui l'utente   *
      *                  * in uso e' abilitato ad operare sull'azienda *
      *                  * in uso                                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-abl-dpz-c01          .
       abl-dpz-022.
           add       1                    to   w-abl-dpz-c01          .
           if        w-abl-dpz-c01        >    20
                     go to abl-dpz-100.
           move      zero                 to   w-abl-dpz-cod
                                              (w-abl-dpz-c01)         .
           go to     abl-dpz-022.
       abl-dpz-100.
      *              *-------------------------------------------------*
      *              * Lettura record relativo all'utente. Se non tro- *
      *              * vato : uscita con abilitazione su tutte le di-  *
      *              * pendenze.                                       *
      *              *-------------------------------------------------*
           move      "ute "               to   auc-tre                .
           move      z-ute                to   auc-kre                .
           read      auc    with no lock
                            invalid key
                            go to   abl-dpz-900.
      *              *-------------------------------------------------*
      *              * Se l'utente e' di tipo supervisore : uscita con *
      *              * abilitazione su tutte le dipendenze             *
      *              *-------------------------------------------------*
           if        w-ute-tip-ute        =    02
                     go to abl-dpz-900.
       abl-dpz-200.
      *              *-------------------------------------------------*
      *              * Ricerca del codice azienda in uso tra le azien- *
      *              * de su cui l'utente e' abilitato ad operare. Se  *
      *              * non trovato : uscita con abilitazione su tutte  *
      *              * le dipendenze.                                  *
      *              *-------------------------------------------------*
       abl-dpz-210.
           move      zero                 to   w-abl-dpz-c01          .
       abl-dpz-220.
           add       1                    to   w-abl-dpz-c01          .
           if        w-abl-dpz-c01        >    20
                     go to abl-dpz-900.
           if        w-ute-cod-azi
                    (w-abl-dpz-c01)       not  = z-azi
                     go to abl-dpz-220.
       abl-dpz-300.
      *              *-------------------------------------------------*
      *              * Ricerca dei codici dipendenza su cui l'utente   *
      *              * in uso e' abilitato ad operare. Corrispondono   *
      *              * agli elementi immediatamente successivi al co-  *
      *              * dice azienda di abilitazione trovato, aventi    *
      *              * formato 'd=nn'.                                 *
      *              *-------------------------------------------------*
           add       1                    to   w-abl-dpz-c01          .
           if        w-abl-dpz-c01        >    20
                     go to abl-dpz-900.
           move      w-ute-cod-azi
                    (w-abl-dpz-c01)       to   w-abl-dpz-rca          .
           if        w-abl-dpz-rca-012    not  = "d="
                     go to abl-dpz-900.
           if        w-abl-dpz-rca-034    not  numeric
                     go to abl-dpz-300.
           add       1                    to   w-abl-dpz-num          .
           move      w-abl-dpz-rca-034    to   w-abl-dpz-cod
                                              (w-abl-dpz-num)         .
           go to     abl-dpz-300.
       abl-dpz-900.
      *              *-------------------------------------------------*
      *              * Preparazione dei parametri in uscita            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero di dipendenze su cui l'utente in uso *
      *                  * e' abilitato ad operare sull'azienda in uso *
      *                  *---------------------------------------------*
           move      w-abl-dpz-num        to   s-num                  .
      *                  *---------------------------------------------*
      *                  * Tabella codici dipendenze su cui l'utente   *
      *                  * in uso e' abilitato ad operare sull'azienda *
      *                  * in uso                                      *
      *                  *---------------------------------------------*
           move      w-abl-dpz-tbl        to   s-alf                  .
       abl-dpz-999.
           exit.

      *    *===========================================================*
      *    * Memorizzazione codice terminale in uso                    *
      *    *-----------------------------------------------------------*
       mem-ter-000.
           move      s-alf                to   z-ipa                  .
           move      s-ter                to   z-ter                  .
       mem-ter-999.
           exit.

      *    *===========================================================*
      *    * Memorizzazione codice stampante e codice stampante locale *
      *    * associate all'utente in uso                               *
      *    *-----------------------------------------------------------*
       mem-cst-000.
           move      s-asx                to   z-cst                  .
           move      s-adx                to   z-csl                  .
       mem-cst-999.
           exit.

      *    *===========================================================*
      *    * Estrazione codice stampante e codice stampante locale as- *
      *    * sociate all'utente in uso                                 *
      *    *-----------------------------------------------------------*
       est-cst-000.
           move      z-cst                to   s-asx                  .
           move      z-csl                to   s-adx                  .
       est-cst-999.
           exit.

      *    *===========================================================*
      *    * Ingresso parametri di selezione stampa                    *
      *    *-----------------------------------------------------------*
       ing-pss-000.
           move      s-num                to   w-pnt                  .
           multiply  80                   by   w-pnt                  .
           subtract  79                   from w-pnt                  .
           string    s-alf      delimited by   size
                                          into z-pss-buf
                                  with pointer w-pnt                  .
       ing-pss-999.
           exit.

      *    *===========================================================*
      *    * Uscita parametri di selezione stampa                      *
      *    *-----------------------------------------------------------*
       usc-pss-000.
           if        s-num                =    1
                     move  s-sts          to   z-pss-tds              .
           move      s-num                to   w-pnt                  .
           multiply  80                   by   w-pnt                  .
           subtract  79                   from w-pnt                  .
           unstring  z-pss-buf            into w-seu
                                  with pointer w-pnt                  .
           move      w-seu                to   s-alf                  .
       usc-pss-999.
           exit.

      *    *===========================================================*
      *    * Ingresso parametri di tipo stampa, da 'mprint'            *
      *    *-----------------------------------------------------------*
       ing-tds-000.
           move      spaces               to   z-pss-tds              .
           move      s-alf                to   z-pss-uts              .
           move      s-num                to   z-pss-upf              .
       ing-tds-999.
           exit.

      *    *===========================================================*
      *    * Uscita parametri di tipo stampa, da 'mbckgv'              *
      *    *-----------------------------------------------------------*
       usc-tds-000.
           move      z-pss-uts            to   s-alf                  .
           move      z-pss-upf            to   s-num                  .
       usc-tds-999.
           exit.

      *    *===========================================================*
      *    * Pathname unico per file temporaneo                        *
      *    *-----------------------------------------------------------*
       pat-uni-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore, variabile da 0001 a 9999, *
      *              * per files temporanei                            *
      *              *-------------------------------------------------*
           add       1                    to   z-ptu                  .
           if        z-ptu                =    zero
                     move  1              to   z-ptu                  .
      *              *-------------------------------------------------*
      *              * Preparazione del pathname unico                 *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "tmp"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      spaces               to   o-com                  .
           string    z-ppu      delimited by   spaces
                     z-fun      delimited by   size
                     z-ptu      delimited by   size
                     ".tmp"     delimited by   size
                                          into o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   s-pat                  .
       pat-uni-999.
           exit.

      *    *===========================================================*
      *    * Memorizzazione codice utente in uso                       *
      *    *-----------------------------------------------------------*
       mem-ute-000.
      *              *-------------------------------------------------*
      *              * Memorizzazione codice utente                    *
      *              *-------------------------------------------------*
           move      s-ute                to   z-ute                  .
      *              *-------------------------------------------------*
      *              * Lettura dati relativi al codice utente          *
      *              *-------------------------------------------------*
           move      z-ute                to   w-usr                  .
           perform   let-ute-auc-000      thru let-ute-auc-999        .
      *              *-------------------------------------------------*
      *              * Memorizzazione indirizzo mail                   *
      *              *-------------------------------------------------*
           move      w-ute-eml-ute        to   z-uml                  .
      *              *-------------------------------------------------*
      *              * Memorizzazione si/no User System Log            *
      *              *-------------------------------------------------*
           move      w-ute-snx-usl        to   z-usl                  .
           if        z-usl                not  = "S"
                     move  "N"            to   z-usl                  .
      *              *-------------------------------------------------*
      *              * Memorizzazione eventuale alias                  *
      *              *-------------------------------------------------*
______*    move      w-ute-eml-ute        to   z-uml                  .
      *              *-------------------------------------------------*
      *              * Comunicazione del codice utente al modulo       *
      *              * 'mopsys'                                        *
      *              *-------------------------------------------------*
           move      "UT"                 to   o-ope                  .
           move      z-ute                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       mem-ute-999.
           exit.

      *    *===========================================================*
      *    * Estrazione informazioni generali                          *
      *    *-----------------------------------------------------------*
       inf-gen-000.
           perform   est-sdt-000          thru est-sdt-999            .
           move      z-fun                to   s-fun                  .
           move      z-bkg-snx            to   s-snb                  .
           move      z-bkg-pro            to   s-npb                  .
           move      z-bkg-des            to   s-asx                  .
           move      z-bkg-pst            to   s-psb                  .
           move      z-ter                to   s-ter                  .
           move      z-ute                to   s-ute                  .
           move      z-uml                to   s-adx                  .
           move      z-azi                to   s-azi                  .
           move      z-sap                to   s-sap                  .
           move      z-arg                to   s-arg                  .
           move      z-set                to   s-set                  .
           move      z-fas                to   s-fas                  .
           move      z-pro                to   s-pro                  .
           move      z-pst                to   s-pst                  .
           move      z-stk-liv            to   s-liv                  .
       inf-gen-999.
           exit.

      *    *===========================================================*
      *    * Estrazione informazioni su livello profondita'            *
      *    *-----------------------------------------------------------*
       inf-liv-000.
           move      z-stk-sap (s-liv)    to   s-sap                  .
           move      z-stk-arg (s-liv)    to   s-arg                  .
           move      z-stk-set (s-liv)    to   s-set                  .
           move      z-stk-fas (s-liv)    to   s-fas                  .
           move      z-stk-pro (s-liv)    to   s-pro                  .
           move      z-stk-pst (s-liv)    to   s-pst                  .
       inf-liv-999.
           exit.

      *    *===========================================================*
      *    * Estrazione informazioni su azienda in uso                 *
      *    *-----------------------------------------------------------*
       inf-azi-000.
           move      z-azi                to   s-azi                  .
           move      z-azd                to   s-alf                  .
       inf-azi-999.
           exit.

      *    *===========================================================*
      *    * Inizializzazione tabella coppie name-pathname             *
      *    *-----------------------------------------------------------*
       cnp-new-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione area names-pathnames           *
      *              *-------------------------------------------------*
           move      spaces               to   z-cnp                  .
           move      zero                 to   z-cnp-ncm              .
       cnp-new-100.
      *              *-------------------------------------------------*
      *              * Pathname di base files di carattere generale    *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "fdb"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   z-cnp-fdb              .
       cnp-new-200.
      *              *-------------------------------------------------*
      *              * Pathname di base per files normali              *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "azi"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      z-azi                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   z-cnp-fls              .
       cnp-new-300.
      *              *-------------------------------------------------*
      *              * Pathname di base per files di stampa            *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "prf"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   z-cnp-prf              .
       cnp-new-400.
      *              *-------------------------------------------------*
      *              * Pathname di base per files di spool             *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "spl"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   z-cnp-spl              .
       cnp-new-500.
      *              *-------------------------------------------------*
      *              * Cancellazione dei moduli di i-o esistenti       *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
       cnp-new-510.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    z-cnp-ncm
                     go to cnp-new-999.
           if        z-cnp-pmo (w-ctr)    =    spaces
                     go to cnp-new-100.
           cancel    z-cnp-pmo (w-ctr)                                .
           go to     cnp-new-510.
       cnp-new-999.
           exit.

      *    *===========================================================*
      *    * Estrazione pathname memorizzato                           *
      *    *-----------------------------------------------------------*
       nam-pat-000.
      *              *-------------------------------------------------*
      *              * Preparazione status in uscita                   *
      *              *-------------------------------------------------*
           if        s-nam                =    w-kse-nam
                     move  spaces         to   w-kse-nam
                     move  w-kse-top      to   s-sts
           else      move  spaces         to   s-sts                  .
      *              *-------------------------------------------------*
      *              * Estrazione pathname da tabella names-pathnames  *
      *              *-------------------------------------------------*
           perform   cnp-get-000          thru cnp-get-999            .
       nam-pat-999.
           exit.

      *    *===========================================================*
      *    * Estrazione pathname directory di base                     *
      *    *-----------------------------------------------------------*
       ept-ddb-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo di subdirectory   *
      *              *-------------------------------------------------*
           if        s-nam                =    "asc" or
                     s-nam                =    "azi" or
                     s-nam                =    "etc" or
                     s-nam                =    "fdb" or
                     s-nam                =    "ftx" or
                     s-nam                =    "tmp" or
                     s-nam                =    "spl"
                     go to ept-ddb-200
           else if   s-nam                =    "azx"
                     go to ept-ddb-400
           else      go to ept-ddb-600.
       ept-ddb-200.
      *              *-------------------------------------------------*
      *              * Se subdirectory vera e propria                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per subdirectory      *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      s-nam                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   s-pat                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ept-ddb-999.
       ept-ddb-400.
      *              *-------------------------------------------------*
      *              * Se subdirectory relativa all'azienda in uso     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per subdirectory      *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "azi"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      z-azi                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   s-pat                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ept-ddb-999.
       ept-ddb-600.
      *              *-------------------------------------------------*
      *              * Se tipo di subdirectory non riconosciuto        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione pathname per [tmp]             *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "tmp"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   s-pat                  .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ept-ddb-999.
       ept-ddb-999.
           exit.

      *    *===========================================================*
      *    * Informazioni su filename ed estensione                    *
      *    *-----------------------------------------------------------*
       inf-fxt-000.
      *              *-------------------------------------------------*
      *              * Richiesta al modulo di interfaccia con il si-   *
      *              * stema operativo ospite, per avere le informa-   *
      *              * zioni :                                         *
      *              *  - o-mxf : Massima dimensione filename          *
      *              *  - o-mxe : Massima dimensione estensione        *
      *              *  - o-che : Carattere di separazione per esten-  *
      *              *            sione                                *
      *              *-------------------------------------------------*
           move      "EX"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Informazioni in uscita stringate in s-alf       *
      *              *-------------------------------------------------*
           move      spaces               to   s-alf                  .
           string    o-mxf
                                delimited by   size
                     o-mxe
                                delimited by   size
                     o-che
                                delimited by   size
                                          into s-alf                  .
       inf-fxt-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di esecuzione di una procedura batch            *
      *    *-----------------------------------------------------------*
       bat-std-000.
      *              *-------------------------------------------------*
      *              * Salvataggio video, se necessario                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        s-svv                not  = "S"
                     go to bat-std-100.
      *                  *---------------------------------------------*
      *                  * Save video                                  *
      *                  *---------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       bat-std-100.
      *              *-------------------------------------------------*
      *              * Richiamo modulo di interfacciamento con il si-  *
      *              * stema operativo ospite, per l'esecuzione vera   *
      *              * e propria dell'operazione                       *
      *              *-------------------------------------------------*
           move      "XB"                 to   o-ope                  .
           move      s-alf                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       bat-std-200.
      *              *-------------------------------------------------*
      *              * Ripristino video, se necessario                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        s-svv                not  = "S"
                     go to bat-std-999.
      *                  *---------------------------------------------*
      *                  * Restore video                               *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Refresh automatico video                    *
      *                  *---------------------------------------------*
           move      "RF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       bat-std-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di concatenamento di un filename con la direc-  *
      *    * tory 'asc'                                                *
      *    *-----------------------------------------------------------*
       cat-asc-000.
      *              *-------------------------------------------------*
      *              * Estrazione pathname directory di base 'asc' in  *
      *              * s-pat                                           *
      *              *-------------------------------------------------*
           move      "asc"                to   s-nam                  .
           perform   ept-ddb-000          thru ept-ddb-999            .
      *              *-------------------------------------------------*
      *              * Concatenamento                                  *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      s-pat                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      s-alf                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Valore in uscita                                *
      *              *-------------------------------------------------*
           move      o-pat                to   s-pat                  .
       cat-asc-999.
           exit.

      *    *===========================================================*
      *    * Preparazione filename per l'esportazione Utente nella     *
      *    * directory 'asc'                                           *
      *    *-----------------------------------------------------------*
       usr-exp-000.
      *              *-------------------------------------------------*
      *              * Bufferizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      s-nam                to   w-nam                  .
           move      s-tip                to   w-flg                  .
      *              *-------------------------------------------------*
      *              * Estrazione pathname directory di base 'asc' in  *
      *              * s-pat                                           *
      *              *-------------------------------------------------*
           move      "asc"                to   s-nam                  .
           perform   ept-ddb-000          thru ept-ddb-999            .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      10                   to   w-all-str-num          .
           move      s-pat                to   w-all-str-cat (1)      .
           move      "/"                  to   w-all-str-cat (2)      .
           move      z-ute                to   w-all-str-cat (3)      .
           move      "/exp/"              to   w-all-str-cat (4)      .
           move      z-azi                to   w-all-str-cat (5)      .
           move      "_"                  to   w-all-str-cat (6)      .
           move      z-fas                to   w-all-str-cat (7)      .
           move      w-flg                to   w-all-str-cat (8)      .
           move      "."                  to   w-all-str-cat (9)      .
           move      w-nam                to   w-all-str-cat (10)     .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Valore in uscita                                *
      *              *-------------------------------------------------*
           move      w-all-str-alf        to   s-pat                  .
      *              *-------------------------------------------------*
      *              * Assemblaggio                                    *
      *              *-------------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      08                   to   w-all-str-num          .
           move      z-ute                to   w-all-str-cat (1)      .
           move      "/exp/"              to   w-all-str-cat (2)      .
           move      z-azi                to   w-all-str-cat (3)      .
           move      "_"                  to   w-all-str-cat (4)      .
           move      z-fas                to   w-all-str-cat (5)      .
           move      w-flg                to   w-all-str-cat (6)      .
           move      "."                  to   w-all-str-cat (7)      .
           move      w-nam                to   w-all-str-cat (8)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Valore in uscita                                *
      *              *-------------------------------------------------*
           move      w-all-str-alf        to   s-alf                  .
       usr-exp-999.
           exit.

      *    *===========================================================*
      *    * Preparazione filename per l'esportazione file nella       *
      *    * directory 'asc' dell'Azienda                              *
      *    *-----------------------------------------------------------*
       azi-exp-000.
      *              *-------------------------------------------------*
      *              * Bufferizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      s-nam                to   w-nam                  .
           move      s-alf                to   w-alf                  .
           move      s-arg                to   w-arg                  .
           move      s-set                to   w-set                  .
      *              *-------------------------------------------------*
      *              * Estrazione pathname directory di base 'asc' in  *
      *              * s-pat                                           *
      *              *-------------------------------------------------*
           move      "asc"                to   s-nam                  .
           perform   ept-ddb-000          thru ept-ddb-999            .
      *              *-------------------------------------------------*
      *              * Trattamento anno                                *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      s-saa                to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Assemblaggio - parte A                          *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      07                   to   w-all-str-num          .
           move      s-pat                to   w-all-str-cat (1)      .
           move      "/"                  to   w-all-str-cat (2)      .
           move      z-azi                to   w-all-str-cat (3)      .
           move      "/"                  to   w-all-str-cat (4)      .
           move      w-arg                to   w-all-str-cat (5)      .
           move      "/"                  to   w-all-str-cat (6)      .
           move      w-set                to   w-all-str-cat (7)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Assemblaggio - parte B                          *
      *              *-------------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      07                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      "/"                  to   w-all-str-cat (2)      .
           move      v-edt                to   w-all-str-cat (3)      .
      *
           if        s-saa                =    zero
                     move  spaces         to   w-all-str-cat (2)
                     move  spaces         to   w-all-str-cat (3)      .
      *
           move      "/"                  to   w-all-str-cat (4)      .
           move      w-alf                to   w-all-str-cat (5)      .
      *
           if        w-nam                =    spaces
                     move  spaces         to   w-all-str-cat (6)
           else      move  "."            to   w-all-str-cat (6)      .
      *
           move      w-nam                to   w-all-str-cat (7)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *              *-------------------------------------------------*
      *              * Valore in uscita                                *
      *              *-------------------------------------------------*
           move      w-all-str-alf        to   s-alf                  .
       azi-exp-999.
           exit.

      *    *===========================================================*
      *    * Estrazione attributi particolari per prossima operazione  *
      *    * di Open sul file                                          *
      *    *-----------------------------------------------------------*
       nam-paf-000.
      *              *-------------------------------------------------*
      *              * Tipo di filtro da applicare                     *
      *              *-------------------------------------------------*
           move      w-kse-tpf            to   s-num                  .
      *              *-------------------------------------------------*
      *              * Filtro da applicare                             *
      *              *-------------------------------------------------*
           move      w-kse-nmf            to   s-pat                  .
       nam-paf-999.
           exit.

      *    *===========================================================*
      *    * Estrazione pathname da tabella coppie name-pathname       *
      *    *-----------------------------------------------------------*
       cnp-get-000.
           move      spaces               to   s-pat                  .
           perform   inx-cnp-000          thru inx-cnp-999            .
           if        w-flg                not  = spaces
                     go to cnp-get-999.
           move      s-nam                to   w-nam                  .
           if        w-nam-fdb            not  = "x"
                     go to cnp-get-500.
      *
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      "A5"                 to   o-ope                  .
           move      z-cnp-fdb            to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      "A5"                 to   o-ope                  .
           move      s-nam                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   s-pat                  .
           go to     cnp-get-999.
       cnp-get-500.
      *
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      "A5"                 to   o-ope                  .
           move      z-cnp-fls            to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      "A5"                 to   o-ope                  .
           move      s-nam                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      "C9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   s-pat                  .
       cnp-get-999.
           exit.

      *    *===========================================================*
      *    * Estrazione pathname di base per files azienda             *
      *    *-----------------------------------------------------------*
       pnb-fls-000.
      *              *-------------------------------------------------*
      *              * Se il pathname richiesto e' gia' stato determi- *
      *              * nato : si ritorna il valore gia' determinato    *
      *              *-------------------------------------------------*
           if        z-cnp-fls            not  = spaces
                     move  z-cnp-fls      to   s-pat
                     go to pnb-fls-999.
      *              *-------------------------------------------------*
      *              * Altrimenti lo si determina ora                  *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "azi"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      z-azi                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   s-pat                  .
       pnb-fls-999.
           exit.

      *    *===========================================================*
      *    * Estrazione pathname di base per print-files               *
      *    *-----------------------------------------------------------*
       pnb-prf-000.
      *              *-------------------------------------------------*
      *              * Se il pathname richiesto e' gia' stato determi- *
      *              * nato : si ritorna il valore gia' determinato    *
      *              *-------------------------------------------------*
           if        z-cnp-prf            not  = spaces
                     move  z-cnp-prf      to   s-pat
                     go to pnb-prf-999.
      *              *-------------------------------------------------*
      *              * Altrimenti lo si determina ora                  *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "prf"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   s-pat                  .
       pnb-prf-999.
           exit.

      *    *===========================================================*
      *    * Estrazione pathname di base per spool-files               *
      *    *-----------------------------------------------------------*
       pnb-spl-000.
      *              *-------------------------------------------------*
      *              * Se il pathname richiesto e' gia' stato determi- *
      *              * nato : si ritorna il valore gia' determinato    *
      *              *-------------------------------------------------*
           if        z-cnp-spl            not  = spaces
                     move  z-cnp-spl      to   s-pat
                     go to pnb-spl-999.
      *              *-------------------------------------------------*
      *              * Altrimenti lo si determina ora                  *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A5"                 to   o-ope                  .
           move      "spl"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   s-pat                  .
       pnb-spl-999.
           exit.

      *    *===========================================================*
      *    * Incremento file open counter                              *
      *    *-----------------------------------------------------------*
       inc-foc-000.
           perform   inx-cnp-000          thru inx-cnp-999            .
           if        w-flg                =    spaces
                     add   1              to   z-cnp-foc
                                              (z-cnp-inx)
                     move  z-cnp-foc
                          (z-cnp-inx)     to   s-foc
           else      move  zero           to   s-foc                  .
       inc-foc-999.
           exit.

      *    *===========================================================*
      *    * Decremento file open counter                              *
      *    *-----------------------------------------------------------*
       dec-foc-000.
           perform   inx-cnp-000          thru inx-cnp-999            .
           if        w-flg                =    spaces
                     subtract 1           from z-cnp-foc
                                              (z-cnp-inx)
                     move  z-cnp-foc
                          (z-cnp-inx)     to   s-foc
           else      move  zero           to   s-foc                  .
       dec-foc-999.
           exit.

      *    *===========================================================*
      *    * Emissione messaggio di i-o fatal error e Stop Run         *
      *    *-----------------------------------------------------------*
       fat-err-000.
      *              *-------------------------------------------------*
      *              * Aggiornamento sessione utente                   *
      *              *-------------------------------------------------*
           move      "errore"             to   z-fas                  .
           perform   trt-usl-000          thru trt-usl-999            .
      *              *-------------------------------------------------*
      *              * Visualizzazione messaggio                       *
      *              *-------------------------------------------------*
           perform   out-err-000          thru out-err-999            .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo per lo 'Stop Run'           *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mstopr"                         .
       fat-err-999.
           exit.

      *    *===========================================================*
      *    * Emissione messaggio di i-o fatal error                    *
      *    *-----------------------------------------------------------*
       out-err-000.
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di emissione messaggio di   *
      *              * errore                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione parametri                      *
      *                  *---------------------------------------------*
           move      z-fun                to   s-fun                  .
           move      z-azi                to   s-azi                  .
           move      z-ter                to   s-ter                  .
           move      z-ute                to   s-ute                  .
           move      z-sap                to   s-sap                  .
           move      z-arg                to   s-arg                  .
           move      z-set                to   s-set                  .
           move      z-fas                to   s-fas                  .
           move      z-pro                to   s-pro                  .
      *                  *---------------------------------------------*
      *                  * Richiamo modulo                             *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/mioerr"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo funzionamento   *
      *              *-------------------------------------------------*
           if        z-fun                =    "B"
                     go to out-err-400.
       out-err-200.
      *              *-------------------------------------------------*
      *              * Se funzionamento in foreground                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo del modulo per lo 'Stop Run'       *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/mstopr"                         .
       out-err-400.
      *              *-------------------------------------------------*
      *              * Se funzionamento in background                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Variabile 'Status Background' a : "T"       *
      *                  *---------------------------------------------*
           move      w-vsb                to   s-var                  .
           move      "G"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      1                    to   s-car                  .
           move      "T"                  to   s-alf                  .
           perform   put-var-000          thru put-var-999            .
       out-err-999.
           exit.

      *    *===========================================================*
      *    * Estrazione file open counter                              *
      *    *-----------------------------------------------------------*
       est-foc-000.
           perform   inx-cnp-000          thru inx-cnp-999            .
           if        w-flg                =    spaces
                     move  z-cnp-foc
                          (z-cnp-inx)     to   s-foc
           else      move  zero           to   s-foc                  .
       est-foc-999.
           exit.

      *    *===========================================================*
      *    * Memorizzazione pathname modulo oggetto di i-o             *
      *    *-----------------------------------------------------------*
       pmo-ioc-000.
           perform   inx-cnp-000          thru inx-cnp-999            .
           if        w-flg                =    spaces
                     move  s-pmo          to   z-cnp-pmo
                                              (z-cnp-inx)             .
       pmo-ioc-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per la determinazione in z-cnp-inx dell'indice *
      *    * su tabella coppie names-pathnames del file-name in s-nam  *
      *    *-----------------------------------------------------------*
       inx-cnp-000.
           move      spaces               to   w-flg                  .
           set       z-cnp-inx            to   1                      .
           search    z-cnp-ele
                     at end go to inx-cnp-200
                     when   z-cnp-nam
                           (z-cnp-inx)    =    s-nam
                            go to inx-cnp-999                         .
       inx-cnp-200.
           if        z-cnp-ncm            =    256
                     move  "#"            to   w-flg
                     go to inx-cnp-999.
           set       z-cnp-inx            to   1                      .
           search    z-cnp-ele
                     at end move  "#"     to   w-flg
                            go to inx-cnp-999
                     when   z-cnp-nam
                           (z-cnp-inx)    =    spaces
                            next  sentence                            .
           add       1                    to   z-cnp-ncm              .
           set       z-cnp-inx            to   z-cnp-ncm              .
           move      s-nam                to   z-cnp-nam
                                              (z-cnp-inx)             .
           move      zero                 to   z-cnp-foc
                                              (z-cnp-inx)             .
           move      spaces               to   z-cnp-pmo
                                              (z-cnp-inx)             .
       inx-cnp-999.
           exit.

      *    *===========================================================*
      *    * Wait di un secondo                                        *
      *    *-----------------------------------------------------------*
       wai-t1s-000.
           call      "swd/mod/prg/obj/mwait0"                         .
       wai-t1s-999.
           exit.

      *    *===========================================================*
      *    * Wait di 'n' secondi                                       *
      *    *-----------------------------------------------------------*
       wai-tns-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        s-num                =    zero
                     go to wai-tns-999.
           if        s-num                >    30
                     move  30             to   s-num                  .
      *              *-------------------------------------------------*
      *              * Ciclo di attesa                                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr                  .
       wai-tns-200.
           add       1                    to   w-ctr                  .
           if        w-ctr                >    s-num
                     go to wai-tns-999.
      *
           call      "swd/mod/prg/obj/mwait0"                         .
      *
           go to     wai-tns-200.
       wai-tns-999.
           exit.

      *    *===========================================================*
      *    * File to file                                              *
      *    *-----------------------------------------------------------*
       fil-tof-000.
      *              *-------------------------------------------------*
      *              * Salvataggio name del file di origine            *
      *              *-------------------------------------------------*
           move      s-nam                to   w-ftf-nam-ori          .
      *              *-------------------------------------------------*
      *              * Salvataggio name del file di destinazione       *
      *              *-------------------------------------------------*
           move      s-alf                to   w-ftf-nam-dst          .
      *              *-------------------------------------------------*
      *              * Salvataggio tipo di file                        *
      *              *-------------------------------------------------*
           move      s-sts                to   w-ftf-tip-fil          .
      *              *-------------------------------------------------*
      *              * Estrazione pathname per name di origine         *
      *              *-------------------------------------------------*
           move      w-ftf-nam-ori        to   s-nam                  .
           perform   cnp-get-000          thru cnp-get-999            .
           move      s-pat                to   w-ftf-pth-ori          .
      *              *-------------------------------------------------*
      *              * Composizione pathname di destinazione avente lo *
      *              * stesso basename di quello di origine            *
      *              *-------------------------------------------------*
           move      "BN"                 to   o-ope                  .
           move      w-ftf-pth-ori        to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      "PN"                 to   o-ope                  .
           move      w-ftf-nam-dst        to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      o-pat                to   w-ftf-pth-dst          .
       fil-tof-200.
      *              *-------------------------------------------------*
      *              * Preparazione File Copy by Pathname              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione                            *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Pathname di origine                         *
      *                  *---------------------------------------------*
           move      "A5"                 to   o-ope                  .
           move      w-ftf-pth-ori        to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Pathname di destinazione                    *
      *                  *---------------------------------------------*
           move      "A5"                 to   o-ope                  .
           move      w-ftf-pth-dst        to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       fil-tof-400.
      *              *-------------------------------------------------*
      *              * Esecuzione File Copy by Pathname                *
      *              *-------------------------------------------------*
           move      "CP"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       fil-tof-999.
           exit.

      *    *===========================================================*
      *    * File delete                                               *
      *    *-----------------------------------------------------------*
       fil-del-000.
      *              *-------------------------------------------------*
      *              * Salvataggio name del file da deletare           *
      *              *-------------------------------------------------*
           move      s-nam                to   w-ftf-nam-ori          .
      *              *-------------------------------------------------*
      *              * Salvataggio tipo di file                        *
      *              *-------------------------------------------------*
           move      s-sts                to   w-ftf-tip-fil          .
      *              *-------------------------------------------------*
      *              * Estrazione pathname per name file da deletare   *
      *              *-------------------------------------------------*
           move      w-ftf-nam-ori        to   s-nam                  .
           perform   cnp-get-000          thru cnp-get-999            .
           move      s-pat                to   w-ftf-pth-ori          .
      *              *-------------------------------------------------*
      *              * Cancellazione secondo il pathname estratto      *
      *              *-------------------------------------------------*
           perform   fop-del-000          thru fop-del-999            .
       fil-del-999.
           exit.

      *    *===========================================================*
      *    * Pathname delete                                           *
      *    *-----------------------------------------------------------*
       pat-del-000.
      *              *-------------------------------------------------*
      *              * Salvataggio tipo di file                        *
      *              *-------------------------------------------------*
           move      s-sts                to   w-ftf-tip-fil          .
      *              *-------------------------------------------------*
      *              * Salvataggio pathname del file da deletare       *
      *              *-------------------------------------------------*
           move      s-pat                to   w-ftf-pth-ori          .
      *              *-------------------------------------------------*
      *              * Cancellazione secondo il pathname               *
      *              *-------------------------------------------------*
           perform   fop-del-000          thru fop-del-999            .
       pat-del-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione secondo il pathname in w-ftf-pth-ori        *
      *    *-----------------------------------------------------------*
       fop-del-000.
      *              *-------------------------------------------------*
      *              * Preparazione File Delete by Pathname            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione                            *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Pathname da cancellare                      *
      *                  *---------------------------------------------*
           move      "A5"                 to   o-ope                  .
           move      w-ftf-pth-ori        to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Esecuzione File Delete by Pathname              *
      *              *-------------------------------------------------*
           move      "DP"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       fop-del-999.
           exit.

      *    *===========================================================*
      *    * File rename                                               *
      *    *-----------------------------------------------------------*
       fil-ren-000.
      *              *-------------------------------------------------*
      *              * Salvataggio name del file di origine            *
      *              *-------------------------------------------------*
           move      s-nam                to   w-ftf-nam-ori          .
      *              *-------------------------------------------------*
      *              * Salvataggio name del file di destinazione       *
      *              *-------------------------------------------------*
           move      s-alf                to   w-ftf-nam-dst          .
      *              *-------------------------------------------------*
      *              * Salvataggio tipo di file                        *
      *              *-------------------------------------------------*
           move      s-sts                to   w-ftf-tip-fil          .
      *              *-------------------------------------------------*
      *              * Estrazione pathname per name di origine         *
      *              *-------------------------------------------------*
           move      w-ftf-nam-ori        to   s-nam                  .
           perform   cnp-get-000          thru cnp-get-999            .
           move      s-pat                to   w-ftf-pth-ori          .
      *              *-------------------------------------------------*
      *              * Composizione pathname di destinazione avente lo *
      *              * stesso basename di quello di origine            *
      *              *-------------------------------------------------*
           move      "BN"                 to   o-ope                  .
           move      w-ftf-pth-ori        to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      "PN"                 to   o-ope                  .
           move      w-ftf-nam-dst        to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      o-pat                to   w-ftf-pth-dst          .
       fil-ren-200.
      *              *-------------------------------------------------*
      *              * Preparazione File Rename by Pathname            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione                            *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Pathname di origine                         *
      *                  *---------------------------------------------*
           move      "A5"                 to   o-ope                  .
           move      w-ftf-pth-ori        to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Pathname di destinazione                    *
      *                  *---------------------------------------------*
           move      "A5"                 to   o-ope                  .
           move      w-ftf-pth-dst        to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       fil-ren-400.
      *              *-------------------------------------------------*
      *              * Esecuzione File Rename by Pathname              *
      *              *-------------------------------------------------*
           move      "RP"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       fil-ren-999.
           exit.

      *    *===========================================================*
      *    * File rename in senso inverso                              *
      *    *-----------------------------------------------------------*
       ren-fil-000.
      *              *-------------------------------------------------*
      *              * Salvataggio name del file di origine            *
      *              *-------------------------------------------------*
           move      s-nam                to   w-ftf-nam-ori          .
      *              *-------------------------------------------------*
      *              * Salvataggio name del file di destinazione       *
      *              *-------------------------------------------------*
           move      s-alf                to   w-ftf-nam-dst          .
      *              *-------------------------------------------------*
      *              * Salvataggio tipo di file                        *
      *              *-------------------------------------------------*
           move      s-sts                to   w-ftf-tip-fil          .
      *              *-------------------------------------------------*
      *              * Estrazione pathname per name di origine         *
      *              *-------------------------------------------------*
           move      w-ftf-nam-ori        to   s-nam                  .
           perform   cnp-get-000          thru cnp-get-999            .
           move      s-pat                to   w-ftf-pth-ori          .
      *              *-------------------------------------------------*
      *              * Composizione pathname di destinazione avente lo *
      *              * stesso basename di quello di origine            *
      *              *-------------------------------------------------*
           move      "BN"                 to   o-ope                  .
           move      w-ftf-pth-ori        to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      "PN"                 to   o-ope                  .
           move      w-ftf-nam-dst        to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           move      o-pat                to   w-ftf-pth-dst          .
       ren-fil-200.
      *              *-------------------------------------------------*
      *              * Preparazione File Rename by Pathname            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione                            *
      *                  *---------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Pathname di origine                         *
      *                  *---------------------------------------------*
           move      "A5"                 to   o-ope                  .
           move      w-ftf-pth-dst        to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Pathname di destinazione                    *
      *                  *---------------------------------------------*
           move      "A5"                 to   o-ope                  .
           move      w-ftf-pth-ori        to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       ren-fil-400.
      *              *-------------------------------------------------*
      *              * Esecuzione File Rename by Pathname              *
      *              *-------------------------------------------------*
           move      "RP"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       ren-fil-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazioni generali da file [auc]          *
      *    *-----------------------------------------------------------*
       let-psg-auc-000.
      *              *-------------------------------------------------*
      *              * Lettura record personalizzazioni generali       *
      *              *-------------------------------------------------*
           move      "psg "               to   auc-tre                .
           move      spaces               to   auc-kre                .
           read      auc    with no lock
                            invalid key
                            go to   let-psg-auc-100.
           go to     let-psg-auc-200.
       let-psg-auc-100.
      *              *-------------------------------------------------*
      *              * Se personalizzazioni generali non esistenti     *
      *              *-------------------------------------------------*
           move      "#"                  to   w-fes-psg              .
           perform   nor-psg-auc-000      thru nor-psg-auc-999        .
           go to     let-psg-auc-300.
       let-psg-auc-200.
      *              *-------------------------------------------------*
      *              * Se personalizzazioni generali esistenti         *
      *              *-------------------------------------------------*
           move      spaces               to   w-fes-psg              .
           move      auc-dat              to   w-psg                  .
       let-psg-auc-300.
      *              *-------------------------------------------------*
      *              * Memorizzazione parametri generali               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No visualizzazione codice azienda        *
      *                  *---------------------------------------------*
           move      w-psg-snx-azi        to   z-vca                  .
      *                  *---------------------------------------------*
      *                  * Template spooler di stampa                  *
      *                  *---------------------------------------------*
           move      w-psg-tem-spl        to   z-tem                  .
      *                  *---------------------------------------------*
      *                  * Incremento o decremento Left/rght in visua- *
      *                  * lizzazione archivi stampa                   *
      *                  *---------------------------------------------*
           move      w-psg-iod-lor        to   z-iod                  .
      *                  *---------------------------------------------*
      *                  * Template blocco spooler di stampa           *
      *                  *---------------------------------------------*
           move      w-psg-tem-bss        to   z-tbs                  .
      *                  *---------------------------------------------*
      *                  * Parametri per il controllo visualizzazione  *
      *                  * archivi stampa                              *
      *                  *---------------------------------------------*
           move      w-psg-cnt-vpf        to   z-vpf                  .
           if        z-vpf (1 : 1)        not  = "T" and
                     z-vpf (1 : 1)        not  = "I"
                     move  "I"            to   z-vpf (1 : 1)          .
           if        z-vpf (2 : 1)        not  = "S" and
                     z-vpf (2 : 1)        not  = "N"
                     move  "N"            to   z-vpf (2 : 1)          .
      *                  *---------------------------------------------*
      *                  * Parametro generale libero 1                 *
      *                  *---------------------------------------------*
           move      w-psg-cnt-pg1        to   z-pg1                  .
      *                  *---------------------------------------------*
      *                  * Parametro generale libero 2                 *
      *                  *---------------------------------------------*
           move      w-psg-cnt-pg2        to   z-pg2                  .
      *                  *---------------------------------------------*
      *                  * Parametro generale libero 3                 *
      *                  *---------------------------------------------*
           move      w-psg-cnt-pg3        to   z-pg3                  .
      *                  *---------------------------------------------*
      *                  * Correttivo data di sistema                  *
      *                  *---------------------------------------------*
           move      w-psg-scg-cdt        to   z-cdt                  .
      *                  *---------------------------------------------*
      *                  * Correttivi ora di sistema                   *
      *                  *---------------------------------------------*
           move      w-psg-scg-chh        to   z-chh                  .
      *                  *---------------------------------------------*
      *                  * Mail server                                 *
      *                  *---------------------------------------------*
           move      w-psg-msa-isp        to   z-msa                  .
      *                  *---------------------------------------------*
      *                  * Postmaster                                  *
      *                  *---------------------------------------------*
           move      w-psg-eml-psm        to   z-psm                  .
      *                  *---------------------------------------------*
      *                  * User ID account SMTP                        *
      *                  *---------------------------------------------*
           move      w-psg-uid-smp        to   z-uid                  .
      *                  *---------------------------------------------*
      *                  * Password account SMTP                       *
      *                  *---------------------------------------------*
           move      w-psg-pwd-smp        to   z-ppi                  .
      *                  *---------------------------------------------*
      *                  * Password per la protezione documenti PDF    *
      *                  *---------------------------------------------*
           move      w-psg-pdf-pde        to   z-ppe                  .
      *                  *---------------------------------------------*
      *                  * Agente di invio documenti documenti         *
      *                  *---------------------------------------------*
           move      w-psg-asd-smp        to   z-aid                  .
      *                  *---------------------------------------------*
      *                  * Password per MySQL                          *
      *                  *---------------------------------------------*
           move      w-psg-pwd-sql        to   z-pws                  .
       let-psg-auc-400.
      *              *-------------------------------------------------*
      *              * Se il sistema operativo ospite non ammette la   *
      *              * stampa in spool si normalizzano i valori per il *
      *              * template dello spooler di stampa e del template *
      *              * per il blocco stampante al valore convenzionale *
      *              * che indica 'operazione non ammissibile'         *
      *              *-------------------------------------------------*
           move      "A0"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
           if        o-sts                not  = spaces
                     move  "."            to   z-tem
                     move  "."            to   z-tbs                  .
       let-psg-auc-999.
           exit.

      *    *===========================================================*
      *    * Work per normalizzazione record di [auc] 'psg'            *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/waucpsg0.cps"                   .

      *    *===========================================================*
      *    * Lettura tabella pathnames di esecuzione sostitutivi       *
      *    *-----------------------------------------------------------*
       let-pnx-sos-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione tabella pathnames di esecuzio- *
      *              * ne sostitutivi                                  *
      *              *-------------------------------------------------*
           move      spaces               to   z-pnx                  .
      *              *-------------------------------------------------*
      *              * Numero coppie memorizzate : zero                *
      *              *-------------------------------------------------*
           move      zero                 to   z-pnx-ncm              .
      *              *-------------------------------------------------*
      *              * Start su [auc] per records di tipo 'pnx '       *
      *              *-------------------------------------------------*
           move      "pnx "               to   auc-tre                .
           move      spaces               to   auc-kre                .
           start     auc   key not less
                           auc-key
                           invalid key
                           go to   let-pnx-sos-999.
       let-pnx-sos-200.
      *              *-------------------------------------------------*
      *              * Next su [auc] per records di tipo 'pnx '        *
      *              *-------------------------------------------------*
           read      auc   next
                           with no lock
                           at end
                           go to let-pnx-sos-999.
       let-pnx-sos-230.
      *              *-------------------------------------------------*
      *              * Max su [auc] per records di tipo 'pnx '         *
      *              *-------------------------------------------------*
           if        auc-tre              not  = "pnx "
                     go to let-pnx-sos-999.
      *              *-------------------------------------------------*
      *              * Da [auc] a work per records di tipo 'pnx '      *
      *              *-------------------------------------------------*
           move      auc-dat              to   w-pnx                  .
       let-pnx-sos-240.
      *              *-------------------------------------------------*
      *              * Sel su [auc] per records di tipo 'pnx '         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se un elemento a spaces : a riciclo         *
      *                  *---------------------------------------------*
           if        w-pnx-pat-org        =    spaces or
                     w-pnx-pat-sos        =    spaces
                     go to let-pnx-sos-200.
      *                  *---------------------------------------------*
      *                  * Se elemento inibito : a riciclo             *
      *                  *---------------------------------------------*
           move      zero                 to   z-pnx-ctr              .
           inspect   w-pnx-pat-sos    tallying z-pnx-ctr
                     for    all "#"                                   .
           if        z-pnx-ctr            >    zero
                     go to let-pnx-sos-200.
       let-pnx-sos-300.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo record letto      *
      *              *-------------------------------------------------*
           if        w-pnx-pat-org        =    "?azi?"
                     go to let-pnx-sos-600.
       let-pnx-sos-400.
      *              *-------------------------------------------------*
      *              * Se letto un record normale                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero coppie memorizzate        *
      *                  *---------------------------------------------*
           add       1                    to   z-pnx-ncm              .
      *                  *---------------------------------------------*
      *                  * Memorizzazione elemento in tabella pathna-  *
      *                  * mes di esecuzione sostitutivi               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Memorizzazione pathname originale       *
      *                      *-----------------------------------------*
           move      w-pnx-pat-org        to   z-pnx-org
                                              (z-pnx-ncm)             .
      *                      *-----------------------------------------*
      *                      * Memorizzazione pathname sostitutivo     *
      *                      *-----------------------------------------*
           move      w-pnx-pat-sos        to   z-pnx-sos
                                              (z-pnx-ncm)             .
      *                  *---------------------------------------------*
      *                  * Se saturazione tabella : uscita             *
      *                  *---------------------------------------------*
           if        z-pnx-ncm            =    64
                     go to let-pnx-sos-999.
      *                  *---------------------------------------------*
      *                  * A next su [auc] per records di tipo 'pnx '  *
      *                  *---------------------------------------------*
           go to     let-pnx-sos-200.
       let-pnx-sos-600.
      *              *-------------------------------------------------*
      *              * Se letto un record relativo alle aziende per    *
      *              * cui operare la sostituzione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se lista aziende a spaces : si ignora il    *
      *                  * record                                      *
      *                  *---------------------------------------------*
           if        w-pnx-pat-sos        =    spaces
                     go to let-pnx-sos-200.
      *                  *---------------------------------------------*
      *                  * Flag di controllo limitato solo ad alcune   *
      *                  * aziende a Si                                *
      *                  *---------------------------------------------*
           move      "S"                  to   z-pnx-aps-snx          .
      *                  *---------------------------------------------*
      *                  * Memorizzazione lista aziende                *
      *                  *---------------------------------------------*
           move      w-pnx-pat-sos        to   z-pnx-aps-laz          .
      *                  *---------------------------------------------*
      *                  * A next su [auc] per records di tipo 'pnx '  *
      *                  *---------------------------------------------*
           go to     let-pnx-sos-200.
       let-pnx-sos-999.
           exit.

      *    *===========================================================*
      *    * Lettura parametri correlati all'utente in uso             *
      *    *-----------------------------------------------------------*
       let-ute-auc-000.
      *              *-------------------------------------------------*
      *              * Lettura record codice utente                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "ute "               to   auc-tre                .
           move      w-usr                to   auc-kre                .
           read      auc    with no lock
                            invalid key
                            go to   let-ute-auc-100.
           go to     let-ute-auc-200.
       let-ute-auc-100.
      *                  *---------------------------------------------*
      *                  * Se codice utente non esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di utente non trovato           *
      *                      *-----------------------------------------*
           move      "#"                  to   w-fes-ute              .
      *                      *-----------------------------------------*
      *                      * Normalizzazione area codice utente      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice utente                       *
      *                          *-------------------------------------*
           move      w-usr                to   w-ute-cod-ute          .
      *                          *-------------------------------------*
      *                          * Descrizione utente                  *
      *                          *-------------------------------------*
           move      spaces               to   w-ute-des-ute          .
           string    "Utente "  delimited by   size
                     w-usr      delimited by   size
                                          into w-ute-des-ute          .
      *                          *-------------------------------------*
      *                          * Tipo utente : normale               *
      *                          *-------------------------------------*
           move      00                   to   w-ute-tip-ute          .
      *                          *-------------------------------------*
      *                          * Password per l' utente : nessuna    *
      *                          *-------------------------------------*
           move      spaces               to   w-ute-pwd-ute          .
      *                          *-------------------------------------*
      *                          * Livello di protezione massimo per   *
      *                          * l' utente : 000000                  *
      *                          *-------------------------------------*
           move      000000               to   w-ute-liv-pro          .
      *                          *-------------------------------------*
      *                          * Status utente : puo' operare        *
      *                          *-------------------------------------*
           move      00                   to   w-ute-sts-ute          .
      *                          *-------------------------------------*
      *                          * Menu' iniziale utente : nessuno     *
      *                          *-------------------------------------*
           move      spaces               to   w-ute-mnu-ini          .
      *                          *-------------------------------------*
      *                          * Numero aziende in cui l'utente puo' *
      *                          * operare : zero, ovvero tutte        *
      *                          *-------------------------------------*
           move      00                   to   w-ute-num-azi          .
      *                          *-------------------------------------*
      *                          * Elenco aziende in cui l'utente puo' *
      *                          * operare : tutte                     *
      *                          *-------------------------------------*
           move      spaces               to   w-ute-tbl-azi          .
      *                          *-------------------------------------*
      *                          * Numero parametri di abilitazione    *
      *                          * per l'utente : zero                 *
      *                          *-------------------------------------*
           move      zero                 to   w-ute-num-abl          .
      *                          *-------------------------------------*
      *                          * Preparazione tabella parametri di   *
      *                          * abilitazione per l'utente           *
      *                          *-------------------------------------*
           move      spaces               to   w-ute-tbl-abl          .
      *                          *-------------------------------------*
      *                          * Background per l'utente             *
      *                          *-------------------------------------*
           move      "N"                  to   w-ute-bkg-ute          .
      *                          *-------------------------------------*
      *                          * Tipo selezione codice stampante as- *
      *                          * sociato all'utente                  *
      *                          *-------------------------------------*
           move      00                   to   w-ute-tse-cst          .
      *                          *-------------------------------------*
      *                          * Codice stampante associato all'u-   *
      *                          * tente : nessuno                     *
      *                          *-------------------------------------*
           move      spaces               to   w-ute-cst-ass          .
      *                          *-------------------------------------*
      *                          * Ampiezza caratteri di default asso- *
      *                          * ciata all'utente : nessuna          *
      *                          *-------------------------------------*
           move      zero                 to   w-ute-amc-def          .
      *                          *-------------------------------------*
      *                          * Altezza interlinea di default asso- *
      *                          * ciata all'utente : nessuna          *
      *                          *-------------------------------------*
           move      zero                 to   w-ute-ali-def          .
      *                          *-------------------------------------*
      *                          * Codice stampante locale associato   *
      *                          * all'utente : nessuno                *
      *                          *-------------------------------------*
           move      spaces               to   w-ute-csl-aau          .
      *                          *-------------------------------------*
      *                          * Si/No Log System per l'utente       *
      *                          *-------------------------------------*
           move      "N"                  to   w-ute-snx-usl          .
      *                          *-------------------------------------*
      *                          * Altri parametri stampa di uso futu- *
      *                          * ro associati all'utente : nessuno   *
      *                          *-------------------------------------*
           move      spaces               to   w-ute-aps-aau          .
      *                          *-------------------------------------*
      *                          * Data aggiornamento password         *
      *                          *-------------------------------------*
           move      zero                 to   w-ute-dat-agg          .
      *                          *-------------------------------------*
      *                          * Data ultimo utilizzo password       *
      *                          *-------------------------------------*
           move      zero                 to   w-ute-dat-uti          .
      *                          *-------------------------------------*
      *                          * Data scadenza password              *
      *                          *-------------------------------------*
           move      zero                 to   w-ute-dat-scd          .
      *                          *-------------------------------------*
      *                          * Frase di aiuto per la password      *
      *                          *-------------------------------------*
           move      spaces               to   w-ute-frs-aid          .
      *                          *-------------------------------------*
      *                          * Indirizzo e-mail utente             *
      *                          *-------------------------------------*
           move      spaces               to   w-ute-eml-ute          .
      *                          *-------------------------------------*
      *                          * Indirizzo IP o codice nodo IP       *
      *                          *-------------------------------------*
           move      spaces               to   w-ute-ipa-ute          .
      *                          *-------------------------------------*
      *                          * Default tipo stampa per l'utente    *
      *                          *-------------------------------------*
           move      spaces               to   w-ute-def-tst          .
      *                          *-------------------------------------*
      *                          * Area libera per espansioni future   *
      *                          *-------------------------------------*
           move      spaces               to   w-ute-alx-exp          .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     let-ute-auc-999.
       let-ute-auc-200.
      *                  *---------------------------------------------*
      *                  * Se codice utente esistente                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di utente esistente             *
      *                      *-----------------------------------------*
           move      spaces               to   w-fes-ute              .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione parametri letti         *
      *                      *-----------------------------------------*
           move      auc-dat              to   w-ute                  .
      *                      *-----------------------------------------*
      *                      * Eventuali normalizzazioni               *
      *                      *-----------------------------------------*
           if        w-ute-tse-cst        not  numeric
                     move  zero           to   w-ute-tse-cst
                     move  spaces         to   w-ute-cst-ass
                     move  zero           to   w-ute-amc-def
                     move  zero           to   w-ute-ali-def
                     move  spaces         to   w-ute-csl-aau
                     move  spaces         to   w-ute-aps-aau          .
           if        w-ute-dat-agg        not  numeric
                     move  zero           to   w-ute-dat-agg          .
           if        w-ute-dat-uti        not  numeric
                     move  zero           to   w-ute-dat-uti          .
           if        w-ute-dat-scd        not  numeric
                     move  zero           to   w-ute-dat-scd          .
       let-ute-auc-999.
           exit.

      *    *===========================================================*
      *    * Lettura parametri correlati all'azienda in uso            *
      *    *-----------------------------------------------------------*
       let-azi-auc-000.
      *              *-------------------------------------------------*
      *              * Lettura record codice azienda                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "azi "               to   auc-tre                .
           move      z-azi                to   auc-kre                .
           read      auc    with no lock
                            invalid key
                            go to   let-azi-auc-100.
           go to     let-azi-auc-200.
       let-azi-auc-100.
      *                  *---------------------------------------------*
      *                  * Se codice azienda non esistente             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di azienda non trovata          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-fes-azi              .
      *                      *-----------------------------------------*
      *                      * Normalizzazione area codice azienda     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice azienda                      *
      *                          *-------------------------------------*
           move      z-azi                to   w-azi-cod-azi          .
      *                          *-------------------------------------*
      *                          * Descrizione azienda                 *
      *                          *-------------------------------------*
           move      spaces               to   w-azi-des-azi          .
           string    "Azienda " delimited by   size
                     z-azi      delimited by   size
                                          into w-azi-des-azi          .
      *                          *-------------------------------------*
      *                          * Password per l' azienda : nessuna   *
      *                          *-------------------------------------*
           move      spaces               to   w-azi-pwd-azi          .
      *                          *-------------------------------------*
      *                          * Status azienda : bloccata           *
      *                          *-------------------------------------*
           move      01                   to   w-azi-sts-azi          .
      *                          *-------------------------------------*
      *                          * Pathname di base per files normali  *
      *                          * per l'azienda : Spaces              *
      *                          *-------------------------------------*
           move      spaces               to   w-azi-pnb-fls          .
      *                          *-------------------------------------*
      *                          * Pathname di base per files di stam- *
      *                          * pa per l'azienda : Spaces           *
      *                          *-------------------------------------*
           move      spaces               to   w-azi-pnb-prf          .
      *                          *-------------------------------------*
      *                          * Pathname di base per files di spool *
      *                          * per l'azienda : Spaces              *
      *                          *-------------------------------------*
           move      spaces               to   w-azi-pnb-spl          .
      *                          *-------------------------------------*
      *                          * Tabella names files normali con un  *
      *                          * pathname specifico per l'azienda :  *
      *                          * Spaces                              *
      *                          *-------------------------------------*
           move      spaces               to   w-azi-pns-tbl          .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     let-azi-auc-999.
       let-azi-auc-200.
      *                  *---------------------------------------------*
      *                  * Se codice azienda esistente                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di azienda esistente            *
      *                      *-----------------------------------------*
           move      spaces               to   w-fes-azi              .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione parametri letti         *
      *                      *-----------------------------------------*
           move      auc-dat              to   w-azi                  .
       let-azi-auc-999.
           exit.

      *    *===========================================================*
      *    * Lettura record comando relativo a s-fas                   *
      *    *-----------------------------------------------------------*
       let-cmd-auc-000.
      *              *-------------------------------------------------*
      *              * Lettura record codice comando                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "cmd "               to   auc-tre                .
           move      s-fas                to   auc-kre                .
           read      auc    with no lock
                            invalid key
                            go to   let-cmd-auc-100.
           go to     let-cmd-auc-200.
       let-cmd-auc-100.
      *                  *---------------------------------------------*
      *                  * Se codice comando non esistente             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di comando non trovato          *
      *                      *-----------------------------------------*
           move      "#"                  to   w-fes-cmd              .
      *                      *-----------------------------------------*
      *                      * Normalizzazione area codice comando     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice comando                      *
      *                          *-------------------------------------*
           move      s-fas                to   w-cmd-cod-cmd          .
      *                          *-------------------------------------*
      *                          * Descrizione comando                 *
      *                          *-------------------------------------*
           move      spaces               to   w-cmd-des-cmd          .
           string    "Comando " delimited by   size
                     s-fas      delimited by   size
                                          into w-cmd-des-cmd          .
      *                          *-------------------------------------*
      *                          * Tipo comando : task                 *
      *                          *-------------------------------------*
           move      00                   to   w-cmd-tip-cmd          .
      *                          *-------------------------------------*
      *                          * Pathname per l'esecuzione           *
      *                          *-------------------------------------*
           move      spaces               to   w-cmd-pth-cmd          .
      *                          *-------------------------------------*
      *                          * Sistema applicativo                 *
      *                          *-------------------------------------*
           move      s-sap                to   w-cmd-sis-cmd          .
      *                          *-------------------------------------*
      *                          * Area gestionale                     *
      *                          *-------------------------------------*
           move      s-arg                to   w-cmd-are-cmd          .
      *                          *-------------------------------------*
      *                          * Settore gestionale                  *
      *                          *-------------------------------------*
           move      s-set                to   w-cmd-set-cmd          .
      *                          *-------------------------------------*
      *                          * Fase gestionale                     *
      *                          *-------------------------------------*
           move      s-fas                to   w-cmd-fas-cmd          .
      *                          *-------------------------------------*
      *                          * Programma gestionale                *
      *                          *-------------------------------------*
           move      s-pro                to   w-cmd-prg-cmd          .
      *                          *-------------------------------------*
      *                          * Menu' di rientro                    *
      *                          *-------------------------------------*
           move      spaces               to   w-cmd-mnu-rie          .
      *                          *-------------------------------------*
      *                          * Password per il comando             *
      *                          *-------------------------------------*
           move      spaces               to   w-cmd-pwd-cmd          .
      *                          *-------------------------------------*
      *                          * Tipo utente che puo' operare        *
      *                          *-------------------------------------*
           move      00                   to   w-cmd-tip-ute          .
      *                          *-------------------------------------*
      *                          * Livello di protezione del comando   *
      *                          *-------------------------------------*
           move      000000               to   w-cmd-liv-pro          .
      *                          *-------------------------------------*
      *                          * Status del comando                  *
      *                          *-------------------------------------*
           move      00                   to   w-cmd-sts-cmd          .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     let-cmd-auc-999.
       let-cmd-auc-200.
      *                  *---------------------------------------------*
      *                  * Se codice comando esistente                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Segnale di comando esistente            *
      *                      *-----------------------------------------*
           move      spaces               to   w-fes-cmd              .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione parametri letti         *
      *                      *-----------------------------------------*
           move      auc-dat              to   w-cmd                  .
       let-cmd-auc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione password                                     *
      *    *-----------------------------------------------------------*
       acc-pwd-000.
      *              *-------------------------------------------------*
      *              * Save video                                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Preparazione video per accettazione password    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      09                   to   v-lin                  .
           move      26                   to   v-pos                  .
           move      15                   to   v-lto                  .
           move      55                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal per tipo password                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      w-acp-lit            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Due punti                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      42                   to   v-pos                  .
           move      ":"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Codice per tipo password                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      44                   to   v-pos                  .
           move      w-acp-cod            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal per parola chiave                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      15                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      "Parola chiave :"    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-pwd-500.
      *              *-------------------------------------------------*
      *              * Accettazione password                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      44                   to   v-pos                  .
           move      "EXIT"               to   v-pfk(20)              .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Salvataggio password e function key         *
      *                  *---------------------------------------------*
           move      v-alf                to   w-acp-acc              .
           move      v-key                to   w-acp-key              .
      *                  *---------------------------------------------*
      *                  * Visualizzazione Spaces                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      44                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Tests su esecuzione accettazione            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
           if        w-acp-key            =    "EXIT"
                     move  "#"            to   w-acp-exi
                     go to acc-pwd-900.
      *                      *-----------------------------------------*
      *                      * Se password a Spaces                    *
      *                      *-----------------------------------------*
           if        w-acp-acc            =    spaces
                     move  "#"            to   w-acp-exi
                     go to acc-pwd-900.
      *                      *-----------------------------------------*
      *                      * Controllo password                      *
      *                      *-----------------------------------------*
           if        w-acp-acc            =    w-acp-psw or
                     w-acp-acc            =    "pisqin"
                     move  spaces         to   w-acp-exi
           else      go to acc-pwd-500.
       acc-pwd-900.
      *              *-------------------------------------------------*
      *              * Restore video                                   *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-pwd-999.
           exit.

      *    *===========================================================*
      *    * Messaggio di 'Permission denied'                          *
      *    *-----------------------------------------------------------*
       prm-dnd-000.
      *              *-------------------------------------------------*
      *              * Save video                                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Erase linee da 04 a 21                          *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Riquadro basso                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      14                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      19                   to   v-lto                  .
           move      78                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      55                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      12                   to   v-pos                  .
           move      "Manca il permesso per l'esecuzione di questa funzi
      -              "one !"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Riquadro intermedio                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      15                   to   v-lto                  .
           move      72                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Funzione                                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Comando    :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Valore                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-dnd-cmd            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Inibizione                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Inibizione :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Valore                              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      45                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      24                   to   v-pos                  .
           if        w-dnd-ibz            =    01
                     move  "Comando bloccato per tutti gli utenti   "
                                          to   v-alf
           else if   w-dnd-ibz            =   02
                     move  "Comando non nella gerarchia consentita  "
                                          to   v-alf
           else if   w-dnd-ibz            =   03
                     move  "Tipo di utente non abilitato            "
                                          to   v-alf
           else if   w-dnd-ibz            =   04
                     move  "Livello di protezione non sufficiente   "
                                          to   v-alf
           else      move  "??"           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Riquadro alto                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      05                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      09                   to   v-lto                  .
           move      65                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      47                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      17                   to   v-pos                  .
           move      "Tentativo di violazione dei permessi di accesso"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      17                   to   v-lin                  .
           move      69                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Restore video                                   *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       prm-dnd-999.
           exit.

      *    *===========================================================*
      *    * Messaggio di programma gia' in esecuzione                 *
      *    *-----------------------------------------------------------*
       gia-ine-000.
      *              *-------------------------------------------------*
      *              * Save video                                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Preparazione video per messaggio                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Box                                         *
      *                  *---------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      10                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      72                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal interno                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      55                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "Programma non eseguibile perche' gia' in esecuzion
      -              "e !  "              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione presa visione                      *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      12                   to   v-lin                  .
           move      68                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Restore video                                   *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       gia-ine-999.
           exit.

      *    *===========================================================*
      *    * Routine di determinazione del giorno della settimana      *
      *    *-----------------------------------------------------------*
      *    * In input  : w-det-dow-dat = Data completa                 *
      *    *                                                           *
      *    * In output : w-det-dow-lit = Giorno della settimana, lite- *
      *    *                             ral                           *
      *    *                                                           *
      *    *             w-det-dow-num = Giorno della settimana, nu-   *
      *    *                             mero secondo la tabella :     *
      *    *                                                           *
      *    *                             -  1 : Lunedi'                *
      *    *                             -  2 : Martedi'               *
      *    *                             -  3 : Mercoledi'             *
      *    *                             -  4 : Giovedi'               *
      *    *                             -  5 : Venerdi'               *
      *    *                             -  6 : Sabato                 *
      *    *                             -  7 : Domenica               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wgdslit0.cps"                   .

      *    *===========================================================*
      *    * Determinazione (data) + (nr. giorni) = (data)             *
      *    *                                                           *
      *    * N.B.: Questa routine e' identica a quella usata in copy   *
      *    *       dai programmi, salvo per la chiamata di controllo   *
      *    *       della data alla segreteria                          *
      *    *-----------------------------------------------------------*
       det-dat-nrg-000.
      *              *-------------------------------------------------*
      *              * Data base in data in output                     *
      *              *-------------------------------------------------*
           move      w-det-dat-nrg-dtb    to   w-det-dat-nrg-dti      .
      *              *-------------------------------------------------*
      *              * Se numero giorni di incremento a zero : uscita  *
      *              *-------------------------------------------------*
           if        w-det-dat-nrg-ngi    =    zero
                     go to det-dat-nrg-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di primo passaggio         *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-dat-nrg-fpp      .
      *              *-------------------------------------------------*
      *              * Inizializzazione progressivo giorni utilizzati  *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-dat-nrg-pgu      .
       det-dat-nrg-100.
      *              *-------------------------------------------------*
      *              * Ciclo di determinazione                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione data di fine mese            *
      *                  *---------------------------------------------*
           move      w-det-dat-nrg-dti    to   s-dat                  .
           move      31                   to   s-gio                  .
       det-dat-nrg-110.
      *                  *---------------------------------------------*
      *                  * Controllo della data                        *
      *                  *---------------------------------------------*
           perform   cnt-dat-000          thru cnt-dat-999            .
      *                      *-----------------------------------------*
      *                      * Esito della lettura                     *
      *                      *-----------------------------------------*
           if        s-sts                not  = spaces
                     subtract 1           from s-gio
                     go to det-dat-nrg-110.
      *                  *---------------------------------------------*
      *                  * Aggiornamento data incrementata             *
      *                  *---------------------------------------------*
           move      s-dat                to   w-det-dat-nrg-dti      .
      *                  *---------------------------------------------*
      *                  * Determinazione numero giorni utilizzati     *
      *                  *---------------------------------------------*
       det-dat-nrg-120.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che sia il primo   *
      *                      * passaggio o no                          *
      *                      *-----------------------------------------*
           if        w-det-dat-nrg-fpp    =    spaces
                     go to det-dat-nrg-122
           else      go to det-dat-nrg-124.
       det-dat-nrg-122.
      *                      *-----------------------------------------*
      *                      * Primo passaggio                         *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-dat-nrg-fpp      .
           subtract  w-det-dat-nrg-dbg    from w-det-dat-nrg-dig
                                        giving w-det-dat-nrg-ngu      .
           go to     det-dat-nrg-130.
       det-dat-nrg-124.
      *                      *-----------------------------------------*
      *                      * Passaggio successivo al primo           *
      *                      *-----------------------------------------*
           move      w-det-dat-nrg-dig    to   w-det-dat-nrg-ngu      .
           go to     det-dat-nrg-130.
       det-dat-nrg-130.
      *                  *---------------------------------------------*
      *                  * Aggiornamento progressivo giorni utilizzati *
      *                  *---------------------------------------------*
           add       w-det-dat-nrg-ngu    to   w-det-dat-nrg-pgu      .
      *                  *---------------------------------------------*
      *                  * Se progressivo giorni utilizzati superiore  *
      *                  * al numero giorni di incremento richiesti :  *
      *                  * a fine ciclo                                *
      *                  *---------------------------------------------*
           if        w-det-dat-nrg-pgu    not  < w-det-dat-nrg-ngi
                     go to det-dat-nrg-150.
      *                  *---------------------------------------------*
      *                  * Incremento mese/anno                        *
      *                  *---------------------------------------------*
           add       1                    to   w-det-dat-nrg-dim      .
           if        w-det-dat-nrg-dim    >    12
                     move  1              to   w-det-dat-nrg-dim
                     add   1              to   w-det-dat-nrg-dia      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-dat-nrg-100.
       det-dat-nrg-150.
      *              *-------------------------------------------------*
      *              * Fine ciclo                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione numero giorni in esubero ri- *
      *                  * spetto a quelli richiesti                   *
      *                  *---------------------------------------------*
           subtract  w-det-dat-nrg-ngi    from w-det-dat-nrg-pgu
                                        giving w-det-dat-nrg-ngu      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento giorno della data incrementa- *
      *                  * ta                                          *
      *                  *---------------------------------------------*
           subtract  w-det-dat-nrg-ngu    from w-det-dat-nrg-dig      .
       det-dat-nrg-999.
           exit.

      *    *===========================================================*
      *    * Determinazione (data) - (nr. giorni) = (data)             *
      *    *                                                           *
      *    * N.B.: Questa routine e' identica a quella usata in copy   *
      *    *       dai programmi, salvo per la chiamata di controllo   *
      *    *       della data alla segreteria                          *
      *    *-----------------------------------------------------------*
       det-nrg-dat-000.
      *              *-------------------------------------------------*
      *              * Data base in data in output                     *
      *              *-------------------------------------------------*
           move      w-det-nrg-dat-dtb    to   w-det-nrg-dat-dtd      .
      *              *-------------------------------------------------*
      *              * Se numero giorni di decremento a zero : uscita  *
      *              *-------------------------------------------------*
           if        w-det-nrg-dat-ngd    =    zero
                     go to det-nrg-dat-999.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di primo passaggio         *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-nrg-dat-fpp      .
      *              *-------------------------------------------------*
      *              * Inizializzazione progressivo giorni utilizzati  *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-nrg-dat-pgu      .
       det-nrg-dat-100.
      *              *-------------------------------------------------*
      *              * Ciclo di determinazione                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Forzatura data di inizio mese               *
      *                  *---------------------------------------------*
           move      01                   to   w-det-nrg-dat-ddg      .
      *                  *---------------------------------------------*
      *                  * Determinazione numero giorni utilizzati     *
      *                  *---------------------------------------------*
       det-nrg-dat-120.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che sia il primo   *
      *                      * passaggio o no                          *
      *                      *-----------------------------------------*
           if        w-det-nrg-dat-fpp    =    spaces
                     go to det-nrg-dat-122
           else      go to det-nrg-dat-124.
       det-nrg-dat-122.
      *                      *-----------------------------------------*
      *                      * Primo passaggio                         *
      *                      *-----------------------------------------*
           move      "#"                  to   w-det-nrg-dat-fpp      .
           subtract  w-det-nrg-dat-ddg    from w-det-nrg-dat-dbg
                                        giving w-det-nrg-dat-ngu      .
           go to     det-nrg-dat-130.
       det-nrg-dat-124.
      *                      *-----------------------------------------*
      *                      * Passaggio successivo al primo           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione numero giorni del    *
      *                          * mese                                *
      *                          *-------------------------------------*
           move      w-det-nrg-dat-dtd    to   s-dat                  .
           move      31                   to   s-gio                  .
       det-nrg-dat-125.
      *                          *-------------------------------------*
      *                          * Controllo della data                *
      *                          *-------------------------------------*
           perform   cnt-dat-000          thru cnt-dat-999            .
      *                              *---------------------------------*
      *                              * Esito della lettura             *
      *                              *---------------------------------*
           if        s-sts                not  = spaces
                     subtract 1           from s-gio
                     go to det-nrg-dat-125.
           move      s-gio                to   w-det-nrg-dat-ngu      .
           go to     det-nrg-dat-130.
       det-nrg-dat-130.
      *                  *---------------------------------------------*
      *                  * Aggiornamento progressivo giorni utilizzati *
      *                  *---------------------------------------------*
           add       w-det-nrg-dat-ngu    to   w-det-nrg-dat-pgu      .
      *                  *---------------------------------------------*
      *                  * Se progressivo giorni utilizzati superiore  *
      *                  * al numero giorni di decremento richiesti :  *
      *                  * a fine ciclo                                *
      *                  *---------------------------------------------*
           if        w-det-nrg-dat-pgu    not  < w-det-nrg-dat-ngd
                     go to det-nrg-dat-150.
      *                  *---------------------------------------------*
      *                  * Decremento mese/anno                        *
      *                  *---------------------------------------------*
           subtract  1                    from w-det-nrg-dat-ddm      .
           if        w-det-nrg-dat-ddm    =    zero
                     move  12             to   w-det-nrg-dat-ddm
                     subtract 1           from w-det-nrg-dat-dda      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-nrg-dat-100.
       det-nrg-dat-150.
      *              *-------------------------------------------------*
      *              * Fine ciclo                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione numero giorni in esubero ri- *
      *                  * spetto a quelli richiesti                   *
      *                  *---------------------------------------------*
           subtract  w-det-nrg-dat-ngd    from w-det-nrg-dat-pgu
                                        giving w-det-nrg-dat-ngu      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento giorno della data decrementa- *
      *                  * ta                                          *
      *                  *---------------------------------------------*
           add       w-det-nrg-dat-ngu    to   w-det-nrg-dat-ddg      .
       det-nrg-dat-999.
           exit.

      *    *===========================================================*
      *    * Richiesta di determinazione dati valuta base relativi al  *
      *    * codice azienda                                            *
      *    *-----------------------------------------------------------*
       val-bas-000.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
           move      w-azi-sgl-vlt        to   s-asx                  .
           move      w-azi-dec-vlt        to   s-dec                  .
           move      w-azi-tdc-vlt        to   s-sgn                  .
           move      w-azi-cdc-vlt        to   s-num                  .
           move      w-azi-des-vlt        to   s-adx (01 : 20)        .
           move      w-azi-din-vlt        to   s-adx (21 : 20)        .
      *              *-------------------------------------------------*
      *              * Test se operazione con esito positivo           *
      *              *-------------------------------------------------*
           if        s-asx                not  = spaces
                     go to val-bas-999.
      *              *-------------------------------------------------*
      *              * Preparazione valori di output con dati relativi *
      *              * letti in apposita interfaccia                   *
      *              *-------------------------------------------------*
           move      c-sgl                to   s-asx                  .
           move      c-dec                to   s-dec                  .
           move      c-tdc                to   s-sgn                  .
           move      c-cdc                to   s-num                  .
           move      c-des                to   s-adx (01 : 20)        .
           move      c-din                to   s-adx (21 : 20)        .
       val-bas-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione promemoria per l'utente                     *
      *    *-----------------------------------------------------------*
       del-upr-000.
      *              *-------------------------------------------------*
      *              * Codice promemoria da chiamante                  *
      *              *-------------------------------------------------*
           move      s-num                to   w-upr-cod              .
      *              *-------------------------------------------------*
      *              * Test se presente codice promemoria              *
      *              *-------------------------------------------------*
           if        w-upr-cod            =    zero
                     go to del-upr-900.
      *              *-------------------------------------------------*
      *              * Utente                                          *
      *              *-------------------------------------------------*
           move      z-ute                to   w-upr-ute              .
       del-upr-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave                             *
      *              *-------------------------------------------------*
           move      w-upr-ute            to   upr-cod-ute            .
           move      w-upr-cod            to   upr-cod-upr            .
      *              *-------------------------------------------------*
      *              * Cancellazione vera e propria                    *
      *              *-------------------------------------------------*
           delete    upr    record
                            invalid key
                            go to del-upr-900.
       del-upr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     del-upr-999.
       del-upr-999.
           exit.

      *    *===========================================================*
      *    * Trattamento promemoria per l'utente                       *
      *    *-----------------------------------------------------------*
       trt-upr-000.
      *              *-------------------------------------------------*
      *              * Utente                                          *
      *              *-------------------------------------------------*
           move      z-ute                to   w-upr-ute              .
      *              *-------------------------------------------------*
      *              * Data di sistema                                 *
      *              *-------------------------------------------------*
           perform   est-sdt-000          thru est-sdt-999            .
           move      s-dat                to   w-upr-daa              .
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-upr-pcn              .
       trt-upr-100.
      *              *-------------------------------------------------*
      *              * Start su file [upr]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione chiave                         *
      *                  *---------------------------------------------*
           move      w-upr-ute            to   upr-cod-ute            .
           move      zero                 to   upr-cod-upr            .
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           start     upr    key not less
                            upr-k01
                            invalid key
                            go to   trt-upr-900.
       trt-upr-200.
      *              *-------------------------------------------------*
      *              * Get-next [upr]                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           read      upr   next
                           at end
                           go to   trt-upr-220.
      *                  *---------------------------------------------*
      *                  * A test sul massimo                          *
      *                  *---------------------------------------------*
           go to     trt-upr-300.
       trt-upr-220.
      *                  *---------------------------------------------*
      *                  * Se definizione non trovata                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Esecuzione unlock                       *
      *                      *-----------------------------------------*
           unlock    upr    records                                   .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     trt-upr-900.
       trt-upr-300.
      *              *-------------------------------------------------*
      *              * Max su [upr]                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su utente : uscita                     *
      *                  *---------------------------------------------*
           if        upr-cod-ute          =    w-upr-ute
                     go to trt-upr-400.
       trt-upr-320.
      *                  *---------------------------------------------*
      *                  * Se test non superato                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Esecuzione unlock                       *
      *                      *-----------------------------------------*
           unlock    upr    records                                   .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     trt-upr-900.
       trt-upr-400.
      *              *-------------------------------------------------*
      *              * Sel su [upr]                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se data scadenza a zero                *
      *                  *---------------------------------------------*
           if        upr-dfi-upr          =    zero
                     go to trt-upr-420.
      *                  *---------------------------------------------*
      *                  * Test se data scadenza inferiore a oggi, se  *
      *                  * data visualizzazione non a zero e se flag   *
      *                  * di presa visione attivo per cancellabilita' *
      *                  * promemoria                                  *
      *                  *---------------------------------------------*
           if        upr-snx-acp          not  = "S"
                     go to trt-upr-420.
           if        upr-dfi-upr          <    w-upr-daa and
                     upr-dpv-upr          >    zero
                     move  "#"            to   w-upr-pcn
                     go to trt-upr-650.
       trt-upr-420.
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   trt-upr-sel-000      thru trt-upr-sel-999        .
      *                  *---------------------------------------------*
      *                  * Test su flag di uscita da selezione         *
      *                  *---------------------------------------------*
           if        w-upr-flg            =    spaces
                     go to trt-upr-500.
      *                  *---------------------------------------------*
      *                  * Rilascio del record                         *
      *                  *---------------------------------------------*
           unlock    upr    records                                   .
      *                  *---------------------------------------------*
      *                  * A riciclo                                   *
      *                  *---------------------------------------------*
           go to     trt-upr-800.
       trt-upr-500.
      *              *-------------------------------------------------*
      *              * Visualizzazione del promemoria                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine                                  *
      *                  *---------------------------------------------*
           perform   trt-upr-vis-000      thru trt-upr-vis-999        .
      *                  *---------------------------------------------*
      *                  * Aggiornamento ultima presa visione          *
      *                  *---------------------------------------------*
           move      w-upr-daa            to   upr-dpv-upr            .
      *                  *---------------------------------------------*
      *                  * Aggiornamento Si/no promemoria visionato,   *
      *                  *---------------------------------------------*
           move      "#"                  to   upr-snx-pvs            .
      *                  *---------------------------------------------*
      *                  * Test su tipo operazione                     *
      *                  *---------------------------------------------*
           if        s-ope                not  = "AP"
                     go to trt-upr-600.
      *                  *---------------------------------------------*
      *                  * Aggiornamento ultima presa visione          *
      *                  *---------------------------------------------*
           move      w-upr-daa            to   upr-dpv-upr            .
      *                  *---------------------------------------------*
      *                  * Test su Modalita' di avviso                 *
      *                  *---------------------------------------------*
           if        upr-mda-upr          not  = 02
                     go to trt-upr-600.
      *                  *---------------------------------------------*
      *                  * Aggiornamento Si/no promemoria visionato,   *
      *                  * per consentire la visualizzazione ad ogni   *
      *                  * collegamento                                *
      *                  *---------------------------------------------*
           move      spaces               to   upr-snx-pvs            .
       trt-upr-600.
      *                  *---------------------------------------------*
      *                  * Update record [upr]                         *
      *                  *---------------------------------------------*
           rewrite   upr-rec invalid key
                             go to  trt-upr-800.
       trt-upr-650.
      *                  *---------------------------------------------*
      *                  * Rilascio del record                         *
      *                  *---------------------------------------------*
           unlock    upr    records                                   .
       trt-upr-700.
      *              *-------------------------------------------------*
      *              * Eventuale cancellazione [upr]                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di cancellazione               *
      *                  *---------------------------------------------*
           if        upr-snx-acp          not  = "S"
                     go to trt-upr-800.
           if        w-upr-pcn            =    spaces
                     go to trt-upr-800.
       trt-upr-750.
      *                  *---------------------------------------------*
      *                  * Cancellazione [upr]                         *
      *                  *---------------------------------------------*
           delete    upr    record
                            invalid key
                            go to trt-upr-800.
       trt-upr-800.
      *              *-------------------------------------------------*
      *              * Riciclo su next [upr]                           *
      *              *-------------------------------------------------*
           go to     trt-upr-200.
       trt-upr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     trt-upr-999.
       trt-upr-999.
           exit.

      *    *===========================================================*
      *    * Determinazione esistenza promemoria per l'utente in uso   *
      *    *                                                           *
      *    * Subroutine di selezione                                   *
      *    *-----------------------------------------------------------*
       trt-upr-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari per le selezioni    *
      *              *-------------------------------------------------*
           move      "#"                  to   w-upr-flg              .
           move      spaces               to   w-upr-txt              .
           move      zero                 to   w-upr-ctr              .
           move      zero                 to   w-upr-dpm (01)         .
           move      zero                 to   w-upr-dpm (02)         .
           move      zero                 to   w-upr-dpm (03)         .
           move      zero                 to   w-upr-dpm (04)         .
           move      zero                 to   w-upr-dpm (05)         .
           move      zero                 to   w-upr-dpm (06)         .
           move      zero                 to   w-upr-dpm (07)         .
           move      zero                 to   w-upr-dpm (08)         .
           move      zero                 to   w-upr-dpm (09)         .
           move      zero                 to   w-upr-dpm (10)         .
           move      zero                 to   w-upr-dpm (11)         .
           move      zero                 to   w-upr-dnv              .
       trt-upr-sel-100.
      *              *-------------------------------------------------*
      *              * Determinazione data relativa al promemoria      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della modalita' di   *
      *                  * ripetizione                                 *
      *                  *---------------------------------------------*
           if        upr-mdr-upr          =    01
                     go to trt-upr-sel-110
           else if   upr-mdr-upr          =    02
                     go to trt-upr-sel-120
           else if   upr-mdr-upr          =    03
                     go to trt-upr-sel-130
           else if   upr-mdr-upr          =    04
                     go to trt-upr-sel-140
           else if   upr-mdr-upr          =    05
                     go to trt-upr-sel-150
           else      go to trt-upr-sel-900.
       trt-upr-sel-110.
      *                  *---------------------------------------------*
      *                  * Se alla data indicata                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-upr-ctr              .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione data                    *
      *                      *-----------------------------------------*
           move      upr-dat-upr          to   w-upr-dpm
                                              (w-upr-ctr)             .
      *                      *-----------------------------------------*
      *                      * A trattamento eventuale preavviso       *
      *                      *-----------------------------------------*
           go to     trt-upr-sel-200.
       trt-upr-sel-120.
      *                  *---------------------------------------------*
      *                  * Lo stesso giorno di ogni settimana          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deduzione del giorno della settimana    *
      *                      * rispetto alla data attuale              *
      *                      *-----------------------------------------*
           move      w-upr-daa            to   w-det-dow-dat          .
           perform   det-dow-lit-000      thru det-dow-lit-999        .
           move      w-det-dow-num        to   w-upr-gds              .
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-upr-ctr              .
      *                      *-----------------------------------------*
      *                      * Test su giorno determinato              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se uguali                           *
      *                          *-------------------------------------*
           if        w-upr-gds            =    upr-gio-set
                     move  w-upr-daa
                                          to   w-upr-dpm
                                              (w-upr-ctr)
                     go to trt-upr-sel-128.
      *                          *-------------------------------------*
      *                          * Se giorno determinato maggiore      *
      *                          *-------------------------------------*
           if        w-upr-gds            <    upr-gio-set
                     subtract  w-upr-gds
                                          from upr-gio-set
                                        giving w-upr-n01
                     go to trt-upr-sel-126.
      *                          *-------------------------------------*
      *                          * Se giorno determinato minore        *
      *                          *-------------------------------------*
           move      w-upr-gds            to   w-upr-n01              .
           add       7                    to   w-upr-n01              .
           subtract  w-upr-gds            from upr-gio-set
                                        giving w-upr-n01              .
       trt-upr-sel-126.
      *                      *-----------------------------------------*
      *                      * Incremento data                         *
      *                      *-----------------------------------------*
           move      w-upr-daa            to   w-det-dat-nrg-dtb      .
           move      w-upr-n01            to   w-det-dat-nrg-ngi      .
           perform   det-dat-nrg-000      thru det-dat-nrg-999        .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione data                    *
      *                      *-----------------------------------------*
           move      w-det-dat-nrg-dti    to   w-upr-dpm
                                              (w-upr-ctr)             .
       trt-upr-sel-128.
      *                      *-----------------------------------------*
      *                      * A trattamento eventuale preavviso       *
      *                      *-----------------------------------------*
           go to     trt-upr-sel-200.
       trt-upr-sel-130.
      *                  *---------------------------------------------*
      *                  * Lo stesso giorno di ogni mese               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Completamento data                      *
      *                      *-----------------------------------------*
           move      w-upr-daa            to   s-dat                  .
           move      upr-gio-rif          to   s-gio                  .
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-upr-ctr              .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione data                    *
      *                      *-----------------------------------------*
           move      s-dat                to   w-upr-dpm
                                              (w-upr-ctr)             .
      *                      *-----------------------------------------*
      *                      * A trattamento eventuale preavviso       *
      *                      *-----------------------------------------*
           go to     trt-upr-sel-200.
       trt-upr-sel-140.
      *                  *---------------------------------------------*
      *                  * Lo stesso giorno di ogni anno               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Completamento data                      *
      *                      *-----------------------------------------*
           move      w-upr-daa            to   s-dat                  .
           move      upr-gio-rif          to   s-gio                  .
           move      upr-mes-rif          to   s-mes                  .
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-upr-ctr              .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione data                    *
      *                      *-----------------------------------------*
           move      s-dat                to   w-upr-dpm
                                              (w-upr-ctr)             .
      *                      *-----------------------------------------*
      *                      * A trattamento eventuale preavviso       *
      *                      *-----------------------------------------*
           go to     trt-upr-sel-200.
       trt-upr-sel-150.
      *                  *---------------------------------------------*
      *                  * Ogni giorno                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-upr-ctr              .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione data                    *
      *                      *-----------------------------------------*
           move      w-upr-daa            to   w-upr-dpm
                                              (w-upr-ctr)             .
      *                      *-----------------------------------------*
      *                      * A trattamento eventuale preavviso       *
      *                      *-----------------------------------------*
           go to     trt-upr-sel-200.
       trt-upr-sel-200.
      *              *-------------------------------------------------*
      *              * Trattamento eventuale preavviso                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su numero giorni di preavviso          *
      *                  *---------------------------------------------*
           if        upr-ngp-upr          =    zero or
                     upr-mdp-upr          =    01
                     go to trt-upr-sel-300.
      *                  *---------------------------------------------*
      *                  * Test se preavviso quotidiano                *
      *                  *---------------------------------------------*
           if        upr-mdp-upr          =    03
                     go to trt-upr-sel-210.
       trt-upr-sel-205.
      *                  *---------------------------------------------*
      *                  * Se preavviso unico                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Decremento data                         *
      *                      *-----------------------------------------*
           move      w-upr-dpm (1)        to   w-det-nrg-dat-dtb      .
           move      upr-ngp-upr          to   w-det-nrg-dat-ngd      .
           perform   det-nrg-dat-000      thru det-nrg-dat-999        .
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-upr-ctr              .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione data                    *
      *                      *-----------------------------------------*
           move      w-det-nrg-dat-dtd    to   w-upr-dpm
                                              (w-upr-ctr)             .
      *                      *-----------------------------------------*
      *                      * A selezione su castelletto date         *
      *                      *-----------------------------------------*
           go to     trt-upr-sel-300.
       trt-upr-sel-210.
      *                  *---------------------------------------------*
      *                  * Ciclo di preparazione castelletto date      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione contatore               *
      *                      *-----------------------------------------*
           move      zero                 to   w-upr-n01              .
       trt-upr-sel-220.
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-upr-n01              .
      *                      *-----------------------------------------*
      *                      * Test su contatore                       *
      *                      *-----------------------------------------*
           if        w-upr-n01            >    upr-ngp-upr
                     go to trt-upr-sel-300.
      *                      *-----------------------------------------*
      *                      * Decremento data                         *
      *                      *-----------------------------------------*
           move      w-upr-dpm (1)        to   w-det-nrg-dat-dtb      .
           move      w-upr-n01            to   w-det-nrg-dat-ngd      .
           perform   det-nrg-dat-000      thru det-nrg-dat-999        .
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-upr-ctr              .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione data                    *
      *                      *-----------------------------------------*
           move      w-det-nrg-dat-dtd    to   w-upr-dpm
                                              (w-upr-ctr)             .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     trt-upr-sel-220.
       trt-upr-sel-300.
      *              *-------------------------------------------------*
      *              * Selezione su castelletto date                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-upr-ctx              .
       trt-upr-sel-320.
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-upr-ctx              .
      *                  *---------------------------------------------*
      *                  * Test sul massimo numero elementi            *
      *                  *---------------------------------------------*
           if        w-upr-ctx            >    w-upr-max
                     go to trt-upr-sel-900.
      *                  *---------------------------------------------*
      *                  * Test sul numero elementi creati             *
      *                  *---------------------------------------------*
           if        w-upr-ctx            >    w-upr-ctr
                     go to trt-upr-sel-900.
      *                  *---------------------------------------------*
      *                  * Test se data a zero                         *
      *                  *---------------------------------------------*
           if        w-upr-dpm
                    (w-upr-ctx)           =    zero
                     go to trt-upr-sel-900.
      *                  *---------------------------------------------*
      *                  * Test se data trovata                        *
      *                  *---------------------------------------------*
           if        w-upr-dpm
                    (w-upr-ctx)           =    w-upr-daa
                     go to trt-upr-sel-400.
      *                  *---------------------------------------------*
      *                  * Test se la data incontrata e' inferiore     *
      *                  * alla data odierna                           *
      *                  *---------------------------------------------*
           if        w-upr-dpm
                    (w-upr-ctx)           not  < w-upr-daa
                     go to trt-upr-sel-380.
      *                  *---------------------------------------------*
      *                  * Test se la data incontrata e' la maggiore   *
      *                  * finora incontrata tra quelle inferiori alla *
      *                  * data odierna, con conseguente memorizzazio- *
      *                  * ne della stessa                             *
      *                  *---------------------------------------------*
           if        w-upr-dpm
                    (w-upr-ctx)           >    w-upr-dnv
                     move  w-upr-dpm
                          (w-upr-ctx)     to   w-upr-dnv              .
      *                  *---------------------------------------------*
      *                  * Test se la data memorizzata e' comunque     *
      *                  * maggiore di quella dell'ultima visualizza-  *
      *                  * zione, allora il promemoria deve ancora es- *
      *                  * sere visto e si salta il test sulla data di *
      *                  * scadenza                                    *
      *                  *---------------------------------------------*
           if        w-upr-dnv            >    upr-dpv-upr
                     move  w-upr-dpm (1)  to   w-upr-dtv
                     go to trt-upr-sel-500.
       trt-upr-sel-380.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     trt-upr-sel-320.
       trt-upr-sel-400.
      *              *-------------------------------------------------*
      *              * Bufferizzazione data da visualizzare            *
      *              *-------------------------------------------------*
           move      w-upr-dpm (1)        to   w-upr-dtv              .
       trt-upr-sel-450.
      *              *-------------------------------------------------*
      *              * Selezione su durata promemoria                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se nessun limite di durata             *
      *                  *---------------------------------------------*
           if        upr-drt-upr          =    01
                     go to trt-upr-sel-500.
      *                  *---------------------------------------------*
      *                  * Test se fino alla data indicata             *
      *                  *---------------------------------------------*
           if        upr-dfi-upr          <    w-upr-daa
                     go to trt-upr-sel-900.
       trt-upr-sel-500.
      *              *-------------------------------------------------*
      *              * Selezione su modalita' di avviso                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione                                  *
      *                  *---------------------------------------------*
           if        upr-mda-upr          =    01
                     go to trt-upr-sel-510
           else if   upr-mda-upr          =    02
                     go to trt-upr-sel-520
           else if   upr-mda-upr          =    03
                     go to trt-upr-sel-530
           else      go to trt-upr-sel-900.
       trt-upr-sel-510.
      *                  *---------------------------------------------*
      *                  * Se al primo collegamento                    *
      *                  *---------------------------------------------*
           if        upr-dpv-upr          <    w-upr-daa
                     go to trt-upr-sel-600
           else      go to trt-upr-sel-900.
       trt-upr-sel-520.
      *                  *---------------------------------------------*
      *                  * Test se ad ogni collegamento                *
      *                  *---------------------------------------------*
           if        s-ope                =    "AP"
                     go to trt-upr-sel-600.
           if        upr-snx-pvs          not  = spaces
                     go to trt-upr-sel-900
           else      go to trt-upr-sel-600.
       trt-upr-sel-530.
      *                  *---------------------------------------------*
      *                  * Se ad ogni cambio menu o programma          *
      *                  *---------------------------------------------*
           go to     trt-upr-sel-600.
       trt-upr-sel-600.
      *              *-------------------------------------------------*
      *              * Selezione superata                              *
      *              *-------------------------------------------------*
           move      spaces               to   w-upr-flg              .
       trt-upr-sel-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     trt-upr-sel-999.
       trt-upr-sel-999.
           exit.

      *    *===========================================================*
      *    * Determinazione esistenza promemoria per l'utente in uso   *
      *    *                                                           *
      *    * Subroutine di visualizzazione                             *
      *    *-----------------------------------------------------------*
       trt-upr-vis-000.
      *              *-------------------------------------------------*
      *              * Test se visualizzazione da effettuare           *
      *              *-------------------------------------------------*
           if        s-ope                =    "AP"
                     go to trt-upr-vis-900.
       trt-upr-vis-100.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione numero linee necessarie      *
      *                  *---------------------------------------------*
           move      11                   to   w-upr-n02              .
           move      zero                 to   w-upr-lin              .
       trt-upr-vis-200.
           subtract  1                    from w-upr-n02              .
           if        w-upr-n02            <    1
                     go to trt-upr-vis-300.
           if        upr-txt-rig
                    (w-upr-n02)           =    spaces
                     go to trt-upr-vis-200.
           move      w-upr-n02            to   w-upr-lin              .
       trt-upr-vis-300.
      *                  *---------------------------------------------*
      *                  * Test sul numero di linee determinate        *
      *                  *---------------------------------------------*
           if        w-upr-lin            =    zero
                     go to trt-upr-vis-900.
       trt-upr-vis-400.
      *                  *---------------------------------------------*
      *                  * Costruzione box                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione box vuoto               *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      w-upr-nli            to   v-lin                  .
           move      18                   to   v-pos                  .
      *
           move      w-upr-nli            to   v-lto                  .
           add       05                   to   v-lto                  .
           add       w-upr-lin            to   v-lto                  .
      *
           move      61                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Determinazione literal data da visua-   *
      *                      * lizzare                                 *
      *                      *-----------------------------------------*
           move      w-upr-dtv            to   w-det-dow-dat          .
           perform   det-dow-lit-000      thru det-dow-lit-999        .
      *                      *-----------------------------------------*
      *                      * Editing data da visualizzare            *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-upr-dtv            to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Preparazione intestazione box           *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
      *
           if        w-upr-ctx            =    01
                     move  "Promemoria di"
                                          to   w-all-str-cat (1)
           else      move  "*** Preavviso per"
                                          to   w-all-str-cat (1)      .
      *
           move      w-det-dow-lit        to   w-all-str-cat (2)      .
           move      v-edt                to   w-all-str-cat (3)      .
      *
           if        w-upr-ctx            =    01
                     move  spaces         to   w-all-str-cat (4)
           else      move  "***"          to   w-all-str-cat (4)      .
      *
           perform   all-str-csb-000      thru all-str-csb-999        .
           perform   all-str-cen-000      thru all-str-cen-999        .
      *                      *-----------------------------------------*
      *                      * Intestazione box                        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
      *
           move      w-upr-nli            to   v-lin                  .
           add       1                    to   v-lin                  .
      *
           move      20                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura intestazione box         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
      *
           move      w-upr-nli            to   v-lin                  .
           add       2                    to   v-lin                  .
      *
           move      20                   to   v-pos                  .
      *
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura inferiore box            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
      *
           move      w-upr-nli            to   v-lin                  .
           add       3                    to   v-lin                  .
           add       w-upr-lin            to   v-lin                  .
      *
           move      20                   to   v-pos                  .
      *
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Prompt nel box                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      34                   to   v-car                  .
      *
           move      w-upr-nli            to   v-lin                  .
           add       4                    to   v-lin                  .
           add       w-upr-lin            to   v-lin                  .
      *
           move      26                   to   v-pos                  .
      *
           move      "Digitare OK per presa visione [  ]"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       trt-upr-vis-500.
      *                  *---------------------------------------------*
      *                  * Visualizzazione righe annotazione           *
      *                  *---------------------------------------------*
           move      zero                 to   w-upr-n02              .
       trt-upr-vis-520.
           add       1                    to   w-upr-n02              .
           if        w-upr-n02            >    w-upr-lin
                     go to trt-upr-vis-700.
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
      *
           move      w-upr-nli            to   v-lin                  .
           add       2                    to   v-lin                  .
           add       w-upr-n02            to   v-lin                  .
      *
           move      20                   to   v-pos                  .
           move      upr-txt-rig
                    (w-upr-n02)           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       trt-upr-vis-600.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     trt-upr-vis-520.
       trt-upr-vis-700.
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       trt-upr-vis-800.
      *              *-------------------------------------------------*
      *              * Normalizzazione function key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Accettazione per presa visione                  *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
      *
           move      w-upr-nli            to   v-lin                  .
           add       4                    to   v-lin                  .
           add       w-upr-lin            to   v-lin                  .
      *
           move      57                   to   v-pos                  .
      *
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Test se 'OK'                                *
      *                  *---------------------------------------------*
           if        v-alf                not  = "OK"
                     go to trt-upr-vis-800.
       trt-upr-vis-850.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       trt-upr-vis-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     trt-upr-vis-999.
       trt-upr-vis-999.
           exit.

      *    *===========================================================*
      *    * Nuovo promemoria per l'utente                             *
      *    *-----------------------------------------------------------*
       new-upr-000.
      *              *-------------------------------------------------*
      *              * Reperimento elementi necessari                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Utente                                      *
      *                  *---------------------------------------------*
           move      z-ute                to   w-upr-ute              .
      *                  *---------------------------------------------*
      *                  * Descrizione promemoria                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-upr-des              .
      *                  *---------------------------------------------*
      *                  * Testo promemoria                            *
      *                  *---------------------------------------------*
           move      s-exp                to   w-upr-txt              .
      *                  *---------------------------------------------*
      *                  * Data promemoria                             *
      *                  *---------------------------------------------*
           move      s-dat                to   w-upr-dat              .
       new-upr-100.
      *              *-------------------------------------------------*
      *              * Start su file [upr]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione chiave                         *
      *                  *---------------------------------------------*
           move      w-upr-ute            to   upr-cod-ute            .
           move      zero                 to   upr-cod-upr            .
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           start     upr    key not less
                            upr-k01
                            invalid key
                            go to   new-upr-600.
       new-upr-200.
      *              *-------------------------------------------------*
      *              * Get-next [upr]                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           read      upr    next
                            with no lock
                            at end
                            go to   new-upr-600.
       new-upr-300.
      *              *-------------------------------------------------*
      *              * Max su [upr]                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su utente : uscita                     *
      *                  *---------------------------------------------*
           if        upr-cod-ute          not  = w-upr-ute
                     go to new-upr-600.
       new-upr-400.
      *              *-------------------------------------------------*
      *              * Sel su [upr]                                    *
      *              *-------------------------------------------------*
       new-upr-500.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     new-upr-200.
       new-upr-600.
      *              *-------------------------------------------------*
      *              * Numero promemoria reperito                      *
      *              *-------------------------------------------------*
           move      upr-cod-upr          to   w-upr-cod              .
      *              *-------------------------------------------------*
      *              * Incremento numero promemoria reperito           *
      *              *-------------------------------------------------*
           add       00100                to   w-upr-cod              .
       new-upr-700.
      *              *-------------------------------------------------*
      *              * Scrittura nuovo promemoria                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione chiave                         *
      *                  *---------------------------------------------*
           move      w-upr-ute            to   upr-cod-ute            .
           move      w-upr-cod            to   upr-cod-upr            .
      *                  *---------------------------------------------*
      *                  * Composizione dati                           *
      *                  *---------------------------------------------*
           move      w-upr-des            to   upr-des-upr            .
           move      w-upr-txt            to   upr-txt-upr            .
           move      01                   to   upr-mdr-upr            .
           move      02                   to   upr-mda-upr            .
           move      w-upr-dat            to   upr-dat-upr            .
           move      zero                 to   upr-gio-set            .
           move      zero                 to   upr-gio-rif            .
           move      zero                 to   upr-mes-rif            .
           move      01                   to   upr-ngp-upr            .
           move      02                   to   upr-mdp-upr            .
           move      02                   to   upr-drt-upr            .
           move      w-upr-dat            to   upr-dfi-upr            .
           move      zero                 to   upr-dpv-upr            .
           move      "S"                  to   upr-snx-acp            .
           move      spaces               to   upr-snx-pvs            .
           move      spaces               to   upr-snx-scf            .
           move      spaces               to   upr-alx-exp            .
      *                  *---------------------------------------------*
      *                  * Scrittura record [upr]                      *
      *                  *---------------------------------------------*
           write     upr-rec invalid key
                             go to  new-upr-900.
       new-upr-800.
      *              *-------------------------------------------------*
      *              * Valori in uscita                                *
      *              *-------------------------------------------------*
           move      w-upr-cod            to   s-num                  .
           move      w-upr-ute            to   s-ute                  .
       new-upr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     new-upr-999.
       new-upr-999.
           exit.

      *    *===========================================================*
      *    * Verifica status promemoria per l'utente                   *
      *    *-----------------------------------------------------------*
       sts-upr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   s-sts                  .
      *              *-------------------------------------------------*
      *              * Codice promemoria da chiamante                  *
      *              *-------------------------------------------------*
           move      s-num                to   w-upr-cod              .
      *              *-------------------------------------------------*
      *              * Test se presente codice promemoria              *
      *              *-------------------------------------------------*
           if        w-upr-cod            =    zero
                     go to sts-upr-900.
      *              *-------------------------------------------------*
      *              * Utente                                          *
      *              *-------------------------------------------------*
           move      z-ute                to   w-upr-ute              .
       sts-upr-100.
      *              *-------------------------------------------------*
      *              * Composizione chiave                             *
      *              *-------------------------------------------------*
           move      w-upr-ute            to   upr-cod-ute            .
           move      w-upr-cod            to   upr-cod-upr            .
      *              *-------------------------------------------------*
      *              * Cancellazione vera e propria                    *
      *              *-------------------------------------------------*
           read      upr    with no lock
                            invalid key
                            move  "#"     to   s-sts                  .
       sts-upr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     sts-upr-999.
       sts-upr-999.
           exit.

      *    *===========================================================*
      *    * Consegna alla segreteria di un percorso archivio          *
      *    *-----------------------------------------------------------*
       cns-pra-000.
      *              *-------------------------------------------------*
      *              * Percorso archivio                               *
      *              *-------------------------------------------------*
           move      s-pat                to   z-pra                  .
       cns-pra-999.
           exit.

      *    *===========================================================*
      *    * Riconsegna dalla segreteria di un percorso archivio       *
      *    *-----------------------------------------------------------*
       rcn-pra-000.
      *              *-------------------------------------------------*
      *              * Percorso archivio                               *
      *              *-------------------------------------------------*
           move      z-pra                to   s-pat                  .
       rcn-pra-999.
           exit.

      *    *===========================================================*
      *    * Consegna alla segreteria di un percorso archivio lungo    *
      *    *-----------------------------------------------------------*
       cns-prl-000.
      *              *-------------------------------------------------*
      *              * Percorso archivio                               *
      *              *-------------------------------------------------*
           move      s-alf                to   z-alf                  .
       cns-prl-999.
           exit.

      *    *===========================================================*
      *    * Riconsegna dalla segreteria di un percorso archivio lungo *
      *    *-----------------------------------------------------------*
       rcn-prl-000.
      *              *-------------------------------------------------*
      *              * Percorso archivio                               *
      *              *-------------------------------------------------*
           move      z-alf                to   s-alf                  .
       rcn-prl-999.
           exit.

      *    *===========================================================*
      *    * Trattamento programma in esecuzione - effetto lancette (/)*
      *    *                                                           *
      *    * - effetto puntini ... in modalita' grafica con box (NO)   *
      *    * - effetto lancette (/) in modalita' testo                 *
      *    *-----------------------------------------------------------*
       pro-ese-000.
      *              *-------------------------------------------------*
      *              * Incremento contatori                            *
      *              *-------------------------------------------------*
           add       1                    to   z-pew                  .
           if        z-pew                >    99998
                     move  zero           to   z-pew                  .
           move      z-pew                to   z-pey                  .
           if        z-pey                not  = zero
                     go to pro-ese-999.
      *
           add       1                    to   z-pec                  .
           if        z-pec                >    3
                     move  zero           to   z-pec                  .
       pro-ese-100.
      *              *-------------------------------------------------*
      *              * Box grafico eventuale                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * ATTUALMENTE INIBITO                         *
      *                  *---------------------------------------------*
______     go to     pro-ese-200.
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-psg-snx-box        not  = "S"
                     go to pro-ese-200.
      *                  *---------------------------------------------*
      *                  * Box grafico                                 *
      *                  *---------------------------------------------*
           move      "BG"                 to   v-ope                  .
           move      12                   to   v-lin                  .
           move      38                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      43                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Effetto movimento                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      39                   to   v-pos                  .
      *
           if        z-pec                =    zero
                     move  ".   "         to   v-alf
           else if   z-pec                =    1
                     move  " .  "         to   v-alf
           else if   z-pec                =    2
                     move  "  . "         to   v-alf
           else if   z-pec                =    3
                     move  "   ."         to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pro-ese-999.
       pro-ese-200.
      *              *-------------------------------------------------*
      *              * Modalita' testo                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      78                   to   v-pos                  .
      *
           if        z-pec                =    zero
                     move  "(|)"          to   v-alf
           else if   z-pec                =    1
                     move  "(/)"          to   v-alf
           else if   z-pec                =    2
                     move  "(-)"          to   v-alf
           else if   z-pec                =    3
                     move  "(\)"          to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     pro-ese-999.
       pro-ese-999.
           exit.

      *    *===========================================================*
      *    * Trattamento programma in esecuzione - cancellazione       *
      *    *-----------------------------------------------------------*
       pro-can-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      24                   to   v-lin                  .
           move      78                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pro-can-999.
           exit.

      *    *===========================================================*
      *    * Memorizzazione eventuale modalita' grafica                *
      *    *-----------------------------------------------------------*
       mem-mdg-000.
      *              *-------------------------------------------------*
      *              * Comunicazione dell'indicatore di modalita' gra- *
      *              * fica attivo al modulo 'mvideo'                  *
      *              *-------------------------------------------------*
           move      "MG"                 to   v-ope                  .
      *
           if        w-psg-snx-box        =    "S"
                     move  1              to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       mem-mdg-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

