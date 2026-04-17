       Identification Division.
       Program-Id.                                 pazi000d           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    azi                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/09/90    *
      *                       Ultima revisione:    NdK del 21/04/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Sottoprogramma di utilita' richiamabile da  *
      *                    tutti gli altri programmi per la gestione   *
      *                    delle informazioni relative alle dipenden-  *
      *                    ze dell'azienda.                            *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Operazioni :                                                   *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "R0" : Preparazione per determinazione dipendenze esistenti    *
      *        per l'azienda, e per scelta codice dipendenza multi-    *
      *        plo, tipo richiesta                                     *
      *                                                                *
      *        Input  : w-dpz-tip-ope = "R0"                           *
      *                                                                *
      *                 w-dpz-snx-zer = Tipo provenienza               *
      *                                   R : Richiesta operatore      *
      *                                   + : Forzatura dei soli co-   *
      *                                       dici dipendenza passa-   *
      *                                       ti con i successivi ri-  *
      *                                       chiami della funzione    *
      *                                       'R5'.                    *
      *                                   - : Forzatura di tutti i co- *
      *                                       dici dipendenza ad e-    *
      *                                       sclusione di quelli pas- *
      *                                       sati con i successivi    *
      *                                       richiami della funzione  *
      *                                       'R5'.                    *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *                                                                *
      *        Nota   : I programmi che utilizzano questa funzione     *
      *                 devono omettere l'istruzione CANCEL che di     *
      *                 norma segue immediatamente il richiamo         *
      *                 della funzione "DA"                            *
      *                                                                *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "R5" : Preparazione per determinazione dipendenze esistenti    *
      *        per l'azienda, e per scelta codice dipendenza multi-    *
      *        plo, codice dipendenza                                  *
      *                                                                *
      *        Input  : w-dpz-tip-ope = "R5"                           *
      *                                                                *
      *                 w-dpz-cod-dpz = Codice dipendenza da include-  *
      *                                 re o escludere                 *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *                                                                *
      *        Nota   : I programmi che utilizzano questa funzione     *
      *                 devono omettere l'istruzione CANCEL che di     *
      *                 norma segue immediatamente il richiamo         *
      *                 della funzione "DA"                            *
      *                                                                *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "DA" : Determinazione dipendenze esistenti per l'azienda       *
      *                                                                *
      *        Input  : w-dpz-tip-ope = "DA"                           *
      *                                                                *
      *                 w-dpz-ide-ter = Codice terminale               *
      *                                                                *
      *                 w-dpz-ide-ute = Codice utente                  *
      *                                                                *
      *                 w-dpz-ide-azi = Codice azienda                 *
      *                                                                *
      *                 w-dpz-ide-sap = Sistema applicativo            *
      *                                                                *
      *                 w-dpz-ide-arg = Area gestionale                *
      *                                                                *
      *                 w-dpz-ide-set = Settore gestionale             *
      *                                                                *
      *                 w-dpz-ide-fas = Fase gestionale                *
      *                                                                *
      *                 w-dpz-ide-des = Descrizione della fase         *
      *                                                                *
      *                 w-dpz-ide-pro = Sigla interna del programma    *
      *                                                                *
      *                                                                *
      *        Output : w-dpz-ctr-dpz = Numero dipendenze esistenti    *
      *                                                                *
      *                 w-dpz-uni-cod = Codice dell'unica dipendenza   *
      *                                 se il numero dipendenze e' pa- *
      *                                 ri a 1                         *
      *                                                                *
      *                 w-dpz-uni-den = Denominazione dell'unica di-   *
      *                                 pendenza se il numero dipen-   *
      *                                 denze e' pari a 1              *
      *                                                                *
      *                 w-dpz-tbl-dpz = Tabella dipendenze esistenti   *
      *                                                                *
      *                     w-dpz-ele-dpz = Elementi della tabella di- *
      *                                     pendenze                   *
      *                                                                *
      *                         w-dpz-ele-flg = Flag di esistenza di-  *
      *                                         pendenza               *
      *                                           - Spaces : No        *
      *                                           - #      : Si        *
      *                                                                *
      *                         w-dpz-ele-den = Denominazione dipen-   *
      *                                         denza                  *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "EN" : Emissione del messaggio di errore relativo al fatto che *
      *        non c'e' alcun codice dipendenza memorizzato per l'a-   *
      *        zienda e pertanto il programma non e' eseguibile        *
      *                                                                *
      *        Input  : w-dpz-tip-ope = "EN"                           *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "FS" : Find su tabella dipendenze con selezione                *
      *                                                                *
      *        Input  : w-dpz-tip-ope = "FS"                           *
      *                                                                *
      *                                                                *
      *        Output : w-dpz-sel-dpz = Flag di selezione avvenuta     *
      *                                   - Spaces : si                *
      *                                   - #      : no                *
      *                                                                *
      *                 w-dpz-cod-dpz = Codice dipendenza selezionata  *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "LE" : Lettura elemento da tabella dipendenze precedentemente  *
      *        caricata per mezzo dell'operazione di 'Determinazione   *
      *        dipendenze esistenti per l'azienda'.                    *
      *                                                                *
      *        Input  : w-dpz-tip-ope = "LE"                           *
      *                                                                *
      *                 w-dpz-cod-dpz = Codice dipendenza da leggere   *
      *                                                                *
      *                                                                *
      *        Output : w-dpz-let-dpz = Flag di elemento trovato       *
      *                                   - Spaces : si                *
      *                                   - #      : no                *
      *                                                                *
      *                 w-dpz-den-dpz = Denominazione dipendenza       *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "LT" : Lettura elemento da tabella dipendenze senza che neces- *
      *        sariamente sia stata in precedenza eseguita l'operazio- *
      *        ne di 'Determinazione dipendenze esistenti per l'azien- *
      *        da'.                                                    *
      *                                                                *
      *        Input  : w-dpz-tip-ope = "LT"                           *
      *                                                                *
      *                 w-dpz-cod-dpz = Codice dipendenza da leggere   *
      *                                                                *
      *                                                                *
      *        Output : w-dpz-let-dpz = Flag di elemento trovato       *
      *                                   - Spaces : si                *
      *                                   - #      : no                *
      *                                                                *
      *                 w-dpz-den-dpz = Denominazione dipendenza       *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "SD" : Scelta del codice dipendenza su cui il programma chia-  *
      *        mante deve lavorare, previa esecuzione dell'operazione  *
      *        di 'Determinazione dipendenze esistenti per l'azienda'. *
      *                                                                *
      *        Nota : Se il modulo di segreteria e' a conoscenza del   *
      *               codice dipendenza in uso, viene ritornato il     *
      *               valore per il codice dipendenza in suo, senza    *
      *               richiesta all'operatore.                         *
      *                                                                *
      *        Nota : Questa opzione consente di scegliere un solo     *
      *               codice dipendenza, oppure zero per selezione     *
      *               non effettuata.                                  *
      *                                                                *
      *        Input  : w-dpz-tip-ope = "SD"                           *
      *                                                                *
      *                 w-dpz-cod-prg = Codice dipendenza per l'inte-  *
      *                                 ro programma precedentemente   *
      *                                 in uso                         *
      *                                                                *
      *                 w-dpz-cod-dpz = Codice dipendenza da proporre  *
      *                                 come default                   *
      *                                                                *
      *                                                                *
      *        Output : w-dpz-cod-prg = Codice dipendenza per l'intero *
      *                                 programma; se zero significa   *
      *                                 che non e' stato scelto nes-   *
      *                                 sun codice dipendenza          *
      *                                                                *
      *                 w-dpz-den-prg = Denominazione dipendenza per   *
      *                                 l'intero programma             *
      *                                                                *
      *                 w-dpz-ctr-dpz = Numero dipendenze esistenti    *
      *                                 per l'azienda : inalterato     *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "FD" : Scelta del codice dipendenza forzatamente da parte del- *
      *        l'operatore.                                            *
      *                                                                *
      *        Nota : Vale tutto quanto e' stato descritto per "SD",   *
      *               ad eccezione del fatto che non si considera il   *
      *               codice dipendenza in uso dalla segreteria.       *
      *                                                                *
      *        Nota : Questa opzione consente di scegliere un solo     *
      *               codice dipendenza, oppure zero per selezione     *
      *               non effettuata.                                  *
      *                                                                *
      *        Input  : w-dpz-tip-ope = "FD"                           *
      *                                                                *
      *                 w-dpz-cod-prg = Codice dipendenza per l'inte-  *
      *                                 ro programma precedentemente   *
      *                                 in uso                         *
      *                                                                *
      *                 w-dpz-cod-dpz = Codice dipendenza da proporre  *
      *                                 come default                   *
      *                                                                *
      *                                                                *
      *        Output : w-dpz-cod-prg = Codice dipendenza per l'intero *
      *                                 programma; se zero significa   *
      *                                 che non e' stato scelto nes-   *
      *                                 sun codice dipendenza          *
      *                                                                *
      *                 w-dpz-den-prg = Denominazione dipendenza per   *
      *                                 l'intero programma             *
      *                                                                *
      *                 w-dpz-ctr-dpz = Numero dipendenze esistenti    *
      *                                 per l'azienda : inalterato     *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "S+" : Scelta del codice dipendenza su cui il programma chia-  *
      *        mante deve lavorare, previa esecuzione dell'operazione  *
      *        di 'Determinazione dipendenze esistenti per l'azienda'. *
      *                                                                *
      *        Nota : Questa opzione consente di scegliere un solo     *
      *               codice dipendenza, oppure zero per selezione     *
      *               multipla, di tutte le dipendenze oppure di u-    *
      *               na parte di esse. In caso di selezione multi-    *
      *               pla le dipendenze non selezionate saranno con-   *
      *               trassegnate come dipendenze non esistenti nel-   *
      *               la tabella passata in link 'w-dpz-tbl-dpz' al    *
      *               relativo elemento 'w-dpz-ele-flg'.               *
      *                                                                *
      *        Input  : w-dpz-tip-ope = "S+"                           *
      *                                                                *
      *                 w-dpz-cod-prg = Codice dipendenza per l'inte-  *
      *                                 ro programma precedentemente   *
      *                                 in uso                         *
      *                                                                *
      *                 w-dpz-cod-dpz = Codice dipendenza da proporre  *
      *                                 come default                   *
      *                                                                *
      *                                                                *
      *        Output : w-dpz-cod-prg = Codice dipendenza per l'intero *
      *                                 programma.                     *
      *                                 Se zero significa che e' stata *
      *                                 effettuata una selezione mul-  *
      *                                 tipla oppure che la selezione  *
      *                                 non e' stata effettuata. Cio'  *
      *                                 puo' essere desunto dal campo  *
      *                                 w-dpz-ctr-dpz che indica quan- *
      *                                 te sono le dipendenze esisten- *
      *                                 ti per l'azienda. Se vale zero *
      *                                 significa che e' non e' stata  *
      *                                 effettuata alcuna selezione.   *
      *                                                                *
      *                 w-dpz-den-prg = Denominazione dipendenza per   *
      *                                 l'intero programma.            *
      *                                 In caso di selezione multipla  *
      *                                 viene ritornato con la descri- *
      *                                 zione valida per il gruppo di  *
      *                                 dipendenze                     *
      *                                                                *
      *                 w-dpz-ctr-dpz = Numero dipendenze esistenti    *
      *                                 per l'azienda                  *
      *                                 - Inalterato se scelto un so-  *
      *                                   lo codice dipendenza         *
      *                                 - Inalterato se effettuata u-  *
      *                                   na selezione multipla        *
      *                                 - Posto a zero se selezione    *
      *                                   non effettuata               *
      *                                                                *
      *                 w-dpz-tbl-dpz = Tabella dipendenze esistenti   *
      *                                 per l'azienda                  *
      *                                 - Inalterata se scelto un so-  *
      *                                   lo codice dipendenza         *
      *                                 - Inalterata se selezione non  *
      *                                   effettuata                   *
      *                                 - Con i flags di esistenza a   *
      *                                   spaces per le dipendenze     *
      *                                   non selezionate in caso di   *
      *                                   selezione multipla           *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "TR" : Controllo di abilitazione riservatezza su codice dipen- *
      *        denza                                                   *
      *                                                                *
      *        Input  : w-dpz-tip-ope = "TR"                           *
      *                                                                *
      *                 w-dpz-cod-dpz = Codice dipendenza su cui ese-  *
      *                                 guire il test di abilitazione; *
      *                                 se zero deve esserci abilita-  *
      *                                 zione per tutte le dipendenze  *
      *                                                                *
      *                                                                *
      *        Output : w-dpz-rsv-dpz = Flag di controllo superato     *
      *                                   - Spaces : si                *
      *                                   - #      : no                *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "TS" : Test sullo status della dipendenza                      *
      *                                                                *
      *        Input  : w-dpz-tip-ope = "TS"                           *
      *                                                                *
      *                 w-dpz-cod-dpz = Codice dipendenza su cui ese-  *
      *                                 guire il test di status        *
      *                                                                *
      *                                                                *
      *        Output : w-dpz-let-dpz = Flag di elemento trovato, in   *
      *                                 base allo status               *
      *                                   - Spaces : si, attiva        *
      *                                   - #      : no, inattiva      *
      *                                                                *
      *        ------------------------------------------------------- *
      *                                                                *
      * "ER" : Emissione del messaggio di errore relativo al fatto che *
      *        manca l'abilitazione sulla dipendenza w-dpz-cod-dpz, o  *
      *        su tutte le dipendenze se w-dpz-cod-dpz vale zero       *
      *                                                                *
      *        Input  : w-dpz-tip-ope = "ER"                           *
      *                                                                *
      *                 w-dpz-cod-dpz = Codice dipendenza interessato  *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
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

       Special-Names.       Decimal-Point is comma .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .

      *    *===========================================================*
      *    * Work-area per le personalizzazioni                        *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazione per tipo di accettazione per il co- *
      *        * dice della dipendenza                                 *
      *        *-------------------------------------------------------*
           05  w-prs-acc-dpz.
      *            *---------------------------------------------------*
      *            * Tipo di accettazione codice dipendenza            *
      *            *---------------------------------------------------*
               10  w-prs-acc-dpz-tac      pic  x(01)     value spaces .
      *        *-------------------------------------------------------*
      *        * Personalizzazione per gestione dipendenze parallele   *
      *        *-------------------------------------------------------*
           05  w-prs-snx-gdp.
      *            *---------------------------------------------------*
      *            * Si/No gestione dipendenze parallele               *
      *            *---------------------------------------------------*
               10  w-prs-snx-gdp-snx      pic  x(01)     value spaces .

      *    *===========================================================*
      *    * Work per preparazione determinazione dipendenze esistenti *
      *    * e scelta codice dipendenza multiplo                       *
      *    *-----------------------------------------------------------*
       01  w-r05.
      *        *-------------------------------------------------------*
      *        * Tipo di provenienza dei codici dipendenza             *
      *        *-------------------------------------------------------*
           05  w-r05-pvz                  pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella codici dipendenza          *
      *        *-------------------------------------------------------*
           05  w-r05-nel                  pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tabella codici dipendenza                             *
      *        *-------------------------------------------------------*
           05  w-r05-cod       occurs 20  pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Contatori ed indici di comodo                         *
      *        *-------------------------------------------------------*
           05  w-r05-c00                  pic  9(03)                  .
           05  w-r05-i01                  pic  9(03)                  .
           05  w-r05-i02                  pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per codice e denominazione dipendenza          *
      *        *-------------------------------------------------------*
           05  w-r05-xco                  pic  9(02)                  .
           05  w-r05-xde                  pic  x(20)                  .

      *    *===========================================================*
      *    * Work per determinazioni                                   *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per determinazione se dipendenza abilitata oppure no  *
      *        *-------------------------------------------------------*
           05  w-det-abl-dpz.
      *            *---------------------------------------------------*
      *            * Numero dipendenze con abilitazione                *
      *            *---------------------------------------------------*
               10  w-det-abl-dpz-num      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tabella dipendenze con abilitazione               *
      *            *---------------------------------------------------*
               10  w-det-abl-dpz-tbl.
                   15  w-det-abl-dpz-cod occurs 20
                                          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Contatori ed indici di comodo                     *
      *            *---------------------------------------------------*
               10  w-det-abl-dpz-c01      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area generica                                        *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Indici                                                *
      *        *-------------------------------------------------------*
           05  w-wrk-inx-dpz              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Contatori                                             *
      *        *-------------------------------------------------------*
           05  w-wrk-ctr-dpz              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per parametri per funzioni video               *
      *        *-------------------------------------------------------*
           05  w-wrk-lin-dsp              pic  9(02)                  .
           05  w-wrk-pos-dsp              pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per le accettazioni                             *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Per accettazione codice dipendenza in selezione codi- *
      *        * ce dipendenza, sia singola che multipla               *
      *        *-------------------------------------------------------*
           05  w-acc-sel-dpz.
      *            *---------------------------------------------------*
      *            * Codice dipendenza impostato                       *
      *            *---------------------------------------------------*
               10  w-acc-sel-dpz-cod      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Function-key di terminazione                      *
      *            *---------------------------------------------------*
               10  w-acc-sel-dpz-key      pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Comodo per composizione stringa con descrizione   *
      *            * dipendenza concatenata                            *
      *            *---------------------------------------------------*
               10  w-acc-sel-dpz-ddd      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Contatore di comodo 1                             *
      *            *---------------------------------------------------*
               10  w-acc-sel-dpz-c01      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Contatore di comodo 2                             *
      *            *---------------------------------------------------*
               10  w-acc-sel-dpz-c02      pic  9(03)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di interfaccia per sottoprogramma         "pazi000d" *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/pazi000d.pgl"                   .

      ******************************************************************
       Procedure Division                using w-dpz                  .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-dpz-sts-exi          .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione per tipo di accetta-  *
      *              * zione per il codice della dipendenza, se neces- *
      *              * sario                                           *
      *              *-------------------------------------------------*
           perform   prs-acc-dpz-000      thru prs-acc-dpz-999        .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione per Si/No gestione    *
      *              * dipendenze parallele, se necessario             *
      *              *-------------------------------------------------*
           perform   prs-snx-gdp-000      thru prs-snx-gdp-999        .
      *              *-------------------------------------------------*
      *              * Richiesta numero e tabella dipendenze abilitate *
      *              * alla segreteria                                 *
      *              *-------------------------------------------------*
           perform   ric-dpz-abl-000      thru ric-dpz-abl-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione per determinazione dipendenze  *
      *                  * e per scelta codice dipendenza multipla  :  *
      *                  * tipo richiesta                              *
      *                  *---------------------------------------------*
           if        w-dpz-tip-ope        =    "R0"
                     perform   pre-dsm-rx0-000
                                          thru pre-dsm-rx0-999
      *                  *---------------------------------------------*
      *                  * Preparazione per determinazione dipendenze  *
      *                  * e per scelta codice dipendenza multipla :   *
      *                  * codice dipendenza                           *
      *                  *---------------------------------------------*
           else if   w-dpz-tip-ope        =    "R5"
                     perform   pre-dsm-rx5-000
                                          thru pre-dsm-rx5-999
      *                  *---------------------------------------------*
      *                  * Determinazione dipendenze esistenti per l'- *
      *                  * azienda                                     *
      *                  *---------------------------------------------*
           else if   w-dpz-tip-ope        =    "DA"
                     perform   det-dpz-azi-000
                                          thru det-dpz-azi-999
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore per nessun    *
      *                  * codice dipendenza memorizzato per l'azienda *
      *                  *---------------------------------------------*
            else if  w-dpz-tip-ope        =    "EN"
                     perform   err-nes-dpz-000
                                          thru err-nes-dpz-999
      *                  *---------------------------------------------*
      *                  * Find su tabella dipendenze con selezione    *
      *                  *---------------------------------------------*
            else if  w-dpz-tip-ope        =    "FS"
                     perform   fnd-sel-dpz-000
                                          thru fnd-sel-dpz-999
      *                  *---------------------------------------------*
      *                  * Lettura da tabella dipendenze precedente-   *
      *                  * mente caricata                              *
      *                  *---------------------------------------------*
            else if  w-dpz-tip-ope        =    "LE"
                     perform   let-ele-dpz-000
                                          thru let-ele-dpz-999
      *                  *---------------------------------------------*
      *                  * Lettura da archivio dipendenze senza prece- *
      *                  * dente caricamento della tabella in memoria  *
      *                  *---------------------------------------------*
            else if  w-dpz-tip-ope        =    "LT"
                     perform   let-tab-dpz-000
                                          thru let-tab-dpz-999
      *                  *---------------------------------------------*
      *                  * Scelta del codice dipendenza su cui il pro- *
      *                  * gramma chiamante deve lavorare, con richie- *
      *                  * sta all'operatore                           *
      *                  *---------------------------------------------*
            else if  w-dpz-tip-ope        =    "FD"
                     perform   sce-cod-dpz-000
                                          thru sce-cod-dpz-999
                     perform   dic-cod-dpz-000
                                          thru dic-cod-dpz-999
      *                  *---------------------------------------------*
      *                  * Scelta del codice dipendenza su cui il pro- *
      *                  * gramma chiamante deve lavorare, con richie- *
      *                  * sta all'operatore solamente se la segrete-  *
      *                  * ria non conosce il codice dipendenza at-    *
      *                  * tualmente in uso                            *
      *                  *---------------------------------------------*
            else if  w-dpz-tip-ope        =    "SD"
                     perform   ric-cod-dpz-000
                                          thru ric-cod-dpz-999
                     perform   dic-cod-dpz-000
                                          thru dic-cod-dpz-999
      *                  *---------------------------------------------*
      *                  * Scelta multipla codici dipendenza           *
      *                  *---------------------------------------------*
            else if  w-dpz-tip-ope        =    "S+"
                     perform   smu-cod-dpz-000
                                          thru smu-cod-dpz-999
                     perform   dic-cod-dpz-000
                                          thru dic-cod-dpz-999
      *                  *---------------------------------------------*
      *                  * Controllo di abilitazione su codice dipen-  *
      *                  * denza                                       *
      *                  *---------------------------------------------*
            else if  w-dpz-tip-ope        =    "TR"
                     perform   tst-abl-rsv-000
                                          thru tst-abl-rsv-999
      *                  *---------------------------------------------*
      *                  * Controllo status su codice dipendenza       *
      *                  *---------------------------------------------*
            else if  w-dpz-tip-ope        =    "TS"
                     perform   tst-sta-tus-000
                                          thru tst-sta-tus-999        .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Preparazione per determinazione dipendenze, e per scelta  *
      *    * dipendenza multipla : tipo richiesta                      *
      *    *-----------------------------------------------------------*
       pre-dsm-rx0-000.
      *              *-------------------------------------------------*
      *              * Tipo provenienza                                *
      *              *-------------------------------------------------*
           if        w-dpz-snx-zer        =    "R" or
                     w-dpz-snx-zer        =    "+" or
                     w-dpz-snx-zer        =    "-"
                     move  w-dpz-snx-zer  to   w-r05-pvz
           else      move  spaces         to   w-r05-pvz              .
      *              *-------------------------------------------------*
      *              * Numero codici dipendenza : a zero               *
      *              *-------------------------------------------------*
           move      zero                 to   w-r05-nel              .
       pre-dsm-rx0-999.
           exit.

      *    *===========================================================*
      *    * Preparazione per determinazione dipendenze, e per scelta  *
      *    * dipendenza multipla : codice dipendenza                   *
      *    *-----------------------------------------------------------*
       pre-dsm-rx5-000.
      *              *-------------------------------------------------*
      *              * Se codice passato a zero : lo si ignora         *
      *              *-------------------------------------------------*
           if        w-dpz-cod-dpz        =    zero
                     go to pre-dsm-rx5-999.
      *              *-------------------------------------------------*
      *              * Se tabella gia' satura : si ignora              *
      *              *-------------------------------------------------*
           if        w-r05-nel            not  < 20
                     go to pre-dsm-rx5-999.
      *              *-------------------------------------------------*
      *              * Incremento numero elementi in tabella           *
      *              *-------------------------------------------------*
           add       1                    to   w-r05-nel              .
      *              *-------------------------------------------------*
      *              * Memorizzazione codice dipendenza passato        *
      *              *-------------------------------------------------*
           move      w-dpz-cod-dpz        to   w-r05-cod
                                              (w-r05-nel)             .
       pre-dsm-rx5-999.
           exit.

      *    *===========================================================*
      *    * Determinazione dipendenze esistenti per l'azienda         *
      *    *-----------------------------------------------------------*
       det-dpz-azi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero dipendenze esistenti                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-dpz-ctr-dpz          .
      *                  *---------------------------------------------*
      *                  * Tabella dipendenze                          *
      *                  *---------------------------------------------*
           move      spaces               to   w-dpz-tbl-dpz          .
      *                  *---------------------------------------------*
      *                  * Unico codice dipendenza                     *
      *                  *---------------------------------------------*
           move      zero                 to   w-dpz-uni-cod          .
      *                  *---------------------------------------------*
      *                  * Unica denominazione dipendenza              *
      *                  *---------------------------------------------*
           move      spaces              to   w-dpz-uni-den           .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza per l'intero programma    *
      *                  *---------------------------------------------*
           move      zero                 to   w-dpz-cod-prg          .
      *                  *---------------------------------------------*
      *                  * Denominazione dipendenza per l'intero pro-  *
      *                  * gramma                                      *
      *                  *---------------------------------------------*
           move      spaces              to   w-dpz-den-prg           .
       det-dpz-azi-100.
      *              *-------------------------------------------------*
      *              * Caricamento dipendenze esistenti in tabella     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Apertura tabella dipendenze                 *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Start su tabella dipendenze                 *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODDPZ    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      zero                 to   rf-ada-cod-dpz         .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
           if        f-sts                not  = e-not-err
                     go to det-dpz-azi-900.
       det-dpz-azi-200.
      *                  *---------------------------------------------*
      *                  * Next su tabella dipendenze                  *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
           if        f-sts                not  = e-not-err
                     go to det-dpz-azi-900.
       det-dpz-azi-300.
      *                  *---------------------------------------------*
      *                  * Selezione su codice dipendenza              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su stato dipendenza                *
      *                      *-----------------------------------------*
           if        rf-ada-sta-tus       not  = spaces
                     go to det-dpz-azi-200.
      *                      *-----------------------------------------*
      *                      * Skip dell'anagrafica Azienda            *
      *                      *-----------------------------------------*
           if        rf-ada-cod-dpz       =    zero
                     go to det-dpz-azi-200.
       det-dpz-azi-350.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda se e' stata ese-   *
      *                      * guita oppure no la preparazione R0-R5   *
      *                      * per la determinazione dei codici dipen- *
      *                      * denza esistenti e per la selezione mul- *
      *                      * tipla, e a seconda di come e' stata e-  *
      *                      * seguita                                 *
      *                      *-----------------------------------------*
           if        w-r05-pvz            =    "+" or
                     w-r05-pvz            =    "-"
                     go to det-dpz-azi-400
           else      go to det-dpz-azi-600.
       det-dpz-azi-400.
      *                      *-----------------------------------------*
      *                      * Se la preparazione R0-R5 e' stata ese-  *
      *                      * guita tipo provenienza '+' oppure '-'   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si memorizza il codice dipendenza   *
      *                          * comunque, escludendo il test sulla  *
      *                          * eventuale limitazione delle dipen-  *
      *                          * denze, in quanto la forzatura pre-  *
      *                          * vale su di esso                     *
      *                          *-------------------------------------*
           go to     det-dpz-azi-700.
       det-dpz-azi-600.
      *                      *-----------------------------------------*
      *                      * Se la preparazione R0-R5 non e' stata   *
      *                      * eseguita, oppure e' stata eseguita con  *
      *                      * tipo provenienza 'R'                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se esiste un limite sulle dipenden- *
      *                          * ze abilitate si controlla che la    *
      *                          * dipendenza appartenga alla tabella  *
      *                          * delle dipendenze abilitate          *
      *                          *-------------------------------------*
           if        w-det-abl-dpz-num    =    zero
                     go to det-dpz-azi-700.
       det-dpz-azi-610.
           move      zero                 to   w-det-abl-dpz-c01      .
       det-dpz-azi-620.
           add       1                    to   w-det-abl-dpz-c01      .
           if        w-det-abl-dpz-c01    >    20
                     go to det-dpz-azi-200.
           if        w-det-abl-dpz-c01    >    w-det-abl-dpz-num
                     go to det-dpz-azi-200.
           if        rf-ada-cod-dpz       =    w-det-abl-dpz-cod
                                              (w-det-abl-dpz-c01)
                     go to det-dpz-azi-700.
           go to     det-dpz-azi-620.
       det-dpz-azi-700.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione dipendenza                  *
      *                  *---------------------------------------------*
           if        rf-ada-cod-dpz       <    01 or
                     rf-ada-cod-dpz       >    99
                     go to det-dpz-azi-800.
           move      rf-ada-cod-dpz       to   w-wrk-inx-dpz          .
           if        w-dpz-ele-flg
                    (w-wrk-inx-dpz)       not  = spaces
                     go to det-dpz-azi-800.
           add       1                    to   w-dpz-ctr-dpz          .
           move      "#"                  to   w-dpz-ele-flg
                                              (w-wrk-inx-dpz)         .
           move      rf-ada-cod-mne       to   w-dpz-ele-den
                                              (w-wrk-inx-dpz)         .
      *                  *---------------------------------------------*
      *                  * Unico codice e denominazione dipendenza     *
      *                  *---------------------------------------------*
           if        w-dpz-uni-cod        =    zero
                     move  rf-ada-cod-dpz to   w-dpz-uni-cod
                     move  rf-ada-cod-mne to   w-dpz-uni-den          .
       det-dpz-azi-800.
      *                  *---------------------------------------------*
      *                  * Riciclo su dipendenza successiva            *
      *                  *---------------------------------------------*
           go to     det-dpz-azi-200.
       det-dpz-azi-900.
      *                  *---------------------------------------------*
      *                  * Chiusura tabella dipendenze                 *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
       det-dpz-azi-999.
           exit.

      *    *===========================================================*
      *    * Emissione del messaggio di errore relativo al fatto che   *
      *    * non c'e' alcun codice dipendenza memorizzato per l'azien- *
      *    * da, e pertanto il programma non e' eseguibile             *
      *    *-----------------------------------------------------------*
       err-nes-dpz-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Visualizzazione titolo programma chiamante      *
      *              *-------------------------------------------------*
           perform   tit-pgm-chi-000      thru tit-pgm-chi-999        .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      16                   to   v-lto                  .
           move      78                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Note interne al box                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      61                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      "Non esiste alcun codice dipendenza memorizzato per
      -              " l'azienda."        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      44                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      "Pertanto questo programma non e' eseguibile."
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      32                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      36                   to   v-pos                  .
           move      "Premere 'OK' per presa visione :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione OK di presa visione                *
      *              *-------------------------------------------------*
           move      spaces               to   v-alf                  .
       err-nes-dpz-400.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      69                   to   v-pos                  .
           move      "EXIT"               to   v-pfk (20)             .
           move      "DO  "               to   v-pfk (05)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "EXIT" or
                     v-key                =    "DO  "
                     go to err-nes-dpz-800.
           if        v-alf                not  = "OK"
                     go to err-nes-dpz-400.
       err-nes-dpz-800.
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       err-nes-dpz-999.
           exit.

      *    *===========================================================*
      *    * Find su tabella dipendenze con selezione                  *
      *    *-----------------------------------------------------------*
       fnd-sel-dpz-000.
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pazi0010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-dpz-sel-dpz
                     move  zero           to   w-dpz-cod-dpz
                     go to  fnd-sel-dpz-999.
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per possibili- *
      *              * ta' di function-key "SLCT" durante l'interroga- *
      *              * zione                                           *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "fkselect"           to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      04                   to   s-car                  .
           move      "SLCT"               to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/azi/prg/obj/pazi0010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuale variabile di i.p.c. de- *
      *              * terminata da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-dpz"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  spaces         to   w-dpz-sel-dpz
                     move  s-num          to   w-dpz-cod-dpz
           else      move  "#"            to   w-dpz-sel-dpz
                     move  zero           to   w-dpz-cod-dpz          .
       fnd-sel-dpz-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura tabella [dpz] da w-dpz-tbl-dpz. Si     *
      *    * presume che il programma chiamante abbia preventivamente  *
      *    * richiamato la funzione di "determinazione dipendenze esi- *
      *    * stenti per l'azienda" e che pertanto la tabella dipenden- *
      *    * ze w-dpz-tbl-dpz risulti carica di valori                 *
      *    *-----------------------------------------------------------*
       let-ele-dpz-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-dpz-let-dpz          .
      *              *-------------------------------------------------*
      *              * Test se codice dipendenza a zero                *
      *              *-------------------------------------------------*
           if        w-dpz-cod-dpz        =    zero
                     go to let-ele-dpz-500.
      *              *-------------------------------------------------*
      *              * Test se gestione dipendenze parallele attiva e  *
      *              * codice dipendenza pari a 50                     *
      *              *-------------------------------------------------*
           if        w-prs-snx-gdp-snx    =    "S" and
                     w-dpz-cod-dpz        =    50
                     go to let-ele-dpz-500.
      *              *-------------------------------------------------*
      *              * Estrazione da tabella in memoria                *
      *              *-------------------------------------------------*
           if        w-dpz-cod-dpz        <    01 or
                     w-dpz-cod-dpz        >    99
                     go to let-ele-dpz-400.
           if        w-dpz-ele-flg
                    (w-dpz-cod-dpz)       =    spaces
                     go to let-ele-dpz-400.
       let-ele-dpz-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      w-dpz-ele-den
                    (w-dpz-cod-dpz)       to   w-dpz-den-dpz          .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-ele-dpz-999.
       let-ele-dpz-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-dpz-let-dpz          .
           move      all   "."            to   w-dpz-den-dpz          .
           go to     let-ele-dpz-999.
       let-ele-dpz-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione dati associati                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-dpz-den-dpz          .
       let-ele-dpz-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura tabella [dpz] senza che necessariamen- *
      *    * te sia stata caricata la tabella in memoria               *
      *    *-----------------------------------------------------------*
       let-tab-dpz-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-dpz-let-dpz          .
      *              *-------------------------------------------------*
      *              * Apertura tabella dipendenze                     *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *              *-------------------------------------------------*
      *              * Lettura tabella dipendenze                      *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODDPZ    "         to   f-key                  .
           move      w-dpz-cod-dpz        to   rf-ada-cod-dpz         .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
           if        f-sts                not  = e-not-err
                     go to let-tab-dpz-400.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ada-cod-mne       to   w-dpz-den-dpz          .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-tab-dpz-900.
       let-tab-dpz-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-dpz-let-dpz          .
           move      all   "."            to   w-dpz-den-dpz          .
       let-tab-dpz-900.
      *              *-------------------------------------------------*
      *              * Chiusura tabella dipendenze                     *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
       let-tab-dpz-999.
           exit.

      *    *===========================================================*
      *    * Scelta del codice dipendenza su cui il programma chiaman- *
      *    * te deve lavorare; si presume che il programma chiamante   *
      *    * abbia preventivamente richiamato la funzione di "determi- *
      *    * nazione dipendenze esistenti per l'azienda" e che pertan- *
      *    * to la tabella dipendenze w-dpz-tbl-dpz risulti carica di  *
      *    * valori. La scelta e' comunque effettuata dall'operatore.  *
      *    *                                                           *
      *    *                                                           *
      *    * Trattamento del codice impostato                          *
      *    *                                                           *
      *    *                                                           *
      *    *   - Gestione dipendenze parallele non attiva              *
      *    *                                                           *
      *    *       - Solo selezione semplice                           *
      *    *                                                           *
      *    *              Deve essere impostato un codice 01..99, che  *
      *    *              deve trovare corrispondenza tra le dipen-    *
      *    *              denze esistenti.                             *
      *    *                                                           *
      *    *       - Selezione multipla consentita                     *
      *    *                                                           *
      *    *              Si puo' impostare un codice 01..99, che deve *
      *    *              trovare corrispondenza tra le dipendenze     *
      *    *              esistenti.                                   *
      *    *                                                           *
      *    *              Oppure si puo' impostare il codice 00, che   *
      *    *              indica la selezione di tutte le dipendenze.  *
      *    *                                                           *
      *    *                                                           *
      *    *   - Gestione dipendenze parallele attiva                  *
      *    *                                                           *
      *    *       - Solo selezione semplice                           *
      *    *                                                           *
      *    *              Deve essere impostato un codice 01..99, che  *
      *    *              deve trovare corrispondenza tra le dipen-    *
      *    *              denze esistenti. Se il valore sara' 01..49   *
      *    *              indichera' una dipendenza normale. Se il     *
      *    *              valore sara' 51..99 indichera' una dipen-    *
      *    *              denza parallela. E' da notare che il codice  *
      *    *              50 non e' ammesso.                           *
      *    *                                                           *
      *    *       - Selezione multipla consentita                     *
      *    *                                                           *
      *    *         In questo caso sara' abilitata la pressione del   *
      *    *         tasto funzione Pf1, oltre al Return.              *
      *    *                                                           *
      *    *              Il valore 00 seguito da Return indica la     *
      *    *              selezione di tutte le dipendenze normali,    *
      *    *              con valore da 01 a 49.                       *
      *    *                                                           *
      *    *              I valori 01..49 seguiti da Return indicano   *
      *    *              la selezione di una sola dipendenza norma-   *
      *    *              le, quella indicata dal valore impostato.    *
      *    *                                                           *
      *    *              Il valore 50 seguito da Return indica la     *
      *    *              selezione di tutte le dipendenze paralle-    *
      *    *              le con valore da 51 a 99.                    *
      *    *                                                           *
      *    *              I valori 51..99 seguiti da Return indicano   *
      *    *              la selezione di una sola dipendenza paral-   *
      *    *              lela, quella indicata dal valore impostato.  *
      *    *                                                           *
      *    *              Il valore 00 seguito da Pf1 indica la se-    *
      *    *              lezione di tutte le dipendenze, sia norma-   *
      *    *              li che parallele, con i valori da 01 a 49    *
      *    *              e da 51 a 99.                                *
      *    *                                                           *
      *    *              I valori 01..49 seguiti da Pf1 indicano la   *
      *    *              selezione di due dipendenze, quella norma-   *
      *    *              le, indicata dal valore impostato, e quel-   *
      *    *              la parallela, corrispondente al valore im-   *
      *    *              postato aumentato di 50.                     *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       sce-cod-dpz-000.
      *              *-------------------------------------------------*
      *              * Test se e' la prima volta oppure no che il pro- *
      *              * gramma chiamante richiede questa funzione       *
      *              *-------------------------------------------------*
           if        w-dpz-cod-prg        =    zero
                     go to sce-cod-dpz-100
           else      go to sce-cod-dpz-900.
       sce-cod-dpz-100.
      *              *-------------------------------------------------*
      *              * Se e' la prima volta che il programma chiamante *
      *              * richiede questa funzione si effettua la scelta  *
      *              * del codice dipendenza                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se c'e' un solo codice dipendenza memoriz-  *
      *                  * zato per l'azienda si forza quel codice con *
      *                  * la denominazione corrispondente             *
      *                  *---------------------------------------------*
           if        w-dpz-ctr-dpz        =    1
                     move  w-dpz-uni-cod  to   w-dpz-cod-prg
                     move  w-dpz-uni-den  to   w-dpz-den-prg
                     go to sce-cod-dpz-999.
       sce-cod-dpz-200.
      *                  *---------------------------------------------*
      *                  * Altrimenti si richiede una scelta da parte  *
      *                  * dell'operatore                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valore di default                       *
      *                      *-----------------------------------------*
           move      w-dpz-cod-dpz        to   w-dpz-cod-prg          .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione titolo programma chia-  *
      *                      * mante                                   *
      *                      *-----------------------------------------*
           perform   tit-pgm-chi-000      thru tit-pgm-chi-999        .
      *                      *-----------------------------------------*
      *                      * Literal per scelta dipendenza           *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      "Scelta codice dipendenza"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Box                                     *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      06                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      12                   to   v-lto                  .
           move      78                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Note interne al box                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      19                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      10                   to   v-pos                  .
           move      "Codice dipendenza :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Pre-visualizzazione codice dipendenza   *
      *                      * di default                              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-dpz-cod-prg        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Lettura tabella dipendenza di default   *
      *                      *-----------------------------------------*
           move      w-dpz-cod-prg        to   w-dpz-cod-dpz          .
           perform   let-ele-dpz-000      thru let-ele-dpz-999        .
           move      w-dpz-den-dpz        to   w-dpz-den-prg          .
      *                      *-----------------------------------------*
      *                      * Visualizzazione denominazione dipenden- *
      *                      * pendenza di default                     *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-dpz-den-prg        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       sce-cod-dpz-220.
      *                      *-----------------------------------------*
      *                      * Visualizzazione legenda dipendenze      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test che il numero dipendenze sia   *
      *                          * inferiore al massimo numero di di-  *
      *                          * pendenze visualizzabili             *
      *                          *-------------------------------------*
           if        w-dpz-ctr-dpz        >    12
                     go to sce-cod-dpz-280.
      *                          *-------------------------------------*
      *                          * Literal                             *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      31                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Legenda dei codici dipendenza :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       sce-cod-dpz-230.
      *                          *-------------------------------------*
      *                          * Dipendenze                          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Inizializzazione indice         *
      *                              *---------------------------------*
           move      zero                 to   w-wrk-inx-dpz          .
      *                              *---------------------------------*
      *                              * Inizializzazione contatore      *
      *                              *---------------------------------*
           move      zero                 to   w-wrk-ctr-dpz          .
       sce-cod-dpz-240.
      *                              *---------------------------------*
      *                              * Incremento indice               *
      *                              *---------------------------------*
           add       1                    to   w-wrk-inx-dpz          .
      *                              *---------------------------------*
      *                              * Se dipendenza non memorizzata   *
      *                              * si ricicla                      *
      *                              *---------------------------------*
           if        w-dpz-ele-flg
                    (w-wrk-inx-dpz)       =    spaces
                     go to sce-cod-dpz-270.
      *                              *---------------------------------*
      *                              * Se gestione dipendenze paralle- *
      *                              * le attiva, e codice dipendenza  *
      *                              * maggiore di 50 : si ricicla     *
      *                              *---------------------------------*
           if        w-prs-snx-gdp-snx    =    "S" and
                     w-wrk-inx-dpz        >    50
                     go to sce-cod-dpz-270.
      *                              *---------------------------------*
      *                              * Incremento contatore            *
      *                              *---------------------------------*
           add       1                    to   w-wrk-ctr-dpz          .
      *                              *---------------------------------*
      *                              * Determinazione linea e posizio- *
      *                              * ne base per visualizzazione     *
      *                              *---------------------------------*
           if        w-wrk-ctr-dpz        =    01
                     move  16             to   w-wrk-lin-dsp
                     move  04             to   w-wrk-pos-dsp
           else if   w-wrk-ctr-dpz        =    02
                     move  17             to   w-wrk-lin-dsp
                     move  04             to   w-wrk-pos-dsp
           else if   w-wrk-ctr-dpz        =    03
                     move  18             to   w-wrk-lin-dsp
                     move  04             to   w-wrk-pos-dsp
           else if   w-wrk-ctr-dpz        =    04
                     move  19             to   w-wrk-lin-dsp
                     move  04             to   w-wrk-pos-dsp
           else if   w-wrk-ctr-dpz        =    05
                     move  16             to   w-wrk-lin-dsp
                     move  30             to   w-wrk-pos-dsp
           else if   w-wrk-ctr-dpz        =    06
                     move  17             to   w-wrk-lin-dsp
                     move  30             to   w-wrk-pos-dsp
           else if   w-wrk-ctr-dpz        =    07
                     move  18             to   w-wrk-lin-dsp
                     move  30             to   w-wrk-pos-dsp
           else if   w-wrk-ctr-dpz        =    08
                     move  19             to   w-wrk-lin-dsp
                     move  30             to   w-wrk-pos-dsp
           else if   w-wrk-ctr-dpz        =    09
                     move  16             to   w-wrk-lin-dsp
                     move  56             to   w-wrk-pos-dsp
           else if   w-wrk-ctr-dpz        =    10
                     move  17             to   w-wrk-lin-dsp
                     move  56             to   w-wrk-pos-dsp
           else if   w-wrk-ctr-dpz        =    11
                     move  18             to   w-wrk-lin-dsp
                     move  56             to   w-wrk-pos-dsp
           else      move  19             to   w-wrk-lin-dsp
                     move  56             to   w-wrk-pos-dsp          .
      *                              *---------------------------------*
      *                              * Visualizzazione codice          *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      w-wrk-lin-dsp        to   v-lin                  .
           move      w-wrk-pos-dsp        to   v-pos                  .
           move      w-wrk-inx-dpz        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Visualizzazione denominazione   *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      w-wrk-lin-dsp        to   v-lin                  .
           move      w-wrk-pos-dsp        to   v-pos                  .
           add       3                    to   v-pos                  .
           move      w-dpz-ele-den
                    (w-wrk-inx-dpz)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       sce-cod-dpz-270.
      *                              *---------------------------------*
      *                              * Se contatore inferiore al mas-  *
      *                              * simo numero di dipendenze pos-  *
      *                              * sibili si ricicla               *
      *                              *---------------------------------*
           if        w-wrk-inx-dpz        <    99
                     go to sce-cod-dpz-240.
       sce-cod-dpz-280.
      *                      *-----------------------------------------*
      *                      * Visualizzazione delle note operative    *
      *                      * relative alla scelta per tutte le di-   *
      *                      * pendenze, solo in caso di possibile se- *
      *                      * lezione multipla.                       *
      *                      *-----------------------------------------*
           if        w-dpz-tip-ope        not  = "S+"
                     go to sce-cod-dpz-290.
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      23                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "N.B. : Scegliere il codice dipendenza zero per ott
      -              "enere tutte le dipendenze     "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       sce-cod-dpz-290.
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       sce-cod-dpz-300.
      *                      *-----------------------------------------*
      *                      * Accettazione codice dipendenza          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Parametri generici                  *
      *                          *-------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
      *                          *-------------------------------------*
      *                          * Tasti funzione                      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Find : sempre ammesso           *
      *                              *---------------------------------*
           move      "FIND"               to   v-pfk (03)             .
      *                              *---------------------------------*
      *                              * Do   : sempre ammesso           *
      *                              *---------------------------------*
           move      "DO  "               to   v-pfk (05)             .
      *                              *---------------------------------*
      *                              * Pf1  : ammesso solamente se ge- *
      *                              *        stione dipendenze paral- *
      *                              *        lele attiva e selezione  *
      *                              *        multipla                 *
      *                              *---------------------------------*
           if        w-prs-snx-gdp-snx    =    "S"  and
                     w-dpz-tip-ope        =    "S+"
                     move  "[1] "         to   v-pfk (14)             .
      *                              *---------------------------------*
      *                              * Exit : sempre ammesso           *
      *                              *---------------------------------*
           move      "EXIT"               to   v-pfk (20)             .
      *                          *-------------------------------------*
      *                          * Valore di default                   *
      *                          *-------------------------------------*
           move      w-dpz-cod-prg        to   v-num                  .
      *                          *-------------------------------------*
      *                          * Richiamo esecuzione                 *
      *                          *-------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       sce-cod-dpz-325.
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     go to sce-cod-dpz-975.
      *                      *-----------------------------------------*
      *                      * Valore impostato in destinazione        *
      *                      *-----------------------------------------*
           move      v-num                to   w-dpz-cod-prg          .
      *                      *-----------------------------------------*
      *                      * Assestamento                            *
      *                      *                                         *
      *                      * N.B.: Da perfezionare perche' riguarda  *
      *                      *       le dipendenze 'parallele'         *
      *                      *-----------------------------------------*
           if        v-num                =    51
                     move  05             to   w-dpz-cod-prg          .
           if        v-num                =    99
                     move  51             to   w-dpz-cod-prg          .
       sce-cod-dpz-350.
      *                      *-----------------------------------------*
      *                      * Se Find                                 *
      *                      *-----------------------------------------*
           if        v-key                not  = "FIND"
                     go to sce-cod-dpz-400.
      *                          *-------------------------------------*
      *                          * Find su tabella dipendenze          *
      *                          *-------------------------------------*
           perform   fnd-sel-dpz-000      thru fnd-sel-dpz-999        .
           if        w-dpz-sel-dpz        not  = spaces
                     go to sce-cod-dpz-300.
           move      w-dpz-cod-dpz        to   w-dpz-cod-prg          .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice dipendenza   *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9B"                 to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-dpz-cod-prg        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Normalizzazione function-key        *
      *                          *-------------------------------------*
           move      spaces               to   v-key                  .
       sce-cod-dpz-400.
      *                          *-------------------------------------*
      *                          * Codice dipendenza impostato in area *
      *                          * work per accettazione codice dipen- *
      *                          * denza in selezione                  *
      *                          *-------------------------------------*
           move      w-dpz-cod-prg        to   w-acc-sel-dpz-cod      .
      *                          *-------------------------------------*
      *                          * Function-key di terminazione in a-  *
      *                          * rea work per accettazione codice    *
      *                          * dipendenza in selezione             *
      *                          *-------------------------------------*
           move      v-key                to   w-acc-sel-dpz-key      .
       sce-cod-dpz-425.
      *                          *-------------------------------------*
      *                          * Lettura tabella dipendenza          *
      *                          *-------------------------------------*
           move      w-dpz-cod-prg        to   w-dpz-cod-dpz          .
           perform   let-ele-dpz-000      thru let-ele-dpz-999        .
           move      w-dpz-den-dpz        to   w-dpz-den-prg          .
       sce-cod-dpz-450.
      *                          *-------------------------------------*
      *                          * Trattamento codice dipendenza impo- *
      *                          * stato                               *
      *                          *-------------------------------------*
       sce-cod-dpz-452.
      *                              *---------------------------------*
      *                              * Deviazione a seconda se gestio- *
      *                              * ne dipendenze parallele attiva  *
      *                              * oppure no                       *
      *                              *---------------------------------*
           if        w-prs-snx-gdp-snx    =    "S"
                     go to sce-cod-dpz-490.
       sce-cod-dpz-460.
      *                              *---------------------------------*
      *                              * Se gestione dipendenze paralle- *
      *                              * le non attiva                   *
      *                              *---------------------------------*
       sce-cod-dpz-462.
      *                                  *-----------------------------*
      *                                  * Deviazione a seconda del    *
      *                                  * valore del codice imposta-  *
      *                                  * to                          *
      *                                  *-----------------------------*
           if        w-dpz-cod-prg        =    zero
                     go to sce-cod-dpz-480.
       sce-cod-dpz-470.
      *                                  *-----------------------------*
      *                                  * Se codice impostato diverso *
      *                                  * da zero                     *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Denominazione in area   *
      *                                      * di accettazione         *
      *                                      *-------------------------*
           move      w-dpz-den-prg        to   w-acc-sel-dpz-ddd      .
      *                                      *-------------------------*
      *                                      * Visualizzazione denomi- *
      *                                      * nazione dipendenza come *
      *                                      * da lettura tabella      *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-sel-dpz-ddd    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                      *-------------------------*
      *                                      * Se codice dipendenza    *
      *                                      * non esistente : a re-   *
      *                                      * impostazione            *
      *                                      *-------------------------*
           if        w-dpz-let-dpz        not  = spaces
                     go to sce-cod-dpz-300.
      *                                      *-------------------------*
      *                                      * Se terminazione con il  *
      *                                      * tasto Do : ad eventua-  *
      *                                      * le controllo di riser-  *
      *                                      * vatezza.                *
      *                                      * Altrimenti : a richie-  *
      *                                      * sta conferma.           *
      *                                      *-------------------------*
           if        v-key                =    "DO  "
                     go to sce-cod-dpz-800
           else      go to sce-cod-dpz-700.
       sce-cod-dpz-480.
      *                                  *-----------------------------*
      *                                  * Se codice impostato pari a  *
      *                                  * zero                        *
      *                                  *-----------------------------*
       sce-cod-dpz-482.
      *                                      *-------------------------*
      *                                      * Deviazione a seconda se *
      *                                      * selezione semplice o    *
      *                                      * multipla                *
      *                                      *-------------------------*
           if        w-dpz-tip-ope        =    "S+"
                     go to sce-cod-dpz-486.
       sce-cod-dpz-484.
      *                                      *-------------------------*
      *                                      * Se selezione semplice   *
      *                                      *-------------------------*
      *                                          *---------------------*
      *                                          * Denominazione in a- *
      *                                          * rea di accettazio-  *
      *                                          * ne a spaces         *
      *                                          *---------------------*
           move      spaces               to   w-acc-sel-dpz-ddd      .
      *                                          *---------------------*
      *                                          * Visualizzazione de- *
      *                                          * nominazione a spa-  *
      *                                          * ces                 *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-sel-dpz-ddd    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                          *---------------------*
      *                                          * Uscita con simula-  *
      *                                          * zione di Exit       *
      *                                          *---------------------*
           go to     sce-cod-dpz-975.
       sce-cod-dpz-486.
      *                                      *-------------------------*
      *                                      * Se selezione multipla   *
      *                                      *-------------------------*
      *                                          *---------------------*
      *                                          * Preparazione deno-  *
      *                                          * minazione in area   *
      *                                          * di uscita           *
      *                                          *---------------------*
           move      "Tutte le dipendenze "
                                          to   w-dpz-den-prg          .
      *                                          *---------------------*
      *                                          * Preparazione deno-  *
      *                                          * minazione in area   *
      *                                          * di accettazione     *
      *                                          *---------------------*
           move      w-dpz-den-prg        to   w-acc-sel-dpz-ddd      .
      *                                          *---------------------*
      *                                          * Visualizzazione de- *
      *                                          * nominazione per     *
      *                                          * tutte le dipendenze *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-sel-dpz-ddd    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                          *---------------------*
      *                                          * Se terminazione con *
      *                                          * il tasto Do : ad e- *
      *                                          * ventuale controllo  *
      *                                          * di riservatezza.    *
      *                                          * Altrimenti : a ri-  *
      *                                          * chiesta conferma.   *
      *                                          *---------------------*
           if        v-key                =    "DO  "
                     go to sce-cod-dpz-800
           else      go to sce-cod-dpz-700.
       sce-cod-dpz-490.
      *                              *---------------------------------*
      *                              * Se gestione dipendenze paralle- *
      *                              * le attiva                       *
      *                              *---------------------------------*
       sce-cod-dpz-492.
      *                                  *-----------------------------*
      *                                  * Deviazione a seconda del    *
      *                                  * valore del codice imposta-  *
      *                                  * to                          *
      *                                  *-----------------------------*
           if        w-dpz-cod-prg        =    zero
                     go to sce-cod-dpz-500
           else if   w-dpz-cod-prg        not  < 01 and
                     w-dpz-cod-prg        not  > 49
                     go to sce-cod-dpz-520
           else if   w-dpz-cod-prg        =    50
                     go to sce-cod-dpz-540
           else if   w-dpz-cod-prg        not  < 51 and
                     w-dpz-cod-prg        not  > 99
                     go to sce-cod-dpz-560
           else      go to sce-cod-dpz-300.
       sce-cod-dpz-500.
      *                                  *-----------------------------*
      *                                  * Se codice impostato pari a  *
      *                                  * zero                        *
      *                                  *-----------------------------*
       sce-cod-dpz-502.
      *                                      *-------------------------*
      *                                      * Deviazione a seconda se *
      *                                      * selezione semplice o    *
      *                                      * multipla                *
      *                                      *-------------------------*
           if        w-dpz-tip-ope        =    "S+"
                     go to sce-cod-dpz-506.
       sce-cod-dpz-504.
      *                                      *-------------------------*
      *                                      * Se selezione semplice   *
      *                                      *-------------------------*
      *                                          *---------------------*
      *                                          * Denominazione in a- *
      *                                          * rea di accettazio-  *
      *                                          * ne a spaces         *
      *                                          *---------------------*
           move      spaces               to   w-acc-sel-dpz-ddd      .
      *                                          *---------------------*
      *                                          * Visualizzazione de- *
      *                                          * nominazione a spa-  *
      *                                          * ces                 *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-sel-dpz-ddd    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                          *---------------------*
      *                                          * Uscita con simula-  *
      *                                          * zione di Exit       *
      *                                          *---------------------*
           go to     sce-cod-dpz-975.
       sce-cod-dpz-506.
      *                                      *-------------------------*
      *                                      * Se selezione multipla   *
      *                                      *-------------------------*
       sce-cod-dpz-508.
      *                                          *---------------------*
      *                                          * Deviazione a secon- *
      *                                          * da se terminazione  *
      *                                          * con Return o Pf1    *
      *                                          *---------------------*
           if        w-acc-sel-dpz-key    =    "[1] "
                     go to sce-cod-dpz-512.
       sce-cod-dpz-510.
      *                                          *---------------------*
      *                                          * Se Return o Do      *
      *                                          *---------------------*
      *                                              *-----------------*
      *                                              * Preparazione    *
      *                                              * denominazione   *
      *                                              * in area uscita  *
      *                                              *-----------------*
           move      "Tutte le normali    "
                                          to   w-dpz-den-prg          .
      *                                              *-----------------*
      *                                              * Preparazione    *
      *                                              * denominazione   *
      *                                              * in area accet-  *
      *                                              * tazione         *
      *                                              *-----------------*
           move      "Tutte le dipendenze normali             "
                                          to   w-acc-sel-dpz-ddd      .
      *                                              *-----------------*
      *                                              * Visualizzazione *
      *                                              * denominazione   *
      *                                              * per tutte le    *
      *                                              * dipendenze nor- *
      *                                              * mali            *
      *                                              *-----------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-sel-dpz-ddd    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                              *-----------------*
      *                                              * Se terminazione *
      *                                              * con tasto Do :  *
      *                                              * ad eventuale    *
      *                                              * controllo di    *
      *                                              * riservatezza.   *
      *                                              * Altrimenti : a  *
      *                                              * richiesta con-  *
      *                                              * ferma.          *
      *                                              *-----------------*
           if        v-key                =    "DO  "
                     go to sce-cod-dpz-800
           else      go to sce-cod-dpz-700.
       sce-cod-dpz-512.
      *                                          *---------------------*
      *                                          * Se Pf1              *
      *                                          *---------------------*
      *                                              *-----------------*
      *                                              * Preparazione    *
      *                                              * denominazione   *
      *                                              * in area uscita  *
      *                                              *-----------------*
           move      "Tutte le normali+   "
                                          to   w-dpz-den-prg          .
      *                                              *-----------------*
      *                                              * Preparazione    *
      *                                              * denominazione   *
      *                                              * in area accet-  *
      *                                              * tazione         *
      *                                              *-----------------*
           move      "Tutte le dipendenze, normali e parallele"
                                          to   w-acc-sel-dpz-ddd      .
      *                                              *-----------------*
      *                                              * Visualizzazione *
      *                                              * denominazione   *
      *                                              * per tutte le    *
      *                                              * dipendenze nor- *
      *                                              * mali e paralle- *
      *                                              * le              *
      *                                              *-----------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-sel-dpz-ddd    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                              *-----------------*
      *                                              * A richiesta di  *
      *                                              * conferma        *
      *                                              *-----------------*
           go to     sce-cod-dpz-700.
       sce-cod-dpz-520.
      *                                  *-----------------------------*
      *                                  * Se codice impostato compre- *
      *                                  * so tra 01 e 49              *
      *                                  *-----------------------------*
       sce-cod-dpz-522.
      *                                      *-------------------------*
      *                                      * Deviazione a seconda se *
      *                                      * selezione semplice o    *
      *                                      * multipla                *
      *                                      *-------------------------*
           if        w-dpz-tip-ope        =    "S+"
                     go to sce-cod-dpz-528.
       sce-cod-dpz-524.
      *                                      *-------------------------*
      *                                      * Se selezione semplice   *
      *                                      *-------------------------*
       sce-cod-dpz-526.
      *                                          *---------------------*
      *                                          * Denominazione in a- *
      *                                          * rea di accettazione *
      *                                          *---------------------*
           move      w-dpz-den-prg        to   w-acc-sel-dpz-ddd      .
      *                                          *---------------------*
      *                                          * Visualizzazione de- *
      *                                          * nominazione dipen-  *
      *                                          * denza come da let-  *
      *                                          * tura tabella        *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-sel-dpz-ddd    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                          *---------------------*
      *                                          * Se codice dipenden- *
      *                                          * za non esistente :  *
      *                                          * a reimpostazione    *
      *                                          *---------------------*
           if        w-dpz-let-dpz        not  = spaces
                     go to sce-cod-dpz-300.
      *                                          *---------------------*
      *                                          * Se terminazione con *
      *                                          * tasto Do : ad even- *
      *                                          * tuale controllo di  *
      *                                          * riservatezza.       *
      *                                          * Altrimenti : a ri-  *
      *                                          * chiesta conferma.   *
      *                                          *---------------------*
           if        v-key                =    "DO  "
                     go to sce-cod-dpz-800
           else      go to sce-cod-dpz-700.
       sce-cod-dpz-528.
      *                                      *-------------------------*
      *                                      * Se selezione multipla   *
      *                                      *-------------------------*
       sce-cod-dpz-530.
      *                                          *---------------------*
      *                                          * Deviazione a secon- *
      *                                          * da se terminazione  *
      *                                          * con Return o Pf1    *
      *                                          *---------------------*
           if        w-acc-sel-dpz-key    =    "[1] "
                     go to sce-cod-dpz-534.
       sce-cod-dpz-532.
      *                                          *---------------------*
      *                                          * Se Return o Do      *
      *                                          *---------------------*
      *                                              *-----------------*
      *                                              * Denominazione   *
      *                                              * in area di ac-  *
      *                                              * cettazione      *
      *                                              *-----------------*
           move      w-dpz-den-prg        to   w-acc-sel-dpz-ddd      .
      *                                              *-----------------*
      *                                              * Visualizzazione *
      *                                              * denominazione   *
      *                                              * dipendenza come *
      *                                              * da lettura ta-  *
      *                                              * bella           *
      *                                              *-----------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-sel-dpz-ddd    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                              *-----------------*
      *                                              * Se codice di-   *
      *                                              * pendenza non e- *
      *                                              * sistente : a    *
      *                                              * reimpostazione  *
      *                                              *-----------------*
           if        w-dpz-let-dpz        not  = spaces
                     go to sce-cod-dpz-300.
      *                                              *-----------------*
      *                                              * Se terminazione *
      *                                              * con tasto Do :  *
      *                                              * ad eventuale    *
      *                                              * controllo di    *
      *                                              * riservatezza.   *
      *                                              * Altreimenti : a *
      *                                              * richiesta della *
      *                                              * conferma.       *
      *                                              *-----------------*
           if        v-key                =    "DO  "
                     go to sce-cod-dpz-800
           else      go to sce-cod-dpz-700.
       sce-cod-dpz-534.
      *                                          *---------------------*
      *                                          * Se Pf1              *
      *                                          *---------------------*
       sce-cod-dpz-535.
      *                                              *-----------------*
      *                                              * Composizione    *
      *                                              * denominazione   *
      *                                              * dipendenza per  *
      *                                              * area accetta-   *
      *                                              * zione           *
      *                                              *-----------------*
           move      w-dpz-den-prg        to   w-acc-sel-dpz-ddd      .
           if        w-dpz-let-dpz        not  = spaces
                     go to sce-cod-dpz-536.
           move      zero                 to   w-acc-sel-dpz-c01      .
           inspect   w-acc-sel-dpz-ddd
                                      tallying w-acc-sel-dpz-c01
                                          for  trailing spaces        .
           move      40                   to   w-acc-sel-dpz-c02      .
           subtract  w-acc-sel-dpz-c01    from w-acc-sel-dpz-c02      .
           add       1                    to   w-acc-sel-dpz-c02      .
           if        w-acc-sel-dpz-c02    <    18
                     move  " + Dipendenza parallela"
                                          to   w-acc-sel-dpz-ddd
                                              (w-acc-sel-dpz-c02 :)
           else      move  " + Dip. parallela"
                                          to   w-acc-sel-dpz-ddd
                                              (w-acc-sel-dpz-c02 :)   .
       sce-cod-dpz-536.
      *                                              *-----------------*
      *                                              * Composizione    *
      *                                              * denominazione   *
      *                                              * dipendenza per  *
      *                                              * area di uscita  *
      *                                              *-----------------*
           if        w-dpz-let-dpz        not  = spaces
                     go to sce-cod-dpz-538.
           move      zero                 to   w-acc-sel-dpz-c01      .
           inspect   w-dpz-den-prg    tallying w-acc-sel-dpz-c01
                                          for  trailing spaces        .
           if        w-acc-sel-dpz-c01    =    zero
                     go to sce-cod-dpz-538.
           move      20                   to   w-acc-sel-dpz-c02      .
           subtract  w-acc-sel-dpz-c01    from w-acc-sel-dpz-c02      .
           add       1                    to   w-acc-sel-dpz-c02      .
           move      "+"                  to   w-dpz-den-prg
                                              (w-acc-sel-dpz-c02 :)   .
       sce-cod-dpz-538.
      *                                              *-----------------*
      *                                              * Visualizzazione *
      *                                              * denominazione   *
      *                                              * dipendenza come *
      *                                              * da lettura ta-  *
      *                                              * bella + dicitu- *
      *                                              * ra per dipen-   *
      *                                              * denza parallela *
      *                                              *-----------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-sel-dpz-ddd    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                              *-----------------*
      *                                              * Se codice di-   *
      *                                              * pendenza non e- *
      *                                              * sistente : a    *
      *                                              * reimpostazione  *
      *                                              *-----------------*
           if        w-dpz-let-dpz        not  = spaces
                     go to sce-cod-dpz-300.
      *                                              *-----------------*
      *                                              * A richiesta di  *
      *                                              * conferma        *
      *                                              *-----------------*
           go to     sce-cod-dpz-700.
       sce-cod-dpz-540.
      *                                  *-----------------------------*
      *                                  * Se codice impostato pari a  *
      *                                  * 50                          *
      *                                  *-----------------------------*
       sce-cod-dpz-542.
      *                                      *-------------------------*
      *                                      * Deviazione a seconda se *
      *                                      * selezione semplice o    *
      *                                      * multipla                *
      *                                      *-------------------------*
           if        w-dpz-tip-ope        =    "S+"
                     go to sce-cod-dpz-546.
       sce-cod-dpz-544.
      *                                      *-------------------------*
      *                                      * Se selezione semplice   *
      *                                      *-------------------------*
      *                                          *---------------------*
      *                                          * Denominazione in a- *
      *                                          * rea di accettazio-  *
      *                                          * ne a errore         *
      *                                          *---------------------*
           move      all  "."             to   w-acc-sel-dpz-ddd      .
      *                                          *---------------------*
      *                                          * Denominazione in a- *
      *                                          * rea di uscita a     *
      *                                          * errore              *
      *                                          *---------------------*
           move      all  "."             to   w-dpz-den-prg          .
      *                                          *---------------------*
      *                                          * Visualizzazione de- *
      *                                          * nominazione dipen-  *
      *                                          * denza a spaces      M
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-sel-dpz-ddd    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                          *---------------------*
      *                                          * A reimpostazione    *
      *                                          *---------------------*
           go to     sce-cod-dpz-300.
       sce-cod-dpz-546.
      *                                      *-------------------------*
      *                                      * Se selezione multipla   *
      *                                      *-------------------------*
       sce-cod-dpz-548.
      *                                          *---------------------*
      *                                          * Deviazione a secon- *
      *                                          * da se terminazione  *
      *                                          * con Return o Pf1    *
      *                                          *---------------------*
           if        w-acc-sel-dpz-key    =    "[1] "
                     go to sce-cod-dpz-552.
       sce-cod-dpz-550.
      *                                          *---------------------*
      *                                          * Se Return o Do      *
      *                                          *---------------------*
      *                                              *-----------------*
      *                                              * Preparazione    *
      *                                              * denominazione   *
      *                                              * in area uscita  *
      *                                              *-----------------*
           move      "Tutte le parallele  "
                                          to   w-dpz-den-prg          .
      *                                              *-----------------*
      *                                              * Preparazione    *
      *                                              * denominazione   *
      *                                              * in area accet-  *
      *                                              * tazione         *
      *                                              *-----------------*
           move      "Tutte le dipendenze parallele           "
                                          to   w-acc-sel-dpz-ddd      .
      *                                              *-----------------*
      *                                              * Visualizzazione *
      *                                              * denominazione   *
      *                                              * per tutte le    *
      *                                              * dipendenze pa-  *
      *                                              * rallele         *
      *                                              *-----------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-sel-dpz-ddd    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                              *-----------------*
      *                                              * Se terminazione *
      *                                              * con tasto Do :  *
      *                                              * ad eventuale    *
      *                                              * controllo di    *
      *                                              * riservatezza.   *
      *                                              * Altreimenti : a *
      *                                              * richiesta della *
      *                                              * conferma.       *
      *                                              *-----------------*
           if        v-key                =    "DO  "
                     go to sce-cod-dpz-800
           else      go to sce-cod-dpz-700.
       sce-cod-dpz-552.
      *                                          *---------------------*
      *                                          * Se Pf1              *
      *                                          *---------------------*
      *                                              *-----------------*
      *                                              * Preparazione    *
      *                                              * denominazione   *
      *                                              * in area uscita  *
      *                                              * per errore      *
      *                                              *-----------------*
           move      all   "."            to   w-dpz-den-prg          .
      *                                              *-----------------*
      *                                              * Preparazione    *
      *                                              * denominazione   *
      *                                              * in area accet-  *
      *                                              * tazione         *
      *                                              *-----------------*
           move      all   "."            to   w-acc-sel-dpz-ddd      .
      *                                              *-----------------*
      *                                              * Visualizzazione *
      *                                              * denominazione   *
      *                                              * dipendenza per  *
      *                                              * errore          *
      *                                              *-----------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-sel-dpz-ddd    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                              *-----------------*
      *                                              * A reimpostazio- *
      *                                              * ne              *
      *                                              *-----------------*
           go to     sce-cod-dpz-300.
       sce-cod-dpz-560.
      *                                  *-----------------------------*
      *                                  * Se codice impostato compre- *
      *                                  * so tra 51 e 99              *
      *                                  *-----------------------------*
       sce-cod-dpz-562.
      *                                      *-------------------------*
      *                                      * Deviazione a seconda se *
      *                                      * selezione semplice o    *
      *                                      * multipla                *
      *                                      *-------------------------*
           if        w-dpz-tip-ope        =    "S+"
                     go to sce-cod-dpz-568.
       sce-cod-dpz-564.
      *                                      *-------------------------*
      *                                      * Se selezione semplice   *
      *                                      *-------------------------*
       sce-cod-dpz-566.
      *                                          *---------------------*
      *                                          * Denominazione in a- *
      *                                          * rea di accettazione *
      *                                          *---------------------*
           move      w-dpz-den-prg        to   w-acc-sel-dpz-ddd      .
      *                                          *---------------------*
      *                                          * Visualizzazione de- *
      *                                          * nominazione dipen-  *
      *                                          * denza come da let-  *
      *                                          * tura tabella        *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-sel-dpz-ddd    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                          *---------------------*
      *                                          * Se codice dipenden- *
      *                                          * za non esistente :  *
      *                                          * a reimpostazione    *
      *                                          *---------------------*
           if        w-dpz-let-dpz        not  = spaces
                     go to sce-cod-dpz-300.
      *                                          *---------------------*
      *                                          * Se terminazione con *
      *                                          * tasto Do : ad even- *
      *                                          * tuale controllo di  *
      *                                          * riservatezza.       *
      *                                          * Altrimenti : a ri-  *
      *                                          * chiesta conferma.   *
      *                                          *---------------------*
           if        v-key                =    "DO  "
                     go to sce-cod-dpz-800
           else      go to sce-cod-dpz-700.
       sce-cod-dpz-568.
      *                                      *-------------------------*
      *                                      * Se selezione multipla   *
      *                                      *-------------------------*
       sce-cod-dpz-570.
      *                                          *---------------------*
      *                                          * Deviazione a secon- *
      *                                          * da se terminazione  *
      *                                          * con Return o Pf1    *
      *                                          *---------------------*
           if        w-acc-sel-dpz-key    =    "[1] "
                     go to sce-cod-dpz-574.
       sce-cod-dpz-572.
      *                                          *---------------------*
      *                                          * Se Return o Do      *
      *                                          *---------------------*
      *                                              *-----------------*
      *                                              * Denominazione   *
      *                                              * in area di ac-  *
      *                                              * cettazione      *
      *                                              *-----------------*
           move      w-dpz-den-prg        to   w-acc-sel-dpz-ddd      .
      *                                              *-----------------*
      *                                              * Visualizzazione *
      *                                              * denominazione   *
      *                                              * dipendenza come *
      *                                              * da lettura ta-  *
      *                                              * bella           *
      *                                              *-----------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-sel-dpz-ddd    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                              *-----------------*
      *                                              * Se codice di-   *
      *                                              * pendenza non e- *
      *                                              * sistente : a    *
      *                                              * reimpostazione  *
      *                                              *-----------------*
           if        w-dpz-let-dpz        not  = spaces
                     go to sce-cod-dpz-300.
      *                                              *-----------------*
      *                                              * Se terminazione *
      *                                              * con tasto Do :  *
      *                                              * ad eventuale    *
      *                                              * controllo di    *
      *                                              * riservatezza.   *
      *                                              * Altreimenti : a *
      *                                              * richiesta della *
      *                                              * conferma.       *
      *                                              *-----------------*
           if        v-key                =    "DO  "
                     go to sce-cod-dpz-800
           else      go to sce-cod-dpz-700.
       sce-cod-dpz-574.
      *                                          *---------------------*
      *                                          * Se Pf1              *
      *                                          *---------------------*
       sce-cod-dpz-576.
      *                                              *-----------------*
      *                                              * Preparazione    *
      *                                              * denominazione   *
      *                                              * in area uscita  *
      *                                              * per errore      *
      *                                              *-----------------*
           move      all   "."            to   w-dpz-den-prg          .
      *                                              *-----------------*
      *                                              * Preparazione    *
      *                                              * denominazione   *
      *                                              * in area accet-  *
      *                                              * tazione         *
      *                                              *-----------------*
           move      all   "."            to   w-acc-sel-dpz-ddd      .
      *                                              *-----------------*
      *                                              * Visualizzazione *
      *                                              * denominazione   *
      *                                              * dipendenza per  *
      *                                              * errore          *
      *                                              *-----------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-acc-sel-dpz-ddd    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                              *-----------------*
      *                                              * A reimpostazio- *
      *                                              * ne              *
      *                                              *-----------------*
           go to     sce-cod-dpz-300.
       sce-cod-dpz-700.
      *                      *-----------------------------------------*
      *                      * Accettazione conferma                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Prompt per conferma                 *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      51                   to   v-pos                  .
           move      "Conferma (S/N/E) :  "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Accettazione                        *
      *                          *-------------------------------------*
       sce-cod-dpz-750.
           move      "S"                  to   v-alf                  .
       sce-cod-dpz-770.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      70                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "UP  "
                     move  "N"            to   v-alf
           else if   v-key                =    "DO  "
                     move  "S"            to   v-alf
           else if   v-key                =    "EXIT"
                     move  "E"            to   v-alf                  .
           if        v-alf                not  = "S" and
                     v-alf                not  = "N" and
                     v-alf                not  = "E"
                     go to sce-cod-dpz-770.
      *                          *-------------------------------------*
      *                          * Test su risposta a conferma         *
      *                          *-------------------------------------*
           if        v-alf                =    "S"
                     go to sce-cod-dpz-800
           else if   v-alf                =    "E"
                     go to sce-cod-dpz-975.
      *                          *-------------------------------------*
      *                          * Cancellazione prompt per conferma   *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      20                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      51                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Ritorno a impostazione dipendenza   *
      *                          *-------------------------------------*
           go to     sce-cod-dpz-300.
       sce-cod-dpz-800.
      *                      *-----------------------------------------*
      *                      * Controllo di abilitazione riservatezza  *
      *                      * sul codice dipendenza                   *
      *                      *-----------------------------------------*
           move      w-dpz-cod-prg        to   w-dpz-cod-dpz          .
           perform   tst-abl-rsv-000      thru tst-abl-rsv-999        .
           if        w-dpz-rsv-dpz        =    spaces
                     go to sce-cod-dpz-820.
           perform   err-non-rsv-000      thru err-non-rsv-999        .
           go to     sce-cod-dpz-300.
       sce-cod-dpz-820.
      *                      *-----------------------------------------*
      *                      * Preparazione dei parametri per l'uscita *
      *                      * in caso di selezione multipla           *
      *                      *-----------------------------------------*
       sce-cod-dpz-821.
      *                          *-------------------------------------*
      *                          * Se non e' ammessa la selezione      *
      *                          * multipla: nessuna azione            *
      *                          *-------------------------------------*
           if        w-dpz-tip-ope        not  = "S+"
                     go to sce-cod-dpz-890.
       sce-cod-dpz-822.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda se gestione    *
      *                          * dipendenze parallele attiva oppure  *
      *                          * no                                  *
      *                          *-------------------------------------*
           if        w-prs-snx-gdp-snx    =    "S"
                     go to sce-cod-dpz-840.
       sce-cod-dpz-830.
      *                          *-------------------------------------*
      *                          * Se gestione dipendenze parallele    *
      *                          * non attiva                          *
      *                          *-------------------------------------*
       sce-cod-dpz-832.
      *                              *---------------------------------*
      *                              * Uscita senza alcuna azione      *
      *                              *---------------------------------*
           go to     sce-cod-dpz-890.
       sce-cod-dpz-840.
      *                          *-------------------------------------*
      *                          * Se gestione dipendenze parallele    *
      *                          * attiva                              *
      *                          *-------------------------------------*
       sce-cod-dpz-842.
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * di terminazione, se con Pf1 o   *
      *                              * senza Pf1                       *
      *                              *---------------------------------*
           if        w-acc-sel-dpz-key    =    "[1] "
                     go to sce-cod-dpz-870.
       sce-cod-dpz-850.
      *                              *---------------------------------*
      *                              * Se terminazione senza Pf1       *
      *                              *---------------------------------*
       sce-cod-dpz-852.
      *                                  *-----------------------------*
      *                                  * Deviazione a seconda del    *
      *                                  * valore del codice dipen-    *
      *                                  * denza impostato             *
      *                                  *-----------------------------*
           if        w-dpz-cod-prg        =    zero
                     go to sce-cod-dpz-854
           else if   w-dpz-cod-prg        not  < 01 and
                     w-dpz-cod-prg        not  > 49
                     go to sce-cod-dpz-858
           else if   w-dpz-cod-prg        =    50
                     go to sce-cod-dpz-860
           else if   w-dpz-cod-prg        not  < 51 and
                     w-dpz-cod-prg        not  > 99
                     go to sce-cod-dpz-864
           else      go to sce-cod-dpz-854.
       sce-cod-dpz-854.
      *                                  *-----------------------------*
      *                                  * Se codice impostato pari a  *
      *                                  * zero : si considerano solo  *
      *                                  * i codici da 01 a 49, per-   *
      *                                  * tanto si eliminano i flags  *
      *                                  * per le dipendenze 51..99    *
      *                                  *-----------------------------*
       sce-cod-dpz-855.
           move      50                   to   w-acc-sel-dpz-c01      .
       sce-cod-dpz-856.
           add       1                    to   w-acc-sel-dpz-c01      .
           if        w-acc-sel-dpz-c01    >    99
                     go to sce-cod-dpz-890.
           move      spaces               to   w-dpz-ele-flg
                                              (w-acc-sel-dpz-c01)     .
           go to     sce-cod-dpz-856.
       sce-cod-dpz-858.
      *                                  *-----------------------------*
      *                                  * Se codice impostato compre- *
      *                                  * so tra 01 e 49              *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Uscita senza alcuna a-  *
      *                                      * zione particolare       *
      *                                      *-------------------------*
           go to     sce-cod-dpz-890.
       sce-cod-dpz-860.
      *                                  *-----------------------------*
      *                                  * Se codice impostato pari a  *
      *                                  * 50 : si considerano solo i  *
      *                                  * codici da 51 a 99, pertan-  *
      *                                  * to si eliminano i flags per *
      *                                  * le dipendenze 01..49, ed i- *
      *                                  * noltre si pone il codice    *
      *                                  * selezionato a zero          *
      *                                  *-----------------------------*
       sce-cod-dpz-861.
           move      zero                 to   w-acc-sel-dpz-c01      .
       sce-cod-dpz-862.
           add       1                    to   w-acc-sel-dpz-c01      .
           if        w-acc-sel-dpz-c01    >    49
                     go to sce-cod-dpz-863.
           move      spaces               to   w-dpz-ele-flg
                                              (w-acc-sel-dpz-c01)     .
           go to     sce-cod-dpz-862.
       sce-cod-dpz-863.
           move      zero                 to   w-dpz-cod-prg          .
           go to     sce-cod-dpz-890.
       sce-cod-dpz-864.
      *                                  *-----------------------------*
      *                                  * Se codice impostato compre- *
      *                                  * so tra 51 e 99              *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Uscita senza alcuna a-  *
      *                                      * zione particolare       *
      *                                      *-------------------------*
           go to     sce-cod-dpz-890.
       sce-cod-dpz-870.
      *                              *---------------------------------*
      *                              * Se terminazione con Pf1         *
      *                              *---------------------------------*
       sce-cod-dpz-872.
      *                                  *-----------------------------*
      *                                  * Deviazione a seconda del    *
      *                                  * valore del codice dipen-    *
      *                                  * denza impostato             *
      *                                  *-----------------------------*
           if        w-dpz-cod-prg        =    zero
                     go to sce-cod-dpz-874
           else if   w-dpz-cod-prg        not  < 01 and
                     w-dpz-cod-prg        not  > 49
                     go to sce-cod-dpz-876
           else if   w-dpz-cod-prg        =    50
                     go to sce-cod-dpz-874
           else if   w-dpz-cod-prg        not  < 51 and
                     w-dpz-cod-prg        not  > 99
                     go to sce-cod-dpz-874
           else      go to sce-cod-dpz-874.
       sce-cod-dpz-874.
      *                                  *-----------------------------*
      *                                  * Se codice impostato pari a  *
      *                                  * zero : uscita senza alcuna  *
      *                                  * azione                      *
      *                                  *-----------------------------*
           go to     sce-cod-dpz-890.
       sce-cod-dpz-876.
      *                                  *-----------------------------*
      *                                  * Se codice impostato compre- *
      *                                  * so tra 01 e 49: si conside- *
      *                                  * rano solamente il codice    *
      *                                  * selezionato, ed il codice   *
      *                                  * corrispondente aumentato di *
      *                                  * 50; pertanto si eliminano   *
      *                                  * i flags di tutti gli altri  *
      *                                  * eventuali elementi; inoltre *
      *                                  * si pone il codice selezio-  *
      *                                  * nato a zero                 *
      *                                  *-----------------------------*
       sce-cod-dpz-877.
           move      zero                 to   w-acc-sel-dpz-c01      .
       sce-cod-dpz-878.
           add       1                    to   w-acc-sel-dpz-c01      .
           if        w-acc-sel-dpz-c01    >    49
                     go to sce-cod-dpz-879.
           if        w-acc-sel-dpz-c01    =    w-dpz-cod-prg
                     go to sce-cod-dpz-878.
           move      spaces               to   w-dpz-ele-flg
                                              (w-acc-sel-dpz-c01)     .
           move      w-acc-sel-dpz-c01    to   w-acc-sel-dpz-c02      .
           add       50                   to   w-acc-sel-dpz-c02      .
           move      spaces               to   w-dpz-ele-flg
                                              (w-acc-sel-dpz-c02)     .
           go to     sce-cod-dpz-878.
       sce-cod-dpz-879.
           move      zero                 to   w-dpz-cod-prg          .
           go to     sce-cod-dpz-890.
       sce-cod-dpz-890.
      *                          *-------------------------------------*
      *                          * Fine preparazione parametri per     *
      *                          * l'uscita in caso di selezione       *
      *                          * multipla : uscita                   *
      *                          *-------------------------------------*
           go to     sce-cod-dpz-999.
       sce-cod-dpz-900.
      *              *-------------------------------------------------*
      *              * Se non e' la prima volta che il programma chia- *
      *              * mante richiede questa funzione                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se c'e' un solo codice dipendenza me-  *
      *                  * morizzato per l'azienda                     *
      *                  *---------------------------------------------*
           if        w-dpz-ctr-dpz        >    1
                     go to sce-cod-dpz-925
           else      go to sce-cod-dpz-950.
       sce-cod-dpz-925.
      *                  *---------------------------------------------*
      *                  * Se c'e' piu' di un codice dipendenza memo-  *
      *                  * rizzato per l'azienda si richiede nuovamen- *
      *                  * te la scelta del codice dipendenza          *
      *                  *---------------------------------------------*
           go to     sce-cod-dpz-100.
       sce-cod-dpz-950.
      *                  *---------------------------------------------*
      *                  * Se c'e' un solo codice dipendenza memoriz-  *
      *                  * zato per l'azienda si esce come per sele-   *
      *                  * zione non effettuata                        *
      *                  *---------------------------------------------*
           go to     sce-cod-dpz-975.
       sce-cod-dpz-975.
      *                  *---------------------------------------------*
      *                  * Se l'operatore ha rifiutato la selezione    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * In caso di possibile selezione multipla *
      *                      *                                         *
      *                      * - Codice dipendenza per l'intero pro-   *
      *                      *   gramma a zero                         *
      *                      * - Denominazione dipendenza per l'intero *
      *                      *   programma a spaces                    *
      *                      * - Numero dipendenze esistenti per l'a-  *
      *                      *   zienda a zero                         *
      *                      * - Uscita                                *
      *                      *                                         *
      *                      * In caso di possibile selezione solamen- *
      *                      * te singola                              *
      *                      *                                         *
      *                      * - Codice dipendenza per l'intero pro-   *
      *                      *   gramma a zero                         *
      *                      * - Denominazione dipendenza per l'intero *
      *                      *   programma a spaces                    *
      *                      * - Uscita                                *
      *                      *                                         *
      *                      *-----------------------------------------*
           if        w-dpz-tip-ope        =    "S+"
                     move  zero           to   w-dpz-cod-prg
                     move  spaces         to   w-dpz-den-prg
                     move  zero           to   w-dpz-ctr-dpz
                     go to sce-cod-dpz-999
            else     move  zero           to   w-dpz-cod-prg
                     move  spaces         to   w-dpz-den-prg
                     go to sce-cod-dpz-999.
       sce-cod-dpz-999.
           exit.

      *    *===========================================================*
      *    * Scelta multipla di codici dipendenza                      *
      *    *-----------------------------------------------------------*
       smu-cod-dpz-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se e' stata eseguita oppu- *
      *              * re no la preparazione R0-R5 per la determina-   *
      *              * zione dei codici dipendenza esistenti e per la  *
      *              * selezione multipla, e a seconda di come e' sta- *
      *              * ta eseguita                                     *
      *              *-------------------------------------------------*
           if        w-r05-pvz            =    "+"
                     go to smu-cod-dpz-100
           else if   w-r05-pvz            =    "-"
                     go to smu-cod-dpz-300
           else      go to smu-cod-dpz-800.
       smu-cod-dpz-100.
      *              *-------------------------------------------------*
      *              * Se la preparazione R0-R5 e' stata eseguita con  *
      *              * tipo provenienza '+' : si passano in rassegna   *
      *              * tutti i codici dipendenza in tabella, e si se-  *
      *              * lezionano solamente quelli che appartengono     *
      *              * alla lista dei codici passati con 'R5'          *
      *              *-------------------------------------------------*
       smu-cod-dpz-110.
      *                  *---------------------------------------------*
      *                  * Se nessun codice passato                    *
      *                  *---------------------------------------------*
           if        w-r05-nel            not  = zero
                     go to smu-cod-dpz-120.
      *                      *-----------------------------------------*
      *                      * Preparazione parametri in uscita        *
      *                      *-----------------------------------------*
           if        w-dpz-ctr-dpz        =    1
                     move  w-dpz-uni-cod  to   w-dpz-cod-prg
                     move  w-dpz-uni-den  to   w-dpz-den-prg
           else      move  zero           to   w-dpz-cod-prg
                     move  spaces         to   w-dpz-den-prg          .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     smu-cod-dpz-999.
       smu-cod-dpz-120.
      *                  *---------------------------------------------*
      *                  * Esame della tabella, con selezione elementi *
      *                  * e conteggio elementi selezionati            *
      *                  *---------------------------------------------*
       smu-cod-dpz-122.
           move      zero                 to   w-r05-c00              .
           move      zero                 to   w-r05-i01              .
           move      zero                 to   w-r05-xco              .
           move      spaces               to   w-r05-xde              .
       smu-cod-dpz-124.
           add       1                    to   w-r05-i01              .
           if        w-r05-i01            >    99
                     go to smu-cod-dpz-140.
           if        w-dpz-ele-flg
                    (w-r05-i01)           =    spaces
                     go to smu-cod-dpz-124.
           move      zero                 to   w-r05-i02              .
       smu-cod-dpz-126.
           add       1                    to   w-r05-i02              .
           if        w-r05-i02            >    w-r05-nel
                     go to smu-cod-dpz-128.
           if        w-r05-i01            =    w-r05-cod
                                              (w-r05-i02)
                     go to smu-cod-dpz-130
           else      go to smu-cod-dpz-126.
       smu-cod-dpz-128.
           move      spaces               to   w-dpz-ele-flg
                                              (w-r05-i01)             .
           go to     smu-cod-dpz-124.
       smu-cod-dpz-130.
           add       1                    to   w-r05-c00              .
           if        w-r05-c00            =    1
                     move  w-r05-i01      to   w-r05-xco
                     move  w-dpz-ele-den
                          (w-r05-i01)     to   w-r05-xde
           else      move  zero           to   w-r05-xco
                     move  spaces         to   w-r05-xde              .
           go to     smu-cod-dpz-124.
       smu-cod-dpz-140.
      *                  *---------------------------------------------*
      *                  * Esame del risultato della selezione         *
      *                  *---------------------------------------------*
       smu-cod-dpz-142.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del numero di    *
      *                      * dipendenze selezionate                  *
      *                      *-----------------------------------------*
           if        w-r05-c00            =    zero
                     go to smu-cod-dpz-144
           else      go to smu-cod-dpz-146.
       smu-cod-dpz-144.
      *                      *-----------------------------------------*
      *                      * Se zero dipendenze selezionate          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Preparazione parametri in uscita    *
      *                          * per selezione non effettuata        *
      *                          *-------------------------------------*
           move      zero                 to   w-dpz-cod-prg          .
           move      spaces               to   w-dpz-den-prg          .
           move      zero                 to   w-dpz-ctr-dpz          .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     smu-cod-dpz-999.
       smu-cod-dpz-146.
      *                      *-----------------------------------------*
      *                      * Se almeno una dipendenza selezionata    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Preparazione parametri in uscita    *
      *                          * per selezione effettuata            *
      *                          *-------------------------------------*
           if        w-r05-c00            =    1
                     move  w-r05-xco      to   w-dpz-cod-prg
                     move  w-r05-xde      to   w-dpz-den-prg
           else      move  zero           to   w-dpz-cod-prg
                     move  spaces         to   w-dpz-den-prg          .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     smu-cod-dpz-999.
       smu-cod-dpz-300.
      *              *-------------------------------------------------*
      *              * Se la preparazione R0-R5 e' stata eseguita con  *
      *              * tipo provenienza '-' : si passano in rassegna   *
      *              * tutti i codici dipendenza in tabella, e si se-  *
      *              * lezionano solamente quelli che non appartengono *
      *              * alla lista dei codici passati con 'R5'          *
      *              *-------------------------------------------------*
       smu-cod-dpz-310.
      *                  *---------------------------------------------*
      *                  * Se nessun codice passato                    *
      *                  *---------------------------------------------*
           if        w-r05-nel            not  = zero
                     go to smu-cod-dpz-320.
      *                      *-----------------------------------------*
      *                      * Preparazione parametri in uscita per    *
      *                      * selezione non effettuata                *
      *                      *-----------------------------------------*
           move      zero                 to   w-dpz-cod-prg          .
           move      spaces               to   w-dpz-den-prg          .
           move      zero                 to   w-dpz-ctr-dpz          .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     smu-cod-dpz-999.
       smu-cod-dpz-320.
      *                  *---------------------------------------------*
      *                  * Esame della tabella, con selezione elementi *
      *                  * e conteggio elementi selezionati            *
      *                  *---------------------------------------------*
       smu-cod-dpz-322.
           move      zero                 to   w-r05-c00              .
           move      zero                 to   w-r05-i01              .
           move      zero                 to   w-r05-xco              .
           move      spaces               to   w-r05-xde              .
       smu-cod-dpz-324.
           add       1                    to   w-r05-i01              .
           if        w-r05-i01            >    99
                     go to smu-cod-dpz-340.
           if        w-dpz-ele-flg
                    (w-r05-i01)           =    spaces
                     go to smu-cod-dpz-324.
           move      zero                 to   w-r05-i02              .
       smu-cod-dpz-326.
           add       1                    to   w-r05-i02              .
           if        w-r05-i02            >    w-r05-nel
                     go to smu-cod-dpz-328.
           if        w-r05-i01            =    w-r05-cod
                                              (w-r05-i02)
                     go to smu-cod-dpz-330
           else      go to smu-cod-dpz-326.
       smu-cod-dpz-328.
           add       1                    to   w-r05-c00              .
           if        w-r05-c00            =    1
                     move  w-r05-i01      to   w-r05-xco
                     move  w-dpz-ele-den
                          (w-r05-i01)     to   w-r05-xde
           else      move  zero           to   w-r05-xco
                     move  spaces         to   w-r05-xde              .
           go to     smu-cod-dpz-324.
       smu-cod-dpz-330.
           move      spaces               to   w-dpz-ele-flg
                                              (w-r05-i01)             .
           go to     smu-cod-dpz-324.
       smu-cod-dpz-340.
      *                  *---------------------------------------------*
      *                  * Esame del risultato della selezione         *
      *                  *---------------------------------------------*
       smu-cod-dpz-342.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del numero di    *
      *                      * dipendenze selezionate                  *
      *                      *-----------------------------------------*
           if        w-r05-c00            =    zero
                     go to smu-cod-dpz-344
           else      go to smu-cod-dpz-346.
       smu-cod-dpz-344.
      *                      *-----------------------------------------*
      *                      * Se zero dipendenze selezionate          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Preparazione parametri in uscita    *
      *                          * per selezione non effettuata        *
      *                          *-------------------------------------*
           move      zero                 to   w-dpz-cod-prg          .
           move      spaces               to   w-dpz-den-prg          .
           move      zero                 to   w-dpz-ctr-dpz          .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     smu-cod-dpz-999.
       smu-cod-dpz-346.
      *                      *-----------------------------------------*
      *                      * Se almeno una dipendenza selezionata    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Preparazione parametri in uscita    *
      *                          * per selezione effettuata            *
      *                          *-------------------------------------*
           if        w-r05-c00            =    1
                     move  w-r05-xco      to   w-dpz-cod-prg
                     move  w-r05-xde      to   w-dpz-den-prg
           else      move  zero           to   w-dpz-cod-prg
                     move  spaces         to   w-dpz-den-prg          .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     smu-cod-dpz-999.
       smu-cod-dpz-800.
      *              *-------------------------------------------------*
      *              * Se la preparazione R0-R5 non e' stata eseguita  *
      *              * oppure e' stata eseguita con tipo provenienza   *
      *              * 'R'                                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Forzatura dell'attuale codice dipendenza a  *
      *                  *---------------------------------------------*
           move      zero                 to   w-dpz-cod-prg          .
      *                  *---------------------------------------------*
      *                  * Preparazione del codice dipendenza di de-   *
      *                  * fault                                       *
      *                  *---------------------------------------------*
           move      "D<"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-num                to   w-dpz-cod-dpz          .
      *                  *---------------------------------------------*
      *                  * Richiesta codice dipendenza all'operatore   *
      *                  *---------------------------------------------*
           perform   sce-cod-dpz-000      thru sce-cod-dpz-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     smu-cod-dpz-999.
       smu-cod-dpz-999.
           exit.

      *    *===========================================================*
      *    * Scelta del codice dipendenza su cui il programma chiaman- *
      *    * te deve lavorare; si presume che il programma chiamante   *
      *    * abbia preventivamente richiamato la funzione di "determi- *
      *    * nazione dipendenze esistenti per l'azienda" e che pertan- *
      *    * to la tabella dipendenze w-dpz-tbl-dpz risulti carica di  *
      *    * valori. La scelta viene richiesta all'operatore solo se   *
      *    * la segreteria non conosce il codice dipendenza attualmen- *
      *    * te in uso                                                 *
      *    *-----------------------------------------------------------*
       ric-cod-dpz-000.
      *              *-------------------------------------------------*
      *              * Se le personalizzazioni indicano che il codice  *
      *              * dipendenza deve comunque essere richiesto al-   *
      *              * l'operatore                                     *
      *              *-------------------------------------------------*
           if        w-prs-acc-dpz-tac    =    "R"
                     go to ric-cod-dpz-400.
       ric-cod-dpz-010.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se deve essere propo-  *
      *                  * sto oppure no un valore di default          *
      *                  *---------------------------------------------*
           if        w-prs-acc-dpz-tac    =    "S"
                     go to ric-cod-dpz-020
           else      go to ric-cod-dpz-030.
       ric-cod-dpz-020.
      *                  *---------------------------------------------*
      *                  * Se nessun valore di default da proporre     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad impostazione manuale                 *
      *                      *-----------------------------------------*
           go to     ric-cod-dpz-600.
       ric-cod-dpz-030.
      *                  *---------------------------------------------*
      *                  * Se valore di default da proporre pari al    *
      *                  * codice dipendenza attualmente in uso        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione del codice dipendenza di   *
      *                      * default                                 *
      *                      *-----------------------------------------*
           move      "D<"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-num                to   w-dpz-cod-dpz          .
      *                      *-----------------------------------------*
      *                      * Ad impostazione manuale                 *
      *                      *-----------------------------------------*
           go to     ric-cod-dpz-600.
       ric-cod-dpz-400.
      *              *-------------------------------------------------*
      *              * Richiesta codice dipendenza alla segreteria     *
      *              *-------------------------------------------------*
           move      "D<"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del valore del codice di-  *
      *              * pendenza attualmente in uso                     *
      *              *-------------------------------------------------*
           if        s-num                =    zero
                     go to ric-cod-dpz-600
           else      go to ric-cod-dpz-800.
       ric-cod-dpz-600.
      *              *-------------------------------------------------*
      *              * Se codice dipendenza attualmente in uso a zero  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiesta codice dipendenza all'operatore   *
      *                  *---------------------------------------------*
           perform   sce-cod-dpz-000      thru sce-cod-dpz-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ric-cod-dpz-999.
       ric-cod-dpz-800.
      *              *-------------------------------------------------*
      *              * Se codice dipendenza attualmente in uso diverso *
      *              * da zero                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice dipendenza in uscita                 *
      *                  *---------------------------------------------*
           move      s-num                to   w-dpz-cod-prg          .
      *                  *---------------------------------------------*
      *                  * Denominazione dipendenza in uscita          *
      *                  *---------------------------------------------*
           move      s-alf                to   w-dpz-den-prg          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ric-cod-dpz-999.
       ric-cod-dpz-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione codice dipendenza alla segreteria dopo la   *
      *    * scelta di una dipendenza                                  *
      *    *-----------------------------------------------------------*
       dic-cod-dpz-000.
      *              *-------------------------------------------------*
      *              * Se non effettuata la selezione : uscita         *
      *              *-------------------------------------------------*
           if        w-dpz-ctr-dpz        =    zero
                     go to dic-cod-dpz-999.
      *              *-------------------------------------------------*
      *              * Dichiarazione codice e denominazione dipendenza *
      *              * alla segreteria                                 *
      *              *-------------------------------------------------*
           move      "D>"                 to   s-ope                  .
           move      w-dpz-ctr-dpz        to   s-car                  .
           move      w-dpz-cod-prg        to   s-num                  .
           move      w-dpz-den-prg        to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-cod-dpz-999.
           exit.

      *    *===========================================================*
      *    * Controllo di abilitazione su codice dipendenza contenuto  *
      *    * in w-dpz-cod-dpz, oppure su tutti i codici dipendenza se  *
      *    * w-dpz-cod-dpz vale zero                                   *
      *    *-----------------------------------------------------------*
       tst-abl-rsv-000.
      *              *-------------------------------------------------*
      *              * Abilitazione sempre concessa                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-dpz-rsv-dpz          .
       tst-abl-rsv-999.
           exit.

      *    *===========================================================*
      *    * Controllo status codice dipendenza                        *
      *    *-----------------------------------------------------------*
       tst-sta-tus-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-dpz-let-dpz          .
       tst-sta-tus-100.
      *              *-------------------------------------------------*
      *              * Test se codice zero                             *
      *              *-------------------------------------------------*
           if        w-dpz-cod-dpz        =    zero
                     go to tst-sta-tus-900.
      *              *-------------------------------------------------*
      *              * Test se codice '01': per convenzione si ritiene *
      *              *                      la dipendenza '01' essere  *
      *              *                      la Sede, pertanto sempre   *
      *              *                      attiva                     *
      *              *-------------------------------------------------*
           if        w-dpz-cod-dpz        =    01
                     go to tst-sta-tus-900.
       tst-sta-tus-200.
      *              *-------------------------------------------------*
      *              * Test su flag di status dipendenza               *
      *              *-------------------------------------------------*
           perform   let-tab-dpz-000      thru let-tab-dpz-999        .
           if        w-dpz-let-dpz        not  = spaces
                     go to tst-sta-tus-900.
           if        rf-ada-sta-tus       not  = spaces
                     move  "#"            to   w-dpz-let-dpz          .
       tst-sta-tus-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     tst-sta-tus-999.
       tst-sta-tus-999.
           exit.

      *    *===========================================================*
      *    * Emissione messaggio di errore relativo al fatto che manca *
      *    * l'abilitazione per il codice dipendenza w-dpz-cod-dpz,    *
      *    * oppure su tutti i codici dipendenza se w-dpz-cod-dpz vale *
      *    * zero                                                      *
      *    *-----------------------------------------------------------*
       err-non-rsv-000.
      *              *-------------------------------------------------*
      *              * Se manca l'abilitazione per una dipendenza      *
      *              *-------------------------------------------------*
           if        w-dpz-cod-dpz        =    zero
                     go to err-non-rsv-500.
           move      "ME"                 to   v-ope                  .
           move      spaces               to   v-not                  .
           string    "Manca l'abilitazione per la dipendenza codice "
                                delimited by   size
                     w-dpz-cod-dpz
                                delimited by   size
                     " ! "      delimited by   size
                                          into v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     err-non-rsv-999.
       err-non-rsv-500.
      *              *-------------------------------------------------*
      *              * Se manca l'abilitazione per tutte le dipendenze *
      *              *-------------------------------------------------*
           move      "ME"                 to   v-ope                  .
           move      "Non c'e' l'abilitazione per tutte le dipendenze !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     err-non-rsv-999.
       err-non-rsv-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di visualizzazione titolo programma chiamante  *
      *    * in funzione di w-dpz-ide-fas e w-dpz-ide-des              *
      *    *-----------------------------------------------------------*
       tit-pgm-chi-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del valore del titolo del  *
      *              * programma chiamante                             *
      *              *-------------------------------------------------*
           if        w-dpz-ide-des        =    spaces
                     go to tit-pgm-chi-200
           else      go to tit-pgm-chi-400.
       tit-pgm-chi-200.
      *              *-------------------------------------------------*
      *              * Se w-dpz-ide-des diverso da spaces              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Erase linee 04..21                          *
      *                  *---------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tit-pgm-chi-999.
       tit-pgm-chi-400.
      *              *-------------------------------------------------*
      *              * Se w-dpz-ide-des diverso da spaces              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Erase video                                 *
      *                  *---------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Trattini a linea 01                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      01                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sigla del programma                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-dpz-ide-fas        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Descrizione del programma                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-dpz-ide-des        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Trattini a linea 03                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Trattini a linea 22                         *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      22                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     tit-pgm-chi-999.
       tit-pgm-chi-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione per tipo di accettazione per    *
      *    * il codice della dipendenza                                *
      *    *-----------------------------------------------------------*
       prs-acc-dpz-000.
      *              *-------------------------------------------------*
      *              * Test se lettura necessaria                      *
      *              *-------------------------------------------------*
           if        w-prs-acc-dpz-tac    not  = spaces
                     go to prs-acc-dpz-999.
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/azi[acc-dpz]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-acc-dpz-tac
           else      move  spaces         to   w-prs-acc-dpz-tac      .
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           if        w-prs-acc-dpz-tac    not  = "D" and
                     w-prs-acc-dpz-tac    not  = "R"
                     move  "S"            to   w-prs-acc-dpz-tac      .
       prs-acc-dpz-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione per Si/No gestione dipendenze   *
      *    * parallele                                                 *
      *    *-----------------------------------------------------------*
       prs-snx-gdp-000.
      *              *-------------------------------------------------*
      *              * Test se lettura necessaria                      *
      *              *-------------------------------------------------*
           if        w-prs-snx-gdp-snx    not  = spaces
                     go to prs-snx-gdp-999.
      *              *-------------------------------------------------*
      *              * Lettura                                         *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/azi[snx-gdp]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-gdp-snx
           else      move  spaces         to   w-prs-snx-gdp-snx      .
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           if        w-prs-snx-gdp-snx    not  = "S"
                     move  "N"            to   w-prs-snx-gdp-snx      .
       prs-snx-gdp-999.
           exit.

      *    *===========================================================*
      *    * Richiesta numero e tabella dipendenze abilitate           *
      *    *-----------------------------------------------------------*
       ric-dpz-abl-000.
      *              *-------------------------------------------------*
      *              * Richiesta a segreteria                          *
      *              *-------------------------------------------------*
           move      "D?"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Memorizzazione risultato                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero dipendenze abilitate                 *
      *                  *---------------------------------------------*
           move      s-num                to   w-det-abl-dpz-num      .
      *                  *---------------------------------------------*
      *                  * Tabella dipendenze abilitate                *
      *                  *---------------------------------------------*
           move      s-alf                to   w-det-abl-dpz-tbl      .
       ric-dpz-abl-999.
           exit.

