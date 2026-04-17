       Identification Division.
       Program-Id.                                 pxpg0004           .
      *================================================================*
      *                                                                *
      * Scelta codice azienda e caricamento dati relativi ad essa      *
      *                                                                *
      *================================================================*

      ******************************************************************
       Environment Division.
      ******************************************************************

      *================================================================*
       Configuration Section.
      *================================================================*

       Source-Computer.     d-K-b-Snc-PD .
       Object-Computer.     d-K-b-Snc-PD .

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [auc]                                        *
      *    *-----------------------------------------------------------*
           select  optional  auc   assign to disk         f-auc-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is auc-key
                             file status  is f-auc-sts                .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *================================================================*
      *    File Description [auc]                                      *
      *----------------------------------------------------------------*
       fd  auc       label record standard                            .
       01  auc-rec.
           05  auc-key.
               10  auc-tre                pic  x(04)                  .
               10  auc-kre                pic  x(40)                  .
           05  auc-dat.
               10  auc-chr.
                   15  filler      occurs 1536
                                          pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

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
      *    * Area di definizione del pacchetto applicativo e delle ca- *
      *    * ratteristiche della release                               *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/a"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mvideo"  *
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
      *    * Work area per accettazione codici e passwords             *
      *    *-----------------------------------------------------------*
       01  w-acp.
           05  w-acp-psw                  pic  x(08)                  .
           05  w-acp-acc                  pic  x(08)                  .
           05  w-acp-key                  pic  x(04)                  .

      *    *===========================================================*
      *    * Work area per accettazione codice azienda                 *
      *    *-----------------------------------------------------------*
       01  w-aaz.
           05  w-aaz-sts                  pic  x(01)                  .
           05  w-aaz-cod                  pic  x(04)                  .
           05  w-aaz-key                  pic  x(04)                  .
           05  w-aaz-des                  pic  x(40)                  .

      *    *===========================================================*
      *    * Work area per routines di Det                             *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per Det codice azienda iniziale da segreteria         *
      *        *-------------------------------------------------------*
           05  w-det-azi-sgr.
      *            *---------------------------------------------------*
      *            * Codice azienda iniziale da segreteria             *
      *            *---------------------------------------------------*
               10  w-det-azi-sgr-cod      pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Per Det numero aziende su cui l'utente puo' operare   *
      *        *-------------------------------------------------------*
           05  w-det-naz-ute.
      *            *---------------------------------------------------*
      *            * Numero aziende su cui l'utente puo' operare       *
      *            * - 0 : nessuna                                     *
      *            * - 1 : una                                         *
      *            * - 2 : piu' di una                                 *
      *            *---------------------------------------------------*
               10  w-det-naz-ute-num      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice azienda se w-det-naz-ute-num = 1           *
      *            *---------------------------------------------------*
               10  w-det-naz-ute-cod      pic  x(04)                  .
      *            *---------------------------------------------------*
      *            * Descrizione azienda se w-det-naz-ute-num = 1      *
      *            *---------------------------------------------------*
               10  w-det-naz-ute-des      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Password azienda se w-det-naz-ute-num = 1         *
      *            *---------------------------------------------------*
               10  w-det-naz-ute-pwd      pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Status azienda se w-det-naz-ute-num = 1           *
      *            *---------------------------------------------------*
               10  w-det-naz-ute-sts      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Work locali                                       *
      *            *---------------------------------------------------*
               10  w-det-naz-ute-wrk.
                   15  w-det-naz-ute-wc1  pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Per Det descrizione azienda centrata                  *
      *        *-------------------------------------------------------*
           05  w-det-daz-cen.
      *            *---------------------------------------------------*
      *            * Descrizione azienda allineata a sinistra          *
      *            *---------------------------------------------------*
               10  w-det-daz-cen-asx.
                   15  w-det-daz-cen-asc occurs 40
                                          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Descrizione azienda allineata al centro           *
      *            *---------------------------------------------------*
               10  w-det-daz-cen-adx.
                   15  w-det-daz-cen-adc occurs 40
                                          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Work locali                                       *
      *            *---------------------------------------------------*
               10  w-det-daz-cen-wrk.
                   15  w-det-daz-cen-w01  pic  9(02)                  .
                   15  w-det-daz-cen-w02  pic  9(02)                  .
                   15  w-det-daz-cen-w03  pic  9(02)                  .

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

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per programmi della serie "pxpg"    *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/pxpglink.cpy"                   .

      ******************************************************************
       Procedure Division                using x                      .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita da programma   *
      *              *-------------------------------------------------*
           move      spaces               to   x-sts                  .
      *              *-------------------------------------------------*
      *              * Apertura file [auc]                             *
      *              *-------------------------------------------------*
           perform   opn-fil-auc-000      thru opn-fil-auc-999        .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni generali              *
      *              *-------------------------------------------------*
           perform   let-psg-auc-000      thru let-psg-auc-999        .
      *              *-------------------------------------------------*
      *              * Lettura codice utente da file [auc]             *
      *              *-------------------------------------------------*
           perform   let-ute-auc-000      thru let-ute-auc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione codice azienda iniziale da se-   *
      *              * greteria                                        *
      *              *-------------------------------------------------*
           perform   det-azi-sgr-000      thru det-azi-sgr-999        .
      *              *-------------------------------------------------*
      *              * Determinazione numero aziende su cui l'utente   *
      *              * puo' operare, se zero, una, piu' di una         *
      *              *-------------------------------------------------*
           perform   det-naz-ute-000      thru det-naz-ute-999        .
      *              *-------------------------------------------------*
      *              * Se numero aziende su cui l'utente puo' operare  *
      *              * a 1, uscita con status ad Ok                    *
      *              *-------------------------------------------------*
           if        w-det-naz-ute-num    =    1
                     move  spaces         to   x-sts
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Se numero aziende su cui l'utente puo' operare  *
      *              * maggiore di 1, oppure pari a zero, cioe' tutte  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione codice azienda                 *
      *                  *---------------------------------------------*
           perform   acc-cod-azi-000      thru acc-cod-azi-999        .
      *                  *---------------------------------------------*
      *                  * Status di uscita in funzione dello status   *
      *                  * di uscita dalla accettazione                *
      *                  *---------------------------------------------*
           if        w-aaz-sts            =    spaces
                     move  spaces         to   x-sts
           else      move  "##"           to   x-sts                  .
       main-900.
      *              *-------------------------------------------------*
      *              * Chiusura file [auc]                             *
      *              *-------------------------------------------------*
           perform   cls-fil-auc-000      thru cls-fil-auc-999        .
       main-999.
           exit      program                                          .

      *    *===========================================================*
      *    * Apertura file [auc]                                       *
      *    *-----------------------------------------------------------*
       opn-fil-auc-000.
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
      *              * Operazione di i-o di Open                       *
      *              *-------------------------------------------------*
           open      i-o    auc                                       .
       opn-fil-auc-999.
           exit.

      *    *===========================================================*
      *    * Chiusura file [auc]                                       *
      *    *-----------------------------------------------------------*
       cls-fil-auc-000.
           close      auc                                             .
       cls-fil-auc-999.
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
           move      spaces               to   w-psg-pwd-azi          .
           move      spaces               to   w-psg-pwd-ute          .
           move      spaces               to   w-psg-pwd-cmd          .
           move      spaces               to   w-psg-pwd-ter          .
           move      spaces               to   w-psg-pwd-stp          .
           move      spaces               to   w-psg-pwd-r00          .
           move      spaces               to   w-psg-rsv-tsk          .
           move      spaces               to   w-psg-rsv-mnu          .
           move      spaces               to   w-psg-rsv-prc          .
           move      spaces               to   w-psg-rsv-ide          .
           move      spaces               to   w-psg-rsv-tut          .
           move      spaces               to   w-psg-rsv-lip          .
           move      spaces               to   w-psg-rsv-r00          .
           move      spaces               to   w-psg-snx-azi          .
           move      spaces               to   w-psg-tem-spl          .
           move      zero                 to   w-psg-iod-lor          .
           move      spaces               to   w-psg-tem-bss          .
           move      spaces               to   w-psg-cnt-vpf          .
           move      spaces               to   w-psg-cnt-pg1          .
           move      spaces               to   w-psg-cnt-pg2          .
           move      spaces               to   w-psg-cnt-pg3          .
           go to     let-psg-auc-999.
       let-psg-auc-200.
      *              *-------------------------------------------------*
      *              * Se personalizzazioni generali esistenti         *
      *              *-------------------------------------------------*
           move      auc-dat              to   w-psg                  .
       let-psg-auc-999.
           exit.

      *    *===========================================================*
      *    * Lettura codice utente da file [auc]                       *
      *    *-----------------------------------------------------------*
       let-ute-auc-000.
      *              *-------------------------------------------------*
      *              * Lettura record codice utente                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ottenimento codice utente da segreteria     *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-ute                to   w-ute-cod-ute          .
      *                  *---------------------------------------------*
      *                  * Lettura record relativo a codice utente     *
      *                  *---------------------------------------------*
           move      "ute "               to   auc-tre                .
           move      w-ute-cod-ute        to   auc-kre                .
           read      auc    with no lock
                            invalid key
                            go to   let-ute-auc-100.
           go to     let-ute-auc-200.
       let-ute-auc-100.
      *                  *---------------------------------------------*
      *                  * Se codice utente non esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione area codice utente      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione utente                  *
      *                          *-------------------------------------*
           move      spaces               to   w-ute-des-ute          .
           string    "Utente "  delimited by   size
                     w-ute-cod-ute
                                delimited by   size
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
      *                          * operare : zero                      *
      *                          *-------------------------------------*
           move      00                   to   w-ute-num-azi          .
      *                          *-------------------------------------*
      *                          * Elenco aziende in cui l'utente puo' *
      *                          * operare : nessuna azienda           *
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
      *                          * sociato all'utente : questo utente  *
      *                          * non puo' stampare                   *
      *                          *-------------------------------------*
           move      99                   to   w-ute-tse-cst          .
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
      *                          * Altri parametri stampa di uso futu- *
      *                          * ro associati all'utente : nessuno   *
      *                          *-------------------------------------*
           move      spaces               to   w-ute-aps-aau          .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     let-ute-auc-999.
       let-ute-auc-200.
      *                  *---------------------------------------------*
      *                  * Se codice utente esistente                  *
      *                  *---------------------------------------------*
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
       let-ute-auc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione codice azienda iniziale da segreteria      *
      *    *-----------------------------------------------------------*
       det-azi-sgr-000.
      *              *-------------------------------------------------*
      *              * Ottenimento codice azienda da segreteria        *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-azi                to   w-det-azi-sgr-cod      .
       det-azi-sgr-999.
           exit.

      *    *===========================================================*
      *    * Determinazione numero aziende su cui l'utente puo' opera- *
      *    * re, e se una sola azienda anche del codice azienda, della *
      *    * descrizione azienda e della password azienda              *
      *    *-----------------------------------------------------------*
       det-naz-ute-000.
      *              *-------------------------------------------------*
      *              * Inizializzazioni                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero aziende                              *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-naz-ute-num      .
      *                  *---------------------------------------------*
      *                  * Codice azienda se una sola                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-naz-ute-cod      .
      *                  *---------------------------------------------*
      *                  * Descrizione azienda se una sola             *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-naz-ute-des      .
      *                  *---------------------------------------------*
      *                  * Password azienda se una sola                *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-naz-ute-pwd      .
      *                  *---------------------------------------------*
      *                  * Status azienda se una sola                  *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-naz-ute-sts      .
       det-naz-ute-100.
      *              *-------------------------------------------------*
      *              * Lettura aziende memorizzate                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su aziende                            *
      *                  *---------------------------------------------*
           move      "azi "               to   auc-tre                .
           move      spaces               to   auc-kre                .
           start     auc    key not less
                            auc-key
                            invalid key
                            go to   det-naz-ute-800.
       det-naz-ute-200.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale                         *
      *                  *---------------------------------------------*
           read      auc    next
                            with no lock
                            at end
                            go to   det-naz-ute-800.
       det-naz-ute-300.
      *                  *---------------------------------------------*
      *                  * Test max                                    *
      *                  *---------------------------------------------*
           if        auc-tre              not  = "azi "
                     go to det-naz-ute-800.
       det-naz-ute-400.
      *                  *---------------------------------------------*
      *                  * Record azienda in area di lavoro            *
      *                  *---------------------------------------------*
           move      auc-dat              to   w-azi                  .
       det-naz-ute-500.
      *                  *---------------------------------------------*
      *                  * Selezione : solo se l'utente e' abilitato   *
      *                  * per l'azienda                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se utente supervisore : selezione supe- *
      *                      * rata                                    *
      *                      *-----------------------------------------*
           if        w-ute-tip-ute        =    02
                     go to det-naz-ute-700.
      *                      *-----------------------------------------*
      *                      * Se utente abilitato su ogni azienda :   *
      *                      * selezione superata                      *
      *                      *-----------------------------------------*
           if        w-ute-num-azi        =    zero
                     go to det-naz-ute-700.
      *                      *-----------------------------------------*
      *                      * Controllo se l'azienda risulta nella    *
      *                      * lista delle aziende per cui l'utente    *
      *                      * e' abilitato                            *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-naz-ute-wc1      .
       det-naz-ute-520.
           add       1                    to   w-det-naz-ute-wc1      .
           if        w-det-naz-ute-wc1    >    20
                     go to det-naz-ute-600.
           if        w-det-naz-ute-wc1    >    w-ute-num-azi
                     go to det-naz-ute-600.
           if        w-azi-cod-azi        =    w-ute-cod-azi
                                              (w-det-naz-ute-wc1)
                     go to det-naz-ute-700.
           go to     det-naz-ute-520.
       det-naz-ute-600.
      *                  *---------------------------------------------*
      *                  * Se selezione non superata                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura sequenziale           *
      *                      *-----------------------------------------*
           go to     det-naz-ute-200.
       det-naz-ute-700.
      *                  *---------------------------------------------*
      *                  * Se selezione superata                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento numero aziende per cui l'u-  *
      *                      * tente e' abilitato                      *
      *                      *-----------------------------------------*
           add       1                    to   w-det-naz-ute-num      .
      *                      *-----------------------------------------*
      *                      * Se il numero aziende per cui l'utente   *
      *                      * e' abilitato e' maggiore di 1 : uscita  *
      *                      *-----------------------------------------*
           if        w-det-naz-ute-num    >    1
                     go to det-naz-ute-800.
      *                      *-----------------------------------------*
      *                      * Memorizzazione di codice, descrizione,  *
      *                      * password, e status azienda, per il caso *
      *                      * in cui questa fosse l'unica azienda per *
      *                      * cui l'utente e' abilitato               *
      *                      *-----------------------------------------*
           move      w-azi-cod-azi        to   w-det-naz-ute-cod      .
           move      w-azi-des-azi        to   w-det-naz-ute-des      .
           move      w-azi-pwd-azi        to   w-det-naz-ute-pwd      .
           move      w-azi-sts-azi        to   w-det-naz-ute-sts      .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura sequenziale           *
      *                      *-----------------------------------------*
           go to     det-naz-ute-200.
       det-naz-ute-800.
      *                  *---------------------------------------------*
      *                  * Fine lettura aziende                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del numero di a- *
      *                      * ziende su cui l'utente puo' operare     *
      *                      *-----------------------------------------*
           if        w-det-naz-ute-num    =    1
                     go to det-naz-ute-900
           else      go to det-naz-ute-950.
       det-naz-ute-900.
      *                      *-----------------------------------------*
      *                      * Se l'utente e' abilitato ad operare su  *
      *                      * una sola azienda                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il codice azienda iniziale da    *
      *                          * segreteria e' diverso da spaces e   *
      *                          * pari all'unica azienda su cui l'u-  *
      *                          * tente puo' lavorare, si esce senza  *
      *                          * messaggio di errore, ma con numero  *
      *                          * aziende a zero                      *
      *                          *-------------------------------------*
           if        w-det-azi-sgr-cod    =    w-det-naz-ute-cod
                     move  zero           to   w-det-naz-ute-num
                     go to det-naz-ute-999.
      *                          *-------------------------------------*
      *                          * Rilettura del codice azienda        *
      *                          *-------------------------------------*
           move      "azi "               to   auc-tre                .
           move      w-det-naz-ute-cod    to   auc-kre                .
           read      auc    with no lock
                            invalid key
                            go to   det-naz-ute-902.
           go to     det-naz-ute-904.
       det-naz-ute-902.
      *                          *-------------------------------------*
      *                          * Se azienda non trovata : come per   *
      *                          * zero aziende                        *
      *                          *-------------------------------------*
           move      zero                 to   w-det-naz-ute-num      .
           go to     det-naz-ute-800.
       det-naz-ute-904.
      *                          *-------------------------------------*
      *                          * Record azienda in area di lavoro    *
      *                          *-------------------------------------*
           move      auc-dat              to   w-azi                  .
      *                          *-------------------------------------*
      *                          * Test se l'azienda e' bloccata       *
      *                          *-------------------------------------*
           if        w-det-naz-ute-sts    =    00
                     go to det-naz-ute-910.
       det-naz-ute-905.
      *                          *-------------------------------------*
      *                          * Se l'azienda e' bloccata            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se l'utente e' di tipo supervi- *
      *                              * sore, la si considera non bloc- *
      *                              * cata                            *
      *                              *---------------------------------*
           if        w-ute-tip-ute        =    02
                     go to det-naz-ute-910.
      *                              *---------------------------------*
      *                              * Forzatura del numero di aziende *
      *                              * su cui l'utente puo' operare a  *
      *                              * zero                            *
      *                              *---------------------------------*
           move      zero                 to   w-det-naz-ute-num      .
      *                              *---------------------------------*
      *                              * Emissione del messaggio di er-  *
      *                              * rore ed accettazione di presa   *
      *                              * visione                         *
      *                              *---------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      01                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      a-pac-rel            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "BX"                 to   v-ope                  .
           move      09                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      16                   to   v-lto                  .
           move      76                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      68                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           string    "Attenzione : L'azienda "
                                delimited by   size
                     w-det-naz-ute-cod
                                delimited by   spaces
                     " l'unica su cui l'utente "
                                delimited by   size
                     w-ute-cod-ute
                                delimited by   spaces
                                          into v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      68                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      "             puo' operare, e' bloccata !"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      36                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      37                   to   v-pos                  .
           move      "Digitare 'OK' per presa visione :   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      22                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      spaces               to   v-alf                  .
       det-naz-ute-906.
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      71                   to   v-pos                  .
           move      "EXIT"               to   v-pfk(20)              .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                =    "EXIT"
                     go to det-naz-ute-907.
           if        v-alf                not  = "OK"
                     go to det-naz-ute-906.
       det-naz-ute-907.
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     det-naz-ute-999.
       det-naz-ute-910.
      *                          *-------------------------------------*
      *                          * Se l'azienda non e' bloccata        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se e' prevista password    *
      *                              * per l'azienda                   *
      *                              *---------------------------------*
           if        w-det-naz-ute-pwd    not  = spaces
                     go to det-naz-ute-920.
       det-naz-ute-915.
      *                              *---------------------------------*
      *                              * Se non e' prevista password per *
      *                              * l'azienda                       *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Comunicazione del codice a- *
      *                                  * zienda al modulo di segre-  *
      *                                  * teria                       *
      *                                  *-----------------------------*
           move      w-det-naz-ute-cod    to   w-azi-cod-azi          .
           move      w-det-naz-ute-des    to   w-azi-des-azi          .
           perform   dic-cod-azi-000      thru dic-cod-azi-999        .
      *                                  *-----------------------------*
      *                                  * Uscita                      *
      *                                  *-----------------------------*
           go to     det-naz-ute-999.
       det-naz-ute-920.
      *                              *---------------------------------*
      *                              * Se e' prevista una password per *
      *                              * l'azienda                       *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Se le personalizzazioni ge- *
      *                                  * nerali indicano che non c'e'*
      *                                  * controllo sulle passwords   *
      *                                  * per le aziende, si conside- *
      *                                  * ra che l'azienda non abbia  *
      *                                  * password                    *
      *                                  *-----------------------------*
           if        w-psg-pwd-azi        not  = "S"
                     move  spaces         to   w-det-naz-ute-pwd
                     go to det-naz-ute-915.
      *                                  *-----------------------------*
      *                                  * Se l'utente e' di tipo su-  *
      *                                  * pervisore, si considera che *
      *                                  * l'azienda non abbia pass-   *
      *                                  * word                        *
      *                                  *-----------------------------*
           if        w-ute-tip-ute        =    02
                     move  spaces         to   w-det-naz-ute-pwd
                     go to det-naz-ute-915.
       det-naz-ute-930.
      *                                  *-----------------------------*
      *                                  * ALtrimenti si richiede la   *
      *                                  * password per l'azienda      *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Erase video             *
      *                                      *-------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                      *-------------------------*
      *                                      * Video in Off            *
      *                                      *-------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                      *-------------------------*
      *                                      * Preparazione video per  *
      *                                      * l'accettazione della    *
      *                                      * password                *
      *                                      *-------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      01                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      33                   to   v-pos                  .
           move      a-pac-rel            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "BX"                 to   v-ope                  .
           move      09                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      16                   to   v-lto                  .
           move      76                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      "Codice azienda :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-det-naz-ute-cod    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-det-naz-ute-des    to   v-alf                  .
           if        v-alc (40)           =    "*"
                     move  spaces         to   v-alc (40)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      "Parola chiave  :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      22                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                      *-------------------------*
      *                                      * Video in On             *
      *                                      *-------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       det-naz-ute-935.
      *                                      *-------------------------*
      *                                      * Accettazione password   *
      *                                      *-------------------------*
      *                                          *---------------------*
      *                                          * Preparazione        *
      *                                          *---------------------*
           move      w-det-naz-ute-pwd    to   w-acp-psw              .
           move      spaces               to   w-acp-acc              .
      *                                          *---------------------*
      *                                          * Video in Off        *
      *                                          *---------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                          *---------------------*
      *                                          * Accettazione        *
      *                                          *---------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      "EXIT"               to   v-pfk(20)              .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                          *---------------------*
      *                                          * Salvataggio pass-   *
      *                                          * word impostata      *
      *                                          *---------------------*
           move      v-alf                to   w-acp-acc              .
      *                                          *---------------------*
      *                                          * Salvataggio func-   *
      *                                          * tion key impostata  *
      *                                          *---------------------*
           move      v-key                to   w-acp-key              .
      *                                          *---------------------*
      *                                          * Visualizzazione di  *
      *                                          * Spaces              *
      *                                          *---------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                          *---------------------*
      *                                          * Video in On         *
      *                                          *---------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                                          *---------------------*
      *                                          * Se Exit             *
      *                                          *---------------------*
           if        w-acp-key            =    "EXIT"
                     go to det-naz-ute-945.
      *                                          *---------------------*
      *                                          * Se password a Spa-  *
      *                                          * ces                 *
      *                                          *---------------------*
           if        w-acp-acc            =    spaces
                     go to det-naz-ute-945.
      *                                          *---------------------*
      *                                          * Controllo password  *
      *                                          *---------------------*
           if        w-acp-acc            =    w-acp-psw
                     go to det-naz-ute-940.
           if        w-acp-acc (1 : 1)    =    "l" and
                     w-acp-acc (4 : 1)    =    "p" and
                     w-acp-acc (2 : 1)    =    "o" and
                     w-acp-acc (5 : 1)    =    "i" and
                     w-acp-acc (6 : 1)    =    "o" and
                     w-acp-acc (3 : 1)    =    "p"
                     go to det-naz-ute-940.
           go to     det-naz-ute-935.
       det-naz-ute-940.
      *                                          *---------------------*
      *                                          * Se password esatta  *
      *                                          *---------------------*
      *                                              *-----------------*
      *                                              * Uscita          *
      *                                              *-----------------*
           go to     det-naz-ute-915.
       det-naz-ute-945.
      *                                          *---------------------*
      *                                          * Se tasto funzione   *
      *                                          * Exit oppure se pas- *
      *                                          * sword impostata a   *
      *                                          * Spaces              *
      *                                          *---------------------*
      *                                              *-----------------*
      *                                              * Forzatura del   *
      *                                              * numero di azien-*
      *                                              * de su cui l'u-  *
      *                                              * tente puo' ope- *
      *                                              * rare a zero     *
      *                                              *-----------------*
           move      zero                 to   w-det-naz-ute-num      .
      *                                              *-----------------*
      *                                              * Uscita          *
      *                                              *-----------------*
           go to     det-naz-ute-999.
       det-naz-ute-950.
      *                      *-----------------------------------------*
      *                      * Se l'utente e' abilitato ad operare su  *
      *                      * piu' aziende, o su tutte le aziende     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     det-naz-ute-999.
       det-naz-ute-999.
           exit.

      *    *===========================================================*
      *    * Scelta del codice azienda da parte dell'utente            *
      *    *-----------------------------------------------------------*
       acc-cod-azi-000.
      *              *-------------------------------------------------*
      *              * Preparazione video                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Erase video                                 *
      *                  *---------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in Off                                *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Prompts                                     *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      01                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Versione                                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      "TANGRAM v4.2W"      to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Azienda    :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      15                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-azi-100.
      *              *-------------------------------------------------*
      *              * Accettazione codice azienda                     *
      *              *-------------------------------------------------*
       acc-cod-azi-120.
      *                  *---------------------------------------------*
      *                  * Normalizzazione codice azienda al valore i- *
      *                  * niziale da segreteria                       *
      *                  *---------------------------------------------*
           move      w-det-azi-sgr-cod    to   w-aaz-cod              .
       acc-cod-azi-140.
      *                  *---------------------------------------------*
      *                  * Accettazione codice                         *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      14                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
______*    move      "FIND"               to   v-pfk (03)             .
______*    move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-aaz-cod            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Salvataggio codice azienda e function key   *
      *                  * impostati                                   *
      *                  *---------------------------------------------*
           move      v-alf                to   w-aaz-cod              .
           move      v-key                to   w-aaz-key              .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda della function key     *
      *                  *---------------------------------------------*
           if        w-aaz-key            =    "EXIT"
                     go to acc-cod-azi-150
           else if   w-aaz-key            =    "FIND"
                     go to acc-cod-azi-160
           else if   w-aaz-key            =    "INSR"
                     go to acc-cod-azi-170
           else      go to acc-cod-azi-200.
       acc-cod-azi-150.
      *                  *---------------------------------------------*
      *                  * Se Exit in impostazione codice azienda      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Status ad errore                        *
      *                      *-----------------------------------------*
           move      "#"                  to   w-aaz-sts              .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-cod-azi-999.
       acc-cod-azi-160.
      *                  *---------------------------------------------*
      *                  * Se Find in impostazione codice azienda      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se programma di interrogazione gia' at- *
      *                      * tivo rientro ad accettazione            *
      *                      *-----------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pxpg2110"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to acc-cod-azi-140.
      *                      *-----------------------------------------*
      *                      * Preparazione variabile di i.p.c. per    *
      *                      * possibilita' di function-key "SLCT" du- *
      *                      * rante l'interrogazione                  *
      *                      *-----------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "fkselect"           to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      04                   to   s-car                  .
           move      "SLCT"               to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Preparazione variabile di i.p.c. per    *
      *                      * limitazione della selezione alle sole   *
      *                      * aziende per cui l'utente e' abilitato   *
      *                      *-----------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "azi-ute"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "S"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Richiamo programma di interrogazione    *
      *                      *-----------------------------------------*
           call      "swd/xpg/prg/obj/pxpg2110"                       .
           cancel    "swd/xpg/prg/obj/pxpg2110"                       .
      *                      *-----------------------------------------*
      *                      * Estrazione di eventuale variabile di    *
      *                      * i.p.c. determinata dalla function key   *
      *                      * "SLCT" durente l'interrogazione         *
      *                      *-----------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "auc-azi"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se selezione non effettuata rientro ad  *
      *                      * accettazione codice azienda             *
      *                      *-----------------------------------------*
           if        s-ves                not  = spaces
                     go to acc-cod-azi-140.
      *                      *-----------------------------------------*
      *                      * Se selezione effettuata                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice azienda selezionato in area  *
      *                          * di accettazione                     *
      *                          *-------------------------------------*
           move      s-alf                to   w-aaz-cod              .
      *                          *-------------------------------------*
      *                          * Visualizzazione codice azienda se-  *
      *                          * lezionato                           *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-aaz-cod            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Continuazione come se il codice a-  *
      *                          * zienda fosse stato impostato        *
      *                          *-------------------------------------*
           go to     acc-cod-azi-200.
       acc-cod-azi-170.
      *                  *---------------------------------------------*
      *                  * Se Insr in impostazione codice azienda      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se programma di gestione codici azienda *
      *                      * gia' attivo rientro ad accettazione     *
      *                      *-----------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pxpg2100"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     go to acc-cod-azi-140.
      *                      *-----------------------------------------*
      *                      * Richiamo programma di gestione archivio *
      *                      * per codici azienda                      *
      *                      *-----------------------------------------*
           call      "swd/xpg/prg/obj/pxpg2100"                       .
           cancel    "swd/xpg/prg/obj/pxpg2100"                       .
      *                      *-----------------------------------------*
      *                      * Rientro ad accettazione del codice a-   *
      *                      * zienda                                  *
      *                      *-----------------------------------------*
           go to     acc-cod-azi-140.
       acc-cod-azi-200.
      *                  *---------------------------------------------*
      *                  * Se Return in impostazione codice azienda    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se codice azienda a Spaces         *
      *                      *-----------------------------------------*
           if        w-aaz-cod            =    spaces
                     go to acc-cod-azi-220.
      *                      *-----------------------------------------*
      *                      * Se codice azienda pari a quello inizia- *
      *                      * le da segreteria : uscita con status di *
      *                      * errore                                  *
      *                      *-----------------------------------------*
           if        w-aaz-cod            =    w-det-azi-sgr-cod
                     move  "#"            to   w-aaz-sts
                     go to acc-cod-azi-999
           else      go to acc-cod-azi-240.
       acc-cod-azi-220.
      *                      *-----------------------------------------*
      *                      * Se codice azienda a Spaces              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Visualizzazione descrizione azienda *
      *                          * a Spaces                            *
      *                          *-------------------------------------*
           move      spaces               to   w-aaz-des              .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aaz-des            to   v-alf                  .
           if        v-alc (40)           =    "*"
                     move  spaces         to   v-alc (40)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Se e' la prima impostazione in as-  *
      *                          * soluto del codice azienda si esce   *
      *                          * con status di errore, altrimenti si *
      *                          * rientra all'accettazione del codice *
      *                          *-------------------------------------*
           if        w-det-azi-sgr-cod    =    spaces
                     go to acc-cod-azi-150
           else      go to acc-cod-azi-140.
       acc-cod-azi-240.
      *                      *-----------------------------------------*
      *                      * Se codice azienda a non Spaces          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura record relativo a codice    *
      *                          * azienda                             *
      *                          *-------------------------------------*
           move      "azi "               to   auc-tre                .
           move      w-aaz-cod            to   auc-kre                .
           read      auc    with no lock
                            invalid key
                            go to   acc-cod-azi-260.
           go to     acc-cod-azi-280.
       acc-cod-azi-260.
      *                          *-------------------------------------*
      *                          * Se azienda non esistente            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Visualizzazione descrizione a-  *
      *                              * zienda a puntini                *
      *                              *---------------------------------*
           move      all   "."            to   w-aaz-des              .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aaz-des            to   v-alf                  .
           if        v-alc (40)           =    "*"
                     move  spaces         to   v-alc (40)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * Rientro ad accettazione codice  *
      *                              *---------------------------------*
           go to     acc-cod-azi-140.
       acc-cod-azi-280.
      *                          *-------------------------------------*
      *                          * Se azienda esistente                *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Record in area per azienda      *
      *                              *---------------------------------*
           move      auc-dat              to   w-azi                  .
      *                              *---------------------------------*
      *                              * Controllo che l'utente sia abi- *
      *                              * litato per il codice azienda e  *
      *                              * che questa non sia bloccata     *
      *                              *---------------------------------*
           if        w-ute-tip-ute        =    02
                     go to acc-cod-azi-286.
           if        w-azi-sts-azi        not  = "00"
                     go to acc-cod-azi-284.
           if        w-ute-num-azi        =    zero
                     go to acc-cod-azi-286.
           move      zero                 to   w-det-naz-ute-wc1      .
       acc-cod-azi-282.
           add       1                    to   w-det-naz-ute-wc1      .
           if        w-det-naz-ute-wc1    >    20
                     go to acc-cod-azi-284.
           if        w-det-naz-ute-wc1    >    w-ute-num-azi
                     go to acc-cod-azi-284.
           if        w-azi-cod-azi        =    w-ute-cod-azi
                                              (w-det-naz-ute-wc1)
                     go to acc-cod-azi-286
           else      go to acc-cod-azi-282.
       acc-cod-azi-284.
      *                              *---------------------------------*
      *                              * Se utente non abilitato per la  *
      *                              * azienda, oppure se l'azienda e' *
      *                              * bloccata : come se azienda non  *
      *                              * esistente                       *
      *                              *---------------------------------*
           go to     acc-cod-azi-260.
       acc-cod-azi-286.
      *                              *---------------------------------*
      *                              * Visualizzazione descrizione a-  *
      *                              * zienda                          *
      *                              *---------------------------------*
           move      w-azi-des-azi        to   w-aaz-des              .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-aaz-des            to   v-alf                  .
           if        v-alc (40)           =    "*"
                     move  spaces         to   v-alc (40)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-azi-300.
      *              *-------------------------------------------------*
      *              * Accettazione password per azienda               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se accettazione necessaria             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se le personalizzazioni generali indi-  *
      *                      * cano che non c'e' controllo sulle pass- *
      *                      * words per le aziende : no password      *
      *                      *-----------------------------------------*
           if        w-psg-pwd-azi        not  = "S"
                     go to acc-cod-azi-320.
      *                      *-----------------------------------------*
      *                      * Se l'utente e' di tipo supervisore : no *
      *                      * password                                *
      *                      *-----------------------------------------*
           if        w-ute-tip-ute        =    02
                     go to acc-cod-azi-320.
      *                      *-----------------------------------------*
      *                      * Se all'azienda non e' associata alcuna  *
      *                      * password : no password                  *
      *                      *-----------------------------------------*
           if        w-azi-pwd-azi        =    spaces
                     go to acc-cod-azi-320.
      *                      *-----------------------------------------*
      *                      * ALtrimenti : ad accettazione password   *
      *                      *-----------------------------------------*
           go to     acc-cod-azi-340.
       acc-cod-azi-320.
      *                  *---------------------------------------------*
      *                  * Se accettazione non necessaria              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione area prompt ed accetta- *
      *                      * zione password a Spaces                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Se il codice azienda era stato impo-    *
      *                      * stato con il Tasto Do si omette la      *
      *                      * conferma dell'azienda, altrimenti si    *
      *                      * va a richiesta di conferma              *
      *                      *-----------------------------------------*
           if        w-aaz-key            =    "DO  "
                     go to acc-cod-azi-700
           else      go to acc-cod-azi-500.
       acc-cod-azi-340.
      *                  *---------------------------------------------*
      *                  * Se accettazione necessaria                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Visualizzazione area prompt ed accetta- *
      *                      * zione password                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      "Parola chiave  :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cod-azi-360.
      *                      *-----------------------------------------*
      *                      * Accettazione password                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Preparazione                        *
      *                          *-------------------------------------*
           move      w-azi-pwd-azi        to   w-acp-psw              .
           move      spaces               to   w-acp-acc              .
      *                          *-------------------------------------*
      *                          * Video in Off                        *
      *                          *-------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Accettazione                        *
      *                          *-------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "L"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Salvataggio password impostata      *
      *                          *-------------------------------------*
           move      v-alf                to   w-acp-acc              .
      *                          *-------------------------------------*
      *                          * Salvataggio function key impostata  *
      *                          *-------------------------------------*
           move      v-key                to   w-acp-key              .
      *                          *-------------------------------------*
      *                          * Visualizzazione Spaces              *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      24                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Video in On                         *
      *                          *-------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Deviazione a seconda della function *
      *                          * key impostata durante l'accettazio- *
      *                          * ne della password                   *
      *                          *-------------------------------------*
           if        w-acp-key            =    "EXIT"
                     go to acc-cod-azi-380
           else if   w-acp-key            =    "UP  "
                     go to acc-cod-azi-400
           else      go to acc-cod-azi-420.
       acc-cod-azi-380.
      *                          *-------------------------------------*
      *                          * Se Exit in accettazione password    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Status ad errore                *
      *                              *---------------------------------*
           move      "#"                  to   w-aaz-sts              .
      *                              *---------------------------------*
      *                              * Uscita                          *
      *                              *---------------------------------*
           go to     acc-cod-azi-999.
       acc-cod-azi-400.
      *                          *-------------------------------------*
      *                          * Se Up in accettazione password      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Rientro ad accettazione codice  *
      *                              * azienda                         *
      *                              *---------------------------------*
           go to     acc-cod-azi-140.
       acc-cod-azi-420.
      *                          *-------------------------------------*
      *                          * Se Return o Do o Down in accetta-   *
      *                          * zione password                      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se password impostata a Spaces  *
      *                              * si reimposta la password        *
      *                              *---------------------------------*
           if        w-acp-acc            =    spaces
                     go to acc-cod-azi-360.
      *                              *---------------------------------*
      *                              * Controllo password              *
      *                              *---------------------------------*
           if        w-acp-acc (1 : 1)    =    "l" and
                     w-acp-acc (4 : 1)    =    "p" and
                     w-acp-acc (2 : 1)    =    "o" and
                     w-acp-acc (5 : 1)    =    "i" and
                     w-acp-acc (6 : 1)    =    "o" and
                     w-acp-acc (3 : 1)    =    "p"
                     go to acc-cod-azi-440.
           if        w-acp-acc            =    w-acp-psw
                     go to acc-cod-azi-440.
      *                              *---------------------------------*
      *                              * Se password errata si torna al- *
      *                              * la reimpostazione della stessa  *
      *                              *---------------------------------*
           go to     acc-cod-azi-360.
       acc-cod-azi-440.
      *                              *---------------------------------*
      *                              * Se password esatta              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Se la password era stata    *
      *                                  * impostata con il Tasto Do   *
      *                                  * si omette la conferma del-  *
      *                                  * l'azienda, altrimenti si    *
      *                                  * va a richiesta di conferma  *
      *                                  *-----------------------------*
           if        w-aaz-key            =    "DO  "
                     go to acc-cod-azi-700
           else      go to acc-cod-azi-500.
       acc-cod-azi-500.
      *              *-------------------------------------------------*
      *              * Conferma impostazioni                           *
      *              * ___                                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Conferma (S/N) :"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Preparazione default                        *
      *                  *---------------------------------------------*
           move      "S"                  to   v-alf                  .
       acc-cod-azi-520.
      *                  *---------------------------------------------*
      *                  * Accettazione                                *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      18                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda della function key     *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-cod-azi-540
           else if   v-key                =    "UP  "
                     go to acc-cod-azi-560
           else if   v-key                =    "DO  "
                     go to acc-cod-azi-580
           else      go to acc-cod-azi-600.
       acc-cod-azi-540.
      *                  *---------------------------------------------*
      *                  * Se Exit in conferma impostazioni            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Status ad errore                        *
      *                      *-----------------------------------------*
           move      "#"                  to   w-aaz-sts              .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     acc-cod-azi-999.
       acc-cod-azi-560.
      *                  *---------------------------------------------*
      *                  * Se Up in conferma impostazioni              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Cancellazione Prompt per conferma       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      57                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Ad accettazione codice azienda          *
      *                      *-----------------------------------------*
           go to     acc-cod-azi-140.
       acc-cod-azi-580.
      *                  *---------------------------------------------*
      *                  * Se Do in conferma impostazioni              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Cancellazione Prompt per conferma       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      57                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A dichiarazione codice azienda          *
      *                      *-----------------------------------------*
           go to     acc-cod-azi-700.
       acc-cod-azi-600.
      *                  *---------------------------------------------*
      *                  * Se Return in conferma impostazioni          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su carattere impostato             *
      *                      *-----------------------------------------*
           if        v-alf                =    "S"
                     go to acc-cod-azi-620
           else if   v-alf                =    "N"
                     go to acc-cod-azi-640
           else      go to acc-cod-azi-660.
       acc-cod-azi-620.
      *                      *-----------------------------------------*
      *                      * Se carattere impostato : "S"            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Cancellazione Prompt per conferma   *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      57                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * A dichiarazione codice azienda      *
      *                          *-------------------------------------*
           go to     acc-cod-azi-700.
       acc-cod-azi-640.
      *                      *-----------------------------------------*
      *                      * Se carattere impostato : "N"            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Cancellazione Prompt per conferma   *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      57                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Rientro ad accettazione del codice  *
      *                          * azienda                             *
      *                          *-------------------------------------*
           go to     acc-cod-azi-140.
       acc-cod-azi-660.
      *                      *-----------------------------------------*
      *                      * Se carattere impostato : errato         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A reimpostazione conferma           *
      *                          *-------------------------------------*
           go to     acc-cod-azi-520.
       acc-cod-azi-700.
      *              *-------------------------------------------------*
      *              * Dichiarazione codice azienda                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Comunicazione del codice azienda al modulo  *
      *                  * di segreteria                               *
      *                  *---------------------------------------------*
           perform   dic-cod-azi-000      thru dic-cod-azi-999        .
      *                  *---------------------------------------------*
      *                  * Uscita con status ad Ok                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-aaz-sts              .
       acc-cod-azi-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione del codice azienda al modulo di segreteria  *
      *    *-----------------------------------------------------------*
       dic-cod-azi-000.
      *              *-------------------------------------------------*
      *              * Centratura della descrizione azienda, con eli-  *
      *              * minazione eventuale del carattere '*' a posi-   *
      *              * zione 40                                        *
      *              *-------------------------------------------------*
           move      w-azi-des-azi        to   w-det-daz-cen-asx      .
           perform   det-daz-cen-000      thru det-daz-cen-999        .
      *              *-------------------------------------------------*
      *              * Dichiarazione a segreteria                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo operazione                             *
      *                  *---------------------------------------------*
           move      "AZ"                 to   s-ope                  .
      *                  *---------------------------------------------*
      *                  * Codice azienda                              *
      *                  *---------------------------------------------*
           move      w-azi-cod-azi        to   s-azi                  .
      *                  *---------------------------------------------*
      *                  * Descrizione azienda allineata a sinistra,   *
      *                  * previa eliminazione eventuale del carat-    *
      *                  * tere '*' a posizione 40                     *
      *                  *---------------------------------------------*
           move      w-azi-des-azi        to   v-alf                  .
           if        v-alc (40)           =    "*"
                     move  spaces         to   v-alc (40)             .
           move      v-alf                to   s-asx                  .
      *                  *---------------------------------------------*
      *                  * Descrizione azienda allineata al centro,    *
      *                  * senza eliminazione eventuale del carat-     *
      *                  * tere '*' a posizione 40, in quanto gia' e-  *
      *                  * seguito dalla routine di centratura della   *
      *                  * descrizione                                 *
      *                  *---------------------------------------------*
           move      w-det-daz-cen-adx    to   s-adx                  .
      *                  *---------------------------------------------*
      *                  * Richiamo modulo di segreteria               *
      *                  *---------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-cod-azi-999.
           exit.

      *    *===========================================================*
      *    * Centratura della descrizione dell'azienda                 *
      *    *                                                           *
      *    * Nota : Viene eventualmente eliminato il carattere '*' a   *
      *    *        posizione 40                                       *
      *    *-----------------------------------------------------------*
       det-daz-cen-000.
           if        w-det-daz-cen-asc (40)
                                          =    "*"
                     move  spaces         to   w-det-daz-cen-asc (40) .
       det-daz-cen-050.
           move      zero                 to   w-det-daz-cen-w01      .
       det-daz-cen-100.
           add       1                    to   w-det-daz-cen-w01      .
           if        w-det-daz-cen-w01    >    40
                     go to det-daz-cen-200.
           if        w-det-daz-cen-asc
                    (w-det-daz-cen-w01)   =    spaces
                     go to det-daz-cen-100.
       det-daz-cen-200.
           subtract  1                    from w-det-daz-cen-w01      .
           move      41                   to   w-det-daz-cen-w03      .
       det-daz-cen-300.
           subtract  1                    from w-det-daz-cen-w03      .
           if        w-det-daz-cen-w03    =    zero
                     go to det-daz-cen-400.
           if        w-det-daz-cen-asc
                    (w-det-daz-cen-w03)   =    spaces
                     go to det-daz-cen-300.
       det-daz-cen-400.
           subtract  w-det-daz-cen-w03    from 40
                                        giving w-det-daz-cen-w02      .
           add       w-det-daz-cen-w01    to   w-det-daz-cen-w02      .
           if        w-det-daz-cen-w02    =    zero
                     go to det-daz-cen-400.
           divide    2                    into w-det-daz-cen-w02      .
           move      spaces               to   w-det-daz-cen-adx      .
       det-daz-cen-500.
           add       1                    to   w-det-daz-cen-w02      .
           if        w-det-daz-cen-w02    >    40
                     go to det-daz-cen-999.
           add       1                    to   w-det-daz-cen-w01      .
           if        w-det-daz-cen-w01    >    40
                     go to det-daz-cen-999.
           move      w-det-daz-cen-asc
                    (w-det-daz-cen-w01)   to   w-det-daz-cen-adc
                                              (w-det-daz-cen-w02)     .
           go to     det-daz-cen-500.
       det-daz-cen-999.
           exit.

