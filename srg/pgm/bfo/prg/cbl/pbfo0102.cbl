       Identification Division.
       Program-Id.                                 pbfo0102           .
      *================================================================*
      *                                                                *
      *          Gestione catena mov per programma pbfo0100            *
      *                                                                *
      * ============================================================== *
      * Tipi operazione                                                *
      *                                                                *
      * "OP" - Open, inizio utilizzo gestione catena                   *
      *                                                                *
      *             Input  : w-cat-rig-ope = "OP"                      *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      * "CL" - Close, fine utilizzo gestione catena                    *
      *                                                                *
      *             Input  : w-cat-rig-ope = "CL"                      *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      * "BE" - Begin, inizio nuova catena                              *
      *                                                                *
      *             Input  : w-cat-rig-ope = "BE"                      *
      *                                                                *
      *             Output : w-cat-rig-cur = zero                      *
      *                      w-cat-rig-max = zero                      *
      *                      w-cat-rig-prg = zero                      *
      *                      w-cat-rig-app = spaces                    *
      *                      w-cat-rig-ins = #                         *
      *                      w-cat-rig-new = spaces                    *
      *                      w-cat-rig-lst = spaces                    *
      *                                                                *
      * "PT" - Put Record, inserimento record pieno a fine catena      *
      *                                                                *
      *             Input  : w-cat-rig-ope = "PT"                      *
      *                      w-cat-rig-prg = numero progressivo da at- *
      *                                      tribuire al record da     *
      *                                      aggiungere in coda        *
      *                      w-cat-rig-buf = buffer dati               *
      *                                                                *
      *             Output : w-cat-rig-exs = return status code        *
      *                      w-cat-rig-cur = numero d'ordine del re-   *
      *                                      cord corrente aggiunto    *
      *                      w-cat-rig-max = aumentato di 1            *
      *                      w-cat-rig-prg = numero progressivo attri- *
      *                                      buito al record corrente  *
      *                                      aggiunto                  *
      *                      w-cat-rig-app = spaces oppure #           *
      *                      w-cat-rig-ins = spaces oppure #           *
      *                      w-cat-rig-new = spaces                    *
      *                      w-cat-rig-lst = #                         *
      *                                                                *
      * "UP" - Update, aggiornamento record in catena                  *
      *                                                                *
      *             Input  : w-cat-rig-ope = "UP"                      *
      *                      w-cat-rig-num = numero d'ordine del re-   *
      *                                      cord da aggiornare        *
      *                      w-cat-rig-buf = buffer dati               *
      *                                                                *
      *             Output : w-cat-rig-exs = return status code        *
      *                      w-cat-rig-cur = invariato                 *
      *                      w-cat-rig-max = invariato                 *
      *                      w-cat-rig-prg = invariato                 *
      *                      w-cat-rig-app = invariato                 *
      *                      w-cat-rig-ins = invariato                 *
      *                      w-cat-rig-new = invariato                 *
      *                      w-cat-rig-lst = invariato                 *
      *                                                                *
      * "ST" - Start, posizionamento su catena                         *
      *                                                                *
      *             Input  : w-cat-rig-ope = "ST"                      *
      *                      w-cat-rig-num = numero d'ordine del re-   *
      *                                      cord su cui posizionarsi  *
      *                                                                *
      *             Output : w-cat-rig-exs = return status code        *
      *                                                                *
      * "RN" - Read Next, lettura sequenziale della catena             *
      *                                                                *
      *             Input  : w-cat-rig-ope = "RN"                      *
      *                                                                *
      *             Output : w-cat-rig-exs = return status code        *
      *                      w-cat-rig-cur = numero d'ordine del re-   *
      *                                      cord corrente letto       *
      *                      w-cat-rig-max = invariato                 *
      *                      w-cat-rig-prg = numero progressivo del    *
      *                                      record corrente letto     *
      *                      w-cat-rig-app = invariato                 *
      *                      w-cat-rig-ins = spaces oppure #           *
      *                      w-cat-rig-new = spaces                    *
      *                      w-cat-rig-lst = spaces oppure #           *
      *                      w-cat-rig-buf = buffer dati               *
      *                                                                *
      * "RD" - Read, lettura casuale della catena                      *
      *                                                                *
      *             Input  : w-cat-rig-ope = "RD"                      *
      *                      w-cat-rig-num = numero d'ordine del re-   *
      *                                      cord da leggere           *
      *                                                                *
      *             Output : w-cat-rig-exs = return status code        *
      *                      w-cat-rig-cur = numero d'ordine del re-   *
      *                                      cord corrente letto       *
      *                      w-cat-rig-max = invariato                 *
      *                      w-cat-rig-prg = numero progressivo del    *
      *                                      record corrente letto     *
      *                      w-cat-rig-app = invariato                 *
      *                      w-cat-rig-ins = spaces oppure #           *
      *                      w-cat-rig-new = spaces                    *
      *                      w-cat-rig-lst = spaces oppure #           *
      *                      w-cat-rig-buf = buffer dati               *
      *                                                                *
      * "IN" - Insert Record, inserimento record vuoto nella catena    *
      *                                                                *
      *             Input  : w-cat-rig-ope = "IN"                      *
      *                      w-cat-rig-num = numero d'ordine del re-   *
      *                                      cord prima del quale de-  *
      *                                      ve essere inserito il re- *
      *                                      cord da inserire          *
      *                                                                *
      *             Output : w-cat-rig-exs = return status code        *
      *                      w-cat-rig-cur = numero d'ordine del re-   *
      *                                      cord corrente inserito    *
      *                      w-cat-rig-max = aumentato di 1            *
      *                      w-cat-rig-prg = numero progressivo attri- *
      *                                      buito al record corrente  *
      *                                      inserito                  *
      *                      w-cat-rig-app = spaces oppure #           *
      *                      w-cat-rig-ins = spaces oppure #           *
      *                      w-cat-rig-new = #                         *
      *                      w-cat-rig-lst = spaces                    *
      *                                                                *
      * "AP" - Append Record, inserimento record vuoto a fine catena   *
      *                                                                *
      *             Input  : w-cat-rig-ope = "AP"                      *
      *                                                                *
      *             Output : w-cat-rig-exs = return status code        *
      *                      w-cat-rig-cur = numero d'ordine del re-   *
      *                                      cord corrente aggiunto    *
      *                      w-cat-rig-max = aumentato di 1            *
      *                      w-cat-rig-prg = numero progressivo attri- *
      *                                      buito al record corrente  *
      *                                      aggiunto                  *
      *                      w-cat-rig-app = spaces oppure #           *
      *                      w-cat-rig-ins = spaces oppure #           *
      *                      w-cat-rig-new = #                         *
      *                      w-cat-rig-lst = #                         *
      *                                                                *
      * "RM" - Remove Record, rimozione record dalla catena            *
      *                                                                *
      *             Input  : w-cat-rig-ope = "RM"                      *
      *                      w-cat-rig-num = numero d'ordine del re-   *
      *                                      cord da rimuovere         *
      *                                                                *
      *             Output : w-cat-rig-exs = return status code        *
      *                      w-cat-rig-cur = numero d'ordine del re-   *
      *                                      cord successivo a quel-   *
      *                                      lo rimosso                *
      *                      w-cat-rig-max = diminuito di 1            *
      *                      w-cat-rig-prg = numero progressivo del    *
      *                                      record successivo a       *
      *                                      quello rimosso            *
      *                      w-cat-rig-app = spaces oppure #           *
      *                      w-cat-rig-ins = spaces oppure #           *
      *                      w-cat-rig-new = spaces oppure #           *
      *                      w-cat-rig-lst = spaces oppure #           *
      *                      w-cat-rig-buf = buffer dati relativo ad   *
      *                                      elemento successivo  al   *
      *                                      rimosso                   *
      *                                                                *
      * "LR" - Last Removed, ultimo record rimosso della catena        *
      *                                                                *
      *             Input  : w-cat-rig-ope = "LR"                      *
      *                                                                *
      *             Output : w-cat-rig-exs = return status code        *
      *                      w-cat-rig-buf = buffer dati               *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [fil]                                        *
      *    *-----------------------------------------------------------*
           select  optional  fil   assign  to disk     w-fil-pat
                             organization  is relative
                             access   mode is dynamic
                             relative key  is          w-rcn-krn
                             file status   is          w-fil-sts      .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [fil]                                    *
      *    *-----------------------------------------------------------*
       fd  fil           label record standard                        .
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  fil-rec.
      *        *-------------------------------------------------------*
      *        * Area dati in grado di ospitare l'area w-rig del pro-  *
      *        * gramma chiamante                                      *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  filler occurs 200      pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * File area per [fil]                                       *
      *    *-----------------------------------------------------------*
       01  w-fil.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  w-fil-nam                  pic  x(04) value "tmp "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  w-fil-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  w-fil-sts                  pic  x(02) value "00"       .

      *    *===========================================================*
      *    * Work-area per key-record number                           *
      *    *-----------------------------------------------------------*
       01  w-rcn.
      *        *-------------------------------------------------------*
      *        * File key record number                                *
      *        *-------------------------------------------------------*
           05  w-rcn-krn                  pic  9(05)                  .

      *    *===========================================================*
      *    * Work per la gestione della catena                         *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Record number primo elemento catena                   *
      *        *-------------------------------------------------------*
           05  w-wrk-rcn-000             pic  9(05)                   .
      *        *-------------------------------------------------------*
      *        * Record number ultimo elemento catena                  *
      *        *-------------------------------------------------------*
           05  w-wrk-rcn-999             pic  9(05)                   .
      *        *-------------------------------------------------------*
      *        * Record number ultimo elemento rimosso                 *
      *        *-------------------------------------------------------*
           05  w-wrk-rcn-uer             pic  9(05)                   .
      *        *-------------------------------------------------------*
      *        * Record number elemento catena da trattare             *
      *        *-------------------------------------------------------*
           05  w-wrk-rec-num             pic  9(05)                   .
      *        *-------------------------------------------------------*
      *        * Record number relativo allo start-pointer             *
      *        *-------------------------------------------------------*
           05  w-wrk-str-rcn             pic  9(05)                   .
      *        *-------------------------------------------------------*
      *        * Numero d'ordine relativo allo start-pointer           *
      *        *-------------------------------------------------------*
           05  w-wrk-str-ord             pic  9(05)                   .
      *        *-------------------------------------------------------*
      *        * Puntatore al record da rimuovere                      *
      *        *-------------------------------------------------------*
           05  w-wrk-pnt-rmv             pic  9(05)                   .
      *        *-------------------------------------------------------*
      *        * Numero d'ordine record da rimuovere                   *
      *        *-------------------------------------------------------*
           05  w-wrk-ord-rmv             pic  9(05)                   .
      *        *-------------------------------------------------------*
      *        * Puntatore generico                                    *
      *        *-------------------------------------------------------*
           05  w-wrk-pnt-gen             pic  9(05)                   .
      *        *-------------------------------------------------------*
      *        * Puntatore per calcoli su numeri progressivi           *
      *        *-------------------------------------------------------*
           05  w-wrk-pnt-prg             pic  9(05)                   .

      *    *===========================================================*
      *    * Work per manipolazione numeri progressivi                 *
      *    *-----------------------------------------------------------*
       01  w-prg.
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo minimo                        *
      *        *-------------------------------------------------------*
           05  w-prg-rig-min              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo massimo                       *
      *        *-------------------------------------------------------*
           05  w-prg-rig-max              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo record precedente             *
      *        *-------------------------------------------------------*
           05  w-prg-rig-pre              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo record successivo             *
      *        *-------------------------------------------------------*
           05  w-prg-rig-suc              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo per inserimento               *
      *        *-------------------------------------------------------*
           05  w-prg-rig-ins              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo per append                    *
      *        *-------------------------------------------------------*
           05  w-prg-rig-app              pic  9(05)                  .

      *    *===========================================================*
      *    * Work per puntatori                                        *
      *    *-----------------------------------------------------------*
       01  w-pnt.
      *        *-------------------------------------------------------*
      *        * Massimo numero elementi gestiti nella tabella         *
      *        *-------------------------------------------------------*
           05  w-pnt-max-ele              pic  9(05) value 999        .
      *        *-------------------------------------------------------*
      *        * Numero elementi memorizzati                           *
      *        *-------------------------------------------------------*
           05  w-pnt-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Elementi, circa il doppio del massimo numero elementi *
      *        * gestiti nella tabella                                 *
      *        *-------------------------------------------------------*
           05  w-pnt-ele-cat occurs 2000.
      *            *---------------------------------------------------*
      *            * Record number elemento successivo                 *
      *            *---------------------------------------------------*
               10  w-pnt-nxt-ele          pic  9(05) comp-3           .
      *            *---------------------------------------------------*
      *            * Record number elemento precedente                 *
      *            *---------------------------------------------------*
               10  w-pnt-prv-ele          pic  9(05) comp-3           .
      *            *---------------------------------------------------*
      *            * Numero progressivo elemento                       *
      *            *---------------------------------------------------*
               10  w-pnt-num-prg          pic  9(05) comp-3           .
      *            *---------------------------------------------------*
      *            * Flag di elemento New                              *
      *            *---------------------------------------------------*
               10  w-pnt-ele-new          pic  x(01)                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Cache memory per i primi 20 elementi                      *
      *    *-----------------------------------------------------------*
       01  w-chm.
      *        *-------------------------------------------------------*
      *        * Numero di elementi nella cache memory                 *
      *        *-------------------------------------------------------*
           05  w-chm-max                  pic  9(02)       value 20   .
      *        *-------------------------------------------------------*
      *        * Elementi, tanti quanti definiti nel campo precedente  *
      *        *-------------------------------------------------------*
           05  w-chm-tbl occurs 20.
      *            *---------------------------------------------------*
      *            * Elemento in grado di ospitare l'area w-rig del    *
      *            * programma chiamante                               *
      *            *---------------------------------------------------*
               10  w-chm-ele.
                   15  filler occurs 200  pic  x(01)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per gestione catena mov             *
      *    *-----------------------------------------------------------*
       01  w-cat-rig.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  w-cat-rig-ope              pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        * - Spaces : operazione eseguita                        *
      *        * - #      : errore di esecuzione                       *
      *        *-------------------------------------------------------*
           05  w-cat-rig-exs              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero d'ordine del record interessato                *
      *        *-------------------------------------------------------*
           05  w-cat-rig-num              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero d'ordine del record corrente                   *
      *        *-------------------------------------------------------*
           05  w-cat-rig-cur              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo record corrente               *
      *        *-------------------------------------------------------*
           05  w-cat-rig-prg              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero d'ordine dell'ultimo record                    *
      *        *-------------------------------------------------------*
           05  w-cat-rig-max              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Flag di append possibile                              *
      *        *-------------------------------------------------------*
           05  w-cat-rig-app              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di inserimento possibile                         *
      *        *-------------------------------------------------------*
           05  w-cat-rig-ins              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di record New                                    *
      *        *-------------------------------------------------------*
           05  w-cat-rig-new              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di record Last                                   *
      *        *-------------------------------------------------------*
           05  w-cat-rig-lst              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Buffer dati, in grado di ospitare l'area w-rig del    *
      *        * programma chiamante                                   *
      *        *-------------------------------------------------------*
           05  w-cat-rig-buf.
               10  filler occurs 200      pic  x(01)                  .

      ******************************************************************
       Procedure Division                using w-cat-rig              .
      ******************************************************************

      *    *===========================================================*
      *    * Declaratives                                              *
      *    *-----------------------------------------------------------*
       Declaratives.
       Decl Section.
           Use after standard error procedure on fil                  .
       decl-000.
      *              *-------------------------------------------------*
      *              * Traslazione del codice di i-o status contenuto  *
      *              * in f-auc-sts nel codice di i-o status conven-   *
      *              * zionale                                         *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/miosts"
                                         using w-fil                  .
      *              *-------------------------------------------------*
      *              * Spostamento cobol-file-status in area per defi- *
      *              * nizione codici di errore di i-o                 *
      *              *-------------------------------------------------*
           move      w-fil-sts            to   e-sts                  .
      *              *-------------------------------------------------*
      *              * Test su tipo di errore intervenuto. Se l'errore *
      *              * non rientra tra i seguenti si termina l'esecu-  *
      *              * zione del programma con segnalazione di errore  *
      *              * fatale.                                         *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-lst and
                     e-sts                not  = e-end-fil and
                     e-sts                not  = e-dup-key and
                     e-sts                not  = e-not-fnd and
                     e-sts                not  = e-opn-err and
                     e-sts                not  = e-use-err and
                     e-sts                not  = e-fil-inc
                     move  "FE"           to   s-ope
                     move  w-fil-nam      to   s-nam
                     move  w-fil-pat      to   s-pat
                     move  w-fil-sts      to   s-sts
                     call  "swd/mod/prg/obj/msegrt"
                                         using s                      .
       End Declaratives.

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       Main Section.
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cat-rig-exs          .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Put Record                                  *
      *                  *---------------------------------------------*
           if        w-cat-rig-ope        =    "PT"
                     perform put-000      thru put-999
      *                  *---------------------------------------------*
      *                  * Update                                      *
      *                  *---------------------------------------------*
           else if   w-cat-rig-ope        =    "UP"
                     perform upd-000      thru upd-999
      *                  *---------------------------------------------*
      *                  * Read Next                                   *
      *                  *---------------------------------------------*
           else if   w-cat-rig-ope        =    "RN"
                     perform rnx-000      thru rnx-999
      *                  *---------------------------------------------*
      *                  * Read                                        *
      *                  *---------------------------------------------*
           else if   w-cat-rig-ope        =    "RD"
                     perform rea-000      thru rea-999
      *                  *---------------------------------------------*
      *                  * Append Record                               *
      *                  *---------------------------------------------*
           else if   w-cat-rig-ope        =    "AP"
                     perform app-000      thru app-999
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           else if   w-cat-rig-ope        =    "ST"
                     perform str-000      thru str-999
      *                  *---------------------------------------------*
      *                  * Last Removed                                *
      *                  *---------------------------------------------*
           else if   w-cat-rig-ope        =    "LR"
                     perform lrm-000      thru lrm-999
      *                  *---------------------------------------------*
      *                  * Insert Record                               *
      *                  *---------------------------------------------*
           else if   w-cat-rig-ope        =    "IN"
                     perform ins-000      thru ins-999
      *                  *---------------------------------------------*
      *                  * Remove Record                               *
      *                  *---------------------------------------------*
           else if   w-cat-rig-ope        =    "RM"
                     perform rmv-000      thru rmv-999
      *                  *---------------------------------------------*
      *                  * Begin                                       *
      *                  *---------------------------------------------*
           else if   w-cat-rig-ope        =    "BE"
                     perform beg-000      thru beg-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   w-cat-rig-ope        =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   w-cat-rig-ope        =    "CL"
                     perform cls-000      thru cls-999                .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       opn-000.
      *              *-------------------------------------------------*
      *              * Richiesta pathname al modulo segreteria         *
      *              *-------------------------------------------------*
           move      "UP"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-pat                to   w-fil-pat              .
      *              *-------------------------------------------------*
      *              * Operazione di open                              *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           open      i-o   fil                                       .
           if        e-sts                not  = e-not-err
                     perform fte-000      thru fte-999                .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Operazione di close                             *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           close     fil                                              .
           if        e-sts                not  = e-not-err
                     perform fte-000      thru fte-999                .
      *              *-------------------------------------------------*
      *              * Operazione di delete file                       *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           delete    file    fil                                      .
           if        e-sts                not  = e-not-err
                     perform fte-000      thru fte-999                .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Begin                                                     *
      *    *-----------------------------------------------------------*
       beg-000.
      *              *-------------------------------------------------*
      *              * Numero d'ordine del record corrente a zero      *
      *              *-------------------------------------------------*
           move      zero                 to   w-cat-rig-cur          .
      *              *-------------------------------------------------*
      *              * Numero d'ordine dell'ultimo record a zero       *
      *              *-------------------------------------------------*
           move      zero                 to   w-cat-rig-max          .
      *              *-------------------------------------------------*
      *              * Record number start-pointer a zero              *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-str-rcn          .
      *              *-------------------------------------------------*
      *              * Numero d'ordine start-pointer a zero            *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-str-ord          .
      *              *-------------------------------------------------*
      *              * Numero elementi memorizzati a zero              *
      *              *-------------------------------------------------*
           move      zero                 to   w-pnt-num-ele          .
      *              *-------------------------------------------------*
      *              * Record number primo elemento catena a zero      *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-rcn-000          .
      *              *-------------------------------------------------*
      *              * Record number ultimo elemento catena a zero     *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-rcn-999          .
      *              *-------------------------------------------------*
      *              * Nr progressivo ultimo elemento catena a zero    *
      *              *-------------------------------------------------*
           move      zero                 to   w-prg-rig-max          .
      *              *-------------------------------------------------*
      *              * Record number ultimo elemento rimosso a zero    *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-rcn-uer          .
      *              *-------------------------------------------------*
      *              * Numero riga progressivo minimo a zero           *
      *              *-------------------------------------------------*
           move      zero                 to   w-prg-rig-min          .
      *              *-------------------------------------------------*
      *              * Numero riga progressivo massimo a zero          *
      *              *-------------------------------------------------*
           move      zero                 to   w-prg-rig-max          .
      *              *-------------------------------------------------*
      *              * Numero riga progressivo corrente a zero         *
      *              *-------------------------------------------------*
           move      zero                 to   w-cat-rig-prg          .
      *              *-------------------------------------------------*
      *              * Numero riga progressivo precedente a zero       *
      *              *-------------------------------------------------*
           move      zero                 to   w-prg-rig-pre          .
      *              *-------------------------------------------------*
      *              * Numero riga progressivo successivo a zero       *
      *              *-------------------------------------------------*
           move      zero                 to   w-prg-rig-suc          .
      *              *-------------------------------------------------*
      *              * Numero riga progressivo per inserimento a zero  *
      *              *-------------------------------------------------*
           move      zero                 to   w-prg-rig-ins          .
      *              *-------------------------------------------------*
      *              * Numero riga progressivo per append a 100        *
      *              *-------------------------------------------------*
           move      100                  to   w-prg-rig-app          .
      *              *-------------------------------------------------*
      *              * Flag di append possibile                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-cat-rig-app          .
      *              *-------------------------------------------------*
      *              * Flag di inserimento non possibile               *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cat-rig-ins          .
      *              *-------------------------------------------------*
      *              * Flag di record New : No                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cat-rig-new          .
      *              *-------------------------------------------------*
      *              * Flag di record Last : No                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-cat-rig-lst          .
       beg-999.
           exit.

      *    *===========================================================*
      *    * Update                                                    *
      *    *-----------------------------------------------------------*
       upd-000.
      *              *-------------------------------------------------*
      *              * Se l'elemento di destinazione e' zero o maggio- *
      *              * re del massimo : update eseguita senza successo *
      *              *-------------------------------------------------*
           if        w-cat-rig-num        =    zero       or
                     w-cat-rig-num        >    w-cat-rig-max
                     go to upd-400.
      *              *-------------------------------------------------*
      *              * Allineamento a primo elemento della catena      *
      *              *-------------------------------------------------*
           move      w-wrk-rcn-000        to   w-wrk-rec-num          .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero d'ordine elemento       *
      *              *-------------------------------------------------*
           move      1                    to   w-cat-rig-cur          .
       upd-200.
      *              *-------------------------------------------------*
      *              * Test se trovato elemento di destinazione        *
      *              *-------------------------------------------------*
           if        w-cat-rig-cur        =    w-cat-rig-num
                     go to upd-600.
      *              *-------------------------------------------------*
      *              * Allineamento su elemento successivo della cate- *
      *              * na                                              *
      *              *-------------------------------------------------*
           move      w-pnt-nxt-ele
                    (w-wrk-rec-num)       to   w-wrk-rec-num          .
      *              *-------------------------------------------------*
      *              * Incremento numero d'ordine elemento             *
      *              *-------------------------------------------------*
           add       1                    to   w-cat-rig-cur          .
      *              *-------------------------------------------------*
      *              * Riciclo su elemento successivo                  *
      *              *-------------------------------------------------*
           go to     upd-200.
       upd-400.
      *              *-------------------------------------------------*
      *              * Update eseguita senza successo                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Exit status a errore                        *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-exs          .
      *                  *---------------------------------------------*
      *                  * Numero d'ordine del record corrente in      *
      *                  * uscita a zero                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-cat-rig-cur          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo corrente a zero          *
      *                  *---------------------------------------------*
           move      zero                 to   w-cat-rig-prg          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo precedente a zero        *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-pre          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo successivo a zero        *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-suc          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo per inserimento a zero   *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di inserimento non possibile           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di record New : No                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-cat-rig-new          .
      *                  *---------------------------------------------*
      *                  * Flag di record Last : No                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-cat-rig-lst          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     upd-999.
       upd-600.
      *              *-------------------------------------------------*
      *              * Update eseguita con successo                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Spostamento record in area file             *
      *                  *---------------------------------------------*
           move      w-cat-rig-buf        to   fil-rec                .
      *                  *---------------------------------------------*
      *                  * Scrittura record                            *
      *                  *---------------------------------------------*
           perform   scr-000              thru scr-999                .
      *                  *---------------------------------------------*
      *                  * Flag di elemento non piu' New               *
      *                  *---------------------------------------------*
           move      spaces               to   w-pnt-ele-new
                                              (w-wrk-rec-num)         .
      *                  *---------------------------------------------*
      *                  * Numero progressivo corrente                 *
      *                  *---------------------------------------------*
           move      w-pnt-num-prg
                    (w-wrk-rec-num)       to   w-cat-rig-prg          .
      *                  *---------------------------------------------*
      *                  * Numeri progressivi e flags vari             *
      *                  *---------------------------------------------*
           perform   dpf-000              thru dpf-999                .
      *                  *---------------------------------------------*
      *                  * Flag di record New : No                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-cat-rig-new          .
       upd-999.
           exit.

      *    *===========================================================*
      *    * Start                                                     *
      *    *-----------------------------------------------------------*
       str-000.
      *              *-------------------------------------------------*
      *              * Se l'elemento di destinazione e' zero o maggio- *
      *              * re del massimo : start eseguita senza successo  *
      *              *-------------------------------------------------*
           if        w-cat-rig-num        =    zero       or
                     w-cat-rig-num        >    w-cat-rig-max
                     go to str-400.
      *              *-------------------------------------------------*
      *              * Inizializzazione record number start-pointer    *
      *              *-------------------------------------------------*
           move      w-wrk-rcn-000        to   w-wrk-str-rcn          .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero d'ordine start-pointer  *
      *              *-------------------------------------------------*
           move      1                    to   w-wrk-str-ord          .
       str-200.
      *              *-------------------------------------------------*
      *              * Test se trovato elemento di destinazione        *
      *              *-------------------------------------------------*
           if        w-wrk-str-ord        =    w-cat-rig-num
                     go to str-999.
      *              *-------------------------------------------------*
      *              * Aggiornamento record number start-pointer       *
      *              *-------------------------------------------------*
           move      w-pnt-nxt-ele
                    (w-wrk-str-rcn)       to   w-wrk-str-rcn          .
      *              *-------------------------------------------------*
      *              * Incremento numero d'ordine start-pointer        *
      *              *-------------------------------------------------*
           add       1                    to   w-wrk-str-ord          .
      *              *-------------------------------------------------*
      *              * Riciclo su elemento successivo                  *
      *              *-------------------------------------------------*
           go to     str-200.
       str-400.
      *              *-------------------------------------------------*
      *              * Start eseguita senza successo                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Record number start-pointer a zero          *
      *                  *---------------------------------------------*
           move      zero                 to   w-wrk-str-rcn          .
      *                  *---------------------------------------------*
      *                  * Numero d'ordine start-pointer a zero        *
      *                  *---------------------------------------------*
           move      zero                 to   w-wrk-str-ord          .
      *                  *---------------------------------------------*
      *                  * Exit status a errore                        *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-exs          .
       str-999.
           exit.

      *    *===========================================================*
      *    * Read Next                                                 *
      *    *-----------------------------------------------------------*
       rnx-000.
      *              *-------------------------------------------------*
      *              * Se lo start-pointer e' indeterminato : read     *
      *              * next eseguita senza successo                    *
      *              *-------------------------------------------------*
           if        w-wrk-str-rcn        not  = zero
                     go to rnx-200.
      *                  *---------------------------------------------*
      *                  * Exit status a errore                        *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-exs          .
      *                  *---------------------------------------------*
      *                  * Numero d'ordine del record corrente in      *
      *                  * uscita a zero                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-cat-rig-cur          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo corrente a zero          *
      *                  *---------------------------------------------*
           move      zero                 to   w-cat-rig-prg          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo precedente a zero        *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-pre          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo successivo a zero        *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-suc          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo per inserimento a zero   *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di inserimento non possibile           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di record New : No                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-cat-rig-new          .
      *                  *---------------------------------------------*
      *                  * Flag di record Last : No                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-cat-rig-lst          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rnx-999.
       rnx-200.
      *              *-------------------------------------------------*
      *              * Lettura del record corrispondente al record     *
      *              * number dello start-pointer e flag di rec. New   *
      *              *-------------------------------------------------*
           move      w-wrk-str-rcn        to   w-wrk-rec-num          .
           perform   let-000              thru let-999                .
      *              *-------------------------------------------------*
      *              * Numero progressivo corrente                     *
      *              *-------------------------------------------------*
           move      w-pnt-num-prg
                    (w-wrk-rec-num)       to   w-cat-rig-prg          .
      *              *-------------------------------------------------*
      *              * Numero d'ordine del record corrente in uscita   *
      *              *-------------------------------------------------*
           move      w-wrk-str-ord        to   w-cat-rig-cur          .
      *              *-------------------------------------------------*
      *              * Aggiornamento record number start-pointer       *
      *              *-------------------------------------------------*
           move      w-pnt-nxt-ele
                    (w-wrk-str-rcn)       to   w-wrk-str-rcn          .
      *              *-------------------------------------------------*
      *              * Aggiornamento numero d'ordine start-pointer     *
      *              *-------------------------------------------------*
           if        w-wrk-str-rcn        =    zero
                     move  zero           to   w-wrk-str-ord
           else      add   1              to   w-wrk-str-ord          .
      *              *-------------------------------------------------*
      *              * Numeri progressivi e flags vari                 *
      *              *-------------------------------------------------*
           perform   dpf-000              thru dpf-999                .
       rnx-999.
           exit.

      *    *===========================================================*
      *    * Read                                                      *
      *    *-----------------------------------------------------------*
       rea-000.
      *              *-------------------------------------------------*
      *              * Se l'elemento di destinazione e' zero o maggio- *
      *              * re del massimo : read eseguita senza successo   *
      *              *-------------------------------------------------*
           if        w-cat-rig-num        =    zero       or
                     w-cat-rig-num        >    w-cat-rig-max
                     go to rea-400.
      *              *-------------------------------------------------*
      *              * Allineamento a primo elemento della catena      *
      *              *-------------------------------------------------*
           move      w-wrk-rcn-000        to   w-wrk-rec-num          .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero d'ordine elemento       *
      *              *-------------------------------------------------*
           move      1                    to   w-cat-rig-cur          .
       rea-200.
      *              *-------------------------------------------------*
      *              * Test se trovato elemento di destinazione        *
      *              *-------------------------------------------------*
           if        w-cat-rig-cur        =    w-cat-rig-num
                     go to rea-600.
      *              *-------------------------------------------------*
      *              * Allineamento su elemento successivo della cate- *
      *              * na                                              *
      *              *-------------------------------------------------*
           move      w-pnt-nxt-ele
                    (w-wrk-rec-num)       to   w-wrk-rec-num          .
      *              *-------------------------------------------------*
      *              * Incremento numero d'ordine elemento             *
      *              *-------------------------------------------------*
           add       1                    to   w-cat-rig-cur          .
      *              *-------------------------------------------------*
      *              * Riciclo su elemento successivo                  *
      *              *-------------------------------------------------*
           go to     rea-200.
       rea-400.
      *              *-------------------------------------------------*
      *              * Read eseguita senza successo                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Exit status a errore                        *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-exs          .
      *                  *---------------------------------------------*
      *                  * Numero d'ordine del record corrente in      *
      *                  * uscita a zero                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-cat-rig-cur          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo corrente a zero          *
      *                  *---------------------------------------------*
           move      zero                 to   w-cat-rig-prg          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo precedente a zero        *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-pre          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo successivo a zero        *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-suc          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo per inserimento a zero   *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di inserimento non possibile           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di record New : No                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-cat-rig-new          .
      *                  *---------------------------------------------*
      *                  * Flag di record Last : No                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-cat-rig-lst          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rea-999.
       rea-600.
      *              *-------------------------------------------------*
      *              * Read eseguita con successo                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record e determinazione se New      *
      *                  *---------------------------------------------*
           perform   let-000              thru let-999                .
      *                  *---------------------------------------------*
      *                  * Numero progressivo corrente                 *
      *                  *---------------------------------------------*
           move      w-pnt-num-prg
                    (w-wrk-rec-num)       to   w-cat-rig-prg          .
      *                  *---------------------------------------------*
      *                  * Numeri progressivi e flags vari             *
      *                  *---------------------------------------------*
           perform   dpf-000              thru dpf-999                .
       rea-999.
           exit.

      *    *===========================================================*
      *    * Insert Record                                             *
      *    *-----------------------------------------------------------*
       ins-000.
      *              *-------------------------------------------------*
      *              * Se l'elemento di destinazione e' zero o maggio- *
      *              * re del massimo : insert eseguita senza successo *
      *              *-------------------------------------------------*
           if        w-cat-rig-num        =    zero       or
                     w-cat-rig-num        >    w-cat-rig-max
                     go to ins-400.
      *              *-------------------------------------------------*
      *              * Allineamento a primo elemento della catena      *
      *              *-------------------------------------------------*
           move      w-wrk-rcn-000        to   w-wrk-rec-num          .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero d'ordine elemento       *
      *              *-------------------------------------------------*
           move      1                    to   w-cat-rig-cur          .
       ins-200.
      *              *-------------------------------------------------*
      *              * Test se trovato elemento di destinazione        *
      *              *-------------------------------------------------*
           if        w-cat-rig-cur        =    w-cat-rig-num
                     go to ins-600.
      *              *-------------------------------------------------*
      *              * Allineamento su elemento successivo della cate- *
      *              * na                                              *
      *              *-------------------------------------------------*
           move      w-pnt-nxt-ele
                    (w-wrk-rec-num)       to   w-wrk-rec-num          .
      *              *-------------------------------------------------*
      *              * Incremento numero d'ordine elemento             *
      *              *-------------------------------------------------*
           add       1                    to   w-cat-rig-cur          .
      *              *-------------------------------------------------*
      *              * Riciclo su elemento successivo                  *
      *              *-------------------------------------------------*
           go to     ins-200.
       ins-400.
      *              *-------------------------------------------------*
      *              * Insert eseguita senza successo                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Exit status a errore                        *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-exs          .
      *                  *---------------------------------------------*
      *                  * Numero d'ordine del record corrente in      *
      *                  * uscita a zero                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-cat-rig-cur          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo corrente a zero          *
      *                  *---------------------------------------------*
           move      zero                 to   w-cat-rig-prg          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo precedente a zero        *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-pre          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo successivo a zero        *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-suc          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo per inserimento a zero   *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di inserimento non possibile           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di record New : No                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-cat-rig-new          .
      *                  *---------------------------------------------*
      *                  * Flag di record Last : No                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-cat-rig-lst          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ins-999.
       ins-600.
      *              *-------------------------------------------------*
      *              * Se trovato record di destinazione               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numeri progressivi e flags vari per il re-  *
      *                  * cord di destinazione                        *
      *                  *---------------------------------------------*
           perform   dpf-000              thru dpf-999                .
      *                  *---------------------------------------------*
      *                  * Se inserimento non possibile : insert ese-  *
      *                  * guita senza successo                        *
      *                  *---------------------------------------------*
           if        w-cat-rig-ins        not  = spaces
                     go to ins-400.
      *                  *---------------------------------------------*
      *                  * Salvataggio puntatore a record precedente   *
      *                  * quello di destinazione                      *
      *                  *---------------------------------------------*
           move      w-pnt-prv-ele
                    (w-wrk-rec-num)       to   w-wrk-pnt-gen          .
      *                  *---------------------------------------------*
      *                  * Incremento numero d'ordine ultimo record    *
      *                  *---------------------------------------------*
           add       1                    to   w-cat-rig-max          .
      *                  *---------------------------------------------*
      *                  * Incremento numero elementi memorizzati      *
      *                  *---------------------------------------------*
           add       1                    to   w-pnt-num-ele          .
      *                  *---------------------------------------------*
      *                  * Puntatore next del nuovo elemento           *
      *                  *---------------------------------------------*
           move      w-wrk-rec-num        to   w-pnt-nxt-ele
                                              (w-pnt-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Puntatore previous del nuovo elemento       *
      *                  *---------------------------------------------*
           move      w-pnt-prv-ele
                    (w-wrk-rec-num)       to   w-pnt-prv-ele
                                              (w-pnt-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Puntatore previous dell'elemento di desti-  *
      *                  * nazione                                     *
      *                  *---------------------------------------------*
           move      w-pnt-num-ele        to   w-pnt-prv-ele
                                              (w-wrk-rec-num)         .
      *                  *---------------------------------------------*
      *                  * Puntatore next dell'elemento precedente     *
      *                  * quello di destinazione                      *
      *                  *---------------------------------------------*
           if        w-wrk-pnt-gen        =    zero
                     move  w-pnt-num-ele  to   w-wrk-rcn-000
           else      move  w-pnt-num-ele  to   w-pnt-nxt-ele
                                              (w-wrk-pnt-gen)         .
      *                  *---------------------------------------------*
      *                  * Normalizzazione area file                   *
      *                  *---------------------------------------------*
           move      spaces               to   fil-rec                .
      *                  *---------------------------------------------*
      *                  * Scrittura record                            *
      *                  *---------------------------------------------*
           move      w-pnt-num-ele        to   w-wrk-rec-num          .
           perform   scr-000              thru scr-999                .
      *                  *---------------------------------------------*
      *                  * Flag di elemento New                        *
      *                  *---------------------------------------------*
           move      "#"                  to   w-pnt-ele-new
                                              (w-wrk-rec-num)         .
      *                  *---------------------------------------------*
      *                  * Numero progressivo corrente                 *
      *                  *---------------------------------------------*
           move      w-prg-rig-ins        to   w-pnt-num-prg
                                              (w-wrk-rec-num)
                                               w-cat-rig-prg          .
      *                  *---------------------------------------------*
      *                  * Numeri progressivi e flags vari             *
      *                  *---------------------------------------------*
           perform   dpf-000              thru dpf-999                .
      *                  *---------------------------------------------*
      *                  * Flag di record New : Si                     *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-new          .
       ins-999.
           exit.

      *    *===========================================================*
      *    * Put Record                                                *
      *    *-----------------------------------------------------------*
       put-000.
      *              *-------------------------------------------------*
      *              * Se append non possibile : put eseguita senza    *
      *              * successo                                        *
      *              *-------------------------------------------------*
           if        w-cat-rig-app        =   spaces
                     go to put-600.
      *              *-------------------------------------------------*
      *              * Put eseguita senza successo                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Exit status a errore                        *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-exs          .
      *                  *---------------------------------------------*
      *                  * Numero d'ordine del record corrente in      *
      *                  * uscita a zero                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-cat-rig-cur          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo corrente a zero          *
      *                  *---------------------------------------------*
           move      zero                 to   w-cat-rig-prg          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo precedente a zero        *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-pre          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo successivo a zero        *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-suc          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo per inserimento a zero   *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di inserimento non possibile           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di record New : No                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-cat-rig-new          .
      *                  *---------------------------------------------*
      *                  * Flag di record Last : No                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-cat-rig-lst          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     put-999.
       put-600.
      *              *-------------------------------------------------*
      *              * Put eseguita con successo                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero elementi memorizzati      *
      *                  *---------------------------------------------*
           add       1                    to   w-pnt-num-ele          .
      *                  *---------------------------------------------*
      *                  * Puntatore next dell'ultimo elemento prece-  *
      *                  * dente                                       *
      *                  *---------------------------------------------*
           if        w-wrk-rcn-999        =    zero
                     move  w-pnt-num-ele  to   w-wrk-rcn-000
           else      move  w-pnt-num-ele  to   w-pnt-nxt-ele
                                              (w-wrk-rcn-999)         .
      *                  *---------------------------------------------*
      *                  * Puntatore next del nuovo elemento           *
      *                  *---------------------------------------------*
           move      zero                 to   w-pnt-nxt-ele
                                              (w-pnt-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Puntatore previous del nuovo elemento       *
      *                  *---------------------------------------------*
           move      w-wrk-rcn-999        to   w-pnt-prv-ele
                                              (w-pnt-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Numero progressivo del nuovo elemento       *
      *                  *---------------------------------------------*
           move      w-cat-rig-prg        to   w-pnt-num-prg
                                              (w-pnt-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Flag non New del nuovo elemento             *
      *                  *---------------------------------------------*
           move      spaces               to   w-pnt-ele-new
                                              (w-pnt-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Aggiornamento puntatore ad ultimo elemento  *
      *                  * della catena                                *
      *                  *---------------------------------------------*
           move      w-pnt-num-ele        to   w-wrk-rcn-999          .
      *                  *---------------------------------------------*
      *                  * Nr progressivo ultimo elemento catena       *
      *                  *---------------------------------------------*
           move      w-cat-rig-prg        to   w-prg-rig-max          .
      *                  *---------------------------------------------*
      *                  * Incremento numero d'ordine ultimo record    *
      *                  *---------------------------------------------*
           add       1                    to   w-cat-rig-max          .
      *                  *---------------------------------------------*
      *                  * Determinazione numero d'ordine elemento     *
      *                  *---------------------------------------------*
           move      w-cat-rig-max        to   w-cat-rig-cur          .
      *                  *---------------------------------------------*
      *                  * Spostamento record in area file             *
      *                  *---------------------------------------------*
           move      w-cat-rig-buf        to   fil-rec                .
      *                  *---------------------------------------------*
      *                  * Scrittura record                            *
      *                  *---------------------------------------------*
           move      w-pnt-num-ele        to   w-wrk-rec-num          .
           perform   scr-000              thru scr-999                .
      *                  *---------------------------------------------*
      *                  * Numeri progressivi e flags vari             *
      *                  *---------------------------------------------*
           perform   dpf-000              thru dpf-999                .
      *                  *---------------------------------------------*
      *                  * Flag di record New : No                     *
      *                  *---------------------------------------------*
           move      spaces             to   w-cat-rig-new          .
       put-999.
           exit.

      *    *===========================================================*
      *    * Append Record                                             *
      *    *-----------------------------------------------------------*
       app-000.
      *              *-------------------------------------------------*
      *              * Se append non possibile : append eseguita senza *
      *              * successo                                        *
      *              *-------------------------------------------------*
           if        w-cat-rig-app        =   spaces
                     go to app-600.
      *              *-------------------------------------------------*
      *              * Append eseguita senza successo                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Exit status a errore                        *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-exs          .
      *                  *---------------------------------------------*
      *                  * Numero d'ordine del record corrente in      *
      *                  * uscita a zero                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-cat-rig-cur          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo corrente a zero          *
      *                  *---------------------------------------------*
           move      zero                 to   w-cat-rig-prg          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo precedente a zero        *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-pre          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo successivo a zero        *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-suc          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo per inserimento a zero   *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di inserimento non possibile           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di record New : No                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-cat-rig-new          .
      *                  *---------------------------------------------*
      *                  * Flag di record Last : No                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-cat-rig-lst          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     app-999.
       app-600.
      *              *-------------------------------------------------*
      *              * Append eseguita con successo                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero elementi memorizzati      *
      *                  *---------------------------------------------*
           add       1                    to   w-pnt-num-ele          .
      *                  *---------------------------------------------*
      *                  * Puntatore next dell'ultimo elemento prece-  *
      *                  * dente                                       *
      *                  *---------------------------------------------*
           if        w-wrk-rcn-999        =    zero
                     move  w-pnt-num-ele  to   w-wrk-rcn-000
           else      move  w-pnt-num-ele  to   w-pnt-nxt-ele
                                              (w-wrk-rcn-999)         .
      *                  *---------------------------------------------*
      *                  * Puntatore next del nuovo elemento           *
      *                  *---------------------------------------------*
           move      zero                 to   w-pnt-nxt-ele
                                              (w-pnt-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Puntatore previous del nuovo elemento       *
      *                  *---------------------------------------------*
           move      w-wrk-rcn-999        to   w-pnt-prv-ele
                                              (w-pnt-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Aggiornamento puntatore ad ultimo elemento  *
      *                  * della catena                                *
      *                  *---------------------------------------------*
           move      w-pnt-num-ele        to   w-wrk-rcn-999          .
      *                  *---------------------------------------------*
      *                  * Incremento numero d'ordine ultimo record    *
      *                  *---------------------------------------------*
           add       1                    to   w-cat-rig-max          .
      *                  *---------------------------------------------*
      *                  * Determinazione numero d'ordine elemento     *
      *                  *---------------------------------------------*
           move      w-cat-rig-max        to   w-cat-rig-cur          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione area file                   *
      *                  *---------------------------------------------*
           move      spaces               to   fil-rec                .
      *                  *---------------------------------------------*
      *                  * Scrittura record                            *
      *                  *---------------------------------------------*
           move      w-pnt-num-ele        to   w-wrk-rec-num          .
           perform   scr-000              thru scr-999                .
      *                  *---------------------------------------------*
      *                  * Flag di elemento New                        *
      *                  *---------------------------------------------*
           move      "#"                  to   w-pnt-ele-new
                                              (w-wrk-rec-num)         .
      *                  *---------------------------------------------*
      *                  * Numeri progressivi e flags vari             *
      *                  *---------------------------------------------*
           perform   dpf-000              thru dpf-999                .
      *                  *---------------------------------------------*
      *                  * Numero progressivo corrente                 *
      *                  *---------------------------------------------*
           move      w-prg-rig-app        to   w-pnt-num-prg
                                              (w-wrk-rec-num)
                                               w-cat-rig-prg          .
      *                  *---------------------------------------------*
      *                  * Nr progressivo ultimo elemento catena       *
      *                  *---------------------------------------------*
           move      w-cat-rig-prg        to   w-prg-rig-max          .
      *                  *---------------------------------------------*
      *                  * Flag di record New : Si                     *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-new          .
       app-999.
           exit.

      *    *===========================================================*
      *    * Remove Record                                             *
      *    *-----------------------------------------------------------*
       rmv-000.
      *              *-------------------------------------------------*
      *              * Se l'elemento di destinazione e' zero o maggio- *
      *              * re del massimo : remove eseguita senza successo *
      *              *-------------------------------------------------*
           if        w-cat-rig-num        =    zero       or
                     w-cat-rig-num        >    w-cat-rig-max
                     go to rmv-400.
      *              *-------------------------------------------------*
      *              * Allineamento a primo elemento della catena      *
      *              *-------------------------------------------------*
           move      w-wrk-rcn-000        to   w-wrk-pnt-rmv          .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero d'ordine destinazione   *
      *              *-------------------------------------------------*
           move      1                    to   w-wrk-ord-rmv          .
       rmv-200.
      *              *-------------------------------------------------*
      *              * Test se trovato elemento di destinazione        *
      *              *-------------------------------------------------*
           if        w-wrk-ord-rmv        =    w-cat-rig-num
                     go to rmv-600.
      *              *-------------------------------------------------*
      *              * Allineamento su elemento successivo della cate- *
      *              * na                                              *
      *              *-------------------------------------------------*
           move      w-pnt-nxt-ele
                    (w-wrk-pnt-rmv)       to   w-wrk-pnt-rmv          .
      *              *-------------------------------------------------*
      *              * Incremento numero ordine elemento destinazione  *
      *              *-------------------------------------------------*
           add       1                    to   w-wrk-ord-rmv          .
      *              *-------------------------------------------------*
      *              * Riciclo su elemento successivo                  *
      *              *-------------------------------------------------*
           go to     rmv-200.
       rmv-400.
      *              *-------------------------------------------------*
      *              * Remove eseguita senza successo                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Exit status a errore                        *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-exs          .
      *                  *---------------------------------------------*
      *                  * Numero d'ordine del record corrente in      *
      *                  * uscita a zero                               *
      *                  *---------------------------------------------*
           move      zero                 to   w-cat-rig-cur          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo corrente a zero          *
      *                  *---------------------------------------------*
           move      zero                 to   w-cat-rig-prg          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo precedente a zero        *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-pre          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo successivo a zero        *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-suc          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo per inserimento a zero   *
      *                  *---------------------------------------------*
           move      zero                 to   w-prg-rig-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di inserimento non possibile           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di record New : No                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-cat-rig-new          .
      *                  *---------------------------------------------*
      *                  * Flag di record Last : No                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-cat-rig-lst          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rmv-999.
       rmv-600.
      *              *-------------------------------------------------*
      *              * Remove eseguita con successo                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Puntatore previous del record successivo a  *
      *                  * quello rimosso                              *
      *                  *---------------------------------------------*
           move      w-pnt-nxt-ele
                    (w-wrk-pnt-rmv)       to   w-wrk-pnt-gen          .
           if        w-wrk-pnt-gen        =    zero
                     move  w-pnt-prv-ele
                          (w-wrk-pnt-rmv) to   w-wrk-rcn-999
                     if    w-wrk-rcn-999  =    zero
                           move  zero     to   w-prg-rig-max
                     else  move  w-pnt-num-prg
                                (w-wrk-rcn-999)
                                          to   w-prg-rig-max
           else      move  w-pnt-prv-ele
                          (w-wrk-pnt-rmv) to   w-pnt-prv-ele
                                              (w-wrk-pnt-gen)         .
      *                  *---------------------------------------------*
      *                  * Puntatore next del record precedente quello *
      *                  * rimosso                                     *
      *                  *---------------------------------------------*
           move      w-pnt-prv-ele
                    (w-wrk-pnt-rmv)       to   w-wrk-pnt-gen          .
           if        w-wrk-pnt-gen        =    zero
                     move  w-pnt-nxt-ele
                          (w-wrk-pnt-rmv) to   w-wrk-rcn-000
           else      move  w-pnt-nxt-ele
                          (w-wrk-pnt-rmv) to   w-pnt-nxt-ele
                                              (w-wrk-pnt-gen)         .
      *                  *---------------------------------------------*
      *                  * Aggiornamento catena inversa dei records    *
      *                  * rimossi se non era un record New            *
      *                  *---------------------------------------------*
           if        w-pnt-ele-new
                    (w-wrk-pnt-rmv)       =    spaces
                     move  w-wrk-rcn-uer  to   w-pnt-prv-ele
                                              (w-wrk-pnt-rmv)
                     move  w-wrk-pnt-rmv  to   w-wrk-rcn-uer
           else      if    w-wrk-pnt-rmv  =    w-pnt-num-ele
                           subtract  1    from w-pnt-num-ele          .
      *                  *---------------------------------------------*
      *                  * Decremento numero d'ordine ultimo record    *
      *                  *---------------------------------------------*
           subtract  1                    from w-cat-rig-max          .
      *                  *---------------------------------------------*
      *                  * Lettura elemento corrente                   *
      *                  * corrente                                    *
      *                  *---------------------------------------------*
           perform   rea-000              thru rea-999                .
       rmv-999.
           exit.

      *    *===========================================================*
      *    * Last Removed                                              *
      *    *-----------------------------------------------------------*
       lrm-000.
      *              *-------------------------------------------------*
      *              * Se non ci sono ulteriori elementi rimossi :     *
      *              * uscita con status di errore                     *
      *              *-------------------------------------------------*
           if        w-wrk-rcn-uer        not  = zero
                     go to lrm-200.
      *                  *---------------------------------------------*
      *                  * Exit status a errore                        *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cat-rig-exs          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     lrm-999.
       lrm-200.
      *              *-------------------------------------------------*
      *              * Puntatore ad ultimo record rimosso              *
      *              *-------------------------------------------------*
           move      w-wrk-rcn-uer        to   w-wrk-rec-num          .
      *              *-------------------------------------------------*
      *              * Aggiornamento puntatore ad ultimo rimosso       *
      *              *-------------------------------------------------*
           move      w-pnt-prv-ele
                    (w-wrk-rcn-uer)       to   w-wrk-rcn-uer          .
      *              *-------------------------------------------------*
      *              * Lettura record rimosso con determinazione del   *
      *              * flag di record New                              *
      *              *-------------------------------------------------*
           perform   let-000              thru let-999                .
       lrm-999.
           exit.

      *    *===========================================================*
      *    * Determinazione flag di record New e lettura elemento del- *
      *    * la catena numero w-wrk-rec-num                            *
      *    *-----------------------------------------------------------*
       let-000.
      *              *-------------------------------------------------*
      *              * Flag di record New                              *
      *              *-------------------------------------------------*
           move      w-pnt-ele-new
                    (w-wrk-rec-num)       to  w-cat-rig-new           .
      *              *-------------------------------------------------*
      *              * Se record New si ritorna un buffer a spaces     *
      *              *-------------------------------------------------*
           if        w-cat-rig-new        not  = spaces
                     move  spaces         to   w-cat-rig-buf
                     go to let-999.
      *              *-------------------------------------------------*
      *              * Preparazione record number                      *
      *              *-------------------------------------------------*
           move      w-wrk-rec-num        to   w-rcn-krn              .
      *              *-------------------------------------------------*
      *              * Se operazione in cache memory                   *
      *              *-------------------------------------------------*
           if        w-rcn-krn            not  > w-chm-max
                     move  w-chm-ele
                          (w-rcn-krn)     to   fil-rec
                     move  e-not-err      to   e-sts
                     go to let-100.
      *              *-------------------------------------------------*
      *              * Operazione di lettura                           *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           read      fil   with no lock
                           invalid key
                           go to   let-200.
       let-100.
      *              *-------------------------------------------------*
      *              * Test se errori                                  *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-err
                     go to let-400.
      *              *-------------------------------------------------*
      *              * Record letto in buffer di uscita                *
      *              *-------------------------------------------------*
           move      fil-rec              to   w-cat-rig-buf          .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-999.
       let-200.
      *              *-------------------------------------------------*
      *              * Se invalid key : status di errore relativo      *
      *              *-------------------------------------------------*
           move      e-not-fnd            to   e-sts                  .
       let-400.
      *              *-------------------------------------------------*
      *              * Fatal error                                     *
      *              *-------------------------------------------------*
           perform   fte-000              thru fte-999                .
       let-999.
           exit.

      *    *===========================================================*
      *    * Scrittura elemento catena numero w-wrk-rec-num            *
      *    *-----------------------------------------------------------*
       scr-000.
      *              *-------------------------------------------------*
      *              * Preparazione record number                      *
      *              *-------------------------------------------------*
           move      w-wrk-rec-num        to   w-rcn-krn              .
      *              *-------------------------------------------------*
      *              * Se operazione in cache memory                   *
      *              *-------------------------------------------------*
           if        w-rcn-krn            not  > w-chm-max
                     move  fil-rec        to   w-chm-ele
                                              (w-rcn-krn)
                     move  e-not-err      to   e-sts
                     go to scr-999.
       scr-100.
      *              *-------------------------------------------------*
      *              * Tentativo di ri-scrittura                       *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           rewrite   fil-rec invalid key
                             go to   scr-200.
      *              *-------------------------------------------------*
      *              * Test se errori                                  *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-err
                     go to scr-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     scr-999.
       scr-200.
      *              *-------------------------------------------------*
      *              * Tentativo di scrittura                          *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           write     fil-rec invalid key
                             go to   scr-100.
      *              *-------------------------------------------------*
      *              * Test se errori                                  *
      *              *-------------------------------------------------*
           if        e-sts                not  = e-not-err
                     go to scr-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     scr-999.
       scr-300.
      *              *-------------------------------------------------*
      *              * Fatal error                                     *
      *              *-------------------------------------------------*
           perform   fte-000              thru fte-999                .
       scr-999.
           exit.

      *    *===========================================================*
      *    * Determinazione numeri progressivi e flags vari :          *
      *    *   -  Numero progressivo precedente                        *
      *    *   -  Numero progressivo successivo                        *
      *    *   -  Numero progressivo per inserimento                   *
      *    *   -  Numero progressivo per append                        *
      *    *   -  Flag di append                                       *
      *    *   -  Flag di inserimento                                  *
      *    *   -  Flag di record Last                                  *
      *    * In funzione di :                                          *
      *    *   -  w-cat-rig-cur                                        *
      *    *   -  w-cat-rig-prg                                        *
      *    *   -  w-cat-rig-max                                        *
      *    *   -  w-pnt-max-ele                                        *
      *    *   -  w-wrk-rec-num                                        *
      *    *-----------------------------------------------------------*
       dpf-000.
      *              *-------------------------------------------------*
      *              * Numero progressivo precedente                   *
      *              *-------------------------------------------------*
           move      w-pnt-prv-ele
                    (w-wrk-rec-num)       to   w-wrk-pnt-prg          .
           if        w-wrk-pnt-prg        =    zero
                     move  zero           to   w-prg-rig-pre
           else      move  w-pnt-num-prg
                          (w-wrk-pnt-prg) to   w-prg-rig-pre          .
      *              *-------------------------------------------------*
      *              * Numero progressivo successivo                   *
      *              *-------------------------------------------------*
           move      w-pnt-nxt-ele
                    (w-wrk-rec-num)       to   w-wrk-pnt-prg          .
           if        w-wrk-pnt-prg        =    zero
                     move  zero           to   w-prg-rig-suc
           else      move  w-pnt-num-prg
                          (w-wrk-pnt-prg) to   w-prg-rig-suc          .
      *              *-------------------------------------------------*
      *              * Numero progressivo per inserimento              *
      *              *-------------------------------------------------*
           if        w-cat-rig-max        not  < w-pnt-max-ele
                     move   zero          to   w-prg-rig-app
           else      subtract w-prg-rig-pre
                                          from w-cat-rig-prg
                                        giving w-prg-rig-ins
                     divide   2           into w-prg-rig-ins
                     add      w-prg-rig-pre
                                          to   w-prg-rig-ins
                     if       w-prg-rig-ins
                                          =    w-prg-rig-pre
                              move  zero  to   w-prg-rig-ins          .
      *              *-------------------------------------------------*
      *              * Numero progressivo per append                   *
      *              *-------------------------------------------------*
           if        w-cat-rig-max        not  < w-pnt-max-ele
                     move   zero          to   w-prg-rig-app
           else      divide 100           into w-prg-rig-max
                                        giving w-prg-rig-app
                     if     w-prg-rig-app =    w-pnt-max-ele
                            move     zero
                                          to   w-prg-rig-app
                     else   add      1
                                          to   w-prg-rig-app
                            multiply 100  by   w-prg-rig-app          .
      *              *-------------------------------------------------*
      *              * Flag di append                                  *
      *              *-------------------------------------------------*
           if        w-prg-rig-app        =    zero
                     move  "#"            to   w-cat-rig-app
           else      move  spaces         to   w-cat-rig-app          .
      *              *-------------------------------------------------*
      *              * Flag di inserimento                             *
      *              *-------------------------------------------------*
           if        w-prg-rig-ins        =    zero
                     move  "#"            to   w-cat-rig-ins
           else      move  spaces         to   w-cat-rig-ins          .
      *              *-------------------------------------------------*
      *              * Flag di record Last                             *
      *              *-------------------------------------------------*
           if        w-cat-rig-cur        =    w-cat-rig-max
                     move  "#"            to   w-cat-rig-lst
           else      move  spaces         to   w-cat-rig-lst          .
       dpf-999.
           exit.

      *    *===========================================================*
      *    * Richiamo del modulo di segreteria per l'emissione  del    *
      *    * messaggio di fatal i-o error e per la terminazione.       *
      *    *-----------------------------------------------------------*
       fte-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri                          *
      *              *-------------------------------------------------*
           move      "FE"                 to   s-ope                  .
           move      w-fil-nam            to   s-nam                  .
           move      w-fil-pat            to   s-pat                  .
           move      w-fil-sts            to   s-sts                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       fte-999.
           exit.
