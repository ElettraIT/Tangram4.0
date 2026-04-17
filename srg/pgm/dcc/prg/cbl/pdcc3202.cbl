       Identification Division.
       Program-Id.                                  pdcc3202          .
      *================================================================*
      *                                                                *
      * Gestione file relative di supporto per programma pdcc3200      *
      *                                                                *
      * ============================================================== *
      * Tipi operazione                                                *
      *                                                                *
      * "OP" - Open, inizio utilizzo file relative di supporto         *
      *                                                                *
      *             Input  : w-rlt-sup-ope = "OP"                      *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      * "CL" - Close, fine utilizzo file relative di supporto          *
      *                                                                *
      *             Input  : w-rlt-sup-ope = "CL"                      *
      *                                                                *
      *             Output : nessuno                                   *
      *                                                                *
      * "BE" - Begin, inizio nuova sessione di supporto                *
      *                                                                *
      *             Input  : w-rlt-sup-ope = "BE"                      *
      *                                                                *
      *             Output : w-rlt-sup-max = zero                      *
      *                      w-rlt-sup-prg = zero                      *
      *                                                                *
      * "PT" - Put Record, inserimento record in coda                  *
      *                                                                *
      *             Input  : w-rlt-sup-ope = "PT"                      *
      *                      w-rlt-sup-buf = buffer dati               *
      *                                                                *
      *             Output : w-rlt-sup-exs = return status code        *
      *                      w-rlt-sup-max = aumentato di 1            *
      *                      w-rlt-sup-prg = pari a w-rlt-sup-max      *
      *                                                                *
      * "UP" - Update, aggiornamento record su file relative           *
      *                                                                *
      *             Input  : w-rlt-sup-ope = "UP"                      *
      *                      w-rlt-sup-prg = numero progressivo del    *
      *                                      record da aggiornare      *
      *                      w-rlt-sup-buf = buffer dati               *
      *                                                                *
      *             Output : w-rlt-sup-exs = return status code        *
      *                      w-rlt-sup-max = invariato                 *
      *                      w-rlt-sup-prg = invariato                 *
      *                                                                *
      * "RD" - Read, lettura casuale del file relative                 *
      *                                                                *
      *             Input  : w-rlt-sup-ope = "RD"                      *
      *                      w-rlt-sup-prg = numero progressivo del    *
      *                                      record da leggere         *
      *                                                                *
      *             Output : w-rlt-sup-exs = return status code        *
      *                      w-rlt-sup-max = invariato                 *
      *                      w-rlt-sup-prg = invariato                 *
      *                      w-rlt-sup-buf = buffer dati               *
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
               10  filler occurs 512      pic  x(01)                  .

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
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per gestione file relative di sup-  *
      *    * porto, con buffer in grado di ospitare l'area w-rig.      *
      *    *-----------------------------------------------------------*
       01  w-rlt-sup.
           05  w-rlt-sup-ope              pic  x(02)                  .
           05  w-rlt-sup-exs              pic  x(01)                  .
           05  w-rlt-sup-prg              pic  9(05)                  .
           05  w-rlt-sup-max              pic  9(05)                  .
           05  w-rlt-sup-ctr              pic  9(05)                  .
           05  w-rlt-sup-buf.
               10  filler occurs 512      pic  x(01)                  .

      ******************************************************************
       Procedure Division                using w-rlt-sup              .
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
           move      spaces               to   w-rlt-sup-exs          .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Put Record                                  *
      *                  *---------------------------------------------*
           if        w-rlt-sup-ope        =    "PT"
                     perform put-000      thru put-999
      *                  *---------------------------------------------*
      *                  * Update                                      *
      *                  *---------------------------------------------*
           else if   w-rlt-sup-ope        =    "UP"
                     perform upd-000      thru upd-999
      *                  *---------------------------------------------*
      *                  * Read                                        *
      *                  *---------------------------------------------*
           else if   w-rlt-sup-ope        =    "RD"
                     perform rea-000      thru rea-999
      *                  *---------------------------------------------*
      *                  * Begin                                       *
      *                  *---------------------------------------------*
           else if   w-rlt-sup-ope        =    "BE"
                     perform beg-000      thru beg-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   w-rlt-sup-ope        =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   w-rlt-sup-ope        =    "CL"
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
           open      i-o   fil                                        .
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
      *              * Numero progressivo ultimo elemento a zero       *
      *              *-------------------------------------------------*
           move      zero                 to   w-rlt-sup-max          .
       beg-999.
           exit.

      *    *===========================================================*
      *    * Update                                                    *
      *    *-----------------------------------------------------------*
       upd-000.
      *              *-------------------------------------------------*
      *              * Se numero progressivo elemento di destinazione  *
      *              * a zero o maggiore del massimo : update esegui-  *
      *              * ta senza successo                               *
      *              *-------------------------------------------------*
           if        w-rlt-sup-prg        not  = zero      and
                     w-rlt-sup-prg        not  > w-rlt-sup-max
                     go to upd-200.
      *                  *---------------------------------------------*
      *                  * Exit status a errore                        *
      *                  *---------------------------------------------*
           move      "#"                  to   w-rlt-sup-exs          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     upd-999.
       upd-200.
      *              *-------------------------------------------------*
      *              * Spostamento record in area file                 *
      *              *-------------------------------------------------*
           move      w-rlt-sup-buf        to   fil-rec                .
      *              *-------------------------------------------------*
      *              * Scrittura record                                *
      *              *-------------------------------------------------*
           perform   scr-000              thru scr-999                .
       upd-999.
           exit.

      *    *===========================================================*
      *    * Read                                                      *
      *    *-----------------------------------------------------------*
       rea-000.
      *              *-------------------------------------------------*
      *              * Se numero progressivo elemento di destinazione  *
      *              * a zero o maggiore del massimo : read eseguita   *
      *              * senza successo                                  *
      *              *-------------------------------------------------*
           if        w-rlt-sup-prg        not  = zero      and
                     w-rlt-sup-prg        not  > w-rlt-sup-max
                     go to rea-200.
      *                  *---------------------------------------------*
      *                  * Exit status a errore                        *
      *                  *---------------------------------------------*
           move      "#"                  to   w-rlt-sup-exs          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rea-999.
       rea-200.
      *              *-------------------------------------------------*
      *              * Lettura record                                  *
      *              *-------------------------------------------------*
           perform   let-000              thru let-999                .
      *              *-------------------------------------------------*
      *              * Spostamento record in area 'w-rig'              *
      *              *-------------------------------------------------*
           move      fil-rec              to   w-rlt-sup-buf          .
       rea-999.
           exit.

      *    *===========================================================*
      *    * Put Record                                                *
      *    *-----------------------------------------------------------*
       put-000.
      *              *-------------------------------------------------*
      *              * Incremento numero progressivo ultimo elemento   *
      *              *-------------------------------------------------*
           add       1                    to   w-rlt-sup-max          .
      *              *-------------------------------------------------*
      *              * Preparazione numero progressivo per scrittura   *
      *              *-------------------------------------------------*
           move      w-rlt-sup-max        to   w-rlt-sup-prg          .
      *              *-------------------------------------------------*
      *              * Spostamento record in area file                 *
      *              *-------------------------------------------------*
           move      w-rlt-sup-buf        to   fil-rec                .
      *              *-------------------------------------------------*
      *              * Scrittura record                                *
      *              *-------------------------------------------------*
           perform   scr-000              thru scr-999                .
       put-999.
           exit.

      *    *===========================================================*
      *    * Lettura fisica elemento                                   *
      *    *-----------------------------------------------------------*
       let-000.
      *              *-------------------------------------------------*
      *              * Preparazione record number                      *
      *              *-------------------------------------------------*
           move      w-rlt-sup-prg        to   w-rcn-krn              .
      *              *-------------------------------------------------*
      *              * Operazione di lettura                           *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           read      fil   with no lock
                           invalid key
                           go to   let-100.
      *              *-------------------------------------------------*
      *              * Se errori in lettura                            *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to let-200.
       let-100.
      *                  *---------------------------------------------*
      *                  * Fatal error                                 *
      *                  *---------------------------------------------*
           perform   fte-000              thru fte-999                .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-999.
       let-200.
      *              *-------------------------------------------------*
      *              * Record letto in buffer di uscita                *
      *              *-------------------------------------------------*
           move      fil-rec              to   w-rlt-sup-buf          .
       let-999.
           exit.

      *    *===========================================================*
      *    * Scrittura fisica elemento                                 *
      *    *-----------------------------------------------------------*
       scr-000.
      *              *-------------------------------------------------*
      *              * Preparazione record number                      *
      *              *-------------------------------------------------*
           move      w-rlt-sup-prg        to   w-rcn-krn              .
       scr-100.
      *              *-------------------------------------------------*
      *              * Tentativo di ri-scrittura, se invalid-key : a   *
      *              * tentativo di scrittura                          *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           rewrite   fil-rec invalid key
                             go to   scr-200.
      *              *-------------------------------------------------*
      *              * Se nessun errore : uscita                       *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to scr-999.
      *              *-------------------------------------------------*
      *              * Se errore grave : a fatal error                 *
      *              *-------------------------------------------------*
           go to     scr-300.
       scr-200.
      *              *-------------------------------------------------*
      *              * Tentativo di scrittura, se invalid key : a ten- *
      *              * tativo di ri-scrittura                          *
      *              *-------------------------------------------------*
           move      e-not-err            to   e-sts                  .
           write     fil-rec invalid key
                             go to   scr-100.
      *              *-------------------------------------------------*
      *              * Se nessun errore : uscita                       *
      *              *-------------------------------------------------*
           if        e-sts                =    e-not-err
                     go to scr-999.
      *              *-------------------------------------------------*
      *              * Se errore grave : a fatal error                 *
      *              *-------------------------------------------------*
           go to     scr-300.
       scr-300.
      *              *-------------------------------------------------*
      *              * Fatal error                                     *
      *              *-------------------------------------------------*
           perform   fte-000              thru fte-999                .
       scr-999.
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
