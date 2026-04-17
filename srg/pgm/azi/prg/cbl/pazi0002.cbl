       Identification Division.
       Program-Id.                                       pazi0002     .
      *================================================================*
      *                                                                *
      * Gestione subfile relative 'rig' per programma pazi0000         *
      *                                                                *
      * Gestione anagrafica azienda e relative dipendenze              *
      *                                                                *
      * ============================================================== *
      *                                                                *
      *                     Tipi operazione                            *
      *                                                                *
      *                   --------------------                         *
      *                                                                *
      * "OP" - Open, inizio utilizzo gestione subfile                  *
      *                                                                *
      *        - w-sub-rel-ope = "OP"                                  *
      *                                                                *
      *                   --------------------                         *
      *                                                                *
      * "CL" - Close, fine utilizzo gestione subfile                   *
      *                                                                *
      *        - w-sub-rel-ope = "CL"                                  *
      *                                                                *
      *                   --------------------                         *
      *                                                                *
      * "BE" - Begin, inizio nuovo subfile                             *
      *                                                                *
      *        - w-sub-rel-ope = "BE"                                  *
      *                                                                *
      *                   --------------------                         *
      *                                                                *
      * "PT" - Put Record, inserimento record pieno in subfile         *
      *                                                                *
      *        - w-sub-rel-ope = "PT"                                  *
      *        - w-sub-rel-prg = numero progressivo record             *
      *        - w-sub-rel-buf = buffer dati                           *
      *                                                                *
      *                   --------------------                         *
      *                                                                *
      * "UP" - Update, aggiornamento record in subfile                 *
      *                                                                *
      *        - w-sub-rel-ope = "UP"                                  *
      *        - w-sub-rel-num = numero d'ordine record attuale        *
      *        - w-sub-rel-prg = numero progressivo record attuale     *
      *        - w-sub-rel-ppm = numero progressivo record precedente  *
      *                          le modifiche pre-update se record new *
      *        - w-sub-rel-buf = buffer dati                           *
      *                                                                *
      *                   --------------------                         *
      *                                                                *
      * "ST" - Start, posizionamento su subfile                        *
      *                                                                *
      *        - w-sub-rel-ope = "ST"                                  *
      *        - w-sub-rel-num = numero d'ordine record                *
      *                                                                *
      *                   --------------------                         *
      *                                                                *
      * "RN" - Read Next, lettura sequenziale del subfile              *
      *                                                                *
      *        - w-sub-rel-ope = "RN"                                  *
      *                                                                *
      *                   --------------------                         *
      *                                                                *
      * "RD" - Read, lettura casuale del subfile                       *
      *                                                                *
      *        - w-sub-rel-ope = "RD"                                  *
      *        - w-sub-rel-num = numero d'ordine record                *
      *                                                                *
      *                   --------------------                         *
      *                                                                *
      * "P?" - Test esistenza record con numero progressivo dato       *
      *                                                                *
      *        - w-sub-rel-ope = "P?"                                  *
      *        - w-sub-rel-prw = numero progressivo record             *
      *                                                                *
      *                   --------------------                         *
      *                                                                *
      * "P<" - Estrazione numero progressivo da numero d'ordine        *
      *                                                                *
      *        - w-sub-rel-ope = "P<"                                  *
      *        - w-sub-rel-prw = numero d'ordine record                *
      *                                                                *
      *                   --------------------                         *
      *                                                                *
      * "O<" - Estrazione numero d'ordine da numero progressivo        *
      *                                                                *
      *        - w-sub-rel-ope = "O<"                                  *
      *        - w-sub-rel-prw = numero progressivo record             *
      *                                                                *
      *                   --------------------                         *
      *                                                                *
      * "IN" - Insert, inserimento record vuoto nel subfile            *
      *                                                                *
      *        - w-sub-rel-ope = "IN"                                  *
      *        - w-sub-rel-prg = numero progressivo record             *
      *                                                                *
      *                   --------------------                         *
      *                                                                *
      * "AP" - Append, inserimento record vuoto a fine subfile         *
      *                                                                *
      *        - w-sub-rel-ope = "AP"                                  *
      *        - w-sub-rel-prg = numero progressivo record             *
      *                                                                *
      *                   --------------------                         *
      *                                                                *
      * "RM" - Remove Record, rimozione record dal subfile             *
      *                                                                *
      *        - w-sub-rel-ope = "RM"                                  *
      *        - w-sub-rel-num = numero d'ordine record                *
      *                                                                *
      *                   --------------------                         *
      *                                                                *
      * "LR" - Last Removed, ultimo record rimosso del subfile         *
      *                                                                *
      *        - w-sub-rel-ope = "LR"                                  *
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
      *        * Area dati                                             *
      *        *-------------------------------------------------------*
           05  fil-dat.
               10  filler occurs 800      pic  x(01)                  .

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
      *        *-------------------------------------------------------*
      *        * Record number effettivo da trattare                   *
      *        *-------------------------------------------------------*
           05  w-rcn-rnu-eff             pic  9(05)                   .

      *    *===========================================================*
      *    * Work per gestione flags e record numbers                  *
      *    *-----------------------------------------------------------*
       01  w-frn.
      *        *-------------------------------------------------------*
      *        * Massimo numero elementi                               *
      *        *-------------------------------------------------------*
           05  w-frn-max-ele              pic  9(05) value 999        .
      *        *-------------------------------------------------------*
      *        * Tabella flags elementi inseriti                       *
      *        *-------------------------------------------------------*
           05  w-frn-tbf-ins.
               10  w-frn-flg-ins occurs 999
                                          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tabella flags elementi new                            *
      *        *-------------------------------------------------------*
           05  w-frn-tbf-new.
               10  w-frn-flg-new occurs 999
                                          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tabella flags elementi deletati                       *
      *        *-------------------------------------------------------*
           05  w-frn-tbf-del.
               10  w-frn-flg-del occurs 999
                                          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Record number massimo finora utilizzato               *
      *        *-------------------------------------------------------*
           05  w-frn-max-rnu              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Tabella record numbers elementi inseriti              *
      *        *-------------------------------------------------------*
           05  w-frn-tbr-ins.
               10  w-frn-rnu-ins occurs 999
                                          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Tabella record numbers elementi deletati              *
      *        *-------------------------------------------------------*
           05  w-frn-tbr-del.
               10  w-frn-rnu-del occurs 999
                                          pic  9(05)                  .

      *    *===========================================================*
      *    * Work per salvataggi vari                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio flag generico                             *
      *        *-------------------------------------------------------*
           05  w-sav-flg-000              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per w-sub-rel-prg                         *
      *        *-------------------------------------------------------*
           05  w-sav-rel-prg              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per w-sub-rel-max                         *
      *        *-------------------------------------------------------*
           05  w-sav-rel-max              pic  9(05)                  .

      *    *===========================================================*
      *    * Work per contatori vari                                   *
      *    *-----------------------------------------------------------*
       01  w-ctr.
      *        *-------------------------------------------------------*
      *        * Contatore per tallying di inspect                     *
      *        *-------------------------------------------------------*
           05  w-ctr-tal-001              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatore su tabella flags                            *
      *        *-------------------------------------------------------*
           05  w-ctr-tbf-001              pic  9(05)                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Cache memory per i primi 10 elementi                      *
      *    *-----------------------------------------------------------*
       01  w-chm.
      *        *-------------------------------------------------------*
      *        * Numero elementi in cache memory                       *
      *        *-------------------------------------------------------*
           05  w-chm-max                  pic  9(03)       value 10   .
      *        *-------------------------------------------------------*
      *        * Elementi                                              *
      *        *-------------------------------------------------------*
           05  w-chm-tbl occurs 10.
      *            *---------------------------------------------------*
      *            * Elemento                                          *
      *            *---------------------------------------------------*
               10  w-chm-ele.
                   15  filler occurs 800  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Indice per cache memory                               *
      *        *-------------------------------------------------------*
           05  w-chm-inx                  pic  9(03)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per gestione subfile 'rig'          *
      *    *-----------------------------------------------------------*
       01  w-sub-rel.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  w-sub-rel-ope              pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        * - Spaces : operazione eseguita                        *
      *        * - #      : errore di esecuzione                       *
      *        *-------------------------------------------------------*
           05  w-sub-rel-exs              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Massimo numero di elementi gestiti                    *
      *        *-------------------------------------------------------*
           05  w-sub-rel-meg              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero d'ordine passato come parametro in input       *
      *        *-------------------------------------------------------*
           05  w-sub-rel-num              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero di elementi attualmente inseriti               *
      *        *-------------------------------------------------------*
           05  w-sub-rel-max              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero d'ordine del record corrente                   *
      *        *-------------------------------------------------------*
           05  w-sub-rel-cur              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero d'ordine del record di start                   *
      *        *-------------------------------------------------------*
           05  w-sub-rel-stn              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero progressivo del record di start                *
      *        *-------------------------------------------------------*
           05  w-sub-rel-stp              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo record corrente               *
      *        *-------------------------------------------------------*
           05  w-sub-rel-prg              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo record corrente pre-modifica  *
      *        *-------------------------------------------------------*
           05  w-sub-rel-ppm              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo di lavoro                     *
      *        *-------------------------------------------------------*
           05  w-sub-rel-prw              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo record precedente             *
      *        *-------------------------------------------------------*
           05  w-sub-rel-npp              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo record successivo             *
      *        *-------------------------------------------------------*
           05  w-sub-rel-nps              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo minimo                        *
      *        *-------------------------------------------------------*
           05  w-sub-rel-np0              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo massimo                       *
      *        *-------------------------------------------------------*
           05  w-sub-rel-np9              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo per inserimento               *
      *        *-------------------------------------------------------*
           05  w-sub-rel-npi              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero riga progressivo per append                    *
      *        *-------------------------------------------------------*
           05  w-sub-rel-npa              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Flag di inserimento possibile                         *
      *        *-------------------------------------------------------*
           05  w-sub-rel-ins              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di append possibile                              *
      *        *-------------------------------------------------------*
           05  w-sub-rel-app              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di record New                                    *
      *        *-------------------------------------------------------*
           05  w-sub-rel-new              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di record Last                                   *
      *        *-------------------------------------------------------*
           05  w-sub-rel-lst              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Buffer dati                                           *
      *        *-------------------------------------------------------*
           05  w-sub-rel-buf.
               10  filler occurs 800      pic  x(01)                  .

      ******************************************************************
       Procedure Division                using w-sub-rel              .
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
           move      spaces               to   w-sub-rel-exs          .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Put Record                                  *
      *                  *---------------------------------------------*
           if        w-sub-rel-ope        =    "PT"
                     perform put-000      thru put-999
      *                  *---------------------------------------------*
      *                  * Update                                      *
      *                  *---------------------------------------------*
           else if   w-sub-rel-ope        =    "UP"
                     perform upd-000      thru upd-999
      *                  *---------------------------------------------*
      *                  * Read Next                                   *
      *                  *---------------------------------------------*
           else if   w-sub-rel-ope        =    "RN"
                     perform rnx-000      thru rnx-999
      *                  *---------------------------------------------*
      *                  * Read                                        *
      *                  *---------------------------------------------*
           else if   w-sub-rel-ope        =    "RD"
                     perform rea-000      thru rea-999
      *                  *---------------------------------------------*
      *                  * Append Record                               *
      *                  *---------------------------------------------*
           else if   w-sub-rel-ope        =    "AP"
                     perform app-000      thru app-999
      *                  *---------------------------------------------*
      *                  * Test esistenza numero progressivo           *
      *                  *---------------------------------------------*
           else if   w-sub-rel-ope        =    "P?"
                     perform tep-000      thru tep-999
      *                  *---------------------------------------------*
      *                  * Numero progressivo da numero d'ordine       *
      *                  *---------------------------------------------*
           else if   w-sub-rel-ope        =    "P<"
                     perform pdo-000      thru pdo-999
      *                  *---------------------------------------------*
      *                  * Numero d'ordine da numero progressivo       *
      *                  *---------------------------------------------*
           else if   w-sub-rel-ope        =    "O<"
                     perform odp-000      thru odp-999
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           else if   w-sub-rel-ope        =    "ST"
                     perform str-000      thru str-999
      *                  *---------------------------------------------*
      *                  * Last Removed                                *
      *                  *---------------------------------------------*
           else if   w-sub-rel-ope        =    "LR"
                     perform lrm-000      thru lrm-999
      *                  *---------------------------------------------*
      *                  * Insert Record                               *
      *                  *---------------------------------------------*
           else if   w-sub-rel-ope        =    "IN"
                     perform ins-000      thru ins-999
      *                  *---------------------------------------------*
      *                  * Remove Record                               *
      *                  *---------------------------------------------*
           else if   w-sub-rel-ope        =    "RM"
                     perform rmv-000      thru rmv-999
      *                  *---------------------------------------------*
      *                  * Begin                                       *
      *                  *---------------------------------------------*
           else if   w-sub-rel-ope        =    "BE"
                     perform beg-000      thru beg-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   w-sub-rel-ope        =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   w-sub-rel-ope        =    "CL"
                     perform cls-000      thru cls-999                .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       opn-000.
      *              *-------------------------------------------------*
      *              * Massimo numero elementi in link-area            *
      *              *-------------------------------------------------*
           move      w-frn-max-ele        to   w-sub-rel-meg          .
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
      *              * Normalizzazione link-area                       *
      *              *-------------------------------------------------*
           perform   nla-000              thru nla-999                .
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           perform   nwa-000              thru nwa-999                .
       beg-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione link-area                                 *
      *    *-----------------------------------------------------------*
       nla-000.
           move      zero                 to   w-sub-rel-max          .
           move      zero                 to   w-sub-rel-cur          .
           move      zero                 to   w-sub-rel-stn          .
           move      zero                 to   w-sub-rel-stp          .
           move      zero                 to   w-sub-rel-prg          .
           move      zero                 to   w-sub-rel-npp          .
           move      zero                 to   w-sub-rel-nps          .
           move      zero                 to   w-sub-rel-np0          .
           move      zero                 to   w-sub-rel-np9          .
           move      zero                 to   w-sub-rel-npi          .
           move      1                    to   w-sub-rel-npa          .
           move      spaces               to   w-sub-rel-app          .
           move      "#"                  to   w-sub-rel-ins          .
           move      spaces               to   w-sub-rel-new          .
           move      spaces               to   w-sub-rel-lst          .
       nla-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione work-area                                 *
      *    *-----------------------------------------------------------*
       nwa-000.
           move      spaces               to   w-frn-tbf-ins          .
           move      spaces               to   w-frn-tbf-new          .
           move      spaces               to   w-frn-tbf-del          .
           move      zero                 to   w-frn-max-rnu          .
       nwa-999.
           exit.

      *    *===========================================================*
      *    * Update                                                    *
      *    *-----------------------------------------------------------*
       upd-000.
      *              *-------------------------------------------------*
      *              * Se numero progressivo invariato : update rego-  *
      *              * lare                                            *
      *              *-------------------------------------------------*
           if        w-sub-rel-prg        =    w-sub-rel-ppm
                     go to upd-700.
      *              *-------------------------------------------------*
      *              * Altrimenti : Remove del vecchio e Put del nuovo *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Remove del vecchio                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento work-area flags e record  *
      *                      * numbers                                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-frn-flg-ins
                                              (w-sub-rel-ppm)         .
           move      w-frn-flg-new
                    (w-sub-rel-ppm)       to   w-sav-flg-000          .
           move      spaces               to   w-frn-flg-new
                                              (w-sub-rel-ppm)         .
           if        w-frn-flg-del
                    (w-sub-rel-ppm)       =    spaces    and
                     w-sav-flg-000        =    spaces
                     move  "#"            to   w-frn-flg-del
                                              (w-sub-rel-ppm)
                     move  w-frn-rnu-ins
                          (w-sub-rel-ppm) to   w-frn-rnu-del
                                              (w-sub-rel-ppm)         .
           if        w-sav-flg-000        not  = spaces
                     subtract    1        from w-frn-max-rnu          .
           move      zero                 to   w-frn-rnu-ins
                                              (w-sub-rel-ppm)         .
      *                      *-----------------------------------------*
      *                      * Aggiornamento link-area                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Numero elementi inseriti            *
      *                          *-------------------------------------*
           if        w-sav-flg-000        =    spaces
                     subtract  1          from w-sub-rel-max          .
      *                      *-----------------------------------------*
      *                      * Numero progressivo minimo               *
      *                      *-----------------------------------------*
           move      zero                 to   w-ctr-tal-001          .
           inspect   w-frn-tbf-ins    tallying w-ctr-tal-001
                     for   characters   before initial   "#"          .
           if        w-ctr-tal-001        =    w-frn-max-ele
                     move  zero           to   w-sub-rel-np0
           else      add   1
                           w-ctr-tal-001
                                        giving w-sub-rel-np0          .
      *                      *-----------------------------------------*
      *                      * Numero progressivo massimo              *
      *                      *-----------------------------------------*
           move      zero                 to   w-sub-rel-np9          .
       upd-300.
           move      zero                 to   w-ctr-tal-001          .
           inspect   w-frn-tbf-ins    tallying w-ctr-tal-001
                     for   characters   before initial   "#"
                     replacing           first "#"
                                          by   "."                    .
           if        w-ctr-tal-001        <    w-frn-max-ele
                     add   1
                           w-ctr-tal-001
                                        giving w-sub-rel-np9
                     go to upd-300.
           inspect   w-frn-tbf-ins   replacing
                                          all  "."
                                          by   "#"                    .
      *                  *---------------------------------------------*
      *                  * Put del nuovo                               *
      *                  *---------------------------------------------*
           perform   put-000              thru put-999                .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     upd-999.
       upd-700.
      *              *-------------------------------------------------*
      *              * Update regolare                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiorn. work-area flags e record numbers   *
      *                  *---------------------------------------------*
           move      "#"                  to   w-frn-flg-ins
                                              (w-sub-rel-prg)         .
           move      w-frn-flg-new
                    (w-sub-rel-prg)       to   w-sav-flg-000          .
           move      spaces               to   w-frn-flg-new
                                              (w-sub-rel-prg)         .
      *                  *---------------------------------------------*
      *                  * Aggiornamento link-area                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero elementi inseriti                *
      *                      *-----------------------------------------*
           if        w-sav-flg-000        not  = spaces
                     add   1              to   w-sub-rel-max          .
      *                      *-----------------------------------------*
      *                      * Numero d'ordine del record corrente     *
      *                      *-----------------------------------------*
           move      w-frn-flg-ins
                    (w-sub-rel-prg)       to   w-sav-flg-000          .
           move      high-value           to   w-frn-flg-ins
                                              (w-sub-rel-prg)         .
           move      zero                 to   w-ctr-tal-001          .
           inspect   w-frn-tbf-ins    tallying w-ctr-tal-001
                     for   all "#"      before initial high-value     .
           add       1
                     w-ctr-tal-001      giving w-sub-rel-cur          .
           move      w-sav-flg-000        to   w-frn-flg-ins
                                              (w-sub-rel-prg)         .
      *                      *-----------------------------------------*
      *                      * Numero d'ordine del record di start     *
      *                      *-----------------------------------------*
           move      zero                 to   w-sub-rel-stn          .
      *                      *-----------------------------------------*
      *                      * Numero progressivo del record di start  *
      *                      *-----------------------------------------*
           move      zero                 to   w-sub-rel-stp          .
      *                      *-----------------------------------------*
      *                      * Numero progressivo record precedente    *
      *                      *-----------------------------------------*
           move      zero                 to   w-sub-rel-npp          .
           if        w-sub-rel-prg        =    w-sub-rel-np0
                     go to upd-750.
           move      w-sub-rel-prg        to   w-sub-rel-npp          .
       upd-725.
           subtract  1                    from w-sub-rel-npp          .
           if        w-frn-flg-ins
                    (w-sub-rel-npp)       =    spaces
                     go to upd-725.
       upd-750.
      *                      *-----------------------------------------*
      *                      * Numero progressivo record successivo    *
      *                      *-----------------------------------------*
           move      zero                 to   w-sub-rel-nps          .
           if        w-sub-rel-prg        =    w-sub-rel-np9
                     go to upd-800.
           move      w-sub-rel-prg        to   w-sub-rel-nps          .
       upd-775.
           add       1                    to   w-sub-rel-nps          .
           if        w-frn-flg-ins
                    (w-sub-rel-nps)       =    spaces
                     go to upd-775.
       upd-800.
      *                      *-----------------------------------------*
      *                      * Numero progressivo per inserimento      *
      *                      *-----------------------------------------*
           move      w-sub-rel-prg        to   w-sub-rel-npi          .
           subtract  1                    from w-sub-rel-npi          .
           if        w-sub-rel-npi        not  > w-sub-rel-npp
                     move  zero           to   w-sub-rel-npi          .
      *                      *-----------------------------------------*
      *                      * Numero progressivo per append           *
      *                      *-----------------------------------------*
           move      w-sub-rel-np9        to   w-sub-rel-npa          .
           add       1                    to   w-sub-rel-npa          .
           if        w-sub-rel-npa        >    w-frn-max-ele
                     move  zero           to   w-sub-rel-npa          .
      *                      *-----------------------------------------*
      *                      * Flag di inserimento possibile o no      *
      *                      *-----------------------------------------*
           if        w-sub-rel-npi        =    zero
                     move  "#"            to   w-sub-rel-ins
           else      move  spaces         to   w-sub-rel-ins          .
      *                      *-----------------------------------------*
      *                      * Flag di append possibile o no           *
      *                      *-----------------------------------------*
           if        w-sub-rel-npa        =    zero
                     move  "#"            to   w-sub-rel-app
           else      move  spaces         to   w-sub-rel-app          .
      *                      *-----------------------------------------*
      *                      * Flag di record new                      *
      *                      *-----------------------------------------*
           move      spaces               to   w-sub-rel-new          .
      *                      *-----------------------------------------*
      *                      * Flag di record last                     *
      *                      *-----------------------------------------*
           if        w-sub-rel-prg        =    w-sub-rel-np9
                     move  "#"            to   w-sub-rel-lst
           else      move  spaces         to   w-sub-rel-lst          .
      *                  *---------------------------------------------*
      *                  * Scrittura effettiva del record              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione record number effettivo    *
      *                      *-----------------------------------------*
           move      w-frn-rnu-ins
                    (w-sub-rel-prg)       to   w-rcn-rnu-eff          .
      *                      *-----------------------------------------*
      *                      * Spostamento record in area file         *
      *                      *-----------------------------------------*
           move      w-sub-rel-buf        to   fil-rec                .
      *                      *-----------------------------------------*
      *                      * Subroutine di scrittura                 *
      *                      *-----------------------------------------*
           perform   scr-000              thru scr-999                .
       upd-999.
           exit.

      *    *===========================================================*
      *    * Start                                                     *
      *    *-----------------------------------------------------------*
       str-000.
      *              *-------------------------------------------------*
      *              * Se nr d'ordine invalido : start eseguita senza  *
      *              * successo                                        *
      *              *-------------------------------------------------*
           if        w-sub-rel-num        not  = zero and
                     w-sub-rel-np0        not  = zero
                     go to str-200.
       str-100.
      *              *-------------------------------------------------*
      *              * Se start eseguita senza successo                *
      *              *-------------------------------------------------*
           move      zero                 to   w-sub-rel-stn          .
           move      zero                 to   w-sub-rel-stp          .
           move      "#"                  to   w-sub-rel-exs          .
           go to     str-999.
       str-200.
      *              *-------------------------------------------------*
      *              * Posizionamento sul primo numero progressivo     *
      *              *-------------------------------------------------*
           move      w-sub-rel-np0        to   w-sub-rel-stp          .
           move      zero                 to   w-sub-rel-stn          .
       str-300.
      *              *-------------------------------------------------*
      *              * Se raggiunto il numero d'ordine di destinazione *
      *              * uscita                                          *
      *              *-------------------------------------------------*
           add       1                    to   w-sub-rel-stn          .
           if        w-sub-rel-stn        =    w-sub-rel-num
                     go to str-999.
       str-400.
      *              *-------------------------------------------------*
      *              * Posizionamento su numero progressivo successivo *
      *              *-------------------------------------------------*
           add       1                    to   w-sub-rel-stp          .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : uscita                    *
      *              *-------------------------------------------------*
           if        w-sub-rel-stp       >    w-frn-max-ele
                     go to str-100.
           if        w-sub-rel-stp        >    w-sub-rel-np9
                     go to str-100.
      *              *-------------------------------------------------*
      *              * Se elemento non inserito si ricicla per posi-   *
      *              * zionarsi sul numero progressivo successivo, al- *
      *              * trimenti si controlla se si e' giunti all' e-   *
      *              * lemento di destinazione                         *
      *              *-------------------------------------------------*
           if        w-frn-flg-ins
                    (w-sub-rel-stp)       =    spaces
                     go to str-400
           else      go to str-300.
       str-999.
           exit.

      *    *===========================================================*
      *    * Read Next                                                 *
      *    *-----------------------------------------------------------*
       rnx-000.
      *              *-------------------------------------------------*
      *              * Se il numero progressivo del record di start e' *
      *              * indeterminato : read next eseguita senza suc-   *
      *              * cesso                                           *
      *              *-------------------------------------------------*
           if        w-sub-rel-stp        not  = zero
                     go to rnx-200.
       rnx-100.
      *              *-------------------------------------------------*
      *              * Se read next eseguita senza successo            *
      *              *-------------------------------------------------*
           move      "#"                  to   w-sub-rel-exs          .
           go to     rnx-999.
       rnx-200.
      *              *-------------------------------------------------*
      *              * Lettura record corrente                         *
      *              *-------------------------------------------------*
           move      w-sub-rel-stn        to   w-sub-rel-num          .
           move      w-sub-rel-stp        to   w-sub-rel-prg          .
           perform   rea-000              thru rea-999                .
      *              *-------------------------------------------------*
      *              * Preparazioni per prossima read next             *
      *              *-------------------------------------------------*
       rnx-300.
      *                  *---------------------------------------------*
      *                  * Posizionamento su nr progressivo successivo *
      *                  *---------------------------------------------*
           add       1                    to   w-sub-rel-stp          .
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo : uscita                *
      *                  *---------------------------------------------*
           if        w-sub-rel-stp        >    w-frn-max-ele
                     go to rnx-400.
           if        w-sub-rel-stp        >    w-sub-rel-np9
                     go to rnx-400.
      *                  *---------------------------------------------*
      *                  * Se elemento non inserito si ricicla per po- *
      *                  * sizionarsi sul nr progressivo successivo    *
      *                  *---------------------------------------------*
           if        w-frn-flg-ins
                    (w-sub-rel-stp)       =    spaces
                     go to rnx-300.
      *                  *---------------------------------------------*
      *                  * Altrimenti si aggiorna il numero d'ordine   *
      *                  * della start e si esce                       *
      *                  *---------------------------------------------*
           add       1                    to   w-sub-rel-stn          .
           go to     rnx-999.
       rnx-400.
      *                  *---------------------------------------------*
      *                  * Normalizzazioni se ultimo record            *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-stn          .
           move      zero                 to   w-sub-rel-stp          .
       rnx-999.
           exit.

      *    *===========================================================*
      *    * Read Record                                               *
      *    *-----------------------------------------------------------*
       rea-000.
      *              *-------------------------------------------------*
      *              * Determinazione del numero progressivo relativo  *
      *              * al numero d'ordine passato                      *
      *              *-------------------------------------------------*
           move      w-sub-rel-num        to   w-sub-rel-prw          .
           perform   pdo-000              thru pdo-999                .
      *              *-------------------------------------------------*
      *              * Se errore : uscita                              *
      *              *-------------------------------------------------*
           if        w-sub-rel-exs        not  = spaces
                     go to rea-999.
           move      w-sub-rel-prw        to   w-sub-rel-prg          .
      *              *-------------------------------------------------*
      *              * Lettura record                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se record New                               *
      *                  *---------------------------------------------*
           if        w-frn-flg-new
                    (w-sub-rel-prg)       =    spaces
                     go to rea-600.
      *                      *-----------------------------------------*
      *                      * Si riempie il buffer con spaces         *
      *                      *-----------------------------------------*
           move      spaces               to   w-sub-rel-buf          .
           go to     rea-700.
       rea-600.
      *                  *---------------------------------------------*
      *                  * Se record non New                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione record number effettivo    *
      *                      *-----------------------------------------*
           move      w-frn-rnu-ins
                    (w-sub-rel-prg)       to   w-rcn-rnu-eff          .
      *                      *-----------------------------------------*
      *                      * Subroutine di lettura                   *
      *                      *-----------------------------------------*
           perform   let-000              thru let-999                .
       rea-700.
      *              *-------------------------------------------------*
      *              * Aggiornamento link-area                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero progressivo record precedente        *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-npp          .
           if        w-sub-rel-prg        =    w-sub-rel-np0
                     go to rea-750.
           move      w-sub-rel-prg        to   w-sub-rel-npp          .
       rea-725.
           subtract  1                    from w-sub-rel-npp          .
           if        w-frn-flg-ins
                    (w-sub-rel-npp)       =    spaces
                     go to rea-725.
       rea-750.
      *                  *---------------------------------------------*
      *                  * Numero progressivo record successivo        *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-nps          .
           if        w-sub-rel-prg        =    w-sub-rel-np9
                     go to rea-800.
           move      w-sub-rel-prg        to   w-sub-rel-nps          .
       rea-775.
           add       1                    to   w-sub-rel-nps          .
           if        w-frn-flg-ins
                    (w-sub-rel-nps)       =    spaces
                     go to rea-775.
       rea-800.
      *                  *---------------------------------------------*
      *                  * Numero progressivo per inserimento          *
      *                  *---------------------------------------------*
           move      w-sub-rel-prg        to   w-sub-rel-npi          .
           subtract  1                    from w-sub-rel-npi          .
           if        w-sub-rel-npi        not  > w-sub-rel-npp
                     move  zero           to   w-sub-rel-npi          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo per append               *
      *                  *---------------------------------------------*
           move      w-sub-rel-np9        to   w-sub-rel-npa          .
           add       1                    to   w-sub-rel-npa          .
           if        w-sub-rel-npa        >    w-frn-max-ele
                     move  zero           to   w-sub-rel-npa          .
      *                  *---------------------------------------------*
      *                  * Flag di inserimento possibile o no          *
      *                  *---------------------------------------------*
           if        w-sub-rel-npi        =    zero
                     move  "#"            to   w-sub-rel-ins
           else      move  spaces         to   w-sub-rel-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di append possibile o no               *
      *                  *---------------------------------------------*
           if        w-sub-rel-npa        =    zero
                     move  "#"            to   w-sub-rel-app
           else      move  spaces         to   w-sub-rel-app          .
      *                  *---------------------------------------------*
      *                  * Flag di record new                          *
      *                  *---------------------------------------------*
           if        w-frn-flg-new
                    (w-sub-rel-prg)       =    "#"
                     move  "#"            to   w-sub-rel-new
           else      move  spaces         to   w-sub-rel-new          .
      *                  *---------------------------------------------*
      *                  * Flag di record last                         *
      *                  *---------------------------------------------*
           if        w-sub-rel-prg        =    w-sub-rel-np9
                     move  "#"            to   w-sub-rel-lst
           else      move  spaces         to   w-sub-rel-lst          .
       rea-999.
           exit.

      *    *===========================================================*
      *    * Insert Record                                             *
      *    *-----------------------------------------------------------*
       ins-000.
      *              *-------------------------------------------------*
      *              * Se nr record invalido : insert eseguita senza   *
      *              * successo                                        *
      *              *-------------------------------------------------*
           if        w-sub-rel-prg        >    w-frn-max-ele or
                     w-sub-rel-prg        =    zero
                     go to ins-100.
      *              *-------------------------------------------------*
      *              * Se record gia' esistente : insert eseguita sen- *
      *              * za successo                                     *
      *              *-------------------------------------------------*
           if        w-frn-flg-ins
                    (w-sub-rel-prg)       =    spaces
                     go to ins-200.
       ins-100.
           move      "#"                  to   w-sub-rel-exs          .
           go to     ins-999.
       ins-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento work-area flags e record numbers  *
      *              *-------------------------------------------------*
           move      "#"                  to   w-frn-flg-ins
                                              (w-sub-rel-prg)         .
           add       1                    to   w-frn-max-rnu          .
           move      w-frn-max-rnu        to   w-frn-rnu-ins
                                              (w-sub-rel-prg)         .
           move      "#"                  to   w-frn-flg-new
                                              (w-sub-rel-prg)         .
      *              *-------------------------------------------------*
      *              * Aggiornamento link-area                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero progressivo minimo                   *
      *                  *---------------------------------------------*
           if        w-sub-rel-np0        =    zero
                     move  w-sub-rel-prg  to   w-sub-rel-np0          .
           if        w-sub-rel-prg        <    w-sub-rel-np0
                     move  w-sub-rel-prg  to   w-sub-rel-np0          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo massimo                  *
      *                  *---------------------------------------------*
           if        w-sub-rel-np9        =    zero
                     move  w-sub-rel-prg  to   w-sub-rel-np9          .
           if        w-sub-rel-prg        >    w-sub-rel-np9
                     move  w-sub-rel-prg  to   w-sub-rel-np9          .
      *                  *---------------------------------------------*
      *                  * Numero d'ordine del record corrente         *
      *                  *---------------------------------------------*
           move      w-frn-flg-ins
                    (w-sub-rel-prg)       to   w-sav-flg-000          .
           move      high-value           to   w-frn-flg-ins
                                              (w-sub-rel-prg)         .
           move      zero                 to   w-ctr-tal-001          .
           inspect   w-frn-tbf-ins    tallying w-ctr-tal-001
                     for   all "#"      before initial high-value     .
           add       1
                     w-ctr-tal-001      giving w-sub-rel-cur          .
           move      w-sav-flg-000        to   w-frn-flg-ins
                                              (w-sub-rel-prg)         .
      *                  *---------------------------------------------*
      *                  * Numero d'ordine del record di start         *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-stn          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo del record di start      *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-stp          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo record precedente        *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-npp          .
           if        w-sub-rel-prg        =    w-sub-rel-np0
                     go to ins-250.
           move      w-sub-rel-prg        to   w-sub-rel-npp          .
       ins-225.
           subtract  1                    from w-sub-rel-npp          .
           if        w-frn-flg-ins
                    (w-sub-rel-npp)       =    spaces
                     go to ins-225.
       ins-250.
      *                  *---------------------------------------------*
      *                  * Numero progressivo record successivo        *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-nps          .
           if        w-sub-rel-prg        =    w-sub-rel-np9
                     go to ins-300.
           move      w-sub-rel-prg        to   w-sub-rel-nps          .
       ins-275.
           add       1                    to   w-sub-rel-nps          .
           if        w-frn-flg-ins
                    (w-sub-rel-nps)       =    spaces
                     go to ins-275.
       ins-300.
      *                  *---------------------------------------------*
      *                  * Numero progressivo per inserimento          *
      *                  *---------------------------------------------*
           move      w-sub-rel-prg        to   w-sub-rel-npi          .
           subtract  1                    from w-sub-rel-npi          .
           if        w-sub-rel-npi        not  > w-sub-rel-npp
                     move  zero           to   w-sub-rel-npi          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo per append               *
      *                  *---------------------------------------------*
           move      w-sub-rel-np9        to   w-sub-rel-npa          .
           add       1                    to   w-sub-rel-npa          .
           if        w-sub-rel-npa        >    w-frn-max-ele
                     move  zero           to   w-sub-rel-npa          .
      *                  *---------------------------------------------*
      *                  * Flag di inserimento possibile o no          *
      *                  *---------------------------------------------*
           if        w-sub-rel-npi        =    zero
                     move  "#"            to   w-sub-rel-ins
           else      move  spaces         to   w-sub-rel-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di append possibile o no               *
      *                  *---------------------------------------------*
           if        w-sub-rel-npa        =    zero
                     move  "#"            to   w-sub-rel-app
           else      move  spaces         to   w-sub-rel-app          .
      *                  *---------------------------------------------*
      *                  * Flag di record new                          *
      *                  *---------------------------------------------*
           move      "#"                  to   w-sub-rel-new          .
      *                  *---------------------------------------------*
      *                  * Flag di record last                         *
      *                  *---------------------------------------------*
           if        w-sub-rel-prg        =    w-sub-rel-np9
                     move  "#"            to   w-sub-rel-lst
           else      move  spaces         to   w-sub-rel-lst          .
      *              *-------------------------------------------------*
      *              * Scrittura effettiva del record                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione record number effettivo        *
      *                  *---------------------------------------------*
           move      w-frn-rnu-ins
                    (w-sub-rel-prg)       to   w-rcn-rnu-eff          .
      *                  *---------------------------------------------*
      *                  * Preparazione area file a spaces             *
      *                  *---------------------------------------------*
           move      spaces               to   fil-rec                .
      *                  *---------------------------------------------*
      *                  * Subroutine di scrittura                     *
      *                  *---------------------------------------------*
           perform   scr-000              thru scr-999                .
       ins-999.
           exit.

      *    *===========================================================*
      *    * Put Record                                                *
      *    *-----------------------------------------------------------*
       put-000.
      *              *-------------------------------------------------*
      *              * Se nr record invalido : put eseguita senza      *
      *              * successo                                        *
      *              *-------------------------------------------------*
           if        w-sub-rel-prg        >    w-frn-max-ele or
                     w-sub-rel-prg        =    zero
                     go to put-100.
      *              *-------------------------------------------------*
      *              * Se record gia' esistente : put eseguita senza   *
      *              * successo                                        *
      *              *-------------------------------------------------*
           if        w-frn-flg-ins
                    (w-sub-rel-prg)       =    spaces
                     go to put-200.
       put-100.
           move      "#"                  to   w-sub-rel-exs          .
           go to     put-999.
       put-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento work-area flags e record numbers  *
      *              *-------------------------------------------------*
           move      "#"                  to   w-frn-flg-ins
                                              (w-sub-rel-prg)         .
           add       1                    to   w-frn-max-rnu          .
           move      w-frn-max-rnu        to   w-frn-rnu-ins
                                              (w-sub-rel-prg)         .
           move      spaces               to   w-frn-flg-new
                                              (w-sub-rel-prg)         .
      *              *-------------------------------------------------*
      *              * Aggiornamento link-area                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero elementi inseriti                    *
      *                  *---------------------------------------------*
           add       1                    to   w-sub-rel-max          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo minimo                   *
      *                  *---------------------------------------------*
           if        w-sub-rel-np0        =    zero
                     move  w-sub-rel-prg  to   w-sub-rel-np0          .
           if        w-sub-rel-prg        <    w-sub-rel-np0
                     move  w-sub-rel-prg  to   w-sub-rel-np0          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo massimo                  *
      *                  *---------------------------------------------*
           if        w-sub-rel-np9        =    zero
                     move  w-sub-rel-prg  to   w-sub-rel-np9          .
           if        w-sub-rel-prg        >    w-sub-rel-np9
                     move  w-sub-rel-prg  to   w-sub-rel-np9          .
      *                  *---------------------------------------------*
      *                  * Numero d'ordine del record corrente         *
      *                  *---------------------------------------------*
           move      w-frn-flg-ins
                    (w-sub-rel-prg)       to   w-sav-flg-000          .
           move      high-value           to   w-frn-flg-ins
                                              (w-sub-rel-prg)         .
           move      zero                 to   w-ctr-tal-001          .
           inspect   w-frn-tbf-ins    tallying w-ctr-tal-001
                     for   all "#"      before initial high-value     .
           add       1
                     w-ctr-tal-001      giving w-sub-rel-cur          .
           move      w-sav-flg-000        to   w-frn-flg-ins
                                              (w-sub-rel-prg)         .
      *                  *---------------------------------------------*
      *                  * Numero d'ordine del record di start         *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-stn          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo del record di start      *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-stp          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo record precedente        *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-npp          .
           if        w-sub-rel-prg        =    w-sub-rel-np0
                     go to put-250.
           move      w-sub-rel-prg        to   w-sub-rel-npp          .
       put-225.
           subtract  1                    from w-sub-rel-npp          .
           if        w-frn-flg-ins
                    (w-sub-rel-npp)       =    spaces
                     go to put-225.
       put-250.
      *                  *---------------------------------------------*
      *                  * Numero progressivo record successivo        *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-nps          .
           if        w-sub-rel-prg        =    w-sub-rel-np9
                     go to put-300.
           move      w-sub-rel-prg        to   w-sub-rel-nps          .
       put-275.
           add       1                    to   w-sub-rel-nps          .
           if        w-frn-flg-ins
                    (w-sub-rel-nps)        =    spaces
                     go to put-275.
       put-300.
      *                  *---------------------------------------------*
      *                  * Numero progressivo per inserimento          *
      *                  *---------------------------------------------*
           move      w-sub-rel-prg        to   w-sub-rel-npi          .
           subtract  1                    from w-sub-rel-npi          .
           if        w-sub-rel-npi        not  > w-sub-rel-npp
                     move  zero           to   w-sub-rel-npi          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo per append               *
      *                  *---------------------------------------------*
           move      w-sub-rel-np9        to   w-sub-rel-npa          .
           add       1                    to   w-sub-rel-npa          .
           if        w-sub-rel-npa        >    w-frn-max-ele
                     move  zero           to   w-sub-rel-npa          .
      *                  *---------------------------------------------*
      *                  * Flag di inserimento possibile o no          *
      *                  *---------------------------------------------*
           if        w-sub-rel-npi        =    zero
                     move  "#"            to   w-sub-rel-ins
           else      move  spaces         to   w-sub-rel-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di append possibile o no               *
      *                  *---------------------------------------------*
           if        w-sub-rel-npa        =    zero
                     move  "#"            to   w-sub-rel-app
           else      move  spaces         to   w-sub-rel-app          .
      *                  *---------------------------------------------*
      *                  * Flag di record new                          *
      *                  *---------------------------------------------*
           move      spaces               to   w-sub-rel-new          .
      *                  *---------------------------------------------*
      *                  * Flag di record last                         *
      *                  *---------------------------------------------*
           if        w-sub-rel-prg        =    w-sub-rel-np9
                     move  "#"            to   w-sub-rel-lst
           else      move  spaces         to   w-sub-rel-lst          .
      *              *-------------------------------------------------*
      *              * Scrittura effettiva del record                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione record number effettivo        *
      *                  *---------------------------------------------*
           move      w-frn-rnu-ins
                    (w-sub-rel-prg)       to   w-rcn-rnu-eff          .
      *                  *---------------------------------------------*
      *                  * Spostamento record in area file             *
      *                  *---------------------------------------------*
           move      w-sub-rel-buf        to   fil-rec                .
      *                  *---------------------------------------------*
      *                  * Subroutine di scrittura                     *
      *                  *---------------------------------------------*
           perform   scr-000              thru scr-999                .
       put-999.
           exit.

      *    *===========================================================*
      *    * Append Record                                             *
      *    *-----------------------------------------------------------*
       app-000.
      *              *-------------------------------------------------*
      *              * Se nr record invalido : append eseguita senza   *
      *              * successo                                        *
      *              *-------------------------------------------------*
           if        w-sub-rel-prg        >    w-frn-max-ele or
                     w-sub-rel-prg        =    zero
                     go to app-100.
      *              *-------------------------------------------------*
      *              * Se nr record non maggiore del massimo : update  *
      *              * eseguita senza successo                         *
      *              *-------------------------------------------------*
           if        w-sub-rel-prg        >    w-sub-rel-np9
                     go to app-200.
       app-100.
           move      "#"                  to   w-sub-rel-exs          .
           go to     app-999.
       app-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento work-area flags e record numbers  *
      *              *-------------------------------------------------*
           move      "#"                  to   w-frn-flg-ins
                                              (w-sub-rel-prg)         .
           add       1                    to   w-frn-max-rnu          .
           move      w-frn-max-rnu        to   w-frn-rnu-ins
                                              (w-sub-rel-prg)         .
           move      "#"                  to   w-frn-flg-new
                                              (w-sub-rel-prg)         .
      *              *-------------------------------------------------*
      *              * Aggiornamento link-area                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero progressivo minimo                   *
      *                  *---------------------------------------------*
           if        w-sub-rel-np0        =    zero
                     move  w-sub-rel-prg  to   w-sub-rel-np0          .
           if        w-sub-rel-prg        <    w-sub-rel-np0
                     move  w-sub-rel-prg  to   w-sub-rel-np0          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo massimo                  *
      *                  *---------------------------------------------*
           move      w-sub-rel-prg        to   w-sub-rel-np9          .
      *                  *---------------------------------------------*
      *                  * Numero d'ordine del record corrente         *
      *                  *---------------------------------------------*
           move      w-frn-flg-ins
                    (w-sub-rel-prg)       to   w-sav-flg-000          .
           move      high-value           to   w-frn-flg-ins
                                              (w-sub-rel-prg)         .
           move      zero                 to   w-ctr-tal-001          .
           inspect   w-frn-tbf-ins    tallying w-ctr-tal-001
                     for   all "#"      before initial high-value     .
           add       1
                     w-ctr-tal-001      giving w-sub-rel-cur          .
           move      w-sav-flg-000        to   w-frn-flg-ins
                                              (w-sub-rel-prg)         .
      *                  *---------------------------------------------*
      *                  * Numero d'ordine del record di start         *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-stn          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo del record di start      *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-stp          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo record precedente        *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-npp          .
           if        w-sub-rel-prg        =    w-sub-rel-np0
                     go to app-250.
           move      w-sub-rel-prg        to   w-sub-rel-npp          .
       app-225.
           subtract  1                    from w-sub-rel-npp          .
           if        w-frn-flg-ins
                    (w-sub-rel-npp)       =    spaces
                     go to app-225.
       app-250.
      *                  *---------------------------------------------*
      *                  * Numero progressivo record successivo        *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-nps          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo per inserimento          *
      *                  *---------------------------------------------*
           move      w-sub-rel-prg        to   w-sub-rel-npi          .
           subtract  1                    from w-sub-rel-npi          .
           if        w-sub-rel-npi        not  > w-sub-rel-npp
                     move  zero           to   w-sub-rel-npi          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo per append               *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-npa          .
      *                  *---------------------------------------------*
      *                  * Flag di inserimento possibile o no          *
      *                  *---------------------------------------------*
           if        w-sub-rel-npi        =    zero
                     move  "#"            to   w-sub-rel-ins
           else      move  spaces         to   w-sub-rel-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di append possibile o no               *
      *                  *---------------------------------------------*
           move      "#"                  to   w-sub-rel-app          .
      *                  *---------------------------------------------*
      *                  * Flag di record new                          *
      *                  *---------------------------------------------*
           move      "#"                  to   w-sub-rel-new          .
      *                  *---------------------------------------------*
      *                  * Flag di record last                         *
      *                  *---------------------------------------------*
           move      "#"                  to   w-sub-rel-lst          .
      *              *-------------------------------------------------*
      *              * Scrittura effettiva del record                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione record number effettivo        *
      *                  *---------------------------------------------*
           move      w-frn-rnu-ins
                    (w-sub-rel-prg)       to   w-rcn-rnu-eff          .
      *                  *---------------------------------------------*
      *                  * Preparazione area file a spaces             *
      *                  *---------------------------------------------*
           move      spaces               to   fil-rec                .
      *                  *---------------------------------------------*
      *                  * Subroutine di scrittura                     *
      *                  *---------------------------------------------*
           perform   scr-000              thru scr-999                .
       app-999.
           exit.

      *    *===========================================================*
      *    * Test esistenza numero progressivo dato                    *
      *    *-----------------------------------------------------------*
       tep-000.
      *              *-------------------------------------------------*
      *              * Se nr d'ordine invalido : errore                *
      *              *-------------------------------------------------*
           if        w-sub-rel-prw        =    zero       or
                     w-sub-rel-prw        >    w-frn-max-ele
                     go to tep-900.
           if        w-frn-flg-ins
                    (w-sub-rel-prw)       not  = "#"
                     go to tep-900.
      *              *-------------------------------------------------*
      *              * Altrimenti : uscita Ok                          *
      *              *-------------------------------------------------*
           go to     tep-999.
       tep-900.
      *              *-------------------------------------------------*
      *              * Se numero progressivo non esistente             *
      *              *-------------------------------------------------*
           move      "#"                  to   w-sub-rel-exs          .
       tep-999.
           exit.

      *    *===========================================================*
      *    * Estrazione numero progressivo da numero d'ordine          *
      *    *-----------------------------------------------------------*
       pdo-000.
      *              *-------------------------------------------------*
      *              * Se nr d'ordine invalido : estrazione numero     *
      *              * progressivo eseguito senza successo             *
      *              *-------------------------------------------------*
           if        w-sub-rel-prw        =    zero or
                     w-sub-rel-np0        =    zero
                     go to pdo-900.
       pdo-200.
      *              *-------------------------------------------------*
      *              * Determinazione del numero progressivo relativo  *
      *              * al numero d'ordine passato                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr-tbf-001          .
       pdo-300.
           move      zero                 to   w-ctr-tal-001          .
           inspect   w-frn-tbf-ins    tallying w-ctr-tal-001
                     for   characters   before initial   "#"          .
           if        w-ctr-tal-001        =    w-frn-max-ele
                     go to pdo-400.
           add       1                    to   w-ctr-tbf-001          .
           if        w-ctr-tbf-001        =    w-sub-rel-prw
                     go to pdo-500.
           add       1                    to   w-ctr-tal-001          .
           move      "."                  to   w-frn-flg-ins
                                              (w-ctr-tal-001)         .
           go to     pdo-300.
       pdo-400.
           inspect   w-frn-tbf-ins   replacing
                                          all  "."
                                          by   "#"                    .
           go to     pdo-900.
       pdo-500.
           inspect   w-frn-tbf-ins   replacing
                                          all  "."
                                          by   "#"                    .
      *                  *---------------------------------------------*
      *                  * Numero progressivo del record corrente      *
      *                  *---------------------------------------------*
           add       1                    to   w-ctr-tal-001          .
           move      w-ctr-tal-001        to   w-sub-rel-prw          .
           go to     pdo-999.
       pdo-900.
      *              *-------------------------------------------------*
      *              * Se estrazione numero progressivo errata         *
      *              *-------------------------------------------------*
           move      "#"                  to   w-sub-rel-exs          .
           move      zero                 to   w-sub-rel-prw          .
       pdo-999.
           exit.

      *    *===========================================================*
      *    * Estrazione numero progressivo da numero d'ordine          *
      *    *-----------------------------------------------------------*
       odp-000.
      *              *-------------------------------------------------*
      *              * Se nr progressivo invalido : estrazione numero  *
      *              * d'ordine eseguito senza successo                *
      *              *-------------------------------------------------*
           if        w-sub-rel-prw        =    zero       or
                     w-sub-rel-prw        >    w-frn-max-ele
                     go to odp-900.
           if        w-frn-flg-ins
                    (w-sub-rel-prw)       not  = "#"
                     go to odp-900.
      *              *-------------------------------------------------*
      *              * Determinazione del numero d'ordine              *
      *              *-------------------------------------------------*
           move      w-frn-flg-ins
                    (w-sub-rel-prw)       to   w-sav-flg-000          .
           move      high-value           to   w-frn-flg-ins
                                              (w-sub-rel-prw)         .
           move      zero                 to   w-ctr-tal-001          .
           inspect   w-frn-tbf-ins    tallying w-ctr-tal-001
                     for   all "#"      before initial high-value     .
           move      w-sav-flg-000        to   w-frn-flg-ins
                                              (w-sub-rel-prw)         .
           add       1
                     w-ctr-tal-001      giving w-sub-rel-prw          .
           go to     odp-999.
       odp-900.
      *              *-------------------------------------------------*
      *              * Se estrazione numero d'ordine errata            *
      *              *-------------------------------------------------*
           move      "#"                  to   w-sub-rel-exs          .
           move      zero                 to   w-sub-rel-prw          .
       odp-999.
           exit.

      *    *===========================================================*
      *    * Remove Record                                             *
      *    *-----------------------------------------------------------*
       rmv-000.
      *              *-------------------------------------------------*
      *              * Determinazione del numero progressivo relativo  *
      *              * al numero d'ordine passato                      *
      *              *-------------------------------------------------*
           move      w-sub-rel-num        to   w-sub-rel-prw          .
           perform   pdo-000              thru pdo-999                .
      *              *-------------------------------------------------*
      *              * Se errore : uscita                              *
      *              *-------------------------------------------------*
           if        w-sub-rel-exs        not  = spaces
                     go to rmv-999.
           move      w-sub-rel-prw        to   w-sub-rel-prg          .
      *              *-------------------------------------------------*
      *              * Aggiornamento work-area flags e record numbers  *
      *              *-------------------------------------------------*
           move      spaces               to   w-frn-flg-ins
                                              (w-sub-rel-prg)         .
           move      w-frn-flg-new
                    (w-sub-rel-prg)       to   w-sav-flg-000          .
           move      spaces               to   w-frn-flg-new
                                              (w-sub-rel-prg)         .
           if        w-frn-flg-del
                    (w-sub-rel-prg)       =    spaces    and
                     w-sav-flg-000        =    spaces
                     move  "#"            to   w-frn-flg-del
                                              (w-sub-rel-prg)
                     move  w-frn-rnu-ins
                          (w-sub-rel-prg) to   w-frn-rnu-del
                                              (w-sub-rel-prg)         .
           if        w-sav-flg-000        not  = spaces
                     subtract    1        from w-frn-max-rnu          .
           move      zero                 to   w-frn-rnu-ins
                                              (w-sub-rel-prg)         .
      *              *-------------------------------------------------*
      *              * Aggiornamento link-area                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero elementi inseriti                    *
      *                  *---------------------------------------------*
           if        w-sav-flg-000        =    spaces
                     subtract  1          from w-sub-rel-max          .
      *                  *---------------------------------------------*
      *                  * Salvataggio w-sub-rel-prg e w-sub-rel-max   *
      *                  *---------------------------------------------*
           move      w-sub-rel-prg        to   w-sav-rel-prg          .
           move      w-sub-rel-max        to   w-sav-rel-max          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione totale della link-area      *
      *                  *---------------------------------------------*
           perform   nla-000              thru nla-999                .
      *                  *---------------------------------------------*
      *                  * Se non c'e' alcun record inserito si esce   *
      *                  *---------------------------------------------*
           if        w-sav-rel-max        not  > zero
                     go to rmv-999.
      *                  *---------------------------------------------*
      *                  * Ripristino w-sub-rel-max                    *
      *                  *---------------------------------------------*
           move      w-sav-rel-max        to   w-sub-rel-max          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo minimo                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-ctr-tal-001          .
           inspect   w-frn-tbf-ins    tallying w-ctr-tal-001
                     for   characters   before initial   "#"          .
           add       1
                     w-ctr-tal-001      giving w-sub-rel-np0          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo massimo                  *
      *                  *---------------------------------------------*
       rmv-600.
           move      zero                 to   w-ctr-tal-001          .
           inspect   w-frn-tbf-ins    tallying w-ctr-tal-001
                     for   characters   before initial   "#"
                     replacing           first "#"
                                          by   "."                    .
           if        w-ctr-tal-001        <    w-frn-max-ele
                     add   1
                           w-ctr-tal-001
                                        giving w-sub-rel-np9
                     go to rmv-600.
           inspect   w-frn-tbf-ins   replacing
                                          all  "."
                                          by   "#"                    .
      *                  *---------------------------------------------*
      *                  * Numero progressivo record corrente          *
      *                  *---------------------------------------------*
           if        w-sav-rel-prg        >    w-sub-rel-np9
                     move  w-sub-rel-np9  to   w-sub-rel-prg
                     go to rmv-650.
           if        w-sav-rel-prg        <    w-sub-rel-np0
                     move  w-sub-rel-np0  to   w-sub-rel-prg
                     go to rmv-650.
           move      w-sav-rel-prg        to   w-sub-rel-prg          .
       rmv-625.
           subtract  1                    from w-sub-rel-prg          .
           if        w-frn-flg-ins
                    (w-sub-rel-prg)       =    spaces
                     go to rmv-625.
       rmv-650.
      *                  *---------------------------------------------*
      *                  * Numero d'ordine del record corrente         *
      *                  *---------------------------------------------*
           move      w-frn-flg-ins
                    (w-sub-rel-prg)       to   w-sav-flg-000          .
           move      high-value           to   w-frn-flg-ins
                                              (w-sub-rel-prg)         .
           move      zero                 to   w-ctr-tal-001          .
           inspect   w-frn-tbf-ins    tallying w-ctr-tal-001
                     for   all "#"      before initial high-value     .
           add       1
                     w-ctr-tal-001      giving w-sub-rel-cur          .
           move      w-sav-flg-000        to   w-frn-flg-ins
                                              (w-sub-rel-prg)         .
      *                  *---------------------------------------------*
      *                  * Numero progressivo record precedente        *
      *                  *---------------------------------------------*
           if        w-sub-rel-prg        =    w-sub-rel-np0
                     go to rmv-700.
           move      w-sub-rel-prg        to   w-sub-rel-npp          .
       rmv-675.
           subtract  1                    from w-sub-rel-npp          .
           if        w-frn-flg-ins
                    (w-sub-rel-npp)       =    spaces
                     go to rmv-675.
       rmv-700.
      *                  *---------------------------------------------*
      *                  * Numero d'ordine del record di start         *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-stn          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo del record di start      *
      *                  *---------------------------------------------*
           move      zero                 to   w-sub-rel-stp          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo record successivo        *
      *                  *---------------------------------------------*
           if        w-sub-rel-prg        =    w-sub-rel-np9
                     go to rmv-750.
           move      w-sub-rel-prg        to   w-sub-rel-nps          .
       rmv-725.
           add       1                    to   w-sub-rel-nps          .
           if        w-frn-flg-ins
                    (w-sub-rel-nps)       =    spaces
                     go to rmv-725.
       rmv-750.
      *                  *---------------------------------------------*
      *                  * Numero progressivo per inserimento          *
      *                  *---------------------------------------------*
           move      w-sub-rel-prg        to   w-sub-rel-npi          .
           subtract  1                    from w-sub-rel-npi          .
           if        w-sub-rel-npi        not  > w-sub-rel-npp
                     move  zero           to   w-sub-rel-npi          .
      *                  *---------------------------------------------*
      *                  * Numero progressivo per append               *
      *                  *---------------------------------------------*
           move      w-sub-rel-np9        to   w-sub-rel-npa          .
           add       1                    to   w-sub-rel-npa          .
           if        w-sub-rel-npa        >    w-frn-max-ele
                     move  zero           to   w-sub-rel-npa          .
      *                  *---------------------------------------------*
      *                  * Flag di inserimento possibile o no          *
      *                  *---------------------------------------------*
           if        w-sub-rel-npi        =    zero
                     move  "#"            to   w-sub-rel-ins
           else      move  spaces         to   w-sub-rel-ins          .
      *                  *---------------------------------------------*
      *                  * Flag di append possibile o no               *
      *                  *---------------------------------------------*
           if        w-sub-rel-npa        =    zero
                     move  "#"            to   w-sub-rel-app
           else      move  spaces         to   w-sub-rel-app          .
      *                  *---------------------------------------------*
      *                  * Flag di record new                          *
      *                  *---------------------------------------------*
           move      spaces               to   w-sub-rel-new          .
      *                  *---------------------------------------------*
      *                  * Flag di record last                         *
      *                  *---------------------------------------------*
           if        w-sub-rel-prg        =    w-sub-rel-np9
                     move  "#"            to   w-sub-rel-lst
           else      move  spaces         to   w-sub-rel-lst          .
      *              *-------------------------------------------------*
      *              * Lettura record corrente                         *
      *              *-------------------------------------------------*
           perform   rea-000              thru rea-999                .
       rmv-999.
           exit.

      *    *===========================================================*
      *    * Last removed                                              *
      *    *-----------------------------------------------------------*
       lrm-000.
      *              *-------------------------------------------------*
      *              * Ricerca del primo flag di elemento deletato     *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctr-tal-001          .
           inspect   w-frn-tbf-del    tallying w-ctr-tal-001
                     for   characters   before initial   "#"          .
           if        w-ctr-tal-001        <    w-frn-max-ele
                     go to lrm-200.
      *              *-------------------------------------------------*
      *              * Se last removed eseguita senza successo         *
      *              *-------------------------------------------------*
       lrm-100.
           move      "#"                  to   w-sub-rel-exs          .
           go to     lrm-999.
       lrm-200.
      *              *-------------------------------------------------*
      *              * Numero progressivo del record deletato          *
      *              *-------------------------------------------------*
           add       1                    to   w-ctr-tal-001          .
           move      w-ctr-tal-001        to   w-sub-rel-prg          .
      *              *-------------------------------------------------*
      *              * Rimozione flag di elemento deletato             *
      *              *-------------------------------------------------*
           move      spaces               to   w-frn-flg-del
                                              (w-sub-rel-prg)         .
      *              *-------------------------------------------------*
      *              * Lettura record                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione record number effettivo        *
      *                  *---------------------------------------------*
           move      w-frn-rnu-del
                    (w-sub-rel-prg)       to   w-rcn-rnu-eff          .
      *                      *-----------------------------------------*
      *                      * Subroutine di lettura                   *
      *                      *-----------------------------------------*
           perform   let-000              thru let-999                .
       lrm-700.
      *              *-------------------------------------------------*
      *              * Aggiornamento link-area                         *
      *              *-------------------------------------------------*
       lrm-999.
           exit.

      *    *===========================================================*
      *    * Lettura elemento subfile numero w-rcn-rnu-eff             *
      *    *-----------------------------------------------------------*
       let-000.
      *              *-------------------------------------------------*
      *              * Test se operazione in cache memory oppure no    *
      *              *-------------------------------------------------*
           if        w-rcn-rnu-eff        >    w-chm-max
                     go to let-200.
      *              *-------------------------------------------------*
      *              * Se operazione in cache memory                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione indice per cache memory        *
      *                  *---------------------------------------------*
           move      w-rcn-rnu-eff        to   w-chm-inx              .
      *                  *---------------------------------------------*
      *                  * Lettura da cache memory                     *
      *                  *---------------------------------------------*
           move      w-chm-ele
                    (w-chm-inx)           to   fil-rec                .
      *                  *---------------------------------------------*
      *                  * Spostamento area file in record             *
      *                  *---------------------------------------------*
           move      fil-rec              to   w-sub-rel-buf          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-999.
       let-200.
      *              *-------------------------------------------------*
      *              * Se operazione su file                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione record number                  *
      *                  *---------------------------------------------*
           move      w-rcn-rnu-eff        to   w-rcn-krn              .
      *                  *---------------------------------------------*
      *                  * Tentativo di lettura ; se invalid key o al- *
      *                  * tro errore : fatal-error                    *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           read      fil   with no lock
                           invalid key
                           move    e-not-fnd
                                          to   e-sts
                           go to   let-600.
           if        e-sts                not  = e-not-err
                     go to let-600.
      *                  *---------------------------------------------*
      *                  * Spostamento area file in record             *
      *                  *---------------------------------------------*
           move      fil-rec              to   w-sub-rel-buf          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-999.
       let-600.
      *                  *---------------------------------------------*
      *                  * Se fatal error                              *
      *                  *---------------------------------------------*
           perform   fte-000              thru fte-999                .
       let-999.
           exit.

      *    *===========================================================*
      *    * Scrittura elemento subfile numero w-rcn-rnu-eff           *
      *    *-----------------------------------------------------------*
       scr-000.
      *              *-------------------------------------------------*
      *              * Test se operazione in cache memory oppure no    *
      *              *-------------------------------------------------*
           if        w-rcn-rnu-eff        >    w-chm-max
                     go to scr-200.
      *              *-------------------------------------------------*
      *              * Se operazione in cache memory                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione indice per cache memory        *
      *                  *---------------------------------------------*
           move      w-rcn-rnu-eff        to   w-chm-inx              .
      *                  *---------------------------------------------*
      *                  * Scrittura in cache memory                   *
      *                  *---------------------------------------------*
           move      fil-rec              to   w-chm-ele
                                              (w-chm-inx)             .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     scr-999.
       scr-200.
      *              *-------------------------------------------------*
      *              * Se operazione su file                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione record number                  *
      *                  *---------------------------------------------*
           move      w-rcn-rnu-eff        to   w-rcn-krn              .
      *                  *---------------------------------------------*
      *                  * Tentativo di ri-scrittura ; se invalid key  *
      *                  * tentativo di scrittura                      *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           rewrite   fil-rec invalid key
                             go to   scr-400.
      *                      *-----------------------------------------*
      *                      * Se altri errori : fatal error           *
      *                      *-----------------------------------------*
           if        e-sts                =    e-not-err
                     go to scr-999
           else      go to scr-600.
       scr-400.
      *                  *---------------------------------------------*
      *                  * Tentativo di scrittura ; se invalid key     *
      *                  * tentativo di ri-scrittura                   *
      *                  *---------------------------------------------*
           move      e-not-err            to   e-sts                  .
           write     fil-rec invalid key
                             go to   scr-200.
      *                      *-----------------------------------------*
      *                      * Se altri errori : fatal error           *
      *                      *-----------------------------------------*
           if        e-sts                =    e-not-err
                     go to scr-999
           else      go to scr-600.
       scr-600.
      *                  *---------------------------------------------*
      *                  * Se fatal error                              *
      *                  *---------------------------------------------*
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
