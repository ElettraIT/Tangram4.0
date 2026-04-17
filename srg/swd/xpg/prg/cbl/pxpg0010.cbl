       Identification Division.
       Program-Id.                                 pxpg0010           .
      *================================================================*
      *                                                                *
      * Decremento numero utenti in uso e cancellazione di tutti i     *
      * files temporanei utilizzati durante la sessione                *
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
      *    * File Control [upn]                                        *
      *    *-----------------------------------------------------------*
           select  optional  upn   assign to disk         f-upn-pat
                             organization is indexed
                             access mode  is dynamic
                             record key   is upn-key
                             file status  is f-upn-sts                .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *================================================================*
      *    File Description [upn]                                      *
      *----------------------------------------------------------------*
       fd  upn       label record standard                            .
       01  upn-rec.
           05  upn-key.
               10  upn-tre                pic  x(04)                  .
               10  upn-kre                pic  x(12)                  .
           05  upn-dat.
               10  upn-prg                pic  9(04)                  .
               10  upn-l01                pic  x(12)                  .
               10  upn-l02                pic  x(12)                  .
               10  upn-l03                pic  x(12)                  .
               10  upn-l04                pic  x(12)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area per parametri di 'chaining' dal chiamante            *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/wenvxpgp.cpy"                   .

      *    *===========================================================*
      *    * Area di comunicazione per programmi della serie "pxpg"    *
      *    *-----------------------------------------------------------*
           copy      "swd/xpg/prg/cpy/pxpglink.cpy"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mopsys"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/o"                                  .

      *    *===========================================================*
      *    * Area ausiliaria per controllo i-o su [upn]                *
      *    *-----------------------------------------------------------*
       01  f-upn.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-upn-nam                  pic  x(04) value "upn "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-upn-pat                  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-upn-sts                  pic  x(02)                  .

      *    *===========================================================*
      *    * Work-area per controllo chiamate a modulo "mopsys"        *
      *    *-----------------------------------------------------------*
       01  w-ops.
      *        *-------------------------------------------------------*
      *        * Tipo di controllo da effettuarsi sulla licenza d'uso, *
      *        * come stabilito in funzione del sistema operativo o-   *
      *        * spite                                                 *
      *        *  - "00" : Nessun controllo                            *
      *        *  - "01" : Controllo solo su licenziatario             *
      *        *  - "02" : Controllo sia sul licenziatario che sul nu- *
      *        *           mero di utenti                              *
      *        *-------------------------------------------------------*
           05  w-ops-tlu                  pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Status                                                *
      *        *-------------------------------------------------------*
           05  w-ops-sts                  pic  9(02)                  .

      ******************************************************************
       Procedure Division             chaining w-env-v-dkb-home
                                               w-env-v-dkb-user
                                               w-env-v-dkb-ttyc
                                               w-env-v-dkb-term
                                               w-env-v-dkb-runt
                                               w-env-v-dkb-getp
                                               w-env-v-dkb-subt
                                               w-env-v-dkb-azie
                                               w-env-v-dkb-uten
                                               w-env-v-dkb-prms
                                               w-env-v-dkb-post       .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Raccolta variabili di chaining                  *
      *              *-------------------------------------------------*
           move      w-env-v-dkb-home     to   x-env-home             .
           move      w-env-v-dkb-user     to   x-env-user             .
           move      w-env-v-dkb-ttyc     to   x-env-ttyc             .
           move      w-env-v-dkb-term     to   x-env-term             .
           move      w-env-v-dkb-runt     to   x-env-runt             .
           move      spaces               to   x-env-pfix
                                               x-env-hoid             .
           unstring  w-env-v-dkb-getp
                                delimited by   all   spaces
                                          into x-env-pfix
                                               x-env-hoid             .
           move      w-env-v-dkb-subt     to   x-env-subt             .
           move      w-env-v-dkb-azie     to   x-env-azie             .
           move      w-env-v-dkb-uten     to   x-env-uten             .
           move      w-env-v-dkb-prms     to   x-env-prms             .
           move      w-env-v-dkb-post     to   x-env-post             .
      *              *-------------------------------------------------*
      *              * Apertura modulo                       "mopsys"  *
      *              *-------------------------------------------------*
           perform   opn-mod-ops-000      thru opn-mod-ops-999        .
      *              *-------------------------------------------------*
      *              * Se status di uscita ad errore : uscita          *
      *              *-------------------------------------------------*
           if        w-ops-sts            not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Cancellazione di tutti i files temporanei uti-  *
      *              * lizzati durante la sessione                     *
      *              *-------------------------------------------------*
           perform   del-tmp-000          thru del-tmp-999            .
      *              *-------------------------------------------------*
      *              * Decremento numero progressivo utenti in uso     *
      *              *-------------------------------------------------*
           perform   dec-upn-000          thru dec-upn-999            .
      *              *-------------------------------------------------*
      *              * Chiusura modulo                       "mopsys"  *
      *              *-------------------------------------------------*
           perform   cls-mod-ops-000      thru cls-mod-ops-999        .
       main-999.
      *              *-------------------------------------------------*
      *              * Esecuzione dello 'Stop Run'                     *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mstopr"                         .

      *    *===========================================================*
      *    * Apertura modulo                                 "mopsys"  *
      *    *-----------------------------------------------------------*
       opn-mod-ops-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-ops-sts              .
      *              *-------------------------------------------------*
      *              * Funzione Open                                   *
      *              *-------------------------------------------------*
           move      "OP"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Se status di uscita ad errore : a trattamento   *
      *              * errore                                          *
      *              *-------------------------------------------------*
           if        o-sts                not  = spaces
                     go to opn-mod-ops-600.
       opn-mod-ops-100.
      *              *-------------------------------------------------*
      *              * Completamento funzione Open                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Variabile V_DKB_HOME                        *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "HOME"               to   o-com                  .
           move      x-env-home           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_DKB_USER                        *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "USER"               to   o-com                  .
           move      x-env-user           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_DKB_TTYC                        *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "TTYC"               to   o-com                  .
           move      x-env-ttyc           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_DKB_TERM                        *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "TERM"               to   o-com                  .
           move      x-env-term           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_DKB_RUNT                        *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "RUNT"               to   o-com                  .
           move      x-env-runt           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_DKB_GETP, componente PFIX       *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "PFIX"               to   o-com                  .
           move      x-env-pfix           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_DKB_GETP, componente HOID       *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "HOID"               to   o-com                  .
           move      x-env-hoid           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_DKB_SUBT                        *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "SUBT"               to   o-com                  .
           move      x-env-subt           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_DKB_AZIE                        *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "AZIE"               to   o-com                  .
           move      x-env-azie           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_DKB_UTEN                        *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "UTEN"               to   o-com                  .
           move      x-env-uten           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_DKB_PRMS                        *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "PRMS"               to   o-com                  .
           move      x-env-prms           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Variabile V_DKB_POST                        *
      *                  *---------------------------------------------*
           move      "O2"                 to   o-ope                  .
           move      "POST"               to   o-com                  .
           move      x-env-post           to   o-pat                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       opn-mod-ops-200.
      *              *-------------------------------------------------*
      *              * Funzione di determinazione tipo di licenza d'u- *
      *              * so a seconda del sistema operativo ospite       *
      *              *-------------------------------------------------*
           move      "LU"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *              *-------------------------------------------------*
      *              * Memorizzazione del tipo di licenza d'uso a se-  *
      *              * conda del sistema operativo ospite              *
      *              *-------------------------------------------------*
           move      o-sts                to   w-ops-tlu              .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     opn-mod-ops-999.
       opn-mod-ops-600.
      *              *-------------------------------------------------*
      *              * Se errore in funzione Open modulo               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Status di uscita ad errore                  *
      *                  *---------------------------------------------*
           move      "##"                 to   w-ops-sts              .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-mod-ops-999.
       opn-mod-ops-999.
           exit.

      *    *===========================================================*
      *    * Chiusura modulo                                 "mopsys"  *
      *    *-----------------------------------------------------------*
       cls-mod-ops-000.
      *              *-------------------------------------------------*
      *              * Tipo operazione                                 *
      *              *-------------------------------------------------*
           move      "CL"                 to   o-ope                  .
      *              *-------------------------------------------------*
      *              * Richiamo modulo                                 *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       cls-mod-ops-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione di tutti i files temporanei utilizzati du-  *
      *    * rante la sessione                                         *
      *    *-----------------------------------------------------------*
       del-tmp-000.
      *              *-------------------------------------------------*
      *              * Se la variabile 'x-env-pfix' non e' definita :  *
      *              * uscita senza alcuna azione                      *
      *              *-------------------------------------------------*
           if        x-env-pfix           =    spaces
                     go to del-tmp-999.
      *              *-------------------------------------------------*
      *              * Rimozione di tutti i files contenuti nella sub- *
      *              * directory [tmp] , aventi il prefisso indicato   *
      *              * da x-env-pfix                                   *
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
           move      "R9"                 to   o-ope                  .
           move      x-env-pfix           to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
       del-tmp-999.
           exit.

      *    *===========================================================*
      *    * Decremento numero progressivo utenti in uso               *
      *    *-----------------------------------------------------------*
       dec-upn-000.
      *              *-------------------------------------------------*
      *              * Se si e' in esecuzione di un desk-accessory :   *
      *              * uscita immediata                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiesta del livello di esecuzione relati- *
      *                  * vo ai desk-accessories                      *
      *                  *---------------------------------------------*
           move      "LD"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        o-sts                not  < "01" and
                     o-sts                not  > "99"
                     go to dec-upn-999.
       dec-upn-100.
      *              *-------------------------------------------------*
      *              * Se il tipo di licenza d'uso a seconda del si-   *
      *              * stema operativo ospite non prevede il control-  *
      *              * lo sul numero utenti : uscita immediata         *
      *              *-------------------------------------------------*
           if        w-ops-tlu            not  = "02"
                     go to dec-upn-999.
       dec-upn-300.
      *              *-------------------------------------------------*
      *              * Preparazione pathname per [upn]                 *
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
           move      "upn"                to   o-com                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      "A9"                 to   o-ope                  .
           call      "swd/mod/prg/obj/mopsys"
                                         using o                      .
      *
           move      o-pat                to   f-upn-pat              .
       dec-upn-400.
      *              *-------------------------------------------------*
      *              * Open [upn]                                      *
      *              *-------------------------------------------------*
           open      i-o    upn                                       .
       dec-upn-500.
      *              *-------------------------------------------------*
      *              * Lettura record [upn]                            *
      *              *-------------------------------------------------*
       dec-upn-525.
      *                  *---------------------------------------------*
      *                  * Preparazione chiave                         *
      *                  *---------------------------------------------*
           move      "RDCG"               to   upn-tre                .
           move      "            "       to   upn-kre                .
       dec-upn-550.
      *                  *---------------------------------------------*
      *                  * Lettura con lock, e deviazione a seconda    *
      *                  * dell'esito della lettura                    *
      *                  *---------------------------------------------*
           read      upn    invalid key
                            go to   dec-upn-575.
           go to     dec-upn-600.
       dec-upn-575.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [upn]                            *
      *                      *-----------------------------------------*
           unlock    upn    records                                   .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     dec-upn-900.
       dec-upn-600.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad aggiornamento [upn]                  *
      *                      *-----------------------------------------*
           go to     dec-upn-700.
       dec-upn-700.
      *              *-------------------------------------------------*
      *              * Aggiornamento [upn]                             *
      *              *-------------------------------------------------*
       dec-upn-725.
      *                  *---------------------------------------------*
      *                  * Decremento numero progressivo               *
      *                  *---------------------------------------------*
           if        upn-prg              >    zero
                     subtract 1           from upn-prg                .
       dec-upn-750.
      *                  *---------------------------------------------*
      *                  * Riscrittura record [upn], e deviazione a    *
      *                  * seconda dell'esito                          *
      *                  *---------------------------------------------*
           rewrite   upn-rec  invalid key
                              go to   dec-upn-775.
           go to     dec-upn-800.
       dec-upn-775.
      *                  *---------------------------------------------*
      *                  * Se riscrittura record [upn] errata          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [upn]                            *
      *                      *-----------------------------------------*
           unlock    upn    records                                   .
      *                      *-----------------------------------------*
      *                      * Riciclo a lettura                       *
      *                      *-----------------------------------------*
           go to     dec-upn-500.
       dec-upn-800.
      *                  *---------------------------------------------*
      *                  * Se riscrittura record [upn] a buon fine     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Unlock [upn]                            *
      *                      *-----------------------------------------*
           unlock    upn    records                                   .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     dec-upn-900.
       dec-upn-900.
      *              *-------------------------------------------------*
      *              * Close [upn]                                     *
      *              *-------------------------------------------------*
           close     upn                                              .
       dec-upn-999.
           exit.
