       Identification Division.
       Program-Id.                                 msetsvf0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    svf                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 24/11/92    *
      *                       Ultima revisione:    NdK del 03/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo per la determinazione set generati   *
      *                    da statistiche di vendita sul fatturato     *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *                                                                *
      *        Input  : w-mod-set-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : w-mod-set-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : w-mod-set-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : w-mod-set-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "NO" - Normalizzazione set                                     *
      *                                                                *
      *                                                                *
      *        Input  : w-mod-set-tip-ope = "NO"                       *
      *                                                                *
      *                 w-mod-set-ide-ute = utente                     *
      *                                                                *
      *                 w-mod-set-ide-fas = fase da normalizzare       *
      *                                                                *
      *                 w-mod-set-msg-exi = descrizione fase           *
      *                                                                *
      *                                                                *
      *        Output : w-mod-set-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CS" - Controllo di esistenza set                              *
      *                                                                *
      *                                                                *
      *        Input  : w-mod-set-tip-ope = "CS"                       *
      *                                                                *
      *                 w-mod-set-ide-ute = utente                     *
      *                                                                *
      *                 w-mod-set-ide-fas = fase da controllare        *
      *                                                                *
      *                                                                *
      *        Output : w-mod-set-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "UP" - Update set                                              *
      *                                                                *
      *                                                                *
      *        Input  : w-mod-set-tip-ope = "UP"                       *
      *                                                                *
      *                 w-mod-set-ide-ute = utente                     *
      *                                                                *
      *                 w-mod-set-ide-fas = fase da aggiornare         *
      *                                                                *
      *                 w-mod-set-tip-arc = tipo archivio              *
      *                                                                *
      *                 w-mod-set-cod-arc = codice archivio            *
      *                                                                *
      *                 w-mod-set-alf-arc = codice alfa archivio       *
      *                                                                *
      *                 w-mod-set-dat-ac1 = dati facoltativi           *
      *                 w-mod-set-dat-ac2                              *
      *                 w-mod-set-dat-ac3                              *
      *                                                                *
      *                                                                *
      *        Output : w-mod-set-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "RD" - Read set                                                *
      *                                                                *
      *                                                                *
      *        Input  : w-mod-set-tip-ope = "RD"                       *
      *                                                                *
      *                 w-mod-set-ide-ute = utente                     *
      *                                                                *
      *                 w-mod-set-ide-fas = fase da leggere            *
      *                                                                *
      *                 w-mod-set-tip-arc = tipo archivio              *
      *                                                                *
      *                 w-mod-set-cod-arc = codice archivio            *
      *                                                                *
      *                 w-mod-set-alf-arc = codice alfa archivio       *
      *                                                                *
      *                 w-mod-set-dat-ac1 = dati facoltativi           *
      *                 w-mod-set-dat-ac2                              *
      *                 w-mod-set-dat-ac3                              *
      *                                                                *
      *                                                                *
      *        Output : w-mod-set-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "FN" - Find set                                                *
      *                                                                *
      *                                                                *
      *        Input  : w-mod-set-tip-ope = "FN"                       *
      *                                                                *
      *                 w-mod-set-ide-ute = utente                     *
      *                                                                *
      *                 w-mod-set-tip-arc = tipo archivio              *
      *                                                                *
      *                                                                *
      *        Output : w-mod-set-ide-fas = fase selezionata           *
      *                                                                *
      *                 w-mod-set-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
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
      *    * File Control [sqz]                                        *
      *    *-----------------------------------------------------------*
           select  sqz       assign to input-output         f-sqz-pat
                             organization is line sequential
                             access  mode is sequential
                             file  status is                f-sqz-sts .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [sqz]                                    *
      *    *-----------------------------------------------------------*
       fd  sqz.
      *    *-----------------------------------------------------------*
      *    * Record                                                    *
      *    *-----------------------------------------------------------*
       01  sqz-rec.
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  sqz-chr occurs       2048  pic  x(01)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area ausiliaria per controlli i-o su [sqz]                *
      *    *-----------------------------------------------------------*
       01  f-sqz.
      *        *-------------------------------------------------------*
      *        * File name                                             *
      *        *-------------------------------------------------------*
           05  f-sqz-nam                  pic  x(04) value "sqz "     .
      *        *-------------------------------------------------------*
      *        * File pathname                                         *
      *        *-------------------------------------------------------*
           05  f-sqz-pat                  pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * File status                                           *
      *        *-------------------------------------------------------*
           05  f-sqz-sts                  pic  x(02) value "00"       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mvideo"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

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
      *        * [svs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/svf/fls/rec/rfsvs"                          .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) value zero       .

      *    *===========================================================*
      *    * Work per buffer set disponibili                           *
      *    *-----------------------------------------------------------*
       01  w-buf-set.
      *        *-------------------------------------------------------*
      *        * Work per buffer                                       *
      *        *-------------------------------------------------------*
           05  w-buf-set-fnd.
      *            *---------------------------------------------------*
      *            * Tipo archivio di richiesta                        *
      *            *---------------------------------------------------*
               10  w-buf-set-fnd-tar      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di selezione                                 *
      *            *---------------------------------------------------*
               10  w-buf-set-fnd-fds      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Set selezionato                                   *
      *            *---------------------------------------------------*
               10  w-buf-set-fnd-sts      pic  x(06)                  .
      *            *---------------------------------------------------*
      *            * Data documento selezionato                        *
      *            *---------------------------------------------------*
               10  w-buf-set-fnd-dds      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo                                    *
      *            *---------------------------------------------------*
               10  w-buf-set-fnd-c01      pic  9(02)                  .
               10  w-buf-set-fnd-c02      pic  9(02)                  .
               10  w-buf-set-fnd-c03      pic  9(02)                  .
               10  w-buf-set-fnd-c04      pic  9(02)                  .
               10  w-buf-set-fnd-c05      pic  9(02)                  .
               10  w-buf-set-fnd-nli      pic  9(02)                  .
               10  w-buf-set-fnd-crb      pic  9(02)                  .
               10  w-buf-set-fnd-cpb      pic  9(02)                  .
               10  w-buf-set-fnd-cpa      pic  9(02)                  .
               10  w-buf-set-fnd-max      pic  9(02) value 54         .
               10  w-buf-set-fnd-buf occurs 54.
                   15  w-buf-set-fnd-fas  pic  x(06)                  .
                   15  w-buf-set-fnd-dat  pic  9(07)                  .
                   15  w-buf-set-fnd-dfs  pic  x(40)                  .
               10  w-buf-set-fnd-ltp.
                   15  filler             pic  x(07) value "Pagina "  .
                   15  w-buf-set-fnd-lt1  pic  9(01)                  .
                   15  filler             pic  x(04) value " di "     .
                   15  w-buf-set-fnd-lt2  pic  9(01)                  .

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
      *    * Link-area per modulo dell'area 'svf'           'msetsvf0' *
      *    *-----------------------------------------------------------*
           copy      "pgm/svf/prg/cpy/msetsvf0.mdl"                   .

      ******************************************************************
       Procedure Division                using w-mod-set              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-mod-set-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           if        w-mod-set-tip-ope    =    "OP"
                     perform opn-000      thru opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   w-mod-set-tip-ope    =    "CL"
                     perform cls-000      thru cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   w-mod-set-tip-ope    =    "C?"
                     perform tcm-000      thru tcm-999
      *                  *---------------------------------------------*
      *                  * Normalizzazione set                         *
      *                  *---------------------------------------------*
           else if   w-mod-set-tip-ope    =    "NO"
                     perform nor-000      thru nor-999
      *                  *---------------------------------------------*
      *                  * Controllo set                               *
      *                  *---------------------------------------------*
           else if   w-mod-set-tip-ope    =    "CS"
                     perform ctl-000      thru ctl-999
      *                  *---------------------------------------------*
      *                  * Update set                                  *
      *                  *---------------------------------------------*
           else if   w-mod-set-tip-ope    =    "UP"
                     perform upd-000      thru upd-999
      *                  *---------------------------------------------*
      *                  * Read set                                    *
      *                  *---------------------------------------------*
           else if   w-mod-set-tip-ope    =    "RD"
                     perform rea-000      thru rea-999
      *                  *---------------------------------------------*
      *                  * Find set                                    *
      *                  *---------------------------------------------*
           else if   w-mod-set-tip-ope    =    "FN"
                     perform fnd-000      thru fnd-999                .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       opn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione progressivo sequenziale         *
      *              *-------------------------------------------------*
           move      zero                 to   w-mod-set-prg-sqz      .
      *              *-------------------------------------------------*
      *              * Incremento contatore Open modulo                *
      *              *-------------------------------------------------*
           add       1                    to   w-nol-ctr-opn          .
       opn-100.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
       opn-200.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [svs]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/svf/fls/ioc/obj/iofsvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-svs                 .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open modulo                *
      *              *-------------------------------------------------*
           subtract  1                    from w-nol-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [svs]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/svf/fls/ioc/obj/iofsvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-svs                 .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Test cancellabilita' modulo                               *
      *    *-----------------------------------------------------------*
       tcm-000.
      *              *-------------------------------------------------*
      *              * Se il contatore di Open e' a zero il modulo e'  *
      *              * cancellabile, altrimenti non lo e'              *
      *              *-------------------------------------------------*
           if        w-nol-ctr-opn        =    zero
                     move  spaces         to   w-mod-set-exi-sts
           else      move  "#"            to   w-mod-set-exi-sts      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione set                                       *
      *    *-----------------------------------------------------------*
       nor-000.
      *              *-------------------------------------------------*
      *              * Open file di appoggio sequenziale [sqz]         *
      *              *-------------------------------------------------*
           perform   opn-fil-sqz-000      thru opn-fil-sqz-999        .
       nor-100.
      *              *-------------------------------------------------*
      *              * Start su file [svs]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "UTEFASARC "         to   f-key                  .
           move      w-mod-set-ide-ute    to   rf-svs-ide-ute         .
           move      w-mod-set-ide-fas    to   rf-svs-ide-fas         .
           move      zero                 to   rf-svs-prg-sqz         .
           move      zero                 to   rf-svs-tip-arc         .
           move      zero                 to   rf-svs-cod-arc         .
           move      spaces               to   rf-svs-alf-arc         .
           move      "pgm/svf/fls/ioc/obj/iofsvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-svs                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to nor-900.
       nor-200.
      *              *-------------------------------------------------*
      *              * Next su [svs]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/svf/fls/ioc/obj/iofsvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-svs                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : oltre                         *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to nor-800.
       nor-300.
      *              *-------------------------------------------------*
      *              * Max su [svs], se non superato : oltre           *
      *              *-------------------------------------------------*
           if        rf-svs-ide-ute       not  = w-mod-set-ide-ute or
                     rf-svs-ide-fas       not  = w-mod-set-ide-fas
                     go to nor-800.
       nor-300.
      *              *-------------------------------------------------*
      *              * Da area record [svs] ad area record file        *
      *              * sequenziale [sqz]                               *
      *              *-------------------------------------------------*
           move      rf-svs               to   sqz-rec                .
      *              *-------------------------------------------------*
      *              * Scrittura record file sequenziale [sqz]         *
      *              *-------------------------------------------------*
           perform   wrt-fil-sqz-000      thru wrt-fil-sqz-999        .
       nor-600.
      *              *-------------------------------------------------*
      *              * Riciclo alla lettura file [svs]                 *
      *              *-------------------------------------------------*
           go to     nor-200.
       nor-800.
      *              *-------------------------------------------------*
      *              * Close file di appoggio sequenziale [sqz]        *
      *              *-------------------------------------------------*
           perform   cls-fil-sqz-000      thru cls-fil-sqz-999        .
      *              *-------------------------------------------------*
      *              * Open file di appoggio sequenziale [sqz]         *
      *              *-------------------------------------------------*
           perform   opn-fil-sqz-000      thru opn-fil-sqz-999        .
       nor-850.
      *              *-------------------------------------------------*
      *              * Read file di appoggio sequenziale [sqz]         *
      *              *-------------------------------------------------*
           perform   rea-fil-sqz-000      thru rea-fil-sqz-999        .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sqz-sts            not  = e-not-err
                     go to nor-900.
      *                  *---------------------------------------------*
      *                  * Da area record file sequenziale [sqz] ad a- *
      *                  * rea record [svs]                            *
      *                  *---------------------------------------------*
           move      sqz-rec              to   rf-svs                 .
      *                  *---------------------------------------------*
      *                  * Delete record                               *
      *                  *---------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "pgm/svf/fls/ioc/obj/iofsvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-svs                 .
      *                  *---------------------------------------------*
      *                  * Riciclo alla lettura sequenziale archivio   *
      *                  * [sqz]                                       *
      *                  *---------------------------------------------*
           go to     nor-850.
       nor-900.
      *              *-------------------------------------------------*
      *              * Operazioni in uscita                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file di appoggio sequenziale [sqz]    *
      *                  *---------------------------------------------*
           perform   cls-fil-sqz-000      thru cls-fil-sqz-999        .
      *                  *---------------------------------------------*
      *                  * Delete file di appoggio sequenziale [sqz]   *
      *                  *---------------------------------------------*
           perform   del-fil-sqz-000      thru del-fil-sqz-999        .
       nor-999.
           exit.

      *    *===========================================================*
      *    * Controllo set                                             *
      *    *-----------------------------------------------------------*
       ctl-000.
      *              *-------------------------------------------------*
      *              * Preparazione flag di uscita                     *
      *              *-------------------------------------------------*
           move      "#"                  to   w-mod-set-exi-sts      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-mod-set-prg-sqz      .
       ctl-100.
      *              *-------------------------------------------------*
      *              * Start su file [svs]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "UTEFASARC "         to   f-key                  .
           move      w-mod-set-ide-ute    to   rf-svs-ide-ute         .
           move      w-mod-set-ide-fas    to   rf-svs-ide-fas         .
           move      zero                 to   rf-svs-prg-sqz         .
           move      zero                 to   rf-svs-tip-arc         .
           move      zero                 to   rf-svs-cod-arc         .
           move      spaces               to   rf-svs-alf-arc         .
           move      "pgm/svf/fls/ioc/obj/iofsvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-svs                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-900.
       ctl-200.
      *              *-------------------------------------------------*
      *              * Next su [svs]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/svf/fls/ioc/obj/iofsvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-svs                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to ctl-900.
       ctl-300.
      *              *-------------------------------------------------*
      *              * Max su [svs], se non superato : ad uscita       *
      *              *-------------------------------------------------*
           if        rf-svs-ide-ute       not  = w-mod-set-ide-ute or
                     rf-svs-ide-fas       not  = w-mod-set-ide-fas
                     go to ctl-900.
       ctl-400.
      *              *-------------------------------------------------*
      *              * Preparazione flag di uscita                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-mod-set-exi-sts      .
       ctl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ctl-999. 
       ctl-999.
           exit.

      *    *===========================================================*
      *    * Update set                                                *
      *    *-----------------------------------------------------------*
       upd-000.
      *              *-------------------------------------------------*
      *              * Test se primo elemento                          *
      *              *-------------------------------------------------*
           if        w-mod-set-prg-sqz    >    zero
                     go to upd-200.
       upd-100.
      *              *-------------------------------------------------*
      *              * Scrittura record di testa                       *
      *              *-------------------------------------------------*
           perform   wrt-rec-svs-tes-000  thru wrt-rec-svs-tes-999    .
       upd-200.
      *              *-------------------------------------------------*
      *              * Incremento progressivo sequenziale              *
      *              *-------------------------------------------------*
           add       100                  to   w-mod-set-prg-sqz      .
       upd-400.
      *              *-------------------------------------------------*
      *              * Write record [svs]                              *
      *              *-------------------------------------------------*
           perform   wrt-rec-svs-000      thru wrt-rec-svs-999        .
       upd-999.
           exit.

      *    *===========================================================*
      *    * Read set                                                  *
      *    *-----------------------------------------------------------*
       rea-000.
      *              *-------------------------------------------------*
      *              * Preparazione flag di uscita                     *
      *              *-------------------------------------------------*
           move      "#"                  to   w-mod-set-exi-sts      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-mod-set-prg-sqz      .
       rea-100.
      *              *-------------------------------------------------*
      *              * Start su file [svs]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "UTEARCFAS "         to   f-key                  .
           move      w-mod-set-ide-ute    to   rf-svs-ide-ute         .
           move      w-mod-set-tip-arc    to   rf-svs-tip-arc         .
           move      w-mod-set-cod-arc    to   rf-svs-cod-arc         .
           move      w-mod-set-alf-arc    to   rf-svs-alf-arc         .
           move      w-mod-set-ide-fas    to   rf-svs-ide-fas         .
           move      zero                 to   rf-svs-prg-sqz         .
           move      "pgm/svf/fls/ioc/obj/iofsvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-svs                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rea-900.
       rea-200.
      *              *-------------------------------------------------*
      *              * Next su [svs]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/svf/fls/ioc/obj/iofsvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-svs                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : ad uscita                     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rea-900.
       rea-300.
      *              *-------------------------------------------------*
      *              * Max su [svs], se non superato : ad uscita       *
      *              *-------------------------------------------------*
           if        rf-svs-ide-ute       not  = w-mod-set-ide-ute or
                     rf-svs-tip-arc       not  = w-mod-set-tip-arc or
                     rf-svs-cod-arc       not  = w-mod-set-cod-arc or
                     rf-svs-alf-arc       not  = w-mod-set-alf-arc or
                     rf-svs-ide-fas       not  = w-mod-set-ide-fas
                     go to rea-900.
       rea-400.
      *              *-------------------------------------------------*
      *              * Preparazione dati accessori di uscita           *
      *              *-------------------------------------------------*
           move      rf-svs-dat-ac1       to   w-mod-set-dat-ac1      .
           move      rf-svs-dat-ac2       to   w-mod-set-dat-ac2      .
           move      rf-svs-dat-ac3       to   w-mod-set-dat-ac3      .
      *              *-------------------------------------------------*
      *              * Preparazione flag di uscita                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-mod-set-exi-sts      .
       rea-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rea-999. 
       rea-999.
           exit.

      *    *===========================================================*
      *    * Find set                                                  *
      *    *-----------------------------------------------------------*
       fnd-000.
      *              *-------------------------------------------------*
      *              * Preparazione flag di uscita                     *
      *              *-------------------------------------------------*
           move      "#"                  to   w-mod-set-exi-sts      .
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-mod-set-ide-fas      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-set-fnd-crb      .
       fnd-100.
      *              *-------------------------------------------------*
      *              * Start su [svs]                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "UTEARCFAS "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-mod-set-ide-ute    to   rf-svs-ide-ute         .
           move      w-mod-set-tip-arc    to   rf-svs-tip-arc         .
           move      zero                 to   rf-svs-cod-arc         .
           move      spaces               to   rf-svs-alf-arc         .
           move      spaces               to   rf-svs-ide-fas         .
           move      zero                 to   rf-svs-prg-sqz         .
           move      "pgm/svf/fls/ioc/obj/iofsvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-svs                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to  fnd-999.
       fnd-200.
      *              *-------------------------------------------------*
      *              * Lettura [svs]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/svf/fls/ioc/obj/iofsvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-svs                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a controllo contatore           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to fnd-500.
       fnd-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a controllo contatore     *
      *              *-------------------------------------------------*
           if        rf-svs-ide-ute       not  = w-mod-set-ide-ute or
                     rf-svs-tip-arc       not  = w-mod-set-tip-arc or
                     rf-svs-cod-arc       not  = zero              or
                     rf-svs-alf-arc       not  = spaces
                     go to fnd-500.
       fnd-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [svs]                              *
      *              *-------------------------------------------------*
       fnd-410.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-set-fnd-crb      .
      *              *-------------------------------------------------*
      *              * Test se buffer oltre il numero previsto         *
      *              *-------------------------------------------------*
           if        w-buf-set-fnd-crb    >    w-buf-set-fnd-max
                     go to fnd-500.
       fnd-420.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice fase                                 *
      *                  *---------------------------------------------*
           move      rf-svs-ide-fas       to   w-buf-set-fnd-fas
                                              (w-buf-set-fnd-crb)     .
      *                  *---------------------------------------------*
      *                  * Data                                        *
      *                  *---------------------------------------------*
           move      rf-svs-ide-dat       to   w-buf-set-fnd-dat
                                              (w-buf-set-fnd-crb)     .
      *                  *---------------------------------------------*
      *                  * Descrizione fase                            *
      *                  *---------------------------------------------*
           move      rf-svs-ide-des       to   w-buf-set-fnd-dfs
                                              (w-buf-set-fnd-crb)     .
       fnd-480.
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura                           *
      *                  *---------------------------------------------*
           go to     fnd-200.
       fnd-500.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-buf-set-fnd-crb    =    zero
                     go to fnd-999.
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-buf-set-fnd-crb    to   w-buf-set-fnd-cpb      .
           subtract  1                    from w-buf-set-fnd-cpb      .
           divide    6                    into w-buf-set-fnd-cpb      .
           add       1                    to   w-buf-set-fnd-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-buf-set-fnd-c01      .
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
           move      04                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      15                   to   v-lto                  .
           move      72                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      "                 SET STATISTICI DISPONIBILI       
      -              "          "         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura di chiusura              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   fnd-950      thru fnd-959        .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fnd-550.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           move      w-buf-set-fnd-c01    to   w-buf-set-fnd-nli      .
       fnd-555.
           if        w-buf-set-fnd-nli    >    6
                     subtract  6          from w-buf-set-fnd-nli
                     go to fnd-555.
      *                          *-------------------------------------*
      *                          * Incremento numero linea a video     *
      *                          * per posizionamento verticale        *
      *                          *-------------------------------------*
           add       06                   to   w-buf-set-fnd-nli      .
       fnd-560.
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
       fnd-575.
      *                      *-----------------------------------------*
      *                      * Accettazione del mark-point             *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-buf-set-fnd-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-buf-set-fnd-c01    <    w-buf-set-fnd-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-buf-set-fnd-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-buf-set-fnd-cpa    <    w-buf-set-fnd-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-buf-set-fnd-nli    to   v-lin                  .
           move      11                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fnd-580.
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to fnd-582
           else if   v-key                =    "UP  "
                     go to fnd-584
           else if   v-key                =    "DOWN"
                     go to fnd-586
           else if   v-key                =    "EXIT"
                     go to fnd-598
           else if   v-key                =    "NXSC"
                     go to fnd-592
           else if   v-key                =    "PRSC"
                     go to fnd-594
           else      go to fnd-575.
       fnd-582.
      *              *-------------------------------------------------*
      *              * Se spaces, Do o select                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Restore video                               *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valori selezionati           *
      *                  *---------------------------------------------*
           move      w-buf-set-fnd-fas
                    (w-buf-set-fnd-c01)   to   w-mod-set-ide-fas      .
      *                  *---------------------------------------------*
      *                  * Preparazione flag di uscita                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-mod-set-exi-sts      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     fnd-999.
       fnd-584.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           subtract  1                    from w-buf-set-fnd-c01      .
           if        w-buf-set-fnd-nli    =    07
                     go to fnd-590
           else      go to fnd-550.
       fnd-586.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           if        w-buf-set-fnd-c01    <    w-buf-set-fnd-crb
                     add   1              to   w-buf-set-fnd-c01
                     go to fnd-588
           else      go to fnd-575.
       fnd-588.
           if        w-buf-set-fnd-nli    =    12
                     go to fnd-590
           else      go to fnd-550.
       fnd-590.
           perform   fnd-950      thru fnd-959        .
           go to     fnd-550.
       fnd-592.
      *              *-------------------------------------------------*
      *              * Se Next screen                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-set-fnd-cpa      .
           go to     fnd-596.
       fnd-594.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
           subtract  1                    from w-buf-set-fnd-cpa      .
       fnd-596.
           move      w-buf-set-fnd-cpa    to   w-buf-set-fnd-c01      .
           multiply  6                    by   w-buf-set-fnd-c01      .
           subtract  5                    from w-buf-set-fnd-c01      .
           go to     fnd-590.
       fnd-598.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di selezione                           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-buf-set-fnd-fds      .
      *                  *---------------------------------------------*
      *                  * Restore video                               *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fnd-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     fnd-999.
       fnd-950.
      *              *=================================================*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-buf-set-fnd-c01    to   w-buf-set-fnd-c02      .
           add       5                    to   w-buf-set-fnd-c02      .
           divide    6                    into w-buf-set-fnd-c02      .
           move      w-buf-set-fnd-c02    to   w-buf-set-fnd-cpa      .
           subtract  1                    from w-buf-set-fnd-c02      .
           multiply  6                    by   w-buf-set-fnd-c02      .
           add       1                    to   w-buf-set-fnd-c02      .
           add       5
                     w-buf-set-fnd-c02  giving w-buf-set-fnd-c03      .
           move      w-buf-set-fnd-c03    to   w-buf-set-fnd-c04      .
           if        w-buf-set-fnd-c03    >    w-buf-set-fnd-crb
                     move  w-buf-set-fnd-crb
                                          to   w-buf-set-fnd-c03      .
           move      07                   to   w-buf-set-fnd-c05      .
       fnd-951.
      *                  *---------------------------------------------*
      *                  * Fase                                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      w-buf-set-fnd-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      w-buf-set-fnd-fas
                    (w-buf-set-fnd-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Data                                        *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-buf-set-fnd-c05    to   v-lin                  .
           move      20                   to   v-pos                  .
           move      w-buf-set-fnd-dat
                    (w-buf-set-fnd-c02)   to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Descrizione fase                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-buf-set-fnd-c05    to   v-lin                  .
           move      31                   to   v-pos                  .
           move      w-buf-set-fnd-dfs
                    (w-buf-set-fnd-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-buf-set-fnd-c02      .
           add       1                    to   w-buf-set-fnd-c05      .
           if        w-buf-set-fnd-c02    not  > w-buf-set-fnd-c03
                     go to fnd-951.
       fnd-952.
           if        w-buf-set-fnd-c02    >    w-buf-set-fnd-c04
                     go to fnd-955.
           if        w-buf-set-fnd-crb    not  > 6
                     go to fnd-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-buf-set-fnd-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-set-fnd-c02      .
           add       1                    to   w-buf-set-fnd-c05      .
           go to     fnd-952.
       fnd-955.
      *                  *---------------------------------------------*
      *                  * Literal 'pagina'                            *
      *                  *---------------------------------------------*
           move      w-buf-set-fnd-cpa    to   w-buf-set-fnd-lt1      .
           move      w-buf-set-fnd-cpb    to   w-buf-set-fnd-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-buf-set-fnd-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       fnd-959.
           exit.
       fnd-999.
           exit.

      *    *===========================================================*
      *    * Open file di appoggio sequenziale [sqz]                   *
      *    *-----------------------------------------------------------*
       opn-fil-sqz-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se il pathname per il file *
      *              * [sqz] e' gia' stato determinato oppure no       *
      *              *-------------------------------------------------*
           if        f-sqz-pat            =    spaces
                     go to opn-fil-sqz-300
           else      go to opn-fil-sqz-600.
       opn-fil-sqz-300.
      *              *-------------------------------------------------*
      *              * Se il pathname per il file [sqz] non e' ancora  *
      *              * stato determinato                               *
      *              *-------------------------------------------------*
       opn-fil-sqz-325.
      *                  *---------------------------------------------*
      *                  * Determinazione pathname per file [sqz]      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prelevamento di un pathname unico per   *
      *                      * files temporanei                        *
      *                      *-----------------------------------------*
           move      "UP"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Salvataggio pathname determinato        *
      *                      *-----------------------------------------*
           move      s-pat                to   f-sqz-pat              .
       opn-fil-sqz-350.
      *                  *---------------------------------------------*
      *                  * Open file [sqz] in output                   *
      *                  *---------------------------------------------*
           open      output sqz                                       .
       opn-fil-sqz-375.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-fil-sqz-999.
       opn-fil-sqz-600.
      *              *-------------------------------------------------*
      *              * Se il pathname per il file [sqz] e' gia' stato  *
      *              * determinato                                     *
      *              *-------------------------------------------------*
       opn-fil-sqz-625.
      *                  *---------------------------------------------*
      *                  * Open file [sqz] in input                    *
      *                  *---------------------------------------------*
           open      input  sqz                                       .
       opn-fil-sqz-650.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     opn-fil-sqz-999.
       opn-fil-sqz-999.
           exit.

      *    *===========================================================*
      *    * Close file di appoggio sequenziale [sqz]                  *
      *    *-----------------------------------------------------------*
       cls-fil-sqz-000.
      *              *-------------------------------------------------*
      *              * Close                                           *
      *              *-------------------------------------------------*
           close     sqz                                              .
       cls-fil-sqz-999.
           exit.

      *    *===========================================================*
      *    * Write file di appoggio sequenziale [sqz]                  *
      *    *-----------------------------------------------------------*
       wrt-fil-sqz-000.
      *              *-------------------------------------------------*
      *              * Write                                           *
      *              *-------------------------------------------------*
           write     sqz-rec                                          .
       wrt-fil-sqz-999.
           exit.

      *    *===========================================================*
      *    * Read file di appoggio sequenziale [sqz]                   *
      *    *-----------------------------------------------------------*
       rea-fil-sqz-000.
      *              *-------------------------------------------------*
      *              * Read                                            *
      *              *-------------------------------------------------*
           read      sqz    at end
                            move  e-end-fil
                                          to   f-sqz-sts
                            go to rea-fil-sqz-999.
           move      e-not-err            to   f-sqz-sts              .
       rea-fil-sqz-999.
           exit.

      *    *===========================================================*
      *    * Delete file di appoggio sequenziale [sqz]                 *
      *    *-----------------------------------------------------------*
       del-fil-sqz-000.
      *              *-------------------------------------------------*
      *              * Richiamo funzione da segreteria                 *
      *              *-------------------------------------------------*
           move      "PD"                 to   s-ope                  .
           move      f-sqz-pat            to   s-pat                  .
           move      "S"                  to   s-sts                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       del-fil-sqz-999.
           exit.

      *    *===========================================================*
      *    * Composizione record [svs]                                 *
      *    *-----------------------------------------------------------*
       cmp-rec-svs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/svf/fls/ioc/obj/iofsvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-svs                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-mod-set-ide-ute    to   rf-svs-ide-ute         .
           move      w-mod-set-tip-arc    to   rf-svs-tip-arc         .
           move      w-mod-set-cod-arc    to   rf-svs-cod-arc         .
           move      w-mod-set-alf-arc    to   rf-svs-alf-arc         .
           move      w-mod-set-ide-fas    to   rf-svs-ide-fas         .
           move      w-mod-set-prg-sqz    to   rf-svs-prg-sqz         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-svs-ide-dat         .
           move      spaces               to   rf-svs-ide-des         .
           move      w-mod-set-dat-ac1    to   rf-svs-dat-ac1         .
           move      w-mod-set-dat-ac2    to   rf-svs-dat-ac2         .
           move      w-mod-set-dat-ac3    to   rf-svs-dat-ac3         .
           move      spaces               to   rf-svs-alx-exp         .
       cmp-rec-svs-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [svs] di testa                           *
      *    *-----------------------------------------------------------*
       wrt-rec-svs-tes-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record                          *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/svf/fls/ioc/obj/iofsvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-svs                 .
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Campi chiave                                *
      *                  *---------------------------------------------*
           move      w-mod-set-ide-ute    to   rf-svs-ide-ute         .
           move      w-mod-set-tip-arc    to   rf-svs-tip-arc         .
           move      zero                 to   rf-svs-cod-arc         .
           move      spaces               to   rf-svs-alf-arc         .
           move      w-mod-set-ide-fas    to   rf-svs-ide-fas         .
           move      zero                 to   rf-svs-prg-sqz         .
      *                  *---------------------------------------------*
      *                  * Campi non chiave                            *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rf-svs-ide-dat         .
           move      w-mod-set-ide-des    to   rf-svs-ide-des         .
      *                  *---------------------------------------------*
      *                  * Allineamento a sx. descrizione fase         *
      *                  *---------------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      rf-svs-ide-des       to   w-all-str-alf          .
           perform   all-str-asx-000      thru all-str-asx-999        .
           move      w-all-str-alf        to   rf-svs-ide-des         .
      *                  *---------------------------------------------*
      *                  * Altri dati                                  *
      *                  *---------------------------------------------*
           move      spaces               to   rf-svs-dat-ac1         .
           move      spaces               to   rf-svs-dat-ac2         .
           move      spaces               to   rf-svs-dat-ac3         .
           move      spaces               to   rf-svs-alx-exp         .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/svf/fls/ioc/obj/iofsvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-svs                 .
       wrt-rec-svs-tes-999.
           exit.

      *    *===========================================================*
      *    * Scrittura record [svs]                                    *
      *    *-----------------------------------------------------------*
       wrt-rec-svs-000.
      *              *-------------------------------------------------*
      *              * Composizione record                             *
      *              *-------------------------------------------------*
           perform   cmp-rec-svs-000      thru cmp-rec-svs-999        .
      *              *-------------------------------------------------*
      *              * Put record                                      *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "pgm/svf/fls/ioc/obj/iofsvs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-svs                 .
       wrt-rec-svs-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .


