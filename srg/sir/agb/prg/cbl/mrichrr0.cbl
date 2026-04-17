       Identification Division.
       Program-Id.                                 mrichrr0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    sir                 *
      *                        Area gestionale:    agb                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 02/08/95    *
      *                       Ultima revisione:    NdK del 20/02/06    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Descrizione pgm:   Modulo per la memorizzazione delle richie-  *
      *                    ste di selezione                            *
      *                                                                *
      *                    SU MISURA PER SIRI                          *
      *                                                                *
      * ============================================================== *
      *                                                                *
      * Tipi operazione :                                              *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "OP"  Open modulo                                              *
      *                                                                *
      *              Input  : w-mem-ric-sel-ope : "OP"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "CL"  Close modulo                                             *
      *                                                                *
      *              Input  : w-mem-ric-sel-ope : "CL"                 *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "C?"  Test di cancellabilita' del modulo                       *
      *                                                                *
      *              Input  : w-mem-ric-sel-ope : "C?"                 *
      *                                                                *
      *                                                                *
      *              Output : w-mem-ric-sel-ope : spaces = Si          *
      *                                           "C?"   = No          *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "RD"  Lettura richieste di selezione memorizzate               *
      *                                                                *
      *              Input  : w-mem-ric-sel-ope : "RD"                 *
      *                                                                *
      *                       w-mem-ric-sel-ute : codice utente        *
      *                                                                *
      *                       w-mem-ric-sel-fas : codice fase          *
      *                                                                *
      *                                                                *
      *              Output : w-mem-ric-sel-flg : flag esito lettura   *
      *                                                                *
      *                                            - spaces : lettura  *
      *                                                       effet-   *
      *                                                       tuata    *
      *                                                                *
      *                                            - '#'    : lettura  *
      *                                                       non ef-  *
      *                                                       fettuata *
      *                                                                *
      *                       w-mem-ric-sel-rrr : richieste di sele-   *
      *                                           zione eventualmente  *
      *                                           lette                *
      *                                                                *
      *       -------------------------------------------------------- *
      *                                                                *
      * "WR"  Scrittura richieste di selezione                         *
      *                                                                *
      *              Input  : w-mem-ric-sel-ope : "WR"                 *
      *                                                                *
      *                       w-mem-ric-sel-ute : codice utente        *
      *                                                                *
      *                       w-mem-ric-sel-fas : codice fase          *
      *                                                                *
      *                       w-mem-ric-sel-rrr : richieste di sele-   *
      *                                           zione preparate      *
      *                                                                *
      *                                                                *
      *              Output : nessuno                                  *
      *                                                                *
      *       -------------------------------------------------------- *
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

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [hrr]                                                 *
      *        *-------------------------------------------------------*
           copy      "sir/agb/fls/rec/rfhrr"                          .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Contatore del numero di Open in corso per il modulo   *
      *        *-------------------------------------------------------*
           05  w-cnt-ctr-opn              pic  9(03)       value zero .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Link-area per memorizzazione richieste di selezione       *
      *    *-----------------------------------------------------------*
           copy      "sir/agb/prg/cpy/mrichrr0.mrl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      ******************************************************************
       Procedure Division                using w-mem-ric-sel
                                               s                      .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-mem-ric-sel-ope    =    "OP"
                     perform   opn-000    thru opn-999
           else if   w-mem-ric-sel-ope    =    "CL"
                     perform   cls-000    thru cls-999
           else if   w-mem-ric-sel-ope    =    "C?"
                     perform   tcm-000    thru tcm-999
           else if   w-mem-ric-sel-ope    =    "RD"
                     perform   rea-000    thru rea-999
           else if   w-mem-ric-sel-ope    =    "WR"
                     perform   wrt-000    thru wrt-999                .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       opn-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore Open in corso per il modu- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Se questa non e' la prima Open per il modulo si *
      *              * esce                                            *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        >    1
                     go to opn-999.
       opn-100.
      *              *-------------------------------------------------*
      *              * Se questa e' la prima Open per il modulo        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open file [hrr]                             *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhrr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hrr                 .
       opn-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       cls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open in corso per il modu- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           subtract  1                    from w-cnt-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Se questa non e' l'ultima Close per il modulo   *
      *              * si esce                                         *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        >    zero
                     go to cls-999.
       cls-100.
      *              *-------------------------------------------------*
      *              * Se questa e' l'ultima Close per il modulo       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Close file [hrr]                            *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhrr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hrr                 .
       cls-999.
           exit.

      *    *===========================================================*
      *    * Test di cancellabilita' per il modulo                     *
      *    *-----------------------------------------------------------*
       tcm-000.
      *              *-------------------------------------------------*
      *              * Se il contatore delle Open in corso per il mo-  *
      *              * dulo e' pari a zero si dichiara che e' cancel-  *
      *              * labile, altrimento che non lo e'                *
      *              *-------------------------------------------------*
           if        w-cnt-ctr-opn        =    zero
                     move  spaces         to   w-mem-ric-sel-ope      .
       tcm-999.
           exit.

      *    *===========================================================*
      *    * Lettura richieste di selezione memorizzate                *
      *    *-----------------------------------------------------------*
       rea-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-mem-ric-sel-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione campi di output                 *
      *              *-------------------------------------------------*
           move      spaces               to   w-mem-ric-sel-rrr      .
           move      spaces               to   w-mem-ric-sel-alx      .
       rea-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [hrr]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhrr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hrr                 .
       rea-200.
      *              *-------------------------------------------------*
      *              * Lettura record [hrr]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "UTEFAS"             to   f-key                  .
           move      w-mem-ric-sel-ute    to   rf-hrr-ide-ute         .
           move      w-mem-ric-sel-fas    to   rf-hrr-ide-fas         .
           move      "sir/agb/fls/ioc/obj/iofhrr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hrr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito della lettura                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rea-900.
       rea-400.
      *              *-------------------------------------------------*
      *              * Bufferizzazione richieste lette                 *
      *              *-------------------------------------------------*
           move      rf-hrr-ric-sel       to   w-mem-ric-sel-rrr      .
       rea-800.
      *              *-------------------------------------------------*
      *              * Uscita per lettura con esito positivo           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rea-999.
       rea-900.
      *              *-------------------------------------------------*
      *              * Uscita per lettura con esito negativo           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-mem-ric-sel-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rea-999.
       rea-999.
           exit.

      *    *===========================================================*
      *    * Scrittura richieste di selezione                          *
      *    *-----------------------------------------------------------*
       wrt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-mem-ric-sel-flg      .
       wrt-050.
      *              *-------------------------------------------------*
      *              * Delete precedente record [hrr]                  *
      *              *-------------------------------------------------*
           perform   del-000              thru del-999                .
       wrt-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [hrr]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhrr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hrr                 .
       wrt-200.
      *              *-------------------------------------------------*
      *              * Composizione record [hrr]                       *
      *              *-------------------------------------------------*
           move      w-mem-ric-sel-ute    to   rf-hrr-ide-ute         .
           move      w-mem-ric-sel-fas    to   rf-hrr-ide-fas         .
           move      w-mem-ric-sel-dat    to   rf-hrr-ide-dat         .
           move      w-mem-ric-sel-rrr    to   rf-hrr-ric-sel         .
           move      w-mem-ric-sel-alx    to   rf-hrr-alx-exp         .
       wrt-400.
      *              *-------------------------------------------------*
      *              * Scrittura record [hrr]                          *
      *              *-------------------------------------------------*
           move      "PT"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhrr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hrr                 .
       wrt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     wrt-999.
       wrt-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione richieste di selezione                      *
      *    *-----------------------------------------------------------*
       del-000.
      *              *-------------------------------------------------*
      *              * Composizione chiave record [hrr]                *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      w-mem-ric-sel-ute    to   rf-hrr-ide-ute         .
           move      w-mem-ric-sel-fas    to   rf-hrr-ide-fas         .
      *              *-------------------------------------------------*
      *              * Delete record [hrr]                             *
      *              *-------------------------------------------------*
           move      "DE"                 to   f-ope                  .
           move      "sir/agb/fls/ioc/obj/iofhrr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-hrr                 .
       del-999.
           exit.

