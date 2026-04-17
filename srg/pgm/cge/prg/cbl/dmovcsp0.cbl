       Identification Division.
       Program-Id.                                 dmovcsp0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    cge                 *
      *                                Settore:    csp                 *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 27/10/07    *
      *                       Ultima revisione:    NdK del 16/05/12    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Modulo per la determinazione, relativamente ad un cespite di : *
      *                                                                *
      *  - Valore iniziale del bene                                    *
      *  - Ammortamento                                                *
      *  - Residuo da ammortizzare                                     *
      *  - Data di acquisto                                            *
      *  - Data di cessione                                            *
      *  - Plus / minus valenza                                        *
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
      *        Input  : d-mov-csp-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-mov-csp-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-mov-csp-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-mov-csp-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "SC" - Situazione cespite                                      *
      *                                                                *
      *                                                                *
      *        Input  : d-mov-csp-tip-ope = "SC"                       *
      *                                                                *
      *                 d-mov-csp-cod-csp = Codice cespite             *
      *                                                                *
      *                 d-mov-csp-dat-sit = Data situazione            *
      *                                                                *
      *                                                                *
      *        Output : d-mov-csp-dat-acq = Data di acquisto           *
      *                                                                *
      *                 d-mov-csp-dat-ven = Data di cessione           *
      *                                                                *
      *                 d-mov-csp-val-ini = Valore iniziale del bene   *
      *                                                                *
      *                 d-mov-csp-val-ind = Valore indeducibile        *
      *                                                                *
      *                 d-mov-csp-val-amm = Ammortamento               *
      *                                                                *
      *                 d-mov-csp-val-ven = Vendita                    *
      *                                                                *
      *                 d-mov-csp-val-res = Residuo                    *
      *                                                                *
      *                 d-mov-csp-sts-csp = Status del cespite         *
      *                                     - 00 : non determinato     *
      *                                     - 01 : da ammortizzare     *
      *                                     - 02 : ammortizzato        *
      *                                     - 03 : venduto             *
      *                                                                *
      *                 d-mov-csp-exi-sts = Risultato elaborazione     *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "AM" - Determinazione valore ammortizzato in un periodo        *
      *                                                                *
      *                                                                *
      *        Input  : d-mov-csp-tip-ope = "AM"                       *
      *                                                                *
      *                 d-mov-csp-cod-csp = Codice cespite             *
      *                                                                *
      *                 d-mov-csp-dat-min = Data minima                *
      *                                                                *
      *                 d-mov-csp-dat-max = Data massima               *
      *                                                                *
      *                                                                *
      *        Output : d-mov-csp-val-amm = Ammortamento nel periodo   *
      *                                                                *
      *                 d-mov-csp-exi-sts = Risultato elaborazione     *
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
      *        * [cer]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcer"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
           05  filler                     pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) trailing
                                                     separate
                                                     character
                                                     value zero       .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Contatore di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-det-mov-csp-ctr          pic  9(09)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione movimenti di un  *
      *    * cespite                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/dmovcsp0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-mov-csp              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-mov-csp-exi-sts      .
       main-100.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           if        d-mov-csp-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-mov-csp-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-mov-csp-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-mov-csp-tip-ope    =    "C?"
                     perform tst-cnc-mod-000
                                          thru tst-cnc-mod-999
      *                  *---------------------------------------------*
      *                  * Situazione cespite                          *
      *                  *---------------------------------------------*
           else if   d-mov-csp-tip-ope    =    "SC"
                     perform det-sit-csp-000
                                          thru det-sit-csp-999
      *                  *---------------------------------------------*
      *                  * Determinazione ammortamento                 *
      *                  *---------------------------------------------*
           else if   d-mov-csp-tip-ope    =    "AM"
                     perform det-amm-csp-000
                                          thru det-amm-csp-999        .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore Open modulo                *
      *              *-------------------------------------------------*
           add       1                    to   w-nol-ctr-opn          .
       rou-opn-fls-100.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
       rou-opn-fls-200.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [cer]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcer"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cer                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open modulo                *
      *              *-------------------------------------------------*
           subtract  1                    from w-nol-ctr-opn          .
       rou-cls-fls-200.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [cer]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcer"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cer                 .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Test cancellabilita' modulo                               *
      *    *-----------------------------------------------------------*
       tst-cnc-mod-000.
      *              *-------------------------------------------------*
      *              * Se il contatore di Open e' a zero il modulo e'  *
      *              * cancellabile, altrimenti non lo e'              *
      *              *-------------------------------------------------*
           if        w-nol-ctr-opn        =    zero
                     move  spaces         to   d-mov-csp-exi-sts
           else      move  "#"            to   d-mov-csp-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione situazione cespite                         *
      *    *-----------------------------------------------------------*
       det-sit-csp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di esito                   *
      *              *-------------------------------------------------*
           move      spaces               to   d-mov-csp-exi-sts      .
      *              *-------------------------------------------------*
      *              * Normalizzazione totalizzatori                   *
      *              *-------------------------------------------------*
           move      zero                 to   d-mov-csp-dat-acq      .
           move      zero                 to   d-mov-csp-dat-ven      .
           move      zero                 to   d-mov-csp-val-ini      .
           move      zero                 to   d-mov-csp-val-ind      .
           move      zero                 to   d-mov-csp-val-amm      .
           move      zero                 to   d-mov-csp-val-ven      .
           move      zero                 to   d-mov-csp-val-res      .
           move      zero                 to   d-mov-csp-sts-csp      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore di comodo             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-mov-csp-ctr      .
       det-sit-csp-100.
      *              *-------------------------------------------------*
      *              * Start su file [cer]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCSP    "         to   f-key                  .
           move      d-mov-csp-cod-csp    to   rf-cer-cod-csp         .
           move      zero                 to   rf-cer-num-prg         .
           move      "pgm/cge/fls/ioc/obj/iofcer"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cer                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-sit-csp-800.
       det-sit-csp-200.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Next su [cer]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcer"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cer                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : uscita                        *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-sit-csp-800.
       det-sit-csp-300.
      *              *-------------------------------------------------*
      *              * Max su [cer]                                    *
      *              *-------------------------------------------------*
           if        rf-cer-cod-csp       not  = d-mov-csp-cod-csp
                     go to det-sit-csp-800.
       det-sit-csp-400.
      *              *-------------------------------------------------*
      *              * Sel su [cer]                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su data situazione                     *
      *                  *---------------------------------------------*
           if        rf-cer-dat-mov       >    d-mov-csp-dat-sit
                     go to det-sit-csp-200.
       det-sit-csp-500.
      *              *-------------------------------------------------*
      *              * Totalizzatori                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore di comodo              *
      *                  *---------------------------------------------*
           add       1                    to   w-det-mov-csp-ctr      .
      *                  *---------------------------------------------*
      *                  * Data acquisto                               *
      *                  *---------------------------------------------*
           if        rf-cer-flg-snx (01)  =    "S" and
                     d-mov-csp-dat-acq    =    zero
                     move rf-cer-dat-mov  to   d-mov-csp-dat-acq      .
      *                  *---------------------------------------------*
      *                  * Data cessione                               *
      *                  *---------------------------------------------*
           if        rf-cer-flg-snx (06)  =    "S" and
                     d-mov-csp-dat-ven    =    zero
                     move rf-cer-dat-mov  to   d-mov-csp-dat-ven      .
      *                  *---------------------------------------------*
      *                  * Valore iniziale                             *
      *                  *---------------------------------------------*
           if        rf-cer-flg-snx (02)  =    "S" and
                     rf-cer-sgn-mov       =    "+"
                     add      rf-cer-imp-mov
                                          to   d-mov-csp-val-ini
           else if   rf-cer-flg-snx (02)  =    "S" and
                     rf-cer-sgn-mov       =    "-"
                     subtract rf-cer-imp-mov
                                          from d-mov-csp-val-ini      .
      *                  *---------------------------------------------*
      *                  * Valore indeducibile                         *
      *                  *---------------------------------------------*
           if        rf-cer-flg-snx (02)  =    "S" and
                     rf-cer-sgn-mov       =    "-"
                     add      rf-cer-imp-mov
                                          to   d-mov-csp-val-ind      .
      *                  *---------------------------------------------*
      *                  * Ammortamento                                *
      *                  *---------------------------------------------*
           if        rf-cer-flg-snx (03)  =    "S" and
                     rf-cer-sgn-mov       =    "-"
                     add      rf-cer-imp-mov
                                          to   d-mov-csp-val-amm      .
      *                  *---------------------------------------------*
      *                  * Vendita                                     *
      *                  *---------------------------------------------*
           if        rf-cer-flg-snx (06)  =    "S" and
                     rf-cer-sgn-mov       =    "-"
                     add      rf-cer-imp-mov
                                          to   d-mov-csp-val-ven      .
      *                  *---------------------------------------------*
      *                  * Residuo                                     *
      *                  *---------------------------------------------*
           if        rf-cer-sgn-mov       =    "-"
                     subtract  rf-cer-imp-mov
                                          from d-mov-csp-val-res
           else      add       rf-cer-imp-mov
                                          to   d-mov-csp-val-res      .
       det-sit-csp-600.
      *              *-------------------------------------------------*
      *              * Riciclo a record [cer] successivo               *
      *              *-------------------------------------------------*
           go to     det-sit-csp-200.
       det-sit-csp-800.
      *              *-------------------------------------------------*
      *              * Test su contatore elementi letti                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se nessune elemento: errore di uscita       *
      *                  *---------------------------------------------*
           if        w-det-mov-csp-ctr    =    zero
                     move  "#"            to   d-mov-csp-exi-sts      .
       det-sit-csp-850.
      *              *-------------------------------------------------*
      *              * Determinazione status cespite                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se cespite venduto                     *
      *                  *---------------------------------------------*
           if        d-mov-csp-dat-ven    >    zero
                     move  03             to   d-mov-csp-sts-csp
                     go to det-sit-csp-900.
      *                  *---------------------------------------------*
      *                  * Test se cespite ammortizzato                *
      *                  *---------------------------------------------*
           if        d-mov-csp-val-res    =    zero
                     move  02             to   d-mov-csp-sts-csp
                     go to det-sit-csp-900.
      *                  *---------------------------------------------*
      *                  * In tutti gli altri casi                     *
      *                  *---------------------------------------------*
           move      01                   to   d-mov-csp-sts-csp      .
       det-sit-csp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-sit-csp-999.
       det-sit-csp-999.
           exit.

      *    *===========================================================*
      *    * Determinazione ammortamento cespite                       *
      *    *-----------------------------------------------------------*
       det-amm-csp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di esito                   *
      *              *-------------------------------------------------*
           move      spaces               to   d-mov-csp-exi-sts      .
      *              *-------------------------------------------------*
      *              * Normalizzazione totalizzatori                   *
      *              *-------------------------------------------------*
           move      zero                 to   d-mov-csp-val-amm      .
           move      zero                 to   d-mov-csp-val-res      .
       det-amm-csp-100.
      *              *-------------------------------------------------*
      *              * Start su file [cer]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCSP    "         to   f-key                  .
           move      d-mov-csp-cod-csp    to   rf-cer-cod-csp         .
           move      zero                 to   rf-cer-num-prg         .
           move      "pgm/cge/fls/ioc/obj/iofcer"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cer                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-amm-csp-800.
       det-amm-csp-200.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Next su [cer]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcer"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cer                 .
      *                  *---------------------------------------------*
      *                  * Se 'at end' : uscita                        *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-amm-csp-800.
       det-amm-csp-300.
      *              *-------------------------------------------------*
      *              * Max su [cer]                                    *
      *              *-------------------------------------------------*
           if        rf-cer-cod-csp       not  = d-mov-csp-cod-csp
                     go to det-amm-csp-800.
       det-amm-csp-400.
      *              *-------------------------------------------------*
      *              * Sel su [cer]                                    *
      *              *-------------------------------------------------*
       det-amm-csp-500.
      *              *-------------------------------------------------*
      *              * Test se movimenti che rientrano nel range       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su data minima e massima               *
      *                  *---------------------------------------------*
           if        rf-cer-dat-mov       <    d-mov-csp-dat-min or
                     rf-cer-dat-mov       >    d-mov-csp-dat-max
                     go to det-amm-csp-600.
       det-amm-csp-520.
      *              *-------------------------------------------------*
      *              * Aggiornamento totale ammortamento               *
      *              *-------------------------------------------------*
           if        rf-cer-flg-snx (03)  =    "S" and
                     rf-cer-sgn-mov       =    "-"
                     move  "#"            to   d-mov-csp-exi-sts
                     subtract  rf-cer-imp-mov
                                          from d-mov-csp-val-amm      .
       det-amm-csp-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento totale residuo                    *
      *              *-------------------------------------------------*
           if        rf-cer-sgn-mov       =    "-"
                     subtract  rf-cer-imp-mov
                                          from d-mov-csp-val-res
           else      add       rf-cer-imp-mov
                                          to   d-mov-csp-val-res      .
       det-amm-csp-700.
      *              *-------------------------------------------------*
      *              * Riciclo a record [cer] successivo               *
      *              *-------------------------------------------------*
           go to     det-amm-csp-200.
       det-amm-csp-800.
      *              *-------------------------------------------------*
      *              * Determinazione status cespite                   *
      *              *-------------------------------------------------*
       det-amm-csp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-amm-csp-999.
       det-amm-csp-999.
           exit.

