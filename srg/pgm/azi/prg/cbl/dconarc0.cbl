       Identification Division.
       Program-Id.                                 dconarc0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    azi                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 25/11/02    *
      *                       Ultima revisione:    NdK del 30/12/21    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Modulo per la determinazione, relativamente ad un archivio dei *
      * suoi contatti                                                  *
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
      *        Input  : d-con-arc-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-con-arc-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-con-arc-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-con-arc-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "DT" - Determinazione contatti                                 *
      *                                                                *
      *                                                                *
      *        Input  : d-con-arc-tip-ope = "DT"                       *
      *                                                                *
      *                 d-con-arc-tip-arc = Tipo archivio              *
      *                                                                *
      *                 d-con-arc-cod-arc = Codice archivio            *
      *                                                                *
      *                 d-con-arc-dpz-arc = Codice dipendenza archivio *
      *                                                                *
      *                 d-con-arc-tip-sel = Tipo contatto (opzionale)  *
      *                                                                *
      *                 d-con-arc-num-edr = Elementi da estrarre (opz) *
      *                                                                *
      *                                                                *
      *        Output : d-con-arc-num-ele = Numero contatti            *
      *                                                                *
      *                 d-con-arc-con-arc = Castelletto contatti       *
      *                                                                *
      *                 d-con-arc-uni-prg = Progressivo disponibile    *
      *                                    (solo se un elemento)       *
      *                                                                *
      *                 d-con-arc-uni-agg = Data aggiornamento         *
      *                                    (solo se un elemento)       *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "IE" - Determinazione indirizzo elettronico per invio          *
      *                                                                *
      *                                                                *
      *        Input  : d-con-arc-tip-ope = "IE"                       *
      *                                                                *
      *                 d-con-arc-tip-arc = Tipo archivio              *
      *                                                                *
      *                 d-con-arc-cod-arc = Codice archivio            *
      *                                                                *
      *                 d-con-arc-dpz-arc = Codice dipendenza archivio *
      *                                                                *
      *                 d-con-arc-tip-sel = Sigla convenzionale tipo   *
      *                                     di documento:              *
      *                                                                *
      *                                     - 'ord' : conferme ordine  *
      *                                     - 'bol' : documenti DDT    *
      *                                     - 'fat' : fatture          *
      *                                     - 'xml' : e-fatture        *
      *                                                                *
      *                 d-con-arc-num-edr = Elementi da estrarre (opz) *
      *                                                                *
      *                                                                *
      *        Output : d-con-arc-num-ele = Numero contatti            *
      *                                                                *
      *                 d-con-arc-con-arc = Castelletto contatti       *
      *                                    (al massimo un elemento)    *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "DP" - Determinazione primo progressivo disponibile archivio   *
      *                                                                *
      *                                                                *
      *        Input  : d-con-arc-tip-ope = "DP"                       *
      *                                                                *
      *                 d-con-arc-tip-arc = Tipo archivio              *
      *                                                                *
      *                 d-con-arc-cod-arc = Codice archivio            *
      *                                                                *
      *                 d-con-arc-dpz-arc = Codice dipendenza archivio *
      *                                                                *
      *                                                                *
      *        Output : d-con-arc-num-prg = Progressivo disponibile    *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "RC" - Ricerca contatto di un archivio                         *
      *                                                                *
      *                                                                *
      *        Input  : d-con-arc-tip-ope = "RC"                       *
      *                                                                *
      *                 d-con-arc-tip-arc = Tipo archivio              *
      *                                                                *
      *                 d-con-arc-cod-arc = Codice archivio            *
      *                                                                *
      *                 d-con-arc-dpz-arc = Codice dipendenza archivio *
      *                                                                *
      *                 d-con-arc-tip-sel = Tipo contatto              *
      *                                                                *
      *                 d-con-arc-num-con (1) = Contatto da ricercare  *
      *                                                                *
      *                                                                *
      *        Output : d-con-arc-num-ele = Numero contatti trovati    *
      *                                                                *
      *                 d-con-arc-num-prg = Progressivo elemento       *
      *                                                                *
      *                 d-con-arc-int-con (1) = Interlocutore          *
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
      *        * [adc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfadc"                          .

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
      *        * Work per Det contatti                                 *
      *        *-------------------------------------------------------*
           05  w-det-con-arc.
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-con-arc-max      pic  9(02) value 50         .
               10  w-det-con-arc-ctr      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Selettori di comodo                               *
      *            *---------------------------------------------------*
               10  w-det-con-arc-tip      pic  x(03)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione contatti         *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/dconarc0.dtl"                   .

      ******************************************************************
       Procedure Division                using d-con-arc              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-con-arc-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione contatti                     *
      *                  *---------------------------------------------*
           if        d-con-arc-tip-ope    =    "DT"
                     perform det-con-arc-000
                                          thru det-con-arc-999
      *                  *---------------------------------------------*
      *                  * Determinazione indirizzo elettronico        *
      *                  *---------------------------------------------*
           else if   d-con-arc-tip-ope    =    "IE"
                     perform det-ind-ele-000
                                          thru det-ind-ele-999
      *                  *---------------------------------------------*
      *                  * Determinazione progressivo disponibile      *
      *                  *---------------------------------------------*
           else if   d-con-arc-tip-ope    =    "DP"
                     perform det-prg-dsp-000
                                          thru det-prg-dsp-999
      *                  *---------------------------------------------*
      *                  * Ricerca contatto di un archivio             *
      *                  *---------------------------------------------*
           else if   d-con-arc-tip-ope    =    "RC"
                     perform det-ric-con-000
                                          thru det-ric-con-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-con-arc-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-con-arc-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-con-arc-tip-ope    =    "C?"
                     perform tst-cnc-mod-000
                                          thru tst-cnc-mod-999        .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione selettori facoltativi           *
      *              *-------------------------------------------------*
           move      spaces               to   d-con-arc-tip-sel      .
           move      zero                 to   d-con-arc-num-prg      .
           move      zero                 to   d-con-arc-num-edr      .
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
      *                  * [adc]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
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
      *                  * [adc]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
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
                     move  spaces         to   d-con-arc-exi-sts
           else      move  "#"            to   d-con-arc-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione contatti archivio                          *
      *    *-----------------------------------------------------------*
       det-con-arc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-con-arc-uni-prg      .
           move      zero                 to   d-con-arc-uni-agg      .
           move      zero                 to   d-con-arc-num-ele      .
           move      zero                 to   w-det-con-arc-ctr      .
       det-con-arc-050.
           add       1                    to   w-det-con-arc-ctr      .
           if        w-det-con-arc-ctr    >    w-det-con-arc-max
                     go to det-con-arc-100.
           move      spaces               to   d-con-arc-tip-con
                                              (w-det-con-arc-ctr)     .
           move      spaces               to   d-con-arc-num-con
                                              (w-det-con-arc-ctr)     .
           move      spaces               to   d-con-arc-int-con
                                              (w-det-con-arc-ctr)     .
           go to     det-con-arc-050.
       det-con-arc-100.
      *              *-------------------------------------------------*
      *              * Start su record [adc]                           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ARCCON    "         to   f-key                  .
           move      d-con-arc-tip-arc    to   rf-adc-tip-arc         .
           move      d-con-arc-cod-arc    to   rf-adc-cod-arc         .
           move      d-con-arc-dpz-arc    to   rf-adc-dpz-arc         .
           move      zero                 to   rf-adc-num-prg         .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-con-arc-900.
       det-con-arc-200.
      *              *-------------------------------------------------*
      *              * Read Next da [adc]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-con-arc-900.
       det-con-arc-300.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sulla chiave                           *
      *                  *---------------------------------------------*
           if        rf-adc-tip-arc       not  = d-con-arc-tip-arc or
                     rf-adc-cod-arc       not  = d-con-arc-cod-arc or
                     rf-adc-dpz-arc       not  = d-con-arc-dpz-arc
                     go to det-con-arc-900.
       det-con-arc-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo contatto, facoltativo          *
      *                  *---------------------------------------------*
           if        d-con-arc-tip-sel    =    spaces
                     go to det-con-arc-420.
           if        rf-adc-tip-con       not  = d-con-arc-tip-sel
                     go to det-con-arc-200.
       det-con-arc-420.
      *                  *---------------------------------------------*
      *                  * Test su elementi da restituire, facoltativo *
      *                  *---------------------------------------------*
           if        d-con-arc-num-edr    =    zero
                     go to det-con-arc-440.
           if        d-con-arc-num-ele    =    d-con-arc-num-edr
                     go to det-con-arc-900.
       det-con-arc-440.
      *                  *---------------------------------------------*
      *                  * Test su progressivo, facoltativo            *
      *                  *---------------------------------------------*
           if        d-con-arc-num-prg    =    zero
                     go to det-con-arc-500.
           if        rf-adc-num-prg       not  = d-con-arc-num-prg
                     go to det-con-arc-200.
       det-con-arc-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   d-con-arc-num-ele      .
           if        d-con-arc-num-ele    >    w-det-con-arc-max
                     go to det-con-arc-900.
       det-con-arc-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione elemento                        *
      *              *-------------------------------------------------*
           move      rf-adc-tip-con       to   d-con-arc-tip-con
                                              (d-con-arc-num-ele)     .
           move      rf-adc-num-con       to   d-con-arc-num-con
                                              (d-con-arc-num-ele)     .
           move      rf-adc-int-con       to   d-con-arc-int-con
                                              (d-con-arc-num-ele)     .
           move      rf-adc-num-prg       to   d-con-arc-uni-prg      .
           move      rf-adc-dat-agg       to   d-con-arc-uni-agg      .
       det-con-arc-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-con-arc-200.
       det-con-arc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-con-arc-999.
       det-con-arc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione indirizzo elettronico                      *
      *    *-----------------------------------------------------------*
       det-ind-ele-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           if        d-con-arc-num-edr    =    zero
                     move  1              to   d-con-arc-num-edr      .
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-con-arc-num-ele      .
           move      zero                 to   w-det-con-arc-ctr      .
       det-ind-ele-050.
           add       1                    to   w-det-con-arc-ctr      .
           if        w-det-con-arc-ctr    >    w-det-con-arc-max
                     go to det-ind-ele-100.
           move      spaces               to   d-con-arc-tip-con
                                              (w-det-con-arc-ctr)     .
           move      spaces               to   d-con-arc-num-con
                                              (w-det-con-arc-ctr)     .
           move      spaces               to   d-con-arc-int-con
                                              (w-det-con-arc-ctr)     .
           go to     det-ind-ele-050.
       det-ind-ele-100.
      *              *-------------------------------------------------*
      *              * Start su record [adc]                           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ARCCON    "         to   f-key                  .
           move      d-con-arc-tip-arc    to   rf-adc-tip-arc         .
           move      d-con-arc-cod-arc    to   rf-adc-cod-arc         .
           move      d-con-arc-dpz-arc    to   rf-adc-dpz-arc         .
           move      zero                 to   rf-adc-num-prg         .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ind-ele-900.
       det-ind-ele-200.
      *              *-------------------------------------------------*
      *              * Read Next da [adc]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ind-ele-900.
       det-ind-ele-300.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sulla chiave                           *
      *                  *---------------------------------------------*
           if        rf-adc-tip-arc       not  = d-con-arc-tip-arc or
                     rf-adc-cod-arc       not  = d-con-arc-cod-arc or
                     rf-adc-dpz-arc       not  = d-con-arc-dpz-arc
                     go to det-ind-ele-900.
       det-ind-ele-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se tipo contatto e-fatture             *
      *                  *---------------------------------------------*
           move      d-con-arc-tip-sel    to   w-det-con-arc-tip      .
      *
           if        d-con-arc-tip-sel    =    "xml"
                     move  "fat"          to   w-det-con-arc-tip      .
      *
           if        d-con-arc-tip-sel    =    "xml" and
                     rf-adc-tip-con       not  = "PEC"
                     go to det-ind-ele-200.
      *                  *---------------------------------------------*
      *                  * Test su tipo contatto: solo e-mail          *
      *                  *---------------------------------------------*
           if        rf-adc-tip-con       not  = "EML" and
                     rf-adc-tip-con       not  = "PEC"
                     go to det-ind-ele-200.
       det-ind-ele-415.
      *                  *---------------------------------------------*
      *                  * Test su tipo documento                      *
      *                  *---------------------------------------------*
           if        rf-adc-int-con
                    (01 : 01)             not  = "<"
                     go to det-ind-ele-200.
      *
           if        rf-adc-int-con
                    (02 : 03)             not  = w-det-con-arc-tip
                     go to det-ind-ele-200.
       det-ind-ele-420.
      *                  *---------------------------------------------*
      *                  * Test su elementi da restituire              *
      *                  *---------------------------------------------*
           if        d-con-arc-num-ele    >    d-con-arc-num-edr
                     go to det-ind-ele-900.
       det-ind-ele-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   d-con-arc-num-ele      .
           if        d-con-arc-num-ele    >    w-det-con-arc-max
                     go to det-ind-ele-900.
       det-ind-ele-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione elemento                        *
      *              *-------------------------------------------------*
           move      rf-adc-tip-con       to   d-con-arc-tip-con
                                              (d-con-arc-num-ele)     .
           move      rf-adc-num-con       to   d-con-arc-num-con
                                              (d-con-arc-num-ele)     .
           move      rf-adc-int-con       to   d-con-arc-int-con
                                              (d-con-arc-num-ele)     .
       det-ind-ele-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-ind-ele-200.
       det-ind-ele-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ind-ele-999.
       det-ind-ele-999.
           exit.

      *    *===========================================================*
      *    * Determinazione primo progressivo disponibile              *
      *    *-----------------------------------------------------------*
       det-prg-dsp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore di output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-con-arc-num-prg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore di comodo             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-con-arc-ctr      .
       det-prg-dsp-100.
      *              *-------------------------------------------------*
      *              * Start su record [adc]                           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "ARCCON    "         to   f-key                  .
           move      d-con-arc-tip-arc    to   rf-adc-tip-arc         .
           move      d-con-arc-cod-arc    to   rf-adc-cod-arc         .
           move      d-con-arc-dpz-arc    to   rf-adc-dpz-arc         .
           move      zero                 to   rf-adc-num-prg         .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-prg-dsp-900.
       det-prg-dsp-200.
      *              *-------------------------------------------------*
      *              * Read Next da [adc]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-prg-dsp-900.
       det-prg-dsp-300.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sulla chiave                           *
      *                  *---------------------------------------------*
           if        rf-adc-tip-arc       not  = d-con-arc-tip-arc or
                     rf-adc-cod-arc       not  = d-con-arc-cod-arc or
                     rf-adc-dpz-arc       not  = d-con-arc-dpz-arc
                     go to det-prg-dsp-900.
       det-prg-dsp-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       det-prg-dsp-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-con-arc-ctr      .
       det-prg-dsp-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     det-prg-dsp-200.
       det-prg-dsp-900.
      *              *-------------------------------------------------*
      *              * Bufferizzazione progressivo determinato         *
      *              *-------------------------------------------------*
           move      w-det-con-arc-ctr    to   d-con-arc-num-prg      .
           add       1                    to   d-con-arc-num-prg      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-prg-dsp-999.
       det-prg-dsp-999.
           exit.

      *    *===========================================================*
      *    * Ricerca contatto di un archivio                           *
      *    *-----------------------------------------------------------*
       det-ric-con-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-con-arc-num-ele      .
           move      zero                 to   d-con-arc-num-prg      .
           move      spaces               to   d-con-arc-int-con (1)  .
       det-ric-con-100.
      *              *-------------------------------------------------*
      *              * Start su record [adc]                           *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMARC    "         to   f-key                  .
           move      d-con-arc-tip-sel    to   rf-adc-tip-con         .
           move      spaces               to   rf-adc-pri-con         .
           move      spaces               to   rf-adc-pre-con         .
           move      d-con-arc-num-con (1)
                                          to   rf-adc-num-con         .
           move      d-con-arc-tip-arc    to   rf-adc-tip-arc         .
           move      d-con-arc-cod-arc    to   rf-adc-cod-arc         .
           move      d-con-arc-dpz-arc    to   rf-adc-dpz-arc         .
           move      zero                 to   rf-adc-num-prg         .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ric-con-900.
       det-ric-con-200.
      *              *-------------------------------------------------*
      *              * Read Next da [adc]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofadc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-adc                 .
      *                  *---------------------------------------------*
      *                  * Se fine file : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-ric-con-900.
       det-ric-con-300.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sulla chiave                           *
      *                  *---------------------------------------------*
           if        rf-adc-tip-con       not  = d-con-arc-tip-sel or
                     rf-adc-pri-con       not  = spaces            or
                     rf-adc-pre-con       not  = spaces            or
                     rf-adc-num-con       not  = d-con-arc-num-con
                                                (1)                or
                     rf-adc-tip-arc       not  = d-con-arc-tip-arc or
                     rf-adc-cod-arc       not  = d-con-arc-cod-arc or
                     rf-adc-dpz-arc       not  = d-con-arc-dpz-arc
                     go to det-ric-con-900.
       det-ric-con-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
       det-ric-con-500.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   d-con-arc-num-ele      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione progressivo determinato         *
      *              *-------------------------------------------------*
           move      rf-adc-num-prg       to   d-con-arc-num-prg      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione interlocutore determinato       *
      *              *-------------------------------------------------*
           move      rf-adc-int-con       to   d-con-arc-int-con (1)  .
       det-ric-con-800.
       det-ric-con-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-ric-con-999.
       det-ric-con-999.
           exit.

