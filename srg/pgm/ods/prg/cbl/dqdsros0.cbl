       Identification Division.
       Program-Id.                                 dqdsros0           .
      *================================================================*
      *                                                                *
      * Modulo per la determinazione, relativamente ad una riga ordine *
      * di spedizione, di :                                            *
      *                                                                *
      * - Quantita' in ordine di spedizione                            *
      * - Quantita' gia' spedita                                       *
      *                                                                *
      * e, di conseguenza, di :                                        *
      *                                                                *
      * - Quantita' ancora da spedire                                  *
      *                                                                *
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
      *        Input  : d-qds-ros-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-qds-ros-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-qds-ros-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-qds-ros-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "DT" - Determinazione status riga ordine di spedizione         *
      *                                                                *
      *                                                                *
      *        Input  : d-qds-ros-tip-ope = "DT"                       *
      *                                                                *
      *                 d-qds-ros-dat-rif = Data di riferimento        *
      *                                     (facoltativa)              *
      *                                                                *
      *                 Record di [osr] relativo alla riga ordine      *
      *                 di spedizione a disposizione in rf-osr         *
      *                                                                *
      *                                                                *
      *        Output : d-qds-ros-qta-ord = Quantita' in ordine di     *
      *                                     spedizione                 *
      *                                                                *
      *                 d-qds-ros-qta-gsp = Quantita' gia' spedita     *
      *                                                                *
      *                 d-qds-ros-qta-dsp = Quantita' ancora da spe-   *
      *                                     dire                       *
      *                                                                *
      *                 d-qds-ros-rda-aoc = Flag di riga di addebito   *
      *                                     aperta/chiusa              *
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
      *        * [bir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbir"                          .

      *    *===========================================================*
      *    * Work-area per parametri esclusi da link-area              *
      *    *-----------------------------------------------------------*
       01  w-nol.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-nol-ctr-opn              pic s9(05) value zero       .

      *    *===========================================================*
      *    * Work-area per ridefinizione tipo riga                     *
      *    *-----------------------------------------------------------*
       01  w-rdf-tip-rig.
           05  w-rdf-tip-rig-wtr.
               10  w-rdf-tip-rig-wtp      pic  x(01)                  .
               10  w-rdf-tip-rig-wtf      pic  x(01)                  .
               10  filler                 pic  x(03)                  .

      *    *===========================================================*
      *    * Work-area per la determinazione, relativamente ad una ri- *
      *    * ga ordine di spedizione, di :                             *
      *    *                                                           *
      *    *    - Quantita' in ordine di spedizione                    *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       01  w-ros-ord.
      *        *-------------------------------------------------------*
      *        * Parametri in Output                                   *
      *        *-------------------------------------------------------*
           05  w-ros-ord-pxo.
      *            *---------------------------------------------------*
      *            * Quantita' in ordine                               *
      *            *---------------------------------------------------*
               10  w-ros-ord-qta-ord      pic s9(08)v9(03)            .

      *    *===========================================================*
      *    * Work-area per la determinazione, relativamente ad una ri- *
      *    * ga ordine di spedizione, di :                             *
      *    *                                                           *
      *    *    - Quantita' gia' spedita                               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       01  w-ros-gsp.
      *        *-------------------------------------------------------*
      *        * Parametri in Output                                   *
      *        *-------------------------------------------------------*
           05  w-ros-gsp-pxo.
      *            *---------------------------------------------------*
      *            * Quantita' gia' spedita                            *
      *            *---------------------------------------------------*
               10  w-ros-gsp-qta-gsp      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Flag di evasione riga di addebito                 *
      *            *---------------------------------------------------*
               10  w-ros-gsp-flg-era      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per la selezione sul record [bir] relativamente *
      *    * al record di [osr], per compatibilita'                    *
      *    *-----------------------------------------------------------*
       01  w-sel-bir-osr.
      *        *-------------------------------------------------------*
      *        * Parametri in Output                                   *
      *        *-------------------------------------------------------*
           05  w-sel-bir-osr-pxo.
      *            *---------------------------------------------------*
      *            * Flag su esito della selezione                     *
      *            *  - Spaces : Selezione superata                    *
      *            *  - #      : Selezione non superata                *
      *            *---------------------------------------------------*
               10  w-sel-bir-osr-flg      pic  x(01)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da     *
      *    * spedire riga ordine di spedizione                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/dqdsros0.dtl"                   .

      *    *-----------------------------------------------------------*
      *    * [osr]                                                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosr"                          .

      ******************************************************************
       Procedure Division                using d-qds-ros
                                               rf-osr                 .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-qds-ros-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           if        d-qds-ros-tip-ope    =    "DT"
                     perform det-qds-ros-000
                                          thru det-qds-ros-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-qds-ros-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-qds-ros-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-qds-ros-tip-ope    =    "C?"
                     perform tst-cnc-mod-000
                                          thru tst-cnc-mod-999        .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione data di riferimento             *
      *              *-------------------------------------------------*
           move      zero                 to   d-qds-ros-dat-rif      .
      *              *-------------------------------------------------*
      *              * Incremento contatore Open modulo                *
      *              *-------------------------------------------------*
           add       1                    to   w-nol-ctr-opn          .
       rou-opn-fls-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [bir]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
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
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [bir]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
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
                     move  spaces         to   d-qds-ros-exi-sts
           else      move  "#"            to   d-qds-ros-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione status riga ordine                         *
      *    *-----------------------------------------------------------*
       det-qds-ros-000.
      *              *-------------------------------------------------*
      *              * Tipo riga in comodo ridefinito                  *
      *              *-------------------------------------------------*
           move      rf-osr-tip-rig       to   w-rdf-tip-rig-wtr      .
      *              *-------------------------------------------------*
      *              * Determinazione della quantita' in ordine di     *
      *              * spedizione                                      *
      *              *-------------------------------------------------*
           perform   rpd-ros-ord-000      thru rpd-ros-ord-999        .
           move      w-ros-ord-qta-ord    to   d-qds-ros-qta-ord      .
       det-qds-ros-100.
      *              *-------------------------------------------------*
      *              * Determinazione della quantita' gia' spedita     *
      *              *-------------------------------------------------*
           perform   rpd-ros-gsp-000      thru rpd-ros-gsp-999        .
           move      w-ros-gsp-qta-gsp    to   d-qds-ros-qta-gsp      .
       det-qds-ros-400.
      *              *-------------------------------------------------*
      *              * Determinazione della quantita' ancora da spedi- *
      *              * re                                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se riga di Addebito                         *
      *                  *---------------------------------------------*
           if        w-rdf-tip-rig-wtp    not  = "A"
                     go to det-qds-ros-410.
      *                      *-----------------------------------------*
      *                      * Determinazione flag di riga aperta o    *
      *                      * chiusa                                  *
      *                      *-----------------------------------------*
           if        w-ros-gsp-flg-era    not  = spaces
                     move  "C"            to   d-qds-ros-rda-aoc
           else      move  "A"            to   d-qds-ros-rda-aoc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     det-qds-ros-999.
       det-qds-ros-410.
      *                  *---------------------------------------------*
      *                  * Se riga chiusa : quantita' da spedire a ze- *
      *                  * ro                                          *
      *                  *---------------------------------------------*
           if        rf-osr-flg-rch       not  = spaces
                     move  zero           to   d-qds-ros-qta-dsp
                     go to det-qds-ros-999.
      *                  *---------------------------------------------*
      *                  * Se quantita' in ordine di spedizione a ze-  *
      *                  * ro : quantita' da spedire a zero            *
      *                  *---------------------------------------------*
           if        d-qds-ros-qta-ord    =    zero
                     move  zero           to   d-qds-ros-qta-dsp
                     go to det-qds-ros-999.
      *                  *---------------------------------------------*
      *                  * Differenza algebrica tra :                  *
      *                  *   - Quantita' in ordine                     *
      *                  * e                                           *
      *                  *   - Quantita' gia' spedita                  *
      *                  *---------------------------------------------*
           move      d-qds-ros-qta-ord    to   d-qds-ros-qta-dsp      .
           subtract  d-qds-ros-qta-gsp    from d-qds-ros-qta-dsp      .
      *                  *---------------------------------------------*
      *                  * Riduzione a zero del residuo eventualmente  *
      *                  * negativo, tenendo conto del segno algebri-  *
      *                  * co della quantita' in ordine                *
      *                  *---------------------------------------------*
           if        d-qds-ros-qta-ord    >    zero and
                     d-qds-ros-qta-dsp    <    zero
                     move  zero           to   d-qds-ros-qta-dsp      .
           if        d-qds-ros-qta-ord    <    zero and
                     d-qds-ros-qta-dsp    >    zero
                     move  zero           to   d-qds-ros-qta-dsp      .
       det-qds-ros-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione, relativamente ad una riga  *
      *    * ordine di spedizione, di :                                *
      *    *                                                           *
      *    *    - Quantita' in ordine                                  *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *    - Record di [osr] relativo alla riga ordine di spedi-  *
      *    *      zione a disposizione in rf-osr                       *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *    - w-ros-ord-qta-ord : Quantita' in ordine              *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       rpd-ros-ord-000.
      *              *-------------------------------------------------*
      *              * Si considera come 'Quantita' in ordine' il cam- *
      *              * po 'qta-ven' del record di [osr] in esame       *
      *              *-------------------------------------------------*
           move      rf-osr-qta-ven       to   w-ros-ord-qta-ord      .
      *              *-------------------------------------------------*
      *              * In presenza di segnale della 3a quantita' atti- *
      *              * va, si pone quest'ultima come quantita' in or-  *
      *              * dine                                            *
      *              *-------------------------------------------------*
           if        rf-osr-snx-3qt       =    1
                     move  rf-osr-qta-a03 to   w-ros-ord-qta-ord      .
       rpd-ros-ord-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione, relativamente ad una riga  *
      *    * ordine di spedizione, di :                                *
      *    *                                                           *
      *    *    - Quantita' gia' spedita                               *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *    - Record di [osr] relativo alla riga ordine di spedi-  *
      *    *      zione a disposizione in rf-osr                       *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *    - w-ros-gsp-qta-gsp : Quantita' gia' spedita           *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       rpd-ros-gsp-000.
      *              *-------------------------------------------------*
      *              * Inizializzazioni preliminari                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag di evasione riga di    *
      *                  * addebito                                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-ros-gsp-flg-era      .
      *                  *---------------------------------------------*
      *                  * Quantita' gia' spedita : a zero             *
      *                  *---------------------------------------------*
           move      zero                 to   w-ros-gsp-qta-gsp      .
       rpd-ros-gsp-100.
      *              *-------------------------------------------------*
      *              * Start iniziale su [bir]                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RIFODS    "         to   f-key                  .
           move      rf-osr-cod-dpz       to   rf-bir-cod-dpz         .
           move      rf-osr-num-prt       to   rf-bir-ods-prt         .
           move      rf-osr-num-prg       to   rf-bir-ods-prg         .
           move      zero                 to   rf-bir-coc-prt         .
           move      zero                 to   rf-bir-coc-prg         .
           move      zero                 to   rf-bir-num-prt         .
           move      zero                 to   rf-bir-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rpd-ros-gsp-999.
       rpd-ros-gsp-200.
      *              *-------------------------------------------------*
      *              * Read Next su [bir]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Read Next                                   *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Se At end : uscita                          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rpd-ros-gsp-999.
       rpd-ros-gsp-300.
      *              *-------------------------------------------------*
      *              * Test Max su [bir]                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su :                                   *
      *                  *    - Codice dipendenza                      *
      *                  *    - Numero protocollo ordine di spedizione *
      *                  *    - Numero riga ordine di spedizione       *
      *                  *---------------------------------------------*
           if        rf-bir-cod-dpz       =    rf-osr-cod-dpz and
                     rf-bir-ods-prt       =    rf-osr-num-prt and
                     rf-bir-ods-prg       =    rf-osr-num-prg
                     go to rpd-ros-gsp-400.
      *                  *---------------------------------------------*
      *                  * Se oltre il Max : uscita                    *
      *                  *---------------------------------------------*
           go to     rpd-ros-gsp-999.
       rpd-ros-gsp-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record di [bir] rispetto al re-   *
      *              * cord di [osr]                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           perform   sel-bir-osr-000      thru sel-bir-osr-999        .
      *                  *---------------------------------------------*
      *                  * Se selezione non superata : si ignora il    *
      *                  * record letto                                *
      *                  *---------------------------------------------*
           if        w-sel-bir-osr-flg    not  = spaces
                     go to rpd-ros-gsp-800.
       rpd-ros-gsp-500.
      *              *-------------------------------------------------*
      *              * Considerazioni sul record di [bir] letto e se-  *
      *              * lezionato rispetto ad [osr]                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si somma la 'Quantita' di vendita' contenu- *
      *                  * ta nel record alla 'Quantita' gia' spedita' *
      *                  * che si sta determinando                     *
      *                  *---------------------------------------------*
           add       rf-bir-qta-ven       to   w-ros-gsp-qta-gsp      .
       rpd-ros-gsp-800.
      *              *-------------------------------------------------*
      *              * Se tipo riga 'A' : set del flag di evasione ad- *
      *              * debito e uscita                                 *
      *              *-------------------------------------------------*
           if        w-rdf-tip-rig-wtp    =    "A"
                     move  "#"            to   w-ros-gsp-flg-era
                     go to rpd-ros-gsp-999.
      *              *-------------------------------------------------*
      *              * Riciclo a Read Next su [bir]                    *
      *              *-------------------------------------------------*
           go to     rpd-ros-gsp-200.
       rpd-ros-gsp-999.
           exit.

      *    *===========================================================*
      *    * Routine per la selezione sul record [bir] relativamente   *
      *    * al record di [osr], per compatibilita'                    *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *    - Record di [bir] relativo alla riga bolla cliente a   *
      *    *      disposizione in rf-bir                               *
      *    *                                                           *
      *    *    - Record di [osr] relativo alla riga ordine di spedi-  *
      *    *      zione a disposizione in rf-osr                       *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *    - w-sel-bir-osr-flg : Esito della selezione            *
      *    *                            - Spaces : Ok                  *
      *    *                            - #      : Ko                  *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       sel-bir-osr-000.
      *              *-------------------------------------------------*
      *              * Nessuna selezione : sempre uscita con Ok        *
      *              *-------------------------------------------------*
           move      spaces               to   w-sel-bir-osr-flg      .
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo archivio                    *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bir-tip-arc       not  = rf-osr-tip-arc
                     go to sel-bir-osr-900.
       sel-bir-osr-025.
      *              *-------------------------------------------------*
      *              * Selezione su : Codice archivio                  *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bir-cod-arc       not  = rf-osr-cod-arc
                     go to sel-bir-osr-900.
       sel-bir-osr-050.
      *              *-------------------------------------------------*
      *              * Selezione su : Codice dipendenza dell'archivio  *
      *              *                                                 *
      *              * Non viene fatta alcuna selezione su questo cam- *
      *              * po, in quanto si permette di spedire ad una di- *
      *              * pendenza cliente diversa da quanto stabilito in *
      *              * ordine cliente.                                 *
      *              *-------------------------------------------------*
       sel-bir-osr-075.
      *              *-------------------------------------------------*
      *              * Selezione su : Flag blocco di documento         *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bir-bld-flb       not  = rf-osr-bld-flb
                     go to sel-bir-osr-900.
       sel-bir-osr-100.
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo riga                        *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bir-tip-rig       not  = rf-osr-tip-rig
                     go to sel-bir-osr-900.
       sel-bir-osr-125.
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo codice di magazzino         *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bir-tip-mag       not  = rf-osr-tip-mag
                     go to sel-bir-osr-900.
       sel-bir-osr-150.
      *              *-------------------------------------------------*
      *              * Selezione su : Codice prodotto numerico         *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bir-num-pro       not  = rf-osr-num-pro
                     go to sel-bir-osr-900.
       sel-bir-osr-175.
      *              *-------------------------------------------------*
      *              * Selezione su : Codice prodotto alfanumerico     *
      *              *                                                 *
      *              * Non viene fatta alcuna selezione su questo cam- *
      *              * po, in quanto si prevede che possa essere nel   *
      *              * frattempo variato l'identificatore alfanumerico *
      *              * del prodotto. In ogni caso l'identificatore e-  *
      *              * sposto sara' l'identificatore alfanumerico at-  *
      *              * tuale.                                          *
      *              *-------------------------------------------------*
       sel-bir-osr-200.
      *              *-------------------------------------------------*
      *              * Selezione su : Sigla della variante per il pro- *
      *              *                dotto                            *
      *              *                                                 *
      *              * Non viene fatta alcuna selezione su questo cam- *
      *              * po, in quanto si permette di spedire una va-    *
      *              * riante diversa da quanto stabilito in ordine    *
      *              * cliente.                                        *
      *              *-------------------------------------------------*
       sel-bir-osr-225.
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo prodotto                    *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bir-tip-pro       not  = rf-osr-tip-pro
                     go to sel-bir-osr-900.
       sel-bir-osr-250.
      *              *-------------------------------------------------*
      *              * Selezione su : Quantita' di vendita             *
      *              *                                                 *
      *              * Deve essere diversa da Zero, non si prendono in *
      *              * considerazione righe bolla con quantita' a Ze-  *
      *              * ro.                                             *
      *              *-------------------------------------------------*
           if        rf-bir-qta-ven       =    zero
                     go to sel-bir-osr-900.
       sel-bir-osr-275.
      *              *-------------------------------------------------*
      *              * Selezione su : Data di riferimento              *
      *              *-------------------------------------------------*
           if        d-qds-ros-dat-rif    =    zero
                     go to sel-bir-osr-800.
           if        rf-bir-dat-doc       >    d-qds-ros-dat-rif
                     go to sel-bir-osr-900.
       sel-bir-osr-800.
      *              *-------------------------------------------------*
      *              * Uscita per selezione : Ok                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-sel-bir-osr-flg      .
           go to     sel-bir-osr-999.
       sel-bir-osr-900.
      *              *-------------------------------------------------*
      *              * Uscita per selezione : Ko                       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-sel-bir-osr-flg      .
           go to     sel-bir-osr-999.
       sel-bir-osr-999.
           exit.
