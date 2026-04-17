       Identification Division.
       Program-Id.                                 dstsorf0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    orf                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 10/11/98    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Modulo per la determinazione, relativamente ad un ordine for-  *
      * nitore, di :                                                   *
      *                                                                *
      * - Stato dell'ordine ad una data con i valori                   *
      *                                                                *
      *   - 01 : Chiuso                                                *
      *   - 02 : Evaso parzialmente                                    *
      *   - 03 : Evaso                                                 *
      *   - 04 : Inevaso                                               *
      *                                                                *
      * ============================================================== *
      *                                                                *
      *   N.B. : La determinazione esegue una scansione delle righe    *
      *          dell'ordine in corso di trattamento, pertanto se il   *
      *          modulo viene richiamato da un programma di scansione  *
      *          righe ordine, e' necessario il ripristino 'start' !!! *
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
      *        Input  : d-sts-orf-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-sts-orf-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-sts-orf-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-sts-orf-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "DT" - Determinazione status ordine                            *
      *                                                                *
      *                                                                *
      *        Input  : d-sts-orf-tip-ope = "DT"                       *
      *                                                                *
      *                 d-sts-orf-dat-rif = Data di riferimento        *
      *                                     (facoltativa)              *
      *                                                                *
      *                 Record di [oft]                                *
      *                                                                *
      *                                                                *
      *        Output : d-sts-orf-sts-ord = Status ordine fornitore    *
      *                                     - 01 : Chiuso              *
      *                                     - 02 : Evaso parzialmente  *
      *                                     - 03 : Evaso               *
      *                                     - 04 : Inevaso             *
      *                                                                *
      *                 d-sts-orf-tot-ord = Totale ordine, in valuta   *
      *                                                                *
      *                 d-sts-orf-imp-res = Importo residuo ordine, in *
      *                                     valuta                     *
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
      *        * [ofr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofr"                          .

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
      *        * Work per Det status ordine fornitore                  *
      *        *-------------------------------------------------------*
           05  w-det-sts-orf.
      *            *---------------------------------------------------*
      *            * Comodo per ridefinizione tipo riga                *
      *            *---------------------------------------------------*
               10  w-det-sts-orf-wtr.
                   15  w-det-sts-orf-wtp  pic  x(01)                  .
                   15  w-det-sts-orf-wtf  pic  x(01)                  .
                   15  filler             pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Status riga                                       *
      *            * - 01 : Chiusa                                     *
      *            * - 02 : Inevasa                                    *
      *            * - 03 : Evasa                                      *
      *            * - 04 : Super-evasa                                *
      *            *---------------------------------------------------*
               10  w-det-sts-orf-rst      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Contatore di righe                                *
      *            *---------------------------------------------------*
               10  w-det-sts-orf-ctr      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contatore di righe chiuse                         *
      *            *---------------------------------------------------*
               10  w-det-sts-orf-rch      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contatore di righe evase                          *
      *            *---------------------------------------------------*
               10  w-det-sts-orf-rev      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contatore di righe inevase                        *
      *            *---------------------------------------------------*
               10  w-det-sts-orf-rin      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contatore di righe evase parzialmente             *
      *            *---------------------------------------------------*
               10  w-det-sts-orf-rep      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Importo residuo riga ordine                       *
      *            *---------------------------------------------------*
               10  w-det-sts-orf-irr      pic s9(11)v9(02)            .
      *            *---------------------------------------------------*
      *            * Cumulo righe ordine                               *
      *            *---------------------------------------------------*
               10  w-det-sts-orf-itr      pic s9(11)v9(02)            .
      *            *---------------------------------------------------*
      *            * Comodo per qta' da ricevere riga ordine           *
      *            *---------------------------------------------------*
               10  w-det-sts-orf-qri      pic s9(06)v9(03)            .
      *            *---------------------------------------------------*
      *            * Max                                               *
      *            *---------------------------------------------------*
               10  w-det-sts-orf-max      pic  9(05)      value 999   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine fornitore                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dqevrof0.dtl"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione status ordine    *
      *    * fornitore                                                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dstsorf0.dtl"                   .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [oft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfoft"                          .

      ******************************************************************
       Procedure Division                using d-sts-orf
                                               rf-oft                 .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-sts-orf-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           if        d-sts-orf-tip-ope    =    "DT"
                     perform det-sts-orf-000
                                          thru det-sts-orf-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-sts-orf-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-sts-orf-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-sts-orf-tip-ope    =    "C?"
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
           move      zero                 to   d-sts-orf-dat-rif      .
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
      *                  * [ofr]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Open modulo di determinazione quantita' da  *
      *                  * evadere riga ordine                         *
      *                  *---------------------------------------------*
           perform   det-qev-rof-opn-000  thru det-qev-rof-opn-999    .
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
      *                  * [ofr]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Chiusura modulo di determinazione quantita' *
      *                  * da evadere riga ordine                      *
      *                  *---------------------------------------------*
           perform   det-qev-rof-cls-000  thru det-qev-rof-cls-999    .
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
                     move  spaces         to   d-sts-orf-exi-sts
           else      move  "#"            to   d-sts-orf-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione status ordine fornitore                    *
      *    *-----------------------------------------------------------*
       det-sts-orf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-sts-orf-sts-ord      .
           move      zero                 to   d-sts-orf-tot-ord      .
           move      zero                 to   d-sts-orf-imp-res      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatori righe                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-sts-orf-ctr      .
           move      zero                 to   w-det-sts-orf-rch      .
           move      zero                 to   w-det-sts-orf-rin      .
           move      zero                 to   w-det-sts-orf-rev      .
           move      zero                 to   w-det-sts-orf-rep      .
      *              *-------------------------------------------------*
      *              * Normalizzazione comodo per cumulo importi righe *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-sts-orf-itr      .
       det-sts-orf-010.
      *              *-------------------------------------------------*
      *              * Totale ordine in valuta : quello contenuto nel  *
      *              * record [oft]                                    *
      *              *-------------------------------------------------*
           move      rf-oft-tot-doc       to   d-sts-orf-tot-ord      .
       det-sts-orf-020.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di ordine chiuso               *
      *                  *---------------------------------------------*
           if        rf-oft-flg-och       not  = spaces
                     move  01             to   d-sts-orf-sts-ord
                     go to det-sts-orf-900.
       det-sts-orf-100.
      *              *-------------------------------------------------*
      *              * Start su file [ofr] : righe ordini              *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-oft-num-prt       to   rf-ofr-num-prt         .
           move      zero                 to   rf-ofr-num-prg         .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Esito della start                           *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to det-sts-orf-200.
      *                  *---------------------------------------------*
      *                  * Ordine chiuso                               *
      *                  *---------------------------------------------*
           move      01                   to   d-sts-orf-sts-ord      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-sts-orf-900.
       det-sts-orf-200.
      *              *-------------------------------------------------*
      *              * Read-next su file [ofr]                         *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Esito della read-next                       *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to det-sts-orf-300.
      *                  *---------------------------------------------*
      *                  * A test sul contatore righe ordine           *
      *                  *---------------------------------------------*
           go to     det-sts-orf-800.
       det-sts-orf-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo : a test sul contatore  *
      *                  *---------------------------------------------*
           if        rf-ofr-num-prt       not  = rf-oft-num-prt
                     go to det-sts-orf-800.
       det-sts-orf-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se riga ordine chiusa                  *
      *                  *---------------------------------------------*
           if        rf-ofr-flg-rch       not  = spaces
                     go to det-sts-orf-200.
      *                  *---------------------------------------------*
      *                  * Ridefinizione tipo riga                     *
      *                  *---------------------------------------------*
           move      rf-ofr-tip-rig       to   w-det-sts-orf-wtr      .
      *                  *---------------------------------------------*
      *                  * Test se riga di Commento                    *
      *                  *---------------------------------------------*
           if        w-det-sts-orf-wtp    =    "C"
                     go to det-sts-orf-200.
       det-sts-orf-500.
      *              *-------------------------------------------------*
      *              * Se la quantita' in riga ordine e' a zero, si    *
      *              * salta il test                                   *
      *              *-------------------------------------------------*
           if        rf-ofr-qta-ord       =    zero
                     go to det-sts-orf-200.
       det-sts-orf-520.
      *              *-------------------------------------------------*
      *              * Incremento del contatore di righe ordine        *
      *              *-------------------------------------------------*
           add       1                    to   w-det-sts-orf-ctr      .
           if        w-det-sts-orf-ctr    >    w-det-sts-orf-max
                     go to det-sts-orf-800.
       det-sts-orf-540.
      *              *-------------------------------------------------*
      *              * Se la quantita' in riga ordine e' negativa, si  *
      *              * forza come chiusa                               *
      *              *-------------------------------------------------*
           if        rf-ofr-qta-ord       <    zero
                     move  "#"            to   rf-ofr-flg-rch
                     go to det-sts-orf-600.
      *              *-------------------------------------------------*
      *              * Determinazione quantita' evasa riga ordine      *
      *              *-------------------------------------------------*
           move      "DT"                 to   d-qev-rof-tip-ope      .
           move      d-sts-orf-dat-rif    to   d-qev-rof-dat-rif      .
           perform   det-qev-rof-cll-000  thru det-qev-rof-cll-999    .
       det-sts-orf-550.
      *              *-------------------------------------------------*
      *              * Se la riga in corso di trattamento e' la prima, *
      *              * si bufferizza la quantita' da ricevere          *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-sts-orf-qri      .
           if        w-det-sts-orf-ctr    =    1
                     move  d-qev-rof-qta-dri
                                          to   w-det-sts-orf-qri      .
      *              *-------------------------------------------------*
      *              * Aggiornamento importo residuo                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la riga in esame non e' 'Inevasa' : la   *
      *                  * si ignora                                   *
      *                  *---------------------------------------------*
           if        d-qev-rof-qta-dri    not  > zero
                     go to det-sts-orf-600.
       det-sts-orf-560.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo riga          *
      *                  *---------------------------------------------*
           if        w-det-sts-orf-wtp    =    "P" or
                     w-det-sts-orf-wtp    =    "M" or
                     w-det-sts-orf-wtp    =    "V"
                     go to det-sts-orf-565
           else if   w-det-sts-orf-wtp    =    "A"
                     go to det-sts-orf-570
           else      go to det-sts-orf-600.
       det-sts-orf-565.
      *                  *---------------------------------------------*
      *                  * Se tipo riga : 'P' o 'M' o 'V'              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Calcolo dell'importo residuo per la     *
      *                      * riga ordine                             *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-sts-orf-irr      .
           multiply  d-qev-rof-qta-dri    by   rf-ofr-prz-net
                                        giving w-det-sts-orf-irr      .
      *                      *-----------------------------------------*
      *                      * Eventuale aggiustamento con decimali    *
      *                      * prezzo                                  *
      *                      *-----------------------------------------*
           if        rf-ofr-dec-prz       =    1
                     divide 10            into w-det-sts-orf-irr
           else if   rf-ofr-dec-prz       =    2
                     divide 100           into w-det-sts-orf-irr      .
      *                      *-----------------------------------------*
      *                      * Cumulo                                  *
      *                      *-----------------------------------------*
           add       w-det-sts-orf-irr    to   d-sts-orf-imp-res      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     det-sts-orf-600.
       det-sts-orf-570.
      *                  *---------------------------------------------*
      *                  * Se tipo riga : 'A'                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Somma dell'importo in valuta della riga *
      *                      * nel totale residuo                      *
      *                      *-----------------------------------------*
           add       rf-ofr-imp-rig       to   d-sts-orf-imp-res      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     det-sts-orf-600.
       det-sts-orf-600.
      *              *-------------------------------------------------*
      *              * Incremento totale righe                         *
      *              *-------------------------------------------------*
           add       rf-ofr-imp-rig       to   w-det-sts-orf-itr      .
      *              *-------------------------------------------------*
      *              * Incremento contatori relativi allo status riga  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Riga chiusa                                 *
      *                  *---------------------------------------------*
           if        rf-ofr-flg-rch       not  = spaces
                     add   1              to   w-det-sts-orf-rch
                     go to det-sts-orf-700.
      *                  *---------------------------------------------*
      *                  * Riga evasa                                  *
      *                  *---------------------------------------------*
           if        d-qev-rof-qta-dri    =    zero
                     add   1              to   w-det-sts-orf-rev
                     go to det-sts-orf-700.
      *                  *---------------------------------------------*
      *                  * Riga super-evasa                            *
      *                  *---------------------------------------------*
           if        d-qev-rof-qta-dri    <    zero
                     add   1              to   w-det-sts-orf-rev
                     go to det-sts-orf-700.
      *                  *---------------------------------------------*
      *                  * Riga inevasa                                *
      *                  *---------------------------------------------*
           if        d-qev-rof-qta-dri    =    d-qev-rof-qta-ord
                     add   1              to   w-det-sts-orf-rin
                     go to det-sts-orf-700.
      *                  *---------------------------------------------*
      *                  * Riga evasa parzialmente                     *
      *                  *---------------------------------------------*
           if        d-qev-rof-qta-dri    >    zero and
                     d-qev-rof-qta-dri    <    d-qev-rof-qta-ord
                     add   1              to   w-det-sts-orf-rep
                     go to det-sts-orf-700.
       det-sts-orf-700.
      *              *-------------------------------------------------*
      *              * Riciclo a riga ordine successiva                *
      *              *-------------------------------------------------*
           go to     det-sts-orf-200.
       det-sts-orf-800.
      *              *-------------------------------------------------*
      *              * Test sul contatore righe di evasione lette      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero di righe  *
      *                  * trovate                                     *
      *                  *---------------------------------------------*
           if        w-det-sts-orf-ctr    =    zero
                     go to det-sts-orf-810
           else if   w-det-sts-orf-ctr    =    1
                     go to det-sts-orf-820
           else if   w-det-sts-orf-ctr    >    1
                     go to det-sts-orf-830.
       det-sts-orf-810.
      *                  *---------------------------------------------*
      *                  * Se nessuna riga trovata status documento    *
      *                  * a 'chiuso'                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Forzatura status                        *
      *                      *-----------------------------------------*
           move      01                   to   d-sts-orf-sts-ord      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     det-sts-orf-900.
       det-sts-orf-820.
      *                  *---------------------------------------------*
      *                  * Se 1 riga trovata                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione status                   *
      *                      *-----------------------------------------*
           if        w-det-sts-orf-rev    >    zero
                     move  03             to   d-sts-orf-sts-ord
                     go to det-sts-orf-900.
           if        w-det-sts-orf-rch    >    zero
                     move  01             to   d-sts-orf-sts-ord
                     go to det-sts-orf-900.
           if        w-det-sts-orf-rin    >    zero
                     move  04             to   d-sts-orf-sts-ord
                     go to det-sts-orf-900.
           if        w-det-sts-orf-rep    >    zero
                     move  02             to   d-sts-orf-sts-ord
                     go to det-sts-orf-900.
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     det-sts-orf-900.
       det-sts-orf-830.
      *                  *---------------------------------------------*
      *                  * Se piu' righe trovate                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se anche una sola riga ordine evasa     *
      *                      * parzialmente : ordine evaso parzialmen- *
      *                      * te                                      *
      *                      *-----------------------------------------*
           if        w-det-sts-orf-rep    >    zero
                     move  02             to   d-sts-orf-sts-ord
                     go to det-sts-orf-900.
      *                      *-----------------------------------------*
      *                      * Se il numero totale di righe e' uguale  *
      *                      * al totale di righe chiuse : ordine      *
      *                      * chiuso                                  *
      *                      *-----------------------------------------*
           if        w-det-sts-orf-ctr    =    w-det-sts-orf-rch
                     move  01             to   d-sts-orf-sts-ord
                     go to det-sts-orf-900.
      *                      *-----------------------------------------*
      *                      * Se il numero totale di righe e' uguale  *
      *                      * al totale di righe inevase : ordine     *
      *                      * inevaso                                 *
      *                      *-----------------------------------------*
           if        w-det-sts-orf-ctr    =    w-det-sts-orf-rin
                     move  04             to   d-sts-orf-sts-ord
                     go to det-sts-orf-900.
      *                      *-----------------------------------------*
      *                      * Se il numero totale di righe e' uguale  *
      *                      * al totale di righe evase : ordine evaso *
      *                      *-----------------------------------------*
           if        w-det-sts-orf-ctr    =    w-det-sts-orf-rev
                     move  03             to   d-sts-orf-sts-ord
                     go to det-sts-orf-900.
      *                      *-----------------------------------------*
      *                      * Se tutti i contatori a zero : ordine    *
      *                      * chiuso                                  *
      *                      *-----------------------------------------*
           if        w-det-sts-orf-rch    =    zero and
                     w-det-sts-orf-rin    =    zero and
                     w-det-sts-orf-rev    =    zero
                     move  01             to   d-sts-orf-sts-ord
                     go to det-sts-orf-900.
      *                      *-----------------------------------------*
      *                      * Se tutti i contatori a non zero: ordine *
      *                      * evaso parzialmente                      *
      *                      *-----------------------------------------*
           if        w-det-sts-orf-rch    >    zero and
                     w-det-sts-orf-rin    >    zero and
                     w-det-sts-orf-rev    >    zero
                     move  02             to   d-sts-orf-sts-ord
                     go to det-sts-orf-900.
      *                      *-----------------------------------------*
      *                      * Se righe chiuse e evase : ordine evaso  *
      *                      *-----------------------------------------*
           if        w-det-sts-orf-rch    >    zero and
                     w-det-sts-orf-rin    =    zero and
                     w-det-sts-orf-rev    >    zero
                     move  03             to   d-sts-orf-sts-ord
                     go to det-sts-orf-900.
      *                      *-----------------------------------------*
      *                      * Se righe chiuse e inevase : ordine      *
      *                      * evaso parzialmente                      *
      *                      *-----------------------------------------*
           if        w-det-sts-orf-rch    >    zero and
                     w-det-sts-orf-rin    >    zero and
                     w-det-sts-orf-rev    =    zero
                     move  02             to   d-sts-orf-sts-ord
                     go to det-sts-orf-900.
      *                      *-----------------------------------------*
      *                      * Se righe evase e inevase : ordine evaso *
      *                      * parzialmente                            *
      *                      *-----------------------------------------*
           if        w-det-sts-orf-rch    =    zero and
                     w-det-sts-orf-rin    >    zero and
                     w-det-sts-orf-rev    >    zero
                     move  02             to   d-sts-orf-sts-ord
                     go to det-sts-orf-900.
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     det-sts-orf-900.
       det-sts-orf-900.
      *              *-------------------------------------------------*
      *              * Se status indeterminato : forzatura a chiuso    *
      *              *-------------------------------------------------*
           if        d-sts-orf-sts-ord    =    zero
                     move  01             to   d-sts-orf-sts-ord      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-sts-orf-999.
       det-sts-orf-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per determinazione quantita' da evadere riga  *
      *    * ordine fornitore                                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dqevrof0.dts"                   .

