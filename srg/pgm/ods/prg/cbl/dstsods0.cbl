       Identification Division.
       Program-Id.                                 dstsods0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    ods                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 10/11/98    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Modulo per la determinazione, relativamente ad un ordine di    *
      * spedizione cliente, di :                                       *
      *                                                                *
      * - Stato dell'ordine di spedizione ad una data con i valori     *
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
      *          righe ordine, porre la massima attenzione ai cicli di *
      *          'start' e 'read-next'                                 *
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
      *        Input  : d-sts-ods-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-sts-ods-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-sts-ods-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-sts-ods-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "DT" - Determinazione status ordine                            *
      *                                                                *
      *                                                                *
      *        Input  : d-sts-ods-tip-ope = "DT"                       *
      *                                                                *
      *                 d-sts-ods-dat-rif = Data di riferimento        *
      *                                     (facoltativa)              *
      *                                                                *
      *                 Record di [ost]                                *
      *                                                                *
      *                                                                *
      *        Output : d-sts-ods-sts-ord = Status ordine spedizione   *
      *                                     - 01 : Chiuso              *
      *                                     - 02 : Evaso parzialmente  *
      *                                     - 03 : Evaso               *
      *                                     - 04 : Inevaso             *
      *                                                                *
      *                 d-sts-ods-tot-ord = Totale ordine, in valuta   *
      *                                                                *
      *                 d-sts-ods-imp-res = Importo residuo ordine, in *
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
      *        * [osr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosr"                          .

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
      *        * Work per Det status ordine cliente                    *
      *        *-------------------------------------------------------*
           05  w-det-sts-ods.
      *            *---------------------------------------------------*
      *            * Comodo per ridefinizione tipo riga                *
      *            *---------------------------------------------------*
               10  w-det-sts-ods-wtr.
                   15  w-det-sts-ods-wtp  pic  x(01)                  .
                   15  w-det-sts-ods-wtf  pic  x(01)                  .
                   15  filler             pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Status riga                                       *
      *            * - 01 : Chiusa                                     *
      *            * - 02 : Inevasa                                    *
      *            * - 03 : Evasa                                      *
      *            * - 04 : Super-evasa                                *
      *            *---------------------------------------------------*
               10  w-det-sts-ods-rst      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Contatore di righe                                *
      *            *---------------------------------------------------*
               10  w-det-sts-ods-ctr      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contatore di righe chiuse                         *
      *            *---------------------------------------------------*
               10  w-det-sts-ods-rch      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contatore di righe evase                          *
      *            *---------------------------------------------------*
               10  w-det-sts-ods-rev      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contatore di righe inevase                        *
      *            *---------------------------------------------------*
               10  w-det-sts-ods-rin      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contatore di righe evase parzialmente             *
      *            *---------------------------------------------------*
               10  w-det-sts-ods-rep      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Importo residuo riga ordine                       *
      *            *---------------------------------------------------*
               10  w-det-sts-ods-irr      pic s9(11)v9(02)            .
      *            *---------------------------------------------------*
      *            * Max                                               *
      *            *---------------------------------------------------*
               10  w-det-sts-ods-max      pic  9(05)      value 999   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine cliente                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/dqdsros0.dtl"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione status ordine    *
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/dstsods0.dtl"                   .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ost]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfost"                          .

      ******************************************************************
       Procedure Division                using d-sts-ods
                                               rf-ost                 .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-sts-ods-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           if        d-sts-ods-tip-ope    =    "DT"
                     perform det-sts-ods-000
                                          thru det-sts-ods-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-sts-ods-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-sts-ods-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-sts-ods-tip-ope    =    "C?"
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
           move      zero                 to   d-sts-ods-dat-rif      .
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
           perform   det-qds-ros-opn-000  thru det-qds-ros-opn-999    .
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
           perform   det-qds-ros-cls-000  thru det-qds-ros-cls-999    .
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
                     move  spaces         to   d-sts-ods-exi-sts
           else      move  "#"            to   d-sts-ods-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione status ordine cliente                      *
      *    *-----------------------------------------------------------*
       det-sts-ods-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-sts-ods-sts-ord      .
           move      zero                 to   d-sts-ods-tot-ord      .
           move      zero                 to   d-sts-ods-imp-res      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatori righe                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-sts-ods-ctr      .
           move      zero                 to   w-det-sts-ods-rch      .
           move      zero                 to   w-det-sts-ods-rin      .
           move      zero                 to   w-det-sts-ods-rev      .
           move      zero                 to   w-det-sts-ods-rep      .
       det-sts-ods-010.
      *              *-------------------------------------------------*
      *              * Totale ordine in valuta : quello contenuto nel  *
      *              * record [ost]                                    *
      *              *-------------------------------------------------*
           move      rf-ost-tot-doc       to   d-sts-ods-tot-ord      .
       det-sts-ods-020.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di ordine chiuso               *
      *                  *---------------------------------------------*
           if        rf-ost-flg-sch       not  = spaces
                     move  01             to   d-sts-ods-sts-ord
                     go to det-sts-ods-900.
       det-sts-ods-100.
      *              *-------------------------------------------------*
      *              * Start su file [osr] : righe ordini              *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-ost-num-prt       to   rf-osr-num-prt         .
           move      zero                 to   rf-osr-num-prg         .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                  *---------------------------------------------*
      *                  * Esito della start                           *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to det-sts-ods-200.
      *                  *---------------------------------------------*
      *                  * Ordine chiuso                               *
      *                  *---------------------------------------------*
           move      01                   to   d-sts-ods-sts-ord      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-sts-ods-900.
       det-sts-ods-200.
      *              *-------------------------------------------------*
      *              * Read-next su file [osr]                         *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                  *---------------------------------------------*
      *                  * Esito della read-next                       *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to det-sts-ods-300.
      *                  *---------------------------------------------*
      *                  * A test sul contatore righe ordine           *
      *                  *---------------------------------------------*
           go to     det-sts-ods-800.
       det-sts-ods-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo : a test sul contatore  *
      *                  *---------------------------------------------*
           if        rf-osr-num-prt       not  = rf-ost-num-prt
                     go to det-sts-ods-800.
       det-sts-ods-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se riga ordine chiusa                  *
      *                  *---------------------------------------------*
           if        rf-osr-flg-rch       not  = spaces
                     go to det-sts-ods-200.
      *                  *---------------------------------------------*
      *                  * Ridefinizione tipo riga                     *
      *                  *---------------------------------------------*
           move      rf-osr-tip-rig       to   w-det-sts-ods-wtr      .
      *                  *---------------------------------------------*
      *                  * Test se riga di Commento                    *
      *                  *---------------------------------------------*
           if        w-det-sts-ods-wtp    =    "C"
                     go to det-sts-ods-200.
       det-sts-ods-500.
      *              *-------------------------------------------------*
      *              * Incremento del contatore di righe ordine        *
      *              *-------------------------------------------------*
           add       1                    to   w-det-sts-ods-ctr      .
           if        w-det-sts-ods-ctr    >    w-det-sts-ods-max
                     go to det-sts-ods-800.
       det-sts-ods-540.
      *              *-------------------------------------------------*
      *              * Se la quantita' in riga ordine e' a zero, si    *
      *              * considera comunque evasa                        *
      *              *-------------------------------------------------*
           if        rf-osr-qta-ven       =    zero
                     move  zero           to   d-qds-ros-qta-dsp
                     go to det-sts-ods-550.
      *              *-------------------------------------------------*
      *              * Determinazione quantita' evasa riga ordine      *
      *              *-------------------------------------------------*
           move      "DT"                 to   d-qds-ros-tip-ope      .
           move      d-sts-ods-dat-rif    to   d-qds-ros-dat-rif      .
           perform   det-qds-ros-cll-000  thru det-qds-ros-cll-999    .
       det-sts-ods-550.
      *              *-------------------------------------------------*
      *              * Aggiornamento importo residuo                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la riga in esame non e' 'Inevasa' : la   *
      *                  * si ignora                                   *
      *                  *---------------------------------------------*
           if        d-qds-ros-qta-dsp    not  > zero
                     go to det-sts-ods-600.
       det-sts-ods-560.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo riga          *
      *                  *---------------------------------------------*
           if        w-det-sts-ods-wtp    =    "P"
                     go to det-sts-ods-565
           else if   w-det-sts-ods-wtp    =    "A"
                     go to det-sts-ods-570
           else      go to det-sts-ods-600.
       det-sts-ods-565.
      *                  *---------------------------------------------*
      *                  * Se tipo riga : 'P'                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Calcolo dell'importo residuo per la     *
      *                      * riga ordine                             *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-sts-ods-irr      .
           multiply  d-qds-ros-qta-dsp    by   rf-osr-prz-net
                                        giving w-det-sts-ods-irr      .
      *                      *-----------------------------------------*
      *                      * Eventuale aggiustamento con decimali    *
      *                      * prezzo                                  *
      *                      *-----------------------------------------*
           if        rf-osr-dec-prz       =    1
                     divide 10            into w-det-sts-ods-irr
           else if   rf-osr-dec-prz       =    2
                     divide 100           into w-det-sts-ods-irr      .
      *                      *-----------------------------------------*
      *                      * Cumulo                                  *
      *                      *-----------------------------------------*
           add       w-det-sts-ods-irr    to   d-sts-ods-imp-res      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     det-sts-ods-600.
       det-sts-ods-570.
      *                  *---------------------------------------------*
      *                  * Se tipo riga : 'A'                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Somma dell'importo in valuta della riga *
      *                      * nel totale residuo                      *
      *                      *-----------------------------------------*
           add       rf-osr-imp-rig       to   d-sts-ods-imp-res      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     det-sts-ods-600.
       det-sts-ods-600.
      *              *-------------------------------------------------*
      *              * Incremento contatori relativi allo status riga  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Riga chiusa                                 *
      *                  *---------------------------------------------*
           if        rf-osr-flg-rch       not  = spaces
                     add   1              to   w-det-sts-ods-rch
                     go to det-sts-ods-700.
      *                  *---------------------------------------------*
      *                  * Riga evasa                                  *
      *                  *---------------------------------------------*
           if        d-qds-ros-qta-dsp    =    zero
                     add   1              to   w-det-sts-ods-rev
                     go to det-sts-ods-700.
      *                  *---------------------------------------------*
      *                  * Riga super-evasa                            *
      *                  *---------------------------------------------*
           if        d-qds-ros-qta-dsp    <    zero
                     add   1              to   w-det-sts-ods-rev
                     go to det-sts-ods-700.
      *                  *---------------------------------------------*
      *                  * Riga inevasa                                *
      *                  *---------------------------------------------*
           if        d-qds-ros-qta-dsp    =    rf-osr-qta-ven
                     add   1              to   w-det-sts-ods-rin
                     go to det-sts-ods-700.
      *                  *---------------------------------------------*
      *                  * Riga evasa parzialmente                     *
      *                  *---------------------------------------------*
           if        d-qds-ros-qta-dsp    >    zero and
                     d-qds-ros-qta-dsp    <    rf-osr-qta-ven
                     add   1              to   w-det-sts-ods-rep
                     go to det-sts-ods-700.
       det-sts-ods-700.
      *              *-------------------------------------------------*
      *              * Riciclo a riga ordine successiva                *
      *              *-------------------------------------------------*
           go to     det-sts-ods-200.
       det-sts-ods-800.
      *              *-------------------------------------------------*
      *              * Test sul contatore righe di evasione lette      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero di righe  *
      *                  * trovate                                     *
      *                  *---------------------------------------------*
           if        w-det-sts-ods-ctr    =    zero
                     go to det-sts-ods-810
           else if   w-det-sts-ods-ctr    =    1
                     go to det-sts-ods-820
           else if   w-det-sts-ods-ctr    >    1
                     go to det-sts-ods-830.
       det-sts-ods-810.
      *                  *---------------------------------------------*
      *                  * Se nessuna riga trovata status documento    *
      *                  * a 'chiuso'                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Forzatura status                        *
      *                      *-----------------------------------------*
           move      01                   to   d-sts-ods-sts-ord      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     det-sts-ods-900.
       det-sts-ods-820.
      *                  *---------------------------------------------*
      *                  * Se 1 riga trovata                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione status                   *
      *                      *-----------------------------------------*
           if        w-det-sts-ods-ctr    =    w-det-sts-ods-rev
                     move  03             to   d-sts-ods-sts-ord
                     go to det-sts-ods-900.
           if        w-det-sts-ods-ctr    =    w-det-sts-ods-rch
                     move  01             to   d-sts-ods-sts-ord
                     go to det-sts-ods-900.
           if        w-det-sts-ods-ctr    =    w-det-sts-ods-rin
                     move  04             to   d-sts-ods-sts-ord
                     go to det-sts-ods-900.
           if        w-det-sts-ods-ctr    =    w-det-sts-ods-rep
                     move  02             to   d-sts-ods-sts-ord
                     go to det-sts-ods-900.
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     det-sts-ods-900.
       det-sts-ods-830.
      *                  *---------------------------------------------*
      *                  * Se piu' righe trovate                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se anche una sola riga ordine evasa     *
      *                      * parzialmente : ordine evaso parzialmen- *
      *                      * te                                      *
      *                      *-----------------------------------------*
           if        w-det-sts-ods-rep    >    zero
                     move  02             to   d-sts-ods-sts-ord
                     go to det-sts-ods-900.
      *                      *-----------------------------------------*
      *                      * Se il numero totale di righe e' uguale  *
      *                      * al totale di righe chiuse : ordine      *
      *                      * chiuso                                  *
      *                      *-----------------------------------------*
           if        w-det-sts-ods-ctr    =    w-det-sts-ods-rch
                     move  01             to   d-sts-ods-sts-ord
                     go to det-sts-ods-900.
      *                      *-----------------------------------------*
      *                      * Se il numero totale di righe e' uguale  *
      *                      * al totale di righe inevase : ordine     *
      *                      * inevaso                                 *
      *                      *-----------------------------------------*
           if        w-det-sts-ods-ctr    =    w-det-sts-ods-rin
                     move  04             to   d-sts-ods-sts-ord
                     go to det-sts-ods-900.
      *                      *-----------------------------------------*
      *                      * Se il numero totale di righe e' uguale  *
      *                      * al totale di righe evase : ordine evaso *
      *                      *-----------------------------------------*
           if        w-det-sts-ods-ctr    =    w-det-sts-ods-rev
                     move  03             to   d-sts-ods-sts-ord
                     go to det-sts-ods-900.
      *                      *-----------------------------------------*
      *                      * Se tutti i contatori a zero : ordine    *
      *                      * chiuso                                  *
      *                      *-----------------------------------------*
           if        w-det-sts-ods-rch    =    zero and
                     w-det-sts-ods-rin    =    zero and
                     w-det-sts-ods-rev    =    zero
                     move  01             to   d-sts-ods-sts-ord
                     go to det-sts-ods-900.
      *                      *-----------------------------------------*
      *                      * Se tutti i contatori a non zero: ordine *
      *                      * evaso parzialmente                      *
      *                      *-----------------------------------------*
           if        w-det-sts-ods-rch    >    zero and
                     w-det-sts-ods-rin    >    zero and
                     w-det-sts-ods-rev    >    zero
                     move  02             to   d-sts-ods-sts-ord
                     go to det-sts-ods-900.
      *                      *-----------------------------------------*
      *                      * Se righe chiuse e evase : ordine evaso  *
      *                      *-----------------------------------------*
           if        w-det-sts-ods-rch    >    zero and
                     w-det-sts-ods-rin    =    zero and
                     w-det-sts-ods-rev    >    zero
                     move  03             to   d-sts-ods-sts-ord
                     go to det-sts-ods-900.
      *                      *-----------------------------------------*
      *                      * Se righe chiuse e inevase : ordine      *
      *                      * evaso parzialmente                      *
      *                      *-----------------------------------------*
           if        w-det-sts-ods-rch    >    zero and
                     w-det-sts-ods-rin    >    zero and
                     w-det-sts-ods-rev    =    zero
                     move  02             to   d-sts-ods-sts-ord
                     go to det-sts-ods-900.
      *                      *-----------------------------------------*
      *                      * Se righe evase e inevase : ordine evaso *
      *                      * parzialmente                            *
      *                      *-----------------------------------------*
           if        w-det-sts-ods-rch    =    zero and
                     w-det-sts-ods-rin    >    zero and
                     w-det-sts-ods-rev    >    zero
                     move  02             to   d-sts-ods-sts-ord
                     go to det-sts-ods-900.
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     det-sts-ods-900.
       det-sts-ods-900.
      *              *-------------------------------------------------*
      *              * Se status indeterminato : forzatura a chiuso    *
      *              *-------------------------------------------------*
           if        d-sts-ods-sts-ord    =    zero
                     move  01             to   d-sts-ods-sts-ord      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-sts-ods-999.
       det-sts-ods-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per determinazione quantita' da evadere riga  *
      *    * ordine di spedizione cliente                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/dqdsros0.dts"                   .

