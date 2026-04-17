       Identification Division.
       Program-Id.                                 dstsorc0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    orc                 *
      *                                Settore:                        *
      *                                   Fase:                        *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 10/11/98    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      * ============================================================== *
      *                                                                *
      * Modulo per la determinazione, relativamente ad un ordine cli-  *
      * ente, di :                                                     *
      *                                                                *
      * - Stato dell'ordine ad una data con i valori                   *
      *                                                                *
      *   - 01 : Chiuso                                                *
      *                                                                *
      *   - 02 : Parzialmente evaso                                    *
      *           - Parzialmente consegnato                            *
      *           - In spedizione                                      *
      *           - Parzialmente in spedizione                         *
      *           - In spedizione - Consegnato                         *
      *                                                                *
      *   - 03 : Evaso                                                 *
      *           - In spedizione                                      *
      *                                                                *
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
      *        Input  : d-sts-orc-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-sts-orc-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-sts-orc-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-sts-orc-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "DT" - Determinazione status ordine                            *
      *                                                                *
      *                                                                *
      *        Input  : d-sts-orc-tip-ope = "DT"                       *
      *                                                                *
      *                 d-sts-orc-dat-rif = Data di riferimento        *
      *                                     (facoltativa)              *
      *                                                                *
      *                 Record di [oct]                                *
      *                                                                *
      *                                                                *
      *        Output : d-sts-orc-sts-ord = Status ordine cliente      *
      *                                     - 01 : Chiuso              *
      *                                     - 02 : Evaso parzialmente  *
      *                                     - 03 : Evaso               *
      *                                     - 04 : Inevaso             *
      *                                                                *
      *                 d-sts-orc-sts-lit = Status ordine cliente,     *
      *                                     literal                    *
      *                                                                *
      *                 d-sts-orc-tot-ord = Totale ordine, in valuta   *
      *                                                                *
      *                 d-sts-orc-imp-res = Importo residuo ordine, in *
      *                                     valuta                     *
      *                                                                *
      *                 d-sts-orc-imp-res = Importo residuo ordine, in *
      *                                     valuta                     *
      *                                                                *
      *                 d-sts-orc-sts-ver = Status verifica ordine     *
      *                                     - spaces : Verificato      *
      *                 (DA FINIRE DI       - '#'    : Da verificare   *
      *                     IMPLEMENTARE    - 'P'    : Da verificare   *
      *                     IN AREA 'orc')             parzialmente    *
      *                                     (solo se personalizzazione *
      *                                      lo prevede)               *
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
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/no ordine in attesa di verifica                    *
      *        *-------------------------------------------------------*
           05  w-prs-snx-oav.
      *            *---------------------------------------------------*
      *            * Si/No gestione verifica ordine                    *
      *            *                                                   *
      *            * - N : No                                          *
      *            * - S : Si                                          *
      *            * - P : Parziale ___ DA IMPLEMENTARE ___            *
      *            *---------------------------------------------------*
               10  w-prs-snx-oav-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Default per accettazione (se 'S' precedente)      *
      *            *                                                   *
      *            * - D : Da eseguire                                 *
      *            * - E : Eseguita                                    *
      *            *---------------------------------------------------*
               10  w-prs-snx-oav-def      pic  x(01)                  .

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
           05  w-det-sts-orc.
      *            *---------------------------------------------------*
      *            * Comodo per ridefinizione tipo riga                *
      *            *---------------------------------------------------*
               10  w-det-sts-orc-wtr.
                   15  w-det-sts-orc-wtp  pic  x(01)                  .
                   15  w-det-sts-orc-wtf  pic  x(01)                  .
                   15  filler             pic  x(03)                  .
      *            *---------------------------------------------------*
      *            * Contatore di righe                                *
      *            *---------------------------------------------------*
               10  w-det-sts-orc-ctr      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contatore di righe chiuse                         *
      *            *---------------------------------------------------*
               10  w-det-sts-orc-rch      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Contatore di righe evase                          *
      *            *---------------------------------------------------*
               10  w-det-sts-orc-rev      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Contatore di righe inevase                        *
      *            *---------------------------------------------------*
               10  w-det-sts-orc-rin      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Contatori di righe in parziale consegna           *
      *            *---------------------------------------------------*
               10  w-det-sts-orc-rpc      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Contatori di righe in spedizione                  *
      *            *---------------------------------------------------*
               10  w-det-sts-orc-ris      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Contatori di righe in parziale spedizione         *
      *            *---------------------------------------------------*
               10  w-det-sts-orc-rps      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Contatori di righe in parziale consegna/spediz.   *
      *            *---------------------------------------------------*
               10  w-det-sts-orc-rcs      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Contatori di righe verificate                     *
      *            *---------------------------------------------------*
               10  w-det-sts-orc-rve      pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Importo residuo riga ordine                       *
      *            *---------------------------------------------------*
               10  w-det-sts-orc-irr      pic s9(11)v9(02)            .
      *            *---------------------------------------------------*
      *            * Cumulo righe ordine                               *
      *            *---------------------------------------------------*
               10  w-det-sts-orc-itr      pic s9(11)v9(02)            .
      *            *---------------------------------------------------*
      *            * Comodo per calcoli                                *
      *            *---------------------------------------------------*
               10  w-det-sts-orc-wrk      pic s9(11)v9(03)            .
               10  w-det-sts-orc-qdv      pic s9(06)v9(03)            .
      *            *---------------------------------------------------*
      *            * Max                                               *
      *            *---------------------------------------------------*
               10  w-det-sts-orc-max      pic  9(05)      value 999   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine cliente                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dqevroc0.dtl"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione status ordine    *
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dstsorc0.dtl"                   .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [oct]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfoct"                          .

      ******************************************************************
       Procedure Division                using d-sts-orc
                                               rf-oct                 .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-sts-orc-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           if        d-sts-orc-tip-ope    =    "DT"
                     perform det-sts-orc-000
                                          thru det-sts-orc-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-sts-orc-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-sts-orc-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-sts-orc-tip-ope    =    "C?"
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
           move      zero                 to   d-sts-orc-dat-rif      .
      *              *-------------------------------------------------*
      *              * Incremento contatore Open modulo                *
      *              *-------------------------------------------------*
           add       1                    to   w-nol-ctr-opn          .
       rou-opn-fls-100.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/no ordine in attesa di verifica          *
      *                  *---------------------------------------------*
           perform   prs-snx-oav-000      thru prs-snx-oav-999        .
       rou-opn-fls-200.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [ocr]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Open modulo di determinazione quantita' da  *
      *                  * evadere riga ordine                         *
      *                  *---------------------------------------------*
           perform   det-qev-roc-opn-000  thru det-qev-roc-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/no ordine in attesa di     *
      *    *                             verifica                      *
      *    *-----------------------------------------------------------*
       prs-snx-oav-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/orc/mov/orc300[snx-oav]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-oav
           else      move  spaces         to   w-prs-snx-oav          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-oav-snx    not   = "S"
                     move  "N"            to   w-prs-snx-oav-snx      .
           if        w-prs-snx-oav-def    not   = "E"
                     move  "D"            to   w-prs-snx-oav-def      .
           if        w-prs-snx-oav-snx    not   = "S"
                     move  "D"            to   w-prs-snx-oav-def      .
       prs-snx-oav-999.
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
      *                  * [ocr]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Chiusura modulo di determinazione quantita' *
      *                  * da evadere riga ordine                      *
      *                  *---------------------------------------------*
           perform   det-qev-roc-cls-000  thru det-qev-roc-cls-999    .
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
                     move  spaces         to   d-sts-orc-exi-sts
           else      move  "#"            to   d-sts-orc-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione status ordine cliente                      *
      *    *-----------------------------------------------------------*
       det-sts-orc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di output                *
      *              *-------------------------------------------------*
           move      zero                 to   d-sts-orc-sts-ord      .
           move      spaces               to   d-sts-orc-sts-lit      .
           move      zero                 to   d-sts-orc-tot-ord      .
           move      zero                 to   d-sts-orc-imp-res      .
           move      spaces               to   d-sts-orc-sts-ver      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatori righe                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-sts-orc-ctr      .
           move      zero                 to   w-det-sts-orc-rch      .
           move      zero                 to   w-det-sts-orc-rev      .
           move      zero                 to   w-det-sts-orc-rin      .
           move      zero                 to   w-det-sts-orc-rpc      .
           move      zero                 to   w-det-sts-orc-ris      .
           move      zero                 to   w-det-sts-orc-rps      .
           move      zero                 to   w-det-sts-orc-rcs      .
           move      zero                 to   w-det-sts-orc-rve      .
      *              *-------------------------------------------------*
      *              * Normalizzazione comodo per cumulo importi righe *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-sts-orc-itr      .
       det-sts-orc-010.
      *              *-------------------------------------------------*
      *              * Totale ordine in valuta : quello contenuto nel  *
      *              * record [oct]                                    *
      *              *-------------------------------------------------*
           move      rf-oct-tot-doc       to   d-sts-orc-tot-ord      .
       det-sts-orc-020.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di ordine chiuso               *
      *                  *---------------------------------------------*
           if        rf-oct-flg-och       not  = spaces
                     move  01             to   d-sts-orc-sts-ord
                     move  "Chiuso                        "
                                          to   d-sts-orc-sts-lit
                     go to det-sts-orc-900.
      *                  *---------------------------------------------*
      *                  * Test su tipo di ordine                      *
      *                  *---------------------------------------------*
           if        rf-oct-tip-ord       =    "P"
                     move  04             to   d-sts-orc-sts-ord
                     move  "Pro-forma                     "
                                          to   d-sts-orc-sts-lit
                     go to det-sts-orc-900.
           if        rf-oct-tip-ord       =    "C"
                     move  04             to   d-sts-orc-sts-ord
                     move  "Contratto                     "
                                          to   d-sts-orc-sts-lit
                     go to det-sts-orc-900.
       det-sts-orc-100.
      *              *-------------------------------------------------*
      *              * Start su file [ocr] : righe ordini              *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-oct-num-prt       to   rf-ocr-num-prt         .
           move      zero                 to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Esito della start                           *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to det-sts-orc-200.
      *                  *---------------------------------------------*
      *                  * Ordine chiuso                               *
      *                  *---------------------------------------------*
           move      01                   to   d-sts-orc-sts-ord      .
           move      "Chiuso                        "
                                          to   d-sts-orc-sts-lit      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-sts-orc-900.
       det-sts-orc-200.
      *              *-------------------------------------------------*
      *              * Read-next su file [ocr]                         *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Esito della read-next                       *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to det-sts-orc-300.
      *                  *---------------------------------------------*
      *                  * A test sul contatore righe ordine           *
      *                  *---------------------------------------------*
           go to     det-sts-orc-800.
       det-sts-orc-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo : a test sul contatore  *
      *                  *---------------------------------------------*
           if        rf-ocr-num-prt       not  = rf-oct-num-prt
                     go to det-sts-orc-800.
       det-sts-orc-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ridefinizione tipo riga                     *
      *                  *---------------------------------------------*
           move      rf-ocr-tip-rig       to   w-det-sts-orc-wtr      .
      *                  *---------------------------------------------*
      *                  * Test se riga di Commento                    *
      *                  *---------------------------------------------*
           if        w-det-sts-orc-wtp    =    "C"
                     go to det-sts-orc-200.
       det-sts-orc-500.
      *              *-------------------------------------------------*
      *              * Incremento del contatore di righe ordine        *
      *              *-------------------------------------------------*
           add       1                    to   w-det-sts-orc-ctr      .
           if        w-det-sts-orc-ctr    >    w-det-sts-orc-max
                     go to det-sts-orc-800.
       det-sts-orc-540.
      *              *-------------------------------------------------*
      *              * Se la quantita' in riga ordine e' a zero, si    *
      *              * considera comunque evasa                        *
      *              *-------------------------------------------------*
           if        rf-ocr-qta-ord       =    zero
                     move  zero           to   d-qev-roc-qta-dev
                     go to det-sts-orc-550.
      *              *-------------------------------------------------*
      *              * Determinazione quantita' evasa riga ordine      *
      *              *-------------------------------------------------*
           move      "DT"                 to   d-qev-roc-tip-ope      .
           move      d-sts-orc-dat-rif    to   d-qev-roc-dat-rif      .
           perform   det-qev-roc-cll-000  thru det-qev-roc-cll-999    .
       det-sts-orc-550.
      *              *-------------------------------------------------*
      *              * Aggiornamento importo residuo                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se la riga in esame non e' 'Inevasa' : la   *
      *                  * si ignora                                   *
      *                  *---------------------------------------------*
           if        d-qev-roc-qta-dev    not  > zero
                     go to det-sts-orc-600.
       det-sts-orc-560.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del tipo riga          *
      *                  *---------------------------------------------*
           if        w-det-sts-orc-wtp    =    "P"
                     go to det-sts-orc-565
           else if   w-det-sts-orc-wtp    =    "A"
                     go to det-sts-orc-570
           else      go to det-sts-orc-600.
       det-sts-orc-565.
      *                  *---------------------------------------------*
      *                  * Se tipo riga : 'P'                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se prezzo per quantita'            *
      *                      *-----------------------------------------*
           if        rf-ocr-flg-puq       not  = "K"
                     go to det-sts-orc-567.
      *                      *-----------------------------------------*
      *                      * Divisione                               *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-sts-orc-irr      .
           divide    1000                 into d-qev-roc-qta-dev
                                        giving w-det-sts-orc-qdv      .
           multiply  w-det-sts-orc-qdv    by   rf-ocr-prz-net
                                        giving w-det-sts-orc-irr      .
      *                      *-----------------------------------------*
      *                      * Eventuale aggiustamento con decimali    *
      *                      * prezzo                                  *
      *                      *-----------------------------------------*
           if        rf-ocr-dec-prz       =    1
                     divide 10            into w-det-sts-orc-irr
           else if   rf-ocr-dec-prz       =    2
                     divide 100           into w-det-sts-orc-irr      .
      *                      *-----------------------------------------*
      *                      * Cumulo                                  *
      *                      *-----------------------------------------*
           add       w-det-sts-orc-irr    to   d-sts-orc-imp-res      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     det-sts-orc-600.
       det-sts-orc-567.
      *                      *-----------------------------------------*
      *                      * Calcolo dell'importo residuo per la     *
      *                      * riga ordine                             *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-sts-orc-irr      .
           multiply  d-qev-roc-qta-dev    by   rf-ocr-prz-net
                                        giving w-det-sts-orc-irr      .
      *                      *-----------------------------------------*
      *                      * Eventuale aggiustamento con decimali    *
      *                      * prezzo                                  *
      *                      *-----------------------------------------*
           if        rf-ocr-dec-prz       =    1
                     divide 10            into w-det-sts-orc-irr
           else if   rf-ocr-dec-prz       =    2
                     divide 100           into w-det-sts-orc-irr      .
      *                      *-----------------------------------------*
      *                      * Cumulo                                  *
      *                      *-----------------------------------------*
           add       w-det-sts-orc-irr    to   d-sts-orc-imp-res      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     det-sts-orc-600.
       det-sts-orc-570.
      *                  *---------------------------------------------*
      *                  * Se tipo riga : 'A'                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Somma dell'importo in valuta della riga *
      *                      * nel totale residuo                      *
      *                      *-----------------------------------------*
           add       rf-ocr-imp-rig       to   d-sts-orc-imp-res      .
      *                      *-----------------------------------------*
      *                      * Status relativo alla riga di addebito   *
      *                      *-----------------------------------------*
           if        rf-ocr-flg-rch       not  = spaces or
                     rf-ocr-sdr-ccs       not  = spaces
                     add   1              to   w-det-sts-orc-rch
                     go to det-sts-orc-580.
           if        d-qev-roc-sts-rda    =    01
                     add   1              to   w-det-sts-orc-rin
                     add   rf-ocr-imp-rig to   d-sts-orc-imp-res
           else if   d-qev-roc-sts-rda    =    02
                     add   1              to   w-det-sts-orc-ris
           else if   d-qev-roc-sts-rda    =    03
                     add   1              to   w-det-sts-orc-rev
           else      add   1              to   w-det-sts-orc-rch      .
       det-sts-orc-580.
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     det-sts-orc-700.
       det-sts-orc-600.
      *              *-------------------------------------------------*
      *              * Incremento contatori relativi allo status riga  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Riga chiusa                                 *
      *                  *---------------------------------------------*
           if        rf-ocr-flg-rch       not  = spaces or
                     rf-ocr-sdr-ccs       not  = spaces
                     add   1              to   w-det-sts-orc-rch
                     go to det-sts-orc-700.
           if        d-qev-roc-qta-ord    =    zero
                     add   1              to   w-det-sts-orc-rch
                     go to det-sts-orc-700.
      *                  *---------------------------------------------*
      *                  * Riga evasa, cioe' tutta consegnata          *
      *                  *---------------------------------------------*
           if        d-qev-roc-qta-ord    not  > d-qev-roc-qta-gsp
                     add   1              to   w-det-sts-orc-rev
                     go to det-sts-orc-700.
      *                  *---------------------------------------------*
      *                  * Riga inevasa, cioe' senza spedizioni ne     *
      *                  * consegne                                    *
      *                  *---------------------------------------------*
           if        d-qev-roc-qta-ics    =    zero and
                     d-qev-roc-qta-gsp    =    zero
                     add   1              to   w-det-sts-orc-rin
                     go to det-sts-orc-700.
      *                  *---------------------------------------------*
      *                  * Riga evasa parzialmente, tutta in spedizio- *
      *                  * ne                                          *
      *                  *---------------------------------------------*
           if        d-qev-roc-qta-ord    not  > d-qev-roc-qta-ics and
                     d-qev-roc-qta-gsp    =    zero
                     add   1              to   w-det-sts-orc-ris
                     go to det-sts-orc-700.
      *                  *---------------------------------------------*
      *                  * Riga evasa parzialmente, senza spedizioni   *
      *                  * cioe' parziale consegna                     *
      *                  *---------------------------------------------*
           if        d-qev-roc-qta-ord    >    d-qev-roc-qta-gsp and
                     d-qev-roc-qta-ics    =    zero
                     add   1              to   w-det-sts-orc-rpc
                     go to det-sts-orc-700.
      *                  *---------------------------------------------*
      *                  * Riga evasa parzialmente, senza consegne     *
      *                  * cioe' parziale spedizione                   *
      *                  *---------------------------------------------*
           if        d-qev-roc-qta-ord    >    d-qev-roc-qta-ics and
                     d-qev-roc-qta-gsp    =    zero
                     add   1              to   w-det-sts-orc-rps
                     go to det-sts-orc-700.
      *                  *---------------------------------------------*
      *                  * Riga evasa parzialmente, con consegne       *
      *                  * e in spedizione                             *
      *                  *---------------------------------------------*
           move      d-qev-roc-qta-gsp    to   w-det-sts-orc-wrk      .
           add       d-qev-roc-qta-ics    to   w-det-sts-orc-wrk      .
      *
           if        d-qev-roc-qta-ord    >    w-det-sts-orc-wrk
                     add   1              to   w-det-sts-orc-rcs
           else if   d-qev-roc-qta-ord    =    w-det-sts-orc-wrk
                     add   1              to   w-det-sts-orc-rcs
                     go to det-sts-orc-700.
       det-sts-orc-700.
      *              *-------------------------------------------------*
      *              * Incremento totale righe                         *
      *              *-------------------------------------------------*
           add       rf-ocr-imp-rig       to   w-det-sts-orc-itr      .
       det-sts-orc-720.
      *              *-------------------------------------------------*
      *              * Status verifica ordine, se previsto dalle per-  *
      *              * sonalizzazioni                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su valore personalizzazione            *
      *                  *---------------------------------------------*
           if        w-prs-snx-oav-snx    =    "N"
                     go to det-sts-orc-750.
      *                  *---------------------------------------------*
      *                  * Test in funzione del flag specifico         *
      *                  *---------------------------------------------*
           if        rf-ocr-flg-nbx (1)   =    spaces
                     add  1               to   w-det-sts-orc-rve      .
       det-sts-orc-750.
      *              *-------------------------------------------------*
      *              * Riciclo a riga ordine successiva                *
      *              *-------------------------------------------------*
           go to     det-sts-orc-200.
       det-sts-orc-800.
      *              *-------------------------------------------------*
      *              * Test sul contatore righe di evasione lette      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero di righe  *
      *                  * trovate                                     *
      *                  *---------------------------------------------*
           if        w-det-sts-orc-ctr    =    zero
                     go to det-sts-orc-810
           else if   w-det-sts-orc-ctr    =    1
                     go to det-sts-orc-820
           else if   w-det-sts-orc-ctr    >    1
                     go to det-sts-orc-830.
       det-sts-orc-810.
      *                  *---------------------------------------------*
      *                  * Se nessuna riga trovata status documento    *
      *                  * a 'chiuso'                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Forzatura status                        *
      *                      *-----------------------------------------*
           move      01                   to   d-sts-orc-sts-ord      .
           move      "Chiuso                        "
                                          to   d-sts-orc-sts-lit      .
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     det-sts-orc-900.
       det-sts-orc-820.
      *                  *---------------------------------------------*
      *                  * Se 1 riga trovata                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione status                   *
      *                      *-----------------------------------------*
           if        w-det-sts-orc-rch    >    zero
                     move  01             to   d-sts-orc-sts-ord
                     move  "Chiuso                        "
                                          to   d-sts-orc-sts-lit
           else if   w-det-sts-orc-rev    >    zero
                     move  03             to   d-sts-orc-sts-ord
                     move  "Evaso                         "
                                          to   d-sts-orc-sts-lit
           else if   w-det-sts-orc-rin    >    zero
                     move  04             to   d-sts-orc-sts-ord
                     move  "Inevaso                       "
                                          to   d-sts-orc-sts-lit
           else      move  02             to   d-sts-orc-sts-ord      .
      *                      *-----------------------------------------*
      *                      * Determinazione literal status per       *
      *                      * evasione parziale                       *
      *                      *-----------------------------------------*
           if        d-sts-orc-sts-ord    not  = 02
                     go to det-sts-orc-825.
      *                      *-----------------------------------------*
      *                      * Determinazione                          *
      *                      *-----------------------------------------*
           if        w-det-sts-orc-ris    >    zero
                     move  "In spedizione                 "
                                          to   d-sts-orc-sts-lit
           else if   w-det-sts-orc-rpc    >    zero
                     move  "Parzialmente consegnato       "
                                          to   d-sts-orc-sts-lit
           else if   w-det-sts-orc-rps    >    zero
                     move  "Parzialmente in spedizione    "
                                          to   d-sts-orc-sts-lit
           else      move  "In spedizione - Consegnato    "
                                          to   d-sts-orc-sts-lit      .
       det-sts-orc-825.
      *                      *-----------------------------------------*
      *                      * Determinazione status verifica          *
      *                      *-----------------------------------------*
           if        w-prs-snx-oav-snx    =    "N"
                     go to det-sts-orc-828.
           if        w-det-sts-orc-rve    =    zero
                     move  "#"            to   d-sts-orc-sts-ver      .
       det-sts-orc-828.
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     det-sts-orc-900.
       det-sts-orc-830.
      *                  *---------------------------------------------*
      *                  * Se piu' righe trovate                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il numero totale di righe e' uguale  *
      *                      * al totale di righe chiuse : ordine      *
      *                      * chiuso                                  *
      *                      *-----------------------------------------*
           if        w-det-sts-orc-ctr    =    w-det-sts-orc-rch
                     move  01             to   d-sts-orc-sts-ord
                     move  "Chiuso                        "
                                          to   d-sts-orc-sts-lit
                     go to det-sts-orc-900.
      *                      *-----------------------------------------*
      *                      * Se il numero totale di righe e' uguale  *
      *                      * al totale di righe inevase : ordine     *
      *                      * inevaso                                 *
      *                      *-----------------------------------------*
           if        w-det-sts-orc-ctr    =    w-det-sts-orc-rin
                     move  04             to   d-sts-orc-sts-ord
                     move  "Inevaso                       "
                                          to   d-sts-orc-sts-lit
                     go to det-sts-orc-900.
      *                      *-----------------------------------------*
      *                      * Se il numero totale di righe e' uguale  *
      *                      * al totale di righe evase : ordine evaso *
      *                      *-----------------------------------------*
           if        w-det-sts-orc-ctr    =    w-det-sts-orc-rev
                     move  03             to   d-sts-orc-sts-ord
                     move  "Evaso                         "
                                          to   d-sts-orc-sts-lit
                     go to det-sts-orc-900.
      *                      *-----------------------------------------*
      *                      * Se il numero totale di righe e' uguale  *
      *                      * al totale di righe evase piu' quelle    *
      *                      * chiuse : ordine evaso                   *
      *                      *-----------------------------------------*
           move      w-det-sts-orc-rev    to   w-det-sts-orc-wrk      .
           add       w-det-sts-orc-rch    to   w-det-sts-orc-wrk      .
           if        w-det-sts-orc-ctr    =    w-det-sts-orc-wrk
                     move  03             to   d-sts-orc-sts-ord
                     move  "Evaso                         "
                                          to   d-sts-orc-sts-lit
                     go to det-sts-orc-900.
      *                      *-----------------------------------------*
      *                      * Se il numero totale di righe e' uguale  *
      *                      * al totale di righe spedite : ordine     *
      *                      * spedito                                 *
      *                      *-----------------------------------------*
           if        w-det-sts-orc-ctr    =    w-det-sts-orc-ris
                     move  03             to   d-sts-orc-sts-ord
                     move  "Evaso - in spedizione         "
                                          to   d-sts-orc-sts-lit
                     go to det-sts-orc-900.
      *                      *-----------------------------------------*
      *                      * Se tutti i contatori a zero : ordine    *
      *                      * chiuso                                  *
      *                      *-----------------------------------------*
           if        w-det-sts-orc-rch    =    zero  and
                     w-det-sts-orc-rin    =    zero  and
                     w-det-sts-orc-rev    =    zero  and
                     w-det-sts-orc-rps    =    zero  and
                     w-det-sts-orc-rpc    =    zero  and
                     w-det-sts-orc-rcs    =    zero  and
                     w-det-sts-orc-ris    =    zero 
                     move  01             to   d-sts-orc-sts-ord
                     move  "Chiuso                        "
                                          to   d-sts-orc-sts-lit
                     go to det-sts-orc-900.
      *                      *-----------------------------------------*
      *                      * In tutti gli altri casi si pone lo sta- *
      *                      * tus a parzialmente evaso e si differen- *
      *                      * zia solamente il literal                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Forzatura status                    *
      *                          *-------------------------------------*
           move      02                   to   d-sts-orc-sts-ord      .
      *                          *-------------------------------------*
      *                          * Test se tutto in spedizione         *
      *                          *-------------------------------------*
           if        w-det-sts-orc-ctr    =    w-det-sts-orc-ris
                     move  "In spedizione                 "
                                          to   d-sts-orc-sts-lit
                     go to det-sts-orc-900.
      *                          *-------------------------------------*
      *                          * Parzialmente consegnato             *
      *                          *-------------------------------------*
           if        w-det-sts-orc-rpc    >    zero  and
                     w-det-sts-orc-rps    =    zero  and
                     w-det-sts-orc-rcs    =    zero  and
                     w-det-sts-orc-ris    =    zero
                     move  "Parzialmente consegnato       "
                                          to   d-sts-orc-sts-lit
                     go to det-sts-orc-900.
      *                          *-------------------------------------*
      *                          * Parzialmente in spedizione - 1      *
      *                          *-------------------------------------*
           if        w-det-sts-orc-rps    >    zero  and
                     w-det-sts-orc-rpc    =    zero  and
                     w-det-sts-orc-rcs    =    zero  and
                     w-det-sts-orc-ris    =    zero 
                     move  "Parzialmente in spedizione    "
                                          to   d-sts-orc-sts-lit
                     go to det-sts-orc-900.
      *                          *-------------------------------------*
      *                          * Parzialmente in spedizione - 2      *
      *                          *-------------------------------------*
           if        w-det-sts-orc-ris    >    zero  and
                     w-det-sts-orc-rpc    =    zero  and
                     w-det-sts-orc-rcs    =    zero  and
                     w-det-sts-orc-rps    =    zero 
                     move  "Parzialmente in spedizione    "
                                          to   d-sts-orc-sts-lit
                     go to det-sts-orc-900.
      *                          *-------------------------------------*
      *                          * Parzialmente in spedizione - 3      *
      *                          *-------------------------------------*
           if        w-det-sts-orc-ris    >    zero  and
                     w-det-sts-orc-rps    >    zero  and
                     w-det-sts-orc-rcs    =    zero  and
                     w-det-sts-orc-rpc    =    zero 
                     move  "Parzialmente in spedizione    "
                                          to   d-sts-orc-sts-lit
                     go to det-sts-orc-900.
      *                          *-------------------------------------*
      *                          * In spedizione - Consegnato          *
      *                          *-------------------------------------*
           if        w-det-sts-orc-rps    >    zero  or
                     w-det-sts-orc-rpc    >    zero  or
                     w-det-sts-orc-rcs    >    zero  or
                     w-det-sts-orc-ris    >    zero
                     move  "In spedizione - Consegnato    "
                                          to   d-sts-orc-sts-lit
                     go to det-sts-orc-900.
      *                          *-------------------------------------*
      *                          * Parzialmente consegnato : in caso   *
      *                          * di alcune righe evase e alcune no   *
      *                          *-------------------------------------*
           move      w-det-sts-orc-ctr    to   w-det-sts-orc-wrk      .
           subtract  w-det-sts-orc-rev    from w-det-sts-orc-wrk      .
           if        w-det-sts-orc-wrk    =    w-det-sts-orc-rin
                     move  "Parzialmente consegnato       "
                                          to   d-sts-orc-sts-lit
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     det-sts-orc-900.
       det-sts-orc-900.
      *              *-------------------------------------------------*
      *              * Determinazione status verifica                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su personalizzazione                   *
      *                  *---------------------------------------------*
           if        w-prs-snx-oav-snx    =    "N"
                     go to det-sts-orc-950.
      *                  *---------------------------------------------*
      *                  * Se tutte le righe verificate                *
      *                  *---------------------------------------------*
           if        w-det-sts-orc-ctr    =    w-det-sts-orc-rve
                     go to det-sts-orc-950.
      *                  *---------------------------------------------*
      *                  * Se tutte le righe da verificare             *
      *                  *---------------------------------------------*
           if        w-det-sts-orc-rve    =    zero
                     move  "#"            to   d-sts-orc-sts-ver
                     go to det-sts-orc-950.
      *                  *---------------------------------------------*
      *                  * Se righe parzialmente verificate            *
      *                  *---------------------------------------------*
           if        w-det-sts-orc-ctr    >    w-det-sts-orc-rve
                     move  "P"            to   d-sts-orc-sts-ver
                     go to det-sts-orc-950.
       det-sts-orc-950.
      *              *-------------------------------------------------*
      *              * Se status indeterminato : forzatura a chiuso    *
      *              *-------------------------------------------------*
           if        d-sts-orc-sts-ord    =    zero
                     move  01             to   d-sts-orc-sts-ord
                     move  "Chiuso                        "
                                          to   d-sts-orc-sts-lit      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-sts-orc-999.
       det-sts-orc-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per determinazione quantita' da evadere riga  *
      *    * ordine cliente                                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dqevroc0.dts"                   .

