       Identification Division.
       Program-Id.                                 dqevroc0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    orc                 *
      *                                Settore:    mov                 *
      *                                   Fase:    dqevroc0            *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 25/07/92    *
      *                       Ultima revisione:    NdK del 07/06/21    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Modulo per la determinazione, relativamente ad una riga ordine *
      * cliente, di :                                                  *
      *                                                                *
      * - Quantita' in ordine                                          *
      * - Quantita' gia' spedita                                       *
      * - Quantita' in corso di spedizione                             *
      *                                                                *
      * e, di conseguenza, di :                                        *
      *                                                                *
      * - Quantita' ancora da evadere                                  *
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
      *        Input  : d-qev-roc-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-qev-roc-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-qev-roc-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-qev-roc-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "DT" - Determinazione status riga ordine                       *
      *                                                                *
      *                                                                *
      *        Input  : d-qev-roc-tip-ope = "DT"                       *
      *                                                                *
      *                 d-qev-roc-dat-rif = Data di riferimento        *
      *                                    (facoltativa)               *
      *                                                                *
      *                 Record di [ocr] relativo alla riga ordine      *
      *                 cliente a disposizione in rf-ocr               *
      *                                                                *
      *                                                                *
      *        Output : d-qev-roc-qta-ord = Quantita' in ordine        *
      *                                                                *
      *                 d-qev-roc-qta-gsp = Quantita' gia' spedita     *
      *                                                                *
      *                 d-qev-roc-qta-ics = Quantita' in corso di      *
      *                                     spedizione                 *
      *                                                                *
      *                 d-qev-roc-qta-dev = Quantita' ancora da eva-   *
      *                                     dere                       *
      *                                                                *
      *                 d-qev-roc-tip-uev = Tipo movimento dell'ultima *
      *                                     evasione                   *
      *                                     - 01 : Bolla di accomp.    *
      *                                     - 02 : Ordine di spediz.   *
      *                                                                *
      *                 d-qev-roc-tdo-uev = Tipo documento dell'ultima *
      *                                     evasione                   *
      *                                                                *
      *                 d-qev-roc-ddo-uev = Data documento dell'ultima *
      *                                     evasione                   *
      *                                                                *
      *                 d-qev-roc-ndo-uev = Num. documento dell'ultima *
      *                                     evasione                   *
      *                                                                *
      *                 d-qev-roc-sts-rda = Status se riga di addebito *
      *                                     - 01 : Inevasa             *
      *                                     - 02 : In Spedizione       *
      *                                     - 03 : Evasa               *
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
      *        *-------------------------------------------------------*
      *        * [osr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosr"                          .
      *        *-------------------------------------------------------*
      *        * [ocf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocf"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/No gestione ordini di spedizione attiva            *
      *        *-------------------------------------------------------*
           05  w-prs-ods-snx              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/no gestione ordini clienti 'forecast'              *
      *        *-------------------------------------------------------*
           05  w-prs-snx-ocf              pic  x(01)                  .

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
      *    * Work-area per ridefinizione tipo riga                     *
      *    *-----------------------------------------------------------*
       01  w-rdf-tip-rig.
           05  w-rdf-tip-rig-wtr.
               10  w-rdf-tip-rig-wtp      pic  x(01)                  .
               10  w-rdf-tip-rig-wtf      pic  x(01)                  .
               10  filler                 pic  x(03)                  .

      *    *===========================================================*
      *    * Work-area per la determinazione, relativamente ad una ri- *
      *    * ga ordine cliente, di :                                   *
      *    *                                                           *
      *    *    - Quantita' in ordine                                  *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       01  w-roc-ord.
      *        *-------------------------------------------------------*
      *        * Parametri in Output                                   *
      *        *-------------------------------------------------------*
           05  w-roc-ord-pxo.
      *            *---------------------------------------------------*
      *            * Quantita' in ordine                               *
      *            *---------------------------------------------------*
               10  w-roc-ord-qta-ord      pic s9(08)v9(03)            .

      *    *===========================================================*
      *    * Work-area per la determinazione, relativamente ad una ri- *
      *    * ga ordine cliente, di :                                   *
      *    *                                                           *
      *    *    - Quantita' gia' spedita                               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       01  w-roc-gsp.
      *        *-------------------------------------------------------*
      *        * Parametri in Output                                   *
      *        *-------------------------------------------------------*
           05  w-roc-gsp-pxo.
      *            *---------------------------------------------------*
      *            * Quantita' gia' spedita                            *
      *            *---------------------------------------------------*
               10  w-roc-gsp-qta-gsp      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Flag per riga di addebito                         *
      *            *---------------------------------------------------*
               10  w-roc-gsp-flg-rda      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Estremi ultima spedizione                         *
      *            *---------------------------------------------------*
               10  w-roc-gsp-tip-doc      pic  x(05)                  .
               10  w-roc-gsp-dat-doc      pic  9(07)                  .
               10  w-roc-gsp-num-doc      pic  9(11)                  .

      *    *===========================================================*
      *    * Work-area per la determinazione, relativamente ad una ri- *
      *    * ga ordine cliente, di :                                   *
      *    *                                                           *
      *    *    - Quantita' in corso di spedizione                     *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       01  w-roc-ics.
      *        *-------------------------------------------------------*
      *        * Parametri in Output                                   *
      *        *-------------------------------------------------------*
           05  w-roc-ics-pxo.
      *            *---------------------------------------------------*
      *            * Quantita' in corso di spedizione                  *
      *            *---------------------------------------------------*
               10  w-roc-ics-qta-ics      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Flag per riga di addebito                         *
      *            *---------------------------------------------------*
               10  w-roc-ics-flg-rda      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Estremi ultima spedizione in corso                *
      *            *---------------------------------------------------*
               10  w-roc-ics-tip-doc      pic  x(05)                  .
               10  w-roc-ics-dat-doc      pic  9(07)                  .
               10  w-roc-ics-num-doc      pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work-area locale                                      *
      *        *-------------------------------------------------------*
           05  w-roc-ics-war.
      *            *---------------------------------------------------*
      *            * Quantita' in corso di spedizione da [osr]         *
      *            *---------------------------------------------------*
               10  w-roc-ics-wqt-osr      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Quantita' spedita a fronte spedizioni da [bir]    *
      *            *---------------------------------------------------*
               10  w-roc-ics-wqt-bir      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Quantita' in corso di spedizione effettivamente   *
      *            * residua, considerando [osr] e [bir]               *
      *            *---------------------------------------------------*
               10  w-roc-ics-wqt-ics      pic s9(08)v9(03)            .

      *    *===========================================================*
      *    * Work-area per la determinazione, relativamente ad una ri- *
      *    * ga ordine 'forecast', di :                                *
      *    *                                                           *
      *    *    - Quantita' evasa                                      *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       01  w-roc-frc.
      *        *-------------------------------------------------------*
      *        * Parametri in Output                                   *
      *        *-------------------------------------------------------*
           05  w-roc-frc-pxo.
      *            *---------------------------------------------------*
      *            * Quantita' gia' spedita                            *
      *            *---------------------------------------------------*
               10  w-roc-frc-qta-eva      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Flag per riga di addebito                         *
      *            *---------------------------------------------------*
               10  w-roc-frc-flg-rda      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Estremi ultima spedizione                         *
      *            *---------------------------------------------------*
               10  w-roc-frc-tip-doc      pic  x(05)                  .
               10  w-roc-frc-dat-doc      pic  9(07)                  .
               10  w-roc-frc-num-doc      pic  9(11)                  .

      *    *===========================================================*
      *    * Work-area per la selezione sul record [bir] relativamente *
      *    * al record di [ocr], per compatibilita'                    *
      *    *-----------------------------------------------------------*
       01  w-sel-bir-ocr.
      *        *-------------------------------------------------------*
      *        * Parametri in Output                                   *
      *        *-------------------------------------------------------*
           05  w-sel-bir-ocr-pxo.
      *            *---------------------------------------------------*
      *            * Flag su esito della selezione                     *
      *            *  - Spaces : Selezione superata                    *
      *            *  - #      : Selezione non superata                *
      *            *---------------------------------------------------*
               10  w-sel-bir-ocr-flg      pic  x(01)                  .

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

      *    *===========================================================*
      *    * Work-area per la selezione sul record [osr] relativamente *
      *    * al record di [ocr], per compatibilita'                    *
      *    *-----------------------------------------------------------*
       01  w-sel-osr-ocr.
      *        *-------------------------------------------------------*
      *        * Parametri in Output                                   *
      *        *-------------------------------------------------------*
           05  w-sel-osr-ocr-pxo.
      *            *---------------------------------------------------*
      *            * Flag su esito della selezione                     *
      *            *  - Spaces : Selezione superata                    *
      *            *  - #      : Selezione non superata                *
      *            *---------------------------------------------------*
               10  w-sel-osr-ocr-flg      pic  x(01)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine cliente                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dqevroc0.dtl"                   .

      *    *-----------------------------------------------------------*
      *    * [ocr]                                                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .

      ******************************************************************
       Procedure Division                using d-qev-roc
                                               rf-ocr                 .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-qev-roc-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           if        d-qev-roc-tip-ope    =    "DT"
                     perform det-qev-roc-000
                                          thru det-qev-roc-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-qev-roc-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-qev-roc-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-qev-roc-tip-ope    =    "C?"
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
           move      zero                 to   d-qev-roc-dat-rif      .
      *              *-------------------------------------------------*
      *              * Incremento contatore Open modulo                *
      *              *-------------------------------------------------*
           add       1                    to   w-nol-ctr-opn          .
       rou-opn-fls-100.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No gestione ordini di spedizione attiva  *
      *                  *---------------------------------------------*
           perform   prs-ods-snx-000      thru prs-ods-snx-999        .
      *                  *---------------------------------------------*
      *                  * Si/No gestione ordini 'forecast'            *
      *                  *---------------------------------------------*
           perform   prs-snx-ocf-000      thru prs-snx-ocf-999        .
       rou-opn-fls-200.
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
      *                  *---------------------------------------------*
      *                  * [osr]                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se gestione spedizioni attiva      *
      *                      *-----------------------------------------*
           if        w-prs-ods-snx        not  = "S"
                     go to rou-opn-fls-300.
      *                      *-----------------------------------------*
      *                      * Apertura file                           *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
       rou-opn-fls-300.
      *                  *---------------------------------------------*
      *                  * [ocf]                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su personalizzazione               *
      *                      *-----------------------------------------*
           if        w-prs-snx-ocf        not  = "S"
                     go to rou-opn-fls-999.
      *                      *-----------------------------------------*
      *                      * Apertura file                           *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocf                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No gestione ordini di spe- *
      *    *                             dizione attiva                *
      *    *-----------------------------------------------------------*
       prs-ods-snx-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/ods[snx]"       to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-ods-snx
           else      move  spaces         to   w-prs-ods-snx          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-ods-snx        =    "S" or
                     w-prs-ods-snx        =    "N"
                     go to prs-ods-snx-999.
           move      "N"                  to   w-prs-ods-snx          .
       prs-ods-snx-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No ordini 'forecast'       *
      *    *-----------------------------------------------------------*
       prs-snx-ocf-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/orc[snx-ocf]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-ocf
           else      move  spaces         to   w-prs-snx-ocf          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-ocf        not  = "S" and
                     w-prs-snx-ocf        not  = "N"
                     move  "N"            to   w-prs-snx-ocf          .
       prs-snx-ocf-999.
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
      *                  *---------------------------------------------*
      *                  * [osr]                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se gestione spedizioni attiva      *
      *                      *-----------------------------------------*
           if        w-prs-ods-snx        not  = "S"
                     go to rou-cls-fls-300.
      *                      *-----------------------------------------*
      *                      * Chiusura file                           *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
       rou-cls-fls-300.
      *                  *---------------------------------------------*
      *                  * [ocf]                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su personalizzazione               *
      *                      *-----------------------------------------*
           if        w-prs-snx-ocf        not  = "S"
                     go to rou-cls-fls-999.
      *                      *-----------------------------------------*
      *                      * Apertura file                           *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocf                 .
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
                     move  spaces         to   d-qev-roc-exi-sts
           else      move  "#"            to   d-qev-roc-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione status riga ordine                         *
      *    *-----------------------------------------------------------*
       det-qev-roc-000.
      *              *-------------------------------------------------*
      *              * Tipo riga in comodo ridefinito                  *
      *              *-------------------------------------------------*
           move      rf-ocr-tip-rig       to   w-rdf-tip-rig-wtr      .
      *              *-------------------------------------------------*
      *              * Normalizzazione estremi ultima evasione         *
      *              *-------------------------------------------------*
           move      zero                 to   d-qev-roc-tip-uev      .
           move      spaces               to   d-qev-roc-tdo-uev      .
           move      zero                 to   d-qev-roc-ddo-uev      .
           move      zero                 to   d-qev-roc-ndo-uev      .
       det-qev-roc-100.
      *              *-------------------------------------------------*
      *              * Determinazione della quantita' in ordine        *
      *              *-------------------------------------------------*
           perform   rpd-roc-ord-000      thru rpd-roc-ord-999        .
           move      w-roc-ord-qta-ord    to   d-qev-roc-qta-ord      .
       det-qev-roc-150.
      *              *-------------------------------------------------*
      *              * Test se ordine di tipo 'forecast'               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        rf-ocr-tip-ord       not  = "F"
                     go to det-qev-roc-200.
      *                  *---------------------------------------------*
      *                  * Subroutine per righe ordine 'forecast'      *
      *                  *---------------------------------------------*
           perform   det-qev-frc-000      thru det-qev-frc-999        .
      *                  *---------------------------------------------*
      *                  * Valori determinati                          *
      *                  *---------------------------------------------*
           move      w-roc-ord-qta-ord    to   d-qev-roc-qta-dev      .
           move      w-roc-frc-qta-eva    to   d-qev-roc-qta-gsp      .
           subtract  w-roc-frc-qta-eva    from d-qev-roc-qta-dev      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento estremi ultima evasione       *
      *                  *---------------------------------------------*
           if        w-roc-frc-tip-doc    =    spaces and
                     w-roc-frc-dat-doc    =    zero   and
                     w-roc-frc-num-doc    =    zero
                     go to det-qev-roc-180.
           move      02                   to   d-qev-roc-tip-uev      .
           move      w-roc-frc-tip-doc    to   d-qev-roc-tdo-uev      .
           move      w-roc-frc-dat-doc    to   d-qev-roc-ddo-uev      .
           move      w-roc-frc-num-doc    to   d-qev-roc-ndo-uev      .
       det-qev-roc-180.
      *                  *---------------------------------------------*
      *                  * Valori normalizzazti                        *
      *                  *---------------------------------------------*
           move      zero                 to   d-qev-roc-qta-ics      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-qev-roc-999.
       det-qev-roc-200.
      *              *-------------------------------------------------*
      *              * Determinazione della quantita' gia' spedita     *
      *              *-------------------------------------------------*
           perform   rpd-roc-gsp-000      thru rpd-roc-gsp-999        .
           move      w-roc-gsp-qta-gsp    to   d-qev-roc-qta-gsp      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento estremi ultima evasione       *
      *                  *---------------------------------------------*
           if        w-roc-gsp-tip-doc    =    spaces and
                     w-roc-gsp-dat-doc    =    zero   and
                     w-roc-gsp-num-doc    =    zero
                     go to det-qev-roc-300.
           move      01                   to   d-qev-roc-tip-uev      .
           move      w-roc-gsp-tip-doc    to   d-qev-roc-tdo-uev      .
           move      w-roc-gsp-dat-doc    to   d-qev-roc-ddo-uev      .
           move      w-roc-gsp-num-doc    to   d-qev-roc-ndo-uev      .
       det-qev-roc-300.
      *              *-------------------------------------------------*
      *              * Determinazione della quantita' in corso di spe- *
      *              * dizione                                         *
      *              *-------------------------------------------------*
           perform   rpd-roc-ics-000      thru rpd-roc-ics-999        .
           move      w-roc-ics-qta-ics    to   d-qev-roc-qta-ics      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento estremi ultima evasione       *
      *                  *---------------------------------------------*
           if        w-roc-ics-tip-doc    =    spaces and
                     w-roc-ics-dat-doc    =    zero   and
                     w-roc-ics-num-doc    =    zero
                     go to det-qev-roc-400.
           if        w-roc-ics-dat-doc    not  > d-qev-roc-ddo-uev
                     go to det-qev-roc-400.
           move      02                   to   d-qev-roc-tip-uev      .
           move      w-roc-ics-tip-doc    to   d-qev-roc-tdo-uev      .
           move      w-roc-ics-dat-doc    to   d-qev-roc-ddo-uev      .
           move      w-roc-ics-num-doc    to   d-qev-roc-ndo-uev      .
       det-qev-roc-400.
      *              *-------------------------------------------------*
      *              * Determinazione status riga ordine nel caso di   *
      *              * riga di addebito                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se riga di Addebito                    *
      *                  *---------------------------------------------*
           if        w-rdf-tip-rig-wtp    not  = "A"
                     go to det-qev-roc-405.
      *                  *---------------------------------------------*
      *                  * Determinazione status confrontando i flag   *
      *                  * per riga di addebito determinati dalle rou- *
      *                  * tines                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se attivo quello relativo alla determi- *
      *                      * nazione della quantita' gia' spedita :  *
      *                      * status di uscita a 'Evasa'              *
      *                      *-----------------------------------------*
           if        w-roc-gsp-flg-rda    not  = spaces
                     move  03             to   d-qev-roc-sts-rda
                     go to det-qev-roc-999.
      *                      *-----------------------------------------*
      *                      * Se attivo quello relativo alla determi- *
      *                      * nazione della quantita' in spedizione : *
      *                      * status di uscita a 'In Spedizione'      *
      *                      *-----------------------------------------*
           if        w-roc-ics-flg-rda    not  = spaces
                     move  02             to   d-qev-roc-sts-rda
                     go to det-qev-roc-999.
      *                      *-----------------------------------------*
      *                      * Altrimenti : status di uscita a 'Ineva- *
      *                      * sa'                                     *
      *                      *-----------------------------------------*
           move      01                   to   d-qev-roc-sts-rda      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     det-qev-roc-999.
       det-qev-roc-405.
      *              *-------------------------------------------------*
      *              * Determinazione della quantita' ancora da evade- *
      *              * re                                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se riga chiusa : quantita' da evadere a ze- *
      *                  * ro                                          *
      *                  *---------------------------------------------*
           if        rf-ocr-flg-rch       not  = spaces
                     move  zero           to   d-qev-roc-qta-dev
                     go to det-qev-roc-999.
      *                  *---------------------------------------------*
      *                  * Se riga comunque da considerarsi saldata :  *
      *                  * quantita' da evadere a zero                 *
      *                  *---------------------------------------------*
           if        rf-ocr-sdr-ccs       not  = spaces
                     move  zero           to   d-qev-roc-qta-dev
                     go to det-qev-roc-999.
      *                  *---------------------------------------------*
      *                  * Se quantita' ordinata a zero : quantita' da *
      *                  * evadere a zero                              *
      *                  *---------------------------------------------*
           if        d-qev-roc-qta-ord    =    zero
                     move  zero           to   d-qev-roc-qta-dev
                     go to det-qev-roc-999.
       det-qev-roc-410.
      *                  *---------------------------------------------*
      *                  * Differenza algebrica tra :                  *
      *                  *   - Quantita' in ordine                     *
      *                  * e                                           *
      *                  *   - Quantita' gia' spedita                  *
      *                  *   - Quantita' in corso di spedizione        *
      *                  *---------------------------------------------*
           move      d-qev-roc-qta-ord    to   d-qev-roc-qta-dev      .
           subtract  d-qev-roc-qta-gsp    from d-qev-roc-qta-dev      .
           subtract  d-qev-roc-qta-ics    from d-qev-roc-qta-dev      .
      *                  *---------------------------------------------*
      *                  * Riduzione a zero del residuo eventualmente  *
      *                  * negativo, tenendo conto del segno algebri-  *
      *                  * co della quantita' in ordine                *
      *                  *---------------------------------------------*
           if        d-qev-roc-qta-ord    >    zero and
                     d-qev-roc-qta-dev    <    zero
                     move  zero           to   d-qev-roc-qta-dev      .
           if        d-qev-roc-qta-ord    <    zero and
                     d-qev-roc-qta-dev    >    zero
                     move  zero           to   d-qev-roc-qta-dev      .
       det-qev-roc-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione, relativamente ad una riga  *
      *    * ordine cliente, di :                                      *
      *    *                                                           *
      *    *    - Quantita' in ordine                                  *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *    - Record di [ocr] relativo alla riga ordine cliente a  *
      *    *      disposizione in rf-ocr                               *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *    - w-roc-ord-qta-ord : Quantita' in ordine              *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       rpd-roc-ord-000.
      *              *-------------------------------------------------*
      *              * Si considera come 'Quantita' in ordine' il cam- *
      *              * po 'qta-ord' del record di [ocr] in esame       *
      *              *-------------------------------------------------*
           move      rf-ocr-qta-ord       to   w-roc-ord-qta-ord      .
       rpd-roc-ord-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione, relativamente ad una riga  *
      *    * ordine cliente, di :                                      *
      *    *                                                           *
      *    *    - Quantita' gia' spedita                               *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *    - Record di [ocr] relativo alla riga ordine cliente a  *
      *    *      disposizione in rf-ocr                               *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *    - w-roc-gsp-qta-gsp : Quantita' gia' spedita           *
      *    *    - w-roc-gsp-tip-doc : Tipo documento ultima spedizione *
      *    *    - w-roc-gsp-dat-doc : Data documento ultima spedizione *
      *    *    - w-roc-gsp-num-doc : Num. documento ultima spedizione *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       rpd-roc-gsp-000.
      *              *-------------------------------------------------*
      *              * Inizializzazioni preliminari                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag per riga di addebito   *
      *                  *---------------------------------------------*
           move      spaces               to   w-roc-gsp-flg-rda      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione estremi ultima spedizione   *
      *                  *---------------------------------------------*
           move      spaces               to   w-roc-gsp-tip-doc      .
           move      zero                 to   w-roc-gsp-dat-doc      .
           move      zero                 to   w-roc-gsp-num-doc      .
      *                  *---------------------------------------------*
      *                  * Quantita' gia' spedita : a zero             *
      *                  *---------------------------------------------*
           move      zero                 to   w-roc-gsp-qta-gsp      .
       rpd-roc-gsp-100.
      *              *-------------------------------------------------*
      *              * Start iniziale su [bir]                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RIFORC    "         to   f-key                  .
           move      rf-ocr-cod-dpz       to   rf-bir-cod-dpz         .
           move      rf-ocr-num-prt       to   rf-bir-coc-prt         .
           move      rf-ocr-num-prg       to   rf-bir-coc-prg         .
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
                     go to rpd-roc-gsp-999.
       rpd-roc-gsp-200.
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
                     go to rpd-roc-gsp-999.
       rpd-roc-gsp-300.
      *              *-------------------------------------------------*
      *              * Test Max su [bir]                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su :                                   *
      *                  *    - Codice dipendenza                      *
      *                  *    - Numero protocollo ordine cliente       *
      *                  *    - Numero riga ordine cliente             *
      *                  *---------------------------------------------*
           if        rf-bir-cod-dpz       =    rf-ocr-cod-dpz and
                     rf-bir-coc-prt       =    rf-ocr-num-prt and
                     rf-bir-coc-prg       =    rf-ocr-num-prg
                     go to rpd-roc-gsp-400.
      *                  *---------------------------------------------*
      *                  * Se oltre il Max : uscita                    *
      *                  *---------------------------------------------*
           go to     rpd-roc-gsp-999.
       rpd-roc-gsp-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record di [bir] rispetto al re-   *
      *              * cord di [ocr]                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           perform   sel-bir-ocr-000      thru sel-bir-ocr-999        .
      *                  *---------------------------------------------*
      *                  * Se selezione non superata : si ignora il    *
      *                  * record letto                                *
      *                  *---------------------------------------------*
           if        w-sel-bir-ocr-flg    not  = spaces
                     go to rpd-roc-gsp-800.
       rpd-roc-gsp-500.
      *              *-------------------------------------------------*
      *              * Considerazioni sul record di [bir] letto e se-  *
      *              * lezionato rispetto ad [ocr]                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento estremi ultima spedizione     *
      *                  *---------------------------------------------*
           if        rf-bir-dat-doc       not  < w-roc-gsp-dat-doc
                     move  rf-bir-cod-tmb to   w-roc-gsp-tip-doc
                     move  rf-bir-dat-doc to   w-roc-gsp-dat-doc
                     move  rf-bir-num-doc to   w-roc-gsp-num-doc      .
      *                  *---------------------------------------------*
      *                  * Si somma la 'Quantita' di vendita' contenu- *
      *                  * ta nel record alla 'Quantita' gia' spedita' *
      *                  * che si sta determinando                     *
      *                  *---------------------------------------------*
           add       rf-bir-qta-ven       to   w-roc-gsp-qta-gsp      .
      *                  *---------------------------------------------*
      *                  * Se tipo riga 'A' : attivazione flag per     *
      *                  * riga di addebito e uscita                   *
      *                  *---------------------------------------------*
           if        w-rdf-tip-rig-wtp    =    "A"
                     move  "#"            to   w-roc-gsp-flg-rda
                     go to rpd-roc-gsp-999.
       rpd-roc-gsp-800.
      *              *-------------------------------------------------*
      *              * Riciclo a Read Next su [bir]                    *
      *              *-------------------------------------------------*
           go to     rpd-roc-gsp-200.
       rpd-roc-gsp-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione, relativamente ad una riga  *
      *    * ordine cliente, di :                                      *
      *    *                                                           *
      *    *    - Quantita' in corso di spedizione                     *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *    - Record di [ocr] relativo alla riga ordine cliente a  *
      *    *      disposizione in rf-ocr                               *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *    - w-roc-ics-qta-ics : Quantita' in corso di spedizione *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       rpd-roc-ics-000.
      *              *-------------------------------------------------*
      *              * Inizializzazioni preliminari                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag per riga di addebito   *
      *                  *---------------------------------------------*
           move      spaces               to   w-roc-ics-flg-rda      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione estremi ultima spedizione   *
      *                  * in corso                                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-roc-ics-tip-doc      .
           move      zero                 to   w-roc-ics-dat-doc      .
           move      zero                 to   w-roc-ics-num-doc      .
      *                  *---------------------------------------------*
      *                  * Quantita' in corso di spedizione : a zero   *
      *                  *---------------------------------------------*
           move      zero                 to   w-roc-ics-qta-ics      .
       rpd-roc-ics-050.
      *              *-------------------------------------------------*
      *              * Test se gestione spedizioni attiva              *
      *              *-------------------------------------------------*
           if        w-prs-ods-snx        not  = "S"
                     go to rpd-roc-ics-999.
       rpd-roc-ics-100.
      *              *-------------------------------------------------*
      *              * Start iniziale su [osr]                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RIFORC    "         to   f-key                  .
           move      rf-ocr-cod-dpz       to   rf-osr-cod-dpz         .
           move      rf-ocr-num-prt       to   rf-osr-coc-prt         .
           move      rf-ocr-num-prg       to   rf-osr-coc-prg         .
           move      zero                 to   rf-osr-num-prt         .
           move      zero                 to   rf-osr-num-prg         .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rpd-roc-ics-999.
       rpd-roc-ics-200.
      *              *-------------------------------------------------*
      *              * Read Next su [osr]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Read Next                                   *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                  *---------------------------------------------*
      *                  * Se At end : uscita                          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rpd-roc-ics-999.
       rpd-roc-ics-300.
      *              *-------------------------------------------------*
      *              * Test Max su [osr]                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su :                                   *
      *                  *    - Codice dipendenza                      *
      *                  *    - Numero protocollo ordine cliente       *
      *                  *    - Numero riga ordine cliente             *
      *                  *---------------------------------------------*
           if        rf-osr-cod-dpz       =    rf-ocr-cod-dpz and
                     rf-osr-coc-prt       =    rf-ocr-num-prt and
                     rf-osr-coc-prg       =    rf-ocr-num-prg
                     go to rpd-roc-ics-400.
      *                  *---------------------------------------------*
      *                  * Se oltre il Max : uscita                    *
      *                  *---------------------------------------------*
           go to     rpd-roc-ics-999.
       rpd-roc-ics-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record di [osr] rispetto al re-   *
      *              * cord di [ocr]                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           perform   sel-osr-ocr-000      thru sel-osr-ocr-999        .
      *                  *---------------------------------------------*
      *                  * Se selezione non superata : si ignora il    *
      *                  * record letto                                *
      *                  *---------------------------------------------*
           if        w-sel-osr-ocr-flg    not  = spaces
                     go to rpd-roc-ics-800.
       rpd-roc-ics-500.
      *              *-------------------------------------------------*
      *              * Considerazioni sul record di [osr] letto e se-  *
      *              * lezionato rispetto ad [ocr]                     *
      *              *-------------------------------------------------*
       rpd-roc-ics-550.
      *                  *---------------------------------------------*
      *                  * Salvataggio della 'Quantita' in corso di    *
      *                  * spedizione' cosi' come da [osr]             *
      *                  *---------------------------------------------*
           move      rf-osr-qta-ven       to   w-roc-ics-wqt-osr      .
       rpd-roc-ics-600.
      *                  *---------------------------------------------*
      *                  * Se tipo riga 'A' : attivazione flag per ri- *
      *                  * ga di addebito                              *
      *                  *---------------------------------------------*
           if        w-rdf-tip-rig-wtp    =    "A"
                     move  "#"            to   w-roc-ics-flg-rda      .
       rpd-roc-ics-605.
      *                  *---------------------------------------------*
      *                  * Determinazione della quantita' spedita a    *
      *                  * fronte ordini di spedizione cosi' come da   *
      *                  * [bir]                                       *
      *                  *---------------------------------------------*
       rpd-roc-ics-610.
      *                      *-----------------------------------------*
      *                      * Quantita' spedita a fronte ordini di    *
      *                      * spedizione cosi' come da [bir] : a zero *
      *                      *-----------------------------------------*
           move      zero                 to   w-roc-ics-wqt-bir      .
       rpd-roc-ics-620.
      *                      *-----------------------------------------*
      *                      * Start iniziale su [bir]                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Start                               *
      *                          *-------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RIFODS    "         to   f-key                  .
           move      rf-ocr-cod-dpz       to   rf-bir-cod-dpz         .
           move      rf-osr-num-prt       to   rf-bir-ods-prt         .
           move      rf-osr-num-prg       to   rf-bir-ods-prg         .
           move      rf-ocr-num-prt       to   rf-bir-coc-prt         .
           move      rf-ocr-num-prg       to   rf-bir-coc-prg         .
           move      zero                 to   rf-bir-num-prt         .
           move      zero                 to   rf-bir-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                          *-------------------------------------*
      *                          * Se Start errata : a determinazione  *
      *                          * della quantita' in corso di spedi-  *
      *                          * zione effettivamente residua        *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rpd-roc-ics-700.
       rpd-roc-ics-630.
      *                      *-----------------------------------------*
      *                      * Read Next su [bir]                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Read Next                           *
      *                          *-------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                          *-------------------------------------*
      *                          * Se At end : a determinazione della  *
      *                          * quantita' in corso di spedizione    *
      *                          * effettivamente residua              *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rpd-roc-ics-700.
       rpd-roc-ics-640.
      *                      *-----------------------------------------*
      *                      * Test Max su [bir]                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su :                           *
      *                          *    - Codice dipendenza              *
      *                          *    - Numero protocollo ordine di    *
      *                          *      spedizione                     *
      *                          *    - Numero riga ordine di spedi-   *
      *                          *      zione                          *
      *                          *    - Numero protocollo ordine cli-  *
      *                          *      ente                           *
      *                          *    - Numero riga ordine cliente     *
      *                          *-------------------------------------*
           if        rf-bir-cod-dpz       =    rf-ocr-cod-dpz and
                     rf-bir-ods-prt       =    rf-osr-num-prt and
                     rf-bir-ods-prg       =    rf-osr-num-prg and
                     rf-bir-coc-prt       =    rf-ocr-num-prt and
                     rf-bir-coc-prg       =    rf-ocr-num-prg
                     go to rpd-roc-ics-650.
      *                          *-------------------------------------*
      *                          * Se oltre il Max : a determinazione  *
      *                          * della quantita' in corso di spedi-  *
      *                          * zione effettivamente residua        *
      *                          *-------------------------------------*
           go to     rpd-roc-ics-700.
       rpd-roc-ics-650.
      *                      *-----------------------------------------*
      *                      * Selezioni sul record di [bir] rispetto  *
      *                      * al record di [osr]                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Selezione                           *
      *                          *-------------------------------------*
           perform   sel-bir-osr-000      thru sel-bir-osr-999        .
      *                          *-------------------------------------*
      *                          * Se selezione non superata : si i-   *
      *                          * gnora il record letto               *
      *                          *-------------------------------------*
           if        w-sel-bir-osr-flg    not  = spaces
                     go to rpd-roc-ics-680.
       rpd-roc-ics-660.
      *                      *-----------------------------------------*
      *                      * Selezioni sul record di [bir] rispetto  *
      *                      * al record di [ocr]                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Selezione                           *
      *                          *-------------------------------------*
           perform   sel-bir-ocr-000      thru sel-bir-ocr-999        .
      *                          *-------------------------------------*
      *                          * Se selezione non superata : si i-   *
      *                          * gnora il record letto               *
      *                          *-------------------------------------*
           if        w-sel-bir-ocr-flg    not  = spaces
                     go to rpd-roc-ics-680.
       rpd-roc-ics-670.
      *                      *-----------------------------------------*
      *                      * Considerazioni sul record di [bir] let- *
      *                      * to e selezionato rispetto ad [osr] e a  *
      *                      * [ocr]                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se tipo riga 'A' : normalizzazione  *
      *                          * flag per riga di addebito e uscita  *
      *                          *-------------------------------------*
           if        w-rdf-tip-rig-wtp    =    "A"
                     move  spaces         to   w-roc-ics-flg-rda
                     go to rpd-roc-ics-999.
      *                          *-------------------------------------*
      *                          * Si somma la 'Quantita' di vendita'  *
      *                          * contenuta nel record alla quantita' *
      *                          * spedita a fronte spedizioni cosi'   *
      *                          * come da [bir]                       *
      *                          *-------------------------------------*
           add       rf-bir-qta-ven       to   w-roc-ics-wqt-bir      .
       rpd-roc-ics-680.
      *                      *-----------------------------------------*
      *                      * Riciclo a Read Next su [bir]            *
      *                      *-----------------------------------------*
           go to     rpd-roc-ics-630.
       rpd-roc-ics-700.
      *                  *---------------------------------------------*
      *                  * Determinazione della quantita' in corso di  *
      *                  * spedizione effettivamente residua, consi-   *
      *                  * derando [osr] e [bir]                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Differenza algebrica tra :              *
      *                      *   - Quantita' in corso di spedizione da *
      *                      *     [osr]                               *
      *                      * e                                       *
      *                      *   - Quantita' spedita a fronte ordini   *
      *                      *     di spedizione da [bir]              *
      *                      *-----------------------------------------*
           move      w-roc-ics-wqt-osr    to   w-roc-ics-wqt-ics      .
           subtract  w-roc-ics-wqt-bir    from w-roc-ics-wqt-ics      .
      *                      *-----------------------------------------*
      *                      * Riduzione a zero del residuo eventual-  *
      *                      * mente negativo, tenendo conto del segno *
      *                      * algebrico della quantita' in corso di   *
      *                      * spedizione da [osr]                     *
      *                      *-----------------------------------------*
           if        w-roc-ics-wqt-osr    >    zero  and
                     w-roc-ics-wqt-ics    <    zero
                     move  zero           to   w-roc-ics-wqt-ics      .
           if        w-roc-ics-wqt-osr    <    zero  and
                     w-roc-ics-wqt-ics    >    zero
                     move  zero           to   w-roc-ics-wqt-ics      .
       rpd-roc-ics-750.
      *                  *---------------------------------------------*
      *                  * Somma della quantita' in corso di spedizio- *
      *                  * spedizione effettivamente residua sulla     *
      *                  * 'Quantita' in corso di spedizione' da de-   *
      *                  * terminare                                   *
      *                  *---------------------------------------------*
           add       w-roc-ics-wqt-ics    to   w-roc-ics-qta-ics      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento estremi ultima spedizione in  *
      *                  * corso                                       *
      *                  *---------------------------------------------*
           if        rf-osr-dat-doc       not  < w-roc-ics-dat-doc
                     move  rf-osr-cod-tms to   w-roc-ics-tip-doc
                     move  rf-osr-dat-doc to   w-roc-ics-dat-doc
                     move  rf-osr-num-prt to   w-roc-ics-num-doc      .
       rpd-roc-ics-800.
      *              *-------------------------------------------------*
      *              * Riciclo a Read Next su [osr]                    *
      *              *-------------------------------------------------*
           go to     rpd-roc-ics-200.
       rpd-roc-ics-999.
           exit.

      *    *===========================================================*
      *    * Routine per la selezione sul record [bir] relativamente   *
      *    * al record di [ocr], per compatibilita'                    *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *    - Record di [bir] relativo alla riga bolla cliente a   *
      *    *      disposizione in rf-bir                               *
      *    *                                                           *
      *    *    - Record di [ocr] relativo alla riga ordine cliente a  *
      *    *      disposizione in rf-ocr                               *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *    - w-sel-bir-ocr-flg : Esito della selezione            *
      *    *                            - Spaces : Ok                  *
      *    *                            - #      : Ko                  *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       sel-bir-ocr-000.
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo archivio                    *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bir-tip-arc       not  = rf-ocr-tip-arc
                     go to sel-bir-ocr-900.
       sel-bir-ocr-025.
      *              *-------------------------------------------------*
      *              * Selezione su : Codice archivio                  *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bir-cod-arc       not  = rf-ocr-cod-arc
                     go to sel-bir-ocr-900.
       sel-bir-ocr-050.
      *              *-------------------------------------------------*
      *              * Selezione su : Codice dipendenza dell'archivio  *
      *              *                                                 *
      *              * Non viene fatta alcuna selezione su questo cam- *
      *              * po, in quanto si permette di spedire ad una di- *
      *              * pendenza cliente diversa da quanto stabilito in *
      *              * ordine cliente.                                 *
      *              *-------------------------------------------------*
       sel-bir-ocr-075.
      *              *-------------------------------------------------*
      *              * Selezione su : Flag blocco di documento         *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *                                                 *
      *              * CAMPI MAI GESTITI                               *
      *              *-------------------------------------------------*
______*    if        rf-bir-bld-flb       not  = rf-ocr-bld-flb
______*              go to sel-bir-ocr-900.
       sel-bir-ocr-100.
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo riga                        *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bir-tip-rig       not  = rf-ocr-tip-rig
                     go to sel-bir-ocr-900.
       sel-bir-ocr-125.
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo codice di magazzino         *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bir-tip-mag       not  = rf-ocr-tip-mag
                     go to sel-bir-ocr-900.
       sel-bir-ocr-150.
      *              *-------------------------------------------------*
      *              * Selezione su : Codice prodotto numerico         *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bir-num-pro       not  = rf-ocr-num-pro
                     go to sel-bir-ocr-900.
       sel-bir-ocr-175.
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
       sel-bir-ocr-200.
      *              *-------------------------------------------------*
      *              * Selezione su : Sigla della variante per il pro- *
      *              *                dotto                            *
      *              *                                                 *
      *              * Non viene fatta alcuna selezione su questo cam- *
      *              * po, in quanto si permette di spedire una va-    *
      *              * riante diversa da quanto stabilito in ordine    *
      *              * cliente.                                        *
      *              *-------------------------------------------------*
       sel-bir-ocr-225.
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo prodotto                    *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bir-tip-pro       not  = rf-ocr-tip-pro
                     go to sel-bir-ocr-900.
       sel-bir-ocr-230.
      *              *-------------------------------------------------*
      *              * Selezione su : Data di riferimento              *
      *              *-------------------------------------------------*
           if        d-qev-roc-dat-rif    =    zero
                     go to sel-bir-ocr-240.
           if        rf-bir-dat-doc       >    d-qev-roc-dat-rif
                     go to sel-bir-ocr-900.
       sel-bir-ocr-240.
      *              *-------------------------------------------------*
      *              * Selezione su : flag di riga da considerare ai   *
      *              * fini dell'evasione                              *
      *              *-------------------------------------------------*
           if        rf-bir-flg-nbx (3)   not  = spaces
                     go to sel-bir-ocr-900.
       sel-bir-ocr-250.
       sel-bir-ocr-800.
      *              *-------------------------------------------------*
      *              * Uscita per selezione : Ok                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-sel-bir-ocr-flg      .
           go to     sel-bir-ocr-999.
       sel-bir-ocr-900.
      *              *-------------------------------------------------*
      *              * Uscita per selezione : Ko                       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-sel-bir-ocr-flg      .
           go to     sel-bir-ocr-999.
       sel-bir-ocr-999.
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
      *    *    - Record di [osr] relativo all'ordine di spedizione    *
      *    *      a disposizione in rf-osr                             *
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
       sel-bir-osr-230.
      *              *-------------------------------------------------*
      *              * Selezione su : Data di riferimento              *
      *              *-------------------------------------------------*
           if        d-qev-roc-dat-rif    =    zero
                     go to sel-bir-osr-800.
           if        rf-bir-dat-doc       >    d-qev-roc-dat-rif
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

      *    *===========================================================*
      *    * Routine per la selezione sul record [osr] relativamente   *
      *    * al record di [ocr], per compatibilita'                    *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *    - Record di [osr] relativo alla riga ordine di spe-    *
      *    *      dizione a disposizione in rf-osr                     *
      *    *                                                           *
      *    *    - Record di [ocr] relativo alla riga ordine cliente a  *
      *    *      disposizione in rf-ocr                               *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *    - w-sel-osr-ocr-flg : Esito della selezione            *
      *    *                            - Spaces : Ok                  *
      *    *                            - #      : Ko                  *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       sel-osr-ocr-000.
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo archivio                    *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-osr-tip-arc       not  = rf-ocr-tip-arc
                     go to sel-osr-ocr-900.
       sel-osr-ocr-025.
      *              *-------------------------------------------------*
      *              * Selezione su : Codice archivio                  *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-osr-cod-arc       not  = rf-ocr-cod-arc
                     go to sel-osr-ocr-900.
       sel-osr-ocr-050.
      *              *-------------------------------------------------*
      *              * Selezione su : Codice dipendenza dell'archivio  *
      *              *                                                 *
      *              * Non viene fatta alcuna selezione su questo cam- *
      *              * po, in quanto si permette di spedire ad una di- *
      *              * pendenza cliente diversa da quanto stabilito in *
      *              * ordine cliente.                                 *
      *              *-------------------------------------------------*
       sel-osr-ocr-075.
      *              *-------------------------------------------------*
      *              * Selezione su : Flag blocco di documento         *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-osr-bld-flb       not  = rf-ocr-bld-flb
                     go to sel-osr-ocr-900.
       sel-osr-ocr-100.
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo riga                        *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-osr-tip-rig       not  = rf-ocr-tip-rig
                     go to sel-osr-ocr-900.
       sel-osr-ocr-125.
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo codice di magazzino         *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-osr-tip-mag       not  = rf-ocr-tip-mag
                     go to sel-osr-ocr-900.
       sel-osr-ocr-150.
      *              *-------------------------------------------------*
      *              * Selezione su : Codice prodotto numerico         *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-osr-num-pro       not  = rf-ocr-num-pro
                     go to sel-osr-ocr-900.
       sel-osr-ocr-175.
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
       sel-osr-ocr-200.
      *              *-------------------------------------------------*
      *              * Selezione su : Sigla della variante per il pro- *
      *              *                dotto                            *
      *              *                                                 *
      *              * Non viene fatta alcuna selezione su questo cam- *
      *              * po, in quanto si permette di spedire una va-    *
      *              * riante diversa da quanto stabilito in ordine    *
      *              * cliente.                                        *
      *              *-------------------------------------------------*
       sel-osr-ocr-225.
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo prodotto                    *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-osr-tip-pro       not  = rf-ocr-tip-pro
                     go to sel-osr-ocr-900.
       sel-osr-ocr-275.
      *              *-------------------------------------------------*
      *              * Selezione su : Flag di riga completamente chiu- *
      *              *                sa                               *
      *              *                                                 *
      *              * Deve essere pari a Spaces, non si prendono in   *
      *              * considerazione le righe di ordine di spedizio-  *
      *              * ne che sono gia' completamente chiuse           *
      *              *-------------------------------------------------*
           if        rf-osr-flg-rch       not  = spaces
                     go to sel-osr-ocr-900.
       sel-osr-ocr-280.
      *              *-------------------------------------------------*
      *              * Selezione su : Data di riferimento              *
      *              *-------------------------------------------------*
           if        d-qev-roc-dat-rif    =    zero
                     go to sel-osr-ocr-800.
           if        rf-osr-dat-doc       >    d-qev-roc-dat-rif
                     go to sel-osr-ocr-900.
       sel-osr-ocr-800.
      *              *-------------------------------------------------*
      *              * Uscita per selezione : Ok                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-sel-osr-ocr-flg      .
           go to     sel-osr-ocr-999.
       sel-osr-ocr-900.
      *              *-------------------------------------------------*
      *              * Uscita per selezione : Ko                       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-sel-osr-ocr-flg      .
           go to     sel-osr-ocr-999.
       sel-osr-ocr-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione, relativamente ad una riga  *
      *    * ordine cliente 'forecast', di :                           *
      *    *                                                           *
      *    *    - Quantita' evasa                                      *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *    - Record di [ocr] relativo alla riga ordine cliente a  *
      *    *      disposizione in rf-ocr                               *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *    - w-roc-frc-qta-eva : Quantita' gia' spedita           *
      *    *    - w-roc-frc-tip-doc : Tipo documento ultima spedizione *
      *    *    - w-roc-frc-dat-doc : Data documento ultima spedizione *
      *    *    - w-roc-frc-num-doc : Num. documento ultima spedizione *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       det-qev-frc-000.
      *              *-------------------------------------------------*
      *              * Inizializzazioni preliminari                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag per riga di addebito   *
      *                  *---------------------------------------------*
           move      spaces               to   w-roc-frc-flg-rda      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione estremi ultima evasione     *
      *                  *---------------------------------------------*
           move      spaces               to   w-roc-frc-tip-doc      .
           move      zero                 to   w-roc-frc-dat-doc      .
           move      zero                 to   w-roc-frc-num-doc      .
      *                  *---------------------------------------------*
      *                  * Quantita' evasa : a zero                    *
      *                  *---------------------------------------------*
           move      zero                 to   w-roc-frc-qta-eva      .
       det-qev-frc-100.
      *              *-------------------------------------------------*
      *              * Start iniziale su [ocf]                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RIFPRE    "         to   f-key                  .
           move      rf-ocr-num-prt       to   rf-ocf-frc-prt         .
           move      rf-ocr-num-prg       to   rf-ocf-frc-prg         .
           move      zero                 to   rf-ocf-num-prt         .
           move      zero                 to   rf-ocf-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocf                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-qev-frc-900.
       det-qev-frc-200.
      *              *-------------------------------------------------*
      *              * Read Next su [ocf]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Read Next                                   *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocf                 .
      *                  *---------------------------------------------*
      *                  * Se At end : uscita                          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-qev-frc-900.
       det-qev-frc-300.
      *              *-------------------------------------------------*
      *              * Test Max su [ocf]                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su :                                   *
      *                  *    - Numero protocollo ordine cliente       *
      *                  *    - Numero riga ordine cliente             *
      *                  *---------------------------------------------*
           if        rf-ocf-frc-prt       =    rf-ocr-num-prt and
                     rf-ocf-frc-prg       =    rf-ocr-num-prg
                     go to det-qev-frc-400.
      *                  *---------------------------------------------*
      *                  * Se oltre il Max : uscita                    *
      *                  *---------------------------------------------*
           go to     det-qev-frc-900.
       det-qev-frc-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record di [ocf] rispetto al re-   *
      *              * cord di [ocr]                                   *
      *              *-------------------------------------------------*
       det-qev-frc-500.
      *              *-------------------------------------------------*
      *              * Considerazioni sul record di [ocf] letto e se-  *
      *              * lezionato rispetto ad [ocr]                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si somma la 'Quantita' di vendita' contenu- *
      *                  * ta nel record alla 'Quantita' gia' spedita' *
      *                  * che si sta determinando                     *
      *                  *---------------------------------------------*
           add       rf-ocf-qta-eva       to   w-roc-frc-qta-eva      .
      *                  *---------------------------------------------*
      *                  * Se tipo riga 'A' : attivazione flag per     *
      *                  * riga di addebito e uscita                   *
      *                  *---------------------------------------------*
           if        w-rdf-tip-rig-wtp    =    "A"
                     move  "#"            to   w-roc-frc-flg-rda
                     go to det-qev-frc-999.
      *                  *---------------------------------------------*
      *                  * Aggiornamento estremi ultima spedizione     *
      *                  *---------------------------------------------*
           if        rf-ocf-dat-doc       not  < w-roc-frc-dat-doc
                     move  rf-ocf-tmo-orc to   w-roc-frc-tip-doc
                     move  rf-ocf-dat-doc to   w-roc-frc-dat-doc
                     move  rf-ocf-num-doc to   w-roc-frc-num-doc      .
       det-qev-frc-800.
      *              *-------------------------------------------------*
      *              * Riciclo a Read Next su [ocf]                    *
      *              *-------------------------------------------------*
           go to     det-qev-frc-200.
       det-qev-frc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-qev-frc-999.
       det-qev-frc-999.
           exit.

