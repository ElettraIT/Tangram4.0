       Identification Division.
       Program-Id.                                 dqevrof0           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    orf                 *
      *                                Settore:    mov                 *
      *                                   Fase:    dqevrof0            *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 25/07/92    *
      *                       Ultima revisione:    NdK del 07/06/21    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Modulo per la determinazione, relativamente ad una riga ordine *
      * fornitore, di :                                                *
      *                                                                *
      * - Quantita' in ordine                                          *
      * - Quantita' gia' ricevuta                                      *
      * - Quantita' in corso di ricevimento                            *
      *                                                                *
      * e, di conseguenza, di :                                        *
      *                                                                *
      * - Quantita' ancora da ricevere                                 *
      *                                                                *
      *                                                                *
      * N.B.: le quantita' sono espresse in 2 formati in presenza      *
      *       della trasformazione della unita' di misura              *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * Memo: Sarebbe utile implementare un flag di riga chiusa o      *
      *       saldata con forzatura                                    *
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
      *        Input  : d-qev-rof-tip-ope = "OP"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *                                                                *
      *        Input  : d-qev-rof-tip-ope = "CL"                       *
      *                                                                *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *                                                                *
      *        Input  : d-qev-rof-tip-ope = "C?"                       *
      *                                                                *
      *                                                                *
      *        Output : d-qev-rof-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * -------------------------------------------------------------- *
      *                                                                *
      * "DT" - Determinazione status riga ordine fornitore             *
      *                                                                *
      *                                                                *
      *        Input  : d-qev-rof-tip-ope = "DT"                       *
      *                                                                *
      *                 d-qev-rof-dat-rif = Data di riferimento        *
      *                                     (facoltativa)              *
      *                                                                *
      *                 Record di [ofr] relativo alla riga ordine      *
      *                 cliente a disposizione in rf-ofr               *
      *                                                                *
      *                                                                *
      *        Output : d-qev-rof-qta-ord = Quantita' in ordine        *
      *                 d-qev-rof-fda-ord                              *
      *                                                                *
      *                 d-qev-rof-qta-ric = Quantita' ricevuta         *
      *                 d-qev-rof-fda-ric                              *
      *                                                                *
      *                 d-qev-rof-qta-icr = Quantita' in corso di ri-  *
      *                 d-qev-rof-fda-icr   cevimento                  *
      *                                                                *
      *                 d-qev-rof-qta-dri = Quantita' ancora da rice-  *
      *                 d-qev-rof-fda-dri   vere                       *
      *                                                                *
      *                 d-qev-rof-snx-tum = Si/no trasformazione unita'*
      *                                     di misura                  *
      *                                                                *
      *                 d-qev-rof-tip-urc = Tipo movimento dell'ultimo *
      *                                     ricevimento                *
      *                                     - 01 : Bolla di accomp.    *
      *                                                                *
      *                 d-qev-rof-tdo-urc = Tipo documento dell'ultimo *
      *                                     ricevimento                *
      *                                                                *
      *                 d-qev-rof-drg-urc = Data registrazione ultimo  *
      *                                     ricevimento                *
      *                                                                *
      *                 d-qev-rof-prt-urc = Numero protocollo ultimo   *
      *                                     ricevimento                *
      *                                                                *
      *                 d-qev-rof-sts-rda = Status se riga di addebito *
      *                                     - 01 : Inevasa             *
      *                                     - 02 : Evasa               *
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
      *        * [bfr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfr"                          .

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
      *    * ga ordine fornitore, di :                                 *
      *    *                                                           *
      *    *    - Quantita' in ordine                                  *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       01  w-rof-ord.
      *        *-------------------------------------------------------*
      *        * Parametri in Output                                   *
      *        *-------------------------------------------------------*
           05  w-rof-ord-pxo.
      *            *---------------------------------------------------*
      *            * Quantita' in ordine                               *
      *            *---------------------------------------------------*
               10  w-rof-ord-qta-ord      pic s9(08)v9(03)            .
               10  w-rof-ord-fda-ord      pic s9(08)v9(03)            .

      *    *===========================================================*
      *    * Work-area per la determinazione, relativamente ad una ri- *
      *    * ga ordine fornitore, di :                                 *
      *    *                                                           *
      *    *    - Quantita' ricevuta                                   *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       01  w-rof-gsp.
      *        *-------------------------------------------------------*
      *        * Parametri in Output                                   *
      *        *-------------------------------------------------------*
           05  w-rof-gsp-pxo.
      *            *---------------------------------------------------*
      *            * Quantita' ricevuta                                *
      *            *---------------------------------------------------*
               10  w-rof-gsp-qta-ric      pic s9(08)v9(03)            .
               10  w-rof-gsp-fda-ric      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Flag per riga di addebito                         *
      *            *---------------------------------------------------*
               10  w-rof-gsp-flg-rda      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Estremi ultimo ricevimento                        *
      *            *---------------------------------------------------*
               10  w-rof-gsp-tip-doc      pic  x(05)                  .
               10  w-rof-gsp-dat-reg      pic  9(07)                  .
               10  w-rof-gsp-num-prt      pic  9(11)                  .

      *    *===========================================================*
      *    * Work-area per la determinazione, relativamente ad una ri- *
      *    * ga ordine fornitore, di :                                 *
      *    *                                                           *
      *    *    - Quantita' in corso di ricevimento                    *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       01  w-rof-icr.
      *        *-------------------------------------------------------*
      *        * Parametri in Output                                   *
      *        *-------------------------------------------------------*
           05  w-rof-icr-pxo.
      *            *---------------------------------------------------*
      *            * Quantita' in corso di ricevimento                 *
      *            *---------------------------------------------------*
               10  w-rof-icr-qta-icr      pic s9(08)v9(03)            .

      *    *===========================================================*
      *    * Work-area per la selezione sul record [bfr] relativamente *
      *    * al record di [ofr], per compatibilita'                    *
      *    *-----------------------------------------------------------*
       01  w-sel-bfr-ofr.
      *        *-------------------------------------------------------*
      *        * Parametri in Output                                   *
      *        *-------------------------------------------------------*
           05  w-sel-bfr-ofr-pxo.
      *            *---------------------------------------------------*
      *            * Flag su esito della selezione                     *
      *            *  - Spaces : Selezione superata                    *
      *            *  - #      : Selezione non superata                *
      *            *---------------------------------------------------*
               10  w-sel-bfr-ofr-flg      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/no documento in attesa di verifica                 *
      *        *-------------------------------------------------------*
           05  w-prs-snx-dav              pic  x(01)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine fornitore                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dqevrof0.dtl"                   .

      *    *-----------------------------------------------------------*
      *    * [ofr]                                                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofr"                          .

      ******************************************************************
       Procedure Division                using d-qev-rof
                                               rf-ofr                 .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   d-qev-rof-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           if        d-qev-rof-tip-ope    =    "DT"
                     perform det-qev-rof-000
                                          thru det-qev-rof-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   d-qev-rof-tip-ope    =    "OP"
                     perform rou-opn-fls-000
                                          thru rou-opn-fls-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   d-qev-rof-tip-ope    =    "CL"
                     perform rou-cls-fls-000
                                          thru rou-cls-fls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   d-qev-rof-tip-ope    =    "C?"
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
           move      zero                 to   d-qev-rof-dat-rif      .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di documento da verificare *
      *              *-------------------------------------------------*
           move      spaces               to   d-qev-rof-flg-dav      .
      *              *-------------------------------------------------*
      *              * Incremento contatore Open modulo                *
      *              *-------------------------------------------------*
           add       1                    to   w-nol-ctr-opn          .
       rou-opn-fls-050.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/no documento in attesa di verifica       *
      *                  *---------------------------------------------*
           perform   prs-snx-dav-000      thru prs-snx-dav-999        .
       rou-opn-fls-100.
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [bfr]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/no documento in attesa di  *
      *    *                             verifica                      *
      *    *-----------------------------------------------------------*
       prs-snx-dav-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/bfo/mov/bfo300[snx-dav]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-dav
           else      move  spaces         to   w-prs-snx-dav          .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-dav        not   = "S"
                     move  "N"            to   w-prs-snx-dav          .
       prs-snx-dav-999.
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
      *                  * [bfr]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
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
                     move  spaces         to   d-qev-rof-exi-sts
           else      move  "#"            to   d-qev-rof-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Determinazione status riga ordine                         *
      *    *-----------------------------------------------------------*
       det-qev-rof-000.
      *              *-------------------------------------------------*
      *              * Si/no trasformazione unita' di misura           *
      *              *-------------------------------------------------*
           move      rf-ofr-snx-tum       to   d-qev-rof-snx-tum      .
      *              *-------------------------------------------------*
      *              * Tipo riga in comodo ridefinito                  *
      *              *-------------------------------------------------*
           move      rf-ofr-tip-rig       to   w-rdf-tip-rig-wtr      .
      *              *-------------------------------------------------*
      *              * Normalizzazione estremi ultimo ricevimento      *
      *              *-------------------------------------------------*
           move      zero                 to   d-qev-rof-tip-urc      .
           move      spaces               to   d-qev-rof-tdo-urc      .
           move      zero                 to   d-qev-rof-drg-urc      .
           move      zero                 to   d-qev-rof-prt-urc      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminare                     *
      *              *-------------------------------------------------*
           move      zero                 to   d-qev-rof-qta-ord      .
           move      zero                 to   d-qev-rof-fda-ord      .
           move      zero                 to   d-qev-rof-qta-dri      .
           move      zero                 to   d-qev-rof-fda-dri      .
      *              *-------------------------------------------------*
      *              * Determinazione della quantita' in ordine        *
      *              *-------------------------------------------------*
           perform   rpd-rof-ord-000      thru rpd-rof-ord-999        .
           move      w-rof-ord-qta-ord    to   d-qev-rof-qta-ord      .
           move      w-rof-ord-fda-ord    to   d-qev-rof-fda-ord      .
       det-qev-rof-200.
      *              *-------------------------------------------------*
      *              * Determinazione della quantita' ricevuta         *
      *              *-------------------------------------------------*
           perform   rpd-rof-ric-000      thru rpd-rof-ric-999        .
           move      w-rof-gsp-qta-ric    to   d-qev-rof-qta-ric      .
           move      w-rof-gsp-fda-ric    to   d-qev-rof-fda-ric      .
      *                  *---------------------------------------------*
      *                  * Aggiornamento estremi ultimo ricevimento    *
      *                  *---------------------------------------------*
           if        w-rof-gsp-tip-doc    =    spaces and
                     w-rof-gsp-dat-reg    =    zero   and
                     w-rof-gsp-num-prt    =    zero
                     go to det-qev-rof-300.
           move      01                   to   d-qev-rof-tip-urc      .
           move      w-rof-gsp-tip-doc    to   d-qev-rof-tdo-urc      .
           move      w-rof-gsp-dat-reg    to   d-qev-rof-drg-urc      .
           move      w-rof-gsp-num-prt    to   d-qev-rof-prt-urc      .
       det-qev-rof-300.
      *              *-------------------------------------------------*
      *              * Determinazione della quantita' in corso di ri-  *
      *              * cevimento                                       *
      *              *-------------------------------------------------*
           perform   rpd-rof-icr-000      thru rpd-rof-icr-999        .
           move      w-rof-icr-qta-icr    to   d-qev-rof-qta-icr      .
           move      w-rof-icr-qta-icr    to   d-qev-rof-fda-icr      .
       det-qev-rof-400.
      *              *-------------------------------------------------*
      *              * Determinazione della quantita' ancora da rice-  *
      *              * vere                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se riga di Addebito                    *
      *                  *---------------------------------------------*
           if        w-rdf-tip-rig-wtp    not  = "A"
                     go to det-qev-rof-405.
      *                  *---------------------------------------------*
      *                  * Determinazione status confrontando i flag   *
      *                  * per riga di addebito determinati dalle rou- *
      *                  * tines                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se attivo quello relativo alla determi- *
      *                      * nazione della quantita' gia' ricevuta : *
      *                      * status di uscita a 'Evasa'              *
      *                      *-----------------------------------------*
           if        w-rof-gsp-flg-rda    not  = spaces
                     move  02             to   d-qev-rof-sts-rda
                     go to det-qev-rof-999.
      *                      *-----------------------------------------*
      *                      * Altrimenti : status di uscita a 'Ineva- *
      *                      * sa'                                     *
      *                      *-----------------------------------------*
           move      01                   to   d-qev-rof-sts-rda      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     det-qev-rof-999.
       det-qev-rof-405.
      *              *-------------------------------------------------*
      *              * Determinazione della quantita' ancora da rice-  *
      *              * vere                                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se riga chiusa : quantita' da ricevere a    *
      *                  * zero                                        *
      *                  *---------------------------------------------*
           if        rf-ofr-flg-rch       not  = spaces
                     move  zero           to   d-qev-rof-qta-dri
                     move  zero           to   d-qev-rof-fda-dri
                     go to det-qev-rof-999.
      *                  *---------------------------------------------*
      *                  * Se riga comunque da considerarsi saldata :  *
      *                  * quantita' da ricevere a zero                *
      *                  *---------------------------------------------*
           if        rf-ofr-sdr-ccs       not  = spaces
                     move  zero           to   d-qev-rof-qta-dri
                     move  zero           to   d-qev-rof-fda-dri
                     go to det-qev-rof-999.
      *                  *---------------------------------------------*
      *                  * Se quantita' ordinata a zero : quantita' da *
      *                  * ricevere a zero                             *
      *                  *---------------------------------------------*
           if        d-qev-rof-qta-ord    =    zero
                     move  zero           to   d-qev-rof-qta-dri
                     move  zero           to   d-qev-rof-fda-dri
                     go to det-qev-rof-999.
       det-qev-rof-410.
      *                  *---------------------------------------------*
      *                  * Differenza algebrica tra :                  *
      *                  *   - Quantita' in ordine                     *
      *                  * e                                           *
      *                  *   - Quantita' gia' ricevuta                 *
      *                  *   - Quantita' in corso di ricevimento       *
      *                  *---------------------------------------------*
           move      d-qev-rof-qta-ord    to   d-qev-rof-qta-dri      .
           move      d-qev-rof-fda-ord    to   d-qev-rof-fda-dri      .
      *
           subtract  d-qev-rof-qta-ric    from d-qev-rof-qta-dri      .
           subtract  d-qev-rof-fda-ric    from d-qev-rof-fda-dri      .
      *
           subtract  d-qev-rof-qta-icr    from d-qev-rof-qta-dri      .
           subtract  d-qev-rof-fda-icr    from d-qev-rof-fda-dri      .
      *                  *---------------------------------------------*
      *                  * Riduzione a zero del residuo eventualmente  *
      *                  * negativo, tenendo conto del segno algebri-  *
      *                  * co della quantita' in ordine                *
      *                  *---------------------------------------------*
           if        d-qev-rof-qta-ord    >    zero and
                     d-qev-rof-qta-dri    <    zero
                     move  zero           to   d-qev-rof-qta-dri
                                               d-qev-rof-fda-dri      .
      *
           if        d-qev-rof-qta-ord    <    zero and
                     d-qev-rof-qta-dri    >    zero
                     move  zero           to   d-qev-rof-qta-dri
                                               d-qev-rof-fda-dri      .
       det-qev-rof-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione, relativamente ad una riga  *
      *    * ordine fornitore, di :                                    *
      *    *                                                           *
      *    *    - Quantita' in ordine                                  *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *    - Record di [ofr] relativo alla riga ordine fornitore  *
      *    *      a disposizione in rf-ofr                             *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *    - w-rof-ord-qta-ord : Quantita' in ordine              *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       rpd-rof-ord-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-rof-ord-qta-ord      .
           move      zero                 to   w-rof-ord-fda-ord      .
      *              *-------------------------------------------------*
      *              * In funzione del tipo trasformazione per l'uni-  *
      *              * ta' di misura                                   *
      *              *-------------------------------------------------*
           if        rf-ofr-snx-tum       =    "P"
                     move   rf-ofr-qta-ord
                                          to   w-rof-ord-qta-ord
                     move   rf-ofr-qta-fda
                                          to   w-rof-ord-fda-ord
           else      move   rf-ofr-qta-fda
                                          to   w-rof-ord-qta-ord
                     move   rf-ofr-qta-ord
                                          to   w-rof-ord-fda-ord      .
       rpd-rof-ord-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione, relativamente ad una riga  *
      *    * ordine fornitore, di :                                    *
      *    *                                                           *
      *    *    - Quantita' ricevuta                                   *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *    - Record di [ofr] relativo alla riga ordine fornitore  *
      *    *      a disposizione in rf-ofr                             *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *    - w-rof-gsp-qta-ric : Quantita' ricevuta               *
      *    *    - w-rof-gsp-tip-doc : Tipo documento ultimo ricevimen- *
      *    *                          to                               *
      *    *    - w-rof-gsp-dat-reg : Data registrazione ultimo rice-  *
      *    *                          vimento                          *
      *    *    - w-rof-gsp-num-prt : Numero protocollo ultimo ricevi- *
      *    *                          mento                            *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       rpd-rof-ric-000.
      *              *-------------------------------------------------*
      *              * Inizializzazioni preliminari                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag per riga di addebito   *
      *                  *---------------------------------------------*
           move      spaces               to   w-rof-gsp-flg-rda      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione estremi ultimo ricevimento  *
      *                  *---------------------------------------------*
           move      spaces               to   w-rof-gsp-tip-doc      .
           move      zero                 to   w-rof-gsp-dat-reg      .
           move      zero                 to   w-rof-gsp-num-prt      .
      *                  *---------------------------------------------*
      *                  * Quantita' gia' spedita : a zero             *
      *                  *---------------------------------------------*
           move      zero                 to   w-rof-gsp-qta-ric      .
           move      zero                 to   w-rof-gsp-fda-ric      .
       rpd-rof-ric-100.
      *              *-------------------------------------------------*
      *              * Start iniziale su [bfr]                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RIFORF    "         to   f-key                  .
           move      rf-ofr-cod-dpz       to   rf-bfr-cod-dpz         .
           move      rf-ofr-num-prt       to   rf-bfr-orf-prt         .
           move      rf-ofr-num-prg       to   rf-bfr-orf-prg         .
           move      zero                 to   rf-bfr-num-prt         .
           move      zero                 to   rf-bfr-num-prg         .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rpd-rof-ric-900.
       rpd-rof-ric-200.
      *              *-------------------------------------------------*
      *              * Read Next su [bfr]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Read Next                                   *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                  *---------------------------------------------*
      *                  * Se At end : uscita                          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to rpd-rof-ric-900.
       rpd-rof-ric-300.
      *              *-------------------------------------------------*
      *              * Test Max su [bfr]                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su :                                   *
      *                  *    - Codice dipendenza                      *
      *                  *    - Numero protocollo ordine fornitore     *
      *                  *    - Numero riga ordine fornitore           *
      *                  *---------------------------------------------*
           if        rf-bfr-cod-dpz       not  = rf-ofr-cod-dpz or
                     rf-bfr-orf-prt       not  = rf-ofr-num-prt or
                     rf-bfr-orf-prg       not  = rf-ofr-num-prg
                     go to rpd-rof-ric-900.
       rpd-rof-ric-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record di [bfr] rispetto al re-   *
      *              * cord di [ofr]                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selezione                                   *
      *                  *---------------------------------------------*
           perform   sel-bfr-ofr-000      thru sel-bfr-ofr-999        .
      *                  *---------------------------------------------*
      *                  * Se selezione non superata : si ignora il    *
      *                  * record letto                                *
      *                  *---------------------------------------------*
           if        w-sel-bfr-ofr-flg    not  = spaces
                     go to rpd-rof-ric-800.
       rpd-rof-ric-500.
      *              *-------------------------------------------------*
      *              * Considerazioni sul record di [bfr] letto e se-  *
      *              * lezionato rispetto ad [ofr]                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento estremi ultimo ricevimento    *
      *                  *---------------------------------------------*
           if        rf-bfr-dat-reg       not  < w-rof-gsp-dat-reg
                     move  rf-bfr-cod-tmb to   w-rof-gsp-tip-doc
                     move  rf-bfr-dat-reg to   w-rof-gsp-dat-reg
                     move  rf-bfr-num-prt to   w-rof-gsp-num-prt      .
      *                  *---------------------------------------------*
      *                  * Se tipo riga 'A' : attivazione flag per     *
      *                  * riga di addebito e uscita                   *
      *                  *---------------------------------------------*
           if        w-rdf-tip-rig-wtp    =    "A"
                     move  "#"            to   w-rof-gsp-flg-rda
                     go to rpd-rof-ric-900.
      *                  *---------------------------------------------*
      *                  * Si somma la 'Quantita' acquistata contenuta *
      *                  * nel record alla 'Quantita' ricevuta' che si *
      *                  * sta determinando, in funzione del tipo tra- *
      *                  * sformazione unita' di misura                *
      *                  *---------------------------------------------*
           if        rf-bfr-snx-tum       =    "P"
                     add    rf-bfr-qta-acq
                                          to   w-rof-gsp-qta-ric
                     add    rf-bfr-qta-fda
                                          to   w-rof-gsp-fda-ric
           else      add    rf-bfr-qta-fda
                                          to   w-rof-gsp-fda-ric
                     add    rf-bfr-qta-acq
                                          to   w-rof-gsp-qta-ric      .
       rpd-rof-ric-800.
      *              *-------------------------------------------------*
      *              * Riciclo a Read Next su [bfr]                    *
      *              *-------------------------------------------------*
           go to     rpd-rof-ric-200.
       rpd-rof-ric-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rpd-rof-ric-999.
       rpd-rof-ric-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione, relativamente ad una riga  *
      *    * ordine fornitore, di :                                    *
      *    *                                                           *
      *    *    - Quantita' in corso di ricevimento                    *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *    - Record di [ofr] relativo alla riga ordine fornitore  *
      *    *      a disposizione in rf-ofr                             *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *    - w-rof-icr-qta-icr : Quantita' in corso di ricevimen- *
      *    *                          to                               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       rpd-rof-icr-000.
      *              *-------------------------------------------------*
      *              * Quantita' in corso di ricevimento : a zero      *
      *              *-------------------------------------------------*
           move      zero                 to   w-rof-icr-qta-icr      .
       rpd-rof-icr-999.
           exit.

      *    *===========================================================*
      *    * Routine per la selezione sul record [bfr] relativamente   *
      *    * al record di [ofr], per compatibilita'                    *
      *    *                                                           *
      *    * Input  :                                                  *
      *    *                                                           *
      *    *    - Record di [bfr] relativo alla riga bolla fornitore   *
      *    *      a disposizione in rf-bfr                             *
      *    *                                                           *
      *    *    - Record di [ofr] relativo alla riga ordine fornitore  *
      *    *      a disposizione in rf-ofr                             *
      *    *                                                           *
      *    * Output :                                                  *
      *    *                                                           *
      *    *    - w-sel-bfr-ofr-flg : Esito della selezione            *
      *    *                            - Spaces : Ok                  *
      *    *                            - #      : Ko                  *
      *    *                                                           *
      *    *-----------------------------------------------------------*
       sel-bfr-ofr-000.
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo archivio                    *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bfr-tip-arc       not  = rf-ofr-tip-arc
                     go to sel-bfr-ofr-900.
       sel-bfr-ofr-025.
      *              *-------------------------------------------------*
      *              * Selezione su : Codice archivio                  *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bfr-cod-arc       not  = rf-ofr-cod-arc
                     go to sel-bfr-ofr-900.
       sel-bfr-ofr-050.
      *              *-------------------------------------------------*
      *              * Selezione su : Codice dipendenza dell'archivio  *
      *              *                                                 *
      *              * Non viene fatta alcuna selezione su questo cam- *
      *              * po, in quanto si permette di spedire ad una di- *
      *              * pendenza fornitore diversa da quanto stabilito  *
      *              * in ordine fornitore.                            *
      *              *-------------------------------------------------*
       sel-bfr-ofr-075.
      *              *-------------------------------------------------*
      *              * Selezione su : Flag blocco di documento         *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *                                                 *
      *              * CAMPI MAI GESTITI                               *
      *              *-------------------------------------------------*
______*    if        rf-bfr-bld-flb       not  = rf-ofr-bld-flb
______*              go to sel-bfr-ofr-900.
       sel-bfr-ofr-100.
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo riga                        *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bfr-tip-rig       not  = rf-ofr-tip-rig
                     go to sel-bfr-ofr-900.
       sel-bfr-ofr-125.
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo codice di magazzino         *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bfr-tip-mag       not  = rf-ofr-tip-mag
                     go to sel-bfr-ofr-900.
       sel-bfr-ofr-150.
      *              *-------------------------------------------------*
      *              * Selezione su : Codice prodotto numerico         *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bfr-num-mag       not  = rf-ofr-num-mag
                     go to sel-bfr-ofr-900.
       sel-bfr-ofr-175.
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
       sel-bfr-ofr-200.
      *              *-------------------------------------------------*
      *              * Selezione su : segnale di si/no documento da    *
      *              *                verificare                       *
      *              *                                                 *
      *              * La selezione puo' essere disattivata da un ap-  *
      *              * posito flag in input                            *
      *              *                                                 *
      *              * Questa selezione vale solamente se attiva la    *
      *              * apposita personalizzazione                      *
      *              *-------------------------------------------------*
           if        w-prs-snx-dav        not   = "S"
                     go to sel-bfr-ofr-225.
           if        d-qev-rof-flg-dav    =    "#"
                     go to sel-bfr-ofr-225.
           if        rf-bfr-flg-nbx (01)  not  = spaces
                     go to sel-bfr-ofr-900.
       sel-bfr-ofr-225.
      *              *-------------------------------------------------*
      *              * Selezione su : Tipo prodotto                    *
      *              *                                                 *
      *              * Deve essere pari a quello della riga ordine che *
      *              * si sta esaminando.                              *
      *              *-------------------------------------------------*
           if        rf-bfr-tip-pro       not  = rf-ofr-tip-pro
                     go to sel-bfr-ofr-900.
       sel-bfr-ofr-250.
      *              *-------------------------------------------------*
      *              * Selezione su : Quantita' acquistata             *
      *              *                                                 *
      *              * Deve essere diversa da Zero, non si prendono in *
      *              * considerazione righe bolla con quantita' a Ze-  *
      *              * ro.                                             *
      *              *-------------------------------------------------*
           if        rf-bfr-snx-tum       =    "P"
                     if    rf-bfr-qta-acq =    zero
                           go to sel-bfr-ofr-900
                     else  go to sel-bfr-ofr-275
           else      if    rf-bfr-qta-fda =    zero
                           go to sel-bfr-ofr-900
                     else  go to sel-bfr-ofr-275.
       sel-bfr-ofr-275.
      *              *-------------------------------------------------*
      *              * Selezione su : Data di riferimento              *
      *              *-------------------------------------------------*
           if        d-qev-rof-dat-rif    =    zero
                     go to sel-bfr-ofr-800.
           if        rf-bfr-dat-doc       >    d-qev-rof-dat-rif
                     go to sel-bfr-ofr-900.
       sel-bfr-ofr-800.
      *              *-------------------------------------------------*
      *              * Uscita per selezione : Ok                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-sel-bfr-ofr-flg      .
           go to     sel-bfr-ofr-999.
       sel-bfr-ofr-900.
      *              *-------------------------------------------------*
      *              * Uscita per selezione : Ko                       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-sel-bfr-ofr-flg      .
           go to     sel-bfr-ofr-999.
       sel-bfr-ofr-999.
           exit.

