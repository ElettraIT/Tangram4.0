       Identification Division.
       Program-Id.                                 pbol300f           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bol                 *
      *                                Settore:    mov                 *
      *                                   Fase:    bol300f             *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/07/92    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Duplicazione righe da bolla di acquisto     *
      *                                                                *
      *                    Richiamata in bol300 da funzione 'PF1'      *
      *                                                                *
      *================================================================*
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *        Input  : l-dup-rba-tip-ope = "OP"                       *
      *                 l-dup-rba-cod-dpz = codice dipendenza in uso   *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *        Input  : l-dup-rba-tip-ope = "CL"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *        Input  : l-dup-rba-tip-ope = "C?"                       *
      *                                                                *
      *        Output : l-dup-rba-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * "RB" - Accettazione dati identificativi bolla fornitore        *
      *                                                                *
      *        Input  : l-dup-rba-tip-ope = "RB"                       *
      *                 l-dup-rba-tmo-bfo = codice tipo movimento bol- *
      *                                     le fornitori da proporre   *
      *                                     come default               *
      *                                                                *
      *        Output : l-dup-rba-exi-sts = spaces: Accettazione ese-  *
      *                                             guita con successo *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "SC" - Saldaconto per selezione righe                          *
      *                                                                *
      *        Input  : l-dup-rba-tip-ope = "SC"                       *
      *                                                                *
      *        Output : l-dup-rba-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "CC" - Caricamento righe ordine selezionate in catena          *
      *                                                                *
      *        Input  : l-dup-rba-tip-ope = "CC"                       *
      *                                                                *
      *        Output : nessuno                                        *
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
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mvideo"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Record files area 'bfo'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [bft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbft"                          .
      *        *-------------------------------------------------------*
      *        * [bfr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfr"                          .
      *        *-------------------------------------------------------*
      *        * [bfx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfx"                          .
      *        *-------------------------------------------------------*
      *        * [ybf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfybf"                          .

      *    *===========================================================*
      *    * Record files area 'dcf'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [aaf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaf"                          .

      *    *===========================================================*
      *    * Record files area 'dcc'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [vlt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfvlt"                          .
      *        *-------------------------------------------------------*
      *        * [zvl]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvl"                          .

      *    *===========================================================*
      *    * Record files area 'dcp'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .

      *    *===========================================================*
      *    * Record files area 'cge'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .

      *    *===========================================================*
      *    * Record files area 'gep'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [cbp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcbp"                          .

      *    *===========================================================*
      *    * Record files area 'abi'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [axs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxs"                          .
      *        *-------------------------------------------------------*
      *        * [axi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxi"                          .

      *    *===========================================================*
      *    * Record files area 'age'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [age]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/age/fls/rec/rfage"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs-rac.
      *        *-------------------------------------------------------*
      *        * Personalizzazioni per la riga di scroll               *
      *        *-------------------------------------------------------*
           05  w-prs-rac-scr.
      *            *---------------------------------------------------*
      *            * Esposizione della descrizione                     *
      *            *                                                   *
      *            *   - 00 : Descrizione completa                     *
      *            *                                                   *
      *            *   - 01 : Primi 25 caratteri della descrizione     *
      *            *          Codice                                   *
      *            *                                                   *
      *            *   - 02 : Primi 21 caratteri della descrizione     *
      *            *          Tipo codice                              *
      *            *          Codice                                   *
      *            *                                                   *
      *            *   - 03 : Codice                                   *
      *            *          Primi 25 caratteri della descrizione     *
      *            *                                                   *
      *            *   - 04 : Tipo codice                              *
      *            *          Codice                                   *
      *            *          Primi 21 caratteri della descrizione     *
      *            *                                                   *
      *            *   - 05 : Codice                                   *
      *            *                                                   *
      *            *   - 06 : Tipo codice                              *
      *            *          Codice                                   *
      *            *                                                   *
      *            *   - 11 : Codice del fornitore                     *
      *            *          Codice                                   *
      *            *                                                   *
      *            *   - 12 : Codice del fornitore                     *
      *            *          Tipo codice                              *
      *            *          Codice                                   *
      *            *                                                   *
      *            *   - 13 : Codice                                   *
      *            *          Codice del fornitore                     *
      *            *                                                   *
      *            *   - 14 : Tipo codice                              *
      *            *          Codice                                   *
      *            *          Codice del fornitore                     *
      *            *                                                   *
      *            *---------------------------------------------------*
               10  w-prs-rac-scr-des      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Esposizione della colonna residuo                 *
      *            *                                                   *
      *            *   - 00 : Quantita' residua                        *
      *            *                                                   *
      *            *   - 01 : Data consegna richiesta e prevista       *
      *            *                                                   *
      *            * N.B.: Significativo solo per programma bol300a    *
      *            *---------------------------------------------------*
               10  w-prs-rac-scr-res      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Work per manipolazione valore personalizzazione   *
      *            *---------------------------------------------------*
               10  w-prs-rac-scr-str      pic  x(05)                  .
               10  w-prs-rac-scr-str-r redefines
                   w-prs-rac-scr-str.
                   15  w-prs-rac-scr-aaa  pic  9(02)                  .
                   15  filler             pic  x(01)                  .
                   15  w-prs-rac-scr-bbb  pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Work-area generica per il programma                       *
      *    *-----------------------------------------------------------*
       01  w-gen.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-gen-ctr-opn              pic s9(05) value zero       .
      *        *-------------------------------------------------------*
      *        * Numero protocollo bolla fornitore in corso di tratta- *
      *        * mento                                                 *
      *        *-------------------------------------------------------*
           05  w-gen-prt-bfo              pic  9(11)                  .

      *    *===========================================================*
      *    * Work-area per campi di accettazione                       *
      *    *-----------------------------------------------------------*
       01  w-acc.
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  w-acc-cod-dpz              pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo                                     *
      *        *-------------------------------------------------------*
           05  w-acc-num-prt              pic  9(11)                  .
           05  w-acc-num-prt-r redefines
               w-acc-num-prt.
               10  w-acc-num-prt-saa      pic  9(03)                  .
               10  w-acc-num-prt-dpz      pic  9(02)                  .
               10  w-acc-num-prt-prg      pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Data documento                                        *
      *        *-------------------------------------------------------*
           05  w-acc-dat-doc              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero documento                                      *
      *        *-------------------------------------------------------*
           05  w-acc-num-doc              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Codice archivio documento richiamato                  *
      *        *-------------------------------------------------------*
           05  w-acc-cod-arc              pic  9(07)                  .

      *    *===========================================================*
      *    * Work per ridefinizione numero protocollo per accettazione *
      *    *-----------------------------------------------------------*
       01  w-rnp.
           05  w-rnp-num-prt-num      pic  9(09)                  .
           05  w-rnp-num-prt-num-r    redefines
               w-rnp-num-prt-num.
               10  w-rnp-num-prt-saa  pic  9(03)                  .
               10  w-rnp-num-prt-prg  pic  9(06)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-val-acc.
               10  filler   occurs 200    pic  x(01)                  .
           05  w-sav-qta-ffo              pic s9(10)v9(03)            .
           05  w-sav-flg-fzs              pic  x(01)                  .
           05  w-sav-snx-aoc              pic  x(01)                  .
           05  w-sav-prz-acq              pic  9(09)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione righe bolla fornitore       *
      *    *-----------------------------------------------------------*
       01  w-brb.
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella                            *
      *        *-------------------------------------------------------*
           05  w-brb-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi in tabella                    *
      *        *-------------------------------------------------------*
           05  w-brb-max-ele              pic  9(05) value 500        .
      *        *-------------------------------------------------------*
      *        * Tabella elementi                                      *
      *        *-------------------------------------------------------*
           05  w-brb-tbl.
               10  w-brb-sng-ele occurs 900.
                   15  w-brb-num-prg      pic  9(05)       comp-3     .
                   15  w-brb-tip-rig      pic  x(01)                  .
                   15  w-brb-des-rig      pic  x(40)                  .
                   15  w-brb-dec-qta      pic  9(01)                  .
                   15  w-brb-qta-bfo      pic s9(10)v9(03) comp-3     .
                   15  w-brb-qta-dup      pic s9(10)v9(03) comp-3     .
                   15  w-brb-flg-idr      pic  x(01)                  .
                   15  w-brb-flg-ids      pic  x(01)                  .
                   15  w-brb-flg-fzs      pic  x(01)                  .
                   15  w-brb-qta-res      pic s9(10)v9(03) comp-3     .
                   15  w-brb-snx-aoc      pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione valute per legame valutario *
      *    *-----------------------------------------------------------*
       01  w-vpl.
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella                            *
      *        *-------------------------------------------------------*
           05  w-vpl-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi in tabella                    *
      *        *-------------------------------------------------------*
           05  w-vpl-max-ele              pic  9(05) value 999        .
      *        *-------------------------------------------------------*
      *        * Tabella elementi                                      *
      *        *-------------------------------------------------------*
           05  w-vpl-tbl.
               10  w-vpl-sng-ele occurs 999.
                   15  w-vpl-sgl-vpl      pic  x(03)                  .
                   15  w-vpl-cdc-vpl      pic  9(06)v9(05)            .
                   15  w-vpl-cdc-rif      pic  9(06)v9(05)            .
                   15  w-vpl-dat-rif      pic  9(07)                  .
                   15  w-vpl-per-scs      pic s9(03)v9(01)            .
               10  w-vpl-ctr-001          pic  9(05)                  .
               10  w-vpl-ctr-002          pic  9(05)                  .
               10  w-vpl-wrk-scs          pic s9(08)v9(05)            .

      *    *===========================================================*
      *    * Work-area per funzione saldaconto                         *
      *    *-----------------------------------------------------------*
       01  w-sdc.
      *        *-------------------------------------------------------*
      *        * Flag di uscita da saldaconto                          *
      *        * - spaces : Saldaconto eseguito                        *
      *        * - #      : Uscita per Exit                            *
      *        *-------------------------------------------------------*
           05  w-sdc-flg-exi              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagine massimo                                 *
      *        *-------------------------------------------------------*
           05  w-sdc-npg-max              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagina attualmente visualizzato                *
      *        *-------------------------------------------------------*
           05  w-sdc-npg-vis              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Numero pagina da trattare                             *
      *        *-------------------------------------------------------*
           05  w-sdc-npg-dat              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Contatore righe saldaconto                            *
      *        *-------------------------------------------------------*
           05  w-sdc-ctr-rig              pic s9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work-area di appoggio                                 *
      *        *-------------------------------------------------------*
           05  w-sdc-wrk-rig              pic  9(03)                  .
           05  w-sdc-wrk-lin              pic  9(03)                  .
           05  w-sdc-wrk-rem              pic  9(03)                  .
           05  w-sdc-wrk-c01              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area di comodo per quantita' senza segno             *
      *    *-----------------------------------------------------------*
       01  w-qss.
           05  w-qss-qta-bfo              pic  9(10)v9(03)            .
           05  w-qss-qta-ffo              pic  9(10)v9(03)            .

      *    *===========================================================*
      *    * Work-area per caricamento righe ordine in catena          *
      *    *-----------------------------------------------------------*
       01  w-crc.
      *        *-------------------------------------------------------*
      *        * Contatore di lavoro                                   *
      *        *-------------------------------------------------------*
           05  w-crc-wrk-ctr              pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per routine buf-tes-bfo-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-buf-tes-bfo.
           05  w-buf-tes-bfo-ctr          pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per routine buf-rig-bfo-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-buf-rig-bfo.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *-------------------------------------------------------*
           05  w-buf-rig-bfo-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo bolla fornitore                     *
      *        *-------------------------------------------------------*
           05  w-buf-rig-bfo-prt          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per ridefinizione tipo riga                    *
      *        *-------------------------------------------------------*
           05  w-buf-rig-bfo-wtr.
               10  w-buf-rig-bfo-wtp      pic  x(01)                  .
               10  w-buf-rig-bfo-wtf      pic  x(01)                  .
               10  filler                 pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione in riga                                   *
      *        *-------------------------------------------------------*
           05  w-buf-rig-bfo-wde          pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per ridefinizione descrizione riga di scroll    *
      *    *-----------------------------------------------------------*
       01  w-des-scr.
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 00                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-000.
               10  w-des-scr-000-d40      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 01                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-001 redefines
               w-des-scr-000.
               10  w-des-scr-001-d25      pic  x(25)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-001-cod      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 02                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-002 redefines
               w-des-scr-000.
               10  w-des-scr-002-d21      pic  x(21)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-002-pqs      pic  x(01)                  .
               10  w-des-scr-002-tco      pic  x(01)                  .
               10  w-des-scr-002-pqd      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-002-cod      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 03                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-003 redefines
               w-des-scr-000.
               10  w-des-scr-003-cod      pic  x(14)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-003-d25      pic  x(25)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 04                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-004 redefines
               w-des-scr-000.
               10  w-des-scr-004-pqs      pic  x(01)                  .
               10  w-des-scr-004-tco      pic  x(01)                  .
               10  w-des-scr-004-pqd      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-004-cod      pic  x(14)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-004-d21      pic  x(21)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 05                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-005 redefines
               w-des-scr-000.
               10  filler                 pic  x(26)                  .
               10  w-des-scr-005-cod      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 06                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-006 redefines
               w-des-scr-000.
               10  filler                 pic  x(22)                  .
               10  w-des-scr-006-pqs      pic  x(01)                  .
               10  w-des-scr-006-tco      pic  x(01)                  .
               10  w-des-scr-006-pqd      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-006-cod      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 11                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-011 redefines
               w-des-scr-000.
               10  filler                 pic  x(03)                  .
               10  w-des-scr-011-pfp      pic  x(03)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-011-cdf      pic  x(14)                  .
               10  filler                 pic  x(05)                  .
               10  w-des-scr-011-cod      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 12                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-012 redefines
               w-des-scr-000.
               10  filler                 pic  x(01)                  .
               10  w-des-scr-012-pfp      pic  x(03)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-012-cdf      pic  x(14)                  .
               10  filler                 pic  x(03)                  .
               10  w-des-scr-012-pqs      pic  x(01)                  .
               10  w-des-scr-012-tco      pic  x(01)                  .
               10  w-des-scr-012-pqd      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-012-cod      pic  x(14)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 13                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-013 redefines
               w-des-scr-000.
               10  w-des-scr-013-cod      pic  x(14)                  .
               10  filler                 pic  x(04)                  .
               10  w-des-scr-013-pfp      pic  x(03)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-013-cdf      pic  x(14)                  .
               10  filler                 pic  x(04)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione : 14                                *
      *        *-------------------------------------------------------*
           05  w-des-scr-014 redefines
               w-des-scr-000.
               10  w-des-scr-014-pqs      pic  x(01)                  .
               10  w-des-scr-014-tco      pic  x(01)                  .
               10  w-des-scr-014-pqd      pic  x(01)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-014-cod      pic  x(14)                  .
               10  filler                 pic  x(02)                  .
               10  w-des-scr-014-pfp      pic  x(03)                  .
               10  filler                 pic  x(01)                  .
               10  w-des-scr-014-cdf      pic  x(14)                  .
               10  filler                 pic  x(02)                  .

      *    *===========================================================*
      *    * Work per routine dec-tip-rig-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-dec-tip-rig.
           05  w-dec-tip-rig-inx          pic  9(02)                  .
           05  w-dec-tip-rig-pnt          pic  9(02)                  .
           05  w-dec-tip-rig-str.
               10  w-dec-tip-rig-chr      occurs 05
                                          pic  x(01)                  .
           05  w-dec-tip-rig-cod.
               10  w-dec-tip-rig-num      occurs 03
                                          pic  9(01)                  .
           05  w-dec-tip-rig-c01          pic  9(02)                  .
           05  w-dec-tip-rig-c02          pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Tipo codice di magazzino                              *
      *        *-------------------------------------------------------*
           05  w-sav-tip-mag              pic  9(02)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo codice di magazzino                   *
      *        *-------------------------------------------------------*
           05  w-exp-tip-mag.
               10  w-exp-tip-mag-num      pic  9(02)       value 2    .
               10  w-exp-tip-mag-lun      pic  9(02)       value 20   .
               10  w-exp-tip-mag-tbl.
                   15  filler             pic  x(20) value
                            "Prodotto di vendita "                    .
                   15  filler             pic  x(20) value
                            "Materia prima       "                    .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [bft]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-bft.
               10  w-fnd-arc-bft-sel      pic  x(01)                  .
               10  w-fnd-arc-bft-dpz      pic  9(02)                  .
               10  w-fnd-arc-bft-fnt      pic  9(07)                  .
               10  w-fnd-arc-bft-dpf      pic  x(04)                  .
               10  w-fnd-arc-bft-tmb      pic  x(05)                  .
               10  w-fnd-arc-bft-dat      pic  9(07)                  .
               10  w-fnd-arc-bft-prt      pic  9(11)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ybf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ybf.
               10  w-let-arc-ybf-flg      pic  x(01)                  .
               10  w-let-arc-ybf-cod      pic  x(05)                  .
               10  w-let-arc-ybf-dpz      pic  9(02)                  .
               10  w-let-arc-ybf-des      pic  x(30)                  .
               10  w-let-arc-ybf-mif      pic  9(02)                  .
               10  w-let-arc-ybf-tmf      pic  x(05)                  .
               10  w-let-arc-ybf-acm      pic  x(01)                  .
               10  w-let-arc-ybf-dtc      pic  x(01)                  .
               10  w-let-arc-ybf-dct      pic  x(03)                  .
               10  w-let-arc-ybf-cam      pic  9(05)                  .
               10  w-let-arc-ybf-ctm      pic  x(03)                  .
               10  w-let-arc-ybf-dfa      pic  x(01)                  .
               10  w-let-arc-ybf-vaa      pic  x(01)                  .
               10  w-let-arc-ybf-lsa      pic  x(04)                  .
               10  w-let-arc-ybf-ord      pic  9(02)                  .
               10  w-let-arc-ybf-prd      pic  9(02)                  .
               10  w-let-arc-ybf-sgl      pic  x(03)                  .
               10  w-let-arc-ybf-maf      pic  9(02)                  .
               10  w-let-arc-ybf-dmf      pic  x(05)                  .
               10  w-let-arc-ybf-tvd      pic  x(01)                  .
               10  w-let-arc-ybf-dsl      pic  x(07)                  .
               10  w-let-arc-ybf-dtr      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [fnt]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-fnt.
               10  w-let-arc-fnt-flg      pic  x(01)                  .
               10  w-let-arc-fnt-cod      pic  9(07)                  .
               10  w-let-arc-fnt-rag      pic  x(40)                  .
               10  w-let-arc-fnt-via      pic  x(40)                  .
               10  w-let-arc-fnt-loc      pic  x(40)                  .
               10  w-let-arc-fnt-piv      pic  9(11)                  .
               10  w-let-arc-fnt-stc      pic  9(07)                  .
               10  w-let-arc-fnt-ass      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zvl]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zvl.
               10  w-let-arc-zvl-flg      pic  x(01)                  .
               10  w-let-arc-zvl-cod      pic  x(03)                  .
               10  w-let-arc-zvl-des      pic  x(20)                  .
               10  w-let-arc-zvl-din      pic  x(20)                  .
               10  w-let-arc-zvl-dec      pic  9(01)                  .
               10  w-let-arc-zvl-tdc      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [yfp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-yfp.
               10  w-let-arc-yfp-flg      pic  x(01)                  .
               10  w-let-arc-yfp-cod      pic  9(07)                  .
               10  w-let-arc-yfp-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cbp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cbp.
               10  w-let-arc-cbp-flg      pic  x(01)                  .
               10  w-let-arc-cbp-tip      pic  9(02)                  .
               10  w-let-arc-cbp-cod      pic  x(10)                  .
               10  w-let-arc-cbp-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [axi]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-axi.
               10  w-let-arc-axi-flg      pic  x(01)                  .
               10  w-let-arc-axi-cod      pic  9(05)                  .
               10  w-let-arc-axi-den      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [axs]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-axs.
               10  w-let-arc-axs-flg      pic  x(01)                  .
               10  w-let-arc-axs-abi      pic  9(05)                  .
               10  w-let-arc-axs-cab      pic  9(05)                  .
               10  w-let-arc-axs-den      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [yin]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-yin.
               10  w-let-arc-yin-flg      pic  x(01)                  .
               10  w-let-arc-yin-cod      pic  x(03)                  .
               10  w-let-arc-yin-tpg      pic  9(02)                  .
               10  w-let-arc-yin-des      pic  x(40)                  .
               10  w-let-arc-yin-coi      pic  9(05)                  .
               10  w-let-arc-yin-ccp      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [ybo]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ybo.
               10  w-let-arc-ybo-flg      pic  x(01)                  .
               10  w-let-arc-ybo-cod      pic  x(03)                  .
               10  w-let-arc-ybo-tpg      pic  9(02)                  .
               10  w-let-arc-ybo-des      pic  x(40)                  .
               10  w-let-arc-ybo-coi      pic  9(05)                  .
               10  w-let-arc-ybo-ccp      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcp.
               10  w-let-arc-dcp-flg      pic  x(01)                  .
               10  w-let-arc-dcp-cod      pic  9(07)                  .
               10  w-let-arc-dcp-alf      pic  x(14)                  .
               10  w-let-arc-dcp-des.
                   15  w-let-arc-dcp-drg occurs 10
                                          pic  x(40)                  .
               10  w-let-arc-dcp-dui      pic  x(40)                  .
               10  w-let-arc-dcp-tpr      pic  9(02)                  .
               10  w-let-arc-dcp-civ      pic  9(05)                  .
               10  w-let-arc-dcp-umi      pic  x(03)                  .
               10  w-let-arc-dcp-deq      pic  9(01)                  .
               10  w-let-arc-dcp-plb      pic  9(09)                  .
               10  w-let-arc-dcp-ctr      pic  9(02)                  .
               10  w-let-arc-dcp-lng      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [aaf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-aaf.
               10  w-let-arc-aaf-flg      pic  x(01)                  .
               10  w-let-arc-aaf-tpm      pic  9(02)                  .
               10  w-let-arc-aaf-cdm      pic  9(07)                  .
               10  w-let-arc-aaf-fnt      pic  9(07)                  .
               10  w-let-arc-aaf-fda      pic  x(14)                  .
               10  w-let-arc-aaf-csf      pic  x(14)                  .
               10  w-let-arc-aaf-dmg.
                   15  w-let-arc-aaf-drm occurs 10
                                          pic  x(40)                  .
               10  w-let-arc-aaf-des.
                   15  w-let-arc-aaf-drg occurs 10
                                          pic  x(40)                  .
               10  w-let-arc-aaf-snu      pic  x(01)                  .
               10  w-let-arc-aaf-umf      pic  x(03)                  .
               10  w-let-arc-aaf-ndu      pic  9(01)                  .
               10  w-let-arc-aaf-cmu      pic  9(06)v9(03)            .
               10  w-let-arc-aaf-cdu      pic  9(06)v9(03)            .
               10  w-let-arc-aaf-tpa      pic  9(02)                  .
               10  w-let-arc-aaf-vlt      pic  x(03)                  .
               10  w-let-arc-aaf-dcv      pic  9(01)                  .
               10  w-let-arc-aaf-ndp      pic  9(01)                  .
               10  w-let-arc-aaf-lda      pic  9(06)v9(03)            .
               10  w-let-arc-aaf-tap      pic  9(02)                  .
               10  w-let-arc-aaf-lvv      pic  x(03)                  .
               10  w-let-arc-aaf-lvd      pic  9(01)                  .
               10  w-let-arc-aaf-lvt      pic  x(01)                  .
               10  w-let-arc-aaf-lvc      pic  9(06)v9(05)            .
               10  w-let-arc-aaf-lvp      pic  9(01)v9(02)            .
               10  w-let-arc-aaf-mpa      pic  9(02)v9(01)            .
               10  w-let-arc-aaf-tbp.
                   15  w-let-arc-aaf-etp occurs 06.
                       20  w-let-arc-aaf-qtp
                                          pic  9(06)v9(03)            .
                       20  w-let-arc-aaf-pzp
                                          pic  9(09)                  .
                       20  w-let-arc-aaf-csp
                                          pic  9(05)                  .
                       20  w-let-arc-aaf-psp occurs 05
                                          pic  9(02)v9(01)            .
               10  w-let-arc-aaf-ctr      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [bfx]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-bfx.
               10  w-let-arc-bfx-flg      pic  x(01)                  .
               10  w-let-arc-bfx-prt      pic  9(11)                  .
               10  w-let-arc-bfx-prg      pic  9(05)                  .
               10  w-let-arc-bfx-trc      pic  9(02)                  .
               10  w-let-arc-bfx-des.
                   15  w-let-arc-bfx-drg occurs 10
                                          pic  x(40)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [zci]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.ltw"                   .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det documento gia' esistente oppure no       *
      *        *-------------------------------------------------------*
           05  w-det-doc-ges.
      *            *---------------------------------------------------*
      *            * Esito della determinazione                        *
      *            * - S : Si, documento del tipo movimento cercato e  *
      *            *       e con data e numero cercato esistente in    *
      *            *       archivio                                    *
      *            * - N : No, documento del tipo movimento cercato e  *
      *            *       e con data e numero cercato non esistente   *
      *            *       in archivio                                 *
      *            * - X : No, documento del tipo movimento cercato e  *
      *            *       e con data e numero cercato non esistente   *
      *            *       in archivio, ma esistenza di un documento   *
      *            *       con data e numero cercato, con stesso co-   *
      *            *       dice numerazione, ma con diverso tipo mo-   *
      *            *       vimento da quello cercato                   *
      *            *---------------------------------------------------*
               10  w-det-doc-ges-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo movimento trovato per valore precedente de-  *
      *            * terminato a 'X'                                   *
      *            *---------------------------------------------------*
               10  w-det-doc-ges-tmt      pic  x(05)                  .

      *    *===========================================================*
      *    * Work per determinazione quantita' secondaria              *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/prg/cpy/wdetqts0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing quantita' da incolonnare  *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wkl"                   .

      *    *===========================================================*
      *    * Work-area per prezzo sottoposto a legame valutario        *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wlvlprz0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per determinazione prezzo netto                 *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cpw"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione importo in riga  *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dimpven0.dtl"                   .

      *    *===========================================================*
      *    * Work-area per conversioni rispetto alla valuta base       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale, o per messaggi centra- *
      *        * li circondati da un box                               *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .

      *    *===========================================================*
      *    * Link-area per accettazione tipo movimento ordini fornitori*
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/acdeyof0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione coefficiente cambio valuta     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto 'dcp'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per duplicazione righe da bolla     *
      *    *-----------------------------------------------------------*
       01  l-dup-rba.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  l-dup-rba-tip-ope          pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        * - Spaces : operazione eseguita                        *
      *        * - #      : errore di esecuzione                       *
      *        *-------------------------------------------------------*
           05  l-dup-rba-exi-sts          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  l-dup-rba-cod-dpz          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Default per codice tipo movimento                     *
      *        *-------------------------------------------------------*
           05  l-dup-rba-tmo-bfo          pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Flag di trattamento bolla di acquisto                 *
      *        *-------------------------------------------------------*
           05  l-dup-rba-flg-bfo          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di function-key Tab in corso                     *
      *        *-------------------------------------------------------*
           05  l-dup-rba-fky-tab          pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area comune per programmi della serie pbol3000       *
      *    *                                                           *
      *    * La link-area comprende :                                  *
      *    *                                                           *
      *    *  - 'w-prs'     : Work-area per personalizzazioni          *
      *    *  - 'w-tes'     : Work-area per bufferizzazione testata    *
      *    *  - 'w-pie'     : Work-area per bufferizzazione piede      *
      *    *  - 'w-rig'     : Work-area per bufferizzazione riga       *
      *    *  - 'w-cat-rig' : Work-area di comunicazione per gestione  *
      *    *                  catena righe                             *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/pbol3000.pgl"                   .

      ******************************************************************
       Procedure Division                using l-dup-rba
                                               w-prs
                                               w-tes
                                               w-pie
                                               w-rig
                                               w-cat-rig              .
      ******************************************************************

      *    *===========================================================*
      *    * Main program                                              *
      *    *-----------------------------------------------------------*
       main-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   l-dup-rba-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione dati identificativi ordine     *
      *                  * fornitore                                   *
      *                  *---------------------------------------------*
           if        l-dup-rba-tip-ope    =    "RB"
                     perform acc-dti-bfo-000
                                          thru acc-dti-bfo-999
      *                  *---------------------------------------------*
      *                  * Saldaconto per evasione righe ordine        *
      *                  *---------------------------------------------*
           else if   l-dup-rba-tip-ope    =    "SC"
                     perform sdc-sel-bfo-000
                                          thru sdc-sel-bfo-999
      *                  *---------------------------------------------*
      *                  * Caricamento righe ordine in catena          *
      *                  *---------------------------------------------*
           else if   l-dup-rba-tip-ope    =    "CC"
                     perform car-rig-cat-000
                                          thru car-rig-cat-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   l-dup-rba-tip-ope    =    "OP"
                     perform exe-fun-opn-000
                                          thru exe-fun-opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   l-dup-rba-tip-ope    =    "CL"
                     perform exe-fun-cls-000
                                          thru exe-fun-cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   l-dup-rba-tip-ope    =    "C?"
                     perform tst-cnc-mod-000
                                          thru tst-cnc-mod-999        .
       main-999.
           exit program.

      *    *===========================================================*
      *    * Open                                                      *
      *    *-----------------------------------------------------------*
       exe-fun-opn-000.
      *              *-------------------------------------------------*
      *              * Incremento contatore Open modulo                *
      *              *-------------------------------------------------*
           add       1                    to   w-gen-ctr-opn          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione codice dipendenza in uso        *
      *              *-------------------------------------------------*
           move      l-dup-rba-cod-dpz    to   w-acc-cod-dpz          .
      *              *-------------------------------------------------*
      *              * Normalizzazione data per accettazione           *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-dat-doc          .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Esposizione della riga in scroll            *
      *                  *---------------------------------------------*
           perform   prs-rig-scr-000      thru prs-rig-scr-999        .
       exe-fun-opn-100.
      *              *-------------------------------------------------*
      *              * Apertura files                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [bft]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
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
      *                  *---------------------------------------------*
      *                  * [bfx]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfx                 .
      *                  *---------------------------------------------*
      *                  * [ybf]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento per gli *
      *              * ordini fornitori                                *
      *              *-------------------------------------------------*
           perform   cod-des-yof-opn-000  thru cod-des-yof-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione coefficiente di cambio *
      *              * valuta                                          *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-opn-000  thru coe-cmb-vlt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice prodotto 'dcp'  *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-opn-000  thru cod-cod-dcp-opn-999    .
       exe-fun-opn-200.
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione importo in riga   *
      *              *-------------------------------------------------*
       exe-fun-opn-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative al tipo di espo- *
      *    * sizione della riga di scroll                              *
      *    *-----------------------------------------------------------*
       prs-rig-scr-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/bfo/mov/bol300[rig-scr]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se personalizzazione esistente si spostano i    *
      *              * valori letti in area di destinazione, altri-    *
      *              * menti si normalizza l'area di destinazione      *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-rac-scr-str
           else      move  spaces         to   w-prs-rac-scr-str      .
      *              *-------------------------------------------------*
      *              * Decomposizione valore                           *
      *              *-------------------------------------------------*
           move      w-prs-rac-scr-aaa    to   w-prs-rac-scr-des      .
           move      w-prs-rac-scr-bbb    to   w-prs-rac-scr-res      .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo esposizione descrizione    *
      *              *-------------------------------------------------*
           if        w-prs-rac-scr-des    not  = 00 and
                     w-prs-rac-scr-des    not  = 01 and
                     w-prs-rac-scr-des    not  = 02 and
                     w-prs-rac-scr-des    not  = 03 and
                     w-prs-rac-scr-des    not  = 04 and
                     w-prs-rac-scr-des    not  = 05 and
                     w-prs-rac-scr-des    not  = 06 and
                     w-prs-rac-scr-des    not  = 11 and
                     w-prs-rac-scr-des    not  = 12 and
                     w-prs-rac-scr-des    not  = 13 and
                     w-prs-rac-scr-des    not  = 14
                     move  00             to   w-prs-rac-scr-des      .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo esposizione residuo        *
      *              *-------------------------------------------------*
           if        w-prs-rac-scr-res    not  = 00 and
                     w-prs-rac-scr-res    not  = 01
                     move  00             to   w-prs-rac-scr-res      .
       prs-rig-scr-999.
           exit.

      *    *===========================================================*
      *    * Close                                                     *
      *    *-----------------------------------------------------------*
       exe-fun-cls-000.
      *              *-------------------------------------------------*
      *              * Decremento contatore Open modulo                *
      *              *-------------------------------------------------*
           subtract  1                    from w-gen-ctr-opn          .
       exe-fun-cls-100.
      *              *-------------------------------------------------*
      *              * Chiusura files                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [bft]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
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
      *                  *---------------------------------------------*
      *                  * [bfx]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfx                 .
      *                  *---------------------------------------------*
      *                  * [ybf]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento per    *
      *              * ordini fornitori                                *
      *              *-------------------------------------------------*
           perform   cod-des-yof-cls-000  thru cod-des-yof-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione coefficiente di cam-  *
      *              * bio valuta                                      *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-cls-000  thru coe-cmb-vlt-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto 'dcp' *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-cls-000  thru cod-cod-dcp-cls-999    .
       exe-fun-cls-200.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione importo in riga  *
      *              *-------------------------------------------------*
       exe-fun-cls-999.
           exit.

      *    *===========================================================*
      *    * Test cancellabilita' modulo                               *
      *    *-----------------------------------------------------------*
       tst-cnc-mod-000.
      *              *-------------------------------------------------*
      *              * Se il contatore di Open e' a zero il modulo e'  *
      *              * cancellabile, altrimenti non lo e'              *
      *              *-------------------------------------------------*
           if        w-gen-ctr-opn        =    zero
                     move  spaces         to   l-dup-rba-exi-sts
           else      move  "#"            to   l-dup-rba-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione dati identificativi bolla fornitore          *
      *    *-----------------------------------------------------------*
       acc-dti-bfo-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      11                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      17                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Normalizzazione work-area di accettazione       *
      *              *-------------------------------------------------*
           perform   nor-wrk-acc-000      thru nor-wrk-acc-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Titolo centrale                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "                 DUPLICAZIONE RIGHE DA DOCUMENTO D
      -              "I ENTRATA                 "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero protocollo                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      27                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Numero protocollo interno :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      42                   to   v-pos                  .
           move      "Data :"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      60                   to   v-pos                  .
           move      "Numero :"           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-dti-bfo-100.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Accettazione dati identificativi bolla          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Numero protocollo                           *
      *                  *---------------------------------------------*
           perform   acc-num-prt-000      thru acc-num-prt-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-bfo-800.
           if        v-key                =    "DO  "
                     go to acc-dti-bfo-400.
       acc-dti-bfo-400.
      *              *-------------------------------------------------*
      *              * Determinazione se documento esistente           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [bft]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [bft]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-acc-num-prt        to   rf-bft-num-prt         .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * Trattamento valori connessi                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione data documento              *
      *                  *---------------------------------------------*
           move      rf-bft-dat-doc       to   w-acc-dat-doc          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione data documento              *
      *                  *---------------------------------------------*
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione numero documento            *
      *                  *---------------------------------------------*
           move      rf-bft-num-doc       to   w-acc-num-doc          .
      *                  *---------------------------------------------*
      *                  * Visualizzazione numero documento            *
      *                  *---------------------------------------------*
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *                  *---------------------------------------------*
      *                  * Valore a zero non ammesso                   *
      *                  *---------------------------------------------*
           if        w-acc-num-prt-prg    =    zero
                     go to acc-dti-bfo-100.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to acc-dti-bfo-420.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Movimento non esistente in archivio !             
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-bfo-100.
       acc-dti-bfo-420.
      *              *-------------------------------------------------*
      *              * Test su flag di bolla chiusa                    *
      *              *-------------------------------------------------*
       acc-dti-bfo-440.
      *              *-------------------------------------------------*
      *              * Test su tipo archivio                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione codice archivio             *
      *                  *---------------------------------------------*
           move      rf-bft-cod-arc       to   w-acc-cod-arc          .
       acc-dti-bfo-500.
      *              *-------------------------------------------------*
      *              * Test se la bolla e' gia' stata richiamata       *
      *              *-------------------------------------------------*
       acc-dti-bfo-540.
      *              *-------------------------------------------------*
      *              * Bufferizzazione righe bolla da trattare         *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-buf-rig-bfo-prt      .
           perform   buf-rig-bfo-000      thru buf-rig-bfo-999        .
      *                  *---------------------------------------------*
      *                  * Se non trovata alcuna riga : uscita come    *
      *                  * per 'exit'                                  *
      *                  *---------------------------------------------*
           if        w-brb-num-ele        =    zero
                     go to acc-dti-bfo-800.
       acc-dti-bfo-600.
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           perform   acc-car-prv-000      thru acc-car-prv-999        .
           if        v-key                =    "UP  "
                     go to acc-dti-bfo-100.
           if        v-key                =    "EXIT"
                     go to acc-dti-bfo-800.
       acc-dti-bfo-650.
      *              *-------------------------------------------------*
      *              * Accettazione castelletto valute per legame va-  *
      *              * lutario                                         *
      *              *-------------------------------------------------*
           perform   acc-cdc-vpl-000      thru acc-cdc-vpl-999
           if        v-key                =    "EXIT"
                     go to acc-dti-bfo-800.
       acc-dti-bfo-700.
      *              *-------------------------------------------------*
      *              * Aggiornamento protocollo bolla fornitore in     *
      *              * corso di trattamento                            *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-gen-prt-bfo          .
      *              *-------------------------------------------------*
      *              * A uscita                                        *
      *              *-------------------------------------------------*
           go to     acc-dti-bfo-900.
       acc-dti-bfo-800.
      *              *-------------------------------------------------*
      *              * Se uscita per Exit                              *
      *              *-------------------------------------------------*
           move      "#"                  to   l-dup-rba-exi-sts      .
       acc-dti-bfo-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-dti-bfo-999.
       acc-dti-bfo-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Numero protocollo                    *
      *    *-----------------------------------------------------------*
       acc-num-prt-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Composizione work per accettazione          *
      *                  *---------------------------------------------*
           move      w-acc-num-prt-saa    to   w-rnp-num-prt-saa      .
           move      w-acc-num-prt-prg    to   w-rnp-num-prt-prg      .
       acc-num-prt-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-rnp-num-prt-num    to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-num-prt-999.
       acc-num-prt-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-rnp-num-prt-num      .
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Decomposizione work di accettazione         *
      *                  *---------------------------------------------*
           move      w-rnp-num-prt-saa    to   w-acc-num-prt-saa      .
           move      w-acc-cod-dpz        to   w-acc-num-prt-dpz      .
           move      w-rnp-num-prt-prg    to   w-acc-num-prt-prg      .
       acc-num-prt-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-prt-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [bft]                      *
      *                  *---------------------------------------------*
           move      w-acc-cod-dpz        to   w-fnd-arc-bft-dpz      .
           move      w-tes-cod-arc (1)    to   w-fnd-arc-bft-fnt      .
           move      w-tes-dpz-arc (1)    to   w-fnd-arc-bft-dpf      .
           perform   fnd-arc-bft-000      thru fnd-arc-bft-999        .
           if        w-fnd-arc-bft-sel    not  = spaces
                     go to acc-num-prt-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
       acc-num-prt-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-prt-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-prt-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-num-prt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Numero protocollo                 *
      *    *-----------------------------------------------------------*
       vis-num-prt-000.
      *              *-------------------------------------------------*
      *              * Composizione work per visualizzazione           *
      *              *-------------------------------------------------*
           move      w-acc-num-prt-saa    to   w-rnp-num-prt-saa      .
           move      w-acc-num-prt-prg    to   w-rnp-num-prt-prg      .
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      31                   to   v-pos                  .
           move      w-rnp-num-prt-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-prt-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Data documento                    *
      *    *-----------------------------------------------------------*
       vis-dat-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      14                   to   v-lin                  .
           move      49                   to   v-pos                  .
           move      w-acc-dat-doc        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Numero documento                  *
      *    *-----------------------------------------------------------*
       vis-num-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      69                   to   v-pos                  .
           move      w-acc-num-doc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-doc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Carattere di presa visione           *
      *    *-----------------------------------------------------------*
       acc-car-prv-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Parantesi quadre di delimitazione           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      76                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-car-prv-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      16                   to   v-lin                  .
           move      77                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-car-prv-999.
       acc-car-prv-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-car-prv-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-car-prv-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-car-prv-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Coefficienti di cambio applicati per lega- *
      *    * me valutario                                              *
      *    *-----------------------------------------------------------*
       acc-cdc-vpl-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da accettare                        *
      *                  *---------------------------------------------*
           if        w-vpl-num-ele        =    zero
                     go to acc-cdc-vpl-999.
      *                  *---------------------------------------------*
      *                  * Preparazione prompts area accettazione      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Box                                     *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      08                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      20                   to   v-lto                  .
           move      75                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Titolo                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      67                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "                        CAMBI DA APPLICARE        
      -              "                 "  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      67                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      " --------+------------------+---------------------
      -              "--+------------- "  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      67                   to   v-car                  .
           move      11                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "  Valuta | Cambio Da Applic.| Cambio di riferiment
      -              "o | Scostamento  "  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      67                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      " --------+------------------+---------------------
      -              "--+------------- "  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Corpo                                   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      67                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "         |                  |                     
      -              "  |              "  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      14                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      15                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      16                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      17                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           move      18                   to   v-lin                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Parantesi quadre per presa visione      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      19                   to   v-lin                  .
           move      71                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cdc-vpl-050.
      *                  *---------------------------------------------*
      *                  * Visualizzazione preliminare valori          *
      *                  *---------------------------------------------*
           move      zero                 to   w-vpl-ctr-001          .
       acc-cdc-vpl-052.
           add       1                    to   w-vpl-ctr-001          .
           if        w-vpl-ctr-001        >    w-vpl-num-ele
                     go to acc-cdc-vpl-075.
           if        w-vpl-ctr-001        >    6
                     go to acc-cdc-vpl-075.
      *                      *-----------------------------------------*
      *                      * Sigla valuta                            *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           add       12
                     w-vpl-ctr-001    giving   v-lin                  .
           move      11                   to   v-pos                  .
           move      w-vpl-sgl-vpl
                    (w-vpl-ctr-001)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio applicato        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       12
                     w-vpl-ctr-001    giving   v-lin                  .
           move      20                   to   v-pos                  .
           move      w-vpl-cdc-vpl
                    (w-vpl-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio di riferimento   *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       12
                     w-vpl-ctr-001    giving   v-lin                  .
           move      38                   to   v-pos                  .
           move      w-vpl-cdc-rif
                    (w-vpl-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Data di riferimento cambio              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           add       12
                     w-vpl-ctr-001    giving   v-lin                  .
           move      51                   to   v-pos                  .
           move      w-vpl-dat-rif
                    (w-vpl-ctr-001)       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Percentuale di scostamento              *
      *                      *-----------------------------------------*
           perform   vis-per-scs-000      thru vis-per-scs-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo su scansione                    *
      *                      *-----------------------------------------*
           go to     acc-cdc-vpl-052.
       acc-cdc-vpl-075.
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Inizializzazione contatore                  *
      *                  *---------------------------------------------*
           move      1                    to   w-vpl-ctr-001          .
       acc-cdc-vpl-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      05                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       12
                     w-vpl-ctr-001    giving   v-lin                  .
           move      20                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      w-vpl-cdc-vpl
                    (w-vpl-ctr-001)       to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-cdc-vpl-999.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-vpl-cdc-vpl
                                              (w-vpl-ctr-001)         .
       acc-cdc-vpl-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valore a zero non ammesso                   *
      *                  *---------------------------------------------*
           if        w-vpl-cdc-vpl
                    (w-vpl-ctr-001)       =    zero
                     go to acc-cdc-vpl-100.
       acc-cdc-vpl-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione percentuale di scostamento   *
      *                  *---------------------------------------------*
           subtract  w-vpl-cdc-rif
                    (w-vpl-ctr-001)       from w-vpl-cdc-vpl
                                              (w-vpl-ctr-001)
                                        giving w-vpl-wrk-scs          .
           multiply  100                  by   w-vpl-wrk-scs          .
           divide    w-vpl-cdc-rif
                    (w-vpl-ctr-001)       into w-vpl-wrk-scs
                                        giving w-vpl-per-scs
                                              (w-vpl-ctr-001)
                                               rounded                .
      *                  *---------------------------------------------*
      *                  * Visualizzazione percentuale di scostamento  *
      *                  *---------------------------------------------*
           perform   vis-per-scs-000      thru vis-per-scs-999        .
       acc-cdc-vpl-700.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione dell'impostazione        *
      *              *-------------------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cdc-vpl-710
           else if   v-key                =    "DOWN"
                     go to acc-cdc-vpl-720
           else if   v-key                =    "DO  "
                     go to acc-cdc-vpl-800
           else      go to acc-cdc-vpl-720.
       acc-cdc-vpl-710.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           if        w-vpl-ctr-001        =    1
                     go to acc-cdc-vpl-100.
           subtract  1                    from w-vpl-ctr-001          .
           go to     acc-cdc-vpl-100.
       acc-cdc-vpl-720.
      *              *-------------------------------------------------*
      *              * Se Down o Return                                *
      *              *-------------------------------------------------*
           if        w-vpl-ctr-001        <    w-vpl-num-ele
                     add   1              to   w-vpl-ctr-001
           else      go to acc-cdc-vpl-722.
           if        w-vpl-ctr-001        >    6
                     go to acc-cdc-vpl-722
           else      go to acc-cdc-vpl-100.
       acc-cdc-vpl-722.
      *                  *---------------------------------------------*
      *                  * Accettazione carattere di presa visione     *
      *                  *---------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      19                   to   v-lin                  .
           move      72                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           if        v-key                not  = "UP  "
                     go to acc-cdc-vpl-800.
           move      w-vpl-num-ele        to   w-vpl-ctr-001          .
           if        w-vpl-ctr-001        >    6
                     move  6              to   w-vpl-ctr-001          .
           go to     acc-cdc-vpl-100.
       acc-cdc-vpl-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo che tutti gli elementi siano sta- *
      *                  * ti impostati                                *
      *                  *---------------------------------------------*
           move      zero                 to   w-vpl-ctr-002          .
       acc-cdc-vpl-820.
           add       1                    to   w-vpl-ctr-002          .
           if        w-vpl-ctr-002        >    w-vpl-num-ele
                     go to acc-cdc-vpl-900.
           if        w-vpl-ctr-002        >    6
                     go to acc-cdc-vpl-900.
           if        w-vpl-cdc-vpl
                    (w-vpl-ctr-002)       =    zero
                     go to acc-cdc-vpl-100.
           go to     acc-cdc-vpl-820.
       acc-cdc-vpl-900.
      *                  *---------------------------------------------*
      *                  * Ripristino immagine video                   *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-cdc-vpl-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Percentuale di scostamento              *
      *    *-----------------------------------------------------------*
       vis-per-scs-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      01                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      spaces               to   v-edm                  .
           add       12
                     w-vpl-ctr-001    giving   v-lin                  .
           move      64                   to   v-pos                  .
           move      w-vpl-per-scs
                    (w-vpl-ctr-001)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-per-scs-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione accettazione di un campo                       *
      *    *-----------------------------------------------------------*
       exe-acc-cmp-000.
      *              *-------------------------------------------------*
      *              * Tasto di funzione Exit : sempre abilitato       *
      *              *-------------------------------------------------*
           move      "EXIT"               to   v-pfk (20)             .
      *              *-------------------------------------------------*
      *              * Accettazione                                    *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       exe-acc-cmp-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione work-area di accettazione                 *
      *    *-----------------------------------------------------------*
       nor-wrk-acc-000.
           move      zero                 to   w-acc-num-prt          .
           move      zero                 to   w-acc-dat-doc          .
           move      spaces               to   w-acc-num-doc          .
       nor-wrk-acc-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione righe bolla da trattare                   *
      *    *-----------------------------------------------------------*
       buf-rig-bfo-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di uscita ad errore       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-buf-rig-bfo-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi di visualizzazione       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-edt-qta-inc-ope      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
      *              *-------------------------------------------------*
      *              * Azzeramento numero righe nel buffer             *
      *              *-------------------------------------------------*
           move      zero                 to   w-brb-num-ele          .
      *              *-------------------------------------------------*
      *              * Azzeramento numero elementi nel buffer valute   *
      *              * per legame                                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-vpl-num-ele          .
       buf-rig-bfo-100.
      *              *-------------------------------------------------*
      *              * Start su file [bfr]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-buf-rig-bfo-prt    to   rf-bfr-num-prt         .
           move      zero                 to   rf-bfr-num-prg         .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-bfo-500.
       buf-rig-bfo-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [bfr]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-bfo-500.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-bfr-num-prt       not  = w-buf-rig-bfo-prt
                     go to buf-rig-bfo-500.
       buf-rig-bfo-250.
      *              *-------------------------------------------------*
      *              * Aggiornamento comodi di visualizzazione         *
      *              *-------------------------------------------------*
           move      "AG"                 to   w-edt-qta-inc-ope      .
           move      rf-bfr-qta-acq       to   w-edt-qta-inc-qta      .
           move      rf-bfr-dec-qta       to   w-edt-qta-inc-dec      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
           move      w-edt-qta-inc-din    to   w-tes-dec-qta          .
      *              *-------------------------------------------------*
      *              * Ridefinizione tipo riga                         *
      *              *-------------------------------------------------*
           move      rf-bfr-tip-rig       to   w-buf-rig-bfo-wtr      .
      *              *-------------------------------------------------*
      *              * Se tipo riga "C" : a bufferizzazione riga       *
      *              *-------------------------------------------------*
           if        w-buf-rig-bfo-wtp    =    "C"
                     go to buf-rig-bfo-300.
       buf-rig-bfo-280.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-rig-bfo-flg      .
       buf-rig-bfo-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione riga ordine                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore elementi nel buffer    *
      *                  *---------------------------------------------*
           if        w-brb-num-ele        <    w-brb-max-ele
                     add   1              to   w-brb-num-ele          .
      *                  *---------------------------------------------*
      *                  * Progressivo riga                            *
      *                  *---------------------------------------------*
           move      rf-bfr-num-prg       to   w-brb-num-prg
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Tipo riga                                   *
      *                  *---------------------------------------------*
           move      w-buf-rig-bfo-wtp    to   w-brb-tip-rig
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Descrizione per la riga                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura preliminare file [bfx]          *
      *                      *-----------------------------------------*
           move      rf-bfr-num-prt       to   w-let-arc-bfx-prt      .
           move      rf-bfr-num-prg       to   w-let-arc-bfx-prg      .
           move      11                   to   w-let-arc-bfx-trc      .
           perform   let-arc-bfx-000      thru let-arc-bfx-999        .
      *
           if        w-let-arc-bfx-flg    not  = spaces
                     go to buf-rig-bfo-305.
      *
           move      w-let-arc-bfx-des    to   w-buf-rig-bfo-wde      .
      *
           go to     buf-rig-bfo-350.
       buf-rig-bfo-305.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del flag di e-   *
      *                      * stensione alla descrizione              *
      *                      *-----------------------------------------*
           if        rf-bfr-des-ext       =    0
                     go to buf-rig-bfo-310
           else if   rf-bfr-des-ext       =    1
                     go to buf-rig-bfo-320
           else if   rf-bfr-des-ext       =    2
                     go to buf-rig-bfo-330
           else if   rf-bfr-des-ext       =    3
                     go to buf-rig-bfo-340.
       buf-rig-bfo-310.
      *                      *-----------------------------------------*
      *                      * Se nessuna estensione : bufferizzazione *
      *                      * descrizione contenuta nel record [bfr]  *
      *                      *-----------------------------------------*
           move      rf-bfr-des-rig       to   w-buf-rig-bfo-wde      .
           go to     buf-rig-bfo-350.
       buf-rig-bfo-320.
      *                      *-----------------------------------------*
      *                      * Se estensione nel file [bfx]            *
      *                      *-----------------------------------------*
           go to     buf-rig-bfo-350.
       buf-rig-bfo-330.
      *                      *-----------------------------------------*
      *                      * Se estensione nel file [pdx]            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcp]              *
      *                          *-------------------------------------*
           move      rf-bfr-num-mag       to   w-let-arc-dcp-cod      .
           move      w-tes-cod-lng (1)    to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
           move      w-let-arc-dcp-des    to   w-buf-rig-bfo-wde      .
           go to     buf-rig-bfo-350.
       buf-rig-bfo-340.
      *                      *-----------------------------------------*
      *                      * Se estensione nel file [pdx] ma con ti- *
      *                      * po record 13 o 33                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcp]              *
      *                          *-------------------------------------*
           move      rf-bfr-num-mag       to   w-let-arc-dcp-cod      .
           move      w-tes-cod-lng (1)    to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                          *-------------------------------------*
      *                          * Lettura archivio [aaf]              *
      *                          *-------------------------------------*
           move      rf-bfr-tip-mag       to   w-let-arc-aaf-tpm      .
           move      rf-bfr-num-mag       to   w-let-arc-aaf-cdm      .
           move      rf-bfr-cod-arc       to   w-let-arc-aaf-fnt      .
           move      rf-bfr-fda-pif       to   w-let-arc-aaf-fda      .
           move      w-let-arc-dcp-des    to   w-let-arc-aaf-dmg      .
           perform   let-arc-aaf-000      thru let-arc-aaf-999        .
           move      w-let-arc-aaf-des    to   w-buf-rig-bfo-wde      .
           go to     buf-rig-bfo-350.
       buf-rig-bfo-350.
      *                      *-----------------------------------------*
      *                      * Composizione descrizione per riga di    *
      *                      * scroll                                  *
      *                      *-----------------------------------------*
           perform   cpz-des-rgs-000      thru cpz-des-rgs-999        .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      w-buf-rig-bfo-wde    to   w-brb-des-rig
                                              (w-brb-num-ele)         .
       buf-rig-bfo-352.
      *                  *---------------------------------------------*
      *                  * Numero decimali quantita'                   *
      *                  *---------------------------------------------*
           move      rf-bfr-dec-qta       to   w-brb-dec-qta
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Quantita' in bolla                          *
      *                  *---------------------------------------------*
       buf-rig-bfo-353.
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento : zero   *
      *                      *-----------------------------------------*
           if        w-buf-rig-bfo-wtp    =    "C" or
                     w-buf-rig-bfo-wtp    =    "A"
                     move  zero           to   w-brb-qta-bfo
                                              (w-brb-num-ele)
                     go to buf-rig-bfo-360.
       buf-rig-bfo-354.
      *                      *-----------------------------------------*
      *                      * Se riga con quantita'                   *
      *                      *-----------------------------------------*
       buf-rig-bfo-355.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo di    *
      *                          * trasformazione per l'unita' di mi-  *
      *                          * sura                                *
      *                          *-------------------------------------*
           if        rf-bfr-snx-tum       =    "P"
                     go to buf-rig-bfo-357.
       buf-rig-bfo-356.
      *                          *-------------------------------------*
      *                          * Se tipo di trasformazione diverso   *
      *                          * da 'P'                              *
      *                          *-------------------------------------*
           move      rf-bfr-qta-fda       to   w-brb-qta-bfo
                                              (w-brb-num-ele)         .
           go to     buf-rig-bfo-360.
       buf-rig-bfo-357.
      *                          *-------------------------------------*
      *                          * Se tipo di trasformazione 'P'       *
      *                          *-------------------------------------*
           move      rf-bfr-qta-acq       to   w-brb-qta-bfo
                                              (w-brb-num-ele)         .
           go to     buf-rig-bfo-360.
       buf-rig-bfo-360.
      *                  *---------------------------------------------*
      *                  * Quantita' da duplicare                      *
      *                  *---------------------------------------------*
           move      w-brb-qta-bfo
                    (w-brb-num-ele)       to   w-brb-qta-dup
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Flag indicatore di residuo                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-brb-flg-idr
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Flag indicatore di saldo                    *
      *                  *---------------------------------------------*
           move      spaces               to   w-brb-flg-ids
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Flag di forzatura a saldo                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-brb-flg-fzs
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Quantita' residua                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-brb-qta-res
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Si/No addebito o commento                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-brb-snx-aoc
                                              (w-brb-num-ele)         .
       buf-rig-bfo-400.
      *              *-------------------------------------------------*
      *              * Eventuale bufferizzazione valuta per legame     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da effettuare                       *
      *                  *---------------------------------------------*
           if        rf-bfr-sgl-vpl       =    spaces
                     go to buf-rig-bfo-480.
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione buffer : se valuta gia'  *
      *                  * bufferizzata, oltre                         *
      *                  *---------------------------------------------*
           move      zero                 to   w-vpl-ctr-001          .
       buf-rig-bfo-420.
           add       1                    to   w-vpl-ctr-001          .
           if        w-vpl-ctr-001        >    w-vpl-num-ele
                     go to buf-rig-bfo-440.
           if        rf-bfr-sgl-vpl       =    w-vpl-sgl-vpl
                                              (w-vpl-ctr-001)
                     go to buf-rig-bfo-480.
           go to     buf-rig-bfo-420.
       buf-rig-bfo-440.
      *                  *---------------------------------------------*
      *                  * Incremento numero elementi nel buffer valu- *
      *                  * te                                          *
      *                  *---------------------------------------------*
           if        w-vpl-num-ele        <    w-vpl-max-ele
                     add   1              to   w-vpl-num-ele          .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione elemento                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sigla valuta per legame                 *
      *                      *-----------------------------------------*
           move      rf-bfr-sgl-vpl       to   w-vpl-sgl-vpl
                                              (w-vpl-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio di riferimento   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione                      *
      *                          *-------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-bfr-sgl-vpl       to   w-coe-cmb-vlt-sdv      .
           move      rf-bfr-tdc-vpl       to   w-coe-cmb-vlt-tdc      .
           if        rf-bfr-dat-doc       not  = zero
                     move  rf-bfr-dat-doc to   w-coe-cmb-vlt-drc
           else      move  rf-bfr-dat-reg to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                          *-------------------------------------*
      *                          * Se esito negativo bufferizzazione   *
      *                          * coefficiente contenuto in [bfr]     *
      *                          *-------------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-bfr-cdc-vpl to   w-coe-cmb-vlt-cdc      .
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-vpl-cdc-rif
                                              (w-vpl-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Data di riferimento cambio              *
      *                      *-----------------------------------------*
           move      w-coe-cmb-vlt-drc    to   w-vpl-dat-rif
                                              (w-vpl-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Coefficiente di cambio applicato        *
      *                      *-----------------------------------------*
           move      rf-bfr-cdc-vpl       to   w-vpl-cdc-vpl
                                              (w-vpl-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Percentuale di scostamento              *
      *                      *-----------------------------------------*
           move      zero                 to   w-vpl-per-scs
                                              (w-vpl-num-ele)         .
       buf-rig-bfo-480.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale file [bfr]       *
      *              *-------------------------------------------------*
           go to     buf-rig-bfo-200.
       buf-rig-bfo-500.
      *              *-------------------------------------------------*
      *              * Test su numero elementi in buffer valute per    *
      *              * legame                                          *
      *              *-------------------------------------------------*
           if        w-vpl-num-ele        not  > 6
                     go to buf-rig-bfo-600.
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "Numero valute per legame valutario trattate nel do
      -              "cumento, mag-  "    to   w-err-box-err-msg      .
           move      "giore di quelle gestibili; per esse verra' forzato
      -              " il ns. cambio."    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
       buf-rig-bfo-600.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rig-bfo-999.
       buf-rig-bfo-999.
           exit.

      *    *===========================================================*
      *    * Composizione descrizione per riga di scroll               *
      *    *-----------------------------------------------------------*
       cpz-des-rgs-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se si sta trattando un co- *
      *              * dice di magazzino oppure no                     *
      *              *-------------------------------------------------*
           if       (w-buf-rig-bfo-wtp    =    "P" or
                     w-buf-rig-bfo-wtp    =    "M" or
                     w-buf-rig-bfo-wtp    =    "V"  ) and
                     w-buf-rig-bfo-wtf    =    spaces
                     go to cpz-des-rgs-200.
       cpz-des-rgs-100.
      *              *-------------------------------------------------*
      *              * Se non si sta trattando un codice di magazzino  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nessuna azione, in quanto la descrizione    *
      *                  * gia' composta va' bene                      *
      *                  *---------------------------------------------*
           go to     cpz-des-rgs-999.
       cpz-des-rgs-200.
      *              *-------------------------------------------------*
      *              * Se si sta trattando un codice di magazzino      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare dell'area di    *
      *                  * lavoro per la composizione                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-des-scr-000          .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore della per-  *
      *                  * sonalizzazione del tipo di visualizzazione  *
      *                  * della riga di scroll                        *
      *                  *---------------------------------------------*
           if        w-prs-rac-scr-des    =    00
                     go to cpz-des-rgs-400
           else if   w-prs-rac-scr-des    =    01
                     go to cpz-des-rgs-420
           else if   w-prs-rac-scr-des    =    02
                     go to cpz-des-rgs-440
           else if   w-prs-rac-scr-des    =    03
                     go to cpz-des-rgs-460
           else if   w-prs-rac-scr-des    =    04
                     go to cpz-des-rgs-480
           else if   w-prs-rac-scr-des    =    05
                     go to cpz-des-rgs-500
           else if   w-prs-rac-scr-des    =    06
                     go to cpz-des-rgs-520
           else if   w-prs-rac-scr-des    =    11
                     go to cpz-des-rgs-540
           else if   w-prs-rac-scr-des    =    12
                     go to cpz-des-rgs-560
           else if   w-prs-rac-scr-des    =    13
                     go to cpz-des-rgs-580
           else if   w-prs-rac-scr-des    =    14
                     go to cpz-des-rgs-600
           else      go to cpz-des-rgs-400.
       cpz-des-rgs-400.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 00              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione di 40 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-bfo-wde    to   w-des-scr-000-d40      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-420.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 01              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione di 25 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-bfo-wde    to   w-des-scr-001-d25      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-bfr-alf-mag       to   w-des-scr-001-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-440.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 02              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione di 21 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-bfo-wde    to   w-des-scr-002-d21      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-002-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-bfo-wtp    to   w-des-scr-002-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-002-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-bfr-alf-mag       to   w-des-scr-002-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-460.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 03              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-bfr-alf-mag       to   w-des-scr-003-cod      .
      *                      *-----------------------------------------*
      *                      * Descrizione di 25 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-bfo-wde    to   w-des-scr-003-d25      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-480.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 04              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-004-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-bfo-wtp    to   w-des-scr-004-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-004-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-bfr-alf-mag       to   w-des-scr-004-cod      .
      *                      *-----------------------------------------*
      *                      * Descrizione di 21 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-bfo-wde    to   w-des-scr-004-d21      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-500.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 05              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-bfr-alf-mag       to   w-des-scr-005-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-520.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 06              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-006-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-bfo-wtp    to   w-des-scr-006-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-006-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-bfr-alf-mag       to   w-des-scr-006-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-540.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 11              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per codice prodotto fornitore    *
      *                      *-----------------------------------------*
           move      "[F]"                to   w-des-scr-011-pfp      .
      *                      *-----------------------------------------*
      *                      * Codice prodotto per fornitore           *
      *                      *-----------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-bfr-alf-mag       to   w-des-scr-011-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-560.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 12              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt per codice prodotto fornitore    *
      *                      *-----------------------------------------*
           move      "[F]"                to   w-des-scr-012-pfp      .
      *                      *-----------------------------------------*
      *                      * Codice prodotto per fornitore           *
      *                      *-----------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-012-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-bfo-wtp    to   w-des-scr-012-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-012-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-bfr-alf-mag       to   w-des-scr-012-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-580.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 13              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-bfr-alf-mag       to   w-des-scr-013-cod      .
      *                      *-----------------------------------------*
      *                      * Prompt per codice prodotto fornitore    *
      *                      *-----------------------------------------*
           move      "[F]"                to   w-des-scr-013-pfp      .
      *                      *-----------------------------------------*
      *                      * Codice prodotto per fornitore           *
      *                      *-----------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-600.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 14              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-014-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-bfo-wtp    to   w-des-scr-014-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-014-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-bfr-alf-mag       to   w-des-scr-014-cod      .
      *                      *-----------------------------------------*
      *                      * Prompt per codice prodotto fornitore    *
      *                      *-----------------------------------------*
           move      "[F]"                to   w-des-scr-014-pfp      .
      *                      *-----------------------------------------*
      *                      * Codice prodotto per fornitore           *
      *                      *-----------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-700.
      *                  *---------------------------------------------*
      *                  * Composizione eseguita in area di destina-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      w-des-scr-000        to   w-buf-rig-bfo-wde      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cpz-des-rgs-999.
       cpz-des-rgs-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per evasione righe ordine fornitore            *
      *    *-----------------------------------------------------------*
       sdc-sel-bfo-000.
      *              *-------------------------------------------------*
      *              * Subroutine per la preparazione dei valori rela- *
      *              * tivi alla valuta base, determinati dalla segre- *
      *              * teria                                           *
      *              *-------------------------------------------------*
           move      "VB"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dec                to   c-dec                  .
           move      s-asx                to   c-sgl                  .
           move      s-sgn                to   c-tdc                  .
           move      s-num                to   c-cdc                  .
           move      s-adx (01 : 20)      to   c-des                  .
           move      s-adx (21 : 20)      to   c-din                  .
      *              *-------------------------------------------------*
      *              * Richiamo funzione di saldaconto                 *
      *              *-------------------------------------------------*
           perform   acc-fun-sdc-000      thru acc-fun-sdc-999        .
           if        w-sdc-flg-exi        not  = spaces
                     move  "#"            to   l-dup-rba-exi-sts      .
       sdc-sel-bfo-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per evasione righe bolla fornitore             *
      *    *-----------------------------------------------------------*
       acc-fun-sdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-sdc-flg-exi          .
      *              *-------------------------------------------------*
      *              * Determinazione numero massimo pagine            *
      *              *-------------------------------------------------*
           move      w-brb-num-ele        to   w-sdc-npg-max          .
           divide    14                   into w-sdc-npg-max
                                        giving w-sdc-npg-max
                                     remainder w-sdc-wrk-rem          .
           if        w-sdc-wrk-rem        >    zero
                     add    1             to   w-sdc-npg-max          .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
           perform   pmt-fun-sdc-000      thru pmt-fun-sdc-999        .
      *              *-------------------------------------------------*
      *              * Numero pagina visualizzata                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-npg-vis          .
      *              *-------------------------------------------------*
      *              * Numero riga da trattare                         *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-ctr-rig          .
       acc-fun-sdc-200.
           add       1                    to   w-sdc-ctr-rig          .
           if        w-sdc-ctr-rig        >    w-brb-num-ele
                     go to  acc-fun-sdc-800.
       acc-fun-sdc-300.
      *              *-------------------------------------------------*
      *              * Determinazione numero pagina da trattare        *
      *              *-------------------------------------------------*
           divide    14                   into w-sdc-ctr-rig
                                        giving w-sdc-npg-dat
                                     remainder w-sdc-wrk-rem          .
           if        w-sdc-wrk-rem        >    zero
                     add    1             to   w-sdc-npg-dat          .
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina da trattare              *
      *              *-------------------------------------------------*
           if        w-sdc-npg-dat        not  = w-sdc-npg-vis
                     perform vis-pag-sdc-000
                                          thru vis-pag-sdc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione numero linea                     *
      *              *-------------------------------------------------*
           subtract  1                    from w-sdc-npg-dat
                                        giving w-sdc-wrk-rig          .
           multiply  14                   by   w-sdc-wrk-rig          .
           subtract  w-sdc-wrk-rig        from w-sdc-ctr-rig
                                        giving w-sdc-wrk-lin          .
           add       7                    to   w-sdc-wrk-lin          .
       acc-fun-sdc-400.
      *              *-------------------------------------------------*
      *              * Accettazione riga saldaconto                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Quantita' da duplicare                      *
      *                  *---------------------------------------------*
           perform   acc-qta-dup-000      thru acc-qta-dup-999        .
      *                  *---------------------------------------------*
      *                  * Si/No addebito o commento                   *
      *                  *---------------------------------------------*
           perform   acc-snx-aoc-000      thru acc-snx-aoc-999        .
      *                  *---------------------------------------------*
      *                  * Test su tipo uscita                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Do                                   *
      *                      *-----------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-fun-sdc-900.
      *                      *-----------------------------------------*
      *                      * Se Exit                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-fun-sdc-920.
      *                      *-----------------------------------------*
      *                      * Se Up                                   *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     subtract 1           from w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
      *                      *-----------------------------------------*
      *                      * Se Down                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "DOWN"
                     go to  acc-fun-sdc-200.
      *                      *-----------------------------------------*
      *                      * Se Next Screen                          *
      *                      *-----------------------------------------*
           if        v-key                not  = "NXSC"
                     go to  acc-fun-sdc-500.
           multiply  14                   by   w-sdc-npg-dat
                                        giving w-sdc-ctr-rig          .
           add       1                    to   w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-500.
      *                      *-----------------------------------------*
      *                      * Se Prev Screen                          *
      *                      *-----------------------------------------*
           if        v-key                not  = "PRSC"
                     go to  acc-fun-sdc-600.
           subtract  1                    from w-sdc-npg-dat          .
           multiply  14                   by   w-sdc-npg-dat
                                        giving w-sdc-ctr-rig          .
           subtract  13                   from w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-600.
      *                      *-----------------------------------------*
      *                      * Se Tab                                  *
      *                      *-----------------------------------------*
           if        v-key                =    "TAB "
                     move   w-brb-num-ele to   w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
      *                      *-----------------------------------------*
      *                      * Se Back                                 *
      *                      *-----------------------------------------*
           if        v-key                =    "BACK"
                     move   1             to   w-sdc-ctr-rig
                     go to  acc-fun-sdc-300.
      *              *-------------------------------------------------*
      *              * Riciclo a riga successiva                       *
      *              *-------------------------------------------------*
           go to     acc-fun-sdc-200.
       acc-fun-sdc-800.
      *              *-------------------------------------------------*
      *              * Conferma impostazioni                           *
      *              *-------------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "#SAV"               to   v-not                  .
           move      spaces               to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-fun-sdc-810.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       acc-fun-sdc-810.
      *              *-------------------------------------------------*
      *              * Test su risposta dell'utente                    *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-fun-sdc-900
           else if   v-key                =    "EXIT"
                     go to acc-fun-sdc-920
           else if   v-key                =    "UP  "
                     go to acc-fun-sdc-820
           else      go to acc-fun-sdc-800.
       acc-fun-sdc-820.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
           subtract  1                    from w-sdc-ctr-rig          .
           go to     acc-fun-sdc-300.
       acc-fun-sdc-900.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           go to     acc-fun-sdc-999.
       acc-fun-sdc-920.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-sdc-flg-exi          .
           go to     acc-fun-sdc-999.
       acc-fun-sdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompt : Funzione saldaconto              *
      *    *-----------------------------------------------------------*
       pmt-fun-sdc-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Erase linee impegnate                           *
      *              *-------------------------------------------------*
           move      "EL"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      21                   to   v-lto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-fun-sdc-100.
      *              *-------------------------------------------------*
      *              * Intestazione documento                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero protocollo                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      27                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Numero protocollo interno :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      w-acc-num-prt-saa    to   w-rnp-num-prt-saa      .
           move      w-acc-num-prt-prg    to   w-rnp-num-prt-prg      .
           move      w-rnp-num-prt-num    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      42                   to   v-pos                  .
           move      "Data :"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      49                   to   v-pos                  .
           move      w-acc-dat-doc        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      62                   to   v-pos                  .
           move      "Numero :"           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      10                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      71                   to   v-pos                  .
           move      w-acc-num-doc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fun-sdc-200.
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-fun-sdc-300.
      *              *-------------------------------------------------*
      *              * Linea di fincatura                              *
      *              *-------------------------------------------------*
       pmt-fun-sdc-320.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore della per-  *
      *                  * sonalizzazione del tipo di visualizzazione  *
      *                  * della riga di scroll                        *
      *                  *---------------------------------------------*
           if        w-prs-rac-scr-des    =    00
                     go to pmt-fun-sdc-340
           else if   w-prs-rac-scr-des    =    01
                     go to pmt-fun-sdc-360
           else if   w-prs-rac-scr-des    =    02
                     go to pmt-fun-sdc-380
           else if   w-prs-rac-scr-des    =    03
                     go to pmt-fun-sdc-400
           else if   w-prs-rac-scr-des    =    04
                     go to pmt-fun-sdc-420
           else if   w-prs-rac-scr-des    =    05
                     go to pmt-fun-sdc-440
           else if   w-prs-rac-scr-des    =    06
                     go to pmt-fun-sdc-460
           else if   w-prs-rac-scr-des    =    11
                     go to pmt-fun-sdc-480
           else if   w-prs-rac-scr-des    =    12
                     go to pmt-fun-sdc-500
           else if   w-prs-rac-scr-des    =    13
                     go to pmt-fun-sdc-520
           else if   w-prs-rac-scr-des    =    14
                     go to pmt-fun-sdc-540
           else      go to pmt-fun-sdc-340.
       pmt-fun-sdc-340.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 00              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "               Descrizione              | In Bolla
      -              "  | Duplicare |               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "----------------------------------------|---------
      -              "--|-----------|               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-360.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 01              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       Descrizione            Codice    | In Bolla
      -              "  | Duplicare |               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "------------------------- --------------|---------
      -              "--|-----------|               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-380.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 02              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "     Descrizione            Codice      | In Bolla
      -              "  | Duplicare |               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "--------------------- ------------------|---------
      -              "--|-----------|               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-400.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 03              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "    Codice            Descrizione       | In Bolla
      -              "  | Duplicare |               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "-------------- -------------------------|---------
      -              "--|-----------|               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-420.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 04              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      Codice            Descrizione     | In Bolla
      -              "  | Duplicare |               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "------------------ ---------------------|---------
      -              "--|-----------|               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-440.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 05              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "       Descrizione            Codice    | In Bolla
      -              "  | Duplicare |               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "------------------------- --------------|---------
      -              "--|-----------|               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-460.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 06              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "     Descrizione            Codice      | In Bolla
      -              "  | Duplicare |               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "--------------------- ------------------|---------
      -              "--|-----------|               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-480.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 11              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  Codice del fornitore        Codice    | In Bolla
      -              "  | Duplicare |               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "------------------------- --------------|---------
      -              "--|-----------|               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-500.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 12              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " Codice del fornitore       Codice      | In Bolla
      -              "  | Duplicare |               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "--------------------- ------------------|---------
      -              "--|-----------|               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-520.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 13              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "    Codice       Codice del fornitore   | In Bolla
      -              "  | Duplicare |               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "-------------- -------------------------|---------
      -              "--|-----------|               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-540.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 14              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di fincatura                      *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "      Codice        Codice del fornitore| In Bolla
      -              "  | Duplicare |               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "------------------ ---------------------|---------
      -              "--|-----------|               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     pmt-fun-sdc-700.
       pmt-fun-sdc-700.
      *              *-------------------------------------------------*
      *              * Colonne per saldaconto                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-wrk-c01          .
       pmt-fun-sdc-710.
           add       1                    to   w-sdc-wrk-c01          .
           if        w-sdc-wrk-c01        >    14
                     go to pmt-fun-sdc-900.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      01                   to   v-pos                  .
           move      "                                        |         
      -              "  |           |               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     pmt-fun-sdc-710.
       pmt-fun-sdc-900.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       pmt-fun-sdc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione pagina saldaconto                         *
      *    *-----------------------------------------------------------*
       vis-pag-sdc-000.
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Aggiornamento numero pagina visualizzata        *
      *              *-------------------------------------------------*
           move      w-sdc-npg-dat        to   w-sdc-npg-vis          .
      *              *-------------------------------------------------*
      *              * Determinazione contatore righe iniziale         *
      *              *-------------------------------------------------*
           multiply  14                   by   w-sdc-npg-dat
                                      giving   w-sdc-wrk-rig          .
           subtract  14                   from w-sdc-wrk-rig          .
      *              *-------------------------------------------------*
      *              * Ciclo di visualizzazione                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-wrk-c01          .
       vis-pag-sdc-100.
           add       1                    to   w-sdc-wrk-c01          .
           if        w-sdc-wrk-c01        >    14
                     go to  vis-pag-sdc-900.
           add       1                    to   w-sdc-wrk-rig          .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda che sia una riga vuo-  *
      *                  * ta o piena                                  *
      *                  *---------------------------------------------*
           if        w-sdc-wrk-rig        >    w-brb-num-ele
                     go to vis-pag-sdc-200
           else      go to vis-pag-sdc-300.
       vis-pag-sdc-200.
      *                  *---------------------------------------------*
      *                  * Se riga vuota                               *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      01                   to   v-pos                  .
           move      "                                        |         
      -              "  |           |               "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     vis-pag-sdc-100.
       vis-pag-sdc-300.
      *                  *---------------------------------------------*
      *                  * Visualizzazione riga piena                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      01                   to   v-pos                  .
           move      w-brb-des-rig
                    (w-sdc-wrk-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Quantita' in bolla                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing quantita' da incolonnare    *
      *                          *-------------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      w-brb-qta-bfo
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-qta      .
           move      w-brb-dec-qta
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-dec      .
           move      "S"                  to   w-edt-qta-inc-sgn      .
           move      11                   to   w-edt-qta-inc-car      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      42                   to   v-pos                  .
           move      w-edt-qta-inc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo riga    *
      *                      *-----------------------------------------*
           if        w-brb-tip-rig
                    (w-sdc-wrk-rig)       not  = "C" and
                     w-brb-tip-rig
                    (w-sdc-wrk-rig)       not  = "A"
                     go to vis-pag-sdc-320.
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      59                   to   v-pos                  .
           if        w-brb-snx-aoc
                    (w-sdc-wrk-rig)       =    "S"
                     move  "Si"           to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A riciclo su riga successiva            *
      *                      *-----------------------------------------*
           go to     vis-pag-sdc-400.
       vis-pag-sdc-320.
      *                      *-----------------------------------------*
      *                      * Se altro tipo riga                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Quantita' fatturata                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Editing quantita'               *
      *                              *---------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      w-brb-qta-dup
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-qta      .
           move      w-brb-dec-qta
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-dec      .
           move      "S"                  to   w-edt-qta-inc-sgn      .
           move      11                   to   w-edt-qta-inc-car      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
      *                              *---------------------------------*
      *                              * Visualizzazione                 *
      *                              *---------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      54                   to   v-pos                  .
           move      w-edt-qta-inc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-sdc-400.
      *                      *-----------------------------------------*
      *                      * Riciclo su riga successiva              *
      *                      *-----------------------------------------*
           go to     vis-pag-sdc-100.
       vis-pag-sdc-900.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-pag-sdc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo saldaconto : Quantita' fatturata       *
      *    *-----------------------------------------------------------*
       acc-qta-dup-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-brb-tip-rig
                    (w-sdc-ctr-rig)       =    "A" or
                     w-brb-tip-rig
                    (w-sdc-ctr-rig)       =    "C"
                     go to acc-qta-dup-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-brb-qta-dup
                    (w-sdc-ctr-rig)       to   w-sav-qta-ffo          .
           move      w-brb-flg-fzs
                    (w-sdc-ctr-rig)       to   w-sav-flg-fzs          .
       acc-qta-dup-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      w-brb-dec-qta
                    (w-sdc-ctr-rig)       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      54                   to   v-pos                  .
           if        w-sdc-ctr-rig        >    1      
                     move   "UP  "        to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-sdc-npg-dat        <    w-sdc-npg-max
                     move   "NXSC"        to   v-pfk (06)             .
           if        w-sdc-npg-dat        >    1
                     move   "PRSC"        to   v-pfk (07)             .
           if        w-sdc-ctr-rig        <    w-brb-num-ele
                     move   "TAB "        to   v-pfk (08)             .
           if        w-sdc-ctr-rig        >    1
                     move   "BACK"        to   v-pfk (09)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-brb-qta-dup
                    (w-sdc-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-qta-dup-999.
       acc-qta-dup-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-brb-qta-dup
                                              (w-sdc-ctr-rig)         .
       acc-qta-dup-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo compatibilita' fra segno quantita'*
      *                  * in fattura e segno quantita' in bolla       *
      *                  *---------------------------------------------*
           if        w-brb-qta-bfo
                    (w-sdc-ctr-rig)       >    zero and
                     w-brb-qta-dup
                    (w-sdc-ctr-rig)       <    zero
                     go to acc-qta-dup-100
           else if   w-brb-qta-bfo
                    (w-sdc-ctr-rig)       <    zero and
                     w-brb-qta-dup
                    (w-sdc-ctr-rig)       >    zero
                     go to acc-qta-dup-100.
      *                  *---------------------------------------------*
      *                  * Se Pf1 : controllo che il valore impostato  *
      *                  * non sia zero                                *
      *                  *---------------------------------------------*
           if        v-key                not  = "[1] "
                     go to acc-qta-dup-420.
           if        w-brb-qta-dup
                    (w-sdc-ctr-rig)       =    zero
                     go to acc-qta-dup-100.
       acc-qta-dup-420.
      *                  *---------------------------------------------*
      *                  * Se Slct : controllo che il valore impostato *
      *                  * sia uguale al precedente                    *
      *                  *---------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-qta-dup-600.
           if        w-brb-qta-dup
                    (w-sdc-ctr-rig)       not  = w-sav-qta-ffo
                     move   w-sav-qta-ffo to   w-brb-qta-dup
                                              (w-sdc-ctr-rig)
                     go to  acc-qta-dup-100.
       acc-qta-dup-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Quantita' in bolla e quantita' in fattura   *
      *                  * in work di comodo senza segno               *
      *                  *---------------------------------------------*
           move      w-brb-qta-bfo
                    (w-sdc-ctr-rig)       to   w-qss-qta-bfo          .
           move      w-brb-qta-dup
                    (w-sdc-ctr-rig)       to   w-qss-qta-ffo          .
      *                  *---------------------------------------------*
      *                  * Se Slct                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to  acc-qta-dup-620.
      *                      *-----------------------------------------*
      *                      * Se valore gia' presente                 *
      *                      *-----------------------------------------*
           if        w-brb-qta-dup
                    (w-sdc-ctr-rig)       =    zero
                     go to acc-qta-dup-610.
      *                          *-------------------------------------*
      *                          * Quantita' in fattura                *
      *                          *-------------------------------------*
           move      zero                 to   w-brb-qta-dup
                                              (w-sdc-ctr-rig)         .
           move      zero                 to   w-qss-qta-ffo          .
      *                          *-------------------------------------*
      *                          * Flag di saldo forzato               *
      *                          *-------------------------------------*
           move      spaces               to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-700.
       acc-qta-dup-610.
      *                      *-----------------------------------------*
      *                      * Se valore non presente                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Quantita' in fattura                *
      *                          *-------------------------------------*
           move      w-brb-qta-bfo
                    (w-sdc-ctr-rig)       to   w-brb-qta-dup
                                              (w-sdc-ctr-rig)         .
           move      w-brb-qta-bfo
                    (w-sdc-ctr-rig)       to   w-qss-qta-ffo          .
      *                          *-------------------------------------*
      *                          * Flag di saldo forzato               *
      *                          *-------------------------------------*
           move      spaces               to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-700.
       acc-qta-dup-620.
      *                  *---------------------------------------------*
      *                  * Se Pf1                                      *
      *                  *---------------------------------------------*
           if        v-key                not  = "[1] "
                     go to  acc-qta-dup-640.
      *                      *-----------------------------------------*
      *                      * Flag di saldo forzato                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se quantita' in fattura maggiore o  *
      *                          * uguale a quella in bolla : spaces   *
      *                          *-------------------------------------*
           if        w-qss-qta-ffo        <    w-qss-qta-bfo
                     go to  acc-qta-dup-630.
           move      spaces               to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-700.
       acc-qta-dup-630.
      *                          *-------------------------------------*
      *                          * Altrimenti : si inverte il valore   *
      *                          * attuale del flag                    *
      *                          *-------------------------------------*
           if        w-brb-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     move  "S"            to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)
           else      move  spaces         to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-700.
       acc-qta-dup-640.
      *                  *---------------------------------------------*
      *                  * Se Return o altri tasti funzione            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di saldo forzato                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se valore impostato pari al prece-  *
      *                          * dente : nessuna variazione          *
      *                          *-------------------------------------*
           if        w-brb-qta-dup
                    (w-sdc-ctr-rig)        not  = w-sav-qta-ffo
                     go to  acc-qta-dup-650.
           go to     acc-qta-dup-700.
       acc-qta-dup-650.
      *                          *-------------------------------------*
      *                          * Se valore impostato diverso dal     *
      *                          * precedente                          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in fattura maggio- *
      *                              * re o uguale a quella in bolla : *
      *                              * spaces                          *
      *                              *---------------------------------*
           if        w-qss-qta-ffo        <    w-qss-qta-bfo
                     go to  acc-qta-dup-655.
           move      spaces               to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-700.
       acc-qta-dup-655.
      *                              *---------------------------------*
      *                              * Altrimenti : valore inalterato  *
      *                              *---------------------------------*
           go to     acc-qta-dup-700.
       acc-qta-dup-700.
      *                  *---------------------------------------------*
      *                  * Trattamento altri valori riga               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag indicatore di residuo              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato a 'S'      *
      *                          *-------------------------------------*
           if        w-brb-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     go to acc-qta-dup-710.
           move      spaces               to   w-brb-flg-idr
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-720.
       acc-qta-dup-710.
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato diverso da *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in fattura maggio- *
      *                              * re o uguale a quella in bolla : *
      *                              * spaces                          *
      *                              *---------------------------------*
           if        w-qss-qta-ffo        <    w-qss-qta-bfo
                     go to  acc-qta-dup-715.
           move      spaces               to   w-brb-flg-idr
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-720.
       acc-qta-dup-715.
      *                              *---------------------------------*
      *                              * Altrimenti : il valore viene    *
      *                              * forzato a '<'                   *
      *                              *---------------------------------*
           move      "<"                  to   w-brb-flg-idr
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-720.
       acc-qta-dup-720.
      *                      *-----------------------------------------*
      *                      * Flag indicatore di saldo                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato a 'S'      *
      *                          *-------------------------------------*
           if        w-brb-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     go to acc-qta-dup-730.
           move      "S"                  to   w-brb-flg-ids
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-740.
       acc-qta-dup-730.
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato diverso da *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in fattura minore  *
      *                              * o uguale a quella in bolla :    *
      *                              * viene forzato il valore spaces  *
      *                              *---------------------------------*
           if        w-qss-qta-ffo        >    w-qss-qta-bfo
                     go to  acc-qta-dup-735.
           move      spaces               to   w-brb-flg-ids
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-740.
       acc-qta-dup-735.
      *                              *---------------------------------*
      *                              * Altrimenti : il valore viene    *
      *                              * forzato a "+"                   *
      *                              *---------------------------------*
           move      "+"                  to   w-brb-flg-ids
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-740.
       acc-qta-dup-740.
      *                      *-----------------------------------------*
      *                      * Quantita' residua                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato a 'S'      *
      *                          *-------------------------------------*
           if        w-brb-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     go to acc-qta-dup-750.
           move      zero                 to   w-brb-qta-res
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-760.
       acc-qta-dup-750.
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato diverso da *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in fattura maggio- *
      *                              * re o uguale a quella in bolla : *
      *                              * spaces                          *
      *                              *---------------------------------*
           if        w-qss-qta-ffo        <    w-qss-qta-bfo
                     go to  acc-qta-dup-755.
           move      zero                 to   w-brb-qta-res
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-760.
       acc-qta-dup-755.
      *                              *---------------------------------*
      *                              * Altrimenti : il valore e' pari  *
      *                              * alla differenza tra quantita'   *
      *                              * in bolla e quantita' in fattura *
      *                              *---------------------------------*
           subtract  w-brb-qta-dup
                    (w-sdc-ctr-rig)       from w-brb-qta-bfo
                                              (w-sdc-ctr-rig)
                                        giving w-brb-qta-res
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-760.
       acc-qta-dup-760.
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori riga                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Quantita' in fattura                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing quantita'                   *
      *                          *-------------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      w-brb-qta-dup
                    (w-sdc-ctr-rig)       to   w-edt-qta-inc-qta      .
           move      w-brb-dec-qta
                    (w-sdc-ctr-rig)       to   w-edt-qta-inc-dec      .
           move      "S"                  to   w-edt-qta-inc-sgn      .
           move      11                   to   w-edt-qta-inc-car      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-edt-qta-inc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-qta-dup-800.
       acc-qta-dup-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo saldaconto : Si/No Addebito o Commento *
      *    *-----------------------------------------------------------*
       acc-snx-aoc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-brb-tip-rig
                    (w-sdc-ctr-rig)       not  = "A" and
                     w-brb-tip-rig
                    (w-sdc-ctr-rig)       not  = "C"
                     go to acc-snx-aoc-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-brb-snx-aoc
                    (w-sdc-ctr-rig)       to   w-sav-snx-aoc          .
       acc-snx-aoc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      59                   to   v-pos                  .
      *                  *---------------------------------------------*
      *                  * Tasti funzione                              *
      *                  *---------------------------------------------*
           if        w-sdc-ctr-rig        >    1      
                     move   "UP  "        to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-sdc-npg-dat        <    w-sdc-npg-max
                     move   "NXSC"        to   v-pfk (06)             .
           if        w-sdc-npg-dat        >    1
                     move   "PRSC"        to   v-pfk (07)             .
           if        w-sdc-ctr-rig        <    w-brb-num-ele
                     move   "TAB "        to   v-pfk (08)             .
           if        w-sdc-ctr-rig        >    1
                     move   "BACK"        to   v-pfk (09)             .
       acc-snx-aoc-150.
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-brb-snx-aoc
                    (w-sdc-ctr-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-snx-aoc-999.
       acc-snx-aoc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-brb-snx-aoc
                                              (w-sdc-ctr-rig)         .
      *              *-------------------------------------------------*
      *              * Se Slct                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to  acc-snx-aoc-400.
      *                  *---------------------------------------------*
      *                  * Se campo modificato : ripristino valore pre-*
      *                  * cedente e reimpostazione                    *
      *                  *---------------------------------------------*
           if        w-brb-snx-aoc
                    (w-sdc-ctr-rig)       not  = w-sav-snx-aoc
                     move   w-sav-snx-aoc to   w-brb-snx-aoc
                                              (w-sdc-ctr-rig)
                     go to  acc-snx-aoc-100.
      *                  *---------------------------------------------*
      *                  * Se valore presente lo si abblenca, altri-   *
      *                  * menti lo si forza a 'S'                     *
      *                  *---------------------------------------------*
           if        w-brb-snx-aoc
                    (w-sdc-ctr-rig)       not  = spaces
                     move  spaces         to   w-brb-snx-aoc
                                              (w-sdc-ctr-rig)
           else      move  "S"            to   w-brb-snx-aoc
                                              (w-sdc-ctr-rig)         .
       acc-snx-aoc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore non ammesso : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-brb-snx-aoc
                    (w-sdc-ctr-rig)       not  = "S" and
                     w-brb-snx-aoc
                    (w-sdc-ctr-rig)       not  = "N" and
                     w-brb-snx-aoc
                    (w-sdc-ctr-rig)       not  = spaces
                     go to acc-snx-aoc-100.
       acc-snx-aoc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione valore                      *
      *                  *---------------------------------------------*
           if        w-brb-snx-aoc
                    (w-sdc-ctr-rig)       =    "N"
                     move  spaces         to   w-brb-snx-aoc
                                              (w-sdc-ctr-rig)         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione Si/No addebito o commento   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      59                   to   v-pos                  .
           if        w-brb-snx-aoc
                    (w-sdc-ctr-rig)       =    "S"
                     move  "Si"           to   v-alf
           else      move  spaces         to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-snx-aoc-800.
       acc-snx-aoc-999.
           exit.

      *    *===========================================================*
      *    * Caricamento righe in catena                               *
      *    *-----------------------------------------------------------*
       car-rig-cat-000.
      *              *-------------------------------------------------*
      *              * Subroutine per la preparazione dei valori rela- *
      *              * tivi alla valuta base, determinati dalla segre- *
      *              * teria                                           *
      *              *-------------------------------------------------*
           move      "VB"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dec                to   c-dec                  .
           move      s-asx                to   c-sgl                  .
           move      s-sgn                to   c-tdc                  .
           move      s-num                to   c-cdc                  .
           move      s-adx (01 : 20)      to   c-des                  .
           move      s-adx (21 : 20)      to   c-din                  .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione buffer righe bolla           *
      *              *-------------------------------------------------*
           move      zero                 to   w-crc-wrk-ctr          .
       car-rig-cat-100.
           add       1                    to   w-crc-wrk-ctr          .
           if        w-crc-wrk-ctr        >    w-brb-num-ele
                     go to car-rig-cat-800.
      *                  *---------------------------------------------*
      *                  * Selezione su righe ordine nel buffer        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo riga    *
      *                      *-----------------------------------------*
           if        w-brb-tip-rig
                    (w-crc-wrk-ctr)       =    "A" or
                     w-brb-tip-rig
                    (w-crc-wrk-ctr)       =    "C"
                     go to car-rig-cat-110
           else      go to car-rig-cat-120.
       car-rig-cat-110.
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento          *
      *                      *-----------------------------------------*
           if        w-brb-snx-aoc
                    (w-crc-wrk-ctr)       not  = "S"
                     go to car-rig-cat-100.
           go to     car-rig-cat-150.
       car-rig-cat-120.
      *                      *-----------------------------------------*
      *                      * Se altro tipo riga                      *
      *                      *-----------------------------------------*
           if        w-brb-qta-dup
                    (w-crc-wrk-ctr)       =    zero
                     go to car-rig-cat-100.
           go to     car-rig-cat-150.
       car-rig-cat-150.
      *                  *---------------------------------------------*
      *                  * Append in catena                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Append non possibile : uscita con    *
      *                      * errore                                  *
      *                      *-----------------------------------------*
           if        w-cat-rig-app        not  = spaces
                     go to  car-rig-cat-900.
      *                      *-----------------------------------------*
      *                      * Append                                  *
      *                      *-----------------------------------------*
           move      "AP"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
       car-rig-cat-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione riga                        *
      *                  *---------------------------------------------*
           perform   nor-nok-rig-000      thru nor-nok-rig-999        .
           move      w-rig-val-aep (1)    to   w-rig-val-aep (2)      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [bfr]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [bfr]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-gen-prt-bfo        to   rf-bfr-num-prt         .
           move      w-brb-num-prg
                    (w-crc-wrk-ctr)       to   rf-bfr-num-prg         .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
       car-rig-cat-300.
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
       car-rig-cat-400.
      *                      *-----------------------------------------*
      *                      * Valori contenuti direttamente in re-    *
      *                      * cord [bfr]                              *
      *                      *-----------------------------------------*
           move      w-cat-rig-prg        to   w-rig-num-prg (1)      .
           move      rf-bfr-bld-flb       to   w-rig-bld-flb (1)      .
           move      rf-bfr-bld-tpb       to   w-rig-bld-tpb (1)      .
           move      rf-bfr-bld-rgb       to   w-rig-bld-rgb (1)      .
           move      rf-bfr-tip-rig       to   w-rig-tip-rig (1)      .
           perform   dec-tip-rig-000      thru dec-tip-rig-999        .
           move      rf-bfr-tip-mag       to   w-rig-tip-mag (1)      .
           move      rf-bfr-num-mag       to   w-rig-num-pro (1)      .
           move      rf-bfr-alf-mag       to   w-rig-alf-pro (1)      .
           move      rf-bfr-sgl-vrn       to   w-rig-sgl-vrn (1)      .
           move      spaces               to   w-rig-cop-scl (1)      .
           move      rf-bfr-des-ext       to   w-rig-des-ext (1)      .
           move      rf-bfr-des-rig       to   w-rig-des-rig (1)      .
           move      rf-bfr-tip-pro       to   w-rig-tip-pro (1)      .
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-500.
      *
           move      rf-bfr-cod-iva       to   w-rig-coi-rig (1)      .
           move      rf-bfr-ctp-acq       to   w-rig-ctv-rig (1)      .
           move      rf-bfr-umi-acq       to   w-rig-umi-ven (1)      .
           move      rf-bfr-dec-qta       to   w-rig-dec-qta (1)      .
           move      rf-bfr-snx-2qt       to   w-rig-snx-2qt (1)      .
           move      rf-bfr-dec-2qt       to   w-rig-dec-2qt (1)      .
           move      rf-bfr-qta-a02       to   w-rig-qta-a02 (1)      .
           move      rf-bfr-snx-3qt       to   w-rig-snx-3qt (1)      .
           move      rf-bfr-dec-3qt       to   w-rig-dec-3qt (1)      .
           move      rf-bfr-qta-a03       to   w-rig-qta-a03 (1)      .
           move      rf-bfr-dec-prz       to   w-rig-dec-prz (1)      .
           move      rf-bfr-prz-acq       to   w-rig-prz-ven (1)      .
           move      rf-bfr-snx-2pz       to   w-rig-snx-2pz (1)      .
           move      rf-bfr-prz-a02       to   w-rig-prz-a02 (1)      .
           move      rf-bfr-sgl-vpl       to   w-rig-sgl-vpl (1)      .
           move      rf-bfr-dec-vpl       to   w-rig-dec-vpl (1)      .
           move      rf-bfr-tdc-vpl       to   w-rig-tdc-vpl (1)      .
           move      rf-bfr-prz-vpl       to   w-rig-prz-vpl (1)      .
           move      rf-bfr-cdc-vpl       to   w-rig-cdc-vpl (1)      .
           move      rf-bfr-ccr-vpl       to   w-rig-ccr-vpl (1)      .
           move      rf-bfr-plm-vpl       to   w-rig-plm-vpl (1)      .
           move      rf-bfr-tlm-vpl       to   w-rig-tlm-vpl (1)      .
           move      rf-bfr-map-vpl       to   w-rig-map-vpl (1)      .
           move      rf-bfr-epz-rgf       to   w-rig-epz-rgf (1)      .
           move      rf-bfr-per-scr (1)   to   w-rig-per-scr (1, 1)   .
           move      rf-bfr-per-scr (2)   to   w-rig-per-scr (1, 2)   .
           move      rf-bfr-per-scr (3)   to   w-rig-per-scr (1, 3)   .
           move      rf-bfr-per-scr (4)   to   w-rig-per-scr (1, 4)   .
           move      rf-bfr-per-scr (5)   to   w-rig-per-scr (1, 5)   .
           move      rf-bfr-prz-net       to   w-rig-prz-net (1)      .
           move      rf-bfr-iau-rig       to   w-rig-iau-rig (1)      .
           move      rf-bfr-cpv-aap       to   w-rig-cpv-aap (1)      .
           move      rf-bfr-ppv-aap (1)   to   w-rig-ppv-aap (1, 1)   .
           move      rf-bfr-ppv-aap (2)   to   w-rig-ppv-aap (1, 2)   .
           move      rf-bfr-ppv-aap (3)   to   w-rig-ppv-aap (1, 3)   .
           move      rf-bfr-fsp-rig       to   w-rig-fsp-rig (1)      .
           move      rf-bfr-cpv-rig       to   w-rig-cpv-rig (1)      .
           move      rf-bfr-ppv-rig (1)   to   w-rig-ppv-rig (1, 1)   .
           move      rf-bfr-ppv-rig (2)   to   w-rig-ppv-rig (1, 2)   .
           move      rf-bfr-ppv-rig (3)   to   w-rig-ppv-rig (1, 3)   .
           move      rf-bfr-pvf-rig       to   w-rig-pvf-rig (1)      .
       car-rig-cat-500.
      *                      *-----------------------------------------*
      *                      * Valori contenuti in buffer righe ordine *
      *                      *-----------------------------------------*
       car-rig-cat-505.
      *                          *-------------------------------------*
      *                          * Deviazione a seconda del tipo riga  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-510
           else      go to car-rig-cat-515.
       car-rig-cat-510.
      *                          *-------------------------------------*
      *                          * Se riga di addebito o commento      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Quantita' secondo noi           *
      *                              *---------------------------------*
           move      zero                 to   w-rig-qta-ven (1)      .
      *                              *---------------------------------*
      *                              * Importo in riga                 *
      *                              *---------------------------------*
           move      rf-bfr-imp-rig       to   w-rig-imp-rig (1)      .
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     car-rig-cat-600.
       car-rig-cat-515.
      *                          *-------------------------------------*
      *                          * Se riga con quantita'               *
      *                          *-------------------------------------*
       car-rig-cat-520.
      *                              *---------------------------------*
      *                              * Richiamo routine per la deter-  *
      *                              * minazione della quantita' se-   *
      *                              * condaria                        *
      *                              *---------------------------------*
           move      w-brb-qta-dup
                    (w-crc-wrk-ctr)       to   w-det-qta-sec-qim      .
           move      w-rig-dec-qta (1)    to   w-det-qta-sec-dgm      .
           perform   det-qta-sec-000      thru det-qta-sec-999        .
       car-rig-cat-525.
      *                              *---------------------------------*
      *                              * Quantita' secondo il formato di *
      *                              * acquisizione                    *
      *                              *---------------------------------*
       car-rig-cat-530.
      *                              *---------------------------------*
      *                              * Quantita' secondo noi           *
      *                              *---------------------------------*
           move  w-brb-qta-dup
                (w-crc-wrk-ctr)           to   w-rig-qta-ven (1)      .
       car-rig-cat-540.
      *                              *---------------------------------*
      *                              * Trattamento legame valutario    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Test se da effettuare       *
      *                                  *-----------------------------*
           if        w-rig-sgl-vpl (1)    =    spaces
                     go to car-rig-cat-560.
      *                                  *-----------------------------*
      *                                  * Ricerca sigla valuta in ta- *
      *                                  * bella valute per legame va- *
      *                                  * lutario                     *
      *                                  *-----------------------------*
           move      zero                 to   w-vpl-ctr-001          .
       car-rig-cat-542.
           add       1                    to   w-vpl-ctr-001          .
           if        w-vpl-ctr-001        >    w-vpl-num-ele
                     go to car-rig-cat-560.
           if        w-rig-sgl-vpl (1)    =    w-vpl-sgl-vpl
                                              (w-vpl-ctr-001)
                     go to car-rig-cat-544.
           go to     car-rig-cat-542.
       car-rig-cat-544.
      *                                  *-----------------------------*
      *                                  * Bufferizzazione coefficien- *
      *                                  * te di cambio applicato      *
      *                                  *-----------------------------*
           move      w-vpl-cdc-vpl
                    (w-vpl-ctr-001)       to   w-rig-cdc-vpl (1)      .
      *                                  *-----------------------------*
      *                                  * Salvataggio prezzo di ac-   *
      *                                  * quisto precedente           *
      *                                  *-----------------------------*
           move      w-rig-prz-ven (1)    to   w-sav-prz-acq          .
      *                                  *-----------------------------*
      *                                  * Applicazione cambio per le- *
      *                                  * game valutario              *
      *                                  *-----------------------------*
           move      w-rig-prz-vpl (1)    to   w-rig-prz-ven (1)      .
      *                                  *-----------------------------*
      *                                  * Parametri in input          *
      *                                  *-----------------------------*
           move      w-rig-prz-ven (1)    to   w-lvl-prz-prz          .
           move      w-rig-sgl-vpl (1)    to   w-lvl-prz-vlt          .
           move      w-rig-tdc-vpl (1)    to   w-lvl-prz-tdc          .
           move      w-rig-ccr-vpl (1)    to   w-lvl-prz-ccr          .
           move      w-rig-cdc-vpl (1)    to   w-lvl-prz-cdc          .
           move      w-rig-plm-vpl (1)    to   w-lvl-prz-plm          .
           move      w-rig-tlm-vpl (1)    to   w-lvl-prz-tlm          .
      *                                  *-----------------------------*
      *                                  * Determinazione              *
      *                                  *-----------------------------*
           perform   lvl-prz-det-000      thru lvl-prz-det-999        .
      *                                  *-----------------------------*
      *                                  * Valore determinato          *
      *                                  *-----------------------------*
           move      w-lvl-prz-prz        to   w-rig-prz-ven (1)      .
      *                                  *-----------------------------*
      *                                  * Se prezzo di acquisto inva- *
      *                                  * riato : oltre               *
      *                                  *-----------------------------*
           if        w-rig-prz-ven (1)    =    w-sav-prz-acq
                     go to car-rig-cat-560.
      *                                  *-----------------------------*
      *                                  * Determinazione prezzo netto *
      *                                  *-----------------------------*
           move      w-rig-prz-ven (1)    to   w-cal-prz-net-prz      .
           move      w-rig-per-scr (1, 1) to   w-cal-prz-net-psc (1)  .
           move      w-rig-per-scr (1, 2) to   w-cal-prz-net-psc (2)  .
           move      w-rig-per-scr (1, 3) to   w-cal-prz-net-psc (3)  .
           move      w-rig-per-scr (1, 4) to   w-cal-prz-net-psc (4)  .
           move      w-rig-per-scr (1, 5) to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   w-rig-prz-net (1)      .
       car-rig-cat-560.
      *                              *---------------------------------*
      *                              * Importo in riga                 *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione              *
      *                                  *-----------------------------*
           perform   det-imp-rig-000      thru det-imp-rig-999        .
       car-rig-cat-580.
      *                              *---------------------------------*
      *                              * Continuazione                   *
      *                              *---------------------------------*
           go to     car-rig-cat-600.
       car-rig-cat-600.
      *                      *-----------------------------------------*
      *                      * Valori non contenuti ne' direttamente   *
      *                      * e ne' indirettamente in record [bfr]    *
      *                      *-----------------------------------------*
           move      spaces               to   w-rig-flg-ela (1)      .
           move      spaces               to   w-rig-flg-pul (1)      .
           move      spaces               to   w-rig-alx-exp (1)      .
       car-rig-cat-700.
      *                      *-----------------------------------------*
      *                      * Valori contenuti indirettamente in re-  *
      *                      * cord [bfr]                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Si/No esistenza formato di acquisi- *
      *                          * zione                               *
      *                          *-------------------------------------*
           move      w-rig-tip-pro (1)    to   w-let-arc-aaf-tpm      .
           move      w-rig-num-pro (1)    to   w-let-arc-aaf-cdm      .
           move      w-acc-cod-arc        to   w-let-arc-aaf-fnt      .
           move      spaces               to   w-let-arc-aaf-fda      .
           move      w-let-arc-dcp-des    to   w-let-arc-aaf-dmg      .
           perform   let-arc-aaf-000      thru let-arc-aaf-999        .
      *                          *-------------------------------------*
      *                          * Descrizione                         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura preliminare file [bfx]  *
      *                              *---------------------------------*
           move      w-gen-prt-bfo        to   w-let-arc-bfx-prt      .
           move      w-brb-num-prg
                    (w-crc-wrk-ctr)       to   w-let-arc-bfx-prg      .
           move      11                   to   w-let-arc-bfx-trc      .
           perform   let-arc-bfx-000      thru let-arc-bfx-999        .
      *
           if        w-let-arc-bfx-flg    not  = spaces
                     go to car-rig-cat-705.
      *
           move      w-let-arc-bfx-des    to   w-rig-des-por (1)
                                               w-rig-des-rig (1)      .
      *
           go to     car-rig-cat-750.
       car-rig-cat-705.
      *                              *---------------------------------*
      *                              * Deviazione in funzione del tipo *
      *                              * di estensione                   *
      *                              *---------------------------------*
           if        w-rig-des-ext (1)    =    0
                     go to car-rig-cat-710
           else if   w-rig-des-ext (1)    =    1
                     go to car-rig-cat-720
           else if   w-rig-des-ext (1)    =    2
                     go to car-rig-cat-730
           else if   w-rig-des-ext (1)    =    3
                     go to car-rig-cat-740.
       car-rig-cat-710.
      *                              *---------------------------------*
      *                              * Se nessuna estensione : nessuna *
      *                              * azione in quanto la descrizione *
      *                              * e' gia' stata bufferizzata      *
      *                              *---------------------------------*
           go to     car-rig-cat-750.
       car-rig-cat-720.
      *                              *---------------------------------*
      *                              * Se estensione nel file [bfx]    *
      *                              *---------------------------------*
           move      w-gen-prt-bfo        to   w-let-arc-bfx-prt      .
           move      w-brb-num-prg
                    (w-crc-wrk-ctr)       to   w-let-arc-bfx-prg      .
           move      11                   to   w-let-arc-bfx-trc      .
           perform   let-arc-bfx-000      thru let-arc-bfx-999        .
           move      w-let-arc-bfx-des    to   w-rig-des-por (1)
                                               w-rig-des-rig (1)      .
           go to     car-rig-cat-750.
       car-rig-cat-730.
      *                              *---------------------------------*
      *                              * Se estensione nel file [pdx]    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura archivio [dcp]      *
      *                                  *-----------------------------*
           move      w-rig-num-pro (1)    to   w-let-arc-dcp-cod      .
           move      w-tes-cod-lng (1)    to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
           move      w-let-arc-dcp-des    to   w-rig-des-por (1)
                                               w-rig-des-rig (1)      .
           go to     car-rig-cat-750.
       car-rig-cat-740.
      *                              *---------------------------------*
      *                              * Se estensione nel file [pdx] ma *
      *                              * con tipo record 13 o 33         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura archivio [dcp]      *
      *                                  *-----------------------------*
           move      w-rig-num-pro (1)    to   w-let-arc-dcp-cod      .
           move      w-tes-cod-lng (1)    to   w-let-arc-dcp-lng      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
      *                                  *-----------------------------*
      *                                  * Lettura archivio [aaf]      *
      *                                  *-----------------------------*
           move      w-rig-tip-pro (1)    to   w-let-arc-aaf-tpm      .
           move      w-rig-num-pro (1)    to   w-let-arc-aaf-cdm      .
           move      w-acc-cod-arc        to   w-let-arc-aaf-fnt      .
           move      spaces               to   w-let-arc-aaf-fda      .
           move      w-let-arc-dcp-des    to   w-let-arc-aaf-dmg      .
           perform   let-arc-aaf-000      thru let-arc-aaf-999        .
           move      w-let-arc-aaf-des    to   w-rig-des-por (1)
                                               w-rig-des-rig (1)      .
           go to     car-rig-cat-750.
       car-rig-cat-750.
      *                              *---------------------------------*
      *                              * Si/No aggiornamenti di magazzi- *
      *                              * no                              *
      *                              *---------------------------------*
       car-rig-cat-760.
      *                          *-------------------------------------*
      *                          * Anagrafica codici iva               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se documento che non interessa  *
      *                              * la fatturazione : oltre         *
      *                              *---------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-780.
      *
           move      w-rig-coi-rig (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
           move      w-let-arc-zci-des    to   w-rig-coi-rig-des (1)  .
      *                          *-------------------------------------*
      *                          * Anagrafica piano dei conti          *
      *                          *-------------------------------------*
           move      w-rig-ctv-rig (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-rig-ctv-rig-des (1)  .
       car-rig-cat-780.
      *                  *---------------------------------------------*
      *                  * Update catena                               *
      *                  *---------------------------------------------*
           move      "UP"                 to   w-cat-rig-ope          .
           move      w-cat-rig-max        to   w-cat-rig-num          .
           move      w-rig                to   w-cat-rig-buf          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Riciclo su riga ordine successiva               *
      *              *-------------------------------------------------*
           go to     car-rig-cat-100.
       car-rig-cat-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     car-rig-cat-999.
       car-rig-cat-900.
      *              *-------------------------------------------------*
      *              * Messaggio di errore per esubero numero righe    *
      *              *-------------------------------------------------*
           move      "Numero righe documento oltre il massimo previsto !
      -              ""
                                          to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       car-rig-cat-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per gestione catena righe         *
      *    *-----------------------------------------------------------*
       cll-sub-cat-000.
           call      "pgm/bol/prg/obj/pbol3002"
                                         using w-cat-rig              .
       cll-sub-cat-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave riga corpo                *
      *    *-----------------------------------------------------------*
       nor-nok-rig-000.
           move      zero                 to   w-rig-num-prg (1)      .
           move      zero                 to   w-rig-bld-flb (1)      .
           move      zero                 to   w-rig-bld-tpb (1)      .
           move      zero                 to   w-rig-bld-rgb (1)      .
           move      spaces               to   w-rig-tip-rig (1)      .
           move      spaces               to   w-rig-tip-rig-tpr (1)  .
           move      spaces               to   w-rig-tip-rig-tfu (1)  .
           move      zero                 to   w-rig-tip-rig-cac (1)  .
           move      spaces               to   w-rig-tip-rig-ast (1)  .
           move      zero                 to   w-rig-tip-mag (1)      .
           move      zero                 to   w-rig-num-pro (1)      .
           move      spaces               to   w-rig-alf-pro (1)      .
           move      spaces               to   w-rig-sgl-vrn (1)      .
           move      spaces               to   w-rig-cop-scl (1)      .
           move      zero                 to   w-rig-des-ext (1)      .
           move      spaces               to   w-rig-des-por (1)      .
           move      spaces               to   w-rig-des-rig (1)      .
           move      zero                 to   w-rig-tip-pro (1)      .
           move      zero                 to   w-rig-coi-rig (1)      .
           move      spaces               to   w-rig-coi-rig-des (1)  .
           move      zero                 to   w-rig-ctv-rig (1)      .
           move      spaces               to   w-rig-ctv-rig-des (1)  .
           move      spaces               to   w-rig-umi-ven (1)      .
           move      zero                 to   w-rig-dec-qta (1)      .
           move      zero                 to   w-rig-qta-ven (1)      .
           move      zero                 to   w-rig-snx-2qt (1)      .
           move      zero                 to   w-rig-dec-2qt (1)      .
           move      zero                 to   w-rig-qta-a02 (1)      .
           move      zero                 to   w-rig-snx-3qt (1)      .
           move      zero                 to   w-rig-dec-3qt (1)      .
           move      zero                 to   w-rig-qta-a03 (1)      .
           move      zero                 to   w-rig-dec-prz (1)      .
           move      spaces               to   w-rig-sgl-vps (1)      .
           move      zero                 to   w-rig-dec-vps (1)      .
           move      spaces               to   w-rig-tdc-vps (1)      .
           move      zero                 to   w-rig-cdc-vps (1)      .
           move      zero                 to   w-rig-prz-lrs (1)      .
           move      zero                 to   w-rig-prz-nts (1)      .
           move      spaces               to   w-rig-sgl-vpp (1)      .
           move      zero                 to   w-rig-dec-vpp (1)      .
           move      spaces               to   w-rig-tdc-vpp (1)      .
           move      zero                 to   w-rig-cdc-vpp (1)      .
           move      zero                 to   w-rig-prz-acc (1)      .
           move      zero                 to   w-rig-prz-ven (1)      .
           move      zero                 to   w-rig-snx-2pz (1)      .
           move      zero                 to   w-rig-prz-a02 (1)      .
           move      spaces               to   w-rig-sgl-vpl (1)      .
           move      zero                 to   w-rig-dec-vpl (1)      .
           move      spaces               to   w-rig-tdc-vpl (1)      .
           move      zero                 to   w-rig-prz-vpl (1)      .
           move      zero                 to   w-rig-cdc-vpl (1)      .
           move      zero                 to   w-rig-ccr-vpl (1)      .
           move      zero                 to   w-rig-plm-vpl (1)      .
           move      spaces               to   w-rig-tlm-vpl (1)      .
           move      spaces               to   w-rig-map-vpl (1)      .
           move      zero                 to   w-rig-epz-rgf (1)      .
           move      zero                 to   w-rig-csr-aap (1)      .
           move      zero                 to   w-rig-psr-aap (1, 1)   .
           move      zero                 to   w-rig-psr-aap (1, 2)   .
           move      zero                 to   w-rig-psr-aap (1, 3)   .
           move      zero                 to   w-rig-psr-aap (1, 4)   .
           move      zero                 to   w-rig-psr-aap (1, 5)   .
           move      zero                 to   w-rig-per-scr (1, 1)   .
           move      zero                 to   w-rig-per-scr (1, 2)   .
           move      zero                 to   w-rig-per-scr (1, 3)   .
           move      zero                 to   w-rig-per-scr (1, 4)   .
           move      zero                 to   w-rig-per-scr (1, 5)   .
           move      zero                 to   w-rig-prz-net (1)      .
           move      zero                 to   w-rig-epz-pes (1)      .
           move      spaces               to   w-rig-sgl-vpc (1)      .
           move      zero                 to   w-rig-dec-vpc (1)      .
           move      spaces               to   w-rig-tdc-vpc (1)      .
           move      zero                 to   w-rig-cdc-vpc (1)      .
           move      zero                 to   w-rig-dec-cos (1)      .
           move      zero                 to   w-rig-cos-rif (1)      .
           move      zero                 to   w-rig-imp-rig (1)      .
           move      zero                 to   w-rig-iau-rig (1)      .
           move      zero                 to   w-rig-cpv-aap (1)      .
           move      zero                 to   w-rig-ppv-aap (1, 1)   .
           move      zero                 to   w-rig-ppv-aap (1, 2)   .
           move      zero                 to   w-rig-ppv-aap (1, 3)   .
           move      zero                 to   w-rig-fsp-rig (1)      .
           move      zero                 to   w-rig-cpv-rig (1)      .
           move      zero                 to   w-rig-ppv-rig (1, 1)   .
           move      zero                 to   w-rig-ppv-rig (1, 2)   .
           move      zero                 to   w-rig-ppv-rig (1, 3)   .
           move      zero                 to   w-rig-pvf-rig (1)      .
           move      zero                 to   w-rig-ocl-dat (1)      .
           move      spaces               to   w-rig-ocl-num (1)      .
           move      spaces               to   w-rig-cmc-tip (1)      .
           move      zero                 to   w-rig-cmc-dat (1)      .
           move      zero                 to   w-rig-cmc-num (1)      .
           move      spaces               to   w-rig-coc-tip (1)      .
           move      zero                 to   w-rig-coc-dat (1)      .
           move      zero                 to   w-rig-coc-num (1)      .
           move      zero                 to   w-rig-coc-prt (1)      .
           move      zero                 to   w-rig-coc-prg (1)      .
           move      spaces               to   w-rig-coc-fzs (1)      .
           move      zero                 to   w-rig-ods-prt (1)      .
           move      zero                 to   w-rig-ods-prg (1)      .
           move      spaces               to   w-rig-fat-snx (1)      .
           move      zero                 to   w-rig-fat-dat (1)      .
           move      zero                 to   w-rig-fat-num (1)      .
           move      zero                 to   w-rig-fat-npb (1)      .
           move      spaces               to   w-rig-flg-ela (1)      .
           move      spaces               to   w-rig-flg-pul (1)      .
           move      spaces               to   w-rig-flg-puq (1)      .
           move      spaces               to   w-rig-alx-exp (1)      .
       nor-nok-rig-999.
           exit.

      *    *===========================================================*
      *    * Routine di decomposizione tipo riga                       *
      *    *                                                           *
      *    * Input  : rf-bfr-tip-rig                                   *
      *    *                                                           *
      *    * Output : w-rig-tip-rig (1) e relativi valori di w-rig con-*
      *    *          nessi                                            *
      *    *-----------------------------------------------------------*
       dec-tip-rig-000.
      *              *-------------------------------------------------*
      *              * Tipo riga in comodo di lavoro ridefinito        *
      *              *-------------------------------------------------*
           move      rf-bfr-tip-rig       to    w-dec-tip-rig-str     .
      *              *-------------------------------------------------*
      *              * Normalizzazione area di w-rig relativa al tipo  *
      *              * riga                                            *
      *              *-------------------------------------------------*
           move      spaces               to    w-rig-tip-rig (1)     .
           move      spaces               to    w-rig-tip-rig-tpr (1) .
           move      spaces               to    w-rig-tip-rig-tfu (1) .
           move      zero                 to    w-rig-tip-rig-cac (1) .
           move      spaces               to    w-rig-tip-rig-ast (1) .
      *              *-------------------------------------------------*
      *              * Presenza o assenza dell'asterisco               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ispezione stringa                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-dec-tip-rig-inx      .
           inspect   w-dec-tip-rig-str
                                      tallying w-dec-tip-rig-inx
                                          for  all "*"                .
      *                  *---------------------------------------------*
      *                  * Se nessun asterisco trovato : oltre         *
      *                  *---------------------------------------------*
           if        w-dec-tip-rig-inx    =    zero
                     go to dec-tip-rig-100.
      *                  *---------------------------------------------*
      *                  * Segnale di presenza asterisco               *
      *                  *---------------------------------------------*
           move      "#"                  to   w-rig-tip-rig-ast (1)  .
      *                  *---------------------------------------------*
      *                  * Pulizia stringa da asterisco                *
      *                  *---------------------------------------------*
           inspect   w-dec-tip-rig-str
                                     replacing all "*" by   spaces    .
       dec-tip-rig-100.
      *              *-------------------------------------------------*
      *              * Memorizzazione tipo riga                        *
      *              *-------------------------------------------------*
           move      w-dec-tip-rig-chr (1)
                                          to   w-rig-tip-rig-tpr (1)  .
      *              *-------------------------------------------------*
      *              * Test se esiste un tipo funzionamento            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo per tipi riga P o L                    *
      *                  *---------------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P" and
                     w-rig-tip-rig-tpr (1)
                                          not  = "L"
                     go to dec-tip-rig-200.
      *                  *---------------------------------------------*
      *                  * Se secondo carattere della stringa a spazi :*
      *                  * oltre                                       *
      *                  *---------------------------------------------*
           if        w-dec-tip-rig-chr (2)
                                          =    spaces
                     go to dec-tip-rig-200.
      *                  *---------------------------------------------*
      *                  * Memorizzazione tipo funzionamento           *
      *                  *---------------------------------------------*
           move      w-dec-tip-rig-chr (2)
                                          to   w-rig-tip-rig-tfu (1)  .
       dec-tip-rig-200.
      *              *-------------------------------------------------*
      *              * Ricerca di un codice addebito o commento        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Solo se tipo riga A o C                     *
      *                  *---------------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "A" and
                     w-rig-tip-rig-tpr (1)
                                          not  = "C"
                     go to dec-tip-rig-300.
      *                  *---------------------------------------------*
      *                  * Ciclo di scansione stringa a ritroso        *
      *                  *---------------------------------------------*
           move      zero                 to   w-dec-tip-rig-cod      .
           move      6                    to   w-dec-tip-rig-c01      .
           move      3                    to   w-dec-tip-rig-c02      .
       dec-tip-rig-210.
           subtract  1                    from w-dec-tip-rig-c01      .
           if        w-dec-tip-rig-chr
                    (w-dec-tip-rig-c01)   =    w-rig-tip-rig-tpr (1)
                     go to dec-tip-rig-220.
           if        w-dec-tip-rig-chr
                    (w-dec-tip-rig-c01)   =    spaces or
                     w-dec-tip-rig-chr
                    (w-dec-tip-rig-c01)   <    "0"    or
                     w-dec-tip-rig-chr
                    (w-dec-tip-rig-c01)   >    "9"
                     go to dec-tip-rig-210.
           move      w-dec-tip-rig-chr
                    (w-dec-tip-rig-c01)   to   w-dec-tip-rig-num
                                              (w-dec-tip-rig-c02)     .
           subtract  1                    from w-dec-tip-rig-c02      .
           go to     dec-tip-rig-210.
       dec-tip-rig-220.
           move      w-dec-tip-rig-cod    to   w-rig-tip-rig-cac (1)  .
       dec-tip-rig-300.
      *              *-------------------------------------------------*
      *              * Preparazione stringa tipo riga per accettazione *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizializzazione puntatore per string       *
      *                  *---------------------------------------------*
           move      01                   to   w-dec-tip-rig-pnt      .
      *                  *---------------------------------------------*
      *                  * Tipo riga                                   *
      *                  *---------------------------------------------*
           move      w-rig-tip-rig-tpr (1)
                                          to   w-rig-tip-rig (1)      .
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se a spazi : oltre                      *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tfu (1)
                                          =    spaces
                     go to dec-tip-rig-320.
      *                      *-----------------------------------------*
      *                      * String del tipo funzionamento           *
      *                      *-----------------------------------------*
           add       1                    to   w-dec-tip-rig-pnt      .
           string    w-rig-tip-rig-tfu (1)
                                delimited by   spaces
                                          into w-rig-tip-rig (1) 
                                  with pointer w-dec-tip-rig-pnt      .
       dec-tip-rig-320.
      *                  *---------------------------------------------*
      *                  * Asterisco                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non presente : oltre                 *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-ast (1)
                                          =    spaces
                     go to dec-tip-rig-400.
      *                      *-----------------------------------------*
      *                      * String asterisco                        *
      *                      *-----------------------------------------*
           add       1                    to   w-dec-tip-rig-pnt      .
           string    "*"        delimited by   size
                                          into w-rig-tip-rig (1) 
                                  with pointer w-dec-tip-rig-pnt      .
       dec-tip-rig-400.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     dec-tip-rig-999.
       dec-tip-rig-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [ybf]                         *
      *    *-----------------------------------------------------------*
       let-arc-ybf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ybf-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-ybf-cod    =    spaces
                     go to let-arc-ybf-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMB"             to   f-key                  .
           move      w-let-arc-ybf-cod    to   rf-ybf-cod-tmb         .
           move      w-let-arc-ybf-dpz    to   rf-ybf-cod-dpz         .
           move      "pgm/bfo/fls/ioc/obj/iofybf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybf                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-ybf-400.
       let-arc-ybf-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ybf-des-tmb       to   w-let-arc-ybf-des      .
           move      rf-ybf-int-ftr       to   w-let-arc-ybf-mif      .
           move      rf-ybf-tmo-ftr       to   w-let-arc-ybf-tmf      .
           move      rf-ybf-cau-mag       to   w-let-arc-ybf-cam      .
           move      rf-ybf-cod-mic       to   w-let-arc-ybf-ctm      .
           move      rf-ybf-def-tar       to   w-let-arc-ybf-dfa      .
           move      rf-ybf-snv-tar       to   w-let-arc-ybf-vaa      .
           move      rf-ybf-lst-tar       to   w-let-arc-ybf-lsa      .
           move      rf-ybf-org-doc       to   w-let-arc-ybf-ord      .
           move      rf-ybf-prv-doc       to   w-let-arc-ybf-prd      .
           move      rf-ybf-mov-afd       to   w-let-arc-ybf-maf      .
           move      rf-ybf-def-tmf       to   w-let-arc-ybf-dmf      .
           move      rf-ybf-vld-dpz       to   w-let-arc-ybf-tvd      .
           move      rf-ybf-cod-dsl       to   w-let-arc-ybf-dsl      .
           move      rf-ybf-def-tpr       to   w-let-arc-ybf-dtr      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-ybf-999.
       let-arc-ybf-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-ybf-flg      .
           move      all   "."            to   w-let-arc-ybf-des      .
           go to     let-arc-ybf-600.
       let-arc-ybf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ybf-des      .
       let-arc-ybf-600.
           move      zero                 to   w-let-arc-ybf-mif      .
           move      spaces               to   w-let-arc-ybf-tmf      .
           move      spaces               to   w-let-arc-ybf-acm      .
           move      spaces               to   w-let-arc-ybf-dtc      .
           move      spaces               to   w-let-arc-ybf-dct      .
           move      zero                 to   w-let-arc-ybf-cam      .
           move      spaces               to   w-let-arc-ybf-ctm      .
           move      spaces               to   w-let-arc-ybf-dfa      .
           move      spaces               to   w-let-arc-ybf-vaa      .
           move      spaces               to   w-let-arc-ybf-lsa      .
           move      zero                 to   w-let-arc-ybf-ord      .
           move      zero                 to   w-let-arc-ybf-prd      .
           move      spaces               to   w-let-arc-ybf-sgl      .
           move      zero                 to   w-let-arc-ybf-maf      .
           move      spaces               to   w-let-arc-ybf-dmf      .
           move      spaces               to   w-let-arc-ybf-tvd      .
           move      spaces               to   w-let-arc-ybf-dsl      .
           move      spaces               to   w-let-arc-ybf-dtr      .
       let-arc-ybf-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [fnt]                         *
      *    *-----------------------------------------------------------*
       let-arc-fnt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-fnt-cod    =    zero
                     go to let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT"             to   f-key                  .
           move      w-let-arc-fnt-cod    to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-fnt-400.
       let-arc-fnt-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-fnt-rag-soc       to   w-let-arc-fnt-rag      .
           move      rf-fnt-via-fnt       to   w-let-arc-fnt-via      .
           move      rf-fnt-loc-fnt       to   w-let-arc-fnt-loc      .
           move      rf-fnt-prt-iva       to   w-let-arc-fnt-piv      .
           move      rf-fnt-cod-cge       to   w-let-arc-fnt-stc      .
           move      rf-fnt-cod-iva       to   w-let-arc-fnt-ass      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-fnt-999.
       let-arc-fnt-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-fnt-flg      .
           move      all   "."            to   w-let-arc-fnt-rag      .
           go to     let-arc-fnt-520.
       let-arc-fnt-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-rag      .
       let-arc-fnt-520.
           move      spaces               to   w-let-arc-fnt-via      .
           move      spaces               to   w-let-arc-fnt-loc      .
           move      zero                 to   w-let-arc-fnt-piv      .
           move      zero                 to   w-let-arc-fnt-stc      .
           move      zero                 to   w-let-arc-fnt-ass      .
       let-arc-fnt-999.
           exit.

      *    *===========================================================*
      *    * Find su archivio [bft]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-bft-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-arc-bft-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pbfo3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-bft-sel
                     go to  fnd-arc-bft-999.
       fnd-arc-bft-050.
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'snx-slc' per il  *
      *              * livello successivo per l'ammissibilita' del ta- *
      *              * sto Slct                                        *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      01                   to   s-car                  .
           move      "S"                  to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       fnd-arc-bft-100.
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'tip-int' per il  *
      *              * livello successivo per il tipo di interrogazio- *
      *              * ne                                              *
      *              *-------------------------------------------------*
       fnd-arc-bft-150.
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per codice di- *
      *              * pendenza                                        *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-dpz"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      02                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-fnd-arc-bft-dpz    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       fnd-arc-bft-200.
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per codice     *
      *              * fornitore                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-fnd-arc-bft-fnt    =    zero
                     go to fnd-arc-bft-250.
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "cod-fnt"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      07                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      w-fnd-arc-bft-fnt    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       fnd-arc-bft-250.
      *              *-------------------------------------------------*
      *              * Preparazione variabile di i.p.c. per dipendenza *
      *              * fornitore                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-fnd-arc-bft-fnt    =    zero
                     go to fnd-arc-bft-300.
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "dpz-fnt"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      04                   to   s-car                  .
           move      w-fnd-arc-bft-dpf    to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       fnd-arc-bft-300.
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/bfo/prg/obj/pbfo3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
      *              *-------------------------------------------------*
      *              * Estrazione di eventuali variabili di i.p.c. de- *
      *              * terminate da function-key "SLCT" durante l'in-  *
      *              * terrogazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-tmb"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-bft-sel
                     go to fnd-arc-bft-999.
           move      s-alf                to   w-fnd-arc-bft-tmb      .
      *                  *---------------------------------------------*
      *                  * Data registrazione                          *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dat-reg"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-bft-sel
                     go to fnd-arc-bft-999.
           move      s-dat                to   w-fnd-arc-bft-dat      .
      *                  *---------------------------------------------*
      *                  * Numero protocollo                           *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-prt"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-bft-sel
                     go to fnd-arc-bft-999.
           move      s-num                to   w-fnd-arc-bft-prt      .
      *              *-------------------------------------------------*
      *              * Salvataggio valori chiave                       *
      *              *-------------------------------------------------*
           move      w-acc                to   w-sav-val-acc          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori estratti                 *
      *              *-------------------------------------------------*
           move      w-fnd-arc-bft-prt    to   w-acc-num-prt          .
           move      w-fnd-arc-bft-dat    to   w-acc-dat-doc          .
       fnd-arc-bft-400.
      *              *-------------------------------------------------*
      *              * Determinazione campi derivati e effettuazione   *
      *              * controlli come se fossero stati impostati       *
      *              *-------------------------------------------------*
       fnd-arc-bft-600.
      *              *-------------------------------------------------*
      *              * Completamento visualizzazione campi di accet-   *
      *              * tazione                                         *
      *              *-------------------------------------------------*
           perform   vis-num-prt-000      thru vis-num-prt-999        .
      *              *-------------------------------------------------*
      *              * Uscita con successo                             *
      *              *-------------------------------------------------*
           go to     fnd-arc-bft-999.
       fnd-arc-bft-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripristino valori chiave salvati            *
      *                  *---------------------------------------------*
           move      w-sav-val-acc        to   w-acc                  .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-fnd-arc-bft-sel      .
       fnd-arc-bft-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zci]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.lts"                   .

      *    *===========================================================*
      *    * Routine lettura tabella [zvl]                             *
      *    *-----------------------------------------------------------*
       let-arc-zvl-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvl-flg      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del valore della sigla va- *
      *              * luta                                            *
      *              *-------------------------------------------------*
           if        w-let-arc-zvl-cod    =    c-sgl
                     go to let-arc-zvl-200
           else if   w-let-arc-zvl-cod    =    spaces
                     go to let-arc-zvl-400
           else      go to let-arc-zvl-600.
       let-arc-zvl-200.
      *              *-------------------------------------------------*
      *              * Se sigla valuta pari alla valuta base           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione valuta in lingua italiana       *
      *                  *---------------------------------------------*
           move      c-des                to   w-let-arc-zvl-des      .
      *                  *---------------------------------------------*
      *                  * Descrizione valuta internazionale           *
      *                  *---------------------------------------------*
           move      c-din                to   w-let-arc-zvl-din      .
      *                  *---------------------------------------------*
      *                  * Numero decimali valuta                      *
      *                  *---------------------------------------------*
           move      c-dec                to   w-let-arc-zvl-dec      .
      *                  *---------------------------------------------*
      *                  * Tipo di coefficiente per cambio valuta      *
      *                  *---------------------------------------------*
           move      c-tdc                to   w-let-arc-zvl-tdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-400.
      *              *-------------------------------------------------*
      *              * Se sigla valuta a Spaces                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione valuta in lingua italiana       *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-des      .
      *                  *---------------------------------------------*
      *                  * Descrizione valuta internazionale           *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-din      .
      *                  *---------------------------------------------*
      *                  * Numero decimali valuta                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-let-arc-zvl-dec      .
      *                  *---------------------------------------------*
      *                  * Tipo di coefficiente per cambio valuta      *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-zvl-tdc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-600.
      *              *-------------------------------------------------*
      *              * Se sigla valuta diversa dalla valuta base ed    *
      *              * anche diversa da Spaces                         *
      *              *-------------------------------------------------*
       let-arc-zvl-650.
      *                  *---------------------------------------------*
      *                  * Lettura per sigla valuta                    *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVLT    "         to   f-key                  .
           move      w-let-arc-zvl-cod    to   rf-zvl-sgl-vlt         .
           move      "pgm/dcc/fls/ioc/obj/iofzvl"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvl                 .
      *                  *---------------------------------------------*
      *                  * Deviazione secondo l'esito della lettura    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-zvl-750.
       let-arc-zvl-700.
      *                  *---------------------------------------------*
      *                  * Se record esistente                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione valuta in lingua italiana   *
      *                      *-----------------------------------------*
           move      rf-zvl-des-ita       to   w-let-arc-zvl-des      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta internazionale       *
      *                      *-----------------------------------------*
           move      rf-zvl-des-int       to   w-let-arc-zvl-din      .
      *                      *-----------------------------------------*
      *                      * Numero decimali valuta                  *
      *                      *-----------------------------------------*
           move      rf-zvl-num-dec       to   w-let-arc-zvl-dec      .
      *                      *-----------------------------------------*
      *                      * Tipo di coefficiente per cambio valuta  *
      *                      *-----------------------------------------*
           move      rf-zvl-def-tdc       to   w-let-arc-zvl-tdc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-750.
      *                  *---------------------------------------------*
      *                  * Se record non esistente                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di uscita ad errore                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-let-arc-zvl-flg      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta in lingua italiana   *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-zvl-des      .
      *                      *-----------------------------------------*
      *                      * Descrizione valuta internazionale       *
      *                      *-----------------------------------------*
           move      all   "."            to   w-let-arc-zvl-din      .
      *                      *-----------------------------------------*
      *                      * Numero decimali valuta                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-let-arc-zvl-dec      .
      *                      *-----------------------------------------*
      *                      * Tipo di coefficiente per cambio valuta  *
      *                      *-----------------------------------------*
           move      spaces               to   w-let-arc-zvl-tdc      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-zvl-999.
       let-arc-zvl-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [pdc]                         *
      *    *-----------------------------------------------------------*
       let-arc-pdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sottoconto a zero                *
      *              *-------------------------------------------------*
           if        w-let-arc-pdc-cod    =    zero
                     go to let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPDC"             to   f-key                  .
           move      w-let-arc-pdc-cod    to   rf-pdc-cod-pdc         .
           move      "pgm/cge/fls/ioc/obj/iofpdc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-pdc-400.
       let-arc-pdc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-pdc-des-pdc       to   w-let-arc-pdc-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-pdc-999.
       let-arc-pdc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-pdc-flg      .
           move      all   "."            to   w-let-arc-pdc-des      .
           go to     let-arc-pdc-999.
       let-arc-pdc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-pdc-des      .
       let-arc-pdc-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cbp]                         *
      *    *-----------------------------------------------------------*
       let-arc-cbp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cbp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spaces                         *
      *              *-------------------------------------------------*
           if        w-let-arc-cbp-cod    =    spaces
                     go to let-arc-cbp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCBP    "         to   f-key                  .
           move      w-let-arc-cbp-tip    to   rf-cbp-tip-cbp         .
           move      w-let-arc-cbp-cod    to   rf-cbp-cod-cbp         .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cbp-400.
       let-arc-cbp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cbp-des-cbp       to   w-let-arc-cbp-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cbp-999.
       let-arc-cbp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cbp-flg      .
           move      all   "."            to   w-let-arc-cbp-des      .
           go to     let-arc-cbp-999.
       let-arc-cbp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cbp-des      .
       let-arc-cbp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [axi]                         *
      *    *-----------------------------------------------------------*
       let-arc-axi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axi-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice istituto a zero                  *
      *              *-------------------------------------------------*
           if        w-let-arc-axi-cod    =    zero
                     go to let-arc-axi-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODABI"             to   f-key                  .
           move      w-let-arc-axi-cod    to   rf-axi-cod-abi         .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-axi-400.
       let-arc-axi-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-axi-den-abi       to   w-let-arc-axi-den      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-axi-999.
       let-arc-axi-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-axi-flg      .
           move      all   "."            to   w-let-arc-axi-den      .
           go to     let-arc-axi-999.
       let-arc-axi-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axi-den      .
       let-arc-axi-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [axs]                         *
      *    *-----------------------------------------------------------*
       let-arc-axs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axs-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice banca a zero                     *
      *              *-------------------------------------------------*
           if        w-let-arc-axs-abi    =    zero
                     go to let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Test se codice agenzia a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-axs-cab    =    zero
                     go to let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ABICAB    "         to   f-key                  .
           move      w-let-arc-axs-abi    to   rf-axs-cod-abi         .
           move      w-let-arc-axs-cab    to   rf-axs-cod-cab         .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-axs-400.
       let-arc-axs-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-axs-den-spt       to   w-let-arc-axs-den      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-axs-999.
       let-arc-axs-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-axs-flg      .
           move      all   "."            to   w-let-arc-axs-den      .
           go to     let-arc-axs-999.
       let-arc-axs-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-axs-den      .
       let-arc-axs-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dcp]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcp-cod    =    zero  
                     go to let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      w-let-arc-dcp-cod    to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-400.
       let-arc-dcp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcp-des-pro       to   w-let-arc-dcp-dui      .
           move      rf-dcp-tip-pro       to   w-let-arc-dcp-tpr      .
           move      rf-dcp-cod-iva       to   w-let-arc-dcp-civ      .
           move      rf-dcp-umi-ven       to   w-let-arc-dcp-umi      .
           move      rf-dcp-dec-qta       to   w-let-arc-dcp-deq      .
       let-arc-dcp-210.
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione della descrizione       *
      *                      *-----------------------------------------*
           move      rf-dcp-des-pro       to   w-let-arc-dcp-des      .
      *                      *-----------------------------------------*
      *                      * Test se codice lingua diverso da "I  "  *
      *                      *-----------------------------------------*
           if        w-let-arc-dcp-lng    not  = "I  "
                     go to let-arc-dcp-350.
       let-arc-dcp-215.
      *                      *-----------------------------------------*
      *                      * Descrizione contenuta nel record        *
      *                      *-----------------------------------------*
           if        rf-dcp-des-pdx       =    0
                     go to let-arc-dcp-300.
      *                      *-----------------------------------------*
      *                      * Descrizione in file di estensione       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione descrizione         *
      *                          *-------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
      *                          *-------------------------------------*
      *                          * Normalizzazione contatore           *
      *                          *-------------------------------------*
           move      zero                 to   w-let-arc-dcp-ctr      .
       let-arc-dcp-220.
           add       1                    to   w-let-arc-dcp-ctr      .
           if        w-let-arc-dcp-ctr    >    10
                     go to let-arc-dcp-300.
      *                          *-------------------------------------*
      *                          * Lettura archivio [pdx]              *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TRCLNG"             to   f-key                  .
           move      01                   to   rf-pdx-tip-rec         .
           move      zero                 to   rf-pdx-cod-arc         .
           move      "I  "                to   rf-pdx-cod-lng         .
           move      w-let-arc-dcp-cod    to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      w-let-arc-dcp-ctr    to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                              *---------------------------------*
      *                              * Se record [pdx] non trovato :   *
      *                              * ad uscita                       *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-300.
      *                          *-------------------------------------*
      *                          * Bufferizzazione riga letta          *
      *                          *-------------------------------------*
           move      rf-pdx-des-pro       to   w-let-arc-dcp-drg
                                              (w-let-arc-dcp-ctr)     .
      *                          *-------------------------------------*
      *                          * Riciclo                             *
      *                          *-------------------------------------*
           go to     let-arc-dcp-220.
       let-arc-dcp-300.
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dcp-999.
       let-arc-dcp-350.
      *                  *---------------------------------------------*
      *                  * Se lingua diversa da "I  "                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione in lingua                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione descrizione         *
      *                          *-------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
      *                          *-------------------------------------*
      *                          * Normalizzazione contatore           *
      *                          *-------------------------------------*
           move      zero                 to   w-let-arc-dcp-ctr      .
       let-arc-dcp-370.
           add       1                    to   w-let-arc-dcp-ctr      .
           if        w-let-arc-dcp-ctr    >    10
                     go to let-arc-dcp-380.
      *                          *-------------------------------------*
      *                          * Lettura archivio [pdx]              *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TRCLNG"             to   f-key                  .
           move      02                   to   rf-pdx-tip-rec         .
           move      zero                 to   rf-pdx-cod-arc         .
           move      w-let-arc-dcp-lng    to   rf-pdx-cod-lng         .
           move      w-let-arc-dcp-cod    to   rf-pdx-cod-num         .
           move      spaces               to   rf-pdx-for-mat         .
           move      w-let-arc-dcp-ctr    to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
      *                              *---------------------------------*
      *                              * Se record [pdx] non trovato :   *
      *                              * ad uscita                       *
      *                              *---------------------------------*
           if        f-sts                not  = e-not-err
                     go to let-arc-dcp-380.
      *                          *-------------------------------------*
      *                          * Bufferizzazione riga letta          *
      *                          *-------------------------------------*
           move      rf-pdx-des-pro       to   w-let-arc-dcp-drg
                                              (w-let-arc-dcp-ctr)     .
      *                          *-------------------------------------*
      *                          * Riciclo                             *
      *                          *-------------------------------------*
           go to     let-arc-dcp-370.
       let-arc-dcp-380.
      *                      *-----------------------------------------*
      *                      * Test in uscita                          *
      *                      *-----------------------------------------*
           if        w-let-arc-dcp-des    =    spaces
                     move  rf-dcp-des-pro to   w-let-arc-dcp-des      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     let-arc-dcp-999.
       let-arc-dcp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcp-flg      .
           move      spaces               to   w-let-arc-dcp-des      .
           move      all   "."            to   w-let-arc-dcp-drg (1)  .
           move      all   "."            to   w-let-arc-dcp-dui      .
           go to     let-arc-dcp-520.
       let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
           move      spaces               to   w-let-arc-dcp-dui      .
       let-arc-dcp-520.
           move      zero                 to   w-let-arc-dcp-tpr      .
           move      zero                 to   w-let-arc-dcp-civ      .
           move      spaces               to   w-let-arc-dcp-umi      .
           move      zero                 to   w-let-arc-dcp-deq      .
       let-arc-dcp-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [aaf]                         *
      *    *-----------------------------------------------------------*
       let-arc-aaf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-aaf-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione record [aaf]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
      *              *-------------------------------------------------*
      *              * Lettura record [aaf]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "PRFNFM"             to   f-key                  .
           move      w-let-arc-aaf-tpm    to   rf-aaf-tip-mag         .
           move      w-let-arc-aaf-cdm    to   rf-aaf-num-pro         .
           move      w-let-arc-aaf-fnt    to   rf-aaf-cod-dcf         .
           move      w-let-arc-aaf-fda    to   rf-aaf-fda-pif         .
           move      "pgm/dcf/fls/ioc/obj/iofaaf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaf                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-aaf-400.
       let-arc-aaf-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-aaf-cop-sfn       to   w-let-arc-aaf-csf      .
           move      rf-aaf-snx-tum       to   w-let-arc-aaf-snu      .
           move      rf-aaf-umf-tum       to   w-let-arc-aaf-umf      .
           move      rf-aaf-nde-tum       to   w-let-arc-aaf-ndu      .
           move      rf-aaf-cmo-tum       to   w-let-arc-aaf-cmu      .
           move      rf-aaf-cdi-tum       to   w-let-arc-aaf-cdu      .
           move      rf-aaf-tip-pza       to   w-let-arc-aaf-tpa      .
           move      rf-aaf-sgl-vlt       to   w-let-arc-aaf-vlt      .
           move      rf-aaf-dec-vlt       to   w-let-arc-aaf-dcv      .
           move      rf-aaf-dec-prz       to   w-let-arc-aaf-ndp      .
           move      rf-aaf-lot-acq       to   w-let-arc-aaf-lda      .
           move      rf-aaf-tap-pes       to   w-let-arc-aaf-tap      .
           move      rf-aaf-lgv-vlt       to   w-let-arc-aaf-lvv      .
           move      rf-aaf-lgv-dcv       to   w-let-arc-aaf-lvd      .
           move      rf-aaf-lgv-tdc       to   w-let-arc-aaf-lvt      .
           move      rf-aaf-lgv-cdc       to   w-let-arc-aaf-lvc      .
           move      rf-aaf-lgv-pdt       to   w-let-arc-aaf-lvp      .
           move      rf-aaf-per-mpa       to   w-let-arc-aaf-mpa      .
           move      zero                 to   w-let-arc-aaf-ctr      .
       let-arc-aaf-202.
           add       1                    to   w-let-arc-aaf-ctr      .
           if        w-let-arc-aaf-ctr    >    6
                     go to let-arc-aaf-204.
           move      rf-aaf-qta-pes
                    (w-let-arc-aaf-ctr)   to   w-let-arc-aaf-qtp
                                              (w-let-arc-aaf-ctr)     .
           move      rf-aaf-prz-pes
                    (w-let-arc-aaf-ctr)   to   w-let-arc-aaf-pzp
                                              (w-let-arc-aaf-ctr)     .
           move      rf-aaf-csr-pes
                    (w-let-arc-aaf-ctr)   to   w-let-arc-aaf-csp
                                              (w-let-arc-aaf-ctr)     .
           move      rf-aaf-psr-pes
                    (w-let-arc-aaf-ctr, 1)
                                          to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 1)  .
           move      rf-aaf-psr-pes
                    (w-let-arc-aaf-ctr, 2)
                                          to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 2)  .
           move      rf-aaf-psr-pes
                    (w-let-arc-aaf-ctr, 3)
                                          to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 3)  .
           move      rf-aaf-psr-pes
                    (w-let-arc-aaf-ctr, 4)
                                          to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 4)  .
           move      rf-aaf-psr-pes
                    (w-let-arc-aaf-ctr, 5)
                                          to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 5)  .
           go to     let-arc-aaf-202.
       let-arc-aaf-204.
      *                  *---------------------------------------------*
      *                  * Trattamento descrizione                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione contenuta nel record        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su flag di estensione          *
      *                          *-------------------------------------*
           if        rf-aaf-xdp-sfn       not  = 0
                     go to let-arc-aaf-210.
      *                          *-------------------------------------*
      *                          * Test se a spaces                    *
      *                          *-------------------------------------*
           if        rf-aaf-dep-sfn       not  = spaces
                     go to let-arc-aaf-208.
           if        w-let-arc-aaf-dmg    =    spaces
                     go to let-arc-aaf-208.
      *                          *-------------------------------------*
      *                          * Se la descrizione e' a spaces si    *
      *                          * utilizza quella letta dalla scheda  *
      *                          * anagrafica del codice magazzino     *
      *                          *-------------------------------------*
           move      w-let-arc-aaf-dmg    to   w-let-arc-aaf-des      .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     let-arc-aaf-300.
       let-arc-aaf-208.
      *                          *-------------------------------------*
      *                          * Descrizione per il fornitore        *
      *                          *-------------------------------------*
           move      rf-aaf-dep-sfn       to   w-let-arc-aaf-des      .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     let-arc-aaf-300.
       let-arc-aaf-210.
      *                      *-----------------------------------------*
      *                      * Descrizione in file di estensione       *
      *                      *-----------------------------------------*
           move      zero                 to   w-let-arc-aaf-ctr      .
       let-arc-aaf-220.
           add       1                    to   w-let-arc-aaf-ctr      .
           if        w-let-arc-aaf-ctr    >    10
                     go to let-arc-aaf-300.
      *                          *-------------------------------------*
      *                          * Lettura archivio [pdx]              *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "TRCLNG"             to   f-key                  .
      *
           if        w-let-arc-aaf-tpm    =    01
                     move  13             to   rf-pdx-tip-rec
           else if   w-let-arc-aaf-tpm    =    03
                     move  33             to   rf-pdx-tip-rec
           else if   w-let-arc-aaf-tpm    =    04
                     move  43             to   rf-pdx-tip-rec         .
      *
           move      w-let-arc-aaf-fnt    to   rf-pdx-cod-arc         .
           move      spaces               to   rf-pdx-cod-lng         .
           move      w-let-arc-aaf-cdm    to   rf-pdx-cod-num         .
           move      w-let-arc-aaf-fda    to   rf-pdx-for-mat         .
           move      w-let-arc-aaf-ctr    to   rf-pdx-num-prg         .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-aaf-300.
           move      rf-pdx-des-pro       to   w-let-arc-aaf-drg
                                              (w-let-arc-aaf-ctr)     .
           go to     let-arc-aaf-220.
       let-arc-aaf-300.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-aaf-999.
       let-arc-aaf-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-aaf-flg      .
           move      spaces               to   w-let-arc-aaf-des      .
           move      all   "."            to   w-let-arc-aaf-drg (1)  .
           go to     let-arc-aaf-520.
       let-arc-aaf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-aaf-des      .
       let-arc-aaf-520.
           move      spaces               to   w-let-arc-aaf-dmg      .
           move      spaces               to   w-let-arc-aaf-csf      .
           move      spaces               to   w-let-arc-aaf-snu      .
           move      spaces               to   w-let-arc-aaf-umf      .
           move      zero                 to   w-let-arc-aaf-ndu      .
           move      zero                 to   w-let-arc-aaf-cmu      .
           move      zero                 to   w-let-arc-aaf-cdu      .
           move      zero                 to   w-let-arc-aaf-tpa      .
           move      spaces               to   w-let-arc-aaf-vlt      .
           move      zero                 to   w-let-arc-aaf-dcv      .
           move      zero                 to   w-let-arc-aaf-ndp      .
           move      zero                 to   w-let-arc-aaf-lda      .
           move      zero                 to   w-let-arc-aaf-tap      .
           move      spaces               to   w-let-arc-aaf-lvv      .
           move      zero                 to   w-let-arc-aaf-lvd      .
           move      zero                 to   w-let-arc-aaf-lvt      .
           move      zero                 to   w-let-arc-aaf-lvc      .
           move      zero                 to   w-let-arc-aaf-lvp      .
           move      zero                 to   w-let-arc-aaf-mpa      .
      *
           move      zero                 to   w-let-arc-aaf-ctr      .
       let-arc-aaf-522.
           add       1                    to   w-let-arc-aaf-ctr      .
           if        w-let-arc-aaf-ctr    >    6
                     go to let-arc-aaf-524.
           move      zero                 to   w-let-arc-aaf-qtp
                                              (w-let-arc-aaf-ctr)     .
           move      zero                 to   w-let-arc-aaf-pzp
                                              (w-let-arc-aaf-ctr)     .
           move      zero                 to   w-let-arc-aaf-csp
                                              (w-let-arc-aaf-ctr)     .
           move      zero                 to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 1)  .
           move      zero                 to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 2)  .
           move      zero                 to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 3)  .
           move      zero                 to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 4)  .
           move      zero                 to   w-let-arc-aaf-psp
                                              (w-let-arc-aaf-ctr, 5)  .
           go to     let-arc-aaf-522.
       let-arc-aaf-524.
       let-arc-aaf-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [bfx]                         *
      *    *-----------------------------------------------------------*
       let-arc-bfx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-bfx-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici nulli                            *
      *              *-------------------------------------------------*
           if        w-let-arc-bfx-prt    =    zero   or
                     w-let-arc-bfx-prg    =    zero   or
                     w-let-arc-bfx-trc    =    zero
                     go to let-arc-bfx-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-let-arc-bfx-prt    to   rf-bfx-num-prt         .
           move      w-let-arc-bfx-prg    to   rf-bfx-num-prg         .
           move      w-let-arc-bfx-trc    to   rf-bfx-tip-rec         .
           move      "pgm/bfo/fls/ioc/obj/iofbfx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfx                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-bfx-400.
       let-arc-bfx-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-bfx-des-400       to   w-let-arc-bfx-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-bfx-999.
       let-arc-bfx-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-bfx-flg      .
           move      spaces               to   w-let-arc-bfx-des      .
           move      all   "."            to   w-let-arc-bfx-drg (1)  .
           go to     let-arc-bfx-999.
       let-arc-bfx-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-bfx-des      .
       let-arc-bfx-999.
           exit.

      *    *===========================================================*
      *    * Determinazione quantita' secondaria per l'acquisto        *
      *    *-----------------------------------------------------------*
           copy      "pgm/ffo/prg/cpy/wdetqts0.wks"                   .

      *    *===========================================================*
      *    * Determinazione prezzo netto                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cps"                   .

      *    *===========================================================*
      *    * Editing quantita' da incolonnare                          *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wks"                   .

      *    *===========================================================*
      *    * Determinazione prezzo sottoposto a legame valutario       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wlvlprz0.cps"                   .

      *    *===========================================================*
      *    * Determinazione importo in riga                            *
      *    *-----------------------------------------------------------*
       det-imp-rig-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per la determinazione    *
      *              *-------------------------------------------------*
           move      "IR"                 to   d-imp-ven-tip-ope      .
           move      w-rig-qta-ven (1)    to   d-imp-ven-qta-ven      .
           move      w-rig-snx-2qt (1)    to   d-imp-ven-snx-2qt      .
           move      w-rig-qta-a02 (1)    to   d-imp-ven-qta-a02      .
           move      w-rig-snx-3qt (1)    to   d-imp-ven-snx-3qt      .
           move      w-rig-qta-a03 (1)    to   d-imp-ven-qta-a03      .
           move      w-rig-prz-net (1)    to   d-imp-ven-prz-uni      .
           move      w-rig-prz-ven (1)    to   d-imp-ven-prz-unl      .
           move      w-rig-per-scr (1, 1) to   d-imp-ven-per-scr (1)  .
           move      w-rig-per-scr (1, 2) to   d-imp-ven-per-scr (2)  .
           move      w-rig-per-scr (1, 3) to   d-imp-ven-per-scr (3)  .
           move      w-rig-per-scr (1, 4) to   d-imp-ven-per-scr (4)  .
           move      w-rig-per-scr (1, 5) to   d-imp-ven-per-scr (5)  .
           move      w-rig-dec-prz (1)    to   d-imp-ven-dec-prz      .
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           perform   det-imp-ven-cll-000  thru det-imp-ven-cll-999    .
      *              *-------------------------------------------------*
      *              * In campo di uscita                              *
      *              *-------------------------------------------------*
           move      d-imp-ven-imp-rig    to   w-rig-imp-rig (1)      .
       det-imp-rig-600.
      *              *-------------------------------------------------*
      *              * Eventuale conversione nella valuta di fattura-  *
      *              * zione                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se necessario                          *
      *                  *---------------------------------------------*
           if        w-rig-sgl-vpp (1)    =    w-tes-sgl-vpf (1)
                     go to det-imp-rig-999.
      *                  *---------------------------------------------*
      *                  * Conversione importo da valuta per il prezzo *
      *                  * a valuta base                               *
      *                  *---------------------------------------------*
           move      w-rig-sgl-vpp (1)    to   w-cvs-vlt-sgl          .
           move      w-rig-dec-vpp (1)    to   w-cvs-vlt-dec          .
           move      w-rig-tdc-vpp (1)    to   w-cvs-vlt-tdc          .
           move      w-rig-cdc-vpp (1)    to   w-cvs-vlt-cdc          .
           move      w-rig-imp-rig (1)    to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                  *---------------------------------------------*
      *                  * Conversione importo da valuta base a valuta *
      *                  * per fatturazione                            *
      *                  *---------------------------------------------*
           move      w-tes-sgl-vpf (1)    to   w-cvs-vlt-sgl          .
           move      w-tes-dec-vpf (1)    to   w-cvs-vlt-dec          .
           move      w-tes-tdc-vpf (1)    to   w-cvs-vlt-tdc          .
           move      w-tes-cdc-vpf (1)    to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione del risultato               *
      *                  *---------------------------------------------*
           move      w-cvs-vlt-aav        to   w-rig-imp-rig (1)      .
       det-imp-rig-999.
           exit.

      *    *===========================================================*
      *    * Routine di conversione da altra valuta a valuta base      *
      *    *                                                           *
      *    * Input  : w-cvs-vlt-sgl = Sigla dell'altra valuta          *
      *    *                                                           *
      *    *          w-cvs-vlt-dec = Numero decimali dell'altra valu- *
      *    *                          ta                               *
      *    *                                                           *
      *    *          w-cvs-vlt-tdc = Tipo di coefficiente dell'altra  *
      *    *                          valuta                           *
      *    *                                                           *
      *    *          w-cvs-vlt-cdc = Coefficiente di cambio dell'al-  *
      *    *                          tra valuta                       *
      *    *                                                           *
      *    *          w-cvs-vlt-aav = Ammontare da convertire, espres- *
      *    *                          so nell'altra valuta             *
      *    *                                                           *
      *    * Output : w-cvs-vlt-avb = Ammontare convertito, espresso   *
      *    *                          nella valuta base                *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    * Routine di conversione da valuta base ad altra valuta     *
      *    *                                                           *
      *    * Input  : w-cvs-vlt-sgl = Sigla dell'altra valuta          *
      *    *                                                           *
      *    *          w-cvs-vlt-dec = Numero decimali dell'altra valu- *
      *    *                          ta                               *
      *    *                                                           *
      *    *          w-cvs-vlt-tdc = Tipo di coefficiente dell'altra  *
      *    *                          valuta                           *
      *    *                                                           *
      *    *          w-cvs-vlt-cdc = Coefficiente di cambio dell'al-  *
      *    *                          tra valuta                       *
      *    *                                                           *
      *    *          w-cvs-vlt-avb = Ammontare da convertire, espres- *
      *    *                          so nella valuta base             *
      *    *                                                           *
      *    * Output : w-cvs-vlt-aav = Ammontare convertito, espresso   *
      *    *                          nell'altra valuta                *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    * Routine di conversione da valuta a valuta                 *
      *    *                                                           *
      *    * Input  : Valuta da trasformare                            *
      *    *                                                           *
      *    *            - w-cvs-vdt-sgl = Sigla valuta                 *
      *    *                                                           *
      *    *            - w-cvs-vdt-dec = Numero decimali              *
      *    *                                                           *
      *    *            - w-cvs-vdt-tdc = Tipo di coefficiente         *
      *    *                                                           *
      *    *            - w-cvs-vdt-cdc = Coefficiente di cambio ri-   *
      *    *                              spetto alla valuta base      *
      *    *                                                           *
      *    *            - w-cvs-vdt-amm = Ammontare da convertire,     *
      *    *                              espresso nella valuta da     *
      *    *                              trasformare                  *
      *    *                                                           *
      *    *          Valuta in cui trasformare                        *
      *    *                                                           *
      *    *            - w-cvs-vnu-sgl = Sigla valuta                 *
      *    *                                                           *
      *    *            - w-cvs-vnu-dec = Numero decimali              *
      *    *                                                           *
      *    *            - w-cvs-vnu-tdc = Tipo di coefficiente         *
      *    *                                                           *
      *    *            - w-cvs-vnu-cdc = Coefficiente di cambio ri-   *
      *    *                              spetto alla valuta base      *
      *    *                                                           *
      *    * Output : Valuta in cui trasformare                        *
      *    *                                                           *
      *    *            - w-cvs-vnu-amm = Ammontare convertito,        *
      *    *                              espresso nella valuta        *
      *    *                              in cui trasformare           *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .

      *    *===========================================================*
      *    * Box per messaggio di errore                               *
      *    *-----------------------------------------------------------*
       box-msg-err-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      16                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      17                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-err-999.
           exit.

      *    *===========================================================*
      *    * Box per messaggio di errore esteso, su due righe          *
      *    *-----------------------------------------------------------*
       box-msg-e02-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in Off                                    *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Box                                             *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      16                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      19                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Linea 01                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-msg    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 02                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m02    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      18                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "[ ]"                to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      18                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-e02-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione tipo movimento per ordini  *
      *    * fornitori                                                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/acdeyof0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per accettazione codice prodotto 'dcp'        *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del coefficiente di cambio *
      *    * valuta                                                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione importo in riga            *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dimpven0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

