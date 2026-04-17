       Identification Division.
       Program-Id.                                 pfat300a           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    fat                 *
      *                                Settore:    mov                 *
      *                                   Fase:    fat300a             *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/07/92    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Fatturazione differita manuale bolle        *
      *                                                                *
      *                    Richiamata in fat300 da funzione 'PF4'      *
      *                                                                *
      *================================================================*
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *        Input  : l-fat-dfm-tip-ope = "OP"                       *
      *                 l-fat-dfm-cod-dpz = codice dipendenza in uso   *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *        Input  : l-fat-dfm-tip-ope = "CL"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *        Input  : l-fat-dfm-tip-ope = "C?"                       *
      *                                                                *
      *        Output : l-fat-dfm-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * "DI" - Dichiarazione di inizio fatturazione differita          *
      *                                                                *
      *        Input  : l-fat-dfm-tip-ope = "DI"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "BC" - Accettazione dati identificativi bolla cliente          *
      *                                                                *
      *        Input  : l-fat-dfm-tip-ope = "BC"                       *
      *                 l-fat-dfm-cod-tmb = codice tipo movimento di   *
      *                                     bollettazione da proporre  *
      *                                     come default               *
      *                                                                *
      *        Output : l-fat-dfm-exi-sts = spaces: Accettazione ese-  *
      *                                             guita con successo *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "T1" - Bufferizzazione testata prima bolla cliente richiamato  *
      *                                                                *
      *        Input  : l-fat-dfm-tip-ope = "T1"                       *
      *                                                                *
      *        Output : l-fat-dfm-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita con errore  *
      *                                                                *
      * "T+" - Confronto dati testata bolla cliente in esame con dati  *
      *        di testata del documento                                *
      *                                                                *
      *        Input  : l-fat-dfm-tip-ope = "T+"                       *
      *                                                                *
      *        Output : l-fat-dfm-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita con errore  *
      *                                                                *
      * "SC" - Saldaconto per inclusione bolla cliente                 *
      *                                                                *
      *        Input  : l-fat-dfm-tip-ope = "SC"                       *
      *                                                                *
      *        Output : l-fat-dfm-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "CC" - Caricamento righe bolla selezionate in catena           *
      *                                                                *
      *        Input  : l-fat-dfm-tip-ope = "CC"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "WR" - Aggiornamento records bolla per quanto riguarda i dati  *
      *        relativi alla fattura differita durante la fase di      *
      *        Inserimento di un nuovo record [fir]                    *
      *                                                                *
      *        Input  : l-fat-dfm-tip-ope = "WR"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "RW" - Aggiornamento records bolla per quanto riguarda i dati  *
      *        relativi alla fattura differita durante la fase di      *
      *        Modifica di un nuovo record [fir]                       *
      *                                                                *
      *        Input  : l-fat-dfm-tip-ope = "RW"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "DE" - Aggiornamento records bolla per quanto riguarda i dati  *
      *        relativi alla fattura differita durante la fase di      *
      *        Cancellazione di un nuovo record [fir]                  *
      *                                                                *
      *        Input  : l-fat-dfm-tip-ope = "DE"                       *
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
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *                                                       *
      *        * N.B.: reiterato da 'fat300' solo per note cliente     *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pfat3000"                                       .

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
      *    * Record files area 'bol'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [bit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbit"                          .
      *        *-------------------------------------------------------*
      *        * [bir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbir"                          .
      *        *-------------------------------------------------------*
      *        * [bix]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbix"                          .
      *        *-------------------------------------------------------*
      *        * [zbi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfzbi"                          .

      *    *===========================================================*
      *    * Record files area 'fat'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [zac]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzac"                          .

      *    *===========================================================*
      *    * Record files area 'dcc'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [zvl]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvl"                          .
      *        *-------------------------------------------------------*
      *        * [zbo]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzbo"                          .
      *        *-------------------------------------------------------*
      *        * [zcs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzcs"                          .
      *        *-------------------------------------------------------*
      *        * [zfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzfp"                          .
      *        *-------------------------------------------------------*
      *        * [zin]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzin"                          .
      *        *-------------------------------------------------------*
      *        * [zvf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvf"                          .
      *        *-------------------------------------------------------*
      *        * [zsf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzsf"                          .

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
      *        *-------------------------------------------------------*
      *        * [zls]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfzls"                          .

      *    *===========================================================*
      *    * Record files area 'cge'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [lic]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rflic"                          .
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
      *    * Work-area generica per il programma                       *
      *    *-----------------------------------------------------------*
       01  w-gen.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-gen-ctr-opn              pic s9(05) value zero       .
      *        *-------------------------------------------------------*
      *        * Numero protocollo bolla cliente in corso di tratta-   *
      *        * mento                                                 *
      *        *-------------------------------------------------------*
           05  w-gen-prt-bol              pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Flag di fatturazione separata                         *
      *        *-------------------------------------------------------*
           05  w-gen-fat-sep              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo conferma d'ordine a cliente         *
      *        *-------------------------------------------------------*
           05  w-gen-coc-prt              pic  9(11)                  .

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
      *        *-------------------------------------------------------*
      *        * Tipo movimento bolle clienti                          *
      *        *-------------------------------------------------------*
           05  w-acc-cod-tmb              pic  x(05)                  .
           05  w-acc-cod-tmb-des          pic  x(30)                  .
           05  w-acc-cod-tmb-mif          pic  9(02)                  .
           05  w-acc-cod-tmb-tmf          pic  x(05)                  .
           05  w-acc-cod-tmb-tm2          pic  x(05)                  .
           05  w-acc-cod-tmb-acm          pic  x(01)                  .
           05  w-acc-cod-tmb-dtc          pic  x(01)                  .
           05  w-acc-cod-tmb-dct          pic  x(03)                  .
           05  w-acc-cod-tmb-cam          pic  9(05)                  .
           05  w-acc-cod-tmb-ctm          pic  x(03)                  .
           05  w-acc-cod-tmb-dfa          pic  x(01)                  .
           05  w-acc-cod-tmb-vaa          pic  x(01)                  .
           05  w-acc-cod-tmb-lsa          pic  x(04)                  .
           05  w-acc-cod-tmb-ord          pic  9(02)                  .
           05  w-acc-cod-tmb-prd          pic  9(02)                  .
           05  w-acc-cod-tmb-sgl          pic  x(03)                  .
           05  w-acc-cod-tmb-maf          pic  9(02)                  .
           05  w-acc-cod-tmb-dmf          pic  x(05)                  .
           05  w-acc-cod-tmb-tvd          pic  x(01)                  .
           05  w-acc-cod-tmb-ddd          pic  x(07)                  .
           05  w-acc-cod-tmb-dsl          pic  x(07)                  .
      *        *-------------------------------------------------------*
      *        * Data documento                                        *
      *        *-------------------------------------------------------*
           05  w-acc-dat-doc              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero documento                                      *
      *        *-------------------------------------------------------*
           05  w-acc-num-doc              pic  9(11)                  .
           05  w-acc-num-doc-r redefines
               w-acc-num-doc.
               10  w-acc-num-doc-saa      pic  9(03)                  .
               10  w-acc-num-doc-dpz      pic  9(02)                  .
               10  w-acc-num-doc-prg      pic  9(06)                  .

      *    *===========================================================*
      *    * Work per subroutines di Select                            *
      *    *-----------------------------------------------------------*
       01  w-slc.
      *        *-------------------------------------------------------*
      *        * Work per Select numero documento                      *
      *        *-------------------------------------------------------*
           05  w-slc-num-bit.
      *            *---------------------------------------------------*
      *            * Valori in entrata                                 *
      *            *---------------------------------------------------*
               10  w-slc-num-bit-dds      pic  9(07)                  .
               10  w-slc-num-bit-nds      pic  9(11)                  .
               10  w-slc-num-bit-nds-r redefines
                   w-slc-num-bit-nds.
                   15  w-slc-num-bit-nsa  pic  9(03)                  .
                   15  w-slc-num-bit-ndp  pic  9(02)                  .
                   15  w-slc-num-bit-npg  pic  9(06)                  .
               10  w-slc-num-bit-dpz      pic  9(02)                  .
               10  w-slc-num-bit-sgl      pic  x(03)                  .
               10  w-slc-num-bit-saa      pic  9(03)                  .
               10  w-slc-num-bit-snf      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valori in uscita                                  *
      *            *---------------------------------------------------*
               10  w-slc-num-bit-sel      pic  x(01)                  .
               10  w-slc-num-bit-toc      pic  x(05)                  .
               10  w-slc-num-bit-dat      pic  9(07)                  .
               10  w-slc-num-bit-num      pic  9(11)                  .
               10  w-slc-num-bit-prt      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo                                    *
      *            *---------------------------------------------------*
               10  w-slc-num-bit-c01      pic  9(02)                  .
               10  w-slc-num-bit-c02      pic  9(02)                  .
               10  w-slc-num-bit-c03      pic  9(02)                  .
               10  w-slc-num-bit-c04      pic  9(02)                  .
               10  w-slc-num-bit-c05      pic  9(02)                  .
               10  w-slc-num-bit-nli      pic  9(02)                  .
               10  w-slc-num-bit-crb      pic  9(02)                  .
               10  w-slc-num-bit-cpb      pic  9(02)                  .
               10  w-slc-num-bit-cpa      pic  9(02)                  .
               10  w-slc-num-bit-buf
                               occurs 30.
                   15  w-slc-num-bit-bpt  pic  9(11)                  .
               10  w-slc-num-bit-ltp.
                   15  filler             pic  x(07) value "Pagina "  .
                   15  w-slc-num-bit-lt1  pic  9(01)                  .
                   15  filler             pic  x(04) value " di "     .
                   15  w-slc-num-bit-lt2  pic  9(01)                  .

      *    *===========================================================*
      *    * Work-area per routine acc-cod-tmb-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-act.
           05  w-acc-cod-tmb-inx          pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-val-acc.
               10  filler   occurs 200    pic  x(01)                  .
           05  w-sav-qta-fat              pic s9(10)v9(03)            .
           05  w-sav-flg-fzs              pic  x(01)                  .
           05  w-sav-snx-aoc              pic  x(01)                  .
           05  w-sav-prz-ven              pic  9(09)                  .
           05  w-sav-qta-rpr              pic s9(10)v9(03)            .

      *    *===========================================================*
      *    * Work-area per bufferizzazione protocolli bolle clienti    *
      *    *-----------------------------------------------------------*
       01  w-pbc.
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella                            *
      *        *-------------------------------------------------------*
           05  w-pbc-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi in tabella                    *
      *        *-------------------------------------------------------*
           05  w-pbc-max-ele              pic  9(05) value 99         .
      *        *-------------------------------------------------------*
      *        * Tabella elementi                                      *
      *        *-------------------------------------------------------*
           05  w-pbc-tbl.
               10  w-pbc-sng-ele occurs 99.
                   15  w-pbc-num-prt      pic  9(11)                  .
               10  w-pbc-ctr-001          pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione righe bolle cliente         *
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
                   15  w-brb-qta-bol      pic s9(10)v9(03) comp-3     .
                   15  w-brb-qta-fat      pic s9(10)v9(03) comp-3     .
                   15  w-brb-flg-idr      pic  x(01)                  .
                   15  w-brb-flg-ids      pic  x(01)                  .
                   15  w-brb-flg-fzs      pic  x(01)                  .
                   15  w-brb-qta-res      pic s9(10)v9(03) comp-3     .
                   15  w-brb-snx-aoc      pic  x(01)                  .

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
           05  w-sdc-ctr-rig              pic  9(03)                  .
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
           05  w-qss-qta-bol              pic  9(06)v9(03)            .
           05  w-qss-qta-fat              pic  9(06)v9(03)            .

      *    *===========================================================*
      *    * Work-area per caricamento righe ordine in catena          *
      *    *-----------------------------------------------------------*
       01  w-crc.
      *        *-------------------------------------------------------*
      *        * Contatore di lavoro                                   *
      *        *-------------------------------------------------------*
           05  w-crc-wrk-ctr              pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per routine buf-tes-bol-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-buf-tes-bol.
      *        *-------------------------------------------------------*
      *        * Contatore di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-buf-tes-bol-ctr          pic  9(05)                  .

      *    *===========================================================*
      *    * Work-area per routine ags-ass-iva-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-ags-ass-iva.
      *        *-------------------------------------------------------*
      *        * Assoggettamento iva originale                 [Input] *
      *        *-------------------------------------------------------*
           05  w-ags-ass-iva-aii          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Codice cliente                                [Input] *
      *        *-------------------------------------------------------*
           05  w-ags-ass-iva-cli          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data documento                                [Input] *
      *        *-------------------------------------------------------*
           05  w-ags-ass-iva-dat          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Flag di uscita                               [Output] *
      *        *-------------------------------------------------------*
           05  w-ags-ass-iva-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Assoggettamento iva determinato              [Output] *
      *        *-------------------------------------------------------*
           05  w-ags-ass-iva-aio          pic  9(05)                  .

      *    *===========================================================*
      *    * Work per subroutines di Vis                               *
      *    *-----------------------------------------------------------*
       01  w-vis.
      *        *-------------------------------------------------------*
      *        * Work per Vis : Numero protocollo                      *
      *        *-------------------------------------------------------*
           05  w-vis-num-prt.
               10  w-vis-num-prt-num      pic  9(11)                  .
               10  w-vis-num-prt-num-r    redefines
                   w-vis-num-prt-num.
                   15  w-vis-num-prt-saa  pic  9(03)                  .
                   15  w-vis-num-prt-dpz  pic  9(02)                  .
                   15  w-vis-num-prt-prg  pic  9(06)                  .
               10  w-vis-num-prt-nvi      pic  9(09)                  .
               10  w-vis-num-prt-nvi-r    redefines
                   w-vis-num-prt-nvi.
                   15  w-vis-num-prt-nvs  pic  9(03)                  .
                   15  w-vis-num-prt-nvp  pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Work per Vis : Data documento                         *
      *        *-------------------------------------------------------*
           05  w-vis-dat-doc.
               10  w-vis-dat-doc-edt      pic  x(08)                  .

      *    *===========================================================*
      *    * Work-area per routine buf-rig-bol-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-buf-rig-bol.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *-------------------------------------------------------*
           05  w-buf-rig-bol-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo bolla cliente                       *
      *        *-------------------------------------------------------*
           05  w-buf-rig-bol-prt          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo prima conferma d'ordine cliente in- *
      *        * contrata nella scansione                              *
      *        *-------------------------------------------------------*
           05  w-buf-rig-bol-pro          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per ridefinizione tipo riga                    *
      *        *-------------------------------------------------------*
           05  w-buf-rig-bol-wtr.
               10  w-buf-rig-bol-wtp      pic  x(01)                  .
               10  w-buf-rig-bol-wtf      pic  x(01)                  .
               10  filler                 pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione in riga                                   *
      *        *-------------------------------------------------------*
           05  w-buf-rig-bol-wde          pic  x(40)                  .

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
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [bit]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-bit.
               10  w-fnd-arc-bit-sel      pic  x(01)                  .
               10  w-fnd-arc-bit-tmb      pic  x(05)                  .
               10  w-fnd-arc-bit-dat      pic  9(07)                  .
               10  w-fnd-arc-bit-num      pic  9(11)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zbi]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zbi.
               10  w-let-arc-zbi-flg      pic  x(01)                  .
               10  w-let-arc-zbi-cod      pic  x(05)                  .
               10  w-let-arc-zbi-dpz      pic  9(02)                  .
               10  w-let-arc-zbi-des      pic  x(30)                  .
               10  w-let-arc-zbi-mif      pic  9(02)                  .
               10  w-let-arc-zbi-tmf      pic  x(05)                  .
               10  w-let-arc-zbi-acm      pic  x(01)                  .
               10  w-let-arc-zbi-dtc      pic  x(01)                  .
               10  w-let-arc-zbi-dct      pic  x(03)                  .
               10  w-let-arc-zbi-cam      pic  9(05)                  .
               10  w-let-arc-zbi-ctm      pic  x(03)                  .
               10  w-let-arc-zbi-dfa      pic  x(01)                  .
               10  w-let-arc-zbi-vaa      pic  x(01)                  .
               10  w-let-arc-zbi-lsa      pic  x(04)                  .
               10  w-let-arc-zbi-ord      pic  9(02)                  .
               10  w-let-arc-zbi-prd      pic  9(02)                  .
               10  w-let-arc-zbi-sgl      pic  x(03)                  .
               10  w-let-arc-zbi-maf      pic  9(02)                  .
               10  w-let-arc-zbi-dmf      pic  x(05)                  .
               10  w-let-arc-zbi-tvd      pic  x(01)                  .
               10  w-let-arc-zbi-dsl      pic  x(07)                  .
               10  w-let-arc-zbi-tm2      pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
               10  w-let-arc-cli-via      pic  x(40)                  .
               10  w-let-arc-cli-loc      pic  x(40)                  .
               10  w-let-arc-cli-piv      pic  9(11)                  .
               10  w-let-arc-cli-stc      pic  9(07)                  .
               10  w-let-arc-cli-ass      pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcc-flg      pic  x(01)                  .
               10  w-let-arc-dcc-cli      pic  9(07)                  .
               10  w-let-arc-dcc-dpz      pic  x(04)                  .
               10  w-let-arc-dcc-rag      pic  x(40)                  .
               10  w-let-arc-dcc-via      pic  x(40)                  .
               10  w-let-arc-dcc-loc      pic  x(40)                  .
               10  w-let-arc-dcc-vlt      pic  x(03)                  .
               10  w-let-arc-dcc-lng      pic  x(03)                  .
               10  w-let-arc-dcc-tai      pic  9(02)                  .
               10  w-let-arc-dcc-blo      pic  9(02)                  .
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
      *        * Work per Let su archivio [zvf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zvf.
               10  w-let-arc-zvf-flg      pic  x(01)                  .
               10  w-let-arc-zvf-num      pic  9(03)                  .
               10  w-let-arc-zvf-cod      pic  x(03)                  .
               10  w-let-arc-zvf-des      pic  x(25)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zcs]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zcs.
               10  w-let-arc-zcs-flg      pic  x(01)                  .
               10  w-let-arc-zcs-tip      pic  9(02)                  .
               10  w-let-arc-zcs-cod      pic  9(05)                  .
               10  w-let-arc-zcs-des      pic  x(20)                  .
               10  w-let-arc-zcs-per occurs 5
                                          pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [age]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-age.
               10  w-let-arc-age-flg      pic  x(01)                  .
               10  w-let-arc-age-cod      pic  9(07)                  .
               10  w-let-arc-age-rag      pic  x(40)                  .
               10  w-let-arc-age-spa      pic  9(07)                  .
               10  w-let-arc-age-cpv      pic  9(05)                  .
               10  w-let-arc-age-ppv occurs 03
                                          pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zfp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zfp.
               10  w-let-arc-zfp-flg      pic  x(01)                  .
               10  w-let-arc-zfp-cod      pic  9(07)                  .
               10  w-let-arc-zfp-des      pic  x(40)                  .
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
      *        * Work per Let su archivio [zin]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zin.
               10  w-let-arc-zin-flg      pic  x(01)                  .
               10  w-let-arc-zin-cod      pic  x(03)                  .
               10  w-let-arc-zin-tpg      pic  9(02)                  .
               10  w-let-arc-zin-des      pic  x(40)                  .
               10  w-let-arc-zin-coi      pic  9(05)                  .
               10  w-let-arc-zin-ccp      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zbo]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zbo.
               10  w-let-arc-zbo-flg      pic  x(01)                  .
               10  w-let-arc-zbo-cod      pic  x(03)                  .
               10  w-let-arc-zbo-tpg      pic  9(02)                  .
               10  w-let-arc-zbo-des      pic  x(40)                  .
               10  w-let-arc-zbo-coi      pic  9(05)                  .
               10  w-let-arc-zbo-ccp      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [bix]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-bix.
               10  w-let-arc-bix-flg      pic  x(01)                  .
               10  w-let-arc-bix-prt      pic  9(11)                  .
               10  w-let-arc-bix-prg      pic  9(05)                  .
               10  w-let-arc-bix-trc      pic  9(02)                  .
               10  w-let-arc-bix-des.
                   15  w-let-arc-bix-drg occurs 10
                                          pic  x(40)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [zac]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/larczac0.ltw"                   .

      *    *===========================================================*
      *    * Work per Let su archivio [zls]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/larczls0.ltw"                   .

      *    *===========================================================*
      *    * Work per Let su archivio [zci]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.ltw"                   .

      *    *===========================================================*
      *    * Work per Let su archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.ltw"                   .

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
      *        *-------------------------------------------------------*
      *        * Work per Det valori testata dipendenti da [dcc]       *
      *        *-------------------------------------------------------*
           05  w-det-vlt-dcc.
      *            *---------------------------------------------------*
      *            * Tipo determinazione                               *
      *            * - C : Per cliente commerciale                     *
      *            * - F : Per cliente per fatturazione                *
      *            *---------------------------------------------------*
               10  w-det-vlt-dcc-tdt      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Work locale                                       *
      *            *---------------------------------------------------*
               10  w-det-vlt-dcc-ctr      pic  9(02)                  .
               10  w-det-vlt-dcc-inx      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det funzionamento spese in fattura           *
      *        *-------------------------------------------------------*
           05  w-det-fun-spe.
      *            *---------------------------------------------------*
      *            * Tipo determinazione                               *
      *            * - C : Per cliente commerciale                     *
      *            * - F : Per cliente per fatturazione                *
      *            *---------------------------------------------------*
               10  w-det-fun-spe-tdt      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Work locale                                       *
      *            *---------------------------------------------------*
               10  w-det-fun-spe-ctr      pic  9(02)                  .
               10  w-det-fun-spe-inx      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det valori testata dipendenti da [dcc] della *
      *        * dipendenza                                            *
      *        *-------------------------------------------------------*
           05  w-det-vlt-dcd.
      *            *---------------------------------------------------*
      *            * Tipo determinazione                               *
      *            * - C : Per cliente commerciale                     *
      *            * - F : Per cliente per fatturazione                *
      *            *---------------------------------------------------*
               10  w-det-vlt-dcd-tdt      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza                                 *
      *            *---------------------------------------------------*
               10  w-det-vlt-dcd-dpz      pic  x(04)                  .

      *    *===========================================================*
      *    * Work per determinazione quantita' secondaria              *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/wdetqts0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing quantita' da incolonnare  *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di Agg                               *
      *    *-----------------------------------------------------------*
       01  w-agg.
      *        *-------------------------------------------------------*
      *        * Dati identificativi bolla                             *
      *        *-------------------------------------------------------*
           05  w-agg-bcc-tip              pic  x(05)                  .
           05  w-agg-bcc-dat              pic  9(07)                  .
           05  w-agg-bcc-num              pic  9(11)                  .
           05  w-agg-bcc-num-r redefines
               w-agg-bcc-num.
               10  w-agg-bcc-num-saa      pic  9(03)                  .
               10  w-agg-bcc-num-dpz      pic  9(02)                  .
               10  w-agg-bcc-num-prg      pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Progressivo bolla in fattura                          *
      *        *-------------------------------------------------------*
           05  w-agg-npr-bif              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per calcolo prezzo netto                        *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per prezzo sottoposto a legame valutario        *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wlvlprz0.cpw"                   .

      *    *===========================================================*
      *    * Work-area per conversioni rispetto alla valuta base       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cpw"                   .

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
      *    * Link-area per accettazione codice cliente commerciale     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice tipo movimento bolla    *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acdezbi0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione coefficiente cambio valuta     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione importo in riga  *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dimpven0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione provvigioni      *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/dpvgage0.dtl"                   .

      *    *===========================================================*
      *    * Work per buffer documenti movimentati                     *
      *    *-----------------------------------------------------------*
       01  w-buf-doc.
      *        *-------------------------------------------------------*
      *        * Work per buffer                                       *
      *        *-------------------------------------------------------*
           05  w-buf-doc-mvm.
      *            *---------------------------------------------------*
      *            * Data di richiesta                                 *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-drc      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice utente                                     *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-ute      pic  x(08)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza                                 *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-dpz      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Flag di selezione                                 *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-fds      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo documento selezionato                        *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-tds      pic  x(05)                  .
      *            *---------------------------------------------------*
      *            * Numero documento selezionato                      *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-nds      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Data documento selezionato                        *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-dds      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo                                    *
      *            *---------------------------------------------------*
               10  w-buf-doc-mvm-c01      pic  9(02)                  .
               10  w-buf-doc-mvm-c02      pic  9(02)                  .
               10  w-buf-doc-mvm-c03      pic  9(02)                  .
               10  w-buf-doc-mvm-c04      pic  9(02)                  .
               10  w-buf-doc-mvm-c05      pic  9(02)                  .
               10  w-buf-doc-mvm-nli      pic  9(02)                  .
               10  w-buf-doc-mvm-crb      pic  9(02)                  .
               10  w-buf-doc-mvm-cpb      pic  9(02)                  .
               10  w-buf-doc-mvm-cpa      pic  9(02)                  .
               10  w-buf-doc-mvm-max      pic  9(02) value 54         .
               10  w-buf-doc-mvm-buf occurs 54.
                   15  w-buf-doc-mvm-tmo  pic  x(05)                  .
                   15  w-buf-doc-mvm-num  pic  9(11)                  .
                   15  w-buf-doc-mvm-dat  pic  9(07)                  .
                   15  w-buf-doc-mvm-rsa  pic  x(40)                  .
               10  w-buf-doc-mvm-ltp.
                   15  filler             pic  x(07) value "Pagina "  .
                   15  w-buf-doc-mvm-lt1  pic  9(01)                  .
                   15  filler             pic  x(04) value " di "     .
                   15  w-buf-doc-mvm-lt2  pic  9(01)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di comunicazione per fatturazione differita manuale  *
      *    *-----------------------------------------------------------*
       01  l-fat-dfm.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  l-fat-dfm-tip-ope          pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        * - Spaces : operazione eseguita                        *
      *        * - #      : errore di esecuzione                       *
      *        *-------------------------------------------------------*
           05  l-fat-dfm-exi-sts          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  l-fat-dfm-cod-dpz          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Default per codice tipo movimento bolle clienti       *
      *        *-------------------------------------------------------*
           05  l-fat-dfm-cod-tmb          pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Flag di trattamento bolla cliente                     *
      *        *-------------------------------------------------------*
           05  l-fat-dfm-flg-bol          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flag di function-key Tab in corso                     *
      *        *-------------------------------------------------------*
           05  l-fat-dfm-fky-tab          pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area comune per programmi della serie pfat3000       *
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
           copy      "pgm/fat/prg/cpy/pfat3000.pgl"                   .

      ******************************************************************
       Procedure Division                using l-fat-dfm
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
           move      spaces               to   l-fat-dfm-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Dichiarazione di inizio ciclo di fattura-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        l-fat-dfm-tip-ope    =    "DI"
                     perform dic-ini-cic-000
                                          thru dic-ini-cic-999
      *                  *---------------------------------------------*
      *                  * Accettazione dati identificativi bolla      *
      *                  * cliente                                     *
      *                  *---------------------------------------------*
           else if   l-fat-dfm-tip-ope    =    "BC"
                     perform acc-dti-bol-000
                                          thru acc-dti-bol-999
      *                  *---------------------------------------------*
      *                  * Bufferizzazione testata primo bolla clien-  *
      *                  * te richiamata                               *
      *                  *---------------------------------------------*
           else if   l-fat-dfm-tip-ope    =    "T1"
                     perform buf-tes-bol-000
                                          thru buf-tes-bol-999
      *                  *---------------------------------------------*
      *                  * Confronto testata bolla cliente richiamata  *
      *                  * con i dati di testata del documento         *
      *                  *---------------------------------------------*
           else if   l-fat-dfm-tip-ope    =    "T+"
                     perform cnf-tes-doc-000
                                          thru cnf-tes-doc-999
      *                  *---------------------------------------------*
      *                  * Saldaconto per fatturazione differita ma-   *
      *                  * nuale righe bolla                           *
      *                  *---------------------------------------------*
           else if   l-fat-dfm-tip-ope    =    "SC"
                     perform sdc-fat-dfm-000
                                          thru sdc-fat-dfm-999
      *                  *---------------------------------------------*
      *                  * Caricamento righe bolla in catena           *
      *                  *---------------------------------------------*
           else if   l-fat-dfm-tip-ope    =    "CC"
                     perform car-rig-cat-000
                                          thru car-rig-cat-999
      *                  *---------------------------------------------*
      *                  * Aggiornamento records bolla per quanto ri-  *
      *                  * guarda i dati relativi alla fattura diffe-  *
      *                  * rita in fase di Inserimento di un nuovo     *
      *                  * record [fir]                                *
      *                  *---------------------------------------------*
           else if   l-fat-dfm-tip-ope    =    "WR"
                     perform wrt-rec-bol-000
                                          thru wrt-rec-bol-999
      *                  *---------------------------------------------*
      *                  * Aggiornamento records bolla per quanto ri-  *
      *                  * guarda i dati relativi alla fattura diffe-  *
      *                  * rita in fase di Modifica di un nuovo record *
      *                  * [fir]                                       *
      *                  *---------------------------------------------*
           else if   l-fat-dfm-tip-ope    =    "RW"
                     perform rew-rec-bol-000
                                          thru rew-rec-bol-999
      *                  *---------------------------------------------*
      *                  * Aggiornamento records bolla per quanto ri-  *
      *                  * guarda i dati relativi alla fattura diffe-  *
      *                  * rita in fase di Cancellazione di un nuovo   *
      *                  * record [fir]                                *
      *                  *---------------------------------------------*
           else if   l-fat-dfm-tip-ope    =    "DE"
                     perform del-rec-bol-000
                                          thru del-rec-bol-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   l-fat-dfm-tip-ope    =    "OP"
                     perform exe-fun-opn-000
                                          thru exe-fun-opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   l-fat-dfm-tip-ope    =    "CL"
                     perform exe-fun-cls-000
                                          thru exe-fun-cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   l-fat-dfm-tip-ope    =    "C?"
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
           move      l-fat-dfm-cod-dpz    to   w-acc-cod-dpz          .
      *              *-------------------------------------------------*
      *              * Normalizzazione data per accettazione           *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-dat-doc          .
      *              *-------------------------------------------------*
      *              * Normalizzazione tipo movimento per bollettazione*
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-cod-tmb          .
       exe-fun-opn-100.
      *              *-------------------------------------------------*
      *              * Apertura files                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * [bit]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
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
      *                  * [bix]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bix                 .
      *                  *---------------------------------------------*
      *                  * [zbi]                                       *
      *                  *---------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
      *                  *---------------------------------------------*
      *                  * [lic]                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su personalizzazione su gestione   *
      *                      * lettere d'intento clienti attiva        *
      *                      *-----------------------------------------*
           if        w-prs-snx-lic        not  = "S"
                     go to exe-fun-opn-120.
      *                      *-----------------------------------------*
      *                      * Open file                               *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioflic"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
       exe-fun-opn-120.
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento per la  *
      *              * bolla                                           *
      *              *-------------------------------------------------*
           perform   cod-des-zbi-opn-000  thru cod-des-zbi-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione coefficiente di cambio *
      *              * valuta                                          *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-opn-000  thru coe-cmb-vlt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice cliente commer- *
      *              * ciale                                           *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-opn-000  thru cod-mne-dcc-opn-999    .
       exe-fun-opn-999.
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
      *                  * [bit]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
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
      *                  * [bix]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bix                 .
      *                  *---------------------------------------------*
      *                  * [zbi]                                       *
      *                  *---------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
      *                  *---------------------------------------------*
      *                  * [lic]                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su personalizzazione su gestione   *
      *                      * lettere d'intento clienti attiva        *
      *                      *-----------------------------------------*
           if        w-prs-snx-lic        not  = "S"
                     go to exe-fun-cls-120.
      *                      *-----------------------------------------*
      *                      * Open file                               *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioflic"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
       exe-fun-cls-120.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione tipo movimento per la *
      *              * bollettazione                                   *
      *              *-------------------------------------------------*
           perform   cod-des-zbi-cls-000  thru cod-des-zbi-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione coefficiente di cam-  *
      *              * bio valuta                                      *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-cls-000  thru coe-cmb-vlt-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente com-   *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-cls-000  thru cod-mne-dcc-cls-999    .
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
                     move  spaces         to   l-fat-dfm-exi-sts
           else      move  "#"            to   l-fat-dfm-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di inizio ciclo di evasione                 *
      *    *-----------------------------------------------------------*
       dic-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Azzeramento contatore numero elementi in ta-    *
      *              * bella protocolli ordini clienti                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-pbc-num-ele          .
      *              *-------------------------------------------------*
      *              * Inizializzazione work per aggiornamenti         *
      *              *-------------------------------------------------*
           move      spaces               to   w-agg-bcc-tip          .
           move      zero                 to   w-agg-bcc-dat          .
           move      zero                 to   w-agg-bcc-num          .
           move      zero                 to   w-agg-npr-bif          .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di fatturazione separata   *
      *              *-------------------------------------------------*
           move      spaces               to   w-gen-fat-sep          .
      *              *-------------------------------------------------*
      *              * Normalizzazione protocollo conferma d'ordine    *
      *              * cliente in corso di fatturazione                *
      *              *-------------------------------------------------*
           move      zero                 to   w-gen-coc-prt          .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di Function-Key Tab in     *
      *              * corso                                           *
      *              *-------------------------------------------------*
           move      spaces               to   l-fat-dfm-fky-tab      .
       dic-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Accettazione dati identificativi bolla cliente            *
      *    *-----------------------------------------------------------*
       acc-dti-bol-000.
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
           move      22                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      29                   to   v-pos                  .
           move      "RICHIAMO BOLLA CLIENTE"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Tipo movimento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "Tipo:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      48                   to   v-pos                  .
           move      "Data:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      65                   to   v-pos                  .
           move      "Numero:"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Se flag di fatturazione separata a "S", vuol    *
      *              * dire che e' gia' stata richiamata una bolla che *
      *              * indicava una fatturazione separata, per cui non *
      *              * possono essere richiamate altre bolle           *
      *              *-------------------------------------------------*
           if        w-gen-fat-sep        not  = "S"
                     go to acc-dti-bol-020.
      *                  *---------------------------------------------*
      *                  * Messaggio                                   *
      *                  *---------------------------------------------*
           move      "La bolla appena richiamata prevede la fatturazione
      -              " separata, per "    to   w-err-box-err-msg      .
           move      "cui non possono essere richiamate altre bolle !   
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                  *---------------------------------------------*
      *                  * A uscita per Exit                           *
      *                  *---------------------------------------------*
           go to     acc-dti-bol-800.
       acc-dti-bol-020.
      *              *-------------------------------------------------*
      *              * Preparazione valori di default per tipo movi-   *
      *              * mento ordini clienti                            *
      *              *-------------------------------------------------*
           move      l-fat-dfm-cod-tmb    to   w-acc-cod-tmb          .
           if        w-acc-cod-tmb        =    spaces
                     go to acc-dti-bol-100.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zbi] generale             *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmb        to   w-let-arc-zbi-cod      .
           move      zero                 to   w-let-arc-zbi-dpz      .
           perform   let-arc-zbi-000      thru let-arc-zbi-999        .
           move      w-let-arc-zbi-des    to   w-acc-cod-tmb-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice                      *
      *                  *---------------------------------------------*
           perform   vis-cod-tmb-000      thru vis-cod-tmb-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-tmb-des-000  thru vis-cod-tmb-des-999    .
      *                  *---------------------------------------------*
      *                  * Ad accettazione data documento              *
      *                  *---------------------------------------------*
           go to     acc-dti-bol-200.
       acc-dti-bol-100.
      *              *-------------------------------------------------*
      *              * Accettazione dati identificativi bolla          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Tipo movimento per bollettazione            *
      *                  *---------------------------------------------*
           perform   acc-cod-tmb-000      thru acc-cod-tmb-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-bol-800.
           if        v-key                =    "DO  "
                     go to acc-dti-bol-400.
       acc-dti-bol-200.
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           perform   acc-dat-doc-000      thru acc-dat-doc-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-bol-800.
           if        v-key                =    "DO  "
                     go to acc-dti-bol-400.
           if        v-key                =    "UP  "
                     go to acc-dti-bol-100.
       acc-dti-bol-300.
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           perform   acc-num-doc-000      thru acc-num-doc-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-bol-800.
           if        v-key                =    "DO  "
                     go to acc-dti-bol-400.
           if        v-key                =    "UP  "
                     go to acc-dti-bol-200.
       acc-dti-bol-400.
      *              *-------------------------------------------------*
      *              * Controllo che esistano tutti i valori di iden-  *
      *              * tificazione della bolla                         *
      *              *-------------------------------------------------*
           if        w-acc-cod-tmb        =    spaces or
                     w-acc-dat-doc        =    zero   or
                     w-acc-num-doc        =    zero
                     go to acc-dti-bol-100.
      *              *-------------------------------------------------*
      *              * Completamento numero documento                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Secolo/Anno                                 *
      *                  *---------------------------------------------*
           move      w-acc-dat-doc        to   s-dat                  .
           move      s-saa                to   w-acc-num-doc-saa      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza                           *
      *                  *---------------------------------------------*
           move      w-acc-cod-dpz        to   w-acc-num-doc-dpz      .
      *              *-------------------------------------------------*
      *              * Determinazione se documento esistente           *
      *              *-------------------------------------------------*
           perform   det-doc-ges-000      thru det-doc-ges-999        .
      *                  *---------------------------------------------*
      *                  * Se documento non esistente                  *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-snx    not  = "N"
                     go to acc-dti-bol-420.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Movimento non esistente in archivio !             
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-bol-100.
       acc-dti-bol-420.
      *                  *---------------------------------------------*
      *                  * Se documento esistente ma con tipo movimen- *
      *                  * to diverso                                  *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-snx    not  = "X"
                     go to acc-dti-bol-440.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "Movimento esistente, ma di tipo '"
                                delimited by   size
                     w-det-doc-ges-tmt
                                delimited by   spaces
                     "' !"
                                delimited by   size
                                          into w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-bol-100.
       acc-dti-bol-440.
      *              *-------------------------------------------------*
      *              * Test se bolla gia' fatturata                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        rf-bit-fat-dat       =    zero and
                     rf-bit-fat-num       =    zero and
                     rf-bit-fat-npb       =    zero
                     go to acc-dti-bol-450.
      *                  *---------------------------------------------*
      *                  * Editing data fattura                        *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      rf-bit-fat-dat       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      v-edt                to   w-vis-dat-doc-edt      .
      *                  *---------------------------------------------*
      *                  * Editing protocollo fattura                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      rf-bit-fat-num       to   w-vis-num-prt-num      .
           move      w-vis-num-prt-prg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      65                   to   w-all-str-lun          .
           move      07                   to   w-all-str-num          .
           move      "Bolla gia' fatturata ! ("
                                          to   w-all-str-cat (1)      .
           move      rf-bit-tmo-ftr       to   w-all-str-cat (2)      .
      *
           if        rf-bit-fat-num       =    zero
                     move  spaces         to   w-all-str-cat (3)
                     move  spaces         to   w-all-str-cat (4)
           else      move  "nr."          to   w-all-str-cat (3)
                     move  v-edt          to   w-all-str-cat (4)      .
      *
           if        rf-bit-fat-dat       =    zero
                     move  spaces         to   w-all-str-cat (5)
                     move  spaces         to   w-all-str-cat (6)
           else      move  "del"          to   w-all-str-cat (5)
                     move  w-vis-dat-doc-edt
                                          to   w-all-str-cat (6)      .
      *
           move      ")"                  to   w-all-str-cat (7)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           move      w-all-str-alf        to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dti-bol-100.
       acc-dti-bol-450.
      *              *-------------------------------------------------*
      *              * Test su data bolla                              *
      *              *-------------------------------------------------*
           if        rf-bit-dat-doc       not  > w-tes-dat-doc
                     go to acc-dti-bol-460.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Data bolla superiore alla data della fattura !    
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dti-bol-100.
       acc-dti-bol-460.
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero protocollo               *
      *              *-------------------------------------------------*
           move      rf-bit-num-prt       to   w-acc-num-prt          .
       acc-dti-bol-500.
      *              *-------------------------------------------------*
      *              * Test se la bolla e' gia' stata richiamata       *
      *              *-------------------------------------------------*
           move      zero                 to   w-pbc-ctr-001          .
       acc-dti-bol-520.
           add       1                    to   w-pbc-ctr-001          .
           if        w-pbc-ctr-001        >    w-pbc-num-ele
                     go to acc-dti-bol-540.
           if        w-acc-num-prt        not  = w-pbc-num-prt
                                                (w-pbc-ctr-001)
                     go to acc-dti-bol-520.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Bolla gia' richiamata !                           
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-dti-bol-100.
       acc-dti-bol-540.
      *              *-------------------------------------------------*
      *              * Incremento contatore protocolli bolle clienti   *
      *              *-------------------------------------------------*
           add       1                    to   w-pbc-num-ele          .
           if        w-pbc-num-ele        not  >  w-pbc-max-ele
                     go to acc-dti-bol-600.
      *                  *---------------------------------------------*
      *                  * Se oltre il massimo                         *
      *                  *---------------------------------------------*
           move      "Numero bolle trattate oltre il massimo !          
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A uscita per Exit                           *
      *                  *---------------------------------------------*
           go to     acc-dti-bol-800.
       acc-dti-bol-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione righe bolla da trattare         *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-buf-rig-bol-prt      .
           perform   buf-rig-bol-000      thru buf-rig-bol-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        w-buf-rig-bol-flg    =    spaces
                     go to acc-dti-bol-620.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Nessuna riga bolla da fatturare !                 
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-bol-100.
       acc-dti-bol-620.
      *              *-------------------------------------------------*
      *              * Se fatturazione separata per l'ordine, control- *
      *              * lo che la bolla richiamata appartenga alla      *
      *              * stessa conferma d'ordine                        *
      *              *-------------------------------------------------*
           if        w-gen-fat-sep        not  = "O"
                     go to acc-dti-bol-640.
           if        w-buf-rig-bol-pro    =    w-gen-coc-prt
                     go to acc-dti-bol-640.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "La bolla richiamata non appartiene all'ordine in c
      -              "orso di        "    to   w-err-box-err-msg      .
           move      "fatturazione !                                    
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-bol-100.
       acc-dti-bol-640.
      *              *-------------------------------------------------*
      *              * Test su segnale di si/no momentanea esclusione  *
      *              * della bolla dalla fatturazione differita auto-  *
      *              * matica                                          *
      *              *-------------------------------------------------*
           if        rf-bit-flg-nbx (1)   =    spaces
                     go to acc-dti-bol-660.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "La bolla richiamata risulta momentaneamente sospes
      -              "a dalla fat-   "    to   w-err-box-err-msg      .
           move      "turazione differita automatica !                  
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                  *---------------------------------------------*
      *                  * Proseguimento                               *
      *                  *---------------------------------------------*
           go to     acc-dti-bol-660.
       acc-dti-bol-660.
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero protocollo in tabella    *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-pbc-num-prt
                                              (w-pbc-num-ele)         .
      *              *-------------------------------------------------*
      *              * Aggiornamento protocollo bolla cliente in cor-  *
      *              * so di trattamento                               *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-gen-prt-bol          .
      *              *-------------------------------------------------*
      *              * A uscita                                        *
      *              *-------------------------------------------------*
           go to     acc-dti-bol-900.
       acc-dti-bol-800.
      *              *-------------------------------------------------*
      *              * Se uscita per Exit                              *
      *              *-------------------------------------------------*
           move      "#"                  to   l-fat-dfm-exi-sts      .
       acc-dti-bol-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-dti-bol-999.
       acc-dti-bol-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo movimento per bollettazione     *
      *    *-----------------------------------------------------------*
       acc-cod-tmb-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-tmb-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zbi-ope      .
           move      w-acc-cod-tmb        to   w-cod-des-zbi-cod      .
           move      14                   to   w-cod-des-zbi-lin      .
           move      09                   to   w-cod-des-zbi-pos      .
           move      14                   to   w-cod-des-zbi-dln      .
           move      15                   to   w-cod-des-zbi-dps      .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-des-zbi-cll-000  thru cod-des-zbi-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cod-des-zbi-foi-000  thru cod-des-zbi-foi-999    .
       acc-cod-tmb-110.
           perform   cod-des-zbi-cll-000  thru cod-des-zbi-cll-999    .
           if        w-cod-des-zbi-ope    =    "F+"
                     go to acc-cod-tmb-115.
           if        w-cod-des-zbi-ope    =    "AC"
                     go to acc-cod-tmb-120.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-cod-tmb-115.
           perform   cod-des-zbi-foi-000  thru cod-des-zbi-foi-999    .
           go to     acc-cod-tmb-110.
       acc-cod-tmb-120.
           move      w-cod-des-zbi-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-cod-tmb-999.
       acc-cod-tmb-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-acc-cod-tmb          .
       acc-cod-tmb-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmb        to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-cod-tmb-100.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zbi] generale             *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmb        to   w-let-arc-zbi-cod      .
           move      zero                 to   w-let-arc-zbi-dpz      .
           perform   let-arc-zbi-000      thru let-arc-zbi-999        .
           move      w-let-arc-zbi-des    to   w-acc-cod-tmb-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-cod-tmb-des-000  thru vis-cod-tmb-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zbi-flg    not  = spaces
                     go to acc-cod-tmb-100.
      *                  *---------------------------------------------*
      *                  * Se a spaces : nessun controllo              *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmb        =    spaces
                     go to acc-cod-tmb-600.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento                                   *
      *                  *---------------------------------------------*
           move      w-let-arc-zbi-mif    to   w-acc-cod-tmb-mif      .
           move      w-let-arc-zbi-tmf    to   w-acc-cod-tmb-tmf      .
           move      w-let-arc-zbi-tm2    to   w-acc-cod-tmb-tm2      .
           move      w-let-arc-zbi-acm    to   w-acc-cod-tmb-acm      .
           move      w-let-arc-zbi-dtc    to   w-acc-cod-tmb-dtc      .
           move      w-let-arc-zbi-dct    to   w-acc-cod-tmb-dct      .
           move      w-let-arc-zbi-cam    to   w-acc-cod-tmb-cam      .
           move      w-let-arc-zbi-ctm    to   w-acc-cod-tmb-ctm      .
           move      w-let-arc-zbi-dfa    to   w-acc-cod-tmb-dfa      .
           move      w-let-arc-zbi-vaa    to   w-acc-cod-tmb-vaa      .
           move      w-let-arc-zbi-lsa    to   w-acc-cod-tmb-lsa      .
           move      w-let-arc-zbi-ord    to   w-acc-cod-tmb-ord      .
           move      w-let-arc-zbi-prd    to   w-acc-cod-tmb-prd      .
           move      w-let-arc-zbi-sgl    to   w-acc-cod-tmb-sgl      .
           move      w-let-arc-zbi-maf    to   w-acc-cod-tmb-maf      .
           move      w-let-arc-zbi-dmf    to   w-acc-cod-tmb-dmf      .
           move      w-let-arc-zbi-tvd    to   w-acc-cod-tmb-tvd      .
           move      w-let-arc-zbi-dsl    to   w-acc-cod-tmb-ddd      .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zbi] relativo alla dipen- *
      *                  * denza                                       *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmb        to   w-let-arc-zbi-cod      .
           move      w-acc-cod-dpz        to   w-let-arc-zbi-dpz      .
           perform   let-arc-zbi-000      thru let-arc-zbi-999        .
       acc-cod-tmb-410.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo validita'   *
      *                  * per le dipendenze                           *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmb-tvd    =    spaces
                     go to acc-cod-tmb-411
           else if   w-acc-cod-tmb-tvd    =    "S"
                     go to acc-cod-tmb-412
           else if   w-acc-cod-tmb-tvd    =    "D"
                     go to acc-cod-tmb-413
           else if   w-acc-cod-tmb-tvd    =    "X"
                     go to acc-cod-tmb-414.
       acc-cod-tmb-411.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Tutte    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cod-tmb-420.
       acc-cod-tmb-412.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo la  *
      *                  * Sede                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-acc-cod-dpz       not  = 1
                     go to acc-cod-tmb-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cod-tmb-420.
       acc-cod-tmb-413.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-acc-cod-dpz       =    1
                     go to acc-cod-tmb-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cod-tmb-420.
       acc-cod-tmb-414.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze indicate                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se record esistente                *
      *                      *-----------------------------------------*
           if        w-let-arc-zbi-flg    not  = spaces
                     go to acc-cod-tmb-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     acc-cod-tmb-420.
       acc-cod-tmb-418.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Tipo movimento incompatibile con la dipendenza in 
      -              "uso !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-cod-tmb-100.
       acc-cod-tmb-420.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione codice dislocazione         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record esistente si bufferizza il    *
      *                      * codice dislocazione in esso contenuto,  *
      *                      * altrimenti si bufferizza quello di de-  *
      *                      * fault                                   *
      *                      *-----------------------------------------*
           if        w-let-arc-zbi-flg    =    spaces
                     move  w-let-arc-zbi-dsl
                                          to   w-acc-cod-tmb-dsl
           else      move  w-acc-cod-tmb-ddd
                                          to   w-acc-cod-tmb-dsl      .
      *                  *---------------------------------------------*
      *                  * Test se tipo movimento per fattura accompa- *
      *                  * gnatoria                                    *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmb-mif    not  = 03
                     go to acc-cod-tmb-440.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento non fatturabile in quanto gia' fatt
      -              "ura !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-tmb-100.
       acc-cod-tmb-440.
      *                  *---------------------------------------------*
      *                  * Test che tra i tipi archivio ammessi per il *
      *                  * movimento ci sia l'archivio clienti         *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-cod-tmb-inx      .
           inspect   w-acc-cod-tmb-lsa
                                      tallying w-acc-cod-tmb-inx
                                          for  all "C"                .
      *                      *-----------------------------------------*
      *                      * Se archivio clienti e' tra quelli am-   *
      *                      * messi per il tipo movimento : oltre     *
      *                      *-----------------------------------------*
           if        w-acc-cod-tmb-inx    >    zero
                     go to acc-cod-tmb-460.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Il tipo movimento non riguarda l'archivio clienti 
      -              "!              "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-tmb-100.
       acc-cod-tmb-460.
      *                  *---------------------------------------------*
      *                  * Se movimento che interessa la fatturazione, *
      *                  * controllo che sia compatibile con quello    *
      *                  * impostato a inizio fattura                  *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmb-mif    not  = 02
                     go to acc-cod-tmb-600.
           if        w-tes-tip-mov-ord    =    11
                     go to acc-cod-tmb-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento per fatturazione associato incompat
      -              "ibile          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-tmb-100.
       acc-cod-tmb-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore di default             *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmb        to   l-fat-dfm-cod-tmb      .
       acc-cod-tmb-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-cod-tmb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo movimento per bollettazione  *
      *    *-----------------------------------------------------------*
       vis-cod-tmb-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      w-acc-cod-tmb        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tmb-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione tipo movimento per    *
      *    * bollettazione                                             *
      *    *-----------------------------------------------------------*
       vis-cod-tmb-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-acc-cod-tmb-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-tmb-des-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Data documento                       *
      *    *-----------------------------------------------------------*
       acc-dat-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale default                           *
      *                  *---------------------------------------------*
           if        w-acc-dat-doc        not  = zero
                     go to acc-dat-doc-100.
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-acc-dat-doc          .
       acc-dat-doc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      14                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-acc-dat-doc        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-dat-doc-999.
       acc-dat-doc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   w-acc-dat-doc          .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-dat-doc-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [bit]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-bit-000      thru fnd-arc-bit-999        .
           if        w-fnd-arc-bit-sel    not  = spaces
                     go to acc-dat-doc-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
       acc-dat-doc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non si sia su Up                        *
      *                  *---------------------------------------------*
           if        w-acc-dat-doc        not  = zero
                     go to acc-dat-doc-600.
           if        v-key                =    "UP  "
                     go to acc-dat-doc-600
           else      go to acc-dat-doc-100.
       acc-dat-doc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-doc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-dat-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Data documento                    *
      *    *-----------------------------------------------------------*
       vis-dat-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      14                   to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-acc-dat-doc        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dat-doc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Numero documento                     *
      *    *-----------------------------------------------------------*
       acc-num-doc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-num-doc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "PRSC"               to   v-pfk (07)             .
           move      "SLCT"               to   v-pfk (11)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-acc-num-doc-prg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-num-doc-999.
       acc-num-doc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-acc-num-doc-prg      .
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-doc-350.
      *                  *---------------------------------------------*
      *                  * Find su archivio [bit]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-bit-000      thru fnd-arc-bit-999        .
           if        w-fnd-arc-bit-sel    not  = spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
       acc-num-doc-350.
      *              *-------------------------------------------------*
      *              * Se Select                                       *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-num-doc-380.
      *                  *---------------------------------------------*
      *                  * Test preliminari                            *
      *                  *---------------------------------------------*
           if        w-acc-num-doc-prg    =    zero
                     go to acc-num-doc-100.
           if        w-acc-dat-doc        =    zero
                     go to acc-num-doc-100.
           if        w-acc-cod-tmb        =    spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Preparazione dei parametri per la selezione *
      *                  *---------------------------------------------*
           move      w-acc-num-doc-prg    to   w-slc-num-bit-npg      .
           move      w-acc-cod-dpz        to   w-slc-num-bit-dpz      .
           move      w-acc-cod-tmb-sgl    to   w-slc-num-bit-sgl      .
           if        w-acc-cod-tmb-mif    =    03
                     move  "S"            to   w-slc-num-bit-snf
           else      move  "N"            to   w-slc-num-bit-snf      .
           move      w-acc-dat-doc        to   w-slc-num-bit-dds      .
      *                  *---------------------------------------------*
      *                  * Routine di selezione                        *
      *                  *---------------------------------------------*
           perform   slc-num-bit-000      thru slc-num-bit-999        .
      *                  *---------------------------------------------*
      *                  * Se non selezionato alcun elemento : a       *
      *                  * reimpostazione                              *
      *                  *---------------------------------------------*
           if        w-slc-num-bit-sel    not  = spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Forzatura function-key Do                   *
      *                  *---------------------------------------------*
           move      "DO  "               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * A controllo valore impostato                *
      *                  *---------------------------------------------*
           go to     acc-num-doc-400.
       acc-num-doc-380.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
           if        v-key                not  = "PRSC"
                     go to acc-num-doc-400.
      *                  *---------------------------------------------*
      *                  * Preparazione dei parametri per la selezione *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-buf-doc-mvm-drc      .
           move      s-ute                to   w-buf-doc-mvm-ute      .
           move      w-tes-cod-dpz        to   w-buf-doc-mvm-dpz      .
      *
           if        w-tes-dat-doc        not  = zero
                     move  w-tes-dat-doc  to   w-buf-doc-mvm-drc      .
      *                  *---------------------------------------------*
      *                  * Routine di ricerca                          *
      *                  *---------------------------------------------*
           perform   buf-doc-mvm-000      thru buf-doc-mvm-999        .
      *                      *-----------------------------------------*
      *                      * Test sul flag di uscita                 *
      *                      *-----------------------------------------*
           if        w-buf-doc-mvm-fds    not  = spaces
                     move  spaces         to   v-key
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Valori selezionati                          *
      *                  *---------------------------------------------*
           move      w-buf-doc-mvm-tds    to   w-acc-cod-tmb          .
           move      w-buf-doc-mvm-nds    to   w-acc-num-doc          .
           move      w-buf-doc-mvm-dds    to   w-acc-dat-doc          .
      *                  *---------------------------------------------*
      *                  * Completamento visualizzazione campi-chiave  *
      *                  *---------------------------------------------*
           perform   vis-cod-tmb-000      thru vis-cod-tmb-999        .
           perform   vis-cod-tmb-des-000  thru vis-cod-tmb-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
       acc-num-doc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-num-doc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-num-doc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-num-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Numero documento                  *
      *    *-----------------------------------------------------------*
       vis-num-doc-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      14                   to   v-lin                  .
           move      73                   to   v-pos                  .
           move      w-acc-num-doc-prg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-num-doc-999.
           exit.

      *    *===========================================================*
      *    * Select archivio [bit] in base al numero documento         *
      *    *-----------------------------------------------------------*
       slc-num-bit-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-slc-num-bit-sel      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-slc-num-bit-toc      .
           move      zero                 to   w-slc-num-bit-dat      .
           move      zero                 to   w-slc-num-bit-num      .
           move      zero                 to   w-slc-num-bit-crb      .
      *              *-------------------------------------------------*
      *              * Preparazione secolo, anno                       *
      *              *-------------------------------------------------*
           move      w-slc-num-bit-dds    to   s-dat                  .
           move      s-saa                to   w-slc-num-bit-saa      .
       slc-num-bit-080.
      *              *-------------------------------------------------*
      *              * Completamento numero documento                  *
      *              *-------------------------------------------------*
           move      w-slc-num-bit-saa    to   w-slc-num-bit-nsa      .
           move      w-slc-num-bit-dpz    to   w-slc-num-bit-ndp      .
       slc-num-bit-100.
      *              *-------------------------------------------------*
      *              * Start su file [bit]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CNTDEN    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-slc-num-bit-snf    to   rf-bit-snx-fac         .
           move      w-slc-num-bit-saa    to   rf-bit-scl-ann         .
           move      w-slc-num-bit-dpz    to   rf-bit-cod-dpz         .
           move      w-slc-num-bit-sgl    to   rf-bit-sgl-num         .
           move      w-slc-num-bit-nds    to   rf-bit-num-doc         .
           move      zero                 to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : a controllo contatore  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to slc-num-bit-800.
       slc-num-bit-200.
      *              *-------------------------------------------------*
      *              * Read-next su [bit]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a controllo contatore           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to slc-num-bit-500.
       slc-num-bit-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a controllo contatore     *
      *              *-------------------------------------------------*
           if        rf-bit-snx-fac       not  = w-slc-num-bit-snf or
                     rf-bit-scl-ann       not  = w-slc-num-bit-saa or
                     rf-bit-cod-dpz       not  = w-slc-num-bit-dpz or
                     rf-bit-sgl-num       not  = w-slc-num-bit-sgl or
                     rf-bit-num-doc       not  = w-slc-num-bit-nds
                     go to slc-num-bit-500.
       slc-num-bit-400.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-slc-num-bit-crb      .
      *              *-------------------------------------------------*
      *              * Test se buffer oltre il numero previsto         *
      *              *-------------------------------------------------*
           if        w-slc-num-bit-crb    >    30
                     go to slc-num-bit-520.
       slc-num-bit-420.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Protocollo ordine                           *
      *                  *---------------------------------------------*
           move      rf-bit-num-prt       to   w-slc-num-bit-bpt
                                              (w-slc-num-bit-crb)     .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura                           *
      *                  *---------------------------------------------*
           go to     slc-num-bit-200.
       slc-num-bit-500.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-slc-num-bit-crb    =    zero
                     go to slc-num-bit-800.
      *                  *---------------------------------------------*
      *                  * Se trovato un solo elemento uscita con      *
      *                  * quello                                      *
      *                  *---------------------------------------------*
           if        w-slc-num-bit-crb    >    1
                     go to slc-num-bit-520.
      *                      *-----------------------------------------*
      *                      * Lettura record [bit]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-bit-bpt (1)
                                          to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-bit-dat-doc
                     move  zero           to   rf-bit-num-doc
                     move  zero           to   rf-bit-cod-arc
                     move  spaces         to   rf-bit-dpz-arc         .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione dati letti              *
      *                      *-----------------------------------------*
           move      rf-bit-dat-doc       to   w-slc-num-bit-dat      .
           move      rf-bit-num-doc       to   w-slc-num-bit-num      .
           move      rf-bit-cod-tmb       to   w-slc-num-bit-toc      .
      *                      *-----------------------------------------*
      *                      * Ad operazioni prima dell'uscita         *
      *                      *-----------------------------------------*
           go to     slc-num-bit-800.
       slc-num-bit-520.
      *                  *---------------------------------------------*
      *                  * Box di espansione                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-slc-num-bit-crb    to   w-slc-num-bit-cpb      .
           subtract  1                    from w-slc-num-bit-cpb      .
           divide    6                    into w-slc-num-bit-cpb      .
           add       1                    to   w-slc-num-bit-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-slc-num-bit-c01      .
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione box vuoto               *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      18                   to   v-lto                  .
           move      80                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      "                       Selezionare l'ordine deside
      -              "rato                      "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura di chiusura              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      76                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      03                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   slc-num-bit-950      thru slc-num-bit-989        .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-bit-550.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           move      w-slc-num-bit-c01    to   w-slc-num-bit-nli      .
       slc-num-bit-555.
           if        w-slc-num-bit-nli    >    6
                     subtract  6          from w-slc-num-bit-nli
                     go to slc-num-bit-555.
      *                          *-------------------------------------*
      *                          * Incremento numero linea a video     *
      *                          * per posizionamento verticale        *
      *                          *-------------------------------------*
           add       09                   to   w-slc-num-bit-nli      .
       slc-num-bit-560.
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
       slc-num-bit-575.
      *                      *-----------------------------------------*
      *                      * Accettazione del mark-point             *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-slc-num-bit-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-slc-num-bit-c01    <    w-slc-num-bit-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-slc-num-bit-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-slc-num-bit-cpa    <    w-slc-num-bit-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-slc-num-bit-nli    to   v-lin                  .
           move      09                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-bit-580.
           if        v-key                =    spaces or
                     v-key                =    "DO  "
                     go to slc-num-bit-582
           else if   v-key                =    "UP  "
                     go to slc-num-bit-584
           else if   v-key                =    "DOWN"
                     go to slc-num-bit-586
           else if   v-key                =    "EXIT"
                     go to slc-num-bit-598
           else if   v-key                =    "NXSC"
                     go to slc-num-bit-592
           else if   v-key                =    "PRSC"
                     go to slc-num-bit-594
           else      go to slc-num-bit-575.
       slc-num-bit-582.
      *              *-------------------------------------------------*
      *              * Se spaces, Do o select                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Restore video                               *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valore selezionato           *
      *                  *---------------------------------------------*
           move      w-slc-num-bit-bpt
                    (w-slc-num-bit-c01)   to   w-slc-num-bit-prt      .
      *                  *---------------------------------------------*
      *                  * Lettura record [bit]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-bit-prt    to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-bit-dat-doc
                     move  zero           to   rf-bit-num-doc
                     move  zero           to   rf-bit-cod-arc
                     move  spaces         to   rf-bit-dpz-arc         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione dati letti                  *
      *                  *---------------------------------------------*
           move      rf-bit-dat-doc       to   w-slc-num-bit-dat      .
           move      rf-bit-num-doc       to   w-slc-num-bit-num      .
           move      rf-bit-cod-tmb       to   w-slc-num-bit-toc      .
      *                  *---------------------------------------------*
      *                  * Ad operazioni prima dell'uscita             *
      *                  *---------------------------------------------*
           go to     slc-num-bit-800.
       slc-num-bit-584.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           subtract  1                    from w-slc-num-bit-c01      .
           if        w-slc-num-bit-nli    =    10
                     go to slc-num-bit-590
           else      go to slc-num-bit-550.
       slc-num-bit-586.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           if        w-slc-num-bit-c01    <    w-slc-num-bit-crb
                     add   1              to   w-slc-num-bit-c01
                     go to slc-num-bit-588
           else      go to slc-num-bit-575.
       slc-num-bit-588.
           if        w-slc-num-bit-nli    =    15
                     go to slc-num-bit-590
           else      go to slc-num-bit-550.
       slc-num-bit-590.
           perform   slc-num-bit-950      thru slc-num-bit-989        .
           go to     slc-num-bit-550.
       slc-num-bit-592.
      *              *-------------------------------------------------*
      *              * Se Next screen                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-slc-num-bit-cpa      .
           go to     slc-num-bit-596.
       slc-num-bit-594.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
           subtract  1                    from w-slc-num-bit-cpa      .
       slc-num-bit-596.
           move      w-slc-num-bit-cpa    to   w-slc-num-bit-c01      .
           multiply  6                    by   w-slc-num-bit-c01      .
           subtract  5                    from w-slc-num-bit-c01      .
           go to     slc-num-bit-590.
       slc-num-bit-598.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-bit-800.
      *              *-------------------------------------------------*
      *              * Salvataggio valori chiave                       *
      *              *-------------------------------------------------*
           move      w-tes-val-key        to   w-sav-val-acc          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori estratti                 *
      *              *-------------------------------------------------*
           move      w-slc-num-bit-toc    to   w-acc-cod-tmb          .
           move      w-slc-num-bit-dat    to   w-acc-dat-doc          .
           move      w-slc-num-bit-num    to   w-acc-num-doc          .
      *              *-------------------------------------------------*
      *              * Test su valori estratti                         *
      *              *-------------------------------------------------*
           if        w-acc-cod-tmb        =    spaces or
                     w-acc-dat-doc        =    zero   or
                     w-acc-num-doc        =    zero
                     go to slc-num-bit-900.
       slc-num-bit-820.
      *              *-------------------------------------------------*
      *              * Determinazione campi derivati ed effettuazione  *
      *              * controlli come se fossero stati impostati       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zbi] generale             *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmb        to   w-let-arc-zbi-cod      .
           move      zero                 to   w-let-arc-zbi-dpz      .
           perform   let-arc-zbi-000      thru let-arc-zbi-999        .
           move      w-let-arc-zbi-des    to   w-acc-cod-tmb-des      .
      *                  *---------------------------------------------*
      *                  * Se codice errato : uscita con errore        *
      *                  *---------------------------------------------*
           if        w-let-arc-zbi-flg    not  = spaces
                     go to slc-num-bit-900.
      *                  *---------------------------------------------*
      *                  * Se a spaces : uscita con errore             *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmb        =    spaces
                     go to slc-num-bit-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento                                   *
      *                  *---------------------------------------------*
           move      w-let-arc-zbi-mif    to   w-acc-cod-tmb-mif      .
           move      w-let-arc-zbi-tmf    to   w-acc-cod-tmb-tmf      .
           move      w-let-arc-zbi-tm2    to   w-acc-cod-tmb-tm2      .
           move      w-let-arc-zbi-acm    to   w-acc-cod-tmb-acm      .
           move      w-let-arc-zbi-dtc    to   w-acc-cod-tmb-dtc      .
           move      w-let-arc-zbi-dct    to   w-acc-cod-tmb-dct      .
           move      w-let-arc-zbi-cam    to   w-acc-cod-tmb-cam      .
           move      w-let-arc-zbi-ctm    to   w-acc-cod-tmb-ctm      .
           move      w-let-arc-zbi-dfa    to   w-acc-cod-tmb-dfa      .
           move      w-let-arc-zbi-vaa    to   w-acc-cod-tmb-vaa      .
           move      w-let-arc-zbi-lsa    to   w-acc-cod-tmb-lsa      .
           move      w-let-arc-zbi-ord    to   w-acc-cod-tmb-ord      .
           move      w-let-arc-zbi-prd    to   w-acc-cod-tmb-prd      .
           move      w-let-arc-zbi-sgl    to   w-acc-cod-tmb-sgl      .
           move      w-let-arc-zbi-maf    to   w-acc-cod-tmb-maf      .
           move      w-let-arc-zbi-dmf    to   w-acc-cod-tmb-dmf      .
           move      w-let-arc-zbi-tvd    to   w-acc-cod-tmb-tvd      .
           move      w-let-arc-zbi-dsl    to   w-acc-cod-tmb-ddd      .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zbi] relativo alla dipen- *
      *                  * denza                                       *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmb        to   w-let-arc-zbi-cod      .
           move      w-acc-cod-dpz        to   w-let-arc-zbi-dpz      .
           perform   let-arc-zbi-000      thru let-arc-zbi-999        .
       slc-num-bit-830.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo validita'   *
      *                  * per le dipendenze                           *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmb-tvd    =    spaces
                     go to slc-num-bit-840
           else if   w-acc-cod-tmb-tvd    =    "S"
                     go to slc-num-bit-842
           else if   w-acc-cod-tmb-tvd    =    "D"
                     go to slc-num-bit-844
           else if   w-acc-cod-tmb-tvd    =    "X"
                     go to slc-num-bit-846.
       slc-num-bit-840.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Tutte    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     slc-num-bit-850.
       slc-num-bit-842.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo la  *
      *                  * Sede                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-acc-cod-dpz       not  = 1
                     go to slc-num-bit-848.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     slc-num-bit-850.
       slc-num-bit-844.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-acc-cod-dpz       =    1
                     go to slc-num-bit-848.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     slc-num-bit-850.
       slc-num-bit-846.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze indicate                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se record esistente                *
      *                      *-----------------------------------------*
           if        w-let-arc-zbi-flg    not  = spaces
                     go to slc-num-bit-848.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     slc-num-bit-850.
       slc-num-bit-848.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Tipo movimento incompatibile con la dipendenza in 
      -              "uso !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A uscita con errore                         *
      *                  *---------------------------------------------*
           go to     slc-num-bit-900.
       slc-num-bit-850.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione codice dislocazione         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record esistente si bufferizza il    *
      *                      * codice dislocazione in esso contenuto,  *
      *                      * altrimenti si bufferizza quello di de-  *
      *                      * fault                                   *
      *                      *-----------------------------------------*
           if        w-let-arc-zbi-flg    =    spaces
                     move  w-let-arc-zbi-dsl
                                          to   w-acc-cod-tmb-dsl
           else      move  w-acc-cod-tmb-ddd
                                          to   w-acc-cod-tmb-dsl      .
      *                  *---------------------------------------------*
      *                  * Test se tipo movimento per fattura accompa- *
      *                  * gnatoria                                    *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmb-mif    not  = 03
                     go to slc-num-bit-856.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento non fatturabile in quanto gia' fatt
      -              "ura !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     slc-num-bit-900.
       slc-num-bit-856.
      *                  *---------------------------------------------*
      *                  * Test che tra i tipi archivio ammessi per il *
      *                  * movimento ci sia l'archivio clienti         *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-cod-tmb-inx      .
           inspect   w-acc-cod-tmb-lsa
                                      tallying w-acc-cod-tmb-inx
                                          for  all "C"                .
      *                      *-----------------------------------------*
      *                      * Se archivio clienti e' tra quelli am-   *
      *                      * messi per il tipo movimento : oltre     *
      *                      *-----------------------------------------*
           if        w-acc-cod-tmb-inx    >    zero
                     go to slc-num-bit-858.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Il tipo movimento non riguarda l'archivio clienti 
      -              "!              "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     slc-num-bit-900.
       slc-num-bit-858.
      *                  *---------------------------------------------*
      *                  * Se movimento che interessa la fatturazione, *
      *                  * controllo che sia compatibile con quello    *
      *                  * impostato a inizio fattura                  *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmb-mif    not  = 02
                     go to slc-num-bit-860.
           if        w-tes-tip-mov-ord    =    11
                     go to slc-num-bit-860.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento per fatturazione associato incompat
      -              "ibile          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     slc-num-bit-900.
       slc-num-bit-860.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Completamento visualizzazione campi-chiave  *
      *                  *---------------------------------------------*
           perform   vis-cod-tmb-000      thru vis-cod-tmb-999        .
           perform   vis-cod-tmb-des-000  thru vis-cod-tmb-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *                  *---------------------------------------------*
      *                  * Uscita con successo                         *
      *                  *---------------------------------------------*
           go to     slc-num-bit-999.
       slc-num-bit-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ripristino valori chiave salvati            *
      *                  *---------------------------------------------*
           move      w-sav-val-acc        to   w-tes-val-key          .
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-slc-num-bit-sel      .
       slc-num-bit-940.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     slc-num-bit-999.
       slc-num-bit-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-slc-num-bit-c01    to   w-slc-num-bit-c02      .
           add       5                    to   w-slc-num-bit-c02      .
           divide    6                    into w-slc-num-bit-c02      .
           move      w-slc-num-bit-c02    to   w-slc-num-bit-cpa      .
           subtract  1                    from w-slc-num-bit-c02      .
           multiply  6                    by   w-slc-num-bit-c02      .
           add       1                    to   w-slc-num-bit-c02      .
           add       5
                     w-slc-num-bit-c02  giving w-slc-num-bit-c03      .
           move      w-slc-num-bit-c03    to   w-slc-num-bit-c04      .
           if        w-slc-num-bit-c03    >    w-slc-num-bit-crb
                     move  w-slc-num-bit-crb
                                          to   w-slc-num-bit-c03      .
           move      10                   to   w-slc-num-bit-c05      .
       slc-num-bit-951.
      *              *-------------------------------------------------*
      *              * Lettura record [bit] per visualizzazione        *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-bit-bpt
                    (w-slc-num-bit-c02)   to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-bit-dat-doc
                     move  zero           to   rf-bit-num-doc
                     move  zero           to   rf-bit-cod-arc
                     move  spaces         to   rf-bit-dpz-arc         .
       slc-num-bit-960.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice tipo movimento                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-slc-num-bit-c05    to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-bit-cod-tmb       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-slc-num-bit-c05    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      rf-bit-dat-doc       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-slc-num-bit-c05    to   v-lin                  .
           move      18                   to   v-pos                  .
           move      rf-bit-num-doc (6:6) to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice archivio                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-slc-num-bit-c05    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      rf-bit-cod-arc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza archivio                  *
      *                  *---------------------------------------------*
           if        rf-bit-dpz-arc       =    spaces
                     go to slc-num-bit-965.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-slc-num-bit-c05    to   v-lin                  .
           move      33                   to   v-pos                  .
           move      "-"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      w-slc-num-bit-c05    to   v-lin                  .
           move      34                   to   v-pos                  .
           move      rf-bit-dpz-arc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-bit-965.
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      rf-bit-cod-arc       to   rf-dcc-cod-cli         .
           move      rf-bit-dpz-arc       to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcc-rag-soc         .
      *                  *---------------------------------------------*
      *                  * Ragione sociale archivio                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      w-slc-num-bit-c05    to   v-lin                  .
           move      39                   to   v-pos                  .
           move      rf-dcc-rag-soc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-slc-num-bit-c02      .
           add       1                    to   w-slc-num-bit-c05      .
           if        w-slc-num-bit-c02    not  > w-slc-num-bit-c03
                     go to slc-num-bit-951.
       slc-num-bit-970.
           if        w-slc-num-bit-c02    >    w-slc-num-bit-c04
                     go to slc-num-bit-980.
           if        w-slc-num-bit-crb    not  > 6
                     go to slc-num-bit-980.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-slc-num-bit-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-slc-num-bit-c02      .
           add       1                    to   w-slc-num-bit-c05      .
           go to     slc-num-bit-970.
       slc-num-bit-980.
      *                  *---------------------------------------------*
      *                  * Literal 'pagina'                            *
      *                  *---------------------------------------------*
           move      w-slc-num-bit-cpa    to   w-slc-num-bit-lt1      .
           move      w-slc-num-bit-cpb    to   w-slc-num-bit-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-slc-num-bit-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-bit-989.
           exit.
       slc-num-bit-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione work-area di accettazione                 *
      *    *-----------------------------------------------------------*
       nor-wrk-acc-000.
      *              *-------------------------------------------------*
      *              * Numero protocollo                               *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-num-prt          .
      *              *-------------------------------------------------*
      *              * Tipo movimento per bollettazione                *
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-cod-tmb-des      .
           move      zero                 to   w-acc-cod-tmb-mif      .
           move      spaces               to   w-acc-cod-tmb-tmf      .
           move      spaces               to   w-acc-cod-tmb-tm2      .
           move      spaces               to   w-acc-cod-tmb-acm      .
           move      spaces               to   w-acc-cod-tmb-dtc      .
           move      spaces               to   w-acc-cod-tmb-dct      .
           move      zero                 to   w-acc-cod-tmb-cam      .
           move      spaces               to   w-acc-cod-tmb-ctm      .
           move      spaces               to   w-acc-cod-tmb-dfa      .
           move      spaces               to   w-acc-cod-tmb-vaa      .
           move      spaces               to   w-acc-cod-tmb-lsa      .
           move      zero                 to   w-acc-cod-tmb-ord      .
           move      zero                 to   w-acc-cod-tmb-prd      .
           move      spaces               to   w-acc-cod-tmb-sgl      .
           move      zero                 to   w-acc-cod-tmb-maf      .
           move      spaces               to   w-acc-cod-tmb-dmf      .
           move      spaces               to   w-acc-cod-tmb-tvd      .
           move      spaces               to   w-acc-cod-tmb-ddd      .
           move      spaces               to   w-acc-cod-tmb-dsl      .
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-num-doc          .
       nor-wrk-acc-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione righe bolla da trattare                   *
      *    *-----------------------------------------------------------*
       buf-rig-bol-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di uscita ad errore       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-buf-rig-bol-flg      .
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
      *              * Normalizzazione numero protocollo prima confer- *
      *              * ma d'ordine incontrata                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-rig-bol-pro      .
       buf-rig-bol-100.
      *              *-------------------------------------------------*
      *              * Start su file [bir]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-buf-rig-bol-prt    to   rf-bir-num-prt         .
           move      zero                 to   rf-bir-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-bol-500.
       buf-rig-bol-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [bir]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-bol-500.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-bir-num-prt       not  = w-buf-rig-bol-prt
                     go to buf-rig-bol-500.
       buf-rig-bol-250.
      *              *-------------------------------------------------*
      *              * Aggiornamento comodi di visualizzazione         *
      *              *-------------------------------------------------*
           move      "AG"                 to   w-edt-qta-inc-ope      .
           move      rf-bir-qta-ven       to   w-edt-qta-inc-qta      .
           move      rf-bir-dec-qta       to   w-edt-qta-inc-dec      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
           move      w-edt-qta-inc-din    to   w-tes-dec-qta          .
      *              *-------------------------------------------------*
      *              * Ridefinizione tipo riga                         *
      *              *-------------------------------------------------*
           move      rf-bir-tip-rig       to   w-buf-rig-bol-wtr      .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-rig-bol-flg      .
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero protocollo prima confer- *
      *              * ma d'ordine cliente incontrata                  *
      *              *-------------------------------------------------*
           if        w-buf-rig-bol-pro    =    zero
                     move  rf-bir-coc-prt to   w-buf-rig-bol-pro      .
       buf-rig-bol-300.
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
           move      rf-bir-num-prg       to   w-brb-num-prg
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Tipo riga                                   *
      *                  *---------------------------------------------*
           move      w-buf-rig-bol-wtp    to   w-brb-tip-rig
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Descrizione per la riga                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura preliminare file [bix]          *
      *                      *-----------------------------------------*
           move      rf-bir-num-prt       to   w-let-arc-bix-prt      .
           move      rf-bir-num-prg       to   w-let-arc-bix-prg      .
           move      11                   to   w-let-arc-bix-trc      .
           perform   let-arc-bix-000      thru let-arc-bix-999        .
      *
           if        w-let-arc-bix-flg    not  = spaces
                     go to buf-rig-bol-305.
      *
           move      w-let-arc-bix-des    to   w-buf-rig-bol-wde      .
      *
           go to     buf-rig-bol-350.
       buf-rig-bol-305.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del flag di e-   *
      *                      * stensione alla descrizione              *
      *                      *-----------------------------------------*
           if        rf-bir-des-ext       =    0
                     go to buf-rig-bol-310
           else if   rf-bir-des-ext       =    1
                     go to buf-rig-bol-320
           else if   rf-bir-des-ext       =    2
                     go to buf-rig-bol-330.
       buf-rig-bol-310.
      *                      *-----------------------------------------*
      *                      * Se nessuna estensione : bufferizzazione *
      *                      * descrizione contenuta nel record [bir]  *
      *                      *-----------------------------------------*
           move      rf-bir-des-rig       to   w-buf-rig-bol-wde      .
           go to     buf-rig-bol-350.
       buf-rig-bol-320.
      *                      *-----------------------------------------*
      *                      * Se estensione nel file [bix]            *
      *                      *-----------------------------------------*
           go to     buf-rig-bol-350.
       buf-rig-bol-330.
      *                      *-----------------------------------------*
      *                      * Se estensione nel file [pdx]            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcp]              *
      *                          *-------------------------------------*
           move      rf-bir-num-pro       to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      "I  "                to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-buf-rig-bol-wde      .
           go to     buf-rig-bol-350.
       buf-rig-bol-350.
      *                      *-----------------------------------------*
      *                      * Composizione descrizione per riga di    *
      *                      * scroll                                  *
      *                      *-----------------------------------------*
           perform   cpz-des-rgs-000      thru cpz-des-rgs-999        .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      w-buf-rig-bol-wde    to   w-brb-des-rig
                                              (w-brb-num-ele)         .
       buf-rig-bol-356.
      *                  *---------------------------------------------*
      *                  * Numero decimali quantita'                   *
      *                  *---------------------------------------------*
           move      rf-bir-dec-qta       to   w-brb-dec-qta
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Quantita' in bolla                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento : zero   *
      *                      *-----------------------------------------*
           if        w-buf-rig-bol-wtp    =    "C" or
                     w-buf-rig-bol-wtp    =    "A"
                     move  zero           to   w-brb-qta-bol
                                              (w-brb-num-ele)
                     go to buf-rig-bol-360.
      *                      *-----------------------------------------*
      *                      * Altrimenti                              *
      *                      *-----------------------------------------*
           move      rf-bir-qta-ven       to   w-brb-qta-bol
                                              (w-brb-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Salvataggio quantita' di vendita        *
      *                      * per eventuale trattamento in righe      *
      *                      * addebito successive                     *
      *                      *-----------------------------------------*
           move      rf-bir-qta-ven       to   w-sav-qta-rpr          .
       buf-rig-bol-360.
      *                  *---------------------------------------------*
      *                  * Quantita' in fattura pari a qunatita' in    *
      *                  * bolla                                       *
      *                  *---------------------------------------------*
           move      w-brb-qta-bol
                    (w-brb-num-ele)       to   w-brb-qta-fat
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
      *                  * Quantita' residua pari alla quantita' da e- *
      *                  * vadere                                      *
      *                  *---------------------------------------------*
           move      zero                 to   w-brb-qta-res
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Si/No addebito o commento                   *
      *                  *---------------------------------------------*
           move      "S"                  to   w-brb-snx-aoc
                                              (w-brb-num-ele)         .
       buf-rig-bol-400.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale file [bir]       *
      *              *-------------------------------------------------*
           go to     buf-rig-bol-200.
       buf-rig-bol-500.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rig-bol-999.
       buf-rig-bol-999.
           exit.

      *    *===========================================================*
      *    * Composizione descrizione per riga di scroll               *
      *    *-----------------------------------------------------------*
       cpz-des-rgs-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se si sta trattando un co- *
      *              * dice di magazzino oppure no                     *
      *              *-------------------------------------------------*
           if       (w-buf-rig-bol-wtp    =    "P" or
                     w-buf-rig-bol-wtp    =    "S" or
                     w-buf-rig-bol-wtp    =    "M"  ) and
                     w-buf-rig-bol-wtf    =    spaces
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
           if        w-prs-rig-scr-des    =    00
                     go to cpz-des-rgs-400
           else if   w-prs-rig-scr-des    =    01
                     go to cpz-des-rgs-420
           else if   w-prs-rig-scr-des    =    02
                     go to cpz-des-rgs-440
           else if   w-prs-rig-scr-des    =    03
                     go to cpz-des-rgs-460
           else if   w-prs-rig-scr-des    =    04
                     go to cpz-des-rgs-480
           else if   w-prs-rig-scr-des    =    05
                     go to cpz-des-rgs-500
           else if   w-prs-rig-scr-des    =    06
                     go to cpz-des-rgs-520
           else      go to cpz-des-rgs-400.
       cpz-des-rgs-400.
      *                  *---------------------------------------------*
      *                  * Se personalizzazione tipo : 00              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione di 40 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-bol-wde    to   w-des-scr-000-d40      .
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
           move      w-buf-rig-bol-wde    to   w-des-scr-001-d25      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-bir-alf-pro       to   w-des-scr-001-cod      .
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
           move      w-buf-rig-bol-wde    to   w-des-scr-002-d21      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-002-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-bol-wtp    to   w-des-scr-002-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-002-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-bir-alf-pro       to   w-des-scr-002-cod      .
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
           move      rf-bir-alf-pro       to   w-des-scr-003-cod      .
      *                      *-----------------------------------------*
      *                      * Descrizione di 25 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-bol-wde    to   w-des-scr-003-d25      .
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
           move      w-buf-rig-bol-wtp    to   w-des-scr-004-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-004-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-bir-alf-pro       to   w-des-scr-004-cod      .
      *                      *-----------------------------------------*
      *                      * Descrizione di 21 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-bol-wde    to   w-des-scr-004-d21      .
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
           move      rf-bir-alf-pro       to   w-des-scr-005-cod      .
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
           move      w-buf-rig-bol-wtp    to   w-des-scr-006-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-006-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-bir-alf-pro       to   w-des-scr-006-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-700.
      *                  *---------------------------------------------*
      *                  * Composizione eseguita in area di destina-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      w-des-scr-000        to   w-buf-rig-bol-wde      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cpz-des-rgs-999.
       cpz-des-rgs-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione testata prima bolla cliente richiamata    *
      *    *-----------------------------------------------------------*
       buf-tes-bol-000.
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
      *              * Test su tipo archivio bolla                     *
      *              *-------------------------------------------------*
           if        rf-bit-tip-arc       =    "C"
                     go to buf-tes-bol-050.
      *                  *---------------------------------------------*
      *                  * Composizione messaggio di errore            *
      *                  *---------------------------------------------*
           move      "Documento non sottoponibile a fatturazione !      
      -              "               "    to   w-err-box-err-msg      .
      *                  *---------------------------------------------*
      *                  * A trattamento errore                        *
      *                  *---------------------------------------------*
           go to     buf-tes-bol-900.
       buf-tes-bol-050.
      *              *-------------------------------------------------*
      *              * Test su data bolla                              *
      *              *-------------------------------------------------*
           if        rf-bit-dat-doc       not  > w-tes-dat-doc
                     go to buf-tes-bol-100.
      *                  *---------------------------------------------*
      *                  * Composizione messaggio di errore            *
      *                  *---------------------------------------------*
           move      "Data bolla superiore alla data della fattura !    
      -              "               "    to   w-err-box-err-msg      .
      *                  *---------------------------------------------*
      *                  * A trattamento errore                        *
      *                  *---------------------------------------------*
           go to     buf-tes-bol-900.
       buf-tes-bol-100.
      *              *-------------------------------------------------*
      *              * Valori contenuti direttamente in record [bit]   *
      *              *-------------------------------------------------*
           move      rf-bit-cod-arc       to   w-tes-cod-cli (1)      .
           move      rf-bit-dpz-arc       to   w-tes-dpz-cli (1)      .
           move      rf-bit-cod-lng       to   w-tes-cod-lng (1)      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda che il tipo movimento sia  *
      *              * fatturabile o meno                              *
      *              *-------------------------------------------------*
           if        w-acc-cod-tmb-mif    =    01
                     go to buf-tes-bol-200
           else      go to buf-tes-bol-300.
       buf-tes-bol-200.
      *              *-------------------------------------------------*
      *              * Se tipo movimento che non interessa la fattura- *
      *              * zione : prelievo elementi testata da record     *
      *              * [cli] e [dcc]                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record [cli]                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                      *-----------------------------------------*
      *                      * Lettura                                 *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      rf-bit-cod-arc       to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Lettura                                 *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      rf-bit-cod-arc       to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori                      *
      *                  *---------------------------------------------*
           if        w-prs-ges-apf        not  = 11 and
                     w-prs-ges-apf        not  = 21 and
                     w-prs-ges-apf        not  = 99
                     move  01             to   w-tes-tip-frn (1)
           else      move  rf-dcc-tip-frn to   w-tes-tip-frn (1)      .
           if        w-tes-tip-frn (1)    =    01
                     move  w-tes-cod-cli (1)
                                          to   w-tes-cli-plf (1)
                     move  w-tes-dpz-cli (1)
                                          to   w-tes-dpc-plf (1)
           else if   w-tes-tip-frn (1)    =    11
                     move  zero           to   w-tes-cli-plf (1)
                     move  spaces         to   w-tes-dpc-plf (1)
           else if   w-tes-tip-frn (1)    =    21
                     move  rf-dcc-arc-plf to   w-tes-cli-plf (1)
                     move  rf-dcc-dpz-plf to   w-tes-dpc-plf (1)      .
           move      rf-dcc-inl-pgt       to   w-tes-inl-pgt (1)      .
           move      rf-dcc-cod-vlt       to   w-tes-sgl-vpf (1)      .
           move      w-tes-sgl-vpf (1)    to   w-let-arc-zvl-cod      .
           perform   let-arc-zvl-000      thru let-arc-zvl-999        .
           move      w-let-arc-zvl-dec    to   w-tes-dec-vpf (1)      .
           move      w-let-arc-zvl-tdc    to   w-tes-tdc-vpf (1)      .
           move      rf-cli-cod-iva       to   w-tes-ass-iva (1)      .
           move      rf-dcc-ctp-ven       to   w-tes-ctp-ven (1)      .
      *
           if        rf-dcc-rag-bft       =    11
                     move  "O"            to   w-gen-fat-sep          .
      *
           move      rf-dcc-vde-cod (1)   to   w-tes-voc-des (1, 1)   .
           move      rf-dcc-vde-cod (2)   to   w-tes-voc-des (1, 2)   .
           move      rf-dcc-vde-cod (3)   to   w-tes-voc-des (1, 3)   .
           move      rf-dcc-vde-cod (4)   to   w-tes-voc-des (1, 4)   .
           move      rf-dcc-vde-cod (5)   to   w-tes-voc-des (1, 5)   .
           move      rf-dcc-vde-cod (6)   to   w-tes-voc-des (1, 6)   .
           move      rf-dcc-cod-lst       to   w-tes-cod-lst (1)      .
           move      rf-dcc-cat-scr       to   w-tes-csr-aac (1)      .
           move      rf-dcc-per-scr (1)   to   w-tes-psr-aac (1, 1)   .
           move      rf-dcc-per-scr (2)   to   w-tes-psr-aac (1, 2)   .
           move      rf-dcc-per-scr (3)   to   w-tes-psr-aac (1, 3)   .
           move      rf-dcc-per-scr (4)   to   w-tes-psr-aac (1, 4)   .
           move      rf-dcc-per-scr (5)   to   w-tes-psr-aac (1, 5)   .
           move      rf-dcc-cat-scc       to   w-tes-csc-aac (1)      .
           move      rf-dcc-per-scc       to   w-tes-psc-aac (1)      .
           move      rf-dcc-cat-pvg       to   w-tes-cpv-aac (1)      .
           move      rf-dcc-per-pvg (1)   to   w-tes-ppv-aac (1, 1)   .
           move      rf-dcc-per-pvg (2)   to   w-tes-ppv-aac (1, 2)   .
           move      rf-dcc-per-pvg (3)   to   w-tes-ppv-aac (1, 3)   .
           move      rf-dcc-cod-age       to   w-tes-cod-age (1)      .
           if        w-prs-age-snx        =    "S"
                     move  01             to   w-tes-fsp-doc (1)
           else      move  02             to   w-tes-fsp-doc (1)      .
           move      zero                 to   w-tes-pvf-age (1)      .
           move      zero                 to   w-tes-tip-vpa (1)      .
           move      w-tes-cod-age (1)    to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
           move      w-let-arc-age-cpv    to   w-tes-cpv-aaa (1)      .
           move      w-let-arc-age-ppv (1)
                                          to   w-tes-ppv-aaa (1, 1)   .
           move      w-let-arc-age-ppv (2)
                                          to   w-tes-ppv-aaa (1, 2)   .
           move      w-let-arc-age-ppv (3)
                                          to   w-tes-ppv-aaa (1, 3)   .
           move      w-let-arc-age-spa    to   w-tes-cod-ime (1)      .
           move      zero                 to   w-tes-pvf-ime (1)      .
           move      rf-dcc-cod-fop       to   w-tes-cod-fop (1)      .
           move      rf-dcc-cod-abi       to   w-tes-cod-abi (1)      .
           move      rf-dcc-cod-cab       to   w-tes-cod-cab (1)      .
           move      rf-dcc-ccc-app       to   w-tes-ccc-app (1)      .
           move      rf-dcc-nos-ban       to   w-tes-nos-ban (1)      .
           move      rf-dcc-nos-ccp       to   w-tes-nos-ccp (1)      .
           move      rf-dcc-add-spi       to   w-tes-add-spi (1)      .
           move      rf-dcc-add-spb       to   w-tes-add-spb (1)      .
           move      zero                 to   w-tes-pag-dsm (1)      .
           move      zero                 to   w-tes-pag-qaf (1)      .
           move      zero                 to   w-tes-pag-act (1)      .
           move      zero                 to   w-pie-tot-scc (1)      .
           move      zero                 to   w-pie-per-scc (1)      .
           move      zero                 to   w-pie-tot-scp (1)      .
           move      zero                 to   w-pie-per-scp (1)      .
           if        w-tes-tip-frn (1)    =    01
                     move  "C"            to   w-det-fun-spe-tdt
           else if   w-tes-tip-frn (1)    =    11
                     move  "C"            to   w-det-fun-spe-tdt
           else if   w-tes-tip-frn (1)    =    21
                     move  "F"            to   w-det-fun-spe-tdt      .
           perform   det-fun-spe-000      thru det-fun-spe-999        .
           go to     buf-tes-bol-400.
       buf-tes-bol-300.
      *              *-------------------------------------------------*
      *              * Se tipo movimento che interessa la fatturazio-  *
      *              * ne : prelievo elementi testata da record [bit]  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di accettazione pagina testata 1       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-tes-fda-pt1 (1)      .
      *                  *---------------------------------------------*
      *                  * Flag di accettazione pagina testata 2       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-tes-fda-pt2 (1)      .
      *                  *---------------------------------------------*
      *                  * Flag di accettazione sigla valuta           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-tes-fda-vpf (1)      .
      *                  *---------------------------------------------*
      *                  * Prelievo elementi testata                   *
      *                  *---------------------------------------------*
           move      rf-bit-tip-frn       to   w-tes-tip-frn (1)      .
           move      rf-bit-arc-plf       to   w-tes-cli-plf (1)      .
           move      rf-bit-dpz-plf       to   w-tes-dpc-plf (1)      .
           move      rf-bit-inl-pgt       to   w-tes-inl-pgt (1)      .
           move      rf-bit-sgl-vpf       to   w-tes-sgl-vpf (1)      .
           move      rf-bit-dec-vpf       to   w-tes-dec-vpf (1)      .
           move      rf-bit-tdc-vpf       to   w-tes-tdc-vpf (1)      .
           move      rf-bit-ass-iva       to   w-tes-ass-iva (1)      .
           move      rf-bit-ctp-ven       to   w-tes-ctp-ven (1)      .
      *
           if        rf-bit-fat-sep       =    "S"
                     move  "S"            to   w-gen-fat-sep
           else if   rf-bit-fat-sep       =    "O"
                     move  "O"            to   w-gen-fat-sep
                     move  w-buf-rig-bol-pro
                                          to   w-gen-coc-prt          .
      *
           move      rf-bit-voc-des (1)   to   w-tes-voc-des (1, 1)   .
           move      rf-bit-voc-des (2)   to   w-tes-voc-des (1, 2)   .
           move      rf-bit-voc-des (3)   to   w-tes-voc-des (1, 3)   .
           move      rf-bit-voc-des (4)   to   w-tes-voc-des (1, 4)   .
           move      rf-bit-voc-des (5)   to   w-tes-voc-des (1, 5)   .
           move      rf-bit-voc-des (6)   to   w-tes-voc-des (1, 6)   .
           move      rf-bit-cod-lst       to   w-tes-cod-lst (1)      .
           move      rf-bit-csr-aac       to   w-tes-csr-aac (1)      .
           move      rf-bit-psr-aac (1)   to   w-tes-psr-aac (1, 1)   .
           move      rf-bit-psr-aac (2)   to   w-tes-psr-aac (1, 2)   .
           move      rf-bit-psr-aac (3)   to   w-tes-psr-aac (1, 3)   .
           move      rf-bit-psr-aac (4)   to   w-tes-psr-aac (1, 4)   .
           move      rf-bit-psr-aac (5)   to   w-tes-psr-aac (1, 5)   .
           move      rf-bit-csc-aac       to   w-tes-csc-aac (1)      .
           move      rf-bit-psc-aac       to   w-tes-psc-aac (1)      .
           move      rf-bit-cpv-aac       to   w-tes-cpv-aac (1)      .
           move      rf-bit-ppv-aac (1)   to   w-tes-ppv-aac (1, 1)   .
           move      rf-bit-ppv-aac (2)   to   w-tes-ppv-aac (1, 2)   .
           move      rf-bit-ppv-aac (3)   to   w-tes-ppv-aac (1, 3)   .
           move      rf-bit-cod-age       to   w-tes-cod-age (1)      .
           move      rf-bit-fsp-doc       to   w-tes-fsp-doc (1)      .
           move      rf-bit-pvf-age       to   w-tes-pvf-age (1)      .
           move      rf-bit-tip-vpa       to   w-tes-tip-vpa (1)      .
           move      rf-bit-cpv-aaa       to   w-tes-cpv-aaa (1)      .
           move      rf-bit-ppv-aaa (1)   to   w-tes-ppv-aaa (1, 1)   .
           move      rf-bit-ppv-aaa (2)   to   w-tes-ppv-aaa (1, 2)   .
           move      rf-bit-ppv-aaa (3)   to   w-tes-ppv-aaa (1, 3)   .
           move      rf-bit-cod-ime       to   w-tes-cod-ime (1)      .
           move      rf-bit-pvf-ime       to   w-tes-pvf-ime (1)      .
           move      rf-bit-cod-fop       to   w-tes-cod-fop (1)      .
           move      rf-bit-scp-aap       to   w-tes-scp-aap (1)      .
      *                      *-----------------------------------------*
      *                      * Subroutine di preparazione dei codici   *
      *                      * ABI e CAB e del conto corrente per      *
      *                      * l'appoggio in base alla personalizza-   *
      *                      * zione                                   *
      *                      *                                         *
      *                      * I valori interessati sono :             *
      *                      *  - 'cod-abi' = Codice Abi               *
      *                      *  - 'cod-cab' = Codice Cab               *
      *                      *  - 'ccc-app' = Conto corrente di appog- *
      *                      *                gio                      *
      *                      *-----------------------------------------*
           perform   buf-tes-bol-abi-000  thru buf-tes-bol-abi-999    .
           move      rf-bit-nos-ban       to   w-tes-nos-ban (1)      .
           move      rf-bit-nos-ccp       to   w-tes-nos-ccp (1)      .
           move      rf-bit-add-spi       to   w-tes-add-spi (1)      .
           move      rf-bit-add-spb       to   w-tes-add-spb (1)      .
           move      rf-bit-pag-dsm       to   w-tes-pag-dsm (1)      .
           move      rf-bit-pag-qaf       to   w-tes-pag-qaf (1)      .
           move      rf-bit-pag-act       to   w-tes-pag-act (1)      .
           move      rf-bit-tot-scc       to   w-pie-tot-scc (1)      .
           move      rf-bit-per-scc       to   w-pie-per-scc (1)      .
           move      rf-bit-tot-scp       to   w-pie-tot-scp (1)      .
           move      rf-bit-per-scp       to   w-pie-per-scp (1)      .
           move      rf-bit-tot-sia       to   w-pie-tot-sia (1)      .
           move      zero                 to   w-buf-tes-bol-ctr      .
       buf-tes-bol-310.
           add       1                    to   w-buf-tes-bol-ctr      .
           if        w-buf-tes-bol-ctr    >    6
                     go to buf-tes-bol-320.
           move      rf-bit-spe-snx
                    (w-buf-tes-bol-ctr)   to   w-pie-spe-snx
                                              (1, w-buf-tes-bol-ctr)  .
           move      rf-bit-spe-mad
                    (w-buf-tes-bol-ctr)   to   w-pie-spe-mad
                                              (1, w-buf-tes-bol-ctr)  .
           move      rf-bit-spe-per
                    (w-buf-tes-bol-ctr)   to   w-pie-spe-per
                                              (1, w-buf-tes-bol-ctr)  .
           move      rf-bit-spe-ibl
                    (w-buf-tes-bol-ctr)   to   w-pie-spe-ibl
                                              (1, w-buf-tes-bol-ctr)  .
           move      rf-bit-ibt-spe
                    (w-buf-tes-bol-ctr)   to   w-pie-spe-ibt
                                              (1, w-buf-tes-bol-ctr)  .
           move      rf-bit-spe-imp
                    (w-buf-tes-bol-ctr)   to   w-pie-spe-imp
                                              (1, w-buf-tes-bol-ctr)  .
           go to     buf-tes-bol-310.
       buf-tes-bol-320.
           go to     buf-tes-bol-400.
       buf-tes-bol-400.
      *              *-------------------------------------------------*
      *              * Eventuale aggiustamento assoggettamento iva     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di regime fiscale particolare          *
      *                  *---------------------------------------------*
           move      rf-bit-flg-rfp       to   w-tes-flg-rfp (1)      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      w-tes-ass-iva (1)    to   w-ags-ass-iva-aii      .
           move      rf-bit-arc-plf       to   w-ags-ass-iva-cli      .
           move      rf-bit-dat-doc       to   w-ags-ass-iva-dat      .
           perform   ags-ass-iva-000      thru ags-ass-iva-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        w-ags-ass-iva-flg    not  = spaces
                     go to buf-tes-bol-950.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
           move      w-ags-ass-iva-aio    to   w-tes-ass-iva (1)      .
       buf-tes-bol-500.
      *              *-------------------------------------------------*
      *              * Valori indiretti                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Anagrafica archivio                         *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli (1)    to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
           move      w-let-arc-cli-rag    to   w-tes-cod-cli-rag (1)  .
           move      w-let-arc-cli-via    to   w-tes-cod-cli-via (1)  .
           move      w-let-arc-cli-loc    to   w-tes-cod-cli-loc (1)  .
           move      w-let-arc-cli-piv    to   w-tes-cod-cli-piv (1)  .
           move      w-let-arc-cli-stc    to   w-tes-cod-cli-stc (1)  .
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc] principale             *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli (1)    to   w-let-arc-dcc-cli      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                      *-----------------------------------------*
      *                      * Test su esito lettura                   *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to buf-tes-bol-540.
      *                          *-------------------------------------*
      *                          * Composizione messaggio di errore    *
      *                          *-------------------------------------*
           move      "Manca l'anagrafica commerciale per il cliente     
      -              "               "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     buf-tes-bol-900.
       buf-tes-bol-540.
      *                  *---------------------------------------------*
      *                  * Subroutine per note su cliente              *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli (1)    to   w-cod-mne-dcc-cod      .
           move      08                   to   w-cod-mne-dcc-lin      .
           move      30                   to   w-cod-mne-dcc-pos      .
           perform   cod-mne-dcc-not-000  thru cod-mne-dcc-not-999    .
       buf-tes-bol-541.
      *                  *---------------------------------------------*
      *                  * Determinazione valori testata indotti da    *
      *                  * record [dcc] principale                     *
      *                  *---------------------------------------------*
           move      "C"                  to   w-det-vlt-dcc-tdt      .
           perform   det-vlt-dcc-000      thru det-vlt-dcc-999        .
      *                  *---------------------------------------------*
      *                  * Anagrafica dipendenza cliente               *
      *                  *---------------------------------------------*
           move      w-tes-cod-cli (1)    to   w-let-arc-dcc-cli      .
           move      w-tes-dpz-cli (1)    to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           move      w-let-arc-dcc-rag    to   w-tes-dpz-cli-rag (1)  .
           move      w-let-arc-dcc-via    to   w-tes-dpz-cli-via (1)  .
           move      w-let-arc-dcc-loc    to   w-tes-dpz-cli-loc (1)  .
      *                  *---------------------------------------------*
      *                  * Determinazione valori testata indotti da    *
      *                  * record [dcc] relativo alla dipendenza del   *
      *                  * cliente                                     *
      *                  *---------------------------------------------*
           if        w-let-arc-dcc-flg    not  = spaces
                     go to buf-tes-bol-542.
           move      "C"                  to   w-det-vlt-dcd-tdt      .
           move      w-tes-dpz-cli (1)    to   w-det-vlt-dcd-dpz      .
           perform   det-vlt-dcd-000      thru det-vlt-dcd-999        .
       buf-tes-bol-542.
      *                  *---------------------------------------------*
      *                  * Archivio per la fatturazione                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se fornitura diretta : oltre            *
      *                      *-----------------------------------------*
           if        w-tes-tip-frn (1)    =    01
                     go to buf-tes-bol-546.
      *                      *-----------------------------------------*
      *                      * Anagrafica cliente per fatturazione     *
      *                      *-----------------------------------------*
           move      w-tes-cli-plf (1)    to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
           move      w-let-arc-cli-rag    to   w-tes-cli-plf-rag (1)  .
           move      w-let-arc-cli-via    to   w-tes-cli-plf-via (1)  .
           move      w-let-arc-cli-loc    to   w-tes-cli-plf-loc (1)  .
           move      w-let-arc-cli-stc    to   w-tes-cod-cli-stc (1)  .
      *                      *-----------------------------------------*
      *                      * Anagrafica commerciale principale cli-  *
      *                      * ente per fatturazione                   *
      *                      *-----------------------------------------*
           move      w-tes-cli-plf (1)    to   w-let-arc-dcc-cli      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                          *-------------------------------------*
      *                          * Se lettura OK : oltre               *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to buf-tes-bol-544.
      *                          *-------------------------------------*
      *                          * Composizione messaggio di errore    *
      *                          *-------------------------------------*
           move      "Manca l'anagrafica commerciale del cliente per fat
      -              "turazione !    "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     buf-tes-bol-900.
       buf-tes-bol-544.
      *                      *-----------------------------------------*
      *                      * Determinazione valori testata indotti   *
      *                      * da record [dcc] principale              *
      *                      *-----------------------------------------*
           move      "F"                  to   w-det-vlt-dcc-tdt      .
           perform   det-vlt-dcc-000      thru det-vlt-dcc-999        .
      *                      *-----------------------------------------*
      *                      * Anagrafica commerciale dipendenza cli-  *
      *                      * ente per fatturazione                   *
      *                      *-----------------------------------------*
           move      w-tes-cli-plf (1)    to   w-let-arc-dcc-cli      .
           move      w-tes-dpc-plf (1)    to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           move      w-let-arc-dcc-rag    to   w-tes-dpc-plf-rag (1)  .
           move      w-let-arc-dcc-via    to   w-tes-dpc-plf-via (1)  .
           move      w-let-arc-dcc-loc    to   w-tes-dpc-plf-loc (1)  .
      *                      *-----------------------------------------*
      *                      * Determinazione valori testata indotti   *
      *                      * da record [dcc] relativo alla dipenza   *
      *                      * del cliente per fatturazione            *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-flg    not  = spaces
                     go to buf-tes-bol-546.
           move      "C"                  to   w-det-vlt-dcd-tdt      .
           move      w-tes-dpc-plf (1)    to   w-det-vlt-dcd-dpz      .
           perform   det-vlt-dcd-000      thru det-vlt-dcd-999        .
       buf-tes-bol-546.
      *                      *-----------------------------------------*
      *                      * Test su codice sottoconto associato al  *
      *                      * cliente                                 *
      *                      *-----------------------------------------*
           if        w-tes-cod-cli-stc (1)
                                          not  = zero
                     go to buf-tes-bol-548.
      *                          *-------------------------------------*
      *                          * Composizione messaggio di errore    *
      *                          *-------------------------------------*
           move      "Manca il sottoconto contabile per il cliente !    
      -              "               "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     buf-tes-bol-900.
       buf-tes-bol-548.
      *                  *---------------------------------------------*
      *                  * Anagrafica valuta                           *
      *                  *---------------------------------------------*
           move      w-tes-sgl-vpf (1)    to   w-let-arc-zvl-cod      .
           perform   let-arc-zvl-000      thru let-arc-zvl-999        .
           move      w-let-arc-zvl-des    to   w-tes-sgl-vpf-des (1)  .
      *                  *---------------------------------------------*
      *                  * Coefficiente di cambio valuta per fattura-  *
      *                  * zione alla data del documento               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione                          *
      *                      *-----------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      w-tes-sgl-vpf (1)    to   w-coe-cmb-vlt-sdv      .
           move      w-tes-tdc-vpf (1)    to   w-coe-cmb-vlt-tdc      .
           move      w-tes-dat-doc        to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                      *-----------------------------------------*
      *                      * Test su esito determinazione            *
      *                      *-----------------------------------------*
           if        w-coe-cmb-vlt-ope    =    "CC"
                     go to buf-tes-bol-549.
      *                          *-------------------------------------*
      *                          * Composizione messaggio di errore    *
      *                          *-------------------------------------*
           move      "Coefficiente di cambio indeterminato !            
      -              "               "    to   w-err-box-err-msg      .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     buf-tes-bol-900.
       buf-tes-bol-549.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione coefficiente di cambio  *
      *                      *-----------------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-tes-cdc-vpf (1)      .
       buf-tes-bol-550.
      *                  *---------------------------------------------*
      *                  * Tabella codici iva per descrizione assog-   *
      *                  * gettamento                                  *
      *                  *---------------------------------------------*
           if        w-tes-flg-rfp (1)    =    "S"
                     move      "S"        to   w-tes-ass-iva-spl (1)
                     move      "Scissione Pagamenti"
                                          to   w-tes-ass-iva-des (1)
                     go to buf-tes-bol-555.
      *
           move      w-tes-ass-iva (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
           move      w-let-arc-zci-des    to   w-tes-ass-iva-des (1)  .
           if        w-tes-ass-iva (1)    =    zero
                     move  "Soggetto ad iva"
                                          to   w-tes-ass-iva-des (1)  .
       buf-tes-bol-555.
      *                  *---------------------------------------------*
      *                  * Anagrafica piano dei conti                  *
      *                  *---------------------------------------------*
           move      w-tes-ctp-ven (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-tes-ctp-ven-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica voci descrittive                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-tes-bol-ctr      .
       buf-tes-bol-560.
           add       1                    to   w-buf-tes-bol-ctr      .
           if        w-buf-tes-bol-ctr    >    6
                     go to buf-tes-bol-570.
           move      w-buf-tes-bol-ctr    to   w-let-arc-zvf-num      .
           move      w-tes-voc-des
                    (1, w-buf-tes-bol-ctr)
                                          to   w-let-arc-zvf-cod      .
           perform   let-arc-zvf-000      thru let-arc-zvf-999        .
           move      w-let-arc-zvf-des    to   w-tes-voc-des-des
                                              (1, w-buf-tes-bol-ctr)  .
           go to     buf-tes-bol-560.
       buf-tes-bol-570.
      *                  *---------------------------------------------*
      *                  * Anagrafica listini                          *
      *                  *---------------------------------------------*
           move      w-tes-cod-lst (1)    to   w-let-arc-zls-cod      .
           perform   let-arc-zls-000      thru let-arc-zls-999        .
           move      w-let-arc-zls-des    to   w-tes-cod-lst-des (1)  .
           move      w-let-arc-zls-drg    to   w-tes-cod-lst-drg (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica categoria di sconto in riga      *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-zcs-tip      .
           move      w-tes-csr-aac (1)    to   w-let-arc-zcs-cod      .
           perform   let-arc-zcs-000      thru let-arc-zcs-999        .
           move      w-let-arc-zcs-des    to   w-tes-csr-aac-des (1)  .
           move      w-let-arc-zcs-per (1)
                                          to   w-tes-psr-acc (1, 1)   .
           move      w-let-arc-zcs-per (2)
                                          to   w-tes-psr-acc (1, 2)   .
           move      w-let-arc-zcs-per (3)
                                          to   w-tes-psr-acc (1, 3)   .
           move      w-let-arc-zcs-per (4)
                                          to   w-tes-psr-acc (1, 4)   .
           move      w-let-arc-zcs-per (5)
                                          to   w-tes-psr-acc (1, 5)   .
      *                  *---------------------------------------------*
      *                  * Anagrafica categoria di sconto in chiusura  *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-zcs-tip      .
           move      w-tes-csc-aac (1)    to   w-let-arc-zcs-cod      .
           perform   let-arc-zcs-000      thru let-arc-zcs-999        .
           move      w-let-arc-zcs-des    to   w-tes-csc-aac-des (1)  .
           move      w-let-arc-zcs-per (1)
                                          to   w-tes-psc-acc (1)      .
      *                  *---------------------------------------------*
      *                  * Anagrafica agenti                           *
      *                  *---------------------------------------------*
           move      w-tes-cod-age (1)    to   w-let-arc-age-cod      .
           perform   let-arc-age-000      thru let-arc-age-999        .
           move      w-let-arc-age-rag    to   w-tes-cod-age-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica forme di pagamento               *
      *                  *---------------------------------------------*
           move      w-tes-cod-fop (1)    to   w-let-arc-zfp-cod      .
           perform   let-arc-zfp-000      thru let-arc-zfp-999        .
           move      w-let-arc-zfp-des    to   w-tes-cod-fop-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica nostra banca per bonifico        *
      *                  *---------------------------------------------*
           move      02                   to   w-let-arc-cbp-tip      .
           move      w-tes-nos-ban (1)    to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
           move      w-let-arc-cbp-des    to   w-tes-nos-ban-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica nostro c/c postale per pagamento *
      *                  *---------------------------------------------*
           move      03                   to   w-let-arc-cbp-tip      .
           move      w-tes-nos-ccp (1)    to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
           move      w-let-arc-cbp-des    to   w-tes-nos-ccp-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica istituti di credito              *
      *                  *---------------------------------------------*
           move      w-tes-cod-abi (1)    to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           move      w-let-arc-axi-den    to   w-tes-cod-abi-den (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica sportelli istituti di credito    *
      *                  *---------------------------------------------*
           move      w-tes-cod-abi (1)    to   w-let-arc-axs-abi      .
           move      w-tes-cod-cab (1)    to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           move      w-let-arc-axs-den    to   w-tes-cod-cab-den (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica categoria spese incasso          *
      *                  *---------------------------------------------*
           move      w-tes-add-spi (1)    to   w-let-arc-zin-cod      .
           move      00                   to   w-let-arc-zin-tpg      .
           perform   let-arc-zin-000      thru let-arc-zin-999        .
           move      w-let-arc-zin-des    to   w-tes-add-spi-des (1)  .
      *                  *---------------------------------------------*
      *                  * Anagrafica categoria spese bollo            *
      *                  *---------------------------------------------*
           move      w-tes-add-spb (1)    to   w-let-arc-zbo-cod      .
           move      00                   to   w-let-arc-zbo-tpg      .
           perform   let-arc-zbo-000      thru let-arc-zbo-999        .
           move      w-let-arc-zbo-des    to   w-tes-add-spb-des (1)  .
       buf-tes-bol-600.
      *                  *---------------------------------------------*
      *                  * Codici iva e contropartite spese in fattura *
      *                  *---------------------------------------------*
           move      zero                 to   w-buf-tes-bol-ctr      .
       buf-tes-bol-610.
           add       1                    to   w-buf-tes-bol-ctr      .
           if        w-buf-tes-bol-ctr    >    6
                     go to buf-tes-bol-620.
      *                      *-----------------------------------------*
      *                      * Normalizzazione record [zsf]            *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *                      *-----------------------------------------*
      *                      * Lettura record [zsf]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPF    "         to   f-key                  .
           move      w-buf-tes-bol-ctr    to   rf-zsf-num-spf         .
           move      "I  "                to   rf-zsf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valori                  *
      *                      *-----------------------------------------*
           move      rf-zsf-civ-spe       to   w-pie-spe-civ
                                              (1, w-buf-tes-bol-ctr)  .
           move      rf-zsf-ccp-spe       to   w-pie-spe-ccp
                                              (1, w-buf-tes-bol-ctr)  .
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     buf-tes-bol-610.
       buf-tes-bol-620.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-tes-bol-999.
       buf-tes-bol-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Visualizzazione messaggio di errore         *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
       buf-tes-bol-950.
      *                  *---------------------------------------------*
      *                  * Uscita con status ad errore                 *
      *                  *---------------------------------------------*
           move      "#"                  to   l-fat-dfm-exi-sts      .
       buf-tes-bol-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione testata prima bolla cliente richiamata    *
      *    *                                                           *
      *    * Subroutine per la modalita' di utilizzo dei codici ABI e  *
      *    * CAB come definito dall'apposita personalizzazione         *
      *    *-----------------------------------------------------------*
       buf-tes-bol-abi-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del valore della perso-  *
      *              * nalizzazione                                    *
      *              *-------------------------------------------------*
           if        w-prs-dcc-abi        =    01
                     go to buf-tes-bol-abi-100
           else      go to buf-tes-bol-abi-200.
       buf-tes-bol-abi-100.
      *              *-------------------------------------------------*
      *              * Se utilizzo dei codici ABI - CAB provenienti    *
      *              * dall'anagrafica commerciale cliente             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Lettura                                 *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      rf-bit-cod-arc       to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Codice istituto di credito per l'appoggio   *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-abi       to   w-tes-cod-abi (1)      .
      *                  *---------------------------------------------*
      *                  * Codice agenzia istituto di credito per      *
      *                  * l'appoggio                                  *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-cab       to   w-tes-cod-cab (1)      .
      *                  *---------------------------------------------*
      *                  * Codice conto corrente per l'appoggio        *
      *                  *---------------------------------------------*
           move      rf-dcc-ccc-app       to   w-tes-ccc-app (1)      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     buf-tes-bol-abi-900.
       buf-tes-bol-abi-200.
      *              *-------------------------------------------------*
      *              * Se utilizzo dei codici ABI - CAB provenienti    *
      *              * dal documento                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice istituto di credito per l'appoggio   *
      *                  *---------------------------------------------*
           move      rf-bit-cod-abi       to   w-tes-cod-abi (1)      .
      *                  *---------------------------------------------*
      *                  * Codice agenzia istituto di credito per      *
      *                  * l'appoggio                                  *
      *                  *---------------------------------------------*
           move      rf-bit-cod-cab       to   w-tes-cod-cab (1)      .
      *                  *---------------------------------------------*
      *                  * Codice conto corrente per l'appoggio        *
      *                  *---------------------------------------------*
           move      rf-bit-ccc-app       to   w-tes-ccc-app (1)      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     buf-tes-bol-abi-900.
       buf-tes-bol-abi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-tes-bol-abi-999.
       buf-tes-bol-abi-999.
           exit.

      *    *===========================================================*
      *    * Eventuale aggiustamento assoggettamento iva del documento *
      *    * in base ai dati anagrafici del cliente                    *
      *    *-----------------------------------------------------------*
       ags-ass-iva-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-ags-ass-iva-flg      .
      *              *-------------------------------------------------*
      *              * Inizializzazione valore in uscita               *
      *              *-------------------------------------------------*
           move      w-ags-ass-iva-aii    to   w-ags-ass-iva-aio      .
      *              *-------------------------------------------------*
      *              * Test se da eseguire                             *
      *              *-------------------------------------------------*
           if        w-prs-trt-aic        not  = 01
                     go to ags-ass-iva-999.
      *              *-------------------------------------------------*
      *              * Lettura record [cli]                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione                             *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-ags-ass-iva-cli    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * Lettura record [dcc]                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione                             *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-ags-ass-iva-cli    to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
       ags-ass-iva-100.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo assogget-   *
      *                  * tamento iva del cliente                     *
      *                  *---------------------------------------------*
           if        rf-dcc-tas-ivc       =    01
                     go to ags-ass-iva-120
           else if   rf-dcc-tas-ivc       =    02
                     go to ags-ass-iva-140
           else if   rf-dcc-tas-ivc       =    03
                     go to ags-ass-iva-160
           else if   rf-dcc-tas-ivc       =    04
                     go to ags-ass-iva-180
           else      go to ags-ass-iva-900.
       ags-ass-iva-120.
      *                  *---------------------------------------------*
      *                  * Tipo assoggettamento iva cliente : sempre   *
      *                  * soggetto ad iva                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Forzatura a zero                        *
      *                      *-----------------------------------------*
           move      zero                 to   w-ags-ass-iva-aio      .
      *                      *-----------------------------------------*
      *                      * A uscita                                *
      *                      *-----------------------------------------*
           go to     ags-ass-iva-800.
       ags-ass-iva-140.
      *                  *---------------------------------------------*
      *                  * Tipo assoggettamento iva cliente : sempre   *
      *                  * esente da iva                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Controllo che il codice iva in anagra-  *
      *                      * fica cliente sia un titolo di esenzione *
      *                      *-----------------------------------------*
           move      rf-cli-cod-iva       to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    >    2 and
                     w-edt-iva-cod-003    <    7
                     go to ags-ass-iva-142.
      *                          *-------------------------------------*
      *                          * Composizione messaggio              *
      *                          *-------------------------------------*
           move      "Assoggettamento iva errato in anagrafica contabile
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     ags-ass-iva-900.
       ags-ass-iva-142.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione assoggettamento iva     *
      *                      *-----------------------------------------*
           move      rf-cli-cod-iva       to   w-ags-ass-iva-aio      .
      *                      *-----------------------------------------*
      *                      * A uscita                                *
      *                      *-----------------------------------------*
           go to     ags-ass-iva-800.
       ags-ass-iva-160.
      *                  *---------------------------------------------*
      *                  * Tipo assoggettamento iva cliente : con pla- *
      *                  * fond                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su personalizzazione relativa alla *
      *                      * gestione lettera d'intenti              *
      *                      *-----------------------------------------*
           if        w-prs-snx-lic        not  = "S"
                     go to ags-ass-iva-162.
      *                      *-----------------------------------------*
      *                      * Normalizzazione record [lic]            *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioflic"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
      *                      *-----------------------------------------*
      *                      * Lettura record [lic]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-ags-ass-iva-cli    to   rf-lic-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/ioflic"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
      *                      *-----------------------------------------*
      *                      * Se data documento non compresa tra data *
      *                      * riferimento iniziale e finale, si pro-  *
      *                      * cede come per cliente sempre soggetto   *
      *                      *-----------------------------------------*
           if        w-ags-ass-iva-dat    not  < rf-lic-drf-ini and
                     w-ags-ass-iva-dat    not  > rf-lic-drf-fin
                     go to ags-ass-iva-162.
      *                          *-------------------------------------*
      *                          * Emissione messaggio                 *
      *                          *-------------------------------------*
           move      "Il documento non ricade entro gli estremi della le
      -              "ttera d'intenti"    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * Si procede come per cliente sempre  *
      *                          * soggetto                            *
      *                          *-------------------------------------*
           go to     ags-ass-iva-120.
       ags-ass-iva-162.
      *                      *-----------------------------------------*
      *                      * Controllo che il codice iva in anagra-  *
      *                      * fica cliente sia un titolo di esenzione *
      *                      *-----------------------------------------*
           move      rf-cli-cod-iva       to   w-edt-iva-cod          .
           if        w-edt-iva-cod-003    >    2 and
                     w-edt-iva-cod-003    <    7
                     go to ags-ass-iva-164.
      *                          *-------------------------------------*
      *                          * Composizione messaggio              *
      *                          *-------------------------------------*
           move      "Assoggettamento iva errato in anagrafica contabile
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A trattamento errore                *
      *                          *-------------------------------------*
           go to     ags-ass-iva-900.
       ags-ass-iva-164.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione assoggettamento iva     *
      *                      *-----------------------------------------*
           move      rf-cli-cod-iva       to   w-ags-ass-iva-aio      .
      *                      *-----------------------------------------*
      *                      * A uscita                                *
      *                      *-----------------------------------------*
           go to     ags-ass-iva-800.
       ags-ass-iva-180.
      *                  *---------------------------------------------*
      *                  * Tipo assoggettamento iva cliente : in regi- *
      *                  * me di scissione pagamenti                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Assoggettamento Iva                     *
      *                      *-----------------------------------------*
           move      00000                to   w-ags-ass-iva-aio      .
      *                      *-----------------------------------------*
      *                      * Flag di regime fiscale particolare      *
      *                      *-----------------------------------------*
           move      "S"                  to   w-tes-flg-rfp (1)      .
      *                      *-----------------------------------------*
      *                      * A uscita                                *
      *                      *-----------------------------------------*
           go to     ags-ass-iva-800.
       ags-ass-iva-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ags-ass-iva-999.
       ags-ass-iva-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-ags-ass-iva-flg      .
       ags-ass-iva-999.
           exit.

      *    *===========================================================*
      *    * Confronto tra dati testata bolla cliente in esame con     *
      *    * dati testata del documento                                *
      *    *-----------------------------------------------------------*
       cnf-tes-doc-000.
      *              *-------------------------------------------------*
      *              * Valori bloccanti                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori comunque bloccanti                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           if        rf-bit-cod-arc       =    w-tes-cod-cli (1)
                     go to cnf-tes-doc-002.
           move      "Codice cliente diverso da quello impostato !      
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-002.
      *                      *-----------------------------------------*
      *                      * Codice lingua                           *
      *                      *-----------------------------------------*
           if        rf-bit-cod-lng       =    w-tes-cod-lng (1)
                     go to cnf-tes-doc-010.
           move      "Codice lingua diverso da quello impostato !       
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-010.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda che il tipo movimento  *
      *                  * per la bolla interessi o meno la fattura-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmb-mif    =    01
                     go to cnf-tes-doc-020
           else      go to cnf-tes-doc-040.
       cnf-tes-doc-020.
      *                  *---------------------------------------------*
      *                  * Se non interessa la fatturazione            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Nessun valore bloccante                 *
      *                      *-----------------------------------------*
           go to     cnf-tes-doc-100.
       cnf-tes-doc-040.
      *                  *---------------------------------------------*
      *                  * Se bolla fatturabile                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo fornitura                  *
      *                      *-----------------------------------------*
           if        rf-bit-tip-frn       =    w-tes-tip-frn (1)
                     go to cnf-tes-doc-041.
           move      "Tipo fornitura diverso da quello impostato !      
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-041.
      *                      *-----------------------------------------*
      *                      * Test su cliente per fatturazione        *
      *                      *-----------------------------------------*
           if        rf-bit-arc-plf       =    w-tes-cli-plf (1)
                     go to cnf-tes-doc-042.
           move      "Cliente per fatturazione diverso da quello imposta
      -              "to !           "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-042.
      *                      *-----------------------------------------*
      *                      * Test su tipo fatturazione per il gruppo *
      *                      * d'acquisto                              *
      *                      *-----------------------------------------*
           if        rf-bit-tip-frn       not  = 21
                     go to cnf-tes-doc-043.
           if        rf-bit-tip-ftz       not  = 02
                     go to cnf-tes-doc-043.
           move      "La fatturazione cumulativa per gli associati in gr
      -              "uppo d'acquisto"    to   w-err-box-err-msg      .
           move      "non e' gestibile con la fatturazione differita man
      -              "uale !         "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
           go to     cnf-tes-doc-950.
       cnf-tes-doc-043.
      *                      *-----------------------------------------*
      *                      * Aggiustamento assoggettamento iva del   *
      *                      * cliente                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      rf-bit-ass-iva       to   w-ags-ass-iva-aii      .
           move      rf-bit-arc-plf       to   w-ags-ass-iva-cli      .
           move      rf-bit-dat-doc       to   w-ags-ass-iva-dat      .
           perform   ags-ass-iva-000      thru ags-ass-iva-999        .
      *                          *-------------------------------------*
      *                          * Test su esito operazione            *
      *                          *-------------------------------------*
           if        w-ags-ass-iva-flg    not  = spaces
                     go to cnf-tes-doc-950.
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      w-ags-ass-iva-aio    to   rf-bit-ass-iva         .
      *                      *-----------------------------------------*
      *                      * Test su assoggettamento iva             *
      *                      *-----------------------------------------*
           if        rf-bit-ass-iva       =    w-tes-ass-iva (1)
                     go to cnf-tes-doc-044.
           move      "Assoggettamento iva diverso da quello impostato ! 
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-044.
      *                      *-----------------------------------------*
      *                      * Test su contropartita vendite           *
      *                      *-----------------------------------------*
           if        rf-bit-ctp-ven       =    w-tes-ctp-ven (1)
                     go to cnf-tes-doc-046.
           move      "Contropartite vendite diverso da quello impostato 
      -              "!              "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-046.
      *                      *-----------------------------------------*
      *                      * Test su inoltro pagamenti               *
      *                      *-----------------------------------------*
           if        rf-bit-inl-pgt       =    w-tes-inl-pgt (1)
                     go to cnf-tes-doc-048.
           move      "Inoltro pagamenti diverso da quello impostato !   
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-048.
      *                      *-----------------------------------------*
      *                      * Test su sigla valuta                    *
      *                      *-----------------------------------------*
           if        rf-bit-sgl-vpf       =    w-tes-sgl-vpf (1)
                     go to cnf-tes-doc-050.
           move      "Sigla valuta diversa da quella impostata !        
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-050.
      *                      *-----------------------------------------*
      *                      * Test su decimali valuta                 *
      *                      *-----------------------------------------*
           if        rf-bit-dec-vpf       =    w-tes-dec-vpf (1)
                     go to cnf-tes-doc-051.
           move      "Decimali valuta diverso da quelli della valuta imp
      -              "ostata !       "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-051.
      *                      *-----------------------------------------*
      *                      * Test su tipo di cambio valuta           *
      *                      *-----------------------------------------*
           if        rf-bit-tdc-vpf       =    w-tes-tdc-vpf (1)
                     go to cnf-tes-doc-052.
           move      "Tipo di cambio valuta diverso da quello della valu
      -              "ta impostata ! "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-052.
      *                      *-----------------------------------------*
      *                      * Test su data scadenza pagamento manuale *
      *                      *-----------------------------------------*
           if        rf-bit-pag-dsm       =    w-tes-pag-dsm (1)
                     go to cnf-tes-doc-054.
           move      "Data scadenza pagamento manuale diversa da quella 
      -              "impostata !    "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-054.
      *                      *-----------------------------------------*
      *                      * Test su quota a forfait pagamento       *
      *                      *-----------------------------------------*
           if        rf-bit-pag-qaf       =    w-tes-pag-qaf (1)
                     go to cnf-tes-doc-056.
           move      "Quota a forfait del pagamento diversa da quella im
      -              "postata !      "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-056.
      *                      *-----------------------------------------*
      *                      * Test su codice agente                   *
      *                      *-----------------------------------------*
           if        rf-bit-cod-age       =    w-tes-cod-age (1)
                     go to cnf-tes-doc-058.
           move      "Codice agente diverso da quello impostato !       
      -              "               "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-058.
      *                      *-----------------------------------------*
      *                      * Test su trattamento provvigioni         *
      *                      *-----------------------------------------*
           if        rf-bit-fsp-doc       =    w-tes-fsp-doc (1)
                     go to cnf-tes-doc-060.
           move      "Trattamento provvigionale diverso da quello impost
      -              "ato !          "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-060.
      *                      *-----------------------------------------*
      *                      * Test su tipo vendita per l'agente       *
      *                      *-----------------------------------------*
           if        rf-bit-tip-vpa       =    w-tes-tip-vpa (1)
                     go to cnf-tes-doc-062.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Tipo vendita per l'agente diversa da quella impost
      -              "ata !          "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-062.
      *                      *-----------------------------------------*
      *                      * Test su fatturazione separata per il    *
      *                      * documento                               *
      *                      *-----------------------------------------*
           if        rf-bit-fat-sep       not  = "S"
                     go to cnf-tes-doc-064.
           move      "La bolla richiamata prevede che debba essere fattu
      -              "rata a parte ! "    to   w-err-box-err-msg      .
           go to     cnf-tes-doc-900.
       cnf-tes-doc-064.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     cnf-tes-doc-100.
       cnf-tes-doc-100.
      *              *-------------------------------------------------*
      *              * Valori non-bloccanti                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori comunque non bloccanti               *
      *                  *---------------------------------------------*
           if        rf-bit-dpz-arc       =    w-tes-dpz-cli (1)
                     go to cnf-tes-doc-120.
      *                      *-----------------------------------------*
      *                      * Messaggio all'operatore                 *
      *                      *-----------------------------------------*
           move      "Dipendenza cliente diversa da quella impostata !  
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-120.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda che il tipo movimento  *
      *                  * per la bolla interessi o meno la fattura-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmb-mif    =    01
                     go to cnf-tes-doc-140
           else      go to cnf-tes-doc-160.
       cnf-tes-doc-140.
      *                  *---------------------------------------------*
      *                  * Se non interessa la fatturazione            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Nessun valore non-bloccante             *
      *                      *-----------------------------------------*
           go to     cnf-tes-doc-500.
       cnf-tes-doc-160.
      *                  *---------------------------------------------*
      *                  * Se bolla fatturabile                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su dipendenza archivio per fattu-  *
      *                      * razione                                 *
      *                      *-----------------------------------------*
           if        rf-bit-dpz-plf       =    w-tes-dpc-plf (1)
                     go to cnf-tes-doc-162.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Dipendenza cliente per fatturazione diversa da que
      -              "lla impostata !"    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-162.
      *                      *-----------------------------------------*
      *                      * Test su forma di pagamento              *
      *                      *-----------------------------------------*
           if        rf-bit-cod-fop       =    w-tes-cod-fop (1)
                     go to cnf-tes-doc-164.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Forma di pagamento diversa da quella impostata !  
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-164.
      *                      *-----------------------------------------*
      *                      * Test su codice ABI d'appoggio           *
      *                      *-----------------------------------------*
           if        w-tes-cod-abi (1)    =    zero
                     go to cnf-tes-doc-166.
           if        rf-bit-cod-abi       =    w-tes-cod-abi (1)
                     go to cnf-tes-doc-166.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Codice ABI per l'appoggio diverso da quello impost
      -              "ato !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-166.
      *                      *-----------------------------------------*
      *                      * Test su codice CAB d'appoggio           *
      *                      *-----------------------------------------*
           if        w-tes-cod-cab (1)    =    zero
                     go to cnf-tes-doc-168.
           if        rf-bit-cod-cab       =    w-tes-cod-cab (1)
                     go to cnf-tes-doc-168.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Codice CAB per l'appoggio diverso da quello impost
      -              "ato !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-168.
      *                      *-----------------------------------------*
      *                      * Test su c/c d'appoggio                  *
      *                      *-----------------------------------------*
           if        w-tes-ccc-app (1)    =    spaces
                     go to cnf-tes-doc-170.
           if        rf-bit-ccc-app       =    w-tes-ccc-app (1)
                     go to cnf-tes-doc-170.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "C/C del cliente per l'appoggio diverso da quello i
      -              "mpostato !     "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-170.
      *                      *-----------------------------------------*
      *                      * Test su nostra banca per bonifico       *
      *                      *-----------------------------------------*
           if        w-tes-nos-ban (1)    =    spaces
                     go to cnf-tes-doc-172.
           if        rf-bit-nos-ban       =    w-tes-nos-ban (1)
                     go to cnf-tes-doc-172.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Nostra banca per appoggio bonifico diversa da quel
      -              "la impostata ! "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-172.
      *                      *-----------------------------------------*
      *                      * Test su nostro c/c postale per bonifico *
      *                      *-----------------------------------------*
           if        w-tes-nos-ccp (1)    =    spaces
                     go to cnf-tes-doc-174.
           if        rf-bit-nos-ccp       =    w-tes-nos-ccp (1)
                     go to cnf-tes-doc-174.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Nostro c/c postale diverso da quello impostato !  
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-174.
      *                      *-----------------------------------------*
      *                      * Test su forfait provvigioni             *
      *                      *-----------------------------------------*
           if        w-tes-pvf-age (1)    =    zero
                     go to cnf-tes-doc-176.
           if        rf-bit-pvf-age       =    w-tes-pvf-age (1)
                     go to cnf-tes-doc-176.
      *                          *-------------------------------------*
      *                          * Messaggio all'operatore             *
      *                          *-------------------------------------*
           move      "Provvigione a forfait diversa da quella impostata 
      -              "!              "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-176.
      *                      *-----------------------------------------*
      *                      * Test su spese per fatturazione          *
      *                      *-----------------------------------------*
           if        rf-bit-spe-snx (1)   =    w-pie-spe-snx (1, 1) and
                     rf-bit-spe-snx (2)   =    w-pie-spe-snx (1, 2) and
                     rf-bit-spe-snx (3)   =    w-pie-spe-snx (1, 3) and
                     rf-bit-spe-snx (4)   =    w-pie-spe-snx (1, 4) and
                     rf-bit-spe-snx (5)   =    w-pie-spe-snx (1, 5) and
                     rf-bit-spe-snx (6)   =    w-pie-spe-snx (1, 6)
                     go to cnf-tes-doc-200.
           move      "La bolla richiamata presenta modalita' di addebito
      -              " delle spese di"    to   w-err-box-err-msg      .
           move      "chiusura diverse da quelle del primo documento ric
      -              "hiamato !      "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
       cnf-tes-doc-200.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     cnf-tes-doc-500.
       cnf-tes-doc-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento progressivi per testata documento *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Acconto pagamento per il documento          *
      *                  *---------------------------------------------*
           add       rf-bit-pag-act       to   w-tes-pag-act (1)      .
      *                  *---------------------------------------------*
      *                  * Provvigioni a forfait per il documento      *
      *                  *---------------------------------------------*
           if        rf-bit-fsp-doc       =    03
                     add  rf-bit-pvf-age  to   w-tes-pvf-age (1)      .
       cnf-tes-doc-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento progressivi per piede documento   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Totale sconto in chiusura                   *
      *                  *---------------------------------------------*
           add       rf-bit-tot-scc       to   w-pie-tot-scc (1)      .
      *                  *---------------------------------------------*
      *                  * Percentuale sconto in chiusura              *
      *                  *---------------------------------------------*
           if        rf-bit-per-scc       not  = w-pie-per-scc (1)
                     move  zero           to   w-pie-per-scc (1)      .
      *                  *---------------------------------------------*
      *                  * Totale sconto pagamento                     *
      *                  *---------------------------------------------*
           add       rf-bit-tot-scp       to   w-pie-tot-scp (1)      .
      *                  *---------------------------------------------*
      *                  * Percentuale sconto in chiusura              *
      *                  *---------------------------------------------*
           if        rf-bit-per-scp       not  = w-pie-per-scp (1)
                     move  zero           to   w-pie-per-scp (1)      .
      *                  *---------------------------------------------*
      *                  * Totale spese in fattura                     *
      *                  *---------------------------------------------*
           add       rf-bit-spe-imp (1)   to   w-pie-spe-imp (1, 1)   .
           add       rf-bit-spe-imp (2)   to   w-pie-spe-imp (1, 2)   .
           add       rf-bit-spe-imp (3)   to   w-pie-spe-imp (1, 3)   .
           add       rf-bit-spe-imp (4)   to   w-pie-spe-imp (1, 4)   .
           add       rf-bit-spe-imp (5)   to   w-pie-spe-imp (1, 5)   .
           add       rf-bit-spe-imp (6)   to   w-pie-spe-imp (1, 6)   .
      *                  *---------------------------------------------*
      *                  * Percentuali spese in fattura                *
      *                  *---------------------------------------------*
           if        rf-bit-spe-snx (1)   =    0 or
                     rf-bit-spe-per (1)   not  = w-pie-spe-per (1, 1)
                     move  zero           to   w-pie-spe-per (1, 1)   .
           if        rf-bit-spe-snx (2)   =    0 or
                     rf-bit-spe-per (2)   not  = w-pie-spe-per (1, 2)
                     move  zero           to   w-pie-spe-per (1, 2)   .
           if        rf-bit-spe-snx (3)   =    0 or
                     rf-bit-spe-per (3)   not  = w-pie-spe-per (1, 3)
                     move  zero           to   w-pie-spe-per (1, 3)   .
           if        rf-bit-spe-snx (4)   =    0 or
                     rf-bit-spe-per (4)   not  = w-pie-spe-per (1, 4)
                     move  zero           to   w-pie-spe-per (1, 4)   .
           if        rf-bit-spe-snx (5)   =    0 or
                     rf-bit-spe-per (5)   not  = w-pie-spe-per (1, 5)
                     move  zero           to   w-pie-spe-per (1, 5)   .
           if        rf-bit-spe-snx (6)   =    0 or
                     rf-bit-spe-per (6)   not  = w-pie-spe-per (1, 6)
                     move  zero           to   w-pie-spe-per (1, 6)   .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     cnf-tes-doc-999.
       cnf-tes-doc-900.
      *              *-------------------------------------------------*
      *              * Trattamento errore                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Emissione messaggio di errore               *
      *                  *---------------------------------------------*
           perform   box-msg-err-000      thru box-msg-err-999        .
       cnf-tes-doc-950.
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   l-fat-dfm-exi-sts      .
       cnf-tes-doc-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per evasione righe ordine cliente              *
      *    *-----------------------------------------------------------*
       sdc-fat-dfm-000.
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
                     move  "#"            to   l-fat-dfm-exi-sts      .
       sdc-fat-dfm-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per evasione righe ordine cliente              *
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
      *              *-------------------------------------------------*
      *              * Se Tab in corso                                 *
      *              *-------------------------------------------------*
           if        l-fat-dfm-fky-tab    =    spaces
                     go to acc-fun-sdc-400.
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag di Tab in corso        *
      *                  *---------------------------------------------*
           move      spaces               to   l-fat-dfm-fky-tab      .
      *                  *---------------------------------------------*
      *                  * Video in On                                 *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-fun-sdc-400.
      *              *-------------------------------------------------*
      *              * Accettazione riga saldaconto                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Quantita' in fattura                        *
      *                  *---------------------------------------------*
           perform   acc-qta-fat-000      thru acc-qta-fat-999        .
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
      *                  * Tipo                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      07                   to   v-pos                  .
           move      w-acc-cod-tmb        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      13                   to   v-pos                  .
           move      w-acc-cod-tmb-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      48                   to   v-pos                  .
           move      "Data:"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      04                   to   v-lin                  .
           move      54                   to   v-pos                  .
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
           move      07                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      67                   to   v-pos                  .
           move      "Numero:"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      04                   to   v-lin                  .
           move      75                   to   v-pos                  .
           move      w-acc-num-doc-prg    to   v-num                  .
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
           if        w-prs-rig-scr-des    =    00
                     go to pmt-fun-sdc-340
           else if   w-prs-rig-scr-des    =    01
                     go to pmt-fun-sdc-360
           else if   w-prs-rig-scr-des    =    02
                     go to pmt-fun-sdc-380
           else if   w-prs-rig-scr-des    =    03
                     go to pmt-fun-sdc-400
           else if   w-prs-rig-scr-des    =    04
                     go to pmt-fun-sdc-420
           else if   w-prs-rig-scr-des    =    05
                     go to pmt-fun-sdc-440
           else if   w-prs-rig-scr-des    =    06
                     go to pmt-fun-sdc-460
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
           move      "               Descrizione              |  In boll
      -              "a | In fattura| I |  Residuo  "
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
      -              "--|-----------|---|-----------"
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
           move      "       Descrizione            Codice    |  In boll
      -              "a | In fattura| I |  Residuo  "
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
      -              "--|-----------|---|-----------"
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
           move      "     Descrizione            Codice      |  In boll
      -              "a | In fattura| I |  Residuo  "
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
      -              "--|-----------|---|-----------"
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
           move      "    Codice            Descrizione       |  In boll
      -              "a | In fattura| I |  Residuo  "
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
      -              "--|-----------|---|-----------"
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
           move      "      Codice            Descrizione     |  In boll
      -              "a | In fattura| I |  Residuo  "
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
      -              "--|-----------|---|-----------"
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
           move      "       Descrizione            Codice    |  In boll
      -              "a | In fattura| I |  Residuo  "
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
      -              "--|-----------|---|-----------"
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
           move      "     Descrizione            Codice      |  In boll
      -              "a | In fattura| I |  Residuo  "
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
      -              "--|-----------|---|-----------"
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
      -              "  |           |   |           "
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
      -              "  |           |   |           "
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
           move      w-brb-qta-bol
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
      *                          * Quantita' in fattura                *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      54                   to   v-pos                  .
           move      w-brb-qta-fat
                    (w-sdc-wrk-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Flag indicatore di residuo          *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      66                   to   v-pos                  .
           move      w-brb-flg-idr
                    (w-sdc-wrk-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Flag indicatore di saldo            *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      68                   to   v-pos                  .
           move      w-brb-flg-ids
                    (w-sdc-wrk-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Quantita' residua                   *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Editing quantita'               *
      *                              *---------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      w-brb-qta-res
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
           move      70                   to   v-pos                  .
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
      *    * Accettazione campo saldaconto : Quantita' in fattura      *
      *    *-----------------------------------------------------------*
       acc-qta-fat-000.
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
                     go to acc-qta-fat-999.
       acc-qta-fat-050.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-brb-qta-fat
                    (w-sdc-ctr-rig)       to   w-sav-qta-fat          .
           move      w-brb-flg-fzs
                    (w-sdc-ctr-rig)       to   w-sav-flg-fzs          .
       acc-qta-fat-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
      *
           if        w-brb-dec-qta
                    (w-sdc-ctr-rig)       >    2
                     move  2              to   v-dec
           else      move  w-brb-dec-qta
                          (w-sdc-ctr-rig) to   v-dec                  .
      *
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
           move      "SLCT"               to   v-pfk (11)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-brb-qta-fat
                    (w-sdc-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-qta-fat-999.
       acc-qta-fat-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   w-brb-qta-fat
                                              (w-sdc-ctr-rig)         .
       acc-qta-fat-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo compatibilita' fra segno quantita'*
      *                  * in fattura e segno quantita' in bolla       *
      *                  *---------------------------------------------*
           if        w-brb-qta-bol
                    (w-sdc-ctr-rig)       >    zero and
                     w-brb-qta-fat
                    (w-sdc-ctr-rig)       <    zero
                     go to acc-qta-fat-100
           else if   w-brb-qta-bol
                    (w-sdc-ctr-rig)       <    zero and
                     w-brb-qta-fat
                    (w-sdc-ctr-rig)       >    zero
                     go to acc-qta-fat-100.
      *                  *---------------------------------------------*
      *                  * Se Pf1 : controllo che il valore impostato  *
      *                  * non sia zero                                *
      *                  *---------------------------------------------*
           if        v-key                not  = "[1] "
                     go to acc-qta-fat-420.
           if        w-brb-qta-fat
                    (w-sdc-ctr-rig)       =    zero
                     go to acc-qta-fat-100.
       acc-qta-fat-420.
      *                  *---------------------------------------------*
      *                  * Se Slct : controllo che il valore impostato *
      *                  * sia uguale al precedente                    *
      *                  *---------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-qta-fat-600.
           if        w-brb-qta-fat
                    (w-sdc-ctr-rig)       not  = w-sav-qta-fat
                     move   w-sav-qta-fat to   w-brb-qta-fat
                                              (w-sdc-ctr-rig)
                     go to  acc-qta-fat-100.
       acc-qta-fat-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Quantita' in bolla e quantita' in fattura   *
      *                  * in work di comodo senza segno               *
      *                  *---------------------------------------------*
           move      w-brb-qta-bol
                    (w-sdc-ctr-rig)       to   w-qss-qta-bol          .
           move      w-brb-qta-fat
                    (w-sdc-ctr-rig)       to   w-qss-qta-fat          .
      *                  *---------------------------------------------*
      *                  * Se Slct                                     *
      *                  *---------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to  acc-qta-fat-620.
      *                      *-----------------------------------------*
      *                      * Se valore gia' presente                 *
      *                      *-----------------------------------------*
           if        w-brb-qta-fat
                    (w-sdc-ctr-rig)       =    zero
                     go to acc-qta-fat-610.
      *                          *-------------------------------------*
      *                          * Quantita' in fattura                *
      *                          *-------------------------------------*
           move      zero                 to   w-brb-qta-fat
                                              (w-sdc-ctr-rig)         .
           move      zero                 to   w-qss-qta-fat          .
      *                          *-------------------------------------*
      *                          * Flag di saldo forzato               *
      *                          *-------------------------------------*
           move      spaces               to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-700.
       acc-qta-fat-610.
      *                      *-----------------------------------------*
      *                      * Se valore non presente                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Quantita' in fattura                *
      *                          *-------------------------------------*
           move      w-brb-qta-bol
                    (w-sdc-ctr-rig)       to   w-brb-qta-fat
                                              (w-sdc-ctr-rig)         .
           move      w-brb-qta-bol
                    (w-sdc-ctr-rig)       to   w-qss-qta-fat          .
      *                          *-------------------------------------*
      *                          * Flag di saldo forzato               *
      *                          *-------------------------------------*
           move      spaces               to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-700.
       acc-qta-fat-620.
      *                  *---------------------------------------------*
      *                  * Se Pf1                                      *
      *                  *---------------------------------------------*
           if        v-key                not  = "[1] "
                     go to  acc-qta-fat-640.
      *                      *-----------------------------------------*
      *                      * Flag di saldo forzato                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se quantita' in fattura maggiore o  *
      *                          * uguale a quella in bolla : spaces   *
      *                          *-------------------------------------*
           if        w-qss-qta-fat        <    w-qss-qta-bol
                     go to  acc-qta-fat-630.
           move      spaces               to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-700.
       acc-qta-fat-630.
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
           go to     acc-qta-fat-700.
       acc-qta-fat-640.
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
           if        w-brb-qta-fat
                    (w-sdc-ctr-rig)        not  = w-sav-qta-fat
                     go to  acc-qta-fat-650.
           go to     acc-qta-fat-700.
       acc-qta-fat-650.
      *                          *-------------------------------------*
      *                          * Se valore impostato diverso dal     *
      *                          * precedente                          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in fattura maggio- *
      *                              * re o uguale a quella in bolla : *
      *                              * spaces                          *
      *                              *---------------------------------*
           if        w-qss-qta-fat        <    w-qss-qta-bol
                     go to  acc-qta-fat-655.
           move      spaces               to   w-brb-flg-fzs
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-700.
       acc-qta-fat-655.
      *                              *---------------------------------*
      *                              * Altrimenti : valore inalterato  *
      *                              *---------------------------------*
           go to     acc-qta-fat-700.
       acc-qta-fat-700.
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
                     go to acc-qta-fat-710.
           move      spaces               to   w-brb-flg-idr
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-720.
       acc-qta-fat-710.
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato diverso da *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in fattura maggio- *
      *                              * re o uguale a quella in bolla : *
      *                              * spaces                          *
      *                              *---------------------------------*
           if        w-qss-qta-fat        <    w-qss-qta-bol
                     go to  acc-qta-fat-715.
           move      spaces               to   w-brb-flg-idr
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-720.
       acc-qta-fat-715.
      *                              *---------------------------------*
      *                              * Altrimenti : il valore viene    *
      *                              * forzato a '<'                   *
      *                              *---------------------------------*
           move      "<"                  to   w-brb-flg-idr
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-720.
       acc-qta-fat-720.
      *                      *-----------------------------------------*
      *                      * Flag indicatore di saldo                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato a 'S'      *
      *                          *-------------------------------------*
           if        w-brb-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     go to acc-qta-fat-730.
           move      "S"                  to   w-brb-flg-ids
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-740.
       acc-qta-fat-730.
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato diverso da *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in fattura minore  *
      *                              * o uguale a quella in bolla :    *
      *                              * viene forzato il valore spaces  *
      *                              *---------------------------------*
           if        w-qss-qta-fat        >    w-qss-qta-bol
                     go to  acc-qta-fat-735.
           move      spaces               to   w-brb-flg-ids
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-740.
       acc-qta-fat-735.
      *                              *---------------------------------*
      *                              * Altrimenti : il valore viene    *
      *                              * forzato a "+"                   *
      *                              *---------------------------------*
           move      "+"                  to   w-brb-flg-ids
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-740.
       acc-qta-fat-740.
      *                      *-----------------------------------------*
      *                      * Quantita' residua                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato a 'S'      *
      *                          *-------------------------------------*
           if        w-brb-flg-fzs
                    (w-sdc-ctr-rig)       =    spaces
                     go to acc-qta-fat-750.
           move      zero                 to   w-brb-qta-res
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-760.
       acc-qta-fat-750.
      *                          *-------------------------------------*
      *                          * Se flag di saldo forzato diverso da *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se quantita' in fattura maggio- *
      *                              * re o uguale a quella in bolla : *
      *                              * spaces                          *
      *                              *---------------------------------*
           if        w-qss-qta-fat        <    w-qss-qta-bol
                     go to  acc-qta-fat-755.
           move      zero                 to   w-brb-qta-res
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-fat-760.
       acc-qta-fat-755.
      *                              *---------------------------------*
      *                              * Altrimenti : il valore e' pari  *
      *                              * alla differenza tra quantita'   *
      *                              * in bolla e quantita' in fattura *
      *                              *---------------------------------*
           subtract  w-brb-qta-fat
                    (w-sdc-ctr-rig)       from w-brb-qta-bol
                                              (w-sdc-ctr-rig)
                                        giving w-brb-qta-res
                                              (w-sdc-ctr-rig)         .
      *                              *---------------------------------*
      *                              * Avvertimento                    *
      *                              *---------------------------------*
           move      "La quantita' residua della riga bolla non potra' e
      -              "ssere richiama-"    to   w-err-box-err-msg      .
           move      "ta da un altro documento !                        
      -              "               "    to   w-err-box-err-m02      .
           perform   box-msg-e02-000      thru box-msg-e02-999        .
      *                              *---------------------------------*
      *                              * A visualizzazione valori riga   *
      *                              *---------------------------------*
           go to     acc-qta-fat-760.
       acc-qta-fat-760.
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori riga                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Quantita' in fattura                    *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
      *
           if        w-brb-dec-qta
                    (w-sdc-ctr-rig)       >    2
                     move  2              to   v-dec
           else      move  w-brb-dec-qta
                          (w-sdc-ctr-rig) to   v-dec                  .
      *
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      54                   to   v-pos                  .
           move      w-brb-qta-fat
                    (w-sdc-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Flag indicatore di residuo              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      66                   to   v-pos                  .
           move      w-brb-flg-idr
                    (w-sdc-ctr-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Flag indicatore di saldo                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      68                   to   v-pos                  .
           move      w-brb-flg-ids
                    (w-sdc-ctr-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Quantita' residua                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing quantita'                   *
      *                          *-------------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      w-brb-qta-res
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
           move      70                   to   v-pos                  .
           move      w-edt-qta-inc-edt    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-qta-fat-800.
       acc-qta-fat-999.
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
           move      "SLCT"               to   v-pfk (11)             .
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
                     go to car-rig-cat-750.
      *                  *---------------------------------------------*
      *                  * Selezione su righe bolla nel buffer         *
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
           if        w-brb-qta-fat
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
      *                  * Normalizzazione record [bir]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [bir]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-gen-prt-bol        to   rf-bir-num-prt         .
           move      w-brb-num-prg
                    (w-crc-wrk-ctr)       to   rf-bir-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
       car-rig-cat-600.
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Valori contenuti direttamente in re-    *
      *                      * cord [bir]                              *
      *                      *-----------------------------------------*
           move      w-cat-rig-prg        to   w-rig-num-prg (1)      .
           move      rf-bir-bld-flb       to   w-rig-bld-flb (1)      .
           move      rf-bir-bld-tpb       to   w-rig-bld-tpb (1)      .
           move      rf-bir-bld-rgb       to   w-rig-bld-rgb (1)      .
           move      rf-bir-tip-rig       to   w-rig-tip-rig (1)      .
           perform   dec-tip-rig-000      thru dec-tip-rig-999        .
           move      rf-bir-tip-mag       to   w-rig-tip-mag (1)      .
           move      rf-bir-num-pro       to   w-rig-num-pro (1)      .
           move      rf-bir-alf-pro       to   w-rig-alf-pro (1)      .
           move      rf-bir-sgl-vrn       to   w-rig-sgl-vrn (1)      .
           move      rf-bir-cop-scl       to   w-rig-cop-scl (1)      .
           move      rf-bir-des-ext       to   w-rig-des-ext (1)      .
           move      rf-bir-des-rig       to   w-rig-des-rig (1)      .
           move      rf-bir-tip-pro       to   w-rig-tip-pro (1)      .
           move      rf-bir-cod-iva       to   w-rig-coi-rig (1)      .
           move      rf-bir-ctp-ven       to   w-rig-ctv-rig (1)      .
           move      rf-bir-umi-ven       to   w-rig-umi-ven (1)      .
           move      rf-bir-dec-qta       to   w-rig-dec-qta (1)      .
           move      rf-bir-snx-2qt       to   w-rig-snx-2qt (1)      .
           move      rf-bir-dec-2qt       to   w-rig-dec-2qt (1)      .
           move      rf-bir-qta-a02       to   w-rig-qta-a02 (1)      .
           move      rf-bir-snx-3qt       to   w-rig-snx-3qt (1)      .
           move      rf-bir-dec-3qt       to   w-rig-dec-3qt (1)      .
           move      rf-bir-qta-a03       to   w-rig-qta-a03 (1)      .
           move      rf-bir-dec-prz       to   w-rig-dec-prz (1)      .
           move      rf-bir-sgl-vps       to   w-rig-sgl-vps (1)      .
           move      rf-bir-dec-vps       to   w-rig-dec-vps (1)      .
           move      rf-bir-tdc-vps       to   w-rig-tdc-vps (1)      .
           move      rf-bir-prz-lrs       to   w-rig-prz-lrs (1)      .
           move      rf-bir-prz-nts       to   w-rig-prz-nts (1)      .
           move      rf-bir-snx-2pz       to   w-rig-snx-2pz (1)      .
           move      rf-bir-prz-a02       to   w-rig-prz-a02 (1)      .
           move      rf-bir-sgl-vpl       to   w-rig-sgl-vpl (1)      .
           move      rf-bir-dec-vpl       to   w-rig-dec-vpl (1)      .
           move      rf-bir-tdc-vpl       to   w-rig-tdc-vpl (1)      .
           move      rf-bir-prz-vpl       to   w-rig-prz-vpl (1)      .
           move      rf-bir-cdc-vpl       to   w-rig-cdc-vpl (1)      .
           move      rf-bir-ccr-vpl       to   w-rig-ccr-vpl (1)      .
           move      rf-bir-plm-vpl       to   w-rig-plm-vpl (1)      .
           move      rf-bir-tlm-vpl       to   w-rig-tlm-vpl (1)      .
           move      rf-bir-map-vpl       to   w-rig-map-vpl (1)      .
           move      rf-bir-epz-rgf       to   w-rig-epz-rgf (1)      .
           move      rf-bir-csr-aap       to   w-rig-csr-aap (1)      .
           move      rf-bir-psr-aap (1)   to   w-rig-psr-aap (1, 1)   .
           move      rf-bir-psr-aap (2)   to   w-rig-psr-aap (1, 2)   .
           move      rf-bir-psr-aap (3)   to   w-rig-psr-aap (1, 3)   .
           move      rf-bir-psr-aap (4)   to   w-rig-psr-aap (1, 4)   .
           move      rf-bir-psr-aap (5)   to   w-rig-psr-aap (1, 5)   .
           move      rf-bir-per-scr (1)   to   w-rig-per-scr (1, 1)   .
           move      rf-bir-per-scr (2)   to   w-rig-per-scr (1, 2)   .
           move      rf-bir-per-scr (3)   to   w-rig-per-scr (1, 3)   .
           move      rf-bir-per-scr (4)   to   w-rig-per-scr (1, 4)   .
           move      rf-bir-per-scr (5)   to   w-rig-per-scr (1, 5)   .
           move      rf-bir-epz-pes       to   w-rig-epz-pes (1)      .
           move      rf-bir-sgl-vpc       to   w-rig-sgl-vpc (1)      .
           move      rf-bir-dec-vpc       to   w-rig-dec-vpc (1)      .
           move      rf-bir-tdc-vpc       to   w-rig-tdc-vpc (1)      .
           move      rf-bir-cdc-vpc       to   w-rig-cdc-vpc (1)      .
           move      rf-bir-dec-cos       to   w-rig-dec-cos (1)      .
           move      rf-bir-cos-rif       to   w-rig-cos-rif (1)      .
           move      rf-bir-iau-rig       to   w-rig-iau-rig (1)      .
           move      rf-bir-cpv-aap       to   w-rig-cpv-aap (1)      .
           move      rf-bir-ppv-aap (1)   to   w-rig-ppv-aap (1, 1)   .
           move      rf-bir-ppv-aap (2)   to   w-rig-ppv-aap (1, 2)   .
           move      rf-bir-ppv-aap (3)   to   w-rig-ppv-aap (1, 3)   .
           move      rf-bir-fsp-rig       to   w-rig-fsp-rig (1)      .
           move      rf-bir-cpv-rig       to   w-rig-cpv-rig (1)      .
           move      rf-bir-ppv-rig (1)   to   w-rig-ppv-rig (1, 1)   .
           move      rf-bir-ppv-rig (2)   to   w-rig-ppv-rig (1, 2)   .
           move      rf-bir-ppv-rig (3)   to   w-rig-ppv-rig (1, 3)   .
           move      rf-bir-pvf-rig       to   w-rig-pvf-rig (1)      .
           move      rf-bir-ocl-dat       to   w-rig-ocl-dat (1)      .
           move      rf-bir-ocl-num       to   w-rig-ocl-num (1)      .
           move      rf-bir-cmc-tip       to   w-rig-cmc-tip (1)      .
           move      rf-bir-cmc-dat       to   w-rig-cmc-dat (1)      .
           move      rf-bir-cmc-num       to   w-rig-cmc-num (1)      .
           move      rf-bir-coc-tip       to   w-rig-coc-tip (1)      .
           move      rf-bir-coc-dat       to   w-rig-coc-dat (1)      .
           move      rf-bir-coc-num       to   w-rig-coc-num (1)      .
           move      rf-bir-cod-tmb       to   w-rig-bcc-tip (1)      .
           move      rf-bir-dat-doc       to   w-rig-bcc-dat (1)      .
           move      rf-bir-num-doc       to   w-rig-bcc-num (1)      .
           move      rf-bir-flg-puq       to   w-rig-flg-puq (1)      .
       car-rig-cat-400.
      *                      *-----------------------------------------*
      *                      * Valori contenuti in buffer righe bolla  *
      *                      *-----------------------------------------*
       car-rig-cat-420.
      *                          *-------------------------------------*
      *                          * Quantita' per la vendita            *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A"
                     move  zero           to   w-rig-qta-ven (1)
           else      move  w-brb-qta-fat
                          (w-crc-wrk-ctr) to   w-rig-qta-ven (1)      .
       car-rig-cat-425.
      *                          *-------------------------------------*
      *                          * Trattamento della seconda quantita' *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se trattamento necessario  *
      *                              *---------------------------------*
           if        w-rig-snx-2qt (1)    not  = 1
                     go to car-rig-cat-430.
           if        w-rig-qta-ven (1)    =    zero
                     move  zero           to   w-rig-qta-a02 (1)
                     go to car-rig-cat-430.
      *                              *---------------------------------*
      *                              * Determinazione                  *
      *                              *---------------------------------*
           move      w-rig-qta-ven (1)    to   w-det-qta-a02-qim      .
           move      w-rig-dec-2qt (1)    to   w-det-qta-a02-ndu      .
      *
           if        w-rig-flg-puq (1)    =    "D"
                     move  10             to   w-det-qta-a02-cmu
           else if   w-rig-flg-puq (1)    =    "C"
                     move  100            to   w-det-qta-a02-cmu
           else if   w-rig-flg-puq (1)    =    "K"
                     move  1000           to   w-det-qta-a02-cmu
           else      move  1              to   w-det-qta-a02-cmu      .
      *
           move      1                    to   w-det-qta-a02-cdu      .
           perform   det-qta-a02-000      thru det-qta-a02-999        .
           move      w-det-qta-a02-qts    to   w-rig-qta-a02 (1)      .
       car-rig-cat-430.
      *                          *-------------------------------------*
      *                          * Coefficiente di cambio valuta per   *
      *                          * i prezzi standard alla data del do- *
      *                          * cumento                             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Determinazione                  *
      *                              *---------------------------------*
           move      w-tes-cdc-vpf (1)    to   w-rig-cdc-vps (1)      .
       car-rig-cat-440.
      *                          *-------------------------------------*
      *                          * Prezzo di vendita e prezzo netto    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se valuta per il prezzo conte-  *
      *                              * nuta nella riga bolla e' pari   *
      *                              * alla valuta per fatturazione    *
      *                              * del documento, vuol dire che    *
      *                              * l'eventuale cambio e' gia' sta- *
      *                              * to eseguito al momento della    *
      *                              * bolla e quindi il prezzo rimane *
      *                              * quello contenuto nel record     *
      *                              * [bir]                           *
      *                              *---------------------------------*
           if        rf-bir-sgl-vpp       =    w-tes-sgl-vpf (1) and
                     rf-bir-dec-vpp       =    w-tes-dec-vpf (1) and
                     rf-bir-tdc-vpp       =    w-tes-tdc-vpf (1)
                     move  rf-bir-prz-ven to   w-rig-prz-ven (1)
                     move  rf-bir-prz-net to   w-rig-prz-net (1)
                     go to car-rig-cat-460.
      *                              *---------------------------------*
      *                              * Trasformazione prezzo di ven-   *
      *                              * dita espresso nella valuta per  *
      *                              * il prezzo nel valore espresso   *
      *                              * nella valuta di fatturazione    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione coefficien-  *
      *                                  * te di cambio per la valuta  *
      *                                  * per il prezzo alla data del *
      *                                  * documento                   *
      *                                  *-----------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-bir-sgl-vpp       to   w-coe-cmb-vlt-sdv      .
           move      rf-bir-tdc-vpp       to   w-coe-cmb-vlt-tdc      .
           move      w-tes-dat-doc        to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                                  *-----------------------------*
      *                                  * Se esito determinazione ne- *
      *                                  * gativo : bufferizzazione    *
      *                                  * coefficiente di cambio con- *
      *                                  * tenuto nel record [bir]     *
      *                                  *-----------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-bir-cdc-vpp to   w-coe-cmb-vlt-cdc      .
      *                                  *-----------------------------*
      *                                  * Conversione da valuta per   *
      *                                  * il prezzo a valuta base     *
      *                                  *-----------------------------*
           move      rf-bir-sgl-vpp       to   w-cvs-vlt-sgl          .
           move      rf-bir-dec-vpp       to   w-cvs-vlt-dec          .
           move      rf-bir-tdc-vpp       to   w-cvs-vlt-tdc          .
           move      w-coe-cmb-vlt-cdc    to   w-cvs-vlt-cdc          .
           move      rf-bir-prz-ven       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                                  *-----------------------------*
      *                                  * Conversione da valuta base  *
      *                                  * a valuta per fatturazione   *
      *                                  *-----------------------------*
           move      w-tes-sgl-vpf (1)    to   w-cvs-vlt-sgl          .
           move      w-tes-dec-vpf (1)    to   w-cvs-vlt-dec          .
           move      w-tes-tdc-vpf (1)    to   w-cvs-vlt-tdc          .
           move      w-tes-cdc-vpf (1)    to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
      *                                  *-----------------------------*
      *                                  * Bufferizzazione nuovo prez- *
      *                                  * zo                          *
      *                                  *-----------------------------*
           move      w-cvs-vlt-aav        to   w-rig-prz-ven (1)      .
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
       car-rig-cat-460.
      *                          *-------------------------------------*
      *                          * Trattamento legame valutario        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se da effettuare           *
      *                              *---------------------------------*
           if        w-tes-mac-lvl (1)    =    00
                     go to car-rig-cat-480.
           if        w-rig-sgl-vpl (1)    =    spaces
                     go to car-rig-cat-480.
      *                              *---------------------------------*
      *                              * Deviazione a seconda se cambio  *
      *                              * da applicare o meno             *
      *                              *---------------------------------*
           if        w-tes-mac-lvl (1)    =    21
                     go to car-rig-cat-462
           else      go to car-rig-cat-466.
       car-rig-cat-462.
      *                              *---------------------------------*
      *                              * Se cambio da applicare          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione coefficiente *
      *                                  * di cambio effettivo per il  *
      *                                  * legame valutario alla data  *
      *                                  * del documento               *
      *                                  *-----------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      w-rig-sgl-vpl (1)    to   w-coe-cmb-vlt-sdv      .
           move      w-rig-tdc-vpl (1)    to   w-coe-cmb-vlt-tdc      .
           move      w-tes-dat-doc        to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                                  *-----------------------------*
      *                                  * Se esito determinazione ne- *
      *                                  * gativo : bufferizzazione    *
      *                                  * coefficiente di cambio con- *
      *                                  * tenuto nel record [bir]     *
      *                                  *-----------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-bir-cdc-vpl to   w-coe-cmb-vlt-cdc      .
      *                                  *-----------------------------*
      *                                  * Bufferizzazione coefficien- *
      *                                  * te di cambio determinato    *
      *                                  *-----------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-rig-cdc-vpl (1)      .
      *                                  *-----------------------------*
      *                                  * Salvataggio prezzo di ven-  *
      *                                  * dita precedente             *
      *                                  *-----------------------------*
           move      w-rig-prz-ven (1)    to   w-sav-prz-ven          .
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
      *                                  * Se prezzo di vendita inva-  *
      *                                  * riato : oltre               *
      *                                  *-----------------------------*
           if        w-rig-prz-ven (1)    =    w-sav-prz-ven
                     go to car-rig-cat-464.
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
      *                                  *-----------------------------*
      *                                  * Provvigioni in riga         *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Se prodotto similare :  *
      *                                      * oltre                   *
      *                                      *-------------------------*
           if        w-rig-tip-rig-tfu (1)
                                          =    "N"
                     go to car-rig-cat-464.
      *                                      *-------------------------*
      *                                      * Test se da trattare     *
      *                                      *-------------------------*
           if        w-prs-age-snx        not  = "S"
                     go to car-rig-cat-464.
      *                                      *-------------------------*
      *                                      * Determinazione          *
      *                                      *-------------------------*
           perform   det-ppv-rig-000      thru det-ppv-rig-999        .
       car-rig-cat-464.
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-480.
       car-rig-cat-466.
      *                              *---------------------------------*
      *                              * Se cambio da non applicare      *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-480.
       car-rig-cat-480.
      *                          *-------------------------------------*
      *                          * Importo in riga                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Deviazione a seconda del tipo   *
      *                              * riga                            *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-482
           else if   w-rig-tip-rig-tpr (1)
                                          =    "A"
                     go to car-rig-cat-484
           else      go to car-rig-cat-486.
       car-rig-cat-482.
      *                              *---------------------------------*
      *                              * Se tipo riga : Commento         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-500.
       car-rig-cat-484.
      *                              *---------------------------------*
      *                              * Se tipo riga : Addebito         *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Valore contenuto in record  *
      *                                  * [bir]                       *
      *                                  *-----------------------------*
           move      rf-bir-imp-rig       to   w-rig-imp-rig (1)      .
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-500.
       car-rig-cat-486.
      *                              *---------------------------------*
      *                              * Se altro tipo riga              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Determinazione in base alla *
      *                                  * quantita' effettivamente    *
      *                                  * fatturata                   *
      *                                  *-----------------------------*
           perform   det-imp-rig-000      thru det-imp-rig-999        .
      *                                  *-----------------------------*
      *                                  * Oltre                       *
      *                                  *-----------------------------*
           go to     car-rig-cat-500.
       car-rig-cat-500.
      *                          *-------------------------------------*
      *                          * Codice iva                          *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se da eseguire             *
      *                              *---------------------------------*
           if        w-prs-trt-aic        not  = 01
                     go to car-rig-cat-550.
      *                              *---------------------------------*
      *                              * Test se da trattare             *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-550.
      *                              *---------------------------------*
      *                              * Deviazione a seconda del con-   *
      *                              * fronto tra assoggettamento iva  *
      *                              * del documento e codice iva in   *
      *                              * riga                            *
      *                              *---------------------------------*
           move      rf-bir-cod-iva       to   w-edt-iva-cod          .
      *
           if       (w-tes-ass-iva (1)    =    zero) and
                    (w-edt-iva-cod-003    >    2 and
                     w-edt-iva-cod-003    <    7   )
                     go to car-rig-cat-510
           else if   w-tes-flg-rfp (1)    =    "S"
                     go to car-rig-cat-510
           else if   w-tes-ass-iva (1)    >    zero
                     go to car-rig-cat-520
           else      go to car-rig-cat-550.
       car-rig-cat-510.
      *                              *---------------------------------*
      *                              * Se documento soggetto e codice  *
      *                              * iva in riga esente              *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Deviazione in funzione del  *
      *                                  * tipo riga                   *
      *                                  *-----------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-512
           else if   w-rig-tip-rig-tpr (1)
                                          =    "A"
                     go to car-rig-cat-514
           else      go to car-rig-cat-516.
       car-rig-cat-512.
      *                                  *-----------------------------*
      *                                  * Tipo riga : Prodotto di     *
      *                                  * vendita                     *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Lettura archivio [dcp]  *
      *                                      *-------------------------*
           move      w-rig-num-pro (1)    to   w-let-dcp-pdx-cod      .
           move      "C"                  to   w-let-dcp-pdx-tar      .
           if        w-tes-tip-frn (1)    =    11
                     move  w-tes-cod-cli (1)
                                          to   w-let-dcp-pdx-arc
           else      move  w-tes-cli-plf (1)
                                          to   w-let-dcp-pdx-arc      .
           move      w-tes-cod-lng (1)    to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
      *                                      *-------------------------*
      *                                      * Bufferizzazione codice  *
      *                                      * iva                     *
      *                                      *-------------------------*
           move      w-let-dcp-pdx-coi    to   w-rig-coi-rig (1)      .
           go to     car-rig-cat-550.
       car-rig-cat-514.
      *                                  *-----------------------------*
      *                                  * Tipo riga : Addebito        *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Lettura archivio [zac]  *
      *                                      *-------------------------*
           move      01                   to   w-let-arc-zac-tip      .
           move      w-rig-tip-rig-cac (1)
                                          to   w-let-arc-zac-cod      .
           perform   let-arc-zac-000      thru let-arc-zac-999        .
      *                                      *-------------------------*
      *                                      * Bufferizzazione codice  *
      *                                      * iva                     *
      *                                      *-------------------------*
           move      w-let-arc-zac-civ    to   w-rig-coi-rig (1)      .
           go to     car-rig-cat-550.
       car-rig-cat-516.
      *                              *---------------------------------*
      *                              * Tipo riga : Altra               *
      *                              *---------------------------------*
           move      zero                 to   w-rig-coi-rig (1)      .
           go to     car-rig-cat-550.
       car-rig-cat-520.
      *                              *---------------------------------*
      *                              * Se documento esente e codice    *
      *                              * iva in riga soggetto            *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Forzatura esenzione in co-  *
      *                                  * dice iva in riga            *
      *                                  *-----------------------------*
           move      w-tes-ass-iva (1)    to   w-rig-coi-rig (1)      .
           go to     car-rig-cat-550.
       car-rig-cat-550.
      *                      *-----------------------------------------*
      *                      * Fine trattamento valori contenuti o de- *
      *                      * dotti da buffer righe bolla             *
      *                      *-----------------------------------------*
       car-rig-cat-600.
      *                      *-----------------------------------------*
      *                      * Valori contenuti indirettamente in re-  *
      *                      * cord [bir]                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione                         *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura preliminare file [bix]  *
      *                              *---------------------------------*
           move      w-gen-prt-bol        to   w-let-arc-bix-prt      .
           move      w-brb-num-prg
                    (w-crc-wrk-ctr)       to   w-let-arc-bix-prg      .
           move      11                   to   w-let-arc-bix-trc      .
           perform   let-arc-bix-000      thru let-arc-bix-999        .
      *
           if        w-let-arc-bix-flg    not  = spaces
                     go to car-rig-cat-605.
      *
           move      w-let-arc-bix-des    to   w-rig-des-rig (1)      .
      *
           go to     car-rig-cat-640.
       car-rig-cat-605.
      *                              *---------------------------------*
      *                              * Deviazione in funzione del tipo *
      *                              * di estensione                   *
      *                              *---------------------------------*
           if        w-rig-des-ext (1)    =    0
                     go to car-rig-cat-610
           else if   w-rig-des-ext (1)    =    1
                     go to car-rig-cat-620
           else if   w-rig-des-ext (1)    =    2
                     go to car-rig-cat-630.
       car-rig-cat-610.
      *                              *---------------------------------*
      *                              * Se nessuna estensione : nessuna *
      *                              * azione in quanto la descrizione *
      *                              * e' gia' stata bufferizzata      *
      *                              *---------------------------------*
           go to     car-rig-cat-640.
       car-rig-cat-620.
      *                              *---------------------------------*
      *                              * Se estensione nel file [bix]    *
      *                              *---------------------------------*
           go to     car-rig-cat-640.
       car-rig-cat-630.
      *                              *---------------------------------*
      *                              * Se estensione nel file [pdx]    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura archivio [dcp]      *
      *                                  *-----------------------------*
           move      w-rig-num-pro (1)    to   w-let-dcp-pdx-cod      .
           move      "C"                  to   w-let-dcp-pdx-tar      .
           if        w-tes-tip-frn (1)    =    11
                     move  w-tes-cod-cli (1)
                                          to   w-let-dcp-pdx-arc
           else      move  w-tes-cli-plf (1)
                                          to   w-let-dcp-pdx-arc      .
           move      w-tes-cod-lng (1)    to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-rig-des-por (1)
                                               w-rig-des-rig (1)      .
           go to     car-rig-cat-640.
       car-rig-cat-640.
      *                          *-------------------------------------*
      *                          * Anagrafica titoli esenzione         *
      *                          *-------------------------------------*
           move      w-rig-coi-rig (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
           move      w-let-arc-zci-des    to   w-rig-coi-rig-des (1)  .
      *                          *-------------------------------------*
      *                          * Anagrafica piano dei conti          *
      *                          *-------------------------------------*
           move      w-rig-ctv-rig (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-rig-ctv-rig-des (1)  .
       car-rig-cat-660.
      *                  *---------------------------------------------*
      *                  * Update catena                               *
      *                  *---------------------------------------------*
           move      "UP"                 to   w-cat-rig-ope          .
           move      w-cat-rig-max        to   w-cat-rig-num          .
           move      w-rig                to   w-cat-rig-buf          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
      *              *-------------------------------------------------*
      *              * Riciclo su riga bolla successiva                *
      *              *-------------------------------------------------*
           go to     car-rig-cat-100.
       car-rig-cat-750.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     car-rig-cat-999.
       car-rig-cat-900.
      *              *-------------------------------------------------*
      *              * Messaggio di errore per esubero numero righe    *
      *              *-------------------------------------------------*
           move      "Numero righe documento oltre il massimo previsto !
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       car-rig-cat-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per gestione catena righe         *
      *    *-----------------------------------------------------------*
       cll-sub-cat-000.
           call      "pgm/fat/prg/obj/pfat3002"
                                         using w-cat-rig              .
       cll-sub-cat-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione dati non chiave riga corpo                *
      *    *-----------------------------------------------------------*
       nor-nok-rig-000.
           move      zero                 to   w-rig-num-prg (1)      .
           move      zero                 to   w-rig-cli-pls (1)      .
           move      spaces               to   w-rig-dpc-pls (1)      .
           move      zero                 to   w-rig-bld-flb (1)      .
           move      zero                 to   w-rig-bld-tpb (1)      .
           move      zero                 to   w-rig-bld-rgb (1)      .
           move      spaces               to   w-rig-tip-rig (1)      .
           move      spaces               to   w-rig-tip-rig-tpr (1)  .
           move      spaces               to   w-rig-tip-rig-tfu (1)  .
           move      zero                 to   w-rig-tip-rig-cac (1)  .
           move      spaces               to   w-rig-tip-rig-ast (1)  .
           move      zero                 to   w-rig-tip-rig-tot (1)  .
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
           move      spaces               to   w-rig-bcc-tip (1)      .
           move      zero                 to   w-rig-bcc-dat (1)      .
           move      zero                 to   w-rig-bcc-num (1)      .
           move      spaces               to   w-rig-flg-ela (1)      .
           move      spaces               to   w-rig-flg-pul (1)      .
           move      spaces               to   w-rig-flg-puq (1)      .
           move      spaces               to   w-rig-alx-exp (1)      .
       nor-nok-rig-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento records [bit] e [bir] per quanto riguarda i *
      *    * dati relativi alla fattura differita durante la fase di   *
      *    * Inserimento di un nuovo record [fir]                      *
      *    *-----------------------------------------------------------*
       wrt-rec-bol-000.
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
      *              * Test se riga fattura che si riferisce ad una    *
      *              * bolla                                           *
      *              *-------------------------------------------------*
           if        w-rig-bcc-tip (1)    =    spaces and
                     w-rig-bcc-dat (1)    =    zero   and
                     w-rig-bcc-num (1)    =    zero
                     go to wrt-rec-bol-999.
      *              *-------------------------------------------------*
      *              * Test se la riga fattura si riferisce alla bolla *
      *              * gia' in esame                                   *
      *              *-------------------------------------------------*
           if        w-rig-bcc-tip (1)    =    w-agg-bcc-tip and
                     w-rig-bcc-dat (1)    =    w-agg-bcc-dat and
                     w-rig-bcc-num (1)    =    w-agg-bcc-num
                     go to wrt-rec-bol-999.
      *              *-------------------------------------------------*
      *              * Se i dati relativi alla fattura differita gia'  *
      *              * esistono : uscita                               *
      *              *-------------------------------------------------*
           if        w-rig-bcc-tip (1)    =    w-rig-bcc-tip (2) and
                     w-rig-bcc-dat (1)    =    w-rig-bcc-dat (2) and
                     w-rig-bcc-num (1)    =    w-rig-bcc-num (2)
                     go to wrt-rec-bol-999.
      *              *-------------------------------------------------*
      *              * Aggiornamento nuovi dati identificativi bolla   *
      *              * in corso                                        *
      *              *-------------------------------------------------*
           move      w-rig-bcc-tip (1)    to   w-agg-bcc-tip          .
           move      w-rig-bcc-dat (1)    to   w-agg-bcc-dat          .
           move      w-rig-bcc-num (1)    to   w-agg-bcc-num          .
      *              *-------------------------------------------------*
      *              * Aggiornamento testata bolla                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [bit]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-rig-bcc-dat (1)    to   rf-bit-dat-doc         .
           move      w-agg-bcc-num-dpz    to   rf-bit-cod-dpz         .
           move      w-rig-bcc-num (1)    to   rf-bit-num-doc         .
           move      w-rig-bcc-tip (1)    to   rf-bit-cod-tmb         .
           move      zero                 to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : uscita                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to wrt-rec-bol-999.
      *                  *---------------------------------------------*
      *                  * Ottenimento sequenziale record [bit]        *
      *                  *---------------------------------------------*
           move      "GN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                      *-----------------------------------------*
      *                      * Se 'At end' : uscita con release        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to wrt-rec-bol-100.
      *                  *---------------------------------------------*
      *                  * Test sul massimo : uscita con release       *
      *                  *---------------------------------------------*
           if        rf-bit-dat-doc       not  = w-rig-bcc-dat (1) or
                     rf-bit-cod-dpz       not  = w-agg-bcc-num-dpz or
                     rf-bit-num-doc       not  = w-rig-bcc-num (1) or
                     rf-bit-cod-tmb       not  = w-rig-bcc-tip (1)
                     go to wrt-rec-bol-100.
      *                  *---------------------------------------------*
      *                  * Incremento progressivo bolla in fattura     *
      *                  *---------------------------------------------*
           if        w-agg-npr-bif        <    999
                     add   1              to   w-agg-npr-bif          .
      *                  *---------------------------------------------*
      *                  * Aggiornamento record con dati fattura       *
      *                  *---------------------------------------------*
           move      w-tes-dat-doc        to   rf-bit-fat-dat         .
           move      w-tes-num-doc        to   rf-bit-fat-num         .
           move      w-agg-npr-bif        to   rf-bit-fat-npb         .
      *                  *---------------------------------------------*
      *                  * Update record [bit]                         *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Release record [bit]                        *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Ad aggiornamento record [bir]               *
      *                  *---------------------------------------------*
           go to     wrt-rec-bol-200.
       wrt-rec-bol-100.
      *                  *---------------------------------------------*
      *                  * Se record [bit] non trovato                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Release record [bit]                    *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     wrt-rec-bol-999.
       wrt-rec-bol-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento righe bolla                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [bir]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-bit-num-prt       to   rf-bir-num-prt         .
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
                     go to wrt-rec-bol-999.
       wrt-rec-bol-300.
      *                  *---------------------------------------------*
      *                  * Ottenimento sequenziale record [bir]        *
      *                  *---------------------------------------------*
           move      "GN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to wrt-rec-bol-900.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-bir-num-prt       not  = rf-bit-num-prt
                     go to wrt-rec-bol-900.
      *                  *---------------------------------------------*
      *                  * Aggiornamento record con dati fattura       *
      *                  *---------------------------------------------*
           move      w-tes-dat-doc        to   rf-bir-fat-dat         .
           move      w-tes-num-doc        to   rf-bir-fat-num         .
           move      w-agg-npr-bif        to   rf-bir-fat-npb         .
      *                  *---------------------------------------------*
      *                  * Update record [bir]                         *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Release record [bir]                        *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Riciclo ottenimento sequenziale [bir]       *
      *                  *---------------------------------------------*
           go to     wrt-rec-bol-300.
       wrt-rec-bol-900.
      *              *-------------------------------------------------*
      *              * Rilascio dell'ultimo record [bir] ottenuto      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Release record [bir]                        *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
       wrt-rec-bol-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento records [bit] e [bir] per quanto riguarda i *
      *    * dati relativi alla fattura differita durante la fase di   *
      *    * Modifica di un record [fir]                               *
      *    *-----------------------------------------------------------*
       rew-rec-bol-000.
       rew-rec-bol-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento records [bit] e [bir] per quanto riguarda i *
      *    * dati relativi alla fattura differita durante la fase di   *
      *    * Cancellazione di un record [fir]                          *
      *    *-----------------------------------------------------------*
       del-rec-bol-000.
      *              *-------------------------------------------------*
      *              * Test se riga fattura che si riferisce ad una    *
      *              * bolla                                           *
      *              *-------------------------------------------------*
           if        w-rig-bcc-tip (1)    =    spaces and
                     w-rig-bcc-dat (1)    =    zero   and
                     w-rig-bcc-num (1)    =    zero
                     go to del-rec-bol-999.
      *              *-------------------------------------------------*
      *              * Test se la riga fattura si riferisce alla bolla *
      *              * gia' in esame                                   *
      *              *-------------------------------------------------*
           if        w-rig-bcc-tip (1)    =    w-agg-bcc-tip and
                     w-rig-bcc-dat (1)    =    w-agg-bcc-dat and
                     w-rig-bcc-num (1)    =    w-agg-bcc-num
                     go to del-rec-bol-999.
      *              *-------------------------------------------------*
      *              * Aggiornamento nuovi dati identificativi bolla   *
      *              * in corso                                        *
      *              *-------------------------------------------------*
           move      w-rig-bcc-tip (1)    to   w-agg-bcc-tip          .
           move      w-rig-bcc-dat (1)    to   w-agg-bcc-dat          .
           move      w-rig-bcc-num (1)    to   w-agg-bcc-num          .
      *              *-------------------------------------------------*
      *              * Aggiornamento testata bolla                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [bit]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-rig-bcc-dat (1)    to   rf-bit-dat-doc         .
           move      w-agg-bcc-num-dpz    to   rf-bit-cod-dpz         .
           move      w-rig-bcc-num (1)    to   rf-bit-num-doc         .
           move      w-rig-bcc-tip (1)    to   rf-bit-cod-tmb         .
           move      zero                 to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                      *-----------------------------------------*
      *                      * Se Start errata : uscita                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to del-rec-bol-999.
      *                  *---------------------------------------------*
      *                  * Ottenimento sequenziale record [bit]        *
      *                  *---------------------------------------------*
           move      "GN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                      *-----------------------------------------*
      *                      * Se 'At end' : uscita                    *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to del-rec-bol-100.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-bit-dat-doc       not  = w-rig-bcc-dat (1) or
                     rf-bit-cod-dpz       not  = w-agg-bcc-num-dpz or
                     rf-bit-num-doc       not  = w-rig-bcc-num (1) or
                     rf-bit-cod-tmb       not  = w-rig-bcc-tip (1)
                     go to del-rec-bol-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione dati bolla relativi alla    *
      *                  * fattura                                     *
      *                  *---------------------------------------------*
           move      zero                 to   rf-bit-fat-dat         .
           move      zero                 to   rf-bit-fat-num         .
           move      zero                 to   rf-bit-fat-npb         .
      *                  *---------------------------------------------*
      *                  * Update record [bit]                         *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Release record [bit]                        *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Ad aggiornamento record [bir]               *
      *                  *---------------------------------------------*
           go to     del-rec-bol-200.
       del-rec-bol-100.
      *                  *---------------------------------------------*
      *                  * Se record [bit] non trovato                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Release record [bit]                    *
      *                      *-----------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     del-rec-bol-999.
       del-rec-bol-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento righe bolla                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start su file [bir]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-bit-num-prt       to   rf-bir-num-prt         .
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
                     go to del-rec-bol-999.
       del-rec-bol-300.
      *                  *---------------------------------------------*
      *                  * Ottenimento sequenziale record [bir]        *
      *                  *---------------------------------------------*
           move      "GN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to del-rec-bol-900.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-bir-num-prt       not  = rf-bit-num-prt
                     go to del-rec-bol-900.
      *                  *---------------------------------------------*
      *                  * Normalizzazione dati bolla relativi alla    *
      *                  * fattura                                     *
      *                  *---------------------------------------------*
           move      zero                 to   rf-bir-fat-dat         .
           move      zero                 to   rf-bir-fat-num         .
           move      zero                 to   rf-bir-fat-npb         .
      *                  *---------------------------------------------*
      *                  * Update record [bir]                         *
      *                  *---------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Release record [bir]                        *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Riciclo ottenimento sequenziale [bir]       *
      *                  *---------------------------------------------*
           go to     del-rec-bol-300.
       del-rec-bol-900.
      *              *-------------------------------------------------*
      *              * Rilascio dell'ultimo record [bir] ottenuto      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Release record [bir]                        *
      *                  *---------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
       del-rec-bol-999.
           exit.

      *    *===========================================================*
      *    * Routine di decomposizione tipo riga                       *
      *    *                                                           *
      *    * Input  : rf-bir-tip-rig                                   *
      *    *                                                           *
      *    * Output : w-rig-tip-rig (1) e relativi valori di w-rig con-*
      *    *          nessi                                            *
      *    *-----------------------------------------------------------*
       dec-tip-rig-000.
      *              *-------------------------------------------------*
      *              * Tipo riga in comodo di lavoro ridefinito        *
      *              *-------------------------------------------------*
           move      rf-bir-tip-rig       to    w-dec-tip-rig-str     .
      *              *-------------------------------------------------*
      *              * Normalizzazione area di w-rig relativa al tipo  *
      *              * riga                                            *
      *              *-------------------------------------------------*
           move      spaces               to   w-rig-tip-rig (1)      .
           move      spaces               to   w-rig-tip-rig-tpr (1)  .
           move      spaces               to   w-rig-tip-rig-tfu (1)  .
           move      zero                 to   w-rig-tip-rig-cac (1)  .
           move      spaces               to   w-rig-tip-rig-ast (1)  .
           move      zero                 to   w-rig-tip-rig-tot (1)  .
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
      *                  *---------------------------------------------*
      *                  * Codice individuato                          *
      *                  *---------------------------------------------*
           move      w-dec-tip-rig-cod    to   w-rig-tip-rig-cac (1)  .
      *                  *---------------------------------------------*
      *                  * Lettura per determinare il tipo totale :    *
      *                  * solo per addebiti con codice diverso da     *
      *                  * zero                                        *
      *                  *---------------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "A"
                     go to dec-tip-rig-300.
           if        w-rig-tip-rig-cac (1)
                                          =    zero
                     go to dec-tip-rig-300.
      *                  *---------------------------------------------*
      *                  * Lettura                                     *
      *                  *---------------------------------------------*
           move      01                   to   w-let-arc-zac-tip      .
           move      w-rig-tip-rig-cac (1)
                                          to   w-let-arc-zac-cod      .
           perform   let-arc-zac-000      thru let-arc-zac-999        .
           move      w-let-arc-zac-tot    to   w-rig-tip-rig-tot (1)  .
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
      *    * Find su archivio [bit]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-bit-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-arc-bit-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pbol3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-bit-sel
                     go to  fnd-arc-bit-999.
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
           move      w-acc-cod-dpz        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo programma di interrogazione            *
      *              *-------------------------------------------------*
           move      "pgm/bol/prg/obj/pbol3010"
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
                     move  "#"            to   w-fnd-arc-bit-sel
                     go to fnd-arc-bit-999.
           move      s-alf                to   w-fnd-arc-bit-tmb      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dat-doc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-bit-sel
                     go to fnd-arc-bit-999.
           move      s-dat                to   w-fnd-arc-bit-dat      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-doc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-bit-sel
                     go to fnd-arc-bit-999.
           move      s-num                to   w-fnd-arc-bit-num      .
      *              *-------------------------------------------------*
      *              * Salvataggio valori chiave                       *
      *              *-------------------------------------------------*
           move      w-acc                to   w-sav-val-acc          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori estratti                 *
      *              *-------------------------------------------------*
           move      w-fnd-arc-bit-tmb    to   w-acc-cod-tmb          .
           move      w-fnd-arc-bit-dat    to   w-acc-dat-doc          .
           move      w-fnd-arc-bit-num    to   w-acc-num-doc          .
       fnd-arc-bit-400.
      *              *-------------------------------------------------*
      *              * Determinazione campi derivati e effettuazione   *
      *              * controlli come se fossero stati impostati       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zbi] generale             *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmb        to   w-let-arc-zbi-cod      .
           move      zero                 to   w-let-arc-zbi-dpz      .
           perform   let-arc-zbi-000      thru let-arc-zbi-999        .
           move      w-let-arc-zbi-des    to   w-acc-cod-tmb-des      .
      *                  *---------------------------------------------*
      *                  * Se codice errato : uscita con errore        *
      *                  *---------------------------------------------*
           if        w-let-arc-zbi-flg    not  = spaces
                     go to fnd-arc-bit-900.
      *                  *---------------------------------------------*
      *                  * Se a spaces : uscita con errore             *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmb        =    spaces
                     go to fnd-arc-bit-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento                                   *
      *                  *---------------------------------------------*
           move      w-let-arc-zbi-mif    to   w-acc-cod-tmb-mif      .
           move      w-let-arc-zbi-tmf    to   w-acc-cod-tmb-tmf      .
           move      w-let-arc-zbi-tm2    to   w-acc-cod-tmb-tm2      .
           move      w-let-arc-zbi-acm    to   w-acc-cod-tmb-acm      .
           move      w-let-arc-zbi-dtc    to   w-acc-cod-tmb-dtc      .
           move      w-let-arc-zbi-dct    to   w-acc-cod-tmb-dct      .
           move      w-let-arc-zbi-cam    to   w-acc-cod-tmb-cam      .
           move      w-let-arc-zbi-ctm    to   w-acc-cod-tmb-ctm      .
           move      w-let-arc-zbi-dfa    to   w-acc-cod-tmb-dfa      .
           move      w-let-arc-zbi-vaa    to   w-acc-cod-tmb-vaa      .
           move      w-let-arc-zbi-lsa    to   w-acc-cod-tmb-lsa      .
           move      w-let-arc-zbi-ord    to   w-acc-cod-tmb-ord      .
           move      w-let-arc-zbi-prd    to   w-acc-cod-tmb-prd      .
           move      w-let-arc-zbi-sgl    to   w-acc-cod-tmb-sgl      .
           move      w-let-arc-zbi-maf    to   w-acc-cod-tmb-maf      .
           move      w-let-arc-zbi-dmf    to   w-acc-cod-tmb-dmf      .
           move      w-let-arc-zbi-tvd    to   w-acc-cod-tmb-tvd      .
           move      w-let-arc-zbi-dsl    to   w-acc-cod-tmb-ddd      .
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zbi] relativo alla dipen- *
      *                  * denza                                       *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmb        to   w-let-arc-zbi-cod      .
           move      w-acc-cod-dpz        to   w-let-arc-zbi-dpz      .
           perform   let-arc-zbi-000      thru let-arc-zbi-999        .
       fnd-arc-bit-410.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo validita'   *
      *                  * per le dipendenze                           *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmb-tvd    =    spaces
                     go to fnd-arc-bit-411
           else if   w-acc-cod-tmb-tvd    =    "S"
                     go to fnd-arc-bit-412
           else if   w-acc-cod-tmb-tvd    =    "D"
                     go to fnd-arc-bit-413
           else if   w-acc-cod-tmb-tvd    =    "X"
                     go to fnd-arc-bit-414.
       fnd-arc-bit-411.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Tutte    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     fnd-arc-bit-420.
       fnd-arc-bit-412.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo la  *
      *                  * Sede                                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-acc-cod-dpz       not  = 1
                     go to fnd-arc-bit-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     fnd-arc-bit-420.
       fnd-arc-bit-413.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su codice dipendenza in uso        *
      *                      *-----------------------------------------*
           if        w-acc-cod-dpz       =    1
                     go to fnd-arc-bit-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     fnd-arc-bit-420.
       fnd-arc-bit-414.
      *                  *---------------------------------------------*
      *                  * Tipo validita' per le dipendenze : Solo le  *
      *                  * Dipendenze indicate                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se record esistente                *
      *                      *-----------------------------------------*
           if        w-let-arc-zbi-flg    not  = spaces
                     go to fnd-arc-bit-418.
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     fnd-arc-bit-420.
       fnd-arc-bit-418.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Tipo movimento incompatibile con la dipendenza in 
      -              "uso !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A uscita con errore                         *
      *                  *---------------------------------------------*
           go to     fnd-arc-bit-900.
       fnd-arc-bit-420.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione codice dislocazione         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se record esistente si bufferizza il    *
      *                      * codice dislocazione in esso contenuto,  *
      *                      * altrimenti si bufferizza quello di de-  *
      *                      * fault                                   *
      *                      *-----------------------------------------*
           if        w-let-arc-zbi-flg    =    spaces
                     move  w-let-arc-zbi-dsl
                                          to   w-acc-cod-tmb-dsl
           else      move  w-acc-cod-tmb-ddd
                                          to   w-acc-cod-tmb-dsl      .
      *                  *---------------------------------------------*
      *                  * Test se tipo movimento per fattura accompa- *
      *                  * gnatoria                                    *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmb-mif    not  = 03
                     go to fnd-arc-bit-440.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento non fatturabile in quanto gia' fatt
      -              "ura !          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     fnd-arc-bit-900.
       fnd-arc-bit-440.
      *                  *---------------------------------------------*
      *                  * Test che tra i tipi archivio ammessi per il *
      *                  * movimento ci sia l'archivio clienti         *
      *                  *---------------------------------------------*
           move      zero                 to   w-acc-cod-tmb-inx      .
           inspect   w-acc-cod-tmb-lsa
                                      tallying w-acc-cod-tmb-inx
                                          for  all "C"                .
      *                      *-----------------------------------------*
      *                      * Se archivio clienti e' tra quelli am-   *
      *                      * messi per il tipo movimento : oltre     *
      *                      *-----------------------------------------*
           if        w-acc-cod-tmb-inx    >    zero
                     go to fnd-arc-bit-460.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Il tipo movimento non riguarda l'archivio clienti 
      -              "!              "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     fnd-arc-bit-900.
       fnd-arc-bit-460.
      *                  *---------------------------------------------*
      *                  * Se movimento che interessa la fatturazione, *
      *                  * controllo che sia compatibile con quello    *
      *                  * impostato a inizio fattura                  *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmb-mif    not  = 02
                     go to fnd-arc-bit-600.
           if        w-tes-tip-mov-ord    =    11
                     go to fnd-arc-bit-600.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Tipo movimento per fatturazione associato incompat
      -              "ibile          "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A uscita con errore                     *
      *                      *-----------------------------------------*
           go to     fnd-arc-bit-900.
       fnd-arc-bit-600.
      *              *-------------------------------------------------*
      *              * Completamento visualizzazione campi di accet-   *
      *              * tazione                                         *
      *              *-------------------------------------------------*
           perform   vis-cod-tmb-000      thru vis-cod-tmb-999        .
           perform   vis-cod-tmb-des-000  thru vis-cod-tmb-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *              *-------------------------------------------------*
      *              * Uscita con successo                             *
      *              *-------------------------------------------------*
           go to     fnd-arc-bit-999.
       fnd-arc-bit-900.
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
           move      "#"                  to   w-fnd-arc-bit-sel      .
       fnd-arc-bit-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zbi]                         *
      *    *-----------------------------------------------------------*
       let-arc-zbi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zbi-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zbi-cod    =    spaces
                     go to let-arc-zbi-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMB"             to   f-key                  .
           move      w-let-arc-zbi-cod    to   rf-zbi-cod-tmb         .
           move      w-let-arc-zbi-dpz    to   rf-zbi-cod-dpz         .
           move      "pgm/bol/fls/ioc/obj/iofzbi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbi                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zbi-400.
       let-arc-zbi-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zbi-des-tmb       to   w-let-arc-zbi-des      .
           move      rf-zbi-int-ftr       to   w-let-arc-zbi-mif      .
           move      rf-zbi-tmo-ftr       to   w-let-arc-zbi-tmf      .
           move      rf-zbi-snx-acm       to   w-let-arc-zbi-acm      .
           move      rf-zbi-def-tac       to   w-let-arc-zbi-dtc      .
           move      rf-zbi-def-ctr       to   w-let-arc-zbi-dct      .
           move      rf-zbi-cau-mag       to   w-let-arc-zbi-cam      .
           move      rf-zbi-cod-mic       to   w-let-arc-zbi-ctm      .
           move      rf-zbi-def-tar       to   w-let-arc-zbi-dfa      .
           move      rf-zbi-snv-tar       to   w-let-arc-zbi-vaa      .
           move      rf-zbi-lst-tar       to   w-let-arc-zbi-lsa      .
           move      rf-zbi-org-doc       to   w-let-arc-zbi-ord      .
           move      rf-zbi-prv-doc       to   w-let-arc-zbi-prd      .
           move      rf-zbi-sgl-num       to   w-let-arc-zbi-sgl      .
           move      rf-zbi-mov-afd       to   w-let-arc-zbi-maf      .
           move      rf-zbi-def-tmf       to   w-let-arc-zbi-dmf      .
           move      rf-zbi-vld-dpz       to   w-let-arc-zbi-tvd      .
           move      rf-zbi-cod-dsl       to   w-let-arc-zbi-dsl      .
           move      rf-zbi-tmo-ft2       to   w-let-arc-zbi-tm2      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zbi-999.
       let-arc-zbi-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zbi-flg      .
           move      all   "."            to   w-let-arc-zbi-des      .
           go to     let-arc-zbi-600.
       let-arc-zbi-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zbi-des      .
       let-arc-zbi-600.
           move      zero                 to   w-let-arc-zbi-mif      .
           move      spaces               to   w-let-arc-zbi-tmf      .
           move      spaces               to   w-let-arc-zbi-acm      .
           move      spaces               to   w-let-arc-zbi-dtc      .
           move      spaces               to   w-let-arc-zbi-dct      .
           move      zero                 to   w-let-arc-zbi-cam      .
           move      spaces               to   w-let-arc-zbi-ctm      .
           move      spaces               to   w-let-arc-zbi-dfa      .
           move      spaces               to   w-let-arc-zbi-vaa      .
           move      spaces               to   w-let-arc-zbi-lsa      .
           move      zero                 to   w-let-arc-zbi-ord      .
           move      zero                 to   w-let-arc-zbi-prd      .
           move      spaces               to   w-let-arc-zbi-sgl      .
           move      zero                 to   w-let-arc-zbi-maf      .
           move      spaces               to   w-let-arc-zbi-dmf      .
           move      spaces               to   w-let-arc-zbi-tvd      .
           move      spaces               to   w-let-arc-zbi-dsl      .
           move      spaces               to   w-let-arc-zbi-tm2      .
       let-arc-zbi-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [cli]                         *
      *    *-----------------------------------------------------------*
       let-arc-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-cli-cod    =    zero
                     go to let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-arc-cli-cod    to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-cli-400.
       let-arc-cli-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-cli-rag-soc       to   w-let-arc-cli-rag      .
           move      rf-cli-via-cli       to   w-let-arc-cli-via      .
           move      rf-cli-loc-cli       to   w-let-arc-cli-loc      .
           move      rf-cli-prt-iva       to   w-let-arc-cli-piv      .
           move      rf-cli-cod-cge       to   w-let-arc-cli-stc      .
           move      rf-cli-cod-iva       to   w-let-arc-cli-ass      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-cli-999.
       let-arc-cli-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-cli-flg      .
           move      all   "."            to   w-let-arc-cli-rag      .
           go to     let-arc-cli-520.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-520.
           move      spaces               to   w-let-arc-cli-via      .
           move      spaces               to   w-let-arc-cli-loc      .
           move      zero                 to   w-let-arc-cli-piv      .
           move      zero                 to   w-let-arc-cli-stc      .
           move      zero                 to   w-let-arc-cli-ass      .
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zci]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [dcc]                         *
      *    *-----------------------------------------------------------*
       let-arc-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici a zero o spazi                   *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-cli    =    zero
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      w-let-arc-dcc-cli    to   rf-dcc-cod-cli         .
           move      w-let-arc-dcc-dpz    to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcc-400.
       let-arc-dcc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcc-rag-soc       to   w-let-arc-dcc-rag      .
           move      rf-dcc-via-dcc       to   w-let-arc-dcc-via      .
           move      rf-dcc-loc-dcc       to   w-let-arc-dcc-loc      .
           move      rf-dcc-cod-vlt       to   w-let-arc-dcc-vlt      .
           move      rf-dcc-cod-lng       to   w-let-arc-dcc-lng      .
           move      rf-dcc-tas-ivc       to   w-let-arc-dcc-tai      .
           move      rf-dcc-fco-blo       to   w-let-arc-dcc-blo      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcc-999.
       let-arc-dcc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcc-flg      .
           move      all   "."            to   w-let-arc-dcc-rag      .
           go to     let-arc-dcc-520.
       let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-rag      .
       let-arc-dcc-520.
           move      spaces               to   w-let-arc-dcc-via      .
           move      spaces               to   w-let-arc-dcc-loc      .
           move      spaces               to   w-let-arc-dcc-vlt      .
           move      spaces               to   w-let-arc-dcc-lng      .
           move      zero                 to   w-let-arc-dcc-tai      .
           move      spaces               to   w-let-arc-dcc-blo      .
       let-arc-dcc-999.
           exit.

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
      *    * Routine di lettura archivio [zvf]                         *
      *    *-----------------------------------------------------------*
       let-arc-zvf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvf-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-zvf-num    =    zero or
                     w-let-arc-zvf-cod    =    spaces
                     go to let-arc-zvf-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVDF"             to   f-key                  .
           move      w-let-arc-zvf-num    to   rf-zvf-num-def         .
           move      w-let-arc-zvf-cod    to   rf-zvf-cod-def         .
           move      "I  "                to   rf-zvf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zvf-400.
       let-arc-zvf-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zvf-des-stp       to   w-let-arc-zvf-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zvf-999.
       let-arc-zvf-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zvf-flg      .
           move      all   "."            to   w-let-arc-zvf-des      .
           go to     let-arc-zvf-999.
       let-arc-zvf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zvf-des      .
       let-arc-zvf-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zls]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/larczls0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [zcs]                         *
      *    *-----------------------------------------------------------*
       let-arc-zcs-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcs-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice sconto a zero                    *
      *              *-------------------------------------------------*
           if        w-let-arc-zcs-cod    =    zero
                     go to let-arc-zcs-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCSC    "         to   f-key                  .
           move      w-let-arc-zcs-tip    to   rf-zcs-tip-csc         .
           move      w-let-arc-zcs-cod    to   rf-zcs-cod-csc         .
           move      "pgm/dcc/fls/ioc/obj/iofzcs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zcs                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zcs-400.
       let-arc-zcs-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zcs-des-csc       to   w-let-arc-zcs-des      .
           move      rf-zcs-per-sco (1)   to   w-let-arc-zcs-per (1)  .
           move      rf-zcs-per-sco (2)   to   w-let-arc-zcs-per (2)  .
           move      rf-zcs-per-sco (3)   to   w-let-arc-zcs-per (3)  .
           move      rf-zcs-per-sco (4)   to   w-let-arc-zcs-per (4)  .
           move      rf-zcs-per-sco (5)   to   w-let-arc-zcs-per (5)  .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zcs-999.
       let-arc-zcs-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zcs-flg      .
           move      all   "."            to   w-let-arc-zcs-des      .
           go to     let-arc-zcs-520.
       let-arc-zcs-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zcs-des      .
       let-arc-zcs-520.
           move      zero                 to   w-let-arc-zcs-per (1)  .
           move      zero                 to   w-let-arc-zcs-per (2)  .
           move      zero                 to   w-let-arc-zcs-per (3)  .
           move      zero                 to   w-let-arc-zcs-per (4)  .
           move      zero                 to   w-let-arc-zcs-per (5)  .
       let-arc-zcs-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [age]                         *
      *    *-----------------------------------------------------------*
       let-arc-age-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-age-cod    =    zero
                     go to let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAGE"             to   f-key                  .
           move      w-let-arc-age-cod    to   rf-age-cod-age         .
           move      "pgm/age/fls/ioc/obj/iofage"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-age                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-age-400.
       let-arc-age-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-age-rag-soc       to   w-let-arc-age-rag      .
           move      rf-age-sup-age       to   w-let-arc-age-spa      .
           move      rf-age-cat-pvg       to   w-let-arc-age-cpv      .
           move      rf-age-per-pvg (1)   to   w-let-arc-age-ppv (1)  .
           move      rf-age-per-pvg (2)   to   w-let-arc-age-ppv (2)  .
           move      rf-age-per-pvg (3)   to   w-let-arc-age-ppv (3)  .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-age-999.
       let-arc-age-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-age-flg      .
           move      all   "."            to   w-let-arc-age-rag      .
           go to     let-arc-age-600.
       let-arc-age-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-age-rag      .
       let-arc-age-600.
           move      zero                 to   w-let-arc-age-spa      .
           move      zero                 to   w-let-arc-age-cpv      .
           move      zero                 to   w-let-arc-age-ppv (1)  .
           move      zero                 to   w-let-arc-age-ppv (2)  .
           move      zero                 to   w-let-arc-age-ppv (3)  .
       let-arc-age-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zfp]                         *
      *    *-----------------------------------------------------------*
       let-arc-zfp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice istituto a zero                  *
      *              *-------------------------------------------------*
           if        w-let-arc-zfp-cod    =    zero
                     go to let-arc-zfp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFOP"             to   f-key                  .
           move      w-let-arc-zfp-cod    to   rf-zfp-cod-fop         .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zfp-400.
       let-arc-zfp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zfp-des-fop       to   w-let-arc-zfp-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zfp-999.
       let-arc-zfp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zfp-flg      .
           move      all   "."            to   w-let-arc-zfp-des      .
           go to     let-arc-zfp-999.
       let-arc-zfp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfp-des      .
       let-arc-zfp-999.
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
      *    * Routine di lettura archivio [zin]                         *
      *    *-----------------------------------------------------------*
       let-arc-zin-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zin-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zin-cod    =    spaces
                     go to let-arc-zin-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPI    "         to   f-key                  .
           move      w-let-arc-zin-cod    to   rf-zin-cod-spi         .
           move      w-let-arc-zin-tpg    to   rf-zin-tip-pag         .
           move      "pgm/dcc/fls/ioc/obj/iofzin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zin                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zin-400.
       let-arc-zin-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zin-des-spi       to   w-let-arc-zin-des      .
           move      rf-zin-civ-spi       to   w-let-arc-zin-coi      .
           move      rf-zin-ccp-spi       to   w-let-arc-zin-ccp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zin-999.
       let-arc-zin-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zin-flg      .
           move      all   "."            to   w-let-arc-zin-des      .
           go to     let-arc-zin-520.
       let-arc-zin-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zin-des      .
       let-arc-zin-520.
           move      zero                 to   w-let-arc-zin-coi      .
           move      zero                 to   w-let-arc-zin-ccp      .
       let-arc-zin-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zbo]                         *
      *    *-----------------------------------------------------------*
       let-arc-zbo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zbo-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zbo-cod    =    spaces
                     go to let-arc-zbo-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPB    "         to   f-key                  .
           move      w-let-arc-zbo-cod    to   rf-zbo-cod-spb         .
           move      w-let-arc-zbo-tpg    to   rf-zbo-tip-pag         .
           move      "pgm/dcc/fls/ioc/obj/iofzbo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbo                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zbo-400.
       let-arc-zbo-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zbo-des-spb       to   w-let-arc-zbo-des      .
           move      rf-zbo-civ-spb       to   w-let-arc-zbo-coi      .
           move      rf-zbo-ccp-spb       to   w-let-arc-zbo-ccp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zbo-999.
       let-arc-zbo-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zbo-flg      .
           move      all   "."            to   w-let-arc-zbo-des      .
           go to     let-arc-zbo-520.
       let-arc-zbo-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zbo-des      .
       let-arc-zbo-520.
           move      zero                 to   w-let-arc-zbo-coi      .
           move      zero                 to   w-let-arc-zbo-ccp      .
       let-arc-zbo-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.lts"                   .

      *    *===========================================================*
      *    * Routine di lettura archivio [bix]                         *
      *    *-----------------------------------------------------------*
       let-arc-bix-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-bix-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici nulli                            *
      *              *-------------------------------------------------*
           if        w-let-arc-bix-prt    =    zero   or
                     w-let-arc-bix-prg    =    zero   or
                     w-let-arc-bix-trc    =    zero
                     go to let-arc-bix-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-let-arc-bix-prt    to   rf-bix-num-prt         .
           move      w-let-arc-bix-prg    to   rf-bix-num-prg         .
           move      w-let-arc-bix-trc    to   rf-bix-tip-rec         .
           move      "pgm/bol/fls/ioc/obj/iofbix"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bix                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-bix-400.
       let-arc-bix-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-bix-des-400       to   w-let-arc-bix-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-bix-999.
       let-arc-bix-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-bix-flg      .
           move      spaces               to   w-let-arc-bix-des      .
           move      all   "."            to   w-let-arc-bix-drg (1)  .
           go to     let-arc-bix-999.
       let-arc-bix-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-bix-des      .
       let-arc-bix-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [zac]                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/larczac0.lts"                   .

      *    *===========================================================*
      *    * Determinazione se documento gia' esistente oppure no      *
      *    *-----------------------------------------------------------*
       det-doc-ges-000.
      *              *-------------------------------------------------*
      *              * Tipo movimento trovato per il caso che la de-   *
      *              * terminazione dia esito pari a 'X' : a spaces    *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-doc-ges-tmt      .
      *              *-------------------------------------------------*
      *              * Start su file [bit]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-acc-dat-doc        to   rf-bit-dat-doc         .
           move      w-acc-cod-dpz        to   rf-bit-cod-dpz         .
           move      w-acc-num-doc        to   rf-bit-num-doc         .
           move      spaces               to   rf-bit-cod-tmb         .
           move      zero                 to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a documento non esistente     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-doc-ges-900.
       det-doc-ges-100.
      *              *-------------------------------------------------*
      *              * Next su [bit]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * Se 'at end' : a determinazione finale           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-doc-ges-800.
      *              *-------------------------------------------------*
      *              * Max su [bit], se non superato : a determinazio- *
      *              * ne finale                                       *
      *              *-------------------------------------------------*
           if        rf-bit-dat-doc       not  = w-acc-dat-doc or
                     rf-bit-cod-dpz       not  = w-acc-cod-dpz or
                     rf-bit-num-doc       not  = w-acc-num-doc
                     go to det-doc-ges-800.
       det-doc-ges-200.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo movimento letto   *
      *              *-------------------------------------------------*
           if        rf-bit-cod-tmb       =    w-acc-cod-tmb
                     go to det-doc-ges-300
           else      go to det-doc-ges-400.
       det-doc-ges-300.
      *              *-------------------------------------------------*
      *              * Se tipo movimento letto pari al tipo movimento  *
      *              * da cercare                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di documento esistente e trovato    *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-doc-ges-snx      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-doc-ges-999.
       det-doc-ges-400.
      *              *-------------------------------------------------*
      *              * Se tipo movimento letto diverso dal tipo movi-  *
      *              * mento da cercare                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se sigla numerazione per il tipo movimento  *
      *                  * letto diversa da sigla numerazione del ti-  *
      *                  * po movimento cercato : si ignora il docu-   *
      *                  * mento e si passa ad esaminare il documento  *
      *                  * successivo                                  *
      *                  *---------------------------------------------*
           if        rf-bit-sgl-num       not  = w-acc-cod-tmb-sgl
                     go to det-doc-ges-100.
      *                  *---------------------------------------------*
      *                  * Se invece la sigla numerazione e' la stessa *
      *                  * si memorizza il codice tipo movimento letto *
      *                  * per il caso in cui la determinazione doves- *
      *                  * se dare esito pari a 'X'                    *
      *                  *---------------------------------------------*
           move      rf-bit-cod-tmb       to   w-det-doc-ges-tmt      .
      *                  *---------------------------------------------*
      *                  * Quindi si ricicla per esaminare eventuali   *
      *                  * documenti successivi                        *
      *                  *---------------------------------------------*
           go to     det-doc-ges-100.
       det-doc-ges-800.
      *              *-------------------------------------------------*
      *              * Determinazioni finali per il caso in cui il do- *
      *              * cumento non sia stato sicuramente trovato       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se durante la ricerca e' stato trovato un   *
      *                  * documento con la data cercata, il numero    *
      *                  * cercato, e la sigla numerazione cercata :   *
      *                  * si esce con 'X', e con il codice del tipo   *
      *                  * movimento trovato gia' preparato; altri-    *
      *                  * menti si esce per documento non trovato     *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-tmt    not  = spaces
                     move  "X"            to   w-det-doc-ges-snx
                     go to det-doc-ges-999
           else      go to det-doc-ges-900.
       det-doc-ges-900.
      *              *-------------------------------------------------*
      *              * Uscita per documento non trovato                *
      *              *-------------------------------------------------*
           move      "N"                  to   w-det-doc-ges-snx      .
       det-doc-ges-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valori testata indotti da record [dcc]     *
      *    * principale                                                *
      *    *-----------------------------------------------------------*
       det-vlt-dcc-000.
      *              *-------------------------------------------------*
      *              * Si/No gestione legame valutario ed eventuale    *
      *              * momento di applicazione cambio                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione e tipo fornitura tramite Leasing: *
      *                  * rimane quanto e' stato determinato per il   *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F" and
                     w-tes-tip-frn (1)    =    11
                     go to det-vlt-dcc-005.
      *                  *---------------------------------------------*
      *                  * Se No gestione legame valutario : oltre     *
      *                  *---------------------------------------------*
           if        w-prs-ges-lvl        =    00
                     go to det-vlt-dcc-005.
      *                  *---------------------------------------------*
      *                  * Se no rapporti con legame valutario con il  *
      *                  * cliente : oltre                             *
      *                  *---------------------------------------------*
           if        rf-dcc-snx-rlv        not  = "S"
                     go to det-vlt-dcc-005.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore contenuto   *
      *                  * nel record [dcc]                            *
      *                  *---------------------------------------------*
           if        rf-dcc-mom-alv       =    01
                     go to det-vlt-dcc-001
           else if   rf-dcc-mom-alv       =    11
                     go to det-vlt-dcc-002
           else if   rf-dcc-mom-alv       =    21
                     go to det-vlt-dcc-003
           else      go to det-vlt-dcc-001.
       det-vlt-dcc-001.
      *                  *---------------------------------------------*
      *                  * Se applicazione cambio : Standard           *
      *                  *---------------------------------------------*
           if        w-prs-ges-lvl        =    11
                     move  11             to   w-tes-mac-lvl (1)
           else if   w-prs-ges-lvl        =    21
                     move  21             to   w-tes-mac-lvl (1)      .
           go to     det-vlt-dcc-005.
       det-vlt-dcc-002.
      *                  *---------------------------------------------*
      *                  * Se applicazione cambio : in bolla           *
      *                  *---------------------------------------------*
           move      11                   to   w-tes-mac-lvl (1)      .
           go to     det-vlt-dcc-005.
       det-vlt-dcc-003.
      *                  *---------------------------------------------*
      *                  * Se applicazione cambio : in fattura         *
      *                  *---------------------------------------------*
           move      21                   to   w-tes-mac-lvl (1)      .
           go to     det-vlt-dcc-005.
       det-vlt-dcc-005.
      *              *-------------------------------------------------*
      *              * Tipo esposizione prezzi e sconti in riga docu-  *
      *              * mento                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione e tipo fornitura tramite Leasing: *
      *                  * rimane quanto e' stato determinato per il   *
      *                  * Cliente Commerciale                         *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F" and
                     w-tes-tip-frn (1)    =    11
                     go to det-vlt-dcc-010.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda del valore contenuto   *
      *                  * nel record [dcc]                            *
      *                  *---------------------------------------------*
           if        rf-dcc-epz-pes       =    01
                     go to det-vlt-dcc-006
           else if   rf-dcc-epz-pes       =    11
                     go to det-vlt-dcc-007
           else if   rf-dcc-epz-pes       =    21
                     go to det-vlt-dcc-008
           else      go to det-vlt-dcc-006.
       det-vlt-dcc-006.
      *                  *---------------------------------------------*
      *                  * Se tipo esposizione : Standard              *
      *                  *---------------------------------------------*
           if        w-prs-epz-pes        =    11
                     move  11             to   w-tes-epz-pes (1)
           else if   w-prs-epz-pes        =    21
                     move  21             to   w-tes-epz-pes (1)      .
           go to     det-vlt-dcc-010.
       det-vlt-dcc-007.
      *                  *---------------------------------------------*
      *                  * Se tipo esposizione: Prezzo e sconti        *
      *                  *---------------------------------------------*
           move      11                   to   w-tes-epz-pes (1)      .
           go to     det-vlt-dcc-010.
       det-vlt-dcc-008.
      *                  *---------------------------------------------*
      *                  * Se tipo esposizione: Prezzo netto           *
      *                  *---------------------------------------------*
           move      21                   to   w-tes-epz-pes (1)      .
           go to     det-vlt-dcc-010.
       det-vlt-dcc-010.
      *              *-------------------------------------------------*
      *              * Inoltro documenti                               *
      *              *-------------------------------------------------*
           move      rf-dcc-inl-dcm       to   w-tes-inl-dcm (1)      .
       det-vlt-dcc-015.
      *              *-------------------------------------------------*
      *              * Codice e descrizione nostra banca per bonifico  *
      *              * associati al cliente                            *
      *              *-------------------------------------------------*
           move      rf-dcc-nos-ban       to   w-tes-nsb-aac (1)      .
           move      02                   to   w-let-arc-cbp-tip      .
           move      w-tes-nsb-aac (1)    to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
           move      w-let-arc-cbp-des    to   w-tes-nsb-aac-des (1)  .
       det-vlt-dcc-020.
      *              *-------------------------------------------------*
      *              * Codice e descrizione nostro c/c postale per pa- *
      *              * gamento associati al cliente                    *
      *              *-------------------------------------------------*
           move      rf-dcc-nos-ccp       to   w-tes-ncp-aac (1)      .
           move      03                   to   w-let-arc-cbp-tip      .
           move      w-tes-ncp-aac (1)    to   w-let-arc-cbp-cod      .
           perform   let-arc-cbp-000      thru let-arc-cbp-999        .
           move      w-let-arc-cbp-des    to   w-tes-ncp-aac-des (1)  .
       det-vlt-dcc-030.
      *              *-------------------------------------------------*
      *              * Codice e descrizione ABI del cliente            *
      *              *-------------------------------------------------*
           move      rf-dcc-cod-abi       to   w-tes-abi-cli (1)      .
           move      w-tes-abi-cli (1)    to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           move      w-let-arc-axi-den    to   w-tes-abi-cli-den (1)  .
       det-vlt-dcc-040.
      *              *-------------------------------------------------*
      *              * Codice e descrizione CAB del cliente            *
      *              *-------------------------------------------------*
           move      rf-dcc-cod-cab       to   w-tes-cab-cli (1)      .
           move      w-tes-abi-cli (1)    to   w-let-arc-axs-abi      .
           move      w-tes-cab-cli (1)    to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           move      w-let-arc-axs-den    to   w-tes-cab-cli-den (1)  .
       det-vlt-dcc-050.
      *              *-------------------------------------------------*
      *              * C/c appoggio del cliente                        *
      *              *-------------------------------------------------*
           move      rf-dcc-ccc-app       to   w-tes-cca-cli (1)      .
       det-vlt-dcc-060.
      *              *-------------------------------------------------*
      *              * Codice e descrizione agente associato al clien- *
      *              * te                                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione e tipo fornitura tramite Gruppo   *
      *                  * d'Acquisto e codice agente esistente : ri-  *
      *                  * mane quanto e' stato determinato per il     *
      *                  * Cliente Commerciale                         *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcc-tdt    =    "F"    and
                     w-tes-tip-frn (1)    =    21     and
                     w-tes-age-aac (1)    not  = zero
                     go to det-vlt-dcc-070.
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-age       to   w-tes-age-aac (1)      .
       det-vlt-dcc-070.
      *              *-------------------------------------------------*
      *              * Altri valori                                    *
      *              *-------------------------------------------------*
           move      rf-dcc-tip-esm       to   w-tes-tip-esm (1)      .
           move      rf-dcc-ggg-alt       to   w-tes-ggg-alt (1)      .
           move      rf-dcc-mmm-e01       to   w-tes-mmm-e01 (1)      .
           move      rf-dcc-mmm-e02       to   w-tes-mmm-e02 (1)      .
       det-vlt-dcc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione funzionamento spese in fattura             *
      *    *-----------------------------------------------------------*
       det-fun-spe-000.
      *              *-------------------------------------------------*
      *              * Se tipo determinazione per cliente per fattura- *
      *              * zione e tipo fornitura tramite Leasing : rimane *
      *              * quanto e' stato determinato per il Cliente Com- *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           if        w-det-fun-spe-tdt    =    "F" and
                     w-tes-tip-frn (1)    =    11
                     go to det-fun-spe-999.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione su tabella personalizzazioni *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-fun-spe-ctr      .
       det-fun-spe-100.
           add       1                    to   w-det-fun-spe-ctr      .
           if        w-det-fun-spe-ctr    >    w-prs-spe-fat-nst
                     go to det-fun-spe-999.
      *              *-------------------------------------------------*
      *              * Preparazione indice per tabella                 *
      *              *-------------------------------------------------*
           move      w-prs-spe-fat-npt
                    (w-det-fun-spe-ctr)   to   w-det-fun-spe-inx      .
      *              *-------------------------------------------------*
      *              * Si/No addebito                                  *
      *              *-------------------------------------------------*
           if        rf-dcc-snm-spe
                    (w-det-fun-spe-inx)   =    zero or
                     rf-dcc-snm-spe
                    (w-det-fun-spe-inx)   =    01
                     move  0              to   w-pie-spe-snx
                                              (1, w-det-fun-spe-inx)
           else      move  1              to   w-pie-spe-snx
                                              (1, w-det-fun-spe-inx)  .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo funzionamento   *
      *              * derivato dalle personalizzazioni                *
      *              *-------------------------------------------------*
           if        w-prs-spe-fat-tfs
                    (w-det-fun-spe-ctr)   =    01
                     go to det-fun-spe-110
           else if   w-prs-spe-fat-tfs
                    (w-det-fun-spe-ctr)   =    02
                     go to det-fun-spe-120
           else if   w-prs-spe-fat-tfs
                    (w-det-fun-spe-ctr)   =    03
                     go to det-fun-spe-130
           else if   w-prs-spe-fat-tfs
                    (w-det-fun-spe-ctr)   =    04
                     go to det-fun-spe-140
           else if   w-prs-spe-fat-tfs
                    (w-det-fun-spe-ctr)   =    05
                     go to det-fun-spe-150.
       det-fun-spe-110.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : % fissa                    *
      *              *-------------------------------------------------*
           move      1                    to   w-pie-spe-mad
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-per
                    (w-det-fun-spe-ctr)   to   w-pie-spe-per
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibl
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibl
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibt
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibt
                                              (1, w-det-fun-spe-inx)  .
           move      zero                 to   w-pie-spe-imp
                                              (1, w-det-fun-spe-inx)  .
           go to     det-fun-spe-200.
       det-fun-spe-120.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : % variabile                *
      *              *-------------------------------------------------*
           move      2                    to   w-pie-spe-mad
                                              (1, w-det-fun-spe-inx)  .
           move      rf-dcc-per-spe
                    (w-det-fun-spe-inx)   to   w-pie-spe-per
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibl
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibl
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibt
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibt
                                              (1, w-det-fun-spe-inx)  .
           move      zero                 to   w-pie-spe-imp
                                              (1, w-det-fun-spe-inx)  .
           go to     det-fun-spe-200.
       det-fun-spe-130.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : importo fisso              *
      *              *-------------------------------------------------*
           move      3                    to   w-pie-spe-mad
                                              (1, w-det-fun-spe-inx)  .
           move      zero                 to   w-pie-spe-per
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibl
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibl
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibt
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibt
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-imp
                    (w-det-fun-spe-ctr)   to   w-pie-spe-imp
                                              (1, w-det-fun-spe-inx)  .
           go to     det-fun-spe-200.
       det-fun-spe-140.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : importo variabile          *
      *              *-------------------------------------------------*
           move      4                    to   w-pie-spe-mad
                                              (1, w-det-fun-spe-inx)  .
           move      zero                 to   w-pie-spe-per
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibl
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibl
                                              (1, w-det-fun-spe-inx)  .
           move      w-prs-spe-fat-ibt
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ibt
                                              (1, w-det-fun-spe-inx)  .
           move      rf-dcc-imp-spe
                    (w-det-fun-spe-inx)   to   w-pie-spe-imp
                                              (1, w-det-fun-spe-inx)  .
           go to     det-fun-spe-200.
       det-fun-spe-150.
      *              *-------------------------------------------------*
      *              * Tipo funzionamento : secondo [dcc]              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo funziona-   *
      *                  * mento espresso in [dcc]                     *
      *                  *---------------------------------------------*
           if        rf-dcc-snm-spe
                    (w-det-fun-spe-inx)   =    03
                     go to det-fun-spe-151
           else if   rf-dcc-snm-spe
                    (w-det-fun-spe-inx)   =    04
                     go to det-fun-spe-152.
       det-fun-spe-151.
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento espresso in [dcc] : a %  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Come per tipo funzionamento espresso in *
      *                      * personalizzazioni : % variabile         *
      *                      *-----------------------------------------*
           go to     det-fun-spe-120.
       det-fun-spe-152.
      *                  *---------------------------------------------*
      *                  * Tipo funzionamento espresso in [dcc] : ad   *
      *                  * importo                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Come per tipo funzionamento espresso in *
      *                      * personalizzazioni : importo variabile   *
      *                      *-----------------------------------------*
           go to     det-fun-spe-140.
       det-fun-spe-200.
      *              *-------------------------------------------------*
      *              * Codice iva della spesa                          *
      *              *-------------------------------------------------*
           move      w-prs-spe-fat-civ
                    (w-det-fun-spe-ctr)   to   w-pie-spe-civ
                                              (1, w-det-fun-spe-inx)  .
      *              *-------------------------------------------------*
      *              * Contropartita della spesa                       *
      *              *-------------------------------------------------*
           move      w-prs-spe-fat-ccp
                    (w-det-fun-spe-ctr)   to   w-pie-spe-ccp
                                              (1, w-det-fun-spe-inx)  .
       det-fun-spe-300.
      *              *-------------------------------------------------*
      *              * Riciclo su scansione tabella personalizzazioni  *
      *              *-------------------------------------------------*
           go to     det-fun-spe-100.
       det-fun-spe-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valori testata indotti da record [dcc]     *
      *    * relativo alla dipendenza del cliente                      *
      *    *-----------------------------------------------------------*
       det-vlt-dcd-000.
      *              *-------------------------------------------------*
      *              * Se dipendenza a spaces : uscita                 *
      *              *-------------------------------------------------*
           if        w-det-vlt-dcd-dpz    =    spaces
                     go to det-vlt-dcd-999.
      *              *-------------------------------------------------*
      *              * Codice e descrizione ABI dipendenza             *
      *              *-------------------------------------------------*
           move      rf-dcc-cod-abi       to   w-tes-abi-dpz (1)      .
           move      w-tes-abi-dpz (1)    to   w-let-arc-axi-cod      .
           perform   let-arc-axi-000      thru let-arc-axi-999        .
           move      w-let-arc-axi-den    to   w-tes-abi-dpz-den (1)  .
      *              *-------------------------------------------------*
      *              * Codice e descrizione CAB dipendenza             *
      *              *-------------------------------------------------*
           move      rf-dcc-cod-cab       to   w-tes-cab-dpz (1)      .
           move      w-tes-abi-dpz (1)    to   w-let-arc-axs-abi      .
           move      w-tes-cab-dpz (1)    to   w-let-arc-axs-cab      .
           perform   let-arc-axs-000      thru let-arc-axs-999        .
           move      w-let-arc-axs-den    to   w-tes-cab-dpz-den (1)  .
      *              *-------------------------------------------------*
      *              * C/c appoggio della dipendenza                   *
      *              *-------------------------------------------------*
           move      rf-dcc-ccc-app       to   w-tes-cca-dpz (1)      .
      *              *-------------------------------------------------*
      *              * Codice e descrizione agente associato alla      *
      *              * dipendenza                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se tipo determinazione per cliente per fat- *
      *                  * turazione e tipo fornitura tramite Gruppo   *
      *                  * d'Acquisto e codice agente esistente : ri-  *
      *                  * mane quanto e' stato determinato per il     *
      *                  * Cliente Commerciale                         *
      *                  *---------------------------------------------*
           if        w-det-vlt-dcd-tdt    =    "F"    and
                     w-tes-tip-frn (1)    =    21     and
                     w-tes-age-aad (1)    not  = zero
                     go to det-vlt-dcd-999.
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      rf-dcc-cod-age       to   w-tes-age-aad (1)      .
       det-vlt-dcd-999.
           exit.

      *    *===========================================================*
      *    * Determinazione percentuali di provvigione in riga con e-  *
      *    * ventuale aggiustamento flag di significativita' provvi-   *
      *    * gioni in riga                                             *
      *    *-----------------------------------------------------------*
       det-ppv-rig-000.
      *              *-------------------------------------------------*
      *              * Test se da effettuare                           *
      *              *-------------------------------------------------*
           if        w-rig-fsp-rig (1)    not  = 01
                     go to det-ppv-rig-999.
       det-ppv-rig-100.
      *              *-------------------------------------------------*
      *              * Preparazione link-area                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Valori diretti da w-tes o w-rig             *
      *                  *---------------------------------------------*
           move      "PP"                 to   d-pvg-age-tip-ope      .
           move      w-tes-cod-age (1)    to   d-pvg-age-cod-age      .
           move      w-tes-tip-vpa (1)    to   d-pvg-age-tip-vpa      .
           move      w-tes-cpv-aaa (1)    to   d-pvg-age-cpv-aaa      .
           move      w-tes-ppv-aaa (1, 1) to   d-pvg-age-ppv-aaa (1)  .
           move      w-tes-ppv-aaa (1, 2) to   d-pvg-age-ppv-aaa (2)  .
           move      w-tes-ppv-aaa (1, 3) to   d-pvg-age-ppv-aaa (3)  .
           move      w-rig-tip-mag (1)    to   d-pvg-age-tip-mag      .
           move      w-rig-num-pro (1)    to   d-pvg-age-num-mag      .
           move      w-tes-cod-lst (1)    to   d-pvg-age-cod-lst      .
           move      w-rig-cpv-aap (1)    to   d-pvg-age-cpv-aap      .
           move      w-rig-ppv-aap (1, 1) to   d-pvg-age-ppv-aap (1)  .
           move      w-rig-ppv-aap (1, 2) to   d-pvg-age-ppv-aap (2)  .
           move      w-rig-ppv-aap (1, 3) to   d-pvg-age-ppv-aap (3)  .
           if        w-tes-tip-frn (1)    =    11
                     move  w-tes-cod-cli (1)
                                          to   d-pvg-age-cod-cli
           else      move  w-tes-cli-plf (1)
                                          to   d-pvg-age-cod-cli      .
           move      w-tes-cpv-aac (1)    to   d-pvg-age-cpv-aac      .
           move      w-tes-ppv-aac (1, 1) to   d-pvg-age-ppv-aac (1)  .
           move      w-tes-ppv-aac (1, 2) to   d-pvg-age-ppv-aac (2)  .
           move      w-tes-ppv-aac (1, 3) to   d-pvg-age-ppv-aac (3)  .
           move      w-tes-sgl-vpf (1)    to   d-pvg-age-sgl-vpp      .
           move      w-rig-per-scr (1, 1) to   d-pvg-age-per-scr (1)  .
           move      w-rig-per-scr (1, 2) to   d-pvg-age-per-scr (2)  .
           move      w-rig-per-scr (1, 3) to   d-pvg-age-per-scr (3)  .
           move      w-rig-per-scr (1, 4) to   d-pvg-age-per-scr (4)  .
           move      w-rig-per-scr (1, 5) to   d-pvg-age-per-scr (5)  .
           move      w-tes-dat-doc        to   d-pvg-age-dat-rif      .
       det-ppv-rig-200.
      *                  *---------------------------------------------*
      *                  * Valori calcolati                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prezzo lordo standard espresso nella    *
      *                      * valuta per fatturazione                 *
      *                      *-----------------------------------------*
           move      w-rig-prz-lrs (1)    to   d-pvg-age-prz-lrs      .
      *                      *-----------------------------------------*
      *                      * Eventuale conversione da valuta per     *
      *                      * prezzi standard a valuta per fattura-   *
      *                      * zione                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se necessario                  *
      *                          *-------------------------------------*
           if        w-rig-sgl-vps (1)    =    w-tes-sgl-vpf (1)
                     go to det-ppv-rig-220.
      *                          *-------------------------------------*
      *                          * Conversione valore da valuta per i  *
      *                          * prezzi standard a valuta base       *
      *                          *-------------------------------------*
           move      w-rig-sgl-vps (1)    to   w-cvs-vlt-sgl          .
           move      w-rig-dec-vps (1)    to   w-cvs-vlt-dec          .
           move      w-rig-tdc-vps (1)    to   w-cvs-vlt-tdc          .
           move      w-rig-cdc-vps (1)    to   w-cvs-vlt-cdc          .
           move      d-pvg-age-prz-lrs    to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                          *-------------------------------------*
      *                          * Conversione valore da valuta base a *
      *                          * valuta per fatturazione             *
      *                          *-------------------------------------*
           move      w-tes-sgl-vpf (1)    to   w-cvs-vlt-sgl          .
           move      w-tes-dec-vpf (1)    to   w-cvs-vlt-dec          .
           move      w-tes-tdc-vpf (1)    to   w-cvs-vlt-tdc          .
           move      w-tes-cdc-vpf (1)    to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
           move      w-cvs-vlt-aav        to   d-pvg-age-prz-lrs      .
       det-ppv-rig-220.
      *                      *-----------------------------------------*
      *                      * Prezzo netto standard espresso nella    *
      *                      * valuta per fatturazione                 *
      *                      *-----------------------------------------*
           move      w-rig-prz-nts (1)    to   d-pvg-age-prz-nts      .
      *                      *-----------------------------------------*
      *                      * Eventuale conversione da valuta per     *
      *                      * prezzi standard a valuta per fattura-   *
      *                      * zione                                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se necessario                  *
      *                          *-------------------------------------*
           if        w-rig-sgl-vps (1)    =    w-tes-sgl-vpf (1)
                     go to det-ppv-rig-240.
      *                          *-------------------------------------*
      *                          * Conversione valore da valuta per i  *
      *                          * prezzi standard a valuta base       *
      *                          *-------------------------------------*
           move      w-rig-sgl-vps (1)    to   w-cvs-vlt-sgl          .
           move      w-rig-dec-vps (1)    to   w-cvs-vlt-dec          .
           move      w-rig-tdc-vps (1)    to   w-cvs-vlt-tdc          .
           move      w-rig-cdc-vps (1)    to   w-cvs-vlt-cdc          .
           move      d-pvg-age-prz-nts    to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                          *-------------------------------------*
      *                          * Conversione valore da valuta base a *
      *                          * valuta per fatturazione             *
      *                          *-------------------------------------*
           move      w-tes-sgl-vpf (1)    to   w-cvs-vlt-sgl          .
           move      w-tes-dec-vpf (1)    to   w-cvs-vlt-dec          .
           move      w-tes-tdc-vpf (1)    to   w-cvs-vlt-tdc          .
           move      w-tes-cdc-vpf (1)    to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
           move      w-cvs-vlt-aav        to   d-pvg-age-prz-nts      .
       det-ppv-rig-240.
      *                      *-----------------------------------------*
      *                      * Prezzo netto effettivo, gia' espresso   *
      *                      * nella valuta per fatturazione           *
      *                      *-----------------------------------------*
           move      w-rig-prz-net (1)    to   d-pvg-age-prz-net      .
       det-ppv-rig-300.
      *              *-------------------------------------------------*
      *              * Richiamo del sottoprogramma                     *
      *              *-------------------------------------------------*
           perform   det-pvg-age-cll-000  thru det-pvg-age-cll-999    .
       det-ppv-rig-400.
      *              *-------------------------------------------------*
      *              * Memorizzazione percentuali determinate          *
      *              *-------------------------------------------------*
           move      d-pvg-age-per-pvg (1)
                                          to   w-rig-ppv-rig (1, 1)   .
           move      d-pvg-age-per-pvg (2)
                                          to   w-rig-ppv-rig (1, 2)   .
           move      d-pvg-age-per-pvg (3)
                                          to   w-rig-ppv-rig (1, 3)   .
       det-ppv-rig-500.
      *              *-------------------------------------------------*
      *              * Eventuale aggiustamento flag di significativi-  *
      *              * ta' provvigioni in riga                         *
      *              *-------------------------------------------------*
           if        d-pvg-age-flg-pvs    not  = spaces
                     move  03             to   w-rig-fsp-rig (1)      .
       det-ppv-rig-999.
           exit.

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
       det-imp-rig-999.
           exit.

      *    *===========================================================*
      *    * Calcolo prezzo netto                                      *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cps"                   .

      *    *===========================================================*
      *    * Determinazione quantita' secondaria per la vendita        *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/wdetqts0.wks"                   .

      *    *===========================================================*
      *    * Editing quantita' da incolonnare                          *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wks"                   .

      *    *===========================================================*
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .

      *    *===========================================================*
      *    * Determinazione prezzo sottoposto a legame valutario       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wlvlprz0.cps"                   .

      *    *===========================================================*
      *    * Routine di conversione da altra valuta a valuta base      *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcvsvlt0.cps"                   .

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
      *              * Normalizzazione function-key impostata          *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-e02-999.
           exit.

      *    *===========================================================*
      *    * Ricerca su buffer documenti movimentati                   *
      *    *-----------------------------------------------------------*
       buf-doc-mvm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valori di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-doc-mvm-fds      .
           move      spaces               to   w-buf-doc-mvm-tds      .
           move      zero                 to   w-buf-doc-mvm-nds      .
           move      zero                 to   w-buf-doc-mvm-dds      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-buf-doc-mvm-crb      .
       buf-doc-mvm-100.
      *              *-------------------------------------------------*
      *              * Start su [bit]                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DATSYS    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-buf-doc-mvm-drc    to   rf-bit-ide-dat         .
           move      zero                 to   rf-bit-dat-doc         .
           move      zero                 to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-buf-doc-mvm-fds
                     go to  buf-doc-mvm-999.
       buf-doc-mvm-200.
      *              *-------------------------------------------------*
      *              * Lettura [bit]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a controllo contatore           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-doc-mvm-500.
       buf-doc-mvm-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a controllo contatore     *
      *              *-------------------------------------------------*
           if        rf-bit-ide-dat       not  = w-buf-doc-mvm-drc
                     go to buf-doc-mvm-500.
       buf-doc-mvm-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [bit]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice utente                       *
      *                  *---------------------------------------------*
           if        rf-bit-ide-ute       not  = w-buf-doc-mvm-ute
                     go to buf-doc-mvm-200.
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rf-bit-cod-dpz       not  = w-buf-doc-mvm-dpz
                     go to buf-doc-mvm-200.
      *                  *---------------------------------------------*
      *                  * Test su tipo archivio                       *
      *                  *---------------------------------------------*
           if        rf-bit-tip-arc       not  = "C"
                     go to buf-doc-mvm-200.
       buf-doc-mvm-410.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-doc-mvm-crb      .
      *              *-------------------------------------------------*
      *              * Test se buffer oltre il numero previsto         *
      *              *-------------------------------------------------*
           if        w-buf-doc-mvm-crb    >    w-buf-doc-mvm-max
                     move  w-buf-doc-mvm-max
                                          to   w-buf-doc-mvm-crb
                     go to buf-doc-mvm-500.
       buf-doc-mvm-420.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo documento                              *
      *                  *---------------------------------------------*
           move      rf-bit-cod-tmb       to   w-buf-doc-mvm-tmo
                                              (w-buf-doc-mvm-crb)     .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      rf-bit-num-doc       to   w-buf-doc-mvm-num
                                              (w-buf-doc-mvm-crb)     .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      rf-bit-dat-doc       to   w-buf-doc-mvm-dat
                                              (w-buf-doc-mvm-crb)     .
       buf-doc-mvm-430.
      *                  *---------------------------------------------*
      *                  * Intestatario documento                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione tipo archivio    *
      *                      *-----------------------------------------*
           if        rf-bit-tip-arc       =    "C"
                     go to buf-doc-mvm-432
           else      go to buf-doc-mvm-440.
       buf-doc-mvm-432.
      *                      *-----------------------------------------*
      *                      * Cliente                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura file [cli]                  *
      *                          *-------------------------------------*
           move      rf-bit-cod-arc       to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
           move      w-let-arc-cli-rag    to   w-buf-doc-mvm-rsa
                                              (w-buf-doc-mvm-crb)     .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     buf-doc-mvm-480.
       buf-doc-mvm-440.
      *                      *-----------------------------------------*
      *                      * Tipo archivio non riconosciuto          *
      *                      *-----------------------------------------*
           move      spaces               to   w-buf-doc-mvm-rsa
                                              (w-buf-doc-mvm-crb)     .
      *                          *-------------------------------------*
      *                          * Oltre                               *
      *                          *-------------------------------------*
           go to     buf-doc-mvm-480.
       buf-doc-mvm-480.
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura                           *
      *                  *---------------------------------------------*
           go to     buf-doc-mvm-200.
       buf-doc-mvm-500.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-buf-doc-mvm-crb    =    zero
                     move  "#"            to   w-buf-doc-mvm-fds
                     go to buf-doc-mvm-999.
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-buf-doc-mvm-crb    to   w-buf-doc-mvm-cpb      .
           subtract  1                    from w-buf-doc-mvm-cpb      .
           divide    6                    into w-buf-doc-mvm-cpb      .
           add       1                    to   w-buf-doc-mvm-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-buf-doc-mvm-c01      .
      *                      *-----------------------------------------*
      *                      * Salvataggio immagine video              *
      *                      *-----------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Video in Off                            *
      *                      *-----------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione box vuoto               *
      *                      *-----------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      04                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      15                   to   v-lto                  .
           move      72                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Editing data                            *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-buf-doc-mvm-drc    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Preparazione fincatura                  *
      *                      *-----------------------------------------*
           move      60                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "DOCUMENTI INSERITI O MODIFICATI"
                                          to   w-all-str-cat (1)      .
      *
           move      w-buf-doc-mvm-drc    to   s-dat                  .
           if        s-gio                =    01 or
                     s-gio                =    08 or
                     s-gio                =    11
                     move  "L'"           to   w-all-str-cat (2)
           else      move  "IL"           to   w-all-str-cat (2)      .
      *
           move      v-edt                to   w-all-str-cat (3)      .
           move      "DA ["               to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           move      03                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      w-buf-doc-mvm-ute    to   w-all-str-cat (2)      .
           move      "]"                  to   w-all-str-cat (3)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *
           perform   all-str-cen-000      thru all-str-cen-999        .
      *                      *-----------------------------------------*
      *                      * Fincatura                               *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura fincatura                *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura di chiusura              *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      11                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                      *-----------------------------------------*
      *                      * Visualizzazione pagina video contenente *
      *                      * il record attualmente trattato          *
      *                      *-----------------------------------------*
           perform   buf-doc-mvm-950      thru buf-doc-mvm-959        .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       buf-doc-mvm-550.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           move      w-buf-doc-mvm-c01    to   w-buf-doc-mvm-nli      .
       buf-doc-mvm-555.
           if        w-buf-doc-mvm-nli    >    6
                     subtract  6          from w-buf-doc-mvm-nli
                     go to buf-doc-mvm-555.
      *                          *-------------------------------------*
      *                          * Incremento numero linea a video     *
      *                          * per posizionamento verticale        *
      *                          *-------------------------------------*
           add       06                   to   w-buf-doc-mvm-nli      .
       buf-doc-mvm-560.
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
       buf-doc-mvm-575.
      *                      *-----------------------------------------*
      *                      * Accettazione del mark-point             *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-buf-doc-mvm-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-buf-doc-mvm-c01    <    w-buf-doc-mvm-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-buf-doc-mvm-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-buf-doc-mvm-cpa    <    w-buf-doc-mvm-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-buf-doc-mvm-nli    to   v-lin                  .
           move      11                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       buf-doc-mvm-580.
           if        v-key                =    spaces or
                     v-key                =    "DO  " or
                     v-key                =    "SLCT"
                     go to buf-doc-mvm-582
           else if   v-key                =    "UP  "
                     go to buf-doc-mvm-584
           else if   v-key                =    "DOWN"
                     go to buf-doc-mvm-586
           else if   v-key                =    "EXIT"
                     go to buf-doc-mvm-598
           else if   v-key                =    "NXSC"
                     go to buf-doc-mvm-592
           else if   v-key                =    "PRSC"
                     go to buf-doc-mvm-594
           else      go to buf-doc-mvm-575.
       buf-doc-mvm-582.
      *              *-------------------------------------------------*
      *              * Se spaces, Do o select                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Restore video                               *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Memorizzazione valori selezionati           *
      *                  *---------------------------------------------*
           move      w-buf-doc-mvm-tmo
                    (w-buf-doc-mvm-c01)   to   w-buf-doc-mvm-tds      .
           move      w-buf-doc-mvm-num
                    (w-buf-doc-mvm-c01)   to   w-buf-doc-mvm-nds      .
           move      w-buf-doc-mvm-dat
                    (w-buf-doc-mvm-c01)   to   w-buf-doc-mvm-dds      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     buf-doc-mvm-999.
       buf-doc-mvm-584.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           subtract  1                    from w-buf-doc-mvm-c01      .
           if        w-buf-doc-mvm-nli    =    07
                     go to buf-doc-mvm-590
           else      go to buf-doc-mvm-550.
       buf-doc-mvm-586.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           if        w-buf-doc-mvm-c01    <    w-buf-doc-mvm-crb
                     add   1              to   w-buf-doc-mvm-c01
                     go to buf-doc-mvm-588
           else      go to buf-doc-mvm-575.
       buf-doc-mvm-588.
           if        w-buf-doc-mvm-nli    =    12
                     go to buf-doc-mvm-590
           else      go to buf-doc-mvm-550.
       buf-doc-mvm-590.
           perform   buf-doc-mvm-950      thru buf-doc-mvm-959        .
           go to     buf-doc-mvm-550.
       buf-doc-mvm-592.
      *              *-------------------------------------------------*
      *              * Se Next screen                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-buf-doc-mvm-cpa      .
           go to     buf-doc-mvm-596.
       buf-doc-mvm-594.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
           subtract  1                    from w-buf-doc-mvm-cpa      .
       buf-doc-mvm-596.
           move      w-buf-doc-mvm-cpa    to   w-buf-doc-mvm-c01      .
           multiply  6                    by   w-buf-doc-mvm-c01      .
           subtract  5                    from w-buf-doc-mvm-c01      .
           go to     buf-doc-mvm-590.
       buf-doc-mvm-598.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di selezione                           *
      *                  *---------------------------------------------*
           move      "#"                  to   w-buf-doc-mvm-fds      .
      *                  *---------------------------------------------*
      *                  * Restore video                               *
      *                  *---------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       buf-doc-mvm-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-doc-mvm-999.
       buf-doc-mvm-950.
      *              *=================================================*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-buf-doc-mvm-c01    to   w-buf-doc-mvm-c02      .
           add       5                    to   w-buf-doc-mvm-c02      .
           divide    6                    into w-buf-doc-mvm-c02      .
           move      w-buf-doc-mvm-c02    to   w-buf-doc-mvm-cpa      .
           subtract  1                    from w-buf-doc-mvm-c02      .
           multiply  6                    by   w-buf-doc-mvm-c02      .
           add       1                    to   w-buf-doc-mvm-c02      .
           add       5
                     w-buf-doc-mvm-c02  giving w-buf-doc-mvm-c03      .
           move      w-buf-doc-mvm-c03    to   w-buf-doc-mvm-c04      .
           if        w-buf-doc-mvm-c03    >    w-buf-doc-mvm-crb
                     move  w-buf-doc-mvm-crb
                                          to   w-buf-doc-mvm-c03      .
           move      07                   to   w-buf-doc-mvm-c05      .
       buf-doc-mvm-951.
      *                  *---------------------------------------------*
      *                  * Tipo documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-buf-doc-mvm-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      w-buf-doc-mvm-tmo
                    (w-buf-doc-mvm-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      w-buf-doc-mvm-c05    to   v-lin                  .
           move      17                   to   v-pos                  .
           move      w-buf-doc-mvm-num
                    (w-buf-doc-mvm-c02)
                    (6 : 6)               to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-buf-doc-mvm-c05    to   v-lin                  .
           move      24                   to   v-pos                  .
           move      w-buf-doc-mvm-dat
                    (w-buf-doc-mvm-c02)   to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Ragione sociale intestatario documento      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      38                   to   v-car                  .
           move      w-buf-doc-mvm-c05    to   v-lin                  .
           move      33                   to   v-pos                  .
           move      w-buf-doc-mvm-rsa
                    (w-buf-doc-mvm-c02)   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-buf-doc-mvm-c02      .
           add       1                    to   w-buf-doc-mvm-c05      .
           if        w-buf-doc-mvm-c02    not  > w-buf-doc-mvm-c03
                     go to buf-doc-mvm-951.
       buf-doc-mvm-952.
           if        w-buf-doc-mvm-c02    >    w-buf-doc-mvm-c04
                     go to buf-doc-mvm-955.
           if        w-buf-doc-mvm-crb    not  > 6
                     go to buf-doc-mvm-955.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-buf-doc-mvm-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-buf-doc-mvm-c02      .
           add       1                    to   w-buf-doc-mvm-c05      .
           go to     buf-doc-mvm-952.
       buf-doc-mvm-955.
      *                  *---------------------------------------------*
      *                  * Literal 'pagina'                            *
      *                  *---------------------------------------------*
           move      w-buf-doc-mvm-cpa    to   w-buf-doc-mvm-lt1      .
           move      w-buf-doc-mvm-cpb    to   w-buf-doc-mvm-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-buf-doc-mvm-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       buf-doc-mvm-959.
           exit.
       buf-doc-mvm-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione tipo movimento per bolla   *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acdezbi0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del coefficiente di cambio *
      *    * valuta                                                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice cliente commerciale *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione importo in riga            *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dimpven0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione provvigioni                *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/dpvgage0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

