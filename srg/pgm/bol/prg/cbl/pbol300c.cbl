       Identification Division.
       Program-Id.                                 pbol300c           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    bol                 *
      *                                Settore:    mov                 *
      *                                   Fase:    bol300c             *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/07/92    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Evasione righe bolla da altra bolla         *
      *                                                                *
      *                    Richiamata in bol300 da funzione 'Expand'   *
      *                                                                *
      *================================================================*
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *        Input  : l-eva-rdb-tip-ope = "OP"                       *
      *                 l-eva-rdb-cod-dpz = codice dipendenza in uso   *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *        Input  : l-eva-rdb-tip-ope = "CL"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *        Input  : l-eva-rdb-tip-ope = "C?"                       *
      *                                                                *
      *        Output : l-eva-rdb-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * "RB" - Accettazione dati identificativi bolla                  *
      *                                                                *
      *        Input  : l-eva-rdb-tip-ope = "RB"                       *
      *                 l-eva-rdb-cod-tmb = codice tipo movimento bol- *
      *                                     la da proporre come de-    *
      *                                     fault                      *
      *                                                                *
      *        Output : l-eva-rdb-exi-sts = spaces: Accettazione ese-  *
      *                                             guita con successo *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "SC" - Saldaconto per selezione righe                          *
      *                                                                *
      *        Input  : l-eva-rdb-tip-ope = "SC"                       *
      *                                                                *
      *        Output : l-eva-rdb-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "CC" - Caricamento righe bolla selezionate in catena           *
      *                                                                *
      *        Input  : l-eva-rdb-tip-ope = "CC"                       *
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
      *        * [bie]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbie"                          .
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
      *        *-------------------------------------------------------*
      *        * [zfi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzfi"                          .

      *    *===========================================================*
      *    * Record files area 'cge'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .

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
      *    * Record files area 'dpm'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dpm]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dpm/fls/rec/rfdpm"                          .

      *    *===========================================================*
      *    * Record files area 'dps'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [dps]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dps/fls/rec/rfdps"                          .

      *    *===========================================================*
      *    * Record files area 'orc'                                   *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .

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
      *        * Numero protocollo bolla in corso di trattamento       *
      *        *-------------------------------------------------------*
           05  w-gen-prt-bol              pic  9(11)                  .

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
           05  w-acc-cod-tmb-sgl          pic  x(03)                  .
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
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-val-acc.
               10  filler   occurs 200    pic  x(01)                  .
           05  w-sav-qta-dup              pic s9(10)v9(03)            .
           05  w-sav-snx-aoc              pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo ricalcolo da effettuare               *
      *        *-------------------------------------------------------*
           05  w-exp-tip-rcl.
               10  w-exp-tip-rcl-num      pic  9(02)       value 4    .
               10  w-exp-tip-rcl-lun      pic  9(02)       value 15   .
               10  w-exp-tip-rcl-tbl.
                   15  filler             pic  x(15) value
                            "[ ] Prezzi     "                         .
                   15  filler             pic  x(15) value
                            "[ ] Sconti     "                         .
                   15  filler             pic  x(15) value
                            "[ ] Provvigioni"                         .
                   15  filler             pic  x(15) value
                            "[ ] Descrizioni"                         .

      *    *===========================================================*
      *    * Work-area per bufferizzazione righe bolla da duplicare    *
      *    *-----------------------------------------------------------*
       01  w-brb.
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella                            *
      *        *-------------------------------------------------------*
           05  w-brb-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi in tabella                    *
      *        *-------------------------------------------------------*
           05  w-brb-max-ele              pic  9(05) value 999        .
      *        *-------------------------------------------------------*
      *        * Tabella elementi                                      *
      *        *-------------------------------------------------------*
           05  w-brb-tbl.
               10  w-brb-sng-ele occurs 999.
                   15  w-brb-num-prg      pic  9(05)       comp-3     .
                   15  w-brb-tip-rig      pic  x(01)                  .
                   15  w-brb-dec-qta      pic  9(01)                  .
                   15  w-brb-des-rig      pic  x(40)                  .
                   15  w-brb-qta-bol      pic s9(10)v9(03) comp-3     .
                   15  w-brb-qta-dup      pic s9(10)v9(03) comp-3     .
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
           05  w-sdc-npg-max              pic  9(04)                  .
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
           05  w-sdc-ctr-rig              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Work-area di appoggio                                 *
      *        *-------------------------------------------------------*
           05  w-sdc-wrk-rig              pic  9(04)                  .
           05  w-sdc-wrk-lin              pic  9(03)                  .
           05  w-sdc-wrk-rem              pic  9(03)                  .
           05  w-sdc-wrk-c01              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per routine buf-rig-bol-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-buf-rig-bol.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *-------------------------------------------------------*
           05  w-buf-rig-bol-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo bolla                               *
      *        *-------------------------------------------------------*
           05  w-buf-rig-bol-prt          pic  9(11)                  .
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
      *    * Work-area per routine car-rig-cat-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-car-rig-cat.
      *        *-------------------------------------------------------*
      *        * Contatore di lavoro                                   *
      *        *-------------------------------------------------------*
           05  w-car-rig-cat-ctr          pic  9(05)                  .

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
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zfi]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zfi.
               10  w-let-arc-zfi-flg      pic  x(01)                  .
               10  w-let-arc-zfi-cod      pic  x(05)                  .
               10  w-let-arc-zfi-vld      pic  9(02)                  .
               10  w-let-arc-zfi-dpz      pic  9(02)                  .
               10  w-let-arc-zfi-tdo      pic  9(02)                  .
               10  w-let-arc-zfi-ord      pic  9(02)                  .
               10  w-let-arc-zfi-prd      pic  9(02)                  .
               10  w-let-arc-zfi-ngi      pic  9(02)                  .
               10  w-let-arc-zfi-sgl      pic  x(03)                  .
               10  w-let-arc-zfi-cau      pic  9(03)                  .
               10  w-let-arc-zfi-siv      pic  9(07)                  .
               10  w-let-arc-zfi-sve      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dpm]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dpm.
               10  w-let-arc-dpm-flg      pic  x(01)                  .
               10  w-let-arc-dpm-cod      pic  9(07)                  .
               10  w-let-arc-dpm-des      pic  x(40)                  .
               10  w-let-arc-dpm-umi      pic  x(03)                  .
               10  w-let-arc-dpm-deq      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dps]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dps.
               10  w-let-arc-dps-flg      pic  x(01)                  .
               10  w-let-arc-dps-cod      pic  9(07)                  .
               10  w-let-arc-dps-des      pic  x(40)                  .
               10  w-let-arc-dps-umi      pic  x(03)                  .
               10  w-let-arc-dps-deq      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zac]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zac.
               10  w-let-arc-zac-flg      pic  x(01)                  .
               10  w-let-arc-zac-tip      pic  9(02)                  .
               10  w-let-arc-zac-cod      pic  9(03)                  .
               10  w-let-arc-zac-des.
                   15  w-let-arc-zac-drg occurs 10
                                          pic  x(40)                  .
               10  w-let-arc-zac-civ      pic  9(05)                  .
               10  w-let-arc-zac-ccp      pic  9(07)                  .
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
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .

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
      *        * Work per Det evasione riga bolla                      *
      *        *-------------------------------------------------------*
           05  w-det-qev-rig.
      *            *---------------------------------------------------*
      *            * Flag                                              *
      *            *---------------------------------------------------*
               10  w-det-qev-rig-flg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Quantita' evasa                                   *
      *            *---------------------------------------------------*
               10  w-det-qev-rig-qev      pic s9(10)v9(03)            .

      *    *===========================================================*
      *    * Work per determinazione quantita' secondaria              *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/wdetqts0.wkl"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing quantita' da incolonnare  *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wkl"                   .

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
      *    * Area di comunicazione per funzione di evasione righe bol- *
      *    * la da altra bolla                                         *
      *    *-----------------------------------------------------------*
       01  l-eva-rdb.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  l-eva-rdb-tip-ope          pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        * - Spaces : operazione eseguita                        *
      *        * - #      : errore di esecuzione                       *
      *        *-------------------------------------------------------*
           05  l-eva-rdb-exi-sts          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  l-eva-rdb-cod-dpz          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Default per codice tipo movimento bolla               *
      *        *-------------------------------------------------------*
           05  l-eva-rdb-cod-tmb          pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Flag di trattamento bolla                             *
      *        *-------------------------------------------------------*
           05  l-eva-rdb-flg-bol          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area libera per espansioni future                     *
      *        *-------------------------------------------------------*
           05  filler                     pic  x(99)                  .

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
       Procedure Division                using l-eva-rdb
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
           move      spaces               to   l-eva-rdb-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione dati identificativi bolla      *
      *                  *---------------------------------------------*
           if        l-eva-rdb-tip-ope    =    "RB"
                     perform acc-dti-bol-000
                                          thru acc-dti-bol-999
      *                  *---------------------------------------------*
      *                  * Saldaconto per selezione righe bolla        *
      *                  *---------------------------------------------*
           else if   l-eva-rdb-tip-ope    =    "SC"
                     perform sdc-sel-bol-000
                                          thru sdc-sel-bol-999
      *                  *---------------------------------------------*
      *                  * Caricamento righe bolla in catena           *
      *                  *---------------------------------------------*
           else if   l-eva-rdb-tip-ope    =    "CC"
                     perform car-rig-cat-000
                                          thru car-rig-cat-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   l-eva-rdb-tip-ope    =    "OP"
                     perform exe-fun-opn-000
                                          thru exe-fun-opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   l-eva-rdb-tip-ope    =    "CL"
                     perform exe-fun-cls-000
                                          thru exe-fun-cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   l-eva-rdb-tip-ope    =    "C?"
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
           move      l-eva-rdb-cod-dpz    to   w-acc-cod-dpz          .
      *              *-------------------------------------------------*
      *              * Normalizzazione data per accettazione           *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-dat-doc          .
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
      *              * [ocr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
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
      *              * [ocr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
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
                     move  spaces         to   l-eva-rdb-exi-sts
           else      move  "#"            to   l-eva-rdb-exi-sts      .
       tst-cnc-mod-999.
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
       acc-dti-bol-010.
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Titolo centrale                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      26                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      28                   to   v-pos                  .
           move      "EVASIONE BOLLA DI CONSEGNA"
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
       acc-dti-bol-030.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Preparazione valori di default per tipo movi-   *
      *              * mento bolla                                     *
      *              *-------------------------------------------------*
           move      l-eva-rdb-cod-tmb    to   w-acc-cod-tmb          .
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
      *                  * Se documento che non interessa la fattura-  *
      *                  * zione : oltre                               *
      *                  *---------------------------------------------*
           if        w-let-arc-zbi-mif    =    01
                     go to acc-dti-bol-050.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zfi]                      *
      *                  *---------------------------------------------*
           move      w-let-arc-zbi-tmf    to   w-let-arc-zfi-cod      .
           perform   let-arc-zfi-000      thru let-arc-zfi-999        .
       acc-dti-bol-050.
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
      *                  * Codice dipendenza o numero giornale Iva     *
      *                  *---------------------------------------------*
           move      w-acc-cod-dpz        to   w-acc-num-doc-dpz      .
           if        w-let-arc-zbi-mif    =    03
                     move  w-let-arc-zfi-ngi
                                          to   w-acc-num-doc-dpz
           else      move  w-acc-cod-dpz  to   w-acc-num-doc-dpz      .
      *                  *---------------------------------------------*
      *                  * Determinazione se documento esistente       *
      *                  *---------------------------------------------*
           perform   det-doc-ges-000      thru det-doc-ges-999        .
      *                      *-----------------------------------------*
      *                      * Se documento non esistente              *
      *                      *-----------------------------------------*
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
      *              * Bufferizzazione numero protocollo               *
      *              *-------------------------------------------------*
           move      rf-bit-num-prt       to   w-acc-num-prt          .
       acc-dti-bol-500.
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
           move      "Nessuna riga bolla da evadere !                   
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-bol-100.
       acc-dti-bol-620.
      *              *-------------------------------------------------*
      *              * Aggiornamento protocollo bolla in corso di      *
      *              * trattamento                                     *
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
           move      "#"                  to   l-eva-rdb-exi-sts      .
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
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-acc-cod-tmb        =    spaces
                     go to acc-cod-tmb-100.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento                                   *
      *                  *---------------------------------------------*
           move      w-let-arc-zbi-sgl    to   w-acc-cod-tmb-sgl      .
      *                  *---------------------------------------------*
      *                  * Se documento che non interessa la fattura-  *
      *                  * zione : oltre                               *
      *                  *---------------------------------------------*
           if        w-let-arc-zbi-mif    =    01
                     go to acc-cod-tmb-600.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zfi]                      *
      *                  *---------------------------------------------*
           move      w-let-arc-zbi-tmf    to   w-let-arc-zfi-cod      .
           perform   let-arc-zfi-000      thru let-arc-zfi-999        .
       acc-cod-tmb-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore di default             *
      *                  *---------------------------------------------*
           move      w-acc-cod-tmb        to   l-eva-rdb-cod-tmb      .
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
      *                  * Normalizzazione function-key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
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
                     go to acc-num-doc-400.
      *                  *---------------------------------------------*
      *                  * Find su archivio [bit]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-bit-000      thru fnd-arc-bit-999        .
           if        w-fnd-arc-bit-sel    not  = spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Normalizzazione function-key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
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
           move      spaces               to   w-acc-cod-tmb-sgl      .
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
                     go to buf-rig-bol-900.
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
                     go to buf-rig-bol-900.
       buf-rig-bol-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-bir-num-prt       not  = w-buf-rig-bol-prt
                     go to buf-rig-bol-900.
       buf-rig-bol-350.
      *              *-------------------------------------------------*
      *              * Test sul contatore elementi nel buffer          *
      *              *-------------------------------------------------*
           if        w-brb-num-ele        not  < w-brb-max-ele
                     go to buf-rig-bol-900.
       buf-rig-bol-400.
      *              *-------------------------------------------------*
      *              * Aggiornamento comodi di visualizzazione         *
      *              *-------------------------------------------------*
           move      "AG"                 to   w-edt-qta-inc-ope      .
           move      rf-bir-qta-ven       to   w-edt-qta-inc-qta      .
           move      rf-bir-dec-qta       to   w-edt-qta-inc-dec      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
           move      w-edt-qta-inc-din    to   w-tes-dec-qta          .
       buf-rig-bol-410.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ridefinizione tipo riga                     *
      *                  *---------------------------------------------*
           move      rf-bir-tip-rig       to   w-buf-rig-bol-wtr      .
      *                  *---------------------------------------------*
      *                  * Selezione su tipo riga                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se documento che non interessa la fat-  *
      *                      * turazione, tipo riga 'A' non ammesso    *
      *                      *-----------------------------------------*
           if        w-tes-int-ftr        =    01  and
                     w-buf-rig-bol-wtp    =    "A"
                     go to buf-rig-bol-200.
       buf-rig-bol-420.
      *                  *---------------------------------------------*
      *                  * Determinazione stato di evasione della riga *
      *                  *---------------------------------------------*
           perform   det-qev-rig-000      thru det-qev-rig-999        .
           if        w-det-qev-rig-flg    not  = spaces
                     go to buf-rig-bol-200.
       buf-rig-bol-600.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-rig-bol-flg      .
       buf-rig-bol-700.
      *              *-------------------------------------------------*
      *              * Bufferizzazione riga bolla                      *
      *              *-------------------------------------------------*
           perform   buf-rig-bol-rig-000  thru buf-rig-bol-rig-999    .
       buf-rig-bol-800.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale file [bir]       *
      *              *-------------------------------------------------*
           go to     buf-rig-bol-200.
       buf-rig-bol-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rig-bol-999.
       buf-rig-bol-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione righe bolla da trattare                   *
      *    *                                                           *
      *    * Trattamento singola riga                                  *
      *    *-----------------------------------------------------------*
       buf-rig-bol-rig-000.
      *              *-------------------------------------------------*
      *              * Singoli campi                                   *
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
       buf-rig-bol-rig-100.
      *                  *---------------------------------------------*
      *                  * Descrizione per la riga                     *
      *                  *                                             *
      *                  * - Se tipo riga 'A', 'C', 'PN', 'M' o 'S':   *
      *                  *   si duplica la descrizione contenuta nel   *
      *                  *   documento di origine                      *
      *                  *                                             *
      *                  * - Se tipo riga 'P':                         *
      *                  *   - Se tipo e codice archivio e codice lin- *
      *                  *     gua del documento di origine sono gli   *
      *                  *     stessi di quelli di testata :           *
      *                  *     si duplica la descrizione contenuta nel *
      *                  *     documento di origine                    *
      *                  *   - Se tipo e codice archivio e codice lin- *
      *                  *     gua del documento di origine sono di-   *
      *                  *     versi da quelli di testata :            *
      *                  *     si ricalcola la descrizione del prodot- *
      *                  *     to                                      *
      *                  *---------------------------------------------*
           if        w-buf-rig-bol-wtp    =    "A"     or
                     w-buf-rig-bol-wtp    =    "C"     or
                    (w-buf-rig-bol-wtp    =    "P" and
                     w-buf-rig-bol-wtf    =    "N"   ) or
                     w-buf-rig-bol-wtp    =    "M"     or
                     w-buf-rig-bol-wtp    =    "S"
                     go to buf-rig-bol-rig-120
           else if   w-buf-rig-bol-wtp    =    "P" and
                     w-buf-rig-bol-wtf    =    " "
                     go to buf-rig-bol-rig-300
           else      go to buf-rig-bol-rig-120.
       buf-rig-bol-rig-120.
      *                      *-----------------------------------------*
      *                      * Se tipo riga 'A', 'C', 'PN', 'M' o 'S'  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura preliminare file [bix]      *
      *                          *-------------------------------------*
           move      rf-bir-num-prt       to   w-let-arc-bix-prt      .
           move      rf-bir-num-prg       to   w-let-arc-bix-prg      .
           move      11                   to   w-let-arc-bix-trc      .
           perform   let-arc-bix-000      thru let-arc-bix-999        .
      *
           if        w-let-arc-bix-flg    not  = spaces
                     go to buf-rig-bol-rig-200.
      *
           move      w-let-arc-bix-des    to   w-buf-rig-bol-wde      .
      *
           go to     buf-rig-bol-rig-400.
       buf-rig-bol-rig-200.
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del flag di  *
      *                          * estensione alla descrizione         *
      *                          *-------------------------------------*
           if        rf-bir-des-ext       =    0
                     go to buf-rig-bol-rig-210
           else if   rf-bir-des-ext       =    1
                     go to buf-rig-bol-rig-220
           else if   rf-bir-des-ext       =    2
                     go to buf-rig-bol-rig-230.
       buf-rig-bol-rig-210.

      *                          *-------------------------------------*
      *                          * Se nessuna estensione : bufferizza- *
      *                          * zione descrizione contenuta record  *
      *                          * [bir]                               *
      *                          *-------------------------------------*
           move      rf-bir-des-rig       to   w-buf-rig-bol-wde      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     buf-rig-bol-rig-400.
       buf-rig-bol-rig-220.
      *                          *-------------------------------------*
      *                          * Se estensione nel file [bix]        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     buf-rig-bol-rig-400.
       buf-rig-bol-rig-230.
      *                          *-------------------------------------*
      *                          * Se estensione nel file [pdx]        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura archivio [dcp]          *
      *                              *---------------------------------*
           move      rf-bir-num-pro       to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      "I  "                to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-buf-rig-bol-wde      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     buf-rig-bol-rig-400.
       buf-rig-bol-rig-300.
      *                      *-----------------------------------------*
      *                      * Se tipo riga 'P'                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su tipo e codice archivio e su *
      *                          * codice lingua                       *
      *                          *-------------------------------------*
           if        rf-bir-tip-arc       =    w-tes-tip-arc (1) and
                     rf-bir-cod-arc       =    w-tes-cod-arc (1) and
                     rf-bir-cod-lng       =    w-tes-cod-lng (1)
                     go to buf-rig-bol-rig-120.
      *                          *-------------------------------------*
      *                          * Determinazione descrizione associa- *
      *                          * ta al prodotto                      *
      *                          *-------------------------------------*
           move      rf-bir-num-pro       to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      "I  "                to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-buf-rig-bol-wde      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     buf-rig-bol-rig-400.
       buf-rig-bol-rig-400.
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
       buf-rig-bol-rig-420.
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
                     go to buf-rig-bol-rig-500.
      *                      *-----------------------------------------*
      *                      * Altrimenti                              *
      *                      *-----------------------------------------*
           move      rf-bir-qta-ven       to   w-brb-qta-bol
                                              (w-brb-num-ele)         .
           subtract  w-det-qev-rig-qev    from w-brb-qta-bol
                                              (w-brb-num-ele)         .
       buf-rig-bol-rig-500.
      *                  *---------------------------------------------*
      *                  * Quantita' da evadere a zero                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-brb-qta-dup
                                              (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Quantita' da evadere pari a quantita' in    *
      *                  * bolla                                       *
      *                  *---------------------------------------------*
______*    move      w-brb-qta-bol
______*             (w-brb-num-ele)       to   w-brb-qta-dup
______*                                       (w-brb-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Si/No addebito o commento                   *
      *                  *---------------------------------------------*
           move      "S"                  to   w-brb-snx-aoc
                                              (w-brb-num-ele)         .
       buf-rig-bol-rig-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rig-bol-rig-999.
       buf-rig-bol-rig-999.
           exit.

      *    *===========================================================*
      *    * Routine per la determinazione, relativamente ad una riga  *
      *    * bolla, della quantita' evasa                              *
      *    *-----------------------------------------------------------*
       det-qev-rig-000.
      *              *-------------------------------------------------*
      *              * Inizializzazioni preliminari                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag di uscita              *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-qev-rig-flg      .
      *                  *---------------------------------------------*
      *                  * Quantita' evasa : a zero                    *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-qev-rig-qev      .
       det-qev-rig-100.
      *              *-------------------------------------------------*
      *              * Start iniziale su [bie]                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RIFPRE    "         to   f-key                  .
           move      rf-bir-num-prt       to   rf-bie-rif-prt         .
           move      rf-bir-num-prg       to   rf-bie-rif-prg         .
           move      zero                 to   rf-bie-num-prt         .
           move      zero                 to   rf-bie-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbie"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bie                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-qev-rig-900.
       det-qev-rig-200.
      *              *-------------------------------------------------*
      *              * Read Next su [bie]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Read Next                                   *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbie"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bie                 .
      *                  *---------------------------------------------*
      *                  * Se At end : uscita                          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-qev-rig-900.
       det-qev-rig-300.
      *              *-------------------------------------------------*
      *              * Test Max su [bie]                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su :                                   *
      *                  *    - Numero protocollo bolla                *
      *                  *    - Numero riga ordine bolla               *
      *                  *---------------------------------------------*
           if        rf-bie-rif-prt       =    rf-bir-num-prt and
                     rf-bie-rif-prg       =    rf-bir-num-prg
                     go to det-qev-rig-400.
      *                  *---------------------------------------------*
      *                  * Se oltre il Max : uscita                    *
      *                  *---------------------------------------------*
           go to     det-qev-rig-900.
       det-qev-rig-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record di [bie] rispetto al re-   *
      *              * cord di [bir]                                   *
      *              *-------------------------------------------------*
       det-qev-rig-500.
      *              *-------------------------------------------------*
      *              * Considerazioni sul record di [bie] letto e se-  *
      *              * lezionato rispetto ad [bir]                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento quantita' evasa                  *
      *                  *---------------------------------------------*
           add       rf-bie-qta-eva       to   w-det-qev-rig-qev      .
           if        w-det-qev-rig-qev    not  < rf-bir-qta-ven
                     move  "#"            to   w-det-qev-rig-flg      .
       det-qev-rig-800.
      *              *-------------------------------------------------*
      *              * Riciclo a Read Next su [bie]                    *
      *              *-------------------------------------------------*
           go to     det-qev-rig-200.
       det-qev-rig-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-qev-rig-999.
       det-qev-rig-999.
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
      *    * Saldaconto per selezione righe bolla da duplicare         *
      *    *-----------------------------------------------------------*
       sdc-sel-bol-000.
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
                     move  "#"            to   l-eva-rdb-exi-sts      .
       sdc-sel-bol-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per selezione righe bolla                      *
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
           move      "Pag.  |               Descrizione              |  
      -              "Quantita' |In evasione |      "
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
           move      "      |----------------------------------------|--
      -              "----------|------------|      "
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
           move      "Pag.  |       Descrizione            Codice    |  
      -              "Quantita' |In evasione |      "
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
           move      "      |------------------------- --------------|--
      -              "----------|------------|      "
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
           move      "Pag.  |     Descrizione            Codice      |  
      -              "Quantita' |In evasione |      "
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
           move      "      |--------------------- ------------------|--
      -              "----------|------------|      "
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
           move      "Pag.  |    Codice            Descrizione       |  
      -              "Quantita' |In evasione |      "
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
           move      "      |-------------- -------------------------|--
      -              "----------|------------|      "
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
           move      "Pag.  |      Codice            Descrizione     |  
      -              "Quantita' |In evasione |      "
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
           move      "      |------------------ ---------------------|--
      -              "----------|------------|      "
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
           move      "Pag.  |       Descrizione            Codice    |  
      -              "Quantita' |In evasione |      "
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
           move      "      |------------------------- --------------|--
      -              "----------|------------|      "
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
           move      "Pag.  |     Descrizione            Codice      |  
      -              "Quantita' |In evasione |      "
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
           move      "      |--------------------- ------------------|--
      -              "----------|------------|      "
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
           move      "      |                                        |  
      -              "          |            |      "
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
      *              * Visualizzazione numero pagina                   *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           move      06                   to   v-lin                  .
           move      05                   to   v-pos                  .
           move      w-sdc-npg-vis        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
           move      "      |                                        |  
      -              "          |            |      "
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
      *                      * Progressivo riga                        *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "B"                  to   v-edm                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      01                   to   v-pos                  .
           move      w-sdc-wrk-rig        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      08                   to   v-pos                  .
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
           move      49                   to   v-pos                  .
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
           move      67                   to   v-pos                  .
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
      *                          * Quantita' da duplicare              *
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
           move      62                   to   v-pos                  .
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
      *    * Accettazione campo saldaconto : Quantita' da duplicare    *
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
                    (w-sdc-ctr-rig)       to   w-sav-qta-dup          .
       acc-qta-dup-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      w-brb-dec-qta
                    (w-sdc-ctr-rig)       to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      62                   to   v-pos                  .
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
      *                  * da duplicare e segno quantita' in bolla     *
      *                  *---------------------------------------------*
           if        w-brb-qta-bol
                    (w-sdc-ctr-rig)       >    zero and
                     w-brb-qta-dup
                    (w-sdc-ctr-rig)       <    zero
                     go to acc-qta-dup-100
           else if   w-brb-qta-bol
                    (w-sdc-ctr-rig)       <    zero and
                     w-brb-qta-dup
                    (w-sdc-ctr-rig)       >    zero
                     go to acc-qta-dup-100.
       acc-qta-dup-420.
      *                  *---------------------------------------------*
      *                  * Se Slct : controllo che il valore impostato *
      *                  * sia uguale al precedente                    *
      *                  *---------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-qta-dup-600.
           if        w-brb-qta-dup
                    (w-sdc-ctr-rig)       not  = w-sav-qta-dup
                     move  w-sav-qta-dup  to   w-brb-qta-dup
                                              (w-sdc-ctr-rig)
                     go to acc-qta-dup-100.
       acc-qta-dup-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
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
      *                          * Quantita' da duplicare              *
      *                          *-------------------------------------*
           move      zero                 to   w-brb-qta-dup
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-700.
       acc-qta-dup-610.
      *                      *-----------------------------------------*
      *                      * Se valore non presente                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Quantita' da duplicare              *
      *                          *-------------------------------------*
           move      w-brb-qta-bol
                    (w-sdc-ctr-rig)       to   w-brb-qta-dup
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-700.
       acc-qta-dup-620.
      *                  *---------------------------------------------*
      *                  * Se Return o altri tasti funzione            *
      *                  *---------------------------------------------*
       acc-qta-dup-700.
      *                  *---------------------------------------------*
      *                  * Trattamento altri valori riga               *
      *                  *---------------------------------------------*
           go to     acc-qta-dup-760.
       acc-qta-dup-760.
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori riga                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Quantita' da duplicare                  *
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
           move      62                   to   v-pos                  .
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
           move      67                   to   v-pos                  .
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
           move      67                   to   v-pos                  .
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
           move      zero                 to   w-car-rig-cat-ctr      .
       car-rig-cat-100.
           add       1                    to   w-car-rig-cat-ctr      .
      *                  *---------------------------------------------*
      *                  * Test se fine buffer                         *
      *                  *---------------------------------------------*
           if        w-car-rig-cat-ctr    >    w-brb-num-ele
                     go to car-rig-cat-900.
      *                  *---------------------------------------------*
      *                  * Se prima riga caricata                      *
      *                  *---------------------------------------------*
           if        w-car-rig-cat-ctr    >    1
                     go to car-rig-cat-105.
      *                  *---------------------------------------------*
      *                  * Emissione commento precablato               *
      *                  *---------------------------------------------*
           perform   car-rig-cat-erc-000  thru car-rig-cat-erc-999    .
      *                  *---------------------------------------------*
      *                  * Oltre                                       *
      *                  *---------------------------------------------*
           go to     car-rig-cat-105.
       car-rig-cat-105.
      *                  *---------------------------------------------*
      *                  * Selezione su righe bolla nel buffer         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo riga    *
      *                      *-----------------------------------------*
           if        w-brb-tip-rig
                    (w-car-rig-cat-ctr)   =    "A" or
                     w-brb-tip-rig
                    (w-car-rig-cat-ctr)   =    "C"
                     go to car-rig-cat-110
           else      go to car-rig-cat-120.
       car-rig-cat-110.
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento          *
      *                      *-----------------------------------------*
           if        w-brb-snx-aoc
                    (w-car-rig-cat-ctr)   not  = "S"
                     go to car-rig-cat-100.
           go to     car-rig-cat-150.
       car-rig-cat-120.
      *                      *-----------------------------------------*
      *                      * Se altro tipo riga                      *
      *                      *-----------------------------------------*
           if        w-brb-qta-dup
                    (w-car-rig-cat-ctr)   =    zero
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
                     go to car-rig-cat-950.
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
                    (w-car-rig-cat-ctr)   to   rf-bir-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
       car-rig-cat-250.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [ocr]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [ocr]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-bir-coc-prt       to   rf-ocr-num-prt         .
           move      rf-bir-coc-prg       to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
       car-rig-cat-300.
      *                  *---------------------------------------------*
      *                  * Composizione riga                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Numero progressivo riga                 *
      *                      *-----------------------------------------*
           move      w-cat-rig-prg        to   w-rig-num-prg (1)      .
       car-rig-cat-310.
      *                      *-----------------------------------------*
      *                      * Dati relativi ai blocchi : non gestiti  *
      *                      *-----------------------------------------*
       car-rig-cat-320.
      *                      *-----------------------------------------*
      *                      * Tipo riga                               *
      *                      *-----------------------------------------*
           move      rf-bir-tip-rig       to   w-rig-tip-rig (1)      .
           perform   dec-tip-rig-000      thru dec-tip-rig-999        .
       car-rig-cat-330.
      *                      *-----------------------------------------*
      *                      * Tipo magazzino                          *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     move  01             to   w-rig-tip-mag (1)
           else if   w-rig-tip-rig-tpr (1)
                                          =    "S"
                     move  02             to   w-rig-tip-mag (1)
           else if   w-rig-tip-rig-tpr (1)
                                          =    "M"
                     move  03             to   w-rig-tip-mag (1)
           else      move  zero           to   w-rig-tip-mag (1)      .
       car-rig-cat-340.
      *                      *-----------------------------------------*
      *                      * Codice numerico magazzino               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da trattare                 *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-350.
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      rf-bir-num-pro       to   w-rig-num-pro (1)      .
       car-rig-cat-350.
      *                      *-----------------------------------------*
      *                      * Codice alfanumerico magazzino           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da trattare                 *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-360.
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      rf-bir-alf-pro       to   w-rig-alf-pro (1)      .
       car-rig-cat-360.
      *                      *-----------------------------------------*
      *                      * Sigla variante : non gestita            *
      *                      *-----------------------------------------*
       car-rig-cat-365.
      *                      *-----------------------------------------*
      *                      * Codice prodotto secondo il cliente      *
      *                      *                                         *
      *                      * - Se tipo riga 'P':                     *
      *                      *   - Se tipo e codice archivio del docu- *
      *                      *     mento di origine sono gli stessi di *
      *                      *     quelli di testata : si duplica il   *
      *                      *     codice prodotto secondo il cliente  *
      *                      *     contenuto nel documento di origine  *
      *                      *   - Se tipo e codice archivio del docu- *
      *                      *     mento di origine sono diversi da    *
      *                      *     quelli di testata : si ricalcola il *
      *                      *     codice prodotto secondo il cliente  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da trattare                 *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P" or
                     w-rig-tip-rig-tfu (1)
                                          not  = spaces
                     go to car-rig-cat-370.
      *                          *-------------------------------------*
      *                          * Confronto tipo e codice archivio    *
      *                          * del documento originale e quelli di *
      *                          * testata                             *
      *                          *-------------------------------------*
           if        rf-bir-tip-arc       =    w-tes-tip-arc (1) and
                     rf-bir-cod-arc       =    w-tes-cod-arc (1)
                     go to car-rig-cat-366
           else      go to car-rig-cat-367.
       car-rig-cat-366.
      *                          *-------------------------------------*
      *                          * Se uguali a quelli del documento o- *
      *                          * riginale                            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione codice prodotto *
      *                              * secondo il cliente da documento *
      *                              * originale                       *
      *                              *---------------------------------*
           move      rf-bir-cop-scl       to   w-rig-cop-scl (1)      .
           go to     car-rig-cat-370.
       car-rig-cat-367.
      *                          *-------------------------------------*
      *                          * Se diversi da quelli del documento  *
      *                          * originale                           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura archivio [pdx]          *
      *                              *---------------------------------*
           move      w-rig-num-pro (1)    to   w-let-dcp-pdx-cod      .
           move      w-tes-tip-arc (1)    to   w-let-dcp-pdx-tar      .
           if        w-tes-tip-frn (1)    =    11
                     move  w-tes-cod-arc (1)
                                          to   w-let-dcp-pdx-arc
           else      move  w-tes-arc-plf (1)
                                          to   w-let-dcp-pdx-arc      .
           move      w-tes-cod-lng (1)    to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
      *                              *---------------------------------*
      *                              * Bufferizzazione codice prodotto *
      *                              * secondo il cliente              *
      *                              *---------------------------------*
           move      w-let-dcp-pdx-cpc    to   w-rig-cop-scl (1)      .
           go to     car-rig-cat-370.
       car-rig-cat-370.
      *                      *-----------------------------------------*
      *                      * Descrizione prodotto originale          *
      *                      *                                         *
      *                      * - Se tipo riga 'A', 'C', 'PN', 'M' o    *
      *                      *   'S': si duplica la descrizione conte- *
      *                      *   nuta nel documento di origine         *
      *                      *                                         *
      *                      * - Se tipo riga 'P':                     *
      *                      *   - Se tipo e codice archivio e codice  *
      *                      *     lingua del documento di origine so- *
      *                      *     no gli stessi di quelli di testa-   *
      *                      *     ta : si duplica la descrizione con- *
      *                      *     tenuta nel documento di origine     *
      *                      *   - Se tipo e codice archivio e codice  *
      *                      *     lingua del documento di origine so- *
      *                      *     no diversi da quelli di testata :   *
      *                      *     si ricalcola la descrizione del     *
      *                      *     prodotto                            *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A"     or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"     or
                    (w-rig-tip-rig-tpr (1)
                                          =    "P" and
                     w-rig-tip-rig-tfu (1)
                                          =    "N"   ) or
                     w-rig-tip-rig-tpr (1)
                                          =    "M"     or
                     w-rig-tip-rig-tpr (1)
                                          =    "S"
                     go to car-rig-cat-371
           else if   w-rig-tip-rig-tpr (1)
                                          =    "P" and
                     w-rig-tip-rig-tfu (1)
                                          =    " "
                     go to car-rig-cat-376
           else      go to car-rig-cat-371.
       car-rig-cat-371.
      *                          *-------------------------------------*
      *                          * Se tipo riga 'A', 'C', 'PN', 'M' o  *
      *                          * 'S'                                 *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura preliminare file [bix]  *
      *                              *---------------------------------*
           move      rf-bir-num-prt       to   w-let-arc-bix-prt      .
           move      rf-bir-num-prg       to   w-let-arc-bix-prg      .
           move      11                   to   w-let-arc-bix-trc      .
           perform   let-arc-bix-000      thru let-arc-bix-999        .
      *
           if        w-let-arc-bix-flg    not  = spaces
                     go to car-rig-cat-372.
      *
           move      w-let-arc-bix-des    to   w-rig-des-rig (1)      .
      *
           go to     car-rig-cat-380.
       car-rig-cat-372.
      *                              *---------------------------------*
      *                              * Deviazione in funzione del flag *
      *                              * di estensione alla descrizione  *
      *                              *---------------------------------*
           if        rf-bir-des-ext       =    0
                     go to car-rig-cat-373
           else if   rf-bir-des-ext       =    1
                     go to car-rig-cat-374
           else if   rf-bir-des-ext       =    2
                     go to car-rig-cat-375.
       car-rig-cat-373.
      *                              *---------------------------------*
      *                              * Se nessuna estensione : buffe-  *
      *                              * rizzazione descrizione contenu- *
      *                              * ta record [bir]                 *
      *                              *---------------------------------*
           move      rf-bir-des-rig       to   w-rig-des-rig (1)      .
           go to     car-rig-cat-380.
       car-rig-cat-374.
      *                              *---------------------------------*
      *                              * Se estensione nel file [bix]    *
      *                              *---------------------------------*
           go to     car-rig-cat-380.
       car-rig-cat-375.
      *                              *---------------------------------*
      *                              * Se estensione nel file [pdx]    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura archivio [dcp]      *
      *                                  *-----------------------------*
           move      w-rig-num-pro (1)    to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      "I  "                to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-rig-des-por (1)
                                               w-rig-des-rig (1)      .
           go to     car-rig-cat-380.
       car-rig-cat-376.
      *                          *-------------------------------------*
      *                          * Se tipo riga 'P'                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test su tipo e codice archivio  *
      *                              * e su codice lingua              *
      *                              *---------------------------------*
           if        rf-bir-tip-arc       =    w-tes-tip-arc (1) and
                     rf-bir-cod-arc       =    w-tes-cod-arc (1) and
                     rf-bir-cod-lng       =    w-tes-cod-lng (1)
                     go to car-rig-cat-371.
      *                              *---------------------------------*
      *                              * Determinazione descrizione as-  *
      *                              * sociata al prodotto             *
      *                              *---------------------------------*
           move      rf-bir-num-pro       to   w-let-dcp-pdx-cod      .
           move      "C"                  to   w-let-dcp-pdx-tar      .
           move      rf-bir-cod-arc       to   w-let-dcp-pdx-arc      .
           move      rf-bir-cod-lng       to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
      *                                  *-----------------------------*
      *                                  * Se non trovato : come sopra *
      *                                  *-----------------------------*
           if        w-let-dcp-pdx-flg    not  = spaces
                     go to car-rig-cat-371.
      *                                  *-----------------------------*
      *                                  * Descrizione per il cliente  *
      *                                  *-----------------------------*
           move      w-let-dcp-pdx-des    to   w-rig-des-por (1)
                                               w-rig-des-rig (1)      .
           go to     car-rig-cat-380.
       car-rig-cat-380.
      *                      *-----------------------------------------*
      *                      * Descrizione riga                        *
      *                      *-----------------------------------------*
       car-rig-cat-390.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica archivio relativo al *
      *                      * tipo riga                               *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-392
           else if   w-rig-tip-rig-tpr (1)
                                          =    "M"
                     go to car-rig-cat-394
           else if   w-rig-tip-rig-tpr (1)
                                          =    "S"
                     go to car-rig-cat-396
           else if   w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-398
           else      go to car-rig-cat-400.
       car-rig-cat-392.
      *                      *-----------------------------------------*
      *                      * Tipo riga : 'P'                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcp]              *
      *                          *-------------------------------------*
           move      w-rig-num-pro (1)    to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      "I  "                to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
      *                              *---------------------------------*
      *                              * A tipo prodotto                 *
      *                              *---------------------------------*
           go to     car-rig-cat-400.
       car-rig-cat-394.
      *                      *-----------------------------------------*
      *                      * Tipo riga : 'M'                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dpm]              *
      *                          *-------------------------------------*
           move      w-rig-num-pro (1)    to   w-let-arc-dpm-cod      .
           perform   let-arc-dpm-000      thru let-arc-dpm-999        .
           go to     car-rig-cat-400.
       car-rig-cat-396.
      *                      *-----------------------------------------*
      *                      * Tipo riga : 'S'                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [dps]              *
      *                          *-------------------------------------*
           move      w-rig-num-pro (1)    to   w-let-arc-dps-cod      .
           perform   let-arc-dps-000      thru let-arc-dps-999        .
           go to     car-rig-cat-400.
       car-rig-cat-398.
      *                      *-----------------------------------------*
      *                      * Tipo riga : 'A' o 'C'                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura archivio [zac]              *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A"
                     move  01             to   w-let-arc-zac-tip
           else      move  02             to   w-let-arc-zac-tip      .
           move      w-rig-tip-rig-cac (1)
                                          to   w-let-arc-zac-cod      .
           perform   let-arc-zac-000      thru let-arc-zac-999        .
           go to     car-rig-cat-400.
       car-rig-cat-400.
      *                      *-----------------------------------------*
      *                      * Tipo prodotto                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ri- *
      *                          * ga                                  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-402
           else if   w-rig-tip-rig-tpr (1)
                                          =    "M"
                     go to car-rig-cat-404
           else if   w-rig-tip-rig-tpr (1)
                                          =    "S"
                     go to car-rig-cat-406
           else      go to car-rig-cat-410.
       car-rig-cat-402.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          *-------------------------------------*
           move      w-let-dcp-pdx-tpr    to   w-rig-tip-pro (1)      .
           go to     car-rig-cat-410.
       car-rig-cat-404.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Materia prima       *
      *                          *-------------------------------------*
           move      01                   to   w-rig-tip-pro (1)      .
           go to     car-rig-cat-410.
       car-rig-cat-406.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Semilavorato        *
      *                          *-------------------------------------*
           move      01                   to   w-rig-tip-pro (1)      .
           go to     car-rig-cat-410.
       car-rig-cat-410.
      *                      *-----------------------------------------*
      *                      * Codice iva                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-420.
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ri- *
      *                          * ga                                  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-412
           else if   w-rig-tip-rig-tpr (1)
                                          =    "A"
                     go to car-rig-cat-414
           else      go to car-rig-cat-420.
       car-rig-cat-412.
      *                          *-------------------------------------*
      *                          * Tipo riga : Prodotto di vendita     *
      *                          *-------------------------------------*
           if        w-tes-ass-iva (1)    not  = zero
                     move  w-tes-ass-iva (1)
                                          to   w-rig-coi-rig (1)
           else      move  w-let-dcp-pdx-coi
                                          to   w-rig-coi-rig (1)      .
           go to     car-rig-cat-420.
       car-rig-cat-414.
      *                          *-------------------------------------*
      *                          * Tipo riga : Addebito                *
      *                          *-------------------------------------*
           move      w-let-arc-zac-civ    to   w-edt-iva-cod          .
           if        w-tes-ass-iva (1)    not  = zero and
                     w-edt-iva-cod-003    <    1
                     move  w-tes-ass-iva (1)
                                          to   w-rig-coi-rig (1)
           else      move  w-let-arc-zac-civ
                                          to   w-rig-coi-rig (1)      .
           go to     car-rig-cat-420.
       car-rig-cat-420.
      *                      *-----------------------------------------*
      *                      * Contropartita vendite                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-430.
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ri- *
      *                          * ga                                  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-422
           else if   w-rig-tip-rig-tpr (1)
                                          =    "A"
                     go to car-rig-cat-424
           else      go to car-rig-cat-430.
       car-rig-cat-422.
      *                          *-------------------------------------*
      *                          * Tipo riga : Prodotto di vendita     *
      *                          *-------------------------------------*
           if        w-tes-ctp-ven (1)    not  = zero
                     move  w-tes-ctp-ven (1)
                                          to   w-rig-ctv-rig (1)
           else      move  w-let-dcp-pdx-ctp
                                          to   w-rig-ctv-rig (1)      .
           go to     car-rig-cat-430.
       car-rig-cat-424.
      *                          *-------------------------------------*
      *                          * Tipo riga : Addebito                *
      *                          *-------------------------------------*
           move      w-let-arc-zac-ccp    to   w-rig-ctv-rig (1)      .
           go to     car-rig-cat-430.
       car-rig-cat-430.
      *                      *-----------------------------------------*
      *                      * Unita' di misura                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ri- *
      *                          * ga                                  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-432
           else if   w-rig-tip-rig-tpr (1)
                                          =    "M"
                     go to car-rig-cat-434
           else if   w-rig-tip-rig-tpr (1)
                                          =    "S"
                     go to car-rig-cat-436
           else      go to car-rig-cat-440.
       car-rig-cat-432.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          *-------------------------------------*
           move      w-let-dcp-pdx-umi    to   w-rig-umi-ven (1)      .
           go to     car-rig-cat-440.
       car-rig-cat-434.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Materia prima       *
      *                          *-------------------------------------*
           move      w-let-arc-dpm-umi    to   w-rig-umi-ven (1)      .
           go to     car-rig-cat-440.
       car-rig-cat-436.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Semilavorato        *
      *                          *-------------------------------------*
           move      w-let-arc-dps-umi    to   w-rig-umi-ven (1)      .
           go to     car-rig-cat-440.
       car-rig-cat-440.
      *                      *-----------------------------------------*
      *                      * Decimali quantita'                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ri- *
      *                          * ga                                  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-442
           else if   w-rig-tip-rig-tpr (1)
                                          =    "M"
                     go to car-rig-cat-444
           else if   w-rig-tip-rig-tpr (1)
                                          =    "S"
                     go to car-rig-cat-446
           else      go to car-rig-cat-450.
       car-rig-cat-442.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          *-------------------------------------*
           move      w-let-dcp-pdx-deq    to   w-rig-dec-qta (1)      .
           go to     car-rig-cat-450.
       car-rig-cat-444.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Materia prima       *
      *                          *-------------------------------------*
           move      w-let-arc-dpm-deq    to   w-rig-dec-qta (1)      .
           go to     car-rig-cat-450.
       car-rig-cat-446.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Semilavorato        *
      *                          *-------------------------------------*
           move      w-let-arc-dps-deq    to   w-rig-dec-qta (1)      .
           go to     car-rig-cat-450.
       car-rig-cat-450.
      *                      *-----------------------------------------*
      *                      * Quantita' di vendita                    *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     move  zero           to   w-rig-qta-ven (1)
           else      move  w-brb-qta-dup
                          (w-car-rig-cat-ctr)
                                          to   w-rig-qta-ven (1)      .
       car-rig-cat-470.
      *                      *-----------------------------------------*
      *                      * Seconda quantita'                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Parametri memorizzati               *
      *                          *-------------------------------------*
           move      rf-bir-flg-puq       to   w-rig-flg-puq (1)      .
           move      rf-bir-snx-2qt       to   w-rig-snx-2qt (1)      .
           move      rf-bir-dec-2qt       to   w-rig-dec-2qt (1)      .
           move      rf-bir-qta-a02       to   w-rig-qta-a02 (1)      .
      *                          *-------------------------------------*
      *                          * Test se trattamento necessario      *
      *                          *-------------------------------------*
           if        w-rig-snx-2qt (1)    not  = 1
                     go to car-rig-cat-480.
           if        w-rig-qta-ven (1)    =    zero
                     move  zero           to   w-rig-qta-a02 (1)
                     go to car-rig-cat-480.
      *                          *-------------------------------------*
      *                          * Determinazione                      *
      *                          *-------------------------------------*
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
       car-rig-cat-480.
      *                      *-----------------------------------------*
      *                      * Terza quantita' : non gestita           *
      *                      *-----------------------------------------*
           move      rf-bir-snx-3qt       to   w-rig-snx-3qt (1)      .
           move      rf-bir-dec-3qt       to   w-rig-dec-3qt (1)      .
           move      rf-bir-qta-a03       to   w-rig-qta-a03 (1)      .
       car-rig-cat-490.
      *                      *-----------------------------------------*
      *                      * Decimali prezzo                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-500.
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ri- *
      *                          * ga                                  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-492
           else if   w-rig-tip-rig-tpr (1)
                                          =    "M"
                     go to car-rig-cat-496
           else if   w-rig-tip-rig-tpr (1)
                                          =    "S"
                     go to car-rig-cat-498
           else      go to car-rig-cat-500.
       car-rig-cat-492.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Quello dell'ordine              *
      *                              *---------------------------------*
           move      rf-ocr-dec-prz       to   w-rig-dec-prz (1)      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-500.
       car-rig-cat-496.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Materia prima       *
      *                          *-------------------------------------*
           move      zero                 to   w-rig-dec-prz (1)      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-500.
       car-rig-cat-498.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Semilavorato        *
      *                          *-------------------------------------*
           move      zero                 to   w-rig-dec-prz (1)      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-500.
       car-rig-cat-500.
      *                      *-----------------------------------------*
      *                      * Valuta per prezzi standard              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-510.
      *                          *-------------------------------------*
      *                          * Se tipo riga diverso da 'P' : oltre *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P"
                     go to car-rig-cat-510.
      *                          *-------------------------------------*
      *                          * Se prodotto similare : oltre        *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tfu (1)
                                          =    "N"
                     go to car-rig-cat-510.
      *                          *-------------------------------------*
      *                          * Quelli dell'ordine                  *
      *                          *-------------------------------------*
           move      rf-ocr-sgl-vps       to   w-rig-sgl-vps (1)      .
           move      rf-ocr-dec-vps       to   w-rig-dec-vps (1)      .
           move      rf-ocr-tdc-vps       to   w-rig-tdc-vps (1)      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-510.
       car-rig-cat-510.
      *                      *-----------------------------------------*
      *                      * Prezzo lordo standard                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-520.
      *                          *-------------------------------------*
      *                          * Se tipo riga diverso da 'P' : oltre *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P"
                     go to car-rig-cat-520.
      *                          *-------------------------------------*
      *                          * Se prodotto similare : oltre        *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tfu (1)
                                          =    "N"
                     go to car-rig-cat-520.
      *                          *-------------------------------------*
      *                          * Quello dell'ordine                  *
      *                          *-------------------------------------*
           move      rf-ocr-prz-lrs       to   w-rig-prz-lrs (1)      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-520.
       car-rig-cat-520.
      *                      *-----------------------------------------*
      *                      * Valuta per il prezzo                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-540.
      *                          *-------------------------------------*
      *                          * Solo se tipo riga 'P' o 'M' o 'S'   *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P" and
                     w-rig-tip-rig-tpr (1)
                                          not  = "M" and
                     w-rig-tip-rig-tpr (1)
                                          not  = "S"
                     go to car-rig-cat-540.
      *                          *-------------------------------------*
      *                          * Bufferizzazione da testata          *
      *                          *-------------------------------------*
           move      w-tes-sgl-vpf (1)    to   w-rig-sgl-vpp (1)      .
           move      w-tes-dec-vpf (1)    to   w-rig-dec-vpp (1)      .
           move      w-tes-tdc-vpf (1)    to   w-rig-tdc-vpp (1)      .
           move      w-tes-cdc-vpf (1)    to   w-rig-cdc-vpp (1)      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-540.
       car-rig-cat-540.
      *                      *-----------------------------------------*
      *                      * Prezzo di vendita                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-550.
      *                          *-------------------------------------*
      *                          * Quello dell'ordine                  *
      *                          *-------------------------------------*
           move      rf-ocr-prz-ven       to   w-rig-prz-ven (1)      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-550.
       car-rig-cat-550.
      *                      *-----------------------------------------*
      *                      * Secondo prezzo : non gestito            *
      *                      *-----------------------------------------*
       car-rig-cat-560.
      *                      *-----------------------------------------*
      *                      * Legame valutario : non gestito          *
      *                      *-----------------------------------------*
       car-rig-cat-570.
      *                      *-----------------------------------------*
      *                      * Tipo esposizione riga in fattura        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-580.
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ri- *
      *                          * ga                                  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-572
           else if   w-rig-tip-rig-tpr (1)
                                          =    "M"
                     go to car-rig-cat-574
           else if   w-rig-tip-rig-tpr (1)
                                          =    "S"
                     go to car-rig-cat-576
           else      go to car-rig-cat-580.
       car-rig-cat-572.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          *-------------------------------------*
           move      w-let-dcp-pdx-epz    to   w-rig-epz-rgf (1)      .
           go to     car-rig-cat-580.
       car-rig-cat-574.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Materia prima       *
      *                          *-------------------------------------*
           move      01                   to   w-rig-epz-rgf (1)      .
           go to     car-rig-cat-580.
       car-rig-cat-576.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Semilavorato        *
      *                          *-------------------------------------*
           move      01                   to   w-rig-epz-rgf (1)      .
           go to     car-rig-cat-580.
       car-rig-cat-580.
      *                      *-----------------------------------------*
      *                      * Categoria e percentuali di sconto in    *
      *                      * riga associate al prodotto              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-590.
      *                          *-------------------------------------*
      *                          * Se tipo riga diverso da 'P' : oltre *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P"
                     go to car-rig-cat-590.
      *                          *-------------------------------------*
      *                          * Se prodotto similare : oltre        *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tfu (1)
                                          =    "N"
                     go to car-rig-cat-590.
      *                          *-------------------------------------*
      *                          * Quelli dell'ordine                  *
      *                          *-------------------------------------*
           move      rf-ocr-csr-aap       to   w-rig-csr-aap (1)      .
           move      rf-ocr-psr-aap (1)   to   w-rig-psr-aap (1, 1)   .
           move      rf-ocr-psr-aap (2)   to   w-rig-psr-aap (1, 2)   .
           move      rf-ocr-psr-aap (3)   to   w-rig-psr-aap (1, 3)   .
           move      rf-ocr-psr-aap (4)   to   w-rig-psr-aap (1, 4)   .
           move      rf-ocr-psr-aap (5)   to   w-rig-psr-aap (1, 5)   .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-590.
       car-rig-cat-590.
      *                      *-----------------------------------------*
      *                      * Sconti in riga                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-600.
      *                          *-------------------------------------*
      *                          * Quelli dell'ordine                  *
      *                          *-------------------------------------*
           move      rf-ocr-per-scr (1)   to   w-rig-per-scr (1, 1)   .
           move      rf-ocr-per-scr (2)   to   w-rig-per-scr (1, 2)   .
           move      rf-ocr-per-scr (3)   to   w-rig-per-scr (1, 3)   .
           move      rf-ocr-per-scr (4)   to   w-rig-per-scr (1, 4)   .
           move      rf-ocr-per-scr (5)   to   w-rig-per-scr (1, 5)   .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-600.
       car-rig-cat-600.
      *                      *-----------------------------------------*
      *                      * Tipo esposizione prezzi e sconti        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-610.
      *                          *-------------------------------------*
      *                          * Se tipo riga 'A' o 'C' : oltre      *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-610.
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      w-tes-epz-pes (1)    to   w-rig-epz-pes (1)      .
       car-rig-cat-610.
      *                      *-----------------------------------------*
      *                      * Valuta per costo di riferimento : non   *
      *                      * gestita                                 *
      *                      *-----------------------------------------*
       car-rig-cat-620.
      *                      *-----------------------------------------*
      *                      * Costo di riferimento : non gestito      *
      *                      *-----------------------------------------*
       car-rig-cat-630.
      *                      *-----------------------------------------*
      *                      * Importo in riga                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-635.
      *                          *-------------------------------------*
      *                          * Se tipo riga diverso da 'A' : oltre *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "A"
                     go to car-rig-cat-635.
      *                          *-------------------------------------*
      *                          * Quello dell'ordine                  *
      *                          *-------------------------------------*
           move      rf-ocr-imp-rig       to   w-rig-imp-rig (1)      .
       car-rig-cat-635.
      *                      *-----------------------------------------*
      *                      * Importo ausiliario riga : non gestito   *
      *                      *-----------------------------------------*
       car-rig-cat-640.
      *                      *-----------------------------------------*
      *                      * Categoria e percentuali di provvigione  *
      *                      * associate al prodotto                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-650.
      *                          *-------------------------------------*
      *                          * Se tipo riga diverso da 'P' : oltre *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P"
                     go to car-rig-cat-650.
      *                          *-------------------------------------*
      *                          * Se prodotto similare : oltre        *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tfu (1)
                                          =    "N"
                     go to car-rig-cat-650.
      *                          *-------------------------------------*
      *                          * Quelli dell'ordine                  *
      *                          *-------------------------------------*
           move      rf-ocr-cpv-aap       to   w-rig-cpv-aap (1)      .
           move      rf-ocr-ppv-aap (1)   to   w-rig-ppv-aap (1, 1)   .
           move      rf-ocr-ppv-aap (2)   to   w-rig-ppv-aap (1, 2)   .
           move      rf-ocr-ppv-aap (3)   to   w-rig-ppv-aap (1, 3)   .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-650.
       car-rig-cat-650.
      *                      *-----------------------------------------*
      *                      * Flag di significativita' provvigioni in *
      *                      * riga                                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-660.
      *                          *-------------------------------------*
      *                          * Se gestione agenti non attiva : ol- *
      *                          * tre                                 *
      *                          *-------------------------------------*
           if        w-prs-age-snx        not  = "S"
                     go to car-rig-cat-660.
      *                          *-------------------------------------*
      *                          * Se flag significativita' provvigio- *
      *                          * ni per il documento indica 'No' o   *
      *                          * 'A forfait': No provvigioni in riga *
      *                          *-------------------------------------*
           if        w-tes-fsp-doc (1)    =    02 or
                     w-tes-fsp-doc (1)    =    03
                     move  02             to   w-rig-fsp-rig (1)
                     go to car-rig-cat-660.
      *                          *-------------------------------------*
      *                          * Se flag significativita' provvigio- *
      *                          * ni per il documento indica 'A for-  *
      *                          * fait' in% : Forzatura provvigioni   *
      *                          * in riga                             *
      *                          *-------------------------------------*
           if        w-tes-fsp-doc (1)    =    04
                     move   03            to   w-rig-fsp-rig (1)
                     divide 100           into w-tes-pvf-age (1)
                                        giving w-rig-ppv-rig (1, 1)
                     go to car-rig-cat-660.
      *                          *-------------------------------------*
      *                          * Quello dell'ordine                  *
      *                          *-------------------------------------*
           move      rf-ocr-fsp-rig       to   w-rig-fsp-rig (1)      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-660.
       car-rig-cat-660.
      *                      *-----------------------------------------*
      *                      * Categoria provvigioni in riga : non ge- *
      *                      * stito                                   *
      *                      *-----------------------------------------*
       car-rig-cat-670.
      *                      *-----------------------------------------*
      *                      * Percentuali di provvigione in riga      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se documento che non interessa la   *
      *                          * fatturazione : oltre                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-680.
      *                          *-------------------------------------*
      *                          * Se tipo riga diverso da 'P' : oltre *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P"
                     go to car-rig-cat-680.
      *                          *-------------------------------------*
      *                          * Se prodotto similare : oltre        *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tfu (1)
                                          =    "N"
                     go to car-rig-cat-680.
      *                          *-------------------------------------*
      *                          * Se flag significativita' provvigio- *
      *                          * ni a No : oltre                     *
      *                          *-------------------------------------*
           if        w-rig-fsp-rig (1)    =    02
                     go to car-rig-cat-680.
      *                          *-------------------------------------*
      *                          * Quelli dell'ordine                  *
      *                          *-------------------------------------*
           move      rf-ocr-ppv-rig (1)   to   w-rig-ppv-rig (1, 1)   .
           move      rf-ocr-ppv-rig (2)   to   w-rig-ppv-rig (1, 2)   .
           move      rf-ocr-ppv-rig (3)   to   w-rig-ppv-rig (1, 3)   .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-680.
       car-rig-cat-680.
      *                      *-----------------------------------------*
      *                      * Provvigione a forfait in riga : zero    *
      *                      *-----------------------------------------*
           move      zero                 to   w-rig-pvf-rig (1)      .
       car-rig-cat-690.
      *                      *-----------------------------------------*
      *                      * Altri valori rimangono normalizzati     *
      *                      *-----------------------------------------*
       car-rig-cat-800.
      *                      *-----------------------------------------*
      *                      * Valori indotti                          *
      *                      *-----------------------------------------*
       car-rig-cat-805.
      *                          *-------------------------------------*
      *                          * Flag di estensione alla descrizione *
      *                          *-------------------------------------*
           move      rf-bir-des-ext       to   w-rig-des-ext (1)      .
       car-rig-cat-810.
      *                          *-------------------------------------*
      *                          * Se il documento non interessa la    *
      *                          * fatturazione : a fine trattamento   *
      *                          * riga                                *
      *                          *-------------------------------------*
           if        w-tes-int-ftr        =    01
                     go to car-rig-cat-840.
       car-rig-cat-815.
      *                          *-------------------------------------*
      *                          * Anagrafica titoli esenzione         *
      *                          *-------------------------------------*
           move      w-rig-coi-rig (1)    to   w-let-arc-zci-cod      .
           perform   let-arc-zci-000      thru let-arc-zci-999        .
           move      w-let-arc-zci-des    to   w-rig-coi-rig-des (1)  .
       car-rig-cat-820.
      *                          *-------------------------------------*
      *                          * Anagrafica piano dei conti          *
      *                          *-------------------------------------*
           move      w-rig-ctv-rig (1)    to   w-let-arc-pdc-cod      .
           perform   let-arc-pdc-000      thru let-arc-pdc-999        .
           move      w-let-arc-pdc-des    to   w-rig-ctv-rig-des (1)  .
       car-rig-cat-825.
      *                          *-------------------------------------*
      *                          * Prezzo netto standard               *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se tipo riga diverso da 'P' :   *
      *                              * oltre                           *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P"
                     go to car-rig-cat-830.
      *                              *---------------------------------*
      *                              * Se prodotto similare : oltre    *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tfu (1)
                                          =    "N"
                     go to car-rig-cat-830.
      *                          *-------------------------------------*
      *                          * Quello dell'ordine                  *
      *                          *-------------------------------------*
           move      rf-ocr-prz-nts       to   w-rig-prz-nts (1)      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-830.
       car-rig-cat-830.
      *                          *-------------------------------------*
      *                          * Prezzo netto effettivo              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se tipo riga 'A' o 'C' : oltre  *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-835.
      *                              *---------------------------------*
      *                              * Determinazione                  *
      *                              *---------------------------------*
           move      w-rig-prz-ven (1)    to   w-cal-prz-net-prz      .
           move      w-rig-per-scr (1, 1) to   w-cal-prz-net-psc (1)  .
           move      w-rig-per-scr (1, 2) to   w-cal-prz-net-psc (2)  .
           move      w-rig-per-scr (1, 3) to   w-cal-prz-net-psc (3)  .
           move      w-rig-per-scr (1, 4) to   w-cal-prz-net-psc (4)  .
           move      w-rig-per-scr (1, 5) to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   w-rig-prz-net (1)      .
       car-rig-cat-835.
      *                          *-------------------------------------*
      *                          * Importo in riga                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se tipo riga 'A' o 'C' : oltre  *
      *                              *---------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     go to car-rig-cat-840.
      *                              *---------------------------------*
      *                              * Determinazione                  *
      *                              *---------------------------------*
           perform   det-imp-rig-000      thru det-imp-rig-999        .
       car-rig-cat-840.
      *                      *-----------------------------------------*
      *                      * Riferimenti ordine originale            *
      *                      *-----------------------------------------*
           move      rf-ocr-ocl-dat       to   w-rig-ocl-dat (1)      .
           move      rf-ocr-ocl-num       to   w-rig-ocl-num (1)      .
           move      rf-ocr-tmo-orc       to   w-rig-coc-tip (1)      .
           move      rf-ocr-dat-doc       to   w-rig-coc-dat (1)      .
           move      rf-ocr-num-doc       to   w-rig-coc-num (1)      .
           move      rf-bir-coc-prt       to   w-rig-coc-prt (1)      .
           move      rf-bir-coc-prg       to   w-rig-coc-prg (1)      .
      *                      *-----------------------------------------*
      *                      * Elementi riga di evasione               *
      *                      *-----------------------------------------*
           move      w-acc-num-prt        to   w-rig-prt-rif (1)      .
           move      rf-bir-num-prg       to   w-rig-prg-rif (1)      .
      *                      *-----------------------------------------*
      *                      * Attivazione del flag non bloccante da   *
      *                      * considerare ai fini dell'evasione       *
      *                      *-----------------------------------------*
           move      "#"                  to   w-rig-flg-nbx (1, 3)   .
       car-rig-cat-850.
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
       car-rig-cat-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     car-rig-cat-999.
       car-rig-cat-950.
      *              *-------------------------------------------------*
      *              * Messaggio di errore per esubero numero righe    *
      *              *-------------------------------------------------*
           move      "Numero righe documento oltre il massimo previsto !
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       car-rig-cat-999.
           exit.

      *    *===========================================================*
      *    * Caricamento righe in catena                               *
      *    *                                                           *
      *    * Emissione prima riga di commento                          *
      *    *-----------------------------------------------------------*
       car-rig-cat-erc-000.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Append in catena                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se Append non possibile : uscita con    *
      *                      * errore                                  *
      *                      *-----------------------------------------*
           if        w-cat-rig-app        not  = spaces
                     go to car-rig-cat-erc-900.
      *                      *-----------------------------------------*
      *                      * Append                                  *
      *                      *-----------------------------------------*
           move      "AP"                 to   w-cat-rig-ope          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
       car-rig-cat-erc-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione riga                        *
      *                  *---------------------------------------------*
           perform   nor-nok-rig-000      thru nor-nok-rig-999        .
           move      w-rig-val-aep (1)    to   w-rig-val-aep (2)      .
       car-rig-cat-erc-300.
      *              *-------------------------------------------------*
      *              * Composizione riga                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero progressivo riga                     *
      *                  *---------------------------------------------*
           move      w-cat-rig-prg        to   w-rig-num-prg (1)      .
      *                  *---------------------------------------------*
      *                  * Tipo riga                                   *
      *                  *---------------------------------------------*
           move      "C    "              to   w-rig-tip-rig (1)      .
           move      "C"                  to   w-rig-tip-rig-tpr (1)  .
      *                  *---------------------------------------------*
      *                  * Editing numero bolla da cui si copia        *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      w-acc-num-doc-prg    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Concatenamento ns. numero conferma ordine   *
      *                  *---------------------------------------------*
           move      120                  to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "DDT."               to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                  *---------------------------------------------*
      *                  * Editing ns. data bolla da cui si copia      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-acc-dat-doc        to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Concatenamento ns. data bolla               *
      *                  *---------------------------------------------*
           move      120                  to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-all-str-alf        to   w-all-str-cat (1)      .
           move      "del"                to   w-all-str-cat (2)      .
           move      v-edt                to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                  *---------------------------------------------*
      *                  * Descrizione riga                            *
      *                  *---------------------------------------------*
           move      w-all-str-alf        to   w-rig-des-rig (1)      .
       car-rig-cat-erc-800.
      *              *-------------------------------------------------*
      *              * Scrittura riga                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Update catena                               *
      *                  *---------------------------------------------*
           move      "UP"                 to   w-cat-rig-ope          .
           move      w-cat-rig-max        to   w-cat-rig-num          .
           move      w-rig                to   w-cat-rig-buf          .
           perform   cll-sub-cat-000      thru cll-sub-cat-999        .
       car-rig-cat-erc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     car-rig-cat-erc-999.
       car-rig-cat-erc-999.
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
           move      w-let-arc-zbi-sgl    to   w-acc-cod-tmb-sgl      .
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
       let-arc-zbi-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zfi]                         *
      *    *-----------------------------------------------------------*
       let-arc-zfi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zfi-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zfi-cod    =    spaces
                     go to let-arc-zfi-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMO"             to   f-key                  .
           move      w-let-arc-zfi-cod    to   rf-zfi-cod-tmo         .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zfi-400.
       let-arc-zfi-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zfi-vld-dpz       to   w-let-arc-zfi-vld      .
           move      rf-zfi-cod-dpz       to   w-let-arc-zfi-dpz      .
           move      rf-zfi-tip-doc       to   w-let-arc-zfi-tdo      .
           move      rf-zfi-org-doc       to   w-let-arc-zfi-ord      .
           move      rf-zfi-prv-doc       to   w-let-arc-zfi-prd      .
           move      rf-zfi-num-giv       to   w-let-arc-zfi-ngi      .
           move      rf-zfi-sgl-num       to   w-let-arc-zfi-sgl      .
           move      rf-zfi-cau-cge       to   w-let-arc-zfi-cau      .
           move      rf-zfi-ctp-ivv       to   w-let-arc-zfi-siv      .
           move      rf-zfi-ctp-ven       to   w-let-arc-zfi-sve      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zfi-999.
       let-arc-zfi-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zfi-flg      .
       let-arc-zfi-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-let-arc-zfi-vld      .
           move      zero                 to   w-let-arc-zfi-dpz      .
           move      zero                 to   w-let-arc-zfi-tdo      .
           move      zero                 to   w-let-arc-zfi-ord      .
           move      zero                 to   w-let-arc-zfi-prd      .
           move      zero                 to   w-let-arc-zfi-ngi      .
           move      spaces               to   w-let-arc-zfi-sgl      .
           move      zero                 to   w-let-arc-zfi-cau      .
           move      zero                 to   w-let-arc-zfi-siv      .
           move      zero                 to   w-let-arc-zfi-sve      .
       let-arc-zfi-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dpm]                         *
      *    *-----------------------------------------------------------*
       let-arc-dpm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dpm-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dpm-cod    =    zero
                     go to let-arc-dpm-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMMAP"             to   f-key                  .
           move      w-let-arc-dpm-cod    to   rf-dpm-num-map         .
           move      "pgm/dpm/fls/ioc/obj/iofdpm"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dpm                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dpm-400.
       let-arc-dpm-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dpm-des-map       to   w-let-arc-dpm-des      .
           move      rf-dpm-umi-prd       to   w-let-arc-dpm-umi      .
           move      rf-dpm-dec-qta       to   w-let-arc-dpm-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dpm-999.
       let-arc-dpm-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dpm-flg      .
           move      all   "."            to   w-let-arc-dpm-des      .
           go to     let-arc-dpm-600.
       let-arc-dpm-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dpm-des      .
       let-arc-dpm-600.
           move      spaces               to   w-let-arc-dpm-umi      .
           move      zero                 to   w-let-arc-dpm-deq      .
       let-arc-dpm-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [dps]                         *
      *    *-----------------------------------------------------------*
       let-arc-dps-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dps-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a zero                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dps-cod    =    zero
                     go to let-arc-dps-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSEM"             to   f-key                  .
           move      w-let-arc-dps-cod    to   rf-dps-num-sem         .
           move      "pgm/dps/fls/ioc/obj/iofdps"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dps                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dps-400.
       let-arc-dps-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dps-des-sem       to   w-let-arc-dps-des      .
           move      rf-dps-umi-prd       to   w-let-arc-dps-umi      .
           move      rf-dps-dec-qta       to   w-let-arc-dps-deq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dps-999.
       let-arc-dps-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dps-flg      .
           move      all   "."            to   w-let-arc-dps-des      .
           go to     let-arc-dps-600.
       let-arc-dps-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dps-des      .
       let-arc-dps-600.
           move      spaces               to   w-let-arc-dps-umi      .
           move      zero                 to   w-let-arc-dps-deq      .
       let-arc-dps-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zac]                         *
      *    *-----------------------------------------------------------*
       let-arc-zac-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zac-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici nulli                            *
      *              *-------------------------------------------------*
           if        w-let-arc-zac-tip    =    zero or
                     w-let-arc-zac-cod    =    zero
                     go to let-arc-zac-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAOC    "         to   f-key                  .
           move      w-let-arc-zac-tip    to   rf-zac-tip-rec         .
           move      w-let-arc-zac-cod    to   rf-zac-cod-aoc         .
           move      "pgm/fat/fls/ioc/obj/iofzac"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zac                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zac-400.
       let-arc-zac-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zac-des-aoc       to   w-let-arc-zac-des      .
           move      rf-zac-cod-iva       to   w-let-arc-zac-civ      .
           move      rf-zac-cod-ctp       to   w-let-arc-zac-ccp      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zac-999.
       let-arc-zac-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zac-flg      .
           move      spaces               to   w-let-arc-zac-des      .
           move      all   "."            to   w-let-arc-zac-drg (1)  .
           go to     let-arc-zac-600.
       let-arc-zac-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zac-des      .
       let-arc-zac-600.
           move      zero                 to   w-let-arc-zac-civ      .
           move      zero                 to   w-let-arc-zac-ccp      .
       let-arc-zac-999.
           exit.

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
      *    * Routine di lettura archivio [zci]                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/larczci0.lts"                   .

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
      *    * Routine lettura archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.lts"                   .

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
                     move  w-tes-cod-arc (1)
                                          to   d-pvg-age-cod-cli
           else      move  w-tes-arc-plf (1)
                                          to   d-pvg-age-cod-cli      .
           move      w-tes-cpv-aac (1)    to   d-pvg-age-cpv-aac      .
           move      w-tes-ppv-aac (1, 1) to   d-pvg-age-ppv-aac (1)  .
           move      w-tes-ppv-aac (1, 2) to   d-pvg-age-ppv-aac (2)  .
           move      w-tes-ppv-aac (1, 3) to   d-pvg-age-ppv-aac (3)  .
           move      w-rig-sgl-vpp (1)    to   d-pvg-age-sgl-vpp      .
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
      *                      * valuta per il prezzo                    *
      *                      *-----------------------------------------*
           move      w-rig-prz-lrs (1)    to   d-pvg-age-prz-lrs      .
      *                      *-----------------------------------------*
      *                      * Eventuale conversione da valuta per     *
      *                      * prezzi standard a valuta per il prezzo  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se necessario                  *
      *                          *-------------------------------------*
           if        w-rig-sgl-vps (1)    =    w-rig-sgl-vpp (1)
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
      *                          * valuta per il prezzo                *
      *                          *-------------------------------------*
           move      w-rig-sgl-vpp (1)    to   w-cvs-vlt-sgl          .
           move      w-rig-dec-vpp (1)    to   w-cvs-vlt-dec          .
           move      w-rig-tdc-vpp (1)    to   w-cvs-vlt-tdc          .
           move      w-rig-cdc-vpp (1)    to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
           move      w-cvs-vlt-aav        to   d-pvg-age-prz-lrs      .
       det-ppv-rig-220.
      *                      *-----------------------------------------*
      *                      * Prezzo netto standard espresso nella    *
      *                      * valuta per il prezzo                    *
      *                      *-----------------------------------------*
           move      w-rig-prz-nts (1)    to   d-pvg-age-prz-nts      .
      *                      *-----------------------------------------*
      *                      * Eventuale conversione da valuta per     *
      *                      * prezzi standard a valuta per il prezzo  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se necessario                  *
      *                          *-------------------------------------*
           if        w-rig-sgl-vps (1)    =    w-rig-sgl-vpp (1)
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
      *                          * valuta per il prezzo                *
      *                          *-------------------------------------*
           move      w-rig-sgl-vpp (1)    to   w-cvs-vlt-sgl          .
           move      w-rig-dec-vpp (1)    to   w-cvs-vlt-dec          .
           move      w-rig-tdc-vpp (1)    to   w-cvs-vlt-tdc          .
           move      w-rig-cdc-vpp (1)    to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
           move      w-cvs-vlt-aav        to   d-pvg-age-prz-nts      .
       det-ppv-rig-240.
      *                      *-----------------------------------------*
      *                      * Prezzo netto effettivo, gia' espresso   *
      *                      * nella valuta per il prezzo              *
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
      *    * Determinazione prezzo sottoposto a legame valutario       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wlvlprz0.cps"                   .

      *    *===========================================================*
      *    * Routine di conversione da altra valuta a valuta base      *
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
      *    * Subroutines per l'accettazione tipo movimento per bolla   *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/acdezbi0.acs"                   .

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

