       Identification Division.
       Program-Id.                                 porc300f           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    orc                 *
      *                                Settore:    mov                 *
      *                                   Fase:    orc300f             *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/07/92    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Evasione righe ordine 'forecast'            *
      *                                                                *
      *                    Richiamata in orc300 da funzione 'PF2'      *
      *                                                                *
      *================================================================*
      *                                                                *
      * Tipi operazione                                                *
      *                                                                *
      * "OP" - Open, inizio utilizzo                                   *
      *                                                                *
      *        Input  : l-eva-frc-tip-ope = "OP"                       *
      *                 l-eva-frc-cod-dpz = codice dipendenza in uso   *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "CL" - Close, fine utilizzo                                    *
      *                                                                *
      *        Input  : l-eva-frc-tip-ope = "CL"                       *
      *                                                                *
      *        Output : nessuno                                        *
      *                                                                *
      * "C?" - Test se modulo cancellabile                             *
      *                                                                *
      *        Input  : l-eva-frc-tip-ope = "C?"                       *
      *                                                                *
      *        Output : l-eva-frc-exi-sts = spaces: Si                 *
      *                                     #     : No                 *
      *                                                                *
      * "RO" - Accettazione dati identificativi ordine                 *
      *                                                                *
      *        Input  : l-eva-frc-tip-ope = "RO"                       *
      *                 l-eva-frc-tmo-orc = codice tipo movimento or-  *
      *                                     dine da proporre come de-  *
      *                                     fault                      *
      *                                                                *
      *        Output : l-eva-frc-exi-sts = spaces: Accettazione ese-  *
      *                                             guita con successo *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "SC" - Saldaconto per selezione righe ordine                   *
      *                                                                *
      *        Input  : l-eva-frc-tip-ope = "SC"                       *
      *                                                                *
      *        Output : l-eva-frc-exi-sts = spaces: Tutto Ok           *
      *                                     #     : Uscita per Exit    *
      *                                                                *
      * "CC" - Caricamento righe ordine selezionate in catena          *
      *                                                                *
      *        Input  : l-eva-frc-tip-ope = "CC"                       *
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
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [oct]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfoct"                          .
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .
      *        *-------------------------------------------------------*
      *        * [ocx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocx"                          .
      *        *-------------------------------------------------------*
      *        * [zoc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfzoc"                          .
      *        *-------------------------------------------------------*
      *        * [zac]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzac"                          .
      *        *-------------------------------------------------------*
      *        * [pdc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfpdc"                          .
      *        *-------------------------------------------------------*
      *        * [zci]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfzci"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .

      *    *===========================================================*
      *    * Work-area generica per il programma                       *
      *    *-----------------------------------------------------------*
       01  w-gen.
      *        *-------------------------------------------------------*
      *        * Contatore di Open modulo                              *
      *        *-------------------------------------------------------*
           05  w-gen-ctr-opn              pic s9(05) value zero       .
      *        *-------------------------------------------------------*
      *        * Numero protocollo ordine in corso di trattamento      *
      *        *-------------------------------------------------------*
           05  w-gen-prt-orc              pic  9(11)                  .

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
      *        * Tipo movimento ordini clienti                         *
      *        *-------------------------------------------------------*
           05  w-acc-tmo-orc              pic  x(05)                  .
           05  w-acc-tmo-orc-des          pic  x(30)                  .
           05  w-acc-tmo-orc-sgl          pic  x(03)                  .
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
      *    * Work-area per bufferizzazione righe ordine da evadere     *
      *    *-----------------------------------------------------------*
       01  w-bro.
      *        *-------------------------------------------------------*
      *        * Comodi di ordinamento                                 *
      *        *-------------------------------------------------------*
           05  w-bro-key-ord              pic  9(05)                  .
           05  w-bro-key-svk              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero elementi in tabella                            *
      *        *-------------------------------------------------------*
           05  w-bro-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Numero massimo elementi in tabella                    *
      *        *-------------------------------------------------------*
           05  w-bro-max-ele              pic  9(05) value 900        .
      *        *-------------------------------------------------------*
      *        * Tabella elementi                                      *
      *        *-------------------------------------------------------*
           05  w-bro-tbl.
               10  w-bro-sng-ele occurs 900.
                   15  w-bro-key-prg      pic  9(05)       comp-3     .
                   15  w-bro-num-prg      pic  9(05)       comp-3     .
                   15  w-bro-num-pro      pic  9(07)       comp-3     .
                   15  w-bro-tip-rig      pic  x(01)                  .
                   15  w-bro-des-rig      pic  x(40)                  .
                   15  w-bro-dec-qta      pic  9(01)                  .
                   15  w-bro-qta-orc      pic s9(10)v9(03) comp-3     .
                   15  w-bro-qta-eva      pic s9(10)v9(03) comp-3     .
                   15  w-bro-snx-aoc      pic  x(01)                  .

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
           05  w-sdc-wrk-c02              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per ricerca codice prodotto                      *
      *        *-------------------------------------------------------*
           05  w-sdc-fnd-pro.
               10  w-sdc-fnd-pro-alf      pic  x(14)                  .
               10  w-sdc-fnd-pro-num      pic  9(07)                  .

      *    *===========================================================*
      *    * Work-area per routine buf-rig-orc-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-buf-rig-orc.
      *        *-------------------------------------------------------*
      *        * Flag di uscita                                        *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-flg          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo ordine                              *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-prt          pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per ridefinizione tipo riga                    *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-wtr.
               10  w-buf-rig-orc-wtp      pic  x(01)                  .
               10  w-buf-rig-orc-wtf      pic  x(01)                  .
               10  filler                 pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione in riga                                   *
      *        *-------------------------------------------------------*
           05  w-buf-rig-orc-wde          pic  x(40)                  .

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
      *        * Contatori di lavoro                                   *
      *        *-------------------------------------------------------*
           05  w-car-rig-cat-ctr          pic  9(05)                  .
           05  w-car-rig-cat-c01          pic  9(05)                  .
           05  w-car-rig-cat-c02          pic  9(05)                  .
           05  w-car-rig-cat-c03          pic  9(05)                  .
           05  w-car-rig-cat-c04          pic  9(05)                  .

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
      *    * Work per subroutines di Select                            *
      *    *-----------------------------------------------------------*
       01  w-slc.
      *        *-------------------------------------------------------*
      *        * Work per Select numero documento                      *
      *        *-------------------------------------------------------*
           05  w-slc-num-oct.
      *            *---------------------------------------------------*
      *            * Valori in entrata                                 *
      *            *---------------------------------------------------*
               10  w-slc-num-oct-dds      pic  9(07)                  .
               10  w-slc-num-oct-nds      pic  9(11)                  .
               10  w-slc-num-oct-nds-r redefines
                   w-slc-num-oct-nds.
                   15  w-slc-num-oct-nsa  pic  9(03)                  .
                   15  w-slc-num-oct-ndp  pic  9(02)                  .
                   15  w-slc-num-oct-npg  pic  9(06)                  .
               10  w-slc-num-oct-dpz      pic  9(02)                  .
               10  w-slc-num-oct-sgl      pic  x(03)                  .
               10  w-slc-num-oct-saa      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Valori in uscita                                  *
      *            *---------------------------------------------------*
               10  w-slc-num-oct-sel      pic  x(01)                  .
               10  w-slc-num-oct-toc      pic  x(05)                  .
               10  w-slc-num-oct-dat      pic  9(07)                  .
               10  w-slc-num-oct-num      pic  9(11)                  .
               10  w-slc-num-oct-prt      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo                                    *
      *            *---------------------------------------------------*
               10  w-slc-num-oct-c01      pic  9(02)                  .
               10  w-slc-num-oct-c02      pic  9(02)                  .
               10  w-slc-num-oct-c03      pic  9(02)                  .
               10  w-slc-num-oct-c04      pic  9(02)                  .
               10  w-slc-num-oct-c05      pic  9(02)                  .
               10  w-slc-num-oct-nli      pic  9(02)                  .
               10  w-slc-num-oct-crb      pic  9(02)                  .
               10  w-slc-num-oct-cpb      pic  9(02)                  .
               10  w-slc-num-oct-cpa      pic  9(02)                  .
               10  w-slc-num-oct-buf
                               occurs 30.
                   15  w-slc-num-oct-bpt  pic  9(11)                  .
               10  w-slc-num-oct-ltp.
                   15  filler             pic  x(07) value "Pagina "  .
                   15  w-slc-num-oct-lt1  pic  9(01)                  .
                   15  filler             pic  x(04) value " di "     .
                   15  w-slc-num-oct-lt2  pic  9(01)                  .

      *    *===========================================================*
      *    * Work per subroutines di Find                              *
      *    *-----------------------------------------------------------*
       01  w-fnd.
      *        *-------------------------------------------------------*
      *        * Work per Find su archivio [oct]                       *
      *        *-------------------------------------------------------*
           05  w-fnd-arc-oct.
               10  w-fnd-arc-oct-sel      pic  x(01)                  .
               10  w-fnd-arc-oct-toc      pic  x(05)                  .
               10  w-fnd-arc-oct-dat      pic  9(07)                  .
               10  w-fnd-arc-oct-num      pic  9(11)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [zoc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-zoc.
               10  w-let-arc-zoc-flg      pic  x(01)                  .
               10  w-let-arc-zoc-cod      pic  x(05)                  .
               10  w-let-arc-zoc-des      pic  x(30)                  .
               10  w-let-arc-zoc-vld      pic  9(02)                  .
               10  w-let-arc-zoc-dpz      pic  9(02)                  .
               10  w-let-arc-zoc-ord      pic  9(02)                  .
               10  w-let-arc-zoc-prd      pic  9(02)                  .
               10  w-let-arc-zoc-sgl      pic  x(03)                  .
               10  w-let-arc-zoc-dtr      pic  x(05)                  .
               10  w-let-arc-zoc-tip      pic  x(01)                  .
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
      *        * Work per Let su archivio [ocx]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-ocx.
               10  w-let-arc-ocx-flg      pic  x(01)                  .
               10  w-let-arc-ocx-prt      pic  9(11)                  .
               10  w-let-arc-ocx-prg      pic  9(05)                  .
               10  w-let-arc-ocx-trc      pic  9(02)                  .
               10  w-let-arc-ocx-des.
                   15  w-let-arc-ocx-drg occurs 10
                                          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [pdc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-pdc.
               10  w-let-arc-pdc-flg      pic  x(01)                  .
               10  w-let-arc-pdc-cod      pic  9(07)                  .
               10  w-let-arc-pdc-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcp.
               10  w-let-arc-dcp-flg      pic  x(01)                  .
               10  w-let-arc-dcp-num      pic  9(07)                  .
               10  w-let-arc-dcp-alf      pic  x(14)                  .
               10  w-let-arc-dcp-des      pic  x(40)                  .
               10  w-let-arc-dcp-umi      pic  x(03)                  .
               10  w-let-arc-dcp-ndq      pic  9(01)                  .

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
      *            *---------------------------------------------------*
      *            * Protocollo movimento trovato                      *
      *            *---------------------------------------------------*
               10  w-det-doc-ges-prt      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Tipo ordine trovato                               *
      *            *---------------------------------------------------*
               10  w-det-doc-ges-tor      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Ordine controllato                                *
      *            *---------------------------------------------------*
               10  w-det-doc-ges-ctl      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente trovato                            *
      *            *---------------------------------------------------*
               10  w-det-doc-ges-car      pic  9(07)                  .

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
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine cliente                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dqevroc0.dtl"                   .

      *    *===========================================================*
      *    * Work-area per determinazione prezzo netto                 *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cpw"                   .

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
      *    * Link-area per accettazione tipo movimento ordini clienti  *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/acdezoc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice prodotto 'dcp'          *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/acoddcp0.acl"                   .

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
      *    * Area di comunicazione per funzione di evasione ordini     *
      *    * 'forecast'                                                *
      *    *-----------------------------------------------------------*
       01  l-eva-frc.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  l-eva-frc-tip-ope          pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Return status code                                    *
      *        * - Spaces : operazione eseguita                        *
      *        * - #      : errore di esecuzione                       *
      *        *-------------------------------------------------------*
           05  l-eva-frc-exi-sts          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  l-eva-frc-cod-dpz          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Default per codice tipo movimento ordine              *
      *        *-------------------------------------------------------*
           05  l-eva-frc-tmo-orc          pic  x(05)                  .
      *        *-------------------------------------------------------*
      *        * Flag di trattamento ordine                            *
      *        *-------------------------------------------------------*
           05  l-eva-frc-flg-orc          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area libera per espansioni future                     *
      *        *-------------------------------------------------------*
           05  filler                     pic  x(99)                  .

      *    *===========================================================*
      *    * Link-area comune per programmi della serie porc3000       *
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
           copy      "pgm/orc/prg/cpy/porc3000.pgl"                   .

      ******************************************************************
       Procedure Division                using l-eva-frc
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
           move      spaces               to   l-eva-frc-exi-sts      .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Accettazione dati identificativi ordine     *
      *                  *---------------------------------------------*
           if        l-eva-frc-tip-ope    =    "RO"
                     perform acc-dti-orc-000
                                          thru acc-dti-orc-999
      *                  *---------------------------------------------*
      *                  * Saldaconto per selezione righe ordine       *
      *                  *---------------------------------------------*
           else if   l-eva-frc-tip-ope    =    "SC"
                     perform sdc-sel-orc-000
                                          thru sdc-sel-orc-999
      *                  *---------------------------------------------*
      *                  * Caricamento righe ordine in catena          *
      *                  *---------------------------------------------*
           else if   l-eva-frc-tip-ope    =    "CC"
                     perform car-rig-cat-000
                                          thru car-rig-cat-999
      *                  *---------------------------------------------*
      *                  * Open                                        *
      *                  *---------------------------------------------*
           else if   l-eva-frc-tip-ope    =    "OP"
                     perform exe-fun-opn-000
                                          thru exe-fun-opn-999
      *                  *---------------------------------------------*
      *                  * Close                                       *
      *                  *---------------------------------------------*
           else if   l-eva-frc-tip-ope    =    "CL"
                     perform exe-fun-cls-000
                                          thru exe-fun-cls-999
      *                  *---------------------------------------------*
      *                  * Test cancellabilita' modulo                 *
      *                  *---------------------------------------------*
           else if   l-eva-frc-tip-ope    =    "C?"
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
           move      l-eva-frc-cod-dpz    to   w-acc-cod-dpz          .
      *              *-------------------------------------------------*
      *              * Normalizzazione data per accettazione           *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-dat-doc          .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione tipo movimento per gli *
      *              * ordini clienti                                  *
      *              *-------------------------------------------------*
           perform   cod-des-zoc-opn-000  thru cod-des-zoc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice prodotto 'dcp'  *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-opn-000  thru cod-cod-dcp-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione coefficiente di cambio *
      *              * valuta                                          *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-opn-000  thru coe-cmb-vlt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione quantita' da e-   *
      *              * vadere riga ordine                              *
      *              *-------------------------------------------------*
           perform   det-qev-roc-opn-000  thru det-qev-roc-opn-999    .
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
      *              * Close modulo accettazione tipo movimento per    *
      *              * ordini clienti                                  *
      *              *-------------------------------------------------*
           perform   cod-des-zoc-cls-000  thru cod-des-zoc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice prodotto 'dcp' *
      *              *-------------------------------------------------*
           perform   cod-cod-dcp-cls-000  thru cod-cod-dcp-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione coefficiente di cam-  *
      *              * bio valuta                                      *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-cls-000  thru coe-cmb-vlt-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione quantita' da e-  *
      *              * vadere riga ordine                              *
      *              *-------------------------------------------------*
           perform   det-qev-roc-cls-000  thru det-qev-roc-cls-999    .
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
                     move  spaces         to   l-eva-frc-exi-sts
           else      move  "#"            to   l-eva-frc-exi-sts      .
       tst-cnc-mod-999.
           exit.

      *    *===========================================================*
      *    * Accettazione dati identificativi ordine cliente           *
      *    *-----------------------------------------------------------*
       acc-dti-orc-000.
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
           move      32                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      25                   to   v-pos                  .
           move      "EVASIONE RIGHE ORDINE 'FORECAST'"
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
      *              *-------------------------------------------------*
      *              * Preparazione valori di default per tipo movi-   *
      *              * mento ordine                                    *
      *              *-------------------------------------------------*
           move      l-eva-frc-tmo-orc    to   w-acc-tmo-orc          .
           if        w-acc-tmo-orc        =    spaces
                     go to acc-dti-orc-100.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc]                      *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orc        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
           move      w-let-arc-zoc-des    to   w-acc-tmo-orc-des      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione codice                      *
      *                  *---------------------------------------------*
           perform   vis-tmo-orc-000      thru vis-tmo-orc-999        .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
      *              *-------------------------------------------------*
      *              * Ad accettazione data documento                  *
      *              *-------------------------------------------------*
           go to     acc-dti-orc-200.
       acc-dti-orc-100.
      *              *-------------------------------------------------*
      *              * Accettazione dati identificativi ordine         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Tipo movimento per ordini clienti           *
      *                  *---------------------------------------------*
           perform   acc-tmo-orc-000      thru acc-tmo-orc-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-orc-800.
           if        v-key                =    "DO  "
                     go to acc-dti-orc-400.
       acc-dti-orc-200.
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           perform   acc-dat-doc-000      thru acc-dat-doc-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-orc-800.
           if        v-key                =    "DO  "
                     go to acc-dti-orc-400.
           if        v-key                =    "UP  "
                     go to acc-dti-orc-100.
       acc-dti-orc-300.
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           perform   acc-num-doc-000      thru acc-num-doc-999        .
           if        v-key                =    "EXIT"
                     go to acc-dti-orc-800.
           if        v-key                =    "DO  "
                     go to acc-dti-orc-400.
           if        v-key                =    "UP  "
                     go to acc-dti-orc-200.
       acc-dti-orc-400.
      *              *-------------------------------------------------*
      *              * Controllo che esistano tutti i valori di iden-  *
      *              * tificazione della ordine                        *
      *              *-------------------------------------------------*
           if        w-acc-tmo-orc        =    spaces or
                     w-acc-dat-doc        =    zero   or
                     w-acc-num-doc        =    zero
                     go to acc-dti-orc-100.
       acc-dti-orc-410.
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
       acc-dti-orc-415.
      *              *-------------------------------------------------*
      *              * Determinazione se documento esistente           *
      *              *-------------------------------------------------*
           perform   det-doc-ges-000      thru det-doc-ges-999        .
      *                  *---------------------------------------------*
      *                  * Se documento non esistente                  *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-snx    not  = "N"
                     go to acc-dti-orc-420.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Movimento non esistente in archivio !             
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-orc-100.
       acc-dti-orc-420.
      *                  *---------------------------------------------*
      *                  * Se documento esistente ma con tipo ordine   *
      *                  * non 'forecast'                              *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-tor    =    "F"
                     go to acc-dti-orc-425.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Ordine non di tipo 'forecast' !                   
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-orc-100.
       acc-dti-orc-425.
      *                  *---------------------------------------------*
      *                  * Se documento esistente ma non controllato   *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-ctl    =    spaces
                     go to acc-dti-orc-430.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Ordine di tipo 'forecast' ma non controllato!     
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-orc-100.
       acc-dti-orc-430.
      *                  *---------------------------------------------*
      *                  * Se documento esistente ma con codice clien- *
      *                  * te diverso                                  *
      *                  *---------------------------------------------*
           if        w-det-doc-ges-car    =    w-tes-cod-arc (1)
                     go to acc-dti-orc-440.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Ordine di tipo 'forecast' intestato ad altro clien
      -              "te !           "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * Messaggio ma non blocco                 *
      *                      *-----------------------------------------*
           go to     acc-dti-orc-440.
       acc-dti-orc-440.
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero protocollo               *
      *              *-------------------------------------------------*
           move      w-det-doc-ges-prt    to   w-acc-num-prt          .
       acc-dti-orc-500.
      *              *-------------------------------------------------*
      *              * Bufferizzazione righe ordine da trattare        *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-buf-rig-orc-prt      .
           perform   buf-rig-orc-000      thru buf-rig-orc-999        .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        w-buf-rig-orc-flg    =    spaces
                     go to acc-dti-orc-620.
      *                      *-----------------------------------------*
      *                      * Messaggio di errore                     *
      *                      *-----------------------------------------*
           move      "Nessuna riga ordine da evadere !                  
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-dti-orc-100.
       acc-dti-orc-620.
      *              *-------------------------------------------------*
      *              * Aggiornamento protocollo ordine in corso di     *
      *              * trattamento                                     *
      *              *-------------------------------------------------*
           move      w-acc-num-prt        to   w-gen-prt-orc          .
      *              *-------------------------------------------------*
      *              * A uscita                                        *
      *              *-------------------------------------------------*
           go to     acc-dti-orc-900.
       acc-dti-orc-800.
      *              *-------------------------------------------------*
      *              * Se uscita per Exit                              *
      *              *-------------------------------------------------*
           move      "#"                  to   l-eva-frc-exi-sts      .
       acc-dti-orc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-dti-orc-999.
       acc-dti-orc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Tipo movimento per ordini clienti    *
      *    *-----------------------------------------------------------*
       acc-tmo-orc-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tmo-orc-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-des-zoc-ope      .
           move      w-acc-tmo-orc        to   w-cod-des-zoc-cod      .
           move      14                   to   w-cod-des-zoc-lin      .
           move      09                   to   w-cod-des-zoc-pos      .
           move      14                   to   w-cod-des-zoc-dln      .
           move      15                   to   w-cod-des-zoc-dps      .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-des-zoc-cll-000  thru cod-des-zoc-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cod-des-zoc-foi-000  thru cod-des-zoc-foi-999    .
       acc-tmo-orc-110.
           perform   cod-des-zoc-cll-000  thru cod-des-zoc-cll-999    .
           if        w-cod-des-zoc-ope    =    "F+"
                     go to acc-tmo-orc-115.
           if        w-cod-des-zoc-ope    =    "AC"
                     go to acc-tmo-orc-120.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-tmo-orc-115.
           perform   cod-des-zoc-foi-000  thru cod-des-zoc-foi-999    .
           go to     acc-tmo-orc-110.
       acc-tmo-orc-120.
           move      w-cod-des-zoc-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-tmo-orc-999.
       acc-tmo-orc-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   w-acc-tmo-orc          .
       acc-tmo-orc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test che non ci siano blanks embedded       *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orc        to   w-all-str-alf          .
           move      05                   to   w-all-str-lun          .
           perform   all-str-ble-000      thru all-str-ble-999        .
           if        w-all-str-flg        not  = spaces
                     go to acc-tmo-orc-100.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc]                      *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orc        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento ordini clienti                    *
      *                  *---------------------------------------------*
           move      w-let-arc-zoc-des    to   w-acc-tmo-orc-des      .
           move      w-let-arc-zoc-sgl    to   w-acc-tmo-orc-sgl      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione descrizione                 *
      *                  *---------------------------------------------*
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to acc-tmo-orc-100.
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-acc-tmo-orc        =    spaces
                     go to acc-tmo-orc-100.
      *                  *---------------------------------------------*
      *                  * Test se ordine di tipo 'forecast'           *
      *                  *---------------------------------------------*
           if        w-let-arc-zoc-tip    =    "F"
                     go to acc-tmo-orc-600.
      *                  *---------------------------------------------*
      *                  * Messaggio di errore                         *
      *                  *---------------------------------------------*
           move      "Tipo di ordine non 'forecast' !                   
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                  *---------------------------------------------*
      *                  * A reimpostazione                            *
      *                  *---------------------------------------------*
           go to     acc-tmo-orc-100.
       acc-tmo-orc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Aggiornamento valore di default             *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orc        to   l-eva-frc-tmo-orc      .
       acc-tmo-orc-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
       acc-tmo-orc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : tipo movimento per ordini clienti *
      *    *-----------------------------------------------------------*
       vis-tmo-orc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      09                   to   v-pos                  .
           move      w-acc-tmo-orc        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tmo-orc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione tipo movimento ordini *
      *    * clienti                                                   *
      *    *-----------------------------------------------------------*
       vis-tmo-orc-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      15                   to   v-pos                  .
           move      w-acc-tmo-orc-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tmo-orc-des-999.
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
      *                  * Find su archivio [oct]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-oct-000      thru fnd-arc-oct-999        .
           if        w-fnd-arc-oct-sel    not  = spaces
                     go to acc-dat-doc-100.
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori                      *
      *                  *---------------------------------------------*
           perform   vis-tmo-orc-000      thru vis-tmo-orc-999        .
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
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
       acc-num-doc-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-num-doc-350.
      *                  *---------------------------------------------*
      *                  * Find su archivio [oct]                      *
      *                  *---------------------------------------------*
           perform   fnd-arc-oct-000      thru fnd-arc-oct-999        .
           if        w-fnd-arc-oct-sel    not  = spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori                      *
      *                  *---------------------------------------------*
           perform   vis-tmo-orc-000      thru vis-tmo-orc-999        .
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *                  *---------------------------------------------*
      *                  * Normalizzazione function-key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
       acc-num-doc-350.
      *              *-------------------------------------------------*
      *              * Se Select                                       *
      *              *-------------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-num-doc-400.
      *                  *---------------------------------------------*
      *                  * Test preliminari                            *
      *                  *---------------------------------------------*
           if        w-acc-num-doc-prg    =    zero
                     go to acc-num-doc-100.
           if        w-acc-dat-doc        =    zero
                     go to acc-num-doc-100.
           if        w-acc-tmo-orc        =    spaces
                     go to acc-num-doc-100.
      *                  *---------------------------------------------*
      *                  * Preparazione dei parametri per la selezione *
      *                  *---------------------------------------------*
           move      w-acc-num-doc-prg    to   w-slc-num-oct-npg      .
           move      w-acc-cod-dpz        to   w-slc-num-oct-dpz      .
           move      w-acc-tmo-orc-sgl    to   w-slc-num-oct-sgl      .
           move      w-acc-dat-doc        to   w-slc-num-oct-dds      .
      *                  *---------------------------------------------*
      *                  * Routine di ricerca                          *
      *                  *---------------------------------------------*
           perform   slc-num-oct-000      thru slc-num-oct-999        .
      *                  *---------------------------------------------*
      *                  * Se non selezionato alcun elemento : a       *
      *                  * reimpostazione                              *
      *                  *---------------------------------------------*
           if        w-slc-num-oct-sel    not  = spaces
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
      *              * Tipo movimento per orclettazione                *
      *              *-------------------------------------------------*
           move      spaces               to   w-acc-tmo-orc-des      .
           move      spaces               to   w-acc-tmo-orc-sgl      .
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
           move      zero                 to   w-acc-num-doc          .
       nor-wrk-acc-999.
           exit.

      *    *===========================================================*
      *    * Bufferizzazione righe ordine da trattare                  *
      *    *-----------------------------------------------------------*
       buf-rig-orc-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di uscita ad errore       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-buf-rig-orc-flg      .
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi di visualizzazione       *
      *              *-------------------------------------------------*
           move      "NO"                 to   w-edt-qta-inc-ope      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
      *              *-------------------------------------------------*
      *              * Normalizzazione comodo di ordinamento           *
      *              *-------------------------------------------------*
           move      zero                 to   w-bro-key-ord          .
      *              *-------------------------------------------------*
      *              * Azzeramento numero righe nel buffer             *
      *              *-------------------------------------------------*
           move      zero                 to   w-bro-num-ele          .
       buf-rig-orc-100.
      *              *-------------------------------------------------*
      *              * Start su file [ocr]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-buf-rig-orc-prt    to   rf-ocr-num-prt         .
           move      zero                 to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-orc-500.
       buf-rig-orc-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [ocr]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to buf-rig-orc-500.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-ocr-num-prt       not  = w-buf-rig-orc-prt
                     go to buf-rig-orc-500.
      *                  *---------------------------------------------*
      *                  * Test sul contatore elementi nel buffer      *
      *                  *---------------------------------------------*
           if        w-bro-num-ele        not  < w-bro-max-ele
                     go to buf-rig-orc-500.
       buf-rig-orc-250.
      *              *-------------------------------------------------*
      *              * Aggiornamento comodi di visualizzazione         *
      *              *-------------------------------------------------*
           move      "AG"                 to   w-edt-qta-inc-ope      .
           move      rf-ocr-qta-ord       to   w-edt-qta-inc-qta      .
           move      rf-ocr-dec-qta       to   w-edt-qta-inc-dec      .
           perform   edt-qta-inc-000      thru edt-qta-inc-999        .
           move      w-edt-qta-inc-din    to   w-tes-dec-qta          .
      *              *-------------------------------------------------*
      *              * Ridefinizione tipo riga                         *
      *              *-------------------------------------------------*
           move      rf-ocr-tip-rig       to   w-buf-rig-orc-wtr      .
      *              *-------------------------------------------------*
      *              * Se tipo riga "C" : a bufferizzazione riga       *
      *              *-------------------------------------------------*
           if        w-buf-rig-orc-wtp    =    "C"
                     go to buf-rig-orc-260.
      *              *-------------------------------------------------*
      *              * Test su flag di riga chiusa                     *
      *              *-------------------------------------------------*
           if        rf-ocr-flg-rch       not  = spaces
                     go to buf-rig-orc-200.
      *              *-------------------------------------------------*
      *              * Test su segnale di riga comunque considerata    *
      *              * saldata                                         *
      *              *-------------------------------------------------*
           if        rf-ocr-sdr-ccs       not  = spaces
                     go to buf-rig-orc-200.
      *              *-------------------------------------------------*
      *              * Se tipo riga "A" : a normalizzazione flag di u- *
      *              * scita e bufferizzazione riga ordine             *
      *              *-------------------------------------------------*
           if        w-buf-rig-orc-wtp    =    "A"
                     go to buf-rig-orc-260.
      *              *-------------------------------------------------*
      *              * Determinazione quantita' evasa riga ordine cli- *
      *              * ente 'forecast'                                 *
      *              *-------------------------------------------------*
           move      "DT"                 to   d-qev-roc-tip-ope      .
           perform   det-qev-roc-cll-000  thru det-qev-roc-cll-999    .
      *              *-------------------------------------------------*
      *              * Se riga evasa : riciclo                         *
      *              *-------------------------------------------------*
           if        d-qev-roc-qta-dev    =    zero
                     go to buf-rig-orc-200.
       buf-rig-orc-260.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-buf-rig-orc-flg      .
       buf-rig-orc-280.
      *              *-------------------------------------------------*
      *              * Bufferizzazione riga ordine                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore elementi nel buffer    *
      *                  *---------------------------------------------*
           if        w-bro-num-ele        <    w-bro-max-ele
                     add   1              to   w-bro-num-ele          .
      *                  *---------------------------------------------*
      *                  * Progressivo riga                            *
      *                  *---------------------------------------------*
           move      rf-ocr-num-prg       to   w-bro-num-prg
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Tipo riga                                   *
      *                  *---------------------------------------------*
           move      w-buf-rig-orc-wtp    to   w-bro-tip-rig
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Codice prodotto                             *
      *                  *---------------------------------------------*
           move      rf-ocr-num-pro       to   w-bro-num-pro
                                              (w-bro-num-ele)         .
       buf-rig-orc-300.
      *                  *---------------------------------------------*
      *                  * Descrizione per la riga                     *
      *                  *                                             *
      *                  * - Se tipo riga 'A', 'C' o 'PN' :            *
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
           if        w-buf-rig-orc-wtp    =    "A"     or
                     w-buf-rig-orc-wtp    =    "C"     or
                    (w-buf-rig-orc-wtp    =    "P" and
                     w-buf-rig-orc-wtf    =    "N"   )
                     go to buf-rig-orc-320
           else if   w-buf-rig-orc-wtp    =    "P" and
                     w-buf-rig-orc-wtf    =    " "
                     go to buf-rig-orc-340
           else      go to buf-rig-orc-320.
       buf-rig-orc-320.
      *                      *-----------------------------------------*
      *                      * Se tipo riga 'A', 'C' o 'PN'            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura preliminare file [ocx]      *
      *                          *-------------------------------------*
           move      rf-ocr-num-prt       to   w-let-arc-ocx-prt      .
           move      rf-ocr-num-prg       to   w-let-arc-ocx-prg      .
           move      11                   to   w-let-arc-ocx-trc      .
           perform   let-arc-ocx-000      thru let-arc-ocx-999        .
      *
           if        w-let-arc-ocx-flg    not  = spaces
                     go to buf-rig-orc-321.
      *
           move      w-let-arc-ocx-des    to   w-buf-rig-orc-wde      .
      *
           go to     buf-rig-orc-350.
       buf-rig-orc-321.
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del flag di  *
      *                          * estensione alla descrizione         *
      *                          *-------------------------------------*
           if        rf-ocr-des-ext       =    0
                     go to buf-rig-orc-322
           else if   rf-ocr-des-ext       =    1
                     go to buf-rig-orc-324
           else if   rf-ocr-des-ext       =    2
                     go to buf-rig-orc-326.
       buf-rig-orc-322.
      *                          *-------------------------------------*
      *                          * Se nessuna estensione : bufferizza- *
      *                          * zione descrizione contenuta record  *
      *                          * [ocr]                               *
      *                          *-------------------------------------*
           move      rf-ocr-des-rig       to   w-buf-rig-orc-wde      .
           go to     buf-rig-orc-350.
       buf-rig-orc-324.
      *                          *-------------------------------------*
      *                          * Se estensione nel file [ocx]        *
      *                          *-------------------------------------*
           go to     buf-rig-orc-350.
       buf-rig-orc-326.
      *                          *-------------------------------------*
      *                          * Se estensione nel file [pdx]        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura archivio [dcp]          *
      *                              *---------------------------------*
           move      rf-ocr-num-pro       to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      "I  "                to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-buf-rig-orc-wde      .
           go to     buf-rig-orc-350.
       buf-rig-orc-340.
      *                      *-----------------------------------------*
      *                      * Se tipo riga 'P'                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test su tipo e codice archivio e su *
      *                          * codice lingua                       *
      *                          *-------------------------------------*
           if        rf-ocr-tip-arc       =    w-tes-tip-arc (1) and
                     rf-ocr-cod-arc       =    w-tes-cod-arc (1) and
                     rf-ocr-cod-lng       =    w-tes-cod-lng (1)
                     go to buf-rig-orc-320.
      *                          *-------------------------------------*
      *                          * Determinazione descrizione associa- *
      *                          * ta al prodotto                      *
      *                          *-------------------------------------*
           move      rf-ocr-num-pro       to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      "I  "                to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-buf-rig-orc-wde      .
           go to     buf-rig-orc-350.
       buf-rig-orc-350.
      *                      *-----------------------------------------*
      *                      * Composizione descrizione per riga di    *
      *                      * scroll                                  *
      *                      *-----------------------------------------*
           perform   cpz-des-rgs-000      thru cpz-des-rgs-999        .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione                         *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wde    to   w-bro-des-rig
                                              (w-bro-num-ele)         .
       buf-rig-orc-356.
      *                  *---------------------------------------------*
      *                  * Numero decimali quantita'                   *
      *                  *---------------------------------------------*
           move      rf-ocr-dec-qta       to   w-bro-dec-qta
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Quantita' in ordine                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento : zero   *
      *                      *-----------------------------------------*
           if        w-buf-rig-orc-wtp    =    "C" or
                     w-buf-rig-orc-wtp    =    "A"
                     move  zero           to   w-bro-qta-orc
                                              (w-bro-num-ele)
                     go to buf-rig-orc-360.
      *                      *-----------------------------------------*
      *                      * Altrimenti                              *
      *                      *-----------------------------------------*
           move      d-qev-roc-qta-dev    to   w-bro-qta-orc
                                              (w-bro-num-ele)         .
       buf-rig-orc-360.
      *                  *---------------------------------------------*
      *                  * Quantita' da duplicare a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-bro-qta-eva
                                              (w-bro-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Si/No addebito o commento                   *
      *                  *---------------------------------------------*
           move      "N"                  to   w-bro-snx-aoc
                                              (w-bro-num-ele)         .
       buf-rig-orc-400.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale file [ocr]       *
      *              *-------------------------------------------------*
           go to     buf-rig-orc-200.
       buf-rig-orc-500.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     buf-rig-orc-999.
       buf-rig-orc-999.
           exit.

      *    *===========================================================*
      *    * Composizione descrizione per riga di scroll               *
      *    *-----------------------------------------------------------*
       cpz-des-rgs-000.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se si sta trattando un co- *
      *              * dice di magazzino oppure no                     *
      *              *-------------------------------------------------*
           if       (w-buf-rig-orc-wtp    =    "P" or
                     w-buf-rig-orc-wtp    =    "S" or
                     w-buf-rig-orc-wtp    =    "M"  ) and
                     w-buf-rig-orc-wtf    =    spaces
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
           move      w-buf-rig-orc-wde    to   w-des-scr-000-d40      .
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
           move      w-buf-rig-orc-wde    to   w-des-scr-001-d25      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ocr-alf-pro       to   w-des-scr-001-cod      .
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
           move      w-buf-rig-orc-wde    to   w-des-scr-002-d21      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra sinistra               *
      *                      *-----------------------------------------*
           move      "["                  to   w-des-scr-002-pqs      .
      *                      *-----------------------------------------*
      *                      * Tipo codice                             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wtp    to   w-des-scr-002-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-002-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ocr-alf-pro       to   w-des-scr-002-cod      .
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
           move      rf-ocr-alf-pro       to   w-des-scr-003-cod      .
      *                      *-----------------------------------------*
      *                      * Descrizione di 25 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wde    to   w-des-scr-003-d25      .
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
           move      w-buf-rig-orc-wtp    to   w-des-scr-004-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-004-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ocr-alf-pro       to   w-des-scr-004-cod      .
      *                      *-----------------------------------------*
      *                      * Descrizione di 21 caratteri             *
      *                      *-----------------------------------------*
           move      w-buf-rig-orc-wde    to   w-des-scr-004-d21      .
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
           move      rf-ocr-alf-pro       to   w-des-scr-005-cod      .
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
           move      w-buf-rig-orc-wtp    to   w-des-scr-006-tco      .
      *                      *-----------------------------------------*
      *                      * Parentesi quadra destra                 *
      *                      *-----------------------------------------*
           move      "]"                  to   w-des-scr-006-pqd      .
      *                      *-----------------------------------------*
      *                      * Codice                                  *
      *                      *-----------------------------------------*
           move      rf-ocr-alf-pro       to   w-des-scr-006-cod      .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     cpz-des-rgs-700.
       cpz-des-rgs-700.
      *                  *---------------------------------------------*
      *                  * Composizione eseguita in area di destina-   *
      *                  * zione                                       *
      *                  *---------------------------------------------*
           move      w-des-scr-000        to   w-buf-rig-orc-wde      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     cpz-des-rgs-999.
       cpz-des-rgs-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per selezione righe ordine da evadere          *
      *    *-----------------------------------------------------------*
       sdc-sel-orc-000.
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
                     move  "#"            to   l-eva-frc-exi-sts      .
       sdc-sel-orc-999.
           exit.

      *    *===========================================================*
      *    * Saldaconto per selezione righe ordine                     *
      *    *-----------------------------------------------------------*
       acc-fun-sdc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-sdc-flg-exi          .
      *              *-------------------------------------------------*
      *              * Normalizzazione comodo per 'Find'               *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-wrk-c02          .
      *              *-------------------------------------------------*
      *              * Determinazione numero massimo pagine            *
      *              *-------------------------------------------------*
           move      w-bro-num-ele        to   w-sdc-npg-max          .
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
           if        w-sdc-ctr-rig        >    w-bro-num-ele
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
      *                  * Quantita' da evadere                        *
      *                  *---------------------------------------------*
           perform   acc-qta-dup-000      thru acc-qta-dup-999        .
      *                      *-----------------------------------------*
      *                      * Se Find                                 *
      *                      *-----------------------------------------*
           if        w-sdc-wrk-c02        not  = zero
                     move  w-sdc-wrk-c02  to   w-sdc-ctr-rig
                     move  zero           to   w-sdc-wrk-c02
                     go to acc-fun-sdc-300.
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
                     move   w-bro-num-ele to   w-sdc-ctr-rig
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
           move      w-acc-tmo-orc        to   v-alf                  .
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
           move      w-acc-tmo-orc-des    to   v-alf                  .
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
      -              "Quantita' |  Evasione  |      "
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
      -              "Quantita' |  Evasione  |      "
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
      -              "Quantita' |  Evasione  |      "
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
      -              "Quantita' |  Evasione  |      "
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
      -              "Quantita' |  Duplicare |      "
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
      -              "Quantita' |  Duplicare |      "
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
      -              "Quantita' |  Duplicare |      "
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
           if        w-sdc-wrk-rig        >    w-bro-num-ele
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
           move      w-bro-des-rig
                    (w-sdc-wrk-rig)       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Quantita' in ordine                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Editing quantita' da incolonnare    *
      *                          *-------------------------------------*
           move      "ED"                 to   w-edt-qta-inc-ope      .
           move      w-bro-qta-orc
                    (w-sdc-wrk-rig)       to   w-edt-qta-inc-qta      .
           move      w-bro-dec-qta
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
           if        w-bro-tip-rig
                    (w-sdc-wrk-rig)       not  = "C" and
                     w-bro-tip-rig
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
           if        w-bro-snx-aoc
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
      *                          * Quantita' da evadere                *
      *                          *-------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           add       7
                     w-sdc-wrk-c01      giving v-lin                  .
           move      62                   to   v-pos                  .
           move      w-bro-qta-eva
                    (w-sdc-wrk-rig)       to   v-num                  .
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
      *    * Accettazione campo saldaconto : Quantita' da evadere      *
      *    *-----------------------------------------------------------*
       acc-qta-dup-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
           if        w-bro-tip-rig
                    (w-sdc-ctr-rig)       =    "A" or
                     w-bro-tip-rig
                    (w-sdc-ctr-rig)       =    "C"
                     go to acc-qta-dup-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-bro-qta-eva
                    (w-sdc-ctr-rig)       to   w-sav-qta-dup          .
       acc-qta-dup-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
      *
           if        w-bro-dec-qta
                    (w-sdc-ctr-rig)       >    2
                     move  2              to   v-dec
           else      move  w-bro-dec-qta
                          (w-sdc-ctr-rig) to   v-dec                  .
      *
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      62                   to   v-pos                  .
           if        w-sdc-ctr-rig        >    1      
                     move   "UP  "        to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-sdc-npg-dat        <    w-sdc-npg-max
                     move   "NXSC"        to   v-pfk (06)             .
           if        w-sdc-npg-dat        >    1
                     move   "PRSC"        to   v-pfk (07)             .
           if        w-sdc-ctr-rig        <    w-bro-num-ele
                     move   "TAB "        to   v-pfk (08)             .
           if        w-sdc-ctr-rig        >    1
                     move   "BACK"        to   v-pfk (09)             .
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-bro-qta-eva
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
           move      v-num                to   w-bro-qta-eva
                                              (w-sdc-ctr-rig)         .
       acc-qta-dup-300.
      *              *-------------------------------------------------*
      *              * Se Find                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        v-key                not  = "FIND"
                     go to acc-qta-dup-400.
      *                  *---------------------------------------------*
      *                  * Routine di ricerca per codice articolo      *
      *                  *---------------------------------------------*
           perform   acc-qta-dup-fnp-000  thru acc-qta-dup-fnp-999    .
      *                  *---------------------------------------------*
      *                  * Normalizzazione comodi di accetazione       *
      *                  *---------------------------------------------*
           move      zero                 to   w-sdc-fnd-pro-num      .
           move      spaces               to   w-sdc-fnd-pro-alf      .
      *                  *---------------------------------------------*
      *                  * Test su esito accettazione                  *
      *                  *---------------------------------------------*
           if        w-sdc-wrk-c02        =    zero
                     go to acc-qta-dup-100.
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-qta-dup-999.
       acc-qta-dup-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Controllo compatibilita' fra segno quantita'*
      *                  * da duplicare e segno quantita' in ordine    *
      *                  *---------------------------------------------*
           if        w-bro-qta-orc
                    (w-sdc-ctr-rig)       >    zero and
                     w-bro-qta-eva
                    (w-sdc-ctr-rig)       <    zero
                     go to acc-qta-dup-100
           else if   w-bro-qta-orc
                    (w-sdc-ctr-rig)       <    zero and
                     w-bro-qta-eva
                    (w-sdc-ctr-rig)       >    zero
                     go to acc-qta-dup-100.
       acc-qta-dup-420.
      *                  *---------------------------------------------*
      *                  * Se Slct : controllo che il valore impostato *
      *                  * sia uguale al precedente                    *
      *                  *---------------------------------------------*
           if        v-key                not  = "SLCT"
                     go to acc-qta-dup-600.
           if        w-bro-qta-eva
                    (w-sdc-ctr-rig)       not  = w-sav-qta-dup
                     move  w-sav-qta-dup  to   w-bro-qta-eva
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
           if        w-bro-qta-eva
                    (w-sdc-ctr-rig)       =    zero
                     go to acc-qta-dup-610.
      *                          *-------------------------------------*
      *                          * Quantita' da duplicare              *
      *                          *-------------------------------------*
           move      zero                 to   w-bro-qta-eva
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-700.
       acc-qta-dup-610.
      *                      *-----------------------------------------*
      *                      * Se valore non presente                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Quantita' da duplicare              *
      *                          *-------------------------------------*
           move      w-bro-qta-orc
                    (w-sdc-ctr-rig)       to   w-bro-qta-eva
                                              (w-sdc-ctr-rig)         .
           go to     acc-qta-dup-700.
       acc-qta-dup-620.
      *                  *---------------------------------------------*
      *                  * Se Return o altri tasti funzione            *
      *                  *---------------------------------------------*
       acc-qta-dup-700.
      *                  *---------------------------------------------*
      *                  * Eventuale assegnazione progressivo riga     *
      *                  *---------------------------------------------*
           if        w-bro-qta-eva
                    (w-sdc-ctr-rig)       =    zero
                     go to acc-qta-dup-760.
           if        w-sav-qta-dup        not  = zero
                     go to acc-qta-dup-760.
           add       00100                to   w-bro-key-ord          .
           move      w-bro-key-ord        to   w-bro-key-prg
                                              (w-sdc-ctr-rig)         .
       acc-qta-dup-760.
      *                  *---------------------------------------------*
      *                  * Visualizzazione valori riga                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Quantita' da duplicare                  *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
      *
           if        w-bro-dec-qta
                    (w-sdc-ctr-rig)       >    2
                     move  2              to   v-dec
           else      move  w-bro-dec-qta
                          (w-sdc-ctr-rig) to   v-dec                  .
      *
           move      "S"                  to   v-sgn                  .
           move      "BD"                 to   v-edm                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      62                   to   v-pos                  .
           move      w-bro-qta-eva
                    (w-sdc-ctr-rig)       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-qta-dup-800.
       acc-qta-dup-999.
           exit.

      *    *===========================================================*
      *    * Routine di Find per codice prodotto                       *
      *    *-----------------------------------------------------------*
       acc-qta-dup-fnp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi di accettazione          *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-fnd-pro-num      .
           move      spaces               to   w-sdc-fnd-pro-alf      .
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Video in 'OFF'                                  *
      *              *-------------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Costruzione box di Expand per la visualizzazio- *
      *              * ne di tutte le righe del documento              *
      *              *-------------------------------------------------*
           move      "BX"                 to   v-ope                  .
           move      11                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      13                   to   v-lto                  .
           move      75                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-qta-dup-fnp-100.
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      08                   to   v-pos                  .
           move      "Codice :"           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-qta-dup-fnp-200.
      *              *-------------------------------------------------*
      *              * Video in On                                     *
      *              *-------------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-qta-dup-fnp-205.
      *              *-------------------------------------------------*
      *              * Accettazione codice prodotto                    *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-cod-dcp-ope      .
           move      "A"                  to   w-cod-cod-dcp-tac      .
           move      w-sdc-fnd-pro-num    to   w-cod-cod-dcp-num      .
           move      w-sdc-fnd-pro-alf    to   w-cod-cod-dcp-alf      .
           move      12                   to   w-cod-cod-dcp-lin      .
           move      17                   to   w-cod-cod-dcp-pos      .
           move      12                   to   w-cod-cod-dcp-dln      .
           move      34                   to   w-cod-cod-dcp-dps      .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
       acc-qta-dup-fnp-210.
           perform   cod-cod-dcp-cll-000  thru cod-cod-dcp-cll-999    .
           if        w-cod-cod-dcp-ope    =    "F+"
                     go to acc-qta-dup-fnp-215.
           if        w-cod-cod-dcp-ope    =    "AC"
                     go to acc-qta-dup-fnp-220.
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-qta-dup-fnp-215.
           perform   cod-cod-dcp-foi-000  thru cod-cod-dcp-foi-999    .
           go to     acc-qta-dup-fnp-210.
       acc-qta-dup-fnp-220.
           move      w-cod-cod-dcp-alf    to   v-alf                  .
           move      w-cod-cod-dcp-num    to   v-num                  .
       acc-qta-dup-fnp-250.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     go to acc-qta-dup-fnp-900.
       acc-qta-dup-fnp-300.
      *              *-------------------------------------------------*
      *              * Lettura archivio [dcp]                          *
      *              *-------------------------------------------------*
           move      v-num                to   w-let-arc-dcp-num      .
           move      v-alf                to   w-let-arc-dcp-alf      .
           perform   let-arc-dcp-000      thru let-arc-dcp-999        .
           move      w-let-arc-dcp-num    to   w-sdc-fnd-pro-num      .
           move      w-let-arc-dcp-alf    to   w-sdc-fnd-pro-alf      .
      *              *-------------------------------------------------*
      *              * Visualizzazione descrizione prodotto            *
      *              *-------------------------------------------------*
           perform   vis-cod-pro-des-000  thru vis-cod-pro-des-999    .
      *              *-------------------------------------------------*
      *              * Se codice a zero : uscita                       *
      *              *-------------------------------------------------*
           if        w-let-arc-dcp-num    =    zero or
                     w-let-arc-dcp-alf    =    spaces
                     go to acc-qta-dup-fnp-900.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-qta-dup-fnp-600.
       acc-qta-dup-fnp-400.
      *              *-------------------------------------------------*
      *              * Accettazione carattere di presa visione         *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DO  "               to   v-pfk (05)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      12                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       acc-qta-dup-fnp-420.
           if        v-key                =    "DO  " or
                     v-key                =    spaces
                     go to acc-qta-dup-fnp-422
           else if   v-key                =    "EXIT"
                     go to acc-qta-dup-fnp-424
           else if   v-key                =    "UP  "
                     go to acc-qta-dup-fnp-426
           else      go to acc-qta-dup-fnp-900.
       acc-qta-dup-fnp-422.
      *              *-------------------------------------------------*
      *              * Se Do o Return                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * A ricerca                                   *
      *                  *---------------------------------------------*
           go to     acc-qta-dup-fnp-600.
       acc-qta-dup-fnp-424.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     acc-qta-dup-fnp-900.
       acc-qta-dup-fnp-426.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione function-key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Ad accettazione prodotto                    *
      *                  *---------------------------------------------*
           go to     acc-qta-dup-fnp-205.
       acc-qta-dup-fnp-600.
      *              *-------------------------------------------------*
      *              * Ricerca vera e propria                          *
      *              *-------------------------------------------------*
           perform   acc-qta-dup-ric-000  thru acc-qta-dup-ric-999    .
       acc-qta-dup-fnp-900.
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-qta-dup-fnp-999.
       acc-qta-dup-fnp-999.
           exit.

      *    *===========================================================*
      *    * Routine di Find per codice prodotto                       *
      *    *                                                           *
      *    * Subroutine di ricerca                                     *
      *    *-----------------------------------------------------------*
       acc-qta-dup-ric-000.
      *              *-------------------------------------------------*
      *              * Inizio scansione                                *
      *              *-------------------------------------------------*
           move      zero                 to   w-sdc-wrk-c01          .
           move      zero                 to   w-sdc-wrk-c02          .
       acc-qta-dup-ric-200.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
           add       1                    to   w-sdc-wrk-c01          .
           if        w-sdc-wrk-c01        >    w-bro-num-ele
                     go to acc-qta-dup-ric-900.
      *              *-------------------------------------------------*
      *              * Test di confronto                               *
      *              *-------------------------------------------------*
           if        w-sdc-fnd-pro-num    =    w-bro-num-pro
                                              (w-sdc-wrk-c01)
                     move  w-sdc-wrk-c01  to   w-sdc-wrk-c02
                     go to acc-qta-dup-ric-900.
       acc-qta-dup-ric-600.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     acc-qta-dup-ric-200.
       acc-qta-dup-ric-800.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
       acc-qta-dup-ric-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     acc-qta-dup-ric-999.
       acc-qta-dup-ric-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Descrizione Prodotto              *
      *    *-----------------------------------------------------------*
       vis-cod-pro-des-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-let-arc-dcp-des    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-pro-des-999.
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
           if        w-bro-tip-rig
                    (w-sdc-ctr-rig)       not  = "A" and
                     w-bro-tip-rig
                    (w-sdc-ctr-rig)       not  = "C"
                     go to acc-snx-aoc-999.
      *                  *---------------------------------------------*
      *                  * Salvataggio valore precedente               *
      *                  *---------------------------------------------*
           move      w-bro-snx-aoc
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
           if        w-sdc-ctr-rig        <    w-bro-num-ele
                     move   "TAB "        to   v-pfk (08)             .
           if        w-sdc-ctr-rig        >    1
                     move   "BACK"        to   v-pfk (09)             .
       acc-snx-aoc-150.
           move      "SLCT"               to   v-pfk (10)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-bro-snx-aoc
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
           move      v-alf                to   w-bro-snx-aoc
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
           if        w-bro-snx-aoc
                    (w-sdc-ctr-rig)       not  = w-sav-snx-aoc
                     move   w-sav-snx-aoc to   w-bro-snx-aoc
                                              (w-sdc-ctr-rig)
                     go to  acc-snx-aoc-100.
      *                  *---------------------------------------------*
      *                  * Se valore presente lo si abblenca, altri-   *
      *                  * menti lo si forza a 'S'                     *
      *                  *---------------------------------------------*
           if        w-bro-snx-aoc
                    (w-sdc-ctr-rig)       not  = spaces
                     move  spaces         to   w-bro-snx-aoc
                                              (w-sdc-ctr-rig)
           else      move  "S"            to   w-bro-snx-aoc
                                              (w-sdc-ctr-rig)         .
       acc-snx-aoc-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore non ammesso : reimpostazione      *
      *                  *---------------------------------------------*
           if        w-bro-snx-aoc
                    (w-sdc-ctr-rig)       not  = "S" and
                     w-bro-snx-aoc
                    (w-sdc-ctr-rig)       not  = "N" and
                     w-bro-snx-aoc
                    (w-sdc-ctr-rig)       not  = spaces
                     go to acc-snx-aoc-100.
       acc-snx-aoc-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione valore                      *
      *                  *---------------------------------------------*
           if        w-bro-snx-aoc
                    (w-sdc-ctr-rig)       =    "N"
                     move  spaces         to   w-bro-snx-aoc
                                              (w-sdc-ctr-rig)         .
      *                  *---------------------------------------------*
      *                  * Visualizzazione Si/No addebito o commento   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      02                   to   v-car                  .
           move      w-sdc-wrk-lin        to   v-lin                  .
           move      67                   to   v-pos                  .
           if        w-bro-snx-aoc
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
      *              * Ordinamento preliminare buffer righe ordine     *
      *              *-------------------------------------------------*
           perform   car-rig-cat-obr-000  thru car-rig-cat-obr-999    .
       car-rig-cat-050.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione buffer righe ordine          *
      *              *-------------------------------------------------*
           move      zero                 to   w-car-rig-cat-ctr      .
       car-rig-cat-100.
           add       1                    to   w-car-rig-cat-ctr      .
           if        w-car-rig-cat-ctr    >    w-bro-num-ele
                     go to car-rig-cat-900.
      *                  *---------------------------------------------*
      *                  * Selezione su righe ordine nel buffer        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo riga    *
      *                      *-----------------------------------------*
           if        w-bro-tip-rig
                    (w-car-rig-cat-ctr)   =    "A" or
                     w-bro-tip-rig
                    (w-car-rig-cat-ctr)   =    "C"
                     go to car-rig-cat-110
           else      go to car-rig-cat-120.
       car-rig-cat-110.
      *                      *-----------------------------------------*
      *                      * Se riga di Addebito o Commento          *
      *                      *-----------------------------------------*
           if        w-bro-snx-aoc
                    (w-car-rig-cat-ctr)   not  = "S"
                     go to car-rig-cat-100.
           go to     car-rig-cat-150.
       car-rig-cat-120.
      *                      *-----------------------------------------*
      *                      * Se altro tipo riga                      *
      *                      *-----------------------------------------*
           if        w-bro-qta-eva
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
           move      w-gen-prt-orc        to   rf-ocr-num-prt         .
           move      w-bro-num-prg
                    (w-car-rig-cat-ctr)   to   rf-ocr-num-prg         .
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
           move      rf-ocr-tip-rig       to   w-rig-tip-rig (1)      .
           perform   dec-tip-rig-000      thru dec-tip-rig-999        .
       car-rig-cat-330.
      *                      *-----------------------------------------*
      *                      * Tipo magazzino                          *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     move  01             to   w-rig-tip-mag (1)
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
           move      rf-ocr-num-pro       to   w-rig-num-pro (1)      .
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
           move      rf-ocr-alf-pro       to   w-rig-alf-pro (1)      .
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
           if        rf-ocr-tip-arc       =    w-tes-tip-arc (1) and
                     rf-ocr-cod-arc       =    w-tes-cod-arc (1)
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
           move      rf-ocr-cop-scl       to   w-rig-cop-scl (1)      .
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
      *                      * - Se tipo riga 'A', 'C' o 'PN':         *
      *                      *   si duplica la descrizione contenuta   *
      *                      *   nel documento di origine              *
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
                                          =    "N"   )
                     go to car-rig-cat-371
           else if   w-rig-tip-rig-tpr (1)
                                          =    "P" and
                     w-rig-tip-rig-tfu (1)
                                          =    " "
                     go to car-rig-cat-376
           else      go to car-rig-cat-371.
       car-rig-cat-371.
      *                          *-------------------------------------*
      *                          * Se tipo riga 'A', 'C' o 'PN'        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Lettura preliminare file [ocx]  *
      *                              *---------------------------------*
           move      rf-ocr-num-prt       to   w-let-arc-ocx-prt      .
           move      rf-ocr-num-prg       to   w-let-arc-ocx-prg      .
           move      11                   to   w-let-arc-ocx-trc      .
           perform   let-arc-ocx-000      thru let-arc-ocx-999        .
      *
           if        w-let-arc-ocx-flg    not  = spaces
                     go to car-rig-cat-372.
      *
           move      w-let-arc-ocx-des    to   w-rig-des-por (1)      .
      *
           go to     car-rig-cat-380.
       car-rig-cat-372.
      *                              *---------------------------------*
      *                              * Deviazione in funzione del flag *
      *                              * di estensione alla descrizione  *
      *                              *---------------------------------*
           if        rf-ocr-des-ext       =    0
                     go to car-rig-cat-373
           else if   rf-ocr-des-ext       =    1
                     go to car-rig-cat-374
           else if   rf-ocr-des-ext       =    2
                     go to car-rig-cat-375.
       car-rig-cat-373.
      *                              *---------------------------------*
      *                              * Se nessuna estensione : buffe-  *
      *                              * rizzazione descrizione contenu- *
      *                              * ta record [ocr]                 *
      *                              *---------------------------------*
           move      rf-ocr-des-rig       to   w-rig-des-por (1)      .
           go to     car-rig-cat-380.
       car-rig-cat-374.
      *                              *---------------------------------*
      *                              * Se estensione nel file [ocx]    *
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
           move      w-let-dcp-pdx-des    to   w-rig-des-por (1)      .
           go to     car-rig-cat-380.
       car-rig-cat-376.
      *                          *-------------------------------------*
      *                          * Se tipo riga 'P'                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test su tipo e codice archivio  *
      *                              * e su codice lingua              *
      *                              *---------------------------------*
           if        rf-ocr-tip-arc       =    w-tes-tip-arc (1) and
                     rf-ocr-cod-arc       =    w-tes-cod-arc (1) and
                     rf-ocr-cod-lng       =    w-tes-cod-lng (1)
                     go to car-rig-cat-371.
      *                              *---------------------------------*
      *                              * Determinazione descrizione as-  *
      *                              * sociata al prodotto             *
      *                              *---------------------------------*
           move      w-rig-num-pro (1)    to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      "I  "                to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-rig-des-por (1)      .
           go to     car-rig-cat-380.
       car-rig-cat-380.
      *                      *-----------------------------------------*
      *                      * Descrizione riga                        *
      *                      *-----------------------------------------*
           move      w-rig-des-por (1)    to   w-rig-des-rig (1)      .
       car-rig-cat-385.
      *                      *-----------------------------------------*
      *                      * Descrizione ad uso interno e lotto di   *
      *                      * vendita                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da bufferizzare             *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P" or
                     w-rig-tip-rig-tfu (1)
                                          not  = spaces
                     go to car-rig-cat-390.
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcp]              *
      *                          *-------------------------------------*
           move      w-rig-num-pro (1)    to   w-let-dcp-pdx-cod      .
           move      spaces               to   w-let-dcp-pdx-tar      .
           move      zero                 to   w-let-dcp-pdx-arc      .
           move      "I  "                to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-dui    to   w-rig-des-int (1)      .
           move      w-let-dcp-pdx-ldv    to   w-rig-lot-ven (1)      .
       car-rig-cat-390.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica archivio relativo al *
      *                      * tipo riga                               *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-392
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
           else      go to car-rig-cat-410.
       car-rig-cat-402.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          *-------------------------------------*
           move      w-let-dcp-pdx-tpr    to   w-rig-tip-pro (1)      .
           go to     car-rig-cat-410.
       car-rig-cat-410.
      *                      *-----------------------------------------*
      *                      * Codice iva                              *
      *                      *-----------------------------------------*
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
      *
           if        w-tes-ass-iva (1)    not  = zero and
                     w-edt-iva-cod-003    =    zero
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
           else      go to car-rig-cat-440.
       car-rig-cat-432.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          *-------------------------------------*
           move      w-let-dcp-pdx-umi    to   w-rig-umi-ven (1)      .
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
           else      go to car-rig-cat-450.
       car-rig-cat-442.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          *-------------------------------------*
           move      w-let-dcp-pdx-deq    to   w-rig-dec-qta (1)      .
           go to     car-rig-cat-450.
       car-rig-cat-450.
      *                      *-----------------------------------------*
      *                      * Quantita' ordinata                      *
      *                      *-----------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "A" or
                     w-rig-tip-rig-tpr (1)
                                          =    "C"
                     move  zero           to   w-rig-qta-ord (1)
           else      move  w-bro-qta-eva
                          (w-car-rig-cat-ctr)
                                          to   w-rig-qta-ord (1)      .
       car-rig-cat-455.
      *                      *-----------------------------------------*
      *                      * Seconda quantita'                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Parametri memorizzati               *
      *                          *-------------------------------------*
           move      rf-ocr-flg-puq       to   w-rig-flg-puq (1)      .
           move      rf-ocr-snx-2qt       to   w-rig-snx-2qt (1)      .
           move      rf-ocr-dec-2qt       to   w-rig-dec-2qt (1)      .
           move      rf-ocr-qta-a02       to   w-rig-qta-a02 (1)      .
      *                          *-------------------------------------*
      *                          * Test se trattamento necessario      *
      *                          *-------------------------------------*
           if        w-rig-snx-2qt (1)    not  = 1
                     go to car-rig-cat-457.
           if        w-rig-qta-ord (1)    =    zero
                     move  zero           to   w-rig-qta-a02 (1)
                     go to car-rig-cat-457.
      *                          *-------------------------------------*
      *                          * Determinazione                      *
      *                          *-------------------------------------*
           move      w-rig-qta-ord (1)    to   w-det-qta-a02-qim      .
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
       car-rig-cat-457.
      *                      *-----------------------------------------*
      *                      * Terza quantita' : non gestita           *
      *                      *-----------------------------------------*
           move      rf-ocr-snx-3qt       to   w-rig-snx-3qt (1)      .
           move      rf-ocr-dec-3qt       to   w-rig-dec-3qt (1)      .
           move      rf-ocr-qta-a03       to   w-rig-qta-a03 (1)      .
       car-rig-cat-460.
      *                      *-----------------------------------------*
      *                      * Segnale di riga considerata comunque    *
      *                      * saldata                                 *
      *                      *-----------------------------------------*
           move      spaces               to   w-rig-sdr-ccs (1)      .
       car-rig-cat-470.
      *                      *-----------------------------------------*
      *                      * Seconda quantita' : non gestita         *
      *                      *-----------------------------------------*
       car-rig-cat-480.
      *                      *-----------------------------------------*
      *                      * Terza quantita' : non gestita           *
      *                      *-----------------------------------------*
       car-rig-cat-490.
      *                      *-----------------------------------------*
      *                      * Decimali prezzo                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ri- *
      *                          * ga                                  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-492
           else      go to car-rig-cat-500.
       car-rig-cat-492.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Se ricalcolo sul prezzo : No    *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Bufferizzazione valore con- *
      *                                  * tenuto nel record [bir]     *
      *                                  *-----------------------------*
           move      rf-ocr-dec-prz       to   w-rig-dec-prz (1)      .
       car-rig-cat-500.
      *                      *-----------------------------------------*
      *                      * Valuta per prezzi standard              *
      *                      *-----------------------------------------*
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
      *                          * Se ricalcolo sul prezzo : No        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione valori contenu- *
      *                              * ti nel record [ocr]             *
      *                              *---------------------------------*
           move      rf-ocr-sgl-vps       to   w-rig-sgl-vps (1)      .
           move      rf-ocr-dec-vps       to   w-rig-dec-vps (1)      .
           move      rf-ocr-tdc-vps       to   w-rig-tdc-vps (1)      .
      *                              *---------------------------------*
      *                              * Determinazione coefficiente di  *
      *                              * cambio valuta per i prezzi      *
      *                              * standard                        *
      *                              *---------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      w-rig-sgl-vps (1)    to   w-coe-cmb-vlt-sdv      .
           move      w-rig-dec-vps (1)    to   w-coe-cmb-vlt-tdc      .
           move      w-tes-dat-doc        to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se cambio indeterminato : for-  *
      *                              * zatura di quello contenuto nel  *
      *                              * record [ocr]                    *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-ocr-cdc-vps to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Bufferizzazione                 *
      *                              *---------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-rig-cdc-vps (1)      .
      *                              *---------------------------------*
      *                              * Oltre                           *
      *                              *---------------------------------*
           go to     car-rig-cat-510.
       car-rig-cat-510.
      *                      *-----------------------------------------*
      *                      * Prezzo lordo standard                   *
      *                      *-----------------------------------------*
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
      *                          * Se ricalcolo sul prezzo : No        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione prezzo lordo    *
      *                              * standard da record [ocr]        *
      *                              *---------------------------------*
           move      rf-ocr-prz-lrs       to   w-rig-prz-lrs (1)      .
       car-rig-cat-520.
      *                      *-----------------------------------------*
      *                      * Valuta per il prezzo                    *
      *                      *-----------------------------------------*
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
      *                          * Se ricalcolo sul prezzo : No        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione valuta per il   *
      *                              * prezzo da record [ocr]          *
      *                              *---------------------------------*
           move      rf-ocr-sgl-vpp       to   w-rig-sgl-vpp (1)      .
           move      rf-ocr-dec-vpp       to   w-rig-dec-vpp (1)      .
           move      rf-ocr-tdc-vpp       to   w-rig-tdc-vpp (1)      .
      *                              *---------------------------------*
      *                              * Determinazione coefficiente di  *
      *                              * cambio valuta per il prezzo     *
      *                              *---------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      w-rig-sgl-vpp (1)    to   w-coe-cmb-vlt-sdv      .
           move      w-rig-dec-vpp (1)    to   w-coe-cmb-vlt-tdc      .
           move      w-tes-dat-doc        to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se cambio indeterminato : buf-  *
      *                              * ferizzazione di quello contenu- *
      *                              * to nel record [ocr]             *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-ocr-cdc-vpp to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Bufferizzazione                 *
      *                              *---------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-rig-cdc-vpp (1)      .
       car-rig-cat-540.
      *                      *-----------------------------------------*
      *                      * Prezzo di vendita                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se ricalcolo sul prezzo : No        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione di vendita da   *
      *                              * record [ocr]                    *
      *                              *---------------------------------*
           move      rf-ocr-prz-ven       to   w-rig-prz-ven (1)      .
       car-rig-cat-550.
      *                      *-----------------------------------------*
      *                      * Secondo prezzo : non gestito            *
      *                      *-----------------------------------------*
       car-rig-cat-560.
      *                      *-----------------------------------------*
      *                      * Legame valutario                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se gestione legame valutario non e- *
      *                          * sistente per il cliente : oltre     *
      *                          *-------------------------------------*
           if        w-tes-mac-lvl (1)    =    00
                     go to car-rig-cat-570.
      *                          *-------------------------------------*
      *                          * Se gestione legame valutario non e- *
      *                          * sistente per la riga : oltre        *
      *                          *-------------------------------------*
           if        rf-ocr-sgl-vpl       =    spaces
                     go to car-rig-cat-570.
      *                          *-------------------------------------*
      *                          * Solo se tipo riga 'P'               *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "P"
                     go to car-rig-cat-570.
      *                          *-------------------------------------*
      *                          * Bufferizzazione dati legame valuta- *
      *                          * rio da record [ocr], escluso coef-  *
      *                          * ficiente di cambio effettivo        *
      *                          *-------------------------------------*
           move      rf-ocr-sgl-vpl       to   w-rig-sgl-vpl (1)      .
           move      rf-ocr-dec-vpl       to   w-rig-dec-vpl (1)      .
           move      rf-ocr-tdc-vpl       to   w-rig-tdc-vpl (1)      .
           move      rf-ocr-prz-vpl       to   w-rig-prz-vpl (1)      .
           move      rf-ocr-ccr-vpl       to   w-rig-ccr-vpl (1)      .
           move      rf-ocr-plm-vpl       to   w-rig-plm-vpl (1)      .
           move      rf-ocr-tlm-vpl       to   w-rig-tlm-vpl (1)      .
           move      rf-ocr-map-vpl       to   w-rig-map-vpl (1)      .
      *                          *-------------------------------------*
      *                          * Determinazione coefficiente di cam- *
      *                          * bio effettivo per legame valutario  *
      *                          *-------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      w-rig-sgl-vpl (1)    to   w-coe-cmb-vlt-sdv      .
           move      w-rig-dec-vpl (1)    to   w-coe-cmb-vlt-tdc      .
           move      w-tes-dat-doc        to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                          *-------------------------------------*
      *                          * Se cambio indeterminato : forzatura *
      *                          * di quello contenuto nel record      *
      *                          * [ocr]                               *
      *                          *-------------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-ocr-cdc-vpl to   w-coe-cmb-vlt-cdc      .
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-rig-cdc-vpl (1)      .
       car-rig-cat-570.
      *                      *-----------------------------------------*
      *                      * Tipo esposizione riga in fattura        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ri- *
      *                          * ga                                  *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          =    "P"
                     go to car-rig-cat-572
           else      go to car-rig-cat-580.
       car-rig-cat-572.
      *                          *-------------------------------------*
      *                          * Tipo prodotto : Prodotto di vendita *
      *                          *-------------------------------------*
           move      w-let-dcp-pdx-epz    to   w-rig-epz-rgf (1)      .
           go to     car-rig-cat-580.
       car-rig-cat-580.
      *                      *-----------------------------------------*
      *                      * Categoria e percentuali di sconto in    *
      *                      * riga associate al prodotto              *
      *                      *-----------------------------------------*
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
      *                          * Se ricalcolo sul prezzo : No        *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione valori contenu- *
      *                              * ti nel record [ocr]             *
      *                              *---------------------------------*
           move      rf-ocr-csr-aap       to   w-rig-csr-aap (1)      .
           move      rf-ocr-psr-aap (1)   to   w-rig-psr-aap (1, 1)   .
           move      rf-ocr-psr-aap (2)   to   w-rig-psr-aap (1, 2)   .
           move      rf-ocr-psr-aap (3)   to   w-rig-psr-aap (1, 3)   .
           move      rf-ocr-psr-aap (4)   to   w-rig-psr-aap (1, 4)   .
           move      rf-ocr-psr-aap (5)   to   w-rig-psr-aap (1, 5)   .
       car-rig-cat-590.
      *                      *-----------------------------------------*
      *                      * Sconti in riga                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se ricalcolo sugli sconti : No      *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione percentuali di  *
      *                              * sconto da record [ocr]          *
      *                              *---------------------------------*
           move      rf-ocr-per-scr (1)   to   w-rig-per-scr (1, 1)   .
           move      rf-ocr-per-scr (2)   to   w-rig-per-scr (1, 2)   .
           move      rf-ocr-per-scr (3)   to   w-rig-per-scr (1, 3)   .
           move      rf-ocr-per-scr (4)   to   w-rig-per-scr (1, 4)   .
           move      rf-ocr-per-scr (5)   to   w-rig-per-scr (1, 5)   .
       car-rig-cat-600.
      *                      *-----------------------------------------*
      *                      * Tipo esposizione prezzi e sconti        *
      *                      *-----------------------------------------*
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
      *                          * Se tipo riga diverso da 'A' : oltre *
      *                          *-------------------------------------*
           if        w-rig-tip-rig-tpr (1)
                                          not  = "A"
                     go to car-rig-cat-635.
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
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
      *                          * Se ricalcolo provvigioni : No       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione valori contenu- *
      *                              * ti nel record [ocr]             *
      *                              *---------------------------------*
           move      rf-ocr-cpv-aap       to   w-rig-cpv-aap (1)      .
           move      rf-ocr-ppv-aap (1)   to   w-rig-ppv-aap (1, 1)   .
           move      rf-ocr-ppv-aap (2)   to   w-rig-ppv-aap (1, 2)   .
           move      rf-ocr-ppv-aap (3)   to   w-rig-ppv-aap (1, 3)   .
       car-rig-cat-650.
      *                      *-----------------------------------------*
      *                      * Flag di significativita' provvigioni in *
      *                      * riga                                    *
      *                      *-----------------------------------------*
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
      *                          * Se ricalcolo provvigioni : No       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione flag significa- *
      *                              * tivita' provvigioni in riga da  *
      *                              * record [ocr]                    *
      *                              *---------------------------------*
           move      rf-ocr-fsp-rig       to   w-rig-fsp-rig (1)      .
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
      *                          * Se ricalcolo provvigioni : No       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione percentuali di  *
      *                              * provvigione da record [ocr]     *
      *                              *---------------------------------*
           move      rf-ocr-ppv-rig (1)   to   w-rig-ppv-rig (1, 1)   .
           move      rf-ocr-ppv-rig (2)   to   w-rig-ppv-rig (1, 2)   .
           move      rf-ocr-ppv-rig (3)   to   w-rig-ppv-rig (1, 3)   .
       car-rig-cat-680.
      *                      *-----------------------------------------*
      *                      * Provvigione a forfait in riga : zero    *
      *                      *-----------------------------------------*
           move      zero                 to   w-rig-pvf-rig (1)      .
       car-rig-cat-690.
      *                      *-----------------------------------------*
      *                      * Data consegna richiesta                 *
      *                      *                                         *
      *                      * Da testata nuovo ordine                 *
      *                      *-----------------------------------------*
           move      w-tes-dat-cns (1)    to   w-rig-dcn-ric (1)      .
       car-rig-cat-700.
      *                      *-----------------------------------------*
      *                      * Data consegna prevista                  *
      *                      *                                         *
      *                      * Da testata nuovo ordine                 *
      *                      *-----------------------------------------*
           move      w-tes-dat-cns (1)    to   w-rig-dcn-prv (1)      .
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
           move      rf-ocr-des-ext       to   w-rig-des-ext (1)      .
       car-rig-cat-810.
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
      *                          * Se ricalcolo prezzo : No            *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Bufferizzazione prezzo netto da *
      *                              * record [ocr]                    *
      *                              *---------------------------------*
           move      rf-ocr-prz-nts       to   w-rig-prz-nts (1)      .
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
      *                      * Elementi riga 'forecast'                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-rig-flg-frc (1)      .
           move      w-acc-num-prt        to   w-rig-prt-frc (1)      .
           move      rf-ocr-num-prg       to   w-rig-prg-frc (1)      .
       car-rig-cat-850.
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
      *    * Ordinamento righe bufferizzate                            *
      *    *-----------------------------------------------------------*
       car-rig-cat-obr-000.
      *              *-------------------------------------------------*
      *              * Test se almeno due codici da ordinare           *
      *              *-------------------------------------------------*
           if        w-bro-num-ele        <    2
                     go to car-rig-cat-obr-999.
       car-rig-cat-obr-050.
      *              *-------------------------------------------------*
      *              * Ciclo di ordinamento                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-car-rig-cat-c01      .
       car-rig-cat-obr-100.
           add       1                    to   w-car-rig-cat-c01      .
           if        w-car-rig-cat-c01    =    w-bro-num-ele
                     go to car-rig-cat-obr-999.
           move      w-car-rig-cat-c01    to   w-car-rig-cat-c02
                                               w-car-rig-cat-c03      .
           move      w-bro-key-prg
                    (w-car-rig-cat-c01)   to   w-bro-key-svk          .
       car-rig-cat-obr-200.
           add       1                    to   w-car-rig-cat-c02      .
           if        w-car-rig-cat-c02    >    w-bro-num-ele
                     go to car-rig-cat-obr-300.
           if        w-bro-key-prg
                    (w-car-rig-cat-c02)   >    w-bro-key-svk
                     go to car-rig-cat-obr-200.
           move      w-car-rig-cat-c02    to   w-car-rig-cat-c03      .
           move      w-bro-key-prg
                    (w-car-rig-cat-c02)   to   w-bro-key-svk          .
           go to     car-rig-cat-obr-200.
       car-rig-cat-obr-300.
           move      w-car-rig-cat-c01    to   w-car-rig-cat-c04      .          
           if        w-bro-key-svk        >    w-bro-key-prg
                                              (w-car-rig-cat-c04)
                     go to car-rig-cat-obr-100.
           move      w-bro-sng-ele
                    (w-car-rig-cat-c03)   to   w-bro-sng-ele
                                              (w-bro-max-ele)         .
           move      w-bro-sng-ele
                    (w-car-rig-cat-c04)   to   w-bro-sng-ele
                                              (w-car-rig-cat-c03)     .
           move      w-bro-sng-ele
                    (w-bro-max-ele)       to   w-bro-sng-ele
                                              (w-car-rig-cat-c04)     .
           go to     car-rig-cat-obr-100.
       car-rig-cat-obr-999.
           exit.

      *    *===========================================================*
      *    * Richiamo sottoprogramma per gestione catena righe         *
      *    *-----------------------------------------------------------*
       cll-sub-cat-000.
           call      "pgm/orc/prg/obj/porc3002"
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
           move      zero                 to   w-rig-lot-ven (1)      .
           move      zero                 to   w-rig-qta-ord (1)      .
           move      spaces               to   w-rig-sdr-ccs (1)      .
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
           move      zero                 to   w-rig-dcn-ric (1)      .
           move      zero                 to   w-rig-dcn-prv (1)      .
           move      spaces               to   w-rig-flg-cnf (1)      .
           move      spaces               to   w-rig-cmc-tip (1)      .
           move      zero                 to   w-rig-cmc-dat (1)      .
           move      zero                 to   w-rig-cmc-num (1)      .
           move      spaces               to   w-rig-flg-rch (1)      .
           move      spaces               to   w-rig-flg-ela (1)      .
           move      spaces               to   w-rig-flg-pul (1)      .
           move      spaces               to   w-rig-alx-exp (1)      .
           move      zero                 to   w-rig-qta-eva (1)      .
           move      spaces               to   w-rig-tdo-uev (1)      .
           move      zero                 to   w-rig-ddo-uev (1)      .
           move      zero                 to   w-rig-ndo-uev (1)      .
       nor-nok-rig-999.
           exit.

      *    *===========================================================*
      *    * Routine di decomposizione tipo riga                       *
      *    *                                                           *
      *    * Input  : rf-ocr-tip-rig                                   *
      *    *                                                           *
      *    * Output : w-rig-tip-rig (1) e relativi valori di w-rig con-*
      *    *          nessi                                            *
      *    *-----------------------------------------------------------*
       dec-tip-rig-000.
      *              *-------------------------------------------------*
      *              * Tipo riga in comodo di lavoro ridefinito        *
      *              *-------------------------------------------------*
           move      rf-ocr-tip-rig       to    w-dec-tip-rig-str     .
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
      *    * Find su archivio [oct]                                    *
      *    *-----------------------------------------------------------*
       fnd-arc-oct-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-fnd-arc-oct-sel      .
      *              *-------------------------------------------------*
      *              * Test se programma di interrogazione gia' attivo *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "porc3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                not  = zero
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to  fnd-arc-oct-999.
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
           move      "pgm/orc/prg/obj/porc3010"
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
           move      "tmo-orc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to fnd-arc-oct-999.
           move      s-alf                to   w-fnd-arc-oct-toc      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dat-doc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to fnd-arc-oct-999.
           move      s-dat                to   w-fnd-arc-oct-dat      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "num-doc"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                not  = spaces
                     move  "#"            to   w-fnd-arc-oct-sel
                     go to fnd-arc-oct-999.
           move      s-num                to   w-fnd-arc-oct-num      .
      *              *-------------------------------------------------*
      *              * Salvataggio valori chiave                       *
      *              *-------------------------------------------------*
           move      w-acc                to   w-sav-val-acc          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori estratti                 *
      *              *-------------------------------------------------*
           move      w-fnd-arc-oct-toc    to   w-acc-tmo-orc          .
           move      w-fnd-arc-oct-dat    to   w-acc-dat-doc          .
           move      w-fnd-arc-oct-num    to   w-acc-num-doc          .
       fnd-arc-oct-400.
      *              *-------------------------------------------------*
      *              * Determinazione campi derivati e effettuazione   *
      *              * controlli come se fossero stati impostati       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc]                      *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orc        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to fnd-arc-oct-900.
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-acc-tmo-orc        =    spaces
                     go to fnd-arc-oct-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento ordini clienti                    *
      *                  *---------------------------------------------*
           move      w-let-arc-zoc-des    to   w-acc-tmo-orc-des      .
           move      w-let-arc-zoc-sgl    to   w-acc-tmo-orc-sgl      .
       fnd-arc-oct-600.
      *              *-------------------------------------------------*
      *              * Completamento visualizzazione campi di accet-   *
      *              * tazione                                         *
      *              *-------------------------------------------------*
           perform   vis-tmo-orc-000      thru vis-tmo-orc-999        .
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *              *-------------------------------------------------*
      *              * Uscita con successo                             *
      *              *-------------------------------------------------*
           go to     fnd-arc-oct-999.
       fnd-arc-oct-900.
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
           move      "#"                  to   w-fnd-arc-oct-sel      .
       fnd-arc-oct-999.
           exit.

      *    *===========================================================*
      *    * Select archivio [oct] in base al numero documento         *
      *    *-----------------------------------------------------------*
       slc-num-oct-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-slc-num-oct-sel      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni iniziali                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-slc-num-oct-toc      .
           move      zero                 to   w-slc-num-oct-dat      .
           move      zero                 to   w-slc-num-oct-num      .
           move      zero                 to   w-slc-num-oct-crb      .
      *              *-------------------------------------------------*
      *              * Preparazione secolo, anno                       *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-dds    to   s-dat                  .
           move      s-saa                to   w-slc-num-oct-saa      .
       slc-num-oct-080.
      *              *-------------------------------------------------*
      *              * Completamento numero documento                  *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-saa    to   w-slc-num-oct-nsa      .
           move      w-slc-num-oct-dpz    to   w-slc-num-oct-ndp      .
       slc-num-oct-100.
      *              *-------------------------------------------------*
      *              * Start su file [oct]                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start                                       *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CNTDEN    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-slc-num-oct-saa    to   rf-oct-scl-ann         .
           move      w-slc-num-oct-dpz    to   rf-oct-cod-dpz         .
           move      w-slc-num-oct-sgl    to   rf-oct-sgl-num         .
           move      w-slc-num-oct-nds    to   rf-oct-num-doc         .
           move      zero                 to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se errore di start : a controllo contatore  *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to slc-num-oct-800.
       slc-num-oct-200.
      *              *-------------------------------------------------*
      *              * Read-next su [oct]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Se at end : a controllo contatore           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to slc-num-oct-500.
       slc-num-oct-300.
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a controllo contatore     *
      *              *-------------------------------------------------*
           if        rf-oct-scl-ann       not  = w-slc-num-oct-saa or
                     rf-oct-cod-dpz       not  = w-slc-num-oct-dpz or
                     rf-oct-sgl-num       not  = w-slc-num-oct-sgl or
                     rf-oct-num-doc       not  = w-slc-num-oct-nds
                     go to slc-num-oct-500.
       slc-num-oct-400.
      *              *-------------------------------------------------*
      *              * Incremento numero records nel buffer            *
      *              *-------------------------------------------------*
           add       1                    to   w-slc-num-oct-crb      .
      *              *-------------------------------------------------*
      *              * Test se buffer oltre il numero previsto         *
      *              *-------------------------------------------------*
           if        w-slc-num-oct-crb    >    30
                     go to slc-num-oct-520.
       slc-num-oct-420.
      *              *-------------------------------------------------*
      *              * Bufferizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Protocollo ordine                           *
      *                  *---------------------------------------------*
           move      rf-oct-num-prt       to   w-slc-num-oct-bpt
                                              (w-slc-num-oct-crb)     .
      *                  *---------------------------------------------*
      *                  * Riciclo a lettura                           *
      *                  *---------------------------------------------*
           go to     slc-num-oct-200.
       slc-num-oct-500.
      *                  *---------------------------------------------*
      *                  * Controllo numero records letti con lo stes- *
      *                  * so valore                                   *
      *                  *---------------------------------------------*
           if        w-slc-num-oct-crb    =    zero
                     go to slc-num-oct-800.
      *                  *---------------------------------------------*
      *                  * Se trovato un solo elemento uscita con      *
      *                  * quello                                      *
      *                  *---------------------------------------------*
           if        w-slc-num-oct-crb    >    1
                     go to slc-num-oct-520.
      *                      *-----------------------------------------*
      *                      * Lettura record [oct]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-oct-bpt (1)
                                          to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-oct-dat-doc
                     move  zero           to   rf-oct-num-doc
                     move  zero           to   rf-oct-cod-arc
                     move  spaces         to   rf-oct-dpz-arc         .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione dati letti              *
      *                      *-----------------------------------------*
           move      rf-oct-dat-doc       to   w-slc-num-oct-dat      .
           move      rf-oct-num-doc       to   w-slc-num-oct-num      .
           move      rf-oct-tmo-orc       to   w-slc-num-oct-toc      .
      *                      *-----------------------------------------*
      *                      * Ad operazioni prima dell'uscita         *
      *                      *-----------------------------------------*
           go to     slc-num-oct-800.
       slc-num-oct-520.
      *                  *---------------------------------------------*
      *                  * Box di espansione                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione numero pagine nel buffer *
      *                      *-----------------------------------------*
           move      w-slc-num-oct-crb    to   w-slc-num-oct-cpb      .
           subtract  1                    from w-slc-num-oct-cpb      .
           divide    6                    into w-slc-num-oct-cpb      .
           add       1                    to   w-slc-num-oct-cpb      .
      *                      *-----------------------------------------*
      *                      * Inizializzazione numero record nel buf- *
      *                      * fer attualmente trattato                *
      *                      *-----------------------------------------*
           move      1                    to   w-slc-num-oct-c01      .
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
           perform   slc-num-oct-950      thru slc-num-oct-989        .
      *                      *-----------------------------------------*
      *                      * Video in On                             *
      *                      *-----------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-550.
      *                      *-----------------------------------------*
      *                      * Determinazione numero linea a video     *
      *                      *-----------------------------------------*
           move      w-slc-num-oct-c01    to   w-slc-num-oct-nli      .
       slc-num-oct-555.
           if        w-slc-num-oct-nli    >    6
                     subtract  6          from w-slc-num-oct-nli
                     go to slc-num-oct-555.
      *                          *-------------------------------------*
      *                          * Incremento numero linea a video     *
      *                          * per posizionamento verticale        *
      *                          *-------------------------------------*
           add       09                   to   w-slc-num-oct-nli      .
       slc-num-oct-560.
      *                      *-----------------------------------------*
      *                      * Espansione record attualmente trattato  *
      *                      *-----------------------------------------*
       slc-num-oct-575.
      *                      *-----------------------------------------*
      *                      * Accettazione del mark-point             *
      *                      *-----------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "K"                  to   v-tip                  .
           move      spaces               to   v-ufk                  .
           if        w-slc-num-oct-c01    >    1
                     move  "UP  "         to   v-pfk (01)             .
           if        w-slc-num-oct-c01    <    w-slc-num-oct-crb
                     move  "DOWN"         to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           if        w-slc-num-oct-cpa    >    1
                     move  "PRSC"         to   v-pfk (07)             .
           if        w-slc-num-oct-cpa    <    w-slc-num-oct-cpb
                     move  "NXSC"         to   v-pfk (08)             .
           move      "EXIT"               to   v-pfk (20)             .
           move      w-slc-num-oct-nli    to   v-lin                  .
           move      09                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-580.
           if        v-key                =    spaces or
                     v-key                =    "DO  "
                     go to slc-num-oct-582
           else if   v-key                =    "UP  "
                     go to slc-num-oct-584
           else if   v-key                =    "DOWN"
                     go to slc-num-oct-586
           else if   v-key                =    "EXIT"
                     go to slc-num-oct-598
           else if   v-key                =    "NXSC"
                     go to slc-num-oct-592
           else if   v-key                =    "PRSC"
                     go to slc-num-oct-594
           else      go to slc-num-oct-575.
       slc-num-oct-582.
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
           move      w-slc-num-oct-bpt
                    (w-slc-num-oct-c01)   to   w-slc-num-oct-prt      .
      *                  *---------------------------------------------*
      *                  * Lettura record [oct]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-oct-prt    to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-oct-dat-doc
                     move  zero           to   rf-oct-num-doc
                     move  zero           to   rf-oct-cod-arc
                     move  spaces         to   rf-oct-dpz-arc         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione dati letti                  *
      *                  *---------------------------------------------*
           move      rf-oct-dat-doc       to   w-slc-num-oct-dat      .
           move      rf-oct-num-doc       to   w-slc-num-oct-num      .
           move      rf-oct-tmo-orc       to   w-slc-num-oct-toc      .
      *                  *---------------------------------------------*
      *                  * Ad operazioni prima dell'uscita             *
      *                  *---------------------------------------------*
           go to     slc-num-oct-800.
       slc-num-oct-584.
      *              *-------------------------------------------------*
      *              * Se Up                                           *
      *              *-------------------------------------------------*
           subtract  1                    from w-slc-num-oct-c01      .
           if        w-slc-num-oct-nli    =    10
                     go to slc-num-oct-590
           else      go to slc-num-oct-550.
       slc-num-oct-586.
      *              *-------------------------------------------------*
      *              * Se Down                                         *
      *              *-------------------------------------------------*
           if        w-slc-num-oct-c01    <    w-slc-num-oct-crb
                     add   1              to   w-slc-num-oct-c01
                     go to slc-num-oct-588
           else      go to slc-num-oct-575.
       slc-num-oct-588.
           if        w-slc-num-oct-nli    =    15
                     go to slc-num-oct-590
           else      go to slc-num-oct-550.
       slc-num-oct-590.
           perform   slc-num-oct-950      thru slc-num-oct-989        .
           go to     slc-num-oct-550.
       slc-num-oct-592.
      *              *-------------------------------------------------*
      *              * Se Next screen                                  *
      *              *-------------------------------------------------*
           add       1                    to   w-slc-num-oct-cpa      .
           go to     slc-num-oct-596.
       slc-num-oct-594.
      *              *-------------------------------------------------*
      *              * Se Previous screen                              *
      *              *-------------------------------------------------*
           subtract  1                    from w-slc-num-oct-cpa      .
       slc-num-oct-596.
           move      w-slc-num-oct-cpa    to   w-slc-num-oct-c01      .
           multiply  6                    by   w-slc-num-oct-c01      .
           subtract  5                    from w-slc-num-oct-c01      .
           go to     slc-num-oct-590.
       slc-num-oct-598.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-800.
      *              *-------------------------------------------------*
      *              * Salvataggio valori chiave                       *
      *              *-------------------------------------------------*
           move      w-acc                to   w-sav-val-acc          .
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori estratti                 *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-toc    to   w-acc-tmo-orc          .
           move      w-slc-num-oct-dat    to   w-acc-dat-doc          .
           move      w-slc-num-oct-num    to   w-acc-num-doc          .
      *              *-------------------------------------------------*
      *              * Test su valori estratti                         *
      *              *-------------------------------------------------*
           if        w-acc-tmo-orc        =    spaces or
                     w-acc-dat-doc        =    zero   or
                     w-acc-num-doc        =    zero
                     go to slc-num-oct-900.
       slc-num-oct-820.
      *              *-------------------------------------------------*
      *              * Determinazione campi derivati e effettuazione   *
      *              * controlli come se fossero stati impostati       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura archivio [zoc]                      *
      *                  *---------------------------------------------*
           move      w-acc-tmo-orc        to   w-let-arc-zoc-cod      .
           perform   let-arc-zoc-000      thru let-arc-zoc-999        .
      *                  *---------------------------------------------*
      *                  * Se codice errato : reimpostazione           *
      *                  *---------------------------------------------*
           if        w-let-arc-zoc-flg    not  = spaces
                     go to slc-num-oct-900.
      *                  *---------------------------------------------*
      *                  * Se a spaces : reimpostazione                *
      *                  *---------------------------------------------*
           if        w-acc-tmo-orc        =    spaces
                     go to slc-num-oct-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori associati al tipo    *
      *                  * movimento ordini clienti                    *
      *                  *---------------------------------------------*
           move      w-let-arc-zoc-des    to   w-acc-tmo-orc-des      .
           move      w-let-arc-zoc-sgl    to   w-acc-tmo-orc-sgl      .
       slc-num-oct-860.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Completamento visualizzazione campi-chiave  *
      *                  *---------------------------------------------*
           perform   vis-tmo-orc-000      thru vis-tmo-orc-999        .
           perform   vis-tmo-orc-des-000  thru vis-tmo-orc-des-999    .
           perform   vis-dat-doc-000      thru vis-dat-doc-999        .
           perform   vis-num-doc-000      thru vis-num-doc-999        .
      *                  *---------------------------------------------*
      *                  * Uscita con successo                         *
      *                  *---------------------------------------------*
           go to     slc-num-oct-999.
       slc-num-oct-900.
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
           move      "#"                  to   w-slc-num-oct-sel      .
       slc-num-oct-940.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     slc-num-oct-999.
       slc-num-oct-950.
      *              *-------------------------------------------------*
      *              * Visualizzazione pagina video contenente il re-  *
      *              * cord attualmente trattato                       *
      *              *-------------------------------------------------*
           move      w-slc-num-oct-c01    to   w-slc-num-oct-c02      .
           add       5                    to   w-slc-num-oct-c02      .
           divide    6                    into w-slc-num-oct-c02      .
           move      w-slc-num-oct-c02    to   w-slc-num-oct-cpa      .
           subtract  1                    from w-slc-num-oct-c02      .
           multiply  6                    by   w-slc-num-oct-c02      .
           add       1                    to   w-slc-num-oct-c02      .
           add       5
                     w-slc-num-oct-c02  giving w-slc-num-oct-c03      .
           move      w-slc-num-oct-c03    to   w-slc-num-oct-c04      .
           if        w-slc-num-oct-c03    >    w-slc-num-oct-crb
                     move  w-slc-num-oct-crb
                                          to   w-slc-num-oct-c03      .
           move      10                   to   w-slc-num-oct-c05      .
       slc-num-oct-951.
      *              *-------------------------------------------------*
      *              * Lettura record [oct] per visualizzazione        *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-slc-num-oct-bpt
                    (w-slc-num-oct-c02)   to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
           if        f-sts                not  = e-not-err
                     move  zero           to   rf-oct-dat-doc
                     move  zero           to   rf-oct-num-doc
                     move  zero           to   rf-oct-cod-arc
                     move  spaces         to   rf-oct-dpz-arc         .
       slc-num-oct-960.
      *              *-------------------------------------------------*
      *              * Visualizzazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Codice tipo movimento                       *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      03                   to   v-pos                  .
           move      rf-oct-tmo-orc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      09                   to   v-pos                  .
           move      rf-oct-dat-doc       to   v-dat                  .
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
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      18                   to   v-pos                  .
           move      rf-oct-num-doc       to   w-acc-num-doc          .
           move      w-acc-num-doc-prg    to   v-num                  .
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
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      26                   to   v-pos                  .
           move      rf-oct-cod-arc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza archivio                  *
      *                  *---------------------------------------------*
           if        rf-oct-dpz-arc       =    spaces
                     go to slc-num-oct-965.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      33                   to   v-pos                  .
           move      "-"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      34                   to   v-pos                  .
           move      rf-oct-dpz-arc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-965.
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      rf-oct-cod-arc       to   rf-dcc-cod-cli         .
           move      rf-oct-dpz-arc       to   rf-dcc-dpz-cli         .
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
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      39                   to   v-pos                  .
           move      rf-dcc-rag-soc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Incremento contatori                        *
      *                  *---------------------------------------------*
           add       1                    to   w-slc-num-oct-c02      .
           add       1                    to   w-slc-num-oct-c05      .
           if        w-slc-num-oct-c02    not  > w-slc-num-oct-c03
                     go to slc-num-oct-951.
       slc-num-oct-970.
           if        w-slc-num-oct-c02    >    w-slc-num-oct-c04
                     go to slc-num-oct-980.
           if        w-slc-num-oct-crb    not  > 6
                     go to slc-num-oct-980.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      60                   to   v-car                  .
           move      w-slc-num-oct-c05    to   v-lin                  .
           move      11                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
           add       1                    to   w-slc-num-oct-c02      .
           add       1                    to   w-slc-num-oct-c05      .
           go to     slc-num-oct-970.
       slc-num-oct-980.
      *                  *---------------------------------------------*
      *                  * Literal 'pagina'                            *
      *                  *---------------------------------------------*
           move      w-slc-num-oct-cpa    to   w-slc-num-oct-lt1      .
           move      w-slc-num-oct-cpb    to   w-slc-num-oct-lt2      .
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      17                   to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-slc-num-oct-ltp    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       slc-num-oct-989.
           exit.
       slc-num-oct-999.
           exit.

      *    *===========================================================*
      *    * Routine di lettura archivio [zoc]                         *
      *    *-----------------------------------------------------------*
       let-arc-zoc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zoc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-zoc-cod    =    spaces
                     go to let-arc-zoc-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTOC"             to   f-key                  .
           move      w-let-arc-zoc-cod    to   rf-zoc-cod-toc         .
           move      "pgm/orc/fls/ioc/obj/iofzoc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zoc                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-zoc-400.
       let-arc-zoc-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zoc-des-toc       to   w-let-arc-zoc-des      .
           move      rf-zoc-vld-dpz       to   w-let-arc-zoc-vld      .
           move      rf-zoc-cod-dpz       to   w-let-arc-zoc-dpz      .
           move      rf-zoc-org-doc       to   w-let-arc-zoc-ord      .
           move      rf-zoc-prv-doc       to   w-let-arc-zoc-prd      .
           move      rf-zoc-sgl-num       to   w-let-arc-zoc-sgl      .
           move      rf-zoc-def-tpr       to   w-let-arc-zoc-dtr      .
           move      rf-zoc-tip-ord       to   w-let-arc-zoc-tip      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-zoc-999.
       let-arc-zoc-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-zoc-flg      .
           move      all   "."            to   w-let-arc-zoc-des      .
           go to     let-arc-zoc-600.
       let-arc-zoc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-zoc-des      .
       let-arc-zoc-600.
           move      spaces               to   w-let-arc-zoc-des      .
           move      zero                 to   w-let-arc-zoc-vld      .
           move      zero                 to   w-let-arc-zoc-dpz      .
           move      zero                 to   w-let-arc-zoc-ord      .
           move      zero                 to   w-let-arc-zoc-prd      .
           move      spaces               to   w-let-arc-zoc-sgl      .
           move      spaces               to   w-let-arc-zoc-dtr      .
           move      spaces               to   w-let-arc-zoc-tip      .
       let-arc-zoc-999.
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
      *    * Routine di lettura archivio [ocx]                         *
      *    *-----------------------------------------------------------*
       let-arc-ocx-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ocx-flg      .
      *              *-------------------------------------------------*
      *              * Test se codici nulli                            *
      *              *-------------------------------------------------*
           if        w-let-arc-ocx-prt    =    zero   or
                     w-let-arc-ocx-prg    =    zero   or
                     w-let-arc-ocx-trc    =    zero
                     go to let-arc-ocx-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-let-arc-ocx-prt    to   rf-ocx-num-prt         .
           move      w-let-arc-ocx-prg    to   rf-ocx-num-prg         .
           move      w-let-arc-ocx-trc    to   rf-ocx-tip-rec         .
           move      "pgm/orc/fls/ioc/obj/iofocx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocx                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-ocx-400.
       let-arc-ocx-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ocx-des-400       to   w-let-arc-ocx-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-ocx-999.
       let-arc-ocx-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-ocx-flg      .
           move      spaces               to   w-let-arc-ocx-des      .
           move      all   "."            to   w-let-arc-ocx-drg (1)  .
           go to     let-arc-ocx-999.
       let-arc-ocx-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-ocx-des      .
       let-arc-ocx-999.
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
      *    * Routine lettura archivio [dcp]                            *
      *    *-----------------------------------------------------------*
       let-arc-dcp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice Prodotto di vendita a zero       *
      *              *-------------------------------------------------*
           if        w-let-arc-dcp-num    =    zero
                     go to let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      w-let-arc-dcp-num    to   rf-dcp-num-pro         .
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
           move      rf-dcp-alf-pro       to   w-let-arc-dcp-alf      .
           move      rf-dcp-des-pro       to   w-let-arc-dcp-des      .
           move      rf-dcp-umi-ven       to   w-let-arc-dcp-umi      .
           move      rf-dcp-dec-qta       to   w-let-arc-dcp-ndq      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcp-999.
       let-arc-dcp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcp-flg      .
           move      all   "."            to   w-let-arc-dcp-des      .
           go to     let-arc-dcp-600.
       let-arc-dcp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcp-des      .
       let-arc-dcp-600.
           move      spaces               to   w-let-arc-dcp-alf      .
           move      spaces               to   w-let-arc-dcp-umi      .
           move      zero                 to   w-let-arc-dcp-ndq      .
       let-arc-dcp-999.
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
      *              * Protocollo movimento trovato                    *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-doc-ges-prt      .
      *              *-------------------------------------------------*
      *              * Tipo movimento trovato per il caso che la de-   *
      *              * terminazione dia esito pari a 'X' : a spaces    *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-doc-ges-tmt      .
      *              *-------------------------------------------------*
      *              * Tipo ordine trovato                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-doc-ges-tor      .
      *              *-------------------------------------------------*
      *              * Ordine controllato                              *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-doc-ges-ctl      .
      *              *-------------------------------------------------*
      *              * Codice cliente trovato                          *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-doc-ges-car      .
      *              *-------------------------------------------------*
      *              * Start su file [oct]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      w-acc-dat-doc        to   rf-oct-dat-doc         .
           move      w-acc-cod-dpz        to   rf-oct-cod-dpz         .
           move      w-acc-num-doc        to   rf-oct-num-doc         .
           move      spaces               to   rf-oct-tmo-orc         .
           move      zero                 to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : a documento non esistente     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-doc-ges-900.
       det-doc-ges-100.
      *              *-------------------------------------------------*
      *              * Next su [oct]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *              *-------------------------------------------------*
      *              * Se 'at end' : a determinazione finale           *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-doc-ges-800.
      *              *-------------------------------------------------*
      *              * Max su [oct], se non superato : a determinazio- *
      *              * ne finale                                       *
      *              *-------------------------------------------------*
           if        rf-oct-dat-doc       not  = w-acc-dat-doc or
                     rf-oct-cod-dpz       not  = w-acc-cod-dpz or
                     rf-oct-num-doc       not  = w-acc-num-doc
                     go to det-doc-ges-800.
       det-doc-ges-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valore determinato              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di documento esistente e trovato    *
      *                  *---------------------------------------------*
           move      "S"                  to   w-det-doc-ges-snx      .
      *                  *---------------------------------------------*
      *                  * Tipo documento esistente e trovato          *
      *                  *---------------------------------------------*
           move      rf-oct-tmo-orc       to   w-det-doc-ges-tmt      .
      *                  *---------------------------------------------*
      *                  * Protocollo individuato                      *
      *                  *---------------------------------------------*
           move      rf-oct-num-prt       to   w-det-doc-ges-prt      .
      *                  *---------------------------------------------*
      *                  * Tipo ordine trovato                         *
      *                  *---------------------------------------------*
           move      rf-oct-tip-ord       to   w-det-doc-ges-tor      .
      *                  *---------------------------------------------*
      *                  * Ordine controllato                          *
      *                  *---------------------------------------------*
           move      rf-oct-flg-nbx (01)  to   w-det-doc-ges-ctl      .
      *                  *---------------------------------------------*
      *                  * Codice cliente trovato                      *
      *                  *---------------------------------------------*
           move      rf-oct-cod-arc       to   w-det-doc-ges-car      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-doc-ges-999.
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
      *    * Determinazione quantita' secondaria per la vendita        *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/wdetqts0.wks"                   .

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
           if        w-tes-ocl-dat (1)    not  = zero
                     move  w-tes-ocl-dat (1)
                                          to   d-pvg-age-dat-rif
           else      move  w-tes-dat-doc  to   d-pvg-age-dat-rif      .
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
      *    * Editing quantita' da incolonnare                          *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wedtqta0.wks"                   .

      *    *===========================================================*
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .

      *    *===========================================================*
      *    * Determinazione importo in riga                            *
      *    *-----------------------------------------------------------*
       det-imp-rig-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per la determinazione    *
      *              *-------------------------------------------------*
           move      "IR"                 to   d-imp-ven-tip-ope      .
           move      w-rig-qta-ord (1)    to   d-imp-ven-qta-ven      .
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
           move      d-imp-ven-imp-rig    to w-rig-imp-rig (1)        .
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
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-e02-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione tipo movimento per ordini  *
      *    * clienti                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/acdezoc0.acs"                   .

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
      *    * Subroutines per determinazione provvigioni                *
      *    *-----------------------------------------------------------*
           copy      "pgm/age/prg/cpy/dpvgage0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione quantita' da evadere riga  *
      *    * ordine cliente                                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dqevroc0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

