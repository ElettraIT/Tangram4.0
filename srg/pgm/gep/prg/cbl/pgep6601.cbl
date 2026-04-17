       Identification division.
       Program-Id.                                 pgep6601           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    gep                 *
      *                                Settore:    cli                 *
      *                                   Fase:    gep660              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 21/02/04    *
      *                       Ultima revisione:    NdK del 28/02/25    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Esecuzione stampa per il programma gep660   *
      *                                                                *
      *                    Stampa clienti con maggiore esposizione     *
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

      *================================================================*
       Input-Output Section.
      *================================================================*

       File-Control.

      *    *===========================================================*
      *    * File Control [srt]                                        *
      *    *-----------------------------------------------------------*
           select  srt       assign       to sort                     .

      ******************************************************************
       Data Division.
      ******************************************************************

      *================================================================*
       File Section.
      *================================================================*

      *    *===========================================================*
      *    * File Description [srt]                                    *
      *    *-----------------------------------------------------------*
       sd  srt.
      *    *-----------------------------------------------------------*
      *    * Sort record                                               *
      *    *-----------------------------------------------------------*
       01  srt-rec.
      *        *-------------------------------------------------------*
      *        * Chiave di ordinamento                                 *
      *        *-------------------------------------------------------*
           05  srt-key.
      *            *---------------------------------------------------*
      *            * Valore ABC cliente                                *
      *            *---------------------------------------------------*
               10  srt-abc-cli            pic s9(15)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Codice cliente                                    *
      *            *---------------------------------------------------*
               10  srt-cod-cli            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Cumulo totale                                     *
      *            *---------------------------------------------------*
               10  srt-cum-tot            pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Cumulo mese in corso                              *
      *            *---------------------------------------------------*
               10  srt-cum-mic            pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Cumulo mese precedente 1                          *
      *            *---------------------------------------------------*
               10  srt-cum-ms1            pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Cumulo mese precedente 2                          *
      *            *---------------------------------------------------*
               10  srt-cum-ms2            pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Cumulo mese precedente 3                          *
      *            *---------------------------------------------------*
               10  srt-cum-ms3            pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Cumulo mese precedente 4                          *
      *            *---------------------------------------------------*
               10  srt-cum-ms4            pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Cumulo mese precedente 5                          *
      *            *---------------------------------------------------*
               10  srt-cum-ms5            pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Cumulo mese precedente 6                          *
      *            *---------------------------------------------------*
               10  srt-cum-ms6            pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Cumulo mese precedente 7                          *
      *            *---------------------------------------------------*
               10  srt-cum-ms7            pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Cumulo oltre                                      *
      *            *---------------------------------------------------*
               10  srt-cum-mso            pic s9(13)                  .

      *================================================================*
       Working-Storage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03) value
                     "pgm"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "gep"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "cli"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "gep660"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pgep6601"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "STAMPA CLIENTI CON MAGGIORE ESPOSIZIONE "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "msegrt"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                "mbckgx"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mmessg"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mxport" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/x"                                  .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Flags di controllo uscita da routines fondamentali    *
      *        *-------------------------------------------------------*
           05  w-cnt-flg.
      *            *---------------------------------------------------*
      *            * Per routine dic-ini-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-dic-ini-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine pre-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-exe-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine let-rec-ric-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-let-rec-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine let-sel-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-let-sel-stp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine exe-rou-srt-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-exe-rou-srt      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo per tipo funzionamento             *
      *        *-------------------------------------------------------*
           05  w-cnt-fun.
      *            *---------------------------------------------------*
      *            * Si/No record richieste                            *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No stampa                                      *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-stp      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per funzionamento print-routine     *
      *        *-------------------------------------------------------*
           05  w-cnt-prn.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cnt-prn-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Segnale di 'begin' eseguito                       *
      *            *---------------------------------------------------*
               10  w-cnt-prn-mrk-beg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-sub      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio parametri rottura livello    *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-liv.
                   15  w-cnt-prn-sav-l05  pic  x(64)                  .
                   15  w-cnt-prn-sav-l04  pic  x(64)                  .
                   15  w-cnt-prn-sav-l03  pic  x(64)                  .
                   15  w-cnt-prn-sav-l02  pic  x(64)                  .
                   15  w-cnt-prn-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio area di rottura              *
      *            *---------------------------------------------------*
               10  w-cnt-prn-sav-rot.
                   15  filler occurs 320  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per manipolazione titolo stampato                *
      *        *-------------------------------------------------------*
           05  w-cnt-tit.
               10  w-cnt-tit-des-tit.
                   15  w-cnt-tit-chr-tit  occurs 80
                                          pic  x(01)                  .
               10  w-cnt-tit-num-pag      pic  9(05)                  .
               10  w-cnt-tit-dat-stp      pic  9(07)                  .
               10  w-cnt-tit-des-azi.
                   15  w-cnt-tit-chr-azi  occurs 40
                                          pic  x(01)                  .
               10  w-cnt-tit-ctr-wrk      pic  9(02)                  .
               10  w-cnt-tit-ctr-azi      pic  9(02)                  .
               10  w-cnt-tit-ctr-tit      pic  9(02)                  .
               10  w-cnt-tit-pos-tit      pic  9(03)                  .
               10  w-cnt-tit-ctr-dep      pic  9(02)                  .
               10  w-cnt-tit-ctr-cif      pic  9(02)                  .
               10  w-cnt-tit-pos-dep      pic  9(03)                  .
               10  w-cnt-tit-num-lin      pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per string-unstring                              *
      *        *-------------------------------------------------------*
           05  w-cnt-stu.
               10  w-cnt-stu-num-seg      pic  9(05)                  .
               10  w-cnt-stu-pnt-stu      pic  9(05)                  .
               10  w-cnt-stu-255-byt.
                   15  filler occurs 255  pic  x(01)                  .
               10  w-cnt-stu-sav-pnt      pic  9(05)                  .

      *    *===========================================================*
      *    * Records logici                                            *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [ccc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfccc"                          .
      *        *-------------------------------------------------------*
      *        * [cec]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcec"                          .
      *        *-------------------------------------------------------*
      *        * [sdb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfsdb"                          .
      *        *-------------------------------------------------------*
      *        * [ddp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfddp"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Data di riferimento                                   *
      *        *-------------------------------------------------------*
           05  rr-dat-rif                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Max elementi                                          *
      *        *-------------------------------------------------------*
           05  rr-max-ele                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Si/no stampa fido                                     *
      *        *                                                       *
      *        *  - '1' : No                                           *
      *        *  - '2' : Si                                           *
      *        *-------------------------------------------------------*
           05  rr-sns-fid                 pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/No esportazione archivio                           *
      *        *-------------------------------------------------------*
           05  rr-snx-exp                 pic  9(01)                  .

      *    *===========================================================*
      *    * Work per tabelle                                          *
      *    *-----------------------------------------------------------*
       01  w-tbl.
      *        *-------------------------------------------------------*
      *        * Tabella mesi                                          *
      *        *-------------------------------------------------------*
           05  w-tbl-mes.
               10  w-tbl-mes-001.
                   15  filler             pic  x(03) value "GEN"      .
                   15  filler             pic  x(03) value "FEB"      .
                   15  filler             pic  x(03) value "MAR"      .
                   15  filler             pic  x(03) value "APR"      .
                   15  filler             pic  x(03) value "MAG"      .
                   15  filler             pic  x(03) value "GIU"      .
                   15  filler             pic  x(03) value "LUG"      .
                   15  filler             pic  x(03) value "AGO"      .
                   15  filler             pic  x(03) value "SET"      .
                   15  filler             pic  x(03) value "OTT"      .
                   15  filler             pic  x(03) value "NOV"      .
                   15  filler             pic  x(03) value "DIC"      .
               10  w-tbl-mes-001-r redefines
                   w-tbl-mes-001.
                   15  w-tbl-mes-de1 occurs 12
                                          pic  x(03)                  .
               10  w-tbl-mes-002.
                   15  filler             pic  x(03) value "gen"      .
                   15  filler             pic  x(03) value "feb"      .
                   15  filler             pic  x(03) value "mar"      .
                   15  filler             pic  x(03) value "apr"      .
                   15  filler             pic  x(03) value "mag"      .
                   15  filler             pic  x(03) value "giu"      .
                   15  filler             pic  x(03) value "lug"      .
                   15  filler             pic  x(03) value "ago"      .
                   15  filler             pic  x(03) value "set"      .
                   15  filler             pic  x(03) value "ott"      .
                   15  filler             pic  x(03) value "nov"      .
                   15  filler             pic  x(03) value "dic"      .
               10  w-tbl-mes-002-r redefines
                   w-tbl-mes-002.
                   15  w-tbl-mes-de2 occurs 12
                                          pic  x(03)                  .
               10  w-tbl-mes-lit          pic  x(03)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det valori associati alla forma di pagamento *
      *        *-------------------------------------------------------*
           05  w-det-vas-fop.
               10  w-det-vas-fop-cod      pic  9(07)                  .
               10  w-det-vas-fop-ctr      pic  9(03)                  .
               10  w-det-vas-fop-wtp      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det valori valori associati alle spese in-   *
      *        * casso                                                 *
      *        *-------------------------------------------------------*
           05  w-det-vas-spi.
               10  w-det-vas-spi-cod      pic  x(03)                  .
               10  w-det-vas-spi-c01      pic  9(02)                  .
               10  w-det-vas-spi-c02      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det valori associati alle spese bollo        *
      *        *-------------------------------------------------------*
           05  w-det-vas-spb.
               10  w-det-vas-spb-cod      pic  x(03)                  .
               10  w-det-vas-spb-c01      pic  9(02)                  .
               10  w-det-vas-spb-c02      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det importo spese                            *
      *        *-------------------------------------------------------*
           05  w-det-imp-spe.
               10  w-det-imp-spe-ibl      pic s9(11)                  .
               10  w-det-imp-spe-ibt.
                   15  w-det-imp-spe-ibx occurs 09
                                          pic  x(01)                  .
               10  w-det-imp-spe-c01      pic  9(02)                  .
               10  w-det-imp-spe-c02      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det totale al netto delle spese in fattura   *
      *        *-------------------------------------------------------*
           05  w-det-tot-nsf.
               10  w-det-tot-nsf-ctr      pic  9(02)                  .
               10  w-det-tot-nsf-was      pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det totale spese incasso calcolate           *
      *        *-------------------------------------------------------*
           05  w-det-tot-sic.
               10  w-det-tot-sic-c01      pic  9(02)                  .
               10  w-det-tot-sic-c02      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det totale spese bollo                       *
      *        *-------------------------------------------------------*
           05  w-det-tot-spb.
               10  w-det-tot-spb-c01      pic  9(03)                  .
               10  w-det-tot-spb-c02      pic  9(03)                  .
               10  w-det-tot-spb-c03      pic  9(03)                  .
               10  w-det-tot-spb-wis      pic s9(11)                  .
               10  w-det-tot-spb-wpi      pic s9(11)                  .
               10  w-det-tot-spb-wss      pic s9(11)                  .
               10  w-det-tot-spb-wpc      pic s9(11)                  .
               10  w-det-tot-spb-s13      pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det totale spese                             *
      *        *-------------------------------------------------------*
           05  w-det-tot-spe.
               10  w-det-tot-spe-ctr      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det totale imponibile                        *
      *        *-------------------------------------------------------*
           05  w-det-tot-ibl.
               10  w-det-tot-ibl-ctr      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det totale imposta                           *
      *        *-------------------------------------------------------*
           05  w-det-tot-imp.
               10  w-det-tot-imp-ctr      pic  9(02)                  .
               10  w-det-tot-imp-wci      pic  9(05)                  .
               10  w-det-tot-imp-wpa      pic s9(11)                  .
               10  w-det-tot-imp-s11      pic s9(11)                  .
               10  w-det-tot-imp-wpr      pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det scadenze del documento                   *
      *        *-------------------------------------------------------*
           05  w-det-sca-doc.
               10  w-det-sca-doc-ctr      pic  9(03)                  .
               10  w-det-sca-doc-cli      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det prezzo eventualmente sottoposto a legame *
      *        * valutario                                             *
      *        *-------------------------------------------------------*
           05  w-det-prz-lvl.
               10  w-det-prz-lvl-prz      pic  9(09)                  .
               10  w-det-prz-lvl-svl      pic  x(03)                  .
               10  w-det-prz-lvl-ccr      pic  9(06)v9(05)            .
               10  w-det-prz-lvl-cdc      pic  9(06)v9(05)            .
               10  w-det-prz-lvl-plm      pic  9(01)v9(02)            .
               10  w-det-prz-lvl-tlm      pic  x(01)                  .
               10  w-det-prz-lvl-wps      pic  9(08)v9(05)            .
               10  w-det-prz-lvl-wpc      pic  9(06)v9(05)            .
               10  w-det-prz-lvl-wpz      pic  9(13)v9(05)            .
      *        *-------------------------------------------------------*
      *        * Work per Det prezzo netto                             *
      *        *-------------------------------------------------------*
           05  w-det-prz-net.
               10  w-det-prz-net-prz      pic  9(09)                  .
               10  w-det-prz-net-psc occurs 05
                                          pic  9(02)v9(01)            .
               10  w-det-prz-net-ctr      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Posizionamenti di stampa                              *
      *        *-------------------------------------------------------*
           05  w-det-pos-stp.
               10  w-det-pos-stp-coe      pic  9(02)v9(07)            .
      *        *-------------------------------------------------------*
      *        * Descrizione per spese in fattura e sconto in chiusura *
      *        * e sconto pagamento                                    *
      *        *-------------------------------------------------------*
           05  w-det-des-zsf.
               10  w-det-des-zsf-cod      pic  9(03)                  .
               10  w-det-des-zsf-lng      pic  x(03)                  .
               10  w-det-des-zsf-des      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione per tipi voci descrittive in fattura      *
      *        *-------------------------------------------------------*
           05  w-det-des-zdf.
               10  w-det-des-zdf-num      pic  9(01)                  .
               10  w-det-des-zdf-lng      pic  x(03)                  .
               10  w-det-des-zdf-des      pic  x(12)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione per voci descrittive in fattura           *
      *        *-------------------------------------------------------*
           05  w-det-des-zvf.
               10  w-det-des-zvf-num      pic  9(01)                  .
               10  w-det-des-zvf-cod      pic  x(03)                  .
               10  w-det-des-zvf-lng      pic  x(03)                  .
               10  w-det-des-zvf-des      pic  x(25)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione status scadenza con riferi-   *
      *        * mento ad una certa data                               *
      *        *-------------------------------------------------------*
           05  w-det-srd-sdb.
      *            *---------------------------------------------------*
      *            * Data di riferimento per la determinazione         *
      *            *---------------------------------------------------*
               10  w-det-srd-sdb-drd      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Status determinato                                *
      *            * - Spaces : Status non determinabile               *
      *            * - A      : Scadenza ancora aperta                 *
      *            * - C      : Scadenza ormai chiusa                  *
      *            * - N      : Scadenza non ancora inserita alla data *
      *            *            di riferimento per la determinazione   *
      *            *---------------------------------------------------*
               10  w-det-srd-sdb-sts      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Sigla ultima operazione eseguita sulla scadenza   *
      *            * - Spaces : Nessuna                                *
      *            * - EMI    : Emissione                              *
      *            * - SSC    : Storno                                 *
      *            * - RIS    : Riscossione                            *
      *            * - PAG    : Pagamento                              *
      *            * - CMP    : Compensazione                          *
      *            * - COM    : Composizione distinta                  *
      *            * - PRE    : Presentazione distinta                 *
      *            * - ACT    : Accettazione distinta                  *
      *            * - ACD    : Accredito distinta                     *
      *            * - RSP    : Richiamo della scadenza presentata     *
      *            * - ACS    : Accredito scadenza al dopo incasso     *
      *            * - NBE    : Notizia di buon esito sulla scadenza   *
      *            * - PBE    : Presunto buon esito sulla scadenza     *
      *            * - ISP    : Insoluto sulla scadenza presentata     *
      *            *---------------------------------------------------*
               10  w-det-srd-sdb-suo      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione status bolla cliente          *
      *        *-------------------------------------------------------*
           05  w-det-sts-bol.
      *            *---------------------------------------------------*
      *            * Flag di uscita dalla determinazione               *
      *            *---------------------------------------------------*
               10  w-det-sts-bol-flg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Contatore elementi                                *
      *            *---------------------------------------------------*
               10  w-det-sts-bol-ctr      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Tipo incidenza documento su totali                *
      *            *  - 'P' : in positivo                              *
      *            *  - 'N' : in negativo                              *
      *            *---------------------------------------------------*
               10  w-det-sts-bol-idt      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione sconto in chiusura            *
      *        *-------------------------------------------------------*
           05  w-det-tot-scc.
      *            *---------------------------------------------------*
      *            * Importo sconto in chiusura, in valuta             *
      *            *---------------------------------------------------*
               10  w-det-tot-scc-tot      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * % sconto in chiusura                              *
      *            *---------------------------------------------------*
               10  w-det-tot-scc-per      pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Work per determinazione pagamento                     *
      *        *-------------------------------------------------------*
           05  w-det-tot-scp.
      *            *---------------------------------------------------*
      *            * Importo sconto                                    *
      *            *---------------------------------------------------*
               10  w-det-tot-scp-tot      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * % sconto                                          *
      *            *---------------------------------------------------*
               10  w-det-tot-scp-per      pic  9(02)v9(01)            .
      *        *-------------------------------------------------------*
      *        * Work per determinazione importo in riga               *
      *        *-------------------------------------------------------*
           05  w-det-imp-rig.
      *            *---------------------------------------------------*
      *            * Quantita'                                         *
      *            *---------------------------------------------------*
               10  w-det-imp-rig-qta      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Prezzo netto                                      *
      *            *---------------------------------------------------*
               10  w-det-imp-rig-prz      pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Decimali prezzo                                   *
      *            *---------------------------------------------------*
               10  w-det-imp-rig-dec      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Importo                                           *
      *            *---------------------------------------------------*
               10  w-det-imp-rig-imp      pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per Det pathname file sequenziale in output      *
      *        *-------------------------------------------------------*
           05  w-det-pth-fso.
               10  w-det-pth-fso-pat      pic  x(80)                  .

      *    *===========================================================*
      *    * Work-area per operazioni sulle date                       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wnrgdat0.cpw"                   .

      *    *===========================================================*
      *    * Work per routine det-dat-ums-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-det-dat-ums.
      *        *-------------------------------------------------------*
      *        * Data base per il calcolo                      [Input] *
      *        *-------------------------------------------------------*
           05  w-det-dat-ums-dtb          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data incrementata                            [Output] *
      *        *-------------------------------------------------------*
           05  w-det-dat-ums-dti          pic  9(07)                  .

      *    *===========================================================*
      *    * Work area per le date di elaborazione                     *
      *    *-----------------------------------------------------------*
       01  w-stp-dat-inp.
      *        *-------------------------------------------------------*
      *        * Data di riferimento o in corso                        *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-rif          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Mese di riferimento o in corso                        *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-mrf          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Secolo / anno di riferimento o in corso               *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-srf          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Data di inizio mese di riferimento o in corso         *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-imr          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mese di riferimento o in corso           *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-fmr          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di inizio mesi precedenti                        *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-imp          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mesi precedenti                          *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-fmp          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di inizio mese 1                                 *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-im1          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mese 1                                   *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-fm1          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di inizio mese 2                                 *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-im2          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mese 2                                   *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-fm2          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di inizio mese 3                                 *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-im3          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mese 3                                   *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-fm3          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di inizio mese 4                                 *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-im4          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mese 4                                   *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-fm4          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di inizio mese 5                                 *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-im5          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mese 5                                   *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-fm5          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di inizio mese 6                                 *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-im6          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mese 6                                   *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-fm6          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di inizio mese 7                                 *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-im7          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mese 7                                   *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-fm7          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di inizio mese oltre                             *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-imo          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mese oltre                               *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-fmo          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di comodo per scadenze                           *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-csd          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di comodo per consegna richiesta                 *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-ric          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per rottura su codice cliente                  *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-rcc          pic  9(07)                  .

      *    *===========================================================*
      *    * Work area per stampa totali generali                      *
      *    *-----------------------------------------------------------*
       01  w-stp-tot.
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa totali                            *
      *        *-------------------------------------------------------*
           05  w-stp-tot-tot.
      *            *---------------------------------------------------*
      *            * Contatore clienti trattati                        *
      *            *---------------------------------------------------*
               10  w-stp-tot-tot-cli      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Totali relativi alle scansioni                    *
      *            *---------------------------------------------------*
               10  w-stp-tot-tot-rls.
      *                *-----------------------------------------------*
      *                * Totale generale                               *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-tot  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese in corso                          *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-mic  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese precedente 1                      *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-ms1  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese precedente 2                      *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-ms2  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese precedente 3                      *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-ms3  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese precedente 4                      *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-ms4  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese precedente 5                      *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-ms5  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese precedente 6                      *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-ms6  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese precedente 7                      *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-ms7  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale oltre                                  *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-mso  pic s9(13)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa sub-totali                        *
      *        *-------------------------------------------------------*
           05  w-stp-sub-tot.
      *            *---------------------------------------------------*
      *            * Totali relativi alle scansioni                    *
      *            *---------------------------------------------------*
               10  w-stp-sub-tot-rls.
      *                *-----------------------------------------------*
      *                * Totale generale                               *
      *                *-----------------------------------------------*
                   15  w-stp-sub-tot-tot  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese in corso                          *
      *                *-----------------------------------------------*
                   15  w-stp-sub-tot-mic  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese precedente 1                      *
      *                *-----------------------------------------------*
                   15  w-stp-sub-tot-ms1  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese precedente 2                      *
      *                *-----------------------------------------------*
                   15  w-stp-sub-tot-ms2  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese precedente 3                      *
      *                *-----------------------------------------------*
                   15  w-stp-sub-tot-ms3  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese precedente 4                      *
      *                *-----------------------------------------------*
                   15  w-stp-sub-tot-ms4  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese precedente 5                      *
      *                *-----------------------------------------------*
                   15  w-stp-sub-tot-ms5  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese precedente 6                      *
      *                *-----------------------------------------------*
                   15  w-stp-sub-tot-ms6  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale mese precedente 7                      *
      *                *-----------------------------------------------*
                   15  w-stp-sub-tot-ms7  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Totale oltre                                  *
      *                *-----------------------------------------------*
                   15  w-stp-sub-tot-mso  pic s9(13)                  .

      *    *===========================================================*
      *    * Work area per stampa tratteggi                            *
      *    *-----------------------------------------------------------*
       01  w-stp-ttg.
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa tratteggio fincature              *
      *        *-------------------------------------------------------*
           05  w-stp-ttg-fin.
      *            *---------------------------------------------------*
      *            * Tipo di tratteggio                                *
      *            *  - '-' : tratteggio sottile                       *
      *            *  - '=' : tratteggio doppio                        *
      *            *---------------------------------------------------*
               10  w-stp-ttg-fin-tdt      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Comodo per la stampa                              *
      *            *---------------------------------------------------*
               10  w-stp-ttg-fin-alf      pic  x(132)                 .

      *    *===========================================================*
      *    * Work area per stampa a livello cliente                    *
      *    *-----------------------------------------------------------*
       01  w-stp-lvc.
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa anagrafica cliente                *
      *        *-------------------------------------------------------*
           05  w-stp-lvc-cli.
      *            *---------------------------------------------------*
      *            * Codice cliente                                    *
      *            *---------------------------------------------------*
               10  w-stp-lvc-cli-cod      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Ragione sociale cliente                           *
      *            *---------------------------------------------------*
               10  w-stp-lvc-cli-rag      pic  x(80)                  .
      *            *---------------------------------------------------*
      *            * Indirizzo cliente                                 *
      *            *---------------------------------------------------*
               10  w-stp-lvc-cli-via      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Localita' cliente                                 *
      *            *---------------------------------------------------*
               10  w-stp-lvc-cli-loc      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Fido cliente                                      *
      *            *---------------------------------------------------*
               10  w-stp-lvc-cli-fid      pic  9(13)                  .
      *            *---------------------------------------------------*
      *            * Data fido cliente                                 *
      *            *---------------------------------------------------*
               10  w-stp-lvc-cli-dfi      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Codice forma di pagamento                         *
      *            *---------------------------------------------------*
               10  w-stp-lvc-cli-fop      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Descrizione forma di pagamento                    *
      *            *---------------------------------------------------*
               10  w-stp-lvc-cli-dfp      pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Data acquisizione cliente                         *
      *            *---------------------------------------------------*
               10  w-stp-lvc-cli-daq      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Data ultima modifica cliente                      *
      *            *---------------------------------------------------*
               10  w-stp-lvc-cli-dum      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Presenza di forme di pagamento diverse per il     *
      *            * cliente                                           *
      *            *---------------------------------------------------*
               10  w-stp-lvc-cli-fpd      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di stampa intestazione pagina da effettuare  *
      *            *---------------------------------------------------*
               10  w-stp-lvc-cli-fip      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di ordini bloccati                           *
      *            *---------------------------------------------------*
               10  w-stp-lvc-cli-for      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di consegne bloccate                         *
      *            *---------------------------------------------------*
               10  w-stp-lvc-cli-fco      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa totali cliente                    *
      *        *-------------------------------------------------------*
           05  w-stp-lvc-tot.
      *            *---------------------------------------------------*
      *            * Totali relativi alle scansioni                    *
      *            *---------------------------------------------------*
               10  w-stp-lvc-tot-rls.
      *                *-----------------------------------------------*
      *                * Cumulo totale                                 *
      *                *-----------------------------------------------*
                   15  w-stp-lvc-tot-tot  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese in corso                          *
      *                *-----------------------------------------------*
                   15  w-stp-lvc-tot-mic  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese precedente 1                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvc-tot-ms1  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese precedente 2                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvc-tot-ms2  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese precedente 3                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvc-tot-ms3  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese precedente 4                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvc-tot-ms4  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese precedente 5                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvc-tot-ms5  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese precedente 6                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvc-tot-ms6  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese precedente 7                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvc-tot-ms7  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo oltre                                  *
      *                *-----------------------------------------------*
                   15  w-stp-lvc-tot-mso  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Comodo per calcolo %                          *
      *                *-----------------------------------------------*
                   15  w-stp-lvc-tot-cmp  pic s9(13)v9(05)            .

      *    *===========================================================*
      *    * Work area per stampa di un valore                         *
      *    *-----------------------------------------------------------*
       01  w-stp-val.
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa di un valore numerico             *
      *        *-------------------------------------------------------*
           05  w-stp-val-num.
      *            *---------------------------------------------------*
      *            * Posizione per la stampa                           *
      *            *---------------------------------------------------*
               10  w-stp-val-num-pos      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Colonna di stampa                                 *
      *            *---------------------------------------------------*
               10  w-stp-val-num-col      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Valore numerico in input                          *
      *            *---------------------------------------------------*
               10  w-stp-val-num-val      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Valore numerico da stampare                       *
      *            *---------------------------------------------------*
               10  w-stp-val-num-vns      pic s9(10)                  .
      *            *---------------------------------------------------*
      *            * Valore da stampare editato                        *
      *            *---------------------------------------------------*
               10  w-stp-val-num-edt      pic  x(09)                  .
      *            *---------------------------------------------------*
      *            * Eventuale riempitivo                              *
      *            *---------------------------------------------------*
               10  w-stp-val-num-rmp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Divisore                                          *
      *            *  - '01' = 100                                     *
      *            *  - '02' = 1000                                    *
      *            *  - '03' = 1000000                                 *
      *            *---------------------------------------------------*
               10  w-stp-val-num-div      pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Cal                               *
      *    *-----------------------------------------------------------*
       01  w-cal.
      *        *-------------------------------------------------------*
      *        * Work per Cal importo scontato                         *
      *        *-------------------------------------------------------*
           05  w-cal-imp-sco.
               10  w-cal-imp-sco-iml      pic s9(11)                  .
               10  w-cal-imp-sco-psc      pic  9(02)v9(01)            .
               10  w-cal-imp-sco-w01      pic  9(03)v9(01)            .
               10  w-cal-imp-sco-w02      pic s9(14)v9(01)            .
               10  w-cal-imp-sco-imn      pic s9(11)                  .
               10  w-cal-imp-sco-ams      pic s9(11)                  .

      *    *===========================================================*
      *    * Work-area per conversioni rispetto alla valuta base       *
      *    *-----------------------------------------------------------*
       01  w-cvs-vlt.
      *        *-------------------------------------------------------*
      *        * Sigla dell'altra valuta                               *
      *        *-------------------------------------------------------*
           05  w-cvs-vlt-sgl              pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Numero decimali dell'altra valuta                     *
      *        *-------------------------------------------------------*
           05  w-cvs-vlt-dec              pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di coefficiente dell'altra valuta                *
      *        *-------------------------------------------------------*
           05  w-cvs-vlt-tdc              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Coefficiente di cambio dell'altra valuta              *
      *        *-------------------------------------------------------*
           05  w-cvs-vlt-cdc              pic  9(06)v9(05)            .
      *        *-------------------------------------------------------*
      *        * Ammontare espresso nell'altra valuta                  *
      *        *-------------------------------------------------------*
           05  w-cvs-vlt-aav              pic s9(17)                  .
      *        *-------------------------------------------------------*
      *        * Ammontare espresso in valuta base                     *
      *        *-------------------------------------------------------*
           05  w-cvs-vlt-avb              pic s9(17)                  .

      *    *===========================================================*
      *    * Work area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-prz-ven              pic  9(09)                  .

      *    *===========================================================*
      *    * Work per subroutines di Ctl                               *
      *    *-----------------------------------------------------------*
       01  w-ctl.
      *        *-------------------------------------------------------*
      *        * Work per Ctl su tipo riga                             *
      *        *-------------------------------------------------------*
           05  w-ctl-tip-rig.
               10  w-ctl-tip-rig-flg      pic  x(01)                  .
               10  w-ctl-tip-rig-ast      pic  x(01)                  .
               10  w-ctl-tip-rig-tfu      pic  x(01)                  .
               10  w-ctl-tip-rig-tpr      pic  x(01)                  .
               10  w-ctl-tip-rig-tri      pic  x(05)                  .
               10  w-ctl-tip-rig-tri-r redefines
                   w-ctl-tip-rig-tri.
                   15  w-ctl-tip-rig-chr occurs 5
                                          pic  x(01)                  .
               10  w-ctl-tip-rig-cod      pic  9(03)                  .
               10  w-ctl-tip-rig-cod-r redefines
                   w-ctl-tip-rig-cod.
                   15  w-ctl-tip-rig-num occurs 3
                                          pic  x(01)                  .
               10  w-ctl-tip-rig-c01      pic  9(02)                  .
               10  w-ctl-tip-rig-c02      pic  9(02)                  .

      *    *===========================================================*
      *    * Work area per totali documento                            *
      *    *-----------------------------------------------------------*
       01  w-tot.
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Coefficiente di cambio valuta     *
      *        * per fatturazione                                      *
      *        *-------------------------------------------------------*
           05  w-tot-cdc-vpf              pic  9(06)v9(05)            .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Forma di pagamento                *
      *        *-------------------------------------------------------*
           05  w-tot-ass-fop.
               10  w-tot-cpg-ass occurs 3.
                   15  w-tot-cod-pag      pic  9(07)                  .
                   15  w-tot-tip-amm      pic  9(02)                  .
                   15  w-tot-per-toi      pic  9(02)v9(01)            .
                   15  w-tot-dim-act      pic  9(02)                  .
                   15  w-tot-tip-pag      pic  9(02)                  .
                   15  w-tot-num-sca      pic  9(02)                  .
                   15  w-tot-dec-prs      pic  9(02)                  .
                   15  w-tot-dap-mes      pic  9(02)                  .
                   15  w-tot-dap-gio      pic  9(02)                  .
                   15  w-tot-ggg-int      pic  9(02)                  .
                   15  w-tot-tip-scm      pic  9(02)                  .
                   15  w-tot-gio-scm      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Sconto in chiusura                *
      *        *-------------------------------------------------------*
           05  w-tot-civ-scc              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Sconto pagamento                  *
      *        *-------------------------------------------------------*
           05  w-tot-civ-scp              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Spese in fattura                  *
      *        *-------------------------------------------------------*
           05  w-tot-spe     occurs 06                                .
               10  w-tot-spe-civ          pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Spese incasso                     *
      *        *-------------------------------------------------------*
           05  w-tot-ass-spi.
               10  w-tot-civ-spi          pic  9(05)                  .
               10  w-tot-eit-spi          pic  9(01)                  .
               10  w-tot-tbl-spi occurs 3                             .
                   15  w-tot-tpg-spi      pic  9(02)                  .
                   15  w-tot-tfu-spi      pic  9(01)                  .
                   15  w-tot-amm-spi      pic  9(07)                  .
                   15  w-tot-per-spi      pic  9(02)v9(01)            .
               10  w-tot-tot-sic          pic s9(09)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Spese bollo                       *
      *        *-------------------------------------------------------*
           05  w-tot-ass-spb.
               10  w-tot-civ-spb          pic  9(05)                  .
               10  w-tot-eit-spb          pic  9(01)                  .
               10  w-tot-tbl-spb occurs 3                             .
                   15  w-tot-tpg-spb      pic  9(02)                  .
                   15  w-tot-tau-spb      pic  9(02)                  .
                   15  w-tot-per-spb      pic  9(02)v9(01)            .
                   15  w-tot-tbs-spb.
                       20  w-tot-tbe-spb occurs 10.
                           25  w-tot-tbe-scg
                                          pic  9(11)                  .
                           25  w-tot-tbe-asc
                                          pic  9(11)                  .
                           25  w-tot-tbe-psc
                                          pic  9(02)v9(01)            .
                   15  w-tot-tet-spb      pic  9(11)                  .
                   15  w-tot-min-spb      pic  9(11)                  .
                   15  w-tot-max-spb      pic  9(11)                  .
                   15  w-tot-tar-spb      pic  9(02)                  .
                   15  w-tot-var-spb      pic  9(07)                  .
               10  w-tot-tot-spb          pic s9(09)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Totali documento                  *
      *        *-------------------------------------------------------*
           05  w-tot-tot-rta              pic s9(11)                  .
           05  w-tot-tot-rig occurs 09    pic s9(11)                  .
           05  w-tot-tot-lor              pic s9(11)                  .
           05  w-tot-tot-nsc              pic s9(11)                  .
           05  w-tot-tot-nsf              pic s9(11)                  .
           05  w-tot-tot-spe              pic s9(11)                  .
           05  w-tot-tot-net              pic s9(11)                  .
           05  w-tot-tot-ibl              pic s9(11)                  .
           05  w-tot-tot-imp              pic s9(11)                  .
           05  w-tot-tot-doc              pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Castelletto iva                   *
      *        *-------------------------------------------------------*
           05  w-tot-iva-cst.
               10  w-tot-iva-ele          pic  9(02)                  .
               10  w-tot-iva-rig occurs 12.
                   15  w-tot-iva-cod      pic  9(05)                  .
                   15  w-tot-iva-ibl      pic s9(11)                  .
                   15  w-tot-iva-imp      pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Valori relativi a : Castelletto scadenze              *
      *        *-------------------------------------------------------*
           05  w-tot-scd-cst.
               10  w-tot-scd-ele          pic  9(02)                  .
               10  w-tot-scd-tbl occurs 96.
                   15  w-tot-scd-tip      pic  9(02)                  .
                   15  w-tot-scd-dat      pic  9(07)                  .
                   15  w-tot-scd-cau      pic  9(03)                  .
                   15  w-tot-scd-imp      pic s9(11)                  .

      *    *===========================================================*
      *    * Work per subroutines di Agg                               *
      *    *-----------------------------------------------------------*
       01  w-agg.
      *        *-------------------------------------------------------*
      *        * Work per Agg castelletto iva                          *
      *        *-------------------------------------------------------*
           05  w-agg-cst-iva.
               10  w-agg-cst-iva-tip      pic  x(01)                  .
               10  w-agg-cst-iva-coi      pic  9(05)                  .
               10  w-agg-cst-iva-imp      pic s9(11)                  .
               10  w-agg-cst-iva-ctr      pic  9(02)                  .
               10  w-agg-cst-iva-tot      pic s9(11)                  .
               10  w-agg-cst-iva-max      pic  9(02)                  .
               10  w-agg-cst-iva-wpa      pic s9(11)                  .
               10  w-agg-cst-iva-s18      pic s9(18)                  .
               10  w-agg-cst-iva-s11      pic s9(11)                  .
      *        *-------------------------------------------------------*
      *        * Work per Agg record [cec]                             *
      *        *-------------------------------------------------------*
           05  w-agg-rec-cec.
      *            *---------------------------------------------------*
      *            * Anno di esercizio                                 *
      *            *---------------------------------------------------*
               10  w-agg-rec-cec-saa      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Tipo record                                       *
      *            *---------------------------------------------------*
               10  w-agg-rec-cec-tip      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente                                    *
      *            *---------------------------------------------------*
               10  w-agg-rec-cec-cli      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Mese di competenza                                *
      *            *---------------------------------------------------*
               10  w-agg-rec-cec-mes      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Progressivo mese di competenza                    *
      *            *---------------------------------------------------*
               10  w-agg-rec-cec-prg      pic s9(13)                  .

      *    *===========================================================*
      *    * Work per subroutines di Compattamento                     *
      *    *-----------------------------------------------------------*
       01  w-cmp.
      *        *-------------------------------------------------------*
      *        * Work per Compattamento castelletto iva                *
      *        *-------------------------------------------------------*
           05  w-cmp-cst-iva.
               10  w-cmp-cst-iva-ctr      pic  9(02)                  .
               10  w-cmp-cst-iva-num      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione righe ordine cliente        *
      *    *-----------------------------------------------------------*
       01  w-roc.
      *        *-------------------------------------------------------*
      *        * Numero massimo di elementi                            *
      *        *-------------------------------------------------------*
           05  w-roc-max-ele              pic  9(05)     value 999    .
      *        *-------------------------------------------------------*
      *        * Contatore elementi                                    *
      *        *-------------------------------------------------------*
           05  w-roc-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-roc-ctr-ele              pic  9(05)                  .
           05  w-roc-ctr-001              pic  9(05)                  .
           05  w-roc-ctr-002              pic  9(05)                  .
           05  w-roc-ctr-003              pic  9(05)                  .
           05  w-roc-ctr-004              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Protocollo                                            *
      *        *-------------------------------------------------------*
           05  w-roc-num-prt              pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio chiave di ordinamento          *
      *        *-------------------------------------------------------*
           05  w-roc-sav-key              pic  x(12)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per rottura su data consegna richiesta         *
      *        *-------------------------------------------------------*
           05  w-roc-rot-ric              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Castelletto progressivi riga salvati                  *
      *        *-------------------------------------------------------*
           05  w-roc-cst-rig.
      *            *---------------------------------------------------*
      *            * Singolo elemento                                  *
      *            *---------------------------------------------------*
               10  w-roc-sng-ele  occurs 999.
      *                *-----------------------------------------------*
      *                * Chiave di ordinamento                         *
      *                *-----------------------------------------------*
                   15  w-roc-key-ord.
      *                    *-------------------------------------------*
      *                    * Data consegna richiesta                   *
      *                    *-------------------------------------------*
                       20  w-roc-key-ric  pic  9(07)                  .
      *                    *-------------------------------------------*
      *                    * Numero progressivo riga                   *
      *                    *-------------------------------------------*
                       20  w-roc-key-prg  pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Dati del buffer                               *
      *                *-----------------------------------------------*
                   15  w-roc-dti-buf.
      *                    *-------------------------------------------*
      *                    * Quantita' da evadere                      *
      *                    *-------------------------------------------*
                       20  w-roc-dti-qde  pic s9(08)v9(03)            .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *    *===========================================================*
      *    * Link-area per accettazione coefficiente cambio valuta     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acl"                   .

      *    *===========================================================*
      *    * Work per calcolo tabella scadenze                         *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dtblscd0.dtw"                   .

      *    *===========================================================*
      *    * Work area per controllo rotture di livello                *
      *    *-----------------------------------------------------------*
       01  w-rot.
      *        *-------------------------------------------------------*
      *        * 5. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l05.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 4. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l04.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 3. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l03.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 2. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l02.
               10  filler                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * 1. livello di rottura                                 *
      *        *-------------------------------------------------------*
           05  w-rot-l01.
               10  filler                 pic  x(01)                  .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Dichiarazione di inizio programma               *
      *              *-------------------------------------------------*
           perform   dic-ini-pgm-000      thru dic-ini-pgm-999        .
           if        w-cnt-dic-ini-pgm    not  = spaces
                     go to main-999.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento                 *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Lettura record richieste                        *
      *              *-------------------------------------------------*
           perform   let-rec-ric-000      thru let-rec-ric-999        .
           if        w-cnt-let-rec-ric    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Lettura parametri di selezione stampa           *
      *              *-------------------------------------------------*
           perform   let-sel-stp-000      thru let-sel-stp-999        .
           if        w-cnt-let-sel-stp    not  = spaces
                     go to main-900.
       main-300.
      *              *-------------------------------------------------*
      *              * Open files per routine di stampa                *
      *              *-------------------------------------------------*
           perform   prn-opn-fls-000      thru prn-opn-fls-999        .
      *              *-------------------------------------------------*
      *              * Esecuzione eventuale sort preliminare           *
      *              *-------------------------------------------------*
           perform   exe-rou-srt-000      thru exe-rou-srt-999        .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se sort eseguito oppure no *
      *              *-------------------------------------------------*
           if        w-cnt-exe-rou-srt    =    spaces
                     go to main-400
           else      go to main-500.
       main-400.
      *              *-------------------------------------------------*
      *              * Se sort non eseguito                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di report-program                     *
      *                  *---------------------------------------------*
           perform   prn-rou-pri-000      thru prn-rou-pri-999        .
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     main-600.
       main-500.
      *              *-------------------------------------------------*
      *              * Se sort eseguito                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Continuazione                               *
      *                  *---------------------------------------------*
           go to     main-600.
       main-600.
      *              *-------------------------------------------------*
      *              * Close files per routine di stampa               *
      *              *-------------------------------------------------*
           perform   prn-cls-fls-000      thru prn-cls-fls-999        .
      *              *-------------------------------------------------*
      *              * Cancellazione modulo stampa                     *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mprint"                         .
       main-900.
      *              *-------------------------------------------------*
      *              * Dichiarazione di fine programma                 *
      *              *-------------------------------------------------*
           perform   dic-fin-pgm-000      thru dic-fin-pgm-999        .
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P+"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sistema applicativo                             *
      *              *-------------------------------------------------*
           move      i-ide-sap            to   s-sap                  .
      *              *-------------------------------------------------*
      *              * Area gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-arg            to   s-arg                  .
      *              *-------------------------------------------------*
      *              * Settore gestionale                              *
      *              *-------------------------------------------------*
           move      i-ide-set            to   s-set                  .
      *              *-------------------------------------------------*
      *              * Fase gestionale                                 *
      *              *-------------------------------------------------*
           move      i-ide-fas            to   s-fas                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Flag di save video                              *
      *              *-------------------------------------------------*
           move      "N"                  to   s-svv                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Controllo esito richiamo modulo                 *
      *              *-------------------------------------------------*
           if        s-liv                =    zero
                     move  "#"            to   w-cnt-dic-ini-pgm
           else      move  spaces         to   w-cnt-dic-ini-pgm      .
       dic-ini-pgm-999.
           exit.

      *    *===========================================================*
      *    * Dichiarazione di fine programma                           *
      *    *-----------------------------------------------------------*
       dic-fin-pgm-000.
      *              *-------------------------------------------------*
      *              * Sigla funzione                                  *
      *              *-------------------------------------------------*
           move      "P-"                 to   s-ope                  .
      *              *-------------------------------------------------*
      *              * Sigla interna del programma                     *
      *              *-------------------------------------------------*
           move      i-ide-pro            to   s-pro                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       dic-fin-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura record richieste                                  *
      *    *-----------------------------------------------------------*
       let-rec-ric-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-let-rec-ric      .
      *              *-------------------------------------------------*
      *              * Test se programma senza richieste               *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to let-rec-ric-999.
      *              *-------------------------------------------------*
      *              * Richiesta tipo funzionamento a segreteria       *
      *              *-------------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Inizio lettura record richieste                 *
      *              *-------------------------------------------------*
           move      "OI"                 to   b-ope                  .
           move      s-fun                to   b-tfe                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita                          *
      *                  *---------------------------------------------*
           if        b-rsc                not  = spaces
                     move  "#"            to   w-cnt-let-rec-ric
                     go to let-rec-ric-900.
      *              *-------------------------------------------------*
      *              * Estrazione segmenti da 255  bytes da record ri- *
      *              * chieste                                         *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stu-pnt-stu      .
       let-rec-ric-100.
           move      "GT"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
           if        b-rsc                not  = spaces
                     go to let-rec-ric-200.
           move      w-cnt-stu-pnt-stu    to   w-cnt-stu-sav-pnt      .
           string    b-chr
                     delimited by size    into rr
                                  with pointer w-cnt-stu-pnt-stu      .
           if        w-cnt-stu-pnt-stu    not  = w-cnt-stu-sav-pnt
                     go to let-rec-ric-100.
       let-rec-ric-200.
      *              *-------------------------------------------------*
      *              * Fine lettura record richieste                   *
      *              *-------------------------------------------------*
           move      "CL"                 to   b-ope                  .
           call      "swd/mod/prg/obj/mbckgr"
                                         using b                      .
      *                  *---------------------------------------------*
      *                  * Se errori : uscita                          *
      *                  *---------------------------------------------*
           if        b-rsc                not  = spaces
                     move  "#"            to   w-cnt-let-rec-ric      .
       let-rec-ric-900.
      *              *-------------------------------------------------*
      *              * Cancel modulo trattamento richieste             *
      *              *-------------------------------------------------*
           cancel    "swd/mod/prg/obj/mbckgr"                         .
       let-rec-ric-999.
           exit.

      *    *===========================================================*
      *    * Lettura parametri di selezione stampa da segreteria       *
      *    *-----------------------------------------------------------*
       let-sel-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-let-sel-stp      .
      *              *-------------------------------------------------*
      *              * Test se programma senza stampa                  *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to let-sel-stp-999.
      *              *-------------------------------------------------*
      *              * Inizializzazione area parametri stampa          *
      *              *-------------------------------------------------*
           move      spaces               to   p-sel                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero progressivo segmento    *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stu-num-seg      .
       let-sel-stp-100.
      *              *-------------------------------------------------*
      *              * Incremento numero progressivo segmento          *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-stu-num-seg      .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria per l'estra-  *
      *              * zione del segmento di parametri stampa          *
      *              *-------------------------------------------------*
           move      "S<"                 to   s-ope                  .
           move      w-cnt-stu-num-seg    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Concatenazione del segmento in area parametri   *
      *              * di stampa selezionati                           *
      *              *-------------------------------------------------*
           move      w-cnt-stu-num-seg    to   w-cnt-stu-pnt-stu      .
           multiply  80                   by   w-cnt-stu-pnt-stu      .
           subtract  79                   from w-cnt-stu-pnt-stu      .
           move      w-cnt-stu-pnt-stu    to   w-cnt-stu-sav-pnt      .
           string    s-alf
                     delimited by size    into p-sel
                                  with pointer w-cnt-stu-pnt-stu      .
      *              *-------------------------------------------------*
      *              * Se non si e' alla fine del record si ricicla    *
      *              *-------------------------------------------------*
           if        w-cnt-stu-pnt-stu    not  = w-cnt-stu-sav-pnt
                     go to let-sel-stp-100.
       let-sel-stp-999.
           exit.

      *    *===========================================================*
      *    * Ciclo di report-program                                   *
      *    *-----------------------------------------------------------*
       prn-rou-pri-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione markers                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-mrk-uno      .
           move      spaces               to   w-cnt-prn-mrk-beg      .
      *              *-------------------------------------------------*
      *              * Inizializzazione area per rotture di livello    *
      *              *-------------------------------------------------*
           move      spaces               to   w-rot                  .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di interruzione forzata   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Start iniziale                                  *
      *              *-------------------------------------------------*
           perform   prn-str-ini-000      thru prn-str-ini-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-600.
       prn-rou-pri-100.
      *              *-------------------------------------------------*
      *              * Salvataggio area rottura in area precedente     *
      *              *-------------------------------------------------*
           move      w-rot-l05            to   w-cnt-prn-sav-l05      .
           move      w-rot-l04            to   w-cnt-prn-sav-l04      .
           move      w-rot-l03            to   w-cnt-prn-sav-l03      .
           move      w-rot-l02            to   w-cnt-prn-sav-l02      .
           move      w-rot-l01            to   w-cnt-prn-sav-l01      .
       prn-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale                             *
      *              *-------------------------------------------------*
           perform   prn-let-seq-000      thru prn-let-seq-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se superamento limiti massimi              *
      *              *-------------------------------------------------*
           perform   prn-tst-max-000      thru prn-tst-max-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Selezione su record letto                       *
      *              *-------------------------------------------------*
           perform   prn-sel-rec-000      thru prn-sel-rec-999        .
           if        w-cnt-prn-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-prn-flg-sub
                     go to  prn-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Composizione area per tests di rottura          *
      *              *-------------------------------------------------*
           perform   prn-cmp-rot-000      thru prn-cmp-rot-999        .
      *              *-------------------------------------------------*
      *              * Test se primo passaggio                         *
      *              *-------------------------------------------------*
           if        w-cnt-prn-mrk-uno    not  = spaces
                     go to prn-rou-pri-300.
      *                  *---------------------------------------------*
      *                  * Test se programma senza stampa              *
      *                  *---------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to prn-rou-pri-250.
      *                      *-----------------------------------------*
      *                      * Begin                                   *
      *                      *-----------------------------------------*
           move      "BE"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Se errori                           *
      *                          *-------------------------------------*
           if        p-rsc                not  = spaces
                     go to prn-rou-pri-900.
      *                      *-----------------------------------------*
      *                      * Marker di Begin eseguito                *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-prn-mrk-beg      .
       prn-rou-pri-250.
      *                  *---------------------------------------------*
      *                  * Inizio di tutti i livelli                   *
      *                  *---------------------------------------------*
           perform   prn-rou-pri-790      thru prn-rou-pri-791        .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-300.
      *              *-------------------------------------------------*
      *              * Se rottura del 5. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l05            =    w-cnt-prn-sav-l05
                     go to prn-rou-pri-310.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l05    to   w-rot-l05              .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-750      thru prn-rou-pri-751        .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-310.
      *              *-------------------------------------------------*
      *              * Se rottura del 4. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l04            =    w-cnt-prn-sav-l04
                     go to prn-rou-pri-320.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l04    to   w-rot-l04              .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-740      thru prn-rou-pri-741        .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-320.
      *              *-------------------------------------------------*
      *              * Se rottura del 3. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l03            =    w-cnt-prn-sav-l03
                     go to prn-rou-pri-330.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l03    to   w-rot-l03              .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-730      thru prn-rou-pri-731        .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-330.
      *              *-------------------------------------------------*
      *              * Se rottura del 2. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l02            =    w-cnt-prn-sav-l02
                     go to prn-rou-pri-340.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l02    to   w-rot-l02              .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-720      thru prn-rou-pri-721        .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-340.
      *              *-------------------------------------------------*
      *              * Se rottura del 1. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l01            =    w-cnt-prn-sav-l01
                     go to prn-rou-pri-400.
           move      w-rot                to   w-cnt-prn-sav-rot      .
           move      w-cnt-prn-sav-l01    to   w-rot-l01              .
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           move      w-cnt-prn-sav-rot    to   w-rot                  .
           perform   prn-rou-pri-710      thru prn-rou-pri-711        .
           go to     prn-rou-pri-400.
       prn-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Esecuzione per il livello di dettaglio          *
      *              *-------------------------------------------------*
           perform   prn-liv-det-000      thru prn-liv-det-999        .
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Segnale di passaggio successivo al primo        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-prn-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura sequenziale file principale   *
      *              *-------------------------------------------------*
           go to     prn-rou-pri-100.
       prn-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se almeno un passaggio                     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-mrk-uno    =    spaces
                     go to prn-rou-pri-600.
           perform   prn-rou-pri-810      thru prn-rou-pri-811        .
           perform   prn-rou-pri-820      thru prn-rou-pri-821        .
           perform   prn-rou-pri-830      thru prn-rou-pri-831        .
           perform   prn-rou-pri-840      thru prn-rou-pri-841        .
           perform   prn-rou-pri-850      thru prn-rou-pri-851        .
           perform   prn-rou-pri-890      thru prn-rou-pri-891        .
           go to     prn-rou-pri-900.
       prn-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Esecuzione per nessuna registrazione da elab.   *
      *              *-------------------------------------------------*
           perform   prn-nes-ela-000      thru prn-nes-ela-999        .
           go to     prn-rou-pri-900.
       prn-rou-pri-710.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 1. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-711.
           perform   prn-ini-lr1-000      thru prn-ini-lr1-999        .
       prn-rou-pri-711.
           exit.
       prn-rou-pri-720.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 2. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-721.
           perform   prn-ini-lr2-000      thru prn-ini-lr2-999        .
       prn-rou-pri-721.
           exit.
       prn-rou-pri-730.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 3. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-731.
           perform   prn-ini-lr3-000      thru prn-ini-lr3-999        .
       prn-rou-pri-731.
           exit.
       prn-rou-pri-740.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 4. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-741.
           perform   prn-ini-lr4-000      thru prn-ini-lr4-999        .
       prn-rou-pri-741.
           exit.
       prn-rou-pri-750.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 5. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-751.
           perform   prn-ini-lr5-000      thru prn-ini-lr5-999        .
       prn-rou-pri-751.
           exit.
       prn-rou-pri-790.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio ciclo                     *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-791.
           perform   prn-ini-cic-000      thru prn-ini-cic-999        .
       prn-rou-pri-791.
           exit.
       prn-rou-pri-810.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 1. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-811.
           perform   prn-fin-lr1-000      thru prn-fin-lr1-999        .
       prn-rou-pri-811.
           exit.
       prn-rou-pri-820.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 2. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-821.
           perform   prn-fin-lr2-000      thru prn-fin-lr2-999        .
       prn-rou-pri-821.
           exit.
       prn-rou-pri-830.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 3. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-831.
           perform   prn-fin-lr3-000      thru prn-fin-lr3-999        .
       prn-rou-pri-831.
           exit.
       prn-rou-pri-840.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 4. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-841.
           perform   prn-fin-lr4-000      thru prn-fin-lr4-999        .
       prn-rou-pri-841.
           exit.
       prn-rou-pri-850.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 5. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-851.
           perform   prn-fin-lr5-000      thru prn-fin-lr5-999        .
       prn-rou-pri-851.
           exit.
       prn-rou-pri-890.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine ciclo                       *
      *              *-------------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-rou-pri-891.
           perform   prn-fin-cic-000      thru prn-fin-cic-999        .
       prn-rou-pri-891.
           exit.
       prn-rou-pri-900.
      *              *-------------------------------------------------*
      *              * End                                             *
      *              *-------------------------------------------------*
           move      "EN"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           if        p-rsc                not  = spaces
                     go to prn-rou-pri-999.
       prn-rou-pri-999.
           exit.

      *    *===========================================================*
      *    * Intestazione pagina standard                              *
      *    *-----------------------------------------------------------*
       int-pag-std-000.
      *              *-------------------------------------------------*
      *              * Elaborazioni preliminari su aree titolo         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area nome azienda  *
      *                  *---------------------------------------------*
           move      "IA"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-asx                to   w-cnt-tit-des-azi      .
           move      40                   to   w-cnt-tit-ctr-azi      .
       int-pag-std-100.
           if        w-cnt-tit-chr-azi
                    (w-cnt-tit-ctr-azi)   =    spaces
                     if     w-cnt-tit-ctr-azi
                                          >    1
                            subtract  1   from w-cnt-tit-ctr-azi
                            go to     int-pag-std-100.
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza titolo stampato    *
      *                  *---------------------------------------------*
           move      80                   to   w-cnt-tit-ctr-tit      .
       int-pag-std-200.
           if        w-cnt-tit-chr-tit
                    (w-cnt-tit-ctr-tit)   =    spaces
                     if     w-cnt-tit-ctr-tit
                                          >    1
                            subtract  1   from w-cnt-tit-ctr-tit
                            go to     int-pag-std-200.
      *                  *---------------------------------------------*
      *                  * Determinazione posizione iniziale titolo    *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-cnt-tit-pos-tit      .
           subtract  w-cnt-tit-ctr-tit    from w-cnt-tit-pos-tit      .
           divide    2                    into w-cnt-tit-pos-tit      .
           add       1                    to   w-cnt-tit-pos-tit      .
      *                  *---------------------------------------------*
      *                  * Determinazione lunghezza area data e pagina *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-pag    =    zero
                     move  4              to   w-cnt-tit-ctr-wrk
                     go to int-pag-std-300.
           move      zero                 to   w-cnt-tit-ctr-wrk      .
           inspect   w-cnt-tit-num-pag
                                      tallying w-cnt-tit-ctr-wrk
                                   for leading "0"                    .
       int-pag-std-300.
           subtract  w-cnt-tit-ctr-wrk    from 27
                                        giving w-cnt-tit-ctr-dep      .
           subtract  w-cnt-tit-ctr-wrk    from 5
                                        giving w-cnt-tit-ctr-cif      .
      *                  *---------------------------------------------*
      *                  * Determinazione posizione iniziale data e p. *
      *                  *---------------------------------------------*
           move      p-sel-als-sel        to   w-cnt-tit-pos-dep      .
           subtract  w-cnt-tit-ctr-dep    from w-cnt-tit-pos-dep      .
           add       1                    to   w-cnt-tit-pos-dep      .
      *                  *---------------------------------------------*
      *                  * Determinazione se titolo su una o due linee *
      *                  *---------------------------------------------*
           move      w-cnt-tit-ctr-azi    to   w-cnt-tit-ctr-wrk      .
           add       2                    to   w-cnt-tit-ctr-wrk      .
           if        w-cnt-tit-ctr-wrk    not  < w-cnt-tit-pos-tit
                     move  2              to   w-cnt-tit-num-lin
                     go to int-pag-std-500.
           move      w-cnt-tit-pos-tit    to   w-cnt-tit-ctr-wrk      .
           add       w-cnt-tit-ctr-tit    to   w-cnt-tit-ctr-wrk      .
           add       1                    to   w-cnt-tit-ctr-wrk      .
           if        w-cnt-tit-ctr-wrk    not  < w-cnt-tit-pos-dep
                     move  2              to   w-cnt-tit-num-lin
                     go to int-pag-std-500.
           move      1                    to   w-cnt-tit-num-lin      .
       int-pag-std-500.
      *              *-------------------------------------------------*
      *              * Avanzamento pagina                              *
      *              *-------------------------------------------------*
           move      "PA"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Se errore grave di i-o su stampa si esce    *
      *                  * con status di errore                        *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  int-pag-std-999.
      *              *-------------------------------------------------*
      *              * Linea di '=' iniziale                           *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "="            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Prima linea titolo                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Nome azienda                                *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-azi    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-cnt-tit-des-azi    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Titolo stampato                             *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-lin    =    2
                     go to int-pag-std-600.
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-tit    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-tit    to   p-pos                  .
           move      w-cnt-tit-des-tit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-600.
      *                  *---------------------------------------------*
      *                  * Literal per Data                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           move      "Data :"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Data                                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       7                    to   p-pos                  .
           move      w-cnt-tit-dat-stp    to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal per Pag.                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       17                   to   p-pos                  .
           move      "Pag."               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Numero pagina                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      w-cnt-tit-ctr-cif    to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-dep    to   p-pos                  .
           add       22                   to   p-pos                  .
           move      w-cnt-tit-num-pag    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Seconda linea titolo                            *
      *              *-------------------------------------------------*
           if        w-cnt-tit-num-lin    not  = 2
                     go to int-pag-std-900.
      *                  *---------------------------------------------*
      *                  * Titolo stampato                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-cnt-tit-ctr-tit    to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-cnt-tit-pos-tit    to   p-pos                  .
           move      w-cnt-tit-des-tit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-900.
      *              *-------------------------------------------------*
      *              * Linea di '-' finale                             *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      p-sel-als-sel        to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      all   "-"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           add       2
                     p-lnr              giving p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-std-999.
           exit.

      *    *===========================================================*
      *    * Routine pre-esecuzione programma                          *
      *    *-----------------------------------------------------------*
       pre-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No record richieste                          *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
      *              *-------------------------------------------------*
      *              * Si/No stampa                                    *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-stp      .
       pre-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Open files                         *
      *    *-----------------------------------------------------------*
       prn-opn-fls-000.
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * [dcc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * [ccc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofccc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ccc                 .
      *              *-------------------------------------------------*
      *              * [sdb]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *              *-------------------------------------------------*
      *              * [ddp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
       prn-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Close files                        *
      *    *-----------------------------------------------------------*
       prn-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [cli]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *              *-------------------------------------------------*
      *              * [dcc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * [ccc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofccc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ccc                 .
      *              *-------------------------------------------------*
      *              * [sdb]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *              *-------------------------------------------------*
      *              * [ddp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
       prn-cls-fls-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-cls-fls-999.
       prn-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Routine di sort preliminare                               *
      *    *-----------------------------------------------------------*
       exe-rou-srt-000.
      *              *-------------------------------------------------*
      *              * Flag di sort eseguito a : Si'                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-exe-rou-srt      .
      *              *-------------------------------------------------*
      *              * Esecuzione sort                                 *
      *              *-------------------------------------------------*
           sort      srt                  on   ascending srt-key
                     input  procedure     is   stp-srt-inp-000
                                          thru stp-srt-inp-999
                     output procedure     is   prn-rou-pri-000
                                          thru prn-rou-pri-999        .
       exe-rou-srt-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *-----------------------------------------------------------*
       stp-srt-inp-000.
      *              *-------------------------------------------------*
      *              * Subroutine di preparazione date                 *
      *              *-------------------------------------------------*
           perform   stp-srt-inp-dat-000  thru stp-srt-inp-dat-999    .
      *              *-------------------------------------------------*
      *              * Preparazione elemento per rottura su cliente    *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-dat-inp-rcc      .
      *              *-------------------------------------------------*
      *              * Normalizzazione totali per rottura su cliente   *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-lvc-tot-tot      .
           move      zero                 to   w-stp-lvc-tot-mic      .
           move      zero                 to   w-stp-lvc-tot-ms1      .
           move      zero                 to   w-stp-lvc-tot-ms2      .
           move      zero                 to   w-stp-lvc-tot-ms3      .
           move      zero                 to   w-stp-lvc-tot-ms4      .
           move      zero                 to   w-stp-lvc-tot-ms5      .
           move      zero                 to   w-stp-lvc-tot-ms6      .
           move      zero                 to   w-stp-lvc-tot-ms7      .
           move      zero                 to   w-stp-lvc-tot-mso      .
       stp-srt-inp-100.
      *              *-------------------------------------------------*
      *              * Start su file [sdb]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DBTDTS    "         to   f-key                  .
           move      01                   to   rf-sdb-tip-dbt         .
           move      zero                 to   rf-sdb-cod-dbt         .
           move      zero                 to   rf-sdb-dts-sdb         .
           move      zero                 to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Se start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-srt-inp-900.
       stp-srt-inp-200.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Read Next da [sdb]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to stp-srt-inp-300.
      *                  *---------------------------------------------*
      *                  * Se fine file : a completamento rottura      *
      *                  *---------------------------------------------*
           perform   stp-srt-inp-rot-000  thru stp-srt-inp-rot-999    .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-900.
       stp-srt-inp-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo debitore                       *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-dbt       =    01
                     go to stp-srt-inp-400.
      *                  *---------------------------------------------*
      *                  * Se non superato : ad completamento rottura  *
      *                  *---------------------------------------------*
           perform   stp-srt-inp-rot-000  thru stp-srt-inp-rot-999    .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-900.
       stp-srt-inp-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice debitore                     *
      *                  *---------------------------------------------*
           if        rf-sdb-cod-dbt       =    zero
                     go to stp-srt-inp-200.
      *                  *---------------------------------------------*
      *                  * Test su data documento di riferimento       *
      *                  *---------------------------------------------*
           if        rf-sdb-dat-ddr       =    zero
                     go to stp-srt-inp-200.
      *                  *---------------------------------------------*
      *                  * Determinazione status scadenza cliente      *
      *                  *---------------------------------------------*
           move      rr-dat-rif           to   w-det-srd-sdb-drd      .
           perform   det-srd-sdb-000      thru det-srd-sdb-999        .
      *                  *---------------------------------------------*
      *                  * Tests                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Su status scadenza                      *
      *                      *-----------------------------------------*
           if        w-det-srd-sdb-sts    not  = "A"
                     go to stp-srt-inp-200.
      *                      *-----------------------------------------*
      *                      * Su sigla ultima operazione eseguita     *
      *                      * sulla scadenza                          *
      *                      *-----------------------------------------*
______*    if        w-det-srd-sdb-suo    not  = "EMI" and
______*              w-det-srd-sdb-suo    not  = "ISP"
______*              go to stp-srt-inp-200.
       stp-srt-inp-500.
      *              *-------------------------------------------------*
      *              * Rottura su codice cliente                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test preliminare                            *
      *                  *---------------------------------------------*
           if        w-stp-dat-inp-rcc    =    zero
                     go to stp-srt-inp-520.
      *                  *---------------------------------------------*
      *                  * Test di rottura                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test di rottura                         *
      *                      *-----------------------------------------*
           if        rf-sdb-cod-dbt       =    w-stp-dat-inp-rcc
                     go to stp-srt-inp-540.
       stp-srt-inp-510.
      *                  *---------------------------------------------*
      *                  * Completamento record di sort                *
      *                  *---------------------------------------------*
           perform   stp-srt-inp-rot-000  thru stp-srt-inp-rot-999    .
       stp-srt-inp-520.
      *                  *---------------------------------------------*
      *                  * Inizio rottura                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice cliente per rottura              *
      *                      *-----------------------------------------*
           move      rf-sdb-cod-dbt       to   w-stp-dat-inp-rcc      .
      *                      *-----------------------------------------*
      *                      * Totali per rottura                      *
      *                      *-----------------------------------------*
           move      zero                 to   w-stp-lvc-tot-tot      .
           move      zero                 to   w-stp-lvc-tot-mic      .
           move      zero                 to   w-stp-lvc-tot-ms1      .
           move      zero                 to   w-stp-lvc-tot-ms2      .
           move      zero                 to   w-stp-lvc-tot-ms3      .
           move      zero                 to   w-stp-lvc-tot-ms4      .
           move      zero                 to   w-stp-lvc-tot-ms5      .
           move      zero                 to   w-stp-lvc-tot-ms6      .
           move      zero                 to   w-stp-lvc-tot-ms7      .
           move      zero                 to   w-stp-lvc-tot-mso      .
       stp-srt-inp-540.
      *                  *---------------------------------------------*
      *                  * Cumulo totali per cliente                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Cumulo totale cliente                   *
      *                      *-----------------------------------------*
           add       rf-sdb-imp-sdb       to   w-stp-lvc-tot-tot      .
      *                      *-----------------------------------------*
      *                      * Data documento di riferimento in comodo *
      *                      *-----------------------------------------*
           move      rf-sdb-dat-ddr       to   w-stp-dat-inp-csd      .
       stp-srt-inp-542.
      *                      *-----------------------------------------*
      *                      * Mese in corso                           *
      *                      *-----------------------------------------*
           if        w-stp-dat-inp-csd    <    w-stp-dat-inp-imr or
                     w-stp-dat-inp-csd    >    w-stp-dat-inp-fmr
                     go to stp-srt-inp-544.
           add       rf-sdb-imp-sdb       to   w-stp-lvc-tot-mic      .
       stp-srt-inp-544.
      *                      *-----------------------------------------*
      *                      * Mese 1                                  *
      *                      *-----------------------------------------*
           if        w-stp-dat-inp-csd    <    w-stp-dat-inp-im1 or
                     w-stp-dat-inp-csd    >    w-stp-dat-inp-fm1
                     go to stp-srt-inp-546.
           add       rf-sdb-imp-sdb       to   w-stp-lvc-tot-ms1      .
       stp-srt-inp-546.
      *                      *-----------------------------------------*
      *                      * Mese 2                                  *
      *                      *-----------------------------------------*
           if        w-stp-dat-inp-csd    <    w-stp-dat-inp-im2 or
                     w-stp-dat-inp-csd    >    w-stp-dat-inp-fm2
                     go to stp-srt-inp-548.
           add       rf-sdb-imp-sdb       to   w-stp-lvc-tot-ms2      .
       stp-srt-inp-548.
      *                      *-----------------------------------------*
      *                      * Mese 3                                  *
      *                      *-----------------------------------------*
           if        w-stp-dat-inp-csd    <    w-stp-dat-inp-im3 or
                     w-stp-dat-inp-csd    >    w-stp-dat-inp-fm3
                     go to stp-srt-inp-550.
           add       rf-sdb-imp-sdb       to   w-stp-lvc-tot-ms3      .
       stp-srt-inp-550.
      *                      *-----------------------------------------*
      *                      * Mese 4                                  *
      *                      *-----------------------------------------*
           if        w-stp-dat-inp-csd    <    w-stp-dat-inp-im4 or
                     w-stp-dat-inp-csd    >    w-stp-dat-inp-fm4
                     go to stp-srt-inp-552.
           add       rf-sdb-imp-sdb       to   w-stp-lvc-tot-ms4      .
       stp-srt-inp-552.
      *                      *-----------------------------------------*
      *                      * Mese 5                                  *
      *                      *-----------------------------------------*
           if        w-stp-dat-inp-csd    <    w-stp-dat-inp-im5 or
                     w-stp-dat-inp-csd    >    w-stp-dat-inp-fm5
                     go to stp-srt-inp-554.
           add       rf-sdb-imp-sdb       to   w-stp-lvc-tot-ms5      .
       stp-srt-inp-554.
      *                      *-----------------------------------------*
      *                      * Mese 6                                  *
      *                      *-----------------------------------------*
           if        w-stp-dat-inp-csd    <    w-stp-dat-inp-im6 or
                     w-stp-dat-inp-csd    >    w-stp-dat-inp-fm6
                     go to stp-srt-inp-556.
           add       rf-sdb-imp-sdb       to   w-stp-lvc-tot-ms6      .
       stp-srt-inp-556.
      *                      *-----------------------------------------*
      *                      * Mese 7                                  *
      *                      *-----------------------------------------*
           if        w-stp-dat-inp-csd    <    w-stp-dat-inp-im7 or
                     w-stp-dat-inp-csd    >    w-stp-dat-inp-fm7
                     go to stp-srt-inp-558.
           add       rf-sdb-imp-sdb       to   w-stp-lvc-tot-ms7      .
       stp-srt-inp-558.
      *                      *-----------------------------------------*
      *                      * Mesi oltre                              *
      *                      *-----------------------------------------*
           if        w-stp-dat-inp-csd    <    w-stp-dat-inp-imo or
                     w-stp-dat-inp-csd    >    w-stp-dat-inp-fmo
                     go to stp-srt-inp-850.
           add       rf-sdb-imp-sdb       to   w-stp-lvc-tot-mso      .
       stp-srt-inp-850.
      *              *-------------------------------------------------*
      *              * Riciclo a record successivo                     *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-200.
       stp-srt-inp-900.
      *              *-------------------------------------------------*
      *              * Fine ciclo, uscita                              *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-999.
       stp-srt-inp-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *                                                           *
      *    * Subroutine per la fine rottura su codice cliente          *
      *    *-----------------------------------------------------------*
       stp-srt-inp-rot-000.
      *              *-------------------------------------------------*
      *              * Composizione record per sort                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   srt-rec                .
      *                  *---------------------------------------------*
      *                  * Chiave di sort                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * ABC cliente                             *
      *                      *-----------------------------------------*
           move      w-stp-lvc-tot-tot    to   srt-abc-cli            .
           multiply  -1                   by   srt-abc-cli            .
           add       9999999999999        to   srt-abc-cli            .
      *                  *---------------------------------------------*
      *                  * Dati di sort                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-rcc    to   srt-cod-cli            .
      *                      *-----------------------------------------*
      *                      * Totali cliente                          *
      *                      *-----------------------------------------*
           move      w-stp-lvc-tot-tot    to   srt-cum-tot            .
           move      w-stp-lvc-tot-mic    to   srt-cum-mic            .
           move      w-stp-lvc-tot-ms1    to   srt-cum-ms1            .
           move      w-stp-lvc-tot-ms2    to   srt-cum-ms2            .
           move      w-stp-lvc-tot-ms3    to   srt-cum-ms3            .
           move      w-stp-lvc-tot-ms4    to   srt-cum-ms4            .
           move      w-stp-lvc-tot-ms5    to   srt-cum-ms5            .
           move      w-stp-lvc-tot-ms6    to   srt-cum-ms6            .
           move      w-stp-lvc-tot-ms7    to   srt-cum-ms7            .
           move      w-stp-lvc-tot-mso    to   srt-cum-mso            .
       stp-srt-inp-rot-800.
      *              *-------------------------------------------------*
      *              * Rilascio del record al Sort                     *
      *              *-------------------------------------------------*
           release   srt-rec                                          .
       stp-srt-inp-rot-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-rot-999.
       stp-srt-inp-rot-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *                                                           *
      *    * Subroutine per la determinazione delle date in base alla  *
      *    * data di riferimento                                       *
      *    *-----------------------------------------------------------*
       stp-srt-inp-dat-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione date                        *
      *                  *---------------------------------------------*
           move      rr-dat-rif           to   w-stp-dat-inp-rif      .
           move      w-stp-dat-inp-rif    to   s-dat                  .
           move      s-saa                to   w-stp-dat-inp-srf      .
           move      s-mes                to   w-stp-dat-inp-mrf      .
           move      zero                 to   w-stp-dat-inp-imr      .
           move      zero                 to   w-stp-dat-inp-fmr      .
           move      zero                 to   w-stp-dat-inp-imp      .
           move      zero                 to   w-stp-dat-inp-fmp      .
           move      zero                 to   w-stp-dat-inp-im1      .
           move      zero                 to   w-stp-dat-inp-fm1      .
           move      zero                 to   w-stp-dat-inp-im2      .
           move      zero                 to   w-stp-dat-inp-fm2      .
           move      zero                 to   w-stp-dat-inp-im3      .
           move      zero                 to   w-stp-dat-inp-fm3      .
           move      zero                 to   w-stp-dat-inp-im4      .
           move      zero                 to   w-stp-dat-inp-fm4      .
           move      zero                 to   w-stp-dat-inp-im5      .
           move      zero                 to   w-stp-dat-inp-fm5      .
           move      zero                 to   w-stp-dat-inp-im6      .
           move      zero                 to   w-stp-dat-inp-fm6      .
           move      zero                 to   w-stp-dat-inp-im7      .
           move      zero                 to   w-stp-dat-inp-fm7      .
           move      zero                 to   w-stp-dat-inp-imo      .
           move      zero                 to   w-stp-dat-inp-fmo      .
       stp-srt-inp-dat-010.
      *              *-------------------------------------------------*
      *              * Calcolo della data di inizio mese di riferimen- *
      *              * to                                              *
      *              *-------------------------------------------------*
           move      w-stp-dat-inp-rif    to   w-stp-dat-inp-imr      .
           move      w-stp-dat-inp-imr    to   s-dat                  .
           move      01                   to   s-gio                  .
           move      s-dat                to   w-stp-dat-inp-imr      .
       stp-srt-inp-dat-020.
      *              *-------------------------------------------------*
      *              * Calcolo della data di fine mese di riferimento  *
      *              *-------------------------------------------------*
           move      w-stp-dat-inp-rif    to   w-stp-dat-inp-fmr      .
      *                  *---------------------------------------------*
      *                  * Fine mese                                   *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-fmr    to   s-dat                  .
           move      31                   to   s-gio                  .
       stp-srt-inp-dat-025.
           move      s-dat                to   w-stp-dat-inp-fmr      .
      *                  *---------------------------------------------*
      *                  * Controllo della data                        *
      *                  *---------------------------------------------*
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se la data e' corretta : oltre              *
      *                  *---------------------------------------------*
           if        s-sts                =    spaces
                     go to stp-srt-inp-dat-030.
           subtract  1                    from s-gio                  .
           go to     stp-srt-inp-dat-025.
       stp-srt-inp-dat-030.
      *              *-------------------------------------------------*
      *              * Calcolo delle date di inizio e fine mesi        *
      *              * precedenti                                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-dat-inp-imp      .
      *                  *---------------------------------------------*
      *                  * Determinazione data precedente all'inizio   *
      *                  * del mese di riferimento                     *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-imr    to   w-det-nrg-dat-dtb      .
           move      1                    to   w-det-nrg-dat-ngd      .
           perform   det-nrg-dat-000      thru det-nrg-dat-999        .
           move      w-det-nrg-dat-dtd    to   w-stp-dat-inp-fmp      .
       stp-srt-inp-dat-040.
      *              *-------------------------------------------------*
      *              * Calcolo delle date di inizio e fine mese 1      *
      *              * precedente                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-imr    to   w-stp-dat-inp-im1      .
           move      w-stp-dat-inp-im1    to   s-dat                  .
           subtract  1                    from s-mes                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-im1      .
      *                  *---------------------------------------------*
      *                  * Fine mese                                   *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im1    to   w-stp-dat-inp-fm1      .
           move      w-stp-dat-inp-fm1    to   s-dat                  .
           move      31                   to   s-gio                  .
       stp-srt-inp-dat-045.
           move      s-dat                to   w-stp-dat-inp-fm1      .
      *                      *-----------------------------------------*
      *                      * Controllo della data                    *
      *                      *-----------------------------------------*
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se la data e' corretta : oltre          *
      *                      *-----------------------------------------*
           if        s-sts                =    spaces
                     go to stp-srt-inp-dat-050.
           subtract  1                    from s-gio                  .
           go to     stp-srt-inp-dat-045.
       stp-srt-inp-dat-050.
      *              *-------------------------------------------------*
      *              * Calcolo delle date di inizio e fine mese 2      *
      *              * precedente                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im1    to   w-stp-dat-inp-im2      .
           move      w-stp-dat-inp-im2    to   s-dat                  .
           subtract  1                    from s-mes                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-im2      .
      *                  *---------------------------------------------*
      *                  * Fine mese                                   *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im2    to   w-stp-dat-inp-fm2      .
           move      w-stp-dat-inp-fm2    to   s-dat                  .
           move      31                   to   s-gio                  .
       stp-srt-inp-dat-055.
           move      s-dat                to   w-stp-dat-inp-fm2      .
      *                      *-----------------------------------------*
      *                      * Controllo della data                    *
      *                      *-----------------------------------------*
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se la data e' corretta : oltre          *
      *                      *-----------------------------------------*
           if        s-sts                =    spaces
                     go to stp-srt-inp-dat-060.
           subtract  1                    from s-gio                  .
           go to     stp-srt-inp-dat-055.
       stp-srt-inp-dat-060.
      *              *-------------------------------------------------*
      *              * Calcolo delle date di inizio e fine mese 3      *
      *              * precedente                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im2    to   w-stp-dat-inp-im3      .
           move      w-stp-dat-inp-im3    to   s-dat                  .
           subtract  1                    from s-mes                  .
           if        s-mes                =    zero
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-im3      .
      *                  *---------------------------------------------*
      *                  * Fine mese                                   *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im3    to   w-stp-dat-inp-fm3      .
           move      w-stp-dat-inp-fm3    to   s-dat                  .
           move      31                   to   s-gio                  .
       stp-srt-inp-dat-065.
           move      s-dat                to   w-stp-dat-inp-fm3      .
      *                      *-----------------------------------------*
      *                      * Controllo della data                    *
      *                      *-----------------------------------------*
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se la data e' corretta : oltre          *
      *                      *-----------------------------------------*
           if        s-sts                =    spaces
                     go to stp-srt-inp-dat-070.
           subtract  1                    from s-gio                  .
           go to     stp-srt-inp-dat-065.
       stp-srt-inp-dat-070.
      *              *-------------------------------------------------*
      *              * Calcolo delle date di inizio e fine mese 4      *
      *              * precedente                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im3    to   w-stp-dat-inp-im4      .
           move      w-stp-dat-inp-im4    to   s-dat                  .
           subtract  1                    from s-mes                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-im4      .
      *                  *---------------------------------------------*
      *                  * Fine mese                                   *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im4    to   w-stp-dat-inp-fm4      .
           move      w-stp-dat-inp-fm4    to   s-dat                  .
           move      31                   to   s-gio                  .
       stp-srt-inp-dat-075.
           move      s-dat                to   w-stp-dat-inp-fm4      .
      *                      *-----------------------------------------*
      *                      * Controllo della data                    *
      *                      *-----------------------------------------*
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se la data e' corretta : oltre          *
      *                      *-----------------------------------------*
           if        s-sts                =    spaces
                     go to stp-srt-inp-dat-080.
           subtract  1                    from s-gio                  .
           go to     stp-srt-inp-dat-075.
       stp-srt-inp-dat-080.
      *              *-------------------------------------------------*
      *              * Calcolo delle date di inizio e fine mese 5      *
      *              * precedente                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im4    to   w-stp-dat-inp-im5      .
           move      w-stp-dat-inp-im5    to   s-dat                  .
           subtract  1                    from s-mes                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-im5      .
      *                  *---------------------------------------------*
      *                  * Fine mese                                   *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im5    to   w-stp-dat-inp-fm5      .
           move      w-stp-dat-inp-fm5    to   s-dat                  .
           move      31                   to   s-gio                  .
       stp-srt-inp-dat-085.
           move      s-dat                to   w-stp-dat-inp-fm5      .
      *                      *-----------------------------------------*
      *                      * Controllo della data                    *
      *                      *-----------------------------------------*
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se la data e' corretta : oltre          *
      *                      *-----------------------------------------*
           if        s-sts                =    spaces
                     go to stp-srt-inp-dat-090.
           subtract  1                    from s-gio                  .
           go to     stp-srt-inp-dat-085.
       stp-srt-inp-dat-090.
      *              *-------------------------------------------------*
      *              * Calcolo delle date di inizio e fine mese 6      *
      *              * mesi 7 - 9 precedenti                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im5    to   w-stp-dat-inp-im6      .
      *                      *-----------------------------------------*
      *                      * Decremento di un mese ( -1)             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-im6    to   s-dat                  .
           subtract  1                    from s-mes                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-im6      .
      *                      *-----------------------------------------*
      *                      * Decremento di un mese ( -2)             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-im6    to   s-dat                  .
           subtract  1                    from s-mes                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-im6      .
      *                      *-----------------------------------------*
      *                      * Decremento di un mese ( -3)             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-im6    to   s-dat                  .
           subtract  1                    from s-mes                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-im6      .
      *                  *---------------------------------------------*
      *                  * Fine mese                                   *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-fm5    to   w-stp-dat-inp-fm6      .
           move      w-stp-dat-inp-fm6    to   s-dat                  .
           move      31                   to   s-gio                  .
           subtract  1                    from s-mes                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
       stp-srt-inp-dat-095.
           move      s-dat                to   w-stp-dat-inp-fm6      .
      *                      *-----------------------------------------*
      *                      * Controllo della data                    *
      *                      *-----------------------------------------*
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se la data e' corretta : oltre          *
      *                      *-----------------------------------------*
           if        s-sts                =    spaces
                     go to stp-srt-inp-dat-100.
           subtract  1                    from s-gio                  .
           go to     stp-srt-inp-dat-095.
       stp-srt-inp-dat-100.
      *              *-------------------------------------------------*
      *              * Calcolo delle date di inizio e fine mese 7      *
      *              * mesi 10 - 12 precedenti                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im6    to   w-stp-dat-inp-im7      .
      *                      *-----------------------------------------*
      *                      * Decremento di un mese ( -1)             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-im7    to   s-dat                  .
           subtract  1                    from s-mes                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-im7      .
      *                      *-----------------------------------------*
      *                      * Decremento di un mese ( -2)             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-im7    to   s-dat                  .
           subtract  1                    from s-mes                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-im7      .
      *                      *-----------------------------------------*
      *                      * Decremento di un mese ( -3)             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-im7    to   s-dat                  .
           subtract  1                    from s-mes                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-im7      .
      *                  *---------------------------------------------*
      *                  * Fine mese                                   *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-fm6    to   w-stp-dat-inp-fm7      .
      *                      *-----------------------------------------*
      *                      * Decremento di un mese ( -1)             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fm7    to   s-dat                  .
           subtract  1                    from s-mes                  .
           move      31                   to   s-gio                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-fm7      .
      *                      *-----------------------------------------*
      *                      * Decremento di un mese ( -2)             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fm7    to   s-dat                  .
           subtract  1                    from s-mes                  .
           move      31                   to   s-gio                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-fm7      .
      *                      *-----------------------------------------*
      *                      * Decremento di un mese ( -3)             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fm7    to   s-dat                  .
           subtract  1                    from s-mes                  .
           move      31                   to   s-gio                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
       stp-srt-inp-dat-105.
           move      s-dat                to   w-stp-dat-inp-fm7      .
      *                      *-----------------------------------------*
      *                      * Controllo della data                    *
      *                      *-----------------------------------------*
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se la data e' corretta : oltre          *
      *                      *-----------------------------------------*
           if        s-sts                =    spaces
                     go to stp-srt-inp-dat-200.
           subtract  1                    from s-gio                  .
           go to     stp-srt-inp-dat-105.
       stp-srt-inp-dat-200.
      *              *-------------------------------------------------*
      *              * Calcolo delle date di inizio e fine mese oltre  *
      *              * precedenti                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-dat-inp-imo      .
      *                  *---------------------------------------------*
      *                  * Fine mese                                   *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-fm7    to   w-stp-dat-inp-fmo      .
      *                      *-----------------------------------------*
      *                      * Decremento di un mese ( -1)             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fmo    to   s-dat                  .
           subtract  1                    from s-mes                  .
           move      31                   to   s-gio                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-fmo      .
      *                      *-----------------------------------------*
      *                      * Decremento di un mese ( -2)             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fmo    to   s-dat                  .
           subtract  1                    from s-mes                  .
           move      31                   to   s-gio                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-fmo      .
      *                      *-----------------------------------------*
      *                      * Decremento di un mese ( -3)             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fmo    to   s-dat                  .
           subtract  1                    from s-mes                  .
           move      31                   to   s-gio                  .
           if        s-mes                =    0
                     subtract  1          from s-saa
                     move      12         to   s-mes                  .
       stp-srt-inp-dat-205.
           move      s-dat                to   w-stp-dat-inp-fmo      .
      *                      *-----------------------------------------*
      *                      * Controllo della data                    *
      *                      *-----------------------------------------*
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Se la data e' corretta : oltre          *
      *                      *-----------------------------------------*
           if        s-sts                =    spaces
                     go to stp-srt-inp-dat-900.
           subtract  1                    from s-gio                  .
           go to     stp-srt-inp-dat-205.
       stp-srt-inp-dat-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-dat-999.
       stp-srt-inp-dat-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Start iniziale                     *
      *    *-----------------------------------------------------------*
       prn-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Messaggio per nessuna registraz.   *
      *    *-----------------------------------------------------------*
       prn-nes-ela-000.
           move      "WR"                 to   m-ope                  .
           move      "Nessun cliente entro i limiti assegnati !"
                                          to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
       prn-nes-ela-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Lettura sequenziale                *
      *    *-----------------------------------------------------------*
       prn-let-seq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio sortato [srt]      *
      *              *-------------------------------------------------*
           return    srt    at end
                            move  "#"     to   w-cnt-prn-flg-sub
                            go to prn-let-seq-999.
       prn-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Test se superamento limiti massimi *
      *    *-----------------------------------------------------------*
       prn-tst-max-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Selezione su record letto          *
      *    *-----------------------------------------------------------*
       prn-sel-rec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-sub      .
       prn-sel-rec-800.
      *              *-------------------------------------------------*
      *              * Uscita per selezioni superate                   *
      *              *-------------------------------------------------*
           go to     prn-sel-rec-999.
       prn-sel-rec-900.
      *              *-------------------------------------------------*
      *              * Uscita per selezioni non superate               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di errore                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-prn-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-sel-rec-999.
       prn-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Composizione area per rotture      *
      *    *-----------------------------------------------------------*
       prn-cmp-rot-000.
       prn-cmp-rot-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per inizio ciclo        *
      *    *-----------------------------------------------------------*
       prn-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-cic-100.
      *              *-------------------------------------------------*
      *              * Normalizzazioni totali generali                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-tot-tot-cli      .
           move      zero                 to   w-stp-tot-tot-tot      .
           move      zero                 to   w-stp-tot-tot-mic      .
           move      zero                 to   w-stp-tot-tot-ms1      .
           move      zero                 to   w-stp-tot-tot-ms2      .
           move      zero                 to   w-stp-tot-tot-ms3      .
           move      zero                 to   w-stp-tot-tot-ms4      .
           move      zero                 to   w-stp-tot-tot-ms5      .
           move      zero                 to   w-stp-tot-tot-ms6      .
           move      zero                 to   w-stp-tot-tot-ms7      .
           move      zero                 to   w-stp-tot-tot-mso      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni sub-totali generali             *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-sub-tot-tot      .
           move      zero                 to   w-stp-sub-tot-mic      .
           move      zero                 to   w-stp-sub-tot-ms1      .
           move      zero                 to   w-stp-sub-tot-ms2      .
           move      zero                 to   w-stp-sub-tot-ms3      .
           move      zero                 to   w-stp-sub-tot-ms4      .
           move      zero                 to   w-stp-sub-tot-ms5      .
           move      zero                 to   w-stp-sub-tot-ms6      .
           move      zero                 to   w-stp-sub-tot-ms7      .
           move      zero                 to   w-stp-sub-tot-mso      .
       prn-ini-cic-300.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di intestazione foglio     *
      *              *-------------------------------------------------*
           move      spaces               to   w-stp-lvc-cli-fip      .
       prn-ini-cic-800.
      *              *-------------------------------------------------*
      *              * Preparazione emissione sequenziale              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        rr-snx-exp           not  = 2 and
                     rr-snx-exp           not  = 3
                     go to prn-ini-cic-999.
      *                  *---------------------------------------------*
      *                  * Subroutine di inizializzazione              *
      *                  *---------------------------------------------*
           perform   prn-ini-cic-exp-000  thru prn-ini-cic-exp-999    .
       prn-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per inizio ciclo        *
      *    *                                                           *
      *    * Subroutine per inizio emissione file sequenziale          *
      *    *-----------------------------------------------------------*
       prn-ini-cic-exp-000.
      *              *-------------------------------------------------*
      *              * Open file di export                             *
      *              *-------------------------------------------------*
           move      "OP"                 to   x-ope                  .
           move      spaces               to   x-pat                  .
           move      spaces               to   x-nam                  .
           move      02                   to   x-tex                  .
           move      ";"                  to   x-sep                  .
           call      "swd/mod/prg/obj/mxport"
                                        using  x                      .
           move      x-pat                to   w-det-pth-fso-pat      .
       prn-ini-cic-exp-200.
      *              *-------------------------------------------------*
      *              * Intestazioni colonne                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero progressivo                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "H"                  to   x-tip                  .
           move      03                   to   x-car                  .
           move      "NR."                to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "H"                  to   x-tip                  .
           move      06                   to   x-car                  .
           move      "CODICE"             to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Ragione sociale cliente                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      15                   to   x-car                  .
           move      "RAGIONE SOCIALE"    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Totale cliente                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      06                   to   x-car                  .
           move      "TOTALE"             to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Literal mese in corso                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione literal mese             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fmr    to   s-dat                  .
           if        s-saa                =    w-stp-dat-inp-srf
                     move  w-tbl-mes-de1 (s-mes)
                                          to   w-tbl-mes-lit
           else      move  w-tbl-mes-de2 (s-mes)
                                          to   w-tbl-mes-lit          .
      *                      *-----------------------------------------*
      *                      * Editing anno                            *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-stp-dat-inp-fmr    to   s-dat                  .
           move      s-ann                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Assemblaggio                            *
      *                      *-----------------------------------------*
           move      07                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-tbl-mes-lit        to   w-all-str-cat (1)      .
           move      p-edt                to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Emissione                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      07                   to   x-car                  .
           move      w-all-str-alf        to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Literal mese precedente 1                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione literal mese             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fm1    to   s-dat                  .
           if        s-saa                =    w-stp-dat-inp-srf
                     move  w-tbl-mes-de1 (s-mes)
                                          to   w-tbl-mes-lit
           else      move  w-tbl-mes-de2 (s-mes)
                                          to   w-tbl-mes-lit          .
      *                      *-----------------------------------------*
      *                      * Editing anno                            *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-stp-dat-inp-fm1    to   s-dat                  .
           move      s-ann                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Assemblaggio                            *
      *                      *-----------------------------------------*
           move      07                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-tbl-mes-lit        to   w-all-str-cat (1)      .
           move      p-edt                to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Emissione                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      07                   to   x-car                  .
           move      w-all-str-alf        to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Literal mese precedente 2                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione literal mese             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fm2    to   s-dat                  .
           if        s-saa                =    w-stp-dat-inp-srf
                     move  w-tbl-mes-de1 (s-mes)
                                          to   w-tbl-mes-lit
           else      move  w-tbl-mes-de2 (s-mes)
                                          to   w-tbl-mes-lit          .
      *                      *-----------------------------------------*
      *                      * Editing anno                            *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-stp-dat-inp-fm2    to   s-dat                  .
           move      s-ann                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Assemblaggio                            *
      *                      *-----------------------------------------*
           move      07                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-tbl-mes-lit        to   w-all-str-cat (1)      .
           move      p-edt                to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Emissione                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      07                   to   x-car                  .
           move      w-all-str-alf        to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Literal mese precedente 3 ed oltre          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione literal mese             *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fm3    to   s-dat                  .
           if        s-saa                =    w-stp-dat-inp-srf
                     move  w-tbl-mes-de1 (s-mes)
                                          to   w-tbl-mes-lit
           else      move  w-tbl-mes-de2 (s-mes)
                                          to   w-tbl-mes-lit          .
      *                      *-----------------------------------------*
      *                      * Editing anno                            *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      w-stp-dat-inp-fm3    to   s-dat                  .
           move      s-ann                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Assemblaggio                            *
      *                      *-----------------------------------------*
           move      09                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      w-tbl-mes-lit        to   w-all-str-cat (1)      .
           move      p-edt                to   w-all-str-cat (2)      .
           move      "+"                  to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Emissione                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      07                   to   x-car                  .
           move      w-all-str-alf        to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                  *---------------------------------------------*
      *                  * Note                                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "H"                  to   x-tip                  .
           move      04                   to   x-car                  .
           move      "NOTE"               to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
       prn-ini-cic-exp-800.
      *              *-------------------------------------------------*
      *              * Emissione riga                                  *
      *              *-------------------------------------------------*
           move      "PR"                 to   x-ope                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
       prn-ini-cic-exp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-ini-cic-exp-999.
       prn-ini-cic-exp-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per fine ciclo          *
      *    *-----------------------------------------------------------*
       prn-fin-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-cic-100.
      *              *-------------------------------------------------*
      *              * Stampa totali generali                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Tratteggio                                  *
      *                  *---------------------------------------------*
           move      "-"                  to   w-stp-ttg-fin-tdt      .
           perform   stp-ttg-fin-000      thru stp-ttg-fin-999        .
       prn-fin-cic-200.
      *              *-------------------------------------------------*
      *              * Stampa sub-totali                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su linee residue                       *
      *                  *---------------------------------------------*
           if        p-res                >    2
                     go to prn-fin-cic-220.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                  *---------------------------------------------*
      *                  * Test se interruzione forzata                *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-cic-999.
       prn-fin-cic-220.
      *              *-------------------------------------------------*
      *              * Sub-totali                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Griglia vuota                               *
      *                  *---------------------------------------------*
           perform   stp-gri-vuo-000      thru stp-gri-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Prompt per sub-totali                       *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      18                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      041                  to   p-pos                  .
           move      "Totale selezione >" to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale selezione                            *
      *                  *---------------------------------------------*
           move      01                   to   w-stp-val-num-col      .
           move      w-stp-sub-tot-tot    to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese in corso                               *
      *                  *---------------------------------------------*
           move      02                   to   w-stp-val-num-col      .
           move      w-stp-sub-tot-mic    to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 1                           *
      *                  *---------------------------------------------*
           move      03                   to   w-stp-val-num-col      .
           move      w-stp-sub-tot-ms1    to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 2                           *
      *                  *---------------------------------------------*
           move      04                   to   w-stp-val-num-col      .
           move      w-stp-sub-tot-ms2    to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 3 ed oltre                  *
      *                  *---------------------------------------------*
           move      05                   to   w-stp-val-num-col      .
           move      w-stp-sub-tot-ms3    to   w-stp-val-num-val      .
           add       w-stp-sub-tot-ms4    to   w-stp-val-num-val      .
           add       w-stp-sub-tot-ms5    to   w-stp-val-num-val      .
           add       w-stp-sub-tot-ms6    to   w-stp-val-num-val      .
           add       w-stp-sub-tot-ms7    to   w-stp-val-num-val      .
           add       w-stp-sub-tot-mso    to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Tratteggio                                  *
      *                  *---------------------------------------------*
           move      "-"                  to   w-stp-ttg-fin-tdt      .
           perform   stp-ttg-fin-000      thru stp-ttg-fin-999        .
       prn-fin-cic-300.
      *              *-------------------------------------------------*
      *              * Totali generali                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su linee residue                       *
      *                  *---------------------------------------------*
           if        p-res                >    16
                     go to prn-fin-cic-320.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                  *---------------------------------------------*
      *                  * Test se interruzione forzata                *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-cic-999.
       prn-fin-cic-320.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Linea superiore                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "+--------------------------+"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Titolo                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|          Totali          |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Linea inferiore                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "+---------------+----------+"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale mese in corso                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "| Mese in corso |          |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      018                  to   w-stp-val-num-col      .
           move      w-stp-tot-tot-mic    to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale mese 1                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|  30 -  60 gg. |          |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      018                  to   w-stp-val-num-col      .
           move      w-stp-tot-tot-ms1    to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale mese 2                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|  60 -  90 gg. |          |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      018                  to   w-stp-val-num-col      .
           move      w-stp-tot-tot-ms2    to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale mese 3                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|  90 - 120 gg. |          |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      018                  to   w-stp-val-num-col      .
           move      w-stp-tot-tot-ms3    to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale mese 4                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "| 120 - 150 gg. |          |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      018                  to   w-stp-val-num-col      .
           move      w-stp-tot-tot-ms4    to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale mese 5                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "| 150 - 180 gg. |          |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      018                  to   w-stp-val-num-col      .
           move      w-stp-tot-tot-ms5    to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale mese 6                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|  <  9 Mesi    |          |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      018                  to   w-stp-val-num-col      .
           move      w-stp-tot-tot-ms6    to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale mese 7                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|  < 12 Mesi    |          |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      018                  to   w-stp-val-num-col      .
           move      w-stp-tot-tot-ms7    to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale mesi oltre                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "| Oltre 12 Mesi |          |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      018                  to   w-stp-val-num-col      .
           move      w-stp-tot-tot-mso    to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Linea inferiore                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "+---------------+----------+"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale generale                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|        Totale |          |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      018                  to   w-stp-val-num-col      .
           move      w-stp-tot-tot-mic    to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms1    to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms2    to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms3    to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms4    to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms5    to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms6    to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms7    to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mso    to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Linea inferiore                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      28                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "+---------------+----------+"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-cic-400.
      *              *-------------------------------------------------*
      *              * %                                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * AD USCITA FORZATA                           *
      *                  *---------------------------------------------*
           go to     prn-fin-cic-850.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Griglia vuota                               *
      *                  *---------------------------------------------*
           perform   stp-gri-vuo-000      thru stp-gri-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Prompt per %                                *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      18                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      041                  to   p-pos                  .
           move      "               % >" to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale generale                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Calcolo                                 *
      *                      *-----------------------------------------*
           move      w-stp-sub-tot-tot    to   w-stp-lvc-tot-cmp      .
           divide    w-stp-tot-tot-tot    into w-stp-lvc-tot-cmp      .
           multiply  100                  by   w-stp-lvc-tot-cmp      .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      01                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "BD"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      064                  to   p-pos                  .
           move      w-stp-lvc-tot-cmp    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mese in corso                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Calcolo                                 *
      *                      *-----------------------------------------*
           move      w-stp-sub-tot-mic    to   w-stp-lvc-tot-cmp      .
           divide    w-stp-tot-tot-mic    into w-stp-lvc-tot-cmp      .
           multiply  100                  by   w-stp-lvc-tot-cmp      .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      01                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "BD"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      075                  to   p-pos                  .
           move      w-stp-lvc-tot-cmp    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mese precedente 1                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Calcolo                                 *
      *                      *-----------------------------------------*
           move      w-stp-sub-tot-ms1    to   w-stp-lvc-tot-cmp      .
           divide    w-stp-tot-tot-ms1    into w-stp-lvc-tot-cmp      .
           multiply  100                  by   w-stp-lvc-tot-cmp      .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      01                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "BD"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      086                  to   p-pos                  .
           move      w-stp-lvc-tot-cmp    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mese precedente 2                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Calcolo                                 *
      *                      *-----------------------------------------*
           move      w-stp-sub-tot-ms2    to   w-stp-lvc-tot-cmp      .
           divide    w-stp-tot-tot-ms2    into w-stp-lvc-tot-cmp      .
           multiply  100                  by   w-stp-lvc-tot-cmp      .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      01                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "BD"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      097                  to   p-pos                  .
           move      w-stp-lvc-tot-cmp    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mese precedente 3                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Calcolo                                 *
      *                      *-----------------------------------------*
           move      w-stp-sub-tot-ms3    to   w-stp-lvc-tot-cmp      .
           divide    w-stp-tot-tot-ms3    into w-stp-lvc-tot-cmp      .
           multiply  100                  by   w-stp-lvc-tot-cmp      .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      01                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "BD"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      108                  to   p-pos                  .
           move      w-stp-lvc-tot-cmp    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-cic-800.
      *              *-------------------------------------------------*
      *              * Chiusura                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Tratteggio                                  *
      *                  *---------------------------------------------*
           move      "-"                  to   w-stp-ttg-fin-tdt      .
           perform   stp-ttg-fin-000      thru stp-ttg-fin-999        .
       prn-fin-cic-850.
      *              *-------------------------------------------------*
      *              * Chiusura emissione sequenziale                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        rr-snx-exp           not  = 2 and
                     rr-snx-exp           not  = 3
                     go to prn-fin-cic-900.
      *                  *---------------------------------------------*
      *                  * Close file di export                        *
      *                  *---------------------------------------------*
           move      "CL"                 to   x-ope                  .
           move      w-det-pth-fso-pat    to   x-pat                  .
           call      "swd/mod/prg/obj/mxport"
                                        using  x                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo export                 *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mxport"                         .
      *                  *---------------------------------------------*
      *                  * Eventuali messaggi di dati elaborati        *
      *                  *---------------------------------------------*
           if        x-msg                =    spaces
                     go to prn-fin-cic-999.
           move      "WR"                 to   m-ope                  .
           move      x-ms1                to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
           if        x-ms2                =    spaces
                     go to prn-fin-cic-999.
           move      "WR"                 to   m-ope                  .
           move      x-ms2                to   m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prn-fin-cic-999.
       prn-fin-cic-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-fin-cic-999.
       prn-fin-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 5. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 5. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr5-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 4. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 4. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr4-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 3. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 3. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr3-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 2. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 2. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr2-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Inizio 1. livello di rottura       *
      *    *-----------------------------------------------------------*
       prn-ini-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-ini-lr1-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-ini-lr1-999.
       prn-ini-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 1. livello di rottura         *
      *    *-----------------------------------------------------------*
       prn-fin-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
       prn-fin-lr1-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-fin-lr1-999.
       prn-fin-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Livello di dettaglio               *
      *    *-----------------------------------------------------------*
       prn-liv-det-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Incremento contatore clienti trattati           *
      *              *-------------------------------------------------*
           add       1                    to   w-stp-tot-tot-cli      .
      *              *-------------------------------------------------*
      *              * Cumulo totali generali                          *
      *              *-------------------------------------------------*
           add       srt-cum-tot          to   w-stp-tot-tot-tot      .
           add       srt-cum-mic          to   w-stp-tot-tot-mic      .
           add       srt-cum-ms1          to   w-stp-tot-tot-ms1      .
           add       srt-cum-ms2          to   w-stp-tot-tot-ms2      .
           add       srt-cum-ms3          to   w-stp-tot-tot-ms3      .
           add       srt-cum-ms4          to   w-stp-tot-tot-ms4      .
           add       srt-cum-ms5          to   w-stp-tot-tot-ms5      .
           add       srt-cum-ms6          to   w-stp-tot-tot-ms6      .
           add       srt-cum-ms7          to   w-stp-tot-tot-ms7      .
           add       srt-cum-mso          to   w-stp-tot-tot-mso      .
       prn-liv-det-005.
      *              *-------------------------------------------------*
      *              * Test su contatore max elementi                  *
      *              *-------------------------------------------------*
           if        rr-max-ele           =    zero
                     go to prn-liv-det-008.
           if        w-stp-tot-tot-cli    >    rr-max-ele
                     go to prn-liv-det-800.
       prn-liv-det-008.
      *              *-------------------------------------------------*
      *              * Cumulo sub-totali generali                      *
      *              *-------------------------------------------------*
           add       srt-cum-tot          to   w-stp-sub-tot-tot      .
           add       srt-cum-mic          to   w-stp-sub-tot-mic      .
           add       srt-cum-ms1          to   w-stp-sub-tot-ms1      .
           add       srt-cum-ms2          to   w-stp-sub-tot-ms2      .
           add       srt-cum-ms3          to   w-stp-sub-tot-ms3      .
           add       srt-cum-ms4          to   w-stp-sub-tot-ms4      .
           add       srt-cum-ms5          to   w-stp-sub-tot-ms5      .
           add       srt-cum-ms6          to   w-stp-sub-tot-ms6      .
           add       srt-cum-ms7          to   w-stp-sub-tot-ms7      .
           add       srt-cum-mso          to   w-stp-sub-tot-mso      .
       prn-liv-det-010.
      *              *-------------------------------------------------*
      *              * Bufferizzazione anagrafica cliente principale   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [dcc]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [dcc]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      srt-cod-cli          to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcc-rs1-doc         .
       prn-liv-det-020.
      *              *-------------------------------------------------*
      *              * Lettura anagrafica di controllo credito         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [ccc]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofccc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ccc                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [ccc]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      srt-cod-cli          to   rf-ccc-cod-cli         .
           move      "pgm/gep/fls/ioc/obj/iofccc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ccc                 .
      *                  *---------------------------------------------*
      *                  * Normalizzazioni preventive                  *
      *                  *---------------------------------------------*
           if        rf-ccc-max-fid       not  numeric
                     move  zero           to   rf-ccc-max-fid         .
           if        rf-ccc-dat-fid       not  numeric
                     move  zero           to   rf-ccc-dat-fid         .
       prn-liv-det-030.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           move      srt-cod-cli          to   w-stp-lvc-cli-cod      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      rf-dcc-rag-soc       to   w-stp-lvc-cli-rag      .
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
           move      rf-dcc-via-dcc       to   w-stp-lvc-cli-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      rf-dcc-loc-dcc       to   w-stp-lvc-cli-loc      .
      *                      *-----------------------------------------*
      *                      * Fidi massimo                            *
      *                      *-----------------------------------------*
           move      rf-ccc-max-fid       to   w-stp-lvc-cli-fid      .
           move      rf-ccc-dat-fid       to   w-stp-lvc-cli-dfi      .
      *                      *-----------------------------------------*
      *                      * Forma di pagamento                      *
      *                      *-----------------------------------------*
           move      rf-dcc-cod-fop       to   w-stp-lvc-cli-fop      .
           move      spaces               to   w-stp-lvc-cli-dfp      .
      *                      *-----------------------------------------*
      *                      * Data acquisizione                       *
      *                      *-----------------------------------------*
           move      rf-dcc-dat-aqz       to   w-stp-lvc-cli-daq      .
           move      rf-dcc-ide-dat       to   w-stp-lvc-cli-dum      .
      *                      *-----------------------------------------*
      *                      * Blocchi ordini / consegne               *
      *                      *-----------------------------------------*
           if        rf-dcc-for-blo       =    02
                     move  "#"            to   w-stp-lvc-cli-for
           else      move  spaces         to   w-stp-lvc-cli-for      .
           if        rf-dcc-fco-blo       =    02
                     move  "#"            to   w-stp-lvc-cli-fco
           else      move  spaces         to   w-stp-lvc-cli-fco      .
      *                      *-----------------------------------------*
      *                      * Flag di stampa intestazione             *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-lvc-cli-fpd      .
       prn-liv-det-100.
      *              *-------------------------------------------------*
      *              * Stampa cliente                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di intestazione pagina         *
      *                  *---------------------------------------------*
           if        w-stp-lvc-cli-fip    =    spaces
                     move  "#"            to   w-stp-lvc-cli-fip
                     go to prn-liv-det-110.
      *                  *---------------------------------------------*
      *                  * Test su linee residue                       *
      *                  *---------------------------------------------*
           if        p-res                >    2
                     go to prn-liv-det-120.
       prn-liv-det-110.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                  *---------------------------------------------*
      *                  * Test se interruzione forzata                *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-liv-det-999.
      *                  *---------------------------------------------*
      *                  * Fincatura dettaglio                         *
      *                  *---------------------------------------------*
           perform   int-fin-det-000      thru int-fin-det-999        .
       prn-liv-det-120.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Griglia vuota                               *
      *                  *---------------------------------------------*
           perform   stp-gri-vuo-000      thru stp-gri-vuo-999        .
      *                  *---------------------------------------------*
      *                  * Progressivo numerico                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      002                  to   p-pos                  .
           move      w-stp-tot-tot-cli    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      010                  to   p-pos                  .
           move      w-stp-lvc-cli-cod    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Ragione sociale cliente - parte 1           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      019                  to   p-pos                  .
           move      w-stp-lvc-cli-rag    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale cliente                              *
      *                  *---------------------------------------------*
           move      01                   to   w-stp-val-num-col      .
           move      srt-cum-tot          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese in corso                               *
      *                  *---------------------------------------------*
           move      02                   to   w-stp-val-num-col      .
           move      srt-cum-mic          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 1                           *
      *                  *---------------------------------------------*
           move      03                   to   w-stp-val-num-col      .
           move      srt-cum-ms1          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 2                           *
      *                  *---------------------------------------------*
           move      04                   to   w-stp-val-num-col      .
           move      srt-cum-ms2          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 3 e oltre                   *
      *                  *---------------------------------------------*
           move      05                   to   w-stp-val-num-col      .
           move      srt-cum-ms3          to   w-stp-val-num-val      .
           add       srt-cum-ms4          to   w-stp-val-num-val      .
           add       srt-cum-ms5          to   w-stp-val-num-val      .
           add       srt-cum-ms6          to   w-stp-val-num-val      .
           add       srt-cum-ms7          to   w-stp-val-num-val      .
           add       srt-cum-mso          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           move      01                   to   w-stp-val-num-div      .
           perform   stp-val-num-000      thru stp-val-num-999        .
       prn-liv-det-600.
      *              *-------------------------------------------------*
      *              * Stampa Fido                                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        rr-sns-fid           not  = 2
                     go to prn-liv-det-800.
      *                  *---------------------------------------------*
      *                  * Preparazione e stampa                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing valore per concatenamento       *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<BG"                to   p-edm                  .
           move      w-stp-lvc-cli-fid    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Concatenamento Fido                     *
      *                      *-----------------------------------------*
           move      28                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "Fido :"             to   w-all-str-cat (1)      .
           if        w-stp-lvc-cli-fid    =    zero
                     and w-stp-lvc-cli-dfi
                                          not  = zero
                     move "Garantito"     to   w-all-str-cat (2)
           else if   w-stp-lvc-cli-fid    =    zero
                     and w-stp-lvc-cli-dfi
                                          =    zero
                     move "--        "    to   w-all-str-cat (2)
           else      move  p-edt          to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                      *-----------------------------------------*
      *                      * Stampa stringa                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      15                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      116                  to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-liv-det-800.
      *              *-------------------------------------------------*
      *              * Emissione sequenziale                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        rr-snx-exp           not  = 2 and
                     rr-snx-exp           not  = 3
                     go to prn-liv-det-900.
      *                  *---------------------------------------------*
      *                  * Subroutine di emissione                     *
      *                  *---------------------------------------------*
           perform   prn-liv-det-exp-000  thru prn-liv-det-exp-999    .
       prn-liv-det-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-999.
       prn-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Livello di dettaglio               *
      *    *                                                           *
      *    * Subroutine di emissione sequenziale                       *
      *    *-----------------------------------------------------------*
       prn-liv-det-exp-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
       prn-liv-det-exp-200.
      *              *-------------------------------------------------*
      *              * Emissione                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione record di output               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Progressivo numerico                    *
      *                      *-----------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "N"                  to   x-tip                  .
           move      07                   to   x-car                  .
           move      zero                 to   x-dec                  .
           move      spaces               to   x-sgn                  .
           move      "B"                  to   x-edm                  .
           move      w-stp-tot-tot-cli    to   x-num                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "N"                  to   x-tip                  .
           move      07                   to   x-car                  .
           move      zero                 to   x-dec                  .
           move      spaces               to   x-sgn                  .
           move      "B"                  to   x-edm                  .
           move      w-stp-lvc-cli-cod    to   x-num                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale cliente                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      40                   to   x-car                  .
           move      w-stp-lvc-cli-rag    to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                      *-----------------------------------------*
      *                      * Totale cliente                          *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
      *
           if        srt-cum-tot          >    999999999 or
                     srt-cum-tot          <   -999999999
                     move  11             to   p-car
                     move  spaces         to   p-edm
           else      move  09             to   p-car
                     move  "G"            to   p-edm                  .
      *
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      srt-cum-tot          to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      18                   to   x-car                  .
           move      p-edt                to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                      *-----------------------------------------*
      *                      * Mese in corso                           *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
      *
           if        srt-cum-mic          >    999999999 or
                     srt-cum-mic          <   -999999999
                     move  11             to   p-car
                     move  spaces         to   p-edm
           else      move  09             to   p-car
                     move  "G"            to   p-edm                  .
      *
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      srt-cum-mic          to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      18                   to   x-car                  .
           move      p-edt                to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                      *-----------------------------------------*
      *                      * Mese successivo 1                       *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
      *
           if        srt-cum-ms1          >    999999999 or
                     srt-cum-ms1          <   -999999999
                     move  11             to   p-car
                     move  spaces         to   p-edm
           else      move  09             to   p-car
                     move  "G"            to   p-edm                  .
      *
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      srt-cum-ms1          to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      18                   to   x-car                  .
           move      p-edt                to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                      *-----------------------------------------*
      *                      * Mese successivo 2                       *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
      *
           if        srt-cum-ms2          >    999999999 or
                     srt-cum-ms2          <   -999999999
                     move  11             to   p-car
                     move  spaces         to   p-edm
           else      move  09             to   p-car
                     move  "G"            to   p-edm                  .
      *
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      srt-cum-ms2          to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      18                   to   x-car                  .
           move      p-edt                to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                      *-----------------------------------------*
      *                      * Mese successivo 3 e oltre               *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
      *
           if        srt-cum-ms3          >    999999999 or
                     srt-cum-ms3          <   -999999999
                     move  11             to   p-car
                     move  spaces         to   p-edm
           else      move  09             to   p-car
                     move  "G"            to   p-edm                  .
      *
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      srt-cum-ms3          to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      18                   to   x-car                  .
           move      p-edt                to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
      *                      *-----------------------------------------*
      *                      * Fido (Note)                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da stampare                 *
      *                          *-------------------------------------*
           if        rr-sns-fid           not  = 2
                     move  spaces         to   w-all-str-alf
                     go to prn-liv-det-exp-600.
      *                          *-------------------------------------*
      *                          * Editing valore per concatenamento   *
      *                          *-------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<BG"                to   p-edm                  .
           move      w-stp-lvc-cli-fid    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Concatenamento Fido                 *
      *                          *-------------------------------------*
           move      28                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "Fido :"             to   w-all-str-cat (1)      .
           if        w-stp-lvc-cli-fid    =    zero
                     and w-stp-lvc-cli-dfi
                                          not  = zero
                     move "Garantito"     to   w-all-str-cat (2)
           else if   w-stp-lvc-cli-fid    =    zero
                     and w-stp-lvc-cli-dfi
                                          =    zero
                     move "--        "    to   w-all-str-cat (2)
           else      move  p-edt          to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
       prn-liv-det-exp-600.
      *                          *-------------------------------------*
      *                          * Emissione                           *
      *                          *-------------------------------------*
           move      "PF"                 to   x-ope                  .
           move      "A"                  to   x-tip                  .
           move      20                   to   x-car                  .
           move      w-all-str-alf        to   x-alf                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
       prn-liv-det-exp-800.
      *              *-------------------------------------------------*
      *              * Emissione riga                                  *
      *              *-------------------------------------------------*
           move      "PR"                 to   x-ope                  .
           call      "swd/mod/prg/obj/mxport"
                                         using x                      .
       prn-liv-det-exp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-exp-999.
       prn-liv-det-exp-999.
           exit.

      *    *===========================================================*
      *    * Griglia vuota                                             *
      *    *-----------------------------------------------------------*
       stp-gri-vuo-000.
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|       |                                         
      -              "         |          |          |          |       
      -              "   |          |                |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-gri-vuo-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di stampa tratteggio                           *
      *    *-----------------------------------------------------------*
       stp-ttg-fin-000.
      *              *-------------------------------------------------*
      *              * Preparazione campo da stampare                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Default                                     *
      *                  *---------------------------------------------*
           move      "+-------+-----------------------------------------
      -              "---------+----------+----------+----------+-------
      -              "---+----------+----------------+"
                                          to   w-stp-ttg-fin-alf      .
      *                  *---------------------------------------------*
      *                  * Test se modifica tratteggio                 *
      *                  *---------------------------------------------*
           if        w-stp-ttg-fin-tdt    =    "-"
                     go to stp-ttg-fin-200.
      *                  *---------------------------------------------*
      *                  * Modifica tratteggio                         *
      *                  *---------------------------------------------*
           inspect   w-stp-ttg-fin-alf
                                     replacing all "-" by   "="       .
       stp-ttg-fin-200.
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      132                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      w-stp-ttg-fin-alf    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-ttg-fin-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un valore numerico                              *
      *    *-----------------------------------------------------------*
       stp-val-num-000.
      *              *-------------------------------------------------*
      *              * Determinazione della posizione di stampa in ba- *
      *              * se alla colonna di appartenenza                 *
      *              *-------------------------------------------------*
           if        w-stp-val-num-col    =    01
                     move  061            to   w-stp-val-num-pos
           else if   w-stp-val-num-col    =    02
                     move  072            to   w-stp-val-num-pos
           else if   w-stp-val-num-col    =    03
                     move  083            to   w-stp-val-num-pos
           else if   w-stp-val-num-col    =    04
                     move  094            to   w-stp-val-num-pos
           else if   w-stp-val-num-col    =    05
                     move  105            to   w-stp-val-num-pos
           else      move  w-stp-val-num-col
                                          to   w-stp-val-num-pos      .
      *              *-------------------------------------------------*
      *              * Se valore a zero : zero editato                 *
      *              *-------------------------------------------------*
           if        w-stp-val-num-val    =    zero
                     move  "        0"    to   w-stp-val-num-edt
                     go to stp-val-num-200.
       stp-val-num-100.
      *              *-------------------------------------------------*
      *              * Divisione                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione                                  *
      *                  *---------------------------------------------*
           if        w-stp-val-num-div    =    01
                     go to stp-val-num-110
           else if   w-stp-val-num-div    =    02
                     go to stp-val-num-120
           else if   w-stp-val-num-div    =    03
                     go to stp-val-num-130
           else      go to stp-val-num-110.
       stp-val-num-110.
      *                  *---------------------------------------------*
      *                  * Divisione per 100                           *
      *                  *---------------------------------------------*
           move      w-stp-val-num-val (1 : 11)
                                          to   w-stp-val-num-vns      .
           if        w-stp-val-num-val    <    zero
                     multiply -1          by   w-stp-val-num-vns      .
           go to     stp-val-num-150.
       stp-val-num-120.
      *                  *---------------------------------------------*
      *                  * Divisione per 1.000                         *
      *                  *---------------------------------------------*
           move      w-stp-val-num-val (1 : 10)
                                          to   w-stp-val-num-vns      .
           if        w-stp-val-num-val    <    zero
                     multiply -1          by   w-stp-val-num-vns      .
           go to     stp-val-num-150.
       stp-val-num-130.
      *                  *---------------------------------------------*
      *                  * Divisione per 1.000.000                     *
      *                  *---------------------------------------------*
           move      w-stp-val-num-val (1 : 07)
                                          to   w-stp-val-num-vns      .
           if        w-stp-val-num-val    <    zero
                     multiply -1          by   w-stp-val-num-vns      .
           go to     stp-val-num-150.
       stp-val-num-150.
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           if        w-stp-val-num-vns    >    9999999
                     move  09             to   p-car
           else      move  07             to   p-car                  .
           move      zero                 to   p-dec                  .
           if        w-stp-val-num-vns    <    zero
                     move     "S"         to   p-sgn
                     subtract 1           from p-car
           else      move  spaces         to   p-sgn                  .
           if        w-stp-val-num-vns    >    9999999
                     move  "B"            to   p-edm
           else      move  "G"            to   p-edm                  .
           move      w-stp-val-num-vns    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-val-num-edt      .
       stp-val-num-200.
      *              *-------------------------------------------------*
      *              * Eventuale riempitivo                            *
      *              *-------------------------------------------------*
           if        w-stp-val-num-rmp    not  = spaces
                     inspect   w-stp-val-num-edt
                                     replacing
                                       leading spaces
                                          by   w-stp-val-num-rmp      .
       stp-val-num-300.
      *              *-------------------------------------------------*
      *              * Allineamento a destra                           *
      *              *-------------------------------------------------*
           move      09                   to   w-all-str-lun          .
           move      w-stp-val-num-edt    to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-val-num-pos    to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-val-num-500.
      *              *-------------------------------------------------*
      *              * Stampa evidenziatore negativi                   *
      *              *-------------------------------------------------*
           if        w-stp-val-num-val    >    zero
                     go to stp-val-num-900.
           if        w-stp-val-num-val    =    zero
                     go to stp-val-num-900.
      *              *-------------------------------------------------*
      *              * '*'                                             *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-val-num-pos    to   p-pos                  .
           add       9                    to   p-pos                  .
           move      "*"                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-val-num-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-val-num-999.
       stp-val-num-999.
           exit.

      *    *===========================================================*
      *    * Intestazione foglio                                       *
      *    *-----------------------------------------------------------*
       int-pag-sta-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri intestazione standard    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing data di riferimento                 *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rr-dat-rif           to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Concatenamento con titolo                   *
      *                  *---------------------------------------------*
           move      80                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "CLIENTI CON MAGGIORE ESPOSIZIONE -"
                                          to   w-all-str-cat (1)      .
           move      w-stp-dat-inp-fmr    to   s-dat                  .
           move      w-tbl-mes-de1 (s-mes)
                                          to   w-all-str-cat (2)      .
           move      s-ann                to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                  *---------------------------------------------*
      *                  * Titolo                                      *
      *                  *---------------------------------------------*
           move      w-all-str-alf        to   w-cnt-tit-des-tit      .
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-cnt-tit-dat-stp      .
           move      p-pag                to   w-cnt-tit-num-pag      .
           add       1                    to   w-cnt-tit-num-pag      .
      *              *-------------------------------------------------*
      *              * Intestazione standard                           *
      *              *-------------------------------------------------*
           perform   int-pag-std-000      thru int-pag-std-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per interruzione forzata          *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to  int-pag-sta-999.
       int-pag-sta-999.
           exit.

      *    *===========================================================*
      *    * Intestazione foglio                                       *
      *    *                                                           *
      *    * Fincatura per il dettaglio                                *
      *    *-----------------------------------------------------------*
       int-fin-det-000.
      *              *-------------------------------------------------*
      *              * Tratteggio                                      *
      *              *-------------------------------------------------*
           move      "="                  to   w-stp-ttg-fin-tdt      .
           perform   stp-ttg-fin-000      thru stp-ttg-fin-999        .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Griglia vuota                                   *
      *              *-------------------------------------------------*
           perform   stp-gri-vuo-000      thru stp-gri-vuo-999        .
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Progressivo                                 *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      004                  to   p-pos                  .
           move      "Nr."                to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Cliente                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      032                  to   p-pos                  .
           move      "Cliente"            to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totale                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      063                  to   p-pos                  .
           move      "Totale"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mese in corso                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      074                  to   p-pos                  .
           move      w-stp-dat-inp-fmr    to   s-dat                  .
           if        s-saa                =    w-stp-dat-inp-srf
                     move  w-tbl-mes-de1 (s-mes)
                                          to   p-alf
           else      move  w-tbl-mes-de2 (s-mes)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      078                  to   p-pos                  .
           move      w-stp-dat-inp-fmr    to   s-dat                  .
           move      s-ann                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mese precedente 1                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      085                  to   p-pos                  .
           move      w-stp-dat-inp-fm1    to   s-dat                  .
           if        s-saa                =    w-stp-dat-inp-srf
                     move  w-tbl-mes-de1 (s-mes)
                                          to   p-alf
           else      move  w-tbl-mes-de2 (s-mes)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      089                  to   p-pos                  .
           move      w-stp-dat-inp-fm1    to   s-dat                  .
           move      s-ann                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mese precedente 2                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      096                  to   p-pos                  .
           move      w-stp-dat-inp-fm2    to   s-dat                  .
           if        s-saa                =    w-stp-dat-inp-srf
                     move  w-tbl-mes-de1 (s-mes)
                                          to   p-alf
           else      move  w-tbl-mes-de2 (s-mes)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      100                  to   p-pos                  .
           move      w-stp-dat-inp-fm2    to   s-dat                  .
           move      s-ann                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mese precedente 3 ed oltre                  *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      107                  to   p-pos                  .
           move      w-stp-dat-inp-fm3    to   s-dat                  .
           if        s-saa                =    w-stp-dat-inp-srf
                     move  w-tbl-mes-de1 (s-mes)
                                          to   p-alf
           else      move  w-tbl-mes-de2 (s-mes)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "9"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      111                  to   p-pos                  .
           move      w-stp-dat-inp-fm3    to   s-dat                  .
           move      s-ann                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      113                  to   p-pos                  .
           move      "+"                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Note                                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      122                  to   p-pos                  .
           move      "Note"               to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Tratteggio                                      *
      *              *-------------------------------------------------*
           move      "-"                  to   w-stp-ttg-fin-tdt      .
           perform   stp-ttg-fin-000      thru stp-ttg-fin-999        .
       int-fin-det-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     int-fin-det-999.
       int-fin-det-999.
           exit.

      *    *===========================================================*
      *    * Determinazione (data) + (1 mese) = (data)                 *
      *    *-----------------------------------------------------------*
       det-dat-ums-000.
      *              *-------------------------------------------------*
      *              * Data base in data in output                     *
      *              *-------------------------------------------------*
           move      w-det-dat-ums-dtb    to   w-det-dat-ums-dti      .
      *              *-------------------------------------------------*
      *              * Scomposizione data                              *
      *              *-------------------------------------------------*
           move      w-det-dat-ums-dti    to   s-dat                  .
           add       1                    to   s-mes                  .
           if        s-mes                >    12
                     add  1               to   s-saa
                     move 1               to   s-mes                  .
       det-dat-ums-100.
      *              *-------------------------------------------------*
      *              * Valore incrementato in campo di output          *
      *              *-------------------------------------------------*
           move      s-dat                to   w-det-dat-ums-dti      .
       det-dat-ums-200.
      *              *-------------------------------------------------*
      *              * Controllo della data                            *
      *              *-------------------------------------------------*
           move      "CD"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Se la data e' corretta : oltre              *
      *                  *---------------------------------------------*
           if        s-sts                =    spaces
                     go to det-dat-ums-999.
      *                  *---------------------------------------------*
      *                  * Decremento di un giorno                     *
      *                  *---------------------------------------------*
           subtract  1                    from s-gio                  .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-dat-ums-100.
       det-dat-ums-999.
           exit.

      *    *===========================================================*
      *    * Determinazione status scadenza                            *
      *    *-----------------------------------------------------------*
       det-srd-sdb-000.
      *              *-------------------------------------------------*
      *              * Determinazione sigla ultima operazione eseguita *
      *              * sulla scadenza                                  *
      *              *-------------------------------------------------*
       det-srd-sdb-025.
      *                  *---------------------------------------------*
      *                  * Normalizzazione iniziale sigla              *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-srd-sdb-suo      .
       det-srd-sdb-050.
      *                  *---------------------------------------------*
      *                  * Se la data di emissione scadenza e' supe-   *
      *                  * riore alla data di riferimento per la de-   *
      *                  * terminazione : uscita con status 'N'        *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-emi       >    w-det-srd-sdb-drd
                     move  "N"            to   w-det-srd-sdb-sts
                     go to det-srd-sdb-999.
       det-srd-sdb-100.
      *                  *---------------------------------------------*
      *                  * Emissione                                   *
      *                  *---------------------------------------------*
           move      "EMI"                to   w-det-srd-sdb-suo      .
       det-srd-sdb-125.
      *                  *---------------------------------------------*
      *                  * Storno                                      *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-sto       not  > w-det-srd-sdb-drd and
                     rf-sdb-dtr-sto       not  = zero
                     move  "SSC"          to   w-det-srd-sdb-suo
                     go to det-srd-sdb-800.
       det-srd-sdb-150.
      *                  *---------------------------------------------*
      *                  * Riscossione                                 *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-ris       not  > w-det-srd-sdb-drd and
                     rf-sdb-dtr-ris       not  = zero
                     go to det-srd-sdb-155
           else      go to det-srd-sdb-200.
       det-srd-sdb-155.
      *                      *-----------------------------------------*
      *                      * Determinazione in funzione delle moda-  *
      *                      * lita' di riscossione                    *
      *                      *-----------------------------------------*
           if        rf-sdb-mod-ris       =    01 or
                     rf-sdb-mod-ris       =    02 or
                     rf-sdb-mod-ris       =    03 or
                     rf-sdb-mod-ris       =    04
                     move  "RIS"          to   w-det-srd-sdb-suo
           else if   rf-sdb-mod-ris       =    21 or
                     rf-sdb-mod-ris       =    22 or
                     rf-sdb-mod-ris       =    23 or
                     rf-sdb-mod-ris       =    24
                     move  "PAG"          to   w-det-srd-sdb-suo
           else if   rf-sdb-mod-ris       =    50
                     move  "CMP"          to   w-det-srd-sdb-suo      .
           go to     det-srd-sdb-800.
       det-srd-sdb-200.
      *                  *---------------------------------------------*
      *                  * Richiamo della scadenza presentata          *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-rsp       not  > w-det-srd-sdb-drd and
                     rf-sdb-dtr-rsp       not  = zero
                     move  "RSP"          to   w-det-srd-sdb-suo
                     go to det-srd-sdb-800.
       det-srd-sdb-225.
      *                  *---------------------------------------------*
      *                  * Accredito scadenza al dopo incasso          *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-acs       not  > w-det-srd-sdb-drd and
                     rf-sdb-dtr-acs       not  = zero
                     move  "ACS"          to   w-det-srd-sdb-suo
                     go to det-srd-sdb-800.
       det-srd-sdb-250.
      *                  *---------------------------------------------*
      *                  * Notizia di buon esito sulla scadenza        *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-nbe       not  > w-det-srd-sdb-drd and
                     rf-sdb-dtr-nbe       not  = zero
                     move  "NBE"          to   w-det-srd-sdb-suo
                     go to det-srd-sdb-800.
       det-srd-sdb-275.
      *                  *---------------------------------------------*
      *                  * Presunto buon esito sulla scadenza          *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-pbe       not  > w-det-srd-sdb-drd and
                     rf-sdb-dtr-pbe       not  = zero
                     move  "PBE"          to   w-det-srd-sdb-suo
                     go to det-srd-sdb-800.
       det-srd-sdb-300.
      *                  *---------------------------------------------*
      *                  * Insoluto sulla scadenza presentata          *
      *                  *---------------------------------------------*
           if        rf-sdb-dtr-isp       not  > w-det-srd-sdb-drd and
                     rf-sdb-dtr-isp       not  = zero
                     move  "ISP"          to   w-det-srd-sdb-suo
                     go to det-srd-sdb-800.
       det-srd-sdb-400.
      *                  *---------------------------------------------*
      *                  * Operazioni relative alla distinta di pre-   *
      *                  * sentazione                                  *
      *                  *---------------------------------------------*
       det-srd-sdb-425.
      *                      *-----------------------------------------*
      *                      * Se la scadenza non porta un numero di-  *
      *                      * stinta si escludono tutti i controlli   *
      *                      * sulle operazioni relative alla distinta *
      *                      * di presentazione                        *
      *                      *-----------------------------------------*
           if        rf-sdb-num-ddp       =    zero
                     go to det-srd-sdb-625.
       det-srd-sdb-450.
      *                      *-----------------------------------------*
      *                      * Lettura distinta di presentazione       *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMDDP    "         to   f-key                  .
           move      rf-sdb-num-ddp       to   rf-ddp-num-ddp         .
           move      "pgm/gep/fls/ioc/obj/iofddp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ddp                 .
       det-srd-sdb-475.
      *                      *-----------------------------------------*
      *                      * Se distinta di presentazione non esi-   *
      *                      * stente in archivio distinte si escludo- *
      *                      * no tutti i controlli sulle operazioni   *
      *                      * relative alla distinta di presentazione *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-srd-sdb-625.
       det-srd-sdb-500.
      *                      *-----------------------------------------*
      *                      * Se la data di composizione distinta e'  *
      *                      * superiore alla data di riferimento per  *
      *                      * la determinazione si escludono tutti i  *
      *                      * controlli sulle operazioni relative al- *
      *                      * la distinta di presentazione            *
      *                      *-----------------------------------------*
           if        rf-ddp-dtr-com       >    w-det-srd-sdb-drd
                     go to det-srd-sdb-625.
       det-srd-sdb-525.
      *                      *-----------------------------------------*
      *                      * Composizione distinta                   *
      *                      *-----------------------------------------*
           move      "COM"                to   w-det-srd-sdb-suo      .
       det-srd-sdb-550.
      *                      *-----------------------------------------*
      *                      * Presentazione distinta                  *
      *                      *-----------------------------------------*
           if        rf-ddp-dtr-pre       not  > w-det-srd-sdb-drd and
                     rf-ddp-dtr-pre       not  = zero
                     move  "PRE"          to   w-det-srd-sdb-suo      .
       det-srd-sdb-575.
      *                      *-----------------------------------------*
      *                      * Accettazione distinta                   *
      *                      *-----------------------------------------*
           if        rf-ddp-dtr-act       not  > w-det-srd-sdb-drd and
                     rf-ddp-dtr-act       not  = zero
                     move  "ACT"          to   w-det-srd-sdb-suo      .
       det-srd-sdb-600.
      *                      *-----------------------------------------*
      *                      * Accredito distinta                      *
      *                      *-----------------------------------------*
           if        rf-ddp-dtr-acd       not  > w-det-srd-sdb-drd and
                     rf-ddp-dtr-acd       not  = zero
                     move  "ACD"          to   w-det-srd-sdb-suo      .
       det-srd-sdb-625.
      *                      *-----------------------------------------*
      *                      * Fine controlli su operazioni relative   *
      *                      * alla distinta di presentazione          *
      *                      *-----------------------------------------*
           go to     det-srd-sdb-800.
       det-srd-sdb-800.
      *              *-------------------------------------------------*
      *              * Determinazione dello status della scadenza in   *
      *              * funzione della sigla dell'ultima operazione e-  *
      *              * seguita sulla scadenza                          *
      *              *-------------------------------------------------*
           if        w-det-srd-sdb-suo    =    "EMI" or
                     w-det-srd-sdb-suo    =    "COM" or
                     w-det-srd-sdb-suo    =    "PRE" or
                     w-det-srd-sdb-suo    =    "ACT" or
                     w-det-srd-sdb-suo    =    "ACD"
                     move  "A"            to   w-det-srd-sdb-sts
           else if   w-det-srd-sdb-suo    =    "SSC" or
                     w-det-srd-sdb-suo    =    "RIS" or
                     w-det-srd-sdb-suo    =    "PAG" or
                     w-det-srd-sdb-suo    =    "CMP" or
                     w-det-srd-sdb-suo    =    "RSP" or
                     w-det-srd-sdb-suo    =    "ACS" or
                     w-det-srd-sdb-suo    =    "NBE" or
                     w-det-srd-sdb-suo    =    "PBE" or
                     w-det-srd-sdb-suo    =    "ISP"
                     move  "C"            to   w-det-srd-sdb-sts
           else      move  spaces         to   w-det-srd-sdb-sts      .
      *                  *---------------------------------------------*
      *                  * Eventuale aggiustamento in caso di insoluto *
      *                  *---------------------------------------------*
           if        w-det-srd-sdb-suo    not  = "ISP"
                     go to det-srd-sdb-999.
      *                      *-----------------------------------------*
      *                      * Se la scadenza e' comunque stata ri-    *
      *                      * scossa : tutto inalterato               *
      *                      *-----------------------------------------*
           if        rf-sdb-dtr-ris       not  > w-det-srd-sdb-drd and
                     rf-sdb-dtr-ris       not  = zero
                     go to det-srd-sdb-999.
      *                      *-----------------------------------------*
      *                      * Se a fronte dell'insoluto non e' stata  *
      *                      * emessa una nuova scadenza, essa e' da   *
      *                      * intendersi ancora aperta                *
      *                      *-----------------------------------------*
           if        rf-sdb-ens-isp       =    02   and
                     rf-sdb-nns-isp       =    zero
                     move  "A"            to   w-det-srd-sdb-sts      .
       det-srd-sdb-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per operazioni sulle date                     *
      *    *                                                           *
      *    * 'det-dat-nrg-000/999'                                     *
      *    *                                                           *
      *    * Routines per l'aumento di una data in giorni              *
      *    *                                                           *
      *    * Input  : w-det-dat-nrg-dtb = Data iniziale                *
      *    *                                                           *
      *    *          w-det-dat-nrg-ngi = nr. giorni di incremento     *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-det-dat-nrg-dti = Data incrementata            *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * 'det-nrg-dat-000/999'                                     *
      *    *                                                           *
      *    * Routines per la diminuzione di una data in giorni         *
      *    *                                                           *
      *    * Input  : w-det-nrg-dat-dtb = Data iniziale                *
      *    *                                                           *
      *    *          w-det-nrg-dat-ngd = nr. giorni di decremento     *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-det-nrg-dat-dtd = Data decrementata            *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wnrgdat0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del coefficiente di cambio *
      *    * valuta                                                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acs"                   .

