       Identification Division.
       Program-Id.                                 zttfit01           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    cnv                 *
      *                        Area gestionale:    cnv                 *
      *                                Settore:                        *
      *                                   Fase:    zttfit              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 04/09/10    *
      *                       Ultima revisione:    NdK del 04/09/10    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Sistemazione file [fit] : ZATTI             *
      *                                                                *
      *                    Cambio da 'NFA' a 'NFAFO'                   *
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
      *            * Numero protocollo                                 *
      *            *---------------------------------------------------*
               10  srt-num-prt            pic  9(09)                  .
      *            *---------------------------------------------------*
      *            * Data documento                                    *
      *            *---------------------------------------------------*
               10  srt-dat-doc            pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero documento                                  *
      *            *---------------------------------------------------*
               10  srt-num-doc            pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Filler                                            *
      *            *---------------------------------------------------*
               10  filler                 pic  x(01)                  .

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
                     "cnv"                                            .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03) value
                     "cnv"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "   "                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "zttfit"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "zttfit01"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "          Sistemazione fatture          "       .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "msegrt" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mpslct" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/r"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli                 "mbckgx" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/b"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mmessg" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/m"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                "mprint"  *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/p"                                  .

      *    *===========================================================*
      *    * Area per definizione codici di errore di i-o              *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/e"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per moduli di input-output          *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/f"                                  .

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
      *            * Per routine rou-opn-fls-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-opn-fls      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine sel-prm-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-sel-prm-stp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine let-sel-stp-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-let-sel-stp      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine exe-rou-srt-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-exe-rou-srt      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di tipo uscita da routines di accettazione      *
      *        *-------------------------------------------------------*
           05  w-cnt-acc.
      *            *---------------------------------------------------*
      *            * Da accettazione campi richieste                   *
      *            *---------------------------------------------------*
               10  w-cnt-acc-ric-sel      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di uscita da controlli su tasto Do              *
      *        *-------------------------------------------------------*
           05  w-cnt-tdo.
      *            *---------------------------------------------------*
      *            * Per tasto Do su campi richieste                   *
      *            *---------------------------------------------------*
               10  w-cnt-tdo-ric-flg      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status impostazioni             *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-imp.
      *            *---------------------------------------------------*
      *            * Impostazione richieste                            *
      *            *---------------------------------------------------*
               10  w-cnt-sts-imp-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione prompts  *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-pmt.
      *            *---------------------------------------------------*
      *            * Visualizzazione prompts richieste                 *
      *            *---------------------------------------------------*
               10  w-cnt-sts-pmt-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status visualizzazione dati     *
      *        *-------------------------------------------------------*
           05  w-cnt-sts-vis.
      *            *---------------------------------------------------*
      *            * Visualizzazione dati richieste                    *
      *            *---------------------------------------------------*
               10  w-cnt-sts-vis-ric      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Flags di controllo per tipo funzionamento             *
      *        *-------------------------------------------------------*
           05  w-cnt-fun.
      *            *---------------------------------------------------*
      *            * Si/No richieste per programma di esecuzione       *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento ciclico                       *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-cic      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No richiesta di selezione stampa               *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-stp      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area per preparazione parametri selezione stampa      *
      *        *-------------------------------------------------------*
           05  w-cnt-stp.
               10  w-cnt-stp-tip-sel      pic  x(10)                  .
               10  w-cnt-stp-cod-stp      pic  x(08)                  .
               10  w-cnt-stp-tip-sta      pic  x(01)                  .
               10  w-cnt-stp-cod-mod      pic  x(08)                  .
               10  w-cnt-stp-tip-mod      pic  x(01)                  .
               10  w-cnt-stp-amp-lin      pic  9(03)                  .
               10  w-cnt-stp-top-lin      pic  9(04)                  .
               10  w-cnt-stp-lin-min      pic  9(02)                  .
               10  w-cnt-stp-bot-lin      pic  9(04)                  .
               10  w-cnt-stp-amp-car      pic  9(02)v9(02)            .
               10  w-cnt-stp-alt-int      pic  9(02)v9(02)            .
               10  w-cnt-stp-esp-fut      pic  x(99)                  .
               10  w-cnt-stp-fnz-spc      pic  x(99)                  .
      *        *-------------------------------------------------------*
      *        * Work per padding campi alfanumerici con 'z'           *
      *        *-------------------------------------------------------*
           05  w-pad-zzz.
               10  w-pad-zzz-alf.
                   15  w-pad-zzz-alf-chr
                                   occurs 20       
                                          pic  x(01)                  .
               10  w-pad-zzz-ctr          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Work per string-unstring record richieste             *
      *        *-------------------------------------------------------*
           05  w-stu-rrr.
               10  w-stu-rrr-pnt-stu      pic  9(05)                  .
               10  w-stu-rrr-255-byt.
                   15  filler occurs 255  pic  x(01)                  .
               10  w-stu-rrr-sav-pnt      pic  9(05)                  .
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
      *    * Work-area per test se blanks embedded                     *
      *    *-----------------------------------------------------------*
       01  w-bla-emb.
           05  w-bla-emb-flg              pic  x(01)                  .
           05  w-bla-emb-str.
               10  w-bla-emb-chr occurs 40
                                          pic  x(01)                  .
           05  w-bla-emb-ctr              pic  9(02)                  .

      *    *===========================================================*
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [fit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rffit"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
           05  filler                     pic  x(01)                  .

      *    *===========================================================*
      *    * Work area di comodo                                       *
      *    *-----------------------------------------------------------*
       01  w-wrk.
      *        *-------------------------------------------------------*
      *        * Contatore generico                                    *
      *        *-------------------------------------------------------*
           05  w-wrk-ctr-001              pic  9(06)                  .

      *    *===========================================================*
      *    * Work per subroutines di Att                               *
      *    *-----------------------------------------------------------*
       01  w-att.
      *        *-------------------------------------------------------*
      *        * Work per Att numero documento                         *
      *        *-------------------------------------------------------*
           05  w-att-num-doc.
               10  w-att-num-doc-sgl      pic  x(03)                  .
               10  w-att-num-doc-num      pic  9(11)                  .
               10  w-att-num-doc-num-r redefines
                   w-att-num-doc-num.
                   15  w-att-num-doc-saa  pic  9(03)                  .
                   15  w-att-num-doc-ngi  pic  9(02)                  .
                   15  w-att-num-doc-prg  pic  9(06)                  .
               10  w-att-num-doc-wnu      pic  9(11)                  .
               10  w-att-num-doc-wnu-r redefines
                   w-att-num-doc-wnu.
                   15  w-att-num-doc-wsa  pic  9(03)                  .
                   15  w-att-num-doc-wgi  pic  9(02)                  .
                   15  w-att-num-doc-wpr  pic  9(06)                  .
               10  w-att-num-doc-dat      pic  9(07)                  .
               10  w-att-num-doc-new      pic  9(11)                  .
               10  w-att-num-doc-nsv      pic  9(11)                  .
               10  w-att-num-doc-dsv      pic  9(07)                  .
               
      *    *===========================================================*
      *    * Work area per Determinazioni                              *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Per update [fit]                                      *
      *        *-------------------------------------------------------*
           05  w-det-upd-fit.
      *            *---------------------------------------------------*
      *            * Comodi per la ricerca                             *
      *            *---------------------------------------------------*
               10  w-det-upd-tpr          pic  x(05)                  .
               10  w-det-upd-dpr          pic  x(08)                  .
               10  w-det-upd-npr          pic  x(06)                  .
      *            *---------------------------------------------------*
      *            * Tabella                                           *
      *            *---------------------------------------------------*
               10  w-det-upd-max          pic  9(03)       value 169  .
               10  w-det-upd-lun          pic  9(03)       value 21   .
               10  w-det-upd-ele          pic  9(03)                  .
               10  w-det-upd-ctr          pic  9(03)                  .
               10  w-det-upd-tbl.
                   15  filler             pic  x(21) value
                    "NFA   11/04/08    802"                           .
                   15  filler             pic  x(21) value
                    "NFA   29/04/08    935"                           .
                   15  filler             pic  x(21) value
                    "NFA   28/07/08   1495"                           .
                   15  filler             pic  x(21) value
                    "NFA   08/10/08   1938"                           .
                   15  filler             pic  x(21) value
                    "NFA   03/11/08   2169"                           .
                   15  filler             pic  x(21) value
                    "NFA   06/11/08   2204"                           .
                   15  filler             pic  x(21) value
                    "NFA   06/11/08   2209"                           .
                   15  filler             pic  x(21) value
                    "NFA   12/11/08   2317"                           .
                   15  filler             pic  x(21) value
                    "NFA   13/11/08   2337"                           .
                   15  filler             pic  x(21) value
                    "NFA   18/11/08   2400"                           .
                   15  filler             pic  x(21) value
                    "NFA   20/11/08   2435"                           .
                   15  filler             pic  x(21) value
                    "NFA   04/12/08   2690"                           .
                   15  filler             pic  x(21) value
                    "NFA   10/12/08   2758"                           .
                   15  filler             pic  x(21) value
                    "NFA   17/12/08   2872"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/12/08   2945"                           .
                   15  filler             pic  x(21) value
                    "NFA   07/01/09      5"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/02/09    203"                           .
                   15  filler             pic  x(21) value
                    "NFA   09/02/09    276"                           .
                   15  filler             pic  x(21) value
                    "NFA   27/02/09    426"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/03/09    489"                           .
                   15  filler             pic  x(21) value
                    "NFA   11/03/09    548"                           .
                   15  filler             pic  x(21) value
                    "NFA   15/03/09    595"                           .
                   15  filler             pic  x(21) value
                    "NFA   19/03/09    655"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/03/09    690"                           .
                   15  filler             pic  x(21) value
                    "NFA   24/03/09    702"                           .
                   15  filler             pic  x(21) value
                    "NFA   24/03/09    704"                           .
                   15  filler             pic  x(21) value
                    "NFA   26/03/09    725"                           .
                   15  filler             pic  x(21) value
                    "NFA   21/04/09    940"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/04/09    960"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/04/09    964"                           .
                   15  filler             pic  x(21) value
                    "NFA   27/04/09    982"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/05/09   1046"                           .
                   15  filler             pic  x(21) value
                    "NFA   15/05/09   1155"                           .
                   15  filler             pic  x(21) value
                    "NFA   22/06/09   1434"                           .
                   15  filler             pic  x(21) value
                    "NFA   22/06/09   1454"                           .
                   15  filler             pic  x(21) value
                    "NFA   30/06/09   1560"                           .
                   15  filler             pic  x(21) value
                    "NFA   30/06/09   1565"                           .
                   15  filler             pic  x(21) value
                    "NFA   02/07/09   1577"                           .
                   15  filler             pic  x(21) value
                    "NFA   24/08/09   1725"                           .
                   15  filler             pic  x(21) value
                    "NFA   01/09/09   1789"                           .
                   15  filler             pic  x(21) value
                    "NFA   01/09/09   1794"                           .
                   15  filler             pic  x(21) value
                    "NFA   09/09/09   1853"                           .
                   15  filler             pic  x(21) value
                    "NFA   24/09/09   1985"                           .
                   15  filler             pic  x(21) value
                    "NFA   25/09/09   1993"                           .
                   15  filler             pic  x(21) value
                    "NFA   29/09/09   2023"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/10/09   2061"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/10/09   2062"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/10/09   2065"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/10/09   2074"                           .
                   15  filler             pic  x(21) value
                    "NFA   14/10/09   2142"                           .
                   15  filler             pic  x(21) value
                    "NFA   14/10/09   2143"                           .
                   15  filler             pic  x(21) value
                    "NFA   22/10/09   2215"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/10/09   2228"                           .
                   15  filler             pic  x(21) value
                    "NFA   26/10/09   2233"                           .
                   15  filler             pic  x(21) value
                    "NFA   04/11/09   2314"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/11/09   2326"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/11/09   2327"                           .
                   15  filler             pic  x(21) value
                    "NFA   11/11/09   2418"                           .
                   15  filler             pic  x(21) value
                    "NFA   11/11/09   2419"                           .
                   15  filler             pic  x(21) value
                    "NFA   19/11/09   2521"                           .
                   15  filler             pic  x(21) value
                    "NFA   19/11/09   2523"                           .
                   15  filler             pic  x(21) value
                    "NFA   20/11/09   2536"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/11/09   2573"                           .
                   15  filler             pic  x(21) value
                    "NFA   25/11/09   2622"                           .
                   15  filler             pic  x(21) value
                    "NFA   26/11/09   2638"                           .
                   15  filler             pic  x(21) value
                    "NFA   30/11/09   2717"                           .
                   15  filler             pic  x(21) value
                    "NFA   02/12/09   2744"                           .
                   15  filler             pic  x(21) value
                    "NFA   11/12/09   2904"                           .
                   15  filler             pic  x(21) value
                    "NFA   11/12/09   2906"                           .
                   15  filler             pic  x(21) value
                    "NFA   28/01/10    145"                           .
                   15  filler             pic  x(21) value
                    "NFA   12/02/10    286"                           .
                   15  filler             pic  x(21) value
                    "NFA   24/02/10    369"                           .
                   15  filler             pic  x(21) value
                    "NFA   04/03/10    487"                           .
                   15  filler             pic  x(21) value
                    "NFA   16/03/10    591"                           .
                   15  filler             pic  x(21) value
                    "NFA   17/03/10    601"                           .
                   15  filler             pic  x(21) value
                    "NFA   17/03/10    604"                           .
                   15  filler             pic  x(21) value
                    "NFA   22/03/10    671"                           .
                   15  filler             pic  x(21) value
                    "NFA   08/04/10    814"                           .
                   15  filler             pic  x(21) value
                    "NFA   13/04/10    841"                           .
                   15  filler             pic  x(21) value
                    "NFA   21/04/10    905"                           .
                   15  filler             pic  x(21) value
                    "NFA   21/04/10    906"                           .
                   15  filler             pic  x(21) value
                    "NFA   21/04/10    907"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/04/10    928"                           .
                   15  filler             pic  x(21) value
                    "NFA   27/04/10    951"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/05/10   1037"                           .
                   15  filler             pic  x(21) value
                    "NFA   19/05/10   1142"                           .
                   15  filler             pic  x(21) value
                    "NFA   21/05/10   1167"                           .
                   15  filler             pic  x(21) value
                    "NFA   11/06/10   1281"                           .
                   15  filler             pic  x(21) value
                    "NFA   11/06/10   1282"                           .
                   15  filler             pic  x(21) value
                    "NFA   11/06/10   1283"                           .
                   15  filler             pic  x(21) value
                    "NFA   14/06/10   1307"                           .
                   15  filler             pic  x(21) value
                    "NFA   14/06/10   1308"                           .
                   15  filler             pic  x(21) value
                    "NFA   15/06/10   1325"                           .
                   15  filler             pic  x(21) value
                    "NFA   16/06/10   1331"                           .
                   15  filler             pic  x(21) value
                    "NFA   16/06/10   1332"                           .
                   15  filler             pic  x(21) value
                    "NFA   16/06/10   1344"                           .
                   15  filler             pic  x(21) value
                    "NFA   18/06/10   1368"                           .
                   15  filler             pic  x(21) value
                    "NFA   18/06/10   1371"                           .
                   15  filler             pic  x(21) value
                    "NFA   22/06/10   1412"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/06/10   1433"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/06/10   1434"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/06/10   1435"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/06/10   1437"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/06/10   1438"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/06/10   1439"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/06/10   1440"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/06/10   1444"                           .
                   15  filler             pic  x(21) value
                    "NFA   24/06/10   1460"                           .
                   15  filler             pic  x(21) value
                    "NFA   25/06/10   1475"                           .
                   15  filler             pic  x(21) value
                    "NFA   25/06/10   1476"                           .
                   15  filler             pic  x(21) value
                    "NFA   25/06/10   1477"                           .
                   15  filler             pic  x(21) value
                    "NFA   28/06/10   1487"                           .
                   15  filler             pic  x(21) value
                    "NFA   28/06/10   1489"                           .
                   15  filler             pic  x(21) value
                    "NFA   28/06/10   1490"                           .
                   15  filler             pic  x(21) value
                    "NFA   28/06/10   1495"                           .
                   15  filler             pic  x(21) value
                    "NFA   28/06/10   1500"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/07/10   1506"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/07/10   1508"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/07/10   1509"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/07/10   1510"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/07/10   1511"                           .
                   15  filler             pic  x(21) value
                    "NFA   05/07/10   1518"                           .
                   15  filler             pic  x(21) value
                    "NFA   06/07/10   1527"                           .
                   15  filler             pic  x(21) value
                    "NFA   06/07/10   1528"                           .
                   15  filler             pic  x(21) value
                    "NFA   07/07/10   1535"                           .
                   15  filler             pic  x(21) value
                    "NFA   07/07/10   1536"                           .
                   15  filler             pic  x(21) value
                    "NFA   07/07/10   1541"                           .
                   15  filler             pic  x(21) value
                    "NFA   08/07/10   1554"                           .
                   15  filler             pic  x(21) value
                    "NFA   09/07/10   1562"                           .
                   15  filler             pic  x(21) value
                    "NFA   09/07/10   1563"                           .
                   15  filler             pic  x(21) value
                    "NFA   12/07/10   1568"                           .
                   15  filler             pic  x(21) value
                    "NFA   12/07/10   1570"                           .
                   15  filler             pic  x(21) value
                    "NFA   13/07/10   1574"                           .
                   15  filler             pic  x(21) value
                    "NFA   14/07/10   1580"                           .
                   15  filler             pic  x(21) value
                    "NFA   15/07/10   1589"                           .
                   15  filler             pic  x(21) value
                    "NFA   15/07/10   1590"                           .
                   15  filler             pic  x(21) value
                    "NFA   15/07/10   1591"                           .
                   15  filler             pic  x(21) value
                    "NFA   15/07/10   1592"                           .
                   15  filler             pic  x(21) value
                    "NFA   15/07/10   1593"                           .
                   15  filler             pic  x(21) value
                    "NFA   21/07/10   1621"                           .
                   15  filler             pic  x(21) value
                    "NFA   22/07/10   1625"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/07/10   1636"                           .
                   15  filler             pic  x(21) value
                    "NFA   23/07/10   1638"                           .
                   15  filler             pic  x(21) value
                    "NFA   26/07/10   1644"                           .
                   15  filler             pic  x(21) value
                    "NFA   27/07/10   1651"                           .
                   15  filler             pic  x(21) value
                    "NFA   27/07/10   1652"                           .
                   15  filler             pic  x(21) value
                    "NFA   27/07/10   1653"                           .
                   15  filler             pic  x(21) value
                    "NFA   27/07/10   1656"                           .
                   15  filler             pic  x(21) value
                    "NFA   28/07/10   1662"                           .
                   15  filler             pic  x(21) value
                    "NFA   29/07/10   1670"                           .
                   15  filler             pic  x(21) value
                    "NFA   29/07/10   1671"                           .
                   15  filler             pic  x(21) value
                    "NFA   29/07/10   1672"                           .
                   15  filler             pic  x(21) value
                    "NFA   30/07/10   1686"                           .
                   15  filler             pic  x(21) value
                    "NFA   30/07/10   1692"                           .
                   15  filler             pic  x(21) value
                    "NFA   24/08/10   1712"                           .
                   15  filler             pic  x(21) value
                    "NFA   25/08/10   1721"                           .
                   15  filler             pic  x(21) value
                    "NFA   25/08/10   1722"                           .
                   15  filler             pic  x(21) value
                    "NFA   25/08/10   1723"                           .
                   15  filler             pic  x(21) value
                    "NFA   25/08/10   1726"                           .
                   15  filler             pic  x(21) value
                    "NFA   26/08/10   1741"                           .
                   15  filler             pic  x(21) value
                    "NFA   30/08/10   1755"                           .
                   15  filler             pic  x(21) value
                    "NFA   01/09/10   1779"                           .
                   15  filler             pic  x(21) value
                    "NFA   01/09/10   1780"                           .
                   15  filler             pic  x(21) value
                    "NFA   01/09/10   1781"                           .
                   15  filler             pic  x(21) value
                    "NFA   02/09/10   1793"                           .
                   15  filler             pic  x(21) value
                    "NFA   02/09/10   1798"                           .
                   15  filler             pic  x(21) value
                    "NFA   03/09/10   1807"                           .
                   15  filler             pic  x(21) value
                    "NFA   03/09/10   1808"                           .
                   15  filler             pic  x(21) value
                    "NFA   03/09/10   1810"                           .
      *                *-----------------------------------------------*
      *                * Ridefinizione                                 *
      *                *-----------------------------------------------*
               10  w-det-upd-tbr redefines
                   w-det-upd-tbl.
                   15  w-det-upd-buf occurs 169 indexed 
                                          by   w-det-upd-fit-inx      .
                       20  w-det-upd-tmo  pic  x(05)                  .
                       20  filler         pic  x(01)                  .
                       20  w-det-upd-dat  pic  x(08)                  .
                       20  filler         pic  x(01)                  .
                       20  w-det-upd-num  pic  x(06)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

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
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-800.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento programma       *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Se no richieste : a selezione stampante         *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-350.
       main-250.
      *              *-------------------------------------------------*
      *              * Accettazione richieste di selezione             *
      *              *-------------------------------------------------*
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "E"
                     go to main-800.
      *                  *---------------------------------------------*
      *                  * Se uscita per 'N'                           *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "N"
                     go to main-250.
      *              *-------------------------------------------------*
      *              * Regolarizzazione richieste di selezione         *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-000      thru reg-ric-sel-999        .
       main-350.
      *              *-------------------------------------------------*
      *              * Se no stampa : ad esecuzione                    *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to main-450.
      *              *-------------------------------------------------*
      *              * Preparazione defaults per parametri di selezio- *
      *              * ne stampa                                       *
      *              *-------------------------------------------------*
           perform   pre-prm-stp-000      thru pre-prm-stp-999        .
      *              *-------------------------------------------------*
      *              * Selezione parametri stampa                      *
      *              *-------------------------------------------------*
           perform   sel-prm-stp-000      thru sel-prm-stp-999        .
      *                  *---------------------------------------------*
      *                  * Test se uscita                              *
      *                  *---------------------------------------------*
           if        w-cnt-sel-prm-stp    not  = spaces
                     go to main-800.
       main-450.
      *              *-------------------------------------------------*
      *              * Esecuzione in foreground                        *
      *              *-------------------------------------------------*
           perform   exe-pgm-frg-000      thru exe-pgm-frg-999        .
      *              *-------------------------------------------------*
      *              * Test se tipo esecuzione ciclico                 *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S" or
                     w-cnt-fun-snx-cic    not  = "S"
                     go to main-800
           else      go to main-250.
       main-800.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
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
      *    * Dichiarazione di inizio programma                         *
      *    *-----------------------------------------------------------*
       dic-ini-pgm-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-dic-ini-pgm      .
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
           move      "S"                  to   s-svv                  .
      *              *-------------------------------------------------*
      *              * Richiamo del modulo di segreteria               *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Controllo esito richiamo modulo                 *
      *              *-------------------------------------------------*
           if        s-liv                =    zero
                     move  "#"            to   w-cnt-dic-ini-pgm      .
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
      *    * Visualizzazione titolo programma                          *
      *    *-----------------------------------------------------------*
       vis-tit-pgm-000.
      *              *-------------------------------------------------*
      *              * Erase video                                     *
      *              *-------------------------------------------------*
           move      "ER"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 01                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      01                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Sigla del programma                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      i-ide-fas            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Descrizione del programma                       *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      02                   to   v-lin                  .
           move      21                   to   v-pos                  .
           move      i-ide-des            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 03                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      03                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Trattini a linea 22                             *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      22                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "="            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tit-pgm-999.
           exit.

      *    *===========================================================*
      *    * Test se blanks embedded in w-bla-emb-str                  *
      *    *-----------------------------------------------------------*
       tst-bla-emb-000.
           move      spaces               to   w-bla-emb-flg          .
           if        w-bla-emb-str        =    spaces
                     go to tst-bla-emb-999.
           if        w-bla-emb-chr (1)    =    spaces
                     move  "#"            to   w-bla-emb-flg
                     go to tst-bla-emb-999.
           move      1                    to   w-bla-emb-ctr          .
       tst-bla-emb-100.
           add       1                    to   w-bla-emb-ctr          .
           if        w-bla-emb-ctr        >    40
                     go to tst-bla-emb-999.
           if        w-bla-emb-chr
                    (w-bla-emb-ctr)       not  = spaces
                     go to tst-bla-emb-100.
       tst-bla-emb-200.
           add       1                    to   w-bla-emb-ctr          .
           if        w-bla-emb-ctr        >    40
                     go to tst-bla-emb-999.
           if        w-bla-emb-chr
                    (w-bla-emb-ctr)       =    spaces
                     go to tst-bla-emb-200.
           move      "#"                  to   w-bla-emb-flg          .
       tst-bla-emb-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione campo alfanumerico con padding di "z"    *
      *    *-----------------------------------------------------------*
       pad-alf-zzz-000.
           move      20                   to   w-pad-zzz-ctr          .
       pad-alf-zzz-100.
           if        w-pad-zzz-ctr        >    zero
                     if    w-pad-zzz-alf-chr
                          (w-pad-zzz-ctr) =    spaces
                           move    "z"    to   w-pad-zzz-alf-chr
                                              (w-pad-zzz-ctr)
                           subtract 1     from w-pad-zzz-ctr
                           go to    pad-alf-zzz-100.
       pad-alf-zzz-999.
           exit.

      *    *===========================================================*
      *    * Programma di esecuzione in foreground                     *
      *    *-----------------------------------------------------------*
       exe-pgm-frg-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione rullino messaggi di foreground *
      *              *-------------------------------------------------*
           move      "OF"                 to   m-ope                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
      *                  *---------------------------------------------*
      *                  * Se errore : uscita                          *
      *                  *---------------------------------------------*
            if       m-rsc                not  = spaces
                     go to exe-pgm-frg-999.
      *              *-------------------------------------------------*
      *              * Messaggio di programma in esecuzione            *
      *              *-------------------------------------------------*
           move      "PE"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       exe-pgm-frg-300.
      *              *-------------------------------------------------*
      *              * Esecuzione del programma                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura parametri di selezione stampa       *
      *                  *---------------------------------------------*
           perform   let-sel-stp-000      thru let-sel-stp-999        .
           if        w-cnt-let-sel-stp    not  = spaces
                     go to exe-pgm-frg-999.
      *                  *---------------------------------------------*
      *                  * Esecuzione eventuale sort preliminare       *
      *                  *---------------------------------------------*
           perform   exe-rou-srt-000      thru exe-rou-srt-999        .
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se sort eseguito       *
      *                  *---------------------------------------------*
           if        w-cnt-exe-rou-srt    =    spaces
                     go to exe-pgm-frg-400
           else      go to exe-pgm-frg-500.
       exe-pgm-frg-400.
      *                  *---------------------------------------------*
      *                  * Se sort non eseguito                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ciclo di report-program                 *
      *                      *-----------------------------------------*
           perform   prn-rou-pri-000      thru prn-rou-pri-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     exe-pgm-frg-600.
       exe-pgm-frg-500.
      *                  *---------------------------------------------*
      *                  * Se sort eseguito                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     exe-pgm-frg-600.
       exe-pgm-frg-600.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo stampa                 *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mprint"                         .
       exe-pgm-frg-900.
      *                  *---------------------------------------------*
      *                  * Visual. eventuali errori di esecuzione      *
      *                  *---------------------------------------------*
           move      "VE"                 to   b-ope                  .
           move      "F"                  to   b-tfe                  .
           move      i-ide-des            to   b-chr                  .
           call      "swd/mod/prg/obj/mbckgv"
                                         using b                      .
           cancel    "swd/mod/prg/obj/mbckgv"                         .
       exe-pgm-frg-999.
           exit.

      *    *===========================================================*
      *    *  Selezione parametri stampa                               *
      *    *-----------------------------------------------------------*
       sel-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sel-prm-stp      .
      *              *-------------------------------------------------*
      *              * Test se selezione stampa da eseguire            *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-stp    not  = "S"
                     go to sel-prm-stp-999.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per richiamo selezione   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Informazioni generali da segreteria         *
      *                  *---------------------------------------------*
           move      "IG"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Codice azienda                          *
      *                      *-----------------------------------------*
           move      s-azi                to   r-env-cod-azi          .
      *                      *-----------------------------------------*
      *                      * Codice terminale                        *
      *                      *-----------------------------------------*
           move      s-ter                to   r-env-cod-ter          .
      *                      *-----------------------------------------*
      *                      * Codice utente                           *
      *                      *-----------------------------------------*
           move      s-ute                to   r-env-cod-ute          .
      *                      *-----------------------------------------*
      *                      * Date and time da segreteria             *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Date and time                           *
      *                      *-----------------------------------------*
           move      s-sdt                to   r-env-dat-tim          .
      *                  *---------------------------------------------*
      *                  * Informazioni da identificazione programma   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Sistema applicativo                     *
      *                      *-----------------------------------------*
           move      i-ide-sap            to   r-ide-sis-app          .
      *                      *-----------------------------------------*
      *                      * Area gestionale                         *
      *                      *-----------------------------------------*
           move      i-ide-arg            to   r-ide-are-ges          .
      *                      *-----------------------------------------*
      *                      * Settore gestionale                      *
      *                      *-----------------------------------------*
           move      i-ide-set            to   r-ide-set-ges          .
      *                      *-----------------------------------------*
      *                      * Fase gestionale                         *
      *                      *-----------------------------------------*
           move      i-ide-fas            to   r-ide-fas-ges          .
      *                  *---------------------------------------------*
      *                  * Informazioni da preparazione param. stampa  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flags di tipo selezione                 *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-sel    to   r-fix-tip-sel          .
      *                      *-----------------------------------------*
      *                      * Codice stampante                        *
      *                      *-----------------------------------------*
           move      w-cnt-stp-cod-stp    to   r-fix-cod-stp          .
      *                      *-----------------------------------------*
      *                      * Tipo di stampa                          *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-sta    to   r-fix-tip-sta          .
      *                      *-----------------------------------------*
      *                      * Codice modulo                           *
      *                      *-----------------------------------------*
           move      w-cnt-stp-cod-mod    to   r-fix-cod-mod          .
      *                      *-----------------------------------------*
      *                      * Tipo modulo                             *
      *                      *-----------------------------------------*
           move      w-cnt-stp-tip-mod    to   r-fix-tip-mod          .
      *                      *-----------------------------------------*
      *                      * Ampiezza linea di stampa in caratteri   *
      *                      *-----------------------------------------*
           move      w-cnt-stp-amp-lin    to   r-fix-amp-lin          .
      *                      *-----------------------------------------*
      *                      * Top margin in linee                     *
      *                      *-----------------------------------------*
           move      w-cnt-stp-top-lin    to   r-fix-top-lin          .
      *                      *-----------------------------------------*
      *                      * Numero linee di stampa minimo           *
      *                      *-----------------------------------------*
           move      w-cnt-stp-lin-min    to   r-fix-lin-min          .
      *                      *-----------------------------------------*
      *                      * Bottom margin in linee                  *
      *                      *-----------------------------------------*
           move      w-cnt-stp-bot-lin    to   r-fix-bot-lin          .
      *                      *-----------------------------------------*
      *                      * Ampiezza caratteri                      *
      *                      *-----------------------------------------*
           move      w-cnt-stp-amp-car    to   r-fix-amp-car          .
      *                      *-----------------------------------------*
      *                      * Altezza interlinea                      *
      *                      *-----------------------------------------*
           move      w-cnt-stp-alt-int    to   r-fix-alt-int          .
      *                      *-----------------------------------------*
      *                      * Area riservata per espansioni future    *
      *                      *-----------------------------------------*
           move      w-cnt-stp-esp-fut    to   r-fix-esp-fut          .
      *                      *-----------------------------------------*
      *                      * Area riservata per funzioni speciali    *
      *                      *-----------------------------------------*
           move      w-cnt-stp-fnz-spc    to   r-fix-fnz-spc          .
      *              *-------------------------------------------------*
      *              * Richiamo modulo di selezione stampa             *
      *              *-------------------------------------------------*
           call      "swd/mod/prg/obj/mpslct"
                                         using r                      .
           cancel    "swd/mod/prg/obj/mpslct"                         .
      *              *-------------------------------------------------*
      *              * Status di uscita                                *
      *              *-------------------------------------------------*
           if        r-rsc                not  = spaces
                     move  "#"            to   w-cnt-sel-prm-stp      .
       sel-prm-stp-999.
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
      *              * Si/No richieste ad utente                       *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-ric      .
      *              *-------------------------------------------------*
      *              * Si/No funzionamento ciclico stampa              *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-cic      .
      *              *-------------------------------------------------*
      *              * Si/No richiesta di selezione stampa             *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-stp      .
       pre-tip-fun-999.
           exit.

      *    *===========================================================*
      *    * Open files per richieste                                  *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
      *              *-------------------------------------------------*
      *              * [fit]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [fit]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Accettazione richieste di selezione                       *
      *    *-----------------------------------------------------------*
       acc-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-acc-ric-sel      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status impostazione             *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-imp-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione prompts  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-pmt-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione status visualizzazione dati     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-vis-ric      .
      *              *-------------------------------------------------*
      *              * Normalizzazione parametri di selezione          *
      *              *-------------------------------------------------*
           perform   nor-ric-sel-000      thru nor-ric-sel-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione prompts                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Video in 'OFF'                              *
      *                  *---------------------------------------------*
           move      "OF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Visualizzazione titolo programma            *
      *                  *---------------------------------------------*
           perform   vis-tit-pgm-000      thru vis-tit-pgm-999        .
      *                  *---------------------------------------------*
      *                  * Prompts per richieste di selezione          *
      *                  *---------------------------------------------*
           perform   pmt-ric-sel-000      thru pmt-ric-sel-999        .
           move      "#"                  to   w-cnt-sts-pmt-ric      .
      *                  *---------------------------------------------*
      *                  * Video in 'ON'                               *
      *                  *---------------------------------------------*
           move      "ON"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Accettazioni                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione func-key di impostazione    *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Abilitazione tasto Do                       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ric      .
       acc-ric-sel-150.
       acc-ric-sel-900.
      *              *-------------------------------------------------*
      *              * Flag di controllo status impostazioni           *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-imp-ric      .
      *              *-------------------------------------------------*
      *              * Flag di controllo status visualizzazione        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-sts-vis-ric      .
      *              *-------------------------------------------------*
      *              * Conferma impostazioni                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Cancellazione eventuali note operative      *
      *                  *---------------------------------------------*
           move      "NT"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       acc-ric-sel-910.
      *                  *---------------------------------------------*
      *                  * Accettazione conferma                       *
      *                  *---------------------------------------------*
           move      "MX"                 to   v-ope                  .
           move      "U"                  to   v-tip                  .
           move      "Conferma impostazioni (S/N/E) ?"
                                          to   v-not                  .
           move      "S"                  to   v-alf                  .
           move      "SNE"                to   v-msk                  .
           move      "DO  "               to   v-pfk (05)             .
           move      "UP  "               to   v-pfk (01)             .
           move      "EXIT"               to   v-pfk (20)             .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           if        v-key                not  = spaces
                     go to acc-ric-sel-920.
           if        v-alf                =    "S"
                     move   "DO  "        to   v-key
           else if   v-alf                =    "E"
                     move   "EXIT"        to   v-key
           else if   v-alf                =    "N"
                     move   "UP  "        to   v-key                  .
       acc-ric-sel-920.
      *                  *---------------------------------------------*
      *                  * Test su risposta dell'utente                *
      *                  *---------------------------------------------*
           if        v-key                =    "DO  "
                     go to acc-ric-sel-930
           else if   v-key                =    "EXIT"
                     go to acc-ric-sel-940
           else if   v-key                =    "UP  "
                     go to acc-ric-sel-950
           else      go to acc-ric-sel-910.
       acc-ric-sel-930.
      *                  *---------------------------------------------*
      *                  * Se Do                                       *
      *                  *---------------------------------------------*
           perform   tdo-ric-sel-000      thru tdo-ric-sel-999        .
           if        w-cnt-tdo-ric-flg    =    spaces
                     move  "S"            to   w-cnt-acc-ric-sel
                     go to acc-ric-sel-999
           else      move  spaces         to   w-cnt-tdo-ric-flg
                     go to acc-ric-sel-900.
       acc-ric-sel-940.
      *                  *---------------------------------------------*
      *                  * Se Exit                                     *
      *                  *---------------------------------------------*
           move      "E"                  to   w-cnt-acc-ric-sel      .
           go to     acc-ric-sel-999.
       acc-ric-sel-950.
      *                  *---------------------------------------------*
      *                  * Se Up                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad accettazioni                         *
      *                      *-----------------------------------------*
           go to     acc-ric-sel-100.
       acc-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione prompts per richieste di selezione        *
      *    *-----------------------------------------------------------*
       pmt-ric-sel-000.
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
       nor-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Preparazione parametri per selezione stampa               *
      *    *-----------------------------------------------------------*
       pre-prm-stp-000.
      *              *-------------------------------------------------*
      *              * Flags di tipo selezione                         *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sel      .
      *              *-------------------------------------------------*
      *              * Codice stampante                                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-stp      .
      *              *-------------------------------------------------*
      *              * Tipo di stampa                                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-tip-sta      .
      *              *-------------------------------------------------*
      *              * Codice modulo                                   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-cod-mod      .
      *              *-------------------------------------------------*
      *              * Tipo modulo                                     *
      *              *   - L : Libero                                  *
      *              *   - T : Tipografico                             *
      *              *-------------------------------------------------*
           move      "L"                  to   w-cnt-stp-tip-mod      .
      *              *-------------------------------------------------*
      *              * Ampiezza linea di stampa in caratteri           *
      *              *-------------------------------------------------*
           move      132                  to   w-cnt-stp-amp-lin      .
      *              *-------------------------------------------------*
      *              * Top margin in linee                             *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-top-lin      .
      *              *-------------------------------------------------*
      *              * Numero linee di stampa minimo                   *
      *              *-------------------------------------------------*
           move      30                   to   w-cnt-stp-lin-min      .
      *              *-------------------------------------------------*
      *              * Bottom margin in linee                          *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-bot-lin      .
      *              *-------------------------------------------------*
      *              * Ampiezza caratteri                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-amp-car      .
      *              *-------------------------------------------------*
      *              * Altezza interlinea                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-cnt-stp-alt-int      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni future            *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-esp-fut      .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-999.
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
      *              * Ciclo per [fit]                                 *
      *              *-------------------------------------------------*
           perform   stp-srt-inp-fit-000  thru stp-srt-inp-fit-999    .
       stp-srt-inp-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-999.
       stp-srt-inp-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *                                                           *
      *    * Ciclo per [fit]                                           *
      *    *-----------------------------------------------------------*
       stp-srt-inp-fit-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore record aggiornati     *
      *              *-------------------------------------------------*
           move      zero                 to   w-wrk-ctr-001          .
      *              *-------------------------------------------------*
      *              * Start su file [fit]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "IDEDOC    "         to   f-key                  .
           move      1080101              to   rf-fit-dat-doc         .
           move      zero                 to   rf-fit-cod-dpz         .
           move      zero                 to   rf-fit-num-doc         .
           move      spaces               to   rf-fit-cod-tmo         .
           move      zero                 to   rf-fit-num-prt         .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-srt-inp-fit-900.
       stp-srt-inp-fit-200.
      *              *-------------------------------------------------*
      *              * Next su [fit]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                  *---------------------------------------------*
      *                  * Se at end : uscita                          *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-srt-inp-fit-900.
       stp-srt-inp-fit-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
       stp-srt-inp-fit-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rf-fit-cod-dpz       not  = 01
                     go to stp-srt-inp-fit-200.
      *                  *---------------------------------------------*
      *                  * Test su tipo documento                      *
      *                  *---------------------------------------------*
           if        rf-fit-cod-tmo       not  = "NFA  "
                     go to stp-srt-inp-fit-200.
      *                  *---------------------------------------------*
      *                  * Test su coordinate documento                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Trasformazione Tipo documento           *
      *                      *-----------------------------------------*
           move      rf-fit-cod-tmo       to   w-det-upd-tpr          .
      *                      *-----------------------------------------*
      *                      * Trasformazione data                     *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rf-fit-dat-doc       to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-det-upd-dpr          .
      *                      *-----------------------------------------*
      *                      * Trasformazione numero                   *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      rf-fit-num-doc
                    (06 : 06)             to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-det-upd-npr          .
      *                      *-----------------------------------------*
      *                      * Ricerca                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione indice              *
      *                          *-------------------------------------*
           set       w-det-upd-fit-inx    to   1                      .
      *                          *-------------------------------------*
      *                          * Test                                *
      *                          *-------------------------------------*
           search    w-det-upd-buf
                     when    w-det-upd-tmo
                            (w-det-upd-fit-inx)
                                          =    w-det-upd-tpr and
                             w-det-upd-dat
                            (w-det-upd-fit-inx)
                                          =    w-det-upd-dpr and
                             w-det-upd-num
                            (w-det-upd-fit-inx)
                                          =    w-det-upd-npr
                     go to   stp-srt-inp-fit-600.
      *                          *-------------------------------------*
      *                          * Se non trovato                      *
      *                          *-------------------------------------*
           go to     stp-srt-inp-fit-200.
       stp-srt-inp-fit-600.
      *              *-------------------------------------------------*
      *              * Composizione record di sort                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   srt-rec                .
      *                  *---------------------------------------------*
      *                  * Chiave per il sort                          *
      *                  *---------------------------------------------*
           move      rf-fit-num-prt       to   srt-num-prt            .
           move      rf-fit-dat-doc       to   srt-dat-doc            .
           move      rf-fit-num-doc       to   srt-num-doc            .
       stp-srt-inp-fit-700.
      *                  *---------------------------------------------*
      *                  * Rilascio record al sort                     *
      *                  *---------------------------------------------*
           release   srt-rec                                          .
       stp-srt-inp-fit-800.
      *                  *---------------------------------------------*
      *                  * Riciclo a record [fit] successivo           *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-fit-200.
       stp-srt-inp-fit-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-fit-999.
       stp-srt-inp-fit-999.
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
           move      "Nessun elemento entro i limiti assegnati !"
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
       prn-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Esecuzione per fine ciclo          *
      *    *-----------------------------------------------------------*
       prn-fin-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Test su contatore                               *
      *              *-------------------------------------------------*
           if        w-wrk-ctr-001        not  > zero
                     go to prn-fin-cic-999.
      *              *-------------------------------------------------*
      *              * Editing contatore                               *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      w-wrk-ctr-001        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Messaggio di fine esecuzione                    *
      *              *-------------------------------------------------*
           move      "WR"                 to   m-ope                  .
           move      spaces               to   m-msg                  .
           string    "Controllata nr. "
                                delimited by   size
                     p-edt      delimited by   spaces
                     " fattura 2003"
                                delimited by   size
                                          into m-msg                  .
           call      "swd/mod/prg/obj/mmessg"
                                         using m                      .
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
       prn-liv-det-100.
      *              *-------------------------------------------------*
      *              * Update record [fit]                             *
      *              *-------------------------------------------------*
           perform   agg-rec-fit-000      thru agg-rec-fit-999        .
       prn-liv-det-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-999.
       prn-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Update record [fit]                                       *
      *    *-----------------------------------------------------------*
       agg-rec-fit-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione                                 *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *              *-------------------------------------------------*
      *              * Lettura con lock                                *
      *              *-------------------------------------------------*
           move      "GK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      srt-num-prt          to   rf-fit-num-prt         .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : ad uscita           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to agg-rec-fit-900.
       agg-rec-fit-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento record [fit]                      *
      *              *-------------------------------------------------*
           move      "NFAFO"              to   rf-fit-cod-tmo         .
       agg-rec-fit-400.
      *              *-------------------------------------------------*
      *              * Aggiornamento contatore                         *
      *              *-------------------------------------------------*
           add       1                    to   w-wrk-ctr-001          .
      *              *-------------------------------------------------*
      *              * Update record                                   *
      *              *-------------------------------------------------*
           move      "UP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
       agg-rec-fit-500.
      *              *-------------------------------------------------*
      *              * Release record                                  *
      *              *-------------------------------------------------*
           move      "RL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/ioffit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fit                 .
       agg-rec-fit-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     agg-rec-fit-999.
       agg-rec-fit-999.
           exit.

      *    *===========================================================*
      *    * Intestazione foglio                                       *
      *    *-----------------------------------------------------------*
       int-pag-sta-000.
       int-pag-sta-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * 'all-str-asx-000/999'                                     *
      *    * 'all-str-adx-000/999'                                     *
      *    * 'all-str-cen-000/999'                                     *
      *    *                                                           *
      *    * Routines per l'allineamento di una stringa a sx o dx o al *
      *    * centro                                                    *
      *    *                                                           *
      *    * Input  : w-all-str-lun = Lunghezza massima della stringa  *
      *    *                                                           *
      *    *          w-all-str-alf = Valore della stringa da allinea- *
      *    *                          re                               *
      *    *                                                           *
      *    * Output : w-all-str-alf = Valore della stringa allineata   *
      *    *                          a sinistra                       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
      *    *                                                           *
      *    * 'all-str-cat-000/999' - (senza uno spazio di separazione) *
      *    * 'all-str-csb-000/999' - (con uno spazio di separazione)   *
      *    *                                                           *
      *    * Routines per il concatenamento di max 10 stringhe di max  *
      *    * 80 caratteri ciascuna con o senza uno spazio di separa-   *
      *    * zione tra una stringa e l'altra                           *
      *    *                                                           *
      *    *                                                           *
      *    * Input  : w-all-str-lun     = Lunghezza massima della      *
      *    *                              stringa concatenata          *
      *    *                                                           *
      *    *          w-all-str-num     = Numero delle stringhe da     *
      *    *                              concatenare                  *
      *    *                                                           *
      *    *          w-all-str-cat (i) = Valore delle stringhe da     *
      *    *                              concatenare                  *
      *    *                                                           *
      *    * Output : w-all-str-alf     = Valore della stringa con-    *
      *    *                              catenata, allineata a si-    *
      *    *                              nistra                       *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

