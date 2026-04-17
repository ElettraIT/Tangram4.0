       Identification division.
       Program-Id.                                 pscf5801           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    scf                 *
      *                                Settore:    glo                 *
      *                                   Fase:    scf580              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 09/07/96    *
      *                       Ultima revisione:    NdK del 11/08/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Esecuzione stampa per il programma scf580   *
      *                                                                *
      *                    Stampa situazione globale acquisti          *
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
      *            * Ragione sociale fornitore per ordinamento         *
      *            *---------------------------------------------------*
               10  srt-rag-key            pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Codice fornitore                                  *
      *            *---------------------------------------------------*
               10  srt-cod-fnt            pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Dati                                                  *
      *        *-------------------------------------------------------*
           05  srt-dat.
      *            *---------------------------------------------------*
      *            * Ragione sociale fornitore                         *
      *            *---------------------------------------------------*
               10  srt-rag-fnt            pic  x(40)                  .

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
                     "scf"                                            .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03) value
                     "glo"                                            .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06) value
                     "scf580"                                         .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10) value
                     "pscf5801"                                       .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40) value
                     "   STAMPA SITUAZIONE GLOBALE ACQUISTI   "       .

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
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

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
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [oft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfoft"                          .
      *        *-------------------------------------------------------*
      *        * [ofr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orf/fls/rec/rfofr"                          .
      *        *-------------------------------------------------------*
      *        * [yfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfyfp"                          .
      *        *-------------------------------------------------------*
      *        * [yin]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfyin"                          .
      *        *-------------------------------------------------------*
      *        * [ybo]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfybo"                          .
      *        *-------------------------------------------------------*
      *        * [zpg]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfzpg"                          .
      *        *-------------------------------------------------------*
      *        * [bft]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbft"                          .
      *        *-------------------------------------------------------*
      *        * [bfr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bfo/fls/rec/rfbfr"                          .
      *        *-------------------------------------------------------*
      *        * [ffr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ffo/fls/rec/rfffr"                          .
      *        *-------------------------------------------------------*
      *        * [sfs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfs"                          .
      *        *-------------------------------------------------------*
      *        * [sfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfp"                          .
      *        *-------------------------------------------------------*
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .

      *    *===========================================================*
      *    * Work-area richieste per stampa                            *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Codice dipendenza in uso                              *
      *        *-------------------------------------------------------*
           05  rr-dpz-inu                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Data di riferimento                                   *
      *        *-------------------------------------------------------*
           05  rr-dat-rif                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo ordinamento stampa                               *
      *        *                                                       *
      *        *  - 'C' : Per codice                                   *
      *        *  - 'R' : Per ragione sociale                          *
      *        *-------------------------------------------------------*
           05  rr-tip-ord                 pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Selezione su fornitori                                *
      *        *-------------------------------------------------------*
           05  rr-cod-min                 pic  9(07)                  .
           05  rr-cod-max                 pic  9(07)                  .
           05  rr-rag-min                 pic  x(20)                  .
           05  rr-rag-max                 pic  x(20)                  .
      *        *-------------------------------------------------------*
      *        * Voci da esporre                                       *
      *        *  - Spaces : No                                        *
      *        *  - X      : Si                                        *
      *        *-------------------------------------------------------*
           05  rr-vde-stp.
      *            *---------------------------------------------------*
      *            * Ordini inevasi                                    *
      *            *---------------------------------------------------*
               10  rr-vde-stp-orf         pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Bolle da fatturare                                *
      *            *---------------------------------------------------*
               10  rr-vde-stp-bfo         pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Scadenze in portafoglio                           *
      *            *---------------------------------------------------*
               10  rr-vde-stp-scf         pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Stampa dettaglio                                      *
      *        *                                                       *
      *        *  - '1' : No                                           *
      *        *  - '2' : Si                                           *
      *        *-------------------------------------------------------*
           05  rr-sns-det                 pic  9(01)                  .
      *        *-------------------------------------------------------*
      *        * Si/no esecuzione batch                                *
      *        *-------------------------------------------------------*
           05  rr-snx-btc                 pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazioni relative alle spese per la fattura- *
      *        * zione                                                 *
      *        *-------------------------------------------------------*
           05  w-prs-spe-fat.
      *            *---------------------------------------------------*
      *            * Tipo accettazione                                 *
      *            *---------------------------------------------------*
               10  w-prs-spe-fat-tac      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Numero di spese personalizzate totale             *
      *            *---------------------------------------------------*
               10  w-prs-spe-fat-nst      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Tabella spese personalizzate, impaccate in alto,  *
      *            * le spese non personalizzate non sono contenute    *
      *            * nella tabella                                     *
      *            *---------------------------------------------------*
               10  w-prs-spe-fat-tbl occurs 06.
      *                *-----------------------------------------------*
      *                * Numero spesa : 1..6                           *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-npt  pic  9(01)                  .
      *                *-----------------------------------------------*
      *                * Descrizione per il video estesa               *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-dve  pic  x(25)                  .
      *                *-----------------------------------------------*
      *                * Tipo funzionamento spesa                      *
      *                * - 01 : A percentuale fissa per tutti i clien- *
      *                *        ti cui viene addebitata la spesa       *
      *                * - 02 : A percentuale variabile per ogni cli-  *
      *                *        ente cui viene addebitata la spesa,    *
      *                *        con la percentuale di spesa espressa   *
      *                *        nel record dati commerciali cliente    *
      *                * - 03 : Ad importo fisso per tutti i clienti   *
      *                *        cui viene addebitata la spesa          *
      *                * - 04 : Ad importo variabile per tutti i cli-  *
      *                *        enti cui viene addebitata la spesa,    *
      *                *        con l'importo della spesa espresso nel *
      *                *        record dati commerciali cliente        *
      *                * - 05 : A seconda dei clienti, per ognuno dei  *
      *                *        quali si esprimera', nel record dati   *
      *                *        commerciali, se la spesa :             *
      *                *        - non va' addebitata                   *
      *                *        - va' addebitata in percentuale        *
      *                *        - va' addebitata ad importo            *
      *                *        e dove inoltre si esprimera' l'even-   *
      *                *        tuale percentuale o importo da addebi- *
      *                *        tare                                   *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-tfs  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Percentuale fissa per la spesa, in caso che   *
      *                * il tipo funzionamento spesa sia pari a 01,    *
      *                * oppure di default per l'anagrafica dati com-  *
      *                * merciali cliente nel caso che il tipo fun-    *
      *                * zionamento spesa sia pari a 02                *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-per  pic  9(02)v9(01)            .
      *                *-----------------------------------------------*
      *                * Imponibile per la percentuale della spesa,    *
      *                * solo se il tipo funzionamento spesa e' pari a *
      *                * 01 o 02 o 05                                  *
      *                * - 00 : Non significativo                      *
      *                * - 01 : Totale merce                           *
      *                * - 02 : Totale netto                           *
      *                * - 03 : Su una combinazione delle voci seguenti*
      *                * - 11 : Totale merci                           *
      *                * - 12 : Totale servizi                         *
      *                * - 13 : Totale imballi                         *
      *                * - 14 : Totale libero 4                        *
      *                * - 15 : Totale libero 5                        *
      *                * - 16 : Totale libero 6                        *
      *                * - 17 : Totale libero 7                        *
      *                * - 18 : Totale libero 8                        *
      *                * - 19 : Totale extra                           *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-ibl  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Totalizzatori da considerarsi per la determi- *
      *                * nazione dell'imponibile per la percentuale    *
      *                * della spesa                                   *
      *                *                                               *
      *                * Solo se l'imponibile per la percentuale e'    *
      *                * pari a 03                                     *
      *                *                                               *
      *                * Per ogni totalizzatore deve essere specifica- *
      *                * to un valore di un carattere, dove :          *
      *                *                                               *
      *                * - N : Non concorre a formare l'imponibile     *
      *                * - S : Concorre a formare l'imponibile         *
      *                *                                               *
      *                * Il significato dei totalizzatori e' di tipo   *
      *                * posizionale, con la seguente codifica :       *
      *                *                                               *
      *                * 1 : Totale merci                              *
      *                * 2 : Totale servizi                            *
      *                * 3 : Totale imballi                            *
      *                * 4 : Totale libero 4                           *
      *                * 5 : Totale libero 5                           *
      *                * 6 : Totale libero 6                           *
      *                * 7 : Totale libero 7                           *
      *                * 8 : Totale libero 8                           *
      *                * 9 : Totale extra                              *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-ibt.
                       20  w-prs-spe-fat-ibx occurs 09
                                          pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Importo fisso per la spesa in caso che il ti- *
      *                * po funzionamento spesa e' pari a 03, oppure   *
      *                * di default per l'anagrafica dati commerciali  *
      *                * cliente nel caso che il tipo funzionamento    *
      *                * spesa sia pari a 04                           *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-imp  pic  9(09)                  .
      *                *-----------------------------------------------*
      *                * Codice iva per la spesa. Se zero significa che*
      *                * la spesa non e' soggetta ad una aliquota iva  *
      *                * fissa, bensi' che l'importo della spesa va'   *
      *                * sventagliato in proporzione agli altri imponi-*
      *                * bili di fattura                               *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-civ  pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Codice contropartita per la spesa. Se zero si-*
      *                * gnifica che la spesa non ha una contropartita *
      *                * fissa, bensi' che l'importo della spesa va'   *
      *                * sventagliato in proporzione alle altre contro-*
      *                * partite di fattura                            *
      *                *-----------------------------------------------*
                   15  w-prs-spe-fat-ccp  pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Contatori, indici, e comodi locali                *
      *            *---------------------------------------------------*
               10  w-prs-spe-fat-i01      pic  9(01)                  .
               10  w-prs-spe-fat-i02      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Referenze relative allo sconto in chiusura            *
      *        *-------------------------------------------------------*
           05  w-ref-sco-chi.
      *            *---------------------------------------------------*
      *            * Descrizione a video sconto in chiusura            *
      *            *---------------------------------------------------*
               10  w-ref-sco-chi-des      pic  x(25)                  .
      *            *---------------------------------------------------*
      *            * Codice iva sconto in chiusura                     *
      *            *---------------------------------------------------*
               10  w-ref-sco-chi-civ      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contropartita sconto in chiusura                  *
      *            *---------------------------------------------------*
               10  w-ref-sco-chi-ccp      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Referenze relative allo sconto pagamento              *
      *        *-------------------------------------------------------*
           05  w-ref-sco-pag.
      *            *---------------------------------------------------*
      *            * Descrizione a video sconto pagamento              *
      *            *---------------------------------------------------*
               10  w-ref-sco-pag-des      pic  x(25)                  .
      *            *---------------------------------------------------*
      *            * Codice iva sconto pagamento                       *
      *            *---------------------------------------------------*
               10  w-ref-sco-pag-civ      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Contropartita sconto pagamento                    *
      *            *---------------------------------------------------*
               10  w-ref-sco-pag-ccp      pic  9(07)                  .

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
      *        * Work per determinazione status scadenza fornitore con *
      *        * riferimento ad una certa data                         *
      *        *-------------------------------------------------------*
           05  w-det-srd-scf.
      *            *---------------------------------------------------*
      *            * Data di riferimento per la determinazione         *
      *            *---------------------------------------------------*
               10  w-det-srd-scf-drd      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Status determinato                                *
      *            * - Spaces : Status non determinabile               *
      *            * - A      : Scadenza ancora aperta                 *
      *            * - C      : Scadenza ormai chiusa                  *
      *            * - N      : Scadenza non ancora inserita alla data *
      *            *            di riferimento per la determinazione   *
      *            *---------------------------------------------------*
               10  w-det-srd-scf-sts      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Sigla ultima operazione eseguita sulla scadenza   *
      *            * - Spaces : Nessuna                                *
      *            * - REG    : Registrazione                          *
      *            * - SSC    : Storno                                 *
      *            * - PAG    : Pagamento                              *
      *            *---------------------------------------------------*
               10  w-det-srd-scf-suo      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione status bolla fornitore        *
      *        *-------------------------------------------------------*
           05  w-det-sts-bfo.
      *            *---------------------------------------------------*
      *            * Flag di uscita dalla determinazione               *
      *            *---------------------------------------------------*
               10  w-det-sts-bfo-flg      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Contatore elementi                                *
      *            *---------------------------------------------------*
               10  w-det-sts-bfo-ctr      pic  9(03)                  .
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
               10  w-det-imp-rig-prz      pic  9(09)v9(03)            .
      *            *---------------------------------------------------*
      *            * Decimali prezzo                                   *
      *            *---------------------------------------------------*
               10  w-det-imp-rig-dec      pic  9(01)                  .
      *            *---------------------------------------------------*
      *            * Importo                                           *
      *            *---------------------------------------------------*
               10  w-det-imp-rig-imp      pic s9(11)                  .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione imposta in base  *
      *    * ad un imponibile                                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/dimpiva0.dtl"                   .

      *    *===========================================================*
      *    * Work-area per operazioni sulle date                       *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wnrgdat0.cpw"                   .

      *    *===========================================================*
      *    * Work per subroutines di editing codice iva                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wkl"                   .

      *    *===========================================================*
      *    * Work area per le date di elaborazione                     *
      *    *-----------------------------------------------------------*
       01  w-stp-dat-inp.
      *        *-------------------------------------------------------*
      *        * Data di riferimento                                   *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-rif          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Mese di riferimento                                   *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-mrf          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Secolo / anno di riferimento                          *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-srf          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Data di inizio mese di riferimento                    *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-imr          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mese di riferimento                      *
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
      *        * Data di inizio mesi successivi                        *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-ims          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mesi successivi                          *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-fms          pic  9(07)                  .

      *    *===========================================================*
      *    * Work area per stampa totali generali                      *
      *    *-----------------------------------------------------------*
       01  w-stp-tot.
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa totali                            *
      *        *-------------------------------------------------------*
           05  w-stp-tot-tot.
      *            *---------------------------------------------------*
      *            * Contatore fornitori trattati                      *
      *            *---------------------------------------------------*
               10  w-stp-tot-tot-fnt      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Contatore di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-tot-tot-ctr      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Totali relativi alle scansioni                    *
      *            *---------------------------------------------------*
               10  w-stp-tot-tot-rls occurs 3.
      *                *-----------------------------------------------*
      *                * Cumulo precedenti                             *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-pre  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese in corso                          *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-mic  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 1                      *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-ms1  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 2                      *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-ms2  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 3                      *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-ms3  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 4                      *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-ms4  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 5                      *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-ms5  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 6                      *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-ms6  pic s9(13)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mesi successivi oltre                  *
      *                *-----------------------------------------------*
                   15  w-stp-tot-tot-mso  pic s9(13)                  .

      *    *===========================================================*
      *    * Work area per stampa a livello fornitore                  *
      *    *-----------------------------------------------------------*
       01  w-stp-lvf.
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa anagrafica fornitore              *
      *        *-------------------------------------------------------*
           05  w-stp-lvf-fnt.
      *            *---------------------------------------------------*
      *            * Codice fornitore                                  *
      *            *---------------------------------------------------*
               10  w-stp-lvf-fnt-cod      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Ragione sociale fornitore                         *
      *            *---------------------------------------------------*
               10  w-stp-lvf-fnt-rag      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa totali fornitore                  *
      *        *-------------------------------------------------------*
           05  w-stp-lvf-tot.
      *            *---------------------------------------------------*
      *            * Totali relativi alle scansioni                    *
      *            *---------------------------------------------------*
               10  w-stp-lvf-tot-rls.
      *                *-----------------------------------------------*
      *                * Cumulo precedenti                             *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-tot-pre  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese in corso                          *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-tot-mic  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 1                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-tot-ms1  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 2                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-tot-ms2  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 3                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-tot-ms3  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 4                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-tot-ms4  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 5                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-tot-ms5  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 6                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-tot-ms6  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mesi successivi oltre                  *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-tot-mso  pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Contatore di comodo per subtotali                 *
      *            *---------------------------------------------------*
               10  w-stp-lvf-sbt-ctr      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Subtotali relativi alle scansioni                 *
      *            *---------------------------------------------------*
               10  w-stp-lvf-sbt-rls occurs 3.
      *                *-----------------------------------------------*
      *                * Cumulo precedenti                             *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-sbt-pre  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese in corso                          *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-sbt-mic  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 1                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-sbt-ms1  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 2                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-sbt-ms2  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 3                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-sbt-ms3  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 4                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-sbt-ms4  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 5                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-sbt-ms5  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mese successivo 6                      *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-sbt-ms6  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Cumulo mesi successivi oltre                  *
      *                *-----------------------------------------------*
                   15  w-stp-lvf-sbt-mso  pic s9(11)                  .

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
      *            * Valore numerico da stampare                       *
      *            *---------------------------------------------------*
               10  w-stp-val-num-val      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Valore da stampare editato                        *
      *            *---------------------------------------------------*
               10  w-stp-val-num-edt      pic  x(15)                  .
      *            *---------------------------------------------------*
      *            * Eventuale riempitivo                              *
      *            *---------------------------------------------------*
               10  w-stp-val-num-rmp      pic  x(01)                  .

      *    *===========================================================*
      *    * Work area per cumulo ordini inevasi fornitore             *
      *    *-----------------------------------------------------------*
       01  w-stp-orf.
      *        *-------------------------------------------------------*
      *        * Sub-work per cumulo valori                            *
      *        *-------------------------------------------------------*
           05  w-stp-orf-wrk.
      *            *---------------------------------------------------*
      *            * Numero elementi rilevati                          *
      *            *---------------------------------------------------*
               10  w-stp-orf-wrk-ele      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-orf-wrk-ctr      pic  9(03)                  .
               10  w-stp-orf-wrk-ct1      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo per numero documento                       *
      *            *---------------------------------------------------*
               10  w-stp-orf-wrk-doc      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Comodo per prezzo unitario                        *
      *            *---------------------------------------------------*
               10  w-stp-orf-wrk-ppi      pic  9(09)v9(09)            .
      *            *---------------------------------------------------*
      *            * Comodo per salvataggio numero documento           *
      *            *---------------------------------------------------*
               10  w-stp-orf-wrk-svd      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Comodo per tranche documento                      *
      *            *---------------------------------------------------*
               10  w-stp-orf-wrk-tdc      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Massimo elementi                                  *
      *            *---------------------------------------------------*
               10  w-stp-orf-wrk-max      pic  9(03)    value 500     .
      *            *---------------------------------------------------*
      *            * Tabella elementi                                  *
      *            *---------------------------------------------------*
               10  w-stp-orf-wrk-tbl.
      *                *-----------------------------------------------*
      *                * Singole righe che compongono la tabella       *
      *                *-----------------------------------------------*
                   15  w-stp-orf-wrk-rig occurs 500.
      *                    *-------------------------------------------*
      *                    * Estremi ordine                            *
      *                    *-------------------------------------------*
                       20  w-stp-orf-cod-dpz
                                          pic  9(02)                  .
                       20  w-stp-orf-tip-doc
                                          pic  x(05)                  .
                       20  w-stp-orf-num-doc
                                          pic  9(11)                  .
                       20  w-stp-orf-dat-doc
                                          pic  9(07)                  .
      *                    *-------------------------------------------*
      *                    * Cumulo precedenti                         *
      *                    *-------------------------------------------*
                       20  w-stp-orf-cum-pre
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese in corso                      *
      *                    *-------------------------------------------*
                       20  w-stp-orf-cum-mic
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 1                  *
      *                    *-------------------------------------------*
                       20  w-stp-orf-cum-ms1
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 2                  *
      *                    *-------------------------------------------*
                       20  w-stp-orf-cum-ms2
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 3                  *
      *                    *-------------------------------------------*
                       20  w-stp-orf-cum-ms3
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 4                  *
      *                    *-------------------------------------------*
                       20  w-stp-orf-cum-ms4
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 5                  *
      *                    *-------------------------------------------*
                       20  w-stp-orf-cum-ms5
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 6                  *
      *                    *-------------------------------------------*
                       20  w-stp-orf-cum-ms6
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mesi successivi oltre              *
      *                    *-------------------------------------------*
                       20  w-stp-orf-cum-mso
                                          pic s9(11)       comp-3     .

      *    *===========================================================*
      *    * Work area per cumulo bolle da fatturare fornitore         *
      *    *-----------------------------------------------------------*
       01  w-stp-bfo.
      *        *-------------------------------------------------------*
      *        * Sub-work per cumulo valori                            *
      *        *-------------------------------------------------------*
           05  w-stp-bfo-wrk.
      *            *---------------------------------------------------*
      *            * Numero elementi rilevati                          *
      *            *---------------------------------------------------*
               10  w-stp-bfo-wrk-ele      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-bfo-wrk-ctr      pic  9(03)                  .
               10  w-stp-bfo-wrk-ct1      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo per numero documento                       *
      *            *---------------------------------------------------*
               10  w-stp-bfo-wrk-doc      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Comodo per prezzo unitario                        *
      *            *---------------------------------------------------*
               10  w-stp-bfo-wrk-ppi      pic  9(09)v9(09)            .
      *            *---------------------------------------------------*
      *            * Massimo elementi                                  *
      *            *---------------------------------------------------*
               10  w-stp-bfo-wrk-max      pic  9(03)    value 500     .
      *            *---------------------------------------------------*
      *            * Tabella elementi                                  *
      *            *---------------------------------------------------*
               10  w-stp-bfo-wrk-tbl.
      *                *-----------------------------------------------*
      *                * Singole righe che compongono la tabella       *
      *                *-----------------------------------------------*
                   15  w-stp-bfo-wrk-rig occurs 500.
      *                    *-------------------------------------------*
      *                    * Estremi bolla                             *
      *                    *-------------------------------------------*
                       20  w-stp-bfo-cod-dpz
                                          pic  9(02)                  .
                       20  w-stp-bfo-tip-doc
                                          pic  x(05)                  .
                       20  w-stp-bfo-num-doc
                                          pic  9(11)                  .
                       20  w-stp-bfo-dat-doc
                                          pic  9(07)                  .
      *                    *-------------------------------------------*
      *                    * Cumulo precedenti                         *
      *                    *-------------------------------------------*
                       20  w-stp-bfo-cum-pre
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese in corso                      *
      *                    *-------------------------------------------*
                       20  w-stp-bfo-cum-mic
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 1                  *
      *                    *-------------------------------------------*
                       20  w-stp-bfo-cum-ms1
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 2                  *
      *                    *-------------------------------------------*
                       20  w-stp-bfo-cum-ms2
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 3                  *
      *                    *-------------------------------------------*
                       20  w-stp-bfo-cum-ms3
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 4                  *
      *                    *-------------------------------------------*
                       20  w-stp-bfo-cum-ms4
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 5                  *
      *                    *-------------------------------------------*
                       20  w-stp-bfo-cum-ms5
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 6                  *
      *                    *-------------------------------------------*
                       20  w-stp-bfo-cum-ms6
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mesi successivi oltre              *
      *                    *-------------------------------------------*
                       20  w-stp-bfo-cum-mso
                                          pic s9(11)       comp-3     .

      *    *===========================================================*
      *    * Work area per cumulo scadenze in portafoglio fornitore    *
      *    *-----------------------------------------------------------*
       01  w-stp-scf.
      *        *-------------------------------------------------------*
      *        * Sub-work per cumulo valori                            *
      *        *-------------------------------------------------------*
           05  w-stp-scf-wrk.
      *            *---------------------------------------------------*
      *            * Numero elementi rilevati                          *
      *            *---------------------------------------------------*
               10  w-stp-scf-wrk-ele      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Contatore di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-scf-wrk-ctr      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Massimo elementi                                  *
      *            *---------------------------------------------------*
               10  w-stp-scf-wrk-max      pic  9(03)    value 500     .
      *            *---------------------------------------------------*
      *            * Tabella elementi                                  *
      *            *---------------------------------------------------*
               10  w-stp-scf-wrk-tbl.
      *                *-----------------------------------------------*
      *                * Singole righe che compongono la tabella       *
      *                *-----------------------------------------------*
                   15  w-stp-scf-wrk-rig occurs 500.
      *                    *-------------------------------------------*
      *                    * Estremi scadenza                          *
      *                    *-------------------------------------------*
                       20  w-stp-scf-tip-doc
                                          pic  x(05)                  .
                       20  w-stp-scf-num-doc
                                          pic  9(11)                  .
                       20  w-stp-scf-dat-doc
                                          pic  9(07)                  .
      *                    *-------------------------------------------*
      *                    * Cumulo precedenti                         *
      *                    *-------------------------------------------*
                       20  w-stp-scf-cum-pre
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese in corso                      *
      *                    *-------------------------------------------*
                       20  w-stp-scf-cum-mic
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 1                  *
      *                    *-------------------------------------------*
                       20  w-stp-scf-cum-ms1
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 2                  *
      *                    *-------------------------------------------*
                       20  w-stp-scf-cum-ms2
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 3                  *
      *                    *-------------------------------------------*
                       20  w-stp-scf-cum-ms3
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 4                  *
      *                    *-------------------------------------------*
                       20  w-stp-scf-cum-ms4
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 5                  *
      *                    *-------------------------------------------*
                       20  w-stp-scf-cum-ms5
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mese successivo 6                  *
      *                    *-------------------------------------------*
                       20  w-stp-scf-cum-ms6
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mesi successivi oltre              *
      *                    *-------------------------------------------*
                       20  w-stp-scf-cum-mso
                                          pic s9(11)       comp-3     .

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
      *    * Work area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-prz-acq              pic  9(09)                  .

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
      *    * Work area per totali ordine                               *
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
      *    * Work-area per bufferizzazione righe ordine fornitore      *
      *    *-----------------------------------------------------------*
       01  w-rof.
      *        *-------------------------------------------------------*
      *        * Numero massimo di elementi                            *
      *        *-------------------------------------------------------*
           05  w-rof-max-ele              pic  9(05)     value 999    .
      *        *-------------------------------------------------------*
      *        * Contatore elementi                                    *
      *        *-------------------------------------------------------*
           05  w-rof-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-rof-ctr-ele              pic  9(05)                  .
           05  w-rof-ctr-001              pic  9(05)                  .
           05  w-rof-ctr-002              pic  9(05)                  .
           05  w-rof-ctr-003              pic  9(05)                  .
           05  w-rof-ctr-004              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Protocollo                                            *
      *        *-------------------------------------------------------*
           05  w-rof-num-prt              pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio chiave di ordinamento          *
      *        *-------------------------------------------------------*
           05  w-rof-sav-key              pic  x(12)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per rottura su data consegna prevista          *
      *        *-------------------------------------------------------*
           05  w-rof-rot-prv              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Castelletto progressivi riga salvati                  *
      *        *-------------------------------------------------------*
           05  w-rof-cst-rig.
      *            *---------------------------------------------------*
      *            * Singolo elemento                                  *
      *            *---------------------------------------------------*
               10  w-rof-sng-ele  occurs 999.
      *                *-----------------------------------------------*
      *                * Chiave di ordinamento                         *
      *                *-----------------------------------------------*
                   15  w-rof-key-ord.
      *                    *-------------------------------------------*
      *                    * Data consegna prevista                    *
      *                    *-------------------------------------------*
                       20  w-rof-key-prv  pic  9(07)                  .
      *                    *-------------------------------------------*
      *                    * Numero progressivo riga                   *
      *                    *-------------------------------------------*
                       20  w-rof-key-prg  pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Dati del buffer                               *
      *                *-----------------------------------------------*
                   15  w-rof-dti-buf.
      *                    *-------------------------------------------*
      *                    * Numero progressivo riga                   *
      *                    *-------------------------------------------*
                       20  filler         pic  x(01)                  .

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
      *    * Area di comunicazione per determinazione stato ordine     *
      *    * fornitore                                                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dstsorf0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine fornitore                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dqevrof0.dtl"                   .

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
               10  w-rot-l01-cod-fnt      pic  9(07)                  .

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
      *              * Test se esecuzione batch                        *
      *              *-------------------------------------------------*
           if        rr-snx-btc           =    "S"
                     move  "N"            to   w-cnt-fun-snx-ric
                     move  "N"            to   w-cnt-fun-snx-stp      .
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
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * [oft]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *              *-------------------------------------------------*
      *              * [ofr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *              *-------------------------------------------------*
      *              * [yfp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
      *              *-------------------------------------------------*
      *              * [yin]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yin                 .
      *              *-------------------------------------------------*
      *              * [ybo]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofybo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybo                 .
      *              *-------------------------------------------------*
      *              * [zpg]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
      *              *-------------------------------------------------*
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * [ffr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *              *-------------------------------------------------*
      *              * [sfs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
      *              *-------------------------------------------------*
      *              * [sfp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfp                 .
      *              *-------------------------------------------------*
      *              * [ada]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione coefficiente di cambio *
      *              * valuta                                          *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-opn-000  thru coe-cmb-vlt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione status riga       *
      *              *-------------------------------------------------*
           perform   det-qev-rof-opn-000  thru det-qev-rof-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione status ordine     *
      *              *-------------------------------------------------*
           perform   det-sts-orf-opn-000  thru det-sts-orf-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione imposta           *
      *              *-------------------------------------------------*
           perform   det-imp-iva-opn-000  thru det-imp-iva-opn-999    .
       prn-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Close files                        *
      *    *-----------------------------------------------------------*
       prn-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [fnt]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *              *-------------------------------------------------*
      *              * [oft]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *              *-------------------------------------------------*
      *              * [ofr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *              *-------------------------------------------------*
      *              * [yfp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
      *              *-------------------------------------------------*
      *              * [yin]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofyin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yin                 .
      *              *-------------------------------------------------*
      *              * [ybo]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofybo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybo                 .
      *              *-------------------------------------------------*
      *              * [zpg]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
      *              *-------------------------------------------------*
      *              * [bft]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *              *-------------------------------------------------*
      *              * [bfr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *              *-------------------------------------------------*
      *              * [ffr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *              *-------------------------------------------------*
      *              * [sfs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
      *              *-------------------------------------------------*
      *              * [sfp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfp                 .
      *              *-------------------------------------------------*
      *              * [ada]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione coefficiente di cam-  *
      *              * bio valuta                                      *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-cls-000  thru coe-cmb-vlt-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione status riga      *
      *              *-------------------------------------------------*
           perform   det-qev-rof-cls-000  thru det-qev-rof-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione status ordine    *
      *              *-------------------------------------------------*
           perform   det-sts-orf-cls-000  thru det-sts-orf-cls-999    .
       prn-cls-fls-840.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione imposta          *
      *              *-------------------------------------------------*
           perform   det-imp-iva-cls-000  thru det-imp-iva-cls-999    .
       prn-cls-fls-860.
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
      *              * Test se esecuzione batch                        *
      *              *-------------------------------------------------*
           if        rr-snx-btc           not  = "S"
                     go to stp-srt-inp-050.
      *              *-------------------------------------------------*
      *              * Forzature                                       *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   rr-dat-rif             .
      *
           move      "C"                  to   rr-tip-ord             .
           move      0000000              to   rr-cod-min             .
           move      9999999              to   rr-cod-max             .
           move      spaces               to   rr-rag-min             .
           move      all "z"              to   rr-rag-max             .
           move      all "X"              to   rr-vde-stp             .
           move      1                    to   rr-sns-det             .
       stp-srt-inp-050.
      *              *-------------------------------------------------*
      *              * Subroutine di preparazione date                 *
      *              *-------------------------------------------------*
           perform   stp-srt-inp-dat-000  thru stp-srt-inp-dat-999    .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo ordinamento     *
      *              *-------------------------------------------------*
           if        rr-tip-ord           =    "C"
                     go to stp-srt-inp-100
           else if   rr-tip-ord           =    "R"
                     go to stp-srt-inp-120
           else      go to stp-srt-inp-100.
       stp-srt-inp-100.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start per codice                            *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODFNT    "         to   f-key                  .
           move      rr-cod-min           to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-srt-inp-900.
      *                  *---------------------------------------------*
      *                  * Se Ok                                       *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-200.
       stp-srt-inp-120.
      *              *-------------------------------------------------*
      *              * Se ordinamento per ragione sociale              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Start per ragione sociale in uppercase      *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RAGKEY    "         to   f-key                  .
           move      rr-rag-min           to   rf-fnt-rag-key         .
           move      zero                 to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-srt-inp-900.
      *                  *---------------------------------------------*
      *                  * Se Ok                                       *
      *                  *---------------------------------------------*
           go to     stp-srt-inp-200.
       stp-srt-inp-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio [fnt]              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-srt-inp-900.
       stp-srt-inp-300.
      *              *-------------------------------------------------*
      *              * Max su [fnt]                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo ordinamento *
      *                  *---------------------------------------------*
           if        rr-tip-ord           =    "C"
                     go to stp-srt-inp-320
           else if   rr-tip-ord           =    "R"
                     go to stp-srt-inp-340
           else      go to stp-srt-inp-320.
       stp-srt-inp-320.
      *                  *---------------------------------------------*
      *                  * Test su Codice                              *
      *                  *---------------------------------------------*
           if        rf-fnt-cod-fnt       >    rr-cod-max
                     go to stp-srt-inp-900.
      *
           go to     stp-srt-inp-400.
       stp-srt-inp-340.
      *                  *---------------------------------------------*
      *                  * Test su Ragione sociale                     *
      *                  *---------------------------------------------*
           if        rf-fnt-rag-key       >    rr-rag-max
                     go to stp-srt-inp-900.
      *
           go to     stp-srt-inp-400.
       stp-srt-inp-400.
      *              *-------------------------------------------------*
      *              * Selezioni su [cli]                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Selettori min - max                         *
      *                  *---------------------------------------------*
           if        rf-fnt-cod-fnt       <    rr-cod-min or
                     rf-fnt-cod-fnt       >    rr-cod-max or
                     rf-fnt-rag-key       <    rr-rag-min or
                     rf-fnt-rag-key       >    rr-rag-max
                     go to  stp-srt-inp-200.
       stp-srt-inp-500.
      *              *-------------------------------------------------*
      *              * Composizione record di Sort                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiave del Sort                             *
      *                  *---------------------------------------------*
           if        rr-tip-ord           =    "C"
                     move  spaces         to   srt-rag-key
           else      move  rf-fnt-rag-key to   srt-rag-key            .
      *
           move      rf-fnt-cod-fnt       to   srt-cod-fnt            .
      *                  *---------------------------------------------*
      *                  * Dati del Sort                               *
      *                  *---------------------------------------------*
           move      rf-fnt-rag-soc       to   srt-rag-fnt            .
       stp-srt-inp-600.
      *              *-------------------------------------------------*
      *              * Rilascio del record al Sort                     *
      *              *-------------------------------------------------*
           release   srt-rec                                          .
       stp-srt-inp-800.
      *              *-------------------------------------------------*
      *              * Test se batch                                   *
      *              *-------------------------------------------------*
           if        rr-snx-btc           =    "S"
                     go to stp-srt-inp-820.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       stp-srt-inp-820.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     stp-srt-inp-200.
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
           move      zero                 to   w-stp-dat-inp-ims      .
           move      zero                 to   w-stp-dat-inp-fms      .
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
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-imr    to   w-stp-dat-inp-im1      .
           move      w-stp-dat-inp-im1    to   s-dat                  .
           add       1                    to   s-mes                  .
           if        s-mes                >    12
                     add  1               to   s-saa
                     move 1               to   s-mes                  .
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
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im1    to   w-stp-dat-inp-im2      .
           move      w-stp-dat-inp-im2    to   s-dat                  .
           add       1                    to   s-mes                  .
           if        s-mes                >    12
                     add  1               to   s-saa
                     move 1               to   s-mes                  .
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
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im2    to   w-stp-dat-inp-im3      .
           move      w-stp-dat-inp-im3    to   s-dat                  .
           add       1                    to   s-mes                  .
           if        s-mes                >    12
                     add  1               to   s-saa
                     move 1               to   s-mes                  .
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
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im3    to   w-stp-dat-inp-im4      .
           move      w-stp-dat-inp-im4    to   s-dat                  .
           add       1                    to   s-mes                  .
           if        s-mes                >    12
                     add  1               to   s-saa
                     move 1               to   s-mes                  .
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
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im4    to   w-stp-dat-inp-im5      .
           move      w-stp-dat-inp-im5    to   s-dat                  .
           add       1                    to   s-mes                  .
           if        s-mes                >    12
                     add  1               to   s-saa
                     move 1               to   s-mes                  .
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
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im5    to   w-stp-dat-inp-im6      .
           move      w-stp-dat-inp-im6    to   s-dat                  .
           add       1                    to   s-mes                  .
           if        s-mes                >    12
                     add  1               to   s-saa
                     move 1               to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-im6      .
      *                  *---------------------------------------------*
      *                  * Fine mese                                   *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im6    to   w-stp-dat-inp-fm6      .
           move      w-stp-dat-inp-fm6    to   s-dat                  .
           move      31                   to   s-gio                  .
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
      *              * Calcolo delle date di inizio e fine mesi suc-   *
      *              * cessivi                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione del primo giorno successivo  *
      *                  * alla data di fine mese 6                    *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-fm6    to   w-det-dat-nrg-dtb      .
           move      1                    to   w-det-dat-nrg-ngi      .
           perform   det-dat-nrg-000      thru det-dat-nrg-999        .
           move      w-det-dat-nrg-dti    to   w-stp-dat-inp-ims      .
      *                  *---------------------------------------------*
      *                  * Fine mesi successivi                        *
      *                  *---------------------------------------------*
           move      9999999              to   w-stp-dat-inp-fms      .
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
           move      "Nessun fornitore entro i limiti assegnati !"
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
      *              * Test se batch                                   *
      *              *-------------------------------------------------*
           if        rr-snx-btc           =    "S"
                     go to prn-let-seq-200.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       prn-let-seq-200.
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
      *              *-------------------------------------------------*
      *              * Rottura su codice fornitore                     *
      *              *-------------------------------------------------*
           move      srt-cod-fnt          to   w-rot-l01-cod-fnt      .
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
           move      zero                 to   w-stp-tot-tot-fnt      .
           move      zero                 to   w-stp-tot-tot-ctr      .
       prn-ini-cic-120.
           add       1                    to   w-stp-tot-tot-ctr      .
           if        w-stp-tot-tot-ctr    >    3
                     go to prn-ini-cic-300.
           move      zero                 to   w-stp-tot-tot-pre
                                              (w-stp-tot-tot-ctr)     .
           move      zero                 to   w-stp-tot-tot-mic
                                              (w-stp-tot-tot-ctr)     .
           move      zero                 to   w-stp-tot-tot-ms1
                                              (w-stp-tot-tot-ctr)     .
           move      zero                 to   w-stp-tot-tot-ms2
                                              (w-stp-tot-tot-ctr)     .
           move      zero                 to   w-stp-tot-tot-ms3
                                              (w-stp-tot-tot-ctr)     .
           move      zero                 to   w-stp-tot-tot-ms4
                                              (w-stp-tot-tot-ctr)     .
           move      zero                 to   w-stp-tot-tot-ms5
                                              (w-stp-tot-tot-ctr)     .
           move      zero                 to   w-stp-tot-tot-ms6
                                              (w-stp-tot-tot-ctr)     .
           move      zero                 to   w-stp-tot-tot-mso
                                              (w-stp-tot-tot-ctr)     .
           go to     prn-ini-cic-120.
       prn-ini-cic-300.
      *              *-------------------------------------------------*
      *              * Intestazione foglio                             *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *              *-------------------------------------------------*
      *              * Fincatura dettaglio                             *
      *              *-------------------------------------------------*
           perform   int-fin-det-000      thru int-fin-det-999        .
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
       prn-fin-cic-100.
      *              *-------------------------------------------------*
      *              * Test su contatore fornitori                     *
      *              *-------------------------------------------------*
           if        w-stp-tot-tot-fnt    not  > 1
                     go to prn-fin-cic-999.
      *              *-------------------------------------------------*
      *              * Intestazione foglio                             *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *              *-------------------------------------------------*
      *              * Fincatura dettaglio                             *
      *              *-------------------------------------------------*
           perform   int-fin-det-000      thru int-fin-det-999        .
       prn-fin-cic-200.
      *              *-------------------------------------------------*
      *              * Totali generali                                 *
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
           perform   prn-grv-det-000      thru prn-grv-det-999        .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      011                  to   p-pos                  .
           move      "*** TOTALI GENERALI ***                 "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mesi precedenti                             *
      *                  *---------------------------------------------*
           move      052                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-pre (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-pre (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-pre (3)
                                          to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese in corso                               *
      *                  *---------------------------------------------*
           move      068                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-mic (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mic (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mic (3)
                                          to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 1                           *
      *                  *---------------------------------------------*
           move      084                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms1 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms1 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms1 (3)
                                          to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 2                           *
      *                  *---------------------------------------------*
           move      100                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms2 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms2 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms2 (3)
                                          to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 3                           *
      *                  *---------------------------------------------*
           move      116                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms3 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms3 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms3 (3)
                                          to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 4                           *
      *                  *---------------------------------------------*
           move      132                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms4 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms4 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms4 (3)
                                          to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 5                           *
      *                  *---------------------------------------------*
           move      148                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms5 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms5 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms5 (3)
                                          to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 6                           *
      *                  *---------------------------------------------*
           move      164                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms6 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms6 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms6 (3)
                                          to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mesi successivi oltre                       *
      *                  *---------------------------------------------*
           move      180                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-mso (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mso (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mso (3)
                                          to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Totali                                      *
      *                  *---------------------------------------------*
           move      196                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-pre (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-pre (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-pre (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mic (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mic (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mic (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms1 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms1 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms1 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms2 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms2 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms2 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms3 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms3 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms3 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms4 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms4 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms4 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms5 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms5 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms5 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms6 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms6 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms6 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mso (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mso (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mso (3)
                                          to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
       prn-fin-cic-300.
      *              *-------------------------------------------------*
      *              * Subtotali generali - Ordini inevasi             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        rr-vde-stp-orf       =    spaces
                     go to prn-fin-cic-400.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Griglia vuota                               *
      *                  *---------------------------------------------*
           perform   prn-grv-det-000      thru prn-grv-det-999        .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      011                  to   p-pos                  .
           move      "        - Ordini inevasi                "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mesi precedenti                             *
      *                  *---------------------------------------------*
           move      052                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-pre (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese in corso                               *
      *                  *---------------------------------------------*
           move      068                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-mic (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 1                           *
      *                  *---------------------------------------------*
           move      084                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms1 (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 2                           *
      *                  *---------------------------------------------*
           move      100                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms2 (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 3                           *
      *                  *---------------------------------------------*
           move      116                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms3 (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 4                           *
      *                  *---------------------------------------------*
           move      132                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms4 (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 5                           *
      *                  *---------------------------------------------*
           move      148                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms5 (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 6                           *
      *                  *---------------------------------------------*
           move      164                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms6 (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mesi successivi oltre                       *
      *                  *---------------------------------------------*
           move      180                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-mso (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Totali                                      *
      *                  *---------------------------------------------*
           move      196                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-pre (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mic (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms1 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms2 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms3 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms4 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms5 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms6 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mso (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
       prn-fin-cic-400.
      *              *-------------------------------------------------*
      *              * Subtotali generali - Bolle da fatturare         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        rr-vde-stp-bfo       =    spaces
                     go to prn-fin-cic-500.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Griglia vuota                               *
      *                  *---------------------------------------------*
           perform   prn-grv-det-000      thru prn-grv-det-999        .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      011                  to   p-pos                  .
           move      "        - Bolle da fatturare            "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mesi precedenti                             *
      *                  *---------------------------------------------*
           move      052                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-pre (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese in corso                               *
      *                  *---------------------------------------------*
           move      068                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-mic (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 1                           *
      *                  *---------------------------------------------*
           move      084                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms1 (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 2                           *
      *                  *---------------------------------------------*
           move      100                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms2 (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 3                           *
      *                  *---------------------------------------------*
           move      116                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms3 (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 4                           *
      *                  *---------------------------------------------*
           move      132                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms4 (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 5                           *
      *                  *---------------------------------------------*
           move      148                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms5 (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 6                           *
      *                  *---------------------------------------------*
           move      164                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms6 (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mesi successivi oltre                       *
      *                  *---------------------------------------------*
           move      180                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-mso (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Totali                                      *
      *                  *---------------------------------------------*
           move      196                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-pre (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mic (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms1 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms2 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms3 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms4 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms5 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms6 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mso (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
       prn-fin-cic-500.
      *              *-------------------------------------------------*
      *              * Subtotali generali - Scadenze in portafoglio    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        rr-vde-stp-scf       =    spaces
                     go to prn-fin-cic-900.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Griglia vuota                               *
      *                  *---------------------------------------------*
           perform   prn-grv-det-000      thru prn-grv-det-999        .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      011                  to   p-pos                  .
           move      "        - Scadenze in portafoglio       "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mesi precedenti                             *
      *                  *---------------------------------------------*
           move      052                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-pre (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese in corso                               *
      *                  *---------------------------------------------*
           move      068                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-mic (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 1                           *
      *                  *---------------------------------------------*
           move      084                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms1 (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 2                           *
      *                  *---------------------------------------------*
           move      100                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms2 (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 3                           *
      *                  *---------------------------------------------*
           move      116                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms3 (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 4                           *
      *                  *---------------------------------------------*
           move      132                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms4 (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 5                           *
      *                  *---------------------------------------------*
           move      148                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms5 (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 6                           *
      *                  *---------------------------------------------*
           move      164                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-ms6 (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mesi successivi oltre                       *
      *                  *---------------------------------------------*
           move      180                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-mso (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Totali                                      *
      *                  *---------------------------------------------*
           move      196                  to   w-stp-val-num-pos      .
           move      w-stp-tot-tot-pre (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mic (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms1 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms2 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms3 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms4 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms5 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-ms6 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-tot-tot-mso (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
       prn-fin-cic-900.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Sopralineatura                                  *
      *              *-------------------------------------------------*
           perform   prn-stl-det-000      thru prn-stl-det-999        .
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
      *              *-------------------------------------------------*
      *              * Bufferizzazione estremi fornitore               *
      *              *-------------------------------------------------*
           move      w-rot-l01-cod-fnt    to   w-stp-lvf-fnt-cod      .
           move      rf-fnt-rag-soc       to   w-stp-lvf-fnt-rag      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-orf-wrk-ele      .
           move      zero                 to   w-stp-bfo-wrk-ele      .
           move      zero                 to   w-stp-scf-wrk-ele      .
       prn-ini-lr1-100.
      *              *-------------------------------------------------*
      *              * Normalizzazioni totali per fornitore            *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-lvf-tot-pre      .
           move      zero                 to   w-stp-lvf-tot-mic      .
           move      zero                 to   w-stp-lvf-tot-ms1      .
           move      zero                 to   w-stp-lvf-tot-ms2      .
           move      zero                 to   w-stp-lvf-tot-ms3      .
           move      zero                 to   w-stp-lvf-tot-ms4      .
           move      zero                 to   w-stp-lvf-tot-ms5      .
           move      zero                 to   w-stp-lvf-tot-ms6      .
           move      zero                 to   w-stp-lvf-tot-mso      .
       prn-ini-lr1-200.
      *              *-------------------------------------------------*
      *              * Normalizzazioni subtotali per fornitore         *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-lvf-sbt-ctr      .
       prn-ini-lr1-220.
           add       1                    to   w-stp-lvf-sbt-ctr      .
           if        w-stp-lvf-sbt-ctr    >    3
                     go to prn-ini-lr1-900.
           move      zero                 to   w-stp-lvf-sbt-pre
                                              (w-stp-lvf-sbt-ctr)     .
           move      zero                 to   w-stp-lvf-sbt-mic
                                              (w-stp-lvf-sbt-ctr)     .
           move      zero                 to   w-stp-lvf-sbt-ms1
                                              (w-stp-lvf-sbt-ctr)     .
           move      zero                 to   w-stp-lvf-sbt-ms2
                                              (w-stp-lvf-sbt-ctr)     .
           move      zero                 to   w-stp-lvf-sbt-ms3
                                              (w-stp-lvf-sbt-ctr)     .
           move      zero                 to   w-stp-lvf-sbt-ms4
                                              (w-stp-lvf-sbt-ctr)     .
           move      zero                 to   w-stp-lvf-sbt-ms5
                                              (w-stp-lvf-sbt-ctr)     .
           move      zero                 to   w-stp-lvf-sbt-ms6
                                              (w-stp-lvf-sbt-ctr)     .
           move      zero                 to   w-stp-lvf-sbt-mso
                                              (w-stp-lvf-sbt-ctr)     .
           go to     prn-ini-lr1-220.
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
       prn-fin-lr1-100.
      *              *-------------------------------------------------*
      *              * Test su elementi rilevati per il fornitore      *
      *              *-------------------------------------------------*
           if        w-stp-orf-wrk-ele    =    zero and
                     w-stp-bfo-wrk-ele    =    zero and
                     w-stp-scf-wrk-ele    =    zero
                     go to prn-fin-lr1-999.
       prn-fin-lr1-200.
      *              *-------------------------------------------------*
      *              * Incremento contatore fornitori trattati         *
      *              *-------------------------------------------------*
           add       1                    to   w-stp-tot-tot-fnt      .
      *              *-------------------------------------------------*
      *              * Anagrafica fornitore                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su linee residue                       *
      *                  *---------------------------------------------*
           if        p-res                >    5
                     go to prn-fin-lr1-220.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                  *---------------------------------------------*
      *                  * Test se interruzione forzata                *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr1-999.
      *                  *---------------------------------------------*
      *                  * Fincatura dettaglio                         *
      *                  *---------------------------------------------*
           perform   int-fin-det-000      thru int-fin-det-999        .
       prn-fin-lr1-220.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Griglia vuota                               *
      *                  *---------------------------------------------*
           perform   prn-grv-det-000      thru prn-grv-det-999        .
      *                  *---------------------------------------------*
      *                  * Codice fornitore                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      002                  to   p-pos                  .
           move      w-stp-lvf-fnt-cod    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Ragione sociale fornitore                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      011                  to   p-pos                  .
           move      w-stp-lvf-fnt-rag    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-lr1-300.
      *              *-------------------------------------------------*
      *              * Totali progressivi                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Mesi precedenti                             *
      *                  *---------------------------------------------*
           move      052                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-tot-pre    to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese in corso                               *
      *                  *---------------------------------------------*
           move      068                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-tot-mic    to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 1                           *
      *                  *---------------------------------------------*
           move      084                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-tot-ms1    to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 2                           *
      *                  *---------------------------------------------*
           move      100                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-tot-ms2    to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 3                           *
      *                  *---------------------------------------------*
           move      116                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-tot-ms3    to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 4                           *
      *                  *---------------------------------------------*
           move      132                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-tot-ms4    to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 5                           *
      *                  *---------------------------------------------*
           move      148                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-tot-ms5    to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 6                           *
      *                  *---------------------------------------------*
           move      164                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-tot-ms6    to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mesi successivi oltre                       *
      *                  *---------------------------------------------*
           move      180                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-tot-mso    to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Totali                                      *
      *                  *---------------------------------------------*
           move      196                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-tot-pre    to   w-stp-val-num-val      .
           add       w-stp-lvf-tot-mic    to   w-stp-val-num-val      .
           add       w-stp-lvf-tot-ms1    to   w-stp-val-num-val      .
           add       w-stp-lvf-tot-ms2    to   w-stp-val-num-val      .
           add       w-stp-lvf-tot-ms3    to   w-stp-val-num-val      .
           add       w-stp-lvf-tot-ms4    to   w-stp-val-num-val      .
           add       w-stp-lvf-tot-ms5    to   w-stp-val-num-val      .
           add       w-stp-lvf-tot-ms6    to   w-stp-val-num-val      .
           add       w-stp-lvf-tot-mso    to   w-stp-val-num-val      .
           move      "."                  to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
       prn-fin-lr1-400.
      *              *-------------------------------------------------*
      *              * Subtotali - Ordini inevasi                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        w-stp-orf-wrk-ele    =    zero
                     go to prn-fin-lr1-500.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Griglia vuota                               *
      *                  *---------------------------------------------*
           perform   prn-grv-det-000      thru prn-grv-det-999        .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      011                  to   p-pos                  .
           move      "        - Ordini inevasi                "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mesi precedenti                             *
      *                  *---------------------------------------------*
           move      052                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-pre (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese in corso                               *
      *                  *---------------------------------------------*
           move      068                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-mic (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 1                           *
      *                  *---------------------------------------------*
           move      084                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms1 (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 2                           *
      *                  *---------------------------------------------*
           move      100                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms2 (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 3                           *
      *                  *---------------------------------------------*
           move      116                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms3 (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 4                           *
      *                  *---------------------------------------------*
           move      132                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms4 (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 5                           *
      *                  *---------------------------------------------*
           move      148                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms5 (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 6                           *
      *                  *---------------------------------------------*
           move      164                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms6 (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mesi successivi oltre                       *
      *                  *---------------------------------------------*
           move      180                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-mso (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Totali                                      *
      *                  *---------------------------------------------*
           move      196                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-pre (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-mic (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms1 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms2 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms3 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms4 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms5 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms6 (1)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-mso (1)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Eventuale ciclo di stampa dettaglio         *
      *                  *---------------------------------------------*
           if        rr-sns-det           =    1
                     go to prn-fin-lr1-500.
      *                  *---------------------------------------------*
      *                  * Ciclo di stampa dettaglio ordini inevasi    *
      *                  *---------------------------------------------*
           perform   prn-fin-lr1-orf-000  thru prn-fin-lr1-orf-999    .
       prn-fin-lr1-500.
      *              *-------------------------------------------------*
      *              * Subtotali - Bolle da fatturare                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        w-stp-bfo-wrk-ele    =    zero
                     go to prn-fin-lr1-600.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Griglia vuota                               *
      *                  *---------------------------------------------*
           perform   prn-grv-det-000      thru prn-grv-det-999        .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      011                  to   p-pos                  .
           move      "        - Bolle da fatturare            "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mesi precedenti                             *
      *                  *---------------------------------------------*
           move      052                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-pre (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese in corso                               *
      *                  *---------------------------------------------*
           move      068                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-mic (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 1                           *
      *                  *---------------------------------------------*
           move      084                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms1 (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 2                           *
      *                  *---------------------------------------------*
           move      100                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms2 (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 3                           *
      *                  *---------------------------------------------*
           move      116                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms3 (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 4                           *
      *                  *---------------------------------------------*
           move      132                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms4 (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 5                           *
      *                  *---------------------------------------------*
           move      148                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms5 (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 6                           *
      *                  *---------------------------------------------*
           move      164                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms6 (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mesi successivi oltre                       *
      *                  *---------------------------------------------*
           move      180                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-mso (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Totali                                      *
      *                  *---------------------------------------------*
           move      196                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-pre (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-mic (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms1 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms2 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms3 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms4 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms5 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms6 (2)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-mso (2)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Eventuale ciclo di stampa dettaglio         *
      *                  *---------------------------------------------*
           if        rr-sns-det           =    1
                     go to prn-fin-lr1-600.
      *                  *---------------------------------------------*
      *                  * Ciclo di stampa dettaglio bolle da fattura- *
      *                  * re                                          *
      *                  *---------------------------------------------*
           perform   prn-fin-lr1-bfo-000  thru prn-fin-lr1-bfo-999    .
       prn-fin-lr1-600.
      *              *-------------------------------------------------*
      *              * Subtotali - Scadenze in portafoglio             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da stampare                         *
      *                  *---------------------------------------------*
           if        w-stp-scf-wrk-ele    =    zero
                     go to prn-fin-lr1-900.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Griglia vuota                               *
      *                  *---------------------------------------------*
           perform   prn-grv-det-000      thru prn-grv-det-999        .
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      011                  to   p-pos                  .
           move      "        - Scadenze in portafoglio       "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mesi precedenti                             *
      *                  *---------------------------------------------*
           move      052                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-pre (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese in corso                               *
      *                  *---------------------------------------------*
           move      068                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-mic (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 1                           *
      *                  *---------------------------------------------*
           move      084                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms1 (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 2                           *
      *                  *---------------------------------------------*
           move      100                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms2 (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 3                           *
      *                  *---------------------------------------------*
           move      116                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms3 (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 4                           *
      *                  *---------------------------------------------*
           move      132                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms4 (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 5                           *
      *                  *---------------------------------------------*
           move      148                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms5 (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mese successivo 6                           *
      *                  *---------------------------------------------*
           move      164                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-ms6 (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Mesi successivi oltre                       *
      *                  *---------------------------------------------*
           move      180                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-mso (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Totali                                      *
      *                  *---------------------------------------------*
           move      196                  to   w-stp-val-num-pos      .
           move      w-stp-lvf-sbt-pre (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-mic (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms1 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms2 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms3 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms4 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms5 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-ms6 (3)
                                          to   w-stp-val-num-val      .
           add       w-stp-lvf-sbt-mso (3)
                                          to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                  *---------------------------------------------*
      *                  * Eventuale ciclo di stampa dettaglio         *
      *                  *---------------------------------------------*
           if        rr-sns-det           =    1
                     go to prn-fin-lr1-900.
      *                  *---------------------------------------------*
      *                  * Ciclo di stampa dettaglio scadenze          *
      *                  *---------------------------------------------*
           perform   prn-fin-lr1-scf-000  thru prn-fin-lr1-scf-999    .
       prn-fin-lr1-900.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Sopralineatura                                  *
      *              *-------------------------------------------------*
           perform   prn-stl-det-000      thru prn-stl-det-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-fin-lr1-999.
       prn-fin-lr1-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 1. livello di rottura         *
      *    *                                                           *
      *    * Ciclo di stampa dettaglio ordini inevasi                  *
      *    *-----------------------------------------------------------*
       prn-fin-lr1-orf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione comodi per tranche documento    *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-orf-wrk-svd      .
           move      zero                 to   w-stp-orf-wrk-tdc      .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione tabella                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-orf-wrk-ctr      .
       prn-fin-lr1-orf-200.
           add       1                    to   w-stp-orf-wrk-ctr      .
           if        w-stp-orf-wrk-ctr    >    w-stp-orf-wrk-ele
                     go to prn-fin-lr1-orf-900.
           if        w-stp-orf-wrk-ctr    >    w-stp-orf-wrk-max
                     go to prn-fin-lr1-orf-900.
       prn-fin-lr1-orf-300.
      *              *-------------------------------------------------*
      *              * Stampa singolo elemento                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estremi ordine                              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su linee residue                   *
      *                      *-----------------------------------------*
           if        p-res                >    2
                     go to prn-fin-lr1-orf-320.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura di chiusura              *
      *                      *-----------------------------------------*
           perform   prn-stl-det-000      thru prn-stl-det-999        .
      *                      *-----------------------------------------*
      *                      * Intestazione pagina                     *
      *                      *-----------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                      *-----------------------------------------*
      *                      * Test se interruzione forzata            *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr1-orf-999.
      *                      *-----------------------------------------*
      *                      * Fincatura dettaglio                     *
      *                      *-----------------------------------------*
           perform   int-fin-det-000      thru int-fin-det-999        .
       prn-fin-lr1-orf-320.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Griglia vuota                           *
      *                      *-----------------------------------------*
           perform   prn-grv-det-000      thru prn-grv-det-999        .
      *                      *-----------------------------------------*
      *                      * Codice dipendenza                       *
      *                      *-----------------------------------------*
           if        w-stp-orf-cod-dpz
                    (w-stp-orf-wrk-ctr)   =    01
                     go to prn-fin-lr1-orf-340.
      *                          *-------------------------------------*
      *                          * Stampa                              *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      020                  to   p-pos                  .
           move      w-stp-orf-cod-dpz
                    (w-stp-orf-wrk-ctr)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-lr1-orf-340.
      *                      *-----------------------------------------*
      *                      * Tipo documento                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      023                  to   p-pos                  .
           move      w-stp-orf-tip-doc
                    (w-stp-orf-wrk-ctr)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      031                  to   p-pos                  .
           move      w-stp-orf-num-doc
                    (w-stp-orf-wrk-ctr)   to   w-stp-orf-wrk-doc      .
           move      w-stp-orf-wrk-doc
                    (6 : 6)               to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Tranche documento                       *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da stampare                 *
      *                          *-------------------------------------*
           if        w-stp-orf-wrk-doc    not  = w-stp-orf-wrk-svd
                     move  zero           to   w-stp-orf-wrk-tdc
                     go to prn-fin-lr1-orf-400.
           add       1                    to   w-stp-orf-wrk-tdc      .
      *                          *-------------------------------------*
      *                          * Stampa prompt                       *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      01                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      037                  to   p-pos                  .
           move      "."                  to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Stampa tranche                      *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      03                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      038                  to   p-pos                  .
           move      w-stp-orf-wrk-tdc    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-lr1-orf-400.
      *                      *-----------------------------------------*
      *                      * Salvataggio numero documento            *
      *                      *-----------------------------------------*
           move      w-stp-orf-wrk-doc    to   w-stp-orf-wrk-svd      .
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      042                  to   p-pos                  .
           move      w-stp-orf-dat-doc
                    (w-stp-orf-wrk-ctr)   to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Mesi precedenti                         *
      *                      *-----------------------------------------*
           move      052                  to   w-stp-val-num-pos      .
           move      w-stp-orf-cum-pre
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese in corso                           *
      *                      *-----------------------------------------*
           move      068                  to   w-stp-val-num-pos      .
           move      w-stp-orf-cum-mic
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 1                       *
      *                      *-----------------------------------------*
           move      084                  to   w-stp-val-num-pos      .
           move      w-stp-orf-cum-ms1
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 2                       *
      *                      *-----------------------------------------*
           move      100                  to   w-stp-val-num-pos      .
           move      w-stp-orf-cum-ms2
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 3                       *
      *                      *-----------------------------------------*
           move      116                  to   w-stp-val-num-pos      .
           move      w-stp-orf-cum-ms3
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 4                       *
      *                      *-----------------------------------------*
           move      132                  to   w-stp-val-num-pos      .
           move      w-stp-orf-cum-ms4
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 5                       *
      *                      *-----------------------------------------*
           move      148                  to   w-stp-val-num-pos      .
           move      w-stp-orf-cum-ms5
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 6                       *
      *                      *-----------------------------------------*
           move      164                  to   w-stp-val-num-pos      .
           move      w-stp-orf-cum-ms6
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mesi successivi oltre                   *
      *                      *-----------------------------------------*
           move      180                  to   w-stp-val-num-pos      .
           move      w-stp-orf-cum-mso
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Totali                                  *
      *                      *-----------------------------------------*
           move      196                  to   w-stp-val-num-pos      .
           move      w-stp-orf-cum-pre
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-orf-cum-mic
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-orf-cum-ms1
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-orf-cum-ms2
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-orf-cum-ms3
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-orf-cum-ms4
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-orf-cum-ms5
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-orf-cum-ms6
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-orf-cum-mso
                    (w-stp-orf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
       prn-fin-lr1-orf-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     prn-fin-lr1-orf-200.
       prn-fin-lr1-orf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-fin-lr1-orf-999.
       prn-fin-lr1-orf-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 1. livello di rottura         *
      *    *                                                           *
      *    * Ciclo di stampa dettaglio bolle da fatturare              *
      *    *-----------------------------------------------------------*
       prn-fin-lr1-bfo-000.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione tabella                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-bfo-wrk-ctr      .
       prn-fin-lr1-bfo-200.
           add       1                    to   w-stp-bfo-wrk-ctr      .
           if        w-stp-bfo-wrk-ctr    >    w-stp-bfo-wrk-ele
                     go to prn-fin-lr1-bfo-900.
           if        w-stp-bfo-wrk-ctr    >    w-stp-bfo-wrk-max
                     go to prn-fin-lr1-bfo-900.
       prn-fin-lr1-bfo-300.
      *              *-------------------------------------------------*
      *              * Stampa singolo elemento                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estremi bolla                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su linee residue                   *
      *                      *-----------------------------------------*
           if        p-res                >    2
                     go to prn-fin-lr1-bfo-320.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura di chiusura              *
      *                      *-----------------------------------------*
           perform   prn-stl-det-000      thru prn-stl-det-999        .
      *                      *-----------------------------------------*
      *                      * Intestazione pagina                     *
      *                      *-----------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                      *-----------------------------------------*
      *                      * Test se interruzione forzata            *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr1-bfo-999.
      *                      *-----------------------------------------*
      *                      * Fincatura dettaglio                     *
      *                      *-----------------------------------------*
           perform   int-fin-det-000      thru int-fin-det-999        .
       prn-fin-lr1-bfo-320.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Griglia vuota                           *
      *                      *-----------------------------------------*
           perform   prn-grv-det-000      thru prn-grv-det-999        .
      *                      *-----------------------------------------*
      *                      * Codice dipendenza                       *
      *                      *-----------------------------------------*
           if        w-stp-bfo-cod-dpz
                    (w-stp-bfo-wrk-ctr)   =    01
                     go to prn-fin-lr1-bfo-340.
      *                          *-------------------------------------*
      *                          * Stampa                              *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      020                  to   p-pos                  .
           move      w-stp-bfo-cod-dpz
                    (w-stp-bfo-wrk-ctr)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-fin-lr1-bfo-340.
      *                      *-----------------------------------------*
      *                      * Tipo documento                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      023                  to   p-pos                  .
           move      w-stp-bfo-tip-doc
                    (w-stp-bfo-wrk-ctr)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      031                  to   p-pos                  .
           move      w-stp-bfo-num-doc
                    (w-stp-bfo-wrk-ctr)   to   w-stp-bfo-wrk-doc      .
           move      w-stp-bfo-wrk-doc
                    (6 : 6)               to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      042                  to   p-pos                  .
           move      w-stp-bfo-dat-doc
                    (w-stp-bfo-wrk-ctr)   to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Mesi precedenti                         *
      *                      *-----------------------------------------*
           move      052                  to   w-stp-val-num-pos      .
           move      w-stp-bfo-cum-pre
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese in corso                           *
      *                      *-----------------------------------------*
           move      068                  to   w-stp-val-num-pos      .
           move      w-stp-bfo-cum-mic
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 1                       *
      *                      *-----------------------------------------*
           move      084                  to   w-stp-val-num-pos      .
           move      w-stp-bfo-cum-ms1
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 2                       *
      *                      *-----------------------------------------*
           move      100                  to   w-stp-val-num-pos      .
           move      w-stp-bfo-cum-ms2
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 3                       *
      *                      *-----------------------------------------*
           move      116                  to   w-stp-val-num-pos      .
           move      w-stp-bfo-cum-ms3
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 4                       *
      *                      *-----------------------------------------*
           move      132                  to   w-stp-val-num-pos      .
           move      w-stp-bfo-cum-ms4
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 5                       *
      *                      *-----------------------------------------*
           move      148                  to   w-stp-val-num-pos      .
           move      w-stp-bfo-cum-ms5
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 6                       *
      *                      *-----------------------------------------*
           move      164                  to   w-stp-val-num-pos      .
           move      w-stp-bfo-cum-ms6
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mesi successivi oltre                   *
      *                      *-----------------------------------------*
           move      180                  to   w-stp-val-num-pos      .
           move      w-stp-bfo-cum-mso
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Totali                                  *
      *                      *-----------------------------------------*
           move      196                  to   w-stp-val-num-pos      .
           move      w-stp-bfo-cum-pre
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-bfo-cum-mic
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-bfo-cum-ms1
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-bfo-cum-ms2
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-bfo-cum-ms3
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-bfo-cum-ms4
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-bfo-cum-ms5
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-bfo-cum-ms6
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-bfo-cum-mso
                    (w-stp-bfo-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
       prn-fin-lr1-bfo-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     prn-fin-lr1-bfo-200.
       prn-fin-lr1-bfo-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-fin-lr1-bfo-999.
       prn-fin-lr1-bfo-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Fine 1. livello di rottura         *
      *    *                                                           *
      *    * Ciclo di stampa dettaglio scadenze in portafoglio         *
      *    *-----------------------------------------------------------*
       prn-fin-lr1-scf-000.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione tabella                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-scf-wrk-ctr      .
       prn-fin-lr1-scf-200.
           add       1                    to   w-stp-scf-wrk-ctr      .
           if        w-stp-scf-wrk-ctr    >    w-stp-scf-wrk-ele
                     go to prn-fin-lr1-scf-900.
           if        w-stp-scf-wrk-ctr    >    w-stp-scf-wrk-max
                     go to prn-fin-lr1-scf-900.
       prn-fin-lr1-scf-300.
      *              *-------------------------------------------------*
      *              * Stampa singolo elemento                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estremi scadenza                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su linee residue                   *
      *                      *-----------------------------------------*
           if        p-res                >    2
                     go to prn-fin-lr1-scf-320.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Sottolineatura di chiusura              *
      *                      *-----------------------------------------*
           perform   prn-stl-det-000      thru prn-stl-det-999        .
      *                      *-----------------------------------------*
      *                      * Intestazione pagina                     *
      *                      *-----------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                      *-----------------------------------------*
      *                      * Test se interruzione forzata            *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to prn-fin-lr1-scf-999.
      *                      *-----------------------------------------*
      *                      * Fincatura dettaglio                     *
      *                      *-----------------------------------------*
           perform   int-fin-det-000      thru int-fin-det-999        .
       prn-fin-lr1-scf-320.
      *                      *-----------------------------------------*
      *                      * Interlinea                              *
      *                      *-----------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Griglia vuota                           *
      *                      *-----------------------------------------*
           perform   prn-grv-det-000      thru prn-grv-det-999        .
      *                      *-----------------------------------------*
      *                      * Tipo documento                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      023                  to   p-pos                  .
           move      w-stp-scf-tip-doc
                    (w-stp-scf-wrk-ctr)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Numero documento                        *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "P"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           move      029                  to   p-pos                  .
           move      w-stp-scf-num-doc
                    (w-stp-scf-wrk-ctr)   to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Data documento                          *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      p-lnr                to   p-lin                  .
           move      042                  to   p-pos                  .
           move      w-stp-scf-dat-doc
                    (w-stp-scf-wrk-ctr)   to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Mesi precedenti                         *
      *                      *-----------------------------------------*
           move      052                  to   w-stp-val-num-pos      .
           move      w-stp-scf-cum-pre
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese in corso                           *
      *                      *-----------------------------------------*
           move      068                  to   w-stp-val-num-pos      .
           move      w-stp-scf-cum-mic
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 1                       *
      *                      *-----------------------------------------*
           move      084                  to   w-stp-val-num-pos      .
           move      w-stp-scf-cum-ms1
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 2                       *
      *                      *-----------------------------------------*
           move      100                  to   w-stp-val-num-pos      .
           move      w-stp-scf-cum-ms2
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 3                       *
      *                      *-----------------------------------------*
           move      116                  to   w-stp-val-num-pos      .
           move      w-stp-scf-cum-ms3
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 4                       *
      *                      *-----------------------------------------*
           move      132                  to   w-stp-val-num-pos      .
           move      w-stp-scf-cum-ms4
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 5                       *
      *                      *-----------------------------------------*
           move      148                  to   w-stp-val-num-pos      .
           move      w-stp-scf-cum-ms5
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mese successivo 6                       *
      *                      *-----------------------------------------*
           move      164                  to   w-stp-val-num-pos      .
           move      w-stp-scf-cum-ms6
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Mesi successivi oltre                   *
      *                      *-----------------------------------------*
           move      180                  to   w-stp-val-num-pos      .
           move      w-stp-scf-cum-mso
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
      *                      *-----------------------------------------*
      *                      * Totali                                  *
      *                      *-----------------------------------------*
           move      196                  to   w-stp-val-num-pos      .
           move      w-stp-scf-cum-pre
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-scf-cum-mic
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-scf-cum-ms1
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-scf-cum-ms2
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-scf-cum-ms3
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-scf-cum-ms4
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-scf-cum-ms5
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-scf-cum-ms6
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           add       w-stp-scf-cum-mso
                    (w-stp-scf-wrk-ctr)   to   w-stp-val-num-val      .
           move      spaces               to   w-stp-val-num-rmp      .
           perform   stp-val-num-000      thru stp-val-num-999        .
       prn-fin-lr1-scf-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     prn-fin-lr1-scf-200.
       prn-fin-lr1-scf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-fin-lr1-scf-999.
       prn-fin-lr1-scf-999.
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
      *              * Ciclo di preparazione castelletto ordini ineva- *
      *              * si                                              *
      *              *-------------------------------------------------*
           perform   prn-liv-det-orf-000  thru prn-liv-det-orf-999    .
       prn-liv-det-200.
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione castelletto bolle da fat- *
      *              * turare                                          *
      *              *-------------------------------------------------*
           perform   prn-liv-det-bfo-000  thru prn-liv-det-bfo-999    .
       prn-liv-det-300.
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione castelletto scadenze in   *
      *              * portafoglio                                     *
      *              *-------------------------------------------------*
           perform   prn-liv-det-scf-000  thru prn-liv-det-scf-999    .
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
      *    * Ciclo di preparazione castelletto ordini inevasi          *
      *    *-----------------------------------------------------------*
       prn-liv-det-orf-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da fare                             *
      *                  *---------------------------------------------*
           if        rr-vde-stp-orf       =    spaces
                     go to prn-liv-det-orf-900.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
       prn-liv-det-orf-010.
      *              *-------------------------------------------------*
      *              * Start per codice                                *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODDPZ    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      zero                 to   rf-ada-cod-dpz         .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-orf-900.
       prn-liv-det-orf-020.
      *              *-------------------------------------------------*
      *              * Test se batch                                   *
      *              *-------------------------------------------------*
           if        rr-snx-btc           =    "S"
                     go to prn-liv-det-orf-040.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       prn-liv-det-orf-040.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio [ada]              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-orf-900.
       prn-liv-det-orf-100.
      *              *-------------------------------------------------*
      *              * Start su file [oft]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DPZARCDAT "         to   f-key                  .
           move      rf-ada-cod-dpz       to   rf-oft-cod-dpz         .
           move      "F"                  to   rf-oft-tip-arc         .
           move      srt-cod-fnt          to   rf-oft-cod-arc         .
           move      zero                 to   rf-oft-dat-doc         .
           move      zero                 to   rf-oft-num-doc         .
           move      zero                 to   rf-oft-num-prt         .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-orf-020.
       prn-liv-det-orf-200.
      *              *-------------------------------------------------*
      *              * Test se batch                                   *
      *              *-------------------------------------------------*
           if        rr-snx-btc           =    "S"
                     go to prn-liv-det-orf-220.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       prn-liv-det-orf-220.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale record [oft]                *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofoft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oft                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-orf-020.
       prn-liv-det-orf-300.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rf-oft-cod-dpz       not  = rf-ada-cod-dpz
                     go to prn-liv-det-orf-020.
      *                  *---------------------------------------------*
      *                  * Test su tipo archivio                       *
      *                  *---------------------------------------------*
           if        rf-oft-tip-arc       not  = "F"
                     go to prn-liv-det-orf-020.
      *                  *---------------------------------------------*
      *                  * Test su codice fornitore                    *
      *                  *---------------------------------------------*
           if        rf-oft-cod-arc       not  = srt-cod-fnt
                     go to prn-liv-det-orf-020.
      *                  *---------------------------------------------*
      *                  * Test su data documento                      *
      *                  *---------------------------------------------*
           if        rf-oft-dat-doc       >    w-stp-dat-inp-rif
                     go to prn-liv-det-orf-020.
       prn-liv-det-orf-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di ordine chiuso               *
      *                  *---------------------------------------------*
           if        rf-oft-flg-och       not  = spaces
                     go to prn-liv-det-orf-200.
      *                  *---------------------------------------------*
      *                  * Determinazione status dell'ordine           *
      *                  *---------------------------------------------*
           move      "DT"                 to   d-sts-orf-tip-ope      .
           perform   det-sts-orf-cll-000  thru det-sts-orf-cll-999    .
      *                  *---------------------------------------------*
      *                  * Test su status dell'ordine                  *
      *                  *---------------------------------------------*
           if        d-sts-orf-sts-ord    =    02  or
                     d-sts-orf-sts-ord    =    04
                     go to prn-liv-det-orf-500
           else      go to prn-liv-det-orf-200.
       prn-liv-det-orf-500.
      *              *-------------------------------------------------*
      *              * Pre-scansione righe ordine per determinare in   *
      *              * quante tranches deve essere diviso l'ordine     *
      *              * rispetto alle date di consegna prevista in riga *
      *              *-------------------------------------------------*
           move      rf-oft-num-prt       to   w-rof-num-prt          .
           perform   pre-scn-rig-orf-000  thru pre-scn-rig-orf-999    .
      *              *-------------------------------------------------*
      *              * Ordinamento finale delle righe bufferizzate     *
      *              *-------------------------------------------------*
           perform   ord-fin-rig-orf-000  thru ord-fin-rig-orf-999    .
       prn-liv-det-orf-520.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione delle righe bufferizzate ed  *
      *              * ordinate, con rottura su data consegna prevista *
      *              * per ottenere le tranches dell'ordine            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione comodo per rottura su data  *
      *                  * consegna prevista                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-rof-rot-prv          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore righe             *
      *                  *---------------------------------------------*
           move      zero                 to   w-rof-ctr-ele          .
       prn-liv-det-orf-540.
           add       1                    to   w-rof-ctr-ele          .
      *                  *---------------------------------------------*
      *                  * Test di superamento buffer                  *
      *                  *---------------------------------------------*
           if        w-rof-ctr-ele        >    w-rof-num-ele
                     go to prn-liv-det-orf-880.
           if        w-rof-ctr-ele        >    w-rof-max-ele
                     go to prn-liv-det-orf-880.
      *                  *---------------------------------------------*
      *                  * Test di rottura su data consegna prevista   *
      *                  *---------------------------------------------*
           if        w-rof-rot-prv        =    zero
                     move  w-rof-key-prv
                          (w-rof-ctr-ele) to   w-rof-rot-prv
                     go to prn-liv-det-orf-560.
           if        w-rof-key-prv 
                    (w-rof-ctr-ele)       =    w-rof-rot-prv
                     go to prn-liv-det-orf-580
           else      go to prn-liv-det-orf-600.
       prn-liv-det-orf-560.
      *                  *---------------------------------------------*
      *                  * Rottura - inizio livello                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento numero elementi in tabella   *
      *                      * ordini inevasi                          *
      *                      *-----------------------------------------*
           add       1                    to   w-stp-orf-wrk-ele      .
           if        w-stp-orf-wrk-ele    >    w-stp-orf-wrk-max
                     go to prn-liv-det-orf-900.
      *                      *-----------------------------------------*
      *                      * Normalizzazione tabella elementi        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Estremi ordine                      *
      *                          *-------------------------------------*
           move      rf-oft-cod-dpz       to   w-stp-orf-cod-dpz
                                              (w-stp-orf-wrk-ele)     .
           move      rf-oft-tmo-orf       to   w-stp-orf-tip-doc
                                              (w-stp-orf-wrk-ele)     .
           move      rf-oft-num-doc       to   w-stp-orf-num-doc
                                              (w-stp-orf-wrk-ele)     .
           move      rf-oft-dat-doc       to   w-stp-orf-dat-doc
                                              (w-stp-orf-wrk-ele)     .
      *                          *-------------------------------------*
      *                          * Valori per cumulo                   *
      *                          *-------------------------------------*
           move      zero                 to   w-stp-orf-cum-pre
                                              (w-stp-orf-wrk-ele)     .
           move      zero                 to   w-stp-orf-cum-mic
                                              (w-stp-orf-wrk-ele)     .
           move      zero                 to   w-stp-orf-cum-ms1
                                              (w-stp-orf-wrk-ele)     .
           move      zero                 to   w-stp-orf-cum-ms2
                                              (w-stp-orf-wrk-ele)     .
           move      zero                 to   w-stp-orf-cum-ms3
                                              (w-stp-orf-wrk-ele)     .
           move      zero                 to   w-stp-orf-cum-ms4
                                              (w-stp-orf-wrk-ele)     .
           move      zero                 to   w-stp-orf-cum-ms5
                                              (w-stp-orf-wrk-ele)     .
           move      zero                 to   w-stp-orf-cum-ms6
                                              (w-stp-orf-wrk-ele)     .
           move      zero                 to   w-stp-orf-cum-mso
                                              (w-stp-orf-wrk-ele)     .
      *                          *-------------------------------------*
      *                          * Inizializzazione area totali ordine *
      *                          *-------------------------------------*
           perform   ini-tot-orf-000      thru ini-tot-orf-999        .
       prn-liv-det-orf-580.
      *                  *---------------------------------------------*
      *                  * Rottura - dettaglio                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Cumulo totali dalle righe               *
      *                      *-----------------------------------------*
           perform   det-tri-orf-000      thru det-tri-orf-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo a riga successiva               *
      *                      *-----------------------------------------*
           go to     prn-liv-det-orf-540.
       prn-liv-det-orf-600.
      *                  *---------------------------------------------*
      *                  * Rottura - fine livello                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Trattamento fine tranche                *
      *                      *-----------------------------------------*
           perform   trt-fin-trc-orf-000  thru trt-fin-trc-orf-999    .
      *                      *-----------------------------------------*
      *                      * Aggiornamento comodo di rottura         *
      *                      *-----------------------------------------*
           move      w-rof-key-prv 
                    (w-rof-ctr-ele)       to   w-rof-rot-prv          .
       prn-liv-det-orf-860.
      *                      *-----------------------------------------*
      *                      * A inizio livello                        *
      *                      *-----------------------------------------*
           go to     prn-liv-det-orf-560.
       prn-liv-det-orf-880.
      *              *-------------------------------------------------*
      *              * Trattamento fine tranche                        *
      *              *-------------------------------------------------*
           perform   trt-fin-trc-orf-000  thru trt-fin-trc-orf-999    .
      *              *-------------------------------------------------*
      *              * Riciclo a testata ordine successiva             *
      *              *-------------------------------------------------*
           go to     prn-liv-det-orf-200.
       prn-liv-det-orf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-orf-999.
       prn-liv-det-orf-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Livello di dettaglio               *
      *    *                                                           *
      *    * Ciclo di preparazione castelletto bolle da fatturare      *
      *    *-----------------------------------------------------------*
       prn-liv-det-bfo-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da fare                             *
      *                  *---------------------------------------------*
           if        rr-vde-stp-bfo       =    spaces
                     go to prn-liv-det-bfo-900.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
       prn-liv-det-bfo-010.
      *              *-------------------------------------------------*
      *              * Start per codice                                *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "CODDPZ    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      zero                 to   rf-ada-cod-dpz         .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-bfo-900.
       prn-liv-det-bfo-020.
      *              *-------------------------------------------------*
      *              * Test se batch                                   *
      *              *-------------------------------------------------*
           if        rr-snx-btc           =    "S"
                     go to prn-liv-det-bfo-040.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       prn-liv-det-bfo-040.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale archivio [ada]              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-bfo-900.
       prn-liv-det-bfo-100.
      *              *-------------------------------------------------*
      *              * Start su file [bft]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DPZARCDAT "         to   f-key                  .
           move      rf-ada-cod-dpz       to   rf-bft-cod-dpz         .
           move      "F"                  to   rf-bft-tip-arc         .
           move      srt-cod-fnt          to   rf-bft-cod-arc         .
           move      zero                 to   rf-bft-dat-reg         .
           move      zero                 to   rf-bft-num-doc         .
           move      zero                 to   rf-bft-num-prt         .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-bfo-020.
       prn-liv-det-bfo-200.
      *              *-------------------------------------------------*
      *              * Test se batch                                   *
      *              *-------------------------------------------------*
           if        rr-snx-btc           =    "S"
                     go to prn-liv-det-bfo-220.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       prn-liv-det-bfo-220.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale record [bft]                *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbft"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bft                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-bfo-020.
       prn-liv-det-bfo-300.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rf-bft-cod-dpz       not  = rf-ada-cod-dpz
                     go to prn-liv-det-bfo-020.
      *                  *---------------------------------------------*
      *                  * Test su tipo archivio                       *
      *                  *---------------------------------------------*
           if        rf-bft-tip-arc       not  = "F"
                     go to prn-liv-det-bfo-020.
      *                  *---------------------------------------------*
      *                  * Test su codice fornitore                    *
      *                  *---------------------------------------------*
           if        rf-bft-cod-arc       not  = srt-cod-fnt
                     go to prn-liv-det-bfo-020.
      *                  *---------------------------------------------*
      *                  * Test su data registrazione                  *
      *                  *---------------------------------------------*
           if        rf-bft-dat-reg       >    w-stp-dat-inp-rif
                     go to prn-liv-det-bfo-020.
       prn-liv-det-bfo-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di movimento che interessa la  *
      *                  * fatturazione                                *
      *                  *---------------------------------------------*
           if        rf-bft-int-ftr       not  = 02
                     go to prn-liv-det-bfo-200.
      *                  *---------------------------------------------*
      *                  * Test se riga chiusa                         *
      *                  *---------------------------------------------*
           if        rf-bft-flg-bch       not  = spaces
                     go to prn-liv-det-bfo-200.
      *                  *---------------------------------------------*
      *                  * Determinazione status della bolla           *
      *                  *---------------------------------------------*
           perform   det-sts-bfo-000      thru det-sts-bfo-999        .
      *                  *---------------------------------------------*
      *                  * Test su status dell'ordine                  *
      *                  *---------------------------------------------*
           if        w-det-sts-bfo-flg    not  = spaces
                     go to prn-liv-det-bfo-200.
       prn-liv-det-bfo-500.
      *              *-------------------------------------------------*
      *              * Incremento numero elementi in tabella ordini    *
      *              * inevasi                                         *
      *              *-------------------------------------------------*
           add       1                    to   w-stp-bfo-wrk-ele      .
           if        w-stp-bfo-wrk-ele    >    w-stp-bfo-wrk-max
                     go to prn-liv-det-bfo-900.
      *              *-------------------------------------------------*
      *              * Normalizzazione tabella elementi                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estremi bolla                               *
      *                  *---------------------------------------------*
           move      rf-bft-cod-dpz       to   w-stp-bfo-cod-dpz
                                              (w-stp-bfo-wrk-ele)     .
           move      rf-bft-cod-tmb       to   w-stp-bfo-tip-doc
                                              (w-stp-bfo-wrk-ele)     .
           move      rf-bft-num-prt       to   w-stp-bfo-num-doc
                                              (w-stp-bfo-wrk-ele)     .
           move      rf-bft-dat-reg       to   w-stp-bfo-dat-doc
                                              (w-stp-bfo-wrk-ele)     .
      *                  *---------------------------------------------*
      *                  * Valori per cumulo                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-bfo-cum-pre
                                              (w-stp-bfo-wrk-ele)     .
           move      zero                 to   w-stp-bfo-cum-mic
                                              (w-stp-bfo-wrk-ele)     .
           move      zero                 to   w-stp-bfo-cum-ms1
                                              (w-stp-bfo-wrk-ele)     .
           move      zero                 to   w-stp-bfo-cum-ms2
                                              (w-stp-bfo-wrk-ele)     .
           move      zero                 to   w-stp-bfo-cum-ms3
                                              (w-stp-bfo-wrk-ele)     .
           move      zero                 to   w-stp-bfo-cum-ms4
                                              (w-stp-bfo-wrk-ele)     .
           move      zero                 to   w-stp-bfo-cum-ms5
                                              (w-stp-bfo-wrk-ele)     .
           move      zero                 to   w-stp-bfo-cum-ms6
                                              (w-stp-bfo-wrk-ele)     .
           move      zero                 to   w-stp-bfo-cum-mso
                                              (w-stp-bfo-wrk-ele)     .
      *              *-------------------------------------------------*
      *              * Inizializzazione area totali bolla              *
      *              *-------------------------------------------------*
           perform   ini-tot-bfo-000      thru ini-tot-bfo-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totali dalle righe               *
      *              *-------------------------------------------------*
           perform   det-tri-bfo-000      thru det-tri-bfo-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totali bolla                     *
      *              *-------------------------------------------------*
           perform   det-tot-bfo-000      thru det-tot-bfo-999        .
       prn-liv-det-bfo-600.
      *              *-------------------------------------------------*
      *              * Sventagliamento delle scadenze nei mesi di      *
      *              * competenza                                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-bfo-wrk-ctr      .
       prn-liv-det-bfo-620.
           add       1                    to   w-stp-bfo-wrk-ctr      .
      *                  *---------------------------------------------*
      *                  * Test preliminari                            *
      *                  *---------------------------------------------*
           if        w-tot-scd-imp
                    (w-stp-bfo-wrk-ctr)   =    zero
                     go to prn-liv-det-bfo-800.
           if        w-stp-bfo-wrk-ctr    >    96
                     go to prn-liv-det-bfo-800.
      *                  *---------------------------------------------*
      *                  * Confronto fra date                          *
      *                  *---------------------------------------------*
           if        w-tot-scd-dat
                    (w-stp-bfo-wrk-ctr)   >    w-stp-dat-inp-fm6
                     add  w-tot-scd-imp
                         (w-stp-bfo-wrk-ctr)
                                          to   w-stp-bfo-cum-mso
                                              (w-stp-bfo-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bfo-wrk-ctr)   >    w-stp-dat-inp-fm5
                     add  w-tot-scd-imp
                         (w-stp-bfo-wrk-ctr)
                                          to   w-stp-bfo-cum-ms6
                                              (w-stp-bfo-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bfo-wrk-ctr)   >    w-stp-dat-inp-fm4
                     add  w-tot-scd-imp
                         (w-stp-bfo-wrk-ctr)
                                          to   w-stp-bfo-cum-ms5
                                              (w-stp-bfo-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bfo-wrk-ctr)   >    w-stp-dat-inp-fm3
                     add  w-tot-scd-imp
                         (w-stp-bfo-wrk-ctr)
                                          to   w-stp-bfo-cum-ms4
                                              (w-stp-bfo-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bfo-wrk-ctr)   >    w-stp-dat-inp-fm2
                     add  w-tot-scd-imp
                         (w-stp-bfo-wrk-ctr)
                                          to   w-stp-bfo-cum-ms3
                                              (w-stp-bfo-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bfo-wrk-ctr)   >    w-stp-dat-inp-fm1
                     add  w-tot-scd-imp
                         (w-stp-bfo-wrk-ctr)
                                          to   w-stp-bfo-cum-ms2
                                              (w-stp-bfo-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bfo-wrk-ctr)   >    w-stp-dat-inp-fmr
                     add  w-tot-scd-imp
                         (w-stp-bfo-wrk-ctr)
                                          to   w-stp-bfo-cum-ms1
                                              (w-stp-bfo-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bfo-wrk-ctr)   >    w-stp-dat-inp-fmp
                     add  w-tot-scd-imp
                         (w-stp-bfo-wrk-ctr)
                                          to   w-stp-bfo-cum-mic
                                              (w-stp-bfo-wrk-ele)
           else      add  w-tot-scd-imp
                         (w-stp-bfo-wrk-ctr)
                                          to   w-stp-bfo-cum-pre
                                              (w-stp-bfo-wrk-ele)     .
           go to     prn-liv-det-bfo-620.
       prn-liv-det-bfo-800.
      *              *-------------------------------------------------*
      *              * Cumulo totali generali                          *
      *              *-------------------------------------------------*
           add       w-stp-bfo-cum-pre
                    (w-stp-bfo-wrk-ele)   to   w-stp-tot-tot-pre (2)  .
           add       w-stp-bfo-cum-mic
                    (w-stp-bfo-wrk-ele)   to   w-stp-tot-tot-mic (2)  .
           add       w-stp-bfo-cum-ms1
                    (w-stp-bfo-wrk-ele)   to   w-stp-tot-tot-ms1 (2)  .
           add       w-stp-bfo-cum-ms2
                    (w-stp-bfo-wrk-ele)   to   w-stp-tot-tot-ms2 (2)  .
           add       w-stp-bfo-cum-ms3
                    (w-stp-bfo-wrk-ele)   to   w-stp-tot-tot-ms3 (2)  .
           add       w-stp-bfo-cum-ms4
                    (w-stp-bfo-wrk-ele)   to   w-stp-tot-tot-ms4 (2)  .
           add       w-stp-bfo-cum-ms5
                    (w-stp-bfo-wrk-ele)   to   w-stp-tot-tot-ms5 (2)  .
           add       w-stp-bfo-cum-ms6
                    (w-stp-bfo-wrk-ele)   to   w-stp-tot-tot-ms6 (2)  .
           add       w-stp-bfo-cum-mso
                    (w-stp-bfo-wrk-ele)   to   w-stp-tot-tot-mso (2)  .
      *              *-------------------------------------------------*
      *              * Cumulo totali fornitore                         *
      *              *-------------------------------------------------*
           add       w-stp-bfo-cum-pre
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-tot-pre      .
           add       w-stp-bfo-cum-mic
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-tot-mic      .
           add       w-stp-bfo-cum-ms1
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-tot-ms1      .
           add       w-stp-bfo-cum-ms2
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-tot-ms2      .
           add       w-stp-bfo-cum-ms3
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-tot-ms3      .
           add       w-stp-bfo-cum-ms4
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-tot-ms4      .
           add       w-stp-bfo-cum-ms5
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-tot-ms5      .
           add       w-stp-bfo-cum-ms6
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-tot-ms6      .
           add       w-stp-bfo-cum-mso
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-tot-mso      .
      *              *-------------------------------------------------*
      *              * Cumulo subtotali fornitore                      *
      *              *-------------------------------------------------*
           add       w-stp-bfo-cum-pre
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-sbt-pre (2)  .
           add       w-stp-bfo-cum-mic
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-sbt-mic (2)  .
           add       w-stp-bfo-cum-ms1
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-sbt-ms1 (2)  .
           add       w-stp-bfo-cum-ms2
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-sbt-ms2 (2)  .
           add       w-stp-bfo-cum-ms3
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-sbt-ms3 (2)  .
           add       w-stp-bfo-cum-ms4
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-sbt-ms4 (2)  .
           add       w-stp-bfo-cum-ms5
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-sbt-ms5 (2)  .
           add       w-stp-bfo-cum-ms6
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-sbt-ms6 (2)  .
           add       w-stp-bfo-cum-mso
                    (w-stp-bfo-wrk-ele)   to   w-stp-lvf-sbt-mso (2)  .
      *              *-------------------------------------------------*
      *              * Riciclo a testata ordine successiva             *
      *              *-------------------------------------------------*
           go to     prn-liv-det-bfo-200.
       prn-liv-det-bfo-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-bfo-999.
       prn-liv-det-bfo-999.
           exit.

      *    *===========================================================*
      *    * Ciclo Report Program : Livello di dettaglio               *
      *    *                                                           *
      *    * Ciclo di preparazione castelletto scadenze in portafoglio *
      *    *-----------------------------------------------------------*
       prn-liv-det-scf-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da fare                             *
      *                  *---------------------------------------------*
           if        rr-vde-stp-scf       =    spaces
                     go to prn-liv-det-scf-900.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
       prn-liv-det-scf-100.
      *              *-------------------------------------------------*
      *              * Start su file [sfs]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "FNTDTS    "         to   f-key                  .
           move      srt-cod-fnt          to   rf-sfs-cod-fnt         .
           move      zero                 to   rf-sfs-dts-scf         .
           move      zero                 to   rf-sfs-num-scf         .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-scf-900.
       prn-liv-det-scf-200.
      *              *-------------------------------------------------*
      *              * Test se batch                                   *
      *              *-------------------------------------------------*
           if        rr-snx-btc           =    "S"
                     go to prn-liv-det-scf-220.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       prn-liv-det-scf-220.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale record [sfs]                *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to prn-liv-det-scf-900.
       prn-liv-det-scf-300.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice fornitore                    *
      *                  *---------------------------------------------*
           if        rf-sfs-cod-fnt       not  = srt-cod-fnt
                     go to prn-liv-det-scf-900.
       prn-liv-det-scf-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione status scadenza fornitore    *
      *                  *---------------------------------------------*
           move      rr-dat-rif           to   w-det-srd-scf-drd      .
           perform   det-srd-scf-000      thru det-srd-scf-999        .
      *                  *---------------------------------------------*
      *                  * Tests                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Su status scadenza                      *
      *                      *-----------------------------------------*
           if        w-det-srd-scf-sts    not  = "A"
                     go to prn-liv-det-scf-200.
      *                      *-----------------------------------------*
      *                      * Su sigla ultima operazione eseguita     *
      *                      * sulla scadenza                          *
      *                      *-----------------------------------------*
           if        w-det-srd-scf-suo    not  = "REG"
                     go to prn-liv-det-scf-200.
       prn-liv-det-scf-500.
      *              *-------------------------------------------------*
      *              * Incremento numero elementi in tabella           *
      *              *-------------------------------------------------*
           add       1                    to   w-stp-scf-wrk-ele      .
           if        w-stp-scf-wrk-ele    >    w-stp-scf-wrk-max
                     go to prn-liv-det-scf-900.
      *              *-------------------------------------------------*
      *              * Normalizzazione tabella elementi                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estremi scadenza                            *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       =    01
                     move  "RD  "         to   w-stp-scf-tip-doc
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-tip-scf       =    02
                     move  "IE  "         to   w-stp-scf-tip-doc
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-tip-scf       =    03
                     move  "RIBA"         to   w-stp-scf-tip-doc
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-tip-scf       =    04
                     move  "CDO "         to   w-stp-scf-tip-doc
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-tip-scf       =    05
                     move  "MAV "         to   w-stp-scf-tip-doc
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-tip-scf       =    06
                     move  "RID "         to   w-stp-scf-tip-doc
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-tip-scf       =    07
                     move  "BB  "         to   w-stp-scf-tip-doc
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-tip-scf       =    08
                     move  "CCP "         to   w-stp-scf-tip-doc
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-tip-scf       =    09
                     move  "RB  "         to   w-stp-scf-tip-doc
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-tip-scf       =    10
                     move  "TR  "         to   w-stp-scf-tip-doc
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-tip-scf       =    11
                     move  "PC  "         to   w-stp-scf-tip-doc
                                              (w-stp-scf-wrk-ele)
           else      move  spaces         to   w-stp-scf-tip-doc
                                              (w-stp-scf-wrk-ele)     .
           move      rf-sfs-num-scf       to   w-stp-scf-num-doc
                                              (w-stp-scf-wrk-ele)     .
           move      rf-sfs-dtr-rgs       to   w-stp-scf-dat-doc
                                              (w-stp-scf-wrk-ele)     .
      *                  *---------------------------------------------*
      *                  * Valori per cumulo                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-scf-cum-pre
                                              (w-stp-scf-wrk-ele)     .
           move      zero                 to   w-stp-scf-cum-mic
                                              (w-stp-scf-wrk-ele)     .
           move      zero                 to   w-stp-scf-cum-ms1
                                              (w-stp-scf-wrk-ele)     .
           move      zero                 to   w-stp-scf-cum-ms2
                                              (w-stp-scf-wrk-ele)     .
           move      zero                 to   w-stp-scf-cum-ms3
                                              (w-stp-scf-wrk-ele)     .
           move      zero                 to   w-stp-scf-cum-ms4
                                              (w-stp-scf-wrk-ele)     .
           move      zero                 to   w-stp-scf-cum-ms5
                                              (w-stp-scf-wrk-ele)     .
           move      zero                 to   w-stp-scf-cum-ms6
                                              (w-stp-scf-wrk-ele)     .
           move      zero                 to   w-stp-scf-cum-mso
                                              (w-stp-scf-wrk-ele)     .
       prn-liv-det-scf-600.
      *              *-------------------------------------------------*
      *              * Sventagliamento delle scadenze nei mesi di      *
      *              * competenza                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Eventuale aggiustamento del segno           *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-ddr       =    11
                     multiply -1          by   rf-sfs-imp-scf         .
      *                  *---------------------------------------------*
      *                  * Aggiustamento date scadenza a vista         *
      *                  *---------------------------------------------*
           if        rf-sfs-dts-scf       =    zero
                     move  rf-sfs-dat-ddr to   rf-sfs-dts-scf         .
           if        rf-sfs-dts-scf       =    zero
                     move  rf-sfs-dtr-rgs to   rf-sfs-dts-scf         .
      *                  *---------------------------------------------*
      *                  * Confronto fra date                          *
      *                  *---------------------------------------------*
           if        rf-sfs-dts-scf       >    w-stp-dat-inp-fm6
                     add  rf-sfs-imp-scf  to   w-stp-scf-cum-mso
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-dts-scf       >    w-stp-dat-inp-fm5
                     add  rf-sfs-imp-scf  to   w-stp-scf-cum-ms6
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-dts-scf       >    w-stp-dat-inp-fm4
                     add  rf-sfs-imp-scf  to   w-stp-scf-cum-ms5
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-dts-scf       >    w-stp-dat-inp-fm3
                     add  rf-sfs-imp-scf  to   w-stp-scf-cum-ms4
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-dts-scf       >    w-stp-dat-inp-fm2
                     add  rf-sfs-imp-scf  to   w-stp-scf-cum-ms3
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-dts-scf       >    w-stp-dat-inp-fm1
                     add  rf-sfs-imp-scf  to   w-stp-scf-cum-ms2
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-dts-scf       >    w-stp-dat-inp-fmr
                     add  rf-sfs-imp-scf  to   w-stp-scf-cum-ms1
                                              (w-stp-scf-wrk-ele)
           else if   rf-sfs-dts-scf       >    w-stp-dat-inp-fmp
                     add  rf-sfs-imp-scf  to   w-stp-scf-cum-mic
                                              (w-stp-scf-wrk-ele)
           else      add  rf-sfs-imp-scf  to   w-stp-scf-cum-pre
                                              (w-stp-scf-wrk-ele)     .
       prn-liv-det-scf-800.
      *              *-------------------------------------------------*
      *              * Cumulo totali generali                          *
      *              *-------------------------------------------------*
           add       w-stp-scf-cum-pre
                    (w-stp-scf-wrk-ele)   to   w-stp-tot-tot-pre (3)  .
           add       w-stp-scf-cum-mic
                    (w-stp-scf-wrk-ele)   to   w-stp-tot-tot-mic (3)  .
           add       w-stp-scf-cum-ms1
                    (w-stp-scf-wrk-ele)   to   w-stp-tot-tot-ms1 (3)  .
           add       w-stp-scf-cum-ms2
                    (w-stp-scf-wrk-ele)   to   w-stp-tot-tot-ms2 (3)  .
           add       w-stp-scf-cum-ms3
                    (w-stp-scf-wrk-ele)   to   w-stp-tot-tot-ms3 (3)  .
           add       w-stp-scf-cum-ms4
                    (w-stp-scf-wrk-ele)   to   w-stp-tot-tot-ms4 (3)  .
           add       w-stp-scf-cum-ms5
                    (w-stp-scf-wrk-ele)   to   w-stp-tot-tot-ms5 (3)  .
           add       w-stp-scf-cum-ms6
                    (w-stp-scf-wrk-ele)   to   w-stp-tot-tot-ms6 (3)  .
           add       w-stp-scf-cum-mso
                    (w-stp-scf-wrk-ele)   to   w-stp-tot-tot-mso (3)  .
      *              *-------------------------------------------------*
      *              * Cumulo totali fornitore                         *
      *              *-------------------------------------------------*
           add       w-stp-scf-cum-pre
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-tot-pre      .
           add       w-stp-scf-cum-mic
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-tot-mic      .
           add       w-stp-scf-cum-ms1
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-tot-ms1      .
           add       w-stp-scf-cum-ms2
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-tot-ms2      .
           add       w-stp-scf-cum-ms3
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-tot-ms3      .
           add       w-stp-scf-cum-ms4
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-tot-ms4      .
           add       w-stp-scf-cum-ms5
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-tot-ms5      .
           add       w-stp-scf-cum-ms6
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-tot-ms6      .
           add       w-stp-scf-cum-mso
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-tot-mso      .
      *              *-------------------------------------------------*
      *              * Cumulo subtotali fornitore                      *
      *              *-------------------------------------------------*
           add       w-stp-scf-cum-pre
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-sbt-pre (3)  .
           add       w-stp-scf-cum-mic
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-sbt-mic (3)  .
           add       w-stp-scf-cum-ms1
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-sbt-ms1 (3)  .
           add       w-stp-scf-cum-ms2
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-sbt-ms2 (3)  .
           add       w-stp-scf-cum-ms3
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-sbt-ms3 (3)  .
           add       w-stp-scf-cum-ms4
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-sbt-ms4 (3)  .
           add       w-stp-scf-cum-ms5
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-sbt-ms5 (3)  .
           add       w-stp-scf-cum-ms6
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-sbt-ms6 (3)  .
           add       w-stp-scf-cum-mso
                    (w-stp-scf-wrk-ele)   to   w-stp-lvf-sbt-mso (3)  .
      *              *-------------------------------------------------*
      *              * Riciclo a scadenza successiva                   *
      *              *-------------------------------------------------*
           go to     prn-liv-det-scf-200.
       prn-liv-det-scf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prn-liv-det-scf-999.
       prn-liv-det-scf-999.
           exit.

      *    *===========================================================*
      *    * Griglia vuota per il dettaglio                            *
      *    *-----------------------------------------------------------*
       prn-grv-det-000.
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      220                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "|                                                 
      -              "|               |               |               | 
      -              "              |               |               |   
      -              "            |               |               |     
      -              "          |        |"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-grv-det-999.
           exit.

      *    *===========================================================*
      *    * Subroutine di sottolineatura dettaglio                    *
      *    *-----------------------------------------------------------*
       prn-stl-det-000.
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      220                  to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "+-------------------------------------------------
      -              "+---------------+---------------+---------------+-
      -              "--------------+---------------+---------------+---
      -              "------------+---------------+---------------+-----
      -              "----------+--------+"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       prn-stl-det-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un valore numerico                              *
      *    *-----------------------------------------------------------*
       stp-val-num-000.
      *              *-------------------------------------------------*
      *              * Se valore a zero : zero editato                 *
      *              *-------------------------------------------------*
           if        w-stp-val-num-val    =    zero
                     move  "             0"
                                          to   w-stp-val-num-edt
                     go to stp-val-num-200.
       stp-val-num-100.
      *              *-------------------------------------------------*
      *              * Editing                                         *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "V"                  to   p-tip                  .
           if        w-stp-val-num-val    >    9999999999
                     move  13             to   p-car
           else      move  11             to   p-car                  .
           move      c-dec                to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           if        w-stp-val-num-val    >    9999999999
                     move  "B"            to   p-edm
           else      move  "G"            to   p-edm                  .
           move      w-stp-val-num-val    to   p-num                  .
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
           move      15                   to   w-all-str-lun          .
           move      w-stp-val-num-edt    to   w-all-str-alf          .
           perform   all-str-adx-000      thru all-str-adx-999        .
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      15                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      w-stp-val-num-pos    to   p-pos                  .
           move      w-all-str-alf        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
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
           move      02                   to   w-all-str-num          .
           if        rr-sns-det           =    1
                     move  "RIEPILOGO SITUAZIONE GLOBALE ACQUISTI AL"
                                          to   w-all-str-cat (1)
           else      move  "SITUAZIONE GLOBALE ACQUISTI AL"
                                          to   w-all-str-cat (1)      .
           move      p-edt                to   w-all-str-cat (2)      .
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
      *              * Sopralineatura                                  *
      *              *-------------------------------------------------*
           perform   prn-stl-det-000      thru prn-stl-det-999        .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Griglia vuota                                   *
      *              *-------------------------------------------------*
           perform   prn-grv-det-000      thru prn-grv-det-999        .
      *              *-------------------------------------------------*
      *              * Fincatura                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Fornitore                                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      022                  to   p-pos                  .
           move      "Fornitore"          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mesi precedenti                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      10                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      055                  to   p-pos                  .
           move      "Precedenti"         to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mesi in corso                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      069                  to   p-pos                  .
           move      "Mese in corso"      to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal del mese, da segreteria             *
      *                  *---------------------------------------------*
           move      "LM"                 to   s-ope                  .
           move      "E"                  to   s-tip                  .
           move      w-stp-dat-inp-im1    to   s-dat                  .
           move      s-mes                to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Mese 1                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      086                  to   p-pos                  .
           move      s-alf                to   p-alf                  .
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
           move      096                  to   p-pos                  .
           move      w-stp-dat-inp-im1    to   s-dat                  .
           move      s-ann                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal del mese, da segreteria             *
      *                  *---------------------------------------------*
           move      "LM"                 to   s-ope                  .
           move      "E"                  to   s-tip                  .
           move      w-stp-dat-inp-im2    to   s-dat                  .
           move      s-mes                to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Mese 2                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      102                  to   p-pos                  .
           move      s-alf                to   p-alf                  .
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
           move      112                  to   p-pos                  .
           move      w-stp-dat-inp-im2    to   s-dat                  .
           move      s-ann                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal del mese, da segreteria             *
      *                  *---------------------------------------------*
           move      "LM"                 to   s-ope                  .
           move      "E"                  to   s-tip                  .
           move      w-stp-dat-inp-im3    to   s-dat                  .
           move      s-mes                to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Mese 3                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      118                  to   p-pos                  .
           move      s-alf                to   p-alf                  .
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
           move      128                  to   p-pos                  .
           move      w-stp-dat-inp-im3    to   s-dat                  .
           move      s-ann                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal del mese, da segreteria             *
      *                  *---------------------------------------------*
           move      "LM"                 to   s-ope                  .
           move      "E"                  to   s-tip                  .
           move      w-stp-dat-inp-im4    to   s-dat                  .
           move      s-mes                to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Mese 4                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      134                  to   p-pos                  .
           move      s-alf                to   p-alf                  .
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
           move      144                  to   p-pos                  .
           move      w-stp-dat-inp-im4    to   s-dat                  .
           move      s-ann                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal del mese, da segreteria             *
      *                  *---------------------------------------------*
           move      "LM"                 to   s-ope                  .
           move      "E"                  to   s-tip                  .
           move      w-stp-dat-inp-im5    to   s-dat                  .
           move      s-mes                to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Mese 5                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      150                  to   p-pos                  .
           move      s-alf                to   p-alf                  .
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
           move      160                  to   p-pos                  .
           move      w-stp-dat-inp-im5    to   s-dat                  .
           move      s-ann                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Literal del mese, da segreteria             *
      *                  *---------------------------------------------*
           move      "LM"                 to   s-ope                  .
           move      "E"                  to   s-tip                  .
           move      w-stp-dat-inp-im6    to   s-dat                  .
           move      s-mes                to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Mese 6                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      09                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      166                  to   p-pos                  .
           move      s-alf                to   p-alf                  .
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
           move      176                  to   p-pos                  .
           move      w-stp-dat-inp-im6    to   s-dat                  .
           move      s-ann                to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Mesi successivi                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      10                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      183                  to   p-pos                  .
           move      "Successivi"         to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Totali                                      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      200                  to   p-pos                  .
           move      "Totali"             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Note                                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      04                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      214                  to   p-pos                  .
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
      *              * Sottolineatura                                  *
      *              *-------------------------------------------------*
           perform   prn-stl-det-000      thru prn-stl-det-999        .
       int-fin-det-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     int-fin-det-999.
       int-fin-det-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valori associati alla forma di pagamento   *
      *    *-----------------------------------------------------------*
       det-vas-fop-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-vas-fop-ctr      .
       det-vas-fop-020.
           add       1                    to   w-det-vas-fop-ctr      .
           if        w-det-vas-fop-ctr    >    3
                     go to det-vas-fop-040.
           move      zero                 to   w-tot-cod-pag
                                              (w-det-vas-fop-ctr)     .
           move      zero                 to   w-tot-tip-amm
                                              (w-det-vas-fop-ctr)     .
           move      zero                 to   w-tot-per-toi
                                              (w-det-vas-fop-ctr)     .
           move      zero                 to   w-tot-dim-act
                                              (w-det-vas-fop-ctr)     .
           move      zero                 to   w-tot-tip-pag
                                              (w-det-vas-fop-ctr)     .
           move      zero                 to   w-tot-num-sca
                                              (w-det-vas-fop-ctr)     .
           move      zero                 to   w-tot-dec-prs
                                              (w-det-vas-fop-ctr)     .
           move      zero                 to   w-tot-dap-mes
                                              (w-det-vas-fop-ctr)     .
           move      zero                 to   w-tot-dap-gio
                                              (w-det-vas-fop-ctr)     .
           move      zero                 to   w-tot-ggg-int
                                              (w-det-vas-fop-ctr)     .
           move      zero                 to   w-tot-tip-scm
                                              (w-det-vas-fop-ctr)     .
           move      zero                 to   w-tot-gio-scm
                                              (w-det-vas-fop-ctr)     .
           go to     det-vas-fop-020.
       det-vas-fop-040.
      *              *-------------------------------------------------*
      *              * Se forma di pagamento a zero : uscita           *
      *              *-------------------------------------------------*
           if        w-det-vas-fop-cod    =    zero
                     go to det-vas-fop-200.
      *              *-------------------------------------------------*
      *              * Lettura record [yfp]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFOP"             to   f-key                  .
           move      w-det-vas-fop-cod    to   rf-yfp-cod-fop         .
           move      "pgm/dcf/fls/ioc/obj/iofyfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yfp                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : uscita              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-vas-fop-200.
      *              *-------------------------------------------------*
      *              * Ciclo per tre codici pagamento possibili        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-vas-fop-ctr      .
       det-vas-fop-100.
           add       1                    to   w-det-vas-fop-ctr      .
           if        w-det-vas-fop-ctr    >    3
                     go to det-vas-fop-200.
           if        rf-yfp-cod-pag
                    (w-det-vas-fop-ctr)   =    zero
                     go to det-vas-fop-100.
      *                  *---------------------------------------------*
      *                  * Campi derivati da record [yfp]              *
      *                  *---------------------------------------------*
           move      rf-yfp-cod-pag
                    (w-det-vas-fop-ctr)   to   w-tot-cod-pag
                                              (w-det-vas-fop-ctr)     .
           move      rf-yfp-tip-amm
                    (w-det-vas-fop-ctr)   to   w-tot-tip-amm
                                              (w-det-vas-fop-ctr)     .
           move      rf-yfp-per-toi
                    (w-det-vas-fop-ctr)   to   w-tot-per-toi
                                              (w-det-vas-fop-ctr)     .
           move      rf-yfp-dim-act
                    (w-det-vas-fop-ctr)   to   w-tot-dim-act
                                              (w-det-vas-fop-ctr)     .
      *                  *---------------------------------------------*
      *                  * Campi derivati da record [zpg]              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record [zpg]            *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
      *                      *-----------------------------------------*
      *                      * Lettura record [zpg]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPAG"             to   f-key                  .
           move      w-tot-cod-pag
                    (w-det-vas-fop-ctr)   to   rf-zpg-cod-pag         .
           move      "pgm/gep/fls/ioc/obj/iofzpg"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zpg                 .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valori                  *
      *                      *-----------------------------------------*
           move      rf-zpg-tip-pag       to   w-tot-tip-pag
                                              (w-det-vas-fop-ctr)     .
           move      rf-zpg-num-sca       to   w-tot-num-sca
                                              (w-det-vas-fop-ctr)     .
           move      rf-zpg-dec-prs       to   w-tot-dec-prs
                                              (w-det-vas-fop-ctr)     .
           move      rf-zpg-dap-mes       to   w-tot-dap-mes
                                              (w-det-vas-fop-ctr)     .
           move      rf-zpg-dap-gio       to   w-tot-dap-gio
                                              (w-det-vas-fop-ctr)     .
           move      rf-zpg-ggg-int       to   w-tot-ggg-int
                                              (w-det-vas-fop-ctr)     .
           move      rf-zpg-tip-scm       to   w-tot-tip-scm
                                              (w-det-vas-fop-ctr)     .
           move      rf-zpg-gio-scm       to   w-tot-gio-scm
                                              (w-det-vas-fop-ctr)     .
      *                  *---------------------------------------------*
      *                  * Campi derivati da lettura personalizzazioni *
      *                  * su gestione portafoglio                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione indice di comodo per ti- *
      *                      * po pagamento                            *
      *                      *-----------------------------------------*
           move      w-tot-tip-pag
                    (w-det-vas-fop-ctr)   to   w-det-vas-fop-wtp      .
      *              *-------------------------------------------------*
      *              * Riciclo su prossimo codice pagamento            *
      *              *-------------------------------------------------*
           go to     det-vas-fop-100.
       det-vas-fop-200.
      *              *-------------------------------------------------*
      *              * Campi derivati da record [dcf]                  *
      *              *-------------------------------------------------*
       det-vas-fop-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valori associati alle spese incasso        *
      *    *-----------------------------------------------------------*
       det-vas-spi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-civ-spi          .
           move      zero                 to   w-tot-eit-spi          .
           move      zero                 to   w-det-vas-spi-c01      .
       det-vas-spi-020.
           add       1                    to   w-det-vas-spi-c01      .
           if        w-det-vas-spi-c01    >    3
                     go to det-vas-spi-040.
           move      zero                 to   w-tot-tpg-spi
                                              (w-det-vas-spi-c01)     .
           move      zero                 to   w-tot-tfu-spi
                                              (w-det-vas-spi-c01)     .
           move      zero                 to   w-tot-amm-spi
                                              (w-det-vas-spi-c01)     .
           move      zero                 to   w-tot-per-spi
                                              (w-det-vas-spi-c01)     .
           go to     det-vas-spi-020.
       det-vas-spi-040.
      *              *-------------------------------------------------*
      *              * Se codice spesa incasso non esistente : uscita  *
      *              *-------------------------------------------------*
           if        w-det-vas-spi-cod     =    spaces
                     go to det-vas-spi-999.
      *              *-------------------------------------------------*
      *              * Lettura record [yin] principale                 *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPI    "         to   f-key                  .
           move      w-det-vas-spi-cod    to   rf-yin-cod-spi         .
           move      zero                 to   rf-yin-tip-pag         .
           move      "pgm/dcf/fls/ioc/obj/iofyin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yin                 .
      *              *-------------------------------------------------*
      *              * Se record non esistente : uscita                *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-vas-spi-999.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-yin-civ-spi       to   w-tot-civ-spi          .
      *              *-------------------------------------------------*
      *              * Ciclo per tre codici pagamento possibili        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-vas-spi-c01      .
       det-vas-spi-100.
           add       1                    to   w-det-vas-spi-c01      .
           if        w-det-vas-spi-c01    >    3
                     go to det-vas-spi-999.
           if        w-tot-cod-pag
                    (w-det-vas-spi-c01)   =    zero
                     go to det-vas-spi-100.
      *                  *---------------------------------------------*
      *                  * Ciclo su elementi tabella gia' presenti     *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-vas-spi-c02      .
       det-vas-spi-200.
           add       1                    to   w-det-vas-spi-c02      .
           if        w-det-vas-spi-c02    >    w-tot-eit-spi
                     go to det-vas-spi-300.
           if        w-tot-tpg-spi
                    (w-det-vas-spi-c02)   =    w-tot-tip-pag
                                              (w-det-vas-spi-c01)
                     go to det-vas-spi-100.
       det-vas-spi-300.
      *                  *---------------------------------------------*
      *                  * Lettura record [yin] relativo al tipo paga- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPI    "         to   f-key                  .
           move      w-det-vas-spi-cod    to   rf-yin-cod-spi         .
           move      w-tot-tip-pag
                    (w-det-vas-spi-c01)
                                          to   rf-yin-tip-pag         .
           move      "pgm/dcf/fls/ioc/obj/iofyin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-yin                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : riciclo             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-vas-spi-100.
      *                  *---------------------------------------------*
      *                  * Incremento numero elementi in tabella       *
      *                  *---------------------------------------------*
           add       1                    to   w-tot-eit-spi          .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori                      *
      *                  *---------------------------------------------*
           move      rf-yin-tip-pag       to   w-tot-tpg-spi
                                              (w-tot-eit-spi)         .
           move      rf-yin-tfu-spi       to   w-tot-tfu-spi
                                              (w-tot-eit-spi)         .
           move      rf-yin-amm-spi       to   w-tot-amm-spi
                                              (w-tot-eit-spi)         .
           move      rf-yin-per-spi       to   w-tot-per-spi
                                              (w-tot-eit-spi)         .
      *              *-------------------------------------------------*
      *              * Riciclo su prossimo codice pagamento            *
      *              *-------------------------------------------------*
           go to     det-vas-spi-100.
       det-vas-spi-999.
           exit.

      *    *===========================================================*
      *    * Determinazione valori prima bolla relativi alle spese     *
      *    * bollo                                                     *
      *    *-----------------------------------------------------------*
       det-vas-spb-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione iniziale                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-civ-spb          .
           move      zero                 to   w-tot-eit-spb          .
           move      zero                 to   w-det-vas-spb-c01      .
       det-vas-spb-020.
           add       1                    to   w-det-vas-spb-c01      .
           if        w-det-vas-spb-c01    >    3
                     go to det-vas-spb-040.
           move      zero                 to   w-tot-tpg-spb
                                              (w-det-vas-spb-c01)     .
           move      zero                 to   w-tot-tau-spb
                                              (w-det-vas-spb-c01)     .
           move      zero                 to   w-tot-per-spb
                                              (w-det-vas-spb-c01)     .
           move      zero                 to   w-det-vas-spb-c02      .
       det-vas-spb-030.
           add       1                    to   w-det-vas-spb-c02      .
           if        w-det-vas-spb-c02    >    10
                     go to det-vas-spb-035.
           move      zero                 to   w-tot-tbe-scg
                                              (w-det-vas-spb-c01
                                               w-det-vas-spb-c02)     .
           move      zero                 to   w-tot-tbe-asc
                                              (w-det-vas-spb-c01
                                               w-det-vas-spb-c02)     .
           move      zero                 to   w-tot-tbe-psc
                                              (w-det-vas-spb-c01
                                               w-det-vas-spb-c02)     .
           go to     det-vas-spb-030.
       det-vas-spb-035.
           move      zero                 to   w-tot-tet-spb
                                              (w-det-vas-spb-c01)     .
           move      zero                 to   w-tot-min-spb
                                              (w-det-vas-spb-c01)     .
           move      zero                 to   w-tot-max-spb
                                              (w-det-vas-spb-c01)     .
           move      zero                 to   w-tot-tar-spb
                                              (w-det-vas-spb-c01)     .
           move      zero                 to   w-tot-var-spb
                                              (w-det-vas-spb-c01)     .
           go to     det-vas-spb-020.
       det-vas-spb-040.
      *              *-------------------------------------------------*
      *              * Se codice spesa bollo non esistente : uscita    *
      *              *-------------------------------------------------*
           if        w-det-vas-spb-cod    =    spaces
                     go to det-vas-spb-999.
      *              *-------------------------------------------------*
      *              * Lettura record [ybo] principale                 *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPB    "         to   f-key                  .
           move      w-det-vas-spb-cod    to   rf-ybo-cod-spb         .
           move      zero                 to   rf-ybo-tip-pag         .
           move      "pgm/dcf/fls/ioc/obj/iofybo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybo                 .
      *              *-------------------------------------------------*
      *              * Se record non esistente : uscita                *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-vas-spb-999.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-ybo-civ-spb       to   w-tot-civ-spb          .
      *              *-------------------------------------------------*
      *              * Ciclo per tre codici pagamento possibili        *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-vas-spb-c01      .
       det-vas-spb-100.
           add       1                    to   w-det-vas-spb-c01      .
           if        w-det-vas-spb-c01    >    3
                     go to det-vas-spb-999.
           if        w-tot-cod-pag
                    (w-det-vas-spb-c01)   =    zero
                     go to det-vas-spb-100.
      *                  *---------------------------------------------*
      *                  * Ciclo su elementi tabella gia' presenti     *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-vas-spb-c02      .
       det-vas-spb-200.
           add       1                    to   w-det-vas-spb-c02      .
           if        w-det-vas-spb-c02    >    w-tot-eit-spb
                     go to det-vas-spb-300.
           if        w-tot-tpg-spb
                    (w-det-vas-spb-c02)   =    w-tot-tip-pag
                                              (w-det-vas-spb-c01)
                     go to det-vas-spb-100.
       det-vas-spb-300.
      *                  *---------------------------------------------*
      *                  * Lettura record [ybo] relativo al tipo paga- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPB    "         to   f-key                  .
           move      w-det-vas-spb-cod    to   rf-ybo-cod-spb         .
           move      w-tot-tip-pag
                    (w-det-vas-spb-c01)
                                          to   rf-ybo-tip-pag         .
           move      "pgm/dcf/fls/ioc/obj/iofybo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ybo                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : riciclo             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-vas-spb-100.
      *                  *---------------------------------------------*
      *                  * Incremento numero elementi in tabella       *
      *                  *---------------------------------------------*
           add       1                    to   w-tot-eit-spb          .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valori                      *
      *                  *---------------------------------------------*
           move      rf-ybo-tip-pag       to   w-tot-tpg-spb
                                              (w-tot-eit-spb)         .
           move      rf-ybo-tau-spb       to   w-tot-tau-spb
                                              (w-tot-eit-spb)         .
           move      rf-ybo-per-spb       to   w-tot-per-spb
                                              (w-tot-eit-spb)         .
           move      zero                 to   w-det-vas-spb-c02      .
       det-vas-spb-320.
           add       1                    to   w-det-vas-spb-c02      .
           if        w-det-vas-spb-c02    >    10
                     go to det-vas-spb-340.
           move      rf-ybo-tbe-scg
                    (w-det-vas-spb-c02)   to   w-tot-tbe-scg
                                              (w-tot-eit-spb,
                                               w-det-vas-spb-c02)     .
           move      rf-ybo-tbe-asc
                    (w-det-vas-spb-c02)   to   w-tot-tbe-asc
                                              (w-tot-eit-spb,
                                               w-det-vas-spb-c02)     .
           move      rf-ybo-tbe-psc
                    (w-det-vas-spb-c02)   to   w-tot-tbe-psc
                                              (w-tot-eit-spb,
                                               w-det-vas-spb-c02)     .
           go to     det-vas-spb-320.
       det-vas-spb-340.
           move      rf-ybo-tet-spb       to   w-tot-tet-spb
                                              (w-tot-eit-spb)         .
           move      rf-ybo-min-spb       to   w-tot-min-spb
                                              (w-tot-eit-spb)         .
           move      rf-ybo-max-spb       to   w-tot-max-spb
                                              (w-tot-eit-spb)         .
           move      rf-ybo-tar-spb       to   w-tot-tar-spb
                                              (w-tot-eit-spb)         .
           move      rf-ybo-var-spb       to   w-tot-var-spb
                                              (w-tot-eit-spb)         .
      *              *-------------------------------------------------*
      *              * Riciclo su prossimo codice pagamento            *
      *              *-------------------------------------------------*
           go to     det-vas-spb-100.
       det-vas-spb-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento castelletto iva                             *
      *    *-----------------------------------------------------------*
       agg-cst-iva-000.
      *              *-------------------------------------------------*
      *              * Se importo a zero : uscita                      *
      *              *-------------------------------------------------*
           if        w-agg-cst-iva-imp    =    zero
                     go to agg-cst-iva-999.
      *              *-------------------------------------------------*
      *              * Inversione del segno in caso di aggiornamento   *
      *              * negativo                                        *
      *              *-------------------------------------------------*
           if        w-agg-cst-iva-tip    =    "-"
                     multiply -1          by   w-agg-cst-iva-imp      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda che il campo abbia un'ali- *
      *              * quota propria oppure debba essere sventagliato  *
      *              * su tutto il castelletto iva                     *
      *              *-------------------------------------------------*
           if        w-agg-cst-iva-coi    not  = zero
                     go to agg-cst-iva-200
           else      go to agg-cst-iva-300.
       agg-cst-iva-200.
      *                  *---------------------------------------------*
      *                  * Caso in cui abbia un'aliquota propria       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Aggiornamento riga castelletto iva      *
      *                      *-----------------------------------------*
           move      zero                 to   w-agg-cst-iva-ctr      .
       agg-cst-iva-220.
           add       1                    to   w-agg-cst-iva-ctr      .
           if        w-agg-cst-iva-ctr    >    12
                     move  12             to   w-agg-cst-iva-ctr
                     go to agg-cst-iva-260.
           if        w-tot-iva-cod        
                    (w-agg-cst-iva-ctr)   =    zero
                     go to agg-cst-iva-240.
           if        w-agg-cst-iva-coi    =    w-tot-iva-cod
                                              (w-agg-cst-iva-ctr)
                     go to agg-cst-iva-260.
           go to     agg-cst-iva-220.
       agg-cst-iva-240.
           add       1                    to   w-tot-iva-ele          .
           move      w-agg-cst-iva-coi    to   w-tot-iva-cod
                                              (w-agg-cst-iva-ctr)     .
       agg-cst-iva-260.
           add       w-agg-cst-iva-imp    to   w-tot-iva-ibl
                                              (w-agg-cst-iva-ctr)     .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     agg-cst-iva-999.
       agg-cst-iva-300.
      *                  *---------------------------------------------*
      *                  * Caso in cui debba essere sventagliato sul   *
      *                  * castelletto iva gia' esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione del numero di elementi   *
      *                      * presenti in castelletto iva e del tota- *
      *                      * le imponibile                           *
      *                      *-----------------------------------------*
           move      zero                 to   w-agg-cst-iva-tot      .
           move      zero                 to   w-agg-cst-iva-ctr      .
       agg-cst-iva-320.
           add       1                    to   w-agg-cst-iva-ctr      .
           if        w-tot-iva-cod        
                    (w-agg-cst-iva-ctr)   =    zero
                     go to agg-cst-iva-330.
           add       w-tot-iva-ibl
                    (w-agg-cst-iva-ctr)   to   w-agg-cst-iva-tot      .
           go to     agg-cst-iva-320.
       agg-cst-iva-330.
      *                      *-----------------------------------------*
      *                      * Memorizzazione numero elementi in ca-   *
      *                      * stelletto iva                           *
      *                      *-----------------------------------------*
           move      w-agg-cst-iva-ctr    to   w-agg-cst-iva-max      .
      *                      *-----------------------------------------*
      *                      * Ciclo di aggiornamento castelletto iva  *
      *                      * fino all'elemento  w-agg-cst-iva-max    *
      *                      * escluso                                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-agg-cst-iva-wpa      .
           move      zero                 to   w-agg-cst-iva-ctr      .
       agg-cst-iva-340.
           add       1                    to   w-agg-cst-iva-ctr      .
           if        w-agg-cst-iva-ctr    =    w-agg-cst-iva-max
                     go to agg-cst-iva-360.
      *                          *-------------------------------------*
      *                          * Determinazione porzione dell'impor- *
      *                          * to relativa alla riga di castelletto*
      *                          * iva                                 *
      *                          *-------------------------------------*
           multiply  w-agg-cst-iva-imp    by   w-tot-iva-ibl
                                              (w-agg-cst-iva-ctr)
                                        giving w-agg-cst-iva-s18      .
           divide    w-agg-cst-iva-tot    into w-agg-cst-iva-s18
                                                         rounded      .
           add       w-agg-cst-iva-s18    to   w-tot-iva-ibl
                                              (w-agg-cst-iva-ctr)     .
           add       w-agg-cst-iva-s18    to   w-agg-cst-iva-wpa      .
           go to     agg-cst-iva-340.
       agg-cst-iva-360.
      *                      *-----------------------------------------*
      *                      * Trattamento ultimo elemento castelletto *
      *                      *-----------------------------------------*
           subtract  w-agg-cst-iva-wpa    from w-agg-cst-iva-imp
                                        giving w-agg-cst-iva-s11      .
           add       w-agg-cst-iva-s11    to   w-tot-iva-ibl
                                              (w-agg-cst-iva-max)     .
       agg-cst-iva-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione totali ordine                             *
      *    *-----------------------------------------------------------*
       ini-tot-orf-000.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Coefficiente di cambio va-  *
      *              * luta per fatturazione                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-oft-sgl-vpf       to   w-coe-cmb-vlt-sdv      .
           move      rf-oft-tdc-vpf       to   w-coe-cmb-vlt-tdc      .
           move      rf-oft-dat-doc       to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                  *---------------------------------------------*
      *                  * Se esito determinazione negativo : buffe-   *
      *                  * rizzazione coefficiente di cambio contenuto *
      *                  * nel record [ost]                            *
      *                  *---------------------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-oft-cdc-vpf to   w-coe-cmb-vlt-cdc      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione coefficiente di cambio      *
      *                  *---------------------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-tot-cdc-vpf          .
       ini-tot-orf-100.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Forma di pagamento          *
      *              *-------------------------------------------------*
           move      rf-oft-cod-fop       to   w-det-vas-fop-cod      .
           perform   det-vas-fop-000      thru det-vas-fop-999        .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Sconto in chiusura          *
      *              *-------------------------------------------------*
           move      w-ref-sco-chi-civ    to   w-tot-civ-scc          .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Sconto pagamento            *
      *              *-------------------------------------------------*
           move      w-ref-sco-pag-civ    to   w-tot-civ-scp          .
       ini-tot-orf-200.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Spese in fattura            *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-orf-wrk-ct1      .
       ini-tot-orf-220.
           add       1                    to   w-stp-orf-wrk-ct1      .
           if        w-stp-orf-wrk-ct1    >    6
                     go to ini-tot-orf-300.
           move      w-prs-spe-fat-civ
                    (w-stp-orf-wrk-ct1)   to   w-tot-spe-civ
                                              (w-stp-orf-wrk-ct1)     .
           go to     ini-tot-orf-220.
       ini-tot-orf-300.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Spese incasso               *
      *              *-------------------------------------------------*
           move      rf-oft-add-spi       to   w-det-vas-spi-cod      .
           perform   det-vas-spi-000      thru det-vas-spi-999        .
           move      zero                 to   w-tot-tot-sic          .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Spese bollo                 *
      *              *-------------------------------------------------*
           move      rf-oft-add-spb       to   w-det-vas-spb-cod      .
           perform   det-vas-spb-000      thru det-vas-spb-999        .
           move      zero                 to   w-tot-tot-spb          .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Totali documento            *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-rta          .
           move      zero                 to   w-tot-tot-rig (01)     .
           move      zero                 to   w-tot-tot-rig (02)     .
           move      zero                 to   w-tot-tot-rig (03)     .
           move      zero                 to   w-tot-tot-rig (04)     .
           move      zero                 to   w-tot-tot-rig (05)     .
           move      zero                 to   w-tot-tot-rig (06)     .
           move      zero                 to   w-tot-tot-rig (07)     .
           move      zero                 to   w-tot-tot-rig (08)     .
           move      zero                 to   w-tot-tot-rig (09)     .
           move      zero                 to   w-tot-tot-lor          .
           move      zero                 to   w-tot-tot-nsc          .
           move      zero                 to   w-tot-tot-nsf          .
           move      zero                 to   w-tot-tot-spe          .
           move      zero                 to   w-tot-tot-net          .
           move      zero                 to   w-tot-tot-ibl          .
           move      zero                 to   w-tot-tot-imp          .
           move      zero                 to   w-tot-tot-doc          .
       ini-tot-orf-400.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Castelletto iva             *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-iva-ele          .
           move      zero                 to   w-stp-orf-wrk-ct1      .
       ini-tot-orf-420.
           add       1                    to   w-stp-orf-wrk-ct1      .
           if        w-stp-orf-wrk-ct1    >    12
                     go to ini-tot-orf-500.
           move      zero                 to   w-tot-iva-cod
                                              (w-stp-orf-wrk-ct1)     .
           move      zero                 to   w-tot-iva-ibl
                                              (w-stp-orf-wrk-ct1)     .
           move      zero                 to   w-tot-iva-imp
                                              (w-stp-orf-wrk-ct1)     .
           go to     ini-tot-orf-420.
       ini-tot-orf-500.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Castelletto scadenze        *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-scd-ele          .
           move      zero                 to   w-stp-orf-wrk-ct1      .
       ini-tot-orf-520.
           add       1                    to   w-stp-orf-wrk-ct1      .
           if        w-stp-orf-wrk-ct1    >    96
                     go to ini-tot-orf-999.
           move      zero                 to   w-tot-scd-tip
                                              (w-stp-orf-wrk-ct1)     .
           move      zero                 to   w-tot-scd-dat
                                              (w-stp-orf-wrk-ct1)     .
           move      zero                 to   w-tot-scd-cau
                                              (w-stp-orf-wrk-ct1)     .
           move      zero                 to   w-tot-scd-imp
                                              (w-stp-orf-wrk-ct1)     .
           go to     ini-tot-orf-520.
       ini-tot-orf-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione totali bolla                              *
      *    *-----------------------------------------------------------*
       ini-tot-bfo-000.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Coefficiente di cambio va-  *
      *              * luta per fatturazione                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-bft-sgl-vpf       to   w-coe-cmb-vlt-sdv      .
           move      rf-bft-tdc-vpf       to   w-coe-cmb-vlt-tdc      .
           move      rf-bft-dat-doc       to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                  *---------------------------------------------*
      *                  * Se esito determinazione negativo : buffe-   *
      *                  * rizzazione coefficiente di cambio contenuto *
      *                  * nel record [ost]                            *
      *                  *---------------------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-bft-cdc-vpf to   w-coe-cmb-vlt-cdc      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione coefficiente di cambio      *
      *                  *---------------------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-tot-cdc-vpf          .
       ini-tot-bfo-100.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Forma di pagamento          *
      *              *-------------------------------------------------*
           move      rf-bft-cod-fop       to   w-det-vas-fop-cod      .
           perform   det-vas-fop-000      thru det-vas-fop-999        .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Sconto in chiusura          *
      *              *-------------------------------------------------*
           move      w-ref-sco-chi-civ    to   w-tot-civ-scc          .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Sconto pagamento            *
      *              *-------------------------------------------------*
           move      w-ref-sco-pag-civ    to   w-tot-civ-scp          .
       ini-tot-bfo-200.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Spese in fattura            *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-bfo-wrk-ct1      .
       ini-tot-bfo-220.
           add       1                    to   w-stp-bfo-wrk-ct1      .
           if        w-stp-bfo-wrk-ct1    >    6
                     go to ini-tot-bfo-300.
           move      w-prs-spe-fat-civ
                    (w-stp-bfo-wrk-ct1)   to   w-tot-spe-civ
                                              (w-stp-bfo-wrk-ct1)     .
           go to     ini-tot-bfo-220.
       ini-tot-bfo-300.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Spese incasso               *
      *              *-------------------------------------------------*
           move      rf-bft-add-spi       to   w-det-vas-spi-cod      .
           perform   det-vas-spi-000      thru det-vas-spi-999        .
           move      zero                 to   w-tot-tot-sic          .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Spese bollo                 *
      *              *-------------------------------------------------*
           move      rf-bft-add-spb       to   w-det-vas-spb-cod      .
           perform   det-vas-spb-000      thru det-vas-spb-999        .
           move      zero                 to   w-tot-tot-spb          .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Totali documento            *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-rta          .
           move      zero                 to   w-tot-tot-rig (01)     .
           move      zero                 to   w-tot-tot-rig (02)     .
           move      zero                 to   w-tot-tot-rig (03)     .
           move      zero                 to   w-tot-tot-rig (04)     .
           move      zero                 to   w-tot-tot-rig (05)     .
           move      zero                 to   w-tot-tot-rig (06)     .
           move      zero                 to   w-tot-tot-rig (07)     .
           move      zero                 to   w-tot-tot-rig (08)     .
           move      zero                 to   w-tot-tot-rig (09)     .
           move      zero                 to   w-tot-tot-lor          .
           move      zero                 to   w-tot-tot-nsc          .
           move      zero                 to   w-tot-tot-nsf          .
           move      zero                 to   w-tot-tot-spe          .
           move      zero                 to   w-tot-tot-net          .
           move      zero                 to   w-tot-tot-ibl          .
           move      zero                 to   w-tot-tot-imp          .
           move      zero                 to   w-tot-tot-doc          .
       ini-tot-bfo-400.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Castelletto iva             *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-iva-ele          .
           move      zero                 to   w-stp-bfo-wrk-ct1      .
       ini-tot-bfo-420.
           add       1                    to   w-stp-bfo-wrk-ct1      .
           if        w-stp-bfo-wrk-ct1    >    12
                     go to ini-tot-bfo-500.
           move      zero                 to   w-tot-iva-cod
                                              (w-stp-bfo-wrk-ct1)     .
           move      zero                 to   w-tot-iva-ibl
                                              (w-stp-bfo-wrk-ct1)     .
           move      zero                 to   w-tot-iva-imp
                                              (w-stp-bfo-wrk-ct1)     .
           go to     ini-tot-bfo-420.
       ini-tot-bfo-500.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Castelletto scadenze        *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-scd-ele          .
           move      zero                 to   w-stp-bfo-wrk-ct1      .
       ini-tot-bfo-520.
           add       1                    to   w-stp-bfo-wrk-ct1      .
           if        w-stp-bfo-wrk-ct1    >    96
                     go to ini-tot-bfo-999.
           move      zero                 to   w-tot-scd-tip
                                              (w-stp-bfo-wrk-ct1)     .
           move      zero                 to   w-tot-scd-dat
                                              (w-stp-bfo-wrk-ct1)     .
           move      zero                 to   w-tot-scd-cau
                                              (w-stp-bfo-wrk-ct1)     .
           move      zero                 to   w-tot-scd-imp
                                              (w-stp-bfo-wrk-ct1)     .
           go to     ini-tot-bfo-520.
       ini-tot-bfo-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totali da righe corpo documento            *
      *    *-----------------------------------------------------------*
       det-tri-orf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [ofr]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *              *-------------------------------------------------*
      *              * Lettura riga in corso di trattamento            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-rof-num-prt        to   rf-ofr-num-prt         .
           move      w-rof-key-prg 
                    (w-rof-ctr-ele)       to   rf-ofr-num-prg         .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : uscita              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-tri-orf-900.
       det-tri-orf-100.
      *              *-------------------------------------------------*
      *              * Cumulo riga documento                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scomposizione tipo riga                     *
      *                  *---------------------------------------------*
           move      rf-ofr-tip-rig       to   w-ctl-tip-rig-tri      .
           perform   ctl-tip-rig-000      thru ctl-tip-rig-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione della quantita' residua      *
      *                  *---------------------------------------------*
           move      "DT"                 to   d-qev-rof-tip-ope      .
           perform   det-qev-rof-cll-000  thru det-qev-rof-cll-999    .
      *                  *---------------------------------------------*
      *                  * Determinazione importo in riga              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se prezzo per quantita'            *
      *                      *-----------------------------------------*
           if        rf-ofr-snx-tum       not  = "P"
                     move  rf-ofr-prz-net to   w-stp-orf-wrk-ppi
                     go to det-tri-orf-120.
      *                      *-----------------------------------------*
      *                      * Determinazione prezzo unitario per il   *
      *                      * calcolo dell'importo in caso di tra-    *
      *                      * sformazione unita' di misura di tipo    *
      *                      * 'P'                                     *
      *                      *-----------------------------------------*
           divide    rf-ofr-qta-ord       into rf-ofr-imp-rig
                                        giving w-stp-orf-wrk-ppi
                                               rounded                .
           multiply  d-qev-rof-qta-dri    by   w-stp-orf-wrk-ppi
                                        giving w-det-imp-rig-imp      .
           move      w-det-imp-rig-imp    to   rf-ofr-imp-rig         .
           go to     det-tri-orf-125.
       det-tri-orf-120.
      *                      *-----------------------------------------*
      *                      * Determinazione importo                  *
      *                      *-----------------------------------------*
           move      w-stp-orf-wrk-ppi    to   w-det-imp-rig-prz      .
           move      rf-ofr-dec-prz       to   w-det-imp-rig-dec      .
           move      d-qev-rof-qta-dri    to   w-det-imp-rig-qta      .
           perform   det-imp-rig-000      thru det-imp-rig-999        .
           move      w-det-imp-rig-imp    to   rf-ofr-imp-rig         .
       det-tri-orf-125.
      *                  *---------------------------------------------*
      *                  * Trattamento prezzo di vendita e importo     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da effettuare                   *
      *                      *-----------------------------------------*
           if        w-ctl-tip-rig-tpr    =    "C" or
                     w-ctl-tip-rig-tpr    =    "A"
                     go to det-tri-orf-600.
      *                      *-----------------------------------------*
      *                      * Determinazione prezzo di acquisto nella *
      *                      * valuta base                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da effettuare               *
      *                          *-------------------------------------*
           if        rf-ofr-sgl-vpf       =    c-sgl
                     go to det-tri-orf-205.
      *                          *-------------------------------------*
      *                          * Trasformazione prezzo di vendita    *
      *                          * espresso nella valuta per il prezzo *
      *                          * nel valore espresso nella valuta    *
      *                          * base                                *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Determinazione coefficiente di  *
      *                              * cambio per la valuta per il     *
      *                              * prezzo alla data del documento  *
      *                              *---------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-ofr-sgl-vpf       to   w-coe-cmb-vlt-sdv      .
           move      rf-ofr-tdc-vpf       to   w-coe-cmb-vlt-tdc      .
           move      rf-ofr-dat-doc       to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se esito determinazione negati- *
      *                              * vo : bufferizzazione coeffi-    *
      *                              * ciente di cambio contenuto nel  *
      *                              * record [ofr]                    *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-ofr-cdc-vpp to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Conversione da valuta per il    *
      *                              * prezzo a valuta base            *
      *                              *---------------------------------*
           move      rf-ofr-sgl-vpf       to   w-cvs-vlt-sgl          .
           move      rf-ofr-dec-vpf       to   w-cvs-vlt-dec          .
           move      rf-ofr-tdc-vpf       to   w-cvs-vlt-tdc          .
           move      w-coe-cmb-vlt-cdc    to   w-cvs-vlt-cdc          .
           move      rf-ofr-prz-acq       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                              *---------------------------------*
      *                              * Bufferizzazione nuovo prezzo    *
      *                              *---------------------------------*
           move      w-cvs-vlt-avb        to   rf-ofr-prz-acq         .
      *                              *---------------------------------*
      *                              * Determinazione prezzo netto     *
      *                              *---------------------------------*
           move      rf-ofr-prz-acq       to   w-cal-prz-net-prz      .
           move      rf-ofr-per-scr (1)   to   w-cal-prz-net-psc (1)  .
           move      rf-ofr-per-scr (2)   to   w-cal-prz-net-psc (2)  .
           move      rf-ofr-per-scr (3)   to   w-cal-prz-net-psc (3)  .
           move      rf-ofr-per-scr (4)   to   w-cal-prz-net-psc (4)  .
           move      rf-ofr-per-scr (5)   to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   rf-ofr-prz-net         .
      *                              *---------------------------------*
      *                              * Determinazione importo in riga  *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Test se prezzo per quantita'*
      *                                  *-----------------------------*
           if        rf-ofr-snx-tum       not  = "P"
                     move  rf-ofr-prz-net to   w-stp-orf-wrk-ppi
                     go to det-tri-orf-140.
      *                                  *-----------------------------*
      *                                  * Determinazione prezzo uni-  *
      *                                  * tario per il calcolo del-   *
      *                                  * l'importo in caso di tras-  *
      *                                  * formazione unita' di misura *
      *                                  * di tipo 'P'                 *
      *                                  *-----------------------------*
           divide    rf-ofr-qta-ord       into rf-ofr-imp-rig
                                        giving w-stp-orf-wrk-ppi
                                               rounded                .
       det-tri-orf-140.
      *                                  *-----------------------------*
      *                                  * Determinazione importo      *
      *                                  *-----------------------------*
           move      w-stp-orf-wrk-ppi    to   w-det-imp-rig-prz      .
           move      rf-ofr-dec-prz       to   w-det-imp-rig-dec      .
           move      d-qev-rof-qta-dri    to   w-det-imp-rig-qta      .
           perform   det-imp-rig-000      thru det-imp-rig-999        .
           move      w-det-imp-rig-imp    to   rf-ofr-imp-rig         .
       det-tri-orf-205.
      *                      *-----------------------------------------*
      *                      * Determinazione prezzo di vendita con    *
      *                      * legame valutario                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da effettuare               *
      *                          *-------------------------------------*
           if        rf-ofr-sgl-vpl       =    spaces
                     go to det-tri-orf-600.
      *                          *-------------------------------------*
      *                          * Determinazione coefficiente di cam- *
      *                          * bio effettivo per il legame valuta- *
      *                          * rio alla data del documento         *
      *                          *-------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-ofr-sgl-vpl       to   w-coe-cmb-vlt-sdv      .
           move      rf-ofr-tdc-vpl       to   w-coe-cmb-vlt-tdc      .
           move      rf-ofr-dat-doc       to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se esito determinazione negati- *
      *                              * vo : bufferizzazione coeffi-    *
      *                              * ciente di cambio contenuto nel  *
      *                              * record [osr]                    *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-ofr-cdc-vpl to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Bufferizzazione coefficiente di *
      *                              * cambio determinato              *
      *                              *---------------------------------*
           move      w-coe-cmb-vlt-cdc    to   rf-ofr-cdc-vpl         .
      *                              *---------------------------------*
      *                              * Salvataggio prezzo di vendita   *
      *                              * precedente                      *
      *                              *---------------------------------*
           move      rf-ofr-prz-acq       to   w-sav-prz-acq          .
      *                              *---------------------------------*
      *                              * Applicazione cambio per legame  *
      *                              * valutario                       *
      *                              *---------------------------------*
           move      rf-ofr-prz-net       to   w-lvl-prz-prz          .
           move      rf-ofr-sgl-vpl       to   w-lvl-prz-vlt          .
           move      rf-ofr-tdc-vpl       to   w-lvl-prz-tdc          .
           move      rf-ofr-ccr-vpl       to   w-lvl-prz-ccr          .
           move      rf-ofr-cdc-vpl       to   w-lvl-prz-cdc          .
           move      rf-ofr-plm-vpl       to   w-lvl-prz-plm          .
           move      rf-ofr-tlm-vpl       to   w-lvl-prz-tlm          .
           perform   lvl-prz-det-000      thru lvl-prz-det-999        .
           move      w-lvl-prz-prz        to   rf-ofr-prz-acq         .
      *                              *---------------------------------*
      *                              * Se prezzo di vendita invariato: *
      *                              * oltre                           *
      *                              *---------------------------------*
           if        rf-ofr-prz-acq       =    w-sav-prz-acq
                     go to det-tri-orf-600.
      *                              *---------------------------------*
      *                              * Determinazione prezzo netto     *
      *                              *---------------------------------*
           move      rf-ofr-prz-acq       to   w-cal-prz-net-prz      .
           move      rf-ofr-per-scr (1)   to   w-cal-prz-net-psc (1)  .
           move      rf-ofr-per-scr (2)   to   w-cal-prz-net-psc (2)  .
           move      rf-ofr-per-scr (3)   to   w-cal-prz-net-psc (3)  .
           move      rf-ofr-per-scr (4)   to   w-cal-prz-net-psc (4)  .
           move      rf-ofr-per-scr (5)   to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   rf-ofr-prz-net         .
      *                              *---------------------------------*
      *                              * Determinazione importo in riga  *
      *                              *---------------------------------*
           move      rf-ofr-prz-net       to   w-det-imp-rig-prz      .
           move      rf-ofr-dec-prz       to   w-det-imp-rig-dec      .
           move      d-qev-rof-qta-dri    to   w-det-imp-rig-qta      .
           perform   det-imp-rig-000      thru det-imp-rig-999        .
           move      w-det-imp-rig-imp    to   rf-ofr-imp-rig         .
       det-tri-orf-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento totalizzatori per tipo prodotto   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se riga di omaggio : no totalizzazione      *
      *                  *---------------------------------------------*
           move      rf-ofr-cod-iva       to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    =    9
                     go to det-tri-orf-720.
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           if        rf-ofr-tip-pro       not  = zero
                     add   rf-ofr-imp-rig to   w-tot-tot-rig
                                              (rf-ofr-tip-pro)
           else      add   rf-ofr-imp-rig to   w-tot-tot-rta          .
       det-tri-orf-720.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      rf-ofr-cod-iva       to   w-agg-cst-iva-coi      .
           move      rf-ofr-imp-rig       to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
       det-tri-orf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-tri-orf-999.
       det-tri-orf-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totali da righe corpo documento            *
      *    *-----------------------------------------------------------*
       det-tri-bfo-000.
      *              *-------------------------------------------------*
      *              * Start su righe documento [bfr]                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      rf-bft-num-prt       to   rf-bfr-num-prt         .
           move      zero                 to   rf-bfr-num-prg         .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
           if        f-sts                =    e-not-err
                     go to det-tri-bfo-010.
      *                  *---------------------------------------------*
      *                  * Se Start non ottenuta : ad uscita           *
      *                  *---------------------------------------------*
           go to     det-tri-bfo-900.
       det-tri-bfo-010.
      *              *-------------------------------------------------*
      *              * Test se batch                                   *
      *              *-------------------------------------------------*
           if        rr-snx-btc           =    "S"
                     go to det-tri-bfo-020.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       det-tri-bfo-020.
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
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-tri-bfo-900.
       det-tri-bfo-060.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-bfr-num-prt       =    rf-bft-num-prt
                     go to det-tri-bfo-080.
      *                      *-----------------------------------------*
      *                      * A uscita                                *
      *                      *-----------------------------------------*
           go to     det-tri-bfo-900.
       det-tri-bfo-080.
      *                  *---------------------------------------------*
      *                  * Scomposizione tipo riga                     *
      *                  *---------------------------------------------*
           move      rf-bfr-tip-rig       to   w-ctl-tip-rig-tri      .
           perform   ctl-tip-rig-000      thru ctl-tip-rig-999        .
      *                  *---------------------------------------------*
      *                  * Se tipo riga 'C' : riciclo                  *
      *                  *---------------------------------------------*
           if        w-ctl-tip-rig-tpr    not  = "C"
                     go to det-tri-bfo-100.
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     det-tri-bfo-010.
       det-tri-bfo-100.
      *              *-------------------------------------------------*
      *              * Cumulo riga documento                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se riga bolla gia' fatturata           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su file [ffr]                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RIFBFO    "         to   f-key                  .
           move      rf-bfr-cod-dpz       to   rf-ffr-cod-dpz         .
           move      rf-bfr-num-prt       to   rf-ffr-bfo-prt         .
           move      rf-bfr-num-prg       to   rf-ffr-bfo-prg         .
           move      zero                 to   rf-ffr-num-prt         .
           move      zero                 to   rf-ffr-num-prg         .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *                          *-------------------------------------*
      *                          * Test su esito operazione            *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-tri-bfo-200.
      *                      *-----------------------------------------*
      *                      * Lettura primo record [ffr]              *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *                          *-------------------------------------*
      *                          * Test se 'At End'                    *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-tri-bfo-200.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-ffr-cod-dpz       not  = rf-bfr-cod-dpz or
                     rf-ffr-bfo-prt       not  = rf-bfr-num-prt or
                     rf-ffr-bfo-prg       not  = rf-bfr-num-prg
                     go to det-tri-bfo-200.
      *                      *-----------------------------------------*
      *                      * Riga fatturata quindi riciclo           *
      *                      *-----------------------------------------*
           go to     det-tri-bfo-010.
       det-tri-bfo-200.
      *                  *---------------------------------------------*
      *                  * Determinazione importo in riga              *
      *                  *                                             *
      *                  * N.B.: quello della riga stessa              *
      *                  *---------------------------------------------*
       det-tri-bfo-220.
      *                  *---------------------------------------------*
      *                  * Trattamento prezzo di vendita e importo     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da effettuare                   *
      *                      *-----------------------------------------*
           if        w-ctl-tip-rig-tpr    =    "C" or
                     w-ctl-tip-rig-tpr    =    "A"
                     go to det-tri-bfo-600.
      *                      *-----------------------------------------*
      *                      * Determinazione prezzo di acquisto nella *
      *                      * valuta base                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da effettuare               *
      *                          *-------------------------------------*
           if        rf-bfr-sgl-vpf       =    c-sgl
                     go to det-tri-bfo-300.
      *                          *-------------------------------------*
      *                          * Trasformazione prezzo di vendita    *
      *                          * espresso nella valuta per il prezzo *
      *                          * nel valore espresso nella valuta    *
      *                          * di fatturazione                     *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Determinazione coefficiente di  *
      *                              * cambio per la valuta per il     *
      *                              * prezzo alla data del documento  *
      *                              *---------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-bfr-sgl-vpf       to   w-coe-cmb-vlt-sdv      .
           move      rf-bfr-tdc-vpf       to   w-coe-cmb-vlt-tdc      .
           move      rf-bfr-dat-doc       to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se esito determinazione negati- *
      *                              * vo : bufferizzazione coeffi-    *
      *                              * ciente di cambio contenuto nel  *
      *                              * record [osr]                    *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-bfr-cdc-vpp to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Conversione da valuta per il    *
      *                              * prezzo a valuta base            *
      *                              *---------------------------------*
           move      rf-bfr-sgl-vpf       to   w-cvs-vlt-sgl          .
           move      rf-bfr-dec-vpf       to   w-cvs-vlt-dec          .
           move      rf-bfr-tdc-vpf       to   w-cvs-vlt-tdc          .
           move      w-coe-cmb-vlt-cdc    to   w-cvs-vlt-cdc          .
           move      rf-bfr-prz-acq       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                              *---------------------------------*
      *                              * Bufferizzazione nuovo prezzo    *
      *                              *---------------------------------*
           move      w-cvs-vlt-avb        to   rf-bfr-prz-acq         .
      *                              *---------------------------------*
      *                              * Determinazione prezzo netto     *
      *                              *---------------------------------*
           move      rf-bfr-prz-acq       to   w-cal-prz-net-prz      .
           move      rf-bfr-per-scr (1)   to   w-cal-prz-net-psc (1)  .
           move      rf-bfr-per-scr (2)   to   w-cal-prz-net-psc (2)  .
           move      rf-bfr-per-scr (3)   to   w-cal-prz-net-psc (3)  .
           move      rf-bfr-per-scr (4)   to   w-cal-prz-net-psc (4)  .
           move      rf-bfr-per-scr (5)   to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   rf-bfr-prz-net         .
      *                              *---------------------------------*
      *                              * Determinazione importo in riga  *
      *                              *                                 *
      *                              * N.B.: quello della riga stessa  *
      *                              *---------------------------------*
       det-tri-bfo-300.
      *                      *-----------------------------------------*
      *                      * Determinazione prezzo di vendita con    *
      *                      * legame valutario                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da effettuare               *
      *                          *-------------------------------------*
           if        rf-bfr-sgl-vpl       =    spaces
                     go to det-tri-bfo-600.
      *                          *-------------------------------------*
      *                          * Determinazione coefficiente di cam- *
      *                          * bio effettivo per il legame valuta- *
      *                          * rio alla data del documento         *
      *                          *-------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-bfr-sgl-vpl       to   w-coe-cmb-vlt-sdv      .
           move      rf-bfr-tdc-vpl       to   w-coe-cmb-vlt-tdc      .
           move      rf-bfr-dat-doc       to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se esito determinazione negati- *
      *                              * vo : bufferizzazione coeffi-    *
      *                              * ciente di cambio contenuto nel  *
      *                              * record [osr]                    *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-bfr-cdc-vpl to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Bufferizzazione coefficiente di *
      *                              * cambio determinato              *
      *                              *---------------------------------*
           move      w-coe-cmb-vlt-cdc    to   rf-bfr-cdc-vpl         .
      *                              *---------------------------------*
      *                              * Salvataggio prezzo di vendita   *
      *                              * precedente                      *
      *                              *---------------------------------*
           move      rf-bfr-prz-acq       to   w-sav-prz-acq          .
      *                              *---------------------------------*
      *                              * Applicazione cambio per legame  *
      *                              * valutario                       *
      *                              *---------------------------------*
           move      rf-bfr-prz-net       to   w-lvl-prz-prz          .
           move      rf-bfr-sgl-vpl       to   w-lvl-prz-vlt          .
           move      rf-bfr-tdc-vpl       to   w-lvl-prz-tdc          .
           move      rf-bfr-ccr-vpl       to   w-lvl-prz-ccr          .
           move      rf-bfr-cdc-vpl       to   w-lvl-prz-cdc          .
           move      rf-bfr-plm-vpl       to   w-lvl-prz-plm          .
           move      rf-bfr-tlm-vpl       to   w-lvl-prz-tlm          .
           perform   lvl-prz-det-000      thru lvl-prz-det-999        .
           move      w-lvl-prz-prz        to   rf-bfr-prz-acq         .
      *                              *---------------------------------*
      *                              * Se prezzo di vendita invariato: *
      *                              * oltre                           *
      *                              *---------------------------------*
           if        rf-bfr-prz-acq       =    w-sav-prz-acq
                     go to det-tri-bfo-600.
      *                              *---------------------------------*
      *                              * Determinazione prezzo netto     *
      *                              *---------------------------------*
           move      rf-bfr-prz-acq       to   w-cal-prz-net-prz      .
           move      rf-bfr-per-scr (1)   to   w-cal-prz-net-psc (1)  .
           move      rf-bfr-per-scr (2)   to   w-cal-prz-net-psc (2)  .
           move      rf-bfr-per-scr (3)   to   w-cal-prz-net-psc (3)  .
           move      rf-bfr-per-scr (4)   to   w-cal-prz-net-psc (4)  .
           move      rf-bfr-per-scr (5)   to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   rf-bfr-prz-net         .
      *                              *---------------------------------*
      *                              * Determinazione importo in riga  *
      *                              *                                 *
      *                              * N.B.: quello della riga stessa  *
      *                              *---------------------------------*
       det-tri-bfo-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento totalizzatori per tipo prodotto   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se riga di omaggio : no totalizzazione      *
      *                  *---------------------------------------------*
           move      rf-bfr-cod-iva       to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    =    9
                     go to det-tri-bfo-720.
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           if        rf-bfr-tip-pro       not  = zero
                     add   rf-bfr-imp-rig to   w-tot-tot-rig
                                              (rf-bfr-tip-pro)
           else      add   rf-bfr-imp-rig to   w-tot-tot-rta          .
       det-tri-bfo-720.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      rf-bfr-cod-iva       to   w-agg-cst-iva-coi      .
           move      rf-bfr-imp-rig       to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
       det-tri-bfo-800.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale file [osr]       *
      *              *-------------------------------------------------*
           go to     det-tri-bfo-010.
       det-tri-bfo-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-tri-bfo-999.
       det-tri-bfo-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totali ordine                              *
      *    *-----------------------------------------------------------*
       det-tot-orf-000.
      *              *-------------------------------------------------*
      *              * Determinazione totale lordo                     *
      *              *-------------------------------------------------*
           perform   det-tot-lor-000      thru det-tot-lor-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale sconto in chiusura        *
      *              *-------------------------------------------------*
           move      rf-oft-tot-scc       to   w-det-tot-scc-tot      .
           move      rf-oft-per-scc       to   w-det-tot-scc-per      .
           perform   det-tot-scc-000      thru det-tot-scc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale al netto dello sconto in  *
      *              * chiusura                                        *
      *              *-------------------------------------------------*
           perform   det-tot-nsc-000      thru det-tot-nsc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione importi spese                    *
      *              *-------------------------------------------------*
           perform   det-imp-spe-orf-000  thru det-imp-spe-orf-999    .
      *              *-------------------------------------------------*
      *              * Determinazione totale al netto delle spese in   *
      *              * fattura                                         *
      *              *-------------------------------------------------*
           perform   det-tot-nsf-orf-000  thru det-tot-nsf-orf-999    .
      *              *-------------------------------------------------*
      *              * Determinazione totale sconto pagamento          *
      *              *-------------------------------------------------*
           move      rf-oft-tot-scp       to   w-det-tot-scp-tot      .
           move      rf-oft-per-scp       to   w-det-tot-scp-per      .
           perform   det-tot-scp-000      thru det-tot-scp-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale netto                     *
      *              *-------------------------------------------------*
           perform   det-tot-net-000      thru det-tot-net-999        .
      *              *-------------------------------------------------*
      *              * Determinazione di:                              *
      *              * - totale imponibile                             *
      *              * - totale imposta                                *
      *              * - totale documento                              *
      *              * - castelletto scadenze                          *
      *              * prima del calcolo spese incasso e spese bollo   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione totale imponibile            *
      *                  *---------------------------------------------*
           perform   det-tot-ibl-000      thru det-tot-ibl-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale imposta               *
      *                  *---------------------------------------------*
           perform   det-tot-imp-000      thru det-tot-imp-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale documento             *
      *                  *---------------------------------------------*
           perform   det-tot-doc-000      thru det-tot-doc-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione scadenze del documento       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri in input         *
      *                      *-----------------------------------------*
           move      rf-oft-cod-fop       to   w-clc-tbl-scd-fdp      .
           move      w-rof-rot-prv        to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
           move      rf-oft-pag-qaf       to   w-clc-tbl-scd-qaf      .
           move      rf-oft-pag-act       to   w-clc-tbl-scd-acc      .
           move      rf-oft-pag-dsm       to   w-clc-tbl-scd-ddp      .
           perform   det-sca-doc-000      thru det-sca-doc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione spese incasso calcolate          *
      *              *-------------------------------------------------*
           perform   det-tot-sic-000      thru det-tot-sic-999        .
      *              *-------------------------------------------------*
      *              * Determinazione di:                              *
      *              * - totale imponibile                             *
      *              * - totale imposta                                *
      *              * - totale documento                              *
      *              * - castelletto scadenze                          *
      *              * prima del calcolo spese bollo                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione totale imponibile            *
      *                  *---------------------------------------------*
           perform   det-tot-ibl-000      thru det-tot-ibl-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale imposta               *
      *                  *---------------------------------------------*
           perform   det-tot-imp-000      thru det-tot-imp-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale documento             *
      *                  *---------------------------------------------*
           perform   det-tot-doc-000      thru det-tot-doc-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione scadenze del documento       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri in input         *
      *                      *-----------------------------------------*
           move      rf-oft-cod-fop       to   w-clc-tbl-scd-fdp      .
           move      w-rof-rot-prv        to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
           move      rf-oft-pag-qaf       to   w-clc-tbl-scd-qaf      .
           move      rf-oft-pag-act       to   w-clc-tbl-scd-acc      .
           move      rf-oft-pag-dsm       to   w-clc-tbl-scd-ddp      .
           perform   det-sca-doc-000      thru det-sca-doc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione spese bollo                      *
      *              *-------------------------------------------------*
           perform   det-tot-spb-000      thru det-tot-spb-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale spese                     *
      *              *-------------------------------------------------*
           perform   det-tot-spe-000      thru det-tot-spe-999        .
      *              *-------------------------------------------------*
      *              * Determinazione di:                              *
      *              * - totale imponibile                             *
      *              * - totale imposta                                *
      *              * - totale documento                              *
      *              * dopo la determinazione di spese incasso e bollo *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione totale imponibile            *
      *                  *---------------------------------------------*
           perform   det-tot-ibl-000      thru det-tot-ibl-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale imposta               *
      *                  *---------------------------------------------*
           perform   det-tot-imp-000      thru det-tot-imp-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale documento             *
      *                  *---------------------------------------------*
           perform   det-tot-doc-000      thru det-tot-doc-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione scadenze del documento       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri in input         *
      *                      *-----------------------------------------*
           move      rf-oft-cod-fop       to   w-clc-tbl-scd-fdp      .
           move      w-rof-rot-prv        to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
           move      rf-oft-pag-qaf       to   w-clc-tbl-scd-qaf      .
           move      rf-oft-pag-act       to   w-clc-tbl-scd-acc      .
           move      rf-oft-pag-dsm       to   w-clc-tbl-scd-ddp      .
           perform   det-sca-doc-000      thru det-sca-doc-999        .
      *              *-------------------------------------------------*
      *              * Compattamento castelletto iva                   *
      *              *-------------------------------------------------*
           perform   cmp-cst-iva-000      thru cmp-cst-iva-999        .
       det-tot-orf-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totali bolla                               *
      *    *-----------------------------------------------------------*
       det-tot-bfo-000.
      *              *-------------------------------------------------*
      *              * Determinazione totale lordo                     *
      *              *-------------------------------------------------*
           perform   det-tot-lor-000      thru det-tot-lor-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale sconto in chiusura        *
      *              *-------------------------------------------------*
           move      rf-bft-tot-scc       to   w-det-tot-scc-tot      .
           move      rf-bft-per-scc       to   w-det-tot-scc-per      .
           perform   det-tot-scc-000      thru det-tot-scc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale al netto dello sconto in  *
      *              * chiusura                                        *
      *              *-------------------------------------------------*
           perform   det-tot-nsc-000      thru det-tot-nsc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione importi spese                    *
      *              *-------------------------------------------------*
           perform   det-imp-spe-bfo-000  thru det-imp-spe-bfo-999    .
      *              *-------------------------------------------------*
      *              * Determinazione totale al netto delle spese in   *
      *              * fattura                                         *
      *              *-------------------------------------------------*
           perform   det-tot-nsf-bfo-000  thru det-tot-nsf-bfo-999    .
      *              *-------------------------------------------------*
      *              * Determinazione totale sconto pagamento          *
      *              *-------------------------------------------------*
           move      rf-bft-tot-scp       to   w-det-tot-scp-tot      .
           move      rf-bft-per-scp       to   w-det-tot-scp-per      .
           perform   det-tot-scp-000      thru det-tot-scp-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale netto                     *
      *              *-------------------------------------------------*
           perform   det-tot-net-000      thru det-tot-net-999        .
      *              *-------------------------------------------------*
      *              * Determinazione di:                              *
      *              * - totale imponibile                             *
      *              * - totale imposta                                *
      *              * - totale documento                              *
      *              * - castelletto scadenze                          *
      *              * prima del calcolo spese incasso e spese bollo   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione totale imponibile            *
      *                  *---------------------------------------------*
           perform   det-tot-ibl-000      thru det-tot-ibl-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale imposta               *
      *                  *---------------------------------------------*
           perform   det-tot-imp-000      thru det-tot-imp-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale documento             *
      *                  *---------------------------------------------*
           perform   det-tot-doc-000      thru det-tot-doc-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione scadenze del documento       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri in input         *
      *                      *-----------------------------------------*
           move      rf-bft-cod-fop       to   w-clc-tbl-scd-fdp      .
           move      rf-bft-dat-reg       to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
           move      rf-bft-pag-qaf       to   w-clc-tbl-scd-qaf      .
           move      rf-bft-pag-act       to   w-clc-tbl-scd-acc      .
           move      rf-bft-pag-dsm       to   w-clc-tbl-scd-ddp      .
           perform   det-sca-doc-000      thru det-sca-doc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione spese incasso calcolate          *
      *              *-------------------------------------------------*
           perform   det-tot-sic-000      thru det-tot-sic-999        .
      *              *-------------------------------------------------*
      *              * Determinazione di:                              *
      *              * - totale imponibile                             *
      *              * - totale imposta                                *
      *              * - totale documento                              *
      *              * - castelletto scadenze                          *
      *              * prima del calcolo spese bollo                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione totale imponibile            *
      *                  *---------------------------------------------*
           perform   det-tot-ibl-000      thru det-tot-ibl-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale imposta               *
      *                  *---------------------------------------------*
           perform   det-tot-imp-000      thru det-tot-imp-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale documento             *
      *                  *---------------------------------------------*
           perform   det-tot-doc-000      thru det-tot-doc-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione scadenze del documento       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri in input         *
      *                      *-----------------------------------------*
           move      rf-bft-cod-fop       to   w-clc-tbl-scd-fdp      .
           move      rf-bft-dat-reg       to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
           move      rf-bft-pag-qaf       to   w-clc-tbl-scd-qaf      .
           move      rf-bft-pag-act       to   w-clc-tbl-scd-acc      .
           move      rf-bft-pag-dsm       to   w-clc-tbl-scd-ddp      .
           perform   det-sca-doc-000      thru det-sca-doc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione spese bollo                      *
      *              *-------------------------------------------------*
           perform   det-tot-spb-000      thru det-tot-spb-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale spese                     *
      *              *-------------------------------------------------*
           perform   det-tot-spe-000      thru det-tot-spe-999        .
      *              *-------------------------------------------------*
      *              * Determinazione di:                              *
      *              * - totale imponibile                             *
      *              * - totale imposta                                *
      *              * - totale documento                              *
      *              * dopo la determinazione di spese incasso e bollo *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione totale imponibile            *
      *                  *---------------------------------------------*
           perform   det-tot-ibl-000      thru det-tot-ibl-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale imposta               *
      *                  *---------------------------------------------*
           perform   det-tot-imp-000      thru det-tot-imp-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione totale documento             *
      *                  *---------------------------------------------*
           perform   det-tot-doc-000      thru det-tot-doc-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione scadenze del documento       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione parametri in input         *
      *                      *-----------------------------------------*
           move      rf-bft-cod-fop       to   w-clc-tbl-scd-fdp      .
           move      rf-bft-dat-reg       to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
           move      rf-bft-pag-qaf       to   w-clc-tbl-scd-qaf      .
           move      rf-bft-pag-act       to   w-clc-tbl-scd-acc      .
           move      rf-bft-pag-dsm       to   w-clc-tbl-scd-ddp      .
           perform   det-sca-doc-000      thru det-sca-doc-999        .
      *              *-------------------------------------------------*
      *              * Compattamento castelletto iva                   *
      *              *-------------------------------------------------*
           perform   cmp-cst-iva-000      thru cmp-cst-iva-999        .
       det-tot-bfo-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale lordo                               *
      *    *-----------------------------------------------------------*
       det-tot-lor-000.
           add       w-tot-tot-rig (1)
                     w-tot-tot-rig (2)
                     w-tot-tot-rig (3)
                     w-tot-tot-rig (4)
                     w-tot-tot-rig (5)
                     w-tot-tot-rig (6)
                     w-tot-tot-rig (7)
                     w-tot-tot-rig (8)
                     w-tot-tot-rig (9)
                                        giving w-tot-tot-lor          .
       det-tot-lor-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale sconto in chiusura                  *
      *    *-----------------------------------------------------------*
       det-tot-scc-000.
      *              *-------------------------------------------------*
      *              * Se sia la % che l'importo sono a zero : uscita  *
      *              *-------------------------------------------------*
           if        w-det-tot-scc-tot    =    zero and
                     w-det-tot-scc-per    =    zero
                     go to det-tot-scc-999.
      *              *-------------------------------------------------*
      *              * Se % a zero vuol dire che l'importo dello scon- *
      *              * to in chiusura e' stato impostato manualmente   *
      *              *-------------------------------------------------*
           if        w-det-tot-scc-per    =    zero
                     go to det-tot-scc-100.
      *              *-------------------------------------------------*
      *              * Calcolo                                         *
      *              *-------------------------------------------------*
           multiply  w-det-tot-scc-per    by   w-tot-tot-lor
                                        giving w-det-tot-scc-tot      .
           divide    100                  into w-det-tot-scc-tot
                                                     rounded          .
       det-tot-scc-100.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "-"                  to   w-agg-cst-iva-tip      .
           move      w-tot-civ-scc        to   w-agg-cst-iva-coi      .
           move      w-det-tot-scc-tot    to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
       det-tot-scc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale al netto dello sonto in chiusura    *
      *    *-----------------------------------------------------------*
       det-tot-nsc-000.
           subtract  w-det-tot-scc-tot    from w-tot-tot-lor
                                        giving w-tot-tot-nsc          .
       det-tot-nsc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione importo spese ordine fornitore             *
      *    *-----------------------------------------------------------*
       det-imp-spe-orf-000.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione su tabella spese             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-imp-spe-c01      .
       det-imp-spe-orf-100.
           add       1                    to   w-det-imp-spe-c01      .
           if        w-det-imp-spe-c01    >    6
                     go to det-imp-spe-orf-999.
      *              *-------------------------------------------------*
      *              * Se spesa da non addebitare : riciclo            *
      *              *-------------------------------------------------*
           if        rf-oft-spe-snx
                    (w-det-imp-spe-c01)   =    0
                     go to det-imp-spe-orf-100.
      *              *-------------------------------------------------*
      *              * Se sia la % che l'importo sono a zero : riciclo *
      *              *-------------------------------------------------*
           if        rf-oft-spe-per
                    (w-det-imp-spe-c01)   =    zero and
                     rf-oft-spe-imp
                    (w-det-imp-spe-c01)   =    zero
                     go to det-imp-spe-orf-100.
      *              *-------------------------------------------------*
      *              * Se % a zero vuol dire che l'importo spesa e'    *
      *              * dato dalla somma degli importi delle singole    *
      *              * bolle                                           *
      *              *-------------------------------------------------*
           if        rf-oft-spe-per
                    (w-det-imp-spe-c01)   =    zero
                     go to det-imp-spe-orf-500.
      *              *-------------------------------------------------*
      *              * Calcolo in base alla percentuale                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione imponibile per la spesa     *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-imp-spe-ibl      .
      *                  *---------------------------------------------*
      *                  * Determinazione imponibile per la spesa      *
      *                  *---------------------------------------------*
           if        rf-oft-spe-ibl
                    (w-det-imp-spe-c01)   =    01
                     move w-tot-tot-lor   to   w-det-imp-spe-ibl
           else if   rf-oft-spe-ibl
                    (w-det-imp-spe-c01)   =    02
                     move w-tot-tot-nsc   to   w-det-imp-spe-ibl
           else if   rf-oft-spe-ibl
                    (w-det-imp-spe-c01)   =    03
                     go to det-imp-spe-orf-220
           else if   rf-oft-spe-ibl
                    (w-det-imp-spe-c01)   =    11
                     move w-tot-tot-rig (1)
                                          to   w-det-imp-spe-ibl
           else if   rf-oft-spe-ibl
                    (w-det-imp-spe-c01)   =    12
                     move w-tot-tot-rig (2)
                                          to   w-det-imp-spe-ibl
           else if   rf-oft-spe-ibl
                    (w-det-imp-spe-c01)   =    13
                     move w-tot-tot-rig (3)
                                          to   w-det-imp-spe-ibl
           else if   rf-oft-spe-ibl
                    (w-det-imp-spe-c01)   =    14
                     move w-tot-tot-rig (4)
                                          to   w-det-imp-spe-ibl
           else if   rf-oft-spe-ibl
                    (w-det-imp-spe-c01)   =    15
                     move w-tot-tot-rig (5)
                                          to   w-det-imp-spe-ibl
           else if   rf-oft-spe-ibl
                    (w-det-imp-spe-c01)   =    16
                     move w-tot-tot-rig (6)
                                          to   w-det-imp-spe-ibl
           else if   rf-oft-spe-ibl
                    (w-det-imp-spe-c01)   =    17
                     move w-tot-tot-rig (7)
                                          to   w-det-imp-spe-ibl
           else if   rf-oft-spe-ibl
                    (w-det-imp-spe-c01)   =    18
                     move w-tot-tot-rig (8)
                                          to   w-det-imp-spe-ibl
           else if   rf-oft-spe-ibl
                    (w-det-imp-spe-c01)   =    19
                     move w-tot-tot-rig (9)
                                          to   w-det-imp-spe-ibl      .
           go to     det-imp-spe-orf-250.
       det-imp-spe-orf-220.
      *                      *-----------------------------------------*
      *                      * Se risulta da una combinazione di tota- *
      *                      * li                                      *
      *                      *-----------------------------------------*
           move      w-prs-spe-fat-ibt
                    (w-det-imp-spe-c01)   to   w-det-imp-spe-ibt      .
           move      zero                 to   w-det-imp-spe-c02      .
       det-imp-spe-orf-225.
           add       1                    to   w-det-imp-spe-c02      .
           if        w-det-imp-spe-c02    >    9
                     go to det-imp-spe-orf-250.
           if        w-det-imp-spe-ibx
                    (w-det-imp-spe-c02)   not  = "S"
                     go to det-imp-spe-orf-225.
           add       w-tot-tot-rig
                    (w-det-imp-spe-c02)   to   w-det-imp-spe-ibl      .
           go to     det-imp-spe-orf-225.
       det-imp-spe-orf-250.
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  rf-oft-spe-per
                    (w-det-imp-spe-c01)   by   w-det-imp-spe-ibl
                                        giving rf-oft-spe-imp
                                              (w-det-imp-spe-c01)     .
           divide    100                  into rf-oft-spe-imp
                                              (w-det-imp-spe-c01)
                                                         rounded      .
       det-imp-spe-orf-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      w-tot-spe-civ
                    (w-det-imp-spe-c01)   to   w-agg-cst-iva-coi      .
           move      rf-oft-spe-imp
                    (w-det-imp-spe-c01)   to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
      *              *-------------------------------------------------*
      *              * Riciclo su scansione tabella                    *
      *              *-------------------------------------------------*
           go to     det-imp-spe-orf-100.
       det-imp-spe-orf-999.
           exit.

      *    *===========================================================*
      *    * Determinazione importo spese bolla fornitore              *
      *    *-----------------------------------------------------------*
       det-imp-spe-bfo-000.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione su tabella spese             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-imp-spe-c01      .
       det-imp-spe-bfo-100.
           add       1                    to   w-det-imp-spe-c01      .
           if        w-det-imp-spe-c01    >    6
                     go to det-imp-spe-bfo-999.
      *              *-------------------------------------------------*
      *              * Se spesa da non addebitare : riciclo            *
      *              *-------------------------------------------------*
           if        rf-bft-spe-snx
                    (w-det-imp-spe-c01)   =    0
                     go to det-imp-spe-bfo-100.
      *              *-------------------------------------------------*
      *              * Se sia la % che l'importo sono a zero : riciclo *
      *              *-------------------------------------------------*
           if        rf-bft-spe-per
                    (w-det-imp-spe-c01)   =    zero and
                     rf-bft-spe-imp
                    (w-det-imp-spe-c01)   =    zero
                     go to det-imp-spe-bfo-100.
      *              *-------------------------------------------------*
      *              * Se % a zero vuol dire che l'importo spesa e'    *
      *              * dato dalla somma degli importi delle singole    *
      *              * bolle                                           *
      *              *-------------------------------------------------*
           if        rf-bft-spe-per
                    (w-det-imp-spe-c01)   =    zero
                     go to det-imp-spe-bfo-500.
      *              *-------------------------------------------------*
      *              * Calcolo in base alla percentuale                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione imponibile per la spesa     *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-imp-spe-ibl      .
      *                  *---------------------------------------------*
      *                  * Determinazione imponibile per la spesa      *
      *                  *---------------------------------------------*
           if        rf-bft-spe-ibl
                    (w-det-imp-spe-c01)   =    01
                     move w-tot-tot-lor   to   w-det-imp-spe-ibl
           else if   rf-bft-spe-ibl
                    (w-det-imp-spe-c01)   =    02
                     move w-tot-tot-nsc   to   w-det-imp-spe-ibl
           else if   rf-bft-spe-ibl
                    (w-det-imp-spe-c01)   =    03
                     go to det-imp-spe-bfo-220
           else if   rf-bft-spe-ibl
                    (w-det-imp-spe-c01)   =    11
                     move w-tot-tot-rig (1)
                                          to   w-det-imp-spe-ibl
           else if   rf-bft-spe-ibl
                    (w-det-imp-spe-c01)   =    12
                     move w-tot-tot-rig (2)
                                          to   w-det-imp-spe-ibl
           else if   rf-bft-spe-ibl
                    (w-det-imp-spe-c01)   =    13
                     move w-tot-tot-rig (3)
                                          to   w-det-imp-spe-ibl
           else if   rf-bft-spe-ibl
                    (w-det-imp-spe-c01)   =    14
                     move w-tot-tot-rig (4)
                                          to   w-det-imp-spe-ibl
           else if   rf-bft-spe-ibl
                    (w-det-imp-spe-c01)   =    15
                     move w-tot-tot-rig (5)
                                          to   w-det-imp-spe-ibl
           else if   rf-bft-spe-ibl
                    (w-det-imp-spe-c01)   =    16
                     move w-tot-tot-rig (6)
                                          to   w-det-imp-spe-ibl
           else if   rf-bft-spe-ibl
                    (w-det-imp-spe-c01)   =    17
                     move w-tot-tot-rig (7)
                                          to   w-det-imp-spe-ibl
           else if   rf-bft-spe-ibl
                    (w-det-imp-spe-c01)   =    18
                     move w-tot-tot-rig (8)
                                          to   w-det-imp-spe-ibl
           else if   rf-bft-spe-ibl
                    (w-det-imp-spe-c01)   =    19
                     move w-tot-tot-rig (9)
                                          to   w-det-imp-spe-ibl      .
           go to     det-imp-spe-bfo-250.
       det-imp-spe-bfo-220.
      *                      *-----------------------------------------*
      *                      * Se risulta da una combinazione di tota- *
      *                      * li                                      *
      *                      *-----------------------------------------*
           move      w-prs-spe-fat-ibt
                    (w-det-imp-spe-c01)   to   w-det-imp-spe-ibt      .
           move      zero                 to   w-det-imp-spe-c02      .
       det-imp-spe-bfo-225.
           add       1                    to   w-det-imp-spe-c02      .
           if        w-det-imp-spe-c02    >    9
                     go to det-imp-spe-bfo-250.
           if        w-det-imp-spe-ibx
                    (w-det-imp-spe-c02)   not  = "S"
                     go to det-imp-spe-bfo-225.
           add       w-tot-tot-rig
                    (w-det-imp-spe-c02)   to   w-det-imp-spe-ibl      .
           go to     det-imp-spe-bfo-225.
       det-imp-spe-bfo-250.
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  rf-bft-spe-per
                    (w-det-imp-spe-c01)   by   w-det-imp-spe-ibl
                                        giving rf-bft-spe-imp
                                              (w-det-imp-spe-c01)     .
           divide    100                  into rf-bft-spe-imp
                                              (w-det-imp-spe-c01)
                                                         rounded      .
       det-imp-spe-bfo-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      w-tot-spe-civ
                    (w-det-imp-spe-c01)   to   w-agg-cst-iva-coi      .
           move      rf-bft-spe-imp
                    (w-det-imp-spe-c01)   to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
      *              *-------------------------------------------------*
      *              * Riciclo su scansione tabella                    *
      *              *-------------------------------------------------*
           go to     det-imp-spe-bfo-100.
       det-imp-spe-bfo-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale al netto delle spese in ordine      *
      *    *-----------------------------------------------------------*
       det-tot-nsf-orf-000.
      *              *-------------------------------------------------*
      *              * Azzeramento work per accumulo spese             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-nsf-was      .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione tabella spese                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-nsf-ctr      .
       det-tot-nsf-orf-100.
           add       1                    to   w-det-tot-nsf-ctr      .
           if        w-det-tot-nsf-ctr    >    6
                     go to det-tot-nsf-orf-200.
      *                  *---------------------------------------------*
      *                  * Se spesa da non addebitare : riciclo        *
      *                  *---------------------------------------------*
           if        rf-oft-spe-snx
                    (w-det-tot-nsf-ctr)   =    0
                     go to det-tot-nsf-orf-100.
      *                  *---------------------------------------------*
      *                  * Aggiornamento totalizzatore                 *
      *                  *---------------------------------------------*
           add       rf-oft-spe-imp
                    (w-det-tot-nsf-ctr)   to   w-det-tot-nsf-was      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-tot-nsf-orf-100.
       det-tot-nsf-orf-200.
      *              *-------------------------------------------------*
      *              * Determinazione valore                           *
      *              *-------------------------------------------------*
           move      w-tot-tot-nsc        to    w-tot-tot-nsf         .
           add       w-det-tot-nsf-was    to    w-tot-tot-nsf         .
       det-tot-nsf-orf-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale al netto delle spese in bolla       *
      *    *-----------------------------------------------------------*
       det-tot-nsf-bfo-000.
      *              *-------------------------------------------------*
      *              * Azzeramento work per accumulo spese             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-nsf-was      .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione tabella spese                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-nsf-ctr      .
       det-tot-nsf-bfo-100.
           add       1                    to   w-det-tot-nsf-ctr      .
           if        w-det-tot-nsf-ctr    >    6
                     go to det-tot-nsf-bfo-200.
      *                  *---------------------------------------------*
      *                  * Se spesa da non addebitare : riciclo        *
      *                  *---------------------------------------------*
           if        rf-oft-spe-snx
                    (w-det-tot-nsf-ctr)   =    0
                     go to det-tot-nsf-bfo-100.
      *                  *---------------------------------------------*
      *                  * Aggiornamento totalizzatore                 *
      *                  *---------------------------------------------*
           add       rf-oft-spe-imp
                    (w-det-tot-nsf-ctr)   to   w-det-tot-nsf-was      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-tot-nsf-bfo-100.
       det-tot-nsf-bfo-200.
      *              *-------------------------------------------------*
      *              * Determinazione valore                           *
      *              *-------------------------------------------------*
           move      w-tot-tot-nsc        to    w-tot-tot-nsf         .
           add       w-det-tot-nsf-was    to    w-tot-tot-nsf         .
       det-tot-nsf-bfo-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale sconto pagamento                    *
      *    *-----------------------------------------------------------*
       det-tot-scp-000.
      *              *-------------------------------------------------*
      *              * Se sia la % che l'importo sono a zero : uscita  *
      *              *-------------------------------------------------*
           if        w-det-tot-scp-tot    =    zero and
                     w-det-tot-scp-per    =    zero
                     go to det-tot-scp-999.
      *              *-------------------------------------------------*
      *              * Se % a zero vuol dire che l'importo dello scon- *
      *              * to pagamento e' stato impostato manualmente     *
      *              *-------------------------------------------------*
           if        w-det-tot-scp-per    =    zero
                     go to det-tot-scp-100.
      *              *-------------------------------------------------*
      *              * Calcolo                                         *
      *              *-------------------------------------------------*
           multiply  w-det-tot-scp-per    by   w-tot-tot-nsf
                                        giving w-det-tot-scp-tot      .
           divide    100                  into w-det-tot-scp-tot
                                       rounded                        .
       det-tot-scp-100.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "-"                  to   w-agg-cst-iva-tip      .
           move      w-tot-civ-scp        to   w-agg-cst-iva-coi      .
           move      w-det-tot-scp-tot    to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
       det-tot-scp-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale netto                               *
      *    *-----------------------------------------------------------*
       det-tot-net-000.
           subtract  w-det-tot-scc-tot    from w-tot-tot-lor
                                        giving w-tot-tot-net          .
           subtract  w-det-tot-scp-tot    from w-tot-tot-net          .
       det-tot-net-999.
           exit.

      *    *===========================================================*
      *    * Determinazione spese incasso calcolate                    *
      *    *-----------------------------------------------------------*
       det-tot-sic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione totale spese incasso calcolate  *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-sic          .
      *              *-------------------------------------------------*
      *              * Ciclo su tabella scadenze                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-sic-c01      .
       det-tot-sic-100.
           add       1                    to   w-det-tot-sic-c01      .
           if        w-det-tot-sic-c01    >    w-tot-scd-ele
                     go to det-tot-sic-500.
      *                  *---------------------------------------------*
      *                  * Test se esistono spese incasso per il tipo  *
      *                  * scadenza in esame                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Scansione tabella spese incasso         *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-tot-sic-c02      .
       det-tot-sic-200.
           add       1                    to   w-det-tot-sic-c02      .
           if        w-det-tot-sic-c02    >    w-tot-eit-spi
                     go to det-tot-sic-100.
      *                      *-----------------------------------------*
      *                      * Se tipo pagamento non corrisponde a     *
      *                      * quello della scadenza in esame : rici-  *
      *                      * clo su prossima scadenza                *
      *                      *-----------------------------------------*
           if        w-tot-tpg-spi
                    (w-det-tot-sic-c02)   not  = w-tot-scd-tip
                                                (w-det-tot-sic-c01)
                     go to det-tot-sic-200.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo funzio- *
      *                      * namento spesa incasso                   *
      *                      *-----------------------------------------*
           if        w-tot-tfu-spi
                    (w-det-tot-sic-c02)   =    01
                     go to det-tot-sic-220
           else if   w-tot-tfu-spi
                    (w-det-tot-sic-c02)   =    02
                     go to det-tot-sic-240.
       det-tot-sic-220.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : automatico         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Aggiornamento accumulatore importo  *
      *                          * spese incasso                       *
      *                          *-------------------------------------*
           add       w-tot-amm-spi
                    (w-det-tot-sic-c02)   to   w-tot-tot-sic          .
      *                          *-------------------------------------*
      *                          * A riciclo su tabella scdenze        *
      *                          *-------------------------------------*
           go to     det-tot-sic-100.
       det-tot-sic-240.
      *                      *-----------------------------------------*
      *                      * Tipo funzionamento : manuale            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A riciclo su tabella scdenze        *
      *                          *-------------------------------------*
           go to     det-tot-sic-100.
       det-tot-sic-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      w-tot-civ-spi        to   w-agg-cst-iva-coi      .
           move      w-tot-tot-sic        to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
       det-tot-sic-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale spese bollo                         *
      *    *-----------------------------------------------------------*
       det-tot-spb-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione spese bollo                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-spb          .
      *              *-------------------------------------------------*
      *              * Ciclo su tabella scadenze                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-spb-c01      .
       det-tot-spb-100.
           add       1                    to   w-det-tot-spb-c01      .
           if        w-det-tot-spb-c01    >    w-tot-scd-ele
                     go to det-tot-spb-500.
      *                  *---------------------------------------------*
      *                  * Test se esistono spese bollo per il tipo    *
      *                  * scadenza in esame                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Scansione tabella spese bollo           *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-tot-spb-c02      .
       det-tot-spb-200.
           add       1                    to   w-det-tot-spb-c02      .
           if        w-det-tot-spb-c02    >    w-tot-eit-spb
                     go to det-tot-spb-100.
      *                      *-----------------------------------------*
      *                      * Se tipo pagamento non corrisponde a     *
      *                      * quello della scadenza in esame : riciclo*
      *                      *-----------------------------------------*
           if        w-tot-tpg-spb
                    (w-det-tot-spb-c02)   not  = w-tot-scd-tip
                                                (w-det-tot-spb-c01)
                     go to det-tot-spb-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione work per importo spesa      *
      *                  *---------------------------------------------*
           move      zero                 to   w-det-tot-spb-wis      .
      *                  *---------------------------------------------*
      *                  * Se importo scadenza non e' maggiore del     *
      *                  * tetto di esenzione : riciclo su prossima    *
      *                  * scadenza                                    *
      *                  *---------------------------------------------*
           if        w-tot-scd-imp
                    (w-det-tot-spb-c01)   not  > w-tot-tet-spb
                                                (w-det-tot-spb-c02)
                     go to det-tot-spb-100.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo automatismo *
      *                  * spesa bollo                                 *
      *                  *---------------------------------------------*
           if        w-tot-tau-spb
                    (w-det-tot-spb-c02)   =    01
                     go to det-tot-spb-220
           else if   w-tot-tau-spb
                    (w-det-tot-spb-c02)   =    02
                     go to det-tot-spb-240
           else if   w-tot-tau-spb
                    (w-det-tot-spb-c02)   =    03
                     go to det-tot-spb-260.
       det-tot-spb-220.
      *                  *---------------------------------------------*
      *                  * Tipo automatismo : in percentuale sul tota- *
      *                  * le scadenza                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione importo spesa            *
      *                      *-----------------------------------------*
           multiply  w-tot-per-spb
                    (w-det-tot-spb-c02)   by   w-tot-scd-imp
                                              (w-det-tot-spb-c01)
                                        giving w-det-tot-spb-s13      .
           divide    100                  into w-det-tot-spb-s13
                                        giving w-det-tot-spb-wis
                                                         rounded      .
      *                      *-----------------------------------------*
      *                      * A trattamento arrotondamento            *
      *                      *-----------------------------------------*
           go to     det-tot-spb-300.
       det-tot-spb-240.
      *                  *---------------------------------------------*
      *                  * Tipo automatismo : a scaglioni              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ciclo per 10 scaglioni                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-tot-spb-c03      .
       det-tot-spb-242.
           add       1                    to   w-det-tot-spb-c03      .
           if        w-det-tot-spb-c03    >    10
                     go to det-tot-spb-244.
           if        w-tot-tbe-scg
                    (w-det-tot-spb-c02,
                     w-det-tot-spb-c03)   =    zero
                     go to det-tot-spb-242.
           if        w-tot-scd-imp
                    (w-det-tot-spb-c01)   >    w-tot-tbe-scg
                                              (w-det-tot-spb-c02,
                                               w-det-tot-spb-c03)
                     go to det-tot-spb-242.
           move      w-tot-tbe-asc
                    (w-det-tot-spb-c02,
                     w-det-tot-spb-c03)   to   w-det-tot-spb-wis      .
       det-tot-spb-244.
      *                      *-----------------------------------------*
      *                      * A trattamento arrotondamento            *
      *                      *-----------------------------------------*
           go to     det-tot-spb-300.
       det-tot-spb-260.
      *                  *---------------------------------------------*
      *                  * Tipo automatismo : a scaglioni in percentua-*
      *                  *                    le                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Inizializzazione work per progressivo   *
      *                      * imponibile                              *
      *                      *-----------------------------------------*
           move      w-tot-scd-imp
                    (w-det-tot-spb-c01)   to   w-det-tot-spb-wpi      .
      *                      *-----------------------------------------*
      *                      * Ciclo per 10 scaglioni                  *
      *                      *-----------------------------------------*
           move      zero                 to   w-det-tot-spb-c03      .
       det-tot-spb-262.
           add       1                    to   w-det-tot-spb-c03      .
           if        w-det-tot-spb-c03    >    10
                     go to det-tot-spb-270.
           if        w-tot-tbe-scg
                    (w-det-tot-spb-c02,
                     w-det-tot-spb-c03)   =    zero
                     go to det-tot-spb-262.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che l'importo sca- *
      *                      * glione sia maggiore o meno dell'importo *
      *                      * scadenza                                *
      *                      *-----------------------------------------*
           if        w-tot-tbe-scg
                    (w-det-tot-spb-c02,
                     w-det-tot-spb-c03)   >    w-tot-scd-imp
                                              (w-det-tot-spb-c01)
                     go to det-tot-spb-270
           else      go to det-tot-spb-264.
       det-tot-spb-264.
      *                      *-----------------------------------------*
      *                      * Se l'importo scaglione e' minore o u-   *
      *                      * guale dell'importo scadenza             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Calcolo quota spesa bollo relativa  *
      *                          * allo scaglione                      *
      *                          *-------------------------------------*
           multiply  w-tot-tbe-psc
                    (w-det-tot-spb-c02,
                     w-det-tot-spb-c03)
                                          by   w-tot-tbe-scg
                                              (w-det-tot-spb-c02,
                                               w-det-tot-spb-c03)
                                        giving w-det-tot-spb-s13      .
           divide    100                  into w-det-tot-spb-s13
                                        giving w-det-tot-spb-wss
                                                         rounded      .
      *                          *-------------------------------------*
      *                          * Aggiornamento importo spesa         *
      *                          *-------------------------------------*
           add       w-det-tot-spb-wss    to   w-det-tot-spb-wis      .
      *                          *-------------------------------------*
      *                          * Aggiornamento progressivo imponibile*
      *                          *-------------------------------------*
           subtract  w-tot-tbe-scg
                    (w-det-tot-spb-c02,
                     w-det-tot-spb-c03)   from w-det-tot-spb-wpi      .
      *                          *-------------------------------------*
      *                          * Riciclo su prossimo scaglione       *
      *                          *-------------------------------------*
           go to     det-tot-spb-262.
       det-tot-spb-270.
      *                      *-----------------------------------------*
      *                      * Se l'importo scaglione e' superiore al- *
      *                      * l'importo scadenza                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Calcolo quota spesa bollo relativa  *
      *                          * al residuo, cioe' il progressivo im-*
      *                          * ponibile sin qui determinato        *
      *                          *-------------------------------------*
           multiply  w-tot-tbe-psc
                    (w-det-tot-spb-c02,
                     w-det-tot-spb-c03)
                                          by   w-det-tot-spb-wpi
                                        giving w-det-tot-spb-s13      .
           divide    100                  into w-det-tot-spb-s13
                                        giving w-det-tot-spb-wss
                                                         rounded      .
      *                          *-------------------------------------*
      *                          * Aggiornamento importo spesa         *
      *                          *-------------------------------------*
           add       w-det-tot-spb-wss    to   w-det-tot-spb-wis      .
      *                          *-------------------------------------*
      *                          * A trattamento arrotondamento        *
      *                          *-------------------------------------*
           go to     det-tot-spb-300.
       det-tot-spb-300.
      *                  *---------------------------------------------*
      *                  * Trattamento arrotondamento                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se esiste un valore minimo per la spesa *
      *                      *-----------------------------------------*
           if        w-tot-min-spb
                    (w-det-tot-spb-c02)   =    zero
                     go to det-tot-spb-302.
      *                      *-----------------------------------------*
      *                      * Se l'importo spesa sin qui determinato  *
      *                      * e' inferiore al minimo valore, si forza *
      *                      * il valore minimo                        *
      *                      *-----------------------------------------*
           if        w-det-tot-spb-wis    <    w-tot-min-spb
                                              (w-det-tot-spb-c02)
                     move  w-tot-min-spb
                          (w-det-tot-spb-c02)
                                          to   w-det-tot-spb-wis      .
       det-tot-spb-302.
      *                      *-----------------------------------------*
      *                      * Se esiste un valore massimo per la spesa*
      *                      *-----------------------------------------*
           if        w-tot-max-spb
                    (w-det-tot-spb-c02)   =    zero
                     go to det-tot-spb-304.
      *                      *-----------------------------------------*
      *                      * Se l'importo spesa sin qui determinato  *
      *                      * e' superiore al massimo valore, si forza*
      *                      * il valore massimo                       *
      *                      *-----------------------------------------*
           if        w-det-tot-spb-wis    >    w-tot-max-spb
                                              (w-det-tot-spb-c02)
                     move  w-tot-max-spb
                          (w-det-tot-spb-c02)
                                          to   w-det-tot-spb-wis      .
       det-tot-spb-304.
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo arroton-*
      *                      * damento                                 *
      *                      *-----------------------------------------*
           go to     det-tot-spb-310
                     det-tot-spb-320
                     det-tot-spb-330
                     det-tot-spb-340
                                depending on   w-tot-tar-spb
                                              (w-det-tot-spb-c02)     .
      *                      *-----------------------------------------*
      *                      * Tipo arrotondamento : nessuno           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A riciclo su tabella scadenze       *
      *                          *-------------------------------------*
           go to     det-tot-spb-400.
       det-tot-spb-310.
      *                      *-----------------------------------------*
      *                      * Tipo arrotondamento : al valore superio-*
      *                      *                       re                *
      *                      *-----------------------------------------*
           if        w-det-tot-spb-wis    >    zero
                     add      w-tot-var-spb
                             (w-det-tot-spb-c02)
                                          to   w-det-tot-spb-wis
                     subtract 1           from w-det-tot-spb-wis
           else      subtract w-tot-var-spb
                             (w-det-tot-spb-c02)
                                          from w-det-tot-spb-wis
                     add      1           to   w-det-tot-spb-wis      .
           divide    w-tot-var-spb
                    (w-det-tot-spb-c02)   into w-det-tot-spb-wis      .
           multiply  w-tot-var-spb
                    (w-det-tot-spb-c02)   by   w-det-tot-spb-wis      .
      *                          *-------------------------------------*
      *                          * A riciclo su tabella scdenze        *
      *                          *-------------------------------------*
           go to     det-tot-spb-400.
       det-tot-spb-320.
      *                      *-----------------------------------------*
      *                      * Tipo arrotondamento : al valore inferio-*
      *                      *                       re                *
      *                      *-----------------------------------------*
           divide    w-tot-var-spb
                    (w-det-tot-spb-c02)   into w-det-tot-spb-wis      .
           multiply  w-tot-var-spb
                    (w-det-tot-spb-c02)   by   w-det-tot-spb-wis      .
      *                          *-------------------------------------*
      *                          * A riciclo su tabella scdenze        *
      *                          *-------------------------------------*
           go to     det-tot-spb-400.
       det-tot-spb-330.
      *                      *-----------------------------------------*
      *                      * Tipo arrotondamento : commerciale       *
      *                      *-----------------------------------------*
           divide    w-tot-var-spb
                    (w-det-tot-spb-c02)   into w-det-tot-spb-wis
                                                         rounded      .
           multiply  w-tot-var-spb
                    (w-det-tot-spb-c02)   by   w-det-tot-spb-wis      .
      *                          *-------------------------------------*
      *                          * A riciclo su tabella scdenze        *
      *                          *-------------------------------------*
           go to     det-tot-spb-400.
       det-tot-spb-340.
      *                      *-----------------------------------------*
      *                      * Tipo arrotondamento : statale           *
      *                      *-----------------------------------------*
           divide    2                    into w-tot-var-spb
                                              (w-det-tot-spb-c02)
                                        giving w-det-tot-spb-wpc
                                                          rounded     .
           if        w-det-tot-spb-wis    >    zero
                     add      w-det-tot-spb-wpc
                                          to   w-det-tot-spb-wis
                     subtract 1           from w-det-tot-spb-wis
           else      subtract w-det-tot-spb-wpc
                                          from w-det-tot-spb-wis
                     add      1           to   w-det-tot-spb-wis      .
           divide    w-tot-var-spb
                    (w-det-tot-spb-c02)   into w-det-tot-spb-wis      .
           multiply  w-tot-var-spb
                    (w-det-tot-spb-c02)   by   w-det-tot-spb-wis      .
      *                          *-------------------------------------*
      *                          * A riciclo su tabella scadenze       *
      *                          *-------------------------------------*
           go to     det-tot-spb-400.
       det-tot-spb-400.
      *                  *---------------------------------------------*
      *                  * Aggiornamento totale spese bollo            *
      *                  *---------------------------------------------*
           add       w-det-tot-spb-wis    to   w-tot-tot-spb          .
      *                  *---------------------------------------------*
      *                  * Riciclo su prossima scadenza                *
      *                  *---------------------------------------------*
           go to     det-tot-spb-100.
       det-tot-spb-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      w-tot-civ-spb        to   w-agg-cst-iva-coi      .
           move      w-tot-tot-spb        to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
       det-tot-spb-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale spese                               *
      *    *-----------------------------------------------------------*
       det-tot-spe-000.
      *              *-------------------------------------------------*
      *              * Azzeramento totale spese                        *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-spe          .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione tabella spese                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-spe-ctr      .
       det-tot-spe-100.
           add       1                    to   w-det-tot-spe-ctr      .
           if        w-det-tot-spe-ctr    >    6
                     go to det-tot-spe-200.
      *                  *---------------------------------------------*
      *                  * Se spesa da non addebitare : riciclo        *
      *                  *---------------------------------------------*
           if        rf-oft-spe-snx
                    (w-det-tot-spe-ctr)   =    0
                     go to det-tot-spe-100.
      *                  *---------------------------------------------*
      *                  * Aggiornamento totalizzatore                 *
      *                  *---------------------------------------------*
           add       rf-oft-spe-imp
                    (w-det-tot-spe-ctr)   to   w-tot-tot-spe          .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-tot-spe-100.
       det-tot-spe-200.
      *              *-------------------------------------------------*
      *              * Spese incasso calcolate                         *
      *              *-------------------------------------------------*
           add       w-tot-tot-sic        to   w-tot-tot-spe          .
      *              *-------------------------------------------------*
      *              * Spese bollo                                     *
      *              *-------------------------------------------------*
           add       w-tot-tot-spb        to   w-tot-tot-spe          .
       det-tot-spe-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale imponibile                          *
      *    *-----------------------------------------------------------*
       det-tot-ibl-000.
      *              *-------------------------------------------------*
      *              * Azzeramento totalizzatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-ibl          .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione castelletto iva              *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-ibl-ctr      .
       det-tot-ibl-100.
           add       1                    to   w-det-tot-ibl-ctr      .
           if        w-det-tot-ibl-ctr    >    12
                     go to det-tot-ibl-999.
           if        w-tot-iva-cod
                    (w-det-tot-ibl-ctr)   >    900
                     go to det-tot-ibl-100.
           add       w-tot-iva-ibl
                    (w-det-tot-ibl-ctr)   to   w-tot-tot-ibl          .
           go to     det-tot-ibl-100.
       det-tot-ibl-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale imposta                             *
      *    *-----------------------------------------------------------*
       det-tot-imp-000.
      *              *-------------------------------------------------*
      *              * Azzeramento totalizzatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-imp          .
           move      zero                 to   w-det-tot-imp-wpa      .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione castelletto iva              *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-imp-ctr      .
       det-tot-imp-100.
           add       1                    to   w-det-tot-imp-ctr      .
           if        w-det-tot-imp-ctr    >    6
                     go to det-tot-imp-999.
      *                  *---------------------------------------------*
      *                  * Test se imposta da calcolare                *
      *                  *---------------------------------------------*
           move      w-tot-iva-cod
                    (w-det-tot-imp-ctr)   to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    <    1   or
                     w-edt-iva-cod-003    >    8
                     go to det-tot-imp-120
           else      go to det-tot-imp-100.
       det-tot-imp-120.
      *                  *---------------------------------------------*
      *                  * Determinazione aliquota per il calcolo      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che si tratti di   *
      *                      * un aliquota di sola iva (omaggio) o no  *
      *                      *-----------------------------------------*
           if        w-edt-iva-cod-003    =    9
                     go to det-tot-imp-122
           else      go to det-tot-imp-124.
       det-tot-imp-122.
      *                      *-----------------------------------------*
      *                      * Se aliquota di sola iva (omaggio)       *
      *                      *-----------------------------------------*
           move      w-tot-iva-cod
                    (w-det-tot-imp-ctr)   to   w-det-tot-imp-wci      .
           subtract  900                  from w-det-tot-imp-wci      .
           go to     det-tot-imp-140.
       det-tot-imp-124.
      *                      *-----------------------------------------*
      *                      * Se aliquota normale                     *
      *                      *-----------------------------------------*
           move      w-tot-iva-cod
                    (w-det-tot-imp-ctr)   to   w-det-tot-imp-wci      .
           go to     det-tot-imp-140.
       det-tot-imp-140.
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      "DI"                 to   d-imp-iva-tip-ope      .
           move      w-tot-iva-ibl
                    (w-det-tot-imp-ctr)   to   d-imp-iva-ibl-iva      .
           move      w-det-tot-imp-wci    to   d-imp-iva-cod-iva      .
           perform   det-imp-iva-cll-000  thru det-imp-iva-cll-999    .
      *                  *---------------------------------------------*
      *                  * Aggiornamento imposta relativa all'aliquota *
      *                  *---------------------------------------------*
           move      d-imp-iva-ims-iva    to   w-tot-iva-imp
                                              (w-det-tot-imp-ctr)     .
      *                  *---------------------------------------------*
      *                  * Aggiornamento accumulatore                  *
      *                  *---------------------------------------------*
           add       d-imp-iva-ims-iva    to   w-tot-tot-imp          .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-tot-imp-100.
       det-tot-imp-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale documento                           *
      *    *-----------------------------------------------------------*
       det-tot-doc-000.
           add       w-tot-tot-ibl
                     w-tot-tot-imp      giving w-tot-tot-doc          .
       det-tot-doc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione scadenze per il documento                  *
      *    *-----------------------------------------------------------*
       det-sca-doc-000.
           move      zero                 to   w-det-sca-doc-ctr      .
       det-sca-doc-100.
           add       1                    to   w-det-sca-doc-ctr      .
           if        w-det-sca-doc-ctr    >    3
                     go to det-sca-doc-120.
           move      w-tot-cod-pag
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-cdp
                                              (w-det-sca-doc-ctr)     .
           move      w-tot-tip-amm
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-tam
                                              (w-det-sca-doc-ctr)     .
           move      w-tot-per-toi
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-pti
                                              (w-det-sca-doc-ctr)     .
           move      w-tot-dim-act
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-dda
                                              (w-det-sca-doc-ctr)     .
           move      w-tot-tip-pag
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-tpg
                                              (w-det-sca-doc-ctr)     .
           move      w-tot-num-sca
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-nsp
                                              (w-det-sca-doc-ctr)     .
           move      w-tot-dec-prs
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-dps
                                              (w-det-sca-doc-ctr)     .
           move      w-tot-dap-mes
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-dpm
                                              (w-det-sca-doc-ctr)     .
           move      w-tot-dap-gio
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-dpg
                                              (w-det-sca-doc-ctr)     .
           move      w-tot-ggg-int
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-gdi
                                              (w-det-sca-doc-ctr)     .
           move      w-tot-tip-scm
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-tsm
                                              (w-det-sca-doc-ctr)     .
           move      w-tot-gio-scm
                    (w-det-sca-doc-ctr)   to   w-clc-tbl-scd-gfs
                                              (w-det-sca-doc-ctr)     .
           move      zero                 to   w-clc-tbl-scd-ccp
                                              (w-det-sca-doc-ctr)     .
           go to     det-sca-doc-100.
       det-sca-doc-120.
      *                  *---------------------------------------------*
      *                  * Tipo esclusione mesi : forzato a 01, nessun *
      *                  * mese escluso                                *
      *                  *---------------------------------------------*
           move      01                   to   w-clc-tbl-scd-esm      .
      *                  *---------------------------------------------*
      *                  * Valori di esclusione : normalizzati a zero  *
      *                  *---------------------------------------------*
           move      zero                 to   w-clc-tbl-scd-alt      .
           move      zero                 to   w-clc-tbl-scd-e01      .
           move      zero                 to   w-clc-tbl-scd-e02      .
       det-sca-doc-200.
      *              *-------------------------------------------------*
      *              * Richiamo routine di calcolo                     *
      *              *-------------------------------------------------*
           perform   clc-tbl-scd-000      thru clc-tbl-scd-999        .
       det-sca-doc-300.
      *              *-------------------------------------------------*
      *              * Movimento da work locale a work totali fattura  *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-sca-doc-ctr      .
       det-sca-doc-310.
           add       1                    to   w-det-sca-doc-ctr      .
           if        w-det-sca-doc-ctr    >    w-clc-tbl-scd-nts
                     go to det-sca-doc-320.
      *                  *---------------------------------------------*
      *                  * Dati calcolati                              *
      *                  *---------------------------------------------*
           move      w-clc-tbl-scd-tds
                    (w-det-sca-doc-ctr)   to   w-tot-scd-tip
                                              (w-det-sca-doc-ctr)     .
           move      w-clc-tbl-scd-dds
                    (w-det-sca-doc-ctr)   to   w-tot-scd-dat
                                              (w-det-sca-doc-ctr)     .
           move      w-clc-tbl-scd-ccs
                    (w-det-sca-doc-ctr)   to   w-tot-scd-cau
                                              (w-det-sca-doc-ctr)     .
           move      w-clc-tbl-scd-ids
                    (w-det-sca-doc-ctr)   to   w-tot-scd-imp
                                              (w-det-sca-doc-ctr)     .
      *                  *---------------------------------------------*
      *                  * Eventuale aggiustamento date scadenza a     *
      *                  * vista                                       *
      *                  *---------------------------------------------*
           if        w-tot-scd-dat
                    (w-det-sca-doc-ctr)   =    zero
                     move  w-clc-tbl-scd-ddo
                                          to   w-tot-scd-dat
                                              (w-det-sca-doc-ctr)     .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-sca-doc-310.
       det-sca-doc-320.
      *              *-------------------------------------------------*
      *              * Bufferizzazione numero totale scadenze          *
      *              *-------------------------------------------------*
           move      w-clc-tbl-scd-nts    to   w-tot-scd-ele          .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-sca-doc-999.
       det-sca-doc-999.
           exit.

      *    *===========================================================*
      *    * Calcolo tabella scadenze                                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dtblscd0.dts"                   .

      *    *===========================================================*
      *    * Determinazione importo in riga                            *
      *    *-----------------------------------------------------------*
       det-imp-rig-000.
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           multiply  w-det-imp-rig-qta    by   w-det-imp-rig-prz
                                        giving w-det-imp-rig-imp      .
           if        w-det-imp-rig-dec    =    1
                     divide 10            into w-det-imp-rig-imp
           else if   w-det-imp-rig-dec    =    2
                     divide 100           into w-det-imp-rig-imp      .
       det-imp-rig-999.
           exit.

      *    *===========================================================*
      *    * Routine di compattamento castelletto iva                  *
      *    *-----------------------------------------------------------*
       cmp-cst-iva-000.
      *              *-------------------------------------------------*
      *              * Azzeramento contatori                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-cmp-cst-iva-ctr      .
           move      zero                 to   w-cmp-cst-iva-num      .
       cmp-cst-iva-100.
           add       1                    to   w-cmp-cst-iva-ctr      .
           if        w-cmp-cst-iva-ctr    >    w-tot-iva-ele
                     go to cmp-cst-iva-900.
           if        w-tot-iva-ibl
                    (w-cmp-cst-iva-ctr)   =    zero
                     go to cmp-cst-iva-100.
           add       1                    to   w-cmp-cst-iva-num      .
           move      w-tot-iva-cod
                    (w-cmp-cst-iva-ctr)   to   w-tot-iva-cod
                                              (w-cmp-cst-iva-num)     .
           move      w-tot-iva-ibl
                    (w-cmp-cst-iva-ctr)   to   w-tot-iva-ibl
                                              (w-cmp-cst-iva-num)     .
           move      w-tot-iva-imp
                    (w-cmp-cst-iva-ctr)   to   w-tot-iva-imp
                                              (w-cmp-cst-iva-num)     .
           go to     cmp-cst-iva-100.
       cmp-cst-iva-900.
           move      w-cmp-cst-iva-num    to   w-tot-iva-ele          .
       cmp-cst-iva-999.
           exit.

      *    *===========================================================*
      *    * Calcolo prezzo netto                                      *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wcalprz0.cps"                   .

      *    *===========================================================*
      *    * Determinazione prezzo sottoposto a legame valutario       *
      *    *                                                           *
      *    * Input  : w-lvl-prz-prz = Prezzo da sottoporre a legame    *
      *    *                                                           *
      *    *          w-lvl-prz-vlt = Valuta per il legame             *
      *    *                                                           *
      *    *          w-lvl-prz-tdc = Tipo di cambio valuta            *
      *    *                                                           *
      *    *          w-lvl-prz-cdc = Coefficiente di cambio attuale   *
      *    *                                                           *
      *    *          w-lvl-prz-ccr = Coefficiente di cambio di rife-  *
      *    *                          rimento                          *
      *    *                                                           *
      *    *          w-lvl-prz-plm = Percentuale di limitazione       *
      *    *                                                           *
      *    *          w-lvl-prz-tlm = Tipo di limitazione              *
      *    *                                                           *
      *    *                                                           *
      *    * Output : w-lvl-prz-prz = Prezzo determinato               *
      *    *                                                           *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wlvlprz0.cps"                   .

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
           copy      "swd/std/prg/cpy/wcvsvlt0.cps"                   .

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
      *    * Routines per l'aumento di una data in giorni              *
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
      *    * Controllo tipo riga                                       *
      *    *-----------------------------------------------------------*
       ctl-tip-rig-000.
      *              *-------------------------------------------------*
      *              * Controllo se ci sono asterischi                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-ctl-tip-rig-c01      .
           inspect   w-ctl-tip-rig-tri
                                      tallying w-ctl-tip-rig-c01
                                          for  all "*"                .
      *               *------------------------------------------------*
      *               * Deviazione in funzione del numero di asterischi*
      *               * trovati                                        *
      *               *------------------------------------------------*
           if        w-ctl-tip-rig-c01    =    zero
                     go to ctl-tip-rig-200
           else if   w-ctl-tip-rig-c01    =    1
                     go to ctl-tip-rig-100.
       ctl-tip-rig-100.
      *              *-------------------------------------------------*
      *              * Se c'e' un asterisco, o piu' di uno             *
      *              *-------------------------------------------------*
           inspect   w-ctl-tip-rig-tri
                                     replacing all "*" by   spaces    .
      *              *-------------------------------------------------*
      *              * Segnale di presenza di asterisco                *
      *              *-------------------------------------------------*
           move      "#"                  to   w-ctl-tip-rig-ast      .
       ctl-tip-rig-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione tipo prodotto connesso al tipo  *
      *              * riga                                            *
      *              *-------------------------------------------------*
           move      w-ctl-tip-rig-chr (1)
                                          to   w-ctl-tip-rig-tpr      .
      *              *-------------------------------------------------*
      *              * Se tipo prodotto P                              *
      *              *-------------------------------------------------*
           if        w-ctl-tip-rig-chr (1)
                                          not  = "P"
                     go to ctl-tip-rig-999.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione tipo funzionamento riga     *
      *                  * connesso al tipo riga                       *
      *                  *---------------------------------------------*
           move      w-ctl-tip-rig-chr (2)
                                          to   w-ctl-tip-rig-tfu      .
       ctl-tip-rig-999.
           exit.

      *    *===========================================================*
      *    * Determinazione status scadenza fornitore                  *
      *    *-----------------------------------------------------------*
       det-srd-scf-000.
      *              *-------------------------------------------------*
      *              * Determinazione sigla ultima operazione eseguita *
      *              * sulla scadenza                                  *
      *              *-------------------------------------------------*
       det-srd-scf-025.
      *                  *---------------------------------------------*
      *                  * Normalizzazione iniziale sigla              *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-srd-scf-suo      .
       det-srd-scf-050.
      *                  *---------------------------------------------*
      *                  * Se la data di registrazione scadenza e' su- *
      *                  * periore alla data di riferimento per la de- *
      *                  * terminazione : uscita con status 'N'        *
      *                  *---------------------------------------------*
           if        rf-sfs-dtr-rgs       >    w-det-srd-scf-drd
                     move  "N"            to   w-det-srd-scf-sts
                     go to det-srd-scf-999.
       det-srd-scf-100.
      *                  *---------------------------------------------*
      *                  * Registrazione                               *
      *                  *---------------------------------------------*
           move      "REG"                to   w-det-srd-scf-suo      .
       det-srd-scf-125.
      *                  *---------------------------------------------*
      *                  * Storno                                      *
      *                  *---------------------------------------------*
           if        rf-sfs-dtr-sto       not  > w-det-srd-scf-drd and
                     rf-sfs-dtr-sto       not  = zero
                     move  "SSC"          to   w-det-srd-scf-suo
                     go to det-srd-scf-800.
       det-srd-scf-400.
      *                  *---------------------------------------------*
      *                  * Operazioni relative all'operazione di paga- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
       det-srd-scf-425.
      *                      *-----------------------------------------*
      *                      * Se la scadenza non porta un numero pa-  *
      *                      * gamento si escludono tutti i controlli  *
      *                      * sulle operazioni relative al pagamento  *
      *                      *-----------------------------------------*
           if        rf-sfs-num-pgf       =    zero
                     go to det-srd-scf-625.
       det-srd-scf-450.
      *                      *-----------------------------------------*
      *                      * Lettura record [sfp]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMSFP    "         to   f-key                  .
           move      rf-sfs-num-pgf       to   rf-sfp-num-pgf         .
           move      "pgm/scf/fls/ioc/obj/iofsfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfp                 .
       det-srd-scf-475.
      *                      *-----------------------------------------*
      *                      * Se operazione di pagamento non esisten- *
      *                      * te in archivio distinte si escludono    *
      *                      * tutti i controlli sulle operazioni re-  *
      *                      * lative al pagamento                     *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-srd-scf-625.
       det-srd-scf-500.
      *                      *-----------------------------------------*
      *                      * Se la data di registrazione pagamento   *
      *                      * e' superiore alla data di riferimento   *
      *                      * per la determinazione si escludono tut- *
      *                      * ti i controlli sulle operazioni relati- *
      *                      * ve al pagamento                         *
      *                      *-----------------------------------------*
           if        rf-sfp-dtr-pgf       >    w-det-srd-scf-drd
                     go to det-srd-scf-625.
       det-srd-scf-525.
      *                      *-----------------------------------------*
      *                      * Pagamento                               *
      *                      *-----------------------------------------*
           move      "PAG"                to   w-det-srd-scf-suo      .
       det-srd-scf-625.
      *                      *-----------------------------------------*
      *                      * Fine controlli su operazioni relative   *
      *                      * al pagamento                            *
      *                      *-----------------------------------------*
           go to     det-srd-scf-800.
       det-srd-scf-800.
      *              *-------------------------------------------------*
      *              * Determinazione dello status della scadenza in   *
      *              * funzione della sigla dell'ultima operazione e-  *
      *              * seguita sulla scadenza                          *
      *              *-------------------------------------------------*
           if        w-det-srd-scf-suo    =    "REG"
                     move  "A"            to   w-det-srd-scf-sts
           else if   w-det-srd-scf-suo    =    "SSC" or
                     w-det-srd-scf-suo    =    "PAG"
                     move  "C"            to   w-det-srd-scf-sts
           else      move  spaces         to   w-det-srd-scf-sts      .
       det-srd-scf-999.
           exit.

      *    *===========================================================*
      *    * Determinazione status bolle da fatturare                  *
      *    *-----------------------------------------------------------*
       det-sts-bfo-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-sts-bfo-flg      .
           move      zero                 to   w-det-sts-bfo-ctr      .
       det-sts-bfo-100.
      *              *-------------------------------------------------*
      *              * Start su file [bfr]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-bft-num-prt       to   rf-bfr-num-prt         .
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
                     go to det-sts-bfo-900.
       det-sts-bfo-200.
      *              *-------------------------------------------------*
      *              * Test se batch                                   *
      *              *-------------------------------------------------*
           if        rr-snx-btc           =    "S"
                     go to det-sts-bfo-220.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       det-sts-bfo-220.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale record [bfr]                *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bfo/fls/ioc/obj/iofbfr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bfr                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At End'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-sts-bfo-800.
       det-sts-bfo-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-bfr-num-prt       not  = rf-bft-num-prt
                     go to det-sts-bfo-800.
       det-sts-bfo-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se riga chiusa                         *
      *                  *---------------------------------------------*
           if        rf-bfr-flg-rch       not  = spaces
                     go to det-sts-bfo-200.
       det-sts-bfo-410.
      *                  *---------------------------------------------*
      *                  * Test se riga bolla gia' fatturata           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Start su file [ffr]                     *
      *                      *-----------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "RIFBFO    "         to   f-key                  .
           move      rf-bfr-cod-dpz       to   rf-ffr-cod-dpz         .
           move      rf-bfr-num-prt       to   rf-ffr-bfo-prt         .
           move      rf-bfr-num-prg       to   rf-ffr-bfo-prg         .
           move      zero                 to   rf-ffr-num-prt         .
           move      zero                 to   rf-ffr-num-prg         .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *                          *-------------------------------------*
      *                          * Test su esito operazione            *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-sts-bfo-500.
       det-sts-bfo-420.
      *                      *-----------------------------------------*
      *                      * Lettura primo record [ffr]              *
      *                      *-----------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ffo/fls/ioc/obj/iofffr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ffr                 .
      *                          *-------------------------------------*
      *                          * Test se 'At End'                    *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-sts-bfo-500.
       det-sts-bfo-430.
      *                      *-----------------------------------------*
      *                      * Test sul massimo                        *
      *                      *-----------------------------------------*
           if        rf-ffr-cod-dpz       not  = rf-bfr-cod-dpz or
                     rf-ffr-bfo-prt       not  = rf-bfr-num-prt or
                     rf-ffr-bfo-prg       not  = rf-bfr-num-prg
                     go to det-sts-bfo-500.
      *                      *-----------------------------------------*
      *                      * Riga fatturata quindi riciclo           *
      *                      *-----------------------------------------*
           go to     det-sts-bfo-200.
       det-sts-bfo-500.
      *                  *---------------------------------------------*
      *                  * Incremento elementi utili                   *
      *                  *---------------------------------------------*
           add       1                    to   w-det-sts-bfo-ctr      .
       det-sts-bfo-600.
      *              *-------------------------------------------------*
      *              * Riciclo a riga bolla successiva                 *
      *              *-------------------------------------------------*
           go to     det-sts-bfo-200.
       det-sts-bfo-800.
      *              *-------------------------------------------------*
      *              * Test su numero elementi utili incontrati        *
      *              *-------------------------------------------------*
           if        w-det-sts-bfo-ctr    =    zero
                     go to det-sts-bfo-900.
      *              *-------------------------------------------------*
      *              * Uscita per determinazione superata              *
      *              *-------------------------------------------------*
           go to     det-sts-bfo-999.
       det-sts-bfo-900.
      *              *-------------------------------------------------*
      *              * Uscita per determinazione non superata          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-sts-bfo-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-sts-bfo-999.
       det-sts-bfo-999.
           exit.

      *    *===========================================================*
      *    * Pre-scansione righe ordine fornitore                      *
      *    *-----------------------------------------------------------*
       pre-scn-rig-orf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore elementi              *
      *              *-------------------------------------------------*
           move      zero                 to   w-rof-num-ele          .
       pre-scn-rig-orf-100.
      *              *-------------------------------------------------*
      *              * Start su righe documento [ofr]                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rof-num-prt        to   rf-ofr-num-prt         .
           move      zero                 to   rf-ofr-num-prg         .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Se errore di Start : ad uscita              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pre-scn-rig-orf-900.
       pre-scn-rig-orf-200.
      *              *-------------------------------------------------*
      *              * Test se batch                                   *
      *              *-------------------------------------------------*
           if        rr-snx-btc           =    "S"
                     go to pre-scn-rig-orf-220.
      *              *-------------------------------------------------*
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       pre-scn-rig-orf-220.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [ofr]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orf/fls/ioc/obj/iofofr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ofr                 .
      *                  *---------------------------------------------*
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pre-scn-rig-orf-900.
       pre-scn-rig-orf-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-ofr-num-prt       not  = w-rof-num-prt
                     go to pre-scn-rig-orf-900.
       pre-scn-rig-orf-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scomposizione tipo riga                     *
      *                  *---------------------------------------------*
           move      rf-ofr-tip-rig       to   w-ctl-tip-rig-tri      .
           perform   ctl-tip-rig-000      thru ctl-tip-rig-999        .
      *                  *---------------------------------------------*
      *                  * Se tipo riga 'C' : riciclo                  *
      *                  *---------------------------------------------*
           if        w-ctl-tip-rig-tpr    =    "C"
                     go to pre-scn-rig-orf-200.
      *                  *---------------------------------------------*
      *                  * Determinazione della quantita' residua      *
      *                  *---------------------------------------------*
           move      "DT"                 to   d-qev-rof-tip-ope      .
           perform   det-qev-rof-cll-000  thru det-qev-rof-cll-999    .
      *                  *---------------------------------------------*
      *                  * Considerazioni su quantita' residua         *
      *                  *---------------------------------------------*
           if        d-qev-rof-qta-dri    =    zero
                     go to pre-scn-rig-orf-200.
       pre-scn-rig-orf-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione riga                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-rof-num-ele          .
           if        w-rof-num-ele        >    w-rof-max-ele
                     move  w-rof-max-ele  to   w-rof-num-ele
                     go to pre-scn-rig-orf-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione data consegna prevista      *
      *                  *---------------------------------------------*
           if        rf-ofr-dcn-prv       =    zero
                     move  rf-ofr-dcn-ric to   rf-ofr-dcn-prv         .
           if        rf-ofr-dcn-prv       =    zero
                     move  rf-oft-dat-cns to   rf-ofr-dcn-prv         .
           if        rf-ofr-dcn-prv       =    zero
                     move  rf-oft-dat-doc to   rf-ofr-dcn-prv         .
           move      rf-ofr-dcn-prv       to   w-rof-key-prv
                                              (w-rof-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione progressivo riga            *
      *                  *---------------------------------------------*
           move      rf-ofr-num-prg       to   w-rof-key-prg
                                              (w-rof-num-ele)         .
       pre-scn-rig-orf-800.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale file [ofr]       *
      *              *-------------------------------------------------*
           go to     pre-scn-rig-orf-200.
       pre-scn-rig-orf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pre-scn-rig-orf-999.
       pre-scn-rig-orf-999.
           exit.

      *    *===========================================================*
      *    * Ordinamento righe ordine fornitore bufferizzate           *
      *    *-----------------------------------------------------------*
       ord-fin-rig-orf-000.
      *              *-------------------------------------------------*
      *              * Test se almeno due codici da ordinare           *
      *              *-------------------------------------------------*
           if        w-rof-num-ele        <    2
                     go to ord-fin-rig-orf-999.
       ord-fin-rig-orf-050.
      *              *-------------------------------------------------*
      *              * Ciclo di ordinamento                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-rof-ctr-001          .
       ord-fin-rig-orf-100.
           add       1                    to   w-rof-ctr-001          .
           if        w-rof-ctr-001        =    w-rof-num-ele
                     go to ord-fin-rig-orf-999.
           move      w-rof-ctr-001        to   w-rof-ctr-002
                                               w-rof-ctr-003          .
           move      w-rof-key-ord
                    (w-rof-ctr-001)       to   w-rof-sav-key          .
       ord-fin-rig-orf-200.
           add       1                    to   w-rof-ctr-002          .
           if        w-rof-ctr-002        >    w-rof-num-ele
                     go to ord-fin-rig-orf-300.
           if        w-rof-key-ord
                    (w-rof-ctr-002)       >    w-rof-sav-key
                     go to ord-fin-rig-orf-200.
           move      w-rof-ctr-002        to   w-rof-ctr-003          .
           move      w-rof-key-ord
                    (w-rof-ctr-002)       to   w-rof-sav-key          .
           go to     ord-fin-rig-orf-200.
       ord-fin-rig-orf-300.
           move      w-rof-ctr-001        to   w-rof-ctr-004          .          
           if        w-rof-sav-key        >    w-rof-key-ord
                                              (w-rof-ctr-004)
                     go to ord-fin-rig-orf-100.
           move      w-rof-sng-ele
                    (w-rof-ctr-003)       to   w-rof-sng-ele (999)    .
           move      w-rof-sng-ele
                    (w-rof-ctr-004)       to   w-rof-sng-ele
                                              (w-rof-ctr-003)         .
           move      w-rof-sng-ele (999)  to   w-rof-sng-ele
                                              (w-rof-ctr-004)         .
           go to     ord-fin-rig-orf-100.
       ord-fin-rig-orf-999.
           exit.

      *    *===========================================================*
      *    * Trattamento fine tranche ordine fornitore                 *
      *    *-----------------------------------------------------------*
       trt-fin-trc-orf-000.
      *              *-------------------------------------------------*
      *              * Test su contatore elementi                      *
      *              *-------------------------------------------------*
           if        w-stp-orf-wrk-ele    =    zero
                     go to trt-fin-trc-orf-900.
      *              *-------------------------------------------------*
      *              * Determinazione totali ordine                    *
      *              *-------------------------------------------------*
           perform   det-tot-orf-000      thru det-tot-orf-999        .
      *              *-------------------------------------------------*
      *              * Sventagliamento delle scadenze nei mesi di      *
      *              * competenza                                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-orf-wrk-ctr      .
       trt-fin-trc-orf-200.
           add       1                    to   w-stp-orf-wrk-ctr      .
      *                  *---------------------------------------------*
      *                  * Test preliminari                            *
      *                  *---------------------------------------------*
           if        w-tot-scd-imp
                    (w-stp-orf-wrk-ctr)   =    zero
                     go to trt-fin-trc-orf-800.
           if        w-stp-orf-wrk-ctr    >    96
                     go to trt-fin-trc-orf-800.
      *                  *---------------------------------------------*
      *                  * Confronto fra date                          *
      *                  *---------------------------------------------*
           if        w-tot-scd-dat
                    (w-stp-orf-wrk-ctr)   >    w-stp-dat-inp-fm6
                     add  w-tot-scd-imp
                         (w-stp-orf-wrk-ctr)
                                          to   w-stp-orf-cum-mso
                                              (w-stp-orf-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orf-wrk-ctr)   >    w-stp-dat-inp-fm5
                     add  w-tot-scd-imp
                         (w-stp-orf-wrk-ctr)
                                          to   w-stp-orf-cum-ms6
                                              (w-stp-orf-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orf-wrk-ctr)   >    w-stp-dat-inp-fm4
                     add  w-tot-scd-imp
                         (w-stp-orf-wrk-ctr)
                                          to   w-stp-orf-cum-ms5
                                              (w-stp-orf-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orf-wrk-ctr)   >    w-stp-dat-inp-fm3
                     add  w-tot-scd-imp
                         (w-stp-orf-wrk-ctr)
                                          to   w-stp-orf-cum-ms4
                                              (w-stp-orf-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orf-wrk-ctr)   >    w-stp-dat-inp-fm2
                     add  w-tot-scd-imp
                         (w-stp-orf-wrk-ctr)
                                          to   w-stp-orf-cum-ms3
                                              (w-stp-orf-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orf-wrk-ctr)   >    w-stp-dat-inp-fm1
                     add  w-tot-scd-imp
                         (w-stp-orf-wrk-ctr)
                                          to   w-stp-orf-cum-ms2
                                              (w-stp-orf-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orf-wrk-ctr)   >    w-stp-dat-inp-fmr
                     add  w-tot-scd-imp
                         (w-stp-orf-wrk-ctr)
                                          to   w-stp-orf-cum-ms1
                                              (w-stp-orf-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orf-wrk-ctr)   >    w-stp-dat-inp-fmp
                     add  w-tot-scd-imp
                         (w-stp-orf-wrk-ctr)
                                          to   w-stp-orf-cum-mic
                                              (w-stp-orf-wrk-ele)
           else      add  w-tot-scd-imp
                         (w-stp-orf-wrk-ctr)
                                          to   w-stp-orf-cum-pre
                                              (w-stp-orf-wrk-ele)     .
           go to     trt-fin-trc-orf-200.
       trt-fin-trc-orf-800.
      *              *-------------------------------------------------*
      *              * Cumulo totali generali                          *
      *              *-------------------------------------------------*
           add       w-stp-orf-cum-pre
                    (w-stp-orf-wrk-ele)   to   w-stp-tot-tot-pre (1)  .
           add       w-stp-orf-cum-mic
                    (w-stp-orf-wrk-ele)   to   w-stp-tot-tot-mic (1)  .
           add       w-stp-orf-cum-ms1
                    (w-stp-orf-wrk-ele)   to   w-stp-tot-tot-ms1 (1)  .
           add       w-stp-orf-cum-ms2
                    (w-stp-orf-wrk-ele)   to   w-stp-tot-tot-ms2 (1)  .
           add       w-stp-orf-cum-ms3
                    (w-stp-orf-wrk-ele)   to   w-stp-tot-tot-ms3 (1)  .
           add       w-stp-orf-cum-ms4
                    (w-stp-orf-wrk-ele)   to   w-stp-tot-tot-ms4 (1)  .
           add       w-stp-orf-cum-ms5
                    (w-stp-orf-wrk-ele)   to   w-stp-tot-tot-ms5 (1)  .
           add       w-stp-orf-cum-ms6
                    (w-stp-orf-wrk-ele)   to   w-stp-tot-tot-ms6 (1)  .
           add       w-stp-orf-cum-mso
                    (w-stp-orf-wrk-ele)   to   w-stp-tot-tot-mso (1)  .
      *              *-------------------------------------------------*
      *              * Cumulo totali fornitore                         *
      *              *-------------------------------------------------*
           add       w-stp-orf-cum-pre
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-tot-pre      .
           add       w-stp-orf-cum-mic
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-tot-mic      .
           add       w-stp-orf-cum-ms1
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-tot-ms1      .
           add       w-stp-orf-cum-ms2
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-tot-ms2      .
           add       w-stp-orf-cum-ms3
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-tot-ms3      .
           add       w-stp-orf-cum-ms4
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-tot-ms4      .
           add       w-stp-orf-cum-ms5
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-tot-ms5      .
           add       w-stp-orf-cum-ms6
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-tot-ms6      .
           add       w-stp-orf-cum-mso
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-tot-mso      .
      *              *-------------------------------------------------*
      *              * Cumulo subtotali fornitore                      *
      *              *-------------------------------------------------*
           add       w-stp-orf-cum-pre
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-sbt-pre (1)  .
           add       w-stp-orf-cum-mic
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-sbt-mic (1)  .
           add       w-stp-orf-cum-ms1
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-sbt-ms1 (1)  .
           add       w-stp-orf-cum-ms2
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-sbt-ms2 (1)  .
           add       w-stp-orf-cum-ms3
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-sbt-ms3 (1)  .
           add       w-stp-orf-cum-ms4
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-sbt-ms4 (1)  .
           add       w-stp-orf-cum-ms5
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-sbt-ms5 (1)  .
           add       w-stp-orf-cum-ms6
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-sbt-ms6 (1)  .
           add       w-stp-orf-cum-mso
                    (w-stp-orf-wrk-ele)   to   w-stp-lvf-sbt-mso (1)  .
      *              *-------------------------------------------------*
      *              * Aggiornamento saldi Cash-Flow                   *
      *              *-------------------------------------------------*
______*    perform   agg-rec-cfs-000      thru agg-rec-cfs-999        .
       trt-fin-trc-orf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     trt-fin-trc-orf-999.
       trt-fin-trc-orf-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per editing del codice iva                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/wedtzci0.wks"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione del coefficiente di cambio *
      *    * valuta                                                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoecmb0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione stato ordine fornitore     *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dstsorf0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione quantita' da evadere riga  *
      *    * ordine fornitore                                          *
      *    *-----------------------------------------------------------*
           copy      "pgm/orf/prg/cpy/dqevrof0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione imposta Iva in base ad un  *
      *    * imponibile                                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/dimpiva0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .
