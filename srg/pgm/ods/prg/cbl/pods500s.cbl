       Identification Division.
       Program-Id.                                 pods500s           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    ods                 *
      *                                Settore:    pkl                 *
      *                                   Fase:    ods500              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 31/08/95    *
      *                       Ultima revisione:    NdK del 14/01/15    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Modulo di stampa documento : Packing List   *
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
      *    * Area di comunicazione per modulo                 "msegrt" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/s"                                  .

      *    *===========================================================*
      *    * Area di comunicazione per modulo                 "mpslct" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/r"                                  .

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
      *    * Work-area per la ridefinizione della variabile di i.p.c.  *
      *    * in input  "inp-mst"                                       *
      *    *-----------------------------------------------------------*
       01  w-inp-mst.
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        * - OP : Open modulo di stampa documento                *
      *        * - ST : Stampa documento                               *
      *        * - CL : Close modulo di stampa documento               *
      *        *-------------------------------------------------------*
           05  w-inp-mst-ope              pic  x(02)                  .
      *        *-------------------------------------------------------*
      *        * Dati in input relativi ai tipi operazione             *
      *        *-------------------------------------------------------*
           05  w-inp-mst-dat.
      *            *---------------------------------------------------*
      *            * Dati in input relativi al tipo operazione "OP"    *
      *            *---------------------------------------------------*
               10  w-inp-mst-xop.
      *                *-----------------------------------------------*
      *                * Tipo selezione stampante                      *
      *                * - F : Facoltativa                             *
      *                * - O : Obbligatoria                            *
      *                *-----------------------------------------------*
                   15  w-inp-mst-sel      pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Codice azienda                                *
      *                *-----------------------------------------------*
                   15  w-inp-mst-azi      pic  x(04)                  .
      *                *-----------------------------------------------*
      *                * Codice terminale                              *
      *                *-----------------------------------------------*
                   15  w-inp-mst-ter      pic  x(08)                  .
      *                *-----------------------------------------------*
      *                * Codice utente                                 *
      *                *-----------------------------------------------*
                   15  w-inp-mst-ute      pic  x(08)                  .
      *                *-----------------------------------------------*
      *                * Sigla sistema applicativo                     *
      *                *-----------------------------------------------*
                   15  w-inp-mst-sap      pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Sigla area gestionale                         *
      *                *-----------------------------------------------*
                   15  w-inp-mst-arg      pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Sigla settore gestionale                      *
      *                *-----------------------------------------------*
                   15  w-inp-mst-set      pic  x(03)                  .
      *                *-----------------------------------------------*
      *                * Sigla fase gestionale                         *
      *                *-----------------------------------------------*
                   15  w-inp-mst-fas      pic  x(06)                  .
      *                *-----------------------------------------------*
      *                * Descrizione del programma                     *
      *                *-----------------------------------------------*
                   15  w-inp-mst-dep      pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Filler                                        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(02)                  .
      *            *---------------------------------------------------*
      *            * Dati in input relativi al tipo operazione "ST"    *
      *            *---------------------------------------------------*
               10  w-inp-mst-xst redefines
                   w-inp-mst-xop.
      *                *-----------------------------------------------*
      *                * Identificazione del documento                 *
      *                *-----------------------------------------------*
                   15  w-inp-mst-prt      pic  9(11)                  .
      *                *-----------------------------------------------*
      *                * Identificazione del collo                     *
      *                *-----------------------------------------------*
                   15  w-inp-mst-prc      pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Si/No Eject a fine documento                  *
      *                *  - S : Si                                     *
      *                *  - N : No                                     *
      *                *-----------------------------------------------*
                   15  w-inp-mst-ejc      pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Filler                                        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(61)                  .
      *            *---------------------------------------------------*
      *            * Dati in input relativi al tipo operazione "CL"    *
      *            *---------------------------------------------------*
               10  w-inp-mst-xcl redefines
                   w-inp-mst-xop.
      *                *-----------------------------------------------*
      *                * Nessun dato ulteriore                         *
      *                *-----------------------------------------------*
                   15  filler             pic  x(78)                  .

      *    *===========================================================*
      *    * Work-area per la ridefinizione della variabile di i.p.c.  *
      *    * in output "out-mst"                                       *
      *    *-----------------------------------------------------------*
       01  w-out-mst.
      *        *-------------------------------------------------------*
      *        * Dati in output relativi ai tipi operazione            *
      *        *-------------------------------------------------------*
           05  w-out-mst-dat.
      *            *---------------------------------------------------*
      *            * Dati in output relativi al tipo operazione "OP"   *
      *            *---------------------------------------------------*
               10  w-out-mst-xop.
      *                *-----------------------------------------------*
      *                * Esito della funzione Open                     *
      *                * - Spaces : Operazione correttamente eseguita  *
      *                *            con selezione stampante effettua-  *
      *                *            ta correttamente                   *
      *                * - N      : Operazione correttamente eseguita  *
      *                *            e senza selezione stampante facol- *
      *                *            tativa non effettuata              *
      *                * - X      : Operazione non eseguita in quanto  *
      *                *            selezione stampante obbligatoria   *
      *                *            non effettuata                     *
      *                * - E      : Errore grave avvenuto in esecuzio- *
      *                *            ne della funzione                  *
      *                *-----------------------------------------------*
                   15  w-out-mst-opn      pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Filler                                        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(79)                  .
      *            *---------------------------------------------------*
      *            * Dati in output relativi al tipo operazione "ST"   *
      *            *---------------------------------------------------*
               10  w-out-mst-xst redefines
                   w-out-mst-xop.
      *                *-----------------------------------------------*
      *                * Esito della funzione Stampa                   *
      *                * - Spaces : Operazione correttamente eseguita  *
      *                * - N      : Documento non trovato              *
      *                * - E      : Errore grave avvenuto in esecuzio- *
      *                *            ne della funzione                  *
      *                *-----------------------------------------------*
                   15  w-out-mst-stp      pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Filler                                        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(79)                  .
      *            *---------------------------------------------------*
      *            * Dati in output relativi al tipo operazione "CL"   *
      *            *---------------------------------------------------*
               10  w-out-mst-xcl redefines
                   w-out-mst-xop.
      *                *-----------------------------------------------*
      *                * Esito della funzione Close                    *
      *                * - Spaces : Operazione correttamente eseguita  *
      *                * - E      : Errore grave avvenuto in esecuzio- *
      *                *            ne della funzione                  *
      *                *-----------------------------------------------*
                   15  w-out-mst-cls      pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Filler                                        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(79)                  .

      *    *===========================================================*
      *    * Area di identificazione del programma chiamante           *
      *    *-----------------------------------------------------------*
       01  w-ide.
      *        *-------------------------------------------------------*
      *        * Tipo selezione stampante                              *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-sel              pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Codice azienda                                        *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-azi              pic  x(04) value spaces     .
      *        *-------------------------------------------------------*
      *        * Codice terminale                                      *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-ter              pic  x(08) value spaces     .
      *        *-------------------------------------------------------*
      *        * Codice utente                                         *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-ute              pic  x(08) value spaces     .
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-sap              pic  x(03) value spaces     .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-arg              pic  x(03) value spaces     .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-set              pic  x(03) value spaces     .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-fas              pic  x(06) value spaces     .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-dep              pic  x(40) value spaces     .
      *        *-------------------------------------------------------*
      *        * Data e ora di sistema                                 *
      *        *-------------------------------------------------------*
           05  w-ide-pgc-sdt              pic  9(15) value zero       .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Flags di controllo su status generali del programma   *
      *        *-------------------------------------------------------*
           05  w-cnt-sts.
      *            *---------------------------------------------------*
      *            * Flag di selezione stampante                       *
      *            * - Spaces : Non selezionata                        *
      *            * - S      : Selezionata                            *
      *            *---------------------------------------------------*
               10  w-cnt-sts-flg-sst      pic  x(01) value spaces     .
      *            *---------------------------------------------------*
      *            * Flag di files necessari per la stampa aperti      *
      *            * - Spaces : Non aperti                             *
      *            * - S      : Aperti                                 *
      *            *---------------------------------------------------*
               10  w-cnt-sts-flg-opn      pic  x(01) value spaces     .
      *            *---------------------------------------------------*
      *            * Flag di Begin eseguito                            *
      *            *---------------------------------------------------*
               10  w-cnt-sts-flg-bgn      pic  x(01) value spaces     .
      *        *-------------------------------------------------------*
      *        * Area per preparazione parametri selezione stampa      *
      *        *-------------------------------------------------------*
           05  w-cnt-stp.
      *            *---------------------------------------------------*
      *            * Parametri di stampa specifici del programma       *
      *            *---------------------------------------------------*
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
      *        * Area di controllo per funzionamento print-routine     *
      *        *-------------------------------------------------------*
           05  w-cnt-prn.
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-prn-flg-int      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Work per string-unstring                              *
      *        *-------------------------------------------------------*
           05  w-cnt-stu.
               10  w-cnt-stu-num-seg      pic  9(05)                  .
               10  w-cnt-stu-pnt-stu      pic  9(05)                  .
               10  w-cnt-stu-255-byt.
                   15  filler occurs 255  pic  x(01)                  .
               10  w-cnt-stu-sav-pnt      pic  9(05)                  .
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

      *    *===========================================================*
      *    * Records files                                             *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [osk]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosk"                          .
      *        *-------------------------------------------------------*
      *        * [ost]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfost"                          .
      *        *-------------------------------------------------------*
      *        * [osr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosr"                          .
      *        *-------------------------------------------------------*
      *        * [osx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosx"                          .
      *        *-------------------------------------------------------*
      *        * [zsc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfzsc"                          .
      *        *-------------------------------------------------------*
      *        * [zsa]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfzsa"                          .
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .
      *        *-------------------------------------------------------*
      *        * [dcp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfdcp"                          .
      *        *-------------------------------------------------------*
      *        * [pdx]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcp/fls/rec/rfpdx"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [zfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzfp"                          .
      *        *-------------------------------------------------------*
      *        * [zdf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzdf"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazione per modulo stampa packing list      *
      *        *-------------------------------------------------------*
      *            *---------------------------------------------------*
      *            * Comodo per ridefinizione della personalizzazione  *
      *            *---------------------------------------------------*
           05  w-prs-spl-str              pic  x(40)                  .
           05  w-prs-spl-str-r            redefines
               w-prs-spl-str.
      *            *---------------------------------------------------*
      *            * Tipo intestazione foglio                          *
      *            *                                                   *
      *            *  - 'N' : Nessuna intestazione                     *
      *            *  - 'R' : Intestazione come da referenza           *
      *            *  - 'P' : Intestazione standard da eseguirsi dal   *
      *            *          programma                                *
      *            *---------------------------------------------------*
               10  w-prs-spl-tin          pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice da stampare                                *
      *            *                                                   *
      *            *  - 'N' = Codice nostro                            *
      *            *  - 'C' = Codice del cliente, se esistente         *
      *            *---------------------------------------------------*
               10  w-prs-spl-cds          pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No riga aggiuntiva per stampa codice prodotto  *
      *            *                                                   *
      *            *  - 'N' = Nessuna riga aggiuntiva                  *
      *            *  - 'C' = Una riga aggiuntiva con il codice del    *
      *            *          cliente                                  *
      *            *  - 'M' = Una riga aggiuntiva con il codice nostro *
      *            *  - 'E' = Due righe aggiuntive, la prima con il    *
      *            *          codice del cliente, la seconda con il    *
      *            *          nostro codice                            *
      *            *---------------------------------------------------*
               10  w-prs-spl-rac          pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area libera per implementazioni future            *
      *            *---------------------------------------------------*
               10  w-prs-spl-exp          pic  x(34)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-prs-spl-ctr              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazioni per il tipo ordinamento righe corpo *
      *        *                                                       *
      *        * Questa personalizzazione viene ripresa da quella per  *
      *        * la stampa degli ordini di spedizione                  *
      *        *-------------------------------------------------------*
           05  w-prs-tor-rco.
      *            *---------------------------------------------------*
      *            * Tipo di ordinamento righe corpo                   *
      *            *                                                   *
      *            *  - '00' = Progressivo riga                        *
      *            *  - '01' = Per classe, gruppo e sottogruppo e      *
      *            *           codice prodotto                         *
      *            *  - '02' = Per classe, gruppo e sottogruppo e      *
      *            *           descrizione prodotto                    *
      *            *  - '03' = Per codice prodotto                     *
      *            *  - '04' = Per descrizione prodotto                *
      *            *  - '10' = Percorso di ubicazione, progressivo     *
      *            *           riga                                    *
      *            *  - '13' = Percorso di ubicazione, codice prodotto *
      *            *  - '14' = Percorso di ubicazione, descrizione     *
      *            *           prodotto                                *
      *            *---------------------------------------------------*
               10  w-prs-tor-rco-tip      pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Personalizzazione per modulo stampa ordine di spedi-  *
      *        * zione                                                 *
      *        *-------------------------------------------------------*
      *            *---------------------------------------------------*
      *            * Comodo per ridefinizione della personalizzazione  *
      *            *---------------------------------------------------*
           05  w-prs-sos-str              pic  x(40)                  .
           05  w-prs-sos-str-r            redefines
               w-prs-sos-str.
      *            *---------------------------------------------------*
      *            * Tipo modulo                                       *
      *            *                                                   *
      *            *  - 01 : Modulo senza esposizione codice articolo  *
      *            *  - 51 : Modulo con esposizione codice articolo    *
      *            *---------------------------------------------------*
               10  w-prs-sos-tmd          pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tipo stampato                                     *
      *            *                                                   *
      *            *  - 'T' : Tipografico                              *
      *            *  - 'L' : Libero                                   *
      *            *---------------------------------------------------*
               10  w-prs-sos-tst          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo intestazione foglio                          *
      *            *                                                   *
      *            *  - 'N' : Nessuna intestazione                     *
      *            *  - 'P' : Intestazione standard da eseguirsi dal   *
      *            *          programma                                *
      *            *---------------------------------------------------*
               10  w-prs-sos-tin          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/no stampa voce descrittiva per 6 voci          *
      *            *---------------------------------------------------*
               10  w-prs-sos-snv occurs 06
                                          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/no stampa sconto in chiusura in ordine cliente *
      *            *---------------------------------------------------*
               10  w-prs-sos-snc          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/no stampa sconto pagamento in ordine cliente   *
      *            *---------------------------------------------------*
               10  w-prs-sos-snp          pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/no stampa spesa di fatturazione in ordine      *
      *            * cliente per 6 voci                                *
      *            *---------------------------------------------------*
               10  w-prs-sos-sns occurs 06
                                          pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/no stampa delle quantita' editate a destra     *
      *            *---------------------------------------------------*
               10  w-prs-sos-snq          pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice da stampare                                *
      *            *                                                   *
      *            *  - 'N' = Codice nostro                            *
      *            *  - 'C' = Codice del cliente, se esistente         *
      *            *---------------------------------------------------*
               10  w-prs-sos-cds          pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No riga aggiuntiva per stampa codice prodotto  *
      *            *                                                   *
      *            *  - 'N' = Nessuna riga aggiuntiva                  *
      *            *  - 'C' = Una riga aggiuntiva con il codice del    *
      *            *          cliente                                  *
      *            *  - 'M' = Una riga aggiuntiva con il codice nostro *
      *            *  - 'E' = Due righe aggiuntive, la prima con il    *
      *            *          codice del cliente, la seconda con il    *
      *            *          nostro codice                            *
      *            *---------------------------------------------------*
               10  w-prs-sos-rac          pic  x(01)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo di visualizzazione da effettuare nel box     *
      *            * libero 1                                          *
      *            *                                                   *
      *            *  - '11' = Giorni di chiusura                      *
      *            *---------------------------------------------------*
               10  w-prs-sos-bl1          pic  9(02)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo di visualizzazione da effettuare nel box     *
      *            * libero 2                                          *
      *            *                                                   *
      *            *  - '12' = Orari di apertura                       *
      *            *---------------------------------------------------*
               10  w-prs-sos-bl2          pic  9(02)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Tipo di ordinamento righe corpo                   *
      *            *                                                   *
      *            *  - '00' = Progressivo riga                        *
      *            *  - '01' = Per classe, gruppo e sottogruppo e      *
      *            *           codice prodotto                         *
      *            *  - '02' = Per classe, gruppo e sottogruppo e      *
      *            *           descrizione prodotto                    *
      *            *  - '03' = Per codice prodotto                     *
      *            *  - '04' = Per descrizione prodotto                *
      *            *  - '10' = Percorso di ubicazione, progressivo     *
      *            *           riga                                    *
      *            *  - '13' = Percorso di ubicazione, codice prodotto *
      *            *  - '14' = Percorso di ubicazione, descrizione     *
      *            *           prodotto                                *
      *            *---------------------------------------------------*
               10  w-prs-sos-orr          pic  9(02)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area libera per implementazioni future            *
      *            *---------------------------------------------------*
               10  w-prs-sos-exp          pic  x(06)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-prs-sos-ctr              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Descrizioni per stampa intestazione documento         *
      *        *-------------------------------------------------------*
           05  w-ref-stp-int.
               10  w-ref-stp-int-ade.
                   15  w-ref-stp-int-des occurs 8
                                          pic  x(50)                  .
               10  w-ref-stp-int-ctr      pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per displacement                                *
      *    *-----------------------------------------------------------*
       01  w-dsp.
      *        *-------------------------------------------------------*
      *        * Displacement verticale                                *
      *        *-------------------------------------------------------*
           05  w-dsp-dsp-vrt              pic s9(03)                  .

      *    *===========================================================*
      *    * Work area per esecuzione stampa                           *
      *    *-----------------------------------------------------------*
       01  w-stp.
      *        *-------------------------------------------------------*
      *        * Per testata documento                                 *
      *        *-------------------------------------------------------*
           05  w-stp-tes.
      *            *---------------------------------------------------*
      *            * Numero pagina                                     *
      *            *---------------------------------------------------*
               10  w-stp-tes-num-pag      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Data documento da stampare                        *
      *            *---------------------------------------------------*
               10  w-stp-tes-dds.
                   15  w-stp-tes-dds-dat  pic  9(07)                  .
                   15  w-stp-tes-dds-r01  redefines
                       w-stp-tes-dds-dat.
                       20  w-stp-tes-dds-saa
                                          pic  9(03)                  .
                       20  w-stp-tes-dds-mes
                                          pic  9(02)                  .
                       20  w-stp-tes-dds-gio
                                          pic  9(02)                  .
                   15  w-stp-tes-dds-edt  pic  x(11)                  .
                   15  w-stp-tes-dds-r02  redefines
                       w-stp-tes-dds-edt.
                       20  w-stp-tes-dds-egg
                                          pic  9(02)                  .
                       20  filler         pic  x(01)                  .
                       20  w-stp-tes-dds-emm
                                          pic  x(03)                  .
                       20  filler         pic  x(01)                  .
                       20  w-stp-tes-dds-eaa
                                          pic  9(04)                  .
      *            *---------------------------------------------------*
      *            * Intestatario documento                            *
      *            *---------------------------------------------------*
               10  w-stp-tes-int.
      *                *-----------------------------------------------*
      *                * Ragione sociale 1                             *
      *                *-----------------------------------------------*
                   15  w-stp-tes-int-rag  pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Ragione sociale 2                             *
      *                *-----------------------------------------------*
                   15  w-stp-tes-int-rs2  pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Indirizzo                                     *
      *                *-----------------------------------------------*
                   15  w-stp-tes-int-via  pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Localita'                                     *
      *                *-----------------------------------------------*
                   15  w-stp-tes-int-loc  pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Indirizzo di spedizione                           *
      *            *---------------------------------------------------*
               10  w-stp-tes-ids.
      *                *-----------------------------------------------*
      *                * Ragione sociale 1                             *
      *                *-----------------------------------------------*
                   15  w-stp-tes-ids-rag  pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Ragione sociale 2                             *
      *                *-----------------------------------------------*
                   15  w-stp-tes-ids-rs2  pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Indirizzo                                     *
      *                *-----------------------------------------------*
                   15  w-stp-tes-ids-via  pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Localita'                                     *
      *                *-----------------------------------------------*
                   15  w-stp-tes-ids-loc  pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa righe corpo                       *
      *        *-------------------------------------------------------*
           05  w-stp-rig.
      *            *---------------------------------------------------*
      *            * Numero riga corpo in corso di stampa              *
      *            *---------------------------------------------------*
               10  w-stp-rig-ctr-rcs      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Max riga corpo stampabile                         *
      *            *---------------------------------------------------*
               10  w-stp-rig-ctr-max      pic  9(05) value 37         .
      *            *---------------------------------------------------*
      *            * Posizionamento prima riga corpo                   *
      *            *---------------------------------------------------*
               10  w-stp-rig-ppr-cor      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Comodo per editing dimensioni                     *
      *            *---------------------------------------------------*
               10  w-stp-rig-edd.
                   15  w-stp-rig-edd-001  pic  x(10)                  .
                   15  w-stp-rig-edd-002  pic  x(10)                  .
                   15  w-stp-rig-edd-003  pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo per la stampa delle descrizioni in *
      *            * riga                                              *
      *            *---------------------------------------------------*
               10  w-stp-rig-des.
      *                *-----------------------------------------------*
      *                * Contatori di comodo                           *
      *                *-----------------------------------------------*
                   15  w-stp-rig-des-ctr  pic  9(03)                  .
                   15  w-stp-rig-des-c01  pic  9(02)                  .
                   15  w-stp-rig-des-c02  pic  9(02)                  .
                   15  w-stp-rig-des-nrd  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Comodo per descrizione                        *
      *                *-----------------------------------------------*
                   15  w-stp-rig-des-der.
                       20  w-stp-rig-des-rgd occurs 10
                                          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Per piede documento                                   *
      *        *-------------------------------------------------------*
           05  w-stp-pie.
      *            *---------------------------------------------------*
      *            * Total No. of Packings                             *
      *            *---------------------------------------------------*
               10  w-stp-pie-tno-pkg      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Total Net Weight                                  *
      *            *---------------------------------------------------*
               10  w-stp-pie-tot-ntw      pic s9(08)v9(03)            .
      *            *---------------------------------------------------*
      *            * Total Gross Weight                                *
      *            *---------------------------------------------------*
               10  w-stp-pie-tot-gsw      pic s9(08)v9(03)            .

      *    *===========================================================*
      *    * Work per routine rou-stp-box-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-rou-stp-box.
      *        *-------------------------------------------------------*
      *        * Parametri in input                                    *
      *        *-------------------------------------------------------*
           05  w-rou-stp-box-lin          pic  9(03)                  .
           05  w-rou-stp-box-pin          pic  9(03)                  .
           05  w-rou-stp-box-lfi          pic  9(03)                  .
           05  w-rou-stp-box-pfi          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Campi di lavoro                                       *
      *        *-------------------------------------------------------*
           05  w-rou-stp-box-wln          pic  9(03)                  .
           05  w-rou-stp-box-wps          pic  9(03)                  .
           05  w-rou-stp-box-wsz          pic  9(03)                  .
           05  w-rou-stp-box-wtr.
               10  filler            occurs 132
                                          pic  x(01) value all "-"    .
           05  w-rou-stp-box-wst.
               10  w-rou-stp-box-wch occurs 132
                                          pic  x(01)                  .
           05  w-rou-stp-box-ctr          pic  9(03)                  .

      *    *===========================================================*
      *    * Work per Let su archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.ltw"                   .

      *    *===========================================================*
      *    * Work-area per ridefinizione tipo riga                     *
      *    *-----------------------------------------------------------*
       01  w-rdf-tip-rig.
           05  w-rdf-tip-rig-wtr.
               10  w-rdf-tip-rig-wtp      pic  x(01)                  .
               10  w-rdf-tip-rig-wtf      pic  x(01)                  .
               10  filler                 pic  x(03)                  .

      *    *===========================================================*
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Salvataggio di un intero record [dcc]                 *
      *        *-------------------------------------------------------*
           05  w-sav-rec-dcc.
               10  filler  occurs 2048    pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio di un intero record [dcf]                 *
      *        *-------------------------------------------------------*
           05  w-sav-rec-dcf.
               10  filler  occurs 2048    pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      ******************************************************************
       Procedure Division.
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Lettura della variabile di i.p.c. dello stesso  *
      *              * livello "inp-mst"                               *
      *              *-------------------------------------------------*
           perform   rea-inp-mst-000      thru rea-inp-mst-999        .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo operazione      *
      *              *-------------------------------------------------*
           if        w-inp-mst-ope        =    "OP"
                     go to main-200
           else if   w-inp-mst-ope        =    "ST"
                     go to main-400
           else if   w-inp-mst-ope        =    "CL"
                     go to main-600
           else      go to main-999.
       main-200.
      *              *-------------------------------------------------*
      *              * Se funzione OP : Open                           *
      *              *-------------------------------------------------*
           perform   exe-fun-opn-000      thru exe-fun-opn-999        .
           go to     main-999.
       main-400.
      *              *-------------------------------------------------*
      *              * Se funzione ST : Stampa                         *
      *              *-------------------------------------------------*
           perform   exe-fun-stp-000      thru exe-fun-stp-999        .
           go to     main-999.
       main-600.
      *              *-------------------------------------------------*
      *              * Se funzione CL : Close                          *
      *              *-------------------------------------------------*
           perform   exe-fun-cls-000      thru exe-fun-cls-999        .
           go to     main-999.
       main-999.
           exit      program                                          .

      *================================================================*
      *       Routines                                                 *
      *================================================================*

      *    *===========================================================*
      *    * Lettura della variabile di i.p.c. dello stesso livello di *
      *    * nome "inp-mst" per parametri su funzione da eseguire      *
      *    *-----------------------------------------------------------*
       rea-inp-mst-000.
      *              *-------------------------------------------------*
      *              * Lettura della variabile                         *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "inp-mst"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Test su esito lettura                           *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to rea-inp-mst-600.
       rea-inp-mst-300.
      *              *-------------------------------------------------*
      *              * Se variabile non trovata o non corretta         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione area di ridefinizione della *
      *                  * variabile                                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-inp-mst              .
      *                  *---------------------------------------------*
      *                  * Cancellazione della variabile in uscita     *
      *                  *---------------------------------------------*
           perform   del-out-mst-000      thru del-out-mst-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     rea-inp-mst-999.
       rea-inp-mst-600.
      *              *-------------------------------------------------*
      *              * Se variabile trovata                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su formato della variabile             *
      *                  *---------------------------------------------*
           if        s-tip                not  = "A"
                     go to rea-inp-mst-300.
           if        s-car                not  = 80
                     go to rea-inp-mst-300.
      *                  *---------------------------------------------*
      *                  * Se formato variabile corretto               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Spostamento del valore della variabile  *
      *                      * in area di ridefinizione                *
      *                      *-----------------------------------------*
           move      s-alf                to   w-inp-mst              .
       rea-inp-mst-999.
           exit.

      *    *===========================================================*
      *    * Cancellazione della variabile di i.p.c. dello stesso li-  *
      *    * vello di nome "out-mst"                                   *
      *    *-----------------------------------------------------------*
       del-out-mst-000.
      *              *-------------------------------------------------*
      *              * Cancellazione della variabile                   *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "out-mst"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       del-out-mst-999.
           exit.

      *    *===========================================================*
      *    * Scrittura della variabile di i.p.c. dello stesso livello  *
      *    * di nome "out-mst"                                         *
      *    *-----------------------------------------------------------*
       wrt-out-mst-000.
      *              *-------------------------------------------------*
      *              * Scrittura della variabile                       *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "out-mst"            to   s-var                  .
           move      "="                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      80                   to   s-car                  .
           move      w-out-mst            to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       wrt-out-mst-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione funzione OP : Open                             *
      *    *-----------------------------------------------------------*
       exe-fun-opn-000.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori passati dal chiamante    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo selezione stampante                    *
      *                  *---------------------------------------------*
           move      w-inp-mst-sel        to   w-ide-pgc-sel          .
      *                  *---------------------------------------------*
      *                  * Codice azienda                              *
      *                  *---------------------------------------------*
           move      w-inp-mst-azi        to   w-ide-pgc-azi          .
      *                  *---------------------------------------------*
      *                  * Codice terminale                            *
      *                  *---------------------------------------------*
           move      w-inp-mst-ter        to   w-ide-pgc-ter          .
      *                  *---------------------------------------------*
      *                  * Codice utente                               *
      *                  *---------------------------------------------*
           move      w-inp-mst-ute        to   w-ide-pgc-ute          .
      *                  *---------------------------------------------*
      *                  * Sigla sistema applicativo                   *
      *                  *---------------------------------------------*
           move      w-inp-mst-sap        to   w-ide-pgc-sap          .
      *                  *---------------------------------------------*
      *                  * Sigla area gestionale                       *
      *                  *---------------------------------------------*
           move      w-inp-mst-arg        to   w-ide-pgc-arg          .
      *                  *---------------------------------------------*
      *                  * Sigla settore gestionale                    *
      *                  *---------------------------------------------*
           move      w-inp-mst-set        to   w-ide-pgc-set          .
      *                  *---------------------------------------------*
      *                  * Sigla fase gestionale                       *
      *                  *---------------------------------------------*
           move      w-inp-mst-fas        to   w-ide-pgc-fas          .
      *                  *---------------------------------------------*
      *                  * Descrizione del programma                   *
      *                  *---------------------------------------------*
           move      w-inp-mst-dep        to   w-ide-pgc-dep          .
       exe-fun-opn-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione data e ora di sistema           *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-sdt                to   w-ide-pgc-sdt          .
       exe-fun-opn-400.
      *              *-------------------------------------------------*
      *              * Selezione della stampante                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione parametri per selezione della  *
      *                  * stampante specifici per il programma        *
      *                  *---------------------------------------------*
           perform   pre-prm-stp-000      thru pre-prm-stp-999        .
      *                  *---------------------------------------------*
      *                  * Selezione stampante                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Parametri per modulo "mpslct"           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Codice azienda                      *
      *                          *-------------------------------------*
           move      w-ide-pgc-azi        to   r-env-cod-azi          .
      *                          *-------------------------------------*
      *                          * Codice terminale                    *
      *                          *-------------------------------------*
           move      w-ide-pgc-ter        to   r-env-cod-ter          .
      *                          *-------------------------------------*
      *                          * Codice utente                       *
      *                          *-------------------------------------*
           move      w-ide-pgc-ute        to   r-env-cod-ute          .
      *                          *-------------------------------------*
      *                          * System date and time                *
      *                          *-------------------------------------*
           move      w-ide-pgc-sdt        to   r-env-dat-tim          .
      *                          *-------------------------------------*
      *                          * Sistema applicativo                 *
      *                          *-------------------------------------*
           move      w-ide-pgc-sap        to   r-ide-sis-app          .
      *                          *-------------------------------------*
      *                          * Area gestionale                     *
      *                          *-------------------------------------*
           move      w-ide-pgc-arg        to   r-ide-are-ges          .
      *                          *-------------------------------------*
      *                          * Settore gestionale                  *
      *                          *-------------------------------------*
           move      w-ide-pgc-set        to   r-ide-set-ges          .
      *                          *-------------------------------------*
      *                          * Fase gestionale                     *
      *                          *-------------------------------------*
           move      w-ide-pgc-fas        to   r-ide-fas-ges          .
      *                          *-------------------------------------*
      *                          * Flags di tipo selezione             *
      *                          *-------------------------------------*
           move      w-cnt-stp-tip-sel    to   r-fix-tip-sel          .
      *                          *-------------------------------------*
      *                          * Codice stampante                    *
      *                          *-------------------------------------*
           move      w-cnt-stp-cod-stp    to   r-fix-cod-stp          .
      *                          *-------------------------------------*
      *                          * Tipo di stampa                      *
      *                          *-------------------------------------*
           move      w-cnt-stp-tip-sta    to   r-fix-tip-sta          .
      *                          *-------------------------------------*
      *                          * Codice modulo                       *
      *                          *-------------------------------------*
           move      w-cnt-stp-cod-mod    to   r-fix-cod-mod          .
      *                          *-------------------------------------*
      *                          * Tipo modulo                         *
      *                          *-------------------------------------*
           move      w-cnt-stp-tip-mod    to   r-fix-tip-mod          .
      *                          *-------------------------------------*
      *                          * Ampiezza linea stampa in caratteri  *
      *                          *-------------------------------------*
           move      w-cnt-stp-amp-lin    to   r-fix-amp-lin          .
      *                          *-------------------------------------*
      *                          * Top margin in linee                 *
      *                          *-------------------------------------*
           move      w-cnt-stp-top-lin    to   r-fix-top-lin          .
      *                          *-------------------------------------*
      *                          * Numero linee di stampa minimo       *
      *                          *-------------------------------------*
           move      w-cnt-stp-lin-min    to   r-fix-lin-min          .
      *                          *-------------------------------------*
      *                          * Bottom margin in linee              *
      *                          *-------------------------------------*
           move      w-cnt-stp-bot-lin    to   r-fix-bot-lin          .
      *                          *-------------------------------------*
      *                          * Ampiezza caratteri                  *
      *                          *-------------------------------------*
           move      w-cnt-stp-amp-car    to   r-fix-amp-car          .
      *                          *-------------------------------------*
      *                          * Altezza interlinea                  *
      *                          *-------------------------------------*
           move      w-cnt-stp-alt-int    to   r-fix-alt-int          .
      *                          *-------------------------------------*
      *                          * Area riservata espansioni future    *
      *                          *-------------------------------------*
           move      w-cnt-stp-esp-fut    to   r-fix-esp-fut          .
      *                          *-------------------------------------*
      *                          * Area riservata funzioni speciali    *
      *                          *-------------------------------------*
           move      w-cnt-stp-fnz-spc    to   r-fix-fnz-spc          .
      *                      *-----------------------------------------*
      *                      * Richiamo modulo "mpslct"                *
      *                      *-----------------------------------------*
           call      "swd/mod/prg/obj/mpslct"
                                         using r                      .
           cancel    "swd/mod/prg/obj/mpslct"                         .
      *                  *---------------------------------------------*
      *                  * Test su esito selezione stampante           *
      *                  *---------------------------------------------*
           if        r-rsc                =    spaces
                     go to exe-fun-opn-600.
       exe-fun-opn-425.
      *                  *---------------------------------------------*
      *                  * Se selezione stampante non eseguita         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su tipo selezione stampante        *
      *                      *-----------------------------------------*
           if        w-ide-pgc-sel        =    "O"
                     go to exe-fun-opn-475.
       exe-fun-opn-450.
      *                          *-------------------------------------*
      *                          * Se tipo selezione stampante facol-  *
      *                          * tativo                              *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Apertura files necessari alla   *
      *                              * stampa                          *
      *                              *---------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
      *                              *---------------------------------*
      *                              * Flag di files aperti            *
      *                              *---------------------------------*
           move      "S"                  to   w-cnt-sts-flg-opn      .
      *                              *---------------------------------*
      *                              * Flag di selezione stampante non *
      *                              * effettuata                      *
      *                              *---------------------------------*
           move      spaces               to   w-cnt-sts-flg-sst      .
      *                              *---------------------------------*
      *                              * Flag di Begin non eseguito      *
      *                              *---------------------------------*
           move      spaces               to   w-cnt-sts-flg-bgn      .
      *                              *---------------------------------*
      *                              * Uscita con status a "N"         *
      *                              *---------------------------------*
           move      spaces               to   w-out-mst              .
           move      "N"                  to   w-out-mst-opn          .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     exe-fun-opn-999.
       exe-fun-opn-475.
      *                          *-------------------------------------*
      *                          * Se tipo selezione stampante obbli-  *
      *                          * gatorio                             *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Flag di files non aperti        *
      *                              *---------------------------------*
           move      spaces               to   w-cnt-sts-flg-opn      .
      *                              *---------------------------------*
      *                              * Flag di selezione stampante non *
      *                              * effettuata                      *
      *                              *---------------------------------*
           move      spaces               to   w-cnt-sts-flg-sst      .
      *                              *---------------------------------*
      *                              * Flag di Begin non eseguito      *
      *                              *---------------------------------*
           move      spaces               to   w-cnt-sts-flg-bgn      .
      *                              *---------------------------------*
      *                              * Uscita con status a "X"         *
      *                              *---------------------------------*
           move      spaces               to   w-out-mst              .
           move      "X"                  to   w-out-mst-opn          .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     exe-fun-opn-999.
       exe-fun-opn-600.
      *                  *---------------------------------------------*
      *                  * Se selezione stampante eseguita             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura parametri di selezione stampa   *
      *                      * da segreteria                           *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Inizializzazione area parametri di  *
      *                          * stampa                              *
      *                          *-------------------------------------*
           move      spaces               to   p-sel                  .
      *                          *-------------------------------------*
      *                          * Inizializzazione numero progressivo *
      *                          * segmento                            *
      *                          *-------------------------------------*
           move      zero                 to   w-cnt-stu-num-seg      .
       exe-fun-opn-650.
      *                          *-------------------------------------*
      *                          * Incremento numero progressivo seg-  *
      *                          * mento                               *
      *                          *-------------------------------------*
           add       1                    to   w-cnt-stu-num-seg      .
      *                          *-------------------------------------*
      *                          * Richiamo del modulo di segreteria   *
      *                          * per l'estrazione del segmento di    *
      *                          * parametri stampa                    *
      *                          *-------------------------------------*
           move      "S<"                 to   s-ope                  .
           move      w-cnt-stu-num-seg    to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                          *-------------------------------------*
      *                          * Concatenazione del segmento in area *
      *                          * parametri di stampa selezionati     *
      *                          *-------------------------------------*
           move      w-cnt-stu-num-seg    to   w-cnt-stu-pnt-stu      .
           multiply  80                   by   w-cnt-stu-pnt-stu      .
           subtract  79                   from w-cnt-stu-pnt-stu      .
           move      w-cnt-stu-pnt-stu    to   w-cnt-stu-sav-pnt      .
           string    s-alf
                     delimited by size    into p-sel
                                  with pointer w-cnt-stu-pnt-stu      .
      *                          *-------------------------------------*
      *                          * Riciclo su segmento successivo, a   *
      *                          * meno che non si sia sull'ultimo     *
      *                          *-------------------------------------*
           if        w-cnt-stu-pnt-stu    not  = w-cnt-stu-sav-pnt
                     go to exe-fun-opn-650.
       exe-fun-opn-700.
      *                      *-----------------------------------------*
      *                      * Preparazioni per uscita                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Flag di selezione stampante effet-  *
      *                          * tuata                               *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-sts-flg-sst      .
      *                          *-------------------------------------*
      *                          * Flag di Begin non eseguito          *
      *                          *-------------------------------------*
           move      spaces               to   w-cnt-sts-flg-bgn      .
      *                          *-------------------------------------*
      *                          * Apertura files necessari alla stam- *
      *                          * pa                                  *
      *                          *-------------------------------------*
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
      *                          *-------------------------------------*
      *                          * Flag di files aperti                *
      *                          *-------------------------------------*
           move      "S"                  to   w-cnt-sts-flg-opn      .
      *                          *-------------------------------------*
      *                          * Uscita con status a Spaces          *
      *                          *-------------------------------------*
           move      spaces               to   w-out-mst              .
           move      spaces               to   w-out-mst-opn          .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     exe-fun-opn-999.
       exe-fun-opn-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione funzione CL : Close                            *
      *    *-----------------------------------------------------------*
       exe-fun-cls-000.
      *              *-------------------------------------------------*
      *              * Se files aperti                                 *
      *              *-------------------------------------------------*
           if        w-cnt-sts-flg-opn    not  = "S"
                     go to exe-fun-cls-100.
      *                  *---------------------------------------------*
      *                  * Chiusura files necessari alla stampa        *
      *                  *---------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
       exe-fun-cls-100.
      *              *-------------------------------------------------*
      *              * Se selezione stampante effettuata               *
      *              *-------------------------------------------------*
           if        w-cnt-sts-flg-sst    not  = "S"
                     go to exe-fun-cls-900.
      *                  *---------------------------------------------*
      *                  * Esecuzione End                              *
      *                  *---------------------------------------------*
           move      "EN"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo stampa    "mprint"     *
      *                  *---------------------------------------------*
           cancel    "swd/mod/prg/obj/mprint"                         .
       exe-fun-cls-900.
      *              *-------------------------------------------------*
      *              * Flag di files non aperti                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-flg-opn      .
      *              *-------------------------------------------------*
      *              * Flag di selezione stampante non effettuata      *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-flg-sst      .
      *              *-------------------------------------------------*
      *              * Flag di Begin non eseguito                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-sts-flg-bgn      .
       exe-fun-cls-999.
           exit.

      *    *===========================================================*
      *    * Esecuzione funzione ST : Stampa                           *
      *    *-----------------------------------------------------------*
       exe-fun-stp-000.
      *              *-------------------------------------------------*
      *              * Se files non aperti : uscita                    *
      *              *-------------------------------------------------*
           if        w-cnt-sts-flg-opn    not  = "S"
                     go to exe-fun-stp-999.
      *              *-------------------------------------------------*
      *              * Se selezione stampante non effettuata : uscita  *
      *              *-------------------------------------------------*
           if        w-cnt-sts-flg-sst    not  = "S"
                     go to exe-fun-stp-999.
      *              *-------------------------------------------------*
      *              * Se Begin non eseguito                           *
      *              *-------------------------------------------------*
           if        w-cnt-sts-flg-bgn    =    "S"
                     go to exe-fun-stp-500.
      *                  *---------------------------------------------*
      *                  * Esecuzione Begin                            *
      *                  *---------------------------------------------*
           move      "BE"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Flag di Begin eseguito                      *
      *                  *---------------------------------------------*
           move      "S"                  to   w-cnt-sts-flg-bgn      .
       exe-fun-stp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Esecuzione routine di stampa                    *
      *              *-------------------------------------------------*
           perform   rou-stp-doc-000      thru rou-stp-doc-999        .
       exe-fun-stp-999.
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
           move      096                  to   w-cnt-stp-amp-lin      .
      *              *-------------------------------------------------*
      *              * Top margin in linee                             *
      *              *-------------------------------------------------*
           move      1                    to   w-cnt-stp-top-lin      .
      *              *-------------------------------------------------*
      *              * Numero linee di stampa minimo                   *
      *              *-------------------------------------------------*
           move      70                   to   w-cnt-stp-lin-min      .
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
      *                  *---------------------------------------------*
      *                  * Normalizzazione preliminare                 *
      *                  *---------------------------------------------*
           move      spaces               to   w-cnt-stp-esp-fut      .
      *                  *---------------------------------------------*
      *                  * Flag destinato al controllo del Tipo Stampa *
      *                  * per inibire la possibilita' della stampa a  *
      *                  * Video                                       *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-stp-esp-fut
                                              (98 : 1)                .
      *              *-------------------------------------------------*
      *              * Area riservata per espansioni speciali          *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-stp-fnz-spc      .
       pre-prm-stp-999.
           exit.

      *    *===========================================================*
      *    * Open files necessari alla stampa                          *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni relative alla stampa  *
      *              * packing list                                    *
      *              *-------------------------------------------------*
           perform   prs-stp-pkl-000      thru prs-stp-pkl-999        .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione per tipo ordinamento  *
      *              * righe corpo                                     *
      *              *-------------------------------------------------*
           perform   prs-tor-rco-000      thru prs-tor-rco-999        .
      *              *-------------------------------------------------*
      *              * [osk]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osk                 .
      *              *-------------------------------------------------*
      *              * [ost]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofost"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ost                 .
      *              *-------------------------------------------------*
      *              * [osr]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *              *-------------------------------------------------*
      *              * [osx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osx                 .
      *              *-------------------------------------------------*
      *              * [zsc]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofzsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsc                 .
      *              *-------------------------------------------------*
      *              * [zsa]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofzsa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsa                 .
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
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [pdx]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
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
      *              * [zfp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
      *              *-------------------------------------------------*
      *              * [zdf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzdf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zdf                 .
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
      *              * [dcf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
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
      *              * Lettura referenze relative alla stampa del      *
      *              * packing list                                    *
      *              *-------------------------------------------------*
           perform   ref-stp-pkl-000      thru ref-stp-pkl-999        .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files necessari alla stampa                         *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [osk]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osk                 .
      *              *-------------------------------------------------*
      *              * [ost]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofost"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ost                 .
      *              *-------------------------------------------------*
      *              * [osr]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *              *-------------------------------------------------*
      *              * [osx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osx                 .
      *              *-------------------------------------------------*
      *              * [zsc]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofzsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsc                 .
      *              *-------------------------------------------------*
      *              * [zsa]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofzsa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsa                 .
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
      *              *-------------------------------------------------*
      *              * [dcp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *              *-------------------------------------------------*
      *              * [pdx]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcp/fls/ioc/obj/iofpdx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-pdx                 .
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
      *              * [zfp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
      *              *-------------------------------------------------*
      *              * [zdf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzdf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zdf                 .
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
      *              * [dcf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
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
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Stampa packing list           *
      *    *-----------------------------------------------------------*
       prs-stp-pkl-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione area                            *
      *              *-------------------------------------------------*
           move      spaces               to   w-prs-spl-str          .
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/ods/pkl/ods500s[stp-pkl]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se personalizzazione non esistente              *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-stp-pkl-050.
      *                  *---------------------------------------------*
      *                  * Normalizzazione al valore di default        *
      *                  *---------------------------------------------*
           move      "P"                  to   w-prs-spl-tin          .
           move      "N"                  to   w-prs-spl-cds          .
           move      "N"                  to   w-prs-spl-rac          .
           move      spaces               to   w-prs-spl-exp          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prs-stp-pkl-900.
       prs-stp-pkl-050.
      *              *-------------------------------------------------*
      *              * Valore letto in work personalizzazioni          *
      *              *-------------------------------------------------*
           move      s-alf                to   w-prs-spl-str          .
      *              *-------------------------------------------------*
      *              * Controllo valori letti                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo intestazione stampato                  *
      *                  *---------------------------------------------*
           if        w-prs-spl-tin        not  = "N" and
                     w-prs-spl-tin        not  = "R" and
                     w-prs-spl-tin        not  = "P"
                     move  "P"            to   w-prs-spl-tin          .
      *                  *---------------------------------------------*
      *                  * Codice da stampare                          *
      *                  *---------------------------------------------*
           if        w-prs-spl-cds        not  = "C" and
                     w-prs-spl-cds        not  = "N"
                     move  "N"            to   w-prs-spl-cds          .
      *                  *---------------------------------------------*
      *                  * Si/No riga aggiuntiva per stampa codice     *
      *                  * prodotto                                    *
      *                  *---------------------------------------------*
           if        w-prs-spl-rac        not  = "N" and
                     w-prs-spl-rac        not  = "C" and
                     w-prs-spl-rac        not  = "M" and
                     w-prs-spl-rac        not  = "E"
                     move  "N"            to   w-prs-spl-rac          .
       prs-stp-pkl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-stp-pkl-999.
       prs-stp-pkl-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle personalizzazioni relative al tipo di ordi- *
      *    * namento per le righe corpo                                *
      *    *-----------------------------------------------------------*
       prs-tor-rco-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione per stampa ordine di  *
      *              * spedizione                                      *
      *              *-------------------------------------------------*
           perform   prs-stp-ods-000      thru prs-stp-ods-999        .
      *              *-------------------------------------------------*
      *              * Memorizzazione personalizzazione                *
      *              *-------------------------------------------------*
           move      w-prs-sos-orr        to   w-prs-tor-rco-tip      .
       prs-tor-rco-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Stampa ordine di spedizione   *
      *    *-----------------------------------------------------------*
       prs-stp-ods-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/ods/mov/ods500s[stp-ods]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Se personalizzazione non esistente              *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to prs-stp-ods-050.
      *                  *---------------------------------------------*
      *                  * Normalizzazione al valore di default        *
      *                  *---------------------------------------------*
           move      51                   to   w-prs-sos-tmd          .
           move      "L"                  to   w-prs-sos-tst          .
           move      "N"                  to   w-prs-sos-tin          .
           move      "N"                  to   w-prs-sos-snv (1)      .
           move      "N"                  to   w-prs-sos-snv (2)      .
           move      "N"                  to   w-prs-sos-snv (3)      .
           move      "N"                  to   w-prs-sos-snv (4)      .
           move      "N"                  to   w-prs-sos-snv (5)      .
           move      "N"                  to   w-prs-sos-snv (6)      .
           move      "N"                  to   w-prs-sos-snc          .
           move      "N"                  to   w-prs-sos-snp          .
           move      "N"                  to   w-prs-sos-sns (1)      .
           move      "N"                  to   w-prs-sos-sns (2)      .
           move      "N"                  to   w-prs-sos-sns (3)      .
           move      "N"                  to   w-prs-sos-sns (4)      .
           move      "N"                  to   w-prs-sos-sns (5)      .
           move      "N"                  to   w-prs-sos-sns (6)      .
           move      "N"                  to   w-prs-sos-snq          .
           move      "N"                  to   w-prs-sos-cds          .
           move      "N"                  to   w-prs-sos-rac          .
           move      11                   to   w-prs-sos-bl1          .
           move      12                   to   w-prs-sos-bl2          .
           move      00                   to   w-prs-sos-orr          .
           move      spaces               to   w-prs-sos-exp          .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     prs-stp-ods-900.
       prs-stp-ods-050.
      *              *-------------------------------------------------*
      *              * Valore letto in work personalizzazioni          *
      *              *-------------------------------------------------*
           move      s-alf                to   w-prs-sos-str          .
      *              *-------------------------------------------------*
      *              * Controllo valori letti                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Tipo modulo                                 *
      *                  *---------------------------------------------*
           if        w-prs-sos-tmd        not  numeric
                     move  01             to   w-prs-sos-tmd
                     go to prs-stp-ods-100.
           if        w-prs-sos-tmd        not  = 01 and
                     w-prs-sos-tmd        not  = 51
                     move  01             to   w-prs-sos-tmd          .
       prs-stp-ods-100.
      *                  *---------------------------------------------*
      *                  * Tipo stampato                               *
      *                  *---------------------------------------------*
           if        w-prs-sos-tst        not  = "T" and
                     w-prs-sos-tst        not  = "L"
                     move  "L"            to   w-prs-sos-tst          .
      *                  *---------------------------------------------*
      *                  * Tipo intestazione stampato                  *
      *                  *---------------------------------------------*
           if        w-prs-sos-tin        not  = "N" and
                     w-prs-sos-tin        not  = "R" and
                     w-prs-sos-tin        not  = "P"
                     move  "P"            to   w-prs-sos-tin          .
           if        w-prs-sos-tst        =    "T"
                     move  "N"            to   w-prs-sos-tin          .
      *                  *---------------------------------------------*
      *                  * Si/no stampa voci descrittive               *
      *                  *---------------------------------------------*
           move      zero                 to   w-prs-sos-ctr          .
       prs-stp-ods-110.
           add       1                    to   w-prs-sos-ctr          .
           if        w-prs-sos-ctr        >    6
                     go to prs-stp-ods-120.
           if        w-prs-sos-snv
                    (w-prs-sos-ctr)       not  = "S" and
                     w-prs-sos-snv
                    (w-prs-sos-ctr)       not  = "N"
                     move  "S"            to   w-prs-sos-snv
                                              (w-prs-sos-ctr)         .
           go to     prs-stp-ods-110.
       prs-stp-ods-120.
      *                  *---------------------------------------------*
      *                  * Si/no stampa sconto in chiusura             *
      *                  *---------------------------------------------*
           if        w-prs-sos-snc        not  = "S" and
                     w-prs-sos-snc        not  = "N"
                     move  "S"            to   w-prs-sos-snc          .
       prs-stp-ods-130.
      *                  *---------------------------------------------*
      *                  * Si/no stampa sconto pagamento               *
      *                  *---------------------------------------------*
           if        w-prs-sos-snp        not  = "S" and
                     w-prs-sos-snp        not  = "N"
                     move  "S"            to   w-prs-sos-snp          .
       prs-stp-ods-140.
      *                  *---------------------------------------------*
      *                  * Si/no stampa spese in fattura               *
      *                  *---------------------------------------------*
           move      zero                 to   w-prs-sos-ctr          .
       prs-stp-ods-142.
           add       1                    to   w-prs-sos-ctr          .
           if        w-prs-sos-ctr        >    6
                     go to prs-stp-ods-200.
           if        w-prs-sos-sns
                    (w-prs-sos-ctr)       not  = "S" and
                     w-prs-sos-sns
                    (w-prs-sos-ctr)       not  = "N"
                     move  "S"            to   w-prs-sos-sns
                                              (w-prs-sos-ctr)         .
           go to     prs-stp-ods-142.
       prs-stp-ods-200.
      *                  *---------------------------------------------*
      *                  * Si/no stampa delle quantita' editate a de-  *
      *                  * stra                                        *
      *                  *---------------------------------------------*
           if        w-prs-sos-snq        not  = "S" and
                     w-prs-sos-snq        not  = "N"
                     move  "N"            to   w-prs-sos-snq          .
       prs-stp-ods-600.
      *                  *---------------------------------------------*
      *                  * Codice da stampare                          *
      *                  *---------------------------------------------*
           if        w-prs-sos-cds        not  = "C" and
                     w-prs-sos-cds        not  = "N"
                     move  "N"            to   w-prs-sos-cds          .
       prs-stp-ods-650.
      *                  *---------------------------------------------*
      *                  * Si/No riga aggiuntiva per stampa codice     *
      *                  * prodotto                                    *
      *                  *---------------------------------------------*
           if        w-prs-sos-rac        not  = "N" and
                     w-prs-sos-rac        not  = "C" and
                     w-prs-sos-rac        not  = "M" and
                     w-prs-sos-rac        not  = "E"
                     move  "N"            to   w-prs-sos-rac          .
       prs-stp-ods-700.
      *                  *---------------------------------------------*
      *                  * Tipo di visualizzazione nei box liberi :    *
      *                  * forzatura                                   *
      *                  *---------------------------------------------*
           move      11                   to   w-prs-sos-bl1          .
           move      12                   to   w-prs-sos-bl2          .
       prs-stp-ods-750.
      *                  *---------------------------------------------*
      *                  * Tipo di ordinamento righe corpo             *
      *                  *---------------------------------------------*
           if        w-prs-sos-orr        not  = 00 and
                     w-prs-sos-orr        not  = 01 and
                     w-prs-sos-orr        not  = 02 and
                     w-prs-sos-orr        not  = 03 and
                     w-prs-sos-orr        not  = 04 and
                     w-prs-sos-orr        not  = 10 and
                     w-prs-sos-orr        not  = 13 and
                     w-prs-sos-orr        not  = 14
                     move  00             to   w-prs-sos-orr          .
       prs-stp-ods-800.
      *                  *---------------------------------------------*
      *                  * Area libera per implementazioni future      *
      *                  *---------------------------------------------*
           if        w-prs-sos-exp        not  = spaces
                     move  spaces         to   w-prs-sos-exp          .
       prs-stp-ods-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-stp-ods-999.
       prs-stp-ods-999.
           exit.

      *    *===========================================================*
      *    * Lettura referenze per stampa packing list                 *
      *    *-----------------------------------------------------------*
       ref-stp-pkl-000.
      *              *-------------------------------------------------*
      *              * Referenza per intestazione documento            *
      *              *-------------------------------------------------*
       ref-stp-pkl-025.
      *                  *---------------------------------------------*
      *                  * Normalizzazione area                        *
      *                  *---------------------------------------------*
           move      spaces               to   w-ref-stp-int-ade      .
       ref-stp-pkl-050.
      *                  *---------------------------------------------*
      *                  * Deviazione secondo la personalizzazione     *
      *                  *---------------------------------------------*
            if       w-prs-spl-tin        =    "N"
                     go to ref-stp-pkl-200
            else if  w-prs-spl-tin        =    "R"
                     go to ref-stp-pkl-150.
       ref-stp-pkl-100.
      *                  *---------------------------------------------*
      *                  * Se 'P': intestazione da programma           *
      *                  *---------------------------------------------*
       ref-stp-pkl-105.
      *                      *-----------------------------------------*
      *                      * Lettura record azienda                  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Normalizzazione [ada]               *
      *                          *-------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                          *-------------------------------------*
      *                          * Lettura [ada]                       *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODDPZ    "         to   f-key                  .
           move      zero                 to   rf-ada-cod-dpz         .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
       ref-stp-pkl-110.
      *                      *-----------------------------------------*
      *                      * Preparazione delle 8 linee da stampare  *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * 1. riga: nome azienda da segreteria *
      *                          *-------------------------------------*
           move      "IA"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *
           move      s-asx                to   w-ref-stp-int-des (1)  .
      *                          *-------------------------------------*
      *                          * 3. riga : indirizzo azienda         *
      *                          *-------------------------------------*
           move      rf-ada-via-azi       to   w-ref-stp-int-des (3)  .
      *                          *-------------------------------------*
      *                          * 5. riga : localita' azienda         *
      *                          *-------------------------------------*
           move      rf-ada-loc-azi       to   w-ref-stp-int-des (5)  .
      *                          *-------------------------------------*
      *                          * 8. riga : partita iva azienda       *
      *                          *-------------------------------------*
           if        rf-ada-prt-iva       not  = zero
                     string "Partita iva : "
                                delimited by   size
                            rf-ada-prt-iva
                                delimited by   size
                                          into w-ref-stp-int-des (8)  .
       ref-stp-pkl-115.
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     ref-stp-pkl-900.
       ref-stp-pkl-150.
      *                  *---------------------------------------------*
      *                  * Se 'R': intestazione da referenza           *
      *                  *---------------------------------------------*
       ref-stp-pkl-160.
      *                      *-----------------------------------------*
      *                      * Lettura referenza multipla              *
      *                      *-----------------------------------------*
           move      zero                 to   w-ref-stp-int-ctr      .
       ref-stp-pkl-170.
           add       1                    to   w-ref-stp-int-ctr      .
           if        w-ref-stp-int-ctr    >    8
                     go to ref-stp-pkl-180.
           move      "R:"                 to   s-ope                  .
           move      w-ref-stp-int-ctr    to   s-num                  .
           move      "pgm/ods/pkl/ods500s[int-pkl]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-ref-stp-int-des
                                              (w-ref-stp-int-ctr)
           else      move  spaces         to   w-ref-stp-int-des
                                              (w-ref-stp-int-ctr)     .
           go to     ref-stp-pkl-170.
       ref-stp-pkl-180.
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     ref-stp-pkl-900.
       ref-stp-pkl-200.
      *                  *---------------------------------------------*
      *                  * Se 'N': nessuna intestazione                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ad uscita                               *
      *                      *-----------------------------------------*
           go to     ref-stp-pkl-900.
       ref-stp-pkl-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     ref-stp-pkl-999.
       ref-stp-pkl-999.
           exit.

      *    *===========================================================*
      *    * Stampa documento                                          *
      *    *-----------------------------------------------------------*
       rou-stp-doc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-prn-flg-int      .
      *              *-------------------------------------------------*
      *              * Normalizzazione displacement verticale          *
      *              *-------------------------------------------------*
           move      zero                 to   w-dsp-dsp-vrt          .
      *              *-------------------------------------------------*
      *              * Inizializzazione numero pagina                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-tes-num-pag      .
      *              *-------------------------------------------------*
      *              * Inizializzazione di:                            *
      *              *  - Total No. of Packings                        *
      *              *  - Total Net Weight                             *
      *              *  - Total Gross Weight                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-pie-tno-pkg      .
           move      zero                 to   w-stp-pie-tot-ntw      .
           move      zero                 to   w-stp-pie-tot-gsw      .
       rou-stp-doc-020.
      *              *-------------------------------------------------*
      *              * Lettura record ordine di spedizione relativo al *
      *              * packing list da stampare                        *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-inp-mst-prt        to   rf-ost-num-prt         .
           move      "pgm/ods/fls/ioc/obj/iofost"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ost                 .
      *              *-------------------------------------------------*
      *              * Se record trovato : oltre                       *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rou-stp-doc-040.
      *              *-------------------------------------------------*
      *              * Se record non trovato uscita con status al va-  *
      *              * lore "N"                                        *
      *              *-------------------------------------------------*
           move      spaces               to   w-out-mst              .
           move      "N"                  to   w-out-mst-stp          .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-999.
       rou-stp-doc-040.
      *              *-------------------------------------------------*
      *              * Lettura record dati aggiuntivi di testata       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [osx]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osx                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [osx]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-ost-num-prt       to   rf-osx-num-prt         .
           move      zero                 to   rf-osx-num-prg         .
           move      01                   to   rf-osx-tip-rec         .
           move      "pgm/ods/fls/ioc/obj/iofosx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osx                 .
       rou-stp-doc-200.
      *              *-------------------------------------------------*
      *              * Stampa testata documento                        *
      *              *-------------------------------------------------*
           perform   stp-tes-doc-000      thru stp-tes-doc-999        .
      *              *-------------------------------------------------*
      *              * Test su status di uscita                        *
      *              *-------------------------------------------------*
           if        w-out-mst-stp        =    spaces
                     go to rou-stp-doc-300.
           move      spaces               to   w-out-mst              .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     rou-stp-doc-999.
       rou-stp-doc-300.
      *              *-------------------------------------------------*
      *              * Stampa corpo documento                          *
      *              *-------------------------------------------------*
           perform   stp-cor-doc-000      thru stp-cor-doc-999        .
      *              *-------------------------------------------------*
      *              * Test su status di uscita                        *
      *              *-------------------------------------------------*
           if        w-out-mst-stp        =    spaces
                     go to rou-stp-doc-400.
           move      spaces               to   w-out-mst              .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     rou-stp-doc-999.
       rou-stp-doc-400.
      *              *-------------------------------------------------*
      *              * Stampa piede documento                          *
      *              *-------------------------------------------------*
           perform   stp-pie-doc-000      thru stp-pie-doc-999        .
      *              *-------------------------------------------------*
      *              * Test su status di uscita                        *
      *              *-------------------------------------------------*
           if        w-out-mst-stp        =    spaces
                     go to rou-stp-doc-900.
           move      spaces               to   w-out-mst              .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     rou-stp-doc-999.
       rou-stp-doc-900.
      *              *-------------------------------------------------*
      *              * Page eject                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da eseguire                         *
      *                  *---------------------------------------------*
           if        w-inp-mst-ejc        =    "N"
                     go to rou-stp-doc-950.
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           move      "EJ"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-doc-950.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-doc-999.
       rou-stp-doc-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa testata documento                       *
      *    *-----------------------------------------------------------*
       stp-tes-doc-000.
      *              *-------------------------------------------------*
      *              * Raccolta dati intestatario documento            *
      *              *-------------------------------------------------*
           perform   stp-tes-doc-rdi-000  thru stp-tes-doc-rdi-999    .
       stp-tes-doc-100.
      *              *-------------------------------------------------*
      *              * Intestazione pagina                             *
      *              *   - Page advance                                *
      *              *   - Prefincature                                *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *              *-------------------------------------------------*
      *              * Se errore grave di i-o su stampa si esce con    *
      *              * status di errore                                *
      *              *-------------------------------------------------*
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  stp-tes-doc-999.
       stp-tes-doc-200.
      *              *-------------------------------------------------*
      *              * Stampe relative alla testata documento          *
      *              *-------------------------------------------------*
           perform   stp-pag-sta-000      thru stp-pag-sta-999        .
       stp-tes-doc-800.
      *              *-------------------------------------------------*
      *              * Vertical position per le righe corpo            *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      26                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-tes-doc-999.
       stp-tes-doc-999.
           exit.

      *    *===========================================================*
      *    * Intestazione pagina                                       *
      *    *   - Page advance                                          *
      *    *   - Prefincature                                          *
      *    *-----------------------------------------------------------*
       int-pag-sta-000.
      *              *-------------------------------------------------*
      *              * Incremento numero pagina                        *
      *              *-------------------------------------------------*
           add       1                    to   w-stp-tes-num-pag      .
      *              *-------------------------------------------------*
      *              * Avanzamento pagina                              *
      *              *-------------------------------------------------*
           move      "PA"                 to   p-ope                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *              *-------------------------------------------------*
      *              * Se errore grave di i-o su stampa si esce        *
      *              * con status di errore                            *
      *              *-------------------------------------------------*
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  int-pag-sta-999.
       int-pag-sta-100.
      *              *-------------------------------------------------*
      *              * Prefincatura                                    *
      *              *-------------------------------------------------*
       int-pag-sta-110.
      *                  *---------------------------------------------*
      *                  * Intestazione azienda                        *
      *                  *---------------------------------------------*
           if        w-ref-stp-int-ade    =    spaces
                     go to int-pag-sta-120.
           move      zero                 to   w-ref-stp-int-ctr      .
       int-pag-sta-112.
           add       1                    to   w-ref-stp-int-ctr      .
           if        w-ref-stp-int-ctr    >    8
                     go to int-pag-sta-120.
           if        w-ref-stp-int-des
                    (w-ref-stp-int-ctr)   =    spaces
                     go to int-pag-sta-112.
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      50                   to   p-car                  .
           move      03                   to   p-lin                  .
           add       w-ref-stp-int-ctr    to   p-lin                  .
           subtract  1                    from p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      004                  to   p-pos                  .
           move      w-ref-stp-int-des
                    (w-ref-stp-int-ctr)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           go to     int-pag-sta-112.
       int-pag-sta-120.
      *                  *---------------------------------------------*
      *                  * Titolo documento                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      33                   to   p-car                  .
           move      03                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      064                  to   p-pos                  .
           move      "================================="
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      33                   to   p-car                  .
           move      04                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      064                  to   p-pos                  .
           move      "          PACKING LIST           "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      33                   to   p-car                  .
           move      05                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      064                  to   p-pos                  .
           move      "================================="
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-130.
      *                  *---------------------------------------------*
      *                  * Box per data e pagina                       *
      *                  *---------------------------------------------*
           move      007                  to   w-rou-stp-box-lin      .
           move      064                  to   w-rou-stp-box-pin      .
           move      009                  to   w-rou-stp-box-lfi      .
           move      096                  to   w-rou-stp-box-pfi      .
           perform   rou-stp-box-000      thru rou-stp-box-999        .
       int-pag-sta-140.
      *                  *---------------------------------------------*
      *                  * Box per cliente cui fatturare               *
      *                  *---------------------------------------------*
           move      014                  to   w-rou-stp-box-lin      .
           move      003                  to   w-rou-stp-box-pin      .
           move      021                  to   w-rou-stp-box-lfi      .
           move      046                  to   w-rou-stp-box-pfi      .
           perform   rou-stp-box-000      thru rou-stp-box-999        .
      *                  *---------------------------------------------*
      *                  * Literal per box per cliente cui fatturare   *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      14                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      005                  to   p-pos                  .
           move      "CUSTOMER NAME"      to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-150.
      *                  *---------------------------------------------*
      *                  * Box per destinazione merce                  *
      *                  *---------------------------------------------*
           move      014                  to   w-rou-stp-box-lin      .
           move      053                  to   w-rou-stp-box-pin      .
           move      021                  to   w-rou-stp-box-lfi      .
           move      096                  to   w-rou-stp-box-pfi      .
           perform   rou-stp-box-000      thru rou-stp-box-999        .
      *                  *---------------------------------------------*
      *                  * Literal per box per destinazione merce      *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      14                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      055                  to   p-pos                  .
           move      "DESTINATION"        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-200.
      *                  *---------------------------------------------*
      *                  * Prompts corpo documento                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice prodotto                         *
      *                      *-----------------------------------------*
           move      023                  to   w-rou-stp-box-lin      .
           move      003                  to   w-rou-stp-box-pin      .
           move      025                  to   w-rou-stp-box-lfi      .
           move      018                  to   w-rou-stp-box-pfi      .
           perform   rou-stp-box-000      thru rou-stp-box-999        .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      14                   to   p-car                  .
           move      24                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      004                  to   p-pos                  .
           move      "  Part Number "     to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      023                  to   w-rou-stp-box-lin      .
           move      018                  to   w-rou-stp-box-pin      .
           move      025                  to   w-rou-stp-box-lfi      .
           move      061                  to   w-rou-stp-box-pfi      .
           perform   rou-stp-box-000      thru rou-stp-box-999        .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      24                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      020                  to   p-pos                  .
           move      "              Description               "
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Quantita'                               *
      *                      *-----------------------------------------*
           move      023                  to   w-rou-stp-box-lin      .
           move      061                  to   w-rou-stp-box-pin      .
           move      025                  to   w-rou-stp-box-lfi      .
           move      073                  to   w-rou-stp-box-pfi      .
           perform   rou-stp-box-000      thru rou-stp-box-999        .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      24                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      062                  to   p-pos                  .
           move      " Quantity  "        to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Colonna libera                          *
      *                      *-----------------------------------------*
           move      023                  to   w-rou-stp-box-lin      .
           move      073                  to   w-rou-stp-box-pin      .
           move      025                  to   w-rou-stp-box-lfi      .
           move      096                  to   w-rou-stp-box-pfi      .
           perform   rou-stp-box-000      thru rou-stp-box-999        .
       int-pag-sta-250.
      *                  *---------------------------------------------*
      *                  * Colonne corpo documento                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice prodotto                         *
      *                      *-----------------------------------------*
           move      025                  to   w-rou-stp-box-lin      .
           move      003                  to   w-rou-stp-box-pin      .
           move      063                  to   w-rou-stp-box-lfi      .
           move      018                  to   w-rou-stp-box-pfi      .
           perform   rou-stp-box-000      thru rou-stp-box-999        .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           move      025                  to   w-rou-stp-box-lin      .
           move      018                  to   w-rou-stp-box-pin      .
           move      063                  to   w-rou-stp-box-lfi      .
           move      061                  to   w-rou-stp-box-pfi      .
           perform   rou-stp-box-000      thru rou-stp-box-999        .
      *                      *-----------------------------------------*
      *                      * Quantita'                               *
      *                      *-----------------------------------------*
           move      025                  to   w-rou-stp-box-lin      .
           move      061                  to   w-rou-stp-box-pin      .
           move      063                  to   w-rou-stp-box-lfi      .
           move      073                  to   w-rou-stp-box-pfi      .
           perform   rou-stp-box-000      thru rou-stp-box-999        .
      *                      *-----------------------------------------*
      *                      * Colonna libera                          *
      *                      *-----------------------------------------*
           move      025                  to   w-rou-stp-box-lin      .
           move      073                  to   w-rou-stp-box-pin      .
           move      063                  to   w-rou-stp-box-lfi      .
           move      096                  to   w-rou-stp-box-pfi      .
           perform   rou-stp-box-000      thru rou-stp-box-999        .
       int-pag-sta-300.
      *                  *---------------------------------------------*
      *                  * Box di sinistra                             *
      *                  *---------------------------------------------*
           move      064                  to   w-rou-stp-box-lin      .
           move      003                  to   w-rou-stp-box-pin      .
           move      070                  to   w-rou-stp-box-lfi      .
           move      061                  to   w-rou-stp-box-pfi      .
           perform   rou-stp-box-000      thru rou-stp-box-999        .
       int-pag-sta-310.
      *                  *---------------------------------------------*
      *                  * Box di destra                               *
      *                  *---------------------------------------------*
           move      064                  to   w-rou-stp-box-lin      .
           move      061                  to   w-rou-stp-box-pin      .
           move      070                  to   w-rou-stp-box-lfi      .
           move      096                  to   w-rou-stp-box-pfi      .
           perform   rou-stp-box-000      thru rou-stp-box-999        .
       int-pag-sta-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     int-pag-sta-999.
       int-pag-sta-999.
           exit.

      *    *===========================================================*
      *    * Stampe relative alla testata documento                    *
      *    *-----------------------------------------------------------*
       stp-pag-sta-000.
      *              *-------------------------------------------------*
      *              * Data e pagina                                   *
      *              *-------------------------------------------------*
       stp-pag-sta-020.
      *                  *---------------------------------------------*
      *                  * Data, quella dell'ordine di spedizione      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      08                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      066                  to   p-pos                  .
           move      "Date: "             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      rf-ost-dat-doc       to   w-stp-tes-dds-dat      .
      *
           move      spaces               to   w-stp-tes-dds-edt      .
           move      w-stp-tes-dds-gio    to   w-stp-tes-dds-egg      .
           if        w-stp-tes-dds-mes    =    01
                     move  "JAN"          to   w-stp-tes-dds-emm
           else if   w-stp-tes-dds-mes    =    02
                     move  "FEB"          to   w-stp-tes-dds-emm
           else if   w-stp-tes-dds-mes    =    03
                     move  "MAR"          to   w-stp-tes-dds-emm
           else if   w-stp-tes-dds-mes    =    04
                     move  "APR"          to   w-stp-tes-dds-emm
           else if   w-stp-tes-dds-mes    =    05
                     move  "MAY"          to   w-stp-tes-dds-emm
           else if   w-stp-tes-dds-mes    =    06
                     move  "JUN"          to   w-stp-tes-dds-emm
           else if   w-stp-tes-dds-mes    =    07
                     move  "JUL"          to   w-stp-tes-dds-emm
           else if   w-stp-tes-dds-mes    =    08
                     move  "AUG"          to   w-stp-tes-dds-emm
           else if   w-stp-tes-dds-mes    =    09
                     move  "SEP"          to   w-stp-tes-dds-emm
           else if   w-stp-tes-dds-mes    =    10
                     move  "OCT"          to   w-stp-tes-dds-emm
           else if   w-stp-tes-dds-mes    =    11
                     move  "NOV"          to   w-stp-tes-dds-emm
           else if   w-stp-tes-dds-mes    =    12
                     move  "DEC"          to   w-stp-tes-dds-emm      .
           move      w-stp-tes-dds-saa    to   w-stp-tes-dds-eaa      .
           add       1900                 to   w-stp-tes-dds-eaa      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      11                   to   p-car                  .
           move      08                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      072                  to   p-pos                  .
           move      w-stp-tes-dds-edt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-pag-sta-040.
      *                  *---------------------------------------------*
      *                  * Pagina                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      08                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      087                  to   p-pos                  .
           move      "Page: "             to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      02                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      08                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      093                  to   p-pos                  .
           move      w-stp-tes-num-pag    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-pag-sta-200.
      *              *-------------------------------------------------*
      *              * Intestatario documento                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ragione sociale 1                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      16                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      005                  to   p-pos                  .
           move      w-stp-tes-int-rag    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Ragione sociale 2                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      17                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      005                  to   p-pos                  .
           move      w-stp-tes-int-rs2    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Indirizzo                                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      18                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      005                  to   p-pos                  .
           move      w-stp-tes-int-via    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Localita'                                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      20                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      005                  to   p-pos                  .
           move      w-stp-tes-int-loc    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-pag-sta-300.
      *              *-------------------------------------------------*
      *              * Indirizzo per la spedizione                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ragione sociale 1                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      16                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      055                  to   p-pos                  .
           move      w-stp-tes-ids-rag    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Ragione sociale 2                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      17                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      055                  to   p-pos                  .
           move      w-stp-tes-ids-rs2    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Indirizzo                                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      18                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      055                  to   p-pos                  .
           move      w-stp-tes-ids-via    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Localita'                                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      20                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      055                  to   p-pos                  .
           move      w-stp-tes-ids-loc    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-pag-sta-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa testata documento                       *
      *    *                                                           *
      *    * Subroutine di raccolta dati per l'intestatario del docu-  *
      *    * mento                                                     *
      *    *-----------------------------------------------------------*
       stp-tes-doc-rdi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare dell'area di rac-   *
      *              * colta dati per l'intestatario del documento     *
      *              *-------------------------------------------------*
           move      spaces               to   w-stp-tes-int          .
       stp-tes-doc-rdi-010.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo archivio        *
      *              *                                                 *
      *              * - N : No tipo archivio                          *
      *              * - C : Cliente                                   *
      *              * - F : Fornitore                                 *
      *              * - D : Deposito                                  *
      *              *-------------------------------------------------*
           if        rf-ost-tip-arc       =    "N"
                     go to stp-tes-doc-rdi-100
           else if   rf-ost-tip-arc       =    "C"
                     go to stp-tes-doc-rdi-200
           else if   rf-ost-tip-arc       =    "F"
                     go to stp-tes-doc-rdi-300
           else if   rf-ost-tip-arc       =    "D"
                     go to stp-tes-doc-rdi-400
           else      go to stp-tes-doc-rdi-800.
       stp-tes-doc-rdi-100.
      *              *-------------------------------------------------*
      *              * No tipo archivio                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     stp-tes-doc-rdi-900.
       stp-tes-doc-rdi-200.
      *              *-------------------------------------------------*
      *              * Cliente                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine di raccolta dati                 *
      *                  *---------------------------------------------*
           perform   stp-tes-doc-cli-000  thru stp-tes-doc-cli-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     stp-tes-doc-rdi-900.
       stp-tes-doc-rdi-300.
      *              *-------------------------------------------------*
      *              * Fornitore                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine di raccolta dati                 *
      *                  *---------------------------------------------*
           perform   stp-tes-doc-fnt-000  thru stp-tes-doc-fnt-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     stp-tes-doc-rdi-900.
       stp-tes-doc-rdi-400.
      *              *-------------------------------------------------*
      *              * Dipendenza                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Subroutine di raccolta dati                 *
      *                  *---------------------------------------------*
           perform   stp-tes-doc-dpz-000  thru stp-tes-doc-dpz-999    .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     stp-tes-doc-rdi-900.
       stp-tes-doc-rdi-800.
      *              *-------------------------------------------------*
      *              * Tipo archivio non riconosciuto                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnalazione del tipo archivio non ricono-  *
      *                  * sciuto nel prompt dell'intestazione del     *
      *                  * documento                                   *
      *                  *---------------------------------------------*
           move      "(Tipo archivio non riconosciuto)        "
                                          to   w-stp-tes-int-rag      .
           move      "(Tipo archivio non riconosciuto)        "
                                          to   w-stp-tes-ids-rag      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     stp-tes-doc-rdi-900.
       stp-tes-doc-rdi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-tes-doc-rdi-999.
       stp-tes-doc-rdi-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa testata documento                       *
      *    *                                                           *
      *    * Subroutine di raccolta dati per l'intestatario del docu-  *
      *    * mento                                                     *
      *    *                                                           *
      *    * Cliente                                                   *
      *    *-----------------------------------------------------------*
       stp-tes-doc-cli-000.
      *              *-------------------------------------------------*
      *              * Letture preliminari                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [cli]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [cli]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      rf-ost-cod-arc       to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                      *-----------------------------------------*
      *                      * Se lettura record [cli] con esito nega- *
      *                      * tivo : ragione sociale con puntini      *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-cli-rag-soc         .
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
           move      rf-ost-cod-arc       to   rf-dcc-cod-cli         .
           move      rf-ost-dpz-arc       to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Se lettura record [dcc] con esito nega- *
      *                      * tivo : ragione sociale con puntini      *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcc-rag-soc         .
       stp-tes-doc-cli-100.
      *              *-------------------------------------------------*
      *              * Preparazioni                                    *
      *              *-------------------------------------------------*
       stp-tes-doc-cli-110.
      *                  *---------------------------------------------*
      *                  * Intestatario documento                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      rf-dcc-rs1-doc       to   w-stp-tes-int-rag      .
           move      rf-dcc-rs2-doc       to   w-stp-tes-int-rs2      .
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
           move      rf-dcc-via-dcc       to   w-stp-tes-int-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      rf-dcc-loc-dcc       to   w-stp-tes-int-loc      .
       stp-tes-doc-cli-150.
      *                  *---------------------------------------------*
      *                  * Indirizzo di spedizione                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo indi-   *
      *                      * rizzo di spedizione                     *
      *                      *                                         *
      *                      * - 01 : alla sede legale                 *
      *                      * - 02 : manuale                          *
      *                      * - 03 : alla sede                        *
      *                      * - 04 : alla dipendenza                  *
      *                      *-----------------------------------------*
           if        rf-ost-tip-ids       =    01
                     go to stp-tes-doc-cli-160
           else if   rf-ost-tip-ids       =    02
                     go to stp-tes-doc-cli-170
           else if   rf-ost-tip-ids       =    03
                     go to stp-tes-doc-cli-180
           else if   rf-ost-tip-ids       =    04
                     go to stp-tes-doc-cli-190
           else      go to stp-tes-doc-cli-200.
       stp-tes-doc-cli-160.
      *                      *-----------------------------------------*
      *                      * Alla sede legale                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      rf-cli-rag-soc       to   w-stp-tes-ids-rag      .
           move      spaces               to   w-stp-tes-ids-rs2      .
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
           move      rf-cli-via-cli       to   w-stp-tes-ids-via      .
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           move      rf-cli-loc-cli       to   w-stp-tes-ids-loc      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-tes-doc-cli-200.
       stp-tes-doc-cli-170.
      *                      *-----------------------------------------*
      *                      * Manuale                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      rf-osx-rag-ids       to   w-stp-tes-ids-rag      .
           move      rf-osx-rs2-ids       to   w-stp-tes-ids-rs2      .
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
           move      rf-osx-via-ids       to   w-stp-tes-ids-via      .
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           move      rf-osx-loc-ids       to   w-stp-tes-ids-loc      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-tes-doc-cli-200.
       stp-tes-doc-cli-180.
      *                      *-----------------------------------------*
      *                      * Alla sede                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il codice dipendenza archivio e' *
      *                          * a spaces : ragione sociale da [dcc] *
      *                          * diretta                             *
      *                          *-------------------------------------*
           if        rf-ost-dpz-arc       not  = spaces
                     go to stp-tes-doc-cli-185.
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      rf-dcc-rs1-doc       to   w-stp-tes-ids-rag      .
           move      rf-dcc-rs2-doc       to   w-stp-tes-ids-rs2      .
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
           move      rf-dcc-via-dcc       to   w-stp-tes-ids-via      .
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           move      rf-dcc-loc-dcc       to   w-stp-tes-ids-loc      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-tes-doc-cli-200.
       stp-tes-doc-cli-185.
      *                          *-------------------------------------*
      *                          * Lettura anagrafica sede in caso di  *
      *                          * documento intestato a dipendenza    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Salvataggio record [dcc] in     *
      *                              * corso di trattamento            *
      *                              *---------------------------------*
           move      rf-dcc               to   w-sav-rec-dcc          .
      *                              *---------------------------------*
      *                              * Normalizzazione record [dcc]    *
      *                              *---------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                              *---------------------------------*
      *                              * Lettura record [dcc] per la se- *
      *                              * de                              *
      *                              *---------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      rf-ost-cod-arc       to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                                  *-----------------------------*
      *                                  * Se record non trovato :     *
      *                                  * ragione sociale con puntini *
      *                                  *-----------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcc-rag-soc         .
      *                              *---------------------------------*
      *                              * Ragione sociale                 *
      *                              *---------------------------------*
           move      rf-dcc-rs1-doc       to   w-stp-tes-ids-rag      .
           move      rf-dcc-rs2-doc       to   w-stp-tes-ids-rs2      .
      *                              *---------------------------------*
      *                              * Indirizzo                       *
      *                              *---------------------------------*
           move      rf-dcc-via-dcc       to   w-stp-tes-ids-via      .
      *                              *---------------------------------*
      *                              * Localita'                       *
      *                              *---------------------------------*
           move      rf-dcc-loc-dcc       to   w-stp-tes-ids-loc      .
      *                              *---------------------------------*
      *                              * Ripristino record [dcc] in      *
      *                              * corso di trattamento            *
      *                              *---------------------------------*
           move      w-sav-rec-dcc        to   rf-dcc                 .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-tes-doc-cli-200.
       stp-tes-doc-cli-190.
      *                      *-----------------------------------------*
      *                      * Alla dipendenza                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      rf-dcc-rs1-doc       to   w-stp-tes-ids-rag      .
           move      rf-dcc-rs2-doc       to   w-stp-tes-ids-rs2      .
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
           move      rf-dcc-via-dcc       to   w-stp-tes-ids-via      .
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           move      rf-dcc-loc-dcc       to   w-stp-tes-ids-loc      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-tes-doc-cli-200.
       stp-tes-doc-cli-200.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-tes-doc-cli-999.
       stp-tes-doc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa testata documento                       *
      *    *                                                           *
      *    * Subroutine di raccolta dati per l'intestatario del docu-  *
      *    * mento                                                     *
      *    *                                                           *
      *    * Fornitore                                                 *
      *    *-----------------------------------------------------------*
       stp-tes-doc-fnt-000.
      *              *-------------------------------------------------*
      *              * Letture preliminari                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [fnt]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [fnt]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
           move      rf-ost-cod-arc       to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
      *                      *-----------------------------------------*
      *                      * Se lettura record [fnt] con esito nega- *
      *                      * tivo : ragione sociale con puntini      *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-fnt-rag-soc         .
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [dcf]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                      *-----------------------------------------*
      *                      * Lettura record [dcf]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
           move      rf-ost-cod-arc       to   rf-dcf-cod-fnt         .
           move      rf-ost-dpz-arc       to   rf-dcf-dpz-fnt         .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                      *-----------------------------------------*
      *                      * Se lettura record [dcf] con esito nega- *
      *                      * tivo : ragione sociale con puntini      *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcf-rag-soc         .
       stp-tes-doc-fnt-100.
      *              *-------------------------------------------------*
      *              * Preparazioni                                    *
      *              *-------------------------------------------------*
       stp-tes-doc-fnt-110.
      *                  *---------------------------------------------*
      *                  * Intestatario documento                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      rf-dcf-rs1-doc       to   w-stp-tes-int-rag      .
           move      rf-dcf-rs2-doc       to   w-stp-tes-int-rs2      .
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
           move      rf-dcf-via-dcf       to   w-stp-tes-int-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      rf-dcf-loc-dcf       to   w-stp-tes-int-loc      .
       stp-tes-doc-fnt-150.
      *                  *---------------------------------------------*
      *                  * Indirizzo di spedizione                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione in funzione del tipo indi-   *
      *                      * rizzo di spedizione                     *
      *                      *                                         *
      *                      * - 01 : alla sede legale                 *
      *                      * - 02 : manuale                          *
      *                      * - 03 : alla sede                        *
      *                      * - 04 : alla dipendenza                  *
      *                      *-----------------------------------------*
           if        rf-ost-tip-ids       =    01
                     go to stp-tes-doc-fnt-160
           else if   rf-ost-tip-ids       =    02
                     go to stp-tes-doc-fnt-170
           else if   rf-ost-tip-ids       =    03
                     go to stp-tes-doc-fnt-180
           else if   rf-ost-tip-ids       =    04
                     go to stp-tes-doc-fnt-190
           else      go to stp-tes-doc-fnt-900.
       stp-tes-doc-fnt-160.
      *                      *-----------------------------------------*
      *                      * Alla sede legale                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      rf-fnt-rag-soc       to   w-stp-tes-ids-rag      .
           move      spaces               to   w-stp-tes-ids-rs2      .
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
           move      rf-fnt-via-fnt       to   w-stp-tes-ids-via      .
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           move      rf-fnt-loc-fnt       to   w-stp-tes-ids-loc      .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     stp-tes-doc-fnt-900.
       stp-tes-doc-fnt-170.
      *                      *-----------------------------------------*
      *                      * Manuale                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      rf-osx-rag-ids       to   w-stp-tes-ids-rag      .
           move      rf-osx-rs2-ids       to   w-stp-tes-ids-rs2      .
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
           move      rf-osx-via-ids       to   w-stp-tes-ids-via      .
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           move      rf-osx-loc-ids       to   w-stp-tes-ids-loc      .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     stp-tes-doc-fnt-900.
       stp-tes-doc-fnt-180.
      *                      *-----------------------------------------*
      *                      * Alla sede                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Se il codice dipendenza archivio e' *
      *                          * a spaces : ragione sociale da [dcf] *
      *                          * diretta                             *
      *                          *-------------------------------------*
           if        rf-ost-dpz-arc       not  = spaces
                     go to stp-tes-doc-fnt-185.
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      rf-dcf-rs1-doc       to   w-stp-tes-ids-rag      .
           move      rf-dcf-rs2-doc       to   w-stp-tes-ids-rs2      .
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
           move      rf-dcf-via-dcf       to   w-stp-tes-ids-via      .
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           move      rf-dcf-loc-dcf       to   w-stp-tes-ids-loc      .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     stp-tes-doc-fnt-900.
       stp-tes-doc-fnt-185.
      *                          *-------------------------------------*
      *                          * Lettura anagrafica sede in caso di  *
      *                          * documento intestato a dipendenza    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Salvataggio record [dcf] in     *
      *                              * corso di trattamento            *
      *                              *---------------------------------*
           move      rf-dcf               to   w-sav-rec-dcf          .
      *                              *---------------------------------*
      *                              * Normalizzazione record [dcf]    *
      *                              *---------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                              *---------------------------------*
      *                              * Lettura record [dcf] per la se- *
      *                              * de                              *
      *                              *---------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
           move      rf-ost-cod-arc       to   rf-dcf-cod-fnt         .
           move      spaces               to   rf-dcf-dpz-fnt         .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
      *                                  *-----------------------------*
      *                                  * Se record non trovato :     *
      *                                  * ragione sociale con puntini *
      *                                  *-----------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-dcf-rag-soc         .
      *                              *---------------------------------*
      *                              * Ragione sociale                 *
      *                              *---------------------------------*
           move      rf-dcf-rs1-doc       to   w-stp-tes-ids-rag      .
           move      rf-dcf-rs2-doc       to   w-stp-tes-ids-rs2      .
      *                              *---------------------------------*
      *                              * Indirizzo                       *
      *                              *---------------------------------*
           move      rf-dcf-via-dcf       to   w-stp-tes-ids-via      .
      *                              *---------------------------------*
      *                              * Localita'                       *
      *                              *---------------------------------*
           move      rf-dcf-loc-dcf       to   w-stp-tes-ids-loc      .
      *                              *---------------------------------*
      *                              * Ripristino record [dcf] in      *
      *                              * corso di trattamento            *
      *                              *---------------------------------*
           move      w-sav-rec-dcf        to   rf-dcf                 .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     stp-tes-doc-fnt-900.
       stp-tes-doc-fnt-190.
      *                      *-----------------------------------------*
      *                      * Alla dipendenza                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ragione sociale                     *
      *                          *-------------------------------------*
           move      rf-dcf-rs1-doc       to   w-stp-tes-ids-rag      .
           move      rf-dcf-rs2-doc       to   w-stp-tes-ids-rs2      .
      *                          *-------------------------------------*
      *                          * Indirizzo                           *
      *                          *-------------------------------------*
           move      rf-dcf-via-dcf       to   w-stp-tes-ids-via      .
      *                          *-------------------------------------*
      *                          * Localita'                           *
      *                          *-------------------------------------*
           move      rf-dcf-loc-dcf       to   w-stp-tes-ids-loc      .
      *                          *-------------------------------------*
      *                          * Ad uscita                           *
      *                          *-------------------------------------*
           go to     stp-tes-doc-fnt-900.
       stp-tes-doc-fnt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-tes-doc-fnt-999.
       stp-tes-doc-fnt-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa testata documento                       *
      *    *                                                           *
      *    * Subroutine di raccolta dati per l'intestatario del docu-  *
      *    * mento                                                     *
      *    *                                                           *
      *    * Dipendenza                                                *
      *    *-----------------------------------------------------------*
       stp-tes-doc-dpz-000.
      *              *-------------------------------------------------*
      *              * Letture preliminari                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [ada]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [dpz]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODDPZ    "         to   f-key                  .
           move      rf-ost-cod-dpz       to   rf-ada-cod-dpz         .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                      *-----------------------------------------*
      *                      * Se record non trovato                   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-ada-rag-soc         .
       stp-tes-doc-dpz-100.
      *              *-------------------------------------------------*
      *              * Preparazioni                                    *
      *              *-------------------------------------------------*
       stp-tes-doc-dpz-110.
      *                  *---------------------------------------------*
      *                  * Intestatario documento                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      rf-ada-rag-soc       to   w-stp-tes-int-rag      .
           move      spaces               to   w-stp-tes-int-rs2      .
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
           move      rf-ada-via-azi       to   w-stp-tes-int-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      rf-ada-loc-azi       to   w-stp-tes-int-loc      .
       stp-tes-doc-dpz-150.
      *                  *---------------------------------------------*
      *                  * Indirizzo di spedizione                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      rf-ada-rag-soc       to   w-stp-tes-int-rag      .
           move      spaces               to   w-stp-tes-int-rs2      .
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
           move      rf-ada-via-azi       to   w-stp-tes-int-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      rf-ada-loc-azi       to   w-stp-tes-int-loc      .
       stp-tes-doc-dpz-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-tes-doc-dpz-999.
       stp-tes-doc-dpz-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *-----------------------------------------------------------*
       stp-cor-doc-000.
      *              *-------------------------------------------------*
      *              * Numero riga corpo in corso di stampa : zero     *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-rig-ctr-rcs      .
      *              *-------------------------------------------------*
      *              * Max nr righe di corpo stampabili                *
      *              *-------------------------------------------------*
           move      37                   to   w-stp-rig-ctr-max      .
      *              *-------------------------------------------------*
      *              * Posizionamento prima riga corpo                 *
      *              *-------------------------------------------------*
           move      26                   to   w-stp-rig-ppr-cor      .
       stp-cor-doc-100.
      *              *-------------------------------------------------*
      *              * Start su [osk] per i records relativi ai colli  *
      *              * del packing list in esame                       *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "PRTPRC    "         to   f-key                  .
           move      w-inp-mst-prt        to   rf-osk-num-prt         .
           move      w-inp-mst-prc        to   rf-osk-num-prc         .
           move      zero                 to   rf-osk-num-prg         .
           move      "pgm/ods/fls/ioc/obj/iofosk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osk                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-cor-doc-900.
       stp-cor-doc-200.
      *              *-------------------------------------------------*
      *              * Read Next da [osk]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osk                 .
      *                  *---------------------------------------------*
      *                  * Se Fine File : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-cor-doc-900.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
           if        rf-osk-num-prt       not  = w-inp-mst-prt
                     go to stp-cor-doc-900.
           if        w-inp-mst-prc        =    zero
                     go to stp-cor-doc-300.
           if        rf-osk-num-prc       not  = w-inp-mst-prc
                     go to stp-cor-doc-900.
       stp-cor-doc-300.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del progressivo riga, se   *
      *              * zero o diverso da zero                          *
      *              *-------------------------------------------------*
           if        rf-osk-num-prg       =    zero
                     go to stp-cor-doc-400
           else      go to stp-cor-doc-500.
       stp-cor-doc-400.
      *              *-------------------------------------------------*
      *              * Se progressivo riga a zero                      *
      *              *-------------------------------------------------*
       stp-cor-doc-410.
      *                  *---------------------------------------------*
      *                  * Incremento : Total No. of Packings          *
      *                  *---------------------------------------------*
           add       1                    to   w-stp-pie-tno-pkg      .
      *                  *---------------------------------------------*
      *                  * Incremento : Total Net Weight               *
      *                  *---------------------------------------------*
           add       rf-osk-lrd-prc       to   w-stp-pie-tot-ntw      .
           subtract  rf-osk-tar-prc       from w-stp-pie-tot-ntw      .      
      *                  *---------------------------------------------*
      *                  * Incremento : Total Gross Weight             *
      *                  *---------------------------------------------*
           add       rf-osk-lrd-prc       to   w-stp-pie-tot-gsw      .
       stp-cor-doc-420.
      *                  *---------------------------------------------*
      *                  * Stampa : Packing No.                        *
      *                  *---------------------------------------------*
       stp-cor-doc-422.
      *                      *-----------------------------------------*
      *                      * Incremento numero riga in corso di      *
      *                      * stampa ed eventuale reintestazione      *
      *                      *-----------------------------------------*
           add       1                    to   w-stp-rig-ctr-rcs      .
           if        w-stp-rig-ctr-rcs    not  > w-stp-rig-ctr-max
                     go to stp-cor-doc-423.
           move      1                    to   w-stp-rig-ctr-rcs      .
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  stp-cor-doc-999.
           perform   stp-pag-sta-000      thru stp-pag-sta-999        .
           go to     stp-cor-doc-424.
       stp-cor-doc-423.
           if        w-stp-pie-tno-pkg    not  > 1
                     go to stp-cor-doc-424.
           add       1                    to   w-stp-rig-ctr-rcs      .
           if        w-stp-rig-ctr-rcs    not  > w-stp-rig-ctr-max
                     go to stp-cor-doc-424.
           move      1                    to   w-stp-rig-ctr-rcs      .
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  stp-cor-doc-999.
           perform   stp-pag-sta-000      thru stp-pag-sta-999        .
       stp-cor-doc-424.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      12                   to   p-car                  .
           move      w-stp-rig-ctr-rcs    to   p-lin                  .
           add       w-stp-rig-ppr-cor    to   p-lin                  .
           subtract  1                    from p-lin                  .
           move      020                  to   p-pos                  .
           move      "Packing No. "       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Contrassegno collo                      *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      10                   to   p-car                  .
           move      w-stp-rig-ctr-rcs    to   p-lin                  .
           add       w-stp-rig-ppr-cor    to   p-lin                  .
           subtract  1                    from p-lin                  .
           move      032                  to   p-pos                  .
           move      rf-osk-cts-prc       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-430.
      *                  *---------------------------------------------*
      *                  * Stampa : Net Weight                         *
      *                  *---------------------------------------------*
       stp-cor-doc-431.
      *                      *-----------------------------------------*
      *                      * Se zero : no stampa                     *
      *                      *-----------------------------------------*
           move      rf-osk-lrd-prc       to   p-num                  .
           subtract  rf-osk-tar-prc       from p-num                  .
           if        p-num                =    zero
                     go to stp-cor-doc-440.
       stp-cor-doc-432.
      *                      *-----------------------------------------*
      *                      * Incremento numero riga in corso di      *
      *                      * stampa ed eventuale reintestazione      *
      *                      *-----------------------------------------*
           add       1                    to   w-stp-rig-ctr-rcs      .
           if        w-stp-rig-ctr-rcs    not  > w-stp-rig-ctr-max
                     go to stp-cor-doc-434.
           move      1                    to   w-stp-rig-ctr-rcs      .
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  stp-cor-doc-999.
           perform   stp-pag-sta-000      thru stp-pag-sta-999        .
       stp-cor-doc-434.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      18                   to   p-car                  .
           move      w-stp-rig-ctr-rcs    to   p-lin                  .
           add       w-stp-rig-ppr-cor    to   p-lin                  .
           subtract  1                    from p-lin                  .
           move      020                  to   p-pos                  .
           move      " - Net   Weight : " to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      03                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<GD"                to   p-edm                  .
           move      rf-osk-lrd-prc       to   p-num                  .
           subtract  rf-osk-tar-prc       from p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      18                   to   p-car                  .
           move      w-stp-rig-ctr-rcs    to   p-lin                  .
           add       w-stp-rig-ppr-cor    to   p-lin                  .
           subtract  1                    from p-lin                  .
           move      038                  to   p-pos                  .
           move      spaces               to   p-alf                  .
           string    p-edt      delimited by   spaces
                     " KGS"     delimited by   size
                                          into p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-440.
      *                  *---------------------------------------------*
      *                  * Stampa : Gross Weight                       *
      *                  *---------------------------------------------*
       stp-cor-doc-441.
      *                      *-----------------------------------------*
      *                      * Se zero : no stampa                     *
      *                      *-----------------------------------------*
           if        rf-osk-lrd-prc       =    zero
                     go to stp-cor-doc-450.
       stp-cor-doc-442.
      *                      *-----------------------------------------*
      *                      * Incremento numero riga in corso di      *
      *                      * stampa ed eventuale reintestazione      *
      *                      *-----------------------------------------*
           add       1                    to   w-stp-rig-ctr-rcs      .
           if        w-stp-rig-ctr-rcs    not  > w-stp-rig-ctr-max
                     go to stp-cor-doc-444.
           move      1                    to   w-stp-rig-ctr-rcs      .
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  stp-cor-doc-999.
           perform   stp-pag-sta-000      thru stp-pag-sta-999        .
       stp-cor-doc-444.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      18                   to   p-car                  .
           move      w-stp-rig-ctr-rcs    to   p-lin                  .
           add       w-stp-rig-ppr-cor    to   p-lin                  .
           subtract  1                    from p-lin                  .
           move      020                  to   p-pos                  .
           move      " - Gross Weight : " to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      03                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<GD"                to   p-edm                  .
           move      rf-osk-lrd-prc       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      18                   to   p-car                  .
           move      w-stp-rig-ctr-rcs    to   p-lin                  .
           add       w-stp-rig-ppr-cor    to   p-lin                  .
           subtract  1                    from p-lin                  .
           move      038                  to   p-pos                  .
           move      spaces               to   p-alf                  .
           string    p-edt      delimited by   spaces
                     " KGS"     delimited by   size
                                          into p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-450.
      *                  *---------------------------------------------*
      *                  * Stampa : Dimensions                         *
      *                  *---------------------------------------------*
       stp-cor-doc-451.
      *                      *-----------------------------------------*
      *                      * Se zero : no stampa                     *
      *                      *-----------------------------------------*
           if        rf-osk-dim-lar       =    zero or
                     rf-osk-dim-alt       =    zero or
                     rf-osk-dim-prf       =    zero
                     go to stp-cor-doc-490.
       stp-cor-doc-452.
      *                      *-----------------------------------------*
      *                      * Incremento numero riga in corso di      *
      *                      * stampa ed eventuale reintestazione      *
      *                      *-----------------------------------------*
           add       1                    to   w-stp-rig-ctr-rcs      .
           if        w-stp-rig-ctr-rcs    not  > w-stp-rig-ctr-max
                     go to stp-cor-doc-454.
           move      1                    to   w-stp-rig-ctr-rcs      .
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  stp-cor-doc-999.
           perform   stp-pag-sta-000      thru stp-pag-sta-999        .
       stp-cor-doc-454.
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      18                   to   p-car                  .
           move      w-stp-rig-ctr-rcs    to   p-lin                  .
           add       w-stp-rig-ppr-cor    to   p-lin                  .
           subtract  1                    from p-lin                  .
           move      020                  to   p-pos                  .
           move      " - Dimensions   : " to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Editing delle tre dimensioni            *
      *                      *-----------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      03                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<D"                 to   p-edm                  .
           move      rf-osk-dim-lar       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-rig-edd-001      .
      *
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      03                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<D"                 to   p-edm                  .
           move      rf-osk-dim-alt       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-rig-edd-002      .
      *
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      03                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<D"                 to   p-edm                  .
           move      rf-osk-dim-prf       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-rig-edd-003      .
      *                      *-----------------------------------------*
      *                      * Valore                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      18                   to   p-car                  .
           move      w-stp-rig-ctr-rcs    to   p-lin                  .
           add       w-stp-rig-ppr-cor    to   p-lin                  .
           subtract  1                    from p-lin                  .
           move      038                  to   p-pos                  .
           move      spaces               to   p-alf                  .
           string    w-stp-rig-edd-001
                                delimited by   spaces
                     "x"
                                delimited by   size
                     w-stp-rig-edd-002
                                delimited by   spaces
                     "x"
                                delimited by   size
                     w-stp-rig-edd-003
                                delimited by   spaces
                                          into p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-490.
      *                  *---------------------------------------------*
      *                  * Stampa : Annotazione                        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se a spazi : no stampa                  *
      *                      *-----------------------------------------*
           if        rf-osk-not-prc       =    spaces
                     go to stp-cor-doc-495.
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      34                   to   p-car                  .
           move      65                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      062                  to   p-pos                  .
           move      rf-osk-not-prc       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-495.
      *                  *---------------------------------------------*
      *                  * Riciclo a record successivo                 *
      *                  *---------------------------------------------*
           go to     stp-cor-doc-200.
       stp-cor-doc-500.
      *              *-------------------------------------------------*
      *              * Se progressivo riga diverso da zero             *
      *              *-------------------------------------------------*
       stp-cor-doc-510.
      *                  *---------------------------------------------*
      *                  * Lettura record riga ordine di spedizione    *
      *                  * corrispondente                              *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-osk-num-prt       to   rf-osr-num-prt         .
           move      rf-osk-num-prg       to   rf-osr-num-prg         .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : si ignora la riga       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-cor-doc-200.
       stp-cor-doc-520.
      *                  *---------------------------------------------*
      *                  * Determinazione descrizione in riga          *
      *                  *---------------------------------------------*
           perform   det-des-rig-000      thru det-des-rig-999        .
       stp-cor-doc-530.
      *                  *---------------------------------------------*
      *                  * Incremento numero riga in corso di stampa,  *
      *                  * ed eventuale reintestazione                 *
      *                  *---------------------------------------------*
           add       1                    to   w-stp-rig-ctr-rcs      .
           if        w-stp-rig-ctr-rcs    not  > w-stp-rig-ctr-max
                     go to stp-cor-doc-540.
           move      1                    to   w-stp-rig-ctr-rcs      .
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  stp-cor-doc-999.
           perform   stp-pag-sta-000      thru stp-pag-sta-999        .
       stp-cor-doc-540.
      *                  *---------------------------------------------*
      *                  * Stampa della colonna Part-Number            *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      14                   to   p-car                  .
           move      w-stp-rig-ctr-rcs    to   p-lin                  .
           add       w-stp-rig-ppr-cor    to   p-lin                  .
           subtract  1                    from p-lin                  .
           move      004                  to   p-pos                  .
           if        rf-osr-tip-rig       not  = "P    "
                     move  spaces         to   p-alf
           else if   w-prs-spl-cds        =    "C"
                     move  rf-osr-cop-scl to   p-alf
           else      move  rf-osr-alf-pro to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-550.
      *                  *---------------------------------------------*
      *                  * Stampa della colonna Description, 1. riga   *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      w-stp-rig-ctr-rcs    to   p-lin                  .
           add       w-stp-rig-ppr-cor    to   p-lin                  .
           subtract  1                    from p-lin                  .
           move      020                  to   p-pos                  .
           move      w-stp-rig-des-rgd (1)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-560.
      *                  *---------------------------------------------*
      *                  * Stampa della colonna Quantity               *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      03                   to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           move      "DB"                 to   p-edm                  .
           move      w-stp-rig-ctr-rcs    to   p-lin                  .
           add       w-stp-rig-ppr-cor    to   p-lin                  .
           subtract  1                    from p-lin                  .
           move      062                  to   p-pos                  .
           move      rf-osk-qta-prc       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-570.
      *                  *---------------------------------------------*
      *                  * Stampa della colonna Description, righe e-  *
      *                  * ventuali da 2 a 10                          *
      *                  *---------------------------------------------*
       stp-cor-doc-575.
      *                      *-----------------------------------------*
      *                      * Determinazione indice iniziale e finale *
      *                      * riga da stampare                        *
      *                      * Se nessuna riga da stampare : oltre     *
      *                      *-----------------------------------------*
           move      02                   to   w-stp-rig-des-c01      .
           move      10                   to   w-stp-rig-des-c02      .
       stp-cor-doc-576.
           if        w-stp-rig-des-rgd
                    (w-stp-rig-des-c02)   not  = spaces
                     go to stp-cor-doc-580.
           subtract  1                    from w-stp-rig-des-c02      .
           if        w-stp-rig-des-c02    =    2
                     go to stp-cor-doc-650.
           go to     stp-cor-doc-576.
       stp-cor-doc-580.
      *                      *-----------------------------------------*
      *                      * Incremento numero riga in corso di      *
      *                      * stampa, ed eventuale reintestazione     *
      *                      *-----------------------------------------*
           add       1                    to   w-stp-rig-ctr-rcs      .
           if        w-stp-rig-ctr-rcs    not  > w-stp-rig-ctr-max
                     go to stp-cor-doc-590.
           move      1                    to   w-stp-rig-ctr-rcs      .
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  stp-cor-doc-999.
           perform   stp-pag-sta-000      thru stp-pag-sta-999        .
       stp-cor-doc-590.
      *                      *-----------------------------------------*
      *                      * Stampa Description, riga aggiuntiva     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      w-stp-rig-ctr-rcs    to   p-lin                  .
           add       w-stp-rig-ppr-cor    to   p-lin                  .
           subtract  1                    from p-lin                  .
           move      020                  to   p-pos                  .
           move      w-stp-rig-des-rgd
                    (w-stp-rig-des-c01)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-600.
      *                      *-----------------------------------------*
      *                      * Incremento numero riga, ed eventuale    *
      *                      * riciclo                                 *
      *                      *-----------------------------------------*
           add       1                    to   w-stp-rig-des-c01      .
           if        w-stp-rig-des-c01    not  > w-stp-rig-des-c02
                     go to stp-cor-doc-580.
       stp-cor-doc-650.
      *                  *---------------------------------------------*
      *                  * Stampa prima riga aggiuntiva                *
      *                  *---------------------------------------------*
       stp-cor-doc-651.
      *                      *-----------------------------------------*
      *                      * Se tipo riga non-P : no stampa          *
      *                      *-----------------------------------------*
           if        rf-osr-tip-rig       not  = "P    "
                     go to stp-cor-doc-680.
       stp-cor-doc-652.
      *                      *-----------------------------------------*
      *                      * Deviazione secondo la personalizzazione *
      *                      *-----------------------------------------*
           if        w-prs-spl-rac        =    "C"
                     go to stp-cor-doc-654
           else if   w-prs-spl-rac        =    "M"
                     go to stp-cor-doc-664
           else if   w-prs-spl-rac        =    "E"
                     go to stp-cor-doc-654
           else      go to stp-cor-doc-680.
       stp-cor-doc-654.
      *                      *-----------------------------------------*
      *                      * Se il codice del cliente                *
      *                      *-----------------------------------------*
       stp-cor-doc-656.
      *                          *-------------------------------------*
      *                          * Se codice a spaces : no stampa      *
      *                          *-------------------------------------*
           if        rf-osr-cop-scl       =    spaces
                     go to stp-cor-doc-680.
       stp-cor-doc-658.
      *                          *-------------------------------------*
      *                          * Incremento numero riga in corso di  *
      *                          * stampa, ed eventuale reintestazione *
      *                          *-------------------------------------*
           add       1                    to   w-stp-rig-ctr-rcs      .
           if        w-stp-rig-ctr-rcs    not  > w-stp-rig-ctr-max
                     go to stp-cor-doc-660.
           move      1                    to   w-stp-rig-ctr-rcs      .
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  stp-cor-doc-999.
           perform   stp-pag-sta-000      thru stp-pag-sta-999        .
       stp-cor-doc-660.
      *                          *-------------------------------------*
      *                          * Stampa, con literal                 *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      w-stp-rig-ctr-rcs    to   p-lin                  .
           add       w-stp-rig-ppr-cor    to   p-lin                  .
           subtract  1                    from p-lin                  .
           move      020                  to   p-pos                  .
           move      spaces               to   p-alf                  .
           string    "Your Part-Number : "
                                delimited by   size
                     rf-osr-cop-scl
                                delimited by   size
                                          into p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-cor-doc-680.
       stp-cor-doc-664.
      *                      *-----------------------------------------*
      *                      * Se il nostro codice                     *
      *                      *-----------------------------------------*
       stp-cor-doc-666.
      *                          *-------------------------------------*
      *                          * Se codice a spaces : no stampa      *
      *                          *-------------------------------------*
           if        rf-osr-alf-pro       =    spaces
                     go to stp-cor-doc-680.
       stp-cor-doc-668.
      *                          *-------------------------------------*
      *                          * Incremento numero riga in corso di  *
      *                          * stampa, ed eventuale reintestazione *
      *                          *-------------------------------------*
           add       1                    to   w-stp-rig-ctr-rcs      .
           if        w-stp-rig-ctr-rcs    not  > w-stp-rig-ctr-max
                     go to stp-cor-doc-670.
           move      1                    to   w-stp-rig-ctr-rcs      .
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  stp-cor-doc-999.
           perform   stp-pag-sta-000      thru stp-pag-sta-999        .
       stp-cor-doc-670.
      *                          *-------------------------------------*
      *                          * Stampa, con literal                 *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      w-stp-rig-ctr-rcs    to   p-lin                  .
           add       w-stp-rig-ppr-cor    to   p-lin                  .
           subtract  1                    from p-lin                  .
           move      020                  to   p-pos                  .
           move      spaces               to   p-alf                  .
           string    "Our Part-Number : "
                                delimited by   size
                     rf-osr-alf-pro
                                delimited by   size
                                          into p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-cor-doc-680.
       stp-cor-doc-680.
      *                  *---------------------------------------------*
      *                  * Stampa seconda riga aggiuntiva              *
      *                  *---------------------------------------------*
       stp-cor-doc-681.
      *                      *-----------------------------------------*
      *                      * Se tipo riga non-P : no stampa          *
      *                      *-----------------------------------------*
           if        rf-osr-tip-rig       not  = "P    "
                     go to stp-cor-doc-700.
       stp-cor-doc-682.
      *                      *-----------------------------------------*
      *                      * Deviazione secondo la personalizzazione *
      *                      *-----------------------------------------*
           if        w-prs-spl-rac        =    "E"
                     go to stp-cor-doc-684
           else      go to stp-cor-doc-700.
       stp-cor-doc-684.
      *                      *-----------------------------------------*
      *                      * Se il nostro codice                     *
      *                      *-----------------------------------------*
       stp-cor-doc-686.
      *                          *-------------------------------------*
      *                          * Se codice a spaces : no stampa      *
      *                          *-------------------------------------*
           if        rf-osr-alf-pro       =    spaces
                     go to stp-cor-doc-700.
       stp-cor-doc-688.
      *                          *-------------------------------------*
      *                          * Incremento numero riga in corso di  *
      *                          * stampa, ed eventuale reintestazione *
      *                          *-------------------------------------*
           add       1                    to   w-stp-rig-ctr-rcs      .
           if        w-stp-rig-ctr-rcs    not  > w-stp-rig-ctr-max
                     go to stp-cor-doc-690.
           move      1                    to   w-stp-rig-ctr-rcs      .
           perform   int-pag-sta-000      thru int-pag-sta-999        .
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  stp-cor-doc-999.
           perform   stp-pag-sta-000      thru stp-pag-sta-999        .
       stp-cor-doc-690.
      *                          *-------------------------------------*
      *                          * Stampa, con literal                 *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      w-stp-rig-ctr-rcs    to   p-lin                  .
           add       w-stp-rig-ppr-cor    to   p-lin                  .
           subtract  1                    from p-lin                  .
           move      020                  to   p-pos                  .
           move      spaces               to   p-alf                  .
           string    "Our Part-Number : "
                                delimited by   size
                     rf-osr-alf-pro
                                delimited by   size
                                          into p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to     stp-cor-doc-700.
       stp-cor-doc-700.
      *                  *---------------------------------------------*
      *                  * Riciclo a record successivo                 *
      *                  *---------------------------------------------*
           go to     stp-cor-doc-200.
       stp-cor-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-999.
       stp-cor-doc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione descrizione per la riga                    *
      *    *-----------------------------------------------------------*
       det-des-rig-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del flag di estensione   *
      *              *-------------------------------------------------*
           if        rf-osr-des-ext       =    0
                     go to det-des-rig-100
           else if   rf-osr-des-ext       =    1
                     go to det-des-rig-300
           else if   rf-osr-des-ext       =    2
                     go to det-des-rig-600.
       det-des-rig-100.
      *              *-------------------------------------------------*
      *              * Se nessuna estensione                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Descrizione contenuta in record [osr]       *
      *                  *---------------------------------------------*
           move      rf-osr-des-rig       to   w-stp-rig-des-der      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-des-rig-900.
       det-des-rig-300.
      *              *-------------------------------------------------*
      *              * Se estensione in file [osx]                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [osx]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osx                 .
      *                  *---------------------------------------------*
      *                  * Lettura file [osx]                          *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-osr-num-prt       to   rf-osx-num-prt         .
           move      rf-osr-num-prg       to   rf-osx-num-prg         .
           move      11                   to   rf-osx-tip-rec         .
           move      "pgm/ods/fls/ioc/obj/iofosx"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osx                 .
           if        f-sts                =    e-not-err
                     move  rf-osx-des-400 to   w-stp-rig-des-der
           else      move  spaces         to   w-stp-rig-des-der      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-des-rig-900.
       det-des-rig-600.
      *              *-------------------------------------------------*
      *              * Se estensione in file [pdx]                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione valori di comodo            *
      *                  *---------------------------------------------*
           move      spaces               to   w-stp-rig-des-der      .
           move      zero                 to   w-stp-rig-des-ctr      .
      *                  *---------------------------------------------*
      *                  * Lettura file [pdx]                          *
      *                  *---------------------------------------------*
           move      rf-osr-num-pro       to   w-let-dcp-pdx-cod      .
           move      rf-ost-tip-arc       to   w-let-dcp-pdx-tar      .
      *
           if        rf-ost-tip-frn       =    11
                     move  rf-ost-cod-arc to   w-let-dcp-pdx-arc
           else      move  rf-ost-arc-plf to   w-let-dcp-pdx-arc      .
      *
           move      rf-osr-cod-lng       to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione descrizione contenuta nel   *
      *                  * record [pdx]                                *
      *                  *---------------------------------------------*
           move      w-let-dcp-pdx-des    to   w-stp-rig-des-der      .
       det-des-rig-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-des-rig-999.
       det-des-rig-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa piede documento                         *
      *    *-----------------------------------------------------------*
       stp-pie-doc-000.
      *              *-------------------------------------------------*
      *              * Aventuale ricalcolo dei totali se la stampa e'  *
      *              * per singolo collo                               *
      *              *-------------------------------------------------*
           if        w-inp-mst-prc        =    zero
                     go to stp-pie-doc-010.
      *                  *---------------------------------------------*
      *                  * Subroutine di ricalcolo dei totali          *
      *                  *---------------------------------------------*
           perform   stp-pie-doc-tot-000  thru stp-pie-doc-tot-999    .
       stp-pie-doc-010.
      *              *-------------------------------------------------*
      *              * Total No. of Packings                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Literal                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      23                   to   p-car                  .
           move      65                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      004                  to   p-pos                  .
           move      "Total No. of Packings :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Valore                                      *
      *                  *---------------------------------------------*
           if        w-stp-pie-tno-pkg    =    zero
                     go to stp-pie-doc-100.
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      05                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      65                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      028                  to   p-pos                  .
           move      w-stp-pie-tno-pkg    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-pie-doc-100.
      *              *-------------------------------------------------*
      *              * Total Net Weight                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Literal                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      23                   to   p-car                  .
           move      67                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      004                  to   p-pos                  .
           move      "Total Net Weight      :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Valore                                      *
      *                  *---------------------------------------------*
           if        w-stp-pie-tot-ntw    =    zero
                     go to stp-pie-doc-200.
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      03                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<BD"                to   p-edm                  .
           move      w-stp-pie-tot-ntw    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      20                   to   p-car                  .
           move      67                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      028                  to   p-pos                  .
           move      spaces               to   p-alf                  .
           string    p-edt      delimited by   spaces
                     " KGS"
                                delimited by   size
                                          into p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-pie-doc-200.
      *              *-------------------------------------------------*
      *              * Total Gross Weight                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Literal                                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      23                   to   p-car                  .
           move      68                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      004                  to   p-pos                  .
           move      "Total Gross Weight    :"
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * Valore                                      *
      *                  *---------------------------------------------*
           if        w-stp-pie-tot-gsw    =    zero
                     go to stp-pie-doc-900.
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      08                   to   p-car                  .
           move      03                   to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<BD"                to   p-edm                  .
           move      w-stp-pie-tot-gsw    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      20                   to   p-car                  .
           move      68                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           move      028                  to   p-pos                  .
           move      spaces               to   p-alf                  .
           string    p-edt      delimited by   spaces
                     " KGS"
                                delimited by   size
                                          into p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-pie-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-pie-doc-999.
       stp-pie-doc-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa piede documento                         *
      *    *                                                           *
      *    * Subroutine di ricalcolo dei totali                        *
      *    *-----------------------------------------------------------*
       stp-pie-doc-tot-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-pie-tno-pkg      .
           move      zero                 to   w-stp-pie-tot-ntw      .
           move      zero                 to   w-stp-pie-tot-gsw      .
       stp-pie-doc-tot-100.
      *              *-------------------------------------------------*
      *              * Start su [osk] per i records relativi ai colli  *
      *              * del packing list in esame                       *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "PRTPRC    "         to   f-key                  .
           move      w-inp-mst-prt        to   rf-osk-num-prt         .
           move      zero                 to   rf-osk-num-prc         .
           move      zero                 to   rf-osk-num-prg         .
           move      "pgm/ods/fls/ioc/obj/iofosk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osk                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata : ad uscita                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-pie-doc-tot-900.
       stp-pie-doc-tot-200.
      *              *-------------------------------------------------*
      *              * Read Next da [osk]                              *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosk"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osk                 .
      *                  *---------------------------------------------*
      *                  * Se Fine File : ad uscita                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-pie-doc-tot-900.
       stp-pie-doc-tot-300.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
           if        rf-osk-num-prt       not  = w-inp-mst-prt
                     go to stp-pie-doc-tot-900.
       stp-pie-doc-tot-400.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del progressivo riga, se   *
      *              * zero o diverso da zero                          *
      *              *-------------------------------------------------*
           if        rf-osk-num-prg       =    zero
                     go to stp-pie-doc-tot-500
           else      go to stp-pie-doc-tot-200.
       stp-pie-doc-tot-500.
      *              *-------------------------------------------------*
      *              * Se progressivo riga a zero                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento : Total No. of Packings          *
      *                  *---------------------------------------------*
           add       1                    to   w-stp-pie-tno-pkg      .
      *                  *---------------------------------------------*
      *                  * Incremento : Total Net Weight               *
      *                  *---------------------------------------------*
           add       rf-osk-lrd-prc       to   w-stp-pie-tot-ntw      .
           subtract  rf-osk-tar-prc       from w-stp-pie-tot-ntw      .      
      *                  *---------------------------------------------*
      *                  * Incremento : Total Gross Weight             *
      *                  *---------------------------------------------*
           add       rf-osk-lrd-prc       to   w-stp-pie-tot-gsw      .
       stp-pie-doc-tot-800.
      *              *-------------------------------------------------*
      *              * Riciclo                                         *
      *              *-------------------------------------------------*
           go to     stp-pie-doc-tot-200.
       stp-pie-doc-tot-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-pie-doc-tot-999.
       stp-pie-doc-tot-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa di un box                               *
      *    *-----------------------------------------------------------*
       rou-stp-box-000.
      *              *-------------------------------------------------*
      *              * Assestamenti relativi al displacement verticale *
      *              *-------------------------------------------------*
           add       w-dsp-dsp-vrt        to   w-rou-stp-box-lin      .
           add       w-dsp-dsp-vrt        to   w-rou-stp-box-lfi      .
      *              *-------------------------------------------------*
      *              * Preparazione line - position - size             *
      *              *-------------------------------------------------*
           move      w-rou-stp-box-lin    to   w-rou-stp-box-wln      .
           move      w-rou-stp-box-pin    to   w-rou-stp-box-wps      .
           subtract  w-rou-stp-box-pin    from w-rou-stp-box-pfi
                                        giving w-rou-stp-box-wsz      .
           add       1                    to   w-rou-stp-box-wsz      .
      *              *-------------------------------------------------*
      *              * Linea superiore                                 *
      *              *-------------------------------------------------*
           move      w-rou-stp-box-wtr    to   w-rou-stp-box-wst      .
           move      "+"                  to   w-rou-stp-box-wch (1)  .
           move      "+"                  to   w-rou-stp-box-wch
                                              (w-rou-stp-box-wsz)     .
           perform   rou-stp-box-900      thru rou-stp-box-919        .
      *              *-------------------------------------------------*
      *              * Linee intermedie                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
           move      spaces               to   w-rou-stp-box-wst      .
           move      "|"                  to   w-rou-stp-box-wch (1)  .
           move      "|"                  to   w-rou-stp-box-wch
                                              (w-rou-stp-box-wsz)     .
           subtract  w-rou-stp-box-lin    from w-rou-stp-box-lfi
                                        giving w-rou-stp-box-ctr      .
           subtract  1                    from w-rou-stp-box-ctr      .
       rou-stp-box-500.
           add       1                    to   w-rou-stp-box-wln      .
           if        w-rou-stp-box-ctr    >    zero
                     subtract 1           from w-rou-stp-box-ctr
                     perform  rou-stp-box-900
                                          thru rou-stp-box-919
                     go to    rou-stp-box-500.
      *              *-------------------------------------------------*
      *              * Linea inferiore                                 *
      *              *-------------------------------------------------*
           move      w-rou-stp-box-wtr    to   w-rou-stp-box-wst      .
           move      "+"                  to   w-rou-stp-box-wch (1)  .
           move      "+"                  to   w-rou-stp-box-wch
                                              (w-rou-stp-box-wsz)     .
           perform   rou-stp-box-900      thru rou-stp-box-919        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-box-999.
       rou-stp-box-900.
      *              *=================================================*
      *              * Sub-routine interna di stampa riga              *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-rou-stp-box-wsz    to   p-car                  .
           move      w-rou-stp-box-wln    to   p-lin                  .
           move      w-rou-stp-box-wps    to   p-pos                  .
           move      w-rou-stp-box-wst    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-box-919.
           exit.
       rou-stp-box-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.lts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

