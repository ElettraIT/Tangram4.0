       Identification Division.
       Program-Id.                                 pods300s           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    ele                 *
      *                        Area gestionale:    ods                 *
      *                                Settore:    mov                 *
      *                                   Fase:    ods300              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 28/03/95    *
      *                       Ultima revisione:    NdK del 16/02/09    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Modulo di stampa documento : Ordine di      *
      *                                                 spedizione     *
      *                                                                *
      *                    VERSIONE SU MISURA PER ELETTRA              *
      *                                                                *
      *                !!! NON PIU' IN USO: VEDI 'pods300s'            *
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
      *                * Si/No Eject a fine documento                  *
      *                *  - S : Si                                     *
      *                *  - N : No                                     *
      *                *-----------------------------------------------*
                   15  w-inp-mst-ejc      pic  x(01)                  .
      *                *-----------------------------------------------*
      *                * Filler                                        *
      *                *-----------------------------------------------*
                   15  filler             pic  x(66)                  .
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
      *        * Descrizione del programma                             *
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
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .
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
      *        * [zvf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzvf"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .
      *        *-------------------------------------------------------*
      *        * [aaq]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfaaq"                          .
      *        *-------------------------------------------------------*
      *        * [vet]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfvet"                          .
      *        *-------------------------------------------------------*
      *        * [axi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxi"                          .
      *        *-------------------------------------------------------*
      *        * [axs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/abi/fls/rec/rfaxs"                          .
      *        *-------------------------------------------------------*
      *        * [cbp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfcbp"                          .
      *        *-------------------------------------------------------*
      *        * [mau]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/mag/fls/rec/rfmau"                          .

      *    *===========================================================*
      *    * Work-area personalizzazioni                               *
      *    *-----------------------------------------------------------*
       01  w-prs.
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
      *            *  - 'P' = Una riga aggiuntiva con il codice della  *
      *            *          casa produttrice                         *
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
      *            * Tipo visualizzazione in campo note                *
      *            *                                                   *
      *            *  - '00' = Nessuna visualizzazione                 *
      *            *  - '01' = Giacenza del prodotto                   *
      *            *---------------------------------------------------*
               10  w-prs-sos-tvn          pic  9(02)                  .
               10  filler                 pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area libera per implementazioni future            *
      *            *---------------------------------------------------*
               10  w-prs-sos-exp          pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Contatore di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-prs-sos-ctr              pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area referenze                                       *
      *    *-----------------------------------------------------------*
       01  w-ref.
      *        *-------------------------------------------------------*
      *        * Intestazioni per i box di firma                       *
      *        *-------------------------------------------------------*
           05  w-ref-int-frm.
               10  w-ref-int-frm-des occurs 3
                                          pic  x(40)                  .
               10  w-ref-int-frm-ctr      pic  9(02)                  .

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
      *        * Sub-work per intestazione pagina                      *
      *        *-------------------------------------------------------*
           05  w-stp-int.
      *            *---------------------------------------------------*
      *            * Campi di comodo                                   *
      *            *---------------------------------------------------*
               10  w-stp-int-pnt          pic  9(03)                  .
               10  w-stp-int-ndo          pic  9(11)                  .
               10  w-stp-int-ndo-r redefines
                   w-stp-int-ndo.
                   15  w-stp-int-ndo-saa  pic  9(03)                  .
                   15  w-stp-int-ndo-dpz  pic  9(02)                  .
                   15  w-stp-int-ndo-prg  pic  9(06)                  .
               10  w-stp-int-npr          pic  9(09)                  .
               10  w-stp-int-npr-r redefines
                   w-stp-int-npr.
                   15  w-stp-int-npr-saa  pic  9(03)                  .
                   15  w-stp-int-npr-prg  pic  9(06)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per testata documento                        *
      *        *-------------------------------------------------------*
           05  w-stp-tes.
      *            *---------------------------------------------------*
      *            * Sub-work per intestatario documento               *
      *            *---------------------------------------------------*
               10  w-stp-tes-int.
      *                *-----------------------------------------------*
      *                * Codice archivio editato                       *
      *                *-----------------------------------------------*
                   15  w-stp-tes-int-arc  pic  x(07)                  .
      *                *-----------------------------------------------*
      *                * Prompt                                        *
      *                *-----------------------------------------------*
                   15  w-stp-tes-int-pmt  pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Ragione sociale                               *
      *                *-----------------------------------------------*
                   15  w-stp-tes-int-rag  pic  x(40)                  .
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
      *            * Sub-work per indirizzo di spedizione              *
      *            *---------------------------------------------------*
               10  w-stp-tes-ids.
      *                *-----------------------------------------------*
      *                * Ragione sociale                               *
      *                *-----------------------------------------------*
                   15  w-stp-tes-ids-rag  pic  x(40)                  .
                   15  w-stp-tes-ids-rs2  pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Indirizzo                                     *
      *                *-----------------------------------------------*
                   15  w-stp-tes-ids-via  pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Localita'                                     *
      *                *-----------------------------------------------*
                   15  w-stp-tes-ids-loc  pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Sub-work per condizioni di fornitura              *
      *            *---------------------------------------------------*
               10  w-stp-tes-cdf.
      *                *-----------------------------------------------*
      *                * Castelletto voci descrittive                  *
      *                *-----------------------------------------------*
                   15  w-stp-tes-cdf-vdf occurs 6
                                          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Sub-work per condizioni di pagamento              *
      *            *---------------------------------------------------*
               10  w-stp-tes-cdp.
      *                *-----------------------------------------------*
      *                * Castelletto condizioni di pagamento           *
      *                *-----------------------------------------------*
                   15  w-stp-tes-cdp-cst occurs 6
                                          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Sub-work per note relative all'intestatario       *
      *            *---------------------------------------------------*
               10  w-stp-tes-nri.
      *                *-----------------------------------------------*
      *                * Castelletto giorni ed orario di apertura      *
      *                *-----------------------------------------------*
                   15  w-stp-tes-nri-goa occurs 6
                                          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Sub-work per annotazioni per la spedizione        *
      *            *---------------------------------------------------*
               10  w-stp-tes-aps.
      *                *-----------------------------------------------*
      *                * Castelletto righe di annotazione              *
      *                *-----------------------------------------------*
                   15  w-stp-tes-aps-rda occurs 3
                                          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Sub-work per vettori                              *
      *            *---------------------------------------------------*
               10  w-stp-tes-vet.
      *                *-----------------------------------------------*
      *                * Castelletto vettori                           *
      *                *-----------------------------------------------*
                   15  w-stp-tes-vet-rag occurs 3
                                          pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Sub-work per interlocutore e telefono             *
      *            *---------------------------------------------------*
               10  w-stp-tes-iet.
      *                *-----------------------------------------------*
      *                * Nominativo interlocutore                      *
      *                *-----------------------------------------------*
                   15  w-stp-tes-iet-int  pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Telefono                                      *
      *                *-----------------------------------------------*
                   15  w-stp-tes-iet-tel  pic  x(40)                  .
      *            *---------------------------------------------------*
      *            * Sub-work per campi di comodo                      *
      *            *---------------------------------------------------*
               10  w-stp-tes-wrk.
      *                *-----------------------------------------------*
      *                * Contatori di comodo                           *
      *                *-----------------------------------------------*
                   15  w-stp-tes-wrk-c01  pic  9(03)                  .
                   15  w-stp-tes-wrk-c02  pic  9(03)                  .
      *                *-----------------------------------------------*
      *                * Comodo per composizione stringhe              *
      *                *-----------------------------------------------*
                   15  w-stp-tes-doc-str.
                       20  filler   occurs 132
                                          pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa righe corpo                       *
      *        *-------------------------------------------------------*
           05  w-stp-rig.
      *            *---------------------------------------------------*
      *            * Numero riga corpo in corso di stampa              *
      *            *---------------------------------------------------*
               10  w-stp-rig-ctr-rcs      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Area di comodo per la stampa delle descrizioni in *
      *            * riga                                              *
      *            *---------------------------------------------------*
               10  w-stp-rig-des.
      *                *-----------------------------------------------*
      *                * Contatori di comodo                           *
      *                *-----------------------------------------------*
                   15  w-stp-rig-des-ctr  pic  9(03)                  .
                   15  w-stp-rig-des-nrd  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Comodo per descrizione                        *
      *                *-----------------------------------------------*
                   15  w-stp-rig-des-der.
                       20  w-stp-rig-des-rgd occurs 10
                                          pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per editing attinenti la stampa dei giorni di   *
      *    * chiusura e dell'orario di apertura clienti                *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/egchoap0.edl"                   .

      *    *===========================================================*
      *    * Work per routine box-int-pag-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-box-int-pag.
      *        *-------------------------------------------------------*
      *        * Parametri in input                                    *
      *        *-------------------------------------------------------*
           05  w-box-int-pag-lin          pic  9(03)                  .
           05  w-box-int-pag-pin          pic  9(03)                  .
           05  w-box-int-pag-lfi          pic  9(03)                  .
           05  w-box-int-pag-pfi          pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Campi di lavoro                                       *
      *        *-------------------------------------------------------*
           05  w-box-int-pag-wln          pic  9(03)                  .
           05  w-box-int-pag-wps          pic  9(03)                  .
           05  w-box-int-pag-wsz          pic  9(03)                  .
           05  w-box-int-pag-wtr.
               10  filler            occurs 132
                                          pic  x(01) value all "-"    .
           05  w-box-int-pag-wst.
               10  w-box-int-pag-wch occurs 132
                                          pic  x(01)                  .
           05  w-box-int-pag-ctr          pic  9(03)                  .

      *    *===========================================================*
      *    * Work per routine rou-stp-pmt-000/999                      *
      *    *-----------------------------------------------------------*
       01  w-rou-stp-pmt.
      *        *-------------------------------------------------------*
      *        * Parametri in input                                    *
      *        *-------------------------------------------------------*
           05  w-rou-stp-pmt-lin          pic  9(03)                  .
           05  w-rou-stp-pmt-pos          pic  9(03)                  .
           05  w-rou-stp-pmt-car          pic  9(03)                  .
           05  w-rou-stp-pmt-alf          pic  x(80)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
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
      *        * Work per Det parametri di ubicazione prodotto         *
      *        *-------------------------------------------------------*
           05  w-det-prm-ubi.
      *            *---------------------------------------------------*
      *            * Literal                                           *
      *            *---------------------------------------------------*
               10  w-det-prm-ubi-lit      pic  x(28)                  .
      *            *---------------------------------------------------*
      *            * Valori in input                                   *
      *            *---------------------------------------------------*
               10  w-det-prm-ubi-dpz      pic  9(02)                  .
               10  w-det-prm-ubi-tip      pic  9(02)                  .
               10  w-det-prm-ubi-num      pic  9(07)                  .
               10  w-det-prm-ubi-var      pic  x(14)                  .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione saldi magazzino  *
      *    *-----------------------------------------------------------*
           copy      "pgm/mag/prg/cpy/dsldmag0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine cliente                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dqevroc0.dtl"                   .

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
      *    * Work-area per bufferizzazione righe                       *
      *    *-----------------------------------------------------------*
       01  w-rig.
      *        *-------------------------------------------------------*
      *        * Numero massimo di elementi                            *
      *        *-------------------------------------------------------*
           05  w-rig-max-ele              pic  9(05)     value 999    .
      *        *-------------------------------------------------------*
      *        * Contatore elementi                                    *
      *        *-------------------------------------------------------*
           05  w-rig-num-ele              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Contatori di comodo                                   *
      *        *-------------------------------------------------------*
           05  w-rig-ctr-ele              pic  9(05)                  .
           05  w-rig-ctr-001              pic  9(05)                  .
           05  w-rig-ctr-002              pic  9(05)                  .
           05  w-rig-ctr-003              pic  9(05)                  .
           05  w-rig-ctr-004              pic  9(05)                  .
      *        *-------------------------------------------------------*
      *        * Protocollo                                            *
      *        *-------------------------------------------------------*
           05  w-rig-num-prt              pic  9(11)                  .
      *        *-------------------------------------------------------*
      *        * Comodo per ridefinizione tipo riga                    *
      *        *-------------------------------------------------------*
           05  w-rig-buf-ods-wtr.
               10  w-rig-buf-ods-wtp      pic  x(01)                  .
               10  w-rig-buf-ods-wtf      pic  x(01)                  .
               10  filler                 pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Castelletto progressivi riga salvati                  *
      *        *-------------------------------------------------------*
           05  w-rig-cst-rig.
      *            *---------------------------------------------------*
      *            * Singolo elemento                                  *
      *            *---------------------------------------------------*
               10  w-rig-sng-ele  occurs 999.
      *                *-----------------------------------------------*
      *                * Chiave di ordinamento                         *
      *                *-----------------------------------------------*
                   15  w-rig-key-ord.
      *                    *-------------------------------------------*
      *                    * Chiave                                    *
      *                    *-------------------------------------------*
                       20  w-rig-key-alf  pic  x(60)                  .
      *                    *-------------------------------------------*
      *                    * Ridefinizione di tipo '00'                *
      *                    *-------------------------------------------*
                       20  w-rig-key-alf-00  redefines
                           w-rig-key-alf.
      *                        *---------------------------------------*
      *                        * Numero progressivo riga               *
      *                        *---------------------------------------*
                           25  w-rig-key-prg-00
                                          pic  9(05)                  .
      *                        *---------------------------------------*
      *                        * Riempitivo                            *
      *                        *---------------------------------------*
                           25  filler     pic  x(55)                  .
      *                    *-------------------------------------------*
      *                    * Ridefinizione di tipo '01'                *
      *                    *-------------------------------------------*
                       20  w-rig-key-alf-01  redefines
                           w-rig-key-alf.
      *                        *---------------------------------------*
      *                        * Codice classe merceologica            *
      *                        *---------------------------------------*
                           25  w-rig-key-cla-01
                                          pic  9(05)                  .
      *                        *---------------------------------------*
      *                        * Codice gruppo merceologico            *
      *                        *---------------------------------------*
                           25  w-rig-key-gru-01
                                          pic  9(05)                  .
      *                        *---------------------------------------*
      *                        * Codice sottogruppo merceologico       *
      *                        *---------------------------------------*
                           25  w-rig-key-sgr-01
                                          pic  9(05)                  .
      *                        *---------------------------------------*
      *                        * Codice magazzino                      *
      *                        *---------------------------------------*
                           25  w-rig-key-cod-01
                                          pic  x(14)                  .
      *                        *---------------------------------------*
      *                        * Numero progressivo riga               *
      *                        *---------------------------------------*
                           25  w-rig-key-prg-01
                                          pic  9(05)                  .
      *                        *---------------------------------------*
      *                        * Riempitivo                            *
      *                        *---------------------------------------*
                           25  filler     pic  x(26)                  .
      *                    *-------------------------------------------*
      *                    * Ridefinizione di tipo '03'                *
      *                    *-------------------------------------------*
                       20  w-rig-key-alf-03  redefines
                           w-rig-key-alf.
      *                        *---------------------------------------*
      *                        * Codice magazzino                      *
      *                        *---------------------------------------*
                           25  w-rig-key-cod-03
                                          pic  x(14)                  .
      *                        *---------------------------------------*
      *                        * Riempitivo                            *
      *                        *---------------------------------------*
                           25  filler     pic  x(46)                  .
      *                    *-------------------------------------------*
      *                    * Ridefinizione di tipo '04'                *
      *                    *-------------------------------------------*
                       20  w-rig-key-alf-04  redefines
                           w-rig-key-alf.
      *                        *---------------------------------------*
      *                        * Descrizione riga                      *
      *                        *---------------------------------------*
                           25  w-rig-key-der-04
                                          pic  x(40)                  .
      *                        *---------------------------------------*
      *                        * Riempitivo                            *
      *                        *---------------------------------------*
                           25  filler     pic  x(20)                  .
      *                    *-------------------------------------------*
      *                    * Ridefinizione di tipo '13'                *
      *                    *-------------------------------------------*
                       20  w-rig-key-alf-13  redefines
                           w-rig-key-alf.
      *                        *---------------------------------------*
      *                        * Indice ubicazione                     *
      *                        *                                       *
      *                        * ELETTRA                               *
      *                        *---------------------------------------*
                           25  w-rig-key-inx-13
                                          pic  9(03)                  .
      *                        *---------------------------------------*
      *                        * Sotto-ubicazione                      *
      *                        *---------------------------------------*
                           25  w-rig-key-ubi-13
                                          pic  x(01)                  .
      *                        *---------------------------------------*
      *                        * Codice alfanumerico prodotto          *
      *                        *---------------------------------------*
                           25  w-rig-key-cap-13
                                          pic  x(14)                  .
      *                        *---------------------------------------*
      *                        * Numero progressivo riga               *
      *                        *---------------------------------------*
                           25  w-rig-key-prg-13
                                          pic  9(05)                  .
      *                        *---------------------------------------*
      *                        * Riempitivo                            *
      *                        *---------------------------------------*
                           25  filler     pic  x(37)                  .
      *                *-----------------------------------------------*
      *                * Dati del buffer                               *
      *                *-----------------------------------------------*
                   15  w-rig-dti-buf.
      *                    *-------------------------------------------*
      *                    * Numero progressivo riga                   *
      *                    *-------------------------------------------*
                       20  w-rig-dti-prg  pic  9(05)       comp-3     .
      *        *-------------------------------------------------------*
      *        * Comodo per salvataggio chiave di ordinamento          *
      *        *-------------------------------------------------------*
           05  w-rig-sav-key              pic  x(60)                  .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione contatti         *
      *    *-----------------------------------------------------------*
           copy      "pgm/azi/prg/cpy/dconarc0.dtl"                   .

      *    *===========================================================*
      *    * Work-area per allineamenti a destra o a sinistra oppure   *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cpw"                   .

      *    *===========================================================*
      *    * Work per Let su archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.ltw"                   .

      *    *===========================================================*
      *    * Work per chiave di ordinamento ubicazioni                 *
      *    *-----------------------------------------------------------*
       01  w-uky.
      *        *-------------------------------------------------------*
      *        * Tabella di assegnazione                               *
      *        *-------------------------------------------------------*
           05  w-uky-ass-key.
      *            *---------------------------------------------------*
      *            * Tabella base                                      *
      *            *---------------------------------------------------*
               10  w-uky-ass-ctr          pic  9(03)                  .
               10  w-uky-ass-tbl.
                   15  filler             pic  x(03) value "A01"      .
                   15  filler             pic  x(03) value "B01"      .
                   15  filler             pic  x(03) value "A02"      .
                   15  filler             pic  x(03) value "B02"      .
                   15  filler             pic  x(03) value "A03"      .
                   15  filler             pic  x(03) value "B03"      .
                   15  filler             pic  x(03) value "A04"      .
                   15  filler             pic  x(03) value "B04"      .
                   15  filler             pic  x(03) value "A05"      .
                   15  filler             pic  x(03) value "B05"      .
                   15  filler             pic  x(03) value "A06"      .
                   15  filler             pic  x(03) value "B06"      .
                   15  filler             pic  x(03) value "A07"      .
                   15  filler             pic  x(03) value "B07"      .
                   15  filler             pic  x(03) value "B10"      .
                   15  filler             pic  x(03) value "B20"      .
                   15  filler             pic  x(03) value "C11"      .
                   15  filler             pic  x(03) value "C07"      .
                   15  filler             pic  x(03) value "D07"      .
                   15  filler             pic  x(03) value "C06"      .
                   15  filler             pic  x(03) value "D06"      .
                   15  filler             pic  x(03) value "C05"      .
                   15  filler             pic  x(03) value "D05"      .
                   15  filler             pic  x(03) value "C04"      .
                   15  filler             pic  x(03) value "D04"      .
                   15  filler             pic  x(03) value "C03"      .
                   15  filler             pic  x(03) value "D03"      .
                   15  filler             pic  x(03) value "C02"      .
                   15  filler             pic  x(03) value "D02"      .
                   15  filler             pic  x(03) value "C01"      .
                   15  filler             pic  x(03) value "D01"      .
                   15  filler             pic  x(03) value "E01"      .
                   15  filler             pic  x(03) value "F01"      .
                   15  filler             pic  x(03) value "E02"      .
                   15  filler             pic  x(03) value "F02"      .
                   15  filler             pic  x(03) value "E03"      .
                   15  filler             pic  x(03) value "F03"      .
                   15  filler             pic  x(03) value "E04"      .
                   15  filler             pic  x(03) value "F04"      .
                   15  filler             pic  x(03) value "E05"      .
                   15  filler             pic  x(03) value "F05"      .
                   15  filler             pic  x(03) value "E06"      .
                   15  filler             pic  x(03) value "F06"      .
                   15  filler             pic  x(03) value "E07"      .
                   15  filler             pic  x(03) value "F07"      .
                   15  filler             pic  x(03) value "F10"      .
                   15  filler             pic  x(03) value "F11"      .
                   15  filler             pic  x(03) value "F12"      .
                   15  filler             pic  x(03) value "F13"      .
                   15  filler             pic  x(03) value "F20"      .
                   15  filler             pic  x(03) value "F21"      .
                   15  filler             pic  x(03) value "F30"      .
                   15  filler             pic  x(03) value "F31"      .
                   15  filler             pic  x(03) value "F32"      .
                   15  filler             pic  x(03) value "F33"      .
                   15  filler             pic  x(03) value "F34"      .
                   15  filler             pic  x(03) value "F35"      .
                   15  filler             pic  x(03) value "F36"      .
                   15  filler             pic  x(03) value "H11"      .
                   15  filler             pic  x(03) value "H12"      .
                   15  filler             pic  x(03) value "G07"      .
                   15  filler             pic  x(03) value "H07"      .
                   15  filler             pic  x(03) value "G06"      .
                   15  filler             pic  x(03) value "H06"      .
                   15  filler             pic  x(03) value "G05"      .
                   15  filler             pic  x(03) value "H05"      .
                   15  filler             pic  x(03) value "G04"      .
                   15  filler             pic  x(03) value "H04"      .
                   15  filler             pic  x(03) value "G03"      .
                   15  filler             pic  x(03) value "H03"      .
                   15  filler             pic  x(03) value "G02"      .
                   15  filler             pic  x(03) value "H02"      .
                   15  filler             pic  x(03) value "G01"      .
                   15  filler             pic  x(03) value "H01"      .
                   15  filler             pic  x(03) value "I01"      .
                   15  filler             pic  x(03) value "J01"      .
                   15  filler             pic  x(03) value "I02"      .
                   15  filler             pic  x(03) value "J02"      .
                   15  filler             pic  x(03) value "I03"      .
                   15  filler             pic  x(03) value "J03"      .
                   15  filler             pic  x(03) value "I04"      .
                   15  filler             pic  x(03) value "J04"      .
                   15  filler             pic  x(03) value "I05"      .
                   15  filler             pic  x(03) value "J05"      .
                   15  filler             pic  x(03) value "I06"      .
                   15  filler             pic  x(03) value "J06"      .
                   15  filler             pic  x(03) value "I07"      .
                   15  filler             pic  x(03) value "J07"      .
                   15  filler             pic  x(03) value "I11"      .
                   15  filler             pic  x(03) value "I12"      .
                   15  filler             pic  x(03) value "I13"      .
                   15  filler             pic  x(03) value "I14"      .
                   15  filler             pic  x(03) value "J11"      .
                   15  filler             pic  x(03) value "J12"      .
                   15  filler             pic  x(03) value "L10"      .
                   15  filler             pic  x(03) value "L09"      .
                   15  filler             pic  x(03) value "L08"      .
                   15  filler             pic  x(03) value "K07"      .
                   15  filler             pic  x(03) value "L07"      .
                   15  filler             pic  x(03) value "K06"      .
                   15  filler             pic  x(03) value "L06"      .
                   15  filler             pic  x(03) value "K05"      .
                   15  filler             pic  x(03) value "L05"      .
                   15  filler             pic  x(03) value "K04"      .
                   15  filler             pic  x(03) value "L04"      .
                   15  filler             pic  x(03) value "K03"      .
                   15  filler             pic  x(03) value "L03"      .
                   15  filler             pic  x(03) value "K02"      .
                   15  filler             pic  x(03) value "L02"      .
                   15  filler             pic  x(03) value "K01"      .
                   15  filler             pic  x(03) value "L01"      .
                   15  filler             pic  x(03) value "M01"      .
                   15  filler             pic  x(03) value "N01"      .
                   15  filler             pic  x(03) value "M02"      .
                   15  filler             pic  x(03) value "N02"      .
                   15  filler             pic  x(03) value "M03"      .
                   15  filler             pic  x(03) value "N03"      .
                   15  filler             pic  x(03) value "M04"      .
                   15  filler             pic  x(03) value "N04"      .
                   15  filler             pic  x(03) value "M05"      .
                   15  filler             pic  x(03) value "N05"      .
                   15  filler             pic  x(03) value "M06"      .
                   15  filler             pic  x(03) value "N06"      .
                   15  filler             pic  x(03) value "M07"      .
                   15  filler             pic  x(03) value "N07"      .
                   15  filler             pic  x(03) value "O01"      .
                   15  filler             pic  x(03) value "P01"      .
                   15  filler             pic  x(03) value "O02"      .
                   15  filler             pic  x(03) value "P02"      .
                   15  filler             pic  x(03) value "O03"      .
                   15  filler             pic  x(03) value "P03"      .
                   15  filler             pic  x(03) value "O04"      .
                   15  filler             pic  x(03) value "P04"      .
                   15  filler             pic  x(03) value "O05"      .
                   15  filler             pic  x(03) value "P05"      .
                   15  filler             pic  x(03) value "O06"      .
                   15  filler             pic  x(03) value "P06"      .
                   15  filler             pic  x(03) value "O07"      .
                   15  filler             pic  x(03) value "P07"      .
                   15  filler             pic  x(03) value "Q01"      .
                   15  filler             pic  x(03) value "R01"      .
                   15  filler             pic  x(03) value "Q02"      .
                   15  filler             pic  x(03) value "R02"      .
                   15  filler             pic  x(03) value "Q03"      .
                   15  filler             pic  x(03) value "R03"      .
                   15  filler             pic  x(03) value "Q04"      .
                   15  filler             pic  x(03) value "R04"      .
                   15  filler             pic  x(03) value "Q05"      .
                   15  filler             pic  x(03) value "R05"      .
                   15  filler             pic  x(03) value "Q06"      .
                   15  filler             pic  x(03) value "R06"      .
                   15  filler             pic  x(03) value "Q07"      .
                   15  filler             pic  x(03) value "R07"      .
                   15  filler             pic  x(03) value "S01"      .
                   15  filler             pic  x(03) value "T01"      .
                   15  filler             pic  x(03) value "S02"      .
                   15  filler             pic  x(03) value "T02"      .
                   15  filler             pic  x(03) value "S03"      .
                   15  filler             pic  x(03) value "T03"      .
                   15  filler             pic  x(03) value "S04"      .
                   15  filler             pic  x(03) value "T04"      .
                   15  filler             pic  x(03) value "S05"      .
                   15  filler             pic  x(03) value "T05"      .
                   15  filler             pic  x(03) value "S06"      .
                   15  filler             pic  x(03) value "T06"      .
                   15  filler             pic  x(03) value "S07"      .
                   15  filler             pic  x(03) value "T07"      .
                   15  filler             pic  x(03) value "U01"      .
                   15  filler             pic  x(03) value "V01"      .
                   15  filler             pic  x(03) value "U02"      .
                   15  filler             pic  x(03) value "V02"      .
                   15  filler             pic  x(03) value "U03"      .
                   15  filler             pic  x(03) value "V03"      .
                   15  filler             pic  x(03) value "U04"      .
                   15  filler             pic  x(03) value "V04"      .
                   15  filler             pic  x(03) value "U05"      .
                   15  filler             pic  x(03) value "V05"      .
                   15  filler             pic  x(03) value "U06"      .
                   15  filler             pic  x(03) value "V06"      .
                   15  filler             pic  x(03) value "U07"      .
                   15  filler             pic  x(03) value "V07"      .
                   15  filler             pic  x(03) value "W01"      .
                   15  filler             pic  x(03) value "X01"      .
                   15  filler             pic  x(03) value "W02"      .
                   15  filler             pic  x(03) value "X02"      .
                   15  filler             pic  x(03) value "W03"      .
                   15  filler             pic  x(03) value "X03"      .
                   15  filler             pic  x(03) value "W04"      .
                   15  filler             pic  x(03) value "X04"      .
                   15  filler             pic  x(03) value "W05"      .
                   15  filler             pic  x(03) value "X05"      .
                   15  filler             pic  x(03) value "W06"      .
                   15  filler             pic  x(03) value "X06"      .
                   15  filler             pic  x(03) value "W07"      .
                   15  filler             pic  x(03) value "X07"      .
               10  w-uky-ass-tbr redefines
                   w-uky-ass-tbl.
                   15  w-uky-ass-ele occurs 194
                                 indexed       by w-uky-ass-inx       .
                       20  w-uky-ass-ubi  pic  x(03)                  .
                  
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
           move      50                   to   w-cnt-stp-lin-min      .
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
      *    * Lettura personalizzazione : Stampa ordine di spedizione   *
      *    *-----------------------------------------------------------*
       prs-stp-ods-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/ods/mov/ods300s[stp-ods]"
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
           move      00                   to   w-prs-sos-tvn          .
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
                     w-prs-sos-rac        not  = "P" and
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
       prs-stp-ods-760.
      *                  *---------------------------------------------*
      *                  * Tipo visualizzazione note                   *
      *                  *---------------------------------------------*
           if        w-prs-sos-tvn        not  = 00 and
                     w-prs-sos-tvn        not  = 01
                     move  00             to   w-prs-sos-tvn          .
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
      *    * Open files necessari alla stampa                          *
      *    *-----------------------------------------------------------*
       rou-opn-fls-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni relative alla stampa  *
      *              * ordine di spedizione                            *
      *              *-------------------------------------------------*
           perform   prs-stp-ods-000      thru prs-stp-ods-999        .
      *              *-------------------------------------------------*
      *              * Lettura delle referenze relative all'intesta-   *
      *              * zione dei box di firma                          *
      *              *-------------------------------------------------*
           perform   ref-int-frm-000      thru ref-int-frm-999        .
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
      *              * [zvf]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
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
      *              * [aaq]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * [vet]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *              *-------------------------------------------------*
      *              * [axi]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
      *              *-------------------------------------------------*
      *              * [axs]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
      *              *-------------------------------------------------*
      *              * [cbp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
      *              *-------------------------------------------------*
      *              * [mau]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
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
      *              * Open modulo di determinazione quantita' da e-   *
      *              * vadere riga ordine                              *
      *              *-------------------------------------------------*
           move      "OP"                 to   d-qev-roc-tip-ope      .
           move      "pgm/orc/prg/obj/dqevroc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-qev-roc
                                               rf-ocr                 .
       rou-opn-fls-800.
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione saldo di magazz.  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se necessario                          *
      *                  *---------------------------------------------*
           if        w-prs-sos-tvn        not  = 01
                     go to rou-opn-fls-820.
      *                  *---------------------------------------------*
      *                  * Open modulo                                 *
      *                  *---------------------------------------------*
           move      "OP"                 to   d-sld-mag-tip-ope      .
           move      "pgm/mag/prg/obj/pmag300y"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-sld-mag              .
       rou-opn-fls-820.
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione contatti          *
      *              *-------------------------------------------------*
           move      "OP"                 to   d-con-arc-tip-ope      .
           move      "pgm/azi/prg/obj/dconarc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-con-arc              .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files necessari alla stampa                         *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
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
      *              * [zvf]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
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
      *              * [aaq]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *              *-------------------------------------------------*
      *              * [vet]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *              *-------------------------------------------------*
      *              * [axi]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
      *              *-------------------------------------------------*
      *              * [axs]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
      *              *-------------------------------------------------*
      *              * [cbp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
      *              *-------------------------------------------------*
      *              * [mau]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
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
      *              * Close modulo di determinazione quantita' da e-  *
      *              * vadere riga ordine                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura modulo                             *
      *                  *---------------------------------------------*
           move      "CL"                 to   d-qev-roc-tip-ope      .
           move      "pgm/orc/prg/obj/dqevroc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-qev-roc
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Test se cancellabile                        *
      *                  *---------------------------------------------*
           move      "C?"                 to   d-qev-roc-tip-ope      .
           move      "pgm/orc/prg/obj/dqevroc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-qev-roc
                                               rf-ocr                 .
           if        d-qev-roc-exi-sts    not  = spaces
                     go to rou-cls-fls-800.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           move      "pgm/orc/prg/obj/dqevroc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       rou-cls-fls-800.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione saldo di magazz. *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se necessario                          *
      *                  *---------------------------------------------*
           if        w-prs-sos-tvn        not  = 01
                     go to rou-cls-fls-820.
      *                  *---------------------------------------------*
      *                  * Chiusura modulo                             *
      *                  *---------------------------------------------*
           move      "CL"                 to   d-sld-mag-tip-ope      .
           move      "pgm/mag/prg/obj/pmag300y"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-sld-mag              .
      *                  *---------------------------------------------*
      *                  * Test se cancellabile                        *
      *                  *---------------------------------------------*
           move      "C?"                 to   d-sld-mag-tip-ope      .
           move      "pgm/mag/prg/obj/pmag300y"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-sld-mag              .
           if        d-sld-mag-exi-sts    not  = spaces
                     go to rou-cls-fls-820.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           move      "pgm/mag/prg/obj/pmag300y"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       rou-cls-fls-820.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione contatti         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Chiusura modulo                             *
      *                  *---------------------------------------------*
           move      "CL"                 to   d-con-arc-tip-ope      .
           move      "pgm/azi/prg/obj/dconarc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-con-arc              .
      *                  *---------------------------------------------*
      *                  * Test se cancellabile                        *
      *                  *---------------------------------------------*
           move      "C?"                 to   d-con-arc-tip-ope      .
           move      "pgm/azi/prg/obj/dconarc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-con-arc              .
           if        d-con-arc-exi-sts    not  = spaces
                     go to rou-cls-fls-999.
      *                  *---------------------------------------------*
      *                  * Cancellazione modulo                        *
      *                  *---------------------------------------------*
           move      "pgm/azi/prg/obj/dconarc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           cancel    s-pat                                            .
       rou-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle referenze relative all'intestazione dei box *
      *    * di firma                                                  *
      *    *-----------------------------------------------------------*
       ref-int-frm-000.
      *              *-------------------------------------------------*
      *              * Ciclo per 3 referenze                           *
      *              *-------------------------------------------------*
           move      zero                 to   w-ref-int-frm-ctr      .
       ref-int-frm-500.
           add       1                    to   w-ref-int-frm-ctr      .
           if        w-ref-int-frm-ctr    >    3
                     go to ref-int-frm-999.
      *                  *---------------------------------------------*
      *                  * Lettura referenza multipla                  *
      *                  *---------------------------------------------*
           move      "R:"                 to   s-ope                  .
           move      w-ref-int-frm-ctr    to   s-num                  .
           move      "pgm/orc/mov/orc320[int-frm]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        s-ves                =    spaces
                     move  s-alf          to   w-ref-int-frm-des
                                              (w-ref-int-frm-ctr)
           else      move  spaces         to   w-ref-int-frm-des
                                              (w-ref-int-frm-ctr)     .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     ref-int-frm-500.
       ref-int-frm-999.
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
           move      zero                 to   w-cnt-tit-num-pag      .
       rou-stp-doc-020.
      *              *-------------------------------------------------*
      *              * Lettura record testata da stampare              *
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
      *                  *---------------------------------------------*
      *                  * Se record trovato : oltre                   *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to rou-stp-doc-040.
      *                  *---------------------------------------------*
      *                  * Se record non trovato uscita con status al  *
      *                  * valore "N"                                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-mst              .
           move      "N"                  to   w-out-mst-stp          .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
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
       rou-stp-doc-100.
      *              *-------------------------------------------------*
      *              * Intestazione pagina                             *
      *              *-------------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                  *---------------------------------------------*
      *                  * Se errore grave di i-o su stampa si esce    *
      *                  * con status di errore                        *
      *                  *---------------------------------------------*
           if        p-rsc                not  = spaces
                     move   "#"           to   w-cnt-prn-flg-int
                     go to  rou-stp-doc-999.
       rou-stp-doc-200.
      *              *-------------------------------------------------*
      *              * Stampa testata documento                        *
      *              *-------------------------------------------------*
           perform   stp-tes-doc-000      thru stp-tes-doc-999        .
      *                  *---------------------------------------------*
      *                  * Test su status di uscita                    *
      *                  *---------------------------------------------*
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
      *                  *---------------------------------------------*
      *                  * Test su status di uscita                    *
      *                  *---------------------------------------------*
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
      *                  *---------------------------------------------*
      *                  * Test su status di uscita                    *
      *                  *---------------------------------------------*
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
      *              * Preparazioni preliminari                        *
      *              *-------------------------------------------------*
       stp-tes-doc-010.
      *                  *---------------------------------------------*
      *                  * Raccolta dati intestatario documento        *
      *                  *---------------------------------------------*
           perform   stp-tes-doc-rdi-000  thru stp-tes-doc-rdi-999    .
       stp-tes-doc-100.
      *              *-------------------------------------------------*
      *              * Stampa dati raccolti                            *
      *              *-------------------------------------------------*
       stp-tes-doc-110.
      *                  *---------------------------------------------*
      *                  * Intestatario documento                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se dati da stampare                *
      *                      *-----------------------------------------*
           if        w-stp-tes-int        =    spaces
                     go to stp-tes-doc-150.
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      06                   to   p-lin                  .
           move      092                  to   p-pos                  .
           move      w-stp-tes-int-pmt    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale - 1                     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      07                   to   p-lin                  .
           move      092                  to   p-pos                  .
           move      w-stp-tes-int-rag    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale - 2                     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      08                   to   p-lin                  .
           move      092                  to   p-pos                  .
           move      w-stp-tes-int-rs2    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      09                   to   p-lin                  .
           move      092                  to   p-pos                  .
           move      w-stp-tes-int-via    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      10                   to   p-lin                  .
           move      092                  to   p-pos                  .
           move      w-stp-tes-int-loc    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-150.
      *                  *---------------------------------------------*
      *                  * Indirizzo di spedizione                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione dati da stampare           *
      *                      *-----------------------------------------*
           perform   stp-tes-doc-ism-000  thru stp-tes-doc-ism-999    .
      *                      *-----------------------------------------*
      *                      * Test se dati da stampare                *
      *                      *-----------------------------------------*
           if        w-stp-tes-ids        =    spaces
                     go to stp-tes-doc-200.
      *                      *-----------------------------------------*
      *                      * Ragione sociale - 1                     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      07                   to   p-lin                  .
           move      002                  to   p-pos                  .
           move      w-stp-tes-ids-rag    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Ragione sociale - 2                     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      08                   to   p-lin                  .
           move      002                  to   p-pos                  .
           move      w-stp-tes-ids-rs2    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      09                   to   p-lin                  .
           move      002                  to   p-pos                  .
           move      w-stp-tes-ids-via    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      10                   to   p-lin                  .
           move      002                  to   p-pos                  .
           move      w-stp-tes-ids-loc    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-200.
      *                  *---------------------------------------------*
      *                  * Condizioni di fornitura                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione dati da stampare           *
      *                      *-----------------------------------------*
           perform   stp-tes-doc-cdf-000  thru stp-tes-doc-cdf-999    .
      *                      *-----------------------------------------*
      *                      * Test se dati da stampare                *
      *                      *-----------------------------------------*
           if        w-stp-tes-cdf        =    spaces
                     go to stp-tes-doc-250.
      *                      *-----------------------------------------*
      *                      * Castelletto voci descrittive            *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      13                   to   p-lin                  .
           move      002                  to   p-pos                  .
           move      w-stp-tes-cdf-vdf (1)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      14                   to   p-lin                  .
           move      002                  to   p-pos                  .
           move      w-stp-tes-cdf-vdf (2)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      15                   to   p-lin                  .
           move      002                  to   p-pos                  .
           move      w-stp-tes-cdf-vdf (3)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      16                   to   p-lin                  .
           move      002                  to   p-pos                  .
           move      w-stp-tes-cdf-vdf (4)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      17                   to   p-lin                  .
           move      002                  to   p-pos                  .
           move      w-stp-tes-cdf-vdf (5)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      18                   to   p-lin                  .
           move      002                  to   p-pos                  .
           move      w-stp-tes-cdf-vdf (6)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-250.
      *                  *---------------------------------------------*
      *                  * Condizioni di pagamento                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se dati da stampare                *
      *                      *-----------------------------------------*
           if        w-stp-tes-cdp        =    spaces
                     go to stp-tes-doc-300.
      *                      *-----------------------------------------*
      *                      * Castelletto condizioni di pagamento     *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      13                   to   p-lin                  .
           move      047                  to   p-pos                  .
           move      w-stp-tes-cdp-cst (1)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      14                   to   p-lin                  .
           move      047                  to   p-pos                  .
           move      w-stp-tes-cdp-cst (2)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      15                   to   p-lin                  .
           move      047                  to   p-pos                  .
           move      w-stp-tes-cdp-cst (3)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      16                   to   p-lin                  .
           move      047                  to   p-pos                  .
           move      w-stp-tes-cdp-cst (4)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      17                   to   p-lin                  .
           move      047                  to   p-pos                  .
           move      w-stp-tes-cdp-cst (5)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      18                   to   p-lin                  .
           move      047                  to   p-pos                  .
           move      w-stp-tes-cdp-cst (6)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-300.
      *                  *---------------------------------------------*
      *                  * Note relative all'intestatario documento    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se dati da stampare                *
      *                      *-----------------------------------------*
           if        w-stp-tes-nri        =    spaces
                     go to stp-tes-doc-350.
      *                      *-----------------------------------------*
      *                      * Castelletto voci descrittive            *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      12                   to   p-lin                  .
           move      092                  to   p-pos                  .
           move      w-stp-tes-nri-goa (1)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      13                   to   p-lin                  .
           move      092                  to   p-pos                  .
           move      w-stp-tes-nri-goa (2)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      14                   to   p-lin                  .
           move      092                  to   p-pos                  .
           move      w-stp-tes-nri-goa (3)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      16                   to   p-lin                  .
           move      092                  to   p-pos                  .
           move      w-stp-tes-nri-goa (4)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      17                   to   p-lin                  .
           move      092                  to   p-pos                  .
           move      w-stp-tes-nri-goa (5)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      18                   to   p-lin                  .
           move      092                  to   p-pos                  .
           move      w-stp-tes-nri-goa (6)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-350.
      *                  *---------------------------------------------*
      *                  * Annotazioni per la spedizione               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione dati da stampare           *
      *                      *-----------------------------------------*
           perform   stp-tes-doc-rda-000  thru stp-tes-doc-rda-999    .
      *                      *-----------------------------------------*
      *                      * Test se dati da stampare                *
      *                      *-----------------------------------------*
           if        w-stp-tes-aps        =    spaces
                     go to stp-tes-doc-400.
      *                      *-----------------------------------------*
      *                      * Annotazioni                             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      21                   to   p-lin                  .
           move      002                  to   p-pos                  .
           move      w-stp-tes-aps-rda (1)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      22                   to   p-lin                  .
           move      002                  to   p-pos                  .
           move      w-stp-tes-aps-rda (2)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      23                   to   p-lin                  .
           move      002                  to   p-pos                  .
           move      w-stp-tes-aps-rda (3)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-400.
      *                  *---------------------------------------------*
      *                  * Vettori                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione dati da stampare           *
      *                      *-----------------------------------------*
           perform   stp-tes-doc-rdv-000  thru stp-tes-doc-rdv-999    .
      *                      *-----------------------------------------*
      *                      * Test se dati da stampare                *
      *                      *-----------------------------------------*
           if        w-stp-tes-vet        =    spaces
                     go to stp-tes-doc-450.
      *                      *-----------------------------------------*
      *                      * Ragioni sociali vettori                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      21                   to   p-lin                  .
           move      047                  to   p-pos                  .
           move      w-stp-tes-vet-rag (1)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      22                   to   p-lin                  .
           move      047                  to   p-pos                  .
           move      w-stp-tes-vet-rag (2)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      23                   to   p-lin                  .
           move      047                  to   p-pos                  .
           move      w-stp-tes-vet-rag (3)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-450.
      *                  *---------------------------------------------*
      *                  * Interlocutore e telefono                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se dati da stampare                *
      *                      *-----------------------------------------*
           if        w-stp-tes-iet        =    spaces
                     go to stp-tes-doc-800.
      *                      *-----------------------------------------*
      *                      * Interlocutore                           *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      21                   to   p-lin                  .
           move      092                  to   p-pos                  .
           move      w-stp-tes-iet-int    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                      *-----------------------------------------*
      *                      * Telefono                                *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      23                   to   p-lin                  .
           move      092                  to   p-pos                  .
           move      w-stp-tes-iet-tel    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-tes-doc-800.
      *              *-------------------------------------------------*
      *              * Vertical position per le righe corpo            *
      *              *-------------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      27                   to   p-lin                  .
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
      *    * Routine di stampa testata documento                       *
      *    *                                                           *
      *    * Subroutine di raccolta dati per l'indirizzo di spedizione *
      *    * manuale                                                   *
      *    *-----------------------------------------------------------*
       stp-tes-doc-ism-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        rf-ost-tip-ids       not  = 02
                     go to stp-tes-doc-ism-900.
       stp-tes-doc-ism-100.
      *              *-------------------------------------------------*
      *              * Valori da record aggiuntivo [osx]               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      rf-osx-rag-ids       to   w-stp-tes-ids-rag      .
           move      rf-osx-rs2-ids       to   w-stp-tes-ids-rs2      .
      *                  *---------------------------------------------*
      *                  * Indirizzo                                   *
      *                  *---------------------------------------------*
           move      rf-osx-via-ids       to   w-stp-tes-ids-via      .
      *                  *---------------------------------------------*
      *                  * Localita'                                   *
      *                  *---------------------------------------------*
           move      rf-osx-loc-ids       to   w-stp-tes-ids-loc      .
       stp-tes-doc-ism-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-tes-doc-ism-999.
       stp-tes-doc-ism-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa testata documento                       *
      *    *                                                           *
      *    * Subroutine di raccolta dati per condizioni di fornitura   *
      *    *-----------------------------------------------------------*
       stp-tes-doc-cdf-000.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        rf-ost-voc-des (1)   =    spaces and
                     rf-ost-voc-des (2)   =    spaces and
                     rf-ost-voc-des (3)   =    spaces and
                     rf-ost-voc-des (4)   =    spaces and
                     rf-ost-voc-des (5)   =    spaces and
                     rf-ost-voc-des (6)   =    spaces
                     go to stp-tes-doc-cdf-900.
       stp-tes-doc-cdf-100.
      *              *-------------------------------------------------*
      *              * Preparazione                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo per 6 voci                            *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-tes-wrk-c01      .
       stp-tes-doc-cdf-220.
           add       1                    to   w-stp-tes-wrk-c01      .
           if        w-stp-tes-wrk-c01    >    6
                     go to stp-tes-doc-cdf-900.
           if        rf-ost-voc-des
                    (w-stp-tes-wrk-c01)   =    spaces
                     go to stp-tes-doc-cdf-220.
      *                      *-----------------------------------------*
      *                      * Composizione stringa                    *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-tes-cdf-vdf
                                              (w-stp-tes-wrk-c01)     .
           move      1                    to   w-stp-tes-wrk-c02      .
      *                          *-------------------------------------*
      *                          * Prompt                              *
      *                          *-------------------------------------*
           move      w-stp-tes-wrk-c01    to   w-det-des-zdf-num      .
           move      rf-ost-cod-lng       to   w-det-des-zdf-lng      .
           perform   det-des-zdf-000      thru det-des-zdf-999        .
      *                          *-------------------------------------*
      *                          * String                              *
      *                          *-------------------------------------*
           string    w-det-des-zdf-des
                                delimited by   size
                                          into w-stp-tes-cdf-vdf
                                              (w-stp-tes-wrk-c01)
                                  with pointer w-stp-tes-wrk-c02      .
      *                          *-------------------------------------*
      *                          * ' : '                               *
      *                          *-------------------------------------*
           string    " : "      delimited by   size
                                          into w-stp-tes-cdf-vdf
                                              (w-stp-tes-wrk-c01)
                                  with pointer w-stp-tes-wrk-c02      .
      *                          *-------------------------------------*
      *                          * Valore voce descrittiva             *
      *                          *-------------------------------------*
           move      w-stp-tes-wrk-c01    to   w-det-des-zvf-num      .
           move      rf-ost-voc-des
                    (w-stp-tes-wrk-c01)   to   w-det-des-zvf-cod      .
           move      rf-ost-cod-lng       to   w-det-des-zvf-lng      .
           perform   det-des-zvf-000      thru det-des-zvf-999        .
      *                          *-------------------------------------*
      *                          * String                              *
      *                          *-------------------------------------*
           string    w-det-des-zvf-des
                                delimited by   size
                                          into w-stp-tes-cdf-vdf
                                              (w-stp-tes-wrk-c01)
                                  with pointer w-stp-tes-wrk-c02      .
       stp-tes-doc-cdf-280.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     stp-tes-doc-cdf-220.
       stp-tes-doc-cdf-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-tes-doc-cdf-999.
       stp-tes-doc-cdf-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa testata documento                       *
      *    *                                                           *
      *    * Subroutine di raccolta dati per le annotazioni            *
      *    *-----------------------------------------------------------*
       stp-tes-doc-rda-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-stp-tes-aps          .
      *              *-------------------------------------------------*
      *              * Deviazione in funzione della presenza del codi- *
      *              * ce annotazioni in testata                       *
      *              *-------------------------------------------------*
           if        rf-ost-cod-aps       =    zero
                     go to stp-tes-doc-rda-500.
       stp-tes-doc-rda-100.
      *              *-------------------------------------------------*
      *              * Se annotazioni codificate                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [zsa]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofzsa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsa                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [zsa]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODAPS    "         to   f-key                  .
           move      rf-ost-cod-aps       to   rf-zsa-cod-aps         .
           move      "pgm/ods/fls/ioc/obj/iofzsa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsa                 .
      *                      *-----------------------------------------*
      *                      * Se record non trovato : ad uscita       *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-tes-doc-rda-900.
       stp-tes-doc-rda-200.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione annotazioni codificate      *
      *                  *---------------------------------------------*
           move      rf-zsa-des-aps       to   w-stp-tes-aps          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     stp-tes-doc-rda-900.
       stp-tes-doc-rda-500.
      *              *-------------------------------------------------*
      *              * Se annotazioni non codificate                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se esistono le annotazioni nel file    *
      *                  * parallelo [osx]                             *
      *                  *---------------------------------------------*
           if        rf-osx-des-aps       =    spaces
                     go to stp-tes-doc-rda-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione annotazioni                 *
      *                  *---------------------------------------------*
           move      rf-osx-des-aps       to   w-stp-tes-aps          .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     stp-tes-doc-rda-900.
       stp-tes-doc-rda-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-tes-doc-rda-999.
       stp-tes-doc-rda-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa testata documento                       *
      *    *                                                           *
      *    * Subroutine di raccolta dati per i vettori                 *
      *    *-----------------------------------------------------------*
       stp-tes-doc-rdv-000.
      *              *-------------------------------------------------*
      *              * Test se presenti codici vettori                 *
      *              *-------------------------------------------------*
           if        rf-ost-cod-vet       =    zero
                     go to stp-tes-doc-rdv-900.
       stp-tes-doc-rdv-100.
      *              *-------------------------------------------------*
      *              * Primo vettore                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [vet]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [vet]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVET    "         to   f-key                  .
           move      rf-ost-cod-vet       to   rf-vet-cod-vet         .
           move      "pgm/bol/fls/ioc/obj/iofvet"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-vet                 .
      *                      *-----------------------------------------*
      *                      * Test su esito lettura                   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-vet-rag-soc         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione ragione sociale             *
      *                  *---------------------------------------------*
           move      rf-vet-rag-soc       to   w-stp-tes-vet-rag (1)  .
      *                  *---------------------------------------------*
      *                  * Determinazione telefono                     *
      *                  *---------------------------------------------*
           move      "DT"                 to   d-con-arc-tip-ope      .
           move      41                   to   d-con-arc-tip-arc      .
           move      rf-vet-cod-vet       to   d-con-arc-cod-arc      .
           move      spaces               to   d-con-arc-dpz-arc      .
           move      "TEL"                to   d-con-arc-tip-sel      .
           move      "pgm/azi/prg/obj/dconarc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-con-arc              .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione telefono vettore            *
      *                  *---------------------------------------------*
           if        d-con-arc-num-con (1)
                                          =    spaces
                     go to stp-tes-doc-rdv-900.
      *                      *-----------------------------------------*
      *                      * Concatenamento con prompt               *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "Telefono :"         to   w-all-str-cat (1)      .
           move      d-con-arc-num-con (1)
                                          to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-stp-tes-vet-rag (3)  .
       stp-tes-doc-rdv-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-tes-doc-rdv-999.
       stp-tes-doc-rdv-999.
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
           move      spaces               to   w-stp-tes              .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatori di comodo             *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-tes-wrk-c01      .
           move      zero                 to   w-stp-tes-wrk-c02      .
      *              *-------------------------------------------------*
      *              * Editing preliminare del codice archivio         *
      *              *-------------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           move      07                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<B"                 to   p-edm                  .
           move      rf-ost-cod-arc       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
           move      p-edt                to   w-stp-tes-int-arc      .
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
      *                  * Subroutine di raccolta dati                 *
      *                  *---------------------------------------------*
           perform   stp-tes-doc-nta-000  thru stp-tes-doc-nta-999    .
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
                                          to   w-stp-tes-int-pmt      .
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
      *    * No tipo archivio                                          *
      *    *-----------------------------------------------------------*
       stp-tes-doc-nta-000.
      *              *-------------------------------------------------*
      *              * Intestatario documento                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt                                      *
      *                  *---------------------------------------------*
           move      "       - INTESTAZIONE DOCUMENTO -       "
                                          to   w-stp-tes-int-pmt      .
       stp-tes-doc-nta-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-tes-doc-nta-999.
       stp-tes-doc-nta-999.
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
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "- CLIENTE"          to   w-all-str-cat (1)      .
           move      w-stp-tes-int-arc    to   w-all-str-cat (2)      .
           move      rf-ost-dpz-arc       to   w-all-str-cat (3)      .
           move      "-"                  to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           perform   all-str-cen-000      thru all-str-cen-999        .
           move      w-all-str-alf        to   w-stp-tes-int-pmt      .
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
      *                          * A condizioni di pagamento           *
      *                          *-------------------------------------*
           go to     stp-tes-doc-cli-200.
       stp-tes-doc-cli-170.
      *                      *-----------------------------------------*
      *                      * Manuale                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A condizioni di pagamento           *
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
      *                          * Se esiste il codice vettore abitua- *
      *                          * le 1 ed il suo numero abbonamento,  *
      *                          * si inseriscono nella seconda riga   *
      *                          * dell'anagrafica vettore             *
      *                          *-------------------------------------*
           if        rf-ost-cod-vet       =    zero
                     go to stp-tes-doc-cli-182.
           if        rf-ost-cod-vet       not  = rf-dcc-cod-vet
                     go to stp-tes-doc-cli-182.
           if        rf-dcc-abn-vtt       =    spaces
                     go to stp-tes-doc-cli-182.
      *                          *-------------------------------------*
      *                          * Concatenamento con prompt           *
      *                          *-------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "Abbonam. :"         to   w-all-str-cat (1)      .
           move      rf-dcc-abn-vtt       to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-stp-tes-vet-rag (2)  .
       stp-tes-doc-cli-182.
      *                          *-------------------------------------*
      *                          * A condizioni di pagamento           *
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
      *                          * A condizioni di pagamento           *
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
      *                          * Se esiste il codice vettore abitua- *
      *                          * le 1 ed il suo numero abbonamento,  *
      *                          * si inseriscono nella seconda riga   *
      *                          * dell'anagrafica vettore             *
      *                          *-------------------------------------*
           if        rf-ost-cod-vet       =    zero
                     go to stp-tes-doc-cli-192.
           if        rf-ost-cod-vet       not  = rf-dcc-cod-vet
                     go to stp-tes-doc-cli-192.
           if        rf-dcc-abn-vtt       =    spaces
                     go to stp-tes-doc-cli-192.
      *                          *-------------------------------------*
      *                          * Concatenamento con prompt           *
      *                          *-------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      "Abbonam. :"         to   w-all-str-cat (1)      .
           move      rf-dcc-abn-vtt       to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           move      w-all-str-alf        to   w-stp-tes-vet-rag (3)  .
       stp-tes-doc-cli-192.
      *                          *-------------------------------------*
      *                          * A condizioni di pagamento           *
      *                          *-------------------------------------*
           go to     stp-tes-doc-cli-200.
       stp-tes-doc-cli-200.
      *                  *---------------------------------------------*
      *                  * Condizioni di pagamento                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Forma di pagamento                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se valore a zero               *
      *                          *-------------------------------------*
           if        rf-ost-cod-fop       =    zero
                     go to stp-tes-doc-cli-225.
      *                          *-------------------------------------*
      *                          * Lettura record [zfp]                *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFOP"             to   f-key                  .
           move      rf-ost-cod-fop       to   rf-zfp-cod-fop         .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
           if        f-sts                not  = e-not-err
                     move  spaces         to   rf-zfp-des-fop         .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione in riga box nr. 1       *
      *                      *-----------------------------------------*
           move      rf-zfp-des-fop       to   w-stp-tes-cdp-cst (1)  .
       stp-tes-doc-cli-225.
      *                  *---------------------------------------------*
      *                  * Preparazione di:                            *
      *                  *  - Codice ABI                               *
      *                  *  - Codice CAB                               *
      *                  *  - Ns. C/C bancario                         *
      *                  * in caso di pagamento per bonifico bancario  *
      *                  *                                             *
      *                  *    oppure                                   *
      *                  *                                             *
      *                  * Preparazione di:                            *
      *                  *  - Ns. C/C postale                          *
      *                  * in caso di pagamento per c/c postale        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il codice ABI e' diverso da zero, o  *
      *                      * il codice CAB e' diverso da zero : nes- *
      *                      * suna azione                             *
      *                      *-----------------------------------------*
           if        rf-ost-cod-abi       not  = zero or
                     rf-ost-cod-cab       not  = zero
                     go to stp-tes-doc-cli-230.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che esista un co-  *
      *                      * dice ns. banca per bonifico oppure un   *
      *                      * codice nostro conto corrente postale    *
      *                      *-----------------------------------------*
           if        rf-ost-nos-ban       not  = spaces
                     go to stp-tes-doc-cli-226
           else if   rf-ost-nos-ccp       not  = spaces
                     go to stp-tes-doc-cli-227
           else      go to stp-tes-doc-cli-230.
       stp-tes-doc-cli-226.
      *                      *-----------------------------------------*
      *                      * Se nostra banca                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura nostra banca, se non trova- *
      *                          * ta : nessuna azione, previa norma-  *
      *                          * lizzazione                          *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCBP"             to   f-key                  .
           move      02                   to   rf-cbp-tip-cbp         .
           move      rf-ost-nos-ban       to   rf-cbp-cod-cbp         .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
           if        f-sts                not  = e-not-err
                     go to stp-tes-doc-cli-228.
      *                          *-------------------------------------*
      *                          * Memorizzazione codice ABI, codice   *
      *                          * CAB, e codice sigla del nostro c/c  *
      *                          * bancario                            *
      *                          *-------------------------------------*
           move    rf-cbp-abi-ban         to   rf-ost-cod-abi         .
           move    rf-cbp-cab-ban         to   rf-ost-cod-cab         .
           move    rf-cbp-sgl-ccb         to   rf-ost-ccc-app         .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to   stp-tes-doc-cli-230.
       stp-tes-doc-cli-227.
      *                      *-----------------------------------------*
      *                      * Se nostro c/c postale                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Lettura nostro c/c postale, se non  *
      *                          * trovato : nessuna azione, previa    *
      *                          * normalizzazione                     *
      *                          *-------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCBP"             to   f-key                  .
           move      03                   to   rf-cbp-tip-cbp         .
           move      rf-ost-nos-ccp       to   rf-cbp-cod-cbp         .
           move      "pgm/gep/fls/ioc/obj/iofcbp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cbp                 .
           if        f-sts                not  = e-not-err
                     go to stp-tes-doc-cli-228.
      *                          *-------------------------------------*
      *                          * Memorizzazione sigla del nostro c/c *
      *                          * postale                             *
      *                          * bancario                            *
      *                          *-------------------------------------*
           move    rf-cbp-sgl-ccp         to   rf-ost-ccc-app         .
      *                          *-------------------------------------*
      *                          * Continuazione                       *
      *                          *-------------------------------------*
           go to   stp-tes-doc-cli-230.
       stp-tes-doc-cli-228.
      *                      *-----------------------------------------*
      *                      * Normalizzazione                         *
      *                      *-----------------------------------------*
           move    zero                   to   rf-ost-cod-abi         .
           move    zero                   to   rf-ost-cod-cab         .
           move    spaces                 to   rf-ost-ccc-app         .
       stp-tes-doc-cli-230.
      *                  *---------------------------------------------*
      *                  * Descrizione ABI                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se valore a zero                   *
      *                      *-----------------------------------------*
           if        rf-ost-cod-abi       =    zero
                     go to stp-tes-doc-cli-232.
      *                      *-----------------------------------------*
      *                      * Lettura record [axi]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODABI"             to   f-key                  .
           move      rf-ost-cod-abi       to   rf-axi-cod-abi         .
           move      "pgm/abi/fls/ioc/obj/iofaxi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axi                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-axi-den-abi         .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione in riga box nr. 2       *
      *                      *-----------------------------------------*
           move      rf-axi-den-abi       to   w-stp-tes-cdp-cst (2)  .
       stp-tes-doc-cli-232.
      *                  *---------------------------------------------*
      *                  * Descrizione CAB                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se valore a zero                   *
      *                      *-----------------------------------------*
           if        rf-ost-cod-cab       =    zero
                     go to stp-tes-doc-cli-234.
      *                      *-----------------------------------------*
      *                      * Lettura record [axs]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "ABICAB"             to   f-key                  .
           move      rf-ost-cod-abi       to   rf-axs-cod-abi         .
           move      rf-ost-cod-cab       to   rf-axs-cod-cab         .
           move      "pgm/abi/fls/ioc/obj/iofaxs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-axs                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-axs-den-spt         .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione in riga box nr. 3       *
      *                      *-----------------------------------------*
           move      rf-axs-den-spt       to   w-stp-tes-cdp-cst (3)  .
       stp-tes-doc-cli-234.
      *                  *---------------------------------------------*
      *                  * C/C appoggio                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se valore a spazi                  *
      *                      *-----------------------------------------*
           if        rf-ost-ccc-app       =    spaces
                     go to stp-tes-doc-cli-300.
      *                      *-----------------------------------------*
      *                      * Composizione stringa                    *
      *                      *-----------------------------------------*
           move      spaces               to   w-stp-tes-doc-str      .
           move      1                    to   w-stp-tes-wrk-c02      .
      *                      *-----------------------------------------*
      *                      * Literal                                 *
      *                      *-----------------------------------------*
           string    "C/C"      delimited by   size
                                          into w-stp-tes-doc-str
                                  with pointer w-stp-tes-wrk-c02      .
      *                      *-----------------------------------------*
      *                      * Descrizione                             *
      *                      *-----------------------------------------*
           string    rf-ost-ccc-app
                                delimited by   size
                                          into w-stp-tes-doc-str
                                  with pointer w-stp-tes-wrk-c02      .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione in riga box nr. 4       *
      *                      *-----------------------------------------*
           move      w-stp-tes-doc-str    to   w-stp-tes-cdp-cst (4)  .
       stp-tes-doc-cli-300.
      *                  *---------------------------------------------*
      *                  * Note relative all'intestatario documento    *
      *                  *---------------------------------------------*
       stp-tes-doc-cli-310.
      *                      *-----------------------------------------*
      *                      * Test sul valore della relativa persona- *
      *                      * lizzazione                              *
      *                      *-----------------------------------------*
           if        w-prs-sos-bl1        not  = 11
                     go to stp-tes-doc-cli-320.
      *                      *-----------------------------------------*
      *                      * Editing giorni di chiusura e orario di  *
      *                      * apertura                                *
      *                      *-----------------------------------------*
           move      "S"                  to   w-gch-oap-inp-sng      .
           move      "S"                  to   w-gch-oap-inp-sno      .
           move      zero                 to   w-gch-oap-inp-gn1      .
           move      zero                 to   w-gch-oap-inp-gn2      .
           move      rf-dcc-gdl-nls       to   w-gch-oap-gdl-nls      .
           move      rf-dcc-oam-oin       to   w-gch-oap-oam-oin      .
           move      rf-dcc-oam-ofi       to   w-gch-oap-oam-ofi      .
           move      rf-dcc-oap-oin       to   w-gch-oap-oap-oin      .
           move      rf-dcc-oap-ofi       to   w-gch-oap-oap-ofi      .
           perform   edt-gch-oap-000      thru edt-gch-oap-999        .
      *                      *-----------------------------------------*
      *                      * Se valore editato non esistente : oltre *
      *                      *-----------------------------------------*
           if        w-gch-oap-gch-edt    =    spaces
                     go to stp-tes-doc-cli-320.
      *                      *-----------------------------------------*
      *                      * Se un solo giorno di chiusura, stampa   *
      *                      * in posizione centrale                   *
      *                      *-----------------------------------------*
           if        w-gch-oap-gch-edx (2)
                                          =    spaces and
                     w-gch-oap-gch-edx (3)
                                          =    spaces
                     move  w-gch-oap-gch-edx (1)
                                          to   w-gch-oap-gch-edx (2)
                     move  spaces         to   w-gch-oap-gch-edx (1)  .
      *                      *-----------------------------------------*
      *                      * Memorizzazione                          *
      *                      *-----------------------------------------*
           move      w-gch-oap-gch-edx (1)
                                          to   w-stp-tes-nri-goa (1)  .
           move      w-gch-oap-gch-edx (2)
                                          to   w-stp-tes-nri-goa (2)  .
           move      w-gch-oap-gch-edx (3)
                                          to   w-stp-tes-nri-goa (3)  .
       stp-tes-doc-cli-320.
      *                      *-----------------------------------------*
      *                      * Test sul valore della relativa persona- *
      *                      * lizzazione                              *
      *                      *-----------------------------------------*
           if        w-prs-sos-bl2        not  = 12
                     go to stp-tes-doc-cli-350.
      *                      *-----------------------------------------*
      *                      * Editing giorni di chiusura e orario di  *
      *                      * apertura                                *
      *                      *-----------------------------------------*
           move      rf-dcc-gdl-nls       to   w-gch-oap-gdl-nls      .
           move      rf-dcc-oam-oin       to   w-gch-oap-oam-oin      .
           move      rf-dcc-oam-ofi       to   w-gch-oap-oam-ofi      .
           move      rf-dcc-oap-oin       to   w-gch-oap-oap-oin      .
           move      rf-dcc-oap-ofi       to   w-gch-oap-oap-ofi      .
           perform   edt-gch-oap-000      thru edt-gch-oap-999        .
      *                      *-----------------------------------------*
      *                      * Se valore editato non esistente : oltre *
      *                      *-----------------------------------------*
           if        w-gch-oap-oap-edt    =    spaces
                     go to stp-tes-doc-cli-350.
      *                      *-----------------------------------------*
      *                      * Memorizzazione                          *
      *                      *-----------------------------------------*
           move      w-gch-oap-oap-edt    to   w-stp-tes-nri-goa (4)  .
       stp-tes-doc-cli-350.
      *                  *---------------------------------------------*
      *                  * Annotazioni per la spedizione               *
      *                  *---------------------------------------------*
       stp-tes-doc-cli-400.
      *                  *---------------------------------------------*
      *                  * Vettori                                     *
      *                  *---------------------------------------------*
       stp-tes-doc-cli-450.
      *                  *---------------------------------------------*
      *                  * Interlocutore e telefono                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione                          *
      *                      *-----------------------------------------*
           move      "DT"                 to   d-con-arc-tip-ope      .
           move      02                   to   d-con-arc-tip-arc      .
           move      rf-dcc-cod-cli       to   d-con-arc-cod-arc      .
           move      rf-dcc-dpz-cli       to   d-con-arc-dpz-arc      .
           move      "TEL"                to   d-con-arc-tip-sel      .
           move      "pgm/azi/prg/obj/dconarc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-con-arc              .
      *                      *-----------------------------------------*
      *                      * Interlocutore                           *
      *                      *-----------------------------------------*
           move      d-con-arc-int-con (1)
                                          to   w-stp-tes-iet-int      .
      *                      *-----------------------------------------*
      *                      * Telefono                                *
      *                      *-----------------------------------------*
           move      d-con-arc-num-con (1)
                                          to   w-stp-tes-iet-tel      .
       stp-tes-doc-cli-900.
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
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      "- FORNITORE"        to   w-all-str-cat (1)      .
           move      w-stp-tes-int-arc    to   w-all-str-cat (2)      .
           move      rf-ost-dpz-arc       to   w-all-str-cat (3)      .
           move      "-"                  to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           perform   all-str-cen-000      thru all-str-cen-999        .
           move      w-all-str-alf        to   w-stp-tes-int-pmt      .
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
      *                  * Normalizzazione [ada]                       *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/azi/fls/ioc/obj/iofada"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ada                 .
      *                  *---------------------------------------------*
      *                  * Lettura [ada] dati anagrafici Dipendenza    *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODDPZ    "         to   f-key                  .
           move      rf-ost-cod-arc       to   rf-ada-cod-dpz         .
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
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "- NS. DIPENDENZA"   to   w-all-str-cat (1)      .
           move      w-stp-tes-int-arc    to   w-all-str-cat (2)      .
           move      "-"                  to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
           perform   all-str-cen-000      thru all-str-cen-999        .
           move      w-all-str-alf        to   w-stp-tes-int-pmt      .
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
      *                      * Deviazione in funzione del tipo indi-   *
      *                      * rizzo di spedizione                     *
      *                      *-----------------------------------------*
           if        rf-ost-tip-ids       not  = 01
                     go to stp-tes-doc-dpz-900.
       stp-tes-doc-dpz-152.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
           move      rf-ada-rag-soc       to   w-stp-tes-ids-rag      .
           move      spaces               to   w-stp-tes-ids-rs2      .
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
           move      rf-ada-via-azi       to   w-stp-tes-ids-via      .
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
           move      rf-ada-loc-azi       to   w-stp-tes-ids-loc      .
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
      *              * Preparazione castelletto righe in memoria       *
      *              *-------------------------------------------------*
           move      w-inp-mst-prt        to   w-rig-num-prt          .
           perform   stp-cor-doc-pcr-000  thru stp-cor-doc-pcr-999    .
      *              *-------------------------------------------------*
      *              * Contatore elementi                              *
      *              *-------------------------------------------------*
           move      zero                 to   w-rig-ctr-ele          .
      *              *-------------------------------------------------*
      *              * Test su numero elementi letti                   *
      *              *-------------------------------------------------*
           if        w-rig-num-ele        >    zero
                     go to stp-cor-doc-200.
       stp-cor-doc-100.
      *                  *---------------------------------------------*
      *                  * Se Start non ottenuta uscita con status al  *
      *                  * valore "N"                                  *
      *                  *---------------------------------------------*
           move      spaces               to   w-out-mst              .
           move      "N"                  to   w-out-mst-stp          .
           perform   wrt-out-mst-000      thru wrt-out-mst-999        .
           go to     stp-cor-doc-999.
       stp-cor-doc-200.
      *              *-------------------------------------------------*
      *              * Incremento contatore righe                      *
      *              *-------------------------------------------------*
           add       1                    to   w-rig-ctr-ele          .
      *              *-------------------------------------------------*
      *              * Test su contatore righe                         *
      *              *-------------------------------------------------*
           if        w-rig-ctr-ele        >    w-rig-num-ele
                     go to stp-cor-doc-900.
           if        w-rig-ctr-ele        >    w-rig-max-ele
                     go to stp-cor-doc-900.
           if        w-rig-dti-prg
                    (w-rig-ctr-ele)       =    zero
                     go to stp-cor-doc-200.
       stp-cor-doc-300.
      *              *-------------------------------------------------*
      *              * Lettura riga in corso di trattamento            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-rig-num-prt        to   rf-osr-num-prt         .
           move      w-rig-dti-prg
                    (w-rig-ctr-ele)       to   rf-osr-num-prg         .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata : riciclo                 *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-cor-doc-200.
       stp-cor-doc-400.
      *              *-------------------------------------------------*
      *              * Stampa riga corpo                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero riga corpo in corso       *
      *                  * di stampa                                   *
      *                  *---------------------------------------------*
           add       1                    to   w-stp-rig-ctr-rcs      .
      *                  *---------------------------------------------*
      *                  * Subroutine per la verifica se necessaria la *
      *                  * reintestazione                              *
      *                  *---------------------------------------------*
           perform   stp-cor-doc-vnr-000  thru stp-cor-doc-vnr-999    .
       stp-cor-doc-420.
      *                  *---------------------------------------------*
      *                  * Tipo riga in comodo ridefinito              *
      *                  *---------------------------------------------*
           move      rf-osr-tip-rig       to   w-rdf-tip-rig-wtr      .
      *                  *---------------------------------------------*
      *                  * Codice alfanumerico di magazzino            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da stampare                     *
      *                      *-----------------------------------------*
           if        w-rdf-tip-rig-wtp    not  = "P" or
                     w-rdf-tip-rig-wtf    not  = spaces
                     go to stp-cor-doc-440.
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      14                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           add       w-stp-rig-ctr-rcs    to   p-lin                  .
           move      002                  to   p-pos                  .
           move      rf-osr-alf-pro       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-440.
      *                  *---------------------------------------------*
      *                  * Descrizione                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione relativa alla riga ordine   *
      *                      * in esame in area di comodo              *
      *                      *-----------------------------------------*
           perform   det-des-rig-000      thru det-des-rig-999        .
      *                      *-----------------------------------------*
      *                      * Stampa prima riga di descrizione        *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           add       w-stp-rig-ctr-rcs    to   p-lin                  .
           move      017                  to   p-pos                  .
           move      w-stp-rig-des-rgd (1)
                                          to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-500.
      *                  *---------------------------------------------*
      *                  * Codice prodotto per la casa produttrice     *
      *                  *                                             *
      *                  * ELETTRA (6 caratteri)                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record [aaq]            *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                      *-----------------------------------------*
      *                      * Lettura record [aaq]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      rf-osr-tip-mag       to   rf-aaq-tip-mag         .
           move      rf-osr-num-pro       to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           add       w-stp-rig-ctr-rcs    to   p-lin                  .
           move      058                  to   p-pos                  .
           move      rf-aaq-cdp-pdt       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-550.
      *                  *---------------------------------------------*
      *                  * Riferimenti all'ordine                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se riferimenti esistenti           *
      *                      *-----------------------------------------*
           if        rf-osr-coc-num       =    zero and
                     rf-osr-coc-dat       =    zero
                     go to stp-cor-doc-600.
      *                      *-----------------------------------------*
      *                      * Numero ordine                           *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "P"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "B"                  to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           add       w-stp-rig-ctr-rcs    to   p-lin                  .
           move      065                  to   p-pos                  .
           move      rf-osr-coc-num       to   w-stp-int-ndo          .
           move      w-stp-int-ndo-saa    to   w-stp-int-npr-saa      .
           move      w-stp-int-ndo-prg    to   w-stp-int-npr-prg      .
           move      w-stp-int-npr        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-600.
      *                  *---------------------------------------------*
      *                  * Quantita' residua                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se riferimenti all'ordine cliente  *
      *                      * esistenti                               *
      *                      *-----------------------------------------*
           if        rf-osr-coc-prt       =    zero or
                     rf-osr-coc-prg       =    zero
                     go to stp-cor-doc-650.
      *                      *-----------------------------------------*
      *                      * Test su tipo riga                       *
      *                      *-----------------------------------------*
           if        w-rdf-tip-rig-wtp    =    "A" or
                     w-rdf-tip-rig-wtp    =    "C"
                     go to stp-cor-doc-650.
      *                      *-----------------------------------------*
      *                      * Lettura record [ocr]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      rf-osr-coc-prt       to   rf-ocr-num-prt         .
           move      rf-osr-coc-prg       to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                      *-----------------------------------------*
      *                      * Se lettura errata : oltre               *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-cor-doc-650.
      *                      *-----------------------------------------*
      *                      * Determinazione quantita' evasa riga     *
      *                      * ordine cliente                          *
      *                      *-----------------------------------------*
           move      "DT"                 to   d-qev-roc-tip-ope      .
           move      "pgm/orc/prg/obj/dqevroc0"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-qev-roc
                                               rf-ocr                 .
      *                      *-----------------------------------------*
      *                      * Lettura riga in corso di trattamento    *
      *                      * per il ripristino in quanto la deter-   *
      *                      * minazione appena effettuata potrebbe    *
      *                      * avere modificato il record [osr] in     *
      *                      * corso di trattamento                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-rig-num-prt        to   rf-osr-num-prt         .
           move      w-rig-dti-prg
                    (w-rig-ctr-ele)       to   rf-osr-num-prg         .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                          *-------------------------------------*
      *                          * Se lettura errata : riciclo         *
      *                          *-------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-cor-doc-200.
      *                      *-----------------------------------------*
      *                      * Aggiustamento quantita' da evadere con  *
      *                      * quantita' da spedire                    *
      *                      *-----------------------------------------*
           add       rf-osr-qta-ven       to   d-qev-roc-qta-dev      .
      *                      *-----------------------------------------*
      *                      * Stampa quantita' da evadere             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           if        d-qev-roc-qta-dev    >    99999 or
                     d-qev-roc-qta-dev    <   -99999
                     move  06             to   p-car
           else      move  05             to   p-car                  .
           move      rf-osr-dec-qta       to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           if        d-qev-roc-qta-dev    >    99999 or
                     d-qev-roc-qta-dev    <   -99999
                     move  "B"            to   p-edm
           else      move  "GB"           to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           add       w-stp-rig-ctr-rcs    to   p-lin                  .
           move      075                  to   p-pos                  .
           move      d-qev-roc-qta-dev    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-650.
      *                  *---------------------------------------------*
      *                  * Quantita' da spedire                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           if        rf-osr-qta-ven       >    99999 or
                     rf-osr-qta-ven       <   -99999
                     move  06             to   p-car
           else      move  05             to   p-car                  .
           move      rf-osr-dec-qta       to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           if        rf-osr-qta-ven       >    99999 or
                     rf-osr-qta-ven       <   -99999
                     move  "B"            to   p-edm
           else      move  "GB"           to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           add       w-stp-rig-ctr-rcs    to   p-lin                  .
           move      086                  to   p-pos                  .
           move      rf-osr-qta-ven       to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-700.
      *                  *---------------------------------------------*
      *                  * Giacenza                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da eseguire                     *
      *                      *-----------------------------------------*
           if        w-rdf-tip-rig-wtp    not  = "P" or
                     w-rdf-tip-rig-wtf    not  = spaces
                     go to stp-cor-doc-750.
      *                      *-----------------------------------------*
      *                      * Data di sistema                         *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Preparazione link-area                  *
      *                      *-----------------------------------------*
           move      "SL"                 to   d-sld-mag-tip-ope      .
           move      0000                 to   d-sld-mag-tip-sld      .
           move      s-dat                to   d-sld-mag-dat-sld      .
           move      "U"                  to   d-sld-mag-uot-dpz      .
           move      rf-osr-cod-dpz       to   d-sld-mag-cod-dpz      .
           move      01                   to   d-sld-mag-tip-mag      .
           move      rf-osr-num-pro       to   d-sld-mag-num-mag      .
           move      "T"                  to   d-sld-mag-uot-var      .
           move      spaces               to   d-sld-mag-var-mag      .
           move      "T"                  to   d-sld-mag-uot-dsl      .
           move      spaces               to   d-sld-mag-cod-dsl      .
      *                      *-----------------------------------------*
      *                      * Richiamo del sottoprogramma             *
      *                      *-----------------------------------------*
           move      "pgm/mag/prg/obj/pmag300y"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using d-sld-mag              .
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "N"                  to   p-tip                  .
           if        d-sld-mag-sld-mag    >    99999 or
                     d-sld-mag-sld-mag    <   -99999
                     move  06             to   p-car
           else      move  05             to   p-car                  .
           move      rf-osr-dec-qta       to   p-dec                  .
           move      "S"                  to   p-sgn                  .
           if        d-sld-mag-sld-mag    >    99999 or
                     d-sld-mag-sld-mag    <   -99999
                     move  spaces         to   p-edm
           else      move  "G"            to   p-edm                  .
           move      p-lnr                to   p-lin                  .
           add       w-stp-rig-ctr-rcs    to   p-lin                  .
           move      098                  to   p-pos                  .
           move      d-sld-mag-sld-mag    to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-750.
      *                  *---------------------------------------------*
      *                  * Ubicazione                                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da eseguire                     *
      *                      *-----------------------------------------*
           if        w-rdf-tip-rig-wtp    not  = "P" or
                     w-rdf-tip-rig-wtf    not  = spaces
                     go to stp-cor-doc-800.
      *                      *-----------------------------------------*
      *                      * Determinazione ubicazione               *
      *                      *                                         *
      *                      * ELETTRA - solo Sede                     *
      *                      *-----------------------------------------*
______*    move      rf-osr-cod-dpz       to   w-det-prm-ubi-dpz      .
           move      01                   to   w-det-prm-ubi-dpz      .
           move      01                   to   w-det-prm-ubi-tip      .
           move      rf-osr-num-pro       to   w-det-prm-ubi-num      .
           move      rf-osr-sgl-vrn       to   w-det-prm-ubi-var      .
           perform   det-prm-ubi-000      thru det-prm-ubi-999        .
      *                      *-----------------------------------------*
      *                      * Parametri di ubicazione                 *
      *                      *                                         *
      *                      * ELETTRA : parametri rovesci             *
      *                      *-----------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      24                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           add       w-stp-rig-ctr-rcs    to   p-lin                  .
           move      108                  to   p-pos                  .
           move      w-det-prm-ubi-lit    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-800.
      *                  *---------------------------------------------*
      *                  * Eventuale stampa delle righe di descrizione *
      *                  * aggiuntive                                  *
      *                  *---------------------------------------------*
           perform   stp-cor-doc-rdd-000  thru stp-cor-doc-rdd-999    .
       stp-cor-doc-850.
      *              *-------------------------------------------------*
      *              * A record successivo                             *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-200.
       stp-cor-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-999.
       stp-cor-doc-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di preparazione castelletto righe in memoria   *
      *    *-----------------------------------------------------------*
       stp-cor-doc-pcr-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore elementi              *
      *              *-------------------------------------------------*
           move      zero                 to   w-rig-num-ele          .
       stp-cor-doc-pcr-100.
      *              *-------------------------------------------------*
      *              * Start su righe documento [osr]                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-rig-num-prt        to   rf-osr-num-prt         .
           move      zero                 to   rf-osr-num-prg         .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                  *---------------------------------------------*
      *                  * Test su esito della start                   *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-cor-doc-pcr-900.
       stp-cor-doc-pcr-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale file [osr]                  *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
      *                  *---------------------------------------------*
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-cor-doc-pcr-900.
       stp-cor-doc-pcr-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-osr-num-prt       not  = w-rig-num-prt
                     go to stp-cor-doc-pcr-900.
       stp-cor-doc-pcr-400.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-rig-num-ele          .
      *              *-------------------------------------------------*
      *              * Test sul contatore                              *
      *              *-------------------------------------------------*
           if        w-rig-num-ele        >    w-rig-max-ele
                     go to stp-cor-doc-pcr-900.
      *              *-------------------------------------------------*
      *              * Ridefinizione tipo riga                         *
      *              *-------------------------------------------------*
           move      rf-osr-tip-rig       to   w-rig-buf-ods-wtr      .
      *              *-------------------------------------------------*
      *              * Preparazione chiave e dati per il buffer        *
      *              *-------------------------------------------------*
           perform   stp-cor-doc-obk-000  thru stp-cor-doc-obk-999    .
       stp-cor-doc-pcr-600.
      *              *-------------------------------------------------*
      *              * Riciclo a riga successiva                       *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-pcr-200.
       stp-cor-doc-pcr-900.
      *              *-------------------------------------------------*
      *              * Ordinamento finale delle righe bufferizzate     *
      *              *-------------------------------------------------*
           perform   stp-cor-doc-obr-000  thru stp-cor-doc-obr-999    .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-pcr-999.
       stp-cor-doc-pcr-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Ordinamento righe bufferizzate                            *
      *    *-----------------------------------------------------------*
       stp-cor-doc-obr-000.
      *              *-------------------------------------------------*
      *              * Test se almeno due codici da ordinare           *
      *              *-------------------------------------------------*
           if        w-rig-num-ele        <    2
                     go to stp-cor-doc-obr-999.
       stp-cor-doc-obr-050.
      *              *-------------------------------------------------*
      *              * Ciclo di ordinamento                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-rig-ctr-001          .
       stp-cor-doc-obr-100.
           add       1                    to   w-rig-ctr-001          .
           if        w-rig-ctr-001        =    w-rig-num-ele
                     go to stp-cor-doc-obr-999.
           move      w-rig-ctr-001        to   w-rig-ctr-002
                                               w-rig-ctr-003          .
           move      w-rig-key-ord
                    (w-rig-ctr-001)       to   w-rig-sav-key          .
       stp-cor-doc-obr-200.
           add       1                    to   w-rig-ctr-002          .
           if        w-rig-ctr-002        >    w-rig-num-ele
                     go to stp-cor-doc-obr-300.
           if        w-rig-key-ord
                    (w-rig-ctr-002)       >    w-rig-sav-key
                     go to stp-cor-doc-obr-200.
           move      w-rig-ctr-002        to   w-rig-ctr-003          .
           move      w-rig-key-ord
                    (w-rig-ctr-002)       to   w-rig-sav-key          .
           go to     stp-cor-doc-obr-200.
       stp-cor-doc-obr-300.
           move      w-rig-ctr-001        to   w-rig-ctr-004          .          
           if        w-rig-sav-key        >    w-rig-key-ord
                                              (w-rig-ctr-004)
                     go to stp-cor-doc-obr-100.
           move      w-rig-sng-ele
                    (w-rig-ctr-003)       to   w-rig-sng-ele (999)    .
           move      w-rig-sng-ele
                    (w-rig-ctr-004)       to   w-rig-sng-ele
                                              (w-rig-ctr-003)         .
           move      w-rig-sng-ele (999)  to   w-rig-sng-ele
                                              (w-rig-ctr-004)         .
           go to     stp-cor-doc-obr-100.
       stp-cor-doc-obr-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Preparazione chiave per bufferizzazione righe             *
      *    *-----------------------------------------------------------*
       stp-cor-doc-obk-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare chiave              *
      *              *-------------------------------------------------*
           move      spaces               to   w-rig-key-alf
                                              (w-rig-num-ele)         .
      *              *-------------------------------------------------*
      *              * Normalizzazione preliminare dati                *
      *              *-------------------------------------------------*
           move      zero                 to   w-rig-dti-prg
                                              (w-rig-num-ele)         .
       stp-cor-doc-obk-003.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione delle personalizzazioni  *
      *              *-------------------------------------------------*
           if        w-prs-sos-orr        =    00
                     go to stp-cor-doc-obk-005
           else if   w-prs-sos-orr        =    01
                     go to stp-cor-doc-obk-010
           else if   w-prs-sos-orr        =    02
                     go to stp-cor-doc-obk-020
           else if   w-prs-sos-orr        =    03
                     go to stp-cor-doc-obk-030
           else if   w-prs-sos-orr        =    04
                     go to stp-cor-doc-obk-040
           else if   w-prs-sos-orr        =    10
                     go to stp-cor-doc-obk-100
           else if   w-prs-sos-orr        =    13
                     go to stp-cor-doc-obk-130
           else if   w-prs-sos-orr        =    14
                     go to stp-cor-doc-obk-140
           else      go to stp-cor-doc-obk-005.
       stp-cor-doc-obk-005.
      *              *-------------------------------------------------*
      *              * Se ordinamento per progressivo riga             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione chiave di ordinamento          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione progressivo riga        *
      *                      *-----------------------------------------*
           move      rf-osr-num-prg       to   w-rig-key-prg-00
                                              (w-rig-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     stp-cor-doc-obk-900.
       stp-cor-doc-obk-010.
      *              *-------------------------------------------------*
      *              * Se ordinamento per C.G.S. e codice prodotto     *
      *              *                                                 *
      *              * N.B.: Ordinamento attualmente non implementato  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo riga                           *
      *                  *---------------------------------------------*
           if        rf-osr-tip-rig (1:1) not  = "P" or
                     rf-osr-tip-rig (2:1) not  = spaces
                     move  high-value     to   w-rig-key-alf-01
                                              (w-rig-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Lettura record [dcp]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO    "         to   f-key                  .
           move      rf-osr-num-pro       to   rf-dcp-num-pro         .
           move      "pgm/dcp/fls/ioc/obj/iofdcp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcp                 .
      *                  *---------------------------------------------*
      *                  * Se lettura errata                           *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  99999          to   rf-dcp-cla-pro
                     move  99999          to   rf-dcp-gru-pro
                     move  99999          to   rf-dcp-sgr-pro         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione codice classe               *
      *                  *---------------------------------------------*
           move      rf-dcp-cla-pro       to   w-rig-key-cla-01
                                              (w-rig-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione codice gruppo               *
      *                  *---------------------------------------------*
           move      rf-dcp-gru-pro       to   w-rig-key-gru-01
                                              (w-rig-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione codice sottogruppo          *
      *                  *---------------------------------------------*
           move      rf-dcp-sgr-pro       to   w-rig-key-sgr-01
                                              (w-rig-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione codice prodotto             *
      *                  *---------------------------------------------*
           move      rf-osr-alf-pro       to   w-rig-key-cod-01
                                              (w-rig-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione progressivo riga            *
      *                  *---------------------------------------------*
           move      rf-osr-num-prg       to   w-rig-key-prg-01
                                              (w-rig-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     stp-cor-doc-obk-900.
       stp-cor-doc-obk-020.
      *              *-------------------------------------------------*
      *              * Se ordinamento per C.G.S. e descrizione prodot- *
      *              * to                                              *
      *              *                                                 *
      *              * N.B.: Ordinamento attualmente non implementato  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     stp-cor-doc-obk-900.
       stp-cor-doc-obk-030.
      *              *-------------------------------------------------*
      *              * Se ordinamento per codice prodotto              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione chiave di ordinamento          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione codice magazzino        *
      *                      *-----------------------------------------*
           move      rf-osr-alf-pro       to   w-rig-key-cod-03
                                              (w-rig-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     stp-cor-doc-obk-900.
       stp-cor-doc-obk-040.
      *              *-------------------------------------------------*
      *              * Se ordinamento per descrizione prodotto         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione chiave di ordinamento          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Descrizione relativa alla riga ordine   *
      *                      * in esame in area di comodo              *
      *                      *-----------------------------------------*
           perform   det-des-rig-000      thru det-des-rig-999        .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione descrizione riga        *
      *                      *-----------------------------------------*
           move      w-stp-rig-des-rgd (1)
                                          to   w-rig-key-der-04
                                              (w-rig-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     stp-cor-doc-obk-900.
       stp-cor-doc-obk-100.
      *              *-------------------------------------------------*
      *              * Se ordinamento per percorso di ubicazione,      *
      *              * progressivo riga                                *
      *              *                                                 *
      *              * N.B.: Ordinamento attualmente non implementato  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     stp-cor-doc-obk-900.
       stp-cor-doc-obk-130.
      *              *-------------------------------------------------*
      *              * Se ordinamento per percorso di ubicazione,      *
      *              * codice prodotto                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione chiave di ordinamento          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se tipo riga non "P"                    *
      *                      *-----------------------------------------*
           if        w-rig-buf-ods-wtp    =    "P"
                     go to stp-cor-doc-obk-132.
      *                      *-----------------------------------------*
      *                      * Indice ubicazione                       *
      *                      *                                         *
      *                      * Elettra                                 *
      *                      *-----------------------------------------*
           move      999                  to   w-rig-key-inx-13
                                              (w-rig-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione ubicazione              *
      *                      *-----------------------------------------*
           move      all "z"              to   w-rig-key-ubi-13
                                              (w-rig-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione codice prodotto         *
      *                      *-----------------------------------------*
           move      all "z"              to   w-rig-key-cap-13
                                              (w-rig-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione progressivo riga        *
      *                      *-----------------------------------------*
           move      rf-osr-num-prg       to   w-rig-key-prg-13
                                              (w-rig-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     stp-cor-doc-obk-900.
       stp-cor-doc-obk-132.
      *                      *-----------------------------------------*
      *                      * Determinazione ubicazione               *
      *                      *                                         *
      *                      * ELETTRA - solo Sede                     *
      *                      *-----------------------------------------*
______*    move      rf-osr-cod-dpz       to   w-det-prm-ubi-dpz      .
           move      01                   to   w-det-prm-ubi-dpz      .
           move      01                   to   w-det-prm-ubi-tip      .
           move      rf-osr-num-pro       to   w-det-prm-ubi-num      .
           move      rf-osr-sgl-vrn       to   w-det-prm-ubi-var      .
           perform   det-prm-ubi-000      thru det-prm-ubi-999        .
      *                      *-----------------------------------------*
      *                      * Aggiustamento ubicazione                *
      *                      *-----------------------------------------*
           if        w-det-prm-ubi-lit    =    spaces
                     move  all "z"        to   w-det-prm-ubi-lit      .
      *                      *-----------------------------------------*
      *                      * Indice ubicazione                       *
      *                      *                                         *
      *                      * ELETTRA                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ricerca elemento                    *
      *                          *-------------------------------------*
           set       w-uky-ass-inx        to   1                      .
           search    w-uky-ass-ele
                     when  w-uky-ass-ubi
                          (w-uky-ass-inx) =    w-det-prm-ubi-lit
                                              (01 : 03)
                     move  w-uky-ass-inx  to   w-rig-key-inx-13
                                              (w-rig-num-ele)         .
           if        w-rig-key-inx-13
                    (w-rig-num-ele)       =    zero
                     move  999            to   w-rig-key-inx-13
                                              (w-rig-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione sotto-ubicazione        *
      *                      *-----------------------------------------*
           move      w-det-prm-ubi-lit
                    (04 : 01)             to   w-rig-key-ubi-13
                                              (w-rig-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione codice prodotto         *
      *                      *-----------------------------------------*
           move      rf-osr-alf-pro       to   w-rig-key-cap-13
                                              (w-rig-num-ele)         .
      *                      *-----------------------------------------*
      *                      * Bufferizzazione progressivo riga        *
      *                      *-----------------------------------------*
           move      rf-osr-num-prg       to   w-rig-key-prg-13
                                              (w-rig-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     stp-cor-doc-obk-900.
       stp-cor-doc-obk-140.
      *              *-------------------------------------------------*
      *              * Se ordinamento per percorso di ubicazione,      *
      *              * descrizione prodotto                            *
      *              *                                                 *
      *              * N.B.: Ordinamento attualmente non implementato  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     stp-cor-doc-obk-900.
       stp-cor-doc-obk-900.
      *              *-------------------------------------------------*
      *              * Preparazione dati di ordinamento                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Bufferizzazione progressivo riga            *
      *                  *---------------------------------------------*
           move      rf-osr-num-prg       to   w-rig-dti-prg
                                              (w-rig-num-ele)         .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-obk-999.
       stp-cor-doc-obk-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine di verifica se necessaria reintestazione ed    *
      *    * eventuale sua esecuzione                                  *
      *    *-----------------------------------------------------------*
       stp-cor-doc-vnr-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero di pagina in  *
      *              * corso di trattamento                            *
      *              *-------------------------------------------------*
           if        w-cnt-tit-num-pag    >    1
                     go to stp-cor-doc-vnr-200.
       stp-cor-doc-vnr-100.
      *              *-------------------------------------------------*
      *              * Se pagina in corso di trattamento : 1           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sul numero di riga in corso di stampa  *
      *                  *---------------------------------------------*
           if        w-stp-rig-ctr-rcs    <    37
                     go to stp-cor-doc-vnr-900.
      *                  *---------------------------------------------*
      *                  * A fase comune                               *
      *                  *---------------------------------------------*
           go to     stp-cor-doc-vnr-500.
       stp-cor-doc-vnr-200.
      *              *-------------------------------------------------*
      *              * Se pagina in corso di trattamento oltre la pri- *
      *              * ma                                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test sul numero di riga in corso di stampa  *
      *                  *---------------------------------------------*
           if        w-stp-rig-ctr-rcs    <    57
                     go to stp-cor-doc-vnr-900.
      *                  *---------------------------------------------*
      *                  * A fase comune                               *
      *                  *---------------------------------------------*
           go to     stp-cor-doc-vnr-500.
       stp-cor-doc-vnr-500.
      *              *-------------------------------------------------*
      *              * Fase comune                                     *
      *              *-------------------------------------------------*
       stp-cor-doc-vnr-520.
      *                  *---------------------------------------------*
      *                  * Preparazione contatore riga in corso di     *
      *                  * stampa                                      *
      *                  *---------------------------------------------*
           move      1                    to   w-stp-rig-ctr-rcs      .
      *                  *---------------------------------------------*
      *                  * Stampa del literal per il 'segue'           *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      13                   to   p-car                  .
           move      64                   to   p-lin                  .
           move      118                  to   p-pos                  .
           move      "*** segue ***"      to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-vnr-540.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                      *-----------------------------------------*
      *                      * Se flag di interruzione                 *
      *                      *-----------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to stp-cor-doc-vnr-999.
       stp-cor-doc-vnr-580.
      *                  *---------------------------------------------*
      *                  * Vertical position                           *
      *                  *---------------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      08                   to   p-lin                  .
           add       w-dsp-dsp-vrt        to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-vnr-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-vnr-999.
       stp-cor-doc-vnr-999.
           exit.

      *    *===========================================================*
      *    * Stampa corpo documento                                    *
      *    *                                                           *
      *    * Subroutine per la stampa delle righe di descrizione       *
      *    *-----------------------------------------------------------*
       stp-cor-doc-rdd-000.
      *              *-------------------------------------------------*
      *              * Stampa delle righe di descrizione               *
      *              *-------------------------------------------------*
       stp-cor-doc-rdd-010.
      *                  *---------------------------------------------*
      *                  * Test preliminari                            *
      *                  *---------------------------------------------*
           if        rf-osr-des-ext       =    0
                     go to stp-cor-doc-rdd-600.
       stp-cor-doc-rdd-100.
      *                  *---------------------------------------------*
      *                  * Preparazioni preliminari                    *
      *                  *---------------------------------------------*
       stp-cor-doc-rdd-120.
      *                      *-----------------------------------------*
      *                      * Determinazione quante righe di descri-  *
      *                      * zione ancora da stampare                *
      *                      *-----------------------------------------*
           move      11                   to   w-stp-rig-des-ctr      .
       stp-cor-doc-rdd-200.
           subtract  1                    from w-stp-rig-des-ctr      .
           if        w-stp-rig-des-ctr    =    zero
                     go to stp-cor-doc-rdd-220.
           if        w-stp-rig-des-rgd
                    (w-stp-rig-des-ctr)   =    spaces
                     go to stp-cor-doc-rdd-200.
       stp-cor-doc-rdd-220.
      *                      *-----------------------------------------*
      *                      * Numero di righe da stampare             *
      *                      *-----------------------------------------*
           move      w-stp-rig-des-ctr    to   w-stp-rig-des-nrd      .
       stp-cor-doc-rdd-300.
      *                  *---------------------------------------------*
      *                  * Ciclo per stampa                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione contatore               *
      *                      *-----------------------------------------*
           move      1                    to   w-stp-rig-des-ctr      .
       stp-cor-doc-rdd-400.
      *                      *-----------------------------------------*
      *                      * Incremento contatore                    *
      *                      *-----------------------------------------*
           add       1                    to   w-stp-rig-des-ctr      .
      *                      *-----------------------------------------*
      *                      * Test sul contatore                      *
      *                      *-----------------------------------------*
           if        w-stp-rig-des-ctr    >    w-stp-rig-des-nrd
                     go to stp-cor-doc-rdd-600.
      *                      *-----------------------------------------*
      *                      * Incremento numero riga corpo in corso   *
      *                      * di stampa                               *
      *                      *-----------------------------------------*
           add       1                    to   w-stp-rig-ctr-rcs      .
       stp-cor-doc-rdd-420.
      *                  *---------------------------------------------*
      *                  * Subroutine per la verifica se necessaria la *
      *                  * reintestazione                              *
      *                  *---------------------------------------------*
           perform   stp-cor-doc-vnr-000  thru stp-cor-doc-vnr-999    .
       stp-cor-doc-rdd-430.
      *                  *---------------------------------------------*
      *                  * Stampa riga di descrizione                  *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           add       w-stp-rig-ctr-rcs    to   p-lin                  .
           move      017                  to   p-pos                  .
           move      w-stp-rig-des-rgd
                    (w-stp-rig-des-ctr)   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-rdd-470.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     stp-cor-doc-rdd-400.
       stp-cor-doc-rdd-600.
      *              *-------------------------------------------------*
      *              * Stampa eventuale riga aggiuntiva per il codice  *
      *              * prodotto per la casa produttrice                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su valore personalizzazione            *
      *                  *---------------------------------------------*
           if        w-prs-sos-rac        not  = "P"
                     go to stp-cor-doc-rdd-700.
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [aaq]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [aaq]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRO"             to   f-key                  .
           move      rf-osr-tip-mag       to   rf-aaq-tip-mag         .
           move      rf-osr-num-pro       to   rf-aaq-num-pro         .
           move      "pgm/dcf/fls/ioc/obj/iofaaq"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-aaq                 .
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to stp-cor-doc-rdd-700.
      *                  *---------------------------------------------*
      *                  * Test su valore del campo letto              *
      *                  *---------------------------------------------*
           if        rf-aaq-cdp-pdt       =    spaces
                     go to stp-cor-doc-rdd-700.
      *                  *---------------------------------------------*
      *                  * Incremento numero riga corpo in corso di    *
      *                  * stampa                                      *
      *                  *---------------------------------------------*
           add       1                    to   w-stp-rig-ctr-rcs      .
      *                  *---------------------------------------------*
      *                  * Subroutine per la verifica se necessaria la *
      *                  * reintestazione                              *
      *                  *---------------------------------------------*
           perform   stp-cor-doc-vnr-000  thru stp-cor-doc-vnr-999    .
      *                  *---------------------------------------------*
      *                  * Stampa codice prodotto per la casa produt-  *
      *                  * trice                                       *
      *                  *---------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      40                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           add       w-stp-rig-ctr-rcs    to   p-lin                  .
           move      017                  to   p-pos                  .
           move      rf-aaq-cdp-pdt       to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       stp-cor-doc-rdd-700.
       stp-cor-doc-rdd-800.
      *              *-------------------------------------------------*
      *              * Stampa eventuali interlinee aggiuntive          *
      *              *-------------------------------------------------*
       stp-cor-doc-rdd-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-cor-doc-rdd-999.
       stp-cor-doc-rdd-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa piede documento                         *
      *    *-----------------------------------------------------------*
       stp-pie-doc-000.
       stp-pie-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     stp-pie-doc-999.
       stp-pie-doc-999.
           exit.

      *    *===========================================================*
      *    * Intestazione foglio                                       *
      *    *-----------------------------------------------------------*
       int-pag-sta-000.
      *              *-------------------------------------------------*
      *              * Composizione titolo                             *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tit-des-tit      .
           move      1                    to   w-stp-int-pnt          .
       int-pag-sta-100.
      *              *-------------------------------------------------*
      *              * Descrizione documento                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura record [zsc]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTOS    "         to   f-key                  .
           move      rf-ost-cod-tms       to   rf-zsc-cod-tos         .
           move      "pgm/ods/fls/ioc/obj/iofzsc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsc                 .
           if        f-sts                not  = e-not-err
                     move  all "."        to   rf-zsc-des-tos         .
      *                  *---------------------------------------------*
      *                  * String                                      *
      *                  *---------------------------------------------*
           string    rf-zsc-des-tos
                                delimited by   size
                                          into w-cnt-tit-des-tit
                                  with pointer w-stp-int-pnt          .
      *                      *-----------------------------------------*
      *                      * Riposizionamento puntatore all'ultimo   *
      *                      * carattere non-blank                     *
      *                      *-----------------------------------------*
           move      zero                 to   w-stp-int-pnt          .
           inspect   w-cnt-tit-des-tit    
                                      tallying w-stp-int-pnt
                                  for trailing spaces                 .
           subtract  w-stp-int-pnt        from 80
                                        giving w-stp-int-pnt          .
           add       1                    to   w-stp-int-pnt          .
       int-pag-sta-300.
      *              *-------------------------------------------------*
      *              * Numero documento                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Numero documento in work ridefinito         *
      *                  *---------------------------------------------*
           move      rf-ost-num-prt       to   w-stp-int-ndo          .
           move      w-stp-int-ndo-saa    to   w-stp-int-npr-saa      .
           move      w-stp-int-ndo-prg    to   w-stp-int-npr-prg      .
      *                  *---------------------------------------------*
      *                  * Editing numero progressivo                  *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "P"                  to   p-tip                  .
           move      06                   to   p-car                  .
           move      zero                 to   p-dec                  .
           move      spaces               to   p-sgn                  .
           move      "<"                  to   p-edm                  .
           move      w-stp-int-npr        to   p-num                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * String                                      *
      *                  *---------------------------------------------*
           string    " nr. "
                                delimited by   size
                     p-edt
                                delimited by   spaces
                                          into w-cnt-tit-des-tit
                                  with pointer w-stp-int-pnt          .
       int-pag-sta-400.
      *              *-------------------------------------------------*
      *              * Data documento                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing data documento                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   p-ope                  .
           move      "D"                  to   p-tip                  .
           move      rf-ost-dat-doc       to   p-dat                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                  *---------------------------------------------*
      *                  * String                                      *
      *                  *---------------------------------------------*
           string    " del "
                                delimited by   size
                     p-edt
                                delimited by   spaces
                                          into w-cnt-tit-des-tit
                                  with pointer w-stp-int-pnt          .
       int-pag-sta-500.
      *              *-------------------------------------------------*
      *              * Data di stampa                                  *
      *              *-------------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           move      s-dat                to   w-cnt-tit-dat-stp      .
      *              *-------------------------------------------------*
      *              * Numero pagina                                   *
      *              *-------------------------------------------------*
           add       1                    to   w-cnt-tit-num-pag      .
       int-pag-sta-600.
      *              *-------------------------------------------------*
      *              * Intestazione standard                           *
      *              *-------------------------------------------------*
           perform   int-pag-std-000      thru int-pag-std-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per interruzione forzata          *
      *                  *---------------------------------------------*
           if        w-cnt-prn-flg-int    not  = spaces
                     go to  int-pag-sta-999.
       int-pag-sta-800.
      *              *-------------------------------------------------*
      *              * Definizione della pagina di stampa              *
      *              *-------------------------------------------------*
           perform   int-pag-sta-dps-000  thru   int-pag-sta-dps-999  .
       int-pag-sta-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     int-pag-sta-999.
       int-pag-sta-999.
           exit.

      *    *===========================================================*
      *    * Intestazione foglio                                       *
      *    *                                                           *
      *    * Subroutine per la definizione della pagina di stampa      *
      *    *-----------------------------------------------------------*
       int-pag-sta-dps-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del numero di pagina in  *
      *              * corso di stampa                                 *
      *              *-------------------------------------------------*
           if        w-cnt-tit-num-pag    >    1
                     go to int-pag-sta-dps-140.
      *              *-------------------------------------------------*
      *              * Preparazione dell'intera pagina di stampa       *
      *              *-------------------------------------------------*
       int-pag-sta-dps-120.
      *                  *---------------------------------------------*
      *                  * Testata documento                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Box per Indirizzo di spedizione         *
      *                      *-----------------------------------------*
           move      005                  to   w-box-int-pag-lin      .
           move      001                  to   w-box-int-pag-pin      .
           move      011                  to   w-box-int-pag-lfi      .
           move      042                  to   w-box-int-pag-pfi      .
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Intestazione cliente            *
      *                      *-----------------------------------------*
           move      005                  to   w-box-int-pag-lin      .
           move      091                  to   w-box-int-pag-pin      .
           move      011                  to   w-box-int-pag-lfi      .
           move      132                  to   w-box-int-pag-pfi      .
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Condizioni di fornitura         *
      *                      *-----------------------------------------*
           move      011                  to   w-box-int-pag-lin      .
           move      001                  to   w-box-int-pag-pin      .
           move      019                  to   w-box-int-pag-lfi      .
           move      042                  to   w-box-int-pag-pfi      .
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Condizioni di pagamento         *
      *                      *-----------------------------------------*
           move      011                  to   w-box-int-pag-lin      .
           move      046                  to   w-box-int-pag-pin      .
           move      019                  to   w-box-int-pag-lfi      .
           move      087                  to   w-box-int-pag-pfi      .
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Riferimenti vari - parte 1      *
      *                      *-----------------------------------------*
           move      011                  to   w-box-int-pag-lin      .
           move      091                  to   w-box-int-pag-pin      .
           move      015                  to   w-box-int-pag-lfi      .
           move      132                  to   w-box-int-pag-pfi      .
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Riferimenti vari - parte 2      *
      *                      *-----------------------------------------*
           move      015                  to   w-box-int-pag-lin      .
           move      091                  to   w-box-int-pag-pin      .
           move      019                  to   w-box-int-pag-lfi      .
           move      132                  to   w-box-int-pag-pfi      .
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Annotazioni per la spedizione   *
      *                      *-----------------------------------------*
           move      019                  to   w-box-int-pag-lin      .
           move      001                  to   w-box-int-pag-pin      .
           move      024                  to   w-box-int-pag-lfi      .
           move      042                  to   w-box-int-pag-pfi      .
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Vettori                         *
      *                      *-----------------------------------------*
           move      019                  to   w-box-int-pag-lin      .
           move      046                  to   w-box-int-pag-pin      .
           move      024                  to   w-box-int-pag-lfi      .
           move      087                  to   w-box-int-pag-pfi      .
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Interlocutore e telefono        *
      *                      *-----------------------------------------*
           move      019                  to   w-box-int-pag-lin      .
           move      091                  to   w-box-int-pag-pin      .
           move      024                  to   w-box-int-pag-lfi      .
           move      132                  to   w-box-int-pag-pfi      .
           perform   box-int-pag-000      thru box-int-pag-999        .
       int-pag-sta-dps-140.
      *                  *---------------------------------------------*
      *                  * Corpo documento - box per prompts           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Box per Codice prodotto                 *
      *                      *-----------------------------------------*
           move      025                  to   w-box-int-pag-lin      .
           move      001                  to   w-box-int-pag-pin      .
           move      027                  to   w-box-int-pag-lfi      .
           move      016                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Descrizione prodotto            *
      *                      *-----------------------------------------*
           move      025                  to   w-box-int-pag-lin      .
           move      016                  to   w-box-int-pag-pin      .
           move      027                  to   w-box-int-pag-lfi      .
           move      064                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Riferimenti ordine              *
      *                      *-----------------------------------------*
           move      025                  to   w-box-int-pag-lin      .
           move      064                  to   w-box-int-pag-pin      .
           move      027                  to   w-box-int-pag-lfi      .
           move      074                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Quantita' residua               *
      *                      *-----------------------------------------*
           move      025                  to   w-box-int-pag-lin      .
           move      074                  to   w-box-int-pag-pin      .
           move      027                  to   w-box-int-pag-lfi      .
           move      084                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Quantita' da spedire            *
      *                      *-----------------------------------------*
           move      025                  to   w-box-int-pag-lin      .
           move      084                  to   w-box-int-pag-pin      .
           move      027                  to   w-box-int-pag-lfi      .
           move      097                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Spunta                          *
      *                      *-----------------------------------------*
______*    move      025                  to   w-box-int-pag-lin      .
______*    move      087                  to   w-box-int-pag-pin      .
______*    move      027                  to   w-box-int-pag-lfi      .
______*    move      097                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
______*    perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
______*    perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Giacenza                        *
      *                      *-----------------------------------------*
           move      025                  to   w-box-int-pag-lin      .
           move      097                  to   w-box-int-pag-pin      .
           move      027                  to   w-box-int-pag-lfi      .
           move      107                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Ubicazione                      *
      *                      *-----------------------------------------*
           move      025                  to   w-box-int-pag-lin      .
           move      107                  to   w-box-int-pag-pin      .
           move      027                  to   w-box-int-pag-lfi      .
           move      132                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
           perform   box-int-pag-000      thru box-int-pag-999        .
       int-pag-sta-dps-150.
      *                  *---------------------------------------------*
      *                  * Corpo documento - colonne                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Box per Codice prodotto                 *
      *                      *-----------------------------------------*
           move      027                  to   w-box-int-pag-lin      .
           move      001                  to   w-box-int-pag-pin      .
           move      065                  to   w-box-int-pag-lfi      .
           move      016                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Descrizione prodotto            *
      *                      *-----------------------------------------*
           move      027                  to   w-box-int-pag-lin      .
           move      016                  to   w-box-int-pag-pin      .
           move      065                  to   w-box-int-pag-lfi      .
           move      064                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Riferimenti ordine              *
      *                      *-----------------------------------------*
           move      027                  to   w-box-int-pag-lin      .
           move      064                  to   w-box-int-pag-pin      .
           move      065                  to   w-box-int-pag-lfi      .
           move      074                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Quantita' residua               *
      *                      *-----------------------------------------*
           move      027                  to   w-box-int-pag-lin      .
           move      074                  to   w-box-int-pag-pin      .
           move      065                  to   w-box-int-pag-lfi      .
           move      084                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Quantita' da spedire            *
      *                      *-----------------------------------------*
           move      027                  to   w-box-int-pag-lin      .
           move      084                  to   w-box-int-pag-pin      .
           move      065                  to   w-box-int-pag-lfi      .
           move      097                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Spunta                          *
      *                      *-----------------------------------------*
______*    move      027                  to   w-box-int-pag-lin      .
______*    move      087                  to   w-box-int-pag-pin      .
______*    move      065                  to   w-box-int-pag-lfi      .
______*    move      097                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
______*    perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
______*    perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Giacenza                        *
      *                      *-----------------------------------------*
           move      027                  to   w-box-int-pag-lin      .
           move      097                  to   w-box-int-pag-pin      .
           move      065                  to   w-box-int-pag-lfi      .
           move      107                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per Ubicazione                      *
      *                      *-----------------------------------------*
           move      027                  to   w-box-int-pag-lin      .
           move      107                  to   w-box-int-pag-pin      .
           move      065                  to   w-box-int-pag-lfi      .
           move      132                  to   w-box-int-pag-pfi      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa box                          *
      *                          *-------------------------------------*
           perform   box-int-pag-000      thru box-int-pag-999        .
       int-pag-sta-dps-160.
      *                  *---------------------------------------------*
      *                  * Piede documento                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Box per firma 1                         *
      *                      *-----------------------------------------*
           move      066                  to   w-box-int-pag-lin      .
           move      001                  to   w-box-int-pag-pin      .
           move      071                  to   w-box-int-pag-lfi      .
           move      045                  to   w-box-int-pag-pfi      .
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per firma 2                         *
      *                      *-----------------------------------------*
           move      066                  to   w-box-int-pag-lin      .
           move      045                  to   w-box-int-pag-pin      .
           move      071                  to   w-box-int-pag-lfi      .
           move      088                  to   w-box-int-pag-pfi      .
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Box per firma 3                         *
      *                      *-----------------------------------------*
           move      066                  to   w-box-int-pag-lin      .
           move      088                  to   w-box-int-pag-pin      .
           move      071                  to   w-box-int-pag-lfi      .
           move      132                  to   w-box-int-pag-pfi      .
           perform   box-int-pag-000      thru box-int-pag-999        .
      *                      *-----------------------------------------*
      *                      * Prompt per modello ISO                  *
      *                      *                                         *
      *                      * ELETTRA                                 *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Vertical position                   *
      *                          *-------------------------------------*
           move      "VP"                 to   p-ope                  .
           move      72                   to   p-lin                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
      *                          *-------------------------------------*
      *                          * Stampa                              *
      *                          *-------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      16                   to   p-car                  .
           move      p-lnr                to   p-lin                  .
           move      001                  to   p-pos                  .
           move      "(MOD.007 rev 00)"   to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       int-pag-sta-dps-200.
      *              *-------------------------------------------------*
      *              * Prompts per i box di stampa                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del numero di pagina *
      *                  * in corso di stampa                          *
      *                  *---------------------------------------------*
           if        w-cnt-tit-num-pag    >    1
                     go to int-pag-sta-dps-240.
       int-pag-sta-dps-220.
      *                  *---------------------------------------------*
      *                  * Testata documento                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Indirizzo di spedizione                 *
      *                      *-----------------------------------------*
           move      "- INDIRIZZO DI SPEDIZIONE -"
                                          to   w-rou-stp-pmt-alf      .
           move      27                   to   w-rou-stp-pmt-car      .
           move      06                   to   w-rou-stp-pmt-lin      .
           move      008                  to   w-rou-stp-pmt-pos      .
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
      *                      *-----------------------------------------*
      *                      * Intestazione documento                  *
      *                      *-----------------------------------------*
      *                      *-----------------------------------------*
      *                      * Condizioni di fornitura                 *
      *                      *-----------------------------------------*
           move      "- CONDIZIONI DI FORNITURA -"
                                          to   w-rou-stp-pmt-alf      .
           move      27                   to   w-rou-stp-pmt-car      .
           move      12                   to   w-rou-stp-pmt-lin      .
           move      008                  to   w-rou-stp-pmt-pos      .
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
      *                      *-----------------------------------------*
      *                      * Condizioni di pagamento                 *
      *                      *-----------------------------------------*
           move      "- CONDIZIONI DI PAGAMENTO -"
                                          to   w-rou-stp-pmt-alf      .
           move      27                   to   w-rou-stp-pmt-car      .
           move      12                   to   w-rou-stp-pmt-lin      .
           move      053                  to   w-rou-stp-pmt-pos      .
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
      *                      *-----------------------------------------*
      *                      * Box per Riferimenti vari - parte 1      *
      *                      *-----------------------------------------*
      *                      *-----------------------------------------*
      *                      * Box per Riferimenti vari - parte 2      *
      *                      *-----------------------------------------*
      *                      *-----------------------------------------*
      *                      * Annotazioni per la spedizione           *
      *                      *-----------------------------------------*
           move      "- ANNOTAZIONI PER LA SPEDIZIONE -"
                                          to   w-rou-stp-pmt-alf      .
           move      33                   to   w-rou-stp-pmt-car      .
           move      20                   to   w-rou-stp-pmt-lin      .
           move      006                  to   w-rou-stp-pmt-pos      .
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
      *                      *-----------------------------------------*
      *                      * Vettori                                 *
      *                      *-----------------------------------------*
           move      "- VETTORE -"        to   w-rou-stp-pmt-alf      .
           move      11                   to   w-rou-stp-pmt-car      .
           move      20                   to   w-rou-stp-pmt-lin      .
           move      061                  to   w-rou-stp-pmt-pos      .
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
      *                      *-----------------------------------------*
      *                      * Interlocutore                           *
      *                      *-----------------------------------------*
           move      "- INTERLOCUTORE -"  to   w-rou-stp-pmt-alf      .
           move      17                   to   w-rou-stp-pmt-car      .
           move      20                   to   w-rou-stp-pmt-lin      .
           move      103                  to   w-rou-stp-pmt-pos      .
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
      *                      *-----------------------------------------*
      *                      * Telefono                                *
      *                      *-----------------------------------------*
           move      "- TELEFONO -"       to   w-rou-stp-pmt-alf      .
           move      12                   to   w-rou-stp-pmt-car      .
           move      22                   to   w-rou-stp-pmt-lin      .
           move      106                  to   w-rou-stp-pmt-pos      .
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
       int-pag-sta-dps-240.
      *                  *---------------------------------------------*
      *                  * Corpo documento                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice prodotto                         *
      *                      *-----------------------------------------*
           move      "Codice"             to   w-rou-stp-pmt-alf      .
           move      06                   to   w-rou-stp-pmt-car      .
           move      26                   to   w-rou-stp-pmt-lin      .
           move      006                  to   w-rou-stp-pmt-pos      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa prompt                       *
      *                          *-------------------------------------*
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
      *                      *-----------------------------------------*
      *                      * Descrizione prodotto                    *
      *                      *-----------------------------------------*
           move      "Descrizione"        to   w-rou-stp-pmt-alf      .
           move      11                   to   w-rou-stp-pmt-car      .
           move      26                   to   w-rou-stp-pmt-lin      .
           move      035                  to   w-rou-stp-pmt-pos      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa prompt                       *
      *                          *-------------------------------------*
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
      *                      *-----------------------------------------*
      *                      * Riferimenti ordine                      *
      *                      *-----------------------------------------*
           move      "Ordine"             to   w-rou-stp-pmt-alf      .
           move      06                   to   w-rou-stp-pmt-car      .
           move      26                   to   w-rou-stp-pmt-lin      .
           move      066                  to   w-rou-stp-pmt-pos      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa prompt                       *
      *                          *-------------------------------------*
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
      *                      *-----------------------------------------*
      *                      * Quantita' residua                       *
      *                      *-----------------------------------------*
           move      "Residuo"            to   w-rou-stp-pmt-alf      .
           move      07                   to   w-rou-stp-pmt-car      .
           move      26                   to   w-rou-stp-pmt-lin      .
           move      076                  to   w-rou-stp-pmt-pos      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa prompt                       *
      *                          *-------------------------------------*
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
      *                      *-----------------------------------------*
      *                      * Quantita' da  spedire                   *
      *                      *-----------------------------------------*
           move      "Da spedire"         to   w-rou-stp-pmt-alf      .
           move      10                   to   w-rou-stp-pmt-car      .
           move      26                   to   w-rou-stp-pmt-lin      .
           move      086                  to   w-rou-stp-pmt-pos      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa prompt                       *
      *                          *-------------------------------------*
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
      *                      *-----------------------------------------*
      *                      * Spunta                                  *
      *                      *-----------------------------------------*
      *                      *-----------------------------------------*
      *                      * Giacenza                                *
      *                      *-----------------------------------------*
           move      "Giacenza"           to   w-rou-stp-pmt-alf      .
           move      08                   to   w-rou-stp-pmt-car      .
           move      26                   to   w-rou-stp-pmt-lin      .
           move      098                  to   w-rou-stp-pmt-pos      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa prompt                       *
      *                          *-------------------------------------*
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
      *                      *-----------------------------------------*
      *                      * Ubicazione                              *
      *                      *-----------------------------------------*
           move      "Ubicazione"         to   w-rou-stp-pmt-alf      .
           move      10                   to   w-rou-stp-pmt-car      .
           move      26                   to   w-rou-stp-pmt-lin      .
           move      115                  to   w-rou-stp-pmt-pos      .
      *                          *-------------------------------------*
      *                          * Correttivo per pagine successive    *
      *                          * alla prima                          *
      *                          *-------------------------------------*
           perform   rou-stp-psp-000      thru rou-stp-psp-999        .
      *                          *-------------------------------------*
      *                          * Stampa prompt                       *
      *                          *-------------------------------------*
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
       int-pag-sta-dps-260.
      *                  *---------------------------------------------*
      *                  * Piede documento                             *
      *                  *---------------------------------------------*
       int-pag-sta-dps-280.
      *                      *-----------------------------------------*
      *                      * Eventuale prompt per box firma 1        *
      *                      *-----------------------------------------*
           if        w-ref-int-frm-des (1)
                                          =    spaces
                     go to int-pag-sta-dps-300.
      *                      *-----------------------------------------*
      *                      * Box firma 1                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Allineamento al centro              *
      *                          *-------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      w-ref-int-frm-des (1)
                                          to   w-all-str-alf          .
           perform   all-str-cen-000      thru all-str-cen-999        .
      *                          *-------------------------------------*
      *                          * Subroutine di stampa                *
      *                          *-------------------------------------*
           move      w-all-str-alf        to   w-rou-stp-pmt-alf      .
           move      40                   to   w-rou-stp-pmt-car      .
           move      67                   to   w-rou-stp-pmt-lin      .
           move      003                  to   w-rou-stp-pmt-pos      .
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
       int-pag-sta-dps-300.
      *                      *-----------------------------------------*
      *                      * Eventuale prompt per box firma 2        *
      *                      *-----------------------------------------*
           if        w-ref-int-frm-des (2)
                                          =    spaces
                     go to int-pag-sta-dps-320.
      *                      *-----------------------------------------*
      *                      * Box firma 2                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Allineamento al centro              *
      *                          *-------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      w-ref-int-frm-des (2)
                                          to   w-all-str-alf          .
           perform   all-str-cen-000      thru all-str-cen-999        .
      *                          *-------------------------------------*
      *                          * Subroutine di stampa                *
      *                          *-------------------------------------*
           move      w-all-str-alf        to   w-rou-stp-pmt-alf      .
           move      40                   to   w-rou-stp-pmt-car      .
           move      67                   to   w-rou-stp-pmt-lin      .
           move      047                  to   w-rou-stp-pmt-pos      .
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
       int-pag-sta-dps-320.
      *                      *-----------------------------------------*
      *                      * Eventuale prompt per box firma 3        *
      *                      *-----------------------------------------*
           if        w-ref-int-frm-des (3)
                                          =    spaces
                     go to int-pag-sta-dps-400.
      *                      *-----------------------------------------*
      *                      * Box firma 3                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Allineamento al centro              *
      *                          *-------------------------------------*
           move      40                   to   w-all-str-lun          .
           move      w-ref-int-frm-des (3)
                                          to   w-all-str-alf          .
           perform   all-str-cen-000      thru all-str-cen-999        .
      *                          *-------------------------------------*
      *                          * Subroutine di stampa                *
      *                          *-------------------------------------*
           move      w-all-str-alf        to   w-rou-stp-pmt-alf      .
           move      40                   to   w-rou-stp-pmt-car      .
           move      67                   to   w-rou-stp-pmt-lin      .
           move      090                  to   w-rou-stp-pmt-pos      .
           perform   rou-stp-pmt-000      thru rou-stp-pmt-999        .
       int-pag-sta-dps-400.
       int-pag-sta-dps-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     int-pag-sta-dps-999.
       int-pag-sta-dps-999.
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
      *    * Routine di stampa di un box                               *
      *    *-----------------------------------------------------------*
       box-int-pag-000.
      *              *-------------------------------------------------*
      *              * Assestamenti relativi al displacement verticale *
      *              *-------------------------------------------------*
           add       w-dsp-dsp-vrt        to   w-box-int-pag-lin      .
           add       w-dsp-dsp-vrt        to   w-box-int-pag-lfi      .
      *              *-------------------------------------------------*
      *              * Preparazione line - position - size             *
      *              *-------------------------------------------------*
           move      w-box-int-pag-lin    to   w-box-int-pag-wln      .
           move      w-box-int-pag-pin    to   w-box-int-pag-wps      .
           subtract  w-box-int-pag-pin    from w-box-int-pag-pfi
                                        giving w-box-int-pag-wsz      .
           add       1                    to   w-box-int-pag-wsz      .
      *              *-------------------------------------------------*
      *              * Linea superiore                                 *
      *              *-------------------------------------------------*
           move      w-box-int-pag-wtr    to   w-box-int-pag-wst      .
           move      "+"                  to   w-box-int-pag-wch (1)  .
           move      "+"                  to   w-box-int-pag-wch
                                              (w-box-int-pag-wsz)     .
           perform   box-int-pag-900      thru box-int-pag-919        .
      *              *-------------------------------------------------*
      *              * Linee intermedie                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Preparazione                                *
      *                  *---------------------------------------------*
           move      spaces               to   w-box-int-pag-wst      .
           move      "|"                  to   w-box-int-pag-wch (1)  .
           move      "|"                  to   w-box-int-pag-wch
                                              (w-box-int-pag-wsz)     .
           subtract  w-box-int-pag-lin    from w-box-int-pag-lfi
                                        giving w-box-int-pag-ctr      .
           subtract  1                    from w-box-int-pag-ctr      .
       box-int-pag-500.
           add       1                    to   w-box-int-pag-wln      .
           if        w-box-int-pag-ctr    >    zero
                     subtract 1           from w-box-int-pag-ctr
                     perform  box-int-pag-900
                                          thru box-int-pag-919
                     go to    box-int-pag-500.
      *              *-------------------------------------------------*
      *              * Linea inferiore                                 *
      *              *-------------------------------------------------*
           move      w-box-int-pag-wtr    to   w-box-int-pag-wst      .
           move      "+"                  to   w-box-int-pag-wch (1)  .
           move      "+"                  to   w-box-int-pag-wch
                                              (w-box-int-pag-wsz)     .
           perform   box-int-pag-900      thru box-int-pag-919        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     box-int-pag-999.
       box-int-pag-900.
      *              *=================================================*
      *              * Sub-routine interna di stampa riga              *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-box-int-pag-wsz    to   p-car                  .
           move      w-box-int-pag-wln    to   p-lin                  .
           move      w-box-int-pag-wps    to   p-pos                  .
           move      w-box-int-pag-wst    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       box-int-pag-919.
           exit.
       box-int-pag-999.
           exit.

      *    *===========================================================*
      *    * Routine di correzione per stampa pagine oltre alla prima  *
      *    *-----------------------------------------------------------*
       rou-stp-psp-000.
      *              *-------------------------------------------------*
      *              * Test sul numero di pagina                       *
      *              *-------------------------------------------------*
           if        w-cnt-tit-num-pag    =    1
                     go to rou-stp-psp-999.
      *              *-------------------------------------------------*
      *              * Decremento linee di stampa                      *
      *              *-------------------------------------------------*
           subtract  19                   from w-box-int-pag-lin      .
           subtract  19                   from w-rou-stp-pmt-lin      .
       rou-stp-psp-999.
           exit.

      *    *===========================================================*
      *    * Routine di stampa di un prompt                            *
      *    *-----------------------------------------------------------*
       rou-stp-pmt-000.
      *              *-------------------------------------------------*
      *              * Assestamenti relativi al displacement verticale *
      *              *-------------------------------------------------*
           add       w-dsp-dsp-vrt        to   w-rou-stp-pmt-lin      .
       rou-stp-pmt-200.
      *              *-------------------------------------------------*
      *              * Stampa                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   p-ope                  .
           move      "A"                  to   p-tip                  .
           move      w-rou-stp-pmt-car    to   p-car                  .
           move      w-rou-stp-pmt-lin    to   p-lin                  .
           move      w-rou-stp-pmt-pos    to   p-pos                  .
           move      w-rou-stp-pmt-alf    to   p-alf                  .
           call      "swd/mod/prg/obj/mprint"
                                         using p                      .
       rou-stp-pmt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     rou-stp-pmt-999.
       rou-stp-pmt-999.
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
      *                  * Lettura archivio [dcp]                      *
      *                  *---------------------------------------------*
           move      rf-osr-num-pro       to   w-let-dcp-pdx-cod      .
           move      rf-ost-tip-arc       to   w-let-dcp-pdx-tar      .
           if        rf-ost-tip-frn       =    11
                     move  rf-ost-cod-arc to   w-let-dcp-pdx-arc
           else      move  rf-ost-arc-plf to   w-let-dcp-pdx-arc      .
           move      rf-osr-cod-lng       to   w-let-dcp-pdx-lng      .
           perform   let-dcp-pdx-000      thru let-dcp-pdx-999        .
           move      w-let-dcp-pdx-des    to   w-stp-rig-des-der      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-des-rig-900.
       det-des-rig-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-des-rig-999.
       det-des-rig-999.
           exit.

      *    *===========================================================*
      *    * Descrizione per la stampa di tipo voce descrittiva        *
      *    *-----------------------------------------------------------*
       det-des-zdf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore in uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-des-zdf-des      .
       det-des-zdf-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [zdf]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzdf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zdf                 .
      *              *-------------------------------------------------*
      *              * Tentativo di lettura della descrizione con il   *
      *              * codice lingua del cliente                       *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODDEF"             to   f-key                  .
           move      w-det-des-zdf-num    to   rf-zdf-num-def         .
           move      w-det-des-zdf-lng    to   rf-zdf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzdf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zdf                 .
      *                  *---------------------------------------------*
      *                  * Se valore trovato : uscita con il valore    *
      *                  * letto                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-des-zdf-200.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valore letto                *
      *                  *---------------------------------------------*
           move      rf-zdf-des-stp       to   w-det-des-zdf-des      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-des-zdf-999.
       det-des-zdf-200.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura della descrizione con il   *
      *              * codice lingua per l'Italia                      *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODDEF"             to   f-key                  .
           move      w-det-des-zdf-num    to   rf-zdf-num-def         .
           move      "I  "                to   rf-zdf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzdf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zdf                 .
      *                  *---------------------------------------------*
      *                  * Se valore trovato : uscita con il valore    *
      *                  * letto                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-des-zdf-400.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valore letto                *
      *                  *---------------------------------------------*
           move      rf-zdf-des-stp       to   w-det-des-zdf-des      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-des-zdf-999.
       det-des-zdf-400.
      *              *-------------------------------------------------*
      *              * Uscita con valore a spaces                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-des-zdf-999.
       det-des-zdf-999.
           exit.

      *    *===========================================================*
      *    * Descrizione in lingua per la stampa di : voci descrittive *
      *    * in fattura per i clienti                                  *
      *    *-----------------------------------------------------------*
       det-des-zvf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore in uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-des-zvf-des      .
       det-des-zvf-100.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [zvf]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
      *              *-------------------------------------------------*
      *              * Tentativo di lettura della descrizione con il   *
      *              * codice lingua del cliente                       *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVDF"             to   f-key                  .
           move      w-det-des-zvf-num    to   rf-zvf-num-def         .
           move      w-det-des-zvf-cod    to   rf-zvf-cod-def         .
           move      w-det-des-zvf-lng    to   rf-zvf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
      *                  *---------------------------------------------*
      *                  * Se valore trovato : uscita con il valore    *
      *                  * letto                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-des-zvf-200.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valore letto                *
      *                  *---------------------------------------------*
           move      rf-zvf-des-stp       to   w-det-des-zvf-des      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-des-zvf-999.
       det-des-zvf-200.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura della descrizione con il   *
      *              * codice lingua per l'Italia                      *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODVDF"             to   f-key                  .
           move      w-det-des-zvf-num    to   rf-zvf-num-def         .
           move      w-det-des-zvf-cod    to   rf-zvf-cod-def         .
           move      "I  "                to   rf-zvf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzvf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zvf                 .
      *                  *---------------------------------------------*
      *                  * Se valore trovato : uscita con il valore    *
      *                  * letto                                       *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-des-zvf-400.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione valore letto                *
      *                  *---------------------------------------------*
           move      rf-zvf-des-stp       to   w-det-des-zvf-des      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-des-zvf-999.
       det-des-zvf-400.
      *              *-------------------------------------------------*
      *              * Uscita con valore a spaces                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-des-zvf-999.
       det-des-zvf-999.
           exit.

      *    *===========================================================*
      *    * Routine di determinazione parametri di ubicazione         *
      *    *-----------------------------------------------------------*
       det-prm-ubi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione valore di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-prm-ubi-lit      .
       det-prm-ubi-100.
      *              *-------------------------------------------------*
      *              * Test preliminare                                *
      *              *-------------------------------------------------*
           if        w-det-prm-ubi-num    =    zero
                     go to det-prm-ubi-900.
       det-prm-ubi-200.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [mau]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
      *              *-------------------------------------------------*
      *              * Lettura record [mau]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "MAGUBI    "         to   f-key                  .
           move      w-det-prm-ubi-dpz    to   rf-mau-cod-dpz         .
           move      w-det-prm-ubi-tip    to   rf-mau-tip-mag         .
           move      w-det-prm-ubi-num    to   rf-mau-num-mag         .
           move      w-det-prm-ubi-var    to   rf-mau-var-mag         .
           move      "pgm/mag/fls/ioc/obj/iofmau"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-mau                 .
      *                  *---------------------------------------------*
      *                  * Se record [mau] non trovato : ad uscita     *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-prm-ubi-900.
       det-prm-ubi-300.
      *              *-------------------------------------------------*
      *              * Composizione stringa                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Parametri di ubicazione                     *
      *                  *                                             *
      *                  * ELETTRA : parametri rovesci                 *
      *                  *---------------------------------------------*
           move      28                   to   w-all-str-lun          .
           move      04                   to   w-all-str-num          .
           move      rf-mau-prm-ubi (1)   to   w-all-str-cat (1)      .
           move      rf-mau-prm-ubi (2)   to   w-all-str-cat (2)      .
           move      rf-mau-prm-ubi (3)   to   w-all-str-cat (3)      .
           move      rf-mau-prm-ubi (4)   to   w-all-str-cat (4)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
       det-prm-ubi-800.
      *              *-------------------------------------------------*
      *              * Valore preparato in literal di uscita           *
      *              *-------------------------------------------------*
           move      w-all-str-alf        to   w-det-prm-ubi-lit      .
      *                  *---------------------------------------------*
      *                  * Ad uscita                                   *
      *                  *---------------------------------------------*
           go to     det-prm-ubi-999.
       det-prm-ubi-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-prm-ubi-999.
       det-prm-ubi-999.
           exit.

      *    *===========================================================*
      *    * Subroutine per editing attinenti la stampa dei giorni di  *
      *    * chiusura e dell'orario di apertura clienti                *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/egchoap0.eds"                   .

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

      *    *===========================================================*
      *    * Routine lettura archivio [dcp] e [pdx]                    *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcp/prg/cpy/ldcppdx0.lts"                   .

