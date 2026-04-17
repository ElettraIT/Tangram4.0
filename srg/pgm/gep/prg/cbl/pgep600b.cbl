       Identification Division.
       Program-Id.                                 pgep600b           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    gep                 *
      *                                Settore:    cli                 *
      *                                   Fase:    gep600              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 19/05/09    *
      *                       Ultima revisione:    NdK del 07/06/22    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Analisi di portafoglio per cliente          *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Esposizione globale del cliente             *
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
      *            * Filler                                            *
      *            *---------------------------------------------------*
               10  filler                 pic  x(01)                  .
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
      *    * Area di comunicazione per modulo                 "mvideo" *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/v"                                  .

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
      *    * Area di definizione della valuta base                     *
      *    *-----------------------------------------------------------*
           copy      "swd/mod/int/c"                                  .

      *    *===========================================================*
      *    * Work-area di controllo                                    *
      *    *-----------------------------------------------------------*
       01  w-cnt.
      *        *-------------------------------------------------------*
      *        * Flags di controllo uscita da routines fondamentali    *
      *        *-------------------------------------------------------*
           05  w-cnt-flg.
      *            *---------------------------------------------------*
      *            * Per routine pre-exe-pgm-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-pre-exe-pgm      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine rou-opn-fls-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-rou-opn-fls      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Per routine qry-rou-pri-000                       *
      *            *---------------------------------------------------*
               10  w-cnt-qry-rou-pri      pic  x(01)                  .
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
      *            * Si/No richieste pre esecuzione interrogazione     *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-ric      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento ciclico interrogazione        *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-cic      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Si/No funzionamento automatico interrogazione     *
      *            *---------------------------------------------------*
               10  w-cnt-fun-snx-aut      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di Si/No primo giro di esecuzione            *
      *            *---------------------------------------------------*
               10  w-cnt-fun-prm-gir      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Area di controllo per funzionamento qry-routine       *
      *        *-------------------------------------------------------*
           05  w-cnt-qry.
      *            *---------------------------------------------------*
      *            * Flag di primo giro                                *
      *            *---------------------------------------------------*
               10  w-cnt-qry-mrk-uno      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di interruzione forzata                      *
      *            *---------------------------------------------------*
               10  w-cnt-qry-flg-int      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Flag di uscita da subroutines principali          *
      *            *---------------------------------------------------*
               10  w-cnt-qry-flg-sub      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio parametri rottura livello    *
      *            *---------------------------------------------------*
               10  w-cnt-qry-sav-liv.
                   15  w-cnt-qry-sav-l05  pic  x(64)                  .
                   15  w-cnt-qry-sav-l04  pic  x(64)                  .
                   15  w-cnt-qry-sav-l03  pic  x(64)                  .
                   15  w-cnt-qry-sav-l02  pic  x(64)                  .
                   15  w-cnt-qry-sav-l01  pic  x(64)                  .
      *            *---------------------------------------------------*
      *            * Area per salvataggio area di rottura              *
      *            *---------------------------------------------------*
               10  w-cnt-qry-sav-rot.
                   15  filler occurs 320  pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Literals                                              *
      *        *-------------------------------------------------------*
           05  w-cnt-lit.
               10  w-cnt-lit-t80          pic  x(80) value all "="    .

      *    *===========================================================*
      *    * Record files                                              *
      *    *-----------------------------------------------------------*
      *        *-------------------------------------------------------*
      *        * [ada]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/azi/fls/rec/rfada"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [lic]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rflic"                          .
      *        *-------------------------------------------------------*
      *        * [dcc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfdcc"                          .
      *        *-------------------------------------------------------*
      *        * [oct]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfoct"                          .
      *        *-------------------------------------------------------*
      *        * [ocr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/orc/fls/rec/rfocr"                          .
      *        *-------------------------------------------------------*
      *        * [ost]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfost"                          .
      *        *-------------------------------------------------------*
      *        * [osr]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/ods/fls/rec/rfosr"                          .
      *        *-------------------------------------------------------*
      *        * [zfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzfp"                          .
      *        *-------------------------------------------------------*
      *        * [zin]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzin"                          .
      *        *-------------------------------------------------------*
      *        * [zbo]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzbo"                          .
      *        *-------------------------------------------------------*
      *        * [zsf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcc/fls/rec/rfzsf"                          .
      *        *-------------------------------------------------------*
      *        * [zpg]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfzpg"                          .
      *        *-------------------------------------------------------*
      *        * [ccc]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfccc"                          .
      *        *-------------------------------------------------------*
      *        * [bit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbit"                          .
      *        *-------------------------------------------------------*
      *        * [bir]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/bol/fls/rec/rfbir"                          .
      *        *-------------------------------------------------------*
      *        * [sdb]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfsdb"                          .
      *        *-------------------------------------------------------*
      *        * [ddp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/gep/fls/rec/rfddp"                          .
      *        *-------------------------------------------------------*
      *        * [zfi]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/fat/fls/rec/rfzfi"                          .

      *    *===========================================================*
      *    * Work-area per variabili di i.p.c. eventualmente passate   *
      *    * dal chiamante                                             *
      *    *-----------------------------------------------------------*
       01  w-ipc.
      *        *-------------------------------------------------------*
      *        * Per tipo di chiamante del sottoprogramma              *
      *        *-------------------------------------------------------*
           05  w-ipc-tdc-mos.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al tipo di     *
      *            * chiamante del sottoprogramma                      *
      *            *---------------------------------------------------*
               10  w-ipc-tdc-mos-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al tipo *
      *            * di chiamante del sottoprogramma                   *
      *            * - M : Il main                                     *
      *            * - S : Un altro sottoprogramma                     *
      *            *---------------------------------------------------*
               10  w-ipc-tdc-mos-val      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Per ammissibilita' tasto Slct                         *
      *        *-------------------------------------------------------*
           05  w-ipc-snx-slc.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa all'ammissibi- *
      *            * lita' del tasto Slct                              *
      *            *---------------------------------------------------*
               10  w-ipc-snx-slc-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa all'am- *
      *            * missibilita' del tasto Slct                       *
      *            * - S : Si                                          *
      *            * - N : No                                          *
      *            *---------------------------------------------------*
               10  w-ipc-snx-slc-val      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Per data di riferimento per l'analisi                 *
      *        *-------------------------------------------------------*
           05  w-ipc-dat-rfa.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa alla data di   *
      *            * riferimento per l'analisi                         *
      *            *---------------------------------------------------*
               10  w-ipc-dat-rfa-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa alla    *
      *            * data di riferimento per l'analisi                 *
      *            *---------------------------------------------------*
               10  w-ipc-dat-rfa-val      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Per il debitore ed eventuale dipendenza               *
      *        *-------------------------------------------------------*
           05  w-ipc-cod-dbt.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa al debitore ed *
      *            * eventuale dipendenza                              *
      *            *---------------------------------------------------*
               10  w-ipc-cod-dbt-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa al de-  *
      *            * bitore ed eventuale dipendenza                    *
      *            *---------------------------------------------------*
               10  w-ipc-cod-dbt-val.
      *                *-----------------------------------------------*
      *                * Tipo debitore                                 *
      *                *                                               *
      *                * - 01 : Cliente                                *
      *                * - 02 : Debitori diversi                       *
      *                *-----------------------------------------------*
                   15  w-ipc-cod-dbt-tip  pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Codice debitore                               *
      *                *-----------------------------------------------*
                   15  w-ipc-cod-dbt-cod  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Codice dipendenza per il debitore             *
      *                *-----------------------------------------------*
                   15  w-ipc-cod-dbt-dpz  pic  x(04)                  .

      *    *===========================================================*
      *    * Work-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Data di riferimento per l'analisi                     *
      *        *-------------------------------------------------------*
           05  rr-dat-rfa                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice cliente                                        *
      *        *-------------------------------------------------------*
           05  rr-cod-cli                 pic  9(07)                  .
           05  rr-cod-cli-rag             pic  x(40)                  .
           05  rr-cod-cli-via             pic  x(40)                  .
           05  rr-cod-cli-loc             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza del cliente                         *
      *        *-------------------------------------------------------*
           05  rr-dpz-cli                 pic  x(04)                  .
           05  rr-dpz-cli-rag             pic  x(40)                  .
           05  rr-dpz-cli-via             pic  x(40)                  .
           05  rr-dpz-cli-loc             pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Tipo evidenziazione                                   *
      *        *                                                       *
      *        * - 01 : Sia la lista che il riepilogo finale           *
      *        * - 02 : Solo la lista dei movimenti                    *
      *        * - 03 : Solo il riepilogo finale                       *
      *        *-------------------------------------------------------*
           05  rr-tip-evd                 pic  9(02)                  .

      *    *===========================================================*
      *    * Work-area per personalizzazioni                           *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Si/No proposte tutte dipendenze come default di       *
      *        * selezione per i clienti                               *
      *        *                                                       *
      *        *  - 'N' : Nessun default                               *
      *        *  - 'S' : Tutte le dipendenza come default             *
      *        *-------------------------------------------------------*
           05  w-prs-snx-tdd-dcc          pic  x(01)                  .
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
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
               10  w-let-arc-cli-via      pic  x(40)                  .
               10  w-let-arc-cli-loc      pic  x(40)                  .
               10  w-let-arc-cli-cge      pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcc]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcc.
               10  w-let-arc-dcc-flg      pic  x(01)                  .
               10  w-let-arc-dcc-tle      pic  x(01)                  .
               10  w-let-arc-dcc-cod      pic  9(07)                  .
               10  w-let-arc-dcc-dpz      pic  x(04)                  .
               10  w-let-arc-dcc-rag      pic  x(40)                  .
               10  w-let-arc-dcc-via      pic  x(40)                  .
               10  w-let-arc-dcc-loc      pic  x(40)                  .
               10  w-let-arc-dcc-abi      pic  9(05)                  .
               10  w-let-arc-dcc-cab      pic  9(05)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
      *        *-------------------------------------------------------*
      *        * Work per Det se presenti dipendenze per il cliente    *
      *        * commerciale                                           *
      *        *-------------------------------------------------------*
           05  w-det-snd-dcc.
      *            *---------------------------------------------------*
      *            * Esito della determinazione                        *
      *            * - S : Si, il cliente   commerciale ha dipendenze  *
      *            * - N : No, il cliente   commerciale non ha dipen-  *
      *            *       denze                                       *
      *            *---------------------------------------------------*
               10  w-det-snd-dcc-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Codice cliente commerciale                        *
      *            *---------------------------------------------------*
               10  w-det-snd-dcc-cli      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Contatore dipendenze rilevate                     *
      *            *---------------------------------------------------*
               10  w-det-snd-dcc-ctr      pic  9(05)                  .
      *            *---------------------------------------------------*
      *            * Codice dipendenza, solo se unica per il cliente   *
      *            *---------------------------------------------------*
               10  w-det-snd-dcc-dpz      pic  x(04)                  .
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
      *    * Work-area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
      *        *-------------------------------------------------------*
      *        * Codice cliente                                        *
      *        *-------------------------------------------------------*
           05  w-sav-cod-cli              pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Codice dipendenza del cliente, flag di modifica       *
      *        *-------------------------------------------------------*
           05  w-sav-dpz-cli              pic  x(01)                  .

      *    *===========================================================*
      *    * Link-area per accettazione codice cliente commerciale     *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acl"                   .

      *    *===========================================================*
      *    * Link-area per accettazione codice dipendenza del cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acl"                   .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipo evidenziazione                        *
      *        *-------------------------------------------------------*
           05  w-exp-tip-evd.
               10  w-exp-tip-evd-num      pic  9(02)       value 03   .
               10  w-exp-tip-evd-lun      pic  9(02)       value 40   .
               10  w-exp-tip-evd-tbl.
                   15  filler             pic  x(40) value
                            "Sia la lista che il riepilogo finale    ".
                   15  filler             pic  x(40) value
                            "Solo la lista dei movimenti             ".
                   15  filler             pic  x(40) value
                            "Solo il riepilogo finale                ".

      *    *===========================================================*
      *    * Work per subroutines di Err                               *
      *    *-----------------------------------------------------------*
       01  w-err.
      *        *-------------------------------------------------------*
      *        * Work per Err con box centrale                         *
      *        *-------------------------------------------------------*
           05  w-err-box-err.
               10  w-err-box-err-msg      pic  x(65)                  .
               10  w-err-box-err-m02      pic  x(65)                  .
               10  w-err-box-err-m03      pic  x(65)                  .

      *    *===========================================================*
      *    * Work-area per ridefinizione contenuto del mark-point      *
      *    *-----------------------------------------------------------*
       01  w-mpn.
      *        *-------------------------------------------------------*
      *        * Tipo di mark-point                                    *
      *        *                                                       *
      *        * - 000 : Per lista riepilogativa                       *
      *        *                                                       *
      *        * - 001 : Per ordini clienti                            *
      *        * - 002 : Per ordini di spedizione                      *
      *        * - 003 : Per bolle da fatturare                        *
      *        * - 004 : Per scadenze aperte                           *
      *        *-------------------------------------------------------*
           05  w-mpn-tip-mpn              pic  9(03)                  .
      *        *-------------------------------------------------------*
      *        * Attributi per il mark-point                           *
      *        *-------------------------------------------------------*
           05  w-mpn-att-mpn.
      *            *---------------------------------------------------*
      *            * Definizione generica                              *
      *            *---------------------------------------------------*
               10  w-mpn-att-000.
      *                *-----------------------------------------------*
      *                * Tipo scadenza                                 *
      *                *-----------------------------------------------*
                   15  w-mpn-tsc-000      pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Filler riempitivo                             *
      *                *-----------------------------------------------*
                   15  filler             pic  x(75)                  .
      *            *---------------------------------------------------*
      *            * Definizione per mark-point tipo : 001             *
      *            *---------------------------------------------------*
               10  w-mpn-att-001 redefines
                   w-mpn-att-000.
      *                *-----------------------------------------------*
      *                * Data documento                                *
      *                *-----------------------------------------------*
                   15  w-mpn-dat-orc      pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Numero documento                              *
      *                *-----------------------------------------------*
                   15  w-mpn-num-orc      pic  9(11)                  .
      *                *-----------------------------------------------*
      *                * Tipo movimento                                *
      *                *-----------------------------------------------*
                   15  w-mpn-tip-orc      pic  x(05)                  .
      *                *-----------------------------------------------*
      *                * Numero protocollo                             *
      *                *-----------------------------------------------*
                   15  w-mpn-prt-orc      pic  9(11)                  .
      *                *-----------------------------------------------*
      *                * Filler riempitivo                             *
      *                *-----------------------------------------------*
                   15  filler             pic  x(43)                  .
      *            *---------------------------------------------------*
      *            * Definizione per mark-point tipo : 002             *
      *            *---------------------------------------------------*
               10  w-mpn-att-002 redefines
                   w-mpn-att-000.
      *                *-----------------------------------------------*
      *                * Data documento                                *
      *                *-----------------------------------------------*
                   15  w-mpn-dat-ods      pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Tipo movimento                                *
      *                *-----------------------------------------------*
                   15  w-mpn-tip-ods      pic  x(05)                  .
      *                *-----------------------------------------------*
      *                * Numero protocollo                             *
      *                *-----------------------------------------------*
                   15  w-mpn-prt-ods      pic  9(11)                  .
      *                *-----------------------------------------------*
      *                * Filler riempitivo                             *
      *                *-----------------------------------------------*
                   15  filler             pic  x(54)                  .
      *            *---------------------------------------------------*
      *            * Definizione per mark-point tipo : 003             *
      *            *---------------------------------------------------*
               10  w-mpn-att-003 redefines
                   w-mpn-att-000.
      *                *-----------------------------------------------*
      *                * Data documento                                *
      *                *-----------------------------------------------*
                   15  w-mpn-dat-bol      pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Numero documento                              *
      *                *-----------------------------------------------*
                   15  w-mpn-num-bol      pic  9(11)                  .
      *                *-----------------------------------------------*
      *                * Tipo movimento                                *
      *                *-----------------------------------------------*
                   15  w-mpn-tip-bol      pic  x(05)                  .
      *                *-----------------------------------------------*
      *                * Numero protocollo                             *
      *                *-----------------------------------------------*
                   15  w-mpn-prt-bol      pic  9(11)                  .
      *                *-----------------------------------------------*
      *                * Filler riempitivo                             *
      *                *-----------------------------------------------*
                   15  filler             pic  x(43)                  .
      *            *---------------------------------------------------*
      *            * Definizione per mark-point tipo : 004             *
      *            *---------------------------------------------------*
               10  w-mpn-att-004 redefines
                   w-mpn-att-000.
      *                *-----------------------------------------------*
      *                * Numero scadenza                               *
      *                *-----------------------------------------------*
                   15  w-mpn-num-gep      pic  9(11)                  .
      *                *-----------------------------------------------*
      *                * Filler riempitivo                             *
      *                *-----------------------------------------------*
                   15  filler             pic  x(66)                  .

      *    *===========================================================*
      *    * Work-area per ciclo di Report-Program                     *
      *    *-----------------------------------------------------------*
       01  w-prn.
      *        *-------------------------------------------------------*
      *        * Flag di primo passaggio                               *
      *        *-------------------------------------------------------*
           05  w-prn-flg-uno              pic  x(01)                  .

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
      *    * Work area per salvataggi                                  *
      *    *-----------------------------------------------------------*
       01  w-sav.
           05  w-sav-prz-ven              pic  9(09)                  .

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
      *        *-------------------------------------------------------*
      *        * Totalizzazioni per tipo documento                     *
      *        *-------------------------------------------------------*
           05  w-tot-tot-gen.
      *            *---------------------------------------------------*
      *            * 4 elementi, 1 per ogni area                       *
      *            *                                                   *
      *            *  - 01 : Ordini clienti                            *
      *            *  - 02 : Ordini di spedizione clienti              *
      *            *  - 03 : Bolle insevase clienti                    *
      *            *  - 04 : Scadenze                                  *
      *            *---------------------------------------------------*
               10  w-tot-tot-gen-ele  occurs 04.
      *                *-----------------------------------------------*
      *                * Totale importo                                *
      *                *-----------------------------------------------*
                   15  w-tot-tot-gen-tot  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Totale numero elementi                        *
      *                *-----------------------------------------------*
                   15  w-tot-tot-gen-num  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Totale 'Precedenti'                           *
      *                *-----------------------------------------------*
                   15  w-tot-tot-gen-pre  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Totali a partire dal mese in corso            *
      *                *-----------------------------------------------*
                   15  w-tot-tot-gen-mic  pic s9(11)                  .
                   15  w-tot-tot-gen-ms1  pic s9(11)                  .
                   15  w-tot-tot-gen-ms2  pic s9(11)                  .
                   15  w-tot-tot-gen-ms3  pic s9(11)                  .
                   15  w-tot-tot-gen-ms4  pic s9(11)                  .
                   15  w-tot-tot-gen-ms5  pic s9(11)                  .
                   15  w-tot-tot-gen-ms6  pic s9(11)                  .
                   15  w-tot-tot-gen-ms7  pic s9(11)                  .
                   15  w-tot-tot-gen-ms8  pic s9(11)                  .
                   15  w-tot-tot-gen-ms9  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Totale 'Successivi'                           *
      *                *-----------------------------------------------*
                   15  w-tot-tot-gen-suc  pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Indici per scansione su elementi                  *
      *            *---------------------------------------------------*
               10  w-tot-tot-gen-i01      pic  9(02)                  .
               10  w-tot-tot-gen-i02      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Comodi per calcolo                                *
      *            *---------------------------------------------------*
               10  w-tot-tot-gen-s11      pic s9(11)                  .
               10  w-tot-tot-gen-wfd      pic s9(13)                  .
               10  w-tot-tot-gen-wac      pic s9(13)                  .
               10  w-tot-tot-gen-wcc      pic s9(13)                  .
               10  w-tot-tot-gen-wdd      pic s9(13)                  .
               10  w-tot-tot-gen-wfl      pic  x(01)                  .

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
      *        * Data di inizio mese 7                                 *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-im7          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mese 7                                   *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-fm7          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di inizio mese 8                                 *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-im8          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mese 8                                   *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-fm8          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di inizio mese 9                                 *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-im9          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mese 9                                   *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-fm9          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di inizio mesi successivi                        *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-ims          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Data di fine mesi successivi                          *
      *        *-------------------------------------------------------*
           05  w-stp-dat-inp-fms          pic  9(07)                  .

      *    *===========================================================*
      *    * Work area per stampa di un valore                         *
      *    *-----------------------------------------------------------*
       01  w-stp-val.
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa di un valore numerico             *
      *        *-------------------------------------------------------*
           05  w-stp-imp-vlb.
      *            *---------------------------------------------------*
      *            * Posizione per la stampa                           *
      *            *---------------------------------------------------*
               10  w-stp-imp-vlb-pos      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Valore numerico da stampare                       *
      *            *---------------------------------------------------*
               10  w-stp-imp-vlb-val      pic s9(13)                  .
      *            *---------------------------------------------------*
      *            * Valore da stampare editato                        *
      *            *---------------------------------------------------*
               10  w-stp-imp-vlb-edt      pic  x(15)                  .
      *            *---------------------------------------------------*
      *            * Eventuale riempitivo                              *
      *            *---------------------------------------------------*
               10  w-stp-imp-vlb-rmp      pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Sub-work per stampa di un prompt                      *
      *        *-------------------------------------------------------*
           05  w-stp-lit-pmt.
      *            *---------------------------------------------------*
      *            * Data per la stampa                                *
      *            *---------------------------------------------------*
               10  w-stp-lit-pmt-dat      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Posizione per la stampa                           *
      *            *---------------------------------------------------*
               10  w-stp-lit-pmt-pos      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Linea per la stampa                               *
      *            *---------------------------------------------------*
               10  w-stp-lit-pmt-lin      pic  9(03)                  .

      *    *===========================================================*
      *    * Work area per cumulo ordini inevasi cliente               *
      *    *-----------------------------------------------------------*
       01  w-stp-orc.
      *        *-------------------------------------------------------*
      *        * Sub-work per cumulo valori                            *
      *        *-------------------------------------------------------*
           05  w-stp-orc-wrk.
      *            *---------------------------------------------------*
      *            * Numero elementi rilevati                          *
      *            *---------------------------------------------------*
               10  w-stp-orc-wrk-ele      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo per prompt                                 *
      *            *---------------------------------------------------*
               10  w-stp-orc-wrk-pmt      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-orc-wrk-ctr      pic  9(03)                  .
               10  w-stp-orc-wrk-ct1      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo per numero documento                       *
      *            *---------------------------------------------------*
               10  w-stp-orc-wrk-doc      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Comodo per salvataggio numero documento           *
      *            *---------------------------------------------------*
               10  w-stp-orc-wrk-svd      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Comodo per tranche documento                      *
      *            *---------------------------------------------------*
               10  w-stp-orc-wrk-tdc      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Massimo elementi                                  *
      *            *---------------------------------------------------*
               10  w-stp-orc-wrk-max      pic  9(03)    value 500     .
      *            *---------------------------------------------------*
      *            * Tabella elementi                                  *
      *            *---------------------------------------------------*
               10  w-stp-orc-wrk-tbl.
      *                *-----------------------------------------------*
      *                * Singole righe che compongono la tabella       *
      *                *-----------------------------------------------*
                   15  w-stp-orc-wrk-rig occurs 500.
      *                    *-------------------------------------------*
      *                    * Estremi ordine                            *
      *                    *-------------------------------------------*
                       20  w-stp-orc-tip-doc
                                          pic  x(05)                  .
                       20  w-stp-orc-num-doc
                                          pic  9(11)                  .
                       20  w-stp-orc-dat-doc
                                          pic  9(07)                  .
      *                    *-------------------------------------------*
      *                    * Cumulo precedenti                         *
      *                    *-------------------------------------------*
                       20  w-stp-orc-cum-pre
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mesi 01..09                        *
      *                    *-------------------------------------------*
                       20  w-stp-orc-cum-mic
                                          pic s9(11)       comp-3     .
                       20  w-stp-orc-cum-ms1
                                          pic s9(11)       comp-3     .
                       20  w-stp-orc-cum-ms2
                                          pic s9(11)       comp-3     .
                       20  w-stp-orc-cum-ms3
                                          pic s9(11)       comp-3     .
                       20  w-stp-orc-cum-ms4
                                          pic s9(11)       comp-3     .
                       20  w-stp-orc-cum-ms5
                                          pic s9(11)       comp-3     .
                       20  w-stp-orc-cum-ms6
                                          pic s9(11)       comp-3     .
                       20  w-stp-orc-cum-ms7
                                          pic s9(11)       comp-3     .
                       20  w-stp-orc-cum-ms8
                                          pic s9(11)       comp-3     .
                       20  w-stp-orc-cum-ms9
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo successivi                         *
      *                    *-------------------------------------------*
                       20  w-stp-orc-cum-suc
                                          pic s9(11)       comp-3     .

      *    *===========================================================*
      *    * Work area per cumulo ordini di spedizione inevasi cliente *
      *    *-----------------------------------------------------------*
       01  w-stp-ods.
      *        *-------------------------------------------------------*
      *        * Sub-work per cumulo valori                            *
      *        *-------------------------------------------------------*
           05  w-stp-ods-wrk.
      *            *---------------------------------------------------*
      *            * Numero elementi rilevati                          *
      *            *---------------------------------------------------*
               10  w-stp-ods-wrk-ele      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo per prompt                                 *
      *            *---------------------------------------------------*
               10  w-stp-ods-wrk-pmt      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Contatore di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-ods-wrk-ctr      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo per numero documento                       *
      *            *---------------------------------------------------*
               10  w-stp-ods-wrk-doc      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Massimo elementi                                  *
      *            *---------------------------------------------------*
               10  w-stp-ods-wrk-max      pic  9(03)    value 500     .
      *            *---------------------------------------------------*
      *            * Tabella elementi                                  *
      *            *---------------------------------------------------*
               10  w-stp-ods-wrk-tbl.
      *                *-----------------------------------------------*
      *                * Singole righe che compongono la tabella       *
      *                *-----------------------------------------------*
                   15  w-stp-ods-wrk-rig occurs 500.
      *                    *-------------------------------------------*
      *                    * Estremi ordine                            *
      *                    *-------------------------------------------*
                       20  w-stp-ods-tip-doc
                                          pic  x(05)                  .
                       20  w-stp-ods-num-doc
                                          pic  9(11)                  .
                       20  w-stp-ods-dat-doc
                                          pic  9(07)                  .
      *                    *-------------------------------------------*
      *                    * Cumulo precedenti                         *
      *                    *-------------------------------------------*
                       20  w-stp-ods-cum-pre
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mesi 01..09                        *
      *                    *-------------------------------------------*
                       20  w-stp-ods-cum-mic
                                          pic s9(11)       comp-3     .
                       20  w-stp-ods-cum-ms1
                                          pic s9(11)       comp-3     .
                       20  w-stp-ods-cum-ms2
                                          pic s9(11)       comp-3     .
                       20  w-stp-ods-cum-ms3
                                          pic s9(11)       comp-3     .
                       20  w-stp-ods-cum-ms4
                                          pic s9(11)       comp-3     .
                       20  w-stp-ods-cum-ms5
                                          pic s9(11)       comp-3     .
                       20  w-stp-ods-cum-ms6
                                          pic s9(11)       comp-3     .
                       20  w-stp-ods-cum-ms7
                                          pic s9(11)       comp-3     .
                       20  w-stp-ods-cum-ms8
                                          pic s9(11)       comp-3     .
                       20  w-stp-ods-cum-ms9
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo successivi                         *
      *                    *-------------------------------------------*
                       20  w-stp-ods-cum-suc
                                          pic s9(11)       comp-3     .

      *    *===========================================================*
      *    * Work area per cumulo bolle da fatturare cliente           *
      *    *-----------------------------------------------------------*
       01  w-stp-bol.
      *        *-------------------------------------------------------*
      *        * Sub-work per cumulo valori                            *
      *        *-------------------------------------------------------*
           05  w-stp-bol-wrk.
      *            *---------------------------------------------------*
      *            * Numero elementi rilevati                          *
      *            *---------------------------------------------------*
               10  w-stp-bol-wrk-ele      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo per prompt                                 *
      *            *---------------------------------------------------*
               10  w-stp-bol-wrk-pmt      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Contatori di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-bol-wrk-ctr      pic  9(03)                  .
               10  w-stp-bol-wrk-ct1      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo per numero documento                       *
      *            *---------------------------------------------------*
               10  w-stp-bol-wrk-doc      pic  9(11)                  .
      *            *---------------------------------------------------*
      *            * Massimo elementi                                  *
      *            *---------------------------------------------------*
               10  w-stp-bol-wrk-max      pic  9(03)    value 500     .
      *            *---------------------------------------------------*
      *            * Tabella elementi                                  *
      *            *---------------------------------------------------*
               10  w-stp-bol-wrk-tbl.
      *                *-----------------------------------------------*
      *                * Singole righe che compongono la tabella       *
      *                *-----------------------------------------------*
                   15  w-stp-bol-wrk-rig occurs 500.
      *                    *-------------------------------------------*
      *                    * Estremi bolla                             *
      *                    *-------------------------------------------*
                       20  w-stp-bol-tip-doc
                                          pic  x(05)                  .
                       20  w-stp-bol-num-doc
                                          pic  9(11)                  .
                       20  w-stp-bol-dat-doc
                                          pic  9(07)                  .
      *                    *-------------------------------------------*
      *                    * Cumulo precedenti                         *
      *                    *-------------------------------------------*
                       20  w-stp-bol-cum-pre
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mesi 01..09                        *
      *                    *-------------------------------------------*
                       20  w-stp-bol-cum-mic
                                          pic s9(11)       comp-3     .
                       20  w-stp-bol-cum-ms1
                                          pic s9(11)       comp-3     .
                       20  w-stp-bol-cum-ms2
                                          pic s9(11)       comp-3     .
                       20  w-stp-bol-cum-ms3
                                          pic s9(11)       comp-3     .
                       20  w-stp-bol-cum-ms4
                                          pic s9(11)       comp-3     .
                       20  w-stp-bol-cum-ms5
                                          pic s9(11)       comp-3     .
                       20  w-stp-bol-cum-ms6
                                          pic s9(11)       comp-3     .
                       20  w-stp-bol-cum-ms7
                                          pic s9(11)       comp-3     .
                       20  w-stp-bol-cum-ms8
                                          pic s9(11)       comp-3     .
                       20  w-stp-bol-cum-ms9
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo successivi                         *
      *                    *-------------------------------------------*
                       20  w-stp-bol-cum-suc
                                          pic s9(11)       comp-3     .

      *    *===========================================================*
      *    * Work area per cumulo scadenze in portafoglio cliente      *
      *    *-----------------------------------------------------------*
       01  w-stp-gep.
      *        *-------------------------------------------------------*
      *        * Sub-work per cumulo valori                            *
      *        *-------------------------------------------------------*
           05  w-stp-gep-wrk.
      *            *---------------------------------------------------*
      *            * Numero elementi rilevati                          *
      *            *---------------------------------------------------*
               10  w-stp-gep-wrk-ele      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo per prompt                                 *
      *            *---------------------------------------------------*
               10  w-stp-gep-wrk-pmt      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Contatore di comodo                               *
      *            *---------------------------------------------------*
               10  w-stp-gep-wrk-ctr      pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Massimo elementi                                  *
      *            *---------------------------------------------------*
               10  w-stp-gep-wrk-max      pic  9(03)    value 500     .
      *            *---------------------------------------------------*
      *            * Tabella elementi                                  *
      *            *---------------------------------------------------*
               10  w-stp-gep-wrk-tbl.
      *                *-----------------------------------------------*
      *                * Singole righe che compongono la tabella       *
      *                *-----------------------------------------------*
                   15  w-stp-gep-wrk-rig occurs 500.
      *                    *-------------------------------------------*
      *                    * Estremi scadenza                          *
      *                    *-------------------------------------------*
                       20  w-stp-gep-tip-doc
                                          pic  x(05)                  .
                       20  w-stp-gep-num-doc
                                          pic  9(11)                  .
                       20  w-stp-gep-dat-doc
                                          pic  9(07)                  .
      *                    *-------------------------------------------*
      *                    * Cumulo precedenti                         *
      *                    *-------------------------------------------*
                       20  w-stp-gep-cum-pre
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo mesi 01..09                        *
      *                    *-------------------------------------------*
                       20  w-stp-gep-cum-mic
                                          pic s9(11)       comp-3     .
                       20  w-stp-gep-cum-ms1
                                          pic s9(11)       comp-3     .
                       20  w-stp-gep-cum-ms2
                                          pic s9(11)       comp-3     .
                       20  w-stp-gep-cum-ms3
                                          pic s9(11)       comp-3     .
                       20  w-stp-gep-cum-ms4
                                          pic s9(11)       comp-3     .
                       20  w-stp-gep-cum-ms5
                                          pic s9(11)       comp-3     .
                       20  w-stp-gep-cum-ms6
                                          pic s9(11)       comp-3     .
                       20  w-stp-gep-cum-ms7
                                          pic s9(11)       comp-3     .
                       20  w-stp-gep-cum-ms8
                                          pic s9(11)       comp-3     .
                       20  w-stp-gep-cum-ms9
                                          pic s9(11)       comp-3     .
      *                    *-------------------------------------------*
      *                    * Cumulo successivi                         *
      *                    *-------------------------------------------*
                       20  w-stp-gep-cum-suc
                                          pic s9(11)       comp-3     .

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
      *        * Comodo per rottura su data consegna prevista          *
      *        *-------------------------------------------------------*
           05  w-roc-rot-prv              pic  9(07)                  .
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
      *                    * Data consegna prevista                    *
      *                    *-------------------------------------------*
                       20  w-roc-key-prv  pic  9(07)                  .
      *                    *-------------------------------------------*
      *                    * Numero progressivo riga                   *
      *                    *-------------------------------------------*
                       20  w-roc-key-prg  pic  9(05)                  .
      *                *-----------------------------------------------*
      *                * Dati del buffer                               *
      *                *-----------------------------------------------*
                   15  w-roc-dti-buf.
      *                    *-------------------------------------------*
      *                    * Numero progressivo riga                   *
      *                    *-------------------------------------------*
                       20  filler         pic  x(01)                  .

      *    *===========================================================*
      *    * Work-area locale                                          *
      *    *-----------------------------------------------------------*
       01  w-wrk.
           05  w-wrk-num-prt              pic  9(11)                  .
           05  w-wrk-num-prt-r            redefines
               w-wrk-num-prt                                          .
               10  w-wrk-npt-saa          pic  9(03)                  .
               10  w-wrk-npt-dpz          pic  9(02)                  .
               10  w-wrk-npt-prg          pic  9(06)                  .
           05  w-wrk-prt-stp              pic  9(09)                  .
           05  w-wrk-prt-stp-r            redefines
               w-wrk-prt-stp                                          .
               10  w-wrk-nps-saa          pic  9(03)                  .
               10  w-wrk-nps-prg          pic  9(06)                  .
           05  w-wrk-des-ant              pic  x(25)                  .
           05  w-wrk-num-edt              pic  x(06)                  .
           05  w-wrk-dat-edt              pic  x(08)                  .

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
      *    * Area di comunicazione per determinazione importo in riga  *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dimpven0.dtl"                   .

      *    *===========================================================*
      *    * Link-area per espansione su righe ordine                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/porc301x.pgl"                   .

      *    *===========================================================*
      *    * Link-area per espansione su righe ordine spedizione       *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/pods301x.pgl"                   .

      *    *===========================================================*
      *    * Link-area per espansione su righe bolla                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/pbol301x.pgl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione stato ordine     *
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dstsorc0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine cliente                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dqevroc0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione stato ordine     *
      *    * di spedizione cliente                                     *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/dstsods0.dtl"                   .

      *    *===========================================================*
      *    * Area di comunicazione per determinazione quantita' da e-  *
      *    * vadere riga ordine di spedizione cliente                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/dqdsros0.dtl"                   .

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
               10  w-rot-l01-cod-cli      pic  9(07)                  .

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
      *        *-------------------------------------------------------*
      *        * Sistema applicativo                                   *
      *        *-------------------------------------------------------*
           05  i-ide-sap                  pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Area gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-arg                  pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Settore gestionale                                    *
      *        *-------------------------------------------------------*
           05  i-ide-set                  pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Fase gestionale                                       *
      *        *-------------------------------------------------------*
           05  i-ide-fas                  pic  x(06)                  .
      *        *-------------------------------------------------------*
      *        * Sigla interna del programma                           *
      *        *-------------------------------------------------------*
           05  i-ide-pro                  pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione del programma                             *
      *        *-------------------------------------------------------*
           05  i-ide-des                  pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per tipi interrogazione                         *
      *    *-----------------------------------------------------------*
       01  w-tin.
      *        *-------------------------------------------------------*
      *        * Tabella tipi interrogazione e dati ad essi associati  *
      *        *-------------------------------------------------------*
           05  w-tin-tbl-tin.
      *            *---------------------------------------------------*
      *            * Indice per puntamento su elemento in tabella ed   *
      *            * altri indici di comodo                            *
      *            *---------------------------------------------------*
               10  w-tin-ele-inx          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Comodo per caricamento iniziale elementi          *
      *            *---------------------------------------------------*
               10  w-tin-ele-wci.
                   15  w-tin-ele-wci-des  pic  x(50)                  .
                   15  filler             pic  x(01)                  .
                   15  w-tin-ele-wci-alf  pic  x(10)                  .
                   15  filler             pic  x(01)                  .
                   15  w-tin-ele-wci-ovy  pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Numero effettivo di elementi in tabella           *
      *            *---------------------------------------------------*
               10  w-tin-ele-num          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero di elementi per pagina                     *
      *            *---------------------------------------------------*
               10  w-tin-ele-nep          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero di pagine totali                           *
      *            *---------------------------------------------------*
               10  w-tin-ele-npt          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero pagina attualmente visualizzata            *
      *            *---------------------------------------------------*
               10  w-tin-ele-pag          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Max numero di elementi in tabella                 *
      *            *---------------------------------------------------*
               10  w-tin-ele-max          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Elementi per tipi interrogazione                  *
      *            *---------------------------------------------------*
               10  w-tin-ele-tin occurs 50.
      *                *-----------------------------------------------*
      *                * Codice numerico tipo interrogazione           *
      *                *-----------------------------------------------*
                   15  w-tin-num-tin      pic  9(04)                  .
      *                *-----------------------------------------------*
      *                * Sigla alfanumerica tipo interrogazione        *
      *                *-----------------------------------------------*
                   15  w-tin-alf-tin      pic  x(10)                  .
      *                *-----------------------------------------------*
      *                * Descrizione tipo interrogazione               *
      *                *-----------------------------------------------*
                   15  w-tin-des-tin      pic  x(50)                  .
      *                *-----------------------------------------------*
      *                * Overlay da richiamare per il tipo di interro- *
      *                * gazione                                       *
      *                *-----------------------------------------------*
                   15  w-tin-ovy-tin      pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area per bufferizzazione campi accettazione campi    *
      *    * chiave trattati dal main                                  *
      *    *-----------------------------------------------------------*
       01  w-key.
      *        *-------------------------------------------------------*
      *        * Codice numerico tipo interrogazione                   *
      *        *-------------------------------------------------------*
           05  w-key-num-int              pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Codice alfanumerico tipo interrogazione               *
      *        *-------------------------------------------------------*
           05  w-key-alf-int              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Descrizione per codice tipo interrogazione            *
      *        *-------------------------------------------------------*
           05  w-key-des-int              pic  x(50)                  .
      *        *-------------------------------------------------------*
      *        * Overlay per il tipo interrogazione                    *
      *        *-------------------------------------------------------*
           05  w-key-ovy-int              pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area per richiamo overlay per l'esecuzione effettiva *
      *    * del tipo di interrogazione                                *
      *    *-----------------------------------------------------------*
       01  w-ovy-exe.
      *        *-------------------------------------------------------*
      *        * Pathname per il richiamo della overlay                *
      *        *-------------------------------------------------------*
           05  w-ovy-exe-pat.
      *            *---------------------------------------------------*
      *            * Prefisso comune                                   *
      *            *---------------------------------------------------*
               10  w-ovy-exe-pre          pic  x(16)                  .
      *            *---------------------------------------------------*
      *            * Postfisso variabile                               *
      *            *---------------------------------------------------*
               10  w-ovy-exe-pos          pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Salvataggio per il postfisso variabile                *
      *        *-------------------------------------------------------*
           05  w-ovy-exe-inx              pic  9(02)                  .
           05  w-ovy-exe-spv occurs 20    pic  x(10)                  .

      *    *===========================================================*
      *    * Work-area per sottoprogrammi attivi della fase            *
      *    *-----------------------------------------------------------*
       01  w-spg.
      *        *-------------------------------------------------------*
      *        * Work per test se sottoprogramma gia' attivo, codice   *
      *        * alfanumerico del tipo di interrogazione               *
      *        *-------------------------------------------------------*
           05  w-spg-alf-gat              pic  x(10)                  .
      *        *-------------------------------------------------------*
      *        * Work per test se sottoprogramma gia' attivo, risposta *
      *        * - Spaces : No                                         *
      *        * - S      : Si                                         *
      *        *-------------------------------------------------------*
           05  w-spg-snx-gat              pic  x(01)                  .
      *        *-------------------------------------------------------*
      *        * Tabella dei sottoprogrammi attivi                     *
      *        *-------------------------------------------------------*
           05  w-spg-tbl-spg.
      *            *---------------------------------------------------*
      *            * Indice per puntamento in tabella                  *
      *            *---------------------------------------------------*
               10  w-spg-ele-inx          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero effettivo di elementi in tabella           *
      *            *---------------------------------------------------*
               10  w-spg-ele-num          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Max numero di elementi in tabella                 *
      *            *---------------------------------------------------*
               10  w-spg-ele-max          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Elementi per sottoprogrammi attivi                *
      *            *---------------------------------------------------*
               10  w-spg-ele-spg occurs 99.
      *                *-----------------------------------------------*
      *                * Sigla alfanumerica tipo interrogazione        *
      *                *-----------------------------------------------*
                   15  w-spg-alf-tin      pic  x(10)                  .

      ******************************************************************
       Procedure Division                using i-ide
                                               w-tin
                                               w-key
                                               w-ovy-exe
                                               w-spg                  .
      ******************************************************************

      *================================================================*
      *       Main                                                     *
      *================================================================*
       main-000.
      *              *-------------------------------------------------*
      *              * Salvataggio immagine video                      *
      *              *-------------------------------------------------*
           move      "SV"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *              *-------------------------------------------------*
      *              * Esecuzione routine pre-esecuzione programma     *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-pre-exe-pgm      .
           perform   pre-exe-pgm-000      thru pre-exe-pgm-999        .
           if        w-cnt-pre-exe-pgm    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Preparazione tipo funzionamento                 *
      *              *-------------------------------------------------*
           perform   pre-tip-fun-000      thru pre-tip-fun-999        .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-rou-opn-fls      .
           perform   rou-opn-fls-000      thru rou-opn-fls-999        .
           if        w-cnt-rou-opn-fls    not  = spaces
                     go to main-900.
      *              *-------------------------------------------------*
      *              * Segnale primo giro di esecuzione                *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-prm-gir      .
      *              *-------------------------------------------------*
      *              * Se no richieste : a esecuzione interrogazione   *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-500.
       main-250.
      *              *-------------------------------------------------*
      *              * Accettazione richieste di selezione             *
      *              *-------------------------------------------------*
           perform   acc-ric-sel-000      thru acc-ric-sel-999        .
      *                  *---------------------------------------------*
      *                  * Se uscita per Exit                          *
      *                  *---------------------------------------------*
           if        w-cnt-acc-ric-sel    =    "E"
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Regolarizzazione richieste di selezione         *
      *              *-------------------------------------------------*
           perform   reg-ric-sel-000      thru reg-ric-sel-999        .
       main-500.
      *              *-------------------------------------------------*
      *              * Esecuzione interrogazione                       *
      *              *-------------------------------------------------*
           perform   exe-int-err-000      thru exe-int-err-999        .
      *              *-------------------------------------------------*
      *              * Se no richieste : a fine programma              *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S"
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Test se fine esecuzione da operatore            *
      *              *-------------------------------------------------*
           if        w-cnt-qry-rou-pri    not  = spaces
                     go to main-750.
      *              *-------------------------------------------------*
      *              * Segnale non piu' primo giro di esecuzione       *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-prm-gir      .
      *              *-------------------------------------------------*
      *              * Test se tipo esecuzione ciclico                 *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-ric    not  = "S" or
                     w-cnt-fun-snx-cic    not  = "S"
                     go to main-750
           else      go to main-250.
       main-750.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   rou-cls-fls-000      thru rou-cls-fls-999        .
       main-900.
      *              *-------------------------------------------------*
      *              * Esecuzione routine post-esecuzione programma    *
      *              *-------------------------------------------------*
           perform   pos-exe-pgm-000      thru pos-exe-pgm-999        .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
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
      *    * Esecuzione interrogazione                                 *
      *    *-----------------------------------------------------------*
       exe-int-err-000.
      *              *-------------------------------------------------*
      *              * Open files per interrogazione                   *
      *              *-------------------------------------------------*
           perform   qry-opn-fls-000      thru qry-opn-fls-999        .
       exe-int-err-100.
      *              *-------------------------------------------------*
      *              * Esecuzione eventuale sort preliminare           *
      *              *-------------------------------------------------*
           perform   exe-rou-srt-000      thru exe-rou-srt-999        .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda se sort eseguito           *
      *              *-------------------------------------------------*
           if        w-cnt-exe-rou-srt    =    spaces
                     go to exe-int-err-200
           else      go to exe-int-err-300.
       exe-int-err-200.
      *              *-------------------------------------------------*
      *              * Se sort non eseguito                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ciclo di interrogazione vero e proprio      *
      *                  *---------------------------------------------*
           perform   qry-rou-pri-000      thru qry-rou-pri-999        .
      *                  *---------------------------------------------*
      *                  * A chiusura files per interrogazione         *
      *                  *---------------------------------------------*
           go to     exe-int-err-900.
       exe-int-err-300.
      *              *-------------------------------------------------*
      *              * Se sort eseguito                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * A chiusura files per interrogazione         *
      *                  *---------------------------------------------*
           go to     exe-int-err-900.
       exe-int-err-900.
      *              *-------------------------------------------------*
      *              * Close files per interrogazione                  *
      *              *-------------------------------------------------*
           perform   qry-cls-fls-000      thru qry-cls-fls-999        .
       exe-int-err-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Routine principale                       *
      *    *-----------------------------------------------------------*
       qry-rou-pri-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-rou-pri      .
      *              *-------------------------------------------------*
      *              * Determinazione function-keys in Mark-points     *
      *              *-------------------------------------------------*
           perform   qry-det-fky-000      thru qry-det-fky-999        .
      *              *-------------------------------------------------*
      *              * Begin o Begin Automatico                        *
      *              *-------------------------------------------------*
           if        w-cnt-fun-snx-aut    =    "S"
                     move   "BA"          to   v-ope
           else      move   "BE"          to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Open files                                      *
      *              *-------------------------------------------------*
           perform   qry-opn-fls-000      thru qry-opn-fls-999        .
      *              *-------------------------------------------------*
      *              * Inizializzazione marker di primo giro           *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Inizializzazione flag di interruzione forzata   *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
      *              *-------------------------------------------------*
      *              * Inizializzazione area per rotture di livello    *
      *              *-------------------------------------------------*
           move      spaces               to   w-rot                  .
      *              *-------------------------------------------------*
      *              * Start iniziale                                  *
      *              *-------------------------------------------------*
           perform   qry-str-ini-000      thru qry-str-ini-999        .
           if        w-cnt-qry-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-qry-flg-sub
                     go to  qry-rou-pri-600.
       qry-rou-pri-100.
      *              *-------------------------------------------------*
      *              * Salvataggio area rottura in area precedente     *
      *              *-------------------------------------------------*
           move      w-rot-l05            to   w-cnt-qry-sav-l05      .
           move      w-rot-l04            to   w-cnt-qry-sav-l04      .
           move      w-rot-l03            to   w-cnt-qry-sav-l03      .
           move      w-rot-l02            to   w-cnt-qry-sav-l02      .
           move      w-rot-l01            to   w-cnt-qry-sav-l01      .
       qry-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale                             *
      *              *-------------------------------------------------*
           perform   qry-let-seq-000      thru qry-let-seq-999        .
           if        w-cnt-qry-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-qry-flg-sub
                     go to  qry-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se superamento limiti massimi              *
      *              *-------------------------------------------------*
           perform   qry-tst-max-000      thru qry-tst-max-999        .
           if        w-cnt-qry-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-qry-flg-sub
                     go to  qry-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Selezione su record letto                       *
      *              *-------------------------------------------------*
           perform   qry-sel-rec-000      thru qry-sel-rec-999        .
           if        w-cnt-qry-flg-sub    not  = spaces
                     move   spaces        to   w-cnt-qry-flg-sub
                     go to  qry-rou-pri-200.
      *              *-------------------------------------------------*
      *              * Composizione area per tests di rottura          *
      *              *-------------------------------------------------*
           perform   qry-cmp-rot-000      thru qry-cmp-rot-999        .
      *              *-------------------------------------------------*
      *              * Test se primo passaggio                         *
      *              *-------------------------------------------------*
           if        w-cnt-qry-mrk-uno    not  = spaces
                     go to qry-rou-pri-300.
      *                  *---------------------------------------------*
      *                  * Inizio di tutti i livelli                   *
      *                  *---------------------------------------------*
           perform   qry-rou-pri-790      thru qry-rou-pri-791        .
           perform   qry-rou-pri-750      thru qry-rou-pri-751        .
           perform   qry-rou-pri-740      thru qry-rou-pri-741        .
           perform   qry-rou-pri-730      thru qry-rou-pri-731        .
           perform   qry-rou-pri-720      thru qry-rou-pri-721        .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
           go to     qry-rou-pri-400.
       qry-rou-pri-300.
      *              *-------------------------------------------------*
      *              * Se rottura del 5. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l05            =    w-cnt-qry-sav-l05
                     go to qry-rou-pri-310.
           move      w-rot                to   w-cnt-qry-sav-rot      .
           move      w-cnt-qry-sav-l05    to   w-rot-l05              .
           move      w-cnt-qry-sav-l04    to   w-rot-l04              .
           move      w-cnt-qry-sav-l03    to   w-rot-l03              .
           move      w-cnt-qry-sav-l02    to   w-rot-l02              .
           move      w-cnt-qry-sav-l01    to   w-rot-l01              .
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           perform   qry-rou-pri-820      thru qry-rou-pri-821        .
           perform   qry-rou-pri-830      thru qry-rou-pri-831        .
           perform   qry-rou-pri-840      thru qry-rou-pri-841        .
           perform   qry-rou-pri-850      thru qry-rou-pri-851        .
           move      w-cnt-qry-sav-rot    to   w-rot                  .
           perform   qry-rou-pri-750      thru qry-rou-pri-751        .
           perform   qry-rou-pri-740      thru qry-rou-pri-741        .
           perform   qry-rou-pri-730      thru qry-rou-pri-731        .
           perform   qry-rou-pri-720      thru qry-rou-pri-721        .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
           go to     qry-rou-pri-400.
       qry-rou-pri-310.
      *              *-------------------------------------------------*
      *              * Se rottura del 4. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l04            =    w-cnt-qry-sav-l04
                     go to qry-rou-pri-320.
           move      w-rot                to   w-cnt-qry-sav-rot      .
           move      w-cnt-qry-sav-l04    to   w-rot-l04              .
           move      w-cnt-qry-sav-l03    to   w-rot-l03              .
           move      w-cnt-qry-sav-l02    to   w-rot-l02              .
           move      w-cnt-qry-sav-l01    to   w-rot-l01              .
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           perform   qry-rou-pri-820      thru qry-rou-pri-821        .
           perform   qry-rou-pri-830      thru qry-rou-pri-831        .
           perform   qry-rou-pri-840      thru qry-rou-pri-841        .
           move      w-cnt-qry-sav-rot    to   w-rot                  .
           perform   qry-rou-pri-740      thru qry-rou-pri-741        .
           perform   qry-rou-pri-730      thru qry-rou-pri-731        .
           perform   qry-rou-pri-720      thru qry-rou-pri-721        .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
           go to     qry-rou-pri-400.
       qry-rou-pri-320.
      *              *-------------------------------------------------*
      *              * Se rottura del 3. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l03            =    w-cnt-qry-sav-l03
                     go to qry-rou-pri-330.
           move      w-rot                to   w-cnt-qry-sav-rot      .
           move      w-cnt-qry-sav-l03    to   w-rot-l03              .
           move      w-cnt-qry-sav-l02    to   w-rot-l02              .
           move      w-cnt-qry-sav-l01    to   w-rot-l01              .
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           perform   qry-rou-pri-820      thru qry-rou-pri-821        .
           perform   qry-rou-pri-830      thru qry-rou-pri-831        .
           move      w-cnt-qry-sav-rot    to   w-rot                  .
           perform   qry-rou-pri-730      thru qry-rou-pri-731        .
           perform   qry-rou-pri-720      thru qry-rou-pri-721        .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
           go to     qry-rou-pri-400.
       qry-rou-pri-330.
      *              *-------------------------------------------------*
      *              * Se rottura del 2. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l02            =    w-cnt-qry-sav-l02
                     go to qry-rou-pri-340.
           move      w-rot                to   w-cnt-qry-sav-rot      .
           move      w-cnt-qry-sav-l02    to   w-rot-l02              .
           move      w-cnt-qry-sav-l01    to   w-rot-l01              .
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           perform   qry-rou-pri-820      thru qry-rou-pri-821        .
           move      w-cnt-qry-sav-rot    to   w-rot                  .
           perform   qry-rou-pri-720      thru qry-rou-pri-721        .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
           go to     qry-rou-pri-400.
       qry-rou-pri-340.
      *              *-------------------------------------------------*
      *              * Se rottura del 1. livello                       *
      *              *-------------------------------------------------*
           if        w-rot-l01            =    w-cnt-qry-sav-l01
                     go to qry-rou-pri-400.
           move      w-rot                to   w-cnt-qry-sav-rot      .
           move      w-cnt-qry-sav-l01    to   w-rot-l01              .
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           move      w-cnt-qry-sav-rot    to   w-rot                  .
           perform   qry-rou-pri-710      thru qry-rou-pri-711        .
       qry-rou-pri-400.
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione attivo : fine ciclo  *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Livello di dettaglio                            *
      *              *-------------------------------------------------*
           perform   qry-liv-det-000      thru qry-liv-det-999        .
      *              *-------------------------------------------------*
      *              * Se segnale di interruzione : fine ciclo         *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Segnale di passaggio successivo al primo        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-qry-mrk-uno      .
      *              *-------------------------------------------------*
      *              * Riciclo a lettura sequenziale file principale   *
      *              *-------------------------------------------------*
           go to     qry-rou-pri-100.
       qry-rou-pri-500.
      *              *-------------------------------------------------*
      *              * Test se almeno un passaggio                     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-mrk-uno    =    spaces
                     go to qry-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Fine di tutti i livelli                         *
      *              *-------------------------------------------------*
           perform   qry-rou-pri-810      thru qry-rou-pri-811        .
           perform   qry-rou-pri-820      thru qry-rou-pri-821        .
           perform   qry-rou-pri-830      thru qry-rou-pri-831        .
           perform   qry-rou-pri-840      thru qry-rou-pri-841        .
           perform   qry-rou-pri-850      thru qry-rou-pri-851        .
           perform   qry-rou-pri-890      thru qry-rou-pri-891        .
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Stop                                            *
      *              *-------------------------------------------------*
           move      "ST"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Subroutine di avanzamento pagina                *
      *              *-------------------------------------------------*
           perform   qry-pag-adv-000      thru qry-pag-adv-999        .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     qry-rou-pri-900.
       qry-rou-pri-600.
      *              *-------------------------------------------------*
      *              * Esecuzione per nessuna registrazione da elab.   *
      *              *-------------------------------------------------*
           perform   qry-nes-ela-000      thru qry-nes-ela-999        .
           go to     qry-rou-pri-900.
       qry-rou-pri-710.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 1. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-711.
           perform   qry-ini-lr1-000      thru qry-ini-lr1-999        .
       qry-rou-pri-711.
           exit.
       qry-rou-pri-720.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 2. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-721.
           perform   qry-ini-lr2-000      thru qry-ini-lr2-999        .
       qry-rou-pri-721.
           exit.
       qry-rou-pri-730.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 3. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-731.
           perform   qry-ini-lr3-000      thru qry-ini-lr3-999        .
       qry-rou-pri-731.
           exit.
       qry-rou-pri-740.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 4. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-741.
           perform   qry-ini-lr4-000      thru qry-ini-lr4-999        .
       qry-rou-pri-741.
           exit.
       qry-rou-pri-750.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio 5. livello di rottura     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-751.
           perform   qry-ini-lr5-000      thru qry-ini-lr5-999        .
       qry-rou-pri-751.
           exit.
       qry-rou-pri-790.
      *              *-------------------------------------------------*
      *              * Esecuzione per inizio ciclo                     *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-791.
           perform   qry-ini-cic-000      thru qry-ini-cic-999        .
       qry-rou-pri-791.
           exit.
       qry-rou-pri-810.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 1. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-811.
           perform   qry-fin-lr1-000      thru qry-fin-lr1-999        .
       qry-rou-pri-811.
           exit.
       qry-rou-pri-820.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 2. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-821.
           perform   qry-fin-lr2-000      thru qry-fin-lr2-999        .
       qry-rou-pri-821.
           exit.
       qry-rou-pri-830.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 3. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-831.
           perform   qry-fin-lr3-000      thru qry-fin-lr3-999        .
       qry-rou-pri-831.
           exit.
       qry-rou-pri-840.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 4. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-841.
           perform   qry-fin-lr4-000      thru qry-fin-lr4-999        .
       qry-rou-pri-841.
           exit.
       qry-rou-pri-850.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine 5. livello di rottura       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-851.
           perform   qry-fin-lr5-000      thru qry-fin-lr5-999        .
       qry-rou-pri-851.
           exit.
       qry-rou-pri-890.
      *              *-------------------------------------------------*
      *              * Esecuzione per fine ciclo                       *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-rou-pri-891.
           perform   qry-fin-cic-000      thru qry-fin-cic-999        .
       qry-rou-pri-891.
           exit.
       qry-rou-pri-900.
      *              *-------------------------------------------------*
      *              * Close files                                     *
      *              *-------------------------------------------------*
           perform   qry-cls-fls-000      thru qry-cls-fls-999        .
       qry-rou-pri-950.
      *              *-------------------------------------------------*
      *              * End                                             *
      *              *-------------------------------------------------*
           move      "EN"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-rou-pri-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Subroutine di avanzamento pagina         *
      *    *-----------------------------------------------------------*
       qry-pag-adv-000.
      *              *-------------------------------------------------*
      *              * Avanzamento pagina                              *
      *              *-------------------------------------------------*
           move      "PA"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Test su esito interazione con operatore         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se continuazione normale                    *
      *                  *---------------------------------------------*
           if        v-key                =    spaces
                     go to qry-pag-adv-999.
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata da operatore        *
      *                  *---------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "#"            to   w-cnt-qry-flg-int
                     go to qry-pag-adv-999.
      *                  *---------------------------------------------*
      *                  * Se function-key prevista                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Trattamento function-key                *
      *                      *-----------------------------------------*
           perform   qry-trt-fun-000      thru qry-trt-fun-999        .
      *                      *-----------------------------------------*
      *                      * Test su rientro da trattamento f-key    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se fine ciclo interrogazione   *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    =    spaces
                     go to qry-pag-adv-000.
       qry-pag-adv-999.
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
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * il tipo di chiamante del sottoprogramma, se il  *
      *              * main oppure se un altro sottoprogramma          *
      *              *-------------------------------------------------*
           perform   ipc-tdc-mos-000      thru ipc-tdc-mos-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * l'ammissibilita' del tasto Slct                 *
      *              *-------------------------------------------------*
           perform   ipc-snx-slc-000      thru ipc-snx-slc-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * la data di riferimento per l'analisi            *
      *              *-------------------------------------------------*
           perform   ipc-dat-rfa-000      thru ipc-dat-rfa-999        .
      *              *-------------------------------------------------*
      *              * Lettura della variabile eventuale di i.p.c. per *
      *              * il debitore e l'eventuale dipendenza            *
      *              *-------------------------------------------------*
           perform   ipc-cod-dbt-000      thru ipc-cod-dbt-999        .
      *              *-------------------------------------------------*
      *              * Memorizzazione sottoprogramma in attivita'      *
      *              *-------------------------------------------------*
           move      "ESPCLI    "         to   w-spg-alf-gat          .
           perform   mem-spg-att-000      thru mem-spg-att-999        .
      *              *-------------------------------------------------*
      *              * Se memorizzazione non avvenuta : uscita con er- *
      *              * rore                                            *
      *              *-------------------------------------------------*
           if        w-spg-snx-gat        not  = spaces
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazioni e referenze           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Si/No proposta tutte dipendenze come        *
      *                  * default di selezione per i clienti          *
      *                  *---------------------------------------------*
           perform   prs-snx-tdd-dcc-000  thru prs-snx-tdd-dcc-999    .
      *                  *---------------------------------------------*
      *                  * Tabella [zsf] : Spese per la fatturazione e *
      *                  *                 referenze per sconti a      *
      *                  *                 piede fattura               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Open tabella [zsf]                      *
      *                      *-----------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *                      *-----------------------------------------*
      *                      * Spese per la fatturazione               *
      *                      *-----------------------------------------*
           perform   prs-spe-fat-000      thru prs-spe-fat-999        .
      *                      *-----------------------------------------*
      *                      * Referenze per lo sconto in chiusura     *
      *                      *-----------------------------------------*
           perform   ref-sco-chi-000      thru ref-sco-chi-999        .
      *                      *-----------------------------------------*
      *                      * Referenze per lo sconto pagamento       *
      *                      *-----------------------------------------*
           perform   ref-sco-pag-000      thru ref-sco-pag-999        .
      *                      *-----------------------------------------*
      *                      * Close tabella [zsf]                     *
      *                      *-----------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Si/No proposta tutte dipen-   *
      *    *                             denze come default di sele-   *
      *    *                             zione cliente                 *
      *    *-----------------------------------------------------------*
       prs-snx-tdd-dcc-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione                       *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/dcc[snx-tdd]"   to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-ves                =    spaces
                     move  s-alf          to   w-prs-snx-tdd-dcc
           else      move  spaces         to   w-prs-snx-tdd-dcc      .
      *              *-------------------------------------------------*
      *              * Normalizzazione personalizzazione               *
      *              *-------------------------------------------------*
           if        w-prs-snx-tdd-dcc    not   = "N" and
                     w-prs-snx-tdd-dcc    not   = "S"
                     move  "N"            to   w-prs-snx-tdd-dcc      .
       prs-snx-tdd-dcc-999.
           exit.

      *    *===========================================================*
      *    * Lettura personalizzazione : Spese per la fatturazione     *
      *    *-----------------------------------------------------------*
       prs-spe-fat-000.
      *              *-------------------------------------------------*
      *              * Lettura personalizzazione : Tipo accettazione   *
      *              * spesa                                           *
      *              *-------------------------------------------------*
           move      "P:"                 to   s-ope                  .
           move      "pgm/fat/mov/fat300[tac-spf]"
                                          to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        s-ves                not  = spaces
                     move  zero           to   w-prs-spe-fat-tac
           else      move  s-num          to   w-prs-spe-fat-tac      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione personalizzazione           *
      *                  *---------------------------------------------*
           if        w-prs-spe-fat-tac    not  = 00 and
                     w-prs-spe-fat-tac    not  = 01
                     move  00             to   w-prs-spe-fat-tac      .
      *              *-------------------------------------------------*
      *              * Numero di spese personalizzate caricate : zero  *
      *              *-------------------------------------------------*
           move      zero                 to   w-prs-spe-fat-nst      .
       prs-spe-fat-200.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura spese da 1 a 6 con carica- *
      *              * mento in tabella delle spese trovate            *
      *              *-------------------------------------------------*
       prs-spe-fat-250.
      *                  *---------------------------------------------*
      *                  * Indice 1..6 a zero                          *
      *                  *---------------------------------------------*
           move      zero                 to   w-prs-spe-fat-i01      .
       prs-spe-fat-300.
      *                  *---------------------------------------------*
      *                  * Incremento indice 1..6                      *
      *                  *---------------------------------------------*
           add       1                    to   w-prs-spe-fat-i01      .
      *                  *---------------------------------------------*
      *                  * Se oltre il max : a chiusura                *
      *                  *---------------------------------------------*
           if        w-prs-spe-fat-i01    >    6
                     go to prs-spe-fat-900.
       prs-spe-fat-400.
      *                  *---------------------------------------------*
      *                  * Tentativo di lettura della tabella per la   *
      *                  * spesa corrispondente all'indice, e con co-  *
      *                  * dice lingua per Italia                      *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPF    "         to   f-key                  .
           move      w-prs-spe-fat-i01    to   rf-zsf-num-spf         .
           move      "I  "                to   rf-zsf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione dell'esito della     *
      *                  * lettura                                     *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to prs-spe-fat-500
           else      go to prs-spe-fat-700.
       prs-spe-fat-500.
      *                  *---------------------------------------------*
      *                  * Se record spesa esistente                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Controllo di correttezza, e se non e'   *
      *                      * superato si ricicla a spesa successiva  *
      *                      *-----------------------------------------*
           if        rf-zsf-tfu-spe       <    01 or
                     rf-zsf-tfu-spe       >    05
                     go to prs-spe-fat-800.
      *                      *-----------------------------------------*
      *                      * Bufferizzazione spesa                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Incremento numero di spese persona- *
      *                          * lizzate caricate                    *
      *                          *-------------------------------------*
           add       1                    to   w-prs-spe-fat-nst      .
      *                          *-------------------------------------*
      *                          * Numero spesa                        *
      *                          *-------------------------------------*
           move      w-prs-spe-fat-i01    to   w-prs-spe-fat-npt
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Descrizione per il video estesa     *
      *                          *-------------------------------------*
           move      rf-zsf-des-ves       to   w-prs-spe-fat-dve
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Tipo funzionamento spesa            *
      *                          *-------------------------------------*
           move      rf-zsf-tfu-spe       to   w-prs-spe-fat-tfs
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Percentuale per la spesa            *
      *                          *-------------------------------------*
           move      rf-zsf-per-spe       to   w-prs-spe-fat-per
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Tipo di imponibile per la spesa     *
      *                          *-------------------------------------*
           move      rf-zsf-ibl-spe       to   w-prs-spe-fat-ibl
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Totalizzatori imponibile spesa      *
      *                          *-------------------------------------*
           move      rf-zsf-ibt-spe       to   w-prs-spe-fat-ibt
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Importo spesa                       *
      *                          *-------------------------------------*
           move      rf-zsf-imp-spe       to   w-prs-spe-fat-imp
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Codice iva spesa                    *
      *                          *-------------------------------------*
           move      rf-zsf-civ-spe       to   w-prs-spe-fat-civ
                                              (w-prs-spe-fat-nst)     .
      *                          *-------------------------------------*
      *                          * Contropartita spesa                 *
      *                          *-------------------------------------*
           move      rf-zsf-ccp-spe       to   w-prs-spe-fat-ccp
                                              (w-prs-spe-fat-nst)     .
      *                      *-----------------------------------------*
      *                      * Riciclo su spesa successiva             *
      *                      *-----------------------------------------*
           go to     prs-spe-fat-800.
       prs-spe-fat-700.
      *                  *---------------------------------------------*
      *                  * Se record spesa non esistente               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Riciclo a spesa successiva              *
      *                      *-----------------------------------------*
           go to     prs-spe-fat-800.
       prs-spe-fat-800.
      *                  *---------------------------------------------*
      *                  * Riciclo a spesa successiva                  *
      *                  *---------------------------------------------*
           go to     prs-spe-fat-300.
       prs-spe-fat-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     prs-spe-fat-999.
       prs-spe-fat-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle referenze relative allo sconto in chiusura  *
      *    *-----------------------------------------------------------*
       ref-sco-chi-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione work-area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-ref-sco-chi-des      .
           move      zero                 to   w-ref-sco-chi-civ      .
           move      zero                 to   w-ref-sco-chi-ccp      .
       ref-sco-chi-200.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura della tabella per sconto   *
      *              * in chiusura con codice lingua per l'Italia      *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPF    "         to   f-key                  .
           move      101                  to   rf-zsf-num-spf         .
           move      "I  "                to   rf-zsf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione dell'esito della     *
      *                  * lettura                                     *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to ref-sco-chi-500
           else      go to ref-sco-chi-700.
       ref-sco-chi-500.
      *                  *---------------------------------------------*
      *                  * Se record sconto esistente                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valori letti            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione per il video estesa     *
      *                          *-------------------------------------*
           move      rf-zsf-des-ves       to   w-ref-sco-chi-des      .
      *                          *-------------------------------------*
      *                          * Codice iva spesa                    *
      *                          *-------------------------------------*
           move      rf-zsf-civ-spe       to   w-ref-sco-chi-civ      .
      *                          *-------------------------------------*
      *                          * Contropartita spesa                 *
      *                          *-------------------------------------*
           move      rf-zsf-ccp-spe       to   w-ref-sco-chi-ccp      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ref-sco-chi-999.
       ref-sco-chi-700.
      *                  *---------------------------------------------*
      *                  * Se record sconto non esistente              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Default per la descrizione sconto in    *
      *                      * chiusura                                *
      *                      *-----------------------------------------*
           move      "Sconto incondizionato    "
                                          to   w-ref-sco-chi-des      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ref-sco-chi-999.
       ref-sco-chi-999.
           exit.

      *    *===========================================================*
      *    * Lettura delle referenze relative allo sconto pagamento    *
      *    *-----------------------------------------------------------*
       ref-sco-pag-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione work-area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-ref-sco-pag-des      .
           move      zero                 to   w-ref-sco-pag-civ      .
           move      zero                 to   w-ref-sco-pag-ccp      .
       ref-sco-pag-200.
      *              *-------------------------------------------------*
      *              * Tentativo di lettura della tabella per sconto   *
      *              * pagamento con codice lingua per l'Italia        *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPF    "         to   f-key                  .
           move      102                  to   rf-zsf-num-spf         .
           move      "I  "                to   rf-zsf-cod-lng         .
           move      "pgm/dcc/fls/ioc/obj/iofzsf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zsf                 .
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione dell'esito della     *
      *                  * lettura                                     *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to ref-sco-pag-500
           else      go to ref-sco-pag-700.
       ref-sco-pag-500.
      *                  *---------------------------------------------*
      *                  * Se record sconto pagamento esistente        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Bufferizzazione valori letti            *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Descrizione per il video estesa     *
      *                          *-------------------------------------*
           move      rf-zsf-des-ves       to   w-ref-sco-pag-des      .
      *                          *-------------------------------------*
      *                          * Codice iva spesa                    *
      *                          *-------------------------------------*
           move      rf-zsf-civ-spe       to   w-ref-sco-pag-civ      .
      *                          *-------------------------------------*
      *                          * Contropartita spesa                 *
      *                          *-------------------------------------*
           move      rf-zsf-ccp-spe       to   w-ref-sco-pag-ccp      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ref-sco-pag-999.
       ref-sco-pag-700.
      *                  *---------------------------------------------*
      *                  * Se record sconto pagamento non esistente    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Default per la descrizione sconto       *
      *                      * pagamento                               *
      *                      *-----------------------------------------*
           move      "Sconto pagamento         "
                                          to   w-ref-sco-pag-des      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     ref-sco-pag-999.
       ref-sco-pag-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Eliminazione sottoprogramma in attivita'        *
      *              *-------------------------------------------------*
           move      "ESPCLI    "         to   w-spg-alf-gat          .
           perform   eli-spg-att-000      thru eli-spg-att-999        .
       pos-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per il tipo   *
      *    * di chiamante del sottoprogramma, se il main o un altro    *
      *    * sottoprogramma                                            *
      *    *-----------------------------------------------------------*
       ipc-tdc-mos-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'tdc-mos' dallo      *
      *              * stesso livello di profondita'                   *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "tdc-mos"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito dell'operazione *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to ipc-tdc-mos-200
           else      go to ipc-tdc-mos-400.
       ipc-tdc-mos-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-tdc-mos-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-tdc-mos-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tdc-mos-999.
       ipc-tdc-mos-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-tdc-mos-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a spaces             *
      *                  *---------------------------------------------*
           move      spaces               to   w-ipc-tdc-mos-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-tdc-mos-999.
       ipc-tdc-mos-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per l'ammis-  *
      *    * sibilita' del tasto Slct                                  *
      *    *-----------------------------------------------------------*
       ipc-snx-slc-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'snx-slc' dallo      *
      *              * stesso livello di profondita'                   *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "snx-slc"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito dell'operazione *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to ipc-snx-slc-200
           else      go to ipc-snx-slc-400.
       ipc-snx-slc-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-snx-slc-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-snx-slc-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-snx-slc-999.
       ipc-snx-slc-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-snx-slc-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a spaces             *
      *                  *---------------------------------------------*
           move      spaces               to   w-ipc-snx-slc-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-snx-slc-999.
       ipc-snx-slc-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per la data   *
      *    * di riferimento per l'analisi                              *
      *    *-----------------------------------------------------------*
       ipc-dat-rfa-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'dat-rfa' dallo      *
      *              * stesso livello di profondita'                   *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dat-rfa"            to   s-var                  .
           move      "="                  to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito dell'operazione *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to ipc-dat-rfa-200
           else      go to ipc-dat-rfa-400.
       ipc-dat-rfa-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-dat-rfa-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-dat                to   w-ipc-dat-rfa-val      .
      *                  *---------------------------------------------*
      *                  * Se valore della variabile a zero : come per *
      *                  * variabile non esistente                     *
      *                  *---------------------------------------------*
           if        w-ipc-dat-rfa-val    =    zero
                     go to ipc-dat-rfa-400.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri di accettazione      *
      *                  *---------------------------------------------*
           move      w-ipc-dat-rfa-val    to   rr-dat-rfa             .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-dat-rfa-999.
       ipc-dat-rfa-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-dat-rfa-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-dat-rfa-val      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-dat-rfa-999.
       ipc-dat-rfa-999.
           exit.

      *    *===========================================================*
      *    * Lettura della variabile eventuale di i.p.c. per il debi-  *
      *    * tore e l'eventuale sua dipendenza                         *
      *    *-----------------------------------------------------------*
       ipc-cod-dbt-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'cod-dbt' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "cod-dbt"            to   s-var                  .
           if        w-ipc-tdc-mos-snx    =    "S" and
                     w-ipc-tdc-mos-val    =    "M"
                     move  "-"            to   s-dop
           else      move  "="            to   s-dop                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito dell'operazione *
      *              *-------------------------------------------------*
           if        s-ves                =    spaces
                     go to ipc-cod-dbt-200
           else      go to ipc-cod-dbt-400.
       ipc-cod-dbt-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-cod-dbt-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-cod-dbt-val      .
      *                  *---------------------------------------------*
      *                  * Controllo del valore della variabile        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione eventuale per il tipo   *
      *                      * debitore                                *
      *                      *-----------------------------------------*
           if        w-ipc-cod-dbt-tip    =    zero
                     move  01             to   w-ipc-cod-dbt-tip      .
      *                      *-----------------------------------------*
      *                      * Controllo sul tipo debitore             *
      *                      *-----------------------------------------*
           if        w-ipc-cod-dbt-tip    not  numeric
                     go to ipc-cod-dbt-400.
           if        w-ipc-cod-dbt-tip    not  = 01 and
                     w-ipc-cod-dbt-tip    not  = 02
                     go to ipc-cod-dbt-400.
      *                      *-----------------------------------------*
      *                      * Controllo sul codice debitore           *
      *                      *-----------------------------------------*
           if        w-ipc-cod-dbt-cod    not  numeric
                     go to ipc-cod-dbt-400.
           if        w-ipc-cod-dbt-cod    =    zero
                     go to ipc-cod-dbt-400.
      *                      *-----------------------------------------*
      *                      * Normalizzazione eventuale per il codi-  *
      *                      * ce dipendenza per il debitore           *
      *                      *-----------------------------------------*
           if        w-ipc-cod-dbt-tip    not  = 01
                     move  spaces         to   w-ipc-cod-dbt-dpz      .
      *                  *---------------------------------------------*
      *                  * Preparazione parametri di accettazione      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Codice cliente                          *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      w-ipc-cod-dbt-cod    to   rr-cod-cli             .
      *                          *-------------------------------------*
      *                          * Lettura archivio [cli]              *
      *                          *-------------------------------------*
           move      rr-cod-cli           to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                          *-------------------------------------*
      *                          * Se esito operazione negativo : come *
      *                          * per variabile non trovata           *
      *                          *-------------------------------------*
           if        w-let-arc-cli-flg    not  = spaces
                     go to ipc-cod-dbt-400.
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      w-let-arc-cli-rag    to   rr-cod-cli-rag         .
           move      w-let-arc-cli-via    to   rr-cod-cli-via         .
           move      w-let-arc-cli-loc    to   rr-cod-cli-loc         .
      *                      *-----------------------------------------*
      *                      * Dipendenza cliente                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      w-ipc-cod-dbt-dpz    to   rr-dpz-cli             .
      *                          *-------------------------------------*
      *                          * Lettura archivio [dcc]              *
      *                          *-------------------------------------*
           move      rr-cod-cli           to   w-let-arc-dcc-cod      .
           move      rr-dpz-cli           to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                          *-------------------------------------*
      *                          * Se esito operazione negativo : come *
      *                          * per variabile non trovata           *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-flg    not  = spaces
                     go to ipc-cod-dbt-400.
      *                          *-------------------------------------*
      *                          * Bufferizzazione                     *
      *                          *-------------------------------------*
           move      w-let-arc-dcc-rag    to   rr-dpz-cli-rag         .
           move      w-let-arc-dcc-via    to   rr-dpz-cli-via         .
           move      w-let-arc-dcc-loc    to   rr-dpz-cli-loc         .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-cod-dbt-999.
       ipc-cod-dbt-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-cod-dbt-snx      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione del valore della variabile, *
      *                  * nelle sue componenti :                      *
      *                  * - Tipo debitore                             *
      *                  * - Codice debitore                           *
      *                  * - Codice dipendenza per il debitore         *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-cod-dbt-tip      .
           move      zero                 to   w-ipc-cod-dbt-cod      .
           move      spaces               to   w-ipc-cod-dbt-dpz      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-cod-dbt-999.
       ipc-cod-dbt-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su esito lettura variabili di i.p.c.   *
      *                  *---------------------------------------------*
           if        w-ipc-dat-rfa-snx    =    "S"   and
                     w-ipc-cod-dbt-snx    =    "S"
                     move  "N"            to   w-cnt-fun-snx-ric
           else      move  "S"            to   w-cnt-fun-snx-ric      .
       pre-tip-fun-200.
      *              *-------------------------------------------------*
      *              * Si/No funzionamento ciclico                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su esito lettura variabili di i.p.c.   *
      *                  *---------------------------------------------*
           if        w-ipc-dat-rfa-snx    =    "S"   and
                     w-ipc-cod-dbt-snx    =    "S"
                     move  "N"            to   w-cnt-fun-snx-cic
           else      move  "S"            to   w-cnt-fun-snx-cic      .
       pre-tip-fun-300.
      *              *-------------------------------------------------*
      *              * Si/No funzionamento automatico : Si             *
      *              *-------------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-aut      .
       pre-tip-fun-400.
      *              *-------------------------------------------------*
      *              * Preparazione work-area richieste                *
      *              *-------------------------------------------------*
       pre-tip-fun-500.
      *                  *---------------------------------------------*
      *                  * Data di riferimento per l'analisi           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non esiste una variabile di i.p.c.   *
      *                      * relativa alla data di riferimento per   *
      *                      * l'analisi : no preparazione default     *
      *                      *-----------------------------------------*
           if        w-ipc-dat-rfa-snx    not  = "S"
                     go to pre-tip-fun-600.
      *                      *-----------------------------------------*
      *                      * Se la variabile di i.p.c. ha un valore  *
      *                      * non ammesso : no preparazione default   *
      *                      *-----------------------------------------*
           if        w-ipc-dat-rfa-val    =    zero
                     go to pre-tip-fun-600.
      *                      *-----------------------------------------*
      *                      * Preparazione default                    *
      *                      *-----------------------------------------*
           move      w-ipc-dat-rfa-val    to   rr-dat-rfa             .
       pre-tip-fun-600.
      *                  *---------------------------------------------*
      *                  * Codice cliente ed eventuale dipendenza      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non esiste una variabile di i.p.c.   *
      *                      * relativa al codice cliente ed eventu-   *
      *                      * ale sua dipendenza : no preparazione    *
      *                      * default                                 *
      *                      *-----------------------------------------*
           if        w-ipc-cod-dbt-snx    not  = "S"
                     go to pre-tip-fun-700.
      *                      *-----------------------------------------*
      *                      * Se la variabile di i.p.c. ha un valore  *
      *                      * non ammesso : no preparazione default   *
      *                      *-----------------------------------------*
           if        w-ipc-cod-dbt-tip    not  = 01
                     go to pre-tip-fun-700.
           if        w-ipc-cod-dbt-cod    =    zero
                     go to pre-tip-fun-700.
      *                      *-----------------------------------------*
      *                      * Preparazione default per codice cliente *
      *                      *-----------------------------------------*
           move      w-ipc-cod-dbt-cod    to   rr-cod-cli             .
      *                      *-----------------------------------------*
      *                      * Preparazione dati associati al codice   *
      *                      * cliente                                 *
      *                      *-----------------------------------------*
           move      rr-cod-cli           to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
           move      w-let-arc-cli-rag    to   rr-cod-cli-rag         .
           move      w-let-arc-cli-via    to   rr-cod-cli-via         .
           move      w-let-arc-cli-loc    to   rr-cod-cli-loc         .
      *                      *-----------------------------------------*
      *                      * Preparazione default per codice dipen-  *
      *                      * denza del cliente                       *
      *                      *-----------------------------------------*
           move      w-ipc-cod-dbt-dpz    to   rr-dpz-cli             .
      *                      *-----------------------------------------*
      *                      * Preparazione dati associati al codice   *
      *                      * dipendenza del cliente                  *
      *                      *-----------------------------------------*
           if        rr-dpz-cli           =    spaces
                     move  "C"            to   w-let-arc-dcc-tle
           else      move  "D"            to   w-let-arc-dcc-tle      .
           move      rr-cod-cli           to   w-let-arc-dcc-cod      .
           move      rr-dpz-cli           to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           move      w-let-arc-dcc-rag    to   rr-dpz-cli-rag         .
           move      w-let-arc-dcc-via    to   rr-dpz-cli-via         .
           move      w-let-arc-dcc-loc    to   rr-dpz-cli-loc         .
       pre-tip-fun-700.
      *                  *---------------------------------------------*
      *                  * Tipo evidenziazione                         *
      *                  *---------------------------------------------*
           move      01                   to   rr-tip-evd             .
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
      *              * Open modulo accettazione codice cliente commer- *
      *              * ciale                                           *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-opn-000  thru cod-mne-dcc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione codice dipendenza del  *
      *              * cliente                                         *
      *              *-------------------------------------------------*
           perform   cod-cod-dcc-opn-000  thru cod-cod-dcc-opn-999    .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice cliente com-   *
      *              * merciale                                        *
      *              *-------------------------------------------------*
           perform   cod-mne-dcc-cls-000  thru cod-mne-dcc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione codice dipendenza del *
      *              * cliente                                         *
      *              *-------------------------------------------------*
           perform   cod-cod-dcc-cls-000  thru cod-cod-dcc-cls-999    .
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
      *                  * Data di riferimento per l'analisi           *
      *                  *---------------------------------------------*
           perform   acc-dat-rfa-000      thru acc-dat-rfa-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
       acc-ric-sel-150.
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           perform   acc-cod-cli-000      thru acc-cod-cli-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del cliente               *
      *                  *---------------------------------------------*
           perform   acc-dpz-cli-000      thru acc-dpz-cli-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-150.
       acc-ric-sel-250.
      *                  *---------------------------------------------*
      *                  * Selezione su tipo evidenziazione            *
      *                  *---------------------------------------------*
           perform   acc-tip-evd-000      thru acc-tip-evd-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
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
           move      "Conferma esecuzione (S/N/E) ?"
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
      *              *-------------------------------------------------*
      *              * Prompt tipo analisi                             *
      *              *-------------------------------------------------*
           perform   pmt-tip-ana-000      thru pmt-tip-ana-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo analisi                    *
      *              *-------------------------------------------------*
           perform   vis-tip-ana-000      thru vis-tip-ana-999        .
      *              *-------------------------------------------------*
      *              * Prompt linea di separazione                     *
      *              *-------------------------------------------------*
           perform   pmt-lin-spz-000      thru pmt-lin-spz-999        .
      *              *-------------------------------------------------*
      *              * Prompt data di riferimento per l'analisi        *
      *              *-------------------------------------------------*
           perform   pmt-dat-rfa-000      thru pmt-dat-rfa-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione data di riferimento per l'ana-  *
      *              * lisi                                            *
      *              *-------------------------------------------------*
           perform   vis-dat-rfa-000      thru vis-dat-rfa-999        .
      *              *-------------------------------------------------*
      *              * Prompt codice cliente                           *
      *              *-------------------------------------------------*
           perform   pmt-cod-cli-000      thru pmt-cod-cli-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione codice cliente                  *
      *              *-------------------------------------------------*
           perform   vis-cod-cli-000      thru vis-cod-cli-999        .
           perform   vis-cod-cli-rag-000  thru vis-cod-cli-rag-999    .
           perform   vis-cod-cli-via-000  thru vis-cod-cli-via-999    .
           perform   vis-cod-cli-loc-000  thru vis-cod-cli-loc-999    .
      *              *-------------------------------------------------*
      *              * Prompt codice dipendenza cliente                *
      *              *-------------------------------------------------*
           perform   pmt-dpz-cli-000      thru pmt-dpz-cli-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione codice dipendenza del cliente   *
      *              *-------------------------------------------------*
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
           perform   vis-dpz-cli-rag-000  thru vis-dpz-cli-rag-999    .
           perform   vis-dpz-cli-via-000  thru vis-dpz-cli-via-999    .
           perform   vis-dpz-cli-loc-000  thru vis-dpz-cli-loc-999    .
      *              *-------------------------------------------------*
      *              * Prompt per tipo evidenziazione                  *
      *              *-------------------------------------------------*
           perform   pmt-tip-evd-000      thru pmt-tip-evd-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo evidenziazione             *
      *              *-------------------------------------------------*
           perform   vis-tip-evd-000      thru vis-tip-evd-999        .
       pmt-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Prompt tipo analisi                                       *
      *    *-----------------------------------------------------------*
       pmt-tip-ana-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo di analisi            :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-ana-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Tipo analisi                            *
      *    *-----------------------------------------------------------*
       vis-tip-ana-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-key-des-int        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-tip-ana-999.
           exit.

      *    *===========================================================*
      *    * Prompt linea di separazione                               *
      *    *-----------------------------------------------------------*
       pmt-lin-spz-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-lin-spz-999.
           exit.

      *    *===========================================================*
      *    * Prompt data di riferimento per l'analisi                  *
      *    *-----------------------------------------------------------*
       pmt-dat-rfa-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      06                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data riferimento analisi   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-rfa-999.
           exit.

      *    *===========================================================*
      *    * Prompt codice cliente                                     *
      *    *-----------------------------------------------------------*
       pmt-cod-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Codice cliente             :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Prompt codice dipendenza del cliente                      *
      *    *-----------------------------------------------------------*
       pmt-dpz-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Dipendenza del cliente     :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dpz-cli-999.
           exit.

      *    *===========================================================*
      *    * Prompt per tipo evidenziazione                            *
      *    *-----------------------------------------------------------*
       pmt-tip-evd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      16                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipo evidenziazione        :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-evd-999.
           exit.

      *    *===========================================================*
      *    * Accettazione : Data di riferimento per l'analisi          *
      *    *-----------------------------------------------------------*
       acc-dat-rfa-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-dat-rfa-025.
      *                  *---------------------------------------------*
      *                  * Preparazione eventuale default              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se data gia' presente : no preparazione *
      *                      *-----------------------------------------*
           if        rr-dat-rfa           not  = zero
                     go to acc-dat-rfa-100.
      *                      *-----------------------------------------*
      *                      * Estrazione system date and time attuale *
      *                      * da modulo di segreteria                 *
      *                      *-----------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                      *-----------------------------------------*
      *                      * Preparazione data di default            *
      *                      *-----------------------------------------*
           move      s-dat                to   rr-dat-rfa             .
       acc-dat-rfa-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      spaces               to   v-edm                  .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-rfa           to   v-dat                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dat-rfa-150.
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dat-rfa-999.
       acc-dat-rfa-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-dat                to   rr-dat-rfa             .
       acc-dat-rfa-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-dat-rfa-425.
      *                  *---------------------------------------------*
      *                  * Test che il valore non manchi               *
      *                  *---------------------------------------------*
           if        rr-dat-rfa           =    zero
                     go to acc-dat-rfa-100.
       acc-dat-rfa-450.
      *                  *---------------------------------------------*
      *                  * Fine controlli                              *
      *                  *---------------------------------------------*
           go to     acc-dat-rfa-600.
       acc-dat-rfa-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dat-rfa-625.
      *                  *---------------------------------------------*
      *                  * Scrittura variabile di i.p.c. 'dat-rfa' per *
      *                  * lo stesso livello di profondita' applicati- *
      *                  * va per la data di riferimento per l'analisi *
      *                  *---------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "dat-rfa"            to   s-var                  .
           move      "="                  to   s-dop                  .
           move      "D"                  to   s-tip                  .
           move      rr-dat-rfa           to   s-dat                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
       acc-dat-rfa-650.
      *                  *---------------------------------------------*
      *                  * Fine dipendenze                             *
      *                  *---------------------------------------------*
           go to     acc-dat-rfa-800.
       acc-dat-rfa-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dat-rfa-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dat-rfa-100.
       acc-dat-rfa-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Data di riferimento per l'analisi       *
      *    *-----------------------------------------------------------*
       vis-dat-rfa-000.
           move      "DS"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      06                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-rfa           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-rfa-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice cliente                       *
      *    *-----------------------------------------------------------*
       acc-cod-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-cod-cli-100.
      *              *-------------------------------------------------*
      *              * Salvataggio valore precedente                   *
      *              *-------------------------------------------------*
           move      rr-cod-cli           to   w-sav-cod-cli          .
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   w-cod-mne-dcc-ope      .
           move      rr-cod-cli           to   w-cod-mne-dcc-cod      .
           move      08                   to   w-cod-mne-dcc-lin      .
           move      30                   to   w-cod-mne-dcc-pos      .
           move      08                   to   w-cod-mne-dcc-rln      .
           move      41                   to   w-cod-mne-dcc-rps      .
           move      09                   to   w-cod-mne-dcc-vln      .
           move      41                   to   w-cod-mne-dcc-vps      .
           move      10                   to   w-cod-mne-dcc-lln      .
           move      41                   to   w-cod-mne-dcc-lps      .
           move      "<B"                 to   v-edm                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
       acc-cod-cli-110.
           perform   cod-mne-dcc-cll-000  thru cod-mne-dcc-cll-999    .
           if        w-cod-mne-dcc-ope    =    "F+"
                     go to acc-cod-cli-115.
           if        w-cod-mne-dcc-ope    =    "AC"
                     go to acc-cod-cli-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-cod-cli-115.
           perform   cod-mne-dcc-foi-000  thru cod-mne-dcc-foi-999    .
           go to     acc-cod-cli-110.
       acc-cod-cli-120.
           move      w-cod-mne-dcc-cod    to   v-num                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-cod-cli-999.
       acc-cod-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-cod-cli             .
       acc-cod-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
       acc-cod-cli-410.
      *                  *---------------------------------------------*
      *                  * Lettura file [cli]                          *
      *                  *---------------------------------------------*
           move      rr-cod-cli           to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
      *                  *---------------------------------------------*
      *                  * Memorizzazione ragione sociale, indirizzo,  *
      *                  * localita', e sottoconto associato da ana-   *
      *                  * grafica contabile                           *
      *                  *---------------------------------------------*
           move      w-let-arc-cli-rag    to   rr-cod-cli-rag         .
           move      w-let-arc-cli-via    to   rr-cod-cli-via         .
           move      w-let-arc-cli-loc    to   rr-cod-cli-loc         .
       acc-cod-cli-420.
      *                  *---------------------------------------------*
      *                  * Visualizzazione ragione sociale da anagra-  *
      *                  * fica contabile                              *
      *                  *---------------------------------------------*
           perform   vis-cod-cli-rag-000  thru vis-cod-cli-rag-999    .
           perform   vis-cod-cli-via-000  thru vis-cod-cli-via-999    .
           perform   vis-cod-cli-loc-000  thru vis-cod-cli-loc-999    .
       acc-cod-cli-430.
      *                  *---------------------------------------------*
      *                  * Se cliente non esistente                    *
      *                  *---------------------------------------------*
           if        w-let-arc-cli-flg    =    spaces
                     go to acc-cod-cli-440.
       acc-cod-cli-432.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione di-   *
      *                      * pendenza del cliente                    *
      *                      *-----------------------------------------*
           if        rr-dpz-cli           =    spaces and
                     rr-dpz-cli-rag       =    spaces and
                     rr-dpz-cli-via       =    spaces and
                     rr-dpz-cli-loc       =    spaces
                     go to acc-cod-cli-434.
           move      spaces               to   rr-dpz-cli             .
           move      spaces               to   rr-dpz-cli-rag         .
           move      spaces               to   rr-dpz-cli-via         .
           move      spaces               to   rr-dpz-cli-loc         .
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
           perform   vis-dpz-cli-rag-000  thru vis-dpz-cli-rag-999    .
           perform   vis-dpz-cli-via-000  thru vis-dpz-cli-via-999    .
           perform   vis-dpz-cli-loc-000  thru vis-dpz-cli-loc-999    .
       acc-cod-cli-434.
      *                      *-----------------------------------------*
      *                      * A reimpostazione                        *
      *                      *-----------------------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-440.
      *                  *---------------------------------------------*
      *                  * Se codice a zero                            *
      *                  *---------------------------------------------*
           if        rr-cod-cli           not  = zero
                     go to acc-cod-cli-450.
       acc-cod-cli-442.
      *                      *-----------------------------------------*
      *                      * Normalizzazione e visualizzazione di-   *
      *                      * pendenza del cliente                    *
      *                      *-----------------------------------------*
           if        rr-dpz-cli           =    spaces and
                     rr-dpz-cli-rag       =    spaces and
                     rr-dpz-cli-via       =    spaces and
                     rr-dpz-cli-loc       =    spaces
                     go to acc-cod-cli-444.
           move      spaces               to   rr-dpz-cli             .
           move      spaces               to   rr-dpz-cli-rag         .
           move      spaces               to   rr-dpz-cli-via         .
           move      spaces               to   rr-dpz-cli-loc         .
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
           perform   vis-dpz-cli-rag-000  thru vis-dpz-cli-rag-999    .
           perform   vis-dpz-cli-via-000  thru vis-dpz-cli-via-999    .
           perform   vis-dpz-cli-loc-000  thru vis-dpz-cli-loc-999    .
       acc-cod-cli-444.
      *                      *-----------------------------------------*
      *                      * Reimpostazione, a meno che non si sia   *
      *                      * in tasto Up                             *
      *                      *-----------------------------------------*
           if        v-key                =    "UP  "
                     go to acc-cod-cli-600
           else      go to acc-cod-cli-100.
       acc-cod-cli-450.
      *                  *---------------------------------------------*
      *                  * Se codice diverso da zero                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda del confronto con  *
      *                      * il valore precedente                    *
      *                      *-----------------------------------------*
           if        rr-cod-cli           =    w-sav-cod-cli
                     go to acc-cod-cli-455
           else      go to acc-cod-cli-480.
       acc-cod-cli-455.
      *                      *-----------------------------------------*
      *                      * Se valore impostato pari al valore pre- *
      *                      * cedente                                 *
      *                      *-----------------------------------------*
       acc-cod-cli-457.
      *                          *-------------------------------------*
      *                          * Lettura anagrafica commerciale del  *
      *                          * cliente principale                  *
      *                          *-------------------------------------*
           move      "C"                  to   w-let-arc-dcc-tle      .
           move      rr-cod-cli           to   w-let-arc-dcc-cod      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                          *-------------------------------------*
      *                          * Deviazione a seconda dell'esito     *
      *                          * della lettura                       *
      *                          *-------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-cod-cli-467.
       acc-cod-cli-459.
      *                          *-------------------------------------*
      *                          * Se anagrafica commerciale del cli-  *
      *                          * ente principale non esistente       *
      *                          *-------------------------------------*
       acc-cod-cli-461.
      *                              *---------------------------------*
      *                              * Normalizzazione e visualizza-   *
      *                              * zione dei dati relativi alla    *
      *                              * dipendenza                      *
      *                              *---------------------------------*
           if        rr-dpz-cli           =    spaces and
                     rr-dpz-cli-rag       =    spaces and
                     rr-dpz-cli-via       =    spaces and
                     rr-dpz-cli-loc       =    spaces
                     go to acc-cod-cli-463.
           move      spaces               to   rr-dpz-cli             .
           move      spaces               to   rr-dpz-cli-rag         .
           move      spaces               to   rr-dpz-cli-via         .
           move      spaces               to   rr-dpz-cli-loc         .
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
           perform   vis-dpz-cli-rag-000  thru vis-dpz-cli-rag-999    .
           perform   vis-dpz-cli-via-000  thru vis-dpz-cli-via-999    .
           perform   vis-dpz-cli-loc-000  thru vis-dpz-cli-loc-999    .
       acc-cod-cli-463.
      *                              *---------------------------------*
      *                              * Messaggio di errore             *
      *                              *---------------------------------*
           move      "Manca l'anagrafica commerciale per il cliente     
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
       acc-cod-cli-465.
      *                              *---------------------------------*
      *                              * A reimpostazione                *
      *                              *---------------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-467.
      *                          *-------------------------------------*
      *                          * Se anagrafica commerciale del cli-  *
      *                          * ente principale esistente           *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Memorizzazione valori letti da  *
      *                              * anagrafica commerciale cliente  *
      *                              * principale in dati per la di-   *
      *                              * pendenza                        *
      *                              *---------------------------------*
           move      w-let-arc-dcc-rag    to   rr-dpz-cli-rag         .
           move      w-let-arc-dcc-via    to   rr-dpz-cli-via         .
           move      w-let-arc-dcc-loc    to   rr-dpz-cli-loc         .
      *                              *---------------------------------*
      *                              * Deviazione a seconda se il co-  *
      *                              * dice dipendenza e' a spaces op- *
      *                              * pure diverso da spaces          *
      *                              *---------------------------------*
           if        rr-dpz-cli           =    spaces
                     go to acc-cod-cli-469
           else      go to acc-cod-cli-471.
       acc-cod-cli-469.
      *                              *---------------------------------*
      *                              * Se codice dipendenza a spaces   *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Visualizzazione valori re-  *
      *                                  * lativi alla dipendenza      *
      *                                  *-----------------------------*
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
           perform   vis-dpz-cli-rag-000  thru vis-dpz-cli-rag-999    .
           perform   vis-dpz-cli-via-000  thru vis-dpz-cli-via-999    .
           perform   vis-dpz-cli-loc-000  thru vis-dpz-cli-loc-999    .
      *                                  *-----------------------------*
      *                                  * A dipendenze dall'imposta-  *
      *                                  * zione                       *
      *                                  *-----------------------------*
           go to     acc-cod-cli-600.
       acc-cod-cli-471.
      *                              *---------------------------------*
      *                              * Se codice dipendenza diverso da *
      *                              * spaces                          *
      *                              *---------------------------------*
      *                                  *-----------------------------*
      *                                  * Lettura anagrafica commer-  *
      *                                  * ciale della dipendenza      *
      *                                  *-----------------------------*
           move      "D"                  to   w-let-arc-dcc-tle      .
           move      rr-cod-cli           to   w-let-arc-dcc-cod      .
           move      rr-dpz-cli           to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                                  *-----------------------------*
      *                                  * Deviazione a seconda del-   *
      *                                  * l'esito della lettura       *
      *                                  *-----------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-cod-cli-473
           else      go to acc-cod-cli-475.
       acc-cod-cli-473.
      *                                  *-----------------------------*
      *                                  * Se anagrafica commerciale   *
      *                                  * della dipendenza esistente  *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Memorizzazione valori   *
      *                                      * letti da anagrafica     *
      *                                      * commerciale della di-   *
      *                                      * pendenza                *
      *                                      *-------------------------*
           move      w-let-arc-dcc-rag    to   rr-dpz-cli-rag         .
           move      w-let-arc-dcc-via    to   rr-dpz-cli-via         .
           move      w-let-arc-dcc-loc    to   rr-dpz-cli-loc         .
      *                                      *-------------------------*
      *                                      * Visualizzazione valori  *
      *                                      * relativi alla dipenden- *
      *                                      * za                      *
      *                                      *-------------------------*
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
           perform   vis-dpz-cli-rag-000  thru vis-dpz-cli-rag-999    .
           perform   vis-dpz-cli-via-000  thru vis-dpz-cli-via-999    .
           perform   vis-dpz-cli-loc-000  thru vis-dpz-cli-loc-999    .
      *                                      *-------------------------*
      *                                      * A dipendenze dall'impo- *
      *                                      * stazione                *
      *                                      *-------------------------*
           go to     acc-cod-cli-600.
       acc-cod-cli-475.
      *                                  *-----------------------------*
      *                                  * Se anagrafica commerciale   *
      *                                  * della dipendenza non esi-   *
      *                                  * stente                      *
      *                                  *-----------------------------*
      *                                      *-------------------------*
      *                                      * Memorizzazione valori   *
      *                                      * letti da anagrafica     *
      *                                      * commerciale della di-   *
      *                                      * pendenza                *
      *                                      *-------------------------*
           move      w-let-arc-dcc-rag    to   rr-dpz-cli-rag         .
           move      w-let-arc-dcc-via    to   rr-dpz-cli-via         .
           move      w-let-arc-dcc-loc    to   rr-dpz-cli-loc         .
      *                                      *-------------------------*
      *                                      * Visualizzazione valori  *
      *                                      * relativi alla dipenden- *
      *                                      * za                      *
      *                                      *-------------------------*
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
           perform   vis-dpz-cli-rag-000  thru vis-dpz-cli-rag-999    .
           perform   vis-dpz-cli-via-000  thru vis-dpz-cli-via-999    .
           perform   vis-dpz-cli-loc-000  thru vis-dpz-cli-loc-999    .
      *                                      *-------------------------*
      *                                      * Messaggio di errore     *
      *                                      *-------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "Manca l'anagrafica commerciale per la dipendenza "
                                delimited by   size
                     "'"        delimited by   size
                     rr-dpz-cli       
                                delimited by spaces
                     "'"        delimited by   size
                                          into w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                                      *-------------------------*
      *                                      * A reimpostazione        *
      *                                      *-------------------------*
           go to     acc-cod-cli-100.
       acc-cod-cli-480.
      *                      *-----------------------------------------*
      *                      * Se valore impostato diverso dal valore  *
      *                      * precedente                              *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Flag di modifica codice dipendenza  *
      *                          *-------------------------------------*
           move      spaces               to   w-sav-dpz-cli          .
       acc-cod-cli-482.
      *                              *---------------------------------*
      *                              * Normalizzazione e visualizza-   *
      *                              * zione dei dati relativi alla    *
      *                              * dipendenza                      *
      *                              *---------------------------------*
           if        rr-dpz-cli           =    spaces and
                     rr-dpz-cli-rag       =    spaces and
                     rr-dpz-cli-via       =    spaces and
                     rr-dpz-cli-loc       =    spaces
                     go to acc-cod-cli-484.
           move      spaces               to   rr-dpz-cli             .
           move      spaces               to   rr-dpz-cli-rag         .
           move      spaces               to   rr-dpz-cli-via         .
           move      spaces               to   rr-dpz-cli-loc         .
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
           perform   vis-dpz-cli-rag-000  thru vis-dpz-cli-rag-999    .
           perform   vis-dpz-cli-via-000  thru vis-dpz-cli-via-999    .
           perform   vis-dpz-cli-loc-000  thru vis-dpz-cli-loc-999    .
       acc-cod-cli-484.
      *                              *---------------------------------*
      *                              * Riaggancio                      *
      *                              *---------------------------------*
           go to     acc-cod-cli-457.
       acc-cod-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione se presenti dipendenze per   *
      *                  * l'archivio per fatturazione                 *
      *                  *---------------------------------------------*
           move      rr-cod-cli           to   w-det-snd-dcc-cli      .
           perform   det-snd-dcc-000      thru det-snd-dcc-999        .
       acc-cod-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-cod-cli-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-cod-cli-100.
       acc-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice cliente                          *
      *    *-----------------------------------------------------------*
       vis-cod-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      08                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-cod-cli           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice cliente , ragione sociale        *
      *    *-----------------------------------------------------------*
       vis-cod-cli-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      08                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-cli-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cli-rag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice cliente, indirizzo               *
      *    *-----------------------------------------------------------*
       vis-cod-cli-via-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      09                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-cli-via       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cli-via-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice cliente, localita'               *
      *    *-----------------------------------------------------------*
       vis-cod-cli-loc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-cod-cli-loc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-cod-cli-loc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo : Codice dipendenza cliente            *
      *    *-----------------------------------------------------------*
       acc-dpz-cli-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se campo da accettare                  *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test sul segnale che indica la presen-  *
      *                      * za di dipendenze per il cliente in cor- *
      *                      * so di trattamento.                      *
      *                      * Comunque si va a ritarare il Pop-Up     *
      *                      * relativo all'indirizzo di spedizione    *
      *                      *-----------------------------------------*
           if        w-det-snd-dcc-snx    =    "N"
                     go to acc-dpz-cli-999.
      *                      *-----------------------------------------*
      *                      * Test sul codice cliente                 *
      *                      *-----------------------------------------*
           if        rr-cod-cli           =    zero
                     go to acc-dpz-cli-999.
       acc-dpz-cli-050.
      *                  *---------------------------------------------*
      *                  * Eventuale default                           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su personalizzazione               *
      *                      *-----------------------------------------*
           if        w-prs-snx-tdd-dcc    not  = "S"
                     go to acc-dpz-cli-100.
      *                      *-----------------------------------------*
      *                      * Test se valore gia' impostato           *
      *                      *-----------------------------------------*
           if        w-sav-dpz-cli        not  = spaces
                     go to acc-dpz-cli-100.
           if        rr-dpz-cli           not  = spaces
                     go to acc-dpz-cli-100.
      *                      *-----------------------------------------*
      *                      * Test se cliente con piu' di una dipen-  *
      *                      * denza                                   *
      *                      *-----------------------------------------*
           if        w-det-snd-dcc-snx    =    "N"
                     go to acc-dpz-cli-100.
      *                      *-----------------------------------------*
      *                      * Default                                 *
      *                      *-----------------------------------------*
           move      "*   "               to   rr-dpz-cli             .
      *                      *-----------------------------------------*
      *                      * A lettura e visualizzazione             *
      *                      *-----------------------------------------*
           go to     acc-dpz-cli-440.
       acc-dpz-cli-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "A*"                 to   w-cod-cod-dcc-ope      .
           move      rr-cod-cli           to   w-cod-cod-dcc-cli      .
           move      rr-dpz-cli           to   w-cod-cod-dcc-cod      .
           move      12                   to   w-cod-cod-dcc-lin      .
           move      30                   to   w-cod-cod-dcc-pos      .
           move      12                   to   w-cod-cod-dcc-rln      .
           move      41                   to   w-cod-cod-dcc-rps      .
           move      13                   to   w-cod-cod-dcc-vln      .
           move      41                   to   w-cod-cod-dcc-vps      .
           move      14                   to   w-cod-cod-dcc-lln      .
           move      41                   to   w-cod-cod-dcc-lps      .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "FIND"               to   v-pfk (03)             .
           move      "INSR"               to   v-pfk (04)             .
           move      "DO  "               to   v-pfk (05)             .
           perform   cod-cod-dcc-cll-000  thru cod-cod-dcc-cll-999    .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
           perform   cod-cod-dcc-foi-000  thru cod-cod-dcc-foi-999    .
       acc-dpz-cli-110.
           perform   cod-cod-dcc-cll-000  thru cod-cod-dcc-cll-999    .
           if        w-cod-cod-dcc-ope    =    "F+"
                     go to acc-dpz-cli-115.
           if        w-cod-cod-dcc-ope    =    "AC"
                     go to acc-dpz-cli-120.
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
       acc-dpz-cli-115.
           perform   cod-cod-dcc-foi-000  thru cod-cod-dcc-foi-999    .
           go to     acc-dpz-cli-110.
       acc-dpz-cli-120.
           move      w-cod-cod-dcc-cod    to   v-alf                  .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-dpz-cli-999.
       acc-dpz-cli-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-alf                to   rr-dpz-cli             .
       acc-dpz-cli-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di modifica                            *
      *                  *---------------------------------------------*
           move      "#"                  to   w-sav-dpz-cli          .
       acc-dpz-cli-410.
      *                  *---------------------------------------------*
      *                  * Deviazione a seconda se codice sipendenza   *
      *                  * a spaces oppure no                          *
      *                  *---------------------------------------------*
           if        rr-dpz-cli           =    spaces
                     go to acc-dpz-cli-420
           else      go to acc-dpz-cli-440.
       acc-dpz-cli-420.
      *                  *---------------------------------------------*
      *                  * Se codice dipendenza a spaces               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica commerciale del cli- *
      *                      * ente principale                         *
      *                      *-----------------------------------------*
           move      "C"                  to   w-let-arc-dcc-tle      .
           move      rr-cod-cli           to   w-let-arc-dcc-cod      .
           move      spaces               to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                      *-----------------------------------------*
      *                      * Memorizzazione valori letti da anagra-  *
      *                      * fica commerciale cliente principale in  *
      *                      * dati per la dipendenza                  *
      *                      *-----------------------------------------*
           move      w-let-arc-dcc-rag    to   rr-dpz-cli-rag         .
           move      w-let-arc-dcc-via    to   rr-dpz-cli-via         .
           move      w-let-arc-dcc-loc    to   rr-dpz-cli-loc         .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valori relativi alla    *
      *                      * dipendenza                              *
      *                      *-----------------------------------------*
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
           perform   vis-dpz-cli-rag-000  thru vis-dpz-cli-rag-999    .
           perform   vis-dpz-cli-via-000  thru vis-dpz-cli-via-999    .
           perform   vis-dpz-cli-loc-000  thru vis-dpz-cli-loc-999    .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * lettura                                 *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-dpz-cli-425
           else      go to acc-dpz-cli-430.
       acc-dpz-cli-425.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale del cliente   *
      *                      * principale esistente                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A dipendenze dall'impostazione      *
      *                          *-------------------------------------*
           go to     acc-dpz-cli-600.
       acc-dpz-cli-430.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale del cliente   *
      *                      * principale non esistente                *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Messaggio di errore                 *
      *                          *-------------------------------------*
           move      "Manca l'anagrafica commerciale per il cliente     
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-dpz-cli-100.
       acc-dpz-cli-440.
      *                  *---------------------------------------------*
      *                  * Se codice dipendenza diverso da spaces      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica commerciale per la   *
      *                      * dipendenza                              *
      *                      *-----------------------------------------*
           move      "D"                  to   w-let-arc-dcc-tle      .
           move      rr-cod-cli           to   w-let-arc-dcc-cod      .
           move      rr-dpz-cli           to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
      *                      *-----------------------------------------*
      *                      * Memorizzazione valori letti da anagra-  *
      *                      * fica commerciale della dipendenza in    *
      *                      * dati per la dipendenza                  *
      *                      *-----------------------------------------*
           move      w-let-arc-dcc-rag    to   rr-dpz-cli-rag         .
           move      w-let-arc-dcc-via    to   rr-dpz-cli-via         .
           move      w-let-arc-dcc-loc    to   rr-dpz-cli-loc         .
      *                      *-----------------------------------------*
      *                      * Visualizzazione valori relativi alla    *
      *                      * dipendenza                              *
      *                      *-----------------------------------------*
           perform   vis-dpz-cli-000      thru vis-dpz-cli-999        .
           perform   vis-dpz-cli-rag-000  thru vis-dpz-cli-rag-999    .
           perform   vis-dpz-cli-via-000  thru vis-dpz-cli-via-999    .
           perform   vis-dpz-cli-loc-000  thru vis-dpz-cli-loc-999    .
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda dell'esito della   *
      *                      * lettura                                 *
      *                      *-----------------------------------------*
           if        w-let-arc-dcc-flg    =    spaces
                     go to acc-dpz-cli-445
           else      go to acc-dpz-cli-450.
       acc-dpz-cli-445.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale della dipen-  *
      *                      * denza esistente                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * A dipendenze dall'impostazione      *
      *                          *-------------------------------------*
           go to     acc-dpz-cli-600.
       acc-dpz-cli-450.
      *                      *-----------------------------------------*
      *                      * Se anagrafica commerciale della dipen-  *
      *                      * denza non esistente                     *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Messaggio di errore                 *
      *                          *-------------------------------------*
           move      spaces               to   w-err-box-err-msg      .
           string    "Manca l'anagrafica commerciale per la dipendenza "
                                delimited by   size
                     "'"        delimited by   size
                     rr-dpz-cli       
                                delimited by spaces
                     "'"        delimited by   size
                                          into w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
      *                          *-------------------------------------*
      *                          * A reimpostazione                    *
      *                          *-------------------------------------*
           go to     acc-dpz-cli-100.
       acc-dpz-cli-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-dpz-cli-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-dpz-cli-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-dpz-cli-100.
       acc-dpz-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice dipendenza del cliente           *
      *    *-----------------------------------------------------------*
       vis-dpz-cli-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dpz-cli           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-cli-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice dipendenza cliente, ragione so-  *
      *    *                   ciale                                   *
      *    *-----------------------------------------------------------*
       vis-dpz-cli-rag-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-dpz-cli-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-cli-rag-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice dipendenza cliente, indirizzo    *
      *    *-----------------------------------------------------------*
       vis-dpz-cli-via-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-dpz-cli-via       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-cli-via-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione : Codice dipendenza cliente, localita'    *
      *    *-----------------------------------------------------------*
       vis-dpz-cli-loc-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      14                   to   v-lin                  .
           move      41                   to   v-pos                  .
           move      rr-dpz-cli-loc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-dpz-cli-loc-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipo evidenziazione        *
      *    *-----------------------------------------------------------*
       acc-tip-evd-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-evd-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-evd-lun    to   v-car                  .
           move      w-exp-tip-evd-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-evd-tbl    to   v-txt                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tip-evd           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-evd-999.
       acc-tip-evd-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-evd             .
       acc-tip-evd-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non si sia in Up                        *
      *                  *---------------------------------------------*
           if        rr-tip-evd           not  = zero
                     go to acc-tip-evd-600.
           if        v-key                =    "UP  "
                     go to acc-tip-evd-600
           else      go to acc-tip-evd-100.
       acc-tip-evd-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-evd-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-evd-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-evd-100.
       acc-tip-evd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipo evidenziazione               *
      *    *-----------------------------------------------------------*
       vis-tip-evd-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-evd-lun    to   v-car                  .
           move      w-exp-tip-evd-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-evd-tbl    to   v-txt                  .
           move      16                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-evd           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-evd-999.
           exit.

      *    *===========================================================*
      *    * Controllo su tasto Do in parametri di selezione           *
      *    *-----------------------------------------------------------*
       tdo-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione status di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-tdo-ric-flg      .
       tdo-ric-sel-100.
      *              *-------------------------------------------------*
      *              * Controllo su data di riferimento per l'analisi  *
      *              *-------------------------------------------------*
           if        rr-dat-rfa           not  = zero
                     go to tdo-ric-sel-200.
           move      "Manca la data di riferimento per l'analisi        
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Controllo su codice cliente                     *
      *              *-------------------------------------------------*
           if        rr-cod-cli           not  = zero
                     go to tdo-ric-sel-300.
           move      "Manca il codice cliente                           
      -              "               "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
           move      "#"                  to   w-cnt-tdo-ric-flg      .
           go to     tdo-ric-sel-999.
       tdo-ric-sel-300.
      *              *-------------------------------------------------*
      *              * Fine controlli                                  *
      *              *-------------------------------------------------*
           go to     tdo-ric-sel-999.
       tdo-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Regolarizzazione dei parametri di selezione               *
      *    *-----------------------------------------------------------*
       reg-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Tipo evidenziazione                             *
      *              *-------------------------------------------------*
           if        rr-tip-evd           =    zero
                     move  01             to   rr-tip-evd             .
       reg-ric-sel-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
      *              *-------------------------------------------------*
      *              * Se non e' il primo giro di esecuzione : uscita, *
      *              * conservando le impostazioni precedenti come de- *
      *              * faults                                          *
      *              *-------------------------------------------------*
           if        w-cnt-fun-prm-gir    =    "N"
                     go to nor-ric-sel-999.
       nor-ric-sel-200.
      *              *-------------------------------------------------*
      *              * Normalizzazioni                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Data di riferimento per l'analisi           *
      *                  *---------------------------------------------*
           move      zero                 to   rr-dat-rfa             .
      *                  *---------------------------------------------*
      *                  * Codice cliente                              *
      *                  *---------------------------------------------*
           move      zero                 to   rr-cod-cli             .
           move      spaces               to   rr-cod-cli-rag         .
           move      spaces               to   rr-cod-cli-via         .
           move      spaces               to   rr-cod-cli-loc         .
      *                  *---------------------------------------------*
      *                  * Codice dipendenza del cliente               *
      *                  *---------------------------------------------*
           move      spaces               to   rr-dpz-cli             .
           move      spaces               to   rr-dpz-cli-rag         .
           move      spaces               to   rr-dpz-cli-via         .
           move      spaces               to   rr-dpz-cli-loc         .
      *                  *---------------------------------------------*
      *                  * Tipo evidenziazione                         *
      *                  *---------------------------------------------*
           move      zero                 to   rr-tip-evd             .
       nor-ric-sel-400.
      *              *-------------------------------------------------*
      *              * Preparazione defaults                           *
      *              *-------------------------------------------------*
       nor-ric-sel-500.
      *                  *---------------------------------------------*
      *                  * Data di riferimento per l'analisi           *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non esiste una variabile di i.p.c.   *
      *                      * relativa alla data di riferimento per   *
      *                      * l'analisi : no preparazione default     *
      *                      *-----------------------------------------*
           if        w-ipc-dat-rfa-snx    not  = "S"
                     go to nor-ric-sel-600.
      *                      *-----------------------------------------*
      *                      * Se la variabile di i.p.c. ha un valore  *
      *                      * non ammesso : no preparazione default   *
      *                      *-----------------------------------------*
           if        w-ipc-dat-rfa-val    =    zero
                     go to nor-ric-sel-600.
      *                      *-----------------------------------------*
      *                      * Preparazione default                    *
      *                      *-----------------------------------------*
           move      w-ipc-dat-rfa-val    to   rr-dat-rfa             .
       nor-ric-sel-600.
      *                  *---------------------------------------------*
      *                  * Codice cliente ed eventuale dipendenza      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se non esiste una variabile di i.p.c.   *
      *                      * relativa al codice cliente ed eventu-   *
      *                      * ale sua dipendenza : no preparazione    *
      *                      * default                                 *
      *                      *-----------------------------------------*
           if        w-ipc-cod-dbt-snx    not  = "S"
                     go to nor-ric-sel-700.
      *                      *-----------------------------------------*
      *                      * Se la variabile di i.p.c. ha un valore  *
      *                      * non ammesso : no preparazione default   *
      *                      *-----------------------------------------*
           if        w-ipc-cod-dbt-tip    not  = 01
                     go to nor-ric-sel-700.
           if        w-ipc-cod-dbt-cod    =    zero
                     go to nor-ric-sel-700.
      *                      *-----------------------------------------*
      *                      * Preparazione default per codice cliente *
      *                      *-----------------------------------------*
           move      w-ipc-cod-dbt-cod    to   rr-cod-cli             .
      *                      *-----------------------------------------*
      *                      * Preparazione dati associati al codice   *
      *                      * cliente                                 *
      *                      *-----------------------------------------*
           move      rr-cod-cli           to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
           move      w-let-arc-cli-rag    to   rr-cod-cli-rag         .
           move      w-let-arc-cli-via    to   rr-cod-cli-via         .
           move      w-let-arc-cli-loc    to   rr-cod-cli-loc         .
      *                      *-----------------------------------------*
      *                      * Preparazione default per codice dipen-  *
      *                      * denza del cliente                       *
      *                      *-----------------------------------------*
           move      w-ipc-cod-dbt-dpz    to   rr-dpz-cli             .
      *                      *-----------------------------------------*
      *                      * Preparazione dati associati al codice   *
      *                      * dipendenza del cliente                  *
      *                      *-----------------------------------------*
           if        rr-dpz-cli           =    spaces
                     move  "C"            to   w-let-arc-dcc-tle
           else      move  "D"            to   w-let-arc-dcc-tle      .
           move      rr-cod-cli           to   w-let-arc-dcc-cod      .
           move      rr-dpz-cli           to   w-let-arc-dcc-dpz      .
           perform   let-arc-dcc-000      thru let-arc-dcc-999        .
           move      w-let-arc-dcc-rag    to   rr-dpz-cli-rag         .
           move      w-let-arc-dcc-via    to   rr-dpz-cli-via         .
           move      w-let-arc-dcc-loc    to   rr-dpz-cli-loc         .
       nor-ric-sel-700.
      *                  *---------------------------------------------*
      *                  * Tipo evidenziazione                         *
      *                  *---------------------------------------------*
           move      01                   to   rr-tip-evd             .
      *                  *---------------------------------------------*
      *                  * Fine preparazione defaults                  *
      *                  *---------------------------------------------*
           go to     nor-ric-sel-999.
       nor-ric-sel-999.
           exit.

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
           move      12                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
           move      77                   to   v-pto                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Messaggio nel box                               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
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
           move      13                   to   v-lin                  .
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
           move      13                   to   v-lin                  .
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
           move      11                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
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
           move      12                   to   v-lin                  .
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
           move      13                   to   v-lin                  .
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
           move      13                   to   v-lin                  .
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
           move      13                   to   v-lin                  .
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
      *    * Box per messaggio di errore esteso, su tre righe          *
      *    *-----------------------------------------------------------*
       box-msg-e03-000.
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
           move      10                   to   v-lin                  .
           move      04                   to   v-pos                  .
           move      14                   to   v-lto                  .
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
           move      11                   to   v-lin                  .
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
           move      12                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m02    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea 03                                    *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      65                   to   v-car                  .
           move      13                   to   v-lin                  .
           move      06                   to   v-pos                  .
           move      w-err-box-err-m03    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Parentesi quadre di delimitazione               *
      *              *-------------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      03                   to   v-car                  .
           move      13                   to   v-lin                  .
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
           move      13                   to   v-lin                  .
           move      74                   to   v-pos                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Ripristino immagine video                       *
      *              *-------------------------------------------------*
           move      "RS"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       box-msg-e03-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Open files                               *
      *    *-----------------------------------------------------------*
       qry-opn-fls-000.
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
      *              * [oct]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
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
      *              * [zin]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zin                 .
      *              *-------------------------------------------------*
      *              * [zbo]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzbo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbo                 .
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
      *              * [bit]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * [bir]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *              *-------------------------------------------------*
      *              * [zfi]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *              *-------------------------------------------------*
      *              * Open modulo di espansione righe ordine          *
      *              *-------------------------------------------------*
           perform   esp-rig-ocr-opn-000  thru esp-rig-ocr-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di espansione righe ordine spediz.  *
      *              *-------------------------------------------------*
           perform   esp-rig-osr-opn-000  thru esp-rig-osr-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di espansione righe bolla           *
      *              *-------------------------------------------------*
           perform   esp-rig-bir-opn-000  thru esp-rig-bir-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo accettazione coefficiente di cambio *
      *              * valuta                                          *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-opn-000  thru coe-cmb-vlt-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione status riga       *
      *              * ordine cliente                                  *
      *              *-------------------------------------------------*
           perform   det-qev-roc-opn-000  thru det-qev-roc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione status ordine     *
      *              *-------------------------------------------------*
           perform   det-sts-orc-opn-000  thru det-sts-orc-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione status riga       *
      *              * ordine di spedizione cliente                    *
      *              *-------------------------------------------------*
           perform   det-qds-ros-opn-000  thru det-qds-ros-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione status ordine di  *
      *              * spedizione                                      *
      *              *-------------------------------------------------*
           perform   det-sts-ods-opn-000  thru det-sts-ods-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione imposta           *
      *              *-------------------------------------------------*
           perform   det-imp-iva-opn-000  thru det-imp-iva-opn-999    .
      *              *-------------------------------------------------*
      *              * Open modulo di determinazione importo in riga   *
      *              *-------------------------------------------------*
           perform   det-imp-ven-opn-000  thru det-imp-ven-opn-999    .
       qry-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Close files                              *
      *    *-----------------------------------------------------------*
       qry-cls-fls-000.
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
      *              * [oct]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
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
      *              * [zin]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zin                 .
      *              *-------------------------------------------------*
      *              * [zbo]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofzbo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbo                 .
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
      *              * [bit]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *              *-------------------------------------------------*
      *              * [bir]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *              *-------------------------------------------------*
      *              * [zfi]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *              *-------------------------------------------------*
      *              * Close modulo di espansione righe ordine         *
      *              *-------------------------------------------------*
           perform   esp-rig-ocr-cls-000  thru esp-rig-ocr-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo di espansione righe ordine         *
      *              *-------------------------------------------------*
           perform   esp-rig-osr-cls-000  thru esp-rig-osr-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo di espansione righe bolla          *
      *              *-------------------------------------------------*
           perform   esp-rig-bir-cls-000  thru esp-rig-bir-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo accettazione coefficiente di cam-  *
      *              * bio valuta                                      *
      *              *-------------------------------------------------*
           perform   coe-cmb-vlt-cls-000  thru coe-cmb-vlt-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione status riga      *
      *              * ordine cliente                                  *
      *              *-------------------------------------------------*
           perform   det-sts-orc-cls-000  thru det-sts-orc-cls-999    .
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione status ordine    *
      *              * cliente                                         *
      *              *-------------------------------------------------*
           perform   det-sts-orc-cls-000  thru det-sts-orc-cls-999    .
       qry-cls-fls-800.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione status riga      *
      *              * ordine di spedizione cliente                    *
      *              *-------------------------------------------------*
           perform   det-qds-ros-cls-000  thru det-qds-ros-cls-999    .
       qry-cls-fls-820.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione status ordine    *
      *              * di spedizione cliente                           *
      *              *-------------------------------------------------*
           perform   det-sts-ods-cls-000  thru det-sts-ods-cls-999    .
       qry-cls-fls-840.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione imposta          *
      *              *-------------------------------------------------*
           perform   det-imp-iva-cls-000  thru det-imp-iva-cls-999    .
       qry-cls-fls-850.
      *              *-------------------------------------------------*
      *              * Close modulo di determinazione importo in riga  *
      *              *-------------------------------------------------*
           perform   det-imp-ven-cls-000  thru det-imp-ven-cls-999    .
       qry-cls-fls-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     qry-cls-fls-999.
       qry-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Routine di sort preliminare                               *
      *    *-----------------------------------------------------------*
       exe-rou-srt-000.
      *              *-------------------------------------------------*
      *              * Flag di sort eseguito a : No                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-exe-rou-srt      .
       exe-rou-srt-999.
           exit.

      *    *===========================================================*
      *    * Input procedure per sort                                  *
      *    *-----------------------------------------------------------*
       int-srt-inp-000.
       int-srt-inp-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Start iniziale                           *
      *    *-----------------------------------------------------------*
       qry-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-sub      .
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di primo passaggio         *
      *              *-------------------------------------------------*
           move      spaces               to   w-prn-flg-uno          .
      *              *-------------------------------------------------*
      *              * Subroutine di preparazione date                 *
      *              *-------------------------------------------------*
           perform   qry-str-ini-dat-000  thru qry-str-ini-dat-999    .
       qry-str-ini-100.
      *              *-------------------------------------------------*
      *              * Start su archivio [cli]                         *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCLI    "         to   f-key                  .
           move      rr-cod-cli           to   rf-cli-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-qry-flg-sub      .
       qry-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Start iniziale                           *
      *    *                                                           *
      *    * Subroutine per la determinazione delle date in base alla  *
      *    * data di riferimento                                       *
      *    *-----------------------------------------------------------*
       qry-str-ini-dat-000.
      *              *-------------------------------------------------*
      *              * Operazioni preliminari                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione date                        *
      *                  *---------------------------------------------*
           move      rr-dat-rfa           to   w-stp-dat-inp-rif      .
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
           move      zero                 to   w-stp-dat-inp-im8      .
           move      zero                 to   w-stp-dat-inp-fm8      .
           move      zero                 to   w-stp-dat-inp-im9      .
           move      zero                 to   w-stp-dat-inp-fm9      .
           move      zero                 to   w-stp-dat-inp-ims      .
           move      zero                 to   w-stp-dat-inp-fms      .
       qry-str-ini-dat-010.
      *              *-------------------------------------------------*
      *              * Calcolo della data di inizio mese di riferimen- *
      *              * to                                              *
      *              *-------------------------------------------------*
           move      w-stp-dat-inp-rif    to   w-stp-dat-inp-imr      .
           move      w-stp-dat-inp-imr    to   s-dat                  .
           move      01                   to   s-gio                  .
           move      s-dat                to   w-stp-dat-inp-imr      .
       qry-str-ini-dat-020.
      *              *-------------------------------------------------*
      *              * Calcolo della data di fine mese di riferimento  *
      *              *-------------------------------------------------*
           move      w-stp-dat-inp-rif    to   w-stp-dat-inp-fmr      .
      *                  *---------------------------------------------*
      *                  * Fine mese                                   *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-fmr    to   s-dat                  .
           move      31                   to   s-gio                  .
       qry-str-ini-dat-025.
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
                     go to qry-str-ini-dat-030.
           subtract  1                    from s-gio                  .
           go to     qry-str-ini-dat-025.
       qry-str-ini-dat-030.
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
       qry-str-ini-dat-040.
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
       qry-str-ini-dat-045.
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
                     go to qry-str-ini-dat-050.
           subtract  1                    from s-gio                  .
           go to     qry-str-ini-dat-045.
       qry-str-ini-dat-050.
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
       qry-str-ini-dat-055.
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
                     go to qry-str-ini-dat-060.
           subtract  1                    from s-gio                  .
           go to     qry-str-ini-dat-055.
       qry-str-ini-dat-060.
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
       qry-str-ini-dat-065.
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
                     go to qry-str-ini-dat-070.
           subtract  1                    from s-gio                  .
           go to     qry-str-ini-dat-065.
       qry-str-ini-dat-070.
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
       qry-str-ini-dat-075.
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
                     go to qry-str-ini-dat-080.
           subtract  1                    from s-gio                  .
           go to     qry-str-ini-dat-075.
       qry-str-ini-dat-080.
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
       qry-str-ini-dat-085.
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
                     go to qry-str-ini-dat-090.
           subtract  1                    from s-gio                  .
           go to     qry-str-ini-dat-085.
       qry-str-ini-dat-090.
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
       qry-str-ini-dat-095.
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
                     go to qry-str-ini-dat-100.
           subtract  1                    from s-gio                  .
           go to     qry-str-ini-dat-095.
       qry-str-ini-dat-100.
      *              *-------------------------------------------------*
      *              * Calcolo delle date di inizio e fine mese 7      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im6    to   w-stp-dat-inp-im7      .
           move      w-stp-dat-inp-im7    to   s-dat                  .
           add       1                    to   s-mes                  .
           if        s-mes                >    12
                     add  1               to   s-saa
                     move 1               to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-im7      .
      *                  *---------------------------------------------*
      *                  * Fine mese                                   *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im7    to   w-stp-dat-inp-fm7      .
           move      w-stp-dat-inp-fm7    to   s-dat                  .
           move      31                   to   s-gio                  .
       qry-str-ini-dat-105.
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
                     go to qry-str-ini-dat-110.
           subtract  1                    from s-gio                  .
           go to     qry-str-ini-dat-105.
       qry-str-ini-dat-110.
      *              *-------------------------------------------------*
      *              * Calcolo delle date di inizio e fine mese 8      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im7    to   w-stp-dat-inp-im8      .
           move      w-stp-dat-inp-im8    to   s-dat                  .
           add       1                    to   s-mes                  .
           if        s-mes                >    12
                     add  1               to   s-saa
                     move 1               to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-im8      .
      *                  *---------------------------------------------*
      *                  * Fine mese                                   *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im8    to   w-stp-dat-inp-fm8      .
           move      w-stp-dat-inp-fm8    to   s-dat                  .
           move      31                   to   s-gio                  .
       qry-str-ini-dat-115.
           move      s-dat                to   w-stp-dat-inp-fm8      .
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
                     go to qry-str-ini-dat-120.
           subtract  1                    from s-gio                  .
           go to     qry-str-ini-dat-115.
       qry-str-ini-dat-120.
      *              *-------------------------------------------------*
      *              * Calcolo delle date di inizio e fine mese 9      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Inizio mese                                 *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im8    to   w-stp-dat-inp-im9      .
           move      w-stp-dat-inp-im9    to   s-dat                  .
           add       1                    to   s-mes                  .
           if        s-mes                >    12
                     add  1               to   s-saa
                     move 1               to   s-mes                  .
           move      s-dat                to   w-stp-dat-inp-im9      .
      *                  *---------------------------------------------*
      *                  * Fine mese                                   *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-im9    to   w-stp-dat-inp-fm9      .
           move      w-stp-dat-inp-fm9    to   s-dat                  .
           move      31                   to   s-gio                  .
       qry-str-ini-dat-125.
           move      s-dat                to   w-stp-dat-inp-fm9      .
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
                     go to qry-str-ini-dat-130.
           subtract  1                    from s-gio                  .
           go to     qry-str-ini-dat-125.
       qry-str-ini-dat-130.
      *              *-------------------------------------------------*
      *              * Calcolo delle date di inizio e fine mesi suc-   *
      *              * cessivi                                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione del primo giorno successivo  *
      *                  * alla data di fine mese 10                   *
      *                  *---------------------------------------------*
           move      w-stp-dat-inp-fm9    to   w-det-dat-nrg-dtb      .
           move      1                    to   w-det-dat-nrg-ngi      .
           perform   det-dat-nrg-000      thru det-dat-nrg-999        .
           move      w-det-dat-nrg-dti    to   w-stp-dat-inp-ims      .
      *                  *---------------------------------------------*
      *                  * Fine mesi successivi                        *
      *                  *---------------------------------------------*
           move      9999999              to   w-stp-dat-inp-fms      .
       qry-str-ini-dat-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     qry-str-ini-dat-999.
       qry-str-ini-dat-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Messaggio per nessuna registrazione      *
      *    *-----------------------------------------------------------*
       qry-nes-ela-000.
           move      "ME"                 to   v-ope                  .
           move      "Nessuna registrazione entro i limiti assegnati !"
                                          to   v-not                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-nes-ela-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Lettura sequenziale                      *
      *    *-----------------------------------------------------------*
       qry-let-seq-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-sub      .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda del tipo evidenziazione    *
      *              *-------------------------------------------------*
           if        rr-tip-evd           =    03
                     go to qry-let-seq-100
           else      go to qry-let-seq-200.
       qry-let-seq-100.
      *              *-------------------------------------------------*
      *              * Tipo evidenziazione : solo riepilogo            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trattamento flag di primo passaggio         *
      *                  *---------------------------------------------*
           if        w-prn-flg-uno        =    spaces
                     move  "#"            to   w-prn-flg-uno
           else      move  "#"            to   w-cnt-qry-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-let-seq-999.
       qry-let-seq-200.
      *              *-------------------------------------------------*
      *              * Tipo evidenziazione : anche la lista con o sen- *
      *              * za riepilogo                                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale archivio [cli]          *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/iofcli"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-cli                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     move  "#"            to   w-cnt-qry-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-let-seq-999.
       qry-let-seq-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Test se superamento limiti massimi       *
      *    *-----------------------------------------------------------*
       qry-tst-max-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-sub      .
      *              *-------------------------------------------------*
      *              * Test su Codice                                  *
      *              *-------------------------------------------------*
           if        rf-cli-cod-cli       >    rr-cod-cli
                     move  "#"            to   w-cnt-qry-flg-sub      .
       qry-tst-max-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Selezione su record letto                *
      *    *-----------------------------------------------------------*
       qry-sel-rec-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-sub      .
       qry-sel-rec-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Composizione area per tests di rottura   *
      *    *-----------------------------------------------------------*
       qry-cmp-rot-000.
      *              *-------------------------------------------------*
      *              * Rottura su codice cliente                       *
      *              *-------------------------------------------------*
           move      rf-cli-cod-cli       to   w-rot-l01-cod-cli      .
       qry-cmp-rot-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Esecuzione per inizio ciclo              *
      *    *-----------------------------------------------------------*
       qry-ini-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
      *              *-------------------------------------------------*
      *              * Inizializzazione totali per documento           *
      *              *-------------------------------------------------*
           perform   tot-doc-ini-000      thru tot-doc-ini-999        .
       qry-ini-cic-300.
      *              *-------------------------------------------------*
      *              * Intestazione pagina                             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da effettuare                       *
      *                  *---------------------------------------------*
           if        rr-tip-evd           not  = 01 and
                     rr-tip-evd           not  = 02
                     go to qry-ini-cic-999.
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                  *---------------------------------------------*
      *                  * Test se interruzione forzata                *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-ini-cic-999.
       qry-ini-cic-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Esecuzione per fine ciclo                *
      *    *-----------------------------------------------------------*
       qry-fin-cic-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
      *              *-------------------------------------------------*
      *              * Visualizzazione totali per scadenza             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da effettuare                       *
      *                  *---------------------------------------------*
           if        rr-tip-evd           not  = 01 and
                     rr-tip-evd           not  = 03
                     go to qry-fin-cic-999.
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           perform   vis-tot-doc-000      thru vis-tot-doc-999        .
       qry-fin-cic-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Inizio 5. livello di rottura             *
      *    *-----------------------------------------------------------*
       qry-ini-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-lr5-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Fine 5. livello di rottura               *
      *    *-----------------------------------------------------------*
       qry-fin-lr5-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-lr5-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Inizio 4. livello di rottura             *
      *    *-----------------------------------------------------------*
       qry-ini-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-lr4-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Fine 4. livello di rottura               *
      *    *-----------------------------------------------------------*
       qry-fin-lr4-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-lr4-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Inizio 3. livello di rottura             *
      *    *-----------------------------------------------------------*
       qry-ini-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-lr3-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Fine 3. livello di rottura               *
      *    *-----------------------------------------------------------*
       qry-fin-lr3-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-lr3-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Inizio 2. livello di rottura             *
      *    *-----------------------------------------------------------*
       qry-ini-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-lr2-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Fine 2. livello di rottura               *
      *    *-----------------------------------------------------------*
       qry-fin-lr2-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-lr2-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Inizio 1. livello di rottura             *
      *    *-----------------------------------------------------------*
       qry-ini-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-ini-lr1-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     qry-ini-lr1-999.
       qry-ini-lr1-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Fine 1. livello di rottura               *
      *    *-----------------------------------------------------------*
       qry-fin-lr1-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-fin-lr1-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Livello di dettaglio                     *
      *    *-----------------------------------------------------------*
       qry-liv-det-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-orc-wrk-ele      .
           move      spaces               to   w-stp-orc-wrk-pmt      .
      *
           move      zero                 to   w-stp-ods-wrk-ele      .
           move      spaces               to   w-stp-ods-wrk-pmt      .
      *
           move      zero                 to   w-stp-bol-wrk-ele      .
           move      spaces               to   w-stp-bol-wrk-pmt      .
      *
           move      zero                 to   w-stp-gep-wrk-ele      .
           move      spaces               to   w-stp-gep-wrk-pmt      .
       qry-liv-det-200.
      *              *-------------------------------------------------*
      *              * Lettura eventuale lettera d'intenti             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [lic]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/cge/fls/ioc/obj/ioflic"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [lic]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI"             to   f-key                  .
           move      rf-cli-cod-cli       to   rf-lic-cod-cli         .
           move      "pgm/cge/fls/ioc/obj/ioflic"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-lic                 .
       qry-liv-det-500.
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione castelletto ordini ineva- *
      *              * si                                              *
      *              *-------------------------------------------------*
           perform   qry-liv-det-orc-000  thru qry-liv-det-orc-999    .
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione castelletto ordini di     *
      *              * spedizione inevasi                              *
      *              *-------------------------------------------------*
           perform   qry-liv-det-ods-000  thru qry-liv-det-ods-999    .
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione castelletto bolle da fat- *
      *              * turare                                          *
      *              *-------------------------------------------------*
           perform   qry-liv-det-bol-000  thru qry-liv-det-bol-999    .
      *              *-------------------------------------------------*
      *              * Ciclo di preparazione castelletto scadenze in   *
      *              * portafoglio                                     *
      *              *-------------------------------------------------*
           perform   qry-liv-det-gep-000  thru qry-liv-det-gep-999    .
       qry-liv-det-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     qry-liv-det-999.
       qry-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Livello di dettaglio                     *
      *    *                                                           *
      *    * Ciclo di preparazione castelletto ordini inevasi          *
      *    *-----------------------------------------------------------*
       qry-liv-det-orc-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
       qry-liv-det-orc-010.
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
                     go to qry-liv-det-orc-900.
       qry-liv-det-orc-020.
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
                     go to qry-liv-det-orc-900.
       qry-liv-det-orc-100.
      *              *-------------------------------------------------*
      *              * Start su file [oct]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DPZARCDAT "         to   f-key                  .
           move      rf-ada-cod-dpz       to   rf-oct-cod-dpz         .
           move      "C"                  to   rf-oct-tip-arc         .
           move      rf-cli-cod-cli       to   rf-oct-cod-arc         .
           move      zero                 to   rf-oct-dat-doc         .
           move      zero                 to   rf-oct-num-doc         .
           move      zero                 to   rf-oct-num-prt         .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-liv-det-orc-020.
       qry-liv-det-orc-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale record [oct]                *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofoct"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-oct                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-liv-det-orc-020.
       qry-liv-det-orc-300.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rf-oct-cod-dpz       not  = rf-ada-cod-dpz
                     go to qry-liv-det-orc-020.
      *                  *---------------------------------------------*
      *                  * Test su tipo archivio                       *
      *                  *---------------------------------------------*
           if        rf-oct-tip-arc       not  = "C"
                     go to qry-liv-det-orc-020.
      *                  *---------------------------------------------*
      *                  * Test su codice cliente                      *
      *                  *---------------------------------------------*
           if        rf-oct-cod-arc       not  = rf-cli-cod-cli
                     go to qry-liv-det-orc-020.
      *                  *---------------------------------------------*
      *                  * Test su data documento                      *
      *                  *---------------------------------------------*
           if        rf-oct-dat-doc       >    w-stp-dat-inp-rif
                     go to qry-liv-det-orc-020.
       qry-liv-det-orc-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di ordine chiuso               *
      *                  *---------------------------------------------*
           if        rf-oct-flg-och       not  = spaces
                     go to qry-liv-det-orc-200.
      *                  *---------------------------------------------*
      *                  * Test su tipo di ordine                      *
      *                  *---------------------------------------------*
           if        rf-oct-tip-ord       not  = spaces
                     go to qry-liv-det-orc-200.
      *                  *---------------------------------------------*
      *                  * Test su tipo di archivio                    *
      *                  *---------------------------------------------*
           if        rf-oct-tip-arc       not  = "C"
                     go to qry-liv-det-orc-200.
      *                  *---------------------------------------------*
      *                  * Determinazione status dell'ordine           *
      *                  *---------------------------------------------*
           move      "DT"                 to   d-sts-orc-tip-ope      .
           perform   det-sts-orc-cll-000  thru det-sts-orc-cll-999    .
      *                  *---------------------------------------------*
      *                  * Test su status dell'ordine                  *
      *                  *---------------------------------------------*
           if        d-sts-orc-sts-ord    =    02  or
                     d-sts-orc-sts-ord    =    04
                     go to qry-liv-det-orc-500
           else      go to qry-liv-det-orc-200.
       qry-liv-det-orc-500.
      *              *-------------------------------------------------*
      *              * Pre-scansione righe ordine per determinare in   *
      *              * quante tranches deve essere diviso l'ordine     *
      *              * rispetto alle date di consegna prevista in riga *
      *              *-------------------------------------------------*
           move      rf-oct-num-prt       to   w-roc-num-prt          .
           perform   pre-scn-rig-orc-000  thru pre-scn-rig-orc-999    .
      *              *-------------------------------------------------*
      *              * Ordinamento finale delle righe bufferizzate     *
      *              *-------------------------------------------------*
           perform   ord-fin-rig-orc-000  thru ord-fin-rig-orc-999    .
       qry-liv-det-orc-520.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione delle righe bufferizzate ed  *
      *              * ordinate, con rottura su data consegna prevista *
      *              * per ottenere le tranches dell'ordine            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione comodo per rottura su data  *
      *                  * consegna prevista                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-roc-rot-prv          .
      *                  *---------------------------------------------*
      *                  * Normalizzazione contatore righe             *
      *                  *---------------------------------------------*
           move      zero                 to   w-roc-ctr-ele          .
       qry-liv-det-orc-540.
           add       1                    to   w-roc-ctr-ele          .
      *                  *---------------------------------------------*
      *                  * Test di superamento buffer                  *
      *                  *---------------------------------------------*
           if        w-roc-ctr-ele        >    w-roc-num-ele
                     go to qry-liv-det-orc-880.
           if        w-roc-ctr-ele        >    w-roc-max-ele
                     go to qry-liv-det-orc-880.
      *                  *---------------------------------------------*
      *                  * Test di rottura su data consegna prevista   *
      *                  *---------------------------------------------*
           if        w-roc-rot-prv        =    zero
                     move  w-roc-key-prv
                          (w-roc-ctr-ele) to   w-roc-rot-prv
                     go to qry-liv-det-orc-560.
           if        w-roc-key-prv 
                    (w-roc-ctr-ele)       =    w-roc-rot-prv
                     go to qry-liv-det-orc-580
           else      go to qry-liv-det-orc-600.
       qry-liv-det-orc-560.
      *                  *---------------------------------------------*
      *                  * Rottura - inizio livello                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Incremento numero elementi in tabella   *
      *                      * ordini inevasi                          *
      *                      *-----------------------------------------*
           add       1                    to   w-stp-orc-wrk-ele      .
           if        w-stp-orc-wrk-ele    >    w-stp-orc-wrk-max
                     go to qry-liv-det-orc-900.
      *                      *-----------------------------------------*
      *                      * Normalizzazione tabella elementi        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Estremi ordine                      *
      *                          *-------------------------------------*
           move      rf-oct-tmo-orc       to   w-stp-orc-tip-doc
                                              (w-stp-orc-wrk-ele)     .
           move      rf-oct-num-doc       to   w-stp-orc-num-doc
                                              (w-stp-orc-wrk-ele)     .
           move      rf-oct-dat-doc       to   w-stp-orc-dat-doc
                                              (w-stp-orc-wrk-ele)     .
      *                          *-------------------------------------*
      *                          * Valori per cumulo                   *
      *                          *-------------------------------------*
           move      zero                 to   w-stp-orc-cum-pre
                                              (w-stp-orc-wrk-ele)     .
           move      zero                 to   w-stp-orc-cum-mic
                                              (w-stp-orc-wrk-ele)     .
           move      zero                 to   w-stp-orc-cum-ms1
                                              (w-stp-orc-wrk-ele)     .
           move      zero                 to   w-stp-orc-cum-ms2
                                              (w-stp-orc-wrk-ele)     .
           move      zero                 to   w-stp-orc-cum-ms3
                                              (w-stp-orc-wrk-ele)     .
           move      zero                 to   w-stp-orc-cum-ms4
                                              (w-stp-orc-wrk-ele)     .
           move      zero                 to   w-stp-orc-cum-ms5
                                              (w-stp-orc-wrk-ele)     .
           move      zero                 to   w-stp-orc-cum-ms6
                                              (w-stp-orc-wrk-ele)     .
           move      zero                 to   w-stp-orc-cum-ms7
                                              (w-stp-orc-wrk-ele)     .
           move      zero                 to   w-stp-orc-cum-ms8
                                              (w-stp-orc-wrk-ele)     .
           move      zero                 to   w-stp-orc-cum-ms9
                                              (w-stp-orc-wrk-ele)     .
           move      zero                 to   w-stp-orc-cum-suc
                                              (w-stp-orc-wrk-ele)     .
      *                          *-------------------------------------*
      *                          * Inizializzazione area totali ordine *
      *                          *-------------------------------------*
           perform   ini-tot-orc-000      thru ini-tot-orc-999        .
       qry-liv-det-orc-580.
      *                  *---------------------------------------------*
      *                  * Rottura - dettaglio                         *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Cumulo totali dalle righe               *
      *                      *-----------------------------------------*
           perform   det-tri-orc-000      thru det-tri-orc-999        .
      *                      *-----------------------------------------*
      *                      * Riciclo a riga successiva               *
      *                      *-----------------------------------------*
           go to     qry-liv-det-orc-540.
       qry-liv-det-orc-600.
      *                  *---------------------------------------------*
      *                  * Rottura - fine livello                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Trattamento fine tranche                *
      *                      *-----------------------------------------*
           perform   trt-fin-trc-orc-000  thru trt-fin-trc-orc-999    .
      *                      *-----------------------------------------*
      *                      * Aggiornamento comodo di rottura         *
      *                      *-----------------------------------------*
           move      w-roc-key-prv 
                    (w-roc-ctr-ele)       to   w-roc-rot-prv          .
       qry-liv-det-orc-860.
      *                      *-----------------------------------------*
      *                      * A inizio livello                        *
      *                      *-----------------------------------------*
           go to     qry-liv-det-orc-560.
       qry-liv-det-orc-880.
      *              *-------------------------------------------------*
      *              * Trattamento fine tranche                        *
      *              *-------------------------------------------------*
           perform   trt-fin-trc-orc-000  thru trt-fin-trc-orc-999    .
      *              *-------------------------------------------------*
      *              * Riciclo a testata ordine successiva             *
      *              *-------------------------------------------------*
           go to     qry-liv-det-orc-200.
       qry-liv-det-orc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     qry-liv-det-orc-999.
       qry-liv-det-orc-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Livello di dettaglio                     *
      *    *                                                           *
      *    * Ciclo di preparazione castelletto ordini di spedizione    *
      *    * inevasi                                                   *
      *    *-----------------------------------------------------------*
       qry-liv-det-ods-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
       qry-liv-det-ods-010.
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
                     go to qry-liv-det-ods-900.
       qry-liv-det-ods-020.
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
                     go to qry-liv-det-ods-900.
       qry-liv-det-ods-100.
      *              *-------------------------------------------------*
      *              * Start su file [ost]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DPZARCDAT "         to   f-key                  .
           move      rf-ada-cod-dpz       to   rf-ost-cod-dpz         .
           move      "C"                  to   rf-ost-tip-arc         .
           move      rf-cli-cod-cli       to   rf-ost-cod-arc         .
           move      zero                 to   rf-ost-dat-doc         .
           move      zero                 to   rf-ost-num-prt         .
           move      "pgm/ods/fls/ioc/obj/iofost"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ost                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-liv-det-ods-020.
       qry-liv-det-ods-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale record [ost]                *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/ods/fls/ioc/obj/iofost"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ost                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-liv-det-ods-020.
       qry-liv-det-ods-300.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rf-ost-cod-dpz       not  = rf-ada-cod-dpz
                     go to qry-liv-det-ods-020.
      *                  *---------------------------------------------*
      *                  * Test su tipo archivio                       *
      *                  *---------------------------------------------*
           if        rf-ost-tip-arc       not  = "C"
                     go to qry-liv-det-ods-020.
      *                  *---------------------------------------------*
      *                  * Test su codice cliente                      *
      *                  *---------------------------------------------*
           if        rf-ost-cod-arc       not  = rf-cli-cod-cli
                     go to qry-liv-det-ods-020.
      *                  *---------------------------------------------*
      *                  * Test su data documento                      *
      *                  *---------------------------------------------*
           if        rf-ost-dat-doc       >    w-stp-dat-inp-rif
                     go to qry-liv-det-ods-020.
       qry-liv-det-ods-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di ordine chiuso               *
      *                  *---------------------------------------------*
           if        rf-ost-flg-sch       not  = spaces
                     go to qry-liv-det-ods-200.
      *                  *---------------------------------------------*
      *                  * Determinazione status ordine di spedizione  *
      *                  *---------------------------------------------*
           move      "DT"                 to   d-sts-ods-tip-ope      .
           perform   det-sts-ods-cll-000  thru det-sts-ods-cll-999    .
      *                  *---------------------------------------------*
      *                  * Test su status dell'ordine                  *
      *                  *---------------------------------------------*
           if        d-sts-ods-sts-ord    =    02  or
                     d-sts-ods-sts-ord    =    04
                     go to qry-liv-det-ods-500
           else      go to qry-liv-det-ods-200.
       qry-liv-det-ods-500.
      *              *-------------------------------------------------*
      *              * Incremento numero elementi in tabella ordini di *
      *              * spedizione inevasi                              *
      *              *-------------------------------------------------*
           add       1                    to   w-stp-ods-wrk-ele      .
           if        w-stp-ods-wrk-ele    >    w-stp-ods-wrk-max
                     go to qry-liv-det-ods-900.
      *              *-------------------------------------------------*
      *              * Normalizzazione tabella elementi                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estremi ordine                              *
      *                  *---------------------------------------------*
           move      rf-ost-cod-tms       to   w-stp-ods-tip-doc
                                              (w-stp-ods-wrk-ele)     .
           move      rf-ost-num-prt       to   w-stp-ods-num-doc
                                              (w-stp-ods-wrk-ele)     .
           move      rf-ost-dat-doc       to   w-stp-ods-dat-doc
                                              (w-stp-ods-wrk-ele)     .
      *                  *---------------------------------------------*
      *                  * Valori per cumulo                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-ods-cum-pre
                                              (w-stp-ods-wrk-ele)     .
           move      zero                 to   w-stp-ods-cum-mic
                                              (w-stp-ods-wrk-ele)     .
           move      zero                 to   w-stp-ods-cum-ms1
                                              (w-stp-ods-wrk-ele)     .
           move      zero                 to   w-stp-ods-cum-ms2
                                              (w-stp-ods-wrk-ele)     .
           move      zero                 to   w-stp-ods-cum-ms3
                                              (w-stp-ods-wrk-ele)     .
           move      zero                 to   w-stp-ods-cum-ms4
                                              (w-stp-ods-wrk-ele)     .
           move      zero                 to   w-stp-ods-cum-ms5
                                              (w-stp-ods-wrk-ele)     .
           move      zero                 to   w-stp-ods-cum-ms6
                                              (w-stp-ods-wrk-ele)     .
           move      zero                 to   w-stp-ods-cum-ms7
                                              (w-stp-ods-wrk-ele)     .
           move      zero                 to   w-stp-ods-cum-ms8
                                              (w-stp-ods-wrk-ele)     .
           move      zero                 to   w-stp-ods-cum-ms9
                                              (w-stp-ods-wrk-ele)     .
           move      zero                 to   w-stp-ods-cum-suc
                                              (w-stp-ods-wrk-ele)     .
      *              *-------------------------------------------------*
      *              * Inizializzazione area totali ordine             *
      *              *-------------------------------------------------*
           perform   ini-tot-ods-000      thru ini-tot-ods-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totali dalle righe               *
      *              *-------------------------------------------------*
           perform   det-tri-ods-000      thru det-tri-ods-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totali ordine                    *
      *              *-------------------------------------------------*
           perform   det-tot-ods-000      thru det-tot-ods-999        .
       qry-liv-det-ods-600.
      *              *-------------------------------------------------*
      *              * Sventagliamento delle scadenze nei mesi di      *
      *              * competenza                                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-ods-wrk-ctr      .
       qry-liv-det-ods-620.
           add       1                    to   w-stp-ods-wrk-ctr      .
      *                  *---------------------------------------------*
      *                  * Test preliminari                            *
      *                  *---------------------------------------------*
           if        w-tot-scd-imp
                    (w-stp-ods-wrk-ctr)   =    zero
                     go to qry-liv-det-ods-800.
           if        w-stp-ods-wrk-ctr    >    96
                     go to qry-liv-det-ods-800.
      *                  *---------------------------------------------*
      *                  * Confronto fra date                          *
      *                  *---------------------------------------------*
           if        w-tot-scd-dat
                    (w-stp-ods-wrk-ctr)   >    w-stp-dat-inp-fm9
                     add  w-tot-scd-imp
                         (w-stp-ods-wrk-ctr)
                                          to   w-stp-ods-cum-suc
                                              (w-stp-ods-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-ods-wrk-ctr)   >    w-stp-dat-inp-fm8
                     add  w-tot-scd-imp
                         (w-stp-ods-wrk-ctr)
                                          to   w-stp-ods-cum-ms9
                                              (w-stp-ods-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-ods-wrk-ctr)   >    w-stp-dat-inp-fm7
                     add  w-tot-scd-imp
                         (w-stp-ods-wrk-ctr)
                                          to   w-stp-ods-cum-ms8
                                              (w-stp-ods-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-ods-wrk-ctr)   >    w-stp-dat-inp-fm6
                     add  w-tot-scd-imp
                         (w-stp-ods-wrk-ctr)
                                          to   w-stp-ods-cum-ms7
                                              (w-stp-ods-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-ods-wrk-ctr)   >    w-stp-dat-inp-fm5
                     add  w-tot-scd-imp
                         (w-stp-ods-wrk-ctr)
                                          to   w-stp-ods-cum-ms6
                                              (w-stp-ods-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-ods-wrk-ctr)   >    w-stp-dat-inp-fm4
                     add  w-tot-scd-imp
                         (w-stp-ods-wrk-ctr)
                                          to   w-stp-ods-cum-ms5
                                              (w-stp-ods-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-ods-wrk-ctr)   >    w-stp-dat-inp-fm3
                     add  w-tot-scd-imp
                         (w-stp-ods-wrk-ctr)
                                          to   w-stp-ods-cum-ms4
                                              (w-stp-ods-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-ods-wrk-ctr)   >    w-stp-dat-inp-fm2
                     add  w-tot-scd-imp
                         (w-stp-ods-wrk-ctr)
                                          to   w-stp-ods-cum-ms3
                                              (w-stp-ods-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-ods-wrk-ctr)   >    w-stp-dat-inp-fm1
                     add  w-tot-scd-imp
                         (w-stp-ods-wrk-ctr)
                                          to   w-stp-ods-cum-ms2
                                              (w-stp-ods-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-ods-wrk-ctr)   >    w-stp-dat-inp-fmp
                     add  w-tot-scd-imp
                         (w-stp-ods-wrk-ctr)
                                          to   w-stp-ods-cum-mic
                                              (w-stp-ods-wrk-ele)

           else      add  w-tot-scd-imp
                         (w-stp-ods-wrk-ctr)
                                          to   w-stp-ods-cum-pre
                                              (w-stp-ods-wrk-ele)     .
       qry-liv-det-ods-700.
      *                  *---------------------------------------------*
      *                  * Stampa riga dettaglio                       *
      *                  *---------------------------------------------*
           if        rr-tip-evd           not  = 01 and
                     rr-tip-evd           not  = 02
                     go to qry-liv-det-ods-800.
      *                  *---------------------------------------------*
      *                  * Test se righe residue sufficienti           *
      *                  *---------------------------------------------*
           if        v-res                >    1
                     go to qry-liv-det-ods-720.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                      *-----------------------------------------*
      *                      * Test se interruzione forzata            *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-ods-800.
       qry-liv-det-ods-720.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "B"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      rf-ost-num-prt       to   w-wrk-num-prt          .
           move      w-wrk-npt-saa        to   w-wrk-nps-saa          .
           move      w-wrk-npt-prg        to   w-wrk-nps-prg          .
           move      w-wrk-prt-stp        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Tipo documento                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      14                   to   v-pos                  .
           move      rf-ost-cod-tms       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
           move      rf-ost-dat-doc       to   v-dat                  .
      *
           move      "+"                  to   v-edm                  .
           move      002                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      rf-ost-dat-doc       to   w-mpn-dat-ods          .
           move      rf-ost-cod-tms       to   w-mpn-tip-ods          .
           move      rf-ost-num-prt       to   w-mpn-prt-ods          .
           move      w-mpn                to   v-cnt                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Importo documento                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           if        w-tot-scd-imp
                    (w-stp-ods-wrk-ctr)   >    999999999 or
                     w-tot-scd-imp
                    (w-stp-ods-wrk-ctr)   <   -999999999
                     move  11             to   v-car
           else      move  09             to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           if        w-tot-scd-imp
                    (w-stp-ods-wrk-ctr)   >    999999999 or
                     w-tot-scd-imp
                    (w-stp-ods-wrk-ctr)   <   -999999999
                     move  spaces         to   v-edm
           else      move  "G"            to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      29                   to   v-pos                  .
      *
           if        c-dec                <    1
                     add  1               to   v-pos                  .
      *
           move      w-tot-scd-imp
                    (w-stp-ods-wrk-ctr)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data competenza                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      45                   to   v-pos                  .
           move      w-tot-scd-dat
                    (w-stp-ods-wrk-ctr)   to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Eventuale prompt                            *
      *                  *---------------------------------------------*
           if        w-stp-ods-wrk-pmt    not  = spaces
                     go to qry-liv-det-ods-780.
      *
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      57                   to   v-pos                  .
           move      "Ordini di spedizione    "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "#"                  to   w-stp-ods-wrk-pmt      .
       qry-liv-det-ods-780.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     qry-liv-det-ods-620.
       qry-liv-det-ods-800.
      *              *-------------------------------------------------*
      *              * Cumulo totali generali                          *
      *              *-------------------------------------------------*
           add       w-stp-ods-cum-pre
                    (w-stp-ods-wrk-ele)   to   w-tot-tot-gen-pre (2)  .
           add       w-stp-ods-cum-mic
                    (w-stp-ods-wrk-ele)   to   w-tot-tot-gen-mic (2)  .
           add       w-stp-ods-cum-ms1
                    (w-stp-ods-wrk-ele)   to   w-tot-tot-gen-ms1 (2)  .
           add       w-stp-ods-cum-ms2
                    (w-stp-ods-wrk-ele)   to   w-tot-tot-gen-ms2 (2)  .
           add       w-stp-ods-cum-ms3
                    (w-stp-ods-wrk-ele)   to   w-tot-tot-gen-ms3 (2)  .
           add       w-stp-ods-cum-ms4
                    (w-stp-ods-wrk-ele)   to   w-tot-tot-gen-ms4 (2)  .
           add       w-stp-ods-cum-ms5
                    (w-stp-ods-wrk-ele)   to   w-tot-tot-gen-ms5 (2)  .
           add       w-stp-ods-cum-ms6
                    (w-stp-ods-wrk-ele)   to   w-tot-tot-gen-ms6 (2)  .
           add       w-stp-ods-cum-ms7
                    (w-stp-ods-wrk-ele)   to   w-tot-tot-gen-ms7 (2)  .
           add       w-stp-ods-cum-ms8
                    (w-stp-ods-wrk-ele)   to   w-tot-tot-gen-ms8 (2)  .
           add       w-stp-ods-cum-ms9
                    (w-stp-ods-wrk-ele)   to   w-tot-tot-gen-ms9 (2)  .
           add       w-stp-ods-cum-suc
                    (w-stp-ods-wrk-ele)   to   w-tot-tot-gen-suc (2)  .
       qry-liv-det-ods-890.
      *              *-------------------------------------------------*
      *              * Riciclo a testata ordine successiva             *
      *              *-------------------------------------------------*
           go to     qry-liv-det-ods-200.
       qry-liv-det-ods-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     qry-liv-det-ods-999.
       qry-liv-det-ods-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Livello di dettaglio                     *
      *    *                                                           *
      *    * Ciclo di preparazione castelletto bolle da fatturare      *
      *    *-----------------------------------------------------------*
       qry-liv-det-bol-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
       qry-liv-det-bol-010.
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
                     go to qry-liv-det-bol-900.
       qry-liv-det-bol-020.
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
                     go to qry-liv-det-bol-900.
       qry-liv-det-bol-100.
      *              *-------------------------------------------------*
      *              * Start su file [bit]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DPZARCDAT "         to   f-key                  .
           move      rf-ada-cod-dpz       to   rf-bit-cod-dpz         .
           move      "C"                  to   rf-bit-tip-arc         .
           move      rf-cli-cod-cli       to   rf-bit-cod-arc         .
           move      zero                 to   rf-bit-dat-doc         .
           move      zero                 to   rf-bit-num-doc         .
           move      zero                 to   rf-bit-num-prt         .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-liv-det-bol-020.
       qry-liv-det-bol-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale record [bit]                *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bit                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-liv-det-bol-020.
       qry-liv-det-bol-300.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su codice dipendenza                   *
      *                  *---------------------------------------------*
           if        rf-bit-cod-dpz       not  = rf-ada-cod-dpz
                     go to qry-liv-det-bol-020.
      *                  *---------------------------------------------*
      *                  * Test su tipo archivio                       *
      *                  *---------------------------------------------*
           if        rf-bit-tip-arc       not  = "C"
                     go to qry-liv-det-bol-020.
      *                  *---------------------------------------------*
      *                  * Test su codice cliente                      *
      *                  *---------------------------------------------*
           if        rf-bit-cod-arc       not  = rf-cli-cod-cli
                     go to qry-liv-det-bol-020.
      *                  *---------------------------------------------*
      *                  * Test su data documento                      *
      *                  *---------------------------------------------*
           if        rf-bit-dat-doc       >    w-stp-dat-inp-rif
                     go to qry-liv-det-bol-020.
       qry-liv-det-bol-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su flag di movimento che interessa la  *
      *                  * fatturazion                                 *
      *                  *---------------------------------------------*
           if        rf-bit-int-ftr       not  = 02
                     go to qry-liv-det-bol-200.
           if        rf-bit-tmo-ftr       =    spaces
                     go to qry-liv-det-bol-200.
      *                  *---------------------------------------------*
      *                  * Determinazione status della bolla           *
      *                  *---------------------------------------------*
           perform   det-sts-bol-000      thru det-sts-bol-999        .
      *                  *---------------------------------------------*
      *                  * Test su status della bolla                  *
      *                  *---------------------------------------------*
           if        w-det-sts-bol-flg    not  = spaces
                     go to qry-liv-det-bol-200.
       qry-liv-det-bol-500.
      *              *-------------------------------------------------*
      *              * Incremento numero elementi in tabella bolle da  *
      *              * fatturare                                       *
      *              *-------------------------------------------------*
           add       1                    to   w-stp-bol-wrk-ele      .
           if        w-stp-bol-wrk-ele    >    w-stp-bol-wrk-max
                     go to qry-liv-det-bol-900.
      *              *-------------------------------------------------*
      *              * Normalizzazione tabella elementi                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estremi bolla                               *
      *                  *---------------------------------------------*
           move      rf-bit-cod-tmb       to   w-stp-bol-tip-doc
                                              (w-stp-bol-wrk-ele)     .
           move      rf-bit-num-doc       to   w-stp-bol-num-doc
                                              (w-stp-bol-wrk-ele)     .
           move      rf-bit-dat-doc       to   w-stp-bol-dat-doc
                                              (w-stp-bol-wrk-ele)     .
      *                  *---------------------------------------------*
      *                  * Valori per cumulo                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-bol-cum-pre
                                              (w-stp-bol-wrk-ele)     .
           move      zero                 to   w-stp-bol-cum-mic
                                              (w-stp-bol-wrk-ele)     .
           move      zero                 to   w-stp-bol-cum-ms1
                                              (w-stp-bol-wrk-ele)     .
           move      zero                 to   w-stp-bol-cum-ms2
                                              (w-stp-bol-wrk-ele)     .
           move      zero                 to   w-stp-bol-cum-ms3
                                              (w-stp-bol-wrk-ele)     .
           move      zero                 to   w-stp-bol-cum-ms4
                                              (w-stp-bol-wrk-ele)     .
           move      zero                 to   w-stp-bol-cum-ms5
                                              (w-stp-bol-wrk-ele)     .
           move      zero                 to   w-stp-bol-cum-ms6
                                              (w-stp-bol-wrk-ele)     .
           move      zero                 to   w-stp-bol-cum-ms7
                                              (w-stp-bol-wrk-ele)     .
           move      zero                 to   w-stp-bol-cum-ms8
                                              (w-stp-bol-wrk-ele)     .
           move      zero                 to   w-stp-bol-cum-ms9
                                              (w-stp-bol-wrk-ele)     .
           move      zero                 to   w-stp-bol-cum-suc
                                              (w-stp-bol-wrk-ele)     .
      *              *-------------------------------------------------*
      *              * Inizializzazione area totali bolla              *
      *              *-------------------------------------------------*
           perform   ini-tot-bol-000      thru ini-tot-bol-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totali dalle righe               *
      *              *-------------------------------------------------*
           perform   det-tri-bol-000      thru det-tri-bol-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totali bolla                     *
      *              *-------------------------------------------------*
           perform   det-tot-bol-000      thru det-tot-bol-999        .
       qry-liv-det-bol-600.
      *              *-------------------------------------------------*
      *              * Sventagliamento delle scadenze nei mesi di      *
      *              * competenza                                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-bol-wrk-ctr      .
       qry-liv-det-bol-620.
           add       1                    to   w-stp-bol-wrk-ctr      .
      *                  *---------------------------------------------*
      *                  * Test preliminari                            *
      *                  *---------------------------------------------*
           if        w-tot-scd-imp
                    (w-stp-bol-wrk-ctr)   =    zero
                     go to qry-liv-det-bol-800.
           if        w-stp-bol-wrk-ctr    >    96
                     go to qry-liv-det-bol-800.
      *                  *---------------------------------------------*
      *                  * Eventuale inversione del segno              *
      *                  *---------------------------------------------*
           if        w-det-sts-bol-idt    =    "N"
                     multiply -1          by   w-tot-scd-imp
                                              (w-stp-bol-wrk-ctr)     .
      *                  *---------------------------------------------*
      *                  * Confronto fra date                          *
      *                  *---------------------------------------------*
           if        w-tot-scd-dat
                    (w-stp-bol-wrk-ctr)   >    w-stp-dat-inp-fm9
                     add  w-tot-scd-imp
                         (w-stp-bol-wrk-ctr)
                                          to   w-stp-bol-cum-suc
                                              (w-stp-bol-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bol-wrk-ctr)   >    w-stp-dat-inp-fm8
                     add  w-tot-scd-imp
                         (w-stp-bol-wrk-ctr)
                                          to   w-stp-bol-cum-ms9
                                              (w-stp-bol-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bol-wrk-ctr)   >    w-stp-dat-inp-fm7
                     add  w-tot-scd-imp
                         (w-stp-bol-wrk-ctr)
                                          to   w-stp-bol-cum-ms8
                                              (w-stp-bol-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bol-wrk-ctr)   >    w-stp-dat-inp-fm6
                     add  w-tot-scd-imp
                         (w-stp-bol-wrk-ctr)
                                          to   w-stp-bol-cum-ms7
                                              (w-stp-bol-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bol-wrk-ctr)   >    w-stp-dat-inp-fm5
                     add  w-tot-scd-imp
                         (w-stp-bol-wrk-ctr)
                                          to   w-stp-bol-cum-ms6
                                              (w-stp-bol-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bol-wrk-ctr)   >    w-stp-dat-inp-fm4
                     add  w-tot-scd-imp
                         (w-stp-bol-wrk-ctr)
                                          to   w-stp-bol-cum-ms5
                                              (w-stp-bol-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bol-wrk-ctr)   >    w-stp-dat-inp-fm3
                     add  w-tot-scd-imp
                         (w-stp-bol-wrk-ctr)
                                          to   w-stp-bol-cum-ms4
                                              (w-stp-bol-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bol-wrk-ctr)   >    w-stp-dat-inp-fm2
                     add  w-tot-scd-imp
                         (w-stp-bol-wrk-ctr)
                                          to   w-stp-bol-cum-ms3
                                              (w-stp-bol-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bol-wrk-ctr)   >    w-stp-dat-inp-fm1
                     add  w-tot-scd-imp
                         (w-stp-bol-wrk-ctr)
                                          to   w-stp-bol-cum-ms2
                                              (w-stp-bol-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-bol-wrk-ctr)   >    w-stp-dat-inp-fmp
                     add  w-tot-scd-imp
                         (w-stp-bol-wrk-ctr)
                                          to   w-stp-bol-cum-mic
                                              (w-stp-bol-wrk-ele)

           else      add  w-tot-scd-imp
                         (w-stp-bol-wrk-ctr)
                                          to   w-stp-bol-cum-pre
                                              (w-stp-bol-wrk-ele)     .
       qry-liv-det-bol-700.
      *                  *---------------------------------------------*
      *                  * Stampa riga dettaglio                       *
      *                  *---------------------------------------------*
           if        rr-tip-evd           not  = 01 and
                     rr-tip-evd           not  = 02
                     go to qry-liv-det-bol-800.
      *                  *---------------------------------------------*
      *                  * Test se righe residue sufficienti           *
      *                  *---------------------------------------------*
           if        v-res                >    1
                     go to qry-liv-det-bol-720.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                      *-----------------------------------------*
      *                      * Test se interruzione forzata            *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-bol-800.
       qry-liv-det-bol-720.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "B"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      rf-bit-num-doc       to   w-wrk-num-prt          .
           move      w-wrk-npt-saa        to   w-wrk-nps-saa          .
           move      w-wrk-npt-prg        to   w-wrk-nps-prg          .
           move      w-wrk-prt-stp        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Tipo documento                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      14                   to   v-pos                  .
           move      rf-bit-cod-tmb       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
           move      rf-bit-dat-doc       to   v-dat                  .
      *
           move      "+"                  to   v-edm                  .
           move      003                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      rf-bit-dat-doc       to   w-mpn-dat-bol          .
           move      rf-bit-num-doc       to   w-mpn-num-bol          .
           move      rf-bit-cod-tmb       to   w-mpn-tip-bol          .
           move      rf-bit-num-prt       to   w-mpn-prt-bol          .
           move      w-mpn                to   v-cnt                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Importo documento                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           if        w-tot-scd-imp
                    (w-stp-bol-wrk-ctr)   >    999999999 or
                     w-tot-scd-imp
                    (w-stp-bol-wrk-ctr)   <   -999999999
                     move  11             to   v-car
           else      move  09             to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           if        w-tot-scd-imp
                    (w-stp-bol-wrk-ctr)   >    999999999 or
                     w-tot-scd-imp
                    (w-stp-bol-wrk-ctr)   <   -999999999
                     move  spaces         to   v-edm
           else      move  "G"            to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      29                   to   v-pos                  .
      *
           if        c-dec                <    1
                     add  1               to   v-pos                  .
      *
           move      w-tot-scd-imp
                    (w-stp-bol-wrk-ctr)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data competenza                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      45                   to   v-pos                  .
           move      w-tot-scd-dat
                    (w-stp-bol-wrk-ctr)   to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Eventuale prompt                            *
      *                  *---------------------------------------------*
           if        w-stp-bol-wrk-pmt    not  = spaces
                     go to qry-liv-det-bol-780.
      *
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      57                   to   v-pos                  .
           move      "Bolle da fatturare      "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "#"                  to   w-stp-bol-wrk-pmt      .
       qry-liv-det-bol-780.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     qry-liv-det-bol-620.
       qry-liv-det-bol-800.
      *              *-------------------------------------------------*
      *              * Cumulo totali generali                          *
      *              *-------------------------------------------------*
           add       w-stp-bol-cum-pre
                    (w-stp-bol-wrk-ele)   to   w-tot-tot-gen-pre (3)  .
           add       w-stp-bol-cum-mic
                    (w-stp-bol-wrk-ele)   to   w-tot-tot-gen-mic (3)  .
           add       w-stp-bol-cum-ms1
                    (w-stp-bol-wrk-ele)   to   w-tot-tot-gen-ms1 (3)  .
           add       w-stp-bol-cum-ms2
                    (w-stp-bol-wrk-ele)   to   w-tot-tot-gen-ms2 (3)  .
           add       w-stp-bol-cum-ms3
                    (w-stp-bol-wrk-ele)   to   w-tot-tot-gen-ms3 (3)  .
           add       w-stp-bol-cum-ms4
                    (w-stp-bol-wrk-ele)   to   w-tot-tot-gen-ms4 (3)  .
           add       w-stp-bol-cum-ms5
                    (w-stp-bol-wrk-ele)   to   w-tot-tot-gen-ms5 (3)  .
           add       w-stp-bol-cum-ms6
                    (w-stp-bol-wrk-ele)   to   w-tot-tot-gen-ms6 (3)  .
           add       w-stp-bol-cum-ms7
                    (w-stp-bol-wrk-ele)   to   w-tot-tot-gen-ms7 (3)  .
           add       w-stp-bol-cum-ms8
                    (w-stp-bol-wrk-ele)   to   w-tot-tot-gen-ms8 (3)  .
           add       w-stp-bol-cum-ms9
                    (w-stp-bol-wrk-ele)   to   w-tot-tot-gen-ms9 (3)  .
           add       w-stp-bol-cum-suc
                    (w-stp-bol-wrk-ele)   to   w-tot-tot-gen-suc (3)  .
       qry-liv-det-bol-890.
      *              *-------------------------------------------------*
      *              * Riciclo a testata ordine successiva             *
      *              *-------------------------------------------------*
           go to     qry-liv-det-bol-200.
       qry-liv-det-bol-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     qry-liv-det-bol-999.
       qry-liv-det-bol-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Livello di dettaglio                     *
      *    *                                                           *
      *    * Ciclo di preparazione castelletto scadenze in portafolgio *
      *    *-----------------------------------------------------------*
       qry-liv-det-gep-000.
      *              *-------------------------------------------------*
      *              * Test preliminari                                *
      *              *-------------------------------------------------*
       qry-liv-det-gep-100.
      *              *-------------------------------------------------*
      *              * Start su file [sdb]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "DBTDTS    "         to   f-key                  .
           move      01                   to   rf-sdb-tip-dbt         .
           move      rf-cli-cod-cli       to   rf-sdb-cod-dbt         .
           move      zero                 to   rf-sdb-dts-sdb         .
           move      zero                 to   rf-sdb-num-sdb         .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-liv-det-gep-900.
       qry-liv-det-gep-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale record [sdb]                *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/gep/fls/ioc/obj/iofsdb"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sdb                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-liv-det-gep-900.
       qry-liv-det-gep-300.
      *              *-------------------------------------------------*
      *              * Test max                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su tipo debitore                       *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-dbt       not  = 01
                     go to qry-liv-det-gep-900.
      *                  *---------------------------------------------*
      *                  * Test su codice cliente                      *
      *                  *---------------------------------------------*
           if        rf-sdb-cod-dbt       not  = rf-cli-cod-cli
                     go to qry-liv-det-gep-900.
       qry-liv-det-gep-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione status scadenza cliente      *
      *                  *---------------------------------------------*
           move      rr-dat-rfa           to   w-det-srd-sdb-drd      .
           perform   det-srd-sdb-000      thru det-srd-sdb-999        .
      *                  *---------------------------------------------*
      *                  * Tests                                       *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Su status scadenza                      *
      *                      *-----------------------------------------*
           if        w-det-srd-sdb-sts    not  = "A"
                     go to qry-liv-det-gep-200.
      *                      *-----------------------------------------*
      *                      * Su sigla ultima operazione eseguita     *
      *                      * sulla scadenza                          *
      *                      *-----------------------------------------*
______*    if        w-det-srd-sdb-suo    not  = "EMI" and
______*              w-det-srd-sdb-suo    not  = "ISP"
______*              go to qry-liv-det-gep-200.
       qry-liv-det-gep-500.
      *              *-------------------------------------------------*
      *              * Incremento numero elementi in tabella           *
      *              *-------------------------------------------------*
           add       1                    to   w-stp-gep-wrk-ele      .
           if        w-stp-gep-wrk-ele    >    w-stp-gep-wrk-max
                     go to qry-liv-det-gep-900.
      *              *-------------------------------------------------*
      *              * Normalizzazione tabella elementi                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Estremi scadenza                            *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       =    01
                     move  "RD  "         to   w-stp-gep-tip-doc
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-tip-sdb       =    02
                     move  "IE  "         to   w-stp-gep-tip-doc
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-tip-sdb       =    03
                     move  "RIBA"         to   w-stp-gep-tip-doc
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-tip-sdb       =    04
                     move  "CDO "         to   w-stp-gep-tip-doc
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-tip-sdb       =    05
                     move  "MAV "         to   w-stp-gep-tip-doc
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-tip-sdb       =    06
                     move  "RID "         to   w-stp-gep-tip-doc
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-tip-sdb       =    07
                     move  "BB  "         to   w-stp-gep-tip-doc
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-tip-sdb       =    08
                     move  "CCP "         to   w-stp-gep-tip-doc
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-tip-sdb       =    09
                     move  "RB  "         to   w-stp-gep-tip-doc
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-tip-sdb       =    10
                     move  "TR  "         to   w-stp-gep-tip-doc
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-tip-sdb       =    11
                     move  "PC  "         to   w-stp-gep-tip-doc
                                              (w-stp-gep-wrk-ele)
           else      move  spaces         to   w-stp-gep-tip-doc
                                              (w-stp-gep-wrk-ele)     .
      *                  *---------------------------------------------*
      *                  * Eventuale dilazione concordata              *
      *                  *---------------------------------------------*
           if        rf-sdb-tip-sdb       =    01  and
                     rf-sdb-snx-dlc       =    "S"
                     move  "DC  "         to   w-stp-gep-tip-doc
                                              (w-stp-gep-wrk-ele)     .
      *                  *---------------------------------------------*
      *                  * Altri dati                                  *
      *                  *---------------------------------------------*
           move      rf-sdb-num-sdb       to   w-stp-gep-num-doc
                                              (w-stp-gep-wrk-ele)     .
           move      rf-sdb-dtr-emi       to   w-stp-gep-dat-doc
                                              (w-stp-gep-wrk-ele)     .
      *                  *---------------------------------------------*
      *                  * Valori per cumulo                           *
      *                  *---------------------------------------------*
           move      zero                 to   w-stp-gep-cum-pre
                                              (w-stp-gep-wrk-ele)     .
           move      zero                 to   w-stp-gep-cum-mic
                                              (w-stp-gep-wrk-ele)     .
           move      zero                 to   w-stp-gep-cum-ms1
                                              (w-stp-gep-wrk-ele)     .
           move      zero                 to   w-stp-gep-cum-ms2
                                              (w-stp-gep-wrk-ele)     .
           move      zero                 to   w-stp-gep-cum-ms3
                                              (w-stp-gep-wrk-ele)     .
           move      zero                 to   w-stp-gep-cum-ms4
                                              (w-stp-gep-wrk-ele)     .
           move      zero                 to   w-stp-gep-cum-ms5
                                              (w-stp-gep-wrk-ele)     .
           move      zero                 to   w-stp-gep-cum-ms6
                                              (w-stp-gep-wrk-ele)     .
           move      zero                 to   w-stp-gep-cum-ms7
                                              (w-stp-gep-wrk-ele)     .
           move      zero                 to   w-stp-gep-cum-ms8
                                              (w-stp-gep-wrk-ele)     .
           move      zero                 to   w-stp-gep-cum-ms9
                                              (w-stp-gep-wrk-ele)     .
           move      zero                 to   w-stp-gep-cum-suc
                                              (w-stp-gep-wrk-ele)     .
       qry-liv-det-gep-600.
      *              *-------------------------------------------------*
      *              * Sventagliamento delle scadenze nei mesi di      *
      *              * competenza                                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Confronto fra date                          *
      *                  *---------------------------------------------*
           if        rf-sdb-dts-sdb       >    w-stp-dat-inp-fm9
                     add  rf-sdb-imp-sdb  to   w-stp-gep-cum-suc
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-dts-sdb       >    w-stp-dat-inp-fm8
                     add  rf-sdb-imp-sdb  to   w-stp-gep-cum-ms9
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-dts-sdb       >    w-stp-dat-inp-fm7
                     add  rf-sdb-imp-sdb  to   w-stp-gep-cum-ms8
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-dts-sdb       >    w-stp-dat-inp-fm6
                     add  rf-sdb-imp-sdb  to   w-stp-gep-cum-ms7
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-dts-sdb       >    w-stp-dat-inp-fm5
                     add  rf-sdb-imp-sdb  to   w-stp-gep-cum-ms6
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-dts-sdb       >    w-stp-dat-inp-fm4
                     add  rf-sdb-imp-sdb  to   w-stp-gep-cum-ms5
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-dts-sdb       >    w-stp-dat-inp-fm3
                     add  rf-sdb-imp-sdb  to   w-stp-gep-cum-ms4
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-dts-sdb       >    w-stp-dat-inp-fm2
                     add  rf-sdb-imp-sdb  to   w-stp-gep-cum-ms3
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-dts-sdb       >    w-stp-dat-inp-fm1
                     add  rf-sdb-imp-sdb  to   w-stp-gep-cum-ms2
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-dts-sdb       >    w-stp-dat-inp-fmr
                     add  rf-sdb-imp-sdb  to   w-stp-gep-cum-ms1
                                              (w-stp-gep-wrk-ele)
           else if   rf-sdb-dts-sdb       >    w-stp-dat-inp-fmp
                     add  rf-sdb-imp-sdb  to   w-stp-gep-cum-mic
                                              (w-stp-gep-wrk-ele)
           else      add  rf-sdb-imp-sdb  to   w-stp-gep-cum-pre
                                              (w-stp-gep-wrk-ele)     .
       qry-liv-det-gep-700.
      *                  *---------------------------------------------*
      *                  * Stampa riga dettaglio                       *
      *                  *---------------------------------------------*
           if        rr-tip-evd           not  = 01 and
                     rr-tip-evd           not  = 02
                     go to qry-liv-det-gep-800.
      *                  *---------------------------------------------*
      *                  * Test se righe residue sufficienti           *
      *                  *---------------------------------------------*
           if        v-res                >    1
                     go to qry-liv-det-gep-720.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                      *-----------------------------------------*
      *                      * Test se interruzione forzata            *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-gep-800.
       qry-liv-det-gep-720.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "B"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      rf-sdb-num-sdb       to   w-wrk-num-prt          .
           move      w-wrk-npt-saa        to   w-wrk-nps-saa          .
           move      w-wrk-npt-prg        to   w-wrk-nps-prg          .
           move      w-wrk-prt-stp        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Tipo documento                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      14                   to   v-pos                  .
      *
           if        rf-sdb-tip-sdb       =    01
                     move  "RD  "         to   v-alf
           else if   rf-sdb-tip-sdb       =    02
                     move  "IE  "         to   v-alf
           else if   rf-sdb-tip-sdb       =    03
                     move  "RIBA"         to   v-alf
           else if   rf-sdb-tip-sdb       =    04
                     move  "CDO "         to   v-alf
           else if   rf-sdb-tip-sdb       =    05
                     move  "MAV "         to   v-alf
           else if   rf-sdb-tip-sdb       =    06
                     move  "RID "         to   v-alf
           else if   rf-sdb-tip-sdb       =    07
                     move  "BB  "         to   v-alf
           else if   rf-sdb-tip-sdb       =    08
                     move  "CCP "         to   v-alf
           else if   rf-sdb-tip-sdb       =    09
                     move  "RB  "         to   v-alf
           else if   rf-sdb-tip-sdb       =    10
                     move  "TR  "         to   v-alf
           else if   rf-sdb-tip-sdb       =    11
                     move  "PC  "         to   v-alf
           else      move  spaces         to   v-alf                  .
      *
           if        rf-sdb-tip-sdb       =    01  and
                     rf-sdb-snx-dlc       =    "S"
                     move  "DC  "         to   v-alf                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
      *
           if        rf-sdb-dts-sdb       =    zero
                     move  rf-sdb-dtr-emi to   v-dat
           else      move  rf-sdb-dts-sdb to   v-dat                  .
      *
           move      "+"                  to   v-edm                  .
           move      004                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      rf-sdb-num-sdb       to   w-mpn-num-gep          .
           move      w-mpn                to   v-cnt                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Importo documento                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           if        rf-sdb-imp-sdb       >    999999999 or
                     rf-sdb-imp-sdb       <   -999999999
                     move  11             to   v-car
           else      move  09             to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           if        rf-sdb-imp-sdb       >    999999999 or
                     rf-sdb-imp-sdb       <   -999999999
                     move  spaces         to   v-edm
           else      move  "G"            to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      29                   to   v-pos                  .
      *
           if        c-dec                <    1
                     add  1               to   v-pos                  .
      *
           move      rf-sdb-imp-sdb       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data competenza                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      45                   to   v-pos                  .
      *
           if        rf-sdb-dts-sdb       =    zero
                     move  rf-sdb-dtr-emi to   v-dat
           else      move  rf-sdb-dts-sdb to   v-dat                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Eventuale prompt                            *
      *                  *---------------------------------------------*
           if        w-stp-gep-wrk-pmt    not  = spaces
                     go to qry-liv-det-gep-780.
      *
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      57                   to   v-pos                  .
           move      "Scadenze in portafoglio "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "#"                  to   w-stp-gep-wrk-pmt      .
       qry-liv-det-gep-780.
       qry-liv-det-gep-800.
      *              *-------------------------------------------------*
      *              * Cumulo totali generali                          *
      *              *-------------------------------------------------*
           add       w-stp-gep-cum-pre
                    (w-stp-gep-wrk-ele)   to   w-tot-tot-gen-pre (4)  .
           add       w-stp-gep-cum-mic
                    (w-stp-gep-wrk-ele)   to   w-tot-tot-gen-mic (4)  .
           add       w-stp-gep-cum-ms1
                    (w-stp-gep-wrk-ele)   to   w-tot-tot-gen-ms1 (4)  .
           add       w-stp-gep-cum-ms2
                    (w-stp-gep-wrk-ele)   to   w-tot-tot-gen-ms2 (4)  .
           add       w-stp-gep-cum-ms3
                    (w-stp-gep-wrk-ele)   to   w-tot-tot-gen-ms3 (4)  .
           add       w-stp-gep-cum-ms4
                    (w-stp-gep-wrk-ele)   to   w-tot-tot-gen-ms4 (4)  .
           add       w-stp-gep-cum-ms5
                    (w-stp-gep-wrk-ele)   to   w-tot-tot-gen-ms5 (4)  .
           add       w-stp-gep-cum-ms6
                    (w-stp-gep-wrk-ele)   to   w-tot-tot-gen-ms6 (4)  .
           add       w-stp-gep-cum-ms7
                    (w-stp-gep-wrk-ele)   to   w-tot-tot-gen-ms7 (4)  .
           add       w-stp-gep-cum-ms8
                    (w-stp-gep-wrk-ele)   to   w-tot-tot-gen-ms8 (4)  .
           add       w-stp-gep-cum-ms9
                    (w-stp-gep-wrk-ele)   to   w-tot-tot-gen-ms9 (4)  .
           add       w-stp-gep-cum-suc
                    (w-stp-gep-wrk-ele)   to   w-tot-tot-gen-suc (4)  .
      *              *-------------------------------------------------*
      *              * Riciclo a scadenza successiva                   *
      *              *-------------------------------------------------*
           go to     qry-liv-det-gep-200.
       qry-liv-det-gep-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     qry-liv-det-gep-999.
       qry-liv-det-gep-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione totali divisi per scadenza                *
      *    *-----------------------------------------------------------*
       vis-tot-doc-000.
      *              *-------------------------------------------------*
      *              * Subroutine di avanzamento pagina                *
      *              *-------------------------------------------------*
           perform   qry-pag-adv-000      thru qry-pag-adv-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to vis-tot-doc-999.
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-cnt-lit-t80        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Titolo                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
           move      i-ide-des            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-cnt-lit-t80        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-doc-100.
      *              *-------------------------------------------------*
      *              * Sub-intestazione specifica                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Fincatura                                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      " Mese scadenza    Ordini inevasi    Spedizioni    
      -              "Bolle inevase      Scadenze   "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Sottolineatura fincatura                    *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "----------------  --------------  --------------  
      -              "--------------  --------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-doc-200.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore 01..12               *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-gen-i01      .
       vis-tot-doc-300.
      *              *-------------------------------------------------*
      *              * Incremento contatore 01..12                     *
      *              *-------------------------------------------------*
           add       1                    to   w-tot-tot-gen-i01      .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a totali                  *
      *              *-------------------------------------------------*
           if        w-tot-tot-gen-i01    >    12
                     go to vis-tot-doc-800.
       vis-tot-doc-400.
      *              *-------------------------------------------------*
      *              * Stampa elemento con indice 'w-tot-tot-gen-i01'  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-doc-500.
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo scadenza    *
      *                  *---------------------------------------------*
           go to     vis-tot-doc-510
                     vis-tot-doc-520
                     vis-tot-doc-530
                     vis-tot-doc-540
                     vis-tot-doc-550
                     vis-tot-doc-560
                     vis-tot-doc-570
                     vis-tot-doc-580
                     vis-tot-doc-590
                     vis-tot-doc-600
                     vis-tot-doc-610
                     vis-tot-doc-620
                     depending            on   w-tot-tot-gen-i01      .
           go to     vis-tot-doc-700.
       vis-tot-doc-510.
      *                  *---------------------------------------------*
      *                  * Indice 01 : Precedenti                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      000                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      01                   to   w-mpn-tsc-000          .
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "+"                  to   v-edm                  .
           move      w-mpn                to   v-cnt                  .
           move      "Precedenti      :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Ordini clienti                          *
      *                      *-----------------------------------------*
           move      18                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-pre (01)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Ordini di spedizione                    *
      *                      *-----------------------------------------*
           move      34                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-pre (02)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Bolle inevase                           *
      *                      *-----------------------------------------*
           move      50                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-pre (03)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scadenze aperte                         *
      *                      *-----------------------------------------*
           move      66                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-pre (04)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-doc-700.
       vis-tot-doc-520.
      *                  *---------------------------------------------*
      *                  * Indice 02 : mese in corso                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      000                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      02                   to   w-mpn-tsc-000          .
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fmr    to   w-stp-lit-pmt-dat      .
           move      v-lnr                to   w-stp-lit-pmt-lin      .
           move      01                   to   w-stp-lit-pmt-pos      .
           perform   vis-tot-doc-pmt-000  thru vis-tot-doc-pmt-999    .
      *                      *-----------------------------------------*
      *                      * Ordini clienti                          *
      *                      *-----------------------------------------*
           move      18                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-mic (01)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Ordini di spedizione                    *
      *                      *-----------------------------------------*
           move      34                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-mic (02)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Bolle inevase                           *
      *                      *-----------------------------------------*
           move      50                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-mic (03)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scadenze aperte                         *
      *                      *-----------------------------------------*
           move      66                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-mic (04)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-doc-700.
       vis-tot-doc-530.
      *                  *---------------------------------------------*
      *                  * Indice 03 : mese successivo 01              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      000                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      03                   to   w-mpn-tsc-000          .
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fm1    to   w-stp-lit-pmt-dat      .
           move      v-lnr                to   w-stp-lit-pmt-lin      .
           move      01                   to   w-stp-lit-pmt-pos      .
           perform   vis-tot-doc-pmt-000  thru vis-tot-doc-pmt-999    .
      *                      *-----------------------------------------*
      *                      * Ordini clienti                          *
      *                      *-----------------------------------------*
           move      18                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms1 (01)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Ordini di spedizione                    *
      *                      *-----------------------------------------*
           move      34                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms1 (02)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Bolle inevase                           *
      *                      *-----------------------------------------*
           move      50                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms1 (03)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scadenze aperte                         *
      *                      *-----------------------------------------*
           move      66                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms1 (04)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-doc-700.
       vis-tot-doc-540.
      *                  *---------------------------------------------*
      *                  * Indice 04 : mese successivo 02              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fm2    to   w-stp-lit-pmt-dat      .
           move      v-lnr                to   w-stp-lit-pmt-lin      .
           move      01                   to   w-stp-lit-pmt-pos      .
           perform   vis-tot-doc-pmt-000  thru vis-tot-doc-pmt-999    .
      *                      *-----------------------------------------*
      *                      * Ordini clienti                          *
      *                      *-----------------------------------------*
           move      18                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms2 (01)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Ordini di spedizione                    *
      *                      *-----------------------------------------*
           move      34                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms2 (02)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Bolle inevase                           *
      *                      *-----------------------------------------*
           move      50                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms2 (03)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scadenze aperte                         *
      *                      *-----------------------------------------*
           move      66                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms2 (04)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-doc-700.
       vis-tot-doc-550.
      *                  *---------------------------------------------*
      *                  * Indice 05 : mese successivo 03              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      000                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      05                   to   w-mpn-tsc-000          .
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fm3    to   w-stp-lit-pmt-dat      .
           move      v-lnr                to   w-stp-lit-pmt-lin      .
           move      01                   to   w-stp-lit-pmt-pos      .
           perform   vis-tot-doc-pmt-000  thru vis-tot-doc-pmt-999    .
      *                      *-----------------------------------------*
      *                      * Ordini clienti                          *
      *                      *-----------------------------------------*
           move      18                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms3 (01)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Ordini di spedizione                    *
      *                      *-----------------------------------------*
           move      34                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms3 (02)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Bolle inevase                           *
      *                      *-----------------------------------------*
           move      50                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms3 (03)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scadenze aperte                         *
      *                      *-----------------------------------------*
           move      66                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms3 (04)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-doc-700.
       vis-tot-doc-560.
      *                  *---------------------------------------------*
      *                  * Indice 06 : mese successivo 04              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      000                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      06                   to   w-mpn-tsc-000          .
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fm4    to   w-stp-lit-pmt-dat      .
           move      v-lnr                to   w-stp-lit-pmt-lin      .
           move      01                   to   w-stp-lit-pmt-pos      .
           perform   vis-tot-doc-pmt-000  thru vis-tot-doc-pmt-999    .
      *                      *-----------------------------------------*
      *                      * Ordini clienti                          *
      *                      *-----------------------------------------*
           move      18                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms4 (01)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Ordini di spedizione                    *
      *                      *-----------------------------------------*
           move      34                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms4 (02)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Bolle inevase                           *
      *                      *-----------------------------------------*
           move      50                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms4 (03)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scadenze aperte                         *
      *                      *-----------------------------------------*
           move      66                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms4 (04)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-doc-700.
       vis-tot-doc-570.
      *                  *---------------------------------------------*
      *                  * Indice 07 : mese successivo 05              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      000                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      07                   to   w-mpn-tsc-000          .
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fm5    to   w-stp-lit-pmt-dat      .
           move      v-lnr                to   w-stp-lit-pmt-lin      .
           move      01                   to   w-stp-lit-pmt-pos      .
           perform   vis-tot-doc-pmt-000  thru vis-tot-doc-pmt-999    .
      *                      *-----------------------------------------*
      *                      * Ordini clienti                          *
      *                      *-----------------------------------------*
           move      18                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms5 (01)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Ordini di spedizione                    *
      *                      *-----------------------------------------*
           move      34                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms5 (02)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Bolle inevase                           *
      *                      *-----------------------------------------*
           move      50                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms5 (03)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scadenze aperte                         *
      *                      *-----------------------------------------*
           move      66                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms5 (04)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-doc-700.
       vis-tot-doc-580.
      *                  *---------------------------------------------*
      *                  * Indice 08 : mese successivo 06              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      000                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      08                   to   w-mpn-tsc-000          .
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fm6    to   w-stp-lit-pmt-dat      .
           move      v-lnr                to   w-stp-lit-pmt-lin      .
           move      01                   to   w-stp-lit-pmt-pos      .
           perform   vis-tot-doc-pmt-000  thru vis-tot-doc-pmt-999    .
      *                      *-----------------------------------------*
      *                      * Ordini clienti                          *
      *                      *-----------------------------------------*
           move      18                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms6 (01)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Ordini di spedizione                    *
      *                      *-----------------------------------------*
           move      34                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms6 (02)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Bolle inevase                           *
      *                      *-----------------------------------------*
           move      50                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms6 (03)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scadenze aperte                         *
      *                      *-----------------------------------------*
           move      66                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms6 (04)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-doc-700.
       vis-tot-doc-590.
      *                  *---------------------------------------------*
      *                  * Indice 09 : mese successivo 07              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      000                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      09                   to   w-mpn-tsc-000          .
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fm7    to   w-stp-lit-pmt-dat      .
           move      v-lnr                to   w-stp-lit-pmt-lin      .
           move      01                   to   w-stp-lit-pmt-pos      .
           perform   vis-tot-doc-pmt-000  thru vis-tot-doc-pmt-999    .
      *                      *-----------------------------------------*
      *                      * Ordini clienti                          *
      *                      *-----------------------------------------*
           move      18                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms7 (01)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Ordini di spedizione                    *
      *                      *-----------------------------------------*
           move      34                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms7 (02)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Bolle inevase                           *
      *                      *-----------------------------------------*
           move      50                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms7 (03)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scadenze aperte                         *
      *                      *-----------------------------------------*
           move      66                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms7 (04)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-doc-700.
       vis-tot-doc-600.
      *                  *---------------------------------------------*
      *                  * Indice 10 : mese successivo 08              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      000                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      10                   to   w-mpn-tsc-000          .
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fm8    to   w-stp-lit-pmt-dat      .
           move      v-lnr                to   w-stp-lit-pmt-lin      .
           move      01                   to   w-stp-lit-pmt-pos      .
           perform   vis-tot-doc-pmt-000  thru vis-tot-doc-pmt-999    .
      *                      *-----------------------------------------*
      *                      * Ordini clienti                          *
      *                      *-----------------------------------------*
           move      18                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms8 (01)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Ordini di spedizione                    *
      *                      *-----------------------------------------*
           move      34                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms8 (02)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Bolle inevase                           *
      *                      *-----------------------------------------*
           move      50                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms8 (03)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scadenze aperte                         *
      *                      *-----------------------------------------*
           move      66                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms8 (04)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-doc-700.
       vis-tot-doc-610.
      *                  *---------------------------------------------*
      *                  * Indice 11 : mese successivo 09              *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      000                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      11                   to   w-mpn-tsc-000          .
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      w-stp-dat-inp-fm9    to   w-stp-lit-pmt-dat      .
           move      v-lnr                to   w-stp-lit-pmt-lin      .
           move      01                   to   w-stp-lit-pmt-pos      .
           perform   vis-tot-doc-pmt-000  thru vis-tot-doc-pmt-999    .
      *                      *-----------------------------------------*
      *                      * Ordini clienti                          *
      *                      *-----------------------------------------*
           move      18                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms9 (01)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Ordini di spedizione                    *
      *                      *-----------------------------------------*
           move      34                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms9 (02)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Bolle inevase                           *
      *                      *-----------------------------------------*
           move      50                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms9 (03)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scadenze aperte                         *
      *                      *-----------------------------------------*
           move      66                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-ms9 (04)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-doc-700.
       vis-tot-doc-620.
      *                  *---------------------------------------------*
      *                  * Indice 12 : date successive                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione mark-point                 *
      *                      *-----------------------------------------*
           move      000                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      12                   to   w-mpn-tsc-000          .
      *                      *-----------------------------------------*
      *                      * Prompt                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "+"                  to   v-edm                  .
           move      w-mpn                to   v-cnt                  .
           move      "Successivi      :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Ordini clienti                          *
      *                      *-----------------------------------------*
           move      18                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-suc (01)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Ordini di spedizione                    *
      *                      *-----------------------------------------*
           move      34                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-suc (02)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Bolle inevase                           *
      *                      *-----------------------------------------*
           move      50                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-suc (03)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Scadenze aperte                         *
      *                      *-----------------------------------------*
           move      66                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-suc (04)
                                          to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                      *-----------------------------------------*
      *                      * Continuazione                           *
      *                      *-----------------------------------------*
           go to     vis-tot-doc-700.
       vis-tot-doc-700.
      *                  *---------------------------------------------*
      *                  * Riciclo a tipo scadenza successivo          *
      *                  *---------------------------------------------*
           go to     vis-tot-doc-300.
       vis-tot-doc-800.
      *              *-------------------------------------------------*
      *              * Totali finali                                   *
      *              *-------------------------------------------------*
       vis-tot-doc-805.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-doc-810.
      *                  *---------------------------------------------*
      *                  * Sottolineatura fincatura                    *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "----------------  --------------  --------------  
      -              "--------------  --------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-doc-815.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-doc-820.
      *                  *---------------------------------------------*
      *                  * Literal per i totali                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "         Totali :"  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-doc-830.
      *                  *---------------------------------------------*
      *                  * Totale 'Ordini clienti'                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione del totale                 *
      *                      *-----------------------------------------*
           move      w-tot-tot-gen-pre (1)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-mic (1)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms1 (1)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms2 (1)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms3 (1)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms4 (1)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms5 (1)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms6 (1)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms7 (1)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms8 (1)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms9 (1)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-suc (1)
                                          to   w-tot-tot-gen-s11      .
       vis-tot-doc-833.
      *                      *-----------------------------------------*
      *                      * Stampa del totale                       *
      *                      *-----------------------------------------*
           move      18                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-s11    to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
       vis-tot-doc-840.
      *                  *---------------------------------------------*
      *                  * Totale 'Ordini spedizione clienti'          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione del totale                 *
      *                      *-----------------------------------------*
           move      w-tot-tot-gen-pre (2)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-mic (2)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms1 (2)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms2 (2)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms3 (2)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms4 (2)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms5 (2)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms6 (2)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms7 (2)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms8 (2)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms9 (2)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-suc (2)
                                          to   w-tot-tot-gen-s11      .
      *                      *-----------------------------------------*
      *                      * Stampa del totale                       *
      *                      *-----------------------------------------*
           move      34                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-s11    to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
       vis-tot-doc-850.
      *                  *---------------------------------------------*
      *                  * Totale 'Bolle inevase'                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione del totale                 *
      *                      *-----------------------------------------*
           move      w-tot-tot-gen-pre (3)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-mic (3)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms1 (3)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms2 (3)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms3 (3)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms4 (3)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms5 (3)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms6 (3)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms7 (3)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms8 (3)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms9 (3)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-suc (3)
                                          to   w-tot-tot-gen-s11      .
      *                      *-----------------------------------------*
      *                      * Stampa del totale                       *
      *                      *-----------------------------------------*
           move      50                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-s11    to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
       vis-tot-doc-860.
      *                  *---------------------------------------------*
      *                  * Totale 'Scadenze aperte'                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Preparazione del totale                 *
      *                      *-----------------------------------------*
           move      w-tot-tot-gen-pre (4)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-mic (4)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms1 (4)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms2 (4)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms3 (4)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms4 (4)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms5 (4)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms6 (4)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms7 (4)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms8 (4)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms9 (4)
                                          to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-suc (4)
                                          to   w-tot-tot-gen-s11      .
      *                      *-----------------------------------------*
      *                      * Stampa del totale                       *
      *                      *-----------------------------------------*
           move      66                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-s11    to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
       vis-tot-doc-875.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-doc-880.
      *                  *---------------------------------------------*
      *                  * Lineette di separazione                     *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-doc-885.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-doc-890.
      *                  *---------------------------------------------*
      *                  * Normalizzazioni preliminari per fido        *
      *                  *---------------------------------------------*
           move      spaces               to   w-tot-tot-gen-wfl      .
           move      zero                 to   w-tot-tot-gen-wdd      .
      *                  *---------------------------------------------*
      *                  * Literal per 'Fido'                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Fido :"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
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
           move      rr-cod-cli           to   rf-ccc-cod-cli         .
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
           if        rf-ccc-imp-acr       not  numeric
                     move  zero           to   rf-ccc-imp-acr         .
           if        rf-ccc-dat-acr       not  numeric
                     move  zero           to   rf-ccc-dat-acr         .
           if        rf-ccc-scd-acr       not  numeric
                     move  zero           to   rf-ccc-scd-acr         .
      *                  *---------------------------------------------*
      *                  * Stampa fido cliente                         *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      08                   to   v-pos                  .
           move      rf-ccc-max-fid       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Literal per 'Assicurazione'                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da visualizzare                 *
      *                      *                                         *
      *                      *  - Spaces : Non richiesta               *
      *                      *  - S      : Accordata                   *
      *                      *  - N      : Rifiutata                   *
      *                      *-----------------------------------------*
           if        rf-ccc-snx-acr       =    spaces
                     go to vis-tot-doc-892.
           if        rf-ccc-snx-acr       =    "S"
                     go to vis-tot-doc-891.                     
      *                      *-----------------------------------------*
      *                      * Se rifiutata                            *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      16                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "Assic: Rifiutata"   to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Oltre                                   *
      *                      *-----------------------------------------*
           go to     vis-tot-doc-892.
       vis-tot-doc-891.
      *                      *-----------------------------------------*
      *                      * Se accordata                            *
      *                      *-----------------------------------------*
           if        rf-ccc-imp-acr       =    zero
                     go to vis-tot-doc-892.
      *                      *-----------------------------------------*
      *                      * Visualizzazione                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      19                   to   v-pos                  .
           move      "Assic:"             to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Stampa assicurazione                    *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      26                   to   v-pos                  .
           move      rf-ccc-imp-acr       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-doc-892.
      *                  *---------------------------------------------*
      *                  * Literal per 'Esposizione'                   *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      57                   to   v-pos                  .
           move      "Totale :"           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-doc-895.
      *                  *---------------------------------------------*
      *                  * Totale 'Esposizione'                        *
      *                  *---------------------------------------------*
       vis-tot-doc-896.
      *                      *-----------------------------------------*
      *                      * Preparazione del totale                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-tot-tot-gen-s11      .
           move      zero                 to   w-tot-tot-gen-i01      .
       vis-tot-doc-897.
           add       1                    to   w-tot-tot-gen-i01      .
           if        w-tot-tot-gen-i01    >    04
                     go to vis-tot-doc-898.
      *
           add       w-tot-tot-gen-pre
                    (w-tot-tot-gen-i01)   to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-mic
                    (w-tot-tot-gen-i01)   to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms1
                    (w-tot-tot-gen-i01)   to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms2
                    (w-tot-tot-gen-i01)   to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms3
                    (w-tot-tot-gen-i01)   to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms4
                    (w-tot-tot-gen-i01)   to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms5
                    (w-tot-tot-gen-i01)   to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms6
                    (w-tot-tot-gen-i01)   to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms7
                    (w-tot-tot-gen-i01)   to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms8
                    (w-tot-tot-gen-i01)   to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-ms9
                    (w-tot-tot-gen-i01)   to   w-tot-tot-gen-s11      .
           add       w-tot-tot-gen-suc
                    (w-tot-tot-gen-i01)   to   w-tot-tot-gen-s11      .
      *
           go to     vis-tot-doc-897.
       vis-tot-doc-898.
      *                      *-----------------------------------------*
      *                      * Stampa del totale                       *
      *                      *-----------------------------------------*
           move      66                   to   w-stp-imp-vlb-pos      .
           move      w-tot-tot-gen-s11    to   w-stp-imp-vlb-val      .
           perform   vis-imp-vlb-000      thru vis-imp-vlb-999        .
      *                  *---------------------------------------------*
      *                  * Literal per superamento del fido            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione del parametro da usare   *
      *                      * per il confronto                        *
      *                      *-----------------------------------------*
           if        rf-ccc-max-fid       =    zero and
                     rf-ccc-imp-acr       =    zero
                     go to vis-tot-doc-900.
      *                      *-----------------------------------------*
      *                      * Determinazione assicurazione            *
      *                      *-----------------------------------------*
           move      rf-ccc-imp-acr       to   w-tot-tot-gen-wac      .
      *
           if        c-dec                =    1
                     multiply 10          by   w-tot-tot-gen-wac
           else if   c-dec                =    2
                     multiply 100         by   w-tot-tot-gen-wac      .
      *                      *-----------------------------------------*
      *                      * Determinazione del fido                 *
      *                      *-----------------------------------------*
           move      rf-ccc-max-fid       to   w-tot-tot-gen-wfd      .
      *
           if        c-dec                =    1
                     multiply 10          by   w-tot-tot-gen-wfd
           else if   c-dec                =    2
                     multiply 100         by   w-tot-tot-gen-wfd      .
      *                      *-----------------------------------------*
      *                      * Test                                    *
      *                      *-----------------------------------------*
           if        w-tot-tot-gen-wac    <      w-tot-tot-gen-wfd and
                     w-tot-tot-gen-wac    >      zero
                     move  w-tot-tot-gen-wac
                                          to     w-tot-tot-gen-wcc
                     move  "A"            to     w-tot-tot-gen-wfl
           else      move  w-tot-tot-gen-wfd
                                          to     w-tot-tot-gen-wcc
                     move  "F"            to     w-tot-tot-gen-wfl    .
      *                      *-----------------------------------------*
      *                      * Test con esposizione                    *
      *                      *-----------------------------------------*
           if        w-tot-tot-gen-wcc    =     zero
                     go to vis-tot-doc-900.
      
           if        w-tot-tot-gen-wcc    >      w-tot-tot-gen-s11
                     go to vis-tot-doc-900.
           subtract  w-tot-tot-gen-s11    from   w-tot-tot-gen-wcc
                                        giving   w-tot-tot-gen-wdd    .
      *                      *-----------------------------------------*
      *                      * Test su eventuale differenza            *
      *                      *-----------------------------------------*
           if        w-tot-tot-gen-wdd    >    zero
                     go to vis-tot-doc-900.
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<BG"                to   v-edm                  .
           move      w-tot-tot-gen-wdd    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Assemblaggio                            *
      *                      *-----------------------------------------*
           move      18                   to   w-all-str-lun          .
           move      05                   to   w-all-str-num          .
           move      "!["                 to   w-all-str-cat (1)      .
           move      w-tot-tot-gen-wfl    to   w-all-str-cat (2)      .
           move      ":"                  to   w-all-str-cat (3)      .
           move      v-edt                to   w-all-str-cat (4)      .
           move      "]!"                 to   w-all-str-cat (5)      .
           perform   all-str-cat-000      thru all-str-cat-999        .
      *                      *-----------------------------------------*
      *                      * Visualizzazione eventuale esubero       *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      18                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      38                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-doc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tot-doc-999.
       vis-tot-doc-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione totali divisi per scadenza                *
      *    *                                                           *
      *    * Visualizzazione prompt                                    *
      *    *-----------------------------------------------------------*
       vis-tot-doc-pmt-000.
      *              *-------------------------------------------------*
      *              * Editing anno                                    *
      *              *-------------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      w-stp-lit-pmt-dat    to   s-dat                  .
           move      s-saa                to   v-num                  .
           add       1900                 to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Literal del mese, da segreteria                 *
      *              *-------------------------------------------------*
           move      "LM"                 to   s-ope                  .
           move      "E"                  to   s-tip                  .
           move      s-mes                to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Composizione                                    *
      *              *-------------------------------------------------*
           move      17                   to   w-all-str-lun          .
           move      02                   to   w-all-str-num          .
           move      s-alf                to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *
           move      ":"                  to   w-all-str-alf
                                              (17 : 01)               .
      *                          *-------------------------------------*
      *                          * Visualizzazione                     *
      *                          *-------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      w-stp-lit-pmt-lin    to   v-lin                  .
           move      w-stp-lit-pmt-pos    to   v-pos                  .
           move      "+"                  to   v-edm                  .
           move      w-mpn                to   v-cnt                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-doc-pmt-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tot-doc-pmt-999.
       vis-tot-doc-pmt-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un importo in valuta base                       *
      *    *-----------------------------------------------------------*
       vis-imp-vlb-000.
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      w-stp-imp-vlb-pos    to   v-pos                  .
           move      w-stp-imp-vlb-val    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-imp-vlb-999.
           exit.

      *    *===========================================================*
      *    * Intestazione comune per l'interrogazione                  *
      *    *-----------------------------------------------------------*
       stp-int-pag-000.
       stp-int-pag-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Function-keys previste in Mark-points    *
      *    *-----------------------------------------------------------*
       qry-det-fky-000.
      *              *-------------------------------------------------*
      *              * Function key Slct                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se non e' stata passata una variabile di i. *
      *                  * p.c. per l'ammissibilita' del tasto Slct :  *
      *                  * no function key Slct                        *
      *                  *---------------------------------------------*
           if        w-ipc-snx-slc-snx    not  = "S"
                     move  spaces         to   v-pfk (01)
                     go to qry-det-fky-100.
      *                  *---------------------------------------------*
      *                  * Se il valore della variabile di i.p.c. per  *
      *                  * l'ammissibilita' del tasto Slct e' diverso  *
      *                  * da 'S' : no function key Slct               *
      *                  *---------------------------------------------*
           if        w-ipc-snx-slc-val    not  = "S"
                     move  spaces         to   v-pfk (01)
                     go to qry-det-fky-100.
      *                  *---------------------------------------------*
      *                  * Ammissibilita' del tasto Slct               *
      *                  *---------------------------------------------*
           move      "SLCT"               to   v-pfk (01)             .
       qry-det-fky-100.
      *              *-------------------------------------------------*
      *              * Function key Expd                               *
      *              *-------------------------------------------------*
           move      "P?"                 to   s-ope                  .
           move      "pgep3010"           to   s-pro                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
           if        s-liv                =    zero
                     move  "EXPD"         to   v-pfk(02)
           else      move  spaces         to   v-pfk(02)              .
       qry-det-fky-200.
      *              *-------------------------------------------------*
      *              * Altre function keys                             *
      *              *-------------------------------------------------*
           move      spaces               to   v-pfk (03)             .
           move      spaces               to   v-pfk (04)             .
           move      spaces               to   v-pfk (05)             .
           move      spaces               to   v-pfk (06)             .
           move      spaces               to   v-pfk (07)             .
           move      spaces               to   v-pfk (08)             .
           move      spaces               to   v-pfk (09)             .
           move      spaces               to   v-pfk (10)             .
       qry-det-fky-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Trattamento Function-key selezionata     *
      *    *-----------------------------------------------------------*
       qry-trt-fun-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di interruzione forzata    *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-int      .
       qry-trt-fun-100.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda della function-key         *
      *              *-------------------------------------------------*
           if        v-key                =    "SLCT"
                     go to qry-trt-fun-200
           else if   v-key                =    "EXPD"
                     go to qry-trt-fun-300
           else      go to qry-trt-fun-900.
       qry-trt-fun-200.
      *              *-------------------------------------------------*
      *              * Se function-key Slct                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trattamento function key Slct               *
      *                  *---------------------------------------------*
           perform   trt-fky-slc-000      thru trt-fky-slc-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-trt-fun-999.
       qry-trt-fun-300.
      *              *-------------------------------------------------*
      *              * Se function-key Expd                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Trattamento function key Expd               *
      *                  *---------------------------------------------*
           perform   trt-fky-exp-000      thru trt-fky-exp-999        .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-trt-fun-999.
       qry-trt-fun-900.
      *              *-------------------------------------------------*
      *              * Se function-key non riconosciuta                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione function key                *
      *                  *---------------------------------------------*
           move      spaces               to   v-key                  .
      *                  *---------------------------------------------*
      *                  * Uscita senza alcuna azione                  *
      *                  *---------------------------------------------*
           go to     qry-trt-fun-999.
       qry-trt-fun-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Trattamento Function-key Slct            *
      *    *-----------------------------------------------------------*
       trt-fky-slc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione function key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Uscita senza alcuna azione                      *
      *              *-------------------------------------------------*
           go to     trt-fky-slc-999.
       trt-fky-slc-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Trattamento Function-key Expd            *
      *    *-----------------------------------------------------------*
       trt-fky-exp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione function key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Test su contenuto del mark-point                *
      *              *-------------------------------------------------*
           move      v-cnt                to   w-mpn                  .
           if        w-mpn-tip-mpn        =    001
                     perform  trt-fky-exp-orc-000
                                          thru trt-fky-exp-orc-999
           else if   w-mpn-tip-mpn        =    002
                     perform  trt-fky-exp-ods-000
                                          thru trt-fky-exp-ods-999
           else if   w-mpn-tip-mpn        =    003
                     perform  trt-fky-exp-bol-000
                                          thru trt-fky-exp-bol-999
           else if   w-mpn-tip-mpn        =    004
                     perform  trt-fky-exp-gep-000
                                          thru trt-fky-exp-gep-999
           else      go to trt-fky-exp-999.
       trt-fky-exp-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Trattamento Function-key Expd            *
      *    *                                                           *
      *    * Trattamento ordini clienti                                *
      *    *-----------------------------------------------------------*
       trt-fky-exp-orc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione function key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Modulo di Expand su righe ordine                *
      *              *-------------------------------------------------*
           move      v-cnt                to   w-mpn                  .
           move      w-mpn-dat-orc        to   w-esp-rig-ocr-dat      .
           move      w-mpn-num-orc        to   w-esp-rig-ocr-num      .
           move      w-mpn-tip-orc        to   w-esp-rig-ocr-tmo      .
           move      w-mpn-prt-orc        to   w-esp-rig-ocr-prt      .
           move      spaces               to   w-esp-rig-ocr-pro      .
      *              *-------------------------------------------------*
      *              * Chiamata del modulo                             *
      *              *-------------------------------------------------*
           perform   esp-rig-ocr-esp-000  thru esp-rig-ocr-esp-999    .
       trt-fky-exp-orc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     trt-fky-exp-orc-999.
       trt-fky-exp-orc-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Trattamento Function-key Expd            *
      *    *                                                           *
      *    * Trattamento ordini di spedizione clienti                  *
      *    *-----------------------------------------------------------*
       trt-fky-exp-ods-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione function key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Modulo di Expand su righe ordine                *
      *              *-------------------------------------------------*
           move      v-cnt                to   w-mpn                  .
           move      w-mpn-dat-ods        to   w-esp-rig-osr-dat      .
           move      w-mpn-tip-ods        to   w-esp-rig-osr-tmo      .
           move      w-mpn-prt-ods        to   w-esp-rig-osr-prt      .
           move      spaces               to   w-esp-rig-osr-pro      .
      *              *-------------------------------------------------*
      *              * Chiamata del modulo                             *
      *              *-------------------------------------------------*
           perform   esp-rig-osr-esp-000  thru esp-rig-osr-esp-999    .
       trt-fky-exp-ods-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     trt-fky-exp-ods-999.
       trt-fky-exp-ods-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Trattamento Function-key Expd            *
      *    *                                                           *
      *    * Trattamento bolle clienti                                 *
      *    *-----------------------------------------------------------*
       trt-fky-exp-bol-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione function key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Modulo di Expand su righe ordine                *
      *              *-------------------------------------------------*
           move      v-cnt                to   w-mpn                  .
           move      w-mpn-dat-bol        to   w-esp-rig-bir-dat      .
           move      w-mpn-num-bol        to   w-esp-rig-bir-num      .
           move      w-mpn-tip-bol        to   w-esp-rig-bir-tmo      .
           move      w-mpn-prt-bol        to   w-esp-rig-bir-prt      .
           move      spaces               to   w-esp-rig-bir-pro      .
      *              *-------------------------------------------------*
      *              * Chiamata del modulo                             *
      *              *-------------------------------------------------*
           perform   esp-rig-bir-esp-000  thru esp-rig-bir-esp-999    .
       trt-fky-exp-bol-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     trt-fky-exp-bol-999.
       trt-fky-exp-bol-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Trattamento Function-key Expd            *
      *    *                                                           *
      *    * Trattamento scadenze clienti                              *
      *    *-----------------------------------------------------------*
       trt-fky-exp-gep-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione function key                    *
      *              *-------------------------------------------------*
           move      spaces               to   v-key                  .
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'tip-int' per il  *
      *              * livello successivo per il tipo di interrogazio- *
      *              * ne                                              *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "tip-int"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "A"                  to   s-tip                  .
           move      10                   to   s-car                  .
           move      "ESPSDB    "         to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'num-sdb' per il  *
      *              * livello successivo per il numero scadenza su    *
      *              * cui eseguire l'espansione                       *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "num-sdb"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      11                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      v-cnt                to   w-mpn                  .
           move      w-mpn-num-gep        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo del programma di interrogazione        *
      *              *-------------------------------------------------*
           move      "pgm/gep/prg/obj/pgep3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       trt-fky-exp-gep-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     trt-fky-exp-gep-999.
       trt-fky-exp-gep-999.
           exit.

      *    *===========================================================*
      *    * Intestazione per interrogazione                           *
      *    *-----------------------------------------------------------*
       int-pag-sta-000.
      *              *-------------------------------------------------*
      *              * Subroutine di avanzamento pagina                *
      *              *-------------------------------------------------*
           perform   qry-pag-adv-000      thru qry-pag-adv-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to int-pag-sta-999.
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-cnt-lit-t80        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Titolo                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
           move      i-ide-des            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea di trattini                               *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-cnt-lit-t80        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Intestazione debitore                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Prompt per codice cliente                   *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Cliente    :"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      04                   to   v-lin                  .
           move      14                   to   v-pos                  .
           move      "<B"                 to   v-edm                  .
           move      rr-cod-cli           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Dipendenza                                  *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      22                   to   v-pos                  .
           move      rr-dpz-cli           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      04                   to   v-lin                  .
           move      27                   to   v-pos                  .
           move      rr-cod-cli-rag       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Linea di trattini                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      05                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all "-"              to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Linea di fincatura                              *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  Numero     Tipo     Data      Importo    Compete
      -              "nza                           "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Sottolineatura fincatura                        *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "-----------  -----  --------  -----------  -------
      -              "---                           "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       int-pag-sta-999.
           exit.

      *    *===========================================================*
      *    * Inizializzazione totali per tipo documento                *
      *    *-----------------------------------------------------------*
       tot-doc-ini-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione indice 01..04                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-gen-i01      .
       tot-doc-ini-100.
      *              *-------------------------------------------------*
      *              * Incremento indice 01..04                        *
      *              *-------------------------------------------------*
           add       1                    to   w-tot-tot-gen-i01      .
      *              *-------------------------------------------------*
      *              * Se oltre il max : uscita                        *
      *              *-------------------------------------------------*
           if        w-tot-tot-gen-i01    >    04
                     go to tot-doc-ini-999.
      *              *-------------------------------------------------*
      *              * Azzeramento totale importo                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-gen-tot
                                              (w-tot-tot-gen-i01)     .
      *              *-------------------------------------------------*
      *              * Azzeramento totale numero elementi              *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-gen-num
                                              (w-tot-tot-gen-i01)     .
      *              *-------------------------------------------------*
      *              * Azzeramento totale 'Precedenti'                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-gen-pre
                                              (w-tot-tot-gen-i01)     .
      *              *-------------------------------------------------*
      *              * Azzeramento totali mensili                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-gen-mic
                                              (w-tot-tot-gen-i01)     .
           move      zero                 to   w-tot-tot-gen-ms1
                                              (w-tot-tot-gen-i01)     .
           move      zero                 to   w-tot-tot-gen-ms2
                                              (w-tot-tot-gen-i01)     .
           move      zero                 to   w-tot-tot-gen-ms3
                                              (w-tot-tot-gen-i01)     .
           move      zero                 to   w-tot-tot-gen-ms4
                                              (w-tot-tot-gen-i01)     .
           move      zero                 to   w-tot-tot-gen-ms5
                                              (w-tot-tot-gen-i01)     .
           move      zero                 to   w-tot-tot-gen-ms6
                                              (w-tot-tot-gen-i01)     .
           move      zero                 to   w-tot-tot-gen-ms7
                                              (w-tot-tot-gen-i01)     .
           move      zero                 to   w-tot-tot-gen-ms8
                                              (w-tot-tot-gen-i01)     .
           move      zero                 to   w-tot-tot-gen-ms9
                                              (w-tot-tot-gen-i01)     .
      *              *-------------------------------------------------*
      *              * Azzeramento totale 'Successivi'                 *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-tot-gen-suc
                                              (w-tot-tot-gen-i01)     .
      *              *-------------------------------------------------*
      *              * Riciclo su elemento successivo                  *
      *              *-------------------------------------------------*
           go to     tot-doc-ini-100.
       tot-doc-ini-999.
           exit.

      *    *===========================================================*
      *    * Memorizzazione sottoprogramma in attivita'                *
      *    *-----------------------------------------------------------*
       mem-spg-att-000.
      *              *-------------------------------------------------*
      *              * Se numero elementi in tabella gia' pari al mas- *
      *              * simo numero di elementi possibili : uscita per  *
      *              * memorizzazione non avvenuta                     *
      *              *-------------------------------------------------*
           if        w-spg-ele-num        =    w-spg-ele-max
                     move  "N"            to   w-spg-snx-gat
                     go to mem-spg-att-999.
      *              *-------------------------------------------------*
      *              * Incremento numero elementi in tabella           *
      *              *-------------------------------------------------*
           add       1                    to   w-spg-ele-num          .
      *              *-------------------------------------------------*
      *              * Memorizzazione codice alfanumerico del tipo di  *
      *              * interrogazione                                  *
      *              *-------------------------------------------------*
           move      w-spg-alf-gat        to   w-spg-alf-tin
                                              (w-spg-ele-num)         .
      *              *-------------------------------------------------*
      *              * Uscita per memorizzazione avvenuta              *
      *              *-------------------------------------------------*
           move      spaces               to   w-spg-snx-gat          .
       mem-spg-att-999.
           exit.

      *    *===========================================================*
      *    * Eliminazione sottoprogramma in attivita'                  *
      *    *-----------------------------------------------------------*
       eli-spg-att-000.
      *              *-------------------------------------------------*
      *              * Se numero elementi in tabella a zero : uscita   *
      *              *-------------------------------------------------*
           if        w-spg-ele-num        =    zero
                     go to eli-spg-att-999.
      *              *-------------------------------------------------*
      *              * Se l'ultimo elemento in tabella non e' pari a   *
      *              * quello da eliminare : uscita                    *
      *              *-------------------------------------------------*
           if        w-spg-alf-tin
                    (w-spg-ele-num)       not  = w-spg-alf-gat
                     go to eli-spg-att-999.
      *              *-------------------------------------------------*
      *              * Decremento numero elementi in tabella           *
      *              *-------------------------------------------------*
           subtract  1                    from w-spg-ele-num          .
       eli-spg-att-999.
           exit.

      *    *===========================================================*
      *    * Test se sottoprogramma gia' attivo                        *
      *    *-----------------------------------------------------------*
       tst-spg-gat-000.
      *              *-------------------------------------------------*
      *              * Risposta a : non gia' attivo                    *
      *              *-------------------------------------------------*
           move      spaces               to   w-spg-snx-gat          .
      *              *-------------------------------------------------*
      *              * Se codice alfanumerico del tipo di interroga-   *
      *              * zione a spaces : uscita                         *
      *              *-------------------------------------------------*
           if        w-spg-alf-gat        =    spaces
                     go to tst-spg-gat-999.
      *              *-------------------------------------------------*
      *              * Indice per scansione su tabella a zero          *
      *              *-------------------------------------------------*
           move      zero                 to   w-spg-ele-inx          .
       tst-spg-gat-200.
      *              *-------------------------------------------------*
      *              * Incremento indice per scansione su tabella      *
      *              *-------------------------------------------------*
           add       1                    to   w-spg-ele-inx          .
      *              *-------------------------------------------------*
      *              * Se oltre numero elementi memorizzati : uscita   *
      *              *-------------------------------------------------*
           if        w-spg-ele-inx        >    w-spg-ele-num
                     go to tst-spg-gat-999.
      *              *-------------------------------------------------*
      *              * Se l'elemento non e' quello cercato : riciclo   *
      *              * su elemento successivo                          *
      *              *-------------------------------------------------*
           if        w-spg-alf-tin
                    (w-spg-ele-inx)       not  = w-spg-alf-gat
                     go to tst-spg-gat-200.
       tst-spg-gat-400.
      *              *-------------------------------------------------*
      *              * Risposta a : gia' attivo                        *
      *              *-------------------------------------------------*
           move      "S"                  to   w-spg-snx-gat          .
       tst-spg-gat-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice cliente commerciale *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acmndcc0.acs"                   .

      *    *===========================================================*
      *    * Subroutines per l'accettazione codice dipendenza cliente  *
      *    *-----------------------------------------------------------*
           copy      "pgm/dcc/prg/cpy/acoddcc0.acs"                   .

      *    *===========================================================*
      *    * Normalizzazione totali ordine                             *
      *    *-----------------------------------------------------------*
       ini-tot-orc-000.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Coefficiente di cambio va-  *
      *              * luta per fatturazione                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-oct-sgl-vpf       to   w-coe-cmb-vlt-sdv      .
           move      rf-oct-tdc-vpf       to   w-coe-cmb-vlt-tdc      .
           move      rf-oct-dat-doc       to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                  *---------------------------------------------*
      *                  * Se esito determinazione negativo : buffe-   *
      *                  * rizzazione coefficiente di cambio contenuto *
      *                  * nel record [ost]                            *
      *                  *---------------------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-oct-cdc-vpf to   w-coe-cmb-vlt-cdc      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione coefficiente di cambio      *
      *                  *---------------------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-tot-cdc-vpf          .
       ini-tot-orc-100.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Forma di pagamento          *
      *              *-------------------------------------------------*
           move      rf-oct-cod-fop       to   w-det-vas-fop-cod      .
           perform   det-vas-fop-000      thru det-vas-fop-999        .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Sconto in chiusura          *
      *              *-------------------------------------------------*
           move      w-ref-sco-chi-civ    to   w-tot-civ-scc          .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Sconto pagamento            *
      *              *-------------------------------------------------*
           move      w-ref-sco-pag-civ    to   w-tot-civ-scp          .
       ini-tot-orc-200.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Spese in fattura            *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-orc-wrk-ct1      .
       ini-tot-orc-220.
           add       1                    to   w-stp-orc-wrk-ct1      .
           if        w-stp-orc-wrk-ct1    >    6
                     go to ini-tot-orc-300.
           move      w-prs-spe-fat-civ
                    (w-stp-orc-wrk-ct1)   to   w-tot-spe-civ
                                              (w-stp-orc-wrk-ct1)     .
           go to     ini-tot-orc-220.
       ini-tot-orc-300.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Spese incasso               *
      *              *-------------------------------------------------*
           move      rf-oct-add-spi       to   w-det-vas-spi-cod      .
           perform   det-vas-spi-000      thru det-vas-spi-999        .
           move      zero                 to   w-tot-tot-sic          .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Spese bollo                 *
      *              *-------------------------------------------------*
           move      rf-oct-add-spb       to   w-det-vas-spb-cod      .
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
       ini-tot-orc-400.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Castelletto iva             *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-iva-ele          .
           move      zero                 to   w-stp-orc-wrk-ct1      .
       ini-tot-orc-420.
           add       1                    to   w-stp-orc-wrk-ct1      .
           if        w-stp-orc-wrk-ct1    >    12
                     go to ini-tot-orc-500.
           move      zero                 to   w-tot-iva-cod
                                              (w-stp-orc-wrk-ct1)     .
           move      zero                 to   w-tot-iva-ibl
                                              (w-stp-orc-wrk-ct1)     .
           move      zero                 to   w-tot-iva-imp
                                              (w-stp-orc-wrk-ct1)     .
           go to     ini-tot-orc-420.
       ini-tot-orc-500.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Castelletto scadenze        *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-scd-ele          .
           move      zero                 to   w-stp-orc-wrk-ct1      .
       ini-tot-orc-520.
           add       1                    to   w-stp-orc-wrk-ct1      .
           if        w-stp-orc-wrk-ct1    >    96
                     go to ini-tot-orc-999.
           move      zero                 to   w-tot-scd-tip
                                              (w-stp-orc-wrk-ct1)     .
           move      zero                 to   w-tot-scd-dat
                                              (w-stp-orc-wrk-ct1)     .
           move      zero                 to   w-tot-scd-cau
                                              (w-stp-orc-wrk-ct1)     .
           move      zero                 to   w-tot-scd-imp
                                              (w-stp-orc-wrk-ct1)     .
           go to     ini-tot-orc-520.
       ini-tot-orc-999.
           exit.

      *    *===========================================================*
      *    * Pre-scansione righe ordine                                *
      *    *-----------------------------------------------------------*
       pre-scn-rig-orc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore elementi              *
      *              *-------------------------------------------------*
           move      zero                 to   w-roc-num-ele          .
       pre-scn-rig-orc-100.
      *              *-------------------------------------------------*
      *              * Start su righe documento [ocr]                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      w-roc-num-prt        to   rf-ocr-num-prt         .
           move      zero                 to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Se errore di Start : ad uscita              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pre-scn-rig-orc-900.
       pre-scn-rig-orc-200.
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
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to pre-scn-rig-orc-900.
       pre-scn-rig-orc-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-ocr-num-prt       not  = w-roc-num-prt
                     go to pre-scn-rig-orc-900.
       pre-scn-rig-orc-400.
      *              *-------------------------------------------------*
      *              * Selezioni                                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scomposizione tipo riga                     *
      *                  *---------------------------------------------*
           move      rf-ocr-tip-rig       to   w-ctl-tip-rig-tri      .
           perform   ctl-tip-rig-000      thru ctl-tip-rig-999        .
      *                  *---------------------------------------------*
      *                  * Se tipo riga 'C' : riciclo                  *
      *                  *---------------------------------------------*
           if        w-ctl-tip-rig-tpr    =    "C"
                     go to pre-scn-rig-orc-200.
      *                  *---------------------------------------------*
      *                  * Determinazione della quantita' residua      *
      *                  *---------------------------------------------*
           move      "DT"                 to   d-qev-roc-tip-ope      .
           perform   det-qev-roc-cll-000  thru det-qev-roc-cll-999    .
      *                  *---------------------------------------------*
      *                  * Considerazioni su quantita' residua         *
      *                  *---------------------------------------------*
           if        d-qev-roc-qta-dev    =    zero
                     go to pre-scn-rig-orc-200.
       pre-scn-rig-orc-600.
      *              *-------------------------------------------------*
      *              * Bufferizzazione riga                            *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento contatore                        *
      *                  *---------------------------------------------*
           add       1                    to   w-roc-num-ele          .
           if        w-roc-num-ele        >    w-roc-max-ele
                     move  w-roc-max-ele  to   w-roc-num-ele
                     go to pre-scn-rig-orc-900.
      *                  *---------------------------------------------*
      *                  * Bufferizzazione data consegna prevista      *
      *                  *---------------------------------------------*
           if        rf-ocr-dcn-prv       =    zero
                     move  rf-ocr-dcn-ric to   rf-ocr-dcn-prv         .
           if        rf-ocr-dcn-prv       =    zero
                     move  rf-oct-dat-cns to   rf-ocr-dcn-prv         .
           if        rf-ocr-dcn-prv       =    zero
                     move  rf-oct-dat-doc to   rf-ocr-dcn-prv         .
           move      rf-ocr-dcn-prv       to   w-roc-key-prv
                                              (w-roc-num-ele)         .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione progressivo riga            *
      *                  *---------------------------------------------*
           move      rf-ocr-num-prg       to   w-roc-key-prg
                                              (w-roc-num-ele)         .
       pre-scn-rig-orc-800.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale file [ocr]       *
      *              *-------------------------------------------------*
           go to     pre-scn-rig-orc-200.
       pre-scn-rig-orc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     pre-scn-rig-orc-999.
       pre-scn-rig-orc-999.
           exit.

      *    *===========================================================*
      *    * Ordinamento righe ordine cliente bufferizzate             *
      *    *-----------------------------------------------------------*
       ord-fin-rig-orc-000.
      *              *-------------------------------------------------*
      *              * Test se almeno due codici da ordinare           *
      *              *-------------------------------------------------*
           if        w-roc-num-ele        <    2
                     go to ord-fin-rig-orc-999.
       ord-fin-rig-orc-050.
      *              *-------------------------------------------------*
      *              * Ciclo di ordinamento                            *
      *              *-------------------------------------------------*
           move      zero                 to   w-roc-ctr-001          .
       ord-fin-rig-orc-100.
           add       1                    to   w-roc-ctr-001          .
           if        w-roc-ctr-001        =    w-roc-num-ele
                     go to ord-fin-rig-orc-999.
           move      w-roc-ctr-001        to   w-roc-ctr-002
                                               w-roc-ctr-003          .
           move      w-roc-key-ord
                    (w-roc-ctr-001)       to   w-roc-sav-key          .
       ord-fin-rig-orc-200.
           add       1                    to   w-roc-ctr-002          .
           if        w-roc-ctr-002        >    w-roc-num-ele
                     go to ord-fin-rig-orc-300.
           if        w-roc-key-ord
                    (w-roc-ctr-002)       >    w-roc-sav-key
                     go to ord-fin-rig-orc-200.
           move      w-roc-ctr-002        to   w-roc-ctr-003          .
           move      w-roc-key-ord
                    (w-roc-ctr-002)       to   w-roc-sav-key          .
           go to     ord-fin-rig-orc-200.
       ord-fin-rig-orc-300.
           move      w-roc-ctr-001        to   w-roc-ctr-004          .          
           if        w-roc-sav-key        >    w-roc-key-ord
                                              (w-roc-ctr-004)
                     go to ord-fin-rig-orc-100.
           move      w-roc-sng-ele
                    (w-roc-ctr-003)       to   w-roc-sng-ele (999)    .
           move      w-roc-sng-ele
                    (w-roc-ctr-004)       to   w-roc-sng-ele
                                              (w-roc-ctr-003)         .
           move      w-roc-sng-ele (999)  to   w-roc-sng-ele
                                              (w-roc-ctr-004)         .
           go to     ord-fin-rig-orc-100.
       ord-fin-rig-orc-999.
           exit.

      *    *===========================================================*
      *    * Trattamento fine tranche ordine cliente                   *
      *    *-----------------------------------------------------------*
       trt-fin-trc-orc-000.
      *              *-------------------------------------------------*
      *              * Test su contatore elementi                      *
      *              *-------------------------------------------------*
           if        w-stp-orc-wrk-ele    =    zero
                     go to trt-fin-trc-orc-900.
      *              *-------------------------------------------------*
      *              * Determinazione totali ordine                    *
      *              *-------------------------------------------------*
           perform   det-tot-orc-000      thru det-tot-orc-999        .
       trt-fin-trc-orc-100.
      *              *-------------------------------------------------*
      *              * Sventagliamento delle scadenze nei mesi di      *
      *              * competenza                                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-orc-wrk-ctr      .
       trt-fin-trc-orc-200.
           add       1                    to   w-stp-orc-wrk-ctr      .
      *                  *---------------------------------------------*
      *                  * Test preliminari                            *
      *                  *---------------------------------------------*
           if        w-tot-scd-imp
                    (w-stp-orc-wrk-ctr)   =    zero
                     go to trt-fin-trc-orc-800.
           if        w-stp-orc-wrk-ctr    >    96
                     go to trt-fin-trc-orc-800.
      *                  *---------------------------------------------*
      *                  * Confronto fra date                          *
      *                  *---------------------------------------------*
           if        w-tot-scd-dat
                    (w-stp-orc-wrk-ctr)   >    w-stp-dat-inp-fm9
                     add  w-tot-scd-imp
                         (w-stp-orc-wrk-ctr)
                                          to   w-stp-orc-cum-suc
                                              (w-stp-orc-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orc-wrk-ctr)   >    w-stp-dat-inp-fm8
                     add  w-tot-scd-imp
                         (w-stp-orc-wrk-ctr)
                                          to   w-stp-orc-cum-ms9
                                              (w-stp-orc-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orc-wrk-ctr)   >    w-stp-dat-inp-fm7
                     add  w-tot-scd-imp
                         (w-stp-orc-wrk-ctr)
                                          to   w-stp-orc-cum-ms8
                                              (w-stp-orc-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orc-wrk-ctr)   >    w-stp-dat-inp-fm6
                     add  w-tot-scd-imp
                         (w-stp-orc-wrk-ctr)
                                          to   w-stp-orc-cum-ms7
                                              (w-stp-orc-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orc-wrk-ctr)   >    w-stp-dat-inp-fm5
                     add  w-tot-scd-imp
                         (w-stp-orc-wrk-ctr)
                                          to   w-stp-orc-cum-ms6
                                              (w-stp-orc-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orc-wrk-ctr)   >    w-stp-dat-inp-fm4
                     add  w-tot-scd-imp
                         (w-stp-orc-wrk-ctr)
                                          to   w-stp-orc-cum-ms5
                                              (w-stp-orc-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orc-wrk-ctr)   >    w-stp-dat-inp-fm3
                     add  w-tot-scd-imp
                         (w-stp-orc-wrk-ctr)
                                          to   w-stp-orc-cum-ms4
                                              (w-stp-orc-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orc-wrk-ctr)   >    w-stp-dat-inp-fm2
                     add  w-tot-scd-imp
                         (w-stp-orc-wrk-ctr)
                                          to   w-stp-orc-cum-ms3
                                              (w-stp-orc-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orc-wrk-ctr)   >    w-stp-dat-inp-fm1
                     add  w-tot-scd-imp
                         (w-stp-orc-wrk-ctr)
                                          to   w-stp-orc-cum-ms2
                                              (w-stp-orc-wrk-ele)
           else if   w-tot-scd-dat
                    (w-stp-orc-wrk-ctr)   >    w-stp-dat-inp-fmp
                     add  w-tot-scd-imp
                         (w-stp-orc-wrk-ctr)
                                          to   w-stp-orc-cum-mic
                                              (w-stp-orc-wrk-ele)

           else      add  w-tot-scd-imp
                         (w-stp-orc-wrk-ctr)
                                          to   w-stp-orc-cum-pre
                                              (w-stp-orc-wrk-ele)     .
       trt-fin-trc-orc-400.
      *                  *---------------------------------------------*
      *                  * Stampa riga dettaglio                       *
      *                  *---------------------------------------------*
           if        rr-tip-evd           not  = 01 and
                     rr-tip-evd           not  = 02
                     go to trt-fin-trc-orc-800.
      *                  *---------------------------------------------*
      *                  * Test se righe residue sufficienti           *
      *                  *---------------------------------------------*
           if        v-res                >    1
                     go to trt-fin-trc-orc-420.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                      *-----------------------------------------*
      *                      * Test se interruzione forzata            *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to trt-fin-trc-orc-800.
       trt-fin-trc-orc-420.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero documento                            *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "B"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      rf-oct-num-doc       to   w-wrk-num-prt          .
           move      w-wrk-npt-saa        to   w-wrk-nps-saa          .
           move      w-wrk-npt-prg        to   w-wrk-nps-prg          .
           move      w-wrk-prt-stp        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Tipo documento                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      05                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      14                   to   v-pos                  .
           move      rf-oct-tmo-orc       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data documento                              *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
           move      rf-oct-dat-doc       to   v-dat                  .
      *
           move      "+"                  to   v-edm                  .
           move      001                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      rf-oct-dat-doc       to   w-mpn-dat-orc          .
           move      rf-oct-num-doc       to   w-mpn-num-orc          .
           move      rf-oct-tmo-orc       to   w-mpn-tip-orc          .
           move      rf-oct-num-prt       to   w-mpn-prt-orc          .
           move      w-mpn                to   v-cnt                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Importo documento                           *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           if        w-tot-scd-imp
                    (w-stp-orc-wrk-ctr)   >    999999999 or
                     w-tot-scd-imp
                    (w-stp-orc-wrk-ctr)   <   -999999999
                     move  11             to   v-car
           else      move  09             to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           if        w-tot-scd-imp
                    (w-stp-orc-wrk-ctr)   >    999999999 or
                     w-tot-scd-imp
                    (w-stp-orc-wrk-ctr)   <   -999999999
                     move  spaces         to   v-edm
           else      move  "G"            to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      29                   to   v-pos                  .
      *
           if        c-dec                <    1
                     add  1               to   v-pos                  .
      *
           move      w-tot-scd-imp
                    (w-stp-orc-wrk-ctr)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data competenza                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      45                   to   v-pos                  .
           move      w-tot-scd-dat
                    (w-stp-orc-wrk-ctr)   to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Eventuale prompt                            *
      *                  *---------------------------------------------*
           if        w-stp-orc-wrk-pmt    not  = spaces
                     go to trt-fin-trc-orc-680.
      *
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      24                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      57                   to   v-pos                  .
           move      "Ordini inevasi          "
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *
           move      "#"                  to   w-stp-orc-wrk-pmt      .
       trt-fin-trc-orc-680.
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     trt-fin-trc-orc-200.
       trt-fin-trc-orc-800.
      *              *-------------------------------------------------*
      *              * Cumulo totali generali                          *
      *              *-------------------------------------------------*
           add       w-stp-orc-cum-pre
                    (w-stp-orc-wrk-ele)   to   w-tot-tot-gen-pre (1)  .
           add       w-stp-orc-cum-mic
                    (w-stp-orc-wrk-ele)   to   w-tot-tot-gen-mic (1)  .
           add       w-stp-orc-cum-ms1
                    (w-stp-orc-wrk-ele)   to   w-tot-tot-gen-ms1 (1)  .
           add       w-stp-orc-cum-ms2
                    (w-stp-orc-wrk-ele)   to   w-tot-tot-gen-ms2 (1)  .
           add       w-stp-orc-cum-ms3
                    (w-stp-orc-wrk-ele)   to   w-tot-tot-gen-ms3 (1)  .
           add       w-stp-orc-cum-ms4
                    (w-stp-orc-wrk-ele)   to   w-tot-tot-gen-ms4 (1)  .
           add       w-stp-orc-cum-ms5
                    (w-stp-orc-wrk-ele)   to   w-tot-tot-gen-ms5 (1)  .
           add       w-stp-orc-cum-ms6
                    (w-stp-orc-wrk-ele)   to   w-tot-tot-gen-ms6 (1)  .
           add       w-stp-orc-cum-ms7
                    (w-stp-orc-wrk-ele)   to   w-tot-tot-gen-ms7 (1)  .
           add       w-stp-orc-cum-ms8
                    (w-stp-orc-wrk-ele)   to   w-tot-tot-gen-ms8 (1)  .
           add       w-stp-orc-cum-ms9
                    (w-stp-orc-wrk-ele)   to   w-tot-tot-gen-ms9 (1)  .
           add       w-stp-orc-cum-suc
                    (w-stp-orc-wrk-ele)   to   w-tot-tot-gen-suc (1)  .
       trt-fin-trc-orc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     trt-fin-trc-orc-999.
       trt-fin-trc-orc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totali da righe corpo documento            *
      *    *-----------------------------------------------------------*
       det-tri-orc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione record [ocr]                    *
      *              *-------------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *              *-------------------------------------------------*
      *              * Lettura riga in corso di trattamento            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      w-roc-num-prt        to   rf-ocr-num-prt         .
           move      w-roc-key-prg 
                    (w-roc-ctr-ele)       to   rf-ocr-num-prg         .
           move      "pgm/orc/fls/ioc/obj/iofocr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-ocr                 .
      *                  *---------------------------------------------*
      *                  * Se record non trovato : uscita              *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-tri-orc-900.
       det-tri-orc-100.
      *              *-------------------------------------------------*
      *              * Cumulo riga documento                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Scomposizione tipo riga                     *
      *                  *---------------------------------------------*
           move      rf-ocr-tip-rig       to   w-ctl-tip-rig-tri      .
           perform   ctl-tip-rig-000      thru ctl-tip-rig-999        .
      *                  *---------------------------------------------*
      *                  * Determinazione della quantita' residua      *
      *                  *---------------------------------------------*
           move      "DT"                 to   d-qev-roc-tip-ope      .
           perform   det-qev-roc-cll-000  thru det-qev-roc-cll-999    .
      *                  *---------------------------------------------*
      *                  * Determinazione importo in riga              *
      *                  *---------------------------------------------*
           move      d-qev-roc-qta-dev    to   w-det-imp-rig-qta      .
           perform   det-imp-rig-orc-000  thru det-imp-rig-orc-999    .
      *                  *---------------------------------------------*
      *                  * Trattamento prezzo di vendita e importo     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da effettuare                   *
      *                      *-----------------------------------------*
           if        w-ctl-tip-rig-tpr    =    "C" or
                     w-ctl-tip-rig-tpr    =    "A"
                     go to det-tri-orc-600.
      *                      *-----------------------------------------*
      *                      * Determinazione prezzo di vendita nella  *
      *                      * valuta base                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da effettuare               *
      *                          *-------------------------------------*
           if        rf-ocr-sgl-vpf       =    c-sgl
                     go to det-tri-orc-205.
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
           move      rf-ocr-sgl-vpp       to   w-coe-cmb-vlt-sdv      .
           move      rf-ocr-tdc-vpp       to   w-coe-cmb-vlt-tdc      .
           move      rf-ocr-dat-doc       to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se esito determinazione negati- *
      *                              * vo : bufferizzazione coeffi-    *
      *                              * ciente di cambio contenuto nel  *
      *                              * record [osr]                    *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-ocr-cdc-vpp to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Conversione da valuta per il    *
      *                              * prezzo a valuta base            *
      *                              *---------------------------------*
           move      rf-ocr-sgl-vpp       to   w-cvs-vlt-sgl          .
           move      rf-ocr-dec-vpp       to   w-cvs-vlt-dec          .
           move      rf-ocr-tdc-vpp       to   w-cvs-vlt-tdc          .
           move      w-coe-cmb-vlt-cdc    to   w-cvs-vlt-cdc          .
           move      rf-ocr-prz-ven       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                              *---------------------------------*
      *                              * Bufferizzazione nuovo prezzo    *
      *                              *---------------------------------*
           move      w-cvs-vlt-avb        to   rf-ocr-prz-ven         .
      *                              *---------------------------------*
      *                              * Determinazione prezzo netto     *
      *                              *---------------------------------*
           move      rf-ocr-prz-ven       to   w-cal-prz-net-prz      .
           move      rf-ocr-per-scr (1)   to   w-cal-prz-net-psc (1)  .
           move      rf-ocr-per-scr (2)   to   w-cal-prz-net-psc (2)  .
           move      rf-ocr-per-scr (3)   to   w-cal-prz-net-psc (3)  .
           move      rf-ocr-per-scr (4)   to   w-cal-prz-net-psc (4)  .
           move      rf-ocr-per-scr (5)   to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   rf-ocr-prz-net         .
      *                              *---------------------------------*
      *                              * Determinazione importo in riga  *
      *                              *---------------------------------*
           move      d-qev-roc-qta-dev    to   w-det-imp-rig-qta      .
           perform   det-imp-rig-orc-000  thru det-imp-rig-orc-999    .
      *                          *-------------------------------------*
      *                          * Bufferizzazione valuta per fattura- *
      *                          * zione in valuta per il prezzo       *
      *                          *-------------------------------------*
           move      rf-ocr-sgl-vpf       to   rf-ocr-sgl-vpp         .
           move      rf-ocr-dec-vpf       to   rf-ocr-dec-vpp         .
           move      rf-ocr-tdc-vpf       to   rf-ocr-tdc-vpp         .
           move      rf-ocr-cdc-vpf       to   rf-ocr-cdc-vpp         .
       det-tri-orc-205.
      *                      *-----------------------------------------*
      *                      * Determinazione prezzo di vendita con    *
      *                      * legame valutario                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da effettuare               *
      *                          *-------------------------------------*
           if        rf-ocr-sgl-vpl       =    spaces
                     go to det-tri-orc-600.
      *                          *-------------------------------------*
      *                          * Determinazione coefficiente di cam- *
      *                          * bio effettivo per il legame valuta- *
      *                          * rio alla data del documento         *
      *                          *-------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-ocr-sgl-vpl       to   w-coe-cmb-vlt-sdv      .
           move      rf-ocr-tdc-vpl       to   w-coe-cmb-vlt-tdc      .
           move      rf-ocr-dat-doc       to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se esito determinazione negati- *
      *                              * vo : bufferizzazione coeffi-    *
      *                              * ciente di cambio contenuto nel  *
      *                              * record [osr]                    *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-ocr-cdc-vpl to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Bufferizzazione coefficiente di *
      *                              * cambio determinato              *
      *                              *---------------------------------*
           move      w-coe-cmb-vlt-cdc    to   rf-ocr-cdc-vpl         .
      *                              *---------------------------------*
      *                              * Salvataggio prezzo di vendita   *
      *                              * precedente                      *
      *                              *---------------------------------*
           move      rf-ocr-prz-ven       to   w-sav-prz-ven          .
      *                              *---------------------------------*
      *                              * Applicazione cambio per legame  *
      *                              * valutario                       *
      *                              *---------------------------------*
           move      rf-ocr-prz-net       to   w-lvl-prz-prz          .
           move      rf-ocr-sgl-vpl       to   w-lvl-prz-vlt          .
           move      rf-ocr-tdc-vpl       to   w-lvl-prz-tdc          .
           move      rf-ocr-ccr-vpl       to   w-lvl-prz-ccr          .
           move      rf-ocr-cdc-vpl       to   w-lvl-prz-cdc          .
           move      rf-ocr-plm-vpl       to   w-lvl-prz-plm          .
           move      rf-ocr-tlm-vpl       to   w-lvl-prz-tlm          .
           perform   lvl-prz-det-000      thru lvl-prz-det-999        .
           move      w-lvl-prz-prz        to   rf-ocr-prz-ven         .
      *                              *---------------------------------*
      *                              * Se prezzo di vendita invariato: *
      *                              * oltre                           *
      *                              *---------------------------------*
           if        rf-ocr-prz-ven       =    w-sav-prz-ven
                     go to det-tri-orc-600.
      *                              *---------------------------------*
      *                              * Determinazione prezzo netto     *
      *                              *---------------------------------*
           move      rf-ocr-prz-ven       to   w-cal-prz-net-prz      .
           move      rf-ocr-per-scr (1)   to   w-cal-prz-net-psc (1)  .
           move      rf-ocr-per-scr (2)   to   w-cal-prz-net-psc (2)  .
           move      rf-ocr-per-scr (3)   to   w-cal-prz-net-psc (3)  .
           move      rf-ocr-per-scr (4)   to   w-cal-prz-net-psc (4)  .
           move      rf-ocr-per-scr (5)   to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   rf-ocr-prz-net         .
      *                              *---------------------------------*
      *                              * Determinazione importo in riga  *
      *                              *---------------------------------*
           move      d-qev-roc-qta-dev    to   w-det-imp-rig-qta      .
           perform   det-imp-rig-orc-000  thru det-imp-rig-orc-999    .
       det-tri-orc-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento totalizzatori per tipo prodotto   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se cliente con lettera d'intenti       *
      *                  * attiva                                      *
      *                  *---------------------------------------------*
           if        rf-lic-num-lic       =    spaces
                     go to det-tri-orc-700.
           if        rf-cli-cod-iva       =    zero
                     go to det-tri-orc-700.
           if        rf-lic-drf-ini       =    zero  or
                     rf-lic-drf-fin       =    zero
                     go to det-tri-orc-700.
      *                  *---------------------------------------------*
      *                  * Data di sistema                             *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        s-dat                <    rf-lic-drf-ini or
                     s-dat                >    rf-lic-drf-fin
                     go to det-tri-orc-700.
      *                  *---------------------------------------------*
      *                  * Forzatura codice Iva                        *
      *                  *---------------------------------------------*
           move      rf-cli-cod-iva       to   rf-ocr-cod-iva         .
       det-tri-orc-700.
      *                  *---------------------------------------------*
      *                  * Se riga di omaggio : no totalizzazione      *
      *                  *---------------------------------------------*
           move      rf-ocr-cod-iva       to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    =    9
                     go to det-tri-orc-720.
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           if        rf-ocr-tip-pro       not  = zero
                     add   rf-ocr-imp-rig to   w-tot-tot-rig
                                              (rf-ocr-tip-pro)
           else      add   rf-ocr-imp-rig to   w-tot-tot-rta          .
       det-tri-orc-720.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      rf-ocr-cod-iva       to   w-agg-cst-iva-coi      .
           move      rf-ocr-imp-rig       to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
       det-tri-orc-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-tri-orc-999.
       det-tri-orc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totali ordine                              *
      *    *-----------------------------------------------------------*
       det-tot-orc-000.
      *              *-------------------------------------------------*
      *              * Determinazione totale lordo                     *
      *              *-------------------------------------------------*
           perform   det-tot-lor-000      thru det-tot-lor-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale sconto in chiusura        *
      *              *-------------------------------------------------*
           move      rf-oct-tot-scc       to   w-det-tot-scc-tot      .
           move      rf-oct-per-scc       to   w-det-tot-scc-per      .
           perform   det-tot-scc-000      thru det-tot-scc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale al netto dello sconto in  *
      *              * chiusura                                        *
      *              *-------------------------------------------------*
           perform   det-tot-nsc-000      thru det-tot-nsc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione importi spese                    *
      *              *-------------------------------------------------*
           perform   det-imp-spe-orc-000  thru det-imp-spe-orc-999    .
      *              *-------------------------------------------------*
      *              * Determinazione totale al netto delle spese in   *
      *              * fattura                                         *
      *              *-------------------------------------------------*
           perform   det-tot-nsf-orc-000  thru det-tot-nsf-orc-999    .
      *              *-------------------------------------------------*
      *              * Determinazione totale sconto pagamento          *
      *              *-------------------------------------------------*
           move      rf-oct-tot-scp       to   w-det-tot-scp-tot      .
           move      rf-oct-per-scp       to   w-det-tot-scp-per      .
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
           move      rf-oct-cod-fop       to   w-clc-tbl-scd-fdp      .
           move      w-roc-rot-prv        to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
           move      rf-oct-pag-qaf       to   w-clc-tbl-scd-qaf      .
           move      rf-oct-pag-act       to   w-clc-tbl-scd-acc      .
           move      rf-oct-pag-dsm       to   w-clc-tbl-scd-ddp      .
           move      rf-oct-cod-arc       to   w-det-sca-doc-cli      .
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
           move      rf-oct-cod-fop       to   w-clc-tbl-scd-fdp      .
           move      w-roc-rot-prv        to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
           move      rf-oct-pag-qaf       to   w-clc-tbl-scd-qaf      .
           move      rf-oct-pag-act       to   w-clc-tbl-scd-acc      .
           move      rf-oct-pag-dsm       to   w-clc-tbl-scd-ddp      .
           move      rf-oct-cod-arc       to   w-det-sca-doc-cli      .
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
           move      rf-oct-cod-fop       to   w-clc-tbl-scd-fdp      .
           move      w-roc-rot-prv        to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
           move      rf-oct-pag-qaf       to   w-clc-tbl-scd-qaf      .
           move      rf-oct-pag-act       to   w-clc-tbl-scd-acc      .
           move      rf-oct-pag-dsm       to   w-clc-tbl-scd-ddp      .
           move      rf-oct-cod-arc       to   w-det-sca-doc-cli      .
           perform   det-sca-doc-000      thru det-sca-doc-999        .
      *              *-------------------------------------------------*
      *              * Compattamento castelletto iva                   *
      *              *-------------------------------------------------*
           perform   cmp-cst-iva-000      thru cmp-cst-iva-999        .
       det-tot-orc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totali ordine di spedizione                *
      *    *-----------------------------------------------------------*
       det-tot-ods-000.
      *              *-------------------------------------------------*
      *              * Determinazione totale lordo                     *
      *              *-------------------------------------------------*
           perform   det-tot-lor-000      thru det-tot-lor-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale sconto in chiusura        *
      *              *-------------------------------------------------*
           move      rf-ost-tot-scc       to   w-det-tot-scc-tot      .
           move      rf-ost-per-scc       to   w-det-tot-scc-per      .
           perform   det-tot-scc-000      thru det-tot-scc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale al netto dello sconto in  *
      *              * chiusura                                        *
      *              *-------------------------------------------------*
           perform   det-tot-nsc-000      thru det-tot-nsc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione importi spese                    *
      *              *-------------------------------------------------*
           perform   det-imp-spe-orc-000  thru det-imp-spe-orc-999    .
      *              *-------------------------------------------------*
      *              * Determinazione totale al netto delle spese in   *
      *              * fattura                                         *
      *              *-------------------------------------------------*
           perform   det-tot-nsf-orc-000  thru det-tot-nsf-orc-999    .
      *              *-------------------------------------------------*
      *              * Determinazione totale sconto pagamento          *
      *              *-------------------------------------------------*
           move      rf-ost-tot-scp       to   w-det-tot-scp-tot      .
           move      rf-ost-per-scp       to   w-det-tot-scp-per      .
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
           move      rf-ost-cod-fop       to   w-clc-tbl-scd-fdp      .
           move      rf-ost-dat-doc       to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
           move      rf-ost-pag-qaf       to   w-clc-tbl-scd-qaf      .
           move      rf-ost-pag-act       to   w-clc-tbl-scd-acc      .
           move      rf-ost-pag-dsm       to   w-clc-tbl-scd-ddp      .
           move      rf-ost-cod-arc       to   w-det-sca-doc-cli      .
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
           move      rf-ost-cod-fop       to   w-clc-tbl-scd-fdp      .
           move      rf-ost-dat-doc       to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
           move      rf-ost-pag-qaf       to   w-clc-tbl-scd-qaf      .
           move      rf-ost-pag-act       to   w-clc-tbl-scd-acc      .
           move      rf-ost-pag-dsm       to   w-clc-tbl-scd-ddp      .
           move      rf-ost-cod-arc       to   w-det-sca-doc-cli      .
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
           move      rf-ost-cod-fop       to   w-clc-tbl-scd-fdp      .
           move      rf-ost-dat-doc       to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
           move      rf-ost-pag-qaf       to   w-clc-tbl-scd-qaf      .
           move      rf-ost-pag-act       to   w-clc-tbl-scd-acc      .
           move      rf-ost-pag-dsm       to   w-clc-tbl-scd-ddp      .
           move      rf-ost-cod-arc       to   w-det-sca-doc-cli      .
           perform   det-sca-doc-000      thru det-sca-doc-999        .
      *              *-------------------------------------------------*
      *              * Compattamento castelletto iva                   *
      *              *-------------------------------------------------*
           perform   cmp-cst-iva-000      thru cmp-cst-iva-999        .
       det-tot-ods-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totali da righe corpo documento            *
      *    *-----------------------------------------------------------*
       det-tri-ods-000.
      *              *-------------------------------------------------*
      *              * Start su righe documento [osr]                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      rf-ost-num-prt       to   rf-osr-num-prt         .
           move      zero                 to   rf-osr-num-prg         .
           move      "pgm/ods/fls/ioc/obj/iofosr"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-osr                 .
           if        f-sts                =    e-not-err
                     go to det-tri-ods-010.
      *                  *---------------------------------------------*
      *                  * Se Start non ottenuta : ad uscita           *
      *                  *---------------------------------------------*
           go to     det-tri-ods-900.
       det-tri-ods-010.
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
                     go to det-tri-ods-900.
       det-tri-ods-060.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-osr-num-prt       =    rf-ost-num-prt
                     go to det-tri-ods-080.
      *                      *-----------------------------------------*
      *                      * A uscita                                *
      *                      *-----------------------------------------*
           go to     det-tri-ods-900.
       det-tri-ods-080.
      *                  *---------------------------------------------*
      *                  * Scomposizione tipo riga                     *
      *                  *---------------------------------------------*
           move      rf-osr-tip-rig       to   w-ctl-tip-rig-tri      .
           perform   ctl-tip-rig-000      thru ctl-tip-rig-999        .
      *                  *---------------------------------------------*
      *                  * Se tipo riga 'C' : riciclo                  *
      *                  *---------------------------------------------*
           if        w-ctl-tip-rig-tpr    not  = "C"
                     go to det-tri-ods-100.
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     det-tri-ods-010.
       det-tri-ods-100.
      *              *-------------------------------------------------*
      *              * Cumulo riga documento                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione della quantita' residua      *
      *                  *---------------------------------------------*
           move      "DT"                 to   d-qds-ros-tip-ope      .
           perform   det-qds-ros-cll-000  thru det-qds-ros-cll-999    .
      *                  *---------------------------------------------*
      *                  * Considerazioni su quantita' residua         *
      *                  *---------------------------------------------*
           if        d-qds-ros-qta-dsp    =    zero
                     go to det-tri-ods-800.
      *                  *---------------------------------------------*
      *                  * Determinazione importo in riga              *
      *                  *---------------------------------------------*
           move      d-qds-ros-qta-dsp    to   w-det-imp-rig-qta      .
           perform   det-imp-rig-ods-000  thru det-imp-rig-ods-999    .
      *                  *---------------------------------------------*
      *                  * Trattamento prezzo di vendita e importo     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da effettuare                   *
      *                      *-----------------------------------------*
           if        w-ctl-tip-rig-tpr    =    "C" or
                     w-ctl-tip-rig-tpr    =    "A"
                     go to det-tri-ods-600.
      *                      *-----------------------------------------*
      *                      * Determinazione prezzo di vendita nella  *
      *                      * valuta base                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da effettuare               *
      *                          *-------------------------------------*
           if        rf-osr-sgl-vpf       =    c-sgl
                     go to det-tri-ods-205.
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
           move      rf-osr-sgl-vpp       to   w-coe-cmb-vlt-sdv      .
           move      rf-osr-tdc-vpp       to   w-coe-cmb-vlt-tdc      .
           move      rf-osr-dat-doc       to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se esito determinazione negati- *
      *                              * vo : bufferizzazione coeffi-    *
      *                              * ciente di cambio contenuto nel  *
      *                              * record [osr]                    *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-osr-cdc-vpp to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Conversione da valuta per il    *
      *                              * prezzo a valuta base            *
      *                              *---------------------------------*
           move      rf-osr-sgl-vpp       to   w-cvs-vlt-sgl          .
           move      rf-osr-dec-vpp       to   w-cvs-vlt-dec          .
           move      rf-osr-tdc-vpp       to   w-cvs-vlt-tdc          .
           move      w-coe-cmb-vlt-cdc    to   w-cvs-vlt-cdc          .
           move      rf-osr-prz-ven       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                              *---------------------------------*
      *                              * Bufferizzazione nuovo prezzo    *
      *                              *---------------------------------*
           move      w-cvs-vlt-avb        to   rf-osr-prz-ven         .
      *                              *---------------------------------*
      *                              * Determinazione prezzo netto     *
      *                              *---------------------------------*
           move      rf-osr-prz-ven       to   w-cal-prz-net-prz      .
           move      rf-osr-per-scr (1)   to   w-cal-prz-net-psc (1)  .
           move      rf-osr-per-scr (2)   to   w-cal-prz-net-psc (2)  .
           move      rf-osr-per-scr (3)   to   w-cal-prz-net-psc (3)  .
           move      rf-osr-per-scr (4)   to   w-cal-prz-net-psc (4)  .
           move      rf-osr-per-scr (5)   to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   rf-osr-prz-net         .
      *                              *---------------------------------*
      *                              * Determinazione importo in riga  *
      *                              *---------------------------------*
           move      d-qds-ros-qta-dsp    to   w-det-imp-rig-qta      .
           perform   det-imp-rig-ods-000  thru det-imp-rig-ods-999    .
      *                          *-------------------------------------*
      *                          * Bufferizzazione valuta per fattura- *
      *                          * zione in valuta per il prezzo       *
      *                          *-------------------------------------*
           move      rf-osr-sgl-vpf       to   rf-osr-sgl-vpp         .
           move      rf-osr-dec-vpf       to   rf-osr-dec-vpp         .
           move      rf-osr-tdc-vpf       to   rf-osr-tdc-vpp         .
           move      rf-osr-cdc-vpf       to   rf-osr-cdc-vpp         .
       det-tri-ods-205.
      *                      *-----------------------------------------*
      *                      * Determinazione prezzo di vendita con    *
      *                      * legame valutario                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da effettuare               *
      *                          *-------------------------------------*
           if        rf-osr-sgl-vpl       =    spaces
                     go to det-tri-ods-600.
      *                          *-------------------------------------*
      *                          * Determinazione coefficiente di cam- *
      *                          * bio effettivo per il legame valuta- *
      *                          * rio alla data del documento         *
      *                          *-------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-osr-sgl-vpl       to   w-coe-cmb-vlt-sdv      .
           move      rf-osr-tdc-vpl       to   w-coe-cmb-vlt-tdc      .
           move      rf-osr-dat-doc       to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se esito determinazione negati- *
      *                              * vo : bufferizzazione coeffi-    *
      *                              * ciente di cambio contenuto nel  *
      *                              * record [osr]                    *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-osr-cdc-vpl to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Bufferizzazione coefficiente di *
      *                              * cambio determinato              *
      *                              *---------------------------------*
           move      w-coe-cmb-vlt-cdc    to   rf-osr-cdc-vpl         .
      *                              *---------------------------------*
      *                              * Salvataggio prezzo di vendita   *
      *                              * precedente                      *
      *                              *---------------------------------*
           move      rf-osr-prz-ven       to   w-sav-prz-ven          .
      *                              *---------------------------------*
      *                              * Applicazione cambio per legame  *
      *                              * valutario                       *
      *                              *---------------------------------*
           move      rf-osr-prz-net       to   w-lvl-prz-prz          .
           move      rf-osr-sgl-vpl       to   w-lvl-prz-vlt          .
           move      rf-osr-tdc-vpl       to   w-lvl-prz-tdc          .
           move      rf-osr-ccr-vpl       to   w-lvl-prz-ccr          .
           move      rf-osr-cdc-vpl       to   w-lvl-prz-cdc          .
           move      rf-osr-plm-vpl       to   w-lvl-prz-plm          .
           move      rf-osr-tlm-vpl       to   w-lvl-prz-tlm          .
           perform   lvl-prz-det-000      thru lvl-prz-det-999        .
           move      w-lvl-prz-prz        to   rf-osr-prz-ven         .
      *                              *---------------------------------*
      *                              * Se prezzo di vendita invariato: *
      *                              * oltre                           *
      *                              *---------------------------------*
           if        rf-osr-prz-ven       =    w-sav-prz-ven
                     go to det-tri-ods-600.
      *                              *---------------------------------*
      *                              * Determinazione prezzo netto     *
      *                              *---------------------------------*
           move      rf-osr-prz-ven       to   w-cal-prz-net-prz      .
           move      rf-osr-per-scr (1)   to   w-cal-prz-net-psc (1)  .
           move      rf-osr-per-scr (2)   to   w-cal-prz-net-psc (2)  .
           move      rf-osr-per-scr (3)   to   w-cal-prz-net-psc (3)  .
           move      rf-osr-per-scr (4)   to   w-cal-prz-net-psc (4)  .
           move      rf-osr-per-scr (5)   to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   rf-osr-prz-net         .
      *                              *---------------------------------*
      *                              * Determinazione importo in riga  *
      *                              *---------------------------------*
           move      d-qds-ros-qta-dsp    to   w-det-imp-rig-qta      .
           perform   det-imp-rig-ods-000  thru det-imp-rig-ods-999    .
       det-tri-ods-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento totalizzatori per tipo prodotto   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se cliente con lettera d'intenti       *
      *                  * attiva                                      *
      *                  *---------------------------------------------*
           if        rf-lic-num-lic       =    spaces
                     go to det-tri-ods-700.
           if        rf-cli-cod-iva       =    zero
                     go to det-tri-ods-700.
           if        rf-lic-drf-ini       =    zero  or
                     rf-lic-drf-fin       =    zero
                     go to det-tri-ods-700.
      *                  *---------------------------------------------*
      *                  * Data di sistema                             *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        s-dat                <    rf-lic-drf-ini or
                     s-dat                >    rf-lic-drf-fin
                     go to det-tri-ods-700.
      *                  *---------------------------------------------*
      *                  * Forzatura codice Iva                        *
      *                  *---------------------------------------------*
           move      rf-cli-cod-iva       to   rf-osr-cod-iva         .
       det-tri-ods-700.
      *                  *---------------------------------------------*
      *                  * Se riga di omaggio : no totalizzazione      *
      *                  *---------------------------------------------*
           move      rf-osr-cod-iva       to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    =    9
                     go to det-tri-ods-720.
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           if        rf-osr-tip-pro       not  = zero
                     add   rf-osr-imp-rig to   w-tot-tot-rig
                                              (rf-osr-tip-pro)
           else      add   rf-osr-imp-rig to   w-tot-tot-rta          .
       det-tri-ods-720.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      rf-osr-cod-iva       to   w-agg-cst-iva-coi      .
           move      rf-osr-imp-rig       to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
       det-tri-ods-800.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale file [osr]       *
      *              *-------------------------------------------------*
           go to     det-tri-ods-010.
       det-tri-ods-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-tri-ods-999.
       det-tri-ods-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione totali ordine di spedizione               *
      *    *-----------------------------------------------------------*
       ini-tot-ods-000.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Coefficiente di cambio va-  *
      *              * luta per fatturazione                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-ost-sgl-vpf       to   w-coe-cmb-vlt-sdv      .
           move      rf-ost-tdc-vpf       to   w-coe-cmb-vlt-tdc      .
           move      rf-ost-dat-doc       to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                  *---------------------------------------------*
      *                  * Se esito determinazione negativo : buffe-   *
      *                  * rizzazione coefficiente di cambio contenuto *
      *                  * nel record [ost]                            *
      *                  *---------------------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-ost-cdc-vpf to   w-coe-cmb-vlt-cdc      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione coefficiente di cambio      *
      *                  *---------------------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-tot-cdc-vpf          .
       ini-tot-ods-100.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Forma di pagamento          *
      *              *-------------------------------------------------*
           move      rf-ost-cod-fop       to   w-det-vas-fop-cod      .
           perform   det-vas-fop-000      thru det-vas-fop-999        .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Sconto in chiusura          *
      *              *-------------------------------------------------*
           move      w-ref-sco-chi-civ    to   w-tot-civ-scc          .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Sconto pagamento            *
      *              *-------------------------------------------------*
           move      w-ref-sco-pag-civ    to   w-tot-civ-scp          .
       ini-tot-ods-200.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Spese in fattura            *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-orc-wrk-ct1      .
       ini-tot-ods-220.
           add       1                    to   w-stp-orc-wrk-ct1      .
           if        w-stp-orc-wrk-ct1    >    6
                     go to ini-tot-ods-300.
           move      w-prs-spe-fat-civ
                    (w-stp-orc-wrk-ct1)   to   w-tot-spe-civ
                                              (w-stp-orc-wrk-ct1)     .
           go to     ini-tot-ods-220.
       ini-tot-ods-300.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Spese incasso               *
      *              *-------------------------------------------------*
           move      rf-ost-add-spi       to   w-det-vas-spi-cod      .
           perform   det-vas-spi-000      thru det-vas-spi-999        .
           move      zero                 to   w-tot-tot-sic          .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Spese bollo                 *
      *              *-------------------------------------------------*
           move      rf-ost-add-spb       to   w-det-vas-spb-cod      .
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
       ini-tot-ods-400.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Castelletto iva             *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-iva-ele          .
           move      zero                 to   w-stp-orc-wrk-ct1      .
       ini-tot-ods-420.
           add       1                    to   w-stp-orc-wrk-ct1      .
           if        w-stp-orc-wrk-ct1    >    12
                     go to ini-tot-ods-500.
           move      zero                 to   w-tot-iva-cod
                                              (w-stp-orc-wrk-ct1)     .
           move      zero                 to   w-tot-iva-ibl
                                              (w-stp-orc-wrk-ct1)     .
           move      zero                 to   w-tot-iva-imp
                                              (w-stp-orc-wrk-ct1)     .
           go to     ini-tot-ods-420.
       ini-tot-ods-500.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Castelletto scadenze        *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-scd-ele          .
           move      zero                 to   w-stp-orc-wrk-ct1      .
       ini-tot-ods-520.
           add       1                    to   w-stp-orc-wrk-ct1      .
           if        w-stp-orc-wrk-ct1    >    96
                     go to ini-tot-ods-999.
           move      zero                 to   w-tot-scd-tip
                                              (w-stp-orc-wrk-ct1)     .
           move      zero                 to   w-tot-scd-dat
                                              (w-stp-orc-wrk-ct1)     .
           move      zero                 to   w-tot-scd-cau
                                              (w-stp-orc-wrk-ct1)     .
           move      zero                 to   w-tot-scd-imp
                                              (w-stp-orc-wrk-ct1)     .
           go to     ini-tot-ods-520.
       ini-tot-ods-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totali bolla                               *
      *    *-----------------------------------------------------------*
       det-tot-bol-000.
      *              *-------------------------------------------------*
      *              * Determinazione totale lordo                     *
      *              *-------------------------------------------------*
           perform   det-tot-lor-000      thru det-tot-lor-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale sconto in chiusura        *
      *              *-------------------------------------------------*
           move      rf-bit-tot-scc       to   w-det-tot-scc-tot      .
           move      rf-bit-per-scc       to   w-det-tot-scc-per      .
           perform   det-tot-scc-000      thru det-tot-scc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione totale al netto dello sconto in  *
      *              * chiusura                                        *
      *              *-------------------------------------------------*
           perform   det-tot-nsc-000      thru det-tot-nsc-999        .
      *              *-------------------------------------------------*
      *              * Determinazione importi spese                    *
      *              *-------------------------------------------------*
           perform   det-imp-spe-bol-000  thru det-imp-spe-bol-999    .
      *              *-------------------------------------------------*
      *              * Determinazione totale al netto delle spese in   *
      *              * fattura                                         *
      *              *-------------------------------------------------*
           perform   det-tot-nsf-bol-000  thru det-tot-nsf-bol-999    .
      *              *-------------------------------------------------*
      *              * Determinazione totale sconto pagamento          *
      *              *-------------------------------------------------*
           move      rf-bit-tot-scp       to   w-det-tot-scp-tot      .
           move      rf-bit-per-scp       to   w-det-tot-scp-per      .
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
           move      rf-bit-cod-fop       to   w-clc-tbl-scd-fdp      .
           move      rf-bit-dat-doc       to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
           move      rf-bit-pag-qaf       to   w-clc-tbl-scd-qaf      .
           move      rf-bit-pag-act       to   w-clc-tbl-scd-acc      .
           move      rf-bit-pag-dsm       to   w-clc-tbl-scd-ddp      .
           move      rf-bit-cod-arc       to   w-det-sca-doc-cli      .
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
           move      rf-bit-cod-fop       to   w-clc-tbl-scd-fdp      .
           move      rf-bit-dat-doc       to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
           move      rf-bit-pag-qaf       to   w-clc-tbl-scd-qaf      .
           move      rf-bit-pag-act       to   w-clc-tbl-scd-acc      .
           move      rf-bit-pag-dsm       to   w-clc-tbl-scd-ddp      .
           move      rf-bit-cod-arc       to   w-det-sca-doc-cli      .
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
           move      rf-bit-cod-fop       to   w-clc-tbl-scd-fdp      .
           move      rf-bit-dat-doc       to   w-clc-tbl-scd-ddo      .
           move      w-tot-tot-ibl        to   w-clc-tbl-scd-ibl      .
           move      w-tot-tot-imp        to   w-clc-tbl-scd-iva      .
           move      rf-bit-pag-qaf       to   w-clc-tbl-scd-qaf      .
           move      rf-bit-pag-act       to   w-clc-tbl-scd-acc      .
           move      rf-bit-pag-dsm       to   w-clc-tbl-scd-ddp      .
           move      rf-bit-cod-arc       to   w-det-sca-doc-cli      .
           perform   det-sca-doc-000      thru det-sca-doc-999        .
      *              *-------------------------------------------------*
      *              * Compattamento castelletto iva                   *
      *              *-------------------------------------------------*
           perform   cmp-cst-iva-000      thru cmp-cst-iva-999        .
       det-tot-bol-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totali da righe corpo documento            *
      *    *-----------------------------------------------------------*
       det-tri-bol-000.
      *              *-------------------------------------------------*
      *              * Start su righe documento [bir]                  *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NUMPRT    "         to   f-key                  .
           move      "NL"                 to   f-cfr                  .
           move      rf-bit-num-prt       to   rf-bir-num-prt         .
           move      zero                 to   rf-bir-num-prg         .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
           if        f-sts                =    e-not-err
                     go to det-tri-bol-010.
      *                  *---------------------------------------------*
      *                  * Se Start non ottenuta : ad uscita           *
      *                  *---------------------------------------------*
           go to     det-tri-bol-900.
       det-tri-bol-010.
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
      *                  * Test se 'at end'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-tri-bol-900.
       det-tri-bol-060.
      *                  *---------------------------------------------*
      *                  * Test sul massimo                            *
      *                  *---------------------------------------------*
           if        rf-bir-num-prt       =    rf-bit-num-prt
                     go to det-tri-bol-080.
      *                      *-----------------------------------------*
      *                      * A uscita                                *
      *                      *-----------------------------------------*
           go to     det-tri-bol-900.
       det-tri-bol-080.
      *                  *---------------------------------------------*
      *                  * Scomposizione tipo riga                     *
      *                  *---------------------------------------------*
           move      rf-bir-tip-rig       to   w-ctl-tip-rig-tri      .
           perform   ctl-tip-rig-000      thru ctl-tip-rig-999        .
      *                  *---------------------------------------------*
      *                  * Se tipo riga 'C' : riciclo                  *
      *                  *---------------------------------------------*
           if        w-ctl-tip-rig-tpr    not  = "C"
                     go to det-tri-bol-100.
      *                      *-----------------------------------------*
      *                      * Riciclo                                 *
      *                      *-----------------------------------------*
           go to     det-tri-bol-010.
       det-tri-bol-100.
      *              *-------------------------------------------------*
      *              * Cumulo riga documento                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se riga bolla gia' fatturata           *
      *                  *---------------------------------------------*
           if        rf-bir-fat-num       not  = zero or
                     rf-bir-fat-dat       not  = zero
                     go to det-tri-bol-010.
       det-tri-bol-200.
      *                  *---------------------------------------------*
      *                  * Determinazione importo in riga              *
      *                  *---------------------------------------------*
           perform   det-imp-rig-bol-000  thru det-imp-rig-bol-999    .
      *                  *---------------------------------------------*
      *                  * Trattamento prezzo di vendita e importo     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test se da effettuare                   *
      *                      *-----------------------------------------*
           if        w-ctl-tip-rig-tpr    =    "C" or
                     w-ctl-tip-rig-tpr    =    "A"
                     go to det-tri-bol-600.
      *                      *-----------------------------------------*
      *                      * Determinazione prezzo di vendita nella  *
      *                      * valuta base                             *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da effettuare               *
      *                          *-------------------------------------*
           if        rf-bir-sgl-vpf       =    c-sgl
                     go to det-tri-bol-300.
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
           move      rf-bir-sgl-vpp       to   w-coe-cmb-vlt-sdv      .
           move      rf-bir-tdc-vpp       to   w-coe-cmb-vlt-tdc      .
           move      rf-bir-dat-doc       to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se esito determinazione negati- *
      *                              * vo : bufferizzazione coeffi-    *
      *                              * ciente di cambio contenuto nel  *
      *                              * record [osr]                    *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-bir-cdc-vpp to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Conversione da valuta per il    *
      *                              * prezzo a valuta base            *
      *                              *---------------------------------*
           move      rf-bir-sgl-vpp       to   w-cvs-vlt-sgl          .
           move      rf-bir-dec-vpp       to   w-cvs-vlt-dec          .
           move      rf-bir-tdc-vpp       to   w-cvs-vlt-tdc          .
           move      w-coe-cmb-vlt-cdc    to   w-cvs-vlt-cdc          .
           move      rf-bir-prz-ven       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                              *---------------------------------*
      *                              * Bufferizzazione nuovo prezzo    *
      *                              *---------------------------------*
           move      w-cvs-vlt-avb        to   rf-bir-prz-ven         .
      *                              *---------------------------------*
      *                              * Determinazione prezzo netto     *
      *                              *---------------------------------*
           move      rf-bir-prz-ven       to   w-cal-prz-net-prz      .
           move      rf-bir-per-scr (1)   to   w-cal-prz-net-psc (1)  .
           move      rf-bir-per-scr (2)   to   w-cal-prz-net-psc (2)  .
           move      rf-bir-per-scr (3)   to   w-cal-prz-net-psc (3)  .
           move      rf-bir-per-scr (4)   to   w-cal-prz-net-psc (4)  .
           move      rf-bir-per-scr (5)   to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   rf-bir-prz-net         .
      *                              *---------------------------------*
      *                              * Determinazione importo in riga  *
      *                              *---------------------------------*
           perform   det-imp-rig-bol-000  thru det-imp-rig-bol-999    .
      *                          *-------------------------------------*
      *                          * Bufferizzazione valuta per fattura- *
      *                          * zione in valuta per il prezzo       *
      *                          *-------------------------------------*
           move      rf-bir-sgl-vpf       to   rf-bir-sgl-vpp         .
           move      rf-bir-dec-vpf       to   rf-bir-dec-vpp         .
           move      rf-bir-tdc-vpf       to   rf-bir-tdc-vpp         .
           move      rf-bir-cdc-vpf       to   rf-bir-cdc-vpp         .
       det-tri-bol-300.
      *                      *-----------------------------------------*
      *                      * Determinazione prezzo di vendita con    *
      *                      * legame valutario                        *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da effettuare               *
      *                          *-------------------------------------*
           if        rf-bir-sgl-vpl       =    spaces
                     go to det-tri-bol-600.
      *                          *-------------------------------------*
      *                          * Determinazione coefficiente di cam- *
      *                          * bio effettivo per il legame valuta- *
      *                          * rio alla data del documento         *
      *                          *-------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-bir-sgl-vpl       to   w-coe-cmb-vlt-sdv      .
           move      rf-bir-tdc-vpl       to   w-coe-cmb-vlt-tdc      .
           move      rf-bir-dat-doc       to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                              *---------------------------------*
      *                              * Se esito determinazione negati- *
      *                              * vo : bufferizzazione coeffi-    *
      *                              * ciente di cambio contenuto nel  *
      *                              * record [osr]                    *
      *                              *---------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-bir-cdc-vpl to   w-coe-cmb-vlt-cdc      .
      *                              *---------------------------------*
      *                              * Bufferizzazione coefficiente di *
      *                              * cambio determinato              *
      *                              *---------------------------------*
           move      w-coe-cmb-vlt-cdc    to   rf-bir-cdc-vpl         .
      *                              *---------------------------------*
      *                              * Salvataggio prezzo di vendita   *
      *                              * precedente                      *
      *                              *---------------------------------*
           move      rf-bir-prz-ven       to   w-sav-prz-ven          .
      *                              *---------------------------------*
      *                              * Applicazione cambio per legame  *
      *                              * valutario                       *
      *                              *---------------------------------*
           move      rf-bir-prz-net       to   w-lvl-prz-prz          .
           move      rf-bir-sgl-vpl       to   w-lvl-prz-vlt          .
           move      rf-bir-tdc-vpl       to   w-lvl-prz-tdc          .
           move      rf-bir-ccr-vpl       to   w-lvl-prz-ccr          .
           move      rf-bir-cdc-vpl       to   w-lvl-prz-cdc          .
           move      rf-bir-plm-vpl       to   w-lvl-prz-plm          .
           move      rf-bir-tlm-vpl       to   w-lvl-prz-tlm          .
           perform   lvl-prz-det-000      thru lvl-prz-det-999        .
           move      w-lvl-prz-prz        to   rf-bir-prz-ven         .
      *                              *---------------------------------*
      *                              * Se prezzo di vendita invariato: *
      *                              * oltre                           *
      *                              *---------------------------------*
           if        rf-bir-prz-ven       =    w-sav-prz-ven
                     go to det-tri-bol-600.
      *                              *---------------------------------*
      *                              * Determinazione prezzo netto     *
      *                              *---------------------------------*
           move      rf-bir-prz-ven       to   w-cal-prz-net-prz      .
           move      rf-bir-per-scr (1)   to   w-cal-prz-net-psc (1)  .
           move      rf-bir-per-scr (2)   to   w-cal-prz-net-psc (2)  .
           move      rf-bir-per-scr (3)   to   w-cal-prz-net-psc (3)  .
           move      rf-bir-per-scr (4)   to   w-cal-prz-net-psc (4)  .
           move      rf-bir-per-scr (5)   to   w-cal-prz-net-psc (5)  .
           perform   cal-prz-net-000      thru cal-prz-net-999        .
           move      w-cal-prz-net-prz    to   rf-bir-prz-net         .
      *                              *---------------------------------*
      *                              * Determinazione importo in riga  *
      *                              *---------------------------------*
           perform   det-imp-rig-bol-000  thru det-imp-rig-bol-999    .
       det-tri-bol-600.
      *              *-------------------------------------------------*
      *              * Aggiornamento totalizzatori per tipo prodotto   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se cliente con lettera d'intenti       *
      *                  * attiva                                      *
      *                  *---------------------------------------------*
           if        rf-lic-num-lic       =    spaces
                     go to det-tri-bol-700.
           if        rf-cli-cod-iva       =    zero
                     go to det-tri-bol-700.
           if        rf-lic-drf-ini       =    zero  or
                     rf-lic-drf-fin       =    zero
                     go to det-tri-bol-700.
      *                  *---------------------------------------------*
      *                  * Data di sistema                             *
      *                  *---------------------------------------------*
           move      "DT"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        s-dat                <    rf-lic-drf-ini or
                     s-dat                >    rf-lic-drf-fin
                     go to det-tri-bol-700.
      *                  *---------------------------------------------*
      *                  * Forzatura codice Iva                        *
      *                  *---------------------------------------------*
           move      rf-cli-cod-iva       to   rf-bir-cod-iva         .
       det-tri-bol-700.
      *                  *---------------------------------------------*
      *                  * Se riga di omaggio : no totalizzazione      *
      *                  *---------------------------------------------*
           move      rf-bir-cod-iva       to   w-edt-iva-cod          .
      *
           if        w-edt-iva-cod-003    =    9
                     go to det-tri-bol-720.
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           if        rf-bir-tip-pro       not  = zero
                     add   rf-bir-imp-rig to   w-tot-tot-rig
                                              (rf-bir-tip-pro)
           else      add   rf-bir-imp-rig to   w-tot-tot-rta          .
       det-tri-bol-720.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      rf-bir-cod-iva       to   w-agg-cst-iva-coi      .
           move      rf-bir-imp-rig       to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
       det-tri-bol-800.
      *              *-------------------------------------------------*
      *              * Riciclo su lettura sequenziale file [osr]       *
      *              *-------------------------------------------------*
           go to     det-tri-bol-010.
       det-tri-bol-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     det-tri-bol-999.
       det-tri-bol-999.
           exit.

      *    *===========================================================*
      *    * Normalizzazione totali bolla                              *
      *    *-----------------------------------------------------------*
       ini-tot-bol-000.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Coefficiente di cambio va-  *
      *              * luta per fatturazione                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Determinazione                              *
      *                  *---------------------------------------------*
           move      "CC"                 to   w-coe-cmb-vlt-ope      .
           move      rf-bit-sgl-vpf       to   w-coe-cmb-vlt-sdv      .
           move      rf-bit-tdc-vpf       to   w-coe-cmb-vlt-tdc      .
           move      rf-bit-dat-doc       to   w-coe-cmb-vlt-drc      .
           move      00                   to   w-coe-cmb-vlt-quc      .
           perform   coe-cmb-vlt-cll-000  thru coe-cmb-vlt-cll-999    .
      *                  *---------------------------------------------*
      *                  * Se esito determinazione negativo : buffe-   *
      *                  * rizzazione coefficiente di cambio contenuto *
      *                  * nel record [ost]                            *
      *                  *---------------------------------------------*
           if        w-coe-cmb-vlt-ope    not  = "CC"
                     move  rf-bit-cdc-vpf to   w-coe-cmb-vlt-cdc      .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione coefficiente di cambio      *
      *                  *---------------------------------------------*
           move      w-coe-cmb-vlt-cdc    to   w-tot-cdc-vpf          .
       ini-tot-bol-100.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Forma di pagamento          *
      *              *-------------------------------------------------*
           move      rf-bit-cod-fop       to   w-det-vas-fop-cod      .
           perform   det-vas-fop-000      thru det-vas-fop-999        .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Sconto in chiusura          *
      *              *-------------------------------------------------*
           move      w-ref-sco-chi-civ    to   w-tot-civ-scc          .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Sconto pagamento            *
      *              *-------------------------------------------------*
           move      w-ref-sco-pag-civ    to   w-tot-civ-scp          .
       ini-tot-bol-200.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Spese in fattura            *
      *              *-------------------------------------------------*
           move      zero                 to   w-stp-bol-wrk-ct1      .
       ini-tot-bol-220.
           add       1                    to   w-stp-bol-wrk-ct1      .
           if        w-stp-bol-wrk-ct1    >    6
                     go to ini-tot-bol-300.
           move      w-prs-spe-fat-civ
                    (w-stp-bol-wrk-ct1)   to   w-tot-spe-civ
                                              (w-stp-bol-wrk-ct1)     .
           go to     ini-tot-bol-220.
       ini-tot-bol-300.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Spese incasso               *
      *              *-------------------------------------------------*
           move      rf-bit-add-spi       to   w-det-vas-spi-cod      .
           perform   det-vas-spi-000      thru det-vas-spi-999        .
           move      zero                 to   w-tot-tot-sic          .
      *              *-------------------------------------------------*
      *              * Valori relativi a : Spese bollo                 *
      *              *-------------------------------------------------*
           move      rf-bit-add-spb       to   w-det-vas-spb-cod      .
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
       ini-tot-bol-400.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Castelletto iva             *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-iva-ele          .
           move      zero                 to   w-stp-bol-wrk-ct1      .
       ini-tot-bol-420.
           add       1                    to   w-stp-bol-wrk-ct1      .
           if        w-stp-bol-wrk-ct1    >    12
                     go to ini-tot-bol-500.
           move      zero                 to   w-tot-iva-cod
                                              (w-stp-bol-wrk-ct1)     .
           move      zero                 to   w-tot-iva-ibl
                                              (w-stp-bol-wrk-ct1)     .
           move      zero                 to   w-tot-iva-imp
                                              (w-stp-bol-wrk-ct1)     .
           go to     ini-tot-bol-420.
       ini-tot-bol-500.
      *              *-------------------------------------------------*
      *              * Valori relativi a : Castelletto scadenze        *
      *              *-------------------------------------------------*
           move      zero                 to   w-tot-scd-ele          .
           move      zero                 to   w-stp-bol-wrk-ct1      .
       ini-tot-bol-520.
           add       1                    to   w-stp-bol-wrk-ct1      .
           if        w-stp-bol-wrk-ct1    >    96
                     go to ini-tot-bol-999.
           move      zero                 to   w-tot-scd-tip
                                              (w-stp-bol-wrk-ct1)     .
           move      zero                 to   w-tot-scd-dat
                                              (w-stp-bol-wrk-ct1)     .
           move      zero                 to   w-tot-scd-cau
                                              (w-stp-bol-wrk-ct1)     .
           move      zero                 to   w-tot-scd-imp
                                              (w-stp-bol-wrk-ct1)     .
           go to     ini-tot-bol-520.
       ini-tot-bol-999.
           exit.

      *    *===========================================================*
      *    * Determinazione status bolle da fatturare                  *
      *    *-----------------------------------------------------------*
       det-sts-bol-000.
      *              *-------------------------------------------------*
      *              * Normalizzazioni preliminari                     *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-sts-bol-flg      .
           move      zero                 to   w-det-sts-bol-ctr      .
           move      "P"                  to   w-det-sts-bol-idt      .
      *              *-------------------------------------------------*
      *              * Determinazione tipo incidenza del documento     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione record [zfi]                *
      *                  *---------------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *                  *---------------------------------------------*
      *                  * Lettura record [zfi]                        *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODTMO"             to   f-key                  .
           move      rf-bit-tmo-ftr       to   rf-zfi-cod-tmo         .
           move      "pgm/fat/fls/ioc/obj/iofzfi"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfi                 .
      *                      *-----------------------------------------*
      *                      * Test su esito lettura                   *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-sts-bol-900.
      *                  *---------------------------------------------*
      *                  * Test su tipo documento per la fatturazione  *
      *                  *---------------------------------------------*
           if        rf-zfi-tip-doc       =    04
                     go to det-sts-bol-900.
           if        rf-zfi-tip-doc       =    03
                     move  "N"            to   w-det-sts-bol-idt      .
       det-sts-bol-100.
      *              *-------------------------------------------------*
      *              * Start su file [bir]                             *
      *              *-------------------------------------------------*
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
      *                  * Test su esito operazione                    *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-sts-bol-900.
       det-sts-bol-200.
      *              *-------------------------------------------------*
      *              * Lettura sequenziale record [bir]                *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/bol/fls/ioc/obj/iofbir"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-bir                 .
      *                  *---------------------------------------------*
      *                  * Test se 'At End'                            *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-sts-bol-800.
       det-sts-bol-300.
      *              *-------------------------------------------------*
      *              * Test sul massimo                                *
      *              *-------------------------------------------------*
           if        rf-bir-num-prt       not  = rf-bit-num-prt
                     go to det-sts-bol-800.
       det-sts-bol-400.
      *              *-------------------------------------------------*
      *              * Selezioni sul record                            *
      *              *-------------------------------------------------*
       det-sts-bol-410.
      *                  *---------------------------------------------*
      *                  * Test se riga bolla gia' fatturata           *
      *                  *---------------------------------------------*
           if        rf-bir-fat-num       not  = zero or
                     rf-bir-fat-dat       not  = zero
                     go to det-sts-bol-200.
       det-sts-bol-500.
      *                  *---------------------------------------------*
      *                  * Incremento elementi utili                   *
      *                  *---------------------------------------------*
           add       1                    to   w-det-sts-bol-ctr      .
       det-sts-bol-600.
      *              *-------------------------------------------------*
      *              * Riciclo a riga bolla successiva                 *
      *              *-------------------------------------------------*
           go to     det-sts-bol-200.
       det-sts-bol-800.
      *              *-------------------------------------------------*
      *              * Test su numero elementi utili incontrati        *
      *              *-------------------------------------------------*
           if        w-det-sts-bol-ctr    =    zero
                     go to det-sts-bol-900.
      *              *-------------------------------------------------*
      *              * Uscita per determinazione superata              *
      *              *-------------------------------------------------*
           go to     det-sts-bol-999.
       det-sts-bol-900.
      *              *-------------------------------------------------*
      *              * Uscita per determinazione non superata          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita                              *
      *                  *---------------------------------------------*
           move      "#"                  to   w-det-sts-bol-flg      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-sts-bol-999.
       det-sts-bol-999.
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
      *                      * Determinazione in funizione delle moda- *
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
      *              * Lettura record [zfp]                            *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFOP"             to   f-key                  .
           move      w-det-vas-fop-cod    to   rf-zfp-cod-fop         .
           move      "pgm/dcc/fls/ioc/obj/iofzfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zfp                 .
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
           if        rf-zfp-cod-pag
                    (w-det-vas-fop-ctr)   =    zero
                     go to det-vas-fop-100.
      *                  *---------------------------------------------*
      *                  * Campi derivati da record [zfp]              *
      *                  *---------------------------------------------*
           move      rf-zfp-cod-pag
                    (w-det-vas-fop-ctr)   to   w-tot-cod-pag
                                              (w-det-vas-fop-ctr)     .
           move      rf-zfp-tip-amm
                    (w-det-vas-fop-ctr)   to   w-tot-tip-amm
                                              (w-det-vas-fop-ctr)     .
           move      rf-zfp-per-toi
                    (w-det-vas-fop-ctr)   to   w-tot-per-toi
                                              (w-det-vas-fop-ctr)     .
           move      rf-zfp-dim-act
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
      *              * Lettura record [zin] principale                 *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPI    "         to   f-key                  .
           move      w-det-vas-spi-cod    to   rf-zin-cod-spi         .
           move      zero                 to   rf-zin-tip-pag         .
           move      "pgm/dcc/fls/ioc/obj/iofzin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zin                 .
      *              *-------------------------------------------------*
      *              * Se record non esistente : uscita                *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-vas-spi-999.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zin-civ-spi       to   w-tot-civ-spi          .
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
      *                  * Lettura record [zin] relativo al tipo paga- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPI    "         to   f-key                  .
           move      w-det-vas-spi-cod    to   rf-zin-cod-spi         .
           move      w-tot-tip-pag
                    (w-det-vas-spi-c01)
                                          to   rf-zin-tip-pag         .
           move      "pgm/dcc/fls/ioc/obj/iofzin"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zin                 .
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
           move      rf-zin-tip-pag       to   w-tot-tpg-spi
                                              (w-tot-eit-spi)         .
           move      rf-zin-tfu-spi       to   w-tot-tfu-spi
                                              (w-tot-eit-spi)         .
           move      rf-zin-amm-spi       to   w-tot-amm-spi
                                              (w-tot-eit-spi)         .
           move      rf-zin-per-spi       to   w-tot-per-spi
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
      *              * Lettura record [zbo] principale                 *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPB    "         to   f-key                  .
           move      w-det-vas-spb-cod    to   rf-zbo-cod-spb         .
           move      zero                 to   rf-zbo-tip-pag         .
           move      "pgm/dcc/fls/ioc/obj/iofzbo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbo                 .
      *              *-------------------------------------------------*
      *              * Se record non esistente : uscita                *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-vas-spb-999.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-zbo-civ-spb       to   w-tot-civ-spb          .
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
      *                  * Lettura record [zbo] relativo al tipo paga- *
      *                  * mento                                       *
      *                  *---------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODSPB    "         to   f-key                  .
           move      w-det-vas-spb-cod    to   rf-zbo-cod-spb         .
           move      w-tot-tip-pag
                    (w-det-vas-spb-c01)
                                          to   rf-zbo-tip-pag         .
           move      "pgm/dcc/fls/ioc/obj/iofzbo"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-zbo                 .
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
           move      rf-zbo-tip-pag       to   w-tot-tpg-spb
                                              (w-tot-eit-spb)         .
           move      rf-zbo-tau-spb       to   w-tot-tau-spb
                                              (w-tot-eit-spb)         .
           move      rf-zbo-per-spb       to   w-tot-per-spb
                                              (w-tot-eit-spb)         .
           move      zero                 to   w-det-vas-spb-c02      .
       det-vas-spb-320.
           add       1                    to   w-det-vas-spb-c02      .
           if        w-det-vas-spb-c02    >    10
                     go to det-vas-spb-340.
           move      rf-zbo-tbe-scg
                    (w-det-vas-spb-c02)   to   w-tot-tbe-scg
                                              (w-tot-eit-spb,
                                               w-det-vas-spb-c02)     .
           move      rf-zbo-tbe-asc
                    (w-det-vas-spb-c02)   to   w-tot-tbe-asc
                                              (w-tot-eit-spb,
                                               w-det-vas-spb-c02)     .
           move      rf-zbo-tbe-psc
                    (w-det-vas-spb-c02)   to   w-tot-tbe-psc
                                              (w-tot-eit-spb,
                                               w-det-vas-spb-c02)     .
           go to     det-vas-spb-320.
       det-vas-spb-340.
           move      rf-zbo-tet-spb       to   w-tot-tet-spb
                                              (w-tot-eit-spb)         .
           move      rf-zbo-min-spb       to   w-tot-min-spb
                                              (w-tot-eit-spb)         .
           move      rf-zbo-max-spb       to   w-tot-max-spb
                                              (w-tot-eit-spb)         .
           move      rf-zbo-tar-spb       to   w-tot-tar-spb
                                              (w-tot-eit-spb)         .
           move      rf-zbo-var-spb       to   w-tot-var-spb
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
      *    * Determinazione importo spese ordine cliente               *
      *    *-----------------------------------------------------------*
       det-imp-spe-orc-000.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione su tabella spese             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-imp-spe-c01      .
       det-imp-spe-orc-100.
           add       1                    to   w-det-imp-spe-c01      .
           if        w-det-imp-spe-c01    >    6
                     go to det-imp-spe-orc-999.
      *              *-------------------------------------------------*
      *              * Se spesa da non addebitare : riciclo            *
      *              *-------------------------------------------------*
           if        rf-oct-spe-snx
                    (w-det-imp-spe-c01)   =    0
                     go to det-imp-spe-orc-100.
      *              *-------------------------------------------------*
      *              * Se sia la % che l'importo sono a zero : riciclo *
      *              *-------------------------------------------------*
           if        rf-oct-spe-per
                    (w-det-imp-spe-c01)   =    zero and
                     rf-oct-spe-imp
                    (w-det-imp-spe-c01)   =    zero
                     go to det-imp-spe-orc-100.
      *              *-------------------------------------------------*
      *              * Se % a zero vuol dire che l'importo spesa e'    *
      *              * dato dalla somma degli importi delle singole    *
      *              * bolle                                           *
      *              *-------------------------------------------------*
           if        rf-oct-spe-per
                    (w-det-imp-spe-c01)   =    zero
                     go to det-imp-spe-orc-500.
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
           if        rf-oct-spe-ibl
                    (w-det-imp-spe-c01)   =    01
                     move w-tot-tot-lor   to   w-det-imp-spe-ibl
           else if   rf-oct-spe-ibl
                    (w-det-imp-spe-c01)   =    02
                     move w-tot-tot-nsc   to   w-det-imp-spe-ibl
           else if   rf-oct-spe-ibl
                    (w-det-imp-spe-c01)   =    03
                     go to det-imp-spe-orc-220
           else if   rf-oct-spe-ibl
                    (w-det-imp-spe-c01)   =    11
                     move w-tot-tot-rig (1)
                                          to   w-det-imp-spe-ibl
           else if   rf-oct-spe-ibl
                    (w-det-imp-spe-c01)   =    12
                     move w-tot-tot-rig (2)
                                          to   w-det-imp-spe-ibl
           else if   rf-oct-spe-ibl
                    (w-det-imp-spe-c01)   =    13
                     move w-tot-tot-rig (3)
                                          to   w-det-imp-spe-ibl
           else if   rf-oct-spe-ibl
                    (w-det-imp-spe-c01)   =    14
                     move w-tot-tot-rig (4)
                                          to   w-det-imp-spe-ibl
           else if   rf-oct-spe-ibl
                    (w-det-imp-spe-c01)   =    15
                     move w-tot-tot-rig (5)
                                          to   w-det-imp-spe-ibl
           else if   rf-oct-spe-ibl
                    (w-det-imp-spe-c01)   =    16
                     move w-tot-tot-rig (6)
                                          to   w-det-imp-spe-ibl
           else if   rf-oct-spe-ibl
                    (w-det-imp-spe-c01)   =    17
                     move w-tot-tot-rig (7)
                                          to   w-det-imp-spe-ibl
           else if   rf-oct-spe-ibl
                    (w-det-imp-spe-c01)   =    18
                     move w-tot-tot-rig (8)
                                          to   w-det-imp-spe-ibl
           else if   rf-oct-spe-ibl
                    (w-det-imp-spe-c01)   =    19
                     move w-tot-tot-rig (9)
                                          to   w-det-imp-spe-ibl      .
           go to     det-imp-spe-orc-250.
       det-imp-spe-orc-220.
      *                      *-----------------------------------------*
      *                      * Se risulta da una combinazione di tota- *
      *                      * li                                      *
      *                      *-----------------------------------------*
           move      w-prs-spe-fat-ibt
                    (w-det-imp-spe-c01)   to   w-det-imp-spe-ibt      .
           move      zero                 to   w-det-imp-spe-c02      .
       det-imp-spe-orc-225.
           add       1                    to   w-det-imp-spe-c02      .
           if        w-det-imp-spe-c02    >    9
                     go to det-imp-spe-orc-250.
           if        w-det-imp-spe-ibx
                    (w-det-imp-spe-c02)   not  = "S"
                     go to det-imp-spe-orc-225.
           add       w-tot-tot-rig
                    (w-det-imp-spe-c02)   to   w-det-imp-spe-ibl      .
           go to     det-imp-spe-orc-225.
       det-imp-spe-orc-250.
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  rf-oct-spe-per
                    (w-det-imp-spe-c01)   by   w-det-imp-spe-ibl
                                        giving rf-oct-spe-imp
                                              (w-det-imp-spe-c01)     .
           divide    100                  into rf-oct-spe-imp
                                              (w-det-imp-spe-c01)
                                                         rounded      .
       det-imp-spe-orc-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      w-tot-spe-civ
                    (w-det-imp-spe-c01)   to   w-agg-cst-iva-coi      .
           move      rf-oct-spe-imp
                    (w-det-imp-spe-c01)   to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
      *              *-------------------------------------------------*
      *              * Riciclo su scansione tabella                    *
      *              *-------------------------------------------------*
           go to     det-imp-spe-orc-100.
       det-imp-spe-orc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione importo spese ordine di spedizione         *
      *    *-----------------------------------------------------------*
       det-imp-spe-ods-000.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione su tabella spese             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-imp-spe-c01      .
       det-imp-spe-ods-100.
           add       1                    to   w-det-imp-spe-c01      .
           if        w-det-imp-spe-c01    >    6
                     go to det-imp-spe-ods-999.
      *              *-------------------------------------------------*
      *              * Se spesa da non addebitare : riciclo            *
      *              *-------------------------------------------------*
           if        rf-ost-spe-snx
                    (w-det-imp-spe-c01)   =    0
                     go to det-imp-spe-ods-100.
      *              *-------------------------------------------------*
      *              * Se sia la % che l'importo sono a zero : riciclo *
      *              *-------------------------------------------------*
           if        rf-ost-spe-per
                    (w-det-imp-spe-c01)   =    zero and
                     rf-ost-spe-imp
                    (w-det-imp-spe-c01)   =    zero
                     go to det-imp-spe-ods-100.
      *              *-------------------------------------------------*
      *              * Se % a zero vuol dire che l'importo spesa e'    *
      *              * dato dalla somma degli importi delle singole    *
      *              * bolle                                           *
      *              *-------------------------------------------------*
           if        rf-ost-spe-per
                    (w-det-imp-spe-c01)   =    zero
                     go to det-imp-spe-ods-500.
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
           if        rf-ost-spe-ibl
                    (w-det-imp-spe-c01)   =    01
                     move w-tot-tot-lor   to   w-det-imp-spe-ibl
           else if   rf-ost-spe-ibl
                    (w-det-imp-spe-c01)   =    02
                     move w-tot-tot-nsc   to   w-det-imp-spe-ibl
           else if   rf-ost-spe-ibl
                    (w-det-imp-spe-c01)   =    03
                     go to det-imp-spe-ods-220
           else if   rf-ost-spe-ibl
                    (w-det-imp-spe-c01)   =    11
                     move w-tot-tot-rig (1)
                                          to   w-det-imp-spe-ibl
           else if   rf-ost-spe-ibl
                    (w-det-imp-spe-c01)   =    12
                     move w-tot-tot-rig (2)
                                          to   w-det-imp-spe-ibl
           else if   rf-ost-spe-ibl
                    (w-det-imp-spe-c01)   =    13
                     move w-tot-tot-rig (3)
                                          to   w-det-imp-spe-ibl
           else if   rf-ost-spe-ibl
                    (w-det-imp-spe-c01)   =    14
                     move w-tot-tot-rig (4)
                                          to   w-det-imp-spe-ibl
           else if   rf-ost-spe-ibl
                    (w-det-imp-spe-c01)   =    15
                     move w-tot-tot-rig (5)
                                          to   w-det-imp-spe-ibl
           else if   rf-ost-spe-ibl
                    (w-det-imp-spe-c01)   =    16
                     move w-tot-tot-rig (6)
                                          to   w-det-imp-spe-ibl
           else if   rf-ost-spe-ibl
                    (w-det-imp-spe-c01)   =    17
                     move w-tot-tot-rig (7)
                                          to   w-det-imp-spe-ibl
           else if   rf-ost-spe-ibl
                    (w-det-imp-spe-c01)   =    18
                     move w-tot-tot-rig (8)
                                          to   w-det-imp-spe-ibl
           else if   rf-ost-spe-ibl
                    (w-det-imp-spe-c01)   =    19
                     move w-tot-tot-rig (9)
                                          to   w-det-imp-spe-ibl      .
           go to     det-imp-spe-ods-250.
       det-imp-spe-ods-220.
      *                      *-----------------------------------------*
      *                      * Se risulta da una combinazione di tota- *
      *                      * li                                      *
      *                      *-----------------------------------------*
           move      w-prs-spe-fat-ibt
                    (w-det-imp-spe-c01)   to   w-det-imp-spe-ibt      .
           move      zero                 to   w-det-imp-spe-c02      .
       det-imp-spe-ods-225.
           add       1                    to   w-det-imp-spe-c02      .
           if        w-det-imp-spe-c02    >    9
                     go to det-imp-spe-ods-250.
           if        w-det-imp-spe-ibx
                    (w-det-imp-spe-c02)   not  = "S"
                     go to det-imp-spe-ods-225.
           add       w-tot-tot-rig
                    (w-det-imp-spe-c02)   to   w-det-imp-spe-ibl      .
           go to     det-imp-spe-ods-225.
       det-imp-spe-ods-250.
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  rf-ost-spe-per
                    (w-det-imp-spe-c01)   by   w-det-imp-spe-ibl
                                        giving rf-ost-spe-imp
                                              (w-det-imp-spe-c01)     .
           divide    100                  into rf-ost-spe-imp
                                              (w-det-imp-spe-c01)
                                                         rounded      .
       det-imp-spe-ods-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      w-tot-spe-civ
                    (w-det-imp-spe-c01)   to   w-agg-cst-iva-coi      .
           move      rf-ost-spe-imp
                    (w-det-imp-spe-c01)   to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
      *              *-------------------------------------------------*
      *              * Riciclo su scansione tabella                    *
      *              *-------------------------------------------------*
           go to     det-imp-spe-ods-100.
       det-imp-spe-ods-999.
           exit.

      *    *===========================================================*
      *    * Determinazione importo spese bolla cliente                *
      *    *-----------------------------------------------------------*
       det-imp-spe-bol-000.
      *              *-------------------------------------------------*
      *              * Ciclo di scansione su tabella spese             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-imp-spe-c01      .
       det-imp-spe-bol-100.
           add       1                    to   w-det-imp-spe-c01      .
           if        w-det-imp-spe-c01    >    6
                     go to det-imp-spe-bol-999.
      *              *-------------------------------------------------*
      *              * Se spesa da non addebitare : riciclo            *
      *              *-------------------------------------------------*
           if        rf-bit-spe-snx
                    (w-det-imp-spe-c01)   =    0
                     go to det-imp-spe-bol-100.
      *              *-------------------------------------------------*
      *              * Se sia la % che l'importo sono a zero : riciclo *
      *              *-------------------------------------------------*
           if        rf-bit-spe-per
                    (w-det-imp-spe-c01)   =    zero and
                     rf-bit-spe-imp
                    (w-det-imp-spe-c01)   =    zero
                     go to det-imp-spe-bol-100.
      *              *-------------------------------------------------*
      *              * Se % a zero vuol dire che l'importo spesa e'    *
      *              * dato dalla somma degli importi delle singole    *
      *              * bolle                                           *
      *              *-------------------------------------------------*
           if        rf-bit-spe-per
                    (w-det-imp-spe-c01)   =    zero
                     go to det-imp-spe-bol-500.
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
           if        rf-bit-spe-ibl
                    (w-det-imp-spe-c01)   =    01
                     move w-tot-tot-lor   to   w-det-imp-spe-ibl
           else if   rf-bit-spe-ibl
                    (w-det-imp-spe-c01)   =    02
                     move w-tot-tot-nsc   to   w-det-imp-spe-ibl
           else if   rf-bit-spe-ibl
                    (w-det-imp-spe-c01)   =    03
                     go to det-imp-spe-bol-220
           else if   rf-bit-spe-ibl
                    (w-det-imp-spe-c01)   =    11
                     move w-tot-tot-rig (1)
                                          to   w-det-imp-spe-ibl
           else if   rf-bit-spe-ibl
                    (w-det-imp-spe-c01)   =    12
                     move w-tot-tot-rig (2)
                                          to   w-det-imp-spe-ibl
           else if   rf-bit-spe-ibl
                    (w-det-imp-spe-c01)   =    13
                     move w-tot-tot-rig (3)
                                          to   w-det-imp-spe-ibl
           else if   rf-bit-spe-ibl
                    (w-det-imp-spe-c01)   =    14
                     move w-tot-tot-rig (4)
                                          to   w-det-imp-spe-ibl
           else if   rf-bit-spe-ibl
                    (w-det-imp-spe-c01)   =    15
                     move w-tot-tot-rig (5)
                                          to   w-det-imp-spe-ibl
           else if   rf-bit-spe-ibl
                    (w-det-imp-spe-c01)   =    16
                     move w-tot-tot-rig (6)
                                          to   w-det-imp-spe-ibl
           else if   rf-bit-spe-ibl
                    (w-det-imp-spe-c01)   =    17
                     move w-tot-tot-rig (7)
                                          to   w-det-imp-spe-ibl
           else if   rf-bit-spe-ibl
                    (w-det-imp-spe-c01)   =    18
                     move w-tot-tot-rig (8)
                                          to   w-det-imp-spe-ibl
           else if   rf-bit-spe-ibl
                    (w-det-imp-spe-c01)   =    19
                     move w-tot-tot-rig (9)
                                          to   w-det-imp-spe-ibl      .
           go to     det-imp-spe-bol-250.
       det-imp-spe-bol-220.
      *                      *-----------------------------------------*
      *                      * Se risulta da una combinazione di tota- *
      *                      * li                                      *
      *                      *-----------------------------------------*
           move      w-prs-spe-fat-ibt
                    (w-det-imp-spe-c01)   to   w-det-imp-spe-ibt      .
           move      zero                 to   w-det-imp-spe-c02      .
       det-imp-spe-bol-225.
           add       1                    to   w-det-imp-spe-c02      .
           if        w-det-imp-spe-c02    >    9
                     go to det-imp-spe-bol-250.
           if        w-det-imp-spe-ibx
                    (w-det-imp-spe-c02)   not  = "S"
                     go to det-imp-spe-bol-225.
           add       w-tot-tot-rig
                    (w-det-imp-spe-c02)   to   w-det-imp-spe-ibl      .
           go to     det-imp-spe-bol-225.
       det-imp-spe-bol-250.
      *                  *---------------------------------------------*
      *                  * Calcolo                                     *
      *                  *---------------------------------------------*
           multiply  rf-bit-spe-per
                    (w-det-imp-spe-c01)   by   w-det-imp-spe-ibl
                                        giving rf-bit-spe-imp
                                              (w-det-imp-spe-c01)     .
           divide    100                  into rf-bit-spe-imp
                                              (w-det-imp-spe-c01)
                                                         rounded      .
       det-imp-spe-bol-500.
      *              *-------------------------------------------------*
      *              * Aggiornamento castelletto iva                   *
      *              *-------------------------------------------------*
           move      "+"                  to   w-agg-cst-iva-tip      .
           move      w-tot-spe-civ
                    (w-det-imp-spe-c01)   to   w-agg-cst-iva-coi      .
           move      rf-bit-spe-imp
                    (w-det-imp-spe-c01)   to   w-agg-cst-iva-imp      .
           perform   agg-cst-iva-000      thru agg-cst-iva-999        .
      *              *-------------------------------------------------*
      *              * Riciclo su scansione tabella                    *
      *              *-------------------------------------------------*
           go to     det-imp-spe-bol-100.
       det-imp-spe-bol-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale al netto delle spese in ordine      *
      *    *-----------------------------------------------------------*
       det-tot-nsf-orc-000.
      *              *-------------------------------------------------*
      *              * Azzeramento work per accumulo spese             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-nsf-was      .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione tabella spese                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-nsf-ctr      .
       det-tot-nsf-orc-100.
           add       1                    to   w-det-tot-nsf-ctr      .
           if        w-det-tot-nsf-ctr    >    6
                     go to det-tot-nsf-orc-200.
      *                  *---------------------------------------------*
      *                  * Se spesa da non addebitare : riciclo        *
      *                  *---------------------------------------------*
           if        rf-oct-spe-snx
                    (w-det-tot-nsf-ctr)   =    0
                     go to det-tot-nsf-orc-100.
      *                  *---------------------------------------------*
      *                  * Aggiornamento totalizzatore                 *
      *                  *---------------------------------------------*
           add       rf-oct-spe-imp
                    (w-det-tot-nsf-ctr)   to   w-det-tot-nsf-was      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-tot-nsf-orc-100.
       det-tot-nsf-orc-200.
      *              *-------------------------------------------------*
      *              * Determinazione valore                           *
      *              *-------------------------------------------------*
           move      w-tot-tot-nsc        to    w-tot-tot-nsf         .
           add       w-det-tot-nsf-was    to    w-tot-tot-nsf         .
       det-tot-nsf-orc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale al netto delle spese in ordine di   *
      *    * spedizione                                                *
      *    *-----------------------------------------------------------*
       det-tot-nsf-ods-000.
      *              *-------------------------------------------------*
      *              * Azzeramento work per accumulo spese             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-nsf-was      .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione tabella spese                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-nsf-ctr      .
       det-tot-nsf-ods-100.
           add       1                    to   w-det-tot-nsf-ctr      .
           if        w-det-tot-nsf-ctr    >    6
                     go to det-tot-nsf-ods-200.
      *                  *---------------------------------------------*
      *                  * Se spesa da non addebitare : riciclo        *
      *                  *---------------------------------------------*
           if        rf-ost-spe-snx
                    (w-det-tot-nsf-ctr)   =    0
                     go to det-tot-nsf-ods-100.
      *                  *---------------------------------------------*
      *                  * Aggiornamento totalizzatore                 *
      *                  *---------------------------------------------*
           add       rf-ost-spe-imp
                    (w-det-tot-nsf-ctr)   to   w-det-tot-nsf-was      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-tot-nsf-ods-100.
       det-tot-nsf-ods-200.
      *              *-------------------------------------------------*
      *              * Determinazione valore                           *
      *              *-------------------------------------------------*
           move      w-tot-tot-nsc        to    w-tot-tot-nsf         .
           add       w-det-tot-nsf-was    to    w-tot-tot-nsf         .
       det-tot-nsf-ods-999.
           exit.

      *    *===========================================================*
      *    * Determinazione totale al netto delle spese in bolla       *
      *    *-----------------------------------------------------------*
       det-tot-nsf-bol-000.
      *              *-------------------------------------------------*
      *              * Azzeramento work per accumulo spese             *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-nsf-was      .
      *              *-------------------------------------------------*
      *              * Ciclo di scansione tabella spese                *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-tot-nsf-ctr      .
       det-tot-nsf-bol-100.
           add       1                    to   w-det-tot-nsf-ctr      .
           if        w-det-tot-nsf-ctr    >    6
                     go to det-tot-nsf-bol-200.
      *                  *---------------------------------------------*
      *                  * Se spesa da non addebitare : riciclo        *
      *                  *---------------------------------------------*
           if        rf-oct-spe-snx
                    (w-det-tot-nsf-ctr)   =    0
                     go to det-tot-nsf-bol-100.
      *                  *---------------------------------------------*
      *                  * Aggiornamento totalizzatore                 *
      *                  *---------------------------------------------*
           add       rf-oct-spe-imp
                    (w-det-tot-nsf-ctr)   to   w-det-tot-nsf-was      .
      *                  *---------------------------------------------*
      *                  * Riciclo                                     *
      *                  *---------------------------------------------*
           go to     det-tot-nsf-bol-100.
       det-tot-nsf-bol-200.
      *              *-------------------------------------------------*
      *              * Determinazione valore                           *
      *              *-------------------------------------------------*
           move      w-tot-tot-nsc        to    w-tot-tot-nsf         .
           add       w-det-tot-nsf-was    to    w-tot-tot-nsf         .
       det-tot-nsf-bol-999.
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
           if        rf-oct-spe-snx
                    (w-det-tot-spe-ctr)   =    0
                     go to det-tot-spe-100.
      *                  *---------------------------------------------*
      *                  * Aggiornamento totalizzatore                 *
      *                  *---------------------------------------------*
           add       rf-oct-spe-imp
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
           move      w-tot-iva-ibl
                    (w-det-tot-imp-ctr)   to   d-imp-iva-ibl-iva      .
           move      w-det-tot-imp-wci    to   d-imp-iva-cod-iva      .
           perform   det-imp-iva-000      thru det-imp-iva-999        .
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
      *                  * Valori da anagrafica cliente principale     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Normalizzazione record [dcc]            *
      *                      *-----------------------------------------*
           move      "NO"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *                      *-----------------------------------------*
      *                      * Lettura record [dcc]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-det-sca-doc-cli    to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
           if        f-sts                not  = e-not-err
                     move  01             to   rf-dcc-tip-esm         .
      *                  *---------------------------------------------*
      *                  * Valori di esclusione                        *
      *                  *---------------------------------------------*
           move      rf-dcc-tip-esm       to   w-clc-tbl-scd-esm      .
           move      rf-dcc-ggg-alt       to   w-clc-tbl-scd-alt      .
           move      rf-dcc-mmm-e01       to   w-clc-tbl-scd-e01      .
           move      rf-dcc-mmm-e02       to   w-clc-tbl-scd-e02      .
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
      *    * Determinazione importo in riga                            *
      *    *                                                           *
      *    * Ordini                                                    *
      *    *-----------------------------------------------------------*
       det-imp-rig-orc-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per la determinazione    *
      *              *-------------------------------------------------*
           move      "IR"                 to   d-imp-ven-tip-ope      .
           move      w-det-imp-rig-qta    to   d-imp-ven-qta-ven      .
           move      rf-ocr-snx-2qt       to   d-imp-ven-snx-2qt      .
           move      rf-ocr-qta-a02       to   d-imp-ven-qta-a02      .
           move      rf-ocr-snx-3qt       to   d-imp-ven-snx-3qt      .
           move      rf-ocr-qta-a03       to   d-imp-ven-qta-a03      .
           move      rf-ocr-prz-net       to   d-imp-ven-prz-uni      .
           move      rf-ocr-prz-ven       to   d-imp-ven-prz-unl      .
           move      rf-ocr-per-scr (1)   to   d-imp-ven-per-scr (1)  .
           move      rf-ocr-per-scr (2)   to   d-imp-ven-per-scr (2)  .
           move      rf-ocr-per-scr (3)   to   d-imp-ven-per-scr (3)  .
           move      rf-ocr-per-scr (4)   to   d-imp-ven-per-scr (4)  .
           move      rf-ocr-per-scr (5)   to   d-imp-ven-per-scr (5)  .
           move      rf-ocr-dec-prz       to   d-imp-ven-dec-prz      .
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           perform   det-imp-ven-cll-000  thru det-imp-ven-cll-999    .
      *              *-------------------------------------------------*
      *              * In campo di uscita                              *
      *              *-------------------------------------------------*
           move      d-imp-ven-imp-rig    to   rf-ocr-imp-rig         .
       det-imp-rig-orc-600.
      *              *-------------------------------------------------*
      *              * Eventuale conversione nella valuta di fattura-  *
      *              * zione                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se necessario                          *
      *                  *---------------------------------------------*
           if        rf-ocr-sgl-vpp       =    rf-oct-sgl-vpf
                     go to det-imp-rig-orc-999.
      *                  *---------------------------------------------*
      *                  * Conversione importo da valuta per il prezzo *
      *                  * a valuta base                               *
      *                  *---------------------------------------------*
           move      rf-ocr-sgl-vpp       to   w-cvs-vlt-sgl          .
           move      rf-ocr-dec-vpp       to   w-cvs-vlt-dec          .
           move      rf-ocr-tdc-vpp       to   w-cvs-vlt-tdc          .
           move      rf-ocr-cdc-vpp       to   w-cvs-vlt-cdc          .
           move      rf-ocr-imp-rig       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                  *---------------------------------------------*
      *                  * Conversione importo da valuta base a valuta *
      *                  * per fatturazione                            *
      *                  *---------------------------------------------*
           move      rf-oct-sgl-vpf       to   w-cvs-vlt-sgl          .
           move      rf-oct-dec-vpf       to   w-cvs-vlt-dec          .
           move      rf-oct-tdc-vpf       to   w-cvs-vlt-tdc          .
           move      rf-oct-cdc-vpf       to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione del risultato               *
      *                  *---------------------------------------------*
           move      w-cvs-vlt-aav        to   rf-ocr-imp-rig         .
       det-imp-rig-orc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione importo in riga                            *
      *    *                                                           *
      *    * Ordini di spedizione                                      *
      *    *-----------------------------------------------------------*
       det-imp-rig-ods-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per la determinazione    *
      *              *-------------------------------------------------*
           move      "IR"                 to   d-imp-ven-tip-ope      .
           move      w-det-imp-rig-qta    to   d-imp-ven-qta-ven      .
           move      rf-osr-snx-2qt       to   d-imp-ven-snx-2qt      .
           move      rf-osr-qta-a02       to   d-imp-ven-qta-a02      .
           move      rf-osr-snx-3qt       to   d-imp-ven-snx-3qt      .
           move      rf-osr-qta-a03       to   d-imp-ven-qta-a03      .
           move      rf-osr-prz-net       to   d-imp-ven-prz-uni      .
           move      rf-osr-prz-ven       to   d-imp-ven-prz-unl      .
           move      rf-osr-per-scr (1)   to   d-imp-ven-per-scr (1)  .
           move      rf-osr-per-scr (2)   to   d-imp-ven-per-scr (2)  .
           move      rf-osr-per-scr (3)   to   d-imp-ven-per-scr (3)  .
           move      rf-osr-per-scr (4)   to   d-imp-ven-per-scr (4)  .
           move      rf-osr-per-scr (5)   to   d-imp-ven-per-scr (5)  .
           move      rf-osr-dec-prz       to   d-imp-ven-dec-prz      .
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           perform   det-imp-ven-cll-000  thru det-imp-ven-cll-999    .
      *              *-------------------------------------------------*
      *              * In campo di uscita                              *
      *              *-------------------------------------------------*
           move      d-imp-ven-imp-rig    to   rf-osr-imp-rig         .
       det-imp-rig-ods-600.
      *              *-------------------------------------------------*
      *              * Eventuale conversione nella valuta di fattura-  *
      *              * zione                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se necessario                          *
      *                  *---------------------------------------------*
           if        rf-osr-sgl-vpp       =    rf-ost-sgl-vpf
                     go to det-imp-rig-ods-999.
      *                  *---------------------------------------------*
      *                  * Conversione importo da valuta per il prezzo *
      *                  * a valuta base                               *
      *                  *---------------------------------------------*
           move      rf-osr-sgl-vpp       to   w-cvs-vlt-sgl          .
           move      rf-osr-dec-vpp       to   w-cvs-vlt-dec          .
           move      rf-osr-tdc-vpp       to   w-cvs-vlt-tdc          .
           move      rf-osr-cdc-vpp       to   w-cvs-vlt-cdc          .
           move      rf-osr-imp-rig       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                  *---------------------------------------------*
      *                  * Conversione importo da valuta base a valuta *
      *                  * per fatturazione                            *
      *                  *---------------------------------------------*
           move      rf-ost-sgl-vpf       to   w-cvs-vlt-sgl          .
           move      rf-ost-dec-vpf       to   w-cvs-vlt-dec          .
           move      rf-ost-tdc-vpf       to   w-cvs-vlt-tdc          .
           move      rf-ost-cdc-vpf       to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione del risultato               *
      *                  *---------------------------------------------*
           move      w-cvs-vlt-aav        to   rf-osr-imp-rig         .
       det-imp-rig-ods-999.
           exit.

      *    *===========================================================*
      *    * Determinazione importo in riga                            *
      *    *                                                           *
      *    * Bolle clienti                                             *
      *    *-----------------------------------------------------------*
       det-imp-rig-bol-000.
      *              *-------------------------------------------------*
      *              * Preparazione parametri per la determinazione    *
      *              *-------------------------------------------------*
           move      "IR"                 to   d-imp-ven-tip-ope      .
           move      rf-bir-qta-ven       to   d-imp-ven-qta-ven      .
           move      rf-bir-snx-2qt       to   d-imp-ven-snx-2qt      .
           move      rf-bir-qta-a02       to   d-imp-ven-qta-a02      .
           move      rf-bir-snx-3qt       to   d-imp-ven-snx-3qt      .
           move      rf-bir-qta-a03       to   d-imp-ven-qta-a03      .
           move      rf-bir-prz-net       to   d-imp-ven-prz-uni      .
           move      rf-bir-prz-ven       to   d-imp-ven-prz-unl      .
           move      rf-bir-per-scr (1)   to   d-imp-ven-per-scr (1)  .
           move      rf-bir-per-scr (2)   to   d-imp-ven-per-scr (2)  .
           move      rf-bir-per-scr (3)   to   d-imp-ven-per-scr (3)  .
           move      rf-bir-per-scr (4)   to   d-imp-ven-per-scr (4)  .
           move      rf-bir-per-scr (5)   to   d-imp-ven-per-scr (5)  .
           move      rf-bir-dec-prz       to   d-imp-ven-dec-prz      .
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           perform   det-imp-ven-cll-000  thru det-imp-ven-cll-999    .
      *              *-------------------------------------------------*
      *              * In campo di uscita                              *
      *              *-------------------------------------------------*
           move      d-imp-ven-imp-rig    to   rf-bir-imp-rig         .
       det-imp-rig-bol-600.
      *              *-------------------------------------------------*
      *              * Eventuale conversione nella valuta di fattura-  *
      *              * zione                                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se necessario                          *
      *                  *---------------------------------------------*
           if        rf-bir-sgl-vpp       =    rf-bit-sgl-vpf
                     go to det-imp-rig-bol-999.
      *                  *---------------------------------------------*
      *                  * Conversione importo da valuta per il prezzo *
      *                  * a valuta base                               *
      *                  *---------------------------------------------*
           move      rf-bir-sgl-vpp       to   w-cvs-vlt-sgl          .
           move      rf-bir-dec-vpp       to   w-cvs-vlt-dec          .
           move      rf-bir-tdc-vpp       to   w-cvs-vlt-tdc          .
           move      rf-bir-cdc-vpp       to   w-cvs-vlt-cdc          .
           move      rf-bir-imp-rig       to   w-cvs-vlt-aav          .
           perform   cvs-alt-vlb-000      thru cvs-alt-vlb-999        .
      *                  *---------------------------------------------*
      *                  * Conversione importo da valuta base a valuta *
      *                  * per fatturazione                            *
      *                  *---------------------------------------------*
           move      rf-bit-sgl-vpf       to   w-cvs-vlt-sgl          .
           move      rf-bit-dec-vpf       to   w-cvs-vlt-dec          .
           move      rf-bit-tdc-vpf       to   w-cvs-vlt-tdc          .
           move      rf-bit-cdc-vpf       to   w-cvs-vlt-cdc          .
           perform   cvs-vlb-alt-000      thru cvs-vlb-alt-999        .
      *                  *---------------------------------------------*
      *                  * Bufferizzazione del risultato               *
      *                  *---------------------------------------------*
           move      w-cvs-vlt-aav        to   rf-bir-imp-rig         .
       det-imp-rig-bol-999.
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
      *    * Determinazione imposta                                    *
      *    *-----------------------------------------------------------*
       det-imp-iva-000.
      *              *-------------------------------------------------*
      *              * Determinazione                                  *
      *              *-------------------------------------------------*
           move      "DI"                 to   d-imp-iva-tip-ope      .
           perform   det-imp-iva-cll-000  thru det-imp-iva-cll-999    .
       det-imp-iva-999.
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
      *              * Test se codice cliente a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-cli-cod    =    zero
                     go to let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
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
           move      rf-cli-cod-cge       to   w-let-arc-cli-cge      .
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
           go to     let-arc-cli-600.
       let-arc-cli-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
       let-arc-cli-600.
           move      spaces               to   w-let-arc-cli-via      .
           move      spaces               to   w-let-arc-cli-loc      .
           move      zero                 to   w-let-arc-cli-cge      .
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio dipendenza cliente in [dcc]      *
      *    *-----------------------------------------------------------*
       let-arc-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice cliente a zero                   *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-cod    =    zero
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Test se codice dipendenza a spaces, solo se ri- *
      *              * chiesta di lettura specificamente di una dipen- *
      *              * denza                                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-dpz    =    spaces and
                     w-let-arc-dcc-tle    =    "D"
                     go to let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Test se codice dipendenza a "*   ", solo se ri- *
      *              * chiesta di lettura specificamente di una dipen- *
      *              * denza                                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcc-dpz    =    "*   " and
                     w-let-arc-dcc-tle    =    "D"
                     go to let-arc-dcc-450.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-let-arc-dcc-cod    to   rf-dcc-cod-cli         .
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
           move      rf-dcc-cod-abi       to   w-let-arc-dcc-abi      .
           move      rf-dcc-cod-cab       to   w-let-arc-dcc-cab      .
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
           go to     let-arc-dcc-600.
       let-arc-dcc-450.
      *              *-------------------------------------------------*
      *              * Normalizzazione per codice dipendenza "*   "    *
      *              *-------------------------------------------------*
           move      "Sia la sede che tutte le dipendenze     "
                                          to   w-let-arc-dcc-rag      .
           go to     let-arc-dcc-600.
       let-arc-dcc-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcc-rag      .
       let-arc-dcc-600.
           move      spaces               to   w-let-arc-dcc-via      .
           move      spaces               to   w-let-arc-dcc-loc      .
           move      zero                 to   w-let-arc-dcc-abi      .
           move      zero                 to   w-let-arc-dcc-cab      .
       let-arc-dcc-999.
           exit.

      *    *===========================================================*
      *    * Determinazione se presenti dipendenze per il cliente      *
      *    * commerciale                                               *
      *    *-----------------------------------------------------------*
       det-snd-dcc-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di esito                   *
      *              *-------------------------------------------------*
           move      "N"                  to   w-det-snd-dcc-snx      .
      *              *-------------------------------------------------*
      *              * Normalizzazione codice dipendenza unica         *
      *              *-------------------------------------------------*
           move      spaces               to   w-det-snd-dcc-dpz      .
      *              *-------------------------------------------------*
      *              * Normalizzazione contatore                       *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-snd-dcc-ctr      .
      *              *-------------------------------------------------*
      *              * Start su file [dcc]                             *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "NL"                 to   f-cfr                  .
           move      "CODCLI    "         to   f-key                  .
           move      w-det-snd-dcc-cli    to   rf-dcc-cod-cli         .
           move      spaces               to   rf-dcc-dpz-cli         .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * Se Start errata : uscita con flag a 'no'        *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-snd-dcc-999.
       det-snd-dcc-100.
      *              *-------------------------------------------------*
      *              * Next su [dcc]                                   *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/dcc/fls/ioc/obj/iofdcc"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcc                 .
      *              *-------------------------------------------------*
      *              * Se 'at end' : a test finale                     *
      *              *-------------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-snd-dcc-800.
      *              *-------------------------------------------------*
      *              * Max su [dcc], se non superato : a test finale   *
      *              *-------------------------------------------------*
           if        rf-dcc-cod-cli       not  = w-det-snd-dcc-cli
                     go to det-snd-dcc-800.
       det-snd-dcc-200.
      *              *-------------------------------------------------*
      *              * Test sul codice dipendenza                      *
      *              *-------------------------------------------------*
           if        rf-dcc-dpz-cli       =    spaces
                     go to det-snd-dcc-100.
       det-snd-dcc-300.
      *              *-------------------------------------------------*
      *              * Incremento contatore                            *
      *              *-------------------------------------------------*
           add       1                    to   w-det-snd-dcc-ctr      .
       det-snd-dcc-300.
      *              *-------------------------------------------------*
      *              * Bufferizzazione del primo codice dipendenza     *
      *              *-------------------------------------------------*
           if        w-det-snd-dcc-ctr    >    1
                     go to det-snd-dcc-500.
           move      rf-dcc-dpz-cli       to   w-det-snd-dcc-dpz      .
       det-snd-dcc-500.
      *              *-------------------------------------------------*
      *              * Riciclo a record [dcc] successivo               *
      *              *-------------------------------------------------*
           go to     det-snd-dcc-100.
       det-snd-dcc-800.
      *              *-------------------------------------------------*
      *              * Determinazione finale                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se durante la ricerca e' stato trovato un   *
      *                  * numero di dipendenze superiore a zero si    *
      *                  * esce con il flag di presenza a 'S'          *
      *                  *---------------------------------------------*
           if        w-det-snd-dcc-ctr    >    zero
                     go to det-snd-dcc-900
           else      go to det-snd-dcc-999.
       det-snd-dcc-900.
      *              *-------------------------------------------------*
      *              * Uscita per dipendenze trovate                   *
      *              *-------------------------------------------------*
           move      "S"                  to   w-det-snd-dcc-snx      .
       det-snd-dcc-999.
           exit.

      *    *===========================================================*
      *    * Subroutines per l'espansione righe ordine                 *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/porc301x.pgs"                   .

      *    *===========================================================*
      *    * Subroutines per l'espansione righe ordine spedizione      *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/pods301x.pgs"                   .

      *    *===========================================================*
      *    * Subroutines per l'espansione righe bolla                  *
      *    *-----------------------------------------------------------*
           copy      "pgm/bol/prg/cpy/pbol301x.pgs"                   .

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
      *    * Subroutines per determinazione imposta Iva in base ad un  *
      *    * imponibile                                                *
      *    *-----------------------------------------------------------*
           copy      "pgm/cge/prg/cpy/dimpiva0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione importo in riga            *
      *    *-----------------------------------------------------------*
           copy      "pgm/fat/prg/cpy/dimpven0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione stato ordine cliente       *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dstsorc0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione quantita' da evadere riga  *
      *    * ordine cliente                                            *
      *    *-----------------------------------------------------------*
           copy      "pgm/orc/prg/cpy/dqevroc0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione stato ordine di spedizione *
      *    * cliente                                                   *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/dstsods0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per determinazione quantita' da evadere riga  *
      *    * ordine di spedizione cliente                              *
      *    *-----------------------------------------------------------*
           copy      "pgm/ods/prg/cpy/dqdsros0.dts"                   .

      *    *===========================================================*
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

