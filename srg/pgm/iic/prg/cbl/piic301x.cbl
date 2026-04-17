       Identification Division.
       Program-Id.                                 piic301x           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    iic                 *
      *                                Settore:    mov                 *
      *                                   Fase:    iic301              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 19/01/93    *
      *                       Versione attuale:    NdK del 26/05/99    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Interrogazioni per movimenti per gestione   *
      *                    Iva intracomunitaria                        *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Espansione testata registrazione            *
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
      *    * Descrizione tipo interrogazione per l'overlay             *
      *    *-----------------------------------------------------------*
       01  w-des-tit-pgm-ovy              pic  x(40)       value
                     "     MOVIMENTO IVA INTRACOMUNITARIA     "       .

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
      *        * [iit]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/iic/fls/rec/rfiit"                          .
      *        *-------------------------------------------------------*
      *        * [gxp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxp"                          .
      *        *-------------------------------------------------------*
      *        * [cli]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rfcli"                          .
      *        *-------------------------------------------------------*
      *        * [fnt]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/cge/fls/rec/rffnt"                          .
      *        *-------------------------------------------------------*
      *        * [gxn]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/geo/fls/rec/rfgxn"                          .

      *    *===========================================================*
      *    * Tabella tipi operazione per gestione Iva intracomunitaria *
      *    *-----------------------------------------------------------*
           copy      "pgm/iic/prg/cpy/atopiic0.tab"                   .

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
      *        * Per data registrazione e numero protocollo movimento  *
      *        *-------------------------------------------------------*
           05  w-ipc-dtp-exp.
      *            *---------------------------------------------------*
      *            * Si/No variabile di i.p.c. relativa alla data re-  *
      *            * gistrazione ed al numero protocollo movimento     *
      *            *---------------------------------------------------*
               10  w-ipc-dtp-exp-snx      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Valore della variabile di i.p.c. relativa alla    *
      *            * data registrazione e numero protocollo            *
      *            *---------------------------------------------------*
               10  w-ipc-dtp-exp-mpn.
                   15  w-ipc-dtp-exp-dat  pic  9(07)                  .
                   15  w-ipc-dtp-exp-top  pic  9(04)                  .
                   15  w-ipc-dtp-exp-prt  pic  9(09)                  .

      *    *===========================================================*
      *    * Work-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Data registrazione                                    *
      *        *-------------------------------------------------------*
           05  rr-dat-reg                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo operazione                                       *
      *        *-------------------------------------------------------*
           05  rr-top-iic                 pic  9(04)                  .
      *        *-------------------------------------------------------*
      *        * Numero protocollo                                     *
      *        *-------------------------------------------------------*
           05  rr-num-prt                 pic  9(09)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxp]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxp.
               10  w-let-arc-gxp-flg      pic  x(01)                  .
               10  w-let-arc-gxp-cod      pic  x(03)                  .
               10  w-let-arc-gxp-des      pic  x(25)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [fnt]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-fnt.
               10  w-let-arc-fnt-flg      pic  x(01)                  .
               10  w-let-arc-fnt-cod      pic  9(07)                  .
               10  w-let-arc-fnt-rag      pic  x(40)                  .
               10  w-let-arc-fnt-via      pic  x(40)                  .
               10  w-let-arc-fnt-loc      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [cli]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-cli.
               10  w-let-arc-cli-flg      pic  x(01)                  .
               10  w-let-arc-cli-cod      pic  9(07)                  .
               10  w-let-arc-cli-rag      pic  x(40)                  .
               10  w-let-arc-cli-via      pic  x(40)                  .
               10  w-let-arc-cli-loc      pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [gxn]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-gxn.
               10  w-let-arc-gxn-flg      pic  x(01)                  .
               10  w-let-arc-gxn-cod      pic  x(03)                  .
               10  w-let-arc-gxn-des      pic  x(20)                  .
               10  w-let-arc-gxn-cee      pic  x(01)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Natura della transazione                   *
      *        *-------------------------------------------------------*
           05  w-exp-cdn-dtr.
               10  w-exp-cdn-dtr-num      pic  9(02)       value  9   .
               10  w-exp-cdn-dtr-lun      pic  9(02)       value 44   .
               10  w-exp-cdn-dtr-tbl.
                   15  filler             pic  x(44) value
                       "1. Acquisto o vendita                       " .
                   15  filler             pic  x(44) value
                       "2. Restituzione o sostituzione di merci     " .
                   15  filler             pic  x(44) value
                       "3. Aiuti governativi, privati, ..           " .
                   15  filler             pic  x(44) value
                       "4. Operazione in vista di una lavorazione .." .
                   15  filler             pic  x(44) value
                       "5. Operazione successiva ad una lavoraz. .. " .
                   15  filler             pic  x(44) value
                       "6. Movimento senza trasferimento di propr.  " .
                   15  filler             pic  x(44) value
                       "7. Operazione a titolo di un programma ..   " .
                   15  filler             pic  x(44) value
                       "8. Fornitura di materiali e macchinari ..   " .
                   15  filler             pic  x(44) value
                       "9. Altre transazioni                        " .
      *        *-------------------------------------------------------*
      *        * Work per : Condizioni di consegna                     *
      *        *-------------------------------------------------------*
           05  w-exp-cod-cdc.
               10  w-exp-cod-cdc-num      pic  9(02)       value 13   .
               10  w-exp-cod-cdc-lun      pic  9(02)       value 44   .
               10  w-exp-cod-cdc-tbl.
                   15  filler             pic  x(44) value
                       "E. Franco fabbrica                          " .
                   15  filler             pic  x(44) value
                       "F. Franco vettore                           " .
                   15  filler             pic  x(44) value
                       "F. Franco sotto bordo                       " .
                   15  filler             pic  x(44) value
                       "F. Franco a bordo                           " .
                   15  filler             pic  x(44) value
                       "C. Costo e nolo                             " .
                   15  filler             pic  x(44) value
                       "C. Costo, assicurazione, nolo               " .
                   15  filler             pic  x(44) value
                       "C. Nolo/porto pagato fino a ...             " .
                   15  filler             pic  x(44) value
                       "C. Nolo/porto e assic. pagati fino a ...    " .
                   15  filler             pic  x(44) value
                       "D. Reso frontiera                           " .
                   15  filler             pic  x(44) value
                       "D. Reso franco bordo nave a destino         " .
                   15  filler             pic  x(44) value
                       "D. Reso franco banchina                     " .
                   15  filler             pic  x(44) value
                       "D. Reso non sdoganato                       " .
                   15  filler             pic  x(44) value
                       "D. Reso sdoganato                           " .
      *        *-------------------------------------------------------*
      *        * Work per : Codice del modo di trasporto               *
      *        *-------------------------------------------------------*
           05  w-exp-cod-mdt.
               10  w-exp-cod-mdt-num      pic  9(02)       value  8   .
               10  w-exp-cod-mdt-lun      pic  9(02)       value 44   .
               10  w-exp-cod-mdt-tbl.
                   15  filler             pic  x(44) value
                       "1. Trasporto marittimo                      " .
                   15  filler             pic  x(44) value
                       "2. Trasporto ferroviario                    " .
                   15  filler             pic  x(44) value
                       "3. Trasporto stradale                       " .
                   15  filler             pic  x(44) value
                       "4. Trasporto aereo                          " .
                   15  filler             pic  x(44) value
                       "5. Spedizioni postali                       " .
                   15  filler             pic  x(44) value
                       "7. Installazioni fisse di trasporto         " .
                   15  filler             pic  x(44) value
                       "8. Trasporto per vie d'acqua                " .
                   15  filler             pic  x(44) value
                       "9. Propulsione propria                      " .

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
      *    * Work area per interrogazione movimento                    *
      *    *-----------------------------------------------------------*
       01  w-int-iic.
      *        *-------------------------------------------------------*
      *        * Flag per controllo ciclo interrogazione standard      *
      *        *-------------------------------------------------------*
           05  w-int-iic-flg-uno          pic  x(01)                  .

      *    *===========================================================*
      *    * Work area per stampa valori movimento                     *
      *    *-----------------------------------------------------------*
       01  w-stp-iit.
      *        *-------------------------------------------------------*
      *        * Titolo per testatina per tipo operazione              *
      *        *-------------------------------------------------------*
           05  w-stp-iit-tst-ope          pic  x(40)                  .
      *        *-------------------------------------------------------*
      *        * Prompt per la stampa                                  *
      *        *-------------------------------------------------------*
           05  w-stp-iit-pmt-pls          pic  x(28)                  .
      *        *-------------------------------------------------------*
      *        * Data per la stampa                                    *
      *        *-------------------------------------------------------*
           05  w-stp-iit-dat-pls          pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Numero progressivo s.aa.nnnnnn per la stampa          *
      *        *-------------------------------------------------------*
           05  w-stp-iit-p06-pls          pic  9(09)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di scadenza per la stampa                        *
      *        *-------------------------------------------------------*
           05  w-stp-iit-tds-pls          pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Campo alfanumerico di 50 caratteri per la stampa      *
      *        *-------------------------------------------------------*
           05  w-stp-iit-a50-pls          pic  x(40)                  .

      *    *===========================================================*
      *    * Work-area per ridefinizione contenuto del mark-point      *
      *    *-----------------------------------------------------------*
       01  w-mpn.
           05  filler                     pic  x(01)                  .

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

      *================================================================*
       Linkage Section.
      *================================================================*

      *    *===========================================================*
      *    * Area di identificazione                                   *
      *    *-----------------------------------------------------------*
       01  i-ide.
           05  i-ide-sap                  pic  x(03)                  .
           05  i-ide-arg                  pic  x(03)                  .
           05  i-ide-set                  pic  x(03)                  .
           05  i-ide-fas                  pic  x(06)                  .
           05  i-ide-pro                  pic  x(10)                  .
           05  i-ide-des                  pic  x(40)                  .

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
                   15  w-tin-ele-wci-ast  pic  x(01)                  .
                   15  w-tin-ele-wci-alf  pic  x(10)                  .
                   15  filler             pic  x(01)                  .
                   15  w-tin-ele-wci-ovy  pic  x(10)                  .
      *            *---------------------------------------------------*
      *            * Numero effettivo di elementi in tabella           *
      *            *---------------------------------------------------*
               10  w-tin-ele-num          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero di elementi selezionabili per impostazio-  *
      *            * ne, cioe' escludendo quelli finali asteriscati    *
      *            *---------------------------------------------------*
               10  w-tin-ele-nes          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero di elementi per pagina                     *
      *            *---------------------------------------------------*
               10  w-tin-ele-nep          pic  9(03)                  .
      *            *---------------------------------------------------*
      *            * Numero di pagine totali con elementi selezionabi- *
      *            * li                                                *
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
                                               w-ovy-exe
                                               w-tin
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
      *              * Esecuzione programma di interrogazione          *
      *              *-------------------------------------------------*
           perform   qry-rou-pri-000      thru qry-rou-pri-999        .
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
           move      w-des-tit-pgm-ovy    to   v-alf                  .
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
      *              * data registrazione e numero protocollo movimen- *
      *              * to                                              *
      *              *-------------------------------------------------*
           perform   ipc-dtp-exp-000      thru ipc-dtp-exp-999        .
      *              *-------------------------------------------------*
      *              * Se variabile non esistente : uscita con errore, *
      *              * in quanto il presente sottoprogramma e' stato   *
      *              * costruito esclusivamente per essere richiamato  *
      *              * con variabili di i.p.c.                         *
      *              *-------------------------------------------------*
           if        w-ipc-dtp-exp-snx    not  = "S"
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
      *              *-------------------------------------------------*
      *              * Memorizzazione sottoprogramma in attivita'      *
      *              *-------------------------------------------------*
           move      "ESPIIT    "         to   w-spg-alf-gat          .
           perform   mem-spg-att-000      thru mem-spg-att-999        .
      *              *-------------------------------------------------*
      *              * Se memorizzazione non avvenuta : uscita con er- *
      *              * rore                                            *
      *              *-------------------------------------------------*
           if        w-spg-snx-gat        not  = spaces
                     move  "#"            to   w-cnt-pre-exe-pgm
                     go to pre-exe-pgm-999.
       pre-exe-pgm-999.
           exit.

      *    *===========================================================*
      *    * Routine post-esecuzione programma                         *
      *    *-----------------------------------------------------------*
       pos-exe-pgm-000.
      *              *-------------------------------------------------*
      *              * Eliminazione sottoprogramma in attivita'        *
      *              *-------------------------------------------------*
           move      "ESPIIT    "         to   w-spg-alf-gat          .
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
      *    * ed il numero protocollo del movimento                     *
      *    *-----------------------------------------------------------*
       ipc-dtp-exp-000.
      *              *-------------------------------------------------*
      *              * Estrazione della variabile 'dtp-exp' dal livel- *
      *              * lo di profondita' precedente o dallo stesso li- *
      *              * vello di profondita' applicativa a seconda se   *
      *              * il sottoprogramma e' stato richiamato dal main  *
      *              * oppure da un sottoprogramma dello stesso livel- *
      *              * lo                                              *
      *              *-------------------------------------------------*
           move      "CV"                 to   s-ope                  .
           move      "dtp-exp"            to   s-var                  .
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
                     go to ipc-dtp-exp-200
           else      go to ipc-dtp-exp-400.
       ipc-dtp-exp-200.
      *              *-------------------------------------------------*
      *              * Se variabile esistente                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile esistente              *
      *                  *---------------------------------------------*
           move      "S"                  to   w-ipc-dtp-exp-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile                      *
      *                  *---------------------------------------------*
           move      s-alf                to   w-ipc-dtp-exp-mpn      .
      *                  *---------------------------------------------*
      *                  * Se valore della variabile non formalmente   *
      *                  * corretto : come per variabile non esistente *
      *                  *---------------------------------------------*
           if        w-ipc-dtp-exp-dat    not numeric
                     go to ipc-dtp-exp-400.
           if        w-ipc-dtp-exp-top    not numeric
                     go to ipc-dtp-exp-400.
           if        w-ipc-dtp-exp-prt    not numeric
                     go to ipc-dtp-exp-400.
      *                  *---------------------------------------------*
      *                  * Se valore della variabile mancante : come   *
      *                  * per variabile non esistente                 *
      *                  *---------------------------------------------*
           if        w-ipc-dtp-exp-dat    =    zero
                     go to ipc-dtp-exp-400.
           if        w-ipc-dtp-exp-top    =    zero
                     go to ipc-dtp-exp-400.
           if        w-ipc-dtp-exp-prt    =    zero
                     go to ipc-dtp-exp-400.
      *                  *---------------------------------------------*
      *                  * Preparazione parametri come se accettati    *
      *                  *---------------------------------------------*
           move      w-ipc-dtp-exp-dat    to   rr-dat-reg             .
           move      w-ipc-dtp-exp-top    to   rr-top-iic             .
           move      w-ipc-dtp-exp-prt    to   rr-num-prt             .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-dtp-exp-999.
       ipc-dtp-exp-400.
      *              *-------------------------------------------------*
      *              * Se variabile non esistente                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Segnale di variabile non esistente          *
      *                  *---------------------------------------------*
           move      "N"                  to   w-ipc-dtp-exp-snx      .
      *                  *---------------------------------------------*
      *                  * Valore della variabile a zero               *
      *                  *---------------------------------------------*
           move      zero                 to   w-ipc-dtp-exp-dat      .
           move      zero                 to   w-ipc-dtp-exp-top      .
           move      zero                 to   w-ipc-dtp-exp-prt      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     ipc-dtp-exp-999.
       ipc-dtp-exp-999.
           exit.

      *    *===========================================================*
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se esistono una data registrazione ed un    *
      *                  * numero protocollo passati dal chiamante     *
      *                  * non si eseguono le richieste, altrimenti    *
      *                  * le si eseguono                              *
      *                  *---------------------------------------------*
           if        w-ipc-dtp-exp-snx    =    "S"       and
                     w-ipc-dtp-exp-dat    not  = zero    and
                     w-ipc-dtp-exp-top    not  = zero    and
                     w-ipc-dtp-exp-prt    not  = zero
                     move  "N"            to   w-cnt-fun-snx-ric
           else      move  "S"            to   w-cnt-fun-snx-ric      .
       pre-tip-fun-200.
      *              *-------------------------------------------------*
      *              * Si/No funzionamento ciclico                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se esistono una data registrazione ed un    *
      *                  * numero protocollo passati dal chiamante     *
      *                  * il funzionamento non e' ciclico, altrimenti *
      *                  * e' ciclico                                  *
      *                  *---------------------------------------------*
           if        w-ipc-dtp-exp-snx    =    "S"       and
                     w-ipc-dtp-exp-dat    not  = zero    and
                     w-ipc-dtp-exp-top    not  = zero    and
                     w-ipc-dtp-exp-prt    not  = zero
                     move  "N"            to   w-cnt-fun-snx-cic
           else      move  "S"            to   w-cnt-fun-snx-cic      .
       pre-tip-fun-400.
      *              *-------------------------------------------------*
      *              * Si/No funzionamento automatico                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Sempre funzionamento automatico             *
      *                  *---------------------------------------------*
           move      "S"                  to   w-cnt-fun-snx-aut      .
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
      *              * [iit]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
      *              *-------------------------------------------------*
      *              * [gxp]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
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
      *              * [gxn]                                           *
      *              *-------------------------------------------------*
           move      "OP"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
      *              *-------------------------------------------------*
      *              * [iit]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
      *              *-------------------------------------------------*
      *              * [gxp]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
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
      *              * [gxn]                                           *
      *              *-------------------------------------------------*
           move      "CL"                 to   f-ope                  .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
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
      *                  * Forzatura del tipo uscita per 'E', in quan- *
      *                  * to il programma e' stato costruito solamen- *
      *                  * te per funzionare con variabili di i.p.c.   *
      *                  *---------------------------------------------*
           move      "E"                  to   w-cnt-acc-ric-sel      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     acc-ric-sel-999.
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
           move      "Conferma espansione della scadenza (S/N/E) ?"
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
      *    * Normalizzazione richieste di selezione                    *
      *    *-----------------------------------------------------------*
       nor-ric-sel-000.
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
       qry-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Close files                              *
      *    *-----------------------------------------------------------*
       qry-cls-fls-000.
       qry-cls-fls-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Start iniziale                           *
      *    *-----------------------------------------------------------*
       qry-str-ini-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-cnt-qry-flg-sub      .
       qry-str-ini-100.
      *              *-------------------------------------------------*
      *              * Lettura movimento da [iit]                      *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "DATREG    "         to   f-key                  .
           move      rr-dat-reg           to   rf-iit-dat-reg         .
           move      rr-num-prt           to   rf-iit-num-prt         .
           move      "pgm/iic/fls/ioc/obj/iofiit"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-iit                 .
      *              *-------------------------------------------------*
      *              * Deviazione a seconda dell'esito della lettura   *
      *              *-------------------------------------------------*
           if        f-sts                =    e-not-err
                     go to qry-str-ini-300
           else      go to qry-str-ini-200.
       qry-str-ini-200.
      *              *-------------------------------------------------*
      *              * Se record movimento non esistente               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita ad errore                    *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-qry-flg-sub      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-str-ini-999.
       qry-str-ini-300.
      *              *-------------------------------------------------*
      *              * Se record movimento esistente                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione flag per controllo ciclo    *
      *                  * interrogazione standard                     *
      *                  *---------------------------------------------*
           move      spaces               to   w-int-iic-flg-uno      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-str-ini-999.
       qry-str-ini-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Messaggio per nessuna registrazione      *
      *    *-----------------------------------------------------------*
       qry-nes-ela-000.
      *              *-------------------------------------------------*
      *              * Messaggio di errore                             *
      *              *-------------------------------------------------*
           move      "Movimento non esistente in archivio Iva intracomun
      -              "itaria !       "    to   w-err-box-err-msg      .
           perform   box-msg-err-000      thru box-msg-err-999        .
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
      *              * Test su flag per controllo ciclo interrogazione *
      *              * standard                                        *
      *              *-------------------------------------------------*
           if        w-int-iic-flg-uno    =    spaces
                     move  "#"            to   w-int-iic-flg-uno
           else      move  "#"            to   w-cnt-qry-flg-sub      .
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
       qry-liv-det-050.
      *              *-------------------------------------------------*
      *              * Intestazione pagina                             *
      *              *-------------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-100.
      *              *-------------------------------------------------*
      *              * Stampa dati per registrazione                   *
      *              *-------------------------------------------------*
           perform   stp-dat-rgs-000      thru stp-dat-rgs-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-150.
      *              *-------------------------------------------------*
      *              * Stampa dati per riferimento contabile           *
      *              *-------------------------------------------------*
           perform   stp-dat-cge-000      thru stp-dat-cge-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-200.
      *              *-------------------------------------------------*
      *              * Stampa dati cliente o fornitore attuale         *
      *              *-------------------------------------------------*
           perform   stp-dat-cfa-000      thru stp-dat-cfa-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-250.
      *              *-------------------------------------------------*
      *              * Stampa dati cliente o fornitore precedente      *
      *              *-------------------------------------------------*
           perform   stp-dat-cfp-000      thru stp-dat-cfp-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-300.
      *              *-------------------------------------------------*
      *              * Stampa dati stato CEE attuale                   *
      *              *-------------------------------------------------*
           perform   stp-dat-sca-000      thru stp-dat-sca-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-350.
      *              *-------------------------------------------------*
      *              * Stampa dati stato CEE precedente                *
      *              *-------------------------------------------------*
           perform   stp-dat-scp-000      thru stp-dat-scp-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-400.
      *              *-------------------------------------------------*
      *              * Stampa dati specifiche per il movimento         *
      *              *-------------------------------------------------*
           perform   stp-dat-spf-000      thru stp-dat-spf-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-450.
      *              *-------------------------------------------------*
      *              * Stampa dati per altre informazioni              *
      *              *-------------------------------------------------*
           perform   stp-dat-ain-000      thru stp-dat-ain-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per registrazione                             *
      *    *-----------------------------------------------------------*
       stp-dat-rgs-000.
      *              *-------------------------------------------------*
      *              * Stampa testatina per registrazione              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "             Registrazione              "
                                          to   w-stp-iit-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-025.
      *              *-------------------------------------------------*
      *              * Data regsitrazione                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data registrazione         :"
                                          to   w-stp-iit-pmt-pls      .
           move      rf-iit-dat-reg       to   w-stp-iit-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-050.
      *              *-------------------------------------------------*
      *              * Numero protocollo                               *
      *              *-------------------------------------------------*
           move      "Numero operazione          :"
                                          to   w-stp-iit-pmt-pls      .
           move      rf-iit-num-prt       to   w-stp-iit-p06-pls      .
           perform   stp-p06-pls-000      thru stp-p06-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-075.
      *              *-------------------------------------------------*
      *              * Tipo operazione                                 *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      spaces               to   w-stp-iit-pmt-pls      .
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "9"                  to   v-edm                  .
           move      rf-iit-top-iic       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           string    "Tipo operazione       "
                                delimited by   size
                     v-edt      delimited by   spaces
                     " :"       delimited by   size
                                          into w-stp-iit-pmt-pls      .
           move      "C"                  to   w-top-iic-tab-tle      .
           move      rf-iit-top-iic       to   w-top-iic-tab-top      .
           perform   top-iic-let-000      thru top-iic-let-999        .
           move      w-top-iic-tab-des    to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-rgs-100.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per registrazione              *
      *              *-------------------------------------------------*
           go to     stp-dat-rgs-999.
       stp-dat-rgs-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per riferimento contabile                     *
      *    *-----------------------------------------------------------*
       stp-dat-cge-000.
      *              *-------------------------------------------------*
      *              * Stampa testatina per registrazione              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "     Riferimenti alla contabilita'      "
                                          to   w-stp-iit-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cge-999.
       stp-dat-cge-025.
      *              *-------------------------------------------------*
      *              * Data regsitrazione                              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data registrazione         :"
                                          to   w-stp-iit-pmt-pls      .
           move      rf-iit-dtr-cge       to   w-stp-iit-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cge-999.
       stp-dat-cge-050.
      *              *-------------------------------------------------*
      *              * Numero protocollo                               *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-iit-prt-cge       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Numero protocollo          :"
                                          to   w-stp-iit-pmt-pls      .
           move      v-edt                to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cge-999.
       stp-dat-cge-075.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per riferimenti contabili      *
      *              *-------------------------------------------------*
           go to     stp-dat-cge-999.
       stp-dat-cge-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per cliente o fornitore attuale               *
      *    *-----------------------------------------------------------*
       stp-dat-cfa-000.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo archivio        *
      *              *-------------------------------------------------*
           if        rf-iit-tip-arc       =    "C"
                     go to stp-dat-cfa-200
           else if   rf-iit-tip-arc       =    "F"
                     go to stp-dat-cfa-600
           else      go to stp-dat-cfa-999.
       stp-dat-cfa-200.
      *              *-------------------------------------------------*
      *              * Se tipo archivio 'C'                            *
      *              *-------------------------------------------------*
       stp-dat-cfa-225.
      *                  *---------------------------------------------*
      *                  * Stampa testatina per cliente attuale        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "                Cliente                 "
                                          to   w-stp-iit-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cfa-999.
       stp-dat-cfa-250.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-iit-cod-arc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice cliente             :"
                                          to   w-stp-iit-pmt-pls      .
           move      v-edt                to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cfa-999.
       stp-dat-cfa-275.
      *                  *---------------------------------------------*
      *                  * Anagrafica                                  *
      *                  *---------------------------------------------*
       stp-dat-cfa-277.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica cliente              *
      *                      *-----------------------------------------*
           move      rf-iit-cod-arc       to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
       stp-dat-cfa-279.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      "Ragione sociale            :"
                                          to   w-stp-iit-pmt-pls      .
           move      w-let-arc-cli-rag    to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-cfa-281.
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-iit-pmt-pls      .
           move      w-let-arc-cli-via    to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-cfa-283.
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-iit-pmt-pls      .
           move      w-let-arc-cli-loc    to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-cfa-300.
      *                  *---------------------------------------------*
      *                  * Partita Iva                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice Iva                 :"
                                          to   w-stp-iit-pmt-pls      .
           move      rf-iit-cdi-opi       to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cfa-999.
       stp-dat-cfa-325.
      *                  *---------------------------------------------*
      *                  * Fine stampa dati per cliente attuale        *
      *                  *---------------------------------------------*
           go to     stp-dat-cfa-999.
       stp-dat-cfa-600.
      *              *-------------------------------------------------*
      *              * Se tipo archivio 'F'                            *
      *              *-------------------------------------------------*
       stp-dat-cfa-625.
      *                  *---------------------------------------------*
      *                  * Stampa testatina per fornitore attuale      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "               Fornitore                "
                                          to   w-stp-iit-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cfa-999.
       stp-dat-cfa-650.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-iit-cod-arc       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice fornitore           :"
                                          to   w-stp-iit-pmt-pls      .
           move      v-edt                to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cfa-999.
       stp-dat-cfa-675.
      *                  *---------------------------------------------*
      *                  * Anagrafica                                  *
      *                  *---------------------------------------------*
       stp-dat-cfa-677.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica fornitore            *
      *                      *-----------------------------------------*
           move      rf-iit-cod-arc       to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
       stp-dat-cfa-679.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      "Ragione sociale            :"
                                          to   w-stp-iit-pmt-pls      .
           move      w-let-arc-fnt-rag    to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-cfa-681.
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-iit-pmt-pls      .
           move      w-let-arc-fnt-via    to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-cfa-683.
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-iit-pmt-pls      .
           move      w-let-arc-fnt-loc    to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-cfa-700.
      *                  *---------------------------------------------*
      *                  * Partita Iva                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice Iva                 :"
                                          to   w-stp-iit-pmt-pls      .
           move      rf-iit-cdi-opi       to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cfa-999.
       stp-dat-cfa-725.
      *                  *---------------------------------------------*
      *                  * Fine stampa dati per fornitore attuale      *
      *                  *---------------------------------------------*
           go to     stp-dat-cfa-999.
       stp-dat-cfa-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per cliente o fornitore precedente            *
      *    *-----------------------------------------------------------*
       stp-dat-cfp-000.
      *              *-------------------------------------------------*
      *              * Se codice archivio precedente a zero : uscita   *
      *              *-------------------------------------------------*
           if        rf-iit-arc-pre       =    zero
                     go to stp-dat-cfp-999.
      *              *-------------------------------------------------*
      *              * Deviazione in funzione del tipo archivio        *
      *              *-------------------------------------------------*
           if        rf-iit-tip-arc       =    "C"
                     go to stp-dat-cfp-200
           else if   rf-iit-tip-arc       =    "F"
                     go to stp-dat-cfp-600
           else      go to stp-dat-cfp-999.
       stp-dat-cfp-200.
      *              *-------------------------------------------------*
      *              * Se tipo archivio 'C'                            *
      *              *-------------------------------------------------*
       stp-dat-cfp-225.
      *                  *---------------------------------------------*
      *                  * Stampa testatina per cliente attuale        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "           Cliente precedente           "
                                          to   w-stp-iit-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cfp-999.
       stp-dat-cfp-250.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Editing                                 *
      *                      *-----------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-iit-arc-pre       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice cliente             :"
                                          to   w-stp-iit-pmt-pls      .
           move      v-edt                to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cfp-999.
       stp-dat-cfp-275.
      *                  *---------------------------------------------*
      *                  * Anagrafica                                  *
      *                  *---------------------------------------------*
       stp-dat-cfp-277.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica cliente              *
      *                      *-----------------------------------------*
           move      rf-iit-arc-pre       to   w-let-arc-cli-cod      .
           perform   let-arc-cli-000      thru let-arc-cli-999        .
       stp-dat-cfp-279.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      "Ragione sociale            :"
                                          to   w-stp-iit-pmt-pls      .
           move      w-let-arc-cli-rag    to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-cfp-281.
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-iit-pmt-pls      .
           move      w-let-arc-cli-via    to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-cfp-283.
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-iit-pmt-pls      .
           move      w-let-arc-cli-loc    to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-cfp-300.
      *                  *---------------------------------------------*
      *                  * Partita Iva                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice Iva                 :"
                                          to   w-stp-iit-pmt-pls      .
           move      rf-iit-cdi-ocp       to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cfp-999.
       stp-dat-cfp-325.
      *                  *---------------------------------------------*
      *                  * Fine stampa dati per cliente precedente     *
      *                  *---------------------------------------------*
           go to     stp-dat-cfp-999.
       stp-dat-cfp-600.
      *              *-------------------------------------------------*
      *              * Se tipo archivio 'F'                            *
      *              *-------------------------------------------------*
       stp-dat-cfp-625.
      *                  *---------------------------------------------*
      *                  * Stampa testatina per fornitore precedente   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "          Fornitore precedente          "
                                          to   w-stp-iit-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cfp-999.
       stp-dat-cfp-650.
      *                  *---------------------------------------------*
      *                  * Codice                                      *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<B"                 to   v-edm                  .
           move      rf-iit-arc-pre       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice fornitore           :"
                                          to   w-stp-iit-pmt-pls      .
           move      v-edt                to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cfp-999.
       stp-dat-cfp-675.
      *                  *---------------------------------------------*
      *                  * Anagrafica                                  *
      *                  *---------------------------------------------*
       stp-dat-cfp-677.
      *                      *-----------------------------------------*
      *                      * Lettura anagrafica fornitore            *
      *                      *-----------------------------------------*
           move      rf-iit-arc-pre       to   w-let-arc-fnt-cod      .
           perform   let-arc-fnt-000      thru let-arc-fnt-999        .
       stp-dat-cfp-679.
      *                      *-----------------------------------------*
      *                      * Ragione sociale                         *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      "Ragione sociale            :"
                                          to   w-stp-iit-pmt-pls      .
           move      w-let-arc-fnt-rag    to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-cfp-681.
      *                      *-----------------------------------------*
      *                      * Indirizzo                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-iit-pmt-pls      .
           move      w-let-arc-fnt-via    to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-cfp-683.
      *                      *-----------------------------------------*
      *                      * Localita'                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Richiamo routine                    *
      *                          *-------------------------------------*
           move      spaces               to   w-stp-iit-pmt-pls      .
           move      w-let-arc-fnt-loc    to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                          *-------------------------------------*
      *                          * Se interruzione forzata : uscita    *
      *                          *-------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-rgs-999.
       stp-dat-cfp-700.
      *                  *---------------------------------------------*
      *                  * Partita Iva                                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Richiamo routine                        *
      *                      *-----------------------------------------*
           move      "Codice Iva                 :"
                                          to   w-stp-iit-pmt-pls      .
           move      rf-iit-cdi-ocp       to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                      *-----------------------------------------*
      *                      * Se interruzione forzata : uscita        *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-cfp-999.
       stp-dat-cfp-725.
      *                  *---------------------------------------------*
      *                  * Fine stampa dati per fornitore precedente   *
      *                  *---------------------------------------------*
           go to     stp-dat-cfp-999.
       stp-dat-cfp-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per stato CEE attuale                         *
      *    *-----------------------------------------------------------*
       stp-dat-sca-000.
      *              *-------------------------------------------------*
      *              * Stampa testatina stato CEE attuale              *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "    Stato membro CEE di riferimento     "
                                          to   w-stp-iit-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-sca-999.
       stp-dat-sca-025.
      *              *-------------------------------------------------*
      *              * Codice                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Codice                     :"
                                          to   w-stp-iit-pmt-pls      .
           move      rf-iit-iso-opc       to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-sca-999.
       stp-dat-sca-050.
      *              *-------------------------------------------------*
      *              * Denominazione                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica codice ISO stato CEE     *
      *                  *---------------------------------------------*
           move      rf-iit-iso-opc       to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Denominazione              :"
                                          to   w-stp-iit-pmt-pls      .
           move      w-let-arc-gxn-des    to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-sca-999.
       stp-dat-sca-075.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per stato CEE attuale          *
      *              *-------------------------------------------------*
           go to     stp-dat-sca-999.
       stp-dat-sca-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per stato CEE precedente                      *
      *    *-----------------------------------------------------------*
       stp-dat-scp-000.
      *              *-------------------------------------------------*
      *              * Se stato CEE precedente a spaces : uscita       *
      *              *-------------------------------------------------*
           if        rf-iit-iso-ocp       =    spaces
                     go to stp-dat-scp-999.
       stp-dat-scp-025.
      *              *-------------------------------------------------*
      *              * Stampa testatina stato CEE precedente           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "      Stato membro CEE precedente       "
                                          to   w-stp-iit-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-scp-999.
       stp-dat-scp-050.
      *              *-------------------------------------------------*
      *              * Codice                                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Codice                     :"
                                          to   w-stp-iit-pmt-pls      .
           move      rf-iit-iso-ocp       to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-scp-999.
       stp-dat-scp-075.
      *              *-------------------------------------------------*
      *              * Denominazione                                   *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica codice ISO stato CEE     *
      *                  *---------------------------------------------*
           move      rf-iit-iso-ocp       to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Denominazione              :"
                                          to   w-stp-iit-pmt-pls      .
           move      w-let-arc-gxn-des    to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-scp-999.
       stp-dat-scp-100.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per stato CEE precedente       *
      *              *-------------------------------------------------*
           go to     stp-dat-scp-999.
       stp-dat-scp-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per specifiche per il movimento               *
      *    *-----------------------------------------------------------*
       stp-dat-spf-000.
      *              *-------------------------------------------------*
      *              * Stampa testatina                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "      Specifiche per il movimento       "
                                          to   w-stp-iit-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-spf-999.
       stp-dat-spf-025.
      *              *-------------------------------------------------*
      *              * Natura della transazione                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-cdn-dtr-lun    to   v-car                  .
           move      w-exp-cdn-dtr-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-cdn-dtr-tbl    to   v-txt                  .
           move      rf-iit-cdn-dtr       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Natura della transazione   :"
                                          to   w-stp-iit-pmt-pls      .
           move      v-edt                to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-spf-999.
       stp-dat-spf-050.
      *              *-------------------------------------------------*
      *              * Natura della transazione precedente             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se mancante : nessuna azione                *
      *                  *---------------------------------------------*
           if        rf-iit-cdn-dtp       =    zero
                     go to stp-dat-spf-075.
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-cdn-dtr-lun    to   v-car                  .
           move      w-exp-cdn-dtr-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-cdn-dtr-tbl    to   v-txt                  .
           move      rf-iit-cdn-dtp       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      " ''     ''    ''  preced.  :"
                                          to   w-stp-iit-pmt-pls      .
           move      v-edt                to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-spf-999.
       stp-dat-spf-075.
      *              *-------------------------------------------------*
      *              * Condizioni di consegna                          *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-cod-cdc-lun    to   v-car                  .
           move      w-exp-cod-cdc-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-cod-cdc-tbl    to   v-txt                  .
      *
           if        rf-iit-cod-cdc       =    01
                     move  1              to   v-num
           else if   rf-iit-cod-cdc       =    11
                     move  2              to   v-num
           else if   rf-iit-cod-cdc       =    12
                     move  3              to   v-num
           else if   rf-iit-cod-cdc       =    13
                     move  4              to   v-num
           else if   rf-iit-cod-cdc       =    21
                     move  5              to   v-num
           else if   rf-iit-cod-cdc       =    22
                     move  6              to   v-num
           else if   rf-iit-cod-cdc       =    23
                     move  7              to   v-num
           else if   rf-iit-cod-cdc       =    24
                     move  8              to   v-num
           else if   rf-iit-cod-cdc       =    31
                     move  9              to   v-num
           else if   rf-iit-cod-cdc       =    32
                     move  10             to   v-num
           else if   rf-iit-cod-cdc       =    33
                     move  11             to   v-num
           else if   rf-iit-cod-cdc       =    34
                     move  12             to   v-num
           else if   rf-iit-cod-cdc       =    35
                     move  13             to   v-num
           else      move  zero           to   v-num                  .
      *
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Condizioni di consegna     :"
                                          to   w-stp-iit-pmt-pls      .
           move      v-edt                to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-spf-999.
       stp-dat-spf-100.
      *              *-------------------------------------------------*
      *              * Codice del modo di trasporto                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-cod-mdt-lun    to   v-car                  .
           move      w-exp-cod-mdt-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-cod-mdt-tbl    to   v-txt                  .
           if        rf-iit-cod-mdt       =    1
                     move  1              to   v-num
           else if   rf-iit-cod-mdt       =    2
                     move  2              to   v-num
           else if   rf-iit-cod-mdt       =    3
                     move  3              to   v-num
           else if   rf-iit-cod-mdt       =    4
                     move  4              to   v-num
           else if   rf-iit-cod-mdt       =    5
                     move  5              to   v-num
           else if   rf-iit-cod-mdt       =    7
                     move  6              to   v-num
           else if   rf-iit-cod-mdt       =    8
                     move  7              to   v-num
           else if   rf-iit-cod-mdt       =    9
                     move  8              to   v-num
           else      move  zero           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Codice modo di trasporto   :"
                                          to   w-stp-iit-pmt-pls      .
           move      v-edt                to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-spf-999.
       stp-dat-spf-125.
      *              *-------------------------------------------------*
      *              * Paese di provenienza o destinazione             *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se mancante : nessuna azione                *
      *                  *---------------------------------------------*
           if        rf-iit-iso-pod       =    spaces
                     go to stp-dat-spf-150.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica codice ISO stato CEE     *
      *                  *---------------------------------------------*
           move      rf-iit-iso-pod       to   w-let-arc-gxn-cod      .
           perform   let-arc-gxn-000      thru let-arc-gxn-999        .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           if        rf-iit-top-iic       =    0001 or
                     rf-iit-top-iic       =    0011
                     move  "Paese di provenienza       :"
                                          to   w-stp-iit-pmt-pls
           else if   rf-iit-top-iic       =    0002 or
                     rf-iit-top-iic       =    0012
                     move  "Paese di destinazione      :"
                                          to   w-stp-iit-pmt-pls
           else      move  "Paese di prov. o dest.     :"
                                          to   w-stp-iit-pmt-pls      .
           move      spaces               to   w-stp-iit-a50-pls      .
           string    rf-iit-iso-pod
                                delimited by   size
                     "  "       delimited by   size
                     w-let-arc-gxn-des
                                delimited by   size
                                          into w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-sca-999.
       stp-dat-spf-150.
      *              *-------------------------------------------------*
      *              * Provincia di destinazione o provenienza         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se mancante : nessuna azione                *
      *                  *---------------------------------------------*
           if        rf-iit-pvc-ood       =    spaces
                     go to stp-dat-spf-175.
      *                  *---------------------------------------------*
      *                  * Lettura anagrafica provincia                *
      *                  *---------------------------------------------*
           move      rf-iit-pvc-ood       to   w-let-arc-gxp-cod      .
           perform   let-arc-gxp-000      thru let-arc-gxp-999        .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           if        rf-iit-top-iic       =    0001 or
                     rf-iit-top-iic       =    0011
                     move  "Provincia di destinazione  :"
                                          to   w-stp-iit-pmt-pls
           else if   rf-iit-top-iic       =    0002 or
                     rf-iit-top-iic       =    0012
                     move  "Provincia di provenienza   :"
                                          to   w-stp-iit-pmt-pls
           else      move  "Provincia di prov. o dest. :"
                                          to   w-stp-iit-pmt-pls      .
           move      spaces               to   w-stp-iit-a50-pls      .
           string    rf-iit-pvc-ood
                                delimited by   size
                     "  "       delimited by   size
                     w-let-arc-gxp-des
                                delimited by   size
                                          into w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-sca-999.
       stp-dat-spf-175.
      *              *-------------------------------------------------*
      *              * Corrispettivo in valuta                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se mancante : nessuna azione                *
      *                  *---------------------------------------------*
           if        rf-iit-tco-eiv       =    zero
                     go to stp-dat-spf-200.
      *                  *---------------------------------------------*
      *                  * Editing                                     *
      *                  *---------------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      13                   to   v-car                  .
           move      03                   to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<GBD"               to   v-edm                  .
           move      rf-iit-tco-eiv       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Corrispettivo in valuta    :"
                                          to   w-stp-iit-pmt-pls      .
           move      v-edt                to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-sca-999.
       stp-dat-spf-200.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per specifiche movimento       *
      *              *-------------------------------------------------*
           go to     stp-dat-spf-999.
       stp-dat-spf-999.
           exit.

      *    *===========================================================*
      *    * Stampa dati per altre informazioni                        *
      *    *-----------------------------------------------------------*
       stp-dat-ain-000.
      *              *-------------------------------------------------*
      *              * Stampa testatina per altre informazioni         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "           Altre informazioni           "
                                          to   w-stp-iit-tst-ope      .
           perform   stp-tst-ope-000      thru stp-tst-ope-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-050.
      *              *-------------------------------------------------*
      *              * Flags di elaborazione bloccanti                 *
      *              *-------------------------------------------------*
       stp-dat-ain-055.
      *                  *---------------------------------------------*
      *                  * Se a Spaces : no stampa                     *
      *                  *---------------------------------------------*
           if        rf-iit-flg-blo       =    spaces
                     go to stp-dat-ain-075.
       stp-dat-ain-060.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Flags bloccanti            :"
                                          to   w-stp-iit-pmt-pls      .
           move      rf-iit-flg-blo       to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-075.
      *              *-------------------------------------------------*
      *              * Flags di elaborazione non bloccanti             *
      *              *-------------------------------------------------*
       stp-dat-ain-080.
      *                  *---------------------------------------------*
      *                  * Se a Spaces : no stampa                     *
      *                  *---------------------------------------------*
           if        rf-iit-flg-nbl       =    spaces
                     go to stp-dat-ain-100.
       stp-dat-ain-085.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Flags non bloccanti        :"
                                          to   w-stp-iit-pmt-pls      .
           move      rf-iit-flg-nbl       to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-100.
      *              *-------------------------------------------------*
      *              * Flags di sottoponibilita' a pulizia             *
      *              *-------------------------------------------------*
       stp-dat-ain-105.
      *                  *---------------------------------------------*
      *                  * Se a Spaces : no stampa                     *
      *                  *---------------------------------------------*
           if        rf-iit-flg-pul       =    spaces
                     go to stp-dat-ain-125.
       stp-dat-ain-110.
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Flags per pulizia          :"
                                          to   w-stp-iit-pmt-pls      .
           move      rf-iit-flg-pul       to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-125.
      *              *-------------------------------------------------*
      *              * Data ultimo aggiornamento                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "Data ultimo aggiornamento  :"
                                          to   w-stp-iit-pmt-pls      .
           move      rf-iit-ide-dat       to   w-stp-iit-dat-pls      .
           perform   stp-dat-pls-000      thru stp-dat-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-150.
      *              *-------------------------------------------------*
      *              * Utente ultimo aggiornamento                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "     da parte dell'utente  :"
                                          to   w-stp-iit-pmt-pls      .
           move      rf-iit-ide-ute       to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-175.
      *              *-------------------------------------------------*
      *              * Programma ultimo aggiornamento                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Richiamo routine                            *
      *                  *---------------------------------------------*
           move      "         con il programma  :"
                                          to   w-stp-iit-pmt-pls      .
           move      rf-iit-ide-fas       to   w-stp-iit-a50-pls      .
           perform   stp-a50-pls-000      thru stp-a50-pls-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-ain-999.
       stp-dat-ain-200.
      *              *-------------------------------------------------*
      *              * Fine stampa dati per altre informazioni         *
      *              *-------------------------------------------------*
           go to     stp-dat-ain-999.
       stp-dat-ain-999.
           exit.

      *    *===========================================================*
      *    * Stampa testatina per tipo operazione su scadenza          *
      *    *-----------------------------------------------------------*
       stp-tst-ope-000.
      *              *-------------------------------------------------*
      *              * 1. linea                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se almeno una riga residua, con even-  *
      *                  * tuale reintestazione                        *
      *                  *---------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-tst-ope-999.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * 40 lineette centrali                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-tst-ope-200.
      *              *-------------------------------------------------*
      *              * 2. linea                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se almeno una riga residua, con even-  *
      *                  * tuale reintestazione                        *
      *                  *---------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-tst-ope-999.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Titoletto centrale                          *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
           move      w-stp-iit-tst-ope    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-tst-ope-400.
      *              *-------------------------------------------------*
      *              * 3. linea                                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se almeno una riga residua, con even-  *
      *                  * tuale reintestazione                        *
      *                  *---------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *                  *---------------------------------------------*
      *                  * Se interruzione forzata : uscita            *
      *                  *---------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-tst-ope-999.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * 40 lineette centrali                        *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      40                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      21                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-tst-ope-999.
           exit.

      *    *===========================================================*
      *    * Stampa del solo prompt                                    *
      *    *-----------------------------------------------------------*
       stp-pmt-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-pmt-pls-999.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-stp-iit-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-pmt-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di una data                                        *
      *    *-----------------------------------------------------------*
       stp-dat-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-dat-pls-999.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-stp-iit-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa data                                     *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-iit-dat-pls    to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-dat-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un numero progressivo nel formato s.aa.nnnnnn   *
      *    *-----------------------------------------------------------*
       stp-p06-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-p06-pls-999.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-stp-iit-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa numero progressivo                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      06                   to   v-car                  .
           move      "<"                  to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-iit-p06-pls    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-p06-pls-999.
           exit.

      *    *===========================================================*
      *    * Stampa di un campo alfanumerico di 50 caratteri           *
      *    *-----------------------------------------------------------*
       stp-a50-pls-000.
      *              *-------------------------------------------------*
      *              * Test se almeno una riga residua, con eventuale  *
      *              * reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-tst-001-000      thru stp-tst-001-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-a50-pls-999.
      *              *-------------------------------------------------*
      *              * Interlinea                                      *
      *              *-------------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Prompt                                          *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-stp-iit-pmt-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *              *-------------------------------------------------*
      *              * Stampa campo alfanumerico                       *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      50                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      30                   to   v-pos                  .
           move      w-stp-iit-a50-pls    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       stp-a50-pls-999.
           exit.

      *    *===========================================================*
      *    * Test se almeno una riga residua, con eventuale reintesta- *
      *    * zione                                                     *
      *    *-----------------------------------------------------------*
       stp-tst-001-000.
      *              *-------------------------------------------------*
      *              * Se almeno una riga residua : uscita             *
      *              *-------------------------------------------------*
           if        v-res                >    1
                     go to stp-tst-001-999.
      *              *-------------------------------------------------*
      *              * Reintestazione                                  *
      *              *-------------------------------------------------*
           perform   stp-int-pag-000      thru stp-int-pag-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-tst-001-999.
       stp-tst-001-999.
           exit.

      *    *===========================================================*
      *    * Intestazione per interrogazione                           *
      *    *-----------------------------------------------------------*
       stp-int-pag-000.
      *              *-------------------------------------------------*
      *              * Subroutine di avanzamento pagina                *
      *              *-------------------------------------------------*
           perform   qry-pag-adv-000      thru qry-pag-adv-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to stp-int-pag-999.
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
           move      w-des-tit-pgm-ovy    to   v-alf                  .
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
       stp-int-pag-999.
           exit.

      *    *===========================================================*
      *    * Interrogazione : Function-keys previste in Mark-points    *
      *    *-----------------------------------------------------------*
       qry-det-fky-000.
      *              *-------------------------------------------------*
      *              * Nessuna function key                            *
      *              *-------------------------------------------------*
           move      spaces               to   v-pfk(01)              .
           move      spaces               to   v-pfk(02)              .
           move      spaces               to   v-pfk(03)              .
           move      spaces               to   v-pfk(04)              .
           move      spaces               to   v-pfk(05)              .
           move      spaces               to   v-pfk(06)              .
           move      spaces               to   v-pfk(07)              .
           move      spaces               to   v-pfk(08)              .
           move      spaces               to   v-pfk(09)              .
           move      spaces               to   v-pfk(10)              .
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
       qry-trt-fun-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [cli]                            *
      *    *-----------------------------------------------------------*
       let-arc-cli-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-cli-flg      .
       let-arc-cli-100.
      *              *-------------------------------------------------*
      *              * Se codice cliente a zero                        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-let-arc-cli-cod    not  = ,zero
                     go to let-arc-cli-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione ragione sociale             *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-cli-rag      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione indirizzo                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-cli-via      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione localita'                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-let-arc-cli-loc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-cli-999.
       let-arc-cli-200.
      *              *-------------------------------------------------*
      *              * Lettura per codice cliente                      *
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
       let-arc-cli-300.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-cli-400.
      *                  *---------------------------------------------*
      *                  * Ragione sociale ad errore                   *
      *                  *---------------------------------------------*
           move      all   "."            to   w-let-arc-cli-rag      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione indirizzo                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-cli-via      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione localita'                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-let-arc-cli-loc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-cli-999.
       let-arc-cli-400.
      *              *-------------------------------------------------*
      *              * Estrazione valori dal record                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      rf-cli-rag-soc       to   w-let-arc-cli-rag      .
      *                  *---------------------------------------------*
      *                  * Indirizzo                                   *
      *                  *---------------------------------------------*
           move      rf-cli-via-cli       to   w-let-arc-cli-via      .
      *                  *---------------------------------------------*
      *                  * Localita'                                   *
      *                  *---------------------------------------------*
           move      rf-cli-loc-cli       to   w-let-arc-cli-loc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-cli-999.
       let-arc-cli-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio [fnt]                            *
      *    *-----------------------------------------------------------*
       let-arc-fnt-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione flag di uscita                  *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-fnt-flg      .
       let-arc-fnt-100.
      *              *-------------------------------------------------*
      *              * Se codice fornitore a zero                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        w-let-arc-fnt-cod    not  = ,zero
                     go to let-arc-fnt-200.
      *                  *---------------------------------------------*
      *                  * Normalizzazione ragione sociale             *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-fnt-rag      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione indirizzo                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-fnt-via      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione localita'                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-let-arc-fnt-loc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-fnt-999.
       let-arc-fnt-200.
      *              *-------------------------------------------------*
      *              * Lettura per codice fornitore                    *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
           move      w-let-arc-fnt-cod    to   rf-fnt-cod-fnt         .
           move      "pgm/cge/fls/ioc/obj/ioffnt"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-fnt                 .
       let-arc-fnt-300.
      *              *-------------------------------------------------*
      *              * Se record non esistente                         *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test                                        *
      *                  *---------------------------------------------*
           if        f-sts                =    e-not-err
                     go to let-arc-fnt-400.
      *                  *---------------------------------------------*
      *                  * Ragione sociale ad errore                   *
      *                  *---------------------------------------------*
           move      all   "."            to   w-let-arc-fnt-rag      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione indirizzo                   *
      *                  *---------------------------------------------*
           move      spaces               to   w-let-arc-fnt-via      .
      *                  *---------------------------------------------*
      *                  * Normalizzazione localita'                   *
      *                  *---------------------------------------------*
           move      zero                 to   w-let-arc-fnt-loc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-fnt-999.
       let-arc-fnt-400.
      *              *-------------------------------------------------*
      *              * Estrazione valori dal record                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Ragione sociale                             *
      *                  *---------------------------------------------*
           move      rf-fnt-rag-soc       to   w-let-arc-fnt-rag      .
      *                  *---------------------------------------------*
      *                  * Indirizzo                                   *
      *                  *---------------------------------------------*
           move      rf-fnt-via-fnt       to   w-let-arc-fnt-via      .
      *                  *---------------------------------------------*
      *                  * Localita'                                   *
      *                  *---------------------------------------------*
           move      rf-fnt-loc-fnt       to   w-let-arc-fnt-loc      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     let-arc-fnt-999.
       let-arc-fnt-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [gxn]                             *
      *    *-----------------------------------------------------------*
       let-arc-gxn-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxn-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-gxn-cod    =    spaces
                     go to let-arc-gxn-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODNAZ    "         to   f-key                  .
           move      w-let-arc-gxn-cod    to   rf-gxn-cod-naz         .
           move      "pgm/geo/fls/ioc/obj/iofgxn"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxn                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-gxn-400.
       let-arc-gxn-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-gxn-des-naz       to   w-let-arc-gxn-des      .
           move      rf-gxn-snx-cee       to   w-let-arc-gxn-cee      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-gxn-999.
       let-arc-gxn-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-gxn-flg      .
           move      all   "."            to   w-let-arc-gxn-des      .
           move      spaces               to   w-let-arc-gxn-cee      .
           go to     let-arc-gxn-999.
       let-arc-gxn-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxn-des      .
           move      spaces               to   w-let-arc-gxn-cee      .
       let-arc-gxn-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura tabella [gxp]                             *
      *    *-----------------------------------------------------------*
       let-arc-gxp-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxp-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice a spazi                          *
      *              *-------------------------------------------------*
           if        w-let-arc-gxp-cod    =    spaces
                     go to let-arc-gxp-500.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODPRV    "         to   f-key                  .
           move      w-let-arc-gxp-cod    to   rf-gxp-cod-prv         .
           move      "pgm/geo/fls/ioc/obj/iofgxp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-gxp                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-gxp-400.
       let-arc-gxp-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-gxp-des-prv       to   w-let-arc-gxp-des      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-gxp-999.
       let-arc-gxp-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-gxp-flg      .
           move      all   "."            to   w-let-arc-gxp-des      .
           go to     let-arc-gxp-999.
       let-arc-gxp-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-gxp-des      .
       let-arc-gxp-999.
           exit.

      *    *===========================================================*
      *    * Lettura tabella tipi operazione per gestione Iva intra-   *
      *    * comunitaria                                               *
      *    *-----------------------------------------------------------*
           copy      "pgm/iic/prg/cpy/atopiic0.rtn"                   .

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

