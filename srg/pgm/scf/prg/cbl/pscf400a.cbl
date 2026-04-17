       Identification Division.
       Program-Id.                                 pscf400a           .
      *================================================================*
      *                                                                *
      * Catalogo:          Sistema applicativo:    pgm                 *
      *                        Area gestionale:    scf                 *
      *                                Settore:    glo                 *
      *                                   Fase:    scf400              *
      *                    ------------------------------------------- *
      *                     Versione originale:    001 del 03/12/93    *
      *                       Ultima revisione:    NdK del 05/08/19    *
      *                    ------------------------------------------- *
      *                                 Autore:    Nicola de Kunovich  *
      *================================================================*
      *                                                                *
      * Descrizione pgm:   Analisi scadenze fornitori globale          *
      *                                                                *
      *                    Sottoprogramma per esecuzione funzione di : *
      *                                                                *
      *                    Lista e riepilogo scadenze                  *
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
      *        * [sfs]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfs"                          .
      *        *-------------------------------------------------------*
      *        * [sfp]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfp"                          .
      *        *-------------------------------------------------------*
      *        * [sfa]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/scf/fls/rec/rfsfa"                          .
      *        *-------------------------------------------------------*
      *        * [dcf]                                                 *
      *        *-------------------------------------------------------*
           copy      "pgm/dcf/fls/rec/rfdcf"                          .

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

      *    *===========================================================*
      *    * Work-area richieste per interrogazione                    *
      *    *-----------------------------------------------------------*
       01  rr.
      *        *-------------------------------------------------------*
      *        * Data di riferimento per l'analisi                     *
      *        *-------------------------------------------------------*
           05  rr-dat-rfa                 pic  9(07)                  .
      *        *-------------------------------------------------------*
      *        * Tipo di scadenze da includere                         *
      *        *                                                       *
      *        * - 01 : Tutte                                          *
      *        * - 02 : Rim.Dir.,Bonif.,C/C Post.                      *
      *        * - 03 : Scadenze elettroniche                          *
      *        * - 04 : Ric.Banc.,Tratte,Paghero'                      *
      *        * - 05 : Solo Rimesse Dirette                           *
      *        * - 06 : Solo Incassi Elettronici                       *
      *        * - 07 : Solo Ri.Ba.                                    *
      *        * - 08 : Solo C.d.O.                                    *
      *        * - 09 : Solo M.Av.                                     *
      *        * - 10 : Solo R.I.D.                                    *
      *        * - 11 : Solo Bonifici Bancari                          *
      *        * - 12 : Solo C/C Postali                               *
      *        * - 13 : Solo Ricevute Bancarie                         *
      *        * - 14 : Solo Tratte                                    *
      *        * - 15 : Solo Paghero'                                  *
      *        *-------------------------------------------------------*
           05  rr-tip-scd                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Selezione su status scadenze                          *
      *        *                                                       *
      *        * - 01 : Sia a scadere che scadute                      *
      *        * - 02 : Solo quelle a scadere                          *
      *        * - 03 : Solo quelle scadute                            *
      *        * - 04 : Solo quelle in pagamento                       *
      *        * - 05 : Solo quelle pagate                             *
      *        * - 06 : Solo quelle bloccate                           *
      *        * - 07 : Escluse quelle bloccate                        *
      *        *-------------------------------------------------------*
           05  rr-sts-scd                 pic  9(02)                  .
      *        *-------------------------------------------------------*
      *        * Tipo evidenziazione                                   *
      *        *                                                       *
      *        * - 01 : Sia la lista che il riepilogo finale           *
      *        * - 02 : Solo la lista delle scadenze                   *
      *        * - 03 : Solo il riepilogo finale                       *
      *        *-------------------------------------------------------*
           05  rr-tip-evd                 pic  9(02)                  .

      *    *===========================================================*
      *    * Work per subroutines di Let                               *
      *    *-----------------------------------------------------------*
       01  w-let.
      *        *-------------------------------------------------------*
      *        * Work per Let su archivio [dcf]                        *
      *        *-------------------------------------------------------*
           05  w-let-arc-dcf.
               10  w-let-arc-dcf-flg      pic  x(01)                  .
               10  w-let-arc-dcf-tle      pic  x(01)                  .
               10  w-let-arc-dcf-cod      pic  9(07)                  .
               10  w-let-arc-dcf-dpz      pic  x(04)                  .
               10  w-let-arc-dcf-rag      pic  x(40)                  .
               10  w-let-arc-dcf-via      pic  x(40)                  .
               10  w-let-arc-dcf-loc      pic  x(40)                  .
               10  w-let-arc-dcf-abi      pic  9(05)                  .
               10  w-let-arc-dcf-cab      pic  9(05)                  .

      *    *===========================================================*
      *    * Work per subroutines di Det                               *
      *    *-----------------------------------------------------------*
       01  w-det.
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
      *            * - STO    : Storno                                 *
      *            * - RDP    : Richiesta di pagamento                 *
      *            * - PAG    : Pagamento                              *
      *            *---------------------------------------------------*
               10  w-det-srd-scf-suo      pic  x(03)                  .
      *        *-------------------------------------------------------*
      *        * Work per determinazione di una data maggiorata fino   *
      *        * ad un massimo di 30 giorni                            *
      *        *-------------------------------------------------------*
           05  w-det-dat-mng.
      *            *---------------------------------------------------*
      *            * Data in input                                     *
      *            *---------------------------------------------------*
               10  w-det-dat-mng-dti      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Numero giorni di incremento : 00...30             *
      *            *---------------------------------------------------*
               10  w-det-dat-mng-ngi      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Data in output                                    *
      *            *---------------------------------------------------*
               10  w-det-dat-mng-dto      pic  9(07)                  .
      *            *---------------------------------------------------*
      *            * Comodo per una data con ridefinizioni necessarie  *
      *            *---------------------------------------------------*
               10  w-det-dat-mng-wdt      pic  9(07)                  .
               10  w-det-dat-mng-wdr redefines
                   w-det-dat-mng-wdt.
                   15  w-det-dat-mng-wds  pic  9(03)                  .
                   15  w-det-dat-mng-wdm  pic  9(02)                  .
                   15  w-det-dat-mng-wdg  pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Tabella giorni nei mesi e relativi comodi per il  *
      *            * calcolo del numero giorni di febbraio             *
      *            *---------------------------------------------------*
               10  w-det-dat-mng-wt0.
                   15  filler             pic  9(02)       value 31   .
                   15  filler             pic  9(02)       value 28   .
                   15  filler             pic  9(02)       value 31   .
                   15  filler             pic  9(02)       value 30   .
                   15  filler             pic  9(02)       value 31   .
                   15  filler             pic  9(02)       value 30   .
                   15  filler             pic  9(02)       value 31   .
                   15  filler             pic  9(02)       value 31   .
                   15  filler             pic  9(02)       value 30   .
                   15  filler             pic  9(02)       value 31   .
                   15  filler             pic  9(02)       value 30   .
                   15  filler             pic  9(02)       value 31   .
               10  w-det-dat-mng-wt1 redefines
                   w-det-dat-mng-wt0.
                   15  w-det-dat-mng-wt2
                               occurs 12  pic  9(02)                  .
               10  w-det-dat-mng-wt5      pic  9(04)                  .
               10  w-det-dat-mng-wt6      pic  9(04)                  .
               10  w-det-dat-mng-wt7      pic  9(04)                  .

      *    *===========================================================*
      *    * Work per accettazioni campi espansi                       *
      *    *-----------------------------------------------------------*
       01  w-exp.
      *        *-------------------------------------------------------*
      *        * Work per : Tipi scadenza da includere                 *
      *        *-------------------------------------------------------*
           05  w-exp-tip-scd.
               10  w-exp-tip-scd-num      pic  9(02)       value 15   .
               10  w-exp-tip-scd-lun      pic  9(02)       value 25   .
               10  w-exp-tip-scd-tbl.
                   15  filler             pic  x(25) value
                            "Tutte                    "               .
                   15  filler             pic  x(25) value
                            "Rim.Dir.,Bonif.,C/C Post."               .
                   15  filler             pic  x(25) value
                            "Scadenze elettroniche    "               .
                   15  filler             pic  x(25) value
                            "Ric.Banc.,Tratte,Paghero'"               .
                   15  filler             pic  x(25) value
                            "Solo Rimesse Dirette     "               .
                   15  filler             pic  x(25) value
                            "Solo Incassi Elettronici "               .
                   15  filler             pic  x(25) value
                            "Solo Ri.Ba.              "               .
                   15  filler             pic  x(25) value
                            "Solo C.d.O.              "               .
                   15  filler             pic  x(25) value
                            "Solo M.Av.               "               .
                   15  filler             pic  x(25) value
                            "Solo R.I.D.              "               .
                   15  filler             pic  x(25) value
                            "Solo Bonifici Bancari    "               .
                   15  filler             pic  x(25) value
                            "Solo C/C Postali         "               .
                   15  filler             pic  x(25) value
                            "Solo Ricevute Bancarie   "               .
                   15  filler             pic  x(25) value
                            "Solo Tratte              "               .
                   15  filler             pic  x(25) value
                            "Solo Paghero'            "               .
      *        *-------------------------------------------------------*
      *        * Work per : Selezione su status scadenze               *
      *        *-------------------------------------------------------*
           05  w-exp-sts-scd.
               10  w-exp-sts-scd-num      pic  9(02)       value 07   .
               10  w-exp-sts-scd-lun      pic  9(02)       value 25   .
               10  w-exp-sts-scd-tbl.
                   15  filler             pic  x(25) value
                            "Tutte                    "               .
                   15  filler             pic  x(25) value
                            "solo A scadere           "               .
                   15  filler             pic  x(25) value
                            "solo Scadute             "               .
                   15  filler             pic  x(25) value
                            "solo In pagamento        "               .
                   15  filler             pic  x(25) value
                            "solo Pagate              "               .
                   15  filler             pic  x(25) value
                            "solo Bloccate            "               .
                   15  filler             pic  x(25) value
                            "Escluse le bloccate      "               .
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
                            "solo la Lista delle scadenze            ".
                   15  filler             pic  x(40) value
                            "solo il Riepilogo finale                ".

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
      *        * - 001 : Per totali per scadenza                       *
      *        * - 002 : Per lista scadenze                            *
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
                   15  filler             pic  x(77)                  .
      *            *---------------------------------------------------*
      *            * Definizione per mark-point tipo : 001             *
      *            *---------------------------------------------------*
               10  w-mpn-att-001 redefines
                   w-mpn-att-000.
      *                *-----------------------------------------------*
      *                * Tipo scadenza                                 *
      *                *-----------------------------------------------*
                   15  w-mpn-tsc-001      pic  9(02)                  .
      *                *-----------------------------------------------*
      *                * Filler riempitivo                             *
      *                *-----------------------------------------------*
                   15  filler             pic  x(75)                  .
      *            *---------------------------------------------------*
      *            * Definizione per mark-point tipo : 002             *
      *            *---------------------------------------------------*
               10  w-mpn-att-002 redefines
                   w-mpn-att-000.
      *                *-----------------------------------------------*
      *                * Numero scadenza                               *
      *                *-----------------------------------------------*
                   15  w-mpn-num-002      pic  9(11)                  .
      *                *-----------------------------------------------*
      *                * Codice fornitore                              *
      *                *-----------------------------------------------*
                   15  w-mpn-cod-002      pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Dipendenza fornitore                          *
      *                *-----------------------------------------------*
                   15  w-mpn-dpz-002      pic  x(04)                  .
      *                *-----------------------------------------------*
      *                * Ragione sociale fornitore                     *
      *                *-----------------------------------------------*
                   15  w-mpn-rag-002      pic  x(40)                  .
      *                *-----------------------------------------------*
      *                * Filler riempitivo                             *
      *                *-----------------------------------------------*
                   15  filler             pic  x(15)                  .

      *    *===========================================================*
      *    * Work per tabelle                                          *
      *    *-----------------------------------------------------------*
       01  w-tbl.
      *        *-------------------------------------------------------*
      *        * Tabella tipi pagamento                                *
      *        *-------------------------------------------------------*
           05  w-tbl-tpg.
               10  w-tbl-tpg-001.
                   15  filler             pic  x(04) value
                              "RD  "                                  .
                   15  filler             pic  x(04) value
                              "IE  "                                  .
                   15  filler             pic  x(04) value
                              "RIBA"                                  .
                   15  filler             pic  x(04) value
                              "CDO "                                  .
                   15  filler             pic  x(04) value
                              "MAV "                                  .
                   15  filler             pic  x(04) value
                              "RID "                                  .
                   15  filler             pic  x(04) value
                              "BB  "                                  .
                   15  filler             pic  x(04) value
                              "CCP "                                  .
                   15  filler             pic  x(04) value
                              "RB  "                                  .
                   15  filler             pic  x(04) value
                              "TR  "                                  .
                   15  filler             pic  x(04) value
                              "PC  "                                  .
               10  w-tbl-tpg-002 redefines
                   w-tbl-tpg-001.
                   15  w-tbl-tpg-ele occurs 11
                                          pic  x(04)                  .

      *    *===========================================================*
      *    * Work-area per routine qry-trt-fun-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-qry-trt-fun.
           05  w-qry-trt-fun-x80          pic  x(80)                  .
           05  w-qry-trt-fun-pnt          pic  9(03)                  .

      *    *===========================================================*
      *    * Work-area per ripristino Start                            *
      *    *-----------------------------------------------------------*
       01  w-rip-str.
      *        *-------------------------------------------------------*
      *        * Work-area per ripristino Start su [sfs]               *
      *        *-------------------------------------------------------*
           05  w-rip-str-sfs.
      *            *---------------------------------------------------*
      *            * Flag di End-of-file                               *
      *            *---------------------------------------------------*
               10  w-rip-str-sfs-eof      pic  x(01)                  .
      *            *---------------------------------------------------*
      *            * Parametri di Start                                *
      *            *---------------------------------------------------*
               10  w-rip-str-sfs-dts      pic  9(07)                  .
               10  w-rip-str-sfs-num      pic  9(11)                  .

      *    *===========================================================*
      *    * Work-area per esecuzione analisi scadenze fornitori       *
      *    *-----------------------------------------------------------*
       01  w-ana-scf.
      *        *-------------------------------------------------------*
      *        * Totalizzazioni per tipo scadenza                      *
      *        *-------------------------------------------------------*
           05  w-ana-scf-tts.
      *            *---------------------------------------------------*
      *            * 11 elementi, per ogni tipo scadenza               *
      *            *  - 01 : Rimesse Dirette                           *
      *            *  - 02 : Incassi Elettronici                       *
      *            *  - 03 : Ri.Ba.                                    *
      *            *  - 04 : C.d.O.                                    *
      *            *  - 05 : M.Av.                                     *
      *            *  - 06 : R.I.D.                                    *
      *            *  - 07 : Bonifici Bancari                          *
      *            *  - 08 : C/C Postali                               *
      *            *  - 09 : Ricevute Bancarie                         *
      *            *  - 10 : Tratte                                    *
      *            *  - 11 : Paghero'                                  *
      *            *---------------------------------------------------*
               10  w-ana-scf-tts-ele  occurs 11.
      *                *-----------------------------------------------*
      *                * Totale importo                                *
      *                *-----------------------------------------------*
                   15  w-ana-scf-tts-tot  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Totale numero scadenze                        *
      *                *-----------------------------------------------*
                   15  w-ana-scf-tts-num  pic  9(07)                  .
      *                *-----------------------------------------------*
      *                * Totale 'A scadere'                            *
      *                *-----------------------------------------------*
                   15  w-ana-scf-tts-asc  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Totale 'Scaduto'                              *
      *                *-----------------------------------------------*
                   15  w-ana-scf-tts-sca  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Totale 'In pagamento'                         *
      *                *-----------------------------------------------*
                   15  w-ana-scf-tts-ipg  pic s9(11)                  .
      *                *-----------------------------------------------*
      *                * Totale 'Pagate'                               *
      *                *-----------------------------------------------*
                   15  w-ana-scf-tts-pag  pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Indici per scansione su elementi                  *
      *            *---------------------------------------------------*
               10  w-ana-scf-tts-i01      pic  9(02)                  .
               10  w-ana-scf-tts-i02      pic  9(02)                  .
      *            *---------------------------------------------------*
      *            * Comodi per calcolo                                *
      *            *---------------------------------------------------*
               10  w-ana-scf-tts-s11      pic s9(11)                  .
      *            *---------------------------------------------------*
      *            * Totale 'Bloccate'                                 *
      *            *---------------------------------------------------*
               10  w-ana-scf-tts-blo      pic s9(11)                  .

      *    *===========================================================*
      *    * Work-area per routine qry-liv-det-000/999                 *
      *    *-----------------------------------------------------------*
       01  w-qry-liv-det.
      *        *-------------------------------------------------------*
      *        * Work per importo scadenza                             *
      *        *-------------------------------------------------------*
           05  w-qry-liv-det-wis          pic s9(11)                  .

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
      *    * Work-area per personalizzazioni                           *
      *    *-----------------------------------------------------------*
       01  w-prs.
      *        *-------------------------------------------------------*
      *        * Personalizzazioni per gestione scadenze fornitori     *
      *        *-------------------------------------------------------*
           05  w-prs-scf.
      *            *---------------------------------------------------*
      *            * Numero giorni di ritardo medi affinche' un paga-  *
      *            * mento pervenga al fornitore (01...30)             *
      *            *---------------------------------------------------*
               10  w-prs-scf-grm-ppf      pic  9(02)                  .

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
                                               w-prs
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
      *              * Memorizzazione sottoprogramma in attivita'      *
      *              *-------------------------------------------------*
           move      "AFGLER    "         to   w-spg-alf-gat          .
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
           move      "AFGLER    "         to   w-spg-alf-gat          .
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
      *    * Preparazione tipo funzionamento programma                 *
      *    *-----------------------------------------------------------*
       pre-tip-fun-000.
      *              *-------------------------------------------------*
      *              * Si/No richieste ad utente                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su esito lettura variabili di i.p.c.   *
      *                  *---------------------------------------------*
           if        w-ipc-dat-rfa-snx    =    "S"
                     move  "N"            to   w-cnt-fun-snx-ric
           else      move  "S"            to   w-cnt-fun-snx-ric      .
       pre-tip-fun-200.
      *              *-------------------------------------------------*
      *              * Si/No funzionamento ciclico                     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test su esito lettura variabili di i.p.c.   *
      *                  *---------------------------------------------*
           if        w-ipc-dat-rfa-snx    =    "S"
                     move  "N"            to   w-cnt-fun-snx-cic
           else      move  "S"            to   w-cnt-fun-snx-cic      .
       pre-tip-fun-400.
      *              *-------------------------------------------------*
      *              * Si/No funzionamento automatico : No             *
      *              *-------------------------------------------------*
           move      "N"                  to   w-cnt-fun-snx-aut      .
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
       rou-opn-fls-999.
           exit.

      *    *===========================================================*
      *    * Close files per richieste                                 *
      *    *-----------------------------------------------------------*
       rou-cls-fls-000.
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
       acc-ric-sel-200.
      *                  *---------------------------------------------*
      *                  * Tipi scadenza da includere                  *
      *                  *---------------------------------------------*
           perform   acc-tip-scd-000      thru acc-tip-scd-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-100.
       acc-ric-sel-300.
      *                  *---------------------------------------------*
      *                  * Selezione su status scadenze                *
      *                  *---------------------------------------------*
           perform   acc-sts-scd-000      thru acc-sts-scd-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-200.
       acc-ric-sel-400.
      *                  *---------------------------------------------*
      *                  * Selezione su tipo evidenziazione            *
      *                  *---------------------------------------------*
           perform   acc-tip-evd-000      thru acc-tip-evd-999        .
           if        w-cnt-acc-ric-sel    not  = spaces
                     go to acc-ric-sel-999.
           if        v-key                =    "UP  "
                     go to acc-ric-sel-300.
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
      *              * Prompt tipi scadenza da includere               *
      *              *-------------------------------------------------*
           perform   pmt-tip-scd-000      thru pmt-tip-scd-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione tipo scadenza da includere      *
      *              *-------------------------------------------------*
           perform   vis-tip-scd-000      thru vis-tip-scd-999        .
      *              *-------------------------------------------------*
      *              * Prompt per selezione status scadenze            *
      *              *-------------------------------------------------*
           perform   pmt-sts-scd-000      thru pmt-sts-scd-999        .
      *              *-------------------------------------------------*
      *              * Visualizzazione selezione status scadenze       *
      *              *-------------------------------------------------*
           perform   vis-sts-scd-000      thru vis-sts-scd-999        .
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
           move      07                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Data riferimento analisi   :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-dat-rfa-999.
           exit.

      *    *===========================================================*
      *    * Prompt tipo scadenza da includere                         *
      *    *-----------------------------------------------------------*
       pmt-tip-scd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      10                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Tipi scadenza da includere :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-tip-scd-999.
           exit.

      *    *===========================================================*
      *    * Prompt per selezione status scadenze                      *
      *    *-----------------------------------------------------------*
       pmt-sts-scd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      12                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "Selezione su stato scadenze:"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       pmt-sts-scd-999.
           exit.

      *    *===========================================================*
      *    * Prompt per tipo evidenziazione                            *
      *    *-----------------------------------------------------------*
       pmt-tip-evd-000.
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      28                   to   v-car                  .
           move      14                   to   v-lin                  .
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
           move      07                   to   v-lin                  .
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
           move      07                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-dat-rfa           to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                        using  v                      .
       vis-dat-rfa-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Tipi scadenza da includere *
      *    *-----------------------------------------------------------*
       acc-tip-scd-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-tip-scd-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-scd-lun    to   v-car                  .
           move      w-exp-tip-scd-num    to   v-ldt                  .
           move      spaces               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-scd-tbl    to   v-txt                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-tip-scd           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-tip-scd-999.
       acc-tip-scd-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-tip-scd             .
       acc-tip-scd-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non si sia in Up                        *
      *                  *---------------------------------------------*
           if        rr-tip-scd           not  = zero
                     go to acc-tip-scd-600.
           if        v-key                =    "UP  "
                     go to acc-tip-scd-600
           else      go to acc-tip-scd-100.
       acc-tip-scd-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-tip-scd-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-tip-scd-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-tip-scd-100.
       acc-tip-scd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Tipi scadenza da includere        *
      *    *-----------------------------------------------------------*
       vis-tip-scd-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-tip-scd-lun    to   v-car                  .
           move      w-exp-tip-scd-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-scd-tbl    to   v-txt                  .
           move      10                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-tip-scd           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tip-scd-999.
           exit.

      *    *===========================================================*
      *    * Accettazione campo selezione : Selezione su status sca-   *
      *    * denza                                                     *
      *    *-----------------------------------------------------------*
       acc-sts-scd-000.
      *              *-------------------------------------------------*
      *              * Pre-accettazione                                *
      *              *-------------------------------------------------*
       acc-sts-scd-100.
      *              *-------------------------------------------------*
      *              * Accettazione valore                             *
      *              *-------------------------------------------------*
           move      "AC"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sts-scd-lun    to   v-car                  .
           move      w-exp-sts-scd-num    to   v-ldt                  .
           move      "TASIPBE#"           to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-sts-scd-tbl    to   v-txt                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      "UP  "               to   v-pfk (01)             .
           move      "DOWN"               to   v-pfk (02)             .
           move      "DO  "               to   v-pfk (05)             .
           move      rr-sts-scd           to   v-num                  .
           perform   exe-acc-cmp-000      thru exe-acc-cmp-999        .
      *              *-------------------------------------------------*
      *              * Se Exit                                         *
      *              *-------------------------------------------------*
           if        v-key                =    "EXIT"
                     move  "E"            to   w-cnt-acc-ric-sel
                     go to acc-sts-scd-999.
       acc-sts-scd-200.
      *              *-------------------------------------------------*
      *              * Valore impostato in campo di destinazione       *
      *              *-------------------------------------------------*
           move      v-num                to   rr-sts-scd             .
       acc-sts-scd-400.
      *              *-------------------------------------------------*
      *              * Controllo valore impostato                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se valore a zero : reimpostazione, a meno   *
      *                  * che non si sia in Up                        *
      *                  *---------------------------------------------*
           if        rr-sts-scd           not  = zero
                     go to acc-sts-scd-600.
           if        v-key                =    "UP  "
                     go to acc-sts-scd-600
           else      go to acc-sts-scd-100.
       acc-sts-scd-600.
      *              *-------------------------------------------------*
      *              * Dipendenze dall'impostazione                    *
      *              *-------------------------------------------------*
       acc-sts-scd-800.
      *              *-------------------------------------------------*
      *              * Se Do                                           *
      *              *-------------------------------------------------*
           if        v-key                =    "DO  "
                     perform tdo-ric-sel-000
                                          thru tdo-ric-sel-999
                     if      w-cnt-tdo-ric-flg
                                          =    spaces
                             move  "S"    to   w-cnt-acc-ric-sel
                             go to acc-sts-scd-999
                     else    move  spaces to   w-cnt-tdo-ric-flg
                             go to acc-sts-scd-100.
       acc-sts-scd-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione campo : Selezione su status scadenza      *
      *    *-----------------------------------------------------------*
       vis-sts-scd-000.
           move      "DS"                 to   v-ope                  .
           move      "E"                  to   v-tip                  .
           move      w-exp-sts-scd-lun    to   v-car                  .
           move      w-exp-sts-scd-num    to   v-ldt                  .
           move      spaces               to   v-edm                  .
           move      w-exp-sts-scd-tbl    to   v-txt                  .
           move      12                   to   v-lin                  .
           move      30                   to   v-pos                  .
           move      rr-sts-scd           to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-sts-scd-999.
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
           move      "SLR#"               to   v-msk                  .
           move      spaces               to   v-edm                  .
           move      w-exp-tip-evd-tbl    to   v-txt                  .
           move      14                   to   v-lin                  .
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
           move      14                   to   v-lin                  .
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
      *              * Tipi scadenza da includere                      *
      *              *-------------------------------------------------*
           if        rr-tip-scd           =    zero
                     move  01             to   rr-tip-scd             .
      *              *-------------------------------------------------*
      *              * Selezione status scadenze                       *
      *              *-------------------------------------------------*
           if        rr-sts-scd           =    zero
                     move  01             to   rr-sts-scd             .
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
      *                  * Tipi scadenza da includere                  *
      *                  *---------------------------------------------*
           move      zero                 to   rr-tip-scd             .
      *                  *---------------------------------------------*
      *                  * Selezione status scadenze                   *
      *                  *---------------------------------------------*
           move      zero                 to   rr-sts-scd             .
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
      *                  * Tipi scadenza da includere                  *
      *                  *---------------------------------------------*
           move      01                   to   rr-tip-scd             .
      *                  *---------------------------------------------*
      *                  * Selezione status scadenze                   *
      *                  *---------------------------------------------*
           move      zero                 to   rr-sts-scd             .
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
      *              * Normalizzazione flag di End-of-file relativo    *
      *              * alla ripristino Start                           *
      *              *-------------------------------------------------*
           move      spaces               to   w-rip-str-sfs-eof      .
      *              *-------------------------------------------------*
      *              * Inizializzazione totali per tipo scadenza       *
      *              *-------------------------------------------------*
           perform   tot-tts-ini-000      thru tot-tts-ini-999        .
       qry-str-ini-500.
      *              *-------------------------------------------------*
      *              * Start su [sfs] per data scadenza                *
      *              *-------------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DTSSCF    "         to   f-key                  .
           move      zero                 to   rf-sfs-dts-scf         .
           move      zero                 to   rf-sfs-num-scf         .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
      *                  *---------------------------------------------*
      *                  * Se Start errata                             *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-str-ini-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     qry-str-ini-999.
       qry-str-ini-900.
      *              *-------------------------------------------------*
      *              * Flag di uscita con errore                       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-qry-flg-sub      .
      *              *-------------------------------------------------*
      *              * Flag di End-of-file relativo al ripristino del- *
      *              * la Start                                        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-rip-str-sfs-eof      .
       qry-str-ini-999.
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
      *              * Indicatore di programma in esecuzione           *
      *              *-------------------------------------------------*
           move      "IE"                 to   s-ope                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Next su [sfs] per data scadenza                 *
      *              *-------------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
      *                  *---------------------------------------------*
      *                  * Se fine file                                *
      *                  *---------------------------------------------*
           if        f-sts                not  = e-not-err
                     go to qry-let-seq-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     qry-let-seq-999.
       qry-let-seq-900.
      *              *-------------------------------------------------*
      *              * Flag di uscita con errore                       *
      *              *-------------------------------------------------*
           move      "#"                  to   w-cnt-qry-flg-sub      .
      *              *-------------------------------------------------*
      *              * Flag di End-of-file relativo al ripristino del- *
      *              * la Start                                        *
      *              *-------------------------------------------------*
           move      "#"                  to   w-rip-str-sfs-eof      .
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
       qry-sel-rec-100.
      *              *-------------------------------------------------*
      *              * Selezione su tipo scadenza                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione del tipo scadenza    *
      *                  *---------------------------------------------*
           go to     qry-sel-rec-101
                     qry-sel-rec-102
                     qry-sel-rec-103
                     qry-sel-rec-104
                     qry-sel-rec-105
                     qry-sel-rec-106
                     qry-sel-rec-107
                     qry-sel-rec-108
                     qry-sel-rec-109
                     qry-sel-rec-110
                     qry-sel-rec-111
                     qry-sel-rec-112
                     qry-sel-rec-113
                     qry-sel-rec-114
                     qry-sel-rec-115
                     depending            on   rr-tip-scd             .
       qry-sel-rec-101.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Tutte                       *
      *                  *---------------------------------------------*
           go to     qry-sel-rec-200.
       qry-sel-rec-102.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Rim.Dir.,Bonif.,C/C Post.   *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 01 and
                     rf-sfs-tip-scf       not  = 07 and
                     rf-sfs-tip-scf       not  = 08
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-200.
       qry-sel-rec-103.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Scadenze elettroniche       *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 02 and
                     rf-sfs-tip-scf       not  = 03 and
                     rf-sfs-tip-scf       not  = 04 and
                     rf-sfs-tip-scf       not  = 05 and
                     rf-sfs-tip-scf       not  = 06
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-200.
       qry-sel-rec-104.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Ric.Banc.,Tratte,Paghero'   *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 09 and
                     rf-sfs-tip-scf       not  = 10 and
                     rf-sfs-tip-scf       not  = 11
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-200.
       qry-sel-rec-105.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo Rimesse Dirette        *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 01
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-200.
       qry-sel-rec-106.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo Incassi Elettronici    *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 02
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-200.
       qry-sel-rec-107.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo Ri.Ba.                 *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 03
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-200.
       qry-sel-rec-108.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo C.d.O.                 *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 04
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-200.
       qry-sel-rec-109.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo M.Av.                  *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 05
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-200.
       qry-sel-rec-110.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo R.I.D.                 *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 06
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-200.
       qry-sel-rec-111.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo Bonifici Bancari       *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 07
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-200.
       qry-sel-rec-112.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo C/C Postali            *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 08
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-200.
       qry-sel-rec-113.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo Ricevute Bancarie      *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 09
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-200.
       qry-sel-rec-114.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo Tratte                 *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 10
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-200.
       qry-sel-rec-115.
      *                  *---------------------------------------------*
      *                  * Tipo scadenza : Solo Paghero'               *
      *                  *---------------------------------------------*
           if        rf-sfs-tip-scf       not  = 11
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-200.
       qry-sel-rec-200.
      *              *-------------------------------------------------*
      *              * Determinazione status scadenza                  *
      *              *-------------------------------------------------*
           move      rr-dat-rfa           to   w-det-srd-scf-drd      .
           perform   det-srd-scf-000      thru det-srd-scf-999        .
      *              *-------------------------------------------------*
      *              * Test se scadenza ancora aperta                  *
      *              *-------------------------------------------------*
           if        w-det-srd-scf-sts    not  = "A"
                     go to qry-sel-rec-900.
       qry-sel-rec-300.
      *              *-------------------------------------------------*
      *              * Selezione su status scadenza                    *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Deviazione in funzione della selezione su   *
      *                  * status scadenze                             *
      *                  *---------------------------------------------*
           go to     qry-sel-rec-310
                     qry-sel-rec-320
                     qry-sel-rec-330
                     qry-sel-rec-340
                     qry-sel-rec-350
                     qry-sel-rec-360
                     qry-sel-rec-370
                     depending            on   rr-sts-scd             .
       qry-sel-rec-310.
      *                  *---------------------------------------------*
      *                  * Selezione su status scadenza : Tutte        *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Nessuna selezione                       *
      *                      *-----------------------------------------*
           go to     qry-sel-rec-400.
       qry-sel-rec-320.
      *                  *---------------------------------------------*
      *                  * Selezione su status scadenza : Solo quelle  *
      *                  * a scadere                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su ultima operazione effettuata    *
      *                      *-----------------------------------------*
           if        w-det-srd-scf-suo    not  = "REG"
                     go to qry-sel-rec-900.
      *                      *-----------------------------------------*
      *                      * Test su data scadenza                   *
      *                      *-----------------------------------------*
           if        rf-sfs-dts-scf       not  >  rr-dat-rfa
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-400.
       qry-sel-rec-330.
      *                  *---------------------------------------------*
      *                  * Selezione su status scadenza : Solo quelle  *
      *                  * scadute                                     *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su ultima operazione effettuata    *
      *                      *-----------------------------------------*
           if        w-det-srd-scf-suo    not  = "REG"
                     go to qry-sel-rec-900.
      *                      *-----------------------------------------*
      *                      * Test su data scadenza                   *
      *                      *-----------------------------------------*
           if        rf-sfs-dts-scf       >    rr-dat-rfa
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-400.
       qry-sel-rec-340.
      *                  *---------------------------------------------*
      *                  * Selezione su status scadenza : Solo quelle  *
      *                  * in pagamento                                *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su ultima operazione effettuata    *
      *                      *-----------------------------------------*
           if        w-det-srd-scf-suo    not  = "RDP"
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-400.
       qry-sel-rec-350.
      *                  *---------------------------------------------*
      *                  * Selezione su status scadenza : Solo quelle  *
      *                  * pagate                                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su ultima operazione effettuata    *
      *                      *-----------------------------------------*
           if        w-det-srd-scf-suo    not  = "PAG"
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-400.
       qry-sel-rec-360.
      *                  *---------------------------------------------*
      *                  * Selezione su status scadenza : Solo quelle  *
      *                  * bloccate                                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su apposito segnale                *
      *                      *-----------------------------------------*
           if        rf-sfs-snx-pbl       not  = "S"
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-400.
       qry-sel-rec-370.
      *                  *---------------------------------------------*
      *                  * Selezione su status scadenza : Escluse      *
      *                  * quelle bloccate                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Test su apposito segnale                *
      *                      *-----------------------------------------*
           if        rf-sfs-snx-pbl       =    "S"
                     go to qry-sel-rec-900.
           go to     qry-sel-rec-400.
       qry-sel-rec-400.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     qry-sel-rec-999.
       qry-sel-rec-900.
      *              *-------------------------------------------------*
      *              * Se selezione non superata                       *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Flag di uscita con errore                   *
      *                  *---------------------------------------------*
           move      "#"                  to   w-cnt-qry-flg-sub      .
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
      *              * Visualizzazione risultati analisi               *
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
           perform   vis-ris-ana-000      thru vis-ris-ana-999        .
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
      *              *-------------------------------------------------*
      *              * Preparazione work per importo scadenza          *
      *              *-------------------------------------------------*
           move      rf-sfs-imp-scf       to   w-qry-liv-det-wis      .
           if        rf-sfs-tip-ddr       =    11
                     multiply -1          by   w-qry-liv-det-wis      .
      *              *-------------------------------------------------*
      *              * Totalizzazioni per tipo scadenza                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da effettuare                       *
      *                  *---------------------------------------------*
           if        rr-tip-evd           not  = 01 and
                     rr-tip-evd           not  = 03
                     go to qry-liv-det-050.
      *                  *---------------------------------------------*
      *                  * Esecuzione                                  *
      *                  *---------------------------------------------*
           perform   tot-tts-agg-000      thru tot-tts-agg-999        .
       qry-liv-det-050.
      *              *-------------------------------------------------*
      *              * Stampa riga dettaglio                           *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Test se da effettuare                       *
      *                  *---------------------------------------------*
           if        rr-tip-evd           not  = 01 and
                     rr-tip-evd           not  = 02
                     go to qry-liv-det-999.
      *                  *---------------------------------------------*
      *                  * Test se righe residue sufficienti           *
      *                  *---------------------------------------------*
           if        v-res                >    3
                     go to qry-liv-det-200.
      *                  *---------------------------------------------*
      *                  * Intestazione pagina                         *
      *                  *---------------------------------------------*
           perform   int-pag-sta-000      thru int-pag-sta-999        .
      *                      *-----------------------------------------*
      *                      * Test se interruzione forzata            *
      *                      *-----------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to qry-liv-det-999.
       qry-liv-det-200.
      *                  *---------------------------------------------*
      *                  * Lettura archivio [dcf]                      *
      *                  *---------------------------------------------*
           if        rf-sfs-dpz-fnt       =    spaces
                     move  "C"            to   w-let-arc-dcf-tle
           else      move  "D"            to   w-let-arc-dcf-tle      .
           move      rf-sfs-cod-fnt       to   w-let-arc-dcf-cod      .
           move      rf-sfs-dpz-fnt       to   w-let-arc-dcf-dpz      .
           perform   let-arc-dcf-000      thru let-arc-dcf-999        .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Numero scadenza                             *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "P"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      spaces               to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      rf-sfs-num-scf       to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Tipo scadenza                               *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      04                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      14                   to   v-pos                  .
           move      w-tbl-tpg-ele
                    (rf-sfs-tip-scf)      to   v-alf                  .
      *                      *-----------------------------------------*
      *                      * Mark-Point                              *
      *                      *-----------------------------------------*
           move      "+"                  to   v-edm                  .
           move      002                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      rf-sfs-num-scf       to   w-mpn-num-002          .
           move      rf-sfs-cod-fnt       to   w-mpn-cod-002          .
           move      rf-sfs-dpz-fnt       to   w-mpn-dpz-002          .
           move      w-let-arc-dcf-rag    to   w-mpn-rag-002          .
           move      w-mpn                to   v-cnt                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Data scadenza                               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se data normale                         *
      *                      *-----------------------------------------*
           if        rf-sfs-dts-scf       =    zero
                     go to qry-liv-det-220.
           move      "PF"                 to   v-ope                  .
           move      "D"                  to   v-tip                  .
           move      v-lnr                to   v-lin                  .
           move      20                   to   v-pos                  .
           move      rf-sfs-dts-scf       to   v-dat                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
           go to     qry-liv-det-300.
       qry-liv-det-220.
      *                      *-----------------------------------------*
      *                      * Se data a vista                         *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      08                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      20                   to   v-pos                  .
           move      " A vista"           to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-300.
      *                  *---------------------------------------------*
      *                  * Importo scadenza                            *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Determinazione v-pos                    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Deviazione in funzione del tipo ul- *
      *                          * tima operazione effettuata          *
      *                          *-------------------------------------*
           if        w-det-srd-scf-suo    =    "REG"
                     go to qry-liv-det-310
           else if   w-det-srd-scf-suo    =    "RDP"
                     go to qry-liv-det-320
           else if   w-det-srd-scf-suo    =    "PAG"
                     go to qry-liv-det-330.
       qry-liv-det-310.
      *                          *-------------------------------------*
      *                          * Se ultima operazione effettuata :   *
      *                          * Registrazione                       *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test su data scadenza           *
      *                              *---------------------------------*
           if        rf-sfs-dts-scf       not  >  rr-dat-rfa
                     move  41             to   v-pos
           else      move  28             to   v-pos                  .
           go to     qry-liv-det-380.
       qry-liv-det-320.
      *                          *-------------------------------------*
      *                          * Se ultima operazione effettuata :   *
      *                          * Richiesta di pagamento              *
      *                          *-------------------------------------*
           move      54                   to   v-pos                  .
           go to     qry-liv-det-380.
       qry-liv-det-330.
      *                          *-------------------------------------*
      *                          * Se ultima operazione effettuata :   *
      *                          * Pagamento                           *
      *                          *-------------------------------------*
           move      68                   to   v-pos                  .
           go to     qry-liv-det-380.
       qry-liv-det-380.
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           if        w-qry-liv-det-wis    >    999999999 or
                     w-qry-liv-det-wis    <   -999999999
                     move  11             to   v-car
           else      move  09             to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           if        w-qry-liv-det-wis    >    999999999 or
                     w-qry-liv-det-wis    <   -999999999
                     move  "B"            to   v-edm
           else      move  "BG"           to   v-edm                  .
           move      v-lnr                to   v-lin                  .
      *
           if        c-dec                <    1
                     add  1               to   v-pos                  .
      *
           move      w-qry-liv-det-wis    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-400.
      *                  *---------------------------------------------*
      *                  * Segnale di pagamento bloccato               *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se pagamento non bloccato : no stampa   *
      *                      * sola registrazione : no stampa          *
      *                      *-----------------------------------------*
           if        rf-sfs-snx-pbl       not  = "S"
                     go to qry-liv-det-500.
      *                      *-----------------------------------------*
      *                      * Se l'ultima operazione non e' stata la  *
      *                      * sola registrazione : no stampa          *
      *                      *-----------------------------------------*
           if        w-det-srd-scf-suo    not  = "REG"
                     go to qry-liv-det-500.
      *                      *-----------------------------------------*
      *                      * Stampa                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      12                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      55                   to   v-pos                  .
           move      "* BLOCCATO *"       to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       qry-liv-det-500.
      *                  *---------------------------------------------*
      *                  * Separatore per importi pagati               *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      67                   to   v-pos                  .
           move      "|"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-liv-det-999.
       qry-liv-det-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione risultati analisi                         *
      *    *-----------------------------------------------------------*
       vis-ris-ana-000.
      *              *-------------------------------------------------*
      *              * Visualizzazione totali divisi per scadenza      *
      *              *-------------------------------------------------*
           perform   vis-tot-tts-000      thru vis-tot-tts-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to vis-ris-ana-999.
       vis-ris-ana-999.
           exit.

      *    *===========================================================*
      *    * Visualizzazione totali divisi per scadenza                *
      *    *-----------------------------------------------------------*
       vis-tot-tts-000.
      *              *-------------------------------------------------*
      *              * Subroutine di avanzamento pagina                *
      *              *-------------------------------------------------*
           perform   qry-pag-adv-000      thru qry-pag-adv-999        .
      *              *-------------------------------------------------*
      *              * Se interruzione forzata : uscita                *
      *              *-------------------------------------------------*
           if        w-cnt-qry-flg-int    not  = spaces
                     go to vis-tot-tts-999.
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
       vis-tot-tts-100.
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
           move      " Tipo scadenza       A scadere        Scaduto     
      -              " In pagamento |    Pagate     "
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
      -              "--------------| --------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-200.
      *              *-------------------------------------------------*
      *              * Inizializzazione contatore 01..11               *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-scf-tts-i01      .
       vis-tot-tts-300.
      *              *-------------------------------------------------*
      *              * Incremento contatore 01..11                     *
      *              *-------------------------------------------------*
           add       1                    to   w-ana-scf-tts-i01      .
      *              *-------------------------------------------------*
      *              * Se oltre il massimo : a totali                  *
      *              *-------------------------------------------------*
           if        w-ana-scf-tts-i01    >    11
                     go to vis-tot-tts-800.
       vis-tot-tts-400.
      *              *-------------------------------------------------*
      *              * Stampa elemento con indice 'w-ana-scf-tts-i01'  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Preparazione mark-point                     *
      *                  *---------------------------------------------*
           move      001                  to   w-mpn-tip-mpn          .
           move      spaces               to   w-mpn-att-mpn          .
           move      w-ana-scf-tts-i01    to   w-mpn-tsc-001          .
      *                      *-----------------------------------------*
      *                      * Tipo scadenza                           *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      17                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "+"                  to   v-edm                  .
           move      w-mpn                to   v-cnt                  .
           if        w-ana-scf-tts-i01    =    01
                     move  "Rimesse Dirette :"
                                          to   v-alf
           else if   w-ana-scf-tts-i01    =    02
                     move  "Incassi Elettr. :"
                                          to   v-alf
           else if   w-ana-scf-tts-i01    =    03
                     move  "Ri.Ba.          :"
                                          to   v-alf
           else if   w-ana-scf-tts-i01    =    04
                     move  "C.d.O.          :"
                                          to   v-alf
           else if   w-ana-scf-tts-i01    =    05
                     move  "M.Av.           :"
                                          to   v-alf
           else if   w-ana-scf-tts-i01    =    06
                     move  "R.I.D.          :"
                                          to   v-alf
           else if   w-ana-scf-tts-i01    =    07
                     move  "Bonifici Banc.  :"
                                          to   v-alf
           else if   w-ana-scf-tts-i01    =    08
                     move  "C/C Postali     :"
                                          to   v-alf
           else if   w-ana-scf-tts-i01    =    09
                     move  "Ricevute Banc.  :"
                                          to   v-alf
           else if   w-ana-scf-tts-i01    =    10
                     move  "Tratte          :"
                                          to   v-alf
           else if   w-ana-scf-tts-i01    =    11
                     move  "Paghero'        :"
                                          to   v-alf           .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * A scadere                               *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      18                   to   v-pos                  .
           move      w-ana-scf-tts-asc
                    (w-ana-scf-tts-i01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Scaduto                                 *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-ana-scf-tts-sca
                    (w-ana-scf-tts-i01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * In pagamento                            *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      50                   to   v-pos                  .
           move      w-ana-scf-tts-ipg
                    (w-ana-scf-tts-i01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Separatore per importi pagati           *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      65                   to   v-pos                  .
           move      "|"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Pagate                                  *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      66                   to   v-pos                  .
           move      w-ana-scf-tts-pag
                    (w-ana-scf-tts-i01)   to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-700.
      *                  *---------------------------------------------*
      *                  * Riciclo a tipo scadenza successivo          *
      *                  *---------------------------------------------*
           go to     vis-tot-tts-300.
       vis-tot-tts-800.
      *              *-------------------------------------------------*
      *              * Totali finali                                   *
      *              *-------------------------------------------------*
       vis-tot-tts-805.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-810.
      *                  *---------------------------------------------*
      *                  * Sottolineatura fincatura                    *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "----------------  --------------  --------------  
      -              "--------------| --------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-815.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-820.
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
       vis-tot-tts-830.
      *                  *---------------------------------------------*
      *                  * Totale 'A scadere'                          *
      *                  *---------------------------------------------*
       vis-tot-tts-831.
      *                      *-----------------------------------------*
      *                      * Preparazione del totale                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-ana-scf-tts-s11      .
           move      zero                 to   w-ana-scf-tts-i01      .
       vis-tot-tts-832.
           add       1                    to   w-ana-scf-tts-i01      .
           if        w-ana-scf-tts-i01    >    11
                     go to vis-tot-tts-833.
           add       w-ana-scf-tts-asc
                    (w-ana-scf-tts-i01)   to   w-ana-scf-tts-s11      .
           go to     vis-tot-tts-832.
       vis-tot-tts-833.
      *                      *-----------------------------------------*
      *                      * Stampa del totale                       *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      18                   to   v-pos                  .
           move      w-ana-scf-tts-s11    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-840.
      *                  *---------------------------------------------*
      *                  * Totale 'Scaduto'                            *
      *                  *---------------------------------------------*
       vis-tot-tts-841.
      *                      *-----------------------------------------*
      *                      * Preparazione del totale                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-ana-scf-tts-s11      .
           move      zero                 to   w-ana-scf-tts-i01      .
       vis-tot-tts-842.
           add       1                    to   w-ana-scf-tts-i01      .
           if        w-ana-scf-tts-i01    >    11
                     go to vis-tot-tts-843.
           add       w-ana-scf-tts-sca
                    (w-ana-scf-tts-i01)   to   w-ana-scf-tts-s11      .
           go to     vis-tot-tts-842.
       vis-tot-tts-843.
      *                      *-----------------------------------------*
      *                      * Stampa del totale                       *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      34                   to   v-pos                  .
           move      w-ana-scf-tts-s11    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-850.
      *                  *---------------------------------------------*
      *                  * Totale 'In pagamento'                       *
      *                  *---------------------------------------------*
       vis-tot-tts-851.
      *                      *-----------------------------------------*
      *                      * Preparazione del totale                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-ana-scf-tts-s11      .
           move      zero                 to   w-ana-scf-tts-i01      .
       vis-tot-tts-852.
           add       1                    to   w-ana-scf-tts-i01      .
           if        w-ana-scf-tts-i01    >    11
                     go to vis-tot-tts-853.
           add       w-ana-scf-tts-ipg
                    (w-ana-scf-tts-i01)   to   w-ana-scf-tts-s11      .
           go to     vis-tot-tts-852.
       vis-tot-tts-853.
      *                      *-----------------------------------------*
      *                      * Stampa del totale                       *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      50                   to   v-pos                  .
           move      w-ana-scf-tts-s11    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-860.
      *                  *---------------------------------------------*
      *                  * Totale 'Pagate'                             *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Separatore per importi pagati           *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      01                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      65                   to   v-pos                  .
           move      "|"                  to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Preparazione del totale                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-ana-scf-tts-s11      .
           move      zero                 to   w-ana-scf-tts-i01      .
       vis-tot-tts-862.
           add       1                    to   w-ana-scf-tts-i01      .
           if        w-ana-scf-tts-i01    >    11
                     go to vis-tot-tts-863.
           add       w-ana-scf-tts-pag
                    (w-ana-scf-tts-i01)   to   w-ana-scf-tts-s11      .
           go to     vis-tot-tts-862.
       vis-tot-tts-863.
      *                      *-----------------------------------------*
      *                      * Stampa del totale                       *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "BG"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      66                   to   v-pos                  .
           move      w-ana-scf-tts-s11    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-875.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-880.
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
       vis-tot-tts-885.
      *                  *---------------------------------------------*
      *                  * Interlinea                                  *
      *                  *---------------------------------------------*
           move      "LF"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-890.
      *                  *---------------------------------------------*
      *                  * Literal per 'Situazione debitoria globale'  *
      *                  *---------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      30                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      10                   to   v-pos                  .
           move      "Situazione debitoria globale :"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-895.
      *                  *---------------------------------------------*
      *                  * Totale 'Situazione debitoria globale'       *
      *                  *---------------------------------------------*
       vis-tot-tts-896.
      *                      *-----------------------------------------*
      *                      * Preparazione del totale                 *
      *                      *-----------------------------------------*
           move      zero                 to   w-ana-scf-tts-s11      .
           move      zero                 to   w-ana-scf-tts-i01      .
       vis-tot-tts-897.
           add       1                    to   w-ana-scf-tts-i01      .
           if        w-ana-scf-tts-i01    >    11
                     go to vis-tot-tts-898.
           add       w-ana-scf-tts-asc
                    (w-ana-scf-tts-i01)   to   w-ana-scf-tts-s11      .
           add       w-ana-scf-tts-sca
                    (w-ana-scf-tts-i01)   to   w-ana-scf-tts-s11      .
           add       w-ana-scf-tts-ipg
                    (w-ana-scf-tts-i01)   to   w-ana-scf-tts-s11      .
           go to     vis-tot-tts-897.
       vis-tot-tts-898.
      *                      *-----------------------------------------*
      *                      * Stampa del totale                       *
      *                      *-----------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      v-lnr                to   v-lin                  .
           move      41                   to   v-pos                  .
           move      w-ana-scf-tts-s11    to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-899.
      *                      *-----------------------------------------*
      *                      * Stampa del totale netto (senza quelle   *
      *                      * Bloccate)                               *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Test se da eseguire                 *
      *                          *-------------------------------------*
           if        w-ana-scf-tts-blo    =    zero
                     go to vis-tot-tts-900.
           if        rr-sts-scd           >    5
                     go to vis-tot-tts-900.
      *                          *-------------------------------------*
      *                          * Editing                             *
      *                          *-------------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "V"                  to   v-tip                  .
           move      11                   to   v-car                  .
           move      c-dec                to   v-dec                  .
           move      "S"                  to   v-sgn                  .
           move      "<G"                 to   v-edm                  .
           move      w-ana-scf-tts-s11    to   v-num                  .
           subtract  w-ana-scf-tts-blo    from v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                          *-------------------------------------*
      *                          * Assemblaggio                        *
      *                          *-------------------------------------*
           move      21                   to   w-all-str-lun          .
           move      03                   to   w-all-str-num          .
           move      "(*"                 to   w-all-str-cat (1)      .
           move      v-edt                to   w-all-str-cat (2)      .
           move      "*)"                 to   w-all-str-cat (3)      .
           perform   all-str-csb-000      thru all-str-csb-999        .
      *                          *-------------------------------------*
      *                          * Stampa                              *
      *                          *-------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      21                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      58                   to   v-pos                  .
           move      w-all-str-alf        to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       vis-tot-tts-900.
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     vis-tot-tts-999.
       vis-tot-tts-999.
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
      *                  *---------------------------------------------*
      *                  * Si' function key EXPD                       *
      *                  *---------------------------------------------*
           move      "EXPD"               to   v-pfk (02)             .
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
      *              *-------------------------------------------------*
      *              * Deviazione a seconda della function-key         *
      *              *-------------------------------------------------*
           if        v-key                =    "-PM-"
                     go to qry-trt-fun-100
           else if   v-key                =    "SLCT"
                     go to qry-trt-fun-200
           else if   v-key                =    "EXPD"
                     go to qry-trt-fun-300
           else      go to qry-trt-fun-900.
       qry-trt-fun-100.
      *              *-------------------------------------------------*
      *              * Se funzione pre Mark-Point                      *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Spostamento contenuto del mark-point in a-  *
      *                  * rea mark-point                              *
      *                  *---------------------------------------------*
           move      v-cnt                to   w-mpn                  .
      *                  *---------------------------------------------*
      *                  * Test su tipo mark-point                     *
      *                  *---------------------------------------------*
           if        w-mpn-tip-mpn        not  = 02
                     go to qry-trt-fun-999.
      *                  *---------------------------------------------*
      *                  * Se codice fornitore a zero, a visualizza-   *
      *                  * zione a spaces                              *
      *                  *---------------------------------------------*
           if        w-mpn-cod-002        =    zero
                     go to qry-trt-fun-180.
      *                  *---------------------------------------------*
      *                  * Linea di trattini                           *
      *                  *---------------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Fornitore                                   *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Composizione stringa                    *
      *                      *-----------------------------------------*
           move      spaces               to   w-qry-trt-fun-x80      .
           move      1                    to   w-qry-trt-fun-pnt      .
      *                          *-------------------------------------*
      *                          * Prompt                              *
      *                          *-------------------------------------*
           string    "Fornitore : "
                                delimited by   size
                                          into w-qry-trt-fun-x80
                                  with pointer w-qry-trt-fun-pnt      .
      *                          *-------------------------------------*
      *                          * Codice fornitore                    *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Editing                         *
      *                              *---------------------------------*
           move      "ED"                 to   v-ope                  .
           move      "N"                  to   v-tip                  .
           move      07                   to   v-car                  .
           move      zero                 to   v-dec                  .
           move      spaces               to   v-sgn                  .
           move      "<"                  to   v-edm                  .
           move      w-mpn-cod-002        to   v-num                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                              *---------------------------------*
      *                              * String                          *
      *                              *---------------------------------*
           string    v-edt
                                delimited by   spaces
                                          into w-qry-trt-fun-x80
                                  with pointer w-qry-trt-fun-pnt      .
      *                          *-------------------------------------*
      *                          * Dipendenza fornitore                *
      *                          *-------------------------------------*
      *                              *---------------------------------*
      *                              * Test se esistente               *
      *                              *---------------------------------*
           if        w-mpn-dpz-002        =    spaces
                     go to qry-trt-fun-110.
      *                              *---------------------------------*
      *                              * String                          *
      *                              *---------------------------------*
           string    "-"
                                delimited by   size
                     w-mpn-dpz-002
                                delimited by   spaces
                                          into w-qry-trt-fun-x80
                                  with pointer w-qry-trt-fun-pnt      .
       qry-trt-fun-110.
      *                          *-------------------------------------*
      *                          * Ragione sociale fornitore           *
      *                          *-------------------------------------*
           string    " "
                                delimited by   size
                     w-mpn-rag-002
                                delimited by   size
                                          into w-qry-trt-fun-x80
                                  with pointer w-qry-trt-fun-pnt      .
      *                      *-----------------------------------------*
      *                      * Stampa stringa                          *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      w-qry-trt-fun-x80    to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     qry-trt-fun-999.
       qry-trt-fun-180.
      *                  *---------------------------------------------*
      *                  * Visualizzazione a spaces                    *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Linea di trattini                       *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      20                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      all   "-"            to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                      *-----------------------------------------*
      *                      * Linea fornitore a spazi                 *
      *                      *-----------------------------------------*
           move      "DS"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      21                   to   v-lin                  .
           move      01                   to   v-pos                  .
           move      spaces               to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     qry-trt-fun-999.
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
           if        w-mpn-tip-mpn        not  = 02
                     go to trt-fky-exp-999.
      *              *-------------------------------------------------*
      *              * Salvataggio parametri per Start su [sfs]        *
      *              *-------------------------------------------------*
           if        w-rip-str-sfs-eof    not  = spaces
                     go to trt-fky-exp-100.
           move      rf-sfs-dts-scf       to   w-rip-str-sfs-dts      .
           move      rf-sfs-num-scf       to   w-rip-str-sfs-num      .
       trt-fky-exp-100.
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
           move      "ESPSCF    "         to   s-alf                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Scrittura variabile di i.p.c. 'num-scf' per il  *
      *              * livello successivo per il numero scadenza su    *
      *              * cui eseguire l'espansione                       *
      *              *-------------------------------------------------*
           move      "PV"                 to   s-ope                  .
           move      "num-scf"            to   s-var                  .
           move      "+"                  to   s-dop                  .
           move      "N"                  to   s-tip                  .
           move      11                   to   s-car                  .
           move      zero                 to   s-dec                  .
           move      spaces               to   s-sgn                  .
           move      v-cnt                to   w-mpn                  .
           move      w-mpn-num-002        to   s-num                  .
           call      "swd/mod/prg/obj/msegrt"
                                         using s                      .
      *              *-------------------------------------------------*
      *              * Richiamo del programma di interrogazione        *
      *              *-------------------------------------------------*
           move      "pgm/scf/prg/obj/pscf3010"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat                                            .
           cancel    s-pat                                            .
       trt-fky-exp-900.
      *              *-------------------------------------------------*
      *              * Ripristino Start                                *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Se flag di fine file in On : uscita         *
      *                  *---------------------------------------------*
           if        w-rip-str-sfs-eof    not  = spaces
                     go to trt-fky-exp-999.
      *                  *---------------------------------------------*
      *                  * Start su file [sfs]                         *
      *                  *---------------------------------------------*
           move      "SK"                 to   f-ope                  .
           move      "DTSSCF    "         to   f-key                  .
           move      w-rip-str-sfs-dts    to   rf-sfs-dts-scf         .
           move      w-rip-str-sfs-num    to   rf-sfs-num-scf         .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
      *                      *-----------------------------------------*
      *                      * Test su esito operazione                *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to trt-fky-exp-990.
      *                  *---------------------------------------------*
      *                  * Lettura sequenziale di ripristino           *
      *                  *---------------------------------------------*
           move      "RN"                 to   f-ope                  .
           move      "pgm/scf/fls/ioc/obj/iofsfs"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfs                 .
           if        f-sts                not  = e-not-err
                     go to trt-fky-exp-990.
           go to     trt-fky-exp-999.
       trt-fky-exp-990.
      *                  *---------------------------------------------*
      *                  * Se ripristino non possibile                 *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Flag di interruzione                    *
      *                      *-----------------------------------------*
           move      "#"                  to   w-cnt-qry-flg-int      .
       trt-fky-exp-999.
           exit.

      *    *===========================================================*
      *    * Intestazione per interrogazione                           *
      *    *-----------------------------------------------------------*
       int-pag-sta-000.
      *              *-------------------------------------------------*
      *              * Se si e' al primo avanzamento pagina si richia- *
      *              * ma la funzione di abilitazione della funzione   *
      *              * pre-mark-point                                  *
      *              *-------------------------------------------------*
           if        v-pag                not  = zero
                     go to int-pag-sta-050.
           move      "PM"                 to   v-ope                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       int-pag-sta-050.
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
      *              * Linea di fincatura                              *
      *              *-------------------------------------------------*
           move      "PF"                 to   v-ope                  .
           move      "A"                  to   v-tip                  .
           move      80                   to   v-car                  .
           move      v-lnr                to   v-lin                  .
           move      01                   to   v-pos                  .
           move      "  Numero     Tipo    Data     A scadere     Scadut
      -              "o     In pagam. |   Pagate    "
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
           move      "-----------  ----  --------  -----------  --------
      -              "---  -----------| ------------"
                                          to   v-alf                  .
           call      "swd/mod/prg/obj/mvideo"
                                         using v                      .
       int-pag-sta-999.
           exit.

      *    *===========================================================*
      *    * Inizializzazione totali per tipo scadenza                 *
      *    *-----------------------------------------------------------*
       tot-tts-ini-000.
      *              *-------------------------------------------------*
      *              * Inizializzazione indice 01..11                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-scf-tts-i01      .
      *              *-------------------------------------------------*
      *              * Azzeramento totale 'Bloccate'                   *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-scf-tts-blo      .
       tot-tts-ini-100.
      *              *-------------------------------------------------*
      *              * Incremento indice 01..11                        *
      *              *-------------------------------------------------*
           add       1                    to   w-ana-scf-tts-i01      .
      *              *-------------------------------------------------*
      *              * Se oltre il max : uscita                        *
      *              *-------------------------------------------------*
           if        w-ana-scf-tts-i01    >    11
                     go to tot-tts-ini-999.
      *              *-------------------------------------------------*
      *              * Azzeramento totale importo                      *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-scf-tts-tot
                                              (w-ana-scf-tts-i01)     .
      *              *-------------------------------------------------*
      *              * Azzeramento totale numero scadenze              *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-scf-tts-num
                                              (w-ana-scf-tts-i01)     .
      *              *-------------------------------------------------*
      *              * Azzeramento totale 'A scadere'                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-scf-tts-asc
                                              (w-ana-scf-tts-i01)     .
      *              *-------------------------------------------------*
      *              * Azzeramento totale 'Scaduto'                    *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-scf-tts-sca
                                              (w-ana-scf-tts-i01)     .
      *              *-------------------------------------------------*
      *              * Azzeramento totale 'In pagamento'               *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-scf-tts-ipg
                                              (w-ana-scf-tts-i01)     .
      *              *-------------------------------------------------*
      *              * Azzeramento totale 'Pagate'                     *
      *              *-------------------------------------------------*
           move      zero                 to   w-ana-scf-tts-pag
                                              (w-ana-scf-tts-i01)     .
      *              *-------------------------------------------------*
      *              * Riciclo su elemento successivo                  *
      *              *-------------------------------------------------*
           go to     tot-tts-ini-100.
       tot-tts-ini-999.
           exit.

      *    *===========================================================*
      *    * Aggiornamento totali per tipo scadenza                    *
      *    *-----------------------------------------------------------*
       tot-tts-agg-000.
      *              *-------------------------------------------------*
      *              * Preparazione indice per tipo scadenza           *
      *              *-------------------------------------------------*
           move      rf-sfs-tip-scf       to   w-ana-scf-tts-i01      .
      *              *-------------------------------------------------*
      *              * Aggiornamento totale importo                    *
      *              *-------------------------------------------------*
           add       w-qry-liv-det-wis    to   w-ana-scf-tts-tot
                                              (w-ana-scf-tts-i01)     .
      *              *-------------------------------------------------*
      *              * Aggiornamento totale numero scadenze            *
      *              *-------------------------------------------------*
           add       1                    to   w-ana-scf-tts-num
                                              (w-ana-scf-tts-i01)     .
      *              *-------------------------------------------------*
      *              * Aggiornamento 'A scadere'/'Scaduto'             *
      *              *-------------------------------------------------*
           if        w-det-srd-scf-suo    not  = "REG"
                     go to tot-tts-agg-100.
           if        rf-sfs-dts-scf       >    rr-dat-rfa
                     add   w-qry-liv-det-wis
                                          to   w-ana-scf-tts-asc
                                              (w-ana-scf-tts-i01)
           else      add   w-qry-liv-det-wis
                                          to   w-ana-scf-tts-sca
                                              (w-ana-scf-tts-i01)     .
       tot-tts-agg-100.
      *              *-------------------------------------------------*
      *              * Aggiornamento 'In pagamento'/'Pagate'           *
      *              *-------------------------------------------------*
           if        w-det-srd-scf-suo    =    "RDP"
                     add   w-qry-liv-det-wis
                                          to   w-ana-scf-tts-ipg
                                              (w-ana-scf-tts-i01)
           else if   w-det-srd-scf-suo    =    "PAG"
                     add   w-qry-liv-det-wis
                                          to   w-ana-scf-tts-pag
                                              (w-ana-scf-tts-i01)     .
       tot-tts-agg-200.
      *              *-------------------------------------------------*
      *              * Aggiornamento 'Bloccate'                        *
      *              *-------------------------------------------------*
           if        rf-sfs-snx-pbl       =   "S"
                     add   w-qry-liv-det-wis
                                          to   w-ana-scf-tts-blo      .
       tot-tts-agg-999.
           exit.

      *    *===========================================================*
      *    * Routine lettura archivio dipendenza fornitore in [dcf]    *
      *    *-----------------------------------------------------------*
       let-arc-dcf-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione marker di uscita                *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-flg      .
      *              *-------------------------------------------------*
      *              * Test se codice fornitore a zero                 *
      *              *-------------------------------------------------*
           if        w-let-arc-dcf-cod    =    zero
                     go to let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Test se codice dipendenza a spaces, solo se ri- *
      *              * chiesta di lettura specificamente di una dipen- *
      *              * denza                                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcf-dpz    =    spaces and
                     w-let-arc-dcf-tle    =    "D"
                     go to let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Test se codice dipendenza a "*   ", solo se ri- *
      *              * chiesta di lettura specificamente di una dipen- *
      *              * denza                                           *
      *              *-------------------------------------------------*
           if        w-let-arc-dcf-dpz    =    "*   " and
                     w-let-arc-dcf-tle    =    "D"
                     go to let-arc-dcf-450.
      *              *-------------------------------------------------*
      *              * Lettura per codice                              *
      *              *-------------------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "CODFNT    "         to   f-key                  .
           move      w-let-arc-dcf-cod    to   rf-dcf-cod-fnt         .
           move      w-let-arc-dcf-dpz    to   rf-dcf-dpz-fnt         .
           move      "pgm/dcf/fls/ioc/obj/iofdcf"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-dcf                 .
           if        f-sts                not  = e-not-err
                     go to let-arc-dcf-400.
       let-arc-dcf-200.
      *              *-------------------------------------------------*
      *              * Bufferizzazione valori                          *
      *              *-------------------------------------------------*
           move      rf-dcf-rag-soc       to   w-let-arc-dcf-rag      .
           move      rf-dcf-via-dcf       to   w-let-arc-dcf-via      .
           move      rf-dcf-loc-dcf       to   w-let-arc-dcf-loc      .
           move      rf-dcf-cod-abi       to   w-let-arc-dcf-abi      .
           move      rf-dcf-cod-cab       to   w-let-arc-dcf-cab      .
      *              *-------------------------------------------------*
      *              * Uscita                                          *
      *              *-------------------------------------------------*
           go to     let-arc-dcf-999.
       let-arc-dcf-400.
      *              *-------------------------------------------------*
      *              * Azioni per record non trovato                   *
      *              *-------------------------------------------------*
           move      "#"                  to   w-let-arc-dcf-flg      .
           move      all   "."            to   w-let-arc-dcf-rag      .
           go to     let-arc-dcf-600.
       let-arc-dcf-450.
      *              *-------------------------------------------------*
      *              * Normalizzazione per codice dipendenza "*   "    *
      *              *-------------------------------------------------*
           move      "Sia la sede che tutte le dipendenze     "
                                          to   w-let-arc-dcf-rag      .
           go to     let-arc-dcf-600.
       let-arc-dcf-500.
      *              *-------------------------------------------------*
      *              * Normalizzazione work area                       *
      *              *-------------------------------------------------*
           move      spaces               to   w-let-arc-dcf-rag      .
       let-arc-dcf-600.
           move      spaces               to   w-let-arc-dcf-via      .
           move      spaces               to   w-let-arc-dcf-loc      .
           move      zero                 to   w-let-arc-dcf-abi      .
           move      zero                 to   w-let-arc-dcf-cab      .
       let-arc-dcf-999.
           exit.

      *    *===========================================================*
      *    * Determinazione status scadenza fornitore                  *
      *    *-----------------------------------------------------------*
       det-srd-scf-000.
      *              *-------------------------------------------------*
      *              * Determinazione sigla ultima operazione eseguita *
      *              * sulla scadenza                                  *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Normalizzazione iniziale sigla              *
      *                  *---------------------------------------------*
           move      spaces               to   w-det-srd-scf-suo      .
       det-srd-scf-100.
      *                  *---------------------------------------------*
      *                  * Registrazione scadenza                      *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se la data di registrazione scadenza e' *
      *                      * superiore alla data di riferimento per  *
      *                      * la determinazione : uscita con status   *
      *                      * 'N'                                     *
      *                      *-----------------------------------------*
           if        rf-sfs-dtr-rgs       >    w-det-srd-scf-drd
                     move  "N"            to   w-det-srd-scf-sts
                     go to det-srd-scf-999.
      *                      *-----------------------------------------*
      *                      * Ultima operazione eseguita : Registra-  *
      *                      * zione                                   *
      *                      *-----------------------------------------*
           move      "REG"                to   w-det-srd-scf-suo      .
      *                      *-----------------------------------------*
      *                      * Status scadenza : Aperta                *
      *                      *-----------------------------------------*
           move      "A"                  to   w-det-srd-scf-sts      .
       det-srd-scf-200.
      *                  *---------------------------------------------*
      *                  * Storno scadenza                             *
      *                  *---------------------------------------------*
           if        rf-sfs-dtr-sto       =    zero
                     go to det-srd-scf-300.
      *                      *-----------------------------------------*
      *                      * Se la data di storno scadenza e' supe-  *
      *                      * riore alla data di riferimento per      *
      *                      * la determinazione : oltre               *
      *                      *-----------------------------------------*
           if        rf-sfs-dtr-sto       >    w-det-srd-scf-drd
                     go to det-srd-scf-300.
      *                      *-----------------------------------------*
      *                      * Ultima operazione eseguita : Storno     *
      *                      *-----------------------------------------*
           move      "STO"                to   w-det-srd-scf-suo      .
      *                      *-----------------------------------------*
      *                      * Status scadenza : Chiusa                *
      *                      *-----------------------------------------*
           move      "C"                  to   w-det-srd-scf-sts      .
      *                      *-----------------------------------------*
      *                      * Uscita                                  *
      *                      *-----------------------------------------*
           go to     det-srd-scf-300.
       det-srd-scf-300.
      *                  *---------------------------------------------*
      *                  * Pagamento scadenza                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se la scadenza non porta un numero pa-  *
      *                      * gamento : uscita                        *
      *                      *-----------------------------------------*
           if        rf-sfs-num-pgf       =    zero
                     go to det-srd-scf-999.
      *                      *-----------------------------------------*
      *                      * Lettura record [sfp]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMPGF    "         to   f-key                  .
           move      rf-sfs-num-pgf       to   rf-sfp-num-pgf         .
           move      "pgm/scf/fls/ioc/obj/iofsfp"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfp                 .
      *                      *-----------------------------------------*
      *                      * Se record non esistente : uscita        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-srd-scf-999.
      *                      *-----------------------------------------*
      *                      * Se la data di pagamento e' superiore    *
      *                      * alla data di riferimento per la deter-  *
      *                      * minazione : uscita                      *
      *                      *-----------------------------------------*
           if        rf-sfp-dtr-pgf       >    w-det-srd-scf-drd
                     go to det-srd-scf-999.
      *                      *-----------------------------------------*
      *                      * Deviazione a seconda che la modalita'   *
      *                      * di pagamento implichi o meno un'opera-  *
      *                      * zione di addebito                       *
      *                      *-----------------------------------------*
           if        rf-sfp-mod-pgf       =    01 or
                     rf-sfp-mod-pgf       =    02 or
                     rf-sfp-mod-pgf       =    03 or
                     rf-sfp-mod-pgf       =    04
                     go to det-srd-scf-120
           else if   rf-sfp-mod-pgf       =    13 or
                     rf-sfp-mod-pgf       =    14 or
                     rf-sfp-mod-pgf       =    15
                     go to det-srd-scf-140
           else      go to det-srd-scf-999.
       det-srd-scf-120.
      *                      *-----------------------------------------*
      *                      * Se la modalita' di pagamento non impli- *
      *                      * ca una successiva operazione di addebi- *
      *                      * to                                      *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ultima operazione eseguita : Paga-  *
      *                          * mento                               *
      *                          *-------------------------------------*
           move      "PAG"                to   w-det-srd-scf-suo      .
      *                          *-------------------------------------*
      *                          * Determinazione data di pagamento    *
      *                          * tenendo conto dei giorni di ritardo *
      *                          * medi presunti affinche' il pagamen- *
      *                          * to pervenga al fornitore            *
      *                          *-------------------------------------*
           move      rf-sfp-dtr-pgf       to   w-det-dat-mng-dti      .
           move      w-prs-scf-grm-ppf    to   w-det-dat-mng-ngi      .
           perform   det-dat-mng-000      thru det-dat-mng-999        .
      *                          *-------------------------------------*
      *                          * Se la data di pagamento cosi' otte- *
      *                          * nuta e' superiore alla data di ri-  *
      *                          * ferimento per la determinazione     *
      *                          *-------------------------------------*
           if        w-det-dat-mng-dto    >    w-det-srd-scf-drd
                     move  "A"            to   w-det-srd-scf-sts
           else      move  "C"            to   w-det-srd-scf-sts      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     det-srd-scf-999.
       det-srd-scf-140.
      *                      *-----------------------------------------*
      *                      * Se la modalita' di pagamento implica u- *
      *                      * na successiva operazione di addebito    *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Ultima operazione eseguita : Ri-    *
      *                          * chiesta di pagamento                *
      *                          *-------------------------------------*
           move      "RDP"                to   w-det-srd-scf-suo      .
      *                          *-------------------------------------*
      *                          * Status scadenza : Aperta            *
      *                          *-------------------------------------*
           move      "A"                  to   w-det-srd-scf-sts      .
      *                          *-------------------------------------*
      *                          * A trattamento addebito pagamento    *
      *                          *-------------------------------------*
           go to     det-srd-scf-400.
       det-srd-scf-400.
      *                  *---------------------------------------------*
      *                  * Addebito pagamento                          *
      *                  *---------------------------------------------*
      *                      *-----------------------------------------*
      *                      * Se il pagamento non porta un numero ad- *
      *                      * debito : uscita                         *
      *                      *-----------------------------------------*
           if        rf-sfp-num-adp       =    zero
                     go to det-srd-scf-999.
      *                      *-----------------------------------------*
      *                      * Lettura record [sfa]                    *
      *                      *-----------------------------------------*
           move      "RK"                 to   f-ope                  .
           move      "NUMADP    "         to   f-key                  .
           move      rf-sfp-num-adp       to   rf-sfa-num-adp         .
           move      "pgm/scf/fls/ioc/obj/iofsfa"
                                          to   s-pat                  .
           call      "swd/mod/prg/obj/mfiltp"
                                         using s                      .
           call      s-pat               using f
                                               rf-sfa                 .
      *                      *-----------------------------------------*
      *                      * Se record non esistente : uscita        *
      *                      *-----------------------------------------*
           if        f-sts                not  = e-not-err
                     go to det-srd-scf-999.
      *                      *-----------------------------------------*
      *                      * Se la data di addebito e' superiore     *
      *                      * alla data di riferimento per la deter-  *
      *                      * minazione : uscita                      *
      *                      *-----------------------------------------*
           if        rf-sfa-dtr-adp       >    w-det-srd-scf-drd
                     go to det-srd-scf-999.
      *                      *-----------------------------------------*
      *                      * Ultima operazione eseguita : Pagamento  *
      *                      *-----------------------------------------*
           move      "PAG"                to   w-det-srd-scf-suo      .
      *                      *-----------------------------------------*
      *                      * Determinazione status                   *
      *                      *-----------------------------------------*
      *                          *-------------------------------------*
      *                          * Determinazione data di addebito te- *
      *                          * nendo conto dei giorni di ritardo   *
      *                          * medi presunti affinche' il pagamen- *
      *                          * to pervenga al fornitore            *
      *                          *-------------------------------------*
           move      rf-sfa-dtr-adp       to   w-det-dat-mng-dti      .
           move      w-prs-scf-grm-ppf    to   w-det-dat-mng-ngi      .
           perform   det-dat-mng-000      thru det-dat-mng-999        .
      *                          *-------------------------------------*
      *                          * Se la data di addebito cosi' otte-  *
      *                          * nuta e' superiore alla data di ri-  *
      *                          * ferimento per la determinazione     *
      *                          *-------------------------------------*
           if        w-det-dat-mng-dto    >    w-det-srd-scf-drd
                     move  "A"            to   w-det-srd-scf-sts
           else      move  "C"            to   w-det-srd-scf-sts      .
      *                          *-------------------------------------*
      *                          * Uscita                              *
      *                          *-------------------------------------*
           go to     det-srd-scf-999.
       det-srd-scf-999.
           exit.

      *    *===========================================================*
      *    * Determinazione di una data maggiorata fino ad un massimo  *
      *    * di 30 giorni                                              *
      *    *-----------------------------------------------------------*
       det-dat-mng-000.
      *              *-------------------------------------------------*
      *              * Normalizzazione data in output                  *
      *              *-------------------------------------------------*
           move      zero                 to   w-det-dat-mng-dto      .
      *              *-------------------------------------------------*
      *              * Se data in input a zero : uscita                *
      *              *-------------------------------------------------*
           if        w-det-dat-mng-dti    =    zero
                     go to det-dat-mng-999.
      *              *-------------------------------------------------*
      *              * Se numero giorni d'incremento superiore a 30 :  *
      *              * uscita                                          *
      *              *-------------------------------------------------*
           if        w-det-dat-mng-ngi    >    30
                     go to det-dat-mng-999.
      *              *-------------------------------------------------*
      *              * Se numero giorni d'incremento, zero : data in   *
      *              * input in data in output e uscita                *
      *              *-------------------------------------------------*
           if        w-det-dat-mng-ngi    =    zero
                     move  w-det-dat-mng-dti
                                          to   w-det-dat-mng-dto
                     go to det-dat-mng-999.
      *              *-------------------------------------------------*
      *              * Data in input in comodo ridefinito              *
      *              *-------------------------------------------------*
           move      w-det-dat-mng-dti    to   w-det-dat-mng-wdt      .
      *              *-------------------------------------------------*
      *              * Determinazione numero giorni di febbraio se me- *
      *              * se pari a 02                                    *
      *              *-------------------------------------------------*
           if        w-det-dat-mng-wdm    not  = 02
                     go to det-dat-mng-020.
           move      w-det-dat-mng-wds    to   w-det-dat-mng-wt5      .
           add       1900                 to   w-det-dat-mng-wt5      .
           divide    4                    into w-det-dat-mng-wt5
                                        giving w-det-dat-mng-wt6
                                     remainder w-det-dat-mng-wt7      .
           if        w-det-dat-mng-wt7    not  = zero
                     move  28             to   w-det-dat-mng-wt2 (2)
                     go to det-dat-mng-020.
           divide    400                  into w-det-dat-mng-wt5
                                        giving w-det-dat-mng-wt6
                                     remainder w-det-dat-mng-wt7      .
           if        w-det-dat-mng-wt7    =    zero
                     move  29             to   w-det-dat-mng-wt2 (2)
                     go to det-dat-mng-020.
           divide    100                  into w-det-dat-mng-wt5
                                        giving w-det-dat-mng-wt6
                                     remainder w-det-dat-mng-wt7      .
           if        w-det-dat-mng-wt7    =    zero
                     move  28             to   w-det-dat-mng-wt2 (2)
           else      move  29             to   w-det-dat-mng-wt2 (2)  .
       det-dat-mng-020.
      *              *-------------------------------------------------*
      *              * Deviazione a seconda che il numero giorni d'in- *
      *              * cremento sia 30 oppure no                       *
      *              *-------------------------------------------------*
           if        w-det-dat-mng-ngi    =    30
                     go to det-dat-mng-100
           else      go to det-dat-mng-500.
       det-dat-mng-100.
      *              *-------------------------------------------------*
      *              * Se numero giorni di incremento pari a 30        *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento mese                             *
      *                  *---------------------------------------------*
           add       1                    to   w-det-dat-mng-wdm      .
      *                  *---------------------------------------------*
      *                  * Aggiustamento se oltre i 12                 *
      *                  *---------------------------------------------*
           if        w-det-dat-mng-wdm    >    12
                     move  1              to   w-det-dat-mng-wdm
                     add   1              to   w-det-dat-mng-wds      .
      *                  *---------------------------------------------*
      *                  * Se il giorno calcolato supera l'ultimo      *
      *                  * giorno del mese, lo si riporta all'ultimo   *
      *                  * timo giorno del mese                        *
      *                  *---------------------------------------------*
           if        w-det-dat-mng-wdg    not  > w-det-dat-mng-wt2
                                                (w-det-dat-mng-wdm)
                     go to det-dat-mng-162.
           move      w-det-dat-mng-wt2
                    (w-det-dat-mng-wdm)   to   w-det-dat-mng-wdg      .
       det-dat-mng-162.
      *                  *---------------------------------------------*
      *                  * Data in output                              *
      *                  *---------------------------------------------*
           move      w-det-dat-mng-wdt    to   w-det-dat-mng-dto      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-dat-mng-999.
       det-dat-mng-500.
      *              *-------------------------------------------------*
      *              * Se numero giorni di incremento minore di 30     *
      *              *-------------------------------------------------*
      *                  *---------------------------------------------*
      *                  * Incremento numero giorni                    *
      *                  *---------------------------------------------*
           add       w-det-dat-mng-ngi    to   w-det-dat-mng-wdg      .
      *                  *---------------------------------------------*
      *                  * Se numero giorni inferiore al numero giorni *
      *                  * del mese : a valore finale                  *
      *                  *---------------------------------------------*
           if        w-det-dat-mng-wdg    not  > w-det-dat-mng-wt2
                                                (w-det-dat-mng-wdm)
                     go to det-dat-mng-600.
      *                  *---------------------------------------------*
      *                  * Aggiustamento giorno                        *
      *                  *---------------------------------------------*
           subtract  w-det-dat-mng-wt2
                    (w-det-dat-mng-wdm)   from w-det-dat-mng-wdg      .
      *                  *---------------------------------------------*
      *                  * Incremento mese                             *
      *                  *---------------------------------------------*
           add       1                    to   w-det-dat-mng-wdm      .
      *                  *---------------------------------------------*
      *                  * Eventuale aggiustamento mese                *
      *                  *---------------------------------------------*
           if        w-det-dat-mng-wdm    >    12
                     move  1              to   w-det-dat-mng-wdm
                     add   1              to   w-det-dat-mng-wds      .
       det-dat-mng-600.
      *                  *---------------------------------------------*
      *                  * Data in output                              *
      *                  *---------------------------------------------*
           move      w-det-dat-mng-wdt    to   w-det-dat-mng-dto      .
      *                  *---------------------------------------------*
      *                  * Uscita                                      *
      *                  *---------------------------------------------*
           go to     det-dat-mng-999.
       det-dat-mng-999.
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
      *    * Subroutines per allineamenti a destra o a sinistra oppure *
      *    * al centro di campi alfanumerici di varia lunghezza, fi-   *
      *    * no ad un massimo di 240 caratteri, oppure per il conca-   *
      *    * tenamento, con o senza separazione, di max 10 substrin-   *
      *    * ghe in una unica substringa                               *
      *    *-----------------------------------------------------------*
           copy      "swd/std/prg/cpy/wallstr0.cps"                   .

